#### 06 - CROSS-SCENARIO COMPARISON (optimistic vs realistic vs pessimistic)
# ------------------------------------------------------------
#  Reads the UNIFIED programme-total PSA draws produced by 05
#  (outputs/psa_programme_draws.csv) and makes three clean summary figures:
#    (1) combined CEAC (all scenarios on one axis)
#    (2) scenario forest plot (median + 95% CrI for net DALYs & net cost)
#    (3) Incremental Net Monetary Benefit densities
#  Run 05_subnational_scenario_outputs.R first.

# Paths are relative to the project root. Open business-case-ivermectin.Rproj
# so the working directory is set correctly.
if (!file.exists("data2.xlsx"))
    stop("Working directory is not the project root. Open ",
         "business-case-ivermectin.Rproj, or setwd() to the folder ",
         "containing data2.xlsx.")

# Load packages
library(dplyr)
library(ggplot2)
library(tidyr)

#Set output directory
out_dir <- "outputs"

#Set willingness to pay threshold
wtp <- 500     # keep consistent with the scenario scripts

# read the unified programme-total PSA draws produced by 05 
psa_file <- file.path(out_dir, "psa_programme_draws.csv")
if (!file.exists(psa_file)) #Check if file exists
    stop("Missing ", psa_file, " - run 05_subnational_scenario_outputs.R first.")
psa_all <- read.csv(psa_file) %>%
    transmute(scenario = factor(scenario, levels = c("Optimistic", "Realistic", "Pessimistic")),
              net_cost, net_dalys,
              inmb = wtp * net_dalys - net_cost)   # placeholder in case wtp changes in this script

pal <- c(Optimistic = "#00A087FF", Realistic = "#3C5488FF", Pessimistic = "#E64B35FF")

## 1. COMBINED Cost-effectiveness acceptability curve (CEAC)
thr <- seq(0, 2000, by = 25) #Creates different wtp thresholds
ceac_all <- do.call(rbind, lapply(levels(psa_all$scenario), function(s) { #loops through each scenario to create CEAC curve separately
    d <- filter(psa_all, scenario == s) #selects only PSA simulations belonging to that scenario
    data.frame(scenario = s, wtp = thr,
               prob_ce = sapply(thr, function(l) mean(l * d$net_dalys - d$net_cost > 0))) #For each WTP threshold, calculate probaibility of being cost effective NMB>0
})) %>% mutate(scenario = factor(scenario, levels = levels(psa_all$scenario)))

ceac_cmp <- ggplot(ceac_all, aes(wtp, prob_ce, colour = scenario)) +
    geom_line(linewidth = 1.2) +
    geom_vline(xintercept = wtp, linetype = "dashed", colour = "grey50") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_colour_manual(values = pal) +
    labs( #title = "Cost-effectiveness acceptability curves by scenario",
         #subtitle = "Probability the dose escalation is cost-effective at each WTP threshold",
         x = "Willingness-to-pay (USD per DALY averted)", y = "P(cost-effective)",
         colour = "Scenario") +
    theme_bw()
print(ceac_cmp)
#Save figure
ggsave(file.path(out_dir, "compare_ceac.png"), ceac_cmp, width = 8, height = 5, dpi = 300)

## 2. SCENARIO FOREST PLOT  (median + 95% CrI)

#Median and lower and upper 95% limit of PSA results
mk <- function(col, nice) {
    psa_all %>% group_by(scenario) %>%
        summarise(med = median(.data[[col]]),
                  lo  = quantile(.data[[col]], .025),
                  hi  = quantile(.data[[col]], .975), .groups = "drop") %>%
        mutate(metric = nice)
}
#Creates summaries for two outcomes, then combines them into a table
forest <- bind_rows(mk("net_dalys", "Net DALYs averted"),
                    mk("net_cost",  "Net cost (USD)"))

forest_cmp <- ggplot(forest, aes(x = med, y = scenario, colour = scenario)) +
    geom_vline(xintercept = 0, linetype = "dotted", colour = "grey60") +
    geom_segment(aes(x = lo, xend = hi, y = scenario, yend = scenario), linewidth = 1) +
    geom_point(size = 3.2) +
    facet_wrap(~ metric, scales = "free_x") +
    scale_colour_manual(values = pal) +
    labs(title = "Best / base / worst scenario comparison (efficacy + cost)",
         subtitle = "Point = median across PSA draws; bar = 2.5th–97.5th percentile. Scenarios differ in efficacy AND cost.",
         x = NULL, y = NULL) +
    theme_bw() + theme(legend.position = "none",
                       strip.text = element_text(face = "bold"))
print(forest_cmp)
#Save figure
ggsave(file.path(out_dir, "compare_forest.png"), forest_cmp, width = 9, height = 4, dpi = 300)

## 3. INMB DENSITIES
#    INMB = WTP * DALYs averted - net cost.  INMB > 0 = cost-effective.
# Compares distribution of INMB across three scenarios using all PSA simulations
inmb_cmp <- ggplot(psa_all, aes(x = inmb, fill = scenario)) +
    geom_density(alpha = 0.45, colour = NA) +
    geom_vline(xintercept = 0, linetype = "dashed") + #INMB = 0 left side not cost effective, right side cost effective
    scale_fill_manual(values = pal) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-9, suffix = " bn")) +
    labs(title = sprintf("Incremental Net Monetary Benefit at WTP $%d / DALY", wtp),
         x = "INMB (USD $ billions)", y = "Density", fill = "Scenario") + #in billions because programme total for all draws
    theme_bw()
print(inmb_cmp)
#Save figure
ggsave(file.path(out_dir, "compare_inmb_density.png"), inmb_cmp, width = 8, height = 5, dpi = 300)

## 4. NMB SUMMARY  (mean + 95% credible interval per scenario) for all PSA draws
#    NMB = INMB = wtp * net_dalys - net_cost, one value per PSA draw.
#    2.5th / 97.5th percentiles = 95% CrI, matching the forest plot above.

nmb_summary <- psa_all %>% group_by(scenario) %>%
    summarise(mean_nmb   = mean(inmb),
              lo_nmb     = quantile(inmb, 0.025),
              median_nmb = median(inmb),
              hi_nmb     = quantile(inmb, 0.975),
              .groups = "drop")
print(as.data.frame(nmb_summary), row.names = FALSE)
write.csv(nmb_summary, file.path(out_dir, "nmb_summary_by_scenario.csv"), row.names = FALSE)

# What proportion of PSA simulations for each scenario had positive NMB?

psa_all %>% group_by(scenario) %>%
    summarise(`P(cost-effective)` = sprintf("%.1f%%", 100 * mean(inmb > 0)), #Probability cost effective
              `median net DALYs`  = round(median(net_dalys), 0),
              `median net cost`   = round(median(net_cost), 0),
              .groups = "drop") %>%
    as.data.frame() %>% print(row.names = FALSE)

