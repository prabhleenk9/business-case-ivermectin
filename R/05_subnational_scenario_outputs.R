#### 05 - COST SENSITIVITY AT A COMMON CLINICAL BASE CASE + PER-REGION NMB DISTRIBUTIONS (PSA BOXPLOTS, VARYING COST AND EPI RANGES)
# ------------------------------------------------------------
#  PART 1 — Cost sensitivity at a common clinical base case.
#  Efficacy/clinical parameters are held FIXED (make_params default) across all
#  three scenarios, so the health outcome per subnational region is identical and
#  is shown once (cases_averted, dalys_gained, net_dalys, total_savings). Only the
#  COST stack differs between scenarios, so net_cost and ICER get a column each.
#  This isolates how the cost assumptions ALONE move cost-effectiveness per region.


# Paths are relative to the project root. Open business-case-ivermectin.Rproj
# so the working directory is set correctly.
if (!file.exists("data2.xlsx"))
    stop("Working directory is not the project root. Open ",
         "business-case-ivermectin.Rproj, or setwd() to the folder ",
         "containing data2.xlsx.")

#Source the model 
source("R/00_model_setup.R")   

#Output directory
out_dir <- "outputs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## 1. PREPARE DATA 
data_all <- data   

#Define wtp threshold for second function
wtp <- 500  # willingness-to-pay, USD per DALY

## 2. DEFINE FUNCTIONS

# Function 1: 
# Calculates health outcomes and economic outcomes for each subnational region.
# Scenario-specific planning and programme costs can be included and scaled using
# cost multipliers.
run_subnational_region <- function(p, data, use_planning = FALSE, use_programme = FALSE, #unless specified planning and programme costs are not included
                         plan_mult = 1, prog_mult = 1) {
    epi  <- f_epi(data, p)
    ae   <- f_AE(data, p)
    sae  <- f_SAE(data, p)
    plan_c <- if (use_planning)  plan_mult * data$planning_cost  else 0 #If realistic/pessimistic use, otherwise baseline=0, mult depends on scenario
    prog_c <- if (use_programme) prog_mult * data$programme_cost else 0
    cost <- f_cost(epi, ae, sae, data, p, planning_cost = plan_c, programme_cost = prog_c) #decides if planning and programme costs are included
    data.frame(country = data$country, subnational_region = data$subnational_region,
               cases_averted = epi$cases_averted,
               dalys_gained  = epi$dalys_gained,
               net_dalys     = cost$net_dalys,
               net_cost      = cost$net_cost,
               total_savings = cost$total_savings,
               icer = ifelse(cost$net_dalys != 0, cost$net_cost / cost$net_dalys, NA_real_))
}



#Function 2:
# Calculates net monetary benefit (NMB) for each subnational region under a given
# cost scenario, including optional planning and programme cost adjustments.
# p = parameter list; use_planning / use_programme toggle the cost stack;
# plan_mult / prog_mult scale those costs. Returns country, subnational region, nmb.
subnational_region_nmb <- function(p, data, use_planning = FALSE, use_programme = FALSE,
                                   plan_mult = 1, prog_mult = 1) {
    epi  <- f_epi(data, p)
    ae   <- f_AE(data, p)
    sae  <- f_SAE(data, p)
    plan_c <- if (use_planning)  plan_mult * data$planning_cost  else 0
    prog_c <- if (use_programme) prog_mult * data$programme_cost else 0
    cost <- f_cost(epi, ae, sae, data, p, planning_cost = plan_c, programme_cost = prog_c)
    data.frame(country  = data$country,
               subnational_region = data$subnational_region,
               net_cost  = cost$net_cost,
               net_dalys = cost$net_dalys,
               nmb       = wtp * cost$net_dalys - cost$net_cost)
}



## 3. RUN COST SCENARIOS (FUNCTION 1)

#USES BASELINE/MIDPOINT PARAMETER VALUES FOR EPI
#Function is defined, now run it for three scenarios
# three scenarios at a COMMON clinical base case; only the cost stack differs 
# (clinical params held at make_params() so the DALY/cases columns are shared;
#  each scenario's representative cost multipliers are the midpoints of its ranges)
opt_dist <- run_subnational_region(make_params(), data_all, use_planning = TRUE, use_programme = FALSE,
                          plan_mult = 0.80)                                            # + planning only (0.8x); no programme cost, IVM donated
real_dist <- run_subnational_region(make_params(), data_all, use_planning = TRUE,  use_programme = TRUE,
                          plan_mult = 1.00, prog_mult = 0.75)                                    # + planning + programme (0.75x)
pess_dist <- run_subnational_region(make_params(list(ivm_cost_400 = 0.225)), data_all, use_planning = TRUE, use_programme = TRUE,
                          plan_mult = 1.25, prog_mult = 1.25)                                    # + planning + programme (1.25x) + IVM cost

# ONE WIDE TABLE WITH THREE SCENARIOS
# Clinical metrics (cases_averted, dalys_gained, net_dalys, total_savings) are
# identical across scenarios — the extra costs never touch DALYs or savings — so
# they are shown ONCE. Only net_cost and ICER differ, so those get a column per
# scenario. Uses baseline parameter values e.g IDE.
subnational_region_scenarios <- opt_dist %>%         #From optimistic keeps clinical outcomes (base parameter values)
    transmute(country, subnational_region,
              cases_averted, dalys_gained, net_dalys, total_savings,
              net_cost_optimistic = net_cost,
              icer_optimistic     = icer) %>%
    mutate(net_cost_realistic   = real_dist$net_cost,
           icer_realistic       = real_dist$icer,          #renames outputs for each scenario to specify
           net_cost_pessimistic = pess_dist$net_cost,
           icer_pessimistic     = pess_dist$icer) 

# round for readability, for all numeric coloumns, round to 2 decimal places
num <- sapply(subnational_region_scenarios, is.numeric) 
subnational_region_scenarios[num] <- lapply(subnational_region_scenarios[num], function(x) round(x, 2))

# Compile values and export to csv
write.csv(subnational_region_scenarios, file.path(out_dir, "subnational_region_scenario_comparison_costsonly.csv"), row.names = FALSE)
cat("Wrote", file.path(out_dir, "subnational_region_scenario_comparison_costsonly.csv"),
    "with", nrow(subnational_region_scenarios), "subnational regions.\n")
print(head(subnational_region_scenarios, 8))


## 4. PER-REGION NMB DISTRIBUTIONS BY SCENARIO  (sampling SA -> boxplots, one graph per country)
# For every subnational region we sample each scenario's parameter ranges,
# run the model per draw, and keep the WHOLE distribution of NMB. Distribution of possible outcomes for every region. 

viol_dir <- file.path(out_dir, "nmb_boxplots_by_country")
if (!dir.exists(viol_dir)) dir.create(viol_dir, recursive = TRUE)

# each scenario's sampled parameter ranges (match 03/04/05; DALYs held fixed)
scenarios <- list(
    Optimistic = list(
        ranges = list(IDE = c(0.15, 0.25), IAEp = c(1.00, 1.13), ISAEp = c(1.00, 1.13),
                      cfr_SAE = c(0.02, 0.058),
                      plan_mult = c(0.60, 1.00)),      
        use_planning = TRUE, use_programme = FALSE),
    Realistic = list(
        ranges = list(IDE = c(0.00, 0.25), IAEp = c(1.10, 1.50), ISAEp = c(1.10, 1.50),
                      cfr_SAE = c(0.02, 0.08),
                      plan_mult = c(0.80, 1.20), prog_mult = c(0.00, 1.50)),
        use_planning = TRUE, use_programme = TRUE),
    Pessimistic = list(
        # efficacy capped at the central value (0.125), not the optimistic upper bound
        ranges = list(IDE = c(0.00, 0.125), IAEp = c(1.13, 2.00), ISAEp = c(1.13, 2.00),
                      cfr_SAE = c(0.02, 0.10),
                      plan_mult = c(1.00, 1.50), prog_mult = c(1.00, 1.50),
                      ivm_cost_400 = c(0.05, 0.40)),
        use_planning = TRUE, use_programme = TRUE)
)

## 4a. PROBABILISTIC DRAWS -> per-region NMB distribution
set.seed(2024)
n_sims <- 5000

# sample every parameter uniformly across its scenario range, run the model once
# per draw, return per-region net_cost / net_dalys / NMB for all draws stacked long.
scenario_psa <- function(spec, label, n = n_sims) { 
    r <- spec$ranges #Stores uncertainty ranges for that scenario
    # sample each parameter across its range; params a scenario omits stay at baseline
    smp <- function(nm, base) if (is.null(r[[nm]])) rep(base, n) else runif(n, r[[nm]][1], r[[nm]][2]) #If scenario does not define range for parameter, use baseline (e.g. IVM cost for pess and real scenarios)
    #if scenario does define range ^ sample randomly from a uniform distribution
    draws <- data.frame(
        IDE          = smp("IDE", 0.125), #if range exists randomly sample it otherwise use x (baseline)
        IAEp         = smp("IAEp", 1),
        ISAEp        = smp("ISAEp", 1),
        cfr_SAE      = smp("cfr_SAE", 0.058),
        plan_mult    = smp("plan_mult", 1),
        prog_mult    = smp("prog_mult", 1),
        ivm_cost_400 = smp("ivm_cost_400", 0)
    )
    # run the model once per draw, then tag the per-region output with scenario + draw
    do.call(rbind, lapply(seq_len(n), function(i) { #loops the model for every draw (1000)
        ov  <- as.list(draws[i, c("IDE", "IAEp", "ISAEp", "cfr_SAE", "ivm_cost_400")]) #extracts parameters from that draw
        out <- subnational_region_nmb(make_params(ov), data_all, #overwrites baseline parms in make_params and runs NMB model for every region
                                      spec$use_planning, spec$use_programme,
                                      draws$plan_mult[i], draws$prog_mult[i])
        out$scenario <- label; out$draw <- i #add scenario and draw identifiers
        out
    }))
}

# region-level draws in RAW USD, generates PSA results for every scenario
region_draws_raw <- do.call(rbind, lapply(names(scenarios), function(s) scenario_psa(scenarios[[s]], s))) #Stack all results in one big table, each row is one region in one PSA simulation
region_draws <- region_draws_raw %>%
    mutate(scenario = factor(scenario, levels = c("Optimistic", "Realistic", "Pessimistic"))) #Set scenario order


# PROGRAMME-TOTAL draws (sum all regions per draw), groups by scenario and simulation number
programme_draws <- region_draws %>%
    group_by(scenario, draw) %>%
    summarise(net_cost = sum(net_cost), net_dalys = sum(net_dalys), .groups = "drop") %>% 
    mutate(nmb = wtp * net_dalys - net_cost) #Calculate NMB
write.csv(programme_draws, file.path(out_dir, "psa_programme_draws.csv"), row.names = FALSE)
cat("Wrote", file.path(out_dir, "psa_programme_draws.csv"), "- programme-total PSA draws (feeds 06).\n")


## 4a-i. PSA SUMMARY TABLE  (the numbers quoted in the Results)
# Collapses the 5,000 draws per scenario into one row each. Every row of
# programme_draws is ONE draw with all 202 regions already summed, so these are
# medians ACROSS DRAWS of the programme-wide total, not medians across regions.

psa_summary <- programme_draws %>%
    group_by(scenario) %>%
    summarise(
        draws            = n(),
        net_cost_median  = median(net_cost) / 1e6,
        net_cost_lo      = quantile(net_cost, 0.025) / 1e6,
        net_cost_hi      = quantile(net_cost, 0.975) / 1e6,
        net_dalys_median = median(net_dalys) / 1e3,
        net_dalys_lo     = quantile(net_dalys, 0.025) / 1e3,
        net_dalys_hi     = quantile(net_dalys, 0.975) / 1e3,
        nmb_median       = median(nmb) / 1e6,
        nmb_lo           = quantile(nmb, 0.025) / 1e6,
        nmb_hi           = quantile(nmb, 0.975) / 1e6,
        p_cost_effective = mean(nmb > 0) * 100,                     # NMB > 0 at the WTP used above
        p_cost_saving    = mean(net_cost < 0) * 100,                # escalation saves money
        p_dominant       = mean(net_cost < 0 & net_dalys > 0) * 100,# cost-saving AND health-improving
        .groups = "drop"
    ) %>%
    mutate(scenario = factor(scenario, levels = c("Optimistic", "Realistic", "Pessimistic"))) %>%
    arrange(scenario) %>%
    mutate(across(where(is.numeric), ~ round(.x, 1)))

print(as.data.frame(psa_summary))
write.csv(psa_summary, file.path(out_dir, "psa_scenario_summary.csv"), row.names = FALSE)

# "median (2.5th to 97.5th)" strings
psa_reporting <- psa_summary %>%
    transmute(
        Scenario                = scenario,
        `Net cost (US$m)`       = sprintf("%.1f (%.1f to %.1f)", net_cost_median, net_cost_lo, net_cost_hi),
        `Net DALYs (thousands)` = sprintf("%.1f (%.1f to %.1f)", net_dalys_median, net_dalys_lo, net_dalys_hi),
        `NMB (US$m)`            = sprintf("%.1f (%.1f to %.1f)", nmb_median, nmb_lo, nmb_hi),
        `Cost-effective (%)`    = sprintf("%.1f", p_cost_effective),
        `Dominant (%)`          = sprintf("%.1f", p_dominant)
    )
print(as.data.frame(psa_reporting))
write.csv(psa_reporting, file.path(out_dir, "psa_scenario_summary_formatted.csv"), row.names = FALSE)

# convert per-region NMB in US$ millions for the plots and region/country summaries
nmb_draws <- region_draws %>% mutate(nmb = nmb / 1e6)        # US$ millions

# order regions by their realistic MEDIAN NMB
ord <- nmb_draws %>% filter(scenario == "Realistic") %>%
    group_by(country, subnational_region) %>%
    summarise(ord = median(nmb), .groups = "drop") #group data (diff simulations) by region and calculate NMB for each region
nmb_draws <- left_join(nmb_draws, ord, by = c("country", "subnational_region")) #adds realistic MEDIAN NMB to every row

#all scenarios for that region have same ordering value ^

# Summary statistics: per-region summary (median + 95% CrI), groups results by country, subnational region and scenario
nmb_summary <- nmb_draws %>%
    group_by(country, subnational_region, scenario) %>%
    # Gather media, lower bound and upper bound of 95% interval
    summarise(nmb_median = median(nmb),
              nmb_lo      = quantile(nmb, 0.025),
              nmb_hi      = quantile(nmb, 0.975), .groups = "drop")
#Export into table
write.csv(nmb_summary, file.path(out_dir, "nmb_subnational_region_scenarios.csv"), row.names = FALSE)


## 4a-iii. REGION COUNTS BY SCENARIO  (the heterogeneity numbers quoted in the Results)
# How many of the 202 regions are net-beneficial, and in how many is that
# conclusion certain (the 95% interval excludes zero) rather than marginal.

region_counts <- nmb_summary %>%
    group_by(scenario) %>%
    summarise(
        regions          = n(),
        net_beneficial   = sum(nmb_median > 0),
        net_cost         = sum(nmb_median <= 0),
        pct_beneficial   = round(mean(nmb_median > 0) * 100, 1),
        certain_positive = sum(nmb_lo > 0),   # whole 95% interval above zero
        certain_negative = sum(nmb_hi < 0),   # whole 95% interval below zero
        uncertain        = sum(nmb_lo <= 0 & nmb_hi >= 0),
        .groups = "drop"
    ) %>%
    mutate(scenario = factor(scenario, levels = c("Optimistic", "Realistic", "Pessimistic"))) %>%
    arrange(scenario)

print(as.data.frame(region_counts))
write.csv(region_counts, file.path(out_dir, "region_counts_by_scenario.csv"), row.names = FALSE)

# Country-level: is the whole country beneficial, mixed, or nowhere beneficial?
country_mix <- nmb_summary %>%
    group_by(scenario, country) %>%
    summarise(any_pos = any(nmb_median > 0),
              all_pos = all(nmb_median > 0), .groups = "drop") %>%
    group_by(scenario) %>%
    summarise(countries          = n(),
              all_beneficial     = sum(all_pos),
              mixed              = sum(any_pos & !all_pos),
              none_beneficial    = sum(!any_pos),
              .groups = "drop") %>%
    mutate(scenario = factor(scenario, levels = c("Optimistic", "Realistic", "Pessimistic"))) %>%
    arrange(scenario)

print(as.data.frame(country_mix))
write.csv(country_mix, file.path(out_dir, "country_mix_by_scenario.csv"), row.names = FALSE)


## 4a-ii. COUNTRY-LEVEL NMB SUMMARY 
# For each draw, sums region NMB within a country -> country NMB per draw; then
# summarise across draws. Values in US$ millions (nmb was rescaled above).
nmb_country_summary <- nmb_draws %>%
    group_by(country, scenario, draw) %>% #groups data by country, scenario, PSA simulation number
    summarise(nmb = sum(nmb), .groups = "drop") %>%          # country total per draw, adds regional NMB values together
    group_by(country, scenario) %>% #group all simulations together (for that scenario)
    summarise(mean_nmb   = mean(nmb), #(low, mid, upper end of quartiles)
              lo_nmb     = quantile(nmb, 0.025),
              median_nmb = median(nmb),
              hi_nmb     = quantile(nmb, 0.975), .groups = "drop") %>%
    arrange(country, scenario) #Sort table by country and scenario
#Export table to CSV
write.csv(nmb_country_summary, file.path(out_dir, "nmb_country_scenario_summary.csv"), row.names = FALSE)

print(head(nmb_country_summary, 9))

#Colour mapping for each plot
pal_sc <- c(Optimistic = "#00A087FF", Realistic = "#3C5488FF", Pessimistic = "#E64B35FF") 

## 4b. BOXPLOTS — one PNG per country (full NMB distribution per region & scenario)
make_country_boxplot <- function(df, country_name) {
    ggplot(df, aes(x = reorder(subnational_region, ord), y = nmb, fill = scenario, colour = scenario)) + #regionas are ordered from lowest to highest realistic NMB
        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") + #adds horizontal dashed line at 0 NMB
        geom_boxplot(position = position_dodge(width = 0.8), width = 0.7,
                     alpha = 0.45, linewidth = 0.3,
                     outlier.size = 0.15, outlier.alpha = 0.25) +   # box = IQR (25th-75th), line = median, whiskers = 1.5x IQR
        coord_flip() +
        scale_fill_manual(values = pal_sc) + scale_colour_manual(values = pal_sc) +
        labs(x = "Subnational region", y = "Net monetary benefit (US$ millions)",
             fill = "Scenario", colour = "Scenario") +
        theme_bw() +
        theme(plot.title = element_text(face = "bold"), axis.text.y = element_text(size = 8),
              legend.position = "top")
}

countries <- sort(unique(nmb_draws$country))
for (cn in countries) {
    d  <- filter(nmb_draws, country == cn)
    nd <- length(unique(d$subnational_region))
    p  <- make_country_boxplot(d, cn)
    fname <- gsub("[^A-Za-z0-9]+", "_", trimws(cn))
    h <- max(3.5, 0.7 * nd + 1.6)
    ggsave(file.path(viol_dir, paste0(fname, ".png")), p, width = 9, height = h, dpi = 300)
}
cat("Saved", length(countries), "per-country boxplot figures to", viol_dir, "\n")


