#### 10 - IDE RESPONSE CURVE  (to show IDE at 0)
# ------------------------------------------------------------
# For each scenario we sweep IDE from 0 to 0.30 with all OTHER parameters
#  held at that scenario's central (midpoint) values + its cost stack, and
#  read off:
#    * where the NMB curve starts at IDE = 0 (negative = net-harmful), and
#    * the BREAK-EVEN IDE where NMB crosses 0 (the efficacy the escalation
#      needs before it is worth it).
# Under optimistic, realistic, and pessimistic assumptions,
# how effective does the higher dose need to be before the intervention has positive net monetary benefit?

# Paths are relative to the project root. Open business-case-ivermectin.Rproj
# so the working directory is set correctly.
if (!file.exists("data2.xlsx"))
    stop("Working directory is not the project root. Open ",
         "business-case-ivermectin.Rproj, or setwd() to the folder ",
         "containing data2.xlsx.")

#source model
source("R/00_model_setup.R")   

# set willingness to pay threshold
wtp <- 500

## 1. Load data
data_all <- data   

## 2. PROGRAMME-TOTAL OUTCOMES FOR ONE PARAMETER SET
run_agg <- function(p, data, use_planning = FALSE, use_programme = FALSE,
                    plan_mult = 1, prog_mult = 1) {
    epi  <- f_epi(data, p)
    ae   <- f_AE(data, p)
    sae  <- f_SAE(data, p)
    plan_c <- if (use_planning)  plan_mult * data$planning_cost  else 0
    prog_c <- if (use_programme) prog_mult * data$programme_cost else 0
    cost <- f_cost(epi, ae, sae, data, p, planning_cost = plan_c, programme_cost = prog_c)
    nd <- sum(cost$net_dalys, na.rm = TRUE) #aggregate across all districts
    nc <- sum(cost$net_cost,  na.rm = TRUE)
    c(net_dalys = nd, net_cost = nc, nmb = wtp * nd - nc)
}

## 3. SCENARIO CENTRAL PARAMETERS (all params at midpoint, IDE swept below)
scen <- list(
    Optimistic  = list(ov = list(IAEp = 1.065, ISAEp = 1.065, cfr_SAE = 0.039),
                       use_planning = TRUE,  use_programme = FALSE, plan_mult = 0.80, prog_mult = 1),
    Realistic   = list(ov = list(IAEp = 1.30,  ISAEp = 1.30,  cfr_SAE = 0.05),
                       use_planning = TRUE,  use_programme = TRUE,  plan_mult = 1.00, prog_mult = 0.75),
    Pessimistic = list(ov = list(IAEp = 1.565, ISAEp = 1.565, cfr_SAE = 0.06,
                                 ivm_cost_400 = 0.225),
                       use_planning = TRUE,  use_programme = TRUE,  plan_mult = 1.25, prog_mult = 1.25)
)

## 4. SWEEP IDE FROM 0 TO 0.30 FOR EACH SCENARIO
ide_grid <- seq(0, 0.30, by = 0.002)   # fine near 0 so the break-even is captured

#Run the model for every scenario x IDE value 
curve_df <- do.call(rbind, lapply(names(scen), function(s) {
    cfg <- scen[[s]]
    do.call(rbind, lapply(ide_grid, function(ide) {
        o <- run_agg(make_params(c(list(IDE = ide), cfg$ov)), data_all,
                     cfg$use_planning, cfg$use_programme, cfg$plan_mult, cfg$prog_mult)
        data.frame(scenario = s, IDE = ide,
                   net_dalys = unname(o["net_dalys"]),
                   net_cost  = unname(o["net_cost"]),
                   nmb       = unname(o["nmb"]))
    }))
}))
curve_df$scenario <- factor(curve_df$scenario, levels = names(scen))

# break-even IDE where NMB crosses 0 (NMB rises monotonically with IDE)
breakeven <- curve_df %>% group_by(scenario) %>% #looks at each scenario individually
    summarise(be_ide = if (all(nmb > 0)) 0 else if (all(nmb < 0)) NA_real_ #and finds where NMB =0, to get value of IDE (break even)
                       else approx(nmb, IDE, xout = 0)$y, .groups = "drop")

## 4b. CASES-AVERTED SCALE
# Cases averted = total_cases x IDE x coverage_effectiveness. Coverage does not
# change with IDE, so the relationship is exactly linear: cases = IDE x K, where
# K is obtained by running the epidemiological function at IDE = 1.
# NOTE: K depends on the OBSERVED coverage distribution, not on the disease alone.
# If coverage rose, the same IDE would avert more cases.
epi_k    <- f_epi(data_all, make_params(list(IDE = 1)))
K_CASES  <- sum(epi_k$cases_averted, na.rm = TRUE)
TOT_CASE <- sum(data_all$total_cases, na.rm = TRUE)

breakeven$be_cases     <- breakeven$be_ide * K_CASES
breakeven$be_pct_cases <- 100 * breakeven$be_cases / TOT_CASE

cat("\n===== BREAK-EVEN (NMB = 0) =====\n")
cat(sprintf("Total annual cases: %s | cases averted per unit IDE (K): %s\n\n",
            format(round(TOT_CASE), big.mark = ","), format(round(K_CASES), big.mark = ",")))
print(as.data.frame(breakeven), row.names = FALSE)
write.csv(breakeven, file.path(out_dir, "ide_breakeven.csv"), row.names = FALSE)

# Label helper: a break-even below the grid resolution should not be printed as a
# flat "0" - at IDE = 0 there is no benefit but planning and adverse-event costs
# are still incurred, so NMB is strictly negative and the true break-even is a
# small positive number.
fmt_be <- function(x) ifelse(is.na(x), "not reached",
                      ifelse(x < 0.001, "< 0.001", format(round(x, 4), nsmall = 4)))

pal <- c(Optimistic = "#00A087FF", Realistic = "#3C5488FF", Pessimistic = "#E64B35FF")

## 5. NMB RESPONSE CURVE 
p_nmb <- ggplot(curve_df, aes(IDE, nmb / 1e9, colour = scenario)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 1.2) +
    geom_vline(data = breakeven, aes(xintercept = be_ide, colour = scenario),
               linetype = "dotted", linewidth = 0.6, show.legend = FALSE) +
    geom_point(data = breakeven, aes(x = be_ide, y = 0, colour = scenario),
               size = 3, show.legend = FALSE) +
    scale_colour_manual(values = pal) +
    scale_x_continuous(
        sec.axis = sec_axis(~ . * K_CASES / 1e6,
                            name = "Malaria cases averted (millions, programme-wide)")) +
    labs(x = "IDE  (extra malaria reduction from the 400 ug dose)",
         y = "Net monetary benefit (US$ billions)", colour = "Scenario") +
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), legend.position = "top")
print(p_nmb)
#Save plot
ggsave(file.path(out_dir, "ide_response_nmb.png"), p_nmb, width = 9, height = 6, dpi = 300)

# 5b. ZOOM to the break-even region (where IDE = 0 actually lives)
# Labels give both the efficacy and the number of cases it corresponds to.
be_lab <- breakeven %>%
    mutate(lab = paste0(scenario, ": IDE ", fmt_be(be_ide),
                        "\n(", format(round(be_cases), big.mark = ",", trim = TRUE),
                        " cases averted)"))
p_nmb_zoom <- ggplot(curve_df, aes(IDE, nmb / 1e9, colour = scenario)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 1.2) +
    geom_point(data = breakeven, aes(x = be_ide, y = 0, colour = scenario), size = 3, show.legend = FALSE) +
    ggrepel::geom_label_repel(
        data = be_lab, aes(x = be_ide, y = 0, label = lab, colour = scenario),
        size = 3, lineheight = 0.95, show.legend = FALSE,
        fill = "white", alpha = 0.92, label.size = 0.2,
        box.padding = 0.7, point.padding = 0.35,
        nudge_y = -0.055, direction = "both", force = 4,
        segment.colour = "grey50", segment.size = 0.3,
        min.segment.length = 0, seed = 1) +
    scale_colour_manual(values = pal) +
    scale_x_continuous(
        sec.axis = sec_axis(~ . * K_CASES / 1e6,
                            name = "Malaria cases averted (millions, programme-wide)")) +
    coord_cartesian(xlim = c(0, 0.04), ylim = c(-0.12, 0.35)) +
    labs(x = "IDE  (extra malaria reduction from the 400 ug dose)",
         y = "Net monetary benefit (US$ billions)", colour = "Scenario") +
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), legend.position = "top")
print(p_nmb_zoom)
#Save plot
ggsave(file.path(out_dir, "ide_response_nmb_zoom.png"), p_nmb_zoom, width = 9, height = 6, dpi = 300)

## 6. NET DALYs RESPONSE CURVE  (shows the health side)
p_nd <- ggplot(curve_df, aes(IDE, net_dalys / 1e6, colour = scenario)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 1.2) +
    scale_colour_manual(values = pal) +
    labs(title = "Net DALYs averted vs dose-escalation efficacy (IDE)",
         subtitle = "At IDE = 0 the higher dose averts no malaria, so net DALYs start slightly NEGATIVE (harm only).",
         x = "IDE  (extra malaria reduction from the 400 ug dose)",
         y = "Net DALYs averted (millions)", colour = "Scenario") +
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), legend.position = "top")
print(p_nd)
#Save plot
ggsave(file.path(out_dir, "ide_response_netdalys.png"), p_nd, width = 9, height = 6, dpi = 300)

cat("\nSaved ide_response_nmb.png and ide_response_netdalys.png to", out_dir, "\n")
