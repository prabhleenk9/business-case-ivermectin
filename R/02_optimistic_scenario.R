
#### 02 - OPTIMISTIC SCENARIO ( sensitivity analysis + PSA)
# ------------------------------------------------------------
#  The optimistic scenario: Uses the optimistic parameter
#  ranges. 

# Paths are relative to the project root. Open business-case-ivermectin.Rproj
# so the working directory is set correctly.
if (!file.exists("data2.xlsx"))
    stop("Working directory is not the project root. Open ",
         "business-case-ivermectin.Rproj, or setwd() to the folder ",
         "containing data2.xlsx.")

# Source the model 
source("R/00_model_setup.R")

## 0. OPTIMISTIC RUN WRAPPER  (planning cost ONLY; no programme cost, IVM donated)
# plan_mult scales the per-region planning cost (Sheet3). The optimistic scenario
# carries planning cost at 0.6-1.0x, i.e. delivery is assumed to be at least as
# efficient as costed. Programme cost and incremental drug cost remain zero.
PLAN_MULT_O <- 0.80   # optimistic central planning-cost multiplier (midpoint of 0.6-1.0)
run_model_o <- function(p, data) {
    pm   <- if (!is.null(p$plan_mult)) p$plan_mult else PLAN_MULT_O
    epi  <- f_epi(data, p)
    ae   <- f_AE(data, p)
    sae  <- f_SAE(data, p)
    cost <- f_cost(epi, ae, sae, data, p, planning_cost = pm * data$planning_cost)

    tot <- c(
        cases_averted = sum(epi$cases_averted,  na.rm = TRUE),
        dalys_gained  = sum(epi$dalys_gained,   na.rm = TRUE),
        net_cost      = sum(cost$net_cost,      na.rm = TRUE),
        net_dalys     = sum(cost$net_dalys,     na.rm = TRUE),
        cost_AEs      = sum(cost$cost_AEs,      na.rm = TRUE),
        cost_SAEs     = sum(cost$cost_SAEs,     na.rm = TRUE),
        planning_cost = sum(pm * data$planning_cost, na.rm = TRUE),
        total_savings = sum(cost$total_savings, na.rm = TRUE)
    )
    icer <- if (tot["net_dalys"] != 0) unname(tot["net_cost"] / tot["net_dalys"]) else NA_real_
    nmb  <- unname(wtp * tot["net_dalys"] - tot["net_cost"])
    c(tot, icer = icer, nmb = nmb)
}


## 1. BASE CASE 
base_results <- run_model_o(make_params(), data)
base_results


## 2. PROBABILISTIC SENSITIVITY ANALYSIS (PSA) — OPTIMISTIC
# Samples the optimistic parameter ranges, runs the full model once per draw,
# and summarises how often the escalation is cost-effective across WTP thresholds.
set.seed(2024)          # reproducible draws
n_sims <- 5000          # each draw runs the full 202-subnational region model

# parameter distributions: optimistic (best-case) bundle
# Good efficacy, low harms, planning cost only (IVM donated). IDE floored at 0.15;
# harm parameters capped at (or below) their point estimates.
draws <- data.frame(
    IDE          = runif(n_sims, 0.15, 0.25),          # dose-escalation efficacy
    IAEp         = runif(n_sims, 1.00, 1.13),          # mild/moderate AE relative risk (<= point estimate)
    ISAEp        = runif(n_sims, 1.00, 1.13),          # severe AE relative risk (<= point estimate)
    cfr_SAE      = runif(n_sims, 0.02, 0.058),         # SAE case-fatality ratio (<= point estimate)
    plan_mult    = runif(n_sims, 0.60, 1.00)           # planning cost 0.6x to full
)

# run the model once per draw 
psa_o <- do.call(rbind, lapply(seq_len(n_sims), function(i) {
    p   <- make_params(as.list(draws[i, ]))
    out <- run_model_o(p, data)
    data.frame(sim = i, draws[i, ],
               net_cost  = unname(out["net_cost"]),
               net_dalys = unname(out["net_dalys"]),
               icer      = unname(out["icer"]))
}))

# cost-effectiveness decision (Net Monetary Benefit) 
# NMB = wtp * DALYs_averted - net_cost.  NMB > 0 => cost-effective.
wtp <- 500     # willingness-to-pay, USD per DALY averted
psa_o <- psa_o %>%
    mutate(nmb            = wtp * net_dalys - net_cost,
           cost_effective = nmb > 0,
           dominant       = net_cost < 0 & net_dalys > 0)   # cheaper AND healthier

# headline results 
cat("\n===== OPTIMISTIC PSA SUMMARY (", n_sims, "draws ) =====\n", sep = "")
cat("Median ICER (USD/DALY): ", round(median(psa_o$icer, na.rm = TRUE), 1), "\n", sep = "")
cat("95% credible interval : ",
    paste(round(quantile(psa_o$icer, c(.025, .975), na.rm = TRUE), 1), collapse = " to "), "\n", sep = "")
cat(sprintf("P(cost-effective at $%d/DALY)          : %.1f%%\n", wtp, 100 * mean(psa_o$cost_effective)))
cat(sprintf("P(dominant: cost-saving AND DALY-gain) : %.1f%%\n", 100 * mean(psa_o$dominant)))
write.csv(psa_o, file.path(out_dir, "optimistic_psa_draws.csv"), row.names = FALSE)

# PSA plot A: cost-effectiveness plane (each dot = one simulation) 
ce_plane_psa_o <- ggplot(psa_o, aes(x = net_dalys, y = net_cost)) +
    geom_hline(yintercept = 0, colour = "grey70") +
    geom_vline(xintercept = 0, colour = "grey70") +
    geom_point(aes(colour = cost_effective), alpha = 0.35, size = 1.2) +
    geom_abline(slope = wtp, intercept = 0, linetype = "dashed") +   # WTP line: cost = wtp * effect
    scale_colour_manual(values = c(`TRUE` = "#3C5488FF", `FALSE` = "#E64B35FF"), name = "Cost-effective") +
    labs(title = "PSA — Cost-effectiveness plane (Optimistic)",
         subtitle = sprintf("Dashed line = WTP $%d/DALY; %.0f%% of simulations cost-effective",
                            wtp, 100 * mean(psa_o$cost_effective)),
         x = "Incremental DALYs averted", y = "Incremental net cost (USD)") +
    theme_bw()
print(ce_plane_psa_o)
ggsave(file.path(out_dir, "optimisitc_psa_ce_plane.png"), ce_plane_psa_o, width = 8, height = 6, dpi = 300)

# PSA plot B: cost-effectiveness acceptability curve (CEAC) 
thresholds <- seq(0, 2000, by = 25)
ceac <- data.frame(
    wtp     = thresholds,
    prob_ce = sapply(thresholds, function(l) mean(l * psa_o$net_dalys - psa_o$net_cost > 0, na.rm = TRUE))
)
ceac_plot <- ggplot(ceac, aes(x = wtp, y = prob_ce)) +
    geom_line(linewidth = 1.1, colour = "#3C5488FF") +
    geom_vline(xintercept = wtp, linetype = "dashed", colour = "grey50") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(title = "Cost-effectiveness acceptability curve (CEAC) — Optimistic",
         subtitle = "Share of simulations that are cost-effective at each WTP threshold",
         x = "Willingness-to-pay (USD per DALY averted)", y = "P(cost-effective)") +
    theme_bw()
print(ceac_plot)
ggsave(file.path(out_dir, "optimistic_psa_cea.png"), ceac_plot, width = 8, height = 5, dpi = 300)

# PSA plot C: distribution of the ICER 
icer_hist <- ggplot(psa_o, aes(x = icer)) +
    geom_histogram(bins = 60, fill = "#4DBBD5FF", colour = "white") +
    geom_vline(xintercept = median(psa_o$icer, na.rm = TRUE), linetype = "dashed") +
    labs(title = "PSA — distribution of the ICER (Optimistic)",
         x = "ICER (USD per DALY)", y = "Simulations") +
    theme_bw()
print(icer_hist)
ggsave(file.path(out_dir, "optimistic_psa_icer_distribution.png"), icer_hist, width = 8, height = 5, dpi = 300)

