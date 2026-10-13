#### 04 — PESSIMISTIC SCENARIO (sensitivity + PSA)
# ------------------------------------------------------------
#  Worst-case scenario. On top of the model it adds:
#    (1) subnational region planning cost   (Sheet3, same as realistic)
#    (2) subnational region programme cost   (data2.xlsx Sheet1 col 19 "Program Cost")
#    (3) incremental IVM drug cost (higher 400 ug dose no longer donated):
#        base $0.225/person, varied 0.05 / 0.225 / 0.40.

# Paths are relative to the project root. Open business-case-ivermectin.Rproj
# so the working directory is set correctly.
if (!file.exists("data2.xlsx"))
    stop("Working directory is not the project root. Open ",
         "business-case-ivermectin.Rproj, or setwd() to the folder ",
         "containing data2.xlsx.")

#Source the model
source("R/00_model_setup.R")   

## 1. COSTS ATTACHED TO EACH SUBNATIONAL REGION
# (a) planning cost (Sheet3, country total / n subnational regions) — as in realistic
data_p <- data   # planning_cost (Sheet3) + programme_cost already attached in 00_model_setup.R
stopifnot(sum(is.na(data_p$planning_cost)) == 0,
          sum(is.na(data_p$programme_cost)) == 0)

cat("Planning cost total (USD):  ", round(sum(data_p$planning_cost), 0), "\n")
cat("Programme cost total (USD): ", round(sum(data_p$programme_cost), 0), "\n")

## 2. PESSIMISTIC RUN WRAPPER
# Adds planning + programme + incremental IVM cost.
# plan_mult / prog_mult scale the planning / programme cost (default 1);
# ivm incremental cost = p$ivm_cost_400 (ivm_cost_200 stays 0).
run_model_pess <- function(p, data) {
    pm  <- if (!is.null(p$plan_mult)) p$plan_mult else 1 #These will change in SA
    pgm <- if (!is.null(p$prog_mult)) p$prog_mult else 1
    epi  <- f_epi(data, p)
    ae   <- f_AE(data, p)
    sae  <- f_SAE(data, p)
    cost <- f_cost(epi, ae, sae, data, p,
                   planning_cost  = pm  * data$planning_cost,          # Sheet3 planning meeting cost
                   programme_cost = pgm * data$programme_cost)         # transition/programme cost

    tot <- c(
        cases_averted = sum(epi$cases_averted,  na.rm = TRUE),
        dalys_gained  = sum(epi$dalys_gained,   na.rm = TRUE),
        net_cost      = sum(cost$net_cost,      na.rm = TRUE),
        net_dalys     = sum(cost$net_dalys,     na.rm = TRUE),
        cost_AEs      = sum(cost$cost_AEs,      na.rm = TRUE),
        cost_SAEs     = sum(cost$cost_SAEs,     na.rm = TRUE),
        cost_IVM      = sum(cost$cost_IVM,      na.rm = TRUE),
        planning_cost = sum(pm  * data$planning_cost,  na.rm = TRUE),
        programme_cost= sum(pgm * data$programme_cost, na.rm = TRUE),
        total_savings = sum(cost$total_savings, na.rm = TRUE)
    )
    icer <- if (tot["net_dalys"] != 0) unname(tot["net_cost"] / tot["net_dalys"]) else NA_real_
    nmb  <- unname(wtp * tot["net_dalys"] - tot["net_cost"])   # net monetary benefit
    c(tot, icer = icer, nmb = nmb)
}

## 3. BASE CASE (pessimistic)  — IVM incremental cost at base $0.225. To see the change from base case.
base_p <- run_model_pess(make_params(list(ivm_cost_400 = 0.225)), data_p) #Run pessimistic model adding the drug cost
cat("\n===== PESSIMISTIC BASE CASE =====\n")
print(round(base_p, 2))
cat("Baseline net_cost (no add-ons): ", round(unname(run_model(make_params(), data)["net_cost"]), 0), "\n")
cat("Pessimistic net_cost          : ", round(unname(base_p["net_cost"]), 0), "\n")
cat("Pessimistic ICER (USD/DALY)   : ", round(unname(base_p["icer"]), 2), "\n")


## 4. PROBABILISTIC SENSITIVITY ANALYSIS  (pessimistic)
set.seed(2024)
n_sims <- 5000

# PESSIMISTIC (worst-case): poor efficacy, high harms, full cost stack.
# The efficacy/harm ranges are the WIDEST of the three scenarios — IDE reaches 0,
# AE relative risks go up to 2.0, and the DALY weights take the UPPER half of
# their ranges. Planning and programme costs run from base up to +50%.

draws_p <- data.frame(
    IDE          = runif(n_sims, 0.00, 0.125),   # pessimistic: efficacy capped at the central value
    IAEp         = runif(n_sims, 1.13, 2.00),
    ISAEp        = runif(n_sims, 1.13, 2.00),
    cfr_SAE      = runif(n_sims, 0.02, 0.10)
    # DALY weights held FIXED at baseline (not sampled) — reference low dose has no DALY variance
)
# scenario-specific COST draws (only affect net_cost)
draws_p$plan_mult    <- runif(n_sims, 1.00, 1.50)   # planning cost: base to +50%
draws_p$prog_mult    <- runif(n_sims, 1.00, 1.50)   # programme cost: base to +50%
draws_p$ivm_cost_400 <- runif(n_sims, 0.05, 0.40)   # incremental IVM drug cost

psa_p <- do.call(rbind, lapply(seq_len(n_sims), function(i) {
    out <- run_model_pess(make_params(as.list(draws_p[i, ])), data_p)
    data.frame(sim = i, draws_p[i, ],
               net_cost = unname(out["net_cost"]),
               net_dalys = unname(out["net_dalys"]),
               icer = unname(out["icer"]))
}))

wtp <- 500
psa_p <- psa_p %>%
    mutate(nmb = wtp * net_dalys - net_cost,
           cost_effective = nmb > 0,
           dominant = net_cost < 0 & net_dalys > 0)

cat("\n===== PESSIMISTIC PSA SUMMARY (", n_sims, "draws) =====\n", sep = "")
cat("Median ICER (USD/DALY): ", round(median(psa_p$icer, na.rm = TRUE), 1), "\n", sep = "")
cat("Net DALYs 95% CrI: ", paste(round(quantile(psa_p$net_dalys, c(.025,.975)), 0), collapse = " to "), "\n")
cat("Net cost  95% CrI: ", paste(round(quantile(psa_p$net_cost,  c(.025,.975)), 0), collapse = " to "), "\n")
cat(sprintf("P(cost-effective at $%d/DALY)          : %.1f%%\n", wtp, 100 * mean(psa_p$cost_effective)))
cat(sprintf("P(dominant: cost-saving AND DALY-gain) : %.1f%%\n", 100 * mean(psa_p$dominant)))
write.csv(psa_p, file.path(out_dir, "pessimistic_psa_draws.csv"), row.names = FALSE)

# Cost-effectiveness plane (each dot = one simulation)
ce_plane_p <- ggplot(psa_p, aes(x = net_dalys, y = net_cost)) +
    geom_hline(yintercept = 0, colour = "grey70") + geom_vline(xintercept = 0, colour = "grey70") +
    geom_point(aes(colour = cost_effective), alpha = 0.35, size = 1.2) +
    geom_abline(slope = wtp, intercept = 0, linetype = "dashed") +
    scale_colour_manual(values = c(`TRUE` = "#3C5488FF", `FALSE` = "#E64B35FF"), name = "Cost-effective") +
    labs(title = "Pessimistic scenario — PSA cost-effectiveness plane",
         subtitle = sprintf("Planning + programme + IVM cost; %.0f%% of simulations cost-effective",
                            100 * mean(psa_p$cost_effective)),
         x = "Incremental DALYs averted", y = "Incremental net cost (USD)") + theme_bw()
print(ce_plane_p)
ggsave(file.path(out_dir, "pessimistic_psa_ce_plane.png"), ce_plane_p, width = 8, height = 6, dpi = 300)

# Cost-effectiveness acceptability curve (CEAC) 
thresholds <- seq(0, 2000, by = 25)
ceac_p <- data.frame(wtp = thresholds,
    prob_ce = sapply(thresholds, function(l) mean(l * psa_p$net_dalys - psa_p$net_cost > 0, na.rm = TRUE)))
ceac_p_plot <- ggplot(ceac_p, aes(x = wtp, y = prob_ce)) +
    geom_line(linewidth = 1.1, colour = "#3C5488FF") +
    geom_vline(xintercept = wtp, linetype = "dashed", colour = "grey50") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(title = "Pessimistic scenario — CEAC",
         subtitle = "Share of simulations cost-effective at each WTP threshold",
         x = "Willingness-to-pay (USD per DALY averted)", y = "P(cost-effective)") + theme_bw()
print(ceac_p_plot)
ggsave(file.path(out_dir, "pessimistic_psa_ceac.png"), ceac_p_plot, width = 8, height = 5, dpi = 300)

# ICER distribution
icer_med_p   <- median(psa_p$icer, na.rm = TRUE)
icer_xlim_p  <- quantile(psa_p$icer, c(0.025, 0.975), na.rm = TRUE)
icer_pctout_p<- 100 * mean(psa_p$icer < icer_xlim_p[1] | psa_p$icer > icer_xlim_p[2], na.rm = TRUE)
icer_p_hist <- ggplot(subset(psa_p, icer >= icer_xlim_p[1] & icer <= icer_xlim_p[2]), aes(x = icer)) +
    geom_histogram(bins = 60, fill = "#4DBBD5FF", colour = "white") +
    geom_vline(xintercept = icer_med_p, linetype = "dashed") +
    labs(title = "Pessimistic scenario — PSA ICER distribution",
         subtitle = sprintf("Dashed line = median ICER ($%s/DALY); axis clipped to central 95%% of draws (%.1f%% fall outside)",
                            format(round(icer_med_p), big.mark = ","), icer_pctout_p),
         x = "ICER (USD per DALY)", y = "Simulations") + theme_bw()
print(icer_p_hist)
ggsave(file.path(out_dir, "pessimistic_psa_icer_distribution.png"), icer_p_hist, width = 8, height = 5, dpi = 300)
