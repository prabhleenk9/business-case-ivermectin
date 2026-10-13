#### 03 - REALISTIC SCENARIO: Core part of the results (sensitivity + PSA)
# ------------------------------------------------------------
#  Adds a one-day subnational region planning-meeting cost to every subnational region 
#  plus a lump programme cost (with 0 as a range)

# Paths are relative to the project root. Open business-case-ivermectin.Rproj
# so the working directory is set correctly.
if (!file.exists("data2.xlsx"))
    stop("Working directory is not the project root. Open ",
         "business-case-ivermectin.Rproj, or setwd() to the folder ",
         "containing data2.xlsx.")

#Source the model
source("R/00_model_setup.R")   

## 1: SUBNATIONAL-REGION LEVEL PLANNING COST

#Read the data
data_r <- data   # planning_cost (Sheet3) + programme_cost already attached in 00_model_setup.R

stopifnot(sum(is.na(data_r$planning_cost)) == 0)  

## 2: REALISTIC RUN WRAPPER  (adds planning + programme cost)

# plan_mult scales the planning cost; prog_mult scales the programme cost
# (0 = no programme cost, 1 = full per-subnational region programme cost). Both default
# to 1 when not supplied.

run_model_r <- function(p, data) {
    pm  <- if (!is.null(p$plan_mult)) p$plan_mult else 1 #Used later in SA and adjusted
    pgm <- if (!is.null(p$prog_mult)) p$prog_mult else 1 

    epi  <- f_epi(data, p)
    ae   <- f_AE(data, p)
    sae  <- f_SAE(data, p)
    cost <- f_cost(epi, ae, sae, data, p,
                   planning_cost  = pm  * data$planning_cost,
                   programme_cost = pgm * data$programme_cost)

    tot <- c(
        cases_averted = sum(epi$cases_averted,  na.rm = TRUE),
        dalys_gained  = sum(epi$dalys_gained,   na.rm = TRUE),
        net_cost      = sum(cost$net_cost,      na.rm = TRUE),
        net_dalys     = sum(cost$net_dalys,     na.rm = TRUE),
        cost_AEs      = sum(cost$cost_AEs,      na.rm = TRUE),
        cost_SAEs     = sum(cost$cost_SAEs,     na.rm = TRUE),
        planning_cost = sum(pm  * data$planning_cost,  na.rm = TRUE),
        programme_cost= sum(pgm * data$programme_cost, na.rm = TRUE),
        total_savings = sum(cost$total_savings, na.rm = TRUE)
    )
    icer <- if (tot["net_dalys"] != 0) unname(tot["net_cost"] / tot["net_dalys"]) else NA_real_
    nmb  <- unname(wtp * tot["net_dalys"] - tot["net_cost"])   # net monetary benefit
    c(tot, icer = icer, nmb = nmb)
}

## 3: BASE CASE (realistic)  — realistic base case results
base_r <- run_model_r(make_params(list(plan_mult = 1.00, prog_mult = 0.75)), data_r)
cat("\n===== REALISTIC BASE CASE =====\n")
print(round(base_r, 2))
cat("\nBaseline net_cost (no add-ons)       : ", round(unname(run_model(make_params(), data)["net_cost"]), 0), "\n") #Compare to model with no add ons
cat("Realistic net_cost (+planning +prog) : ", round(unname(base_r["net_cost"]), 0), "\n")
cat("Realistic ICER (USD/DALY)      : ", round(unname(base_r["icer"]), 2), "\n")

## 4: PROBABILISTIC SENSITIVITY ANALYSIS  (realistic)
set.seed(2024)
n_sims <- 5000

# REALISTIC (base-case) mid-range efficacy/harm + planning cost.
# IDE floor is 0.00 (the higher dose may give no extra malaria benefit), and
# harm parameters span up to their point estimates. These ranges are wider than
# the optimistic scenario (03) and narrower than the pessimistic one (05)

draws_r <- data.frame(
    IDE          = runif(n_sims, 0.00, 0.25),
    IAEp         = runif(n_sims, 1.10, 1.50),
    ISAEp        = runif(n_sims, 1.10, 1.50),
    cfr_SAE      = runif(n_sims, 0.02, 0.08)
)
# scenario-specific COST draws (only affect net_cost) 
draws_r$plan_mult <- runif(n_sims, 0.80, 1.20)     # planning cost +/-20%
draws_r$prog_mult <- runif(n_sims, 0.00, 1.50)     # programme cost: 0 up to full +50%

psa_r <- do.call(rbind, lapply(seq_len(n_sims), function(i) {
    p   <- make_params(as.list(draws_r[i, ]))
    out <- run_model_r(p, data_r)
    data.frame(sim = i, draws_r[i, ],
               net_cost  = unname(out["net_cost"]),
               net_dalys = unname(out["net_dalys"]),
               icer      = unname(out["icer"]))
}))

wtp <- 500
psa_r <- psa_r %>%
    mutate(nmb            = wtp * net_dalys - net_cost,
           cost_effective = nmb > 0,
           dominant       = net_cost < 0 & net_dalys > 0)

cat("\n===== REALISTIC PSA SUMMARY (", n_sims, "draws) =====\n", sep = "")
cat("Median ICER (USD/DALY): ", round(median(psa_r$icer, na.rm = TRUE), 1), "\n", sep = "")
cat("95% credible interval : ",
    paste(round(quantile(psa_r$icer, c(.025, .975), na.rm = TRUE), 1), collapse = " to "), "\n", sep = "")
cat(sprintf("P(cost-effective at $%d/DALY)          : %.1f%%\n", wtp, 100 * mean(psa_r$cost_effective)))
cat(sprintf("P(dominant: cost-saving AND DALY-gain) : %.1f%%\n", 100 * mean(psa_r$dominant)))
write.csv(psa_r, file.path(out_dir, "realistic_psa_draws.csv"), row.names = FALSE)

# Cost-effectiveness plane (each dot = one simulation) 
# Points BELOW the dashed WTP line are cost-effective.
ce_plane_r <- ggplot(psa_r, aes(x = net_dalys, y = net_cost)) +
    geom_hline(yintercept = 0, colour = "grey70") +
    geom_vline(xintercept = 0, colour = "grey70") +
    geom_point(aes(colour = cost_effective), alpha = 0.35, size = 1.2) +
    geom_abline(slope = wtp, intercept = 0, linetype = "dashed") +
    scale_colour_manual(values = c(`TRUE` = "#3C5488FF", `FALSE` = "#E64B35FF"),
                        name = "Cost-effective") +
    labs(title = "Realistic scenario — PSA cost-effectiveness plane",
         subtitle = sprintf("With subnational region planning costs; %.0f%% of simulations cost-effective",
                            100 * mean(psa_r$cost_effective)),
         x = "Incremental DALYs averted", y = "Incremental net cost (USD)") +
    theme_bw()
print(ce_plane_r)
ggsave(file.path(out_dir, "realistic_psa_ce_plane.png"), ce_plane_r, width = 8, height = 6, dpi = 300)

# Cost-effectiveness acceptability curve (CEAC) 
# Probability the escalation is cost-effective as the threshold varies.
thresholds <- seq(0, 2000, by = 25)
ceac_r <- data.frame(
    wtp     = thresholds,
    prob_ce = sapply(thresholds, function(l) mean(l * psa_r$net_dalys - psa_r$net_cost > 0, na.rm = TRUE))
)
ceac_r_plot <- ggplot(ceac_r, aes(x = wtp, y = prob_ce)) +
    geom_line(linewidth = 1.1, colour = "#3C5488FF") +
    geom_vline(xintercept = wtp, linetype = "dashed", colour = "grey50") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(title = "Realistic scenario — CEAC",
         subtitle = "Share of simulations cost-effective at each WTP threshold",
         x = "Willingness-to-pay (USD per DALY averted)", y = "P(cost-effective)") +
    theme_bw()
print(ceac_r_plot)
ggsave(file.path(out_dir, "realistic_psa_ceac.png"), ceac_r_plot, width = 8, height = 5, dpi = 300)

#  ICER distribution 
#Clip the display to the central 95% of draws so the distribution is legible (median shown as dashed line).
icer_med   <- median(psa_r$icer, na.rm = TRUE)
icer_xlim  <- quantile(psa_r$icer, c(0.025, 0.975), na.rm = TRUE)
icer_pctout<- 100 * mean(psa_r$icer < icer_xlim[1] | psa_r$icer > icer_xlim[2], na.rm = TRUE)
icer_r_hist <- ggplot(subset(psa_r, icer >= icer_xlim[1] & icer <= icer_xlim[2]), aes(x = icer)) +
    geom_histogram(bins = 60, fill = "#4DBBD5FF", colour = "white") +
    geom_vline(xintercept = icer_med, linetype = "dashed") +
    labs(title = "Realistic scenario — PSA ICER distribution",
         subtitle = sprintf("Dashed line = median ICER ($%s/DALY); axis clipped to central 95%% of draws (%.1f%% fall outside)",
                            format(round(icer_med), big.mark = ","), icer_pctout),
         x = "ICER (USD per DALY)", y = "Simulations") +
    theme_bw()
print(icer_r_hist)
ggsave(file.path(out_dir, "realistic_psa_icer_distribution.png"), icer_r_hist, width = 8, height = 5, dpi = 300)


## 6. NET-BENEFIT HEATMAPS (malaria incidence vs coverage / loiasis)
# Simple tile plots: mean net benefit per subnational region binned by malaria incidence
# against IVM coverage, and against Loa loa burden (realistic base case).
#For subnational regions with different malaria burden, IVM coverage, and Loa loa risk, 
#where does increasing the dose give the most economic value

# per-subnational region model outputs at the realistic base case (planning + programme cost)
p0   <- make_params() #baseline parameter set
epi  <- f_epi(data_r, p0)
ae   <- f_AE(data_r, p0)
sae  <- f_SAE(data_r, p0)
cost <- f_cost(epi, ae, sae, data_r, p0,
               planning_cost = data_r$planning_cost, programme_cost = data_r$programme_cost)

# one row per subnational region: the input variables alongside the output (net benefit = NMB)
hd <- data.frame(
    incidence   = data_r$total_cases,                  # malaria incidence (million cases)
    ivm_cov     = epi$coverage,                           # IVM coverage (n_treated / total pop)
    loa         = data_r$loa_loa_prev,                    # Loa loa prevalence (loaisis burden)
    net_benefit = wtp * cost$net_dalys - cost$net_cost    # net benefit (NMB) per subnational region
)
hd <- hd[is.finite(hd$incidence) & is.finite(hd$ivm_cov) & is.finite(hd$loa) & is.finite(hd$net_benefit), ] #remove rows containing NA or missing (should be 0)

# make bins for plotting (group subnational regions into categories)
# equal-width incidence bins with tidy labels in millions of cases =
inc_bins <- function(x, n = 8) {
    br   <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
    labs <- paste0(formatC(head(br, -1) / 1e6, format = "f", digits = 1), "-",
                   formatC(tail(br, -1) / 1e6, format = "f", digits = 1))
    cut(x, breaks = br, labels = labs, include.lowest = TRUE)
}
# equal-width proportion bins labelled as percentages (e.g. "0-17%")
pct_bins <- function(x, n = 8) {
    br   <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
    labs <- paste0(formatC(head(br, -1) * 100, format = "f", digits = 0), "-",
                   formatC(tail(br, -1) * 100, format = "f", digits = 0), "%")
    cut(x, breaks = br, labels = labs, include.lowest = TRUE)
}

hd <- transform(hd, bin_inc = inc_bins(incidence, 8)) # malaria incidence bins
hd <- transform(hd, bin_loa = pct_bins(loa, 5))       # loiasis (Loa loa) bins, labelled as %
hd <- transform(hd, bin_cov = pct_bins(ivm_cov, 8))   # IVM coverage bins, labelled as %

# mean net benefit for each bin combination
tile_loa <- hd %>% group_by(bin_cov, bin_loa) %>% summarise(net = median(net_benefit), .groups = "drop")
tile_cov <- hd %>% group_by(bin_inc, bin_cov) %>% summarise(net = median(net_benefit), .groups = "drop")

# PLOT A: malaria incidence vs Loa loa, coloured by net benefit
p_netben_loa <- ggplot(tile_loa, aes(x = bin_cov, y = bin_loa, fill = net)) +
    geom_tile(color = "black") +
    scale_fill_gradientn(name = "Median Net Benefit (US millions)", colors = hcl.colors(10, "PuOr"),
                         rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
    labs(x = "IVM coverage", y = "Loa loa prevalence") +
    theme_classic(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_netben_loa)
ggsave(file.path(out_dir, "realistic_netbenefit_malaria_loiasis.png"), p_netben_loa, width = 9, height = 6, dpi = 300)

# PLOT B: malaria incidence vs IVM coverage, coloured by net benefit
p_netben_cov <- ggplot(tile_cov, aes(x = bin_inc, y = bin_cov, fill = net)) +
    geom_tile(color = "black") +
    scale_fill_gradientn(name = "Median Net Benefit (US millions)", colors = hcl.colors(10, "PuOr"),
                         rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
    labs(x = "Annual malaria cases (million)", y = "IVM coverage") +
    theme_classic(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_netben_cov)
ggsave(file.path(out_dir, "realistic_netbenefit_malaria_ivmcov.png"), p_netben_cov, width = 9, height = 6, dpi = 300)

