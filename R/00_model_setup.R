
#####  00 - MODEL SETUP  (shared core: packages, data, functions)
# ------------------------------------------------------------
#  This file ONLY defines things — packages, base parameters,
#  make_params(), the model functions (f_epi/f_AE/f_SAE/f_cost),
#  the run wrappers.

# Paths are relative to the project root. Open business-case-ivermectin.Rproj
# so the working directory is set correctly.
if (!file.exists("data2.xlsx"))
    stop("Working directory is not the project root. Open ",
         "business-case-ivermectin.Rproj, or setwd() to the folder ",
         "containing data2.xlsx.")

## 1. PACKAGES

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(ggsci)
library(ggrepel)

# output folder for all figures
out_dir <- "outputs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# willingness-to-pay threshold (USD per DALY averted); used to compute NMB everywhere
wtp <- 500


## 2. BASE PARAMETERS
# These are the main inputs. Any values calculated from them
# are updated automatically in make_params() when a parameter
# is changed during sensitivity analysis..


base_inputs <- list(
    
    # Malaria Incidence Effect
    IDE          = 0.125,    # Increased Dose Efficacy (extra malaria reduction 200->400 ug); base = midpoint of 0-0.25

    # adverse events (mild/moderate)
    ae_prob_200  = 0.23,      # AE prob per dose at 200 ug (Gardon et al.)
    IAEp         = 0.26/0.23, # AE relative risk 400 vs 200 (= 1.130). Drives ae_prob_400.

    # severe adverse events (Loa loa driven) 
    sae_prob_200 = 6.1/1e5,  # baseline SAE prob (Makenga Bof 2019)
    ISAEp        = NA,       # SAE relative risk 400 vs 200. NA => defaults to IAEp.
    cfr_SAE      = 0.058,    # SAE case-fatality (Makenga Bof)
    hosp_days    = 10,

    # DALY weights 
    daly_per_AE           = 0.000041, # Disability Weight (DW) = 0.005 (WHO score - distance vision:mild impairment), 3 days duration (0.005x(3/365))
    daly_per_SAE_nonfatal = 0.00685, # DW = 0.273 (70% neuro) + 0.196 (30%), 10 days (0.7x0.273) + (0.3x0.196)= 0.2499 x (10/365)
    daly_per_SAE_fatal    = 47.83, # YLL at age 40 (WHO standard life table, Life Expectancy = 90) - assumption


    # AE cost params
    ae_p_outpatient    = 0.025,   # fraction of AEs seeking care
    ae_drug_outpatient = 0.30,    # drug cost per outpatient-managed AE

    # SAE management
    prop_neuro            = 0.70, # proportion of SAEs that are neurological (rest non-neuro)
    sae_bed_days_neuro    = 14, # neurological SAE bed days (Twum-Danso)
    sae_bed_days_nonneuro = 7,  # non-neurological SAE bed days (Makenga Bof)

    # SAE drug basket: use-rates PROBABILITY-WEIGHTED BASKET (micro-costing) refer to 01_model for specifics
    use_iv_fluids = 0.98, use_corticost = 0.89, use_antibiotic = 0.784,
    use_antipyretic = 0.756, use_vitamins = 0.512, use_malaria_tx = 0.444,
    use_ng_nonneuro = 0.114, use_ng_neuro = 0.60,
    # SAE drug basket: cost per course
    c_iv_nonneuro = 4.80, c_iv_neuro = 16.80,
    c_cortico_nonneuro = 6.00, c_cortico_neuro = 10.00,
    c_antibiotic_nonneuro = 3.90, c_antibiotic_neuro = 7.80,
    c_antipyretic_nonneuro = 0.40, c_antipyretic_neuro = 0.56,
    c_vitamins_nonneuro = 0.10, c_vitamins_neuro = 0.15,
    c_malaria_nonneuro = 1.00, c_malaria_neuro = 1.00,
    c_ng_nonneuro = 1.00, c_ng_neuro = 5.00,

    # ivermectin drug cost (dose escalation) 
    ivm_cost_200 = 0,    # per person at 200 ug (donated)
    ivm_cost_400 = 0,    # per person at 400 ug (set > 0 to test higher-dose cost)

    # coverage sigmoid 
    cov_start = 0.20, cov_end = 0.90, sig_mid = 0.50, sig_steepness = 8
)



## 3. make_params(): creates final list of model parameters. Starts with baseline
#parameters and replaces any parameters that you want to test.
#Then recalculates any dependent values that change because of those inputs


make_params <- function(overrides = list()) {
    p <- modifyList(base_inputs, overrides) #Starts with baseline parameters and replaces only values you want to change

    # AE prob at 400 ug is driven by the AE relative risk IAEp, recalculates AE prob when IAE changes in sensitivity
    p$ae_prob_400 <- p$ae_prob_200 * p$IAEp

    # If ISAEp not explicitly set, it equals IAEp (model base-case assumption)
    if (is.na(p$ISAEp)) p$ISAEp <- p$IAEp

    # DALYs per SAE are a case-fatality-weighted average of the fatal and non-fatal
    # disability weights, so this must be recomputed whenever cfr_SAE is swept
    p$daly_per_SAE <- (1 - p$cfr_SAE) * p$daly_per_SAE_nonfatal +
        p$cfr_SAE * p$daly_per_SAE_fatal

    # Probability-weighted SAE drug baskets (micro-costing), avg cost to treat SAE
    p$sae_drug_nonneuro <- p$use_iv_fluids  * p$c_iv_nonneuro +
        p$use_corticost   * p$c_cortico_nonneuro    +
        p$use_antibiotic  * p$c_antibiotic_nonneuro +
        p$use_antipyretic * p$c_antipyretic_nonneuro+
        p$use_vitamins    * p$c_vitamins_nonneuro   +
        p$use_malaria_tx  * p$c_malaria_nonneuro    +
        p$use_ng_nonneuro * p$c_ng_nonneuro
    p$sae_drug_neuro <- p$use_iv_fluids  * p$c_iv_neuro +
        p$use_corticost   * p$c_cortico_neuro    +
        p$use_antibiotic  * p$c_antibiotic_neuro +
        p$use_antipyretic * p$c_antipyretic_neuro+
        p$use_vitamins    * p$c_vitamins_neuro   +
        p$use_malaria_tx  * p$c_malaria_neuro    +
        p$use_ng_neuro    * p$c_ng_neuro
    p
}


## 4. LOAD DATA

data <- read_excel("data2.xlsx", sheet="Sheet1")

new_names <- c(
    "country", "subnational_region", "malaria_incidence", "ivm_target_pop",
    "ivm_treated", "albivm_target_pop", "albivm_treated",
    "total_pop",
    "loa_loa_pct",
    "uncomplicated_per_100k", "severe_per_100k",
    "est_uncomplicated", "est_severe",
    "cost_inpatient_day", "cost_outpatient",
    "unit_cost_uncomplicated", "unit_cost_severe",
    "dalys_malaria_per_case",
    "programme_cost",
    "incidence_rate",         # cases per 1,000 population 
    "total_cases"             # absolute cases = incidence_rate/1000 * total_pop
)

names(data)[seq_along(new_names)] <- new_names

data <- data %>%
    mutate(
        # combine IVM and albendazole+IVM into one count (NA counts as 0)
        n_treated      = ifelse(is.na(ivm_treated),    0, ivm_treated) +
                         ifelse(is.na(albivm_treated), 0, albivm_treated),
        mda_target_pop = ifelse(is.na(ivm_target_pop),    0, ivm_target_pop) +
                         ifelse(is.na(albivm_target_pop), 0, albivm_target_pop),
        coverage       = n_treated / total_pop        # coverage of the TOTAL population
    )

# Where target pop is missing OR 0 but treatment did happen, set target =
# treated so coverage = 1 (everyone reached counts as covered)

data <- data %>%
    mutate(
        mda_target_pop = ifelse(
            (is.na(mda_target_pop) | mda_target_pop == 0) & n_treated > 0,
            n_treated,
            mda_target_pop
        )
    )

# Convert Loa loa from percentage to proportion (0-1) to be used in later equations
data$loa_loa_prev <- data$loa_loa_pct / 100

## 4b. PER-REGION PLANNING COST  (Sheet3 country total / number of regions)
# Attached here ONCE so every script that sources this file gets data$planning_cost (for realsitic and pessimistic scenarios)
# (programme_cost is already column 19). Scenario wrappers scale it with
# plan_mult; run_model() leaves it at 0.
plan_tbl <- read_excel("data2.xlsx", sheet = "Sheet3", range = "A46:J68",
    col_names = c("country","fuel_price","fuel_ratio","transport","food",
                  "leaflets","accom","dsa","staff_time","plan_cost_country")) %>%
    transmute(country = trimws(country), plan_cost_country = as.numeric(plan_cost_country))
n_reg    <- data %>% transmute(country = trimws(country)) %>% count(country, name = "n_regions")
plan_reg <- plan_tbl %>% left_join(n_reg, by = "country") %>%
    mutate(planning_cost = plan_cost_country / n_regions)
data <- data %>%
    mutate(.ckey = trimws(country)) %>%
    left_join(plan_reg %>% select(country, planning_cost), by = c(".ckey" = "country")) %>%
    select(-.ckey)   # data now carries planning_cost (Sheet3) + programme_cost (Sheet1 col 19)


## 5. MODEL FUNCTIONS
# Each function takes the data frame `data` (reads its columns directly) and the
# parameter list `p` from make_params(). 
f_epi <- function(data, p) {
    coverage <- data$n_treated / data$total_pop             # coverage (proportion) of the TOTAL population
    coverage[!is.finite(coverage)] <- 0                     # no/low coverage counts as 0, not NA and no missing values
    cov_scaled <- pmin(pmax((coverage - p$cov_start) / (p$cov_end - p$cov_start), 0), 1) #Scales coverage from start (0.2) to end (0.9), min 0 , max 1
    logit_c <- function(c) 1 / (1 + exp(-p$sig_steepness * (c - p$sig_mid))) #Logistic (sigmoid) function
    raw_c   <- logit_c(cov_scaled); raw_min <- logit_c(0); raw_max <- logit_c(1) #raw sigmoid values
    coverage_eff <- (raw_c - raw_min) / (raw_max - raw_min) #min max normalisation

    cases_averted <- data$total_cases * p$IDE * coverage_eff
    total_est     <- data$est_uncomplicated + data$est_severe
    list(
        coverage              = coverage,
        coverage_eff          = coverage_eff,
        cases_averted         = cases_averted,
        uncomplicated_averted = cases_averted * data$est_uncomplicated / total_est,
        severe_averted        = cases_averted * data$est_severe        / total_est,
        dalys_gained          = cases_averted * data$dalys_malaria_per_case
    )
}

f_AE <- function(data, p) {
    incremental_AEs <- data$n_treated * (p$ae_prob_400 - p$ae_prob_200)
    list(incremental_AEs = incremental_AEs,
         dalys_AEs        = incremental_AEs * p$daly_per_AE)
}

f_SAE <- function(data, p) {
    n_loa_treated    <- data$n_treated * data$loa_loa_prev
    incremental_SAEs <- n_loa_treated * p$sae_prob_200 * (p$ISAEp - 1)
    list(incremental_SAEs = incremental_SAEs,
         dalys_SAEs        = incremental_SAEs * p$daly_per_SAE)
}

f_cost <- function(epi, ae, sae, data, p, planning_cost = 0, programme_cost = 0) { #programme and planning costs set to 0, unless values are passed into function
    cost_AEs <- ae$incremental_AEs * p$ae_p_outpatient *
        (data$cost_outpatient + p$ae_drug_outpatient)

    surviving_SAEs <- sae$incremental_SAEs * (1 - p$cfr_SAE)
    cost_per_sae_neuro    <- p$sae_bed_days_neuro    * data$cost_inpatient_day + p$sae_drug_neuro
    cost_per_sae_nonneuro <- p$sae_bed_days_nonneuro * data$cost_inpatient_day + p$sae_drug_nonneuro
    cost_SAEs <- surviving_SAEs * (p$prop_neuro * cost_per_sae_neuro +
                                       (1 - p$prop_neuro) * cost_per_sae_nonneuro)

    cost_IVM <- data$n_treated * (p$ivm_cost_400 - p$ivm_cost_200)

    total_savings <- epi$uncomplicated_averted * data$unit_cost_uncomplicated +
        epi$severe_averted * data$unit_cost_severe

    total_incremental_cost <- cost_AEs + cost_SAEs + planning_cost + programme_cost + cost_IVM
    net_cost  <- total_incremental_cost - total_savings
    net_dalys <- epi$dalys_gained - ae$dalys_AEs - sae$dalys_SAEs
    icer      <- ifelse(net_dalys != 0, net_cost / net_dalys, NA)

    list(cost_AEs = cost_AEs, cost_SAEs = cost_SAEs, cost_IVM = cost_IVM,
         total_savings = total_savings,
         total_incremental_cost = total_incremental_cost,
         net_cost = net_cost, net_dalys = net_dalys, icer = icer)
}


## 6. RUN WRAPPERS

# Subnational region level: Passes entire columns into function, 
#runs each function for every row in dataset once to get subnational region level data (baseline)
run_model_subnational_region <- function(p, data) {
    epi  <- f_epi(data, p)
    ae   <- f_AE(data, p)
    sae  <- f_SAE(data, p)
    cost <- f_cost(epi, ae, sae, data, p)

    # Create data frame
    data.frame(data,
               coverage_eff  = epi$coverage_eff,   
               cases_averted = epi$cases_averted,
               dalys_gained  = epi$dalys_gained,
               net_cost      = cost$net_cost,
               net_dalys     = cost$net_dalys,
               cost_AEs      = cost$cost_AEs,
               cost_SAEs     = cost$cost_SAEs,
               cost_IVM      = cost$cost_IVM,
               total_savings = cost$total_savings,
               icer = ifelse(cost$net_dalys != 0, cost$net_cost / cost$net_dalys, NA_real_))
}

#Whole intervention level: Same function as above but sumps up everything together (not subnational region level)
run_model <- function(p, data) {
    epi  <- f_epi(data, p)
    ae   <- f_AE(data, p)
    sae  <- f_SAE(data, p)
    cost <- f_cost(epi, ae, sae, data, p)

    tot <- c(
        cases_averted = sum(epi$cases_averted,  na.rm = TRUE),
        dalys_gained  = sum(epi$dalys_gained,   na.rm = TRUE),
        net_cost      = sum(cost$net_cost,      na.rm = TRUE),
        net_dalys     = sum(cost$net_dalys,     na.rm = TRUE),
        cost_AEs      = sum(cost$cost_AEs,      na.rm = TRUE),
        cost_SAEs     = sum(cost$cost_SAEs,      na.rm = TRUE),
        cost_IVM      = sum(cost$cost_IVM,      na.rm = TRUE),
        programme_cost = sum(data$programme_cost, na.rm = TRUE),
        total_savings = sum(cost$total_savings, na.rm = TRUE)
    )
    icer <- if (tot["net_dalys"] != 0) unname(tot["net_cost"] / tot["net_dalys"]) else NA_real_
    nmb  <- unname(wtp * tot["net_dalys"] - tot["net_cost"])   # net monetary benefit
    c(tot, icer = icer, nmb = nmb) # Total program ICER + NMB
}

# outputs to plot
out_cols <- c("cases_averted","dalys_gained","net_cost","net_dalys",
              "cost_AEs","cost_SAEs","total_savings")


