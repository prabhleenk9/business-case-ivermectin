
#### 01 - IVERMECTIN DOSE ESCALATION: COST-BENEFIT MODEL + SIGMOIDAL CURVE
#  200 ug/kg  -->  400 ug/kg
# ------------------------------------------------------------
# Structure:
#   f_epi()  -> malaria cases averted, DALYs gained
#   f_AE()   -> incremental mild/moderate AEs + DALYs lost
#   f_SAE()  -> incremental SAEs (Loa loa driven) + DALYs lost
#   f_cost() -> AE costs + SAE costs + savings + net cost, net DALYs, ICER
#   for loop -> runs all 4 functions across every subnational region in dataset

# RUN THIS SCRIPT FIRST, AND NEVER AFTER ANY OTHER SCRIPT.

# This script is a standalone, script-style statement of the model: it reads
# data2.xlsx itself, holds its parameters as loose global variables, and defines
# its own f_epi(data), f_AE(data), f_SAE(data) and f_cost(epi, ae, sae, data).
#
# Every OTHER script sources R/00_model_setup.R, which defines the same four
# functions in parameterised form - f_epi(data, p) and so on - driven by
# make_params(). Those versions are what the scenario, sensitivity and
# probabilistic analyses depend on.
#
# The two sets of functions therefore share names but NOT signatures. Sourcing
# this script after 00_model_setup.R silently replaces the parameterised
# versions with ones that ignore `p`, so every subsequent analysis would run at
# base case WITHOUT raising an error. Run this script first (or in a clean
# session) and the problem does not arise, because each later script re-sources
# 00_model_setup.R at the top.
#
# The parameter values below duplicate `base_inputs` in 00_model_setup.R and are
# kept in step by hand. If a parameter is changed there, change it here too.

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
library(ggrepel)   

# output folder for figures
out_dir <- "outputs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## 2.GLOBAL PARAMETERS

# base parameters 
IDE          <- 0.125  # Increased Dose Efficacy: extra malaria reduction from 200ug -> 400ug (base = midpoint of 0-0.25)
# AE probabilities (total AE prob per dose)
ae_prob_200  <- 0.23   # 23% at 150-200 ug/kg (Gardon et al.)
ae_prob_400  <- 0.26   # 26% at 400-800 ug/kg (Gardon et al.)
IAEp         <- ae_prob_400 / ae_prob_200   # = 1.130 (risk ratio/relative risk)

# SAE probailities
sae_prob_200 <- 6.1 / 100000     # (Makenga Bof 2019)

ISAEp        <- IAEp    # BASE CASE = 1.130 (Gardon et al. 2002: 26%/23%) 
cfr_SAE      <- 0.058   # SAE case fatality probability (Makenga Bof 2019)
hosp_days    <- 10      # median hospitalisation days (7 Makenga Bof; 14 Twum-Danso)

# DALY parameters (WHO GHE 2019 / GBD 2019)
daly_per_AE           <- 0.000041   # Disability Weight (DW) = 0.005 (WHO score - distance vision:mild impairment), 3 days duration (0.005x(3/365))
daly_per_SAE_nonfatal <- 0.00685    # DW = 0.273 (70% neuro) + 0.196 (30%), 10 days (0.7x0.273) + (0.3x0.196)= 0.2499 x (10/365)
daly_per_SAE_fatal    <- 47.83      # YLL at age 40 (WHO standard life table, Life Expectancy = 90) - assumption
daly_per_SAE          <- (1 - cfr_SAE) * daly_per_SAE_nonfatal + cfr_SAE * daly_per_SAE_fatal
# daly_per_SAE = 2.78 DALYs per SAE (fatal cases dominate at ~99% of burden)

# Cost parameters (all in USD) 
# A) Adverse events and the fraction that seek that care --
ae_p_outpatient    <- 0.025   # 2-3% seek medical attention (outpatient visit) (Chijioke 1992)

# Drug only cost PER adverse event at each level of care
# (The outpatient VISIT fee is separate= cost_outpatient)
ae_drug_outpatient <- 0.30    # drug cost per outpatient-managed AE

# B) Severe AE management
# SAEs split into neurological vs non-neurological (same 70/30 split as DALYs).
prop_neuro            <- 0.70   # proportion of SAEs that are neurological (rest non-neuro)
sae_bed_days_neuro    <- 14     # neurological SAE bed days (Twum-Danso)
sae_bed_days_nonneuro <- 7      # non-neurological SAE bed days (Makenga Bof)

# SAE drug cost: PROBABILITY-WEIGHTED BASKET (micro-costing)
# Unit prices: UNICEF Supply Catalogue /MSH 2015 price guide. Each line = P(receive/treatment used) x cost-per-course over the stay.
# Proportion of SAE patients receiving each component:
use_iv_fluids   <- 0.98    # parenteral rehydration (Makenga Bof 2019)
use_corticost   <- 0.89    # corticosteroids (onchocerciasis-SAE literature)
use_antibiotic  <- 0.784   # antibiotic therapy (Makenga Bof 2019)
use_antipyretic <- 0.756   # antipyretics
use_vitamins    <- 0.512   # vitamins
use_malaria_tx  <- 0.444   # malaria treatment
use_ng_nonneuro <- 0.114   # nasogastric feeding (overall cohort rate)
use_ng_neuro    <- 0.60    # higher for neurological SAE (coma = 94% of severe neuro events)

# Cost per course over the stay (USD); unit-price source in each comment.
# IV fluid @ ~$0.60/L (MSH bulk median; UNICEF single-bag ~$3.20/L)
c_iv_nonneuro          <- 4.80    # ~8 L over 7 d
c_iv_neuro             <- 16.80   # ~28 L over 14 d (coma maintenance)
# Hydrocortisone sod. succ. 100 mg inj @ ~$0.50/vial
c_cortico_nonneuro     <- 6.00    # ~12 vials
c_cortico_neuro        <- 10.00   # ~20 vials
# Ceftriaxone 1 g inj @ $0.78/vial (UNICEF indicative price S1531005)
c_antibiotic_nonneuro  <- 3.90    # 5 days
c_antibiotic_neuro     <- 7.80    # 10 days
# Paracetamol 500 mg @ ~$0.01/tab
c_antipyretic_nonneuro <- 0.40
c_antipyretic_neuro    <- 0.56
# Multivitamin course
c_vitamins_nonneuro    <- 0.10
c_vitamins_neuro       <- 0.15
# ACT (artemether-lumefantrine) course @ ~$1.00 (Global Fund / Uganda ref ~$1.40)
c_malaria_nonneuro     <- 1.00
c_malaria_neuro        <- 1.00
# Nasogastric feeding (+ anticonvulsant such as diazepam in coma)
c_ng_nonneuro          <- 1.00
c_ng_neuro             <- 5.00

# Probability-weighted totals (these feed f_cost):
sae_drug_nonneuro <- use_iv_fluids   * c_iv_nonneuro          +
    use_corticost   * c_cortico_nonneuro     +
    use_antibiotic  * c_antibiotic_nonneuro  +
    use_antipyretic * c_antipyretic_nonneuro +
    use_vitamins    * c_vitamins_nonneuro    +
    use_malaria_tx  * c_malaria_nonneuro     +
    use_ng_nonneuro * c_ng_nonneuro            # ~ $14 per non-neuro SAE

sae_drug_neuro    <- use_iv_fluids   * c_iv_neuro          +
    use_corticost   * c_cortico_neuro     +
    use_antibiotic  * c_antibiotic_neuro  +
    use_antipyretic * c_antipyretic_neuro +
    use_vitamins    * c_vitamins_neuro    +
    use_malaria_tx  * c_malaria_neuro     +
    use_ng_neuro    * c_ng_neuro             # ~ $35 per neuro SAE

# (C) Ivermectin drug cost
# Baseline: IVM (Mectizan) is DONATED -> incremental drug cost = 0.
# To test "what if the higher 400 ug/kg dose costs more", set ivm_cost_400 > 0.
# Incremental IVM cost = n_treated * (ivm_cost_400 - ivm_cost_200).
ivm_cost_200 <- 0   # IVM cost per person at 200 ug/kg (baseline, donated)
ivm_cost_400 <- 0   # IVM cost per person at 400 ug/kg (set > 0 to model higher-dose cost)

# Sigmoid function (herd effect) 
# As coverage rises, mosquito population is progressively killed, reducing vector density for
# everyone, including the untreated. This community (herd) protection effect creates a
# non-linear, S-shaped dose-response with coverage.
# Parameterisation: normalised logistic sigmoid rescaled so f(0)=0, f(1)=1

# Thresholds
cov_start     <- 0.20   # no effect below 20%
cov_end       <- 0.90   # full effect by 90%
sig_mid       <- 0.50   # inflection point of sigmoid (coverage rate, 0-1)
sig_steepness <- 8      # steepness of the S-curve #vary this if needed to see effect
    


## 3. LOAD DATA

data <- read_excel("data2.xlsx", sheet = "Sheet1")

colnames(data) <- c(
    "country", "subnational_region", "malaria_incidence", "ivm_target_pop",
    "ivm_treated", "albivm_target_pop", "albivm_treated",   # ALB co-administered as separate tablet
    "total_pop",
    "loa_loa_pct",    # loa loa prevalence stored as percentage (e.g. 31.8); converted to proportion below
    "uncomplicated_per_100k", "severe_per_100k",
    "est_uncomplicated", "est_severe",
    "cost_inpatient_day", "cost_outpatient",
    "unit_cost_uncomplicated", "unit_cost_severe",
    "dalys_malaria_per_case",
    "programme_cost",
    "incidence_rate",                
    "total_cases"                    # absolute cases = incidence_rate/1000 * total_pop
)

# Derive n_treated (actual IVM recipients)

# #IVM and ALB distributed together in MDA, but as separate tablets, n_treated includes both IVM and ALB+IVM
# ivm_coverage and albivm_coverage both represent people who received an IVM tablet.

data <- data %>%
    mutate(
        # combine IVM and albendazole+IVM into one count (simple sum; NA counts as 0)
        n_treated      = ifelse(is.na(ivm_treated), 0, ivm_treated) +
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


# Convert Loa loa from percentage to proportion (0-1)
data$loa_loa_prev <- data$loa_loa_pct / 100

# Per-subnational-region PLANNING cost (Sheet3 country total / number of regions).
# programme_cost is already column 19; attaching planning_cost here means f_cost
# applies BOTH the planning and programme cost in this standalone model.
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
    select(-.ckey)

# DATA CHECKS

# Remove rows missing key inputs 
data_clean <- data %>%
    filter(
        !is.na(n_treated),
        !is.na(mda_target_pop),
        mda_target_pop > 0,
        !is.na(malaria_incidence),
        !is.na(loa_loa_prev),
        !is.na(dalys_malaria_per_case),
        !is.na(cost_inpatient_day),
        !is.na(cost_outpatient),
        n_treated > 0,
        (uncomplicated_per_100k + severe_per_100k) > 0
    )

cat("\nSubnational regions retained for analysis:", nrow(data_clean), "\n")

#Check any subnational regions that have been removed and why, for check

removed_subnational_regions <- data %>%
    mutate(
        removal_reason = case_when(
            is.na(n_treated) ~ "Missing n_treated",
            is.na(mda_target_pop) ~ "Missing mda_target_pop",
            mda_target_pop <= 0 ~ "Target population ≤ 0",
            is.na(malaria_incidence) ~ "Missing malaria incidence",
            is.na(loa_loa_prev) ~ "Missing Loa loa prevalence",
            is.na(dalys_malaria_per_case) ~ "Missing DALYs",
            is.na(cost_inpatient_day) ~ "Missing inpatient cost",
            is.na(cost_outpatient) ~ "Missing outpatient cost",
            n_treated <= 0 ~ "No one treated",
            (uncomplicated_per_100k + severe_per_100k) <= 0 ~ "No malaria burden",
            TRUE ~ NA_character_
        )
    ) %>%
    filter(!is.na(removal_reason)) %>%
    select(country, subnational_region, removal_reason)

removed_subnational_regions

### 4. FUNCTIONS

## FUNCTION 1: f_epi
# Malaria cases averted and DALYs gained from dose escalation

f_epi <- function(data) {

    # Coverage: fraction (proportion) of the TOTAL population who received IVM
    coverage <- data$n_treated / data$total_pop
    # No/low coverage must count as 0 effect
    coverage[!is.finite(coverage)] <- 0

    # Rescale coverage between 20% and 90%
    cov_scaled <- pmin(pmax((coverage - cov_start) /
                                (cov_end - cov_start), 0), 1)
    
    # Sigmoidal coverage effect (normalised logistic, rescaled to [0, 1])
    logit_c   <- function(c) 1 / (1 + exp(-sig_steepness * (c - sig_mid)))
    raw_c     <- logit_c(cov_scaled)
    raw_min   <- logit_c(0) #rescale sigmoid so 0% coverage = 0 effect and 100% coverage = 1
    raw_max   <- logit_c(1)
    coverage_eff <- (raw_c - raw_min) / (raw_max - raw_min)
    
    # Total cases averted = full IDE benefit * sigmoidal coverage effect
    cases_averted <- data$total_cases * IDE * coverage_eff
    
    # Split averted cases into uncomplicated vs severe
    # using same observed proportion as the subnational region
    total_est          <- data$est_uncomplicated + data$est_severe
    prop_uncomplicated <- data$est_uncomplicated / total_est
    prop_severe        <- data$est_severe        / total_est
    
    uncomplicated_averted <- cases_averted * prop_uncomplicated
    severe_averted        <- cases_averted * prop_severe
    
    dalys_gained <- cases_averted * data$dalys_malaria_per_case
    
    return(list(
        coverage              = coverage,
        coverage_eff          = coverage_eff,
        cases_averted         = cases_averted,
        uncomplicated_averted = uncomplicated_averted,
        severe_averted        = severe_averted,
        dalys_gained          = dalys_gained
    ))
}


## FUNCTION 2: f_AE
# Incremental mild/moderate adverse events and DALYs lost

f_AE <- function(data) {
    
    expected_AEs_200 <- data$n_treated * ae_prob_200 #expected adverse events at 200 dose
    expected_AEs_400 <- data$n_treated * ae_prob_400 #expected adverse events at 400 dose
    incremental_AEs  <- expected_AEs_400 - expected_AEs_200 #incremental AEs due to dose
    
    dalys_AEs <- incremental_AEs * daly_per_AE #dalys lost to AEs
    
    return(list(
        expected_AEs_200 = expected_AEs_200,
        expected_AEs_400 = expected_AEs_400,
        incremental_AEs  = incremental_AEs,
        dalys_AEs        = dalys_AEs
    ))
}

## FUNCTION 3: f_SAE
# Incremental severe adverse events (Loa loa driven) and DALYs lost

f_SAE <- function(data) {
    
    # Number of Loa loa-positive people receiving IVM in this subnational region by taking loa loa prevalence 
    n_loa_treated <- data$n_treated * data$loa_loa_prev
    
    # SAE prob in Loa loa patients at 200ug (baseline SAE prob due to loa loa ASSUMPTION that only driven by loa loa)
    sae_prob_loa_200 <- sae_prob_200 #basically the same thing 
    
    # Expected SAEs in Loa loa patients at each dose
    expected_SAEs_loa_200 <- n_loa_treated * sae_prob_loa_200
    expected_SAEs_loa_400 <- n_loa_treated * sae_prob_loa_200 * ISAEp #increased risk taken from AEs
    
    # Incremental SAEs from dose escalation (0 when ISAEp = 1.0)
    incremental_SAEs <- expected_SAEs_loa_400 - expected_SAEs_loa_200
    
    # DALYs lost (fatal + non-fatal combined in daly_per_SAE)
    dalys_SAEs <- incremental_SAEs * daly_per_SAE
    
    return(list(
        n_loa_treated         = n_loa_treated,
        sae_prob_loa_200      = sae_prob_loa_200,
        expected_SAEs_loa_200 = expected_SAEs_loa_200,
        expected_SAEs_loa_400 = expected_SAEs_loa_400,
        incremental_SAEs      = incremental_SAEs,
        dalys_SAEs            = dalys_SAEs
    ))
}

## FUNCTION 4: f_cost
# All monetary costs + net cost + net DALYs + ICER

# Inputs:
#   epi, ae, sae            - output lists from the three functions above
#   cost_outpatient         - outpatient visit unit cost
#   cost_inpatient_day      - inpatient bed-day cost 
#   unit_cost_uncomplicated - treatment cost per uncomplicated malaria case
#   unit_cost_severe        - treatment cost per severe malaria case
#   cfr_SAE                 - SAE case fatality probability
#   n_treated               - IVM recipients in the subnational region (for IVM drug cost)
#   AE cost params          - care-seeking split + per-AE drug costs
#   SAE cost params         - neuro/non-neuro bed days + drug costs + fixed cost
#   ivm_cost_200/400        - IVM drug cost per person at each dose (0 = donated)
#   programme add-ons       - programme / planning (default 0)


f_cost <- function(epi, ae, sae, data) {
    # per-subnational region add-on costs read straight from the data (0 if the column is absent)
    planning_cost  <- if (!is.null(data$planning_cost))  data$planning_cost  else 0
    programme_cost <- if (!is.null(data$programme_cost)) data$programme_cost else 0
    
    # (1) Mild/moderate AE costs, broken down by level of care 
    # Outpatient-managed AEs: subnational region visit fee + drugs
    cost_AE_outpatient <- ae$incremental_AEs * ae_p_outpatient *
        (data$cost_outpatient + ae_drug_outpatient)
    
    # Total cost AEs
    cost_AEs <- cost_AE_outpatient           
    
    # (2) SAE costs: neurological vs non-neurological 
    # Only non-fatal survivors incur hospitalisation cost (fatal cases captured in DALYs)
    surviving_SAEs <- sae$incremental_SAEs * (1 - cfr_SAE)
    # Per-SAE cost = bed days * subnational region bed-day cost + drug cost + fixed consumables 
    cost_per_sae_neuro    <- sae_bed_days_neuro    * data$cost_inpatient_day + sae_drug_neuro    
    cost_per_sae_nonneuro <- sae_bed_days_nonneuro * data$cost_inpatient_day + sae_drug_nonneuro 
    cost_SAEs <- surviving_SAEs * (prop_neuro       * cost_per_sae_neuro +
                                       (1 - prop_neuro) * cost_per_sae_nonneuro)
    
    # (3) Incremental ivermectin drug cost (dose escalation) 
    # 0 when donated (ivm_cost_400 == ivm_cost_200); > 0 if the higher dose costs more
    cost_IVM <- data$n_treated * (ivm_cost_400 - ivm_cost_200) 
    
    
    # Benefits: savings from averted malaria treatment 
    savings_uncomplicated <- epi$uncomplicated_averted * data$unit_cost_uncomplicated
    savings_severe        <- epi$severe_averted        * data$unit_cost_severe
    total_savings         <- savings_uncomplicated + savings_severe
    
    # Net cost (negative = net saving to health system)
    total_incremental_cost <- cost_AEs + cost_SAEs +
        planning_cost + programme_cost + cost_IVM
    net_cost <- total_incremental_cost - total_savings # + means cost and if - then saves money
    
    # Net DALYs (positive = net health gain) 
    net_dalys <- epi$dalys_gained - ae$dalys_AEs - sae$dalys_SAEs
    
    # ICER: cost per DALY gained (>0 -> cost per health gain, <0 -> cost saving)
    # Negative ICER = dose escalation is cost-saving (dominant strategy)
    icer <- ifelse(net_dalys != 0, net_cost / net_dalys, NA)
    
    return(list(
        cost_AE_outpatient     = cost_AE_outpatient,
        cost_AEs               = cost_AEs,
        cost_SAEs              = cost_SAEs,
        cost_IVM               = cost_IVM,
        savings_uncomplicated  = savings_uncomplicated,
        savings_severe         = savings_severe,
        total_savings          = total_savings,
        total_incremental_cost = total_incremental_cost,
        net_cost               = net_cost,
        net_dalys              = net_dalys,
        icer                   = icer,
        planning_cost          = planning_cost, 
        programme_cost         = programme_cost 
    ))
}



## 5. FOR LOOP: run all 4 functions for each subnational region

results_list <- vector("list", nrow(data_clean))

for (i in 1:nrow(data_clean)) {
    
    row <- data_clean[i, ]
   
    # Function 1: epidemiological benefit 
    epi <- f_epi(row)
    
    # Function 2: mild/moderate AEs (no costs) 
    ae <- f_AE(row)
    
    # Function 3: SAEs driven by Loa loa (no costs) 
    sae <- f_SAE(row)
    
    # Function 4: all costs, net cost, net DALYs, ICER 
    cost <- f_cost(epi, ae, sae, row)
    
    
    # Store results for this subnational region 
    results_list[[i]] <- data.frame(
        country              = row$country,
        subnational_region             = row$subnational_region,
        n_treated            = row$n_treated,
        mda_target_pop       = row$mda_target_pop,  
        loa_loa_pct          = row$loa_loa_pct,
        # Epi
        coverage             = epi$coverage,
        coverage_eff         = epi$coverage_eff,   # sigmoid-adjusted effective coverage
        cases_averted        = epi$cases_averted,
        dalys_gained         = epi$dalys_gained,
        # AEs
        incremental_AEs      = ae$incremental_AEs,
        dalys_AEs            = ae$dalys_AEs,
        # SAEs
        n_loa_treated        = sae$n_loa_treated,
        incremental_SAEs     = sae$incremental_SAEs,
        dalys_SAEs           = sae$dalys_SAEs,
        # Costs (all from f_cost)
        cost_AE_outpatient   = cost$cost_AE_outpatient,
        cost_AEs             = cost$cost_AEs,
        cost_SAEs            = cost$cost_SAEs,
        cost_IVM             = cost$cost_IVM,
        total_savings        = cost$total_savings,
        total_incremental_cost = cost$total_incremental_cost,
        net_cost             = cost$net_cost,
        net_dalys            = cost$net_dalys,
        icer                 = cost$icer
    )
}

# Combine all subnational regions
results <- bind_rows(results_list)

# Quick summary
cat("\n===== RESULTS SUMMARY (IDE =", IDE, ") =====\n")
cat("Subnational regions run:", nrow(results), "\n")
cat("Net cost-saving subnational regions (net_cost < 0):",
    sum(results$net_cost < 0, na.rm = TRUE), "\n")
cat("Net DALY-gaining subnational regions (net_dalys > 0):",
    sum(results$net_dalys > 0, na.rm = TRUE), "\n")
cat("Median ICER (USD per DALY):",
    round(median(results$icer, na.rm = TRUE), 2), "\n")
cat("Total cases averted:", round(sum(results$cases_averted), 0), "\n")
cat("Total DALYs gained:", round(sum(results$dalys_gained), 0), "\n")
cat("Total net cost (USD):", round(sum(results$net_cost), 0), "\n")

print(head(results[, c("country", "subnational_region", "cases_averted",
                       "cost_AEs", "cost_SAEs", "cost_IVM",
                       "net_cost", "net_dalys", "icer")]))

## 6. Sigmoid coverage-effect curve with subnational regions overlaid

curve_df <- data.frame(coverage = seq(0, 1, length.out = 400))
# uses this script's global sigmoid parameters (cov_start, cov_end, sig_mid, sig_steepness)
cov_scaled <- pmin(pmax((curve_df$coverage - cov_start) / (cov_end - cov_start), 0), 1)
lg <- function(c) 1 / (1 + exp(-sig_steepness * (c - sig_mid)))
curve_df$coverage_eff <- (lg(cov_scaled) - lg(0)) / (lg(1) - lg(0))
# mark the coverage thresholds recovered empirically by the classification tree,
# with the proportion of the full efficacy benefit realised at each
eff_at <- function(cov) {
    z <- pmin(pmax((cov - cov_start) / (cov_end - cov_start), 0), 1)
    (lg(z) - lg(0)) / (lg(1) - lg(0))
}
mark_df <- data.frame(coverage = 0.22)
mark_df$coverage_eff <- eff_at(mark_df$coverage)
mark_df$label <- sprintf("%.0f%% coverage\n%.3f of full benefit",
                         mark_df$coverage * 100, mark_df$coverage_eff)

# labels for the two structural bounds of the function
bound_df <- data.frame(
    coverage = c(cov_start, cov_end),
    y        = c(0.90, 0.45),
    hjust    = c(-0.07, 1.07),
    label    = c(sprintf("Lower bound (%.0f%%)", cov_start * 100),
                 sprintf("Upper bound (%.0f%%)", cov_end   * 100)))

sigmoid_plot <- ggplot() +
    geom_vline(xintercept = c(cov_start, cov_end),
               linetype = "dashed", colour = "grey60") +
    geom_line(data = curve_df, aes(coverage, coverage_eff),
              linewidth = 1.1, colour = "steelblue") +
    geom_point(data = results, aes(coverage, coverage_eff),
               colour = "firebrick", size = 2, alpha = 0.7) +
    geom_text(data = bound_df,
              aes(x = coverage, y = y, label = label, hjust = hjust),
              size = 3.5, colour = "grey35") +
    geom_point(data = mark_df, aes(coverage, coverage_eff),
               colour = "black", size = 2.8) +
    ggrepel::geom_label_repel(data = mark_df,
                              aes(coverage, coverage_eff, label = label),
                              size = 3.6, lineheight = 0.95,
                              fill = "white", label.size = 0.2,
                              box.padding = 0.55,
                              nudge_x = 0.14, nudge_y = 0.13,
                              segment.colour = "grey45", min.segment.length = 0) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = "Coverage (% of population treated)",
         y = "Sigmoidal coverage effect (0-1)",
         title = NULL) +
    theme_minimal(base_size = 13)
print(sigmoid_plot)
ggsave(file.path(out_dir, "sigmoid_coverage.png"), sigmoid_plot,
       width = 9, height = 6, dpi = 300, bg = "white")
