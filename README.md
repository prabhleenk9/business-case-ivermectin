# business-case-ivermectin
Code for "Business case for increasing the dose of ivermectin in MDA programmes to combat onchocerciasis"


An incremental cost-effectiveness model comparing ivermectin mass drug administration (MDA)
at **200 µg/kg** against **400 µg/kg**, evaluated independently for **202 first-level
administrative units (admin-1)** across **23 sub-Saharan African countries** where an
ivermectin MDA platform already operates.

The model asks a narrow question: given that community-directed treatment with
ivermectin already reaches these populations, what would raising the dose add? Because
the incremental efficacy of the higher dose has never been measured directly, the model
does not assume a single effect size. It sweeps efficacy across its full plausible range
and identifies the conditions under which escalation is worthwhile.

--

## Quick start

```r
# 1. Open the project in RStudio 
# 2. Install packages (once)

# 3. Run in this order:
source("R/01_model.R")                       # core model + base-case outputs
source("R/02_optimistic_scenario.R")
source("R/03_realistic_scenario.R")
source("R/04_pessimistic_scenario.R")
source("R/05_subnational_scenario_outputs.R")  # writes psa_programme_draws.csv
source("R/06_scenario_comparison.R")           # needs 05
source("R/03b_global_oneway_sensitivity.R")
source("R/07_multivariate_sensitivity.R")
source("R/08_ide_response_curve.R")
source("R/10_epi_drivers.R")
source("R/09_nmb_maps.R")                      # needs 05; needs shapefiles
```

Every script begins by sourcing `R/00_model_setup.R`, which loads the data and defines
the model, so each can be run on its own once the ordering constraints below are met.

---

## Requirements

**R ≥ 4.2.** Open `business-case-ivermectin.Rproj` before running anything — this sets
the working directory to the project root, which all paths are relative to. Each script
checks this and stops with an explanatory message if the working directory is wrong.

```r
install.packages(c(
  "readxl", "tidyverse", "gridExtra", "ggsci", "ggrepel", "scales",
  "MASS", "klaR", "randomForest", "rpart", "rpart.plot", "caret", "pROC",
  "plotly", "htmlwidgets", "sf", "stringi", "ggtern"
))
```

`ggtern` masks several `ggplot2` functions and is therefore loaded last, inside the
ternary-plot section of `10_epi_drivers.R`.

---

## Input data

| File | Contents |
|---|---|
| `data2.xlsx` — Sheet1 | One row per admin-1 region: coverage, population, Loa loa prevalence, malaria burden, unit costs, DALY weights, programme cost |
| `data2.xlsx` — Sheet2 | Country-level salaries and fuel prices |
| `data2.xlsx` — Sheet3 | Reference values and the per-country planning-cost build-up |
| `maps/inputs/ESPEN_ADM0_2023` | Country boundary shapefile |
| `maps/inputs/ESPEN_IU_2023` | Admin-1 boundary shapefile |

Sources are documented in the accompanying supplementary material. 

---

## Order of execution

The dependency chain is short. Only three constraints matter:

1. **`01_model.R` first.** It defines and exercises the core model and writes the
   base-case per-region outputs.
2. **`05` before `06` and `09`.** Script 05 writes `outputs/psa_programme_draws.csv`,
   the single source of truth for programme-level probabilistic results, which 06 reads
   for the cross-scenario figures and 09 uses for the maps.
3. **Everything else is independent** and can be run in any order.

```
00_model_setup.R   (sourced automatically by every script)
        │
   01_model.R  ──────────────► base case, sigmoid curve, per-region outputs
        │
   02 / 03 / 04  ────────────► one scenario each: base case + PSA + CEAC
        │
   05_subnational_scenario_outputs.R  ─► per-region PSA, psa_programme_draws.csv
        │                                 scenario summary, region counts
        ├──► 06_scenario_comparison.R   ─► combined CEAC, forest plot
        └──► 09_nmb_maps.R              ─► choropleth maps

   03b / 07 / 08 / 10   ─────► sensitivity analyses and determinants
                               (independent of 05)
```

---

## What each script does

| Script | Purpose |
|---|---|
| `00_model_setup.R` | Loads and cleans the data, defines all base parameters, and defines the four model functions and `make_params()`. Sourced by every other script. |
| `01_model.R` | Runs the base case across all 202 regions; produces the coverage-effectiveness curve and per-region diagnostic figures. |
| `02_optimistic_scenario.R` | Optimistic scenario: base case, 5,000-draw PSA, cost-effectiveness acceptability curve. |
| `03_realistic_scenario.R` | Realistic scenario, as above, plus the coverage × burden heatmaps. |
| `04_pessimistic_scenario.R` | Pessimistic scenario, as above, including an incremental drug cost. |
| `03b_global_oneway_sensitivity.R` | One-way sensitivity analysis over the full parameter space, and the same analysis conditioned on efficacy. |
| `05_subnational_scenario_outputs.R` | Per-region probabilistic results for all three scenarios; writes the unified PSA draws, box plot figures, scenario summary and region counts. |
| `06_scenario_comparison.R` | Cross-scenario figures from the unified draws. |
| `07_multivariate_sensitivity.R` | Full factorial over all parameters; ranks them by influence. |
| `08_ide_response_curve.R` | Sweeps efficacy from zero and locates the break-even point for each scenario. |
| `09_nmb_maps.R` | Choropleth maps of net benefit, malaria incidence and ivermectin coverage. |
| `10_epi_drivers.R` | Determinants of regional net benefit: classifiers, decision tree, partial dependence, ternary plots, break-even delivery cost. |

A fuller description of each script is given in the supplementary material.

---

## Outputs

All results are written to `outputs/`:

```
outputs/
├── *.csv                       programme- and region-level results
├── maps/                       choropleth maps
├── multivariate_sa/            factorial sensitivity figures
├── nmb_boxplots_by_country/    per-region distributions
└── thresholds/                 determinants, classifiers, decision rule
```

Key files:

| File | Contents |
|---|---|
| `psa_programme_draws.csv` | 5,000 programme-level draws per scenario |
| `psa_scenario_summary.csv` | Headline results by scenario |
| `nmb_subnational_region_scenarios.csv` | Median and 95% interval per region |
| `region_counts_by_scenario.csv` | Net-beneficial region counts |
| `thresholds/tree_rule_table.csv` | Decision rule with supporting counts |
| `thresholds/max_affordable_cost_grid.csv` | Break-even delivery cost by coverage and incidence |

---

## Notes on reproducibility

Random seeds are set before every stochastic step, so probabilistic results are
reproducible. The probabilistic analyses draw 5,000 samples per scenario and the
factorial analysis evaluates 2,187 parameter combinations across 202 regions; expect a
few minutes each on a standard laptop.

---

## Licence

Released for academic use. Boundary shapefiles are redistributed under the terms of the
WHO ESPEN platform.
