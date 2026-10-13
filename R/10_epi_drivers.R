#### 10 - EXPLORATORY DATA ANALYSIS: WHICH EPI + COST COMBINATIONS PAY OFF
# ------------------------------------------------------------
#   Map the three epidemiological covariates onto net monetary benefit (NMB) —
#     * ivermectin coverage   (binned: <=30% / 30-60% / >=60%)
#     * malaria incidence     (cases per 1,000, from total cases)
#     * Loa loa prevalence
#  plus a COST covariate (programme+planning cost per person), to extract a
#  transferable rule: given a country's parameters, will dose escalation
#  be net-beneficial, and roughly how large is the benefit?
#
#  Realistic-central scenario, full cost stack (planning + 0.75*programme).

# Paths are relative to the project root. Open business-case-ivermectin.Rproj
# so the working directory is set correctly.
if (!file.exists("data2.xlsx"))
    stop("Working directory is not the project root. Open ",
         "business-case-ivermectin.Rproj, or setwd() to the folder ",
         "containing data2.xlsx.")

# Source the model
source("R/00_model_setup.R")


## 1. PACKAGES
# install.packages(c("MASS","klaR","randomForest","rpart","rpart.plot","plotly","htmlwidgets"))
library(MASS); library(klaR)   
library(randomForest); library(rpart); library(rpart.plot)
library(plotly); library(htmlwidgets)  
library(caret); library(pROC)

#set output directory
out_dir <- "outputs"; thr_dir <- file.path(out_dir, "thresholds")
if (!dir.exists(thr_dir)) dir.create(thr_dir, recursive = TRUE)

#set willingness to pay threshold
wtp <- 500

## 2. BUILD THE REALISTIC PER-REGION TABLE (with cost per person treated)

p <- make_params(list(IDE = 0.125, IAEp = 1.30, ISAEp = 1.30, cfr_SAE = 0.05)) #realistic parameter values
dat <- data   #working copy of dataset

#run model
epi <- f_epi(dat, p); ae <- f_AE(dat, p); sae <- f_SAE(dat, p)
cost <- f_cost(epi, ae, sae, dat, p,
               planning_cost = 1.00 * dat$planning_cost, programme_cost = 0.75 * dat$programme_cost) #realistic programme and planning cost params

df <- data.frame(
    country         = dat$country,
    region          = dat$subnational_region,
    coverage        = epi$coverage,                          # ivermectin coverage 0-1
    incidence       = dat$incidence_rate,                    # malaria incidence /1,000
    loa             = dat$loa_loa_prev,                       # Loa prevalence 0-1
    cost_per_person_treated = cost$total_incremental_cost / dat$n_treated, #all the costs associated with escalation (including to treat + programme/planning)
    delivery_cost_pp = (1.00 * dat$planning_cost + 0.75 * dat$programme_cost) / dat$n_treated, #discretionary money programme  decided to spend
    nmb_M           = (wtp * cost$net_dalys - cost$net_cost) / 1e6   # NMB in US$ millions
)

df$net <- factor(ifelse(df$nmb_M > 0, "YES", "NO"), levels = c("NO","YES")) #classify whether intervention is beneficial

# coverage bands (proportional)
df$coverage_band <- cut(df$coverage, breaks = c(-Inf, 0.30, 0.60, Inf),
                        labels = c("Low (<=30%)", "Mid (30-60%)", "High (>=60%)"))

# % beneficial by coverage band, groups regions into low coverage (< 30%,), mid (30-60%) and high ( > 60 %)
band_tab <- df %>% group_by(coverage_band) %>% #for each coverage group calculates number of regions
    summarise(n = n(), beneficial = sum(net == "YES"),
              pct_beneficial = round(100 * mean(net == "YES")), .groups = "drop") #percentage of regions where beneficial
cat("\n=== Net-beneficial by coverage band ===\n"); print(band_tab)

#Create csv of outputs
write.csv(band_tab, file.path(thr_dir, "coverage_band_summary.csv"), row.names = FALSE)

#Define predictors
PRED <- c("coverage", "incidence", "loa", "cost_per_person_treated")

#Predict whether a region has a positive net monetary benefit (YES) or not (NO) using four regional characteristics

## 3. (A) LDA: assumes the boundary between YES and NO is approximately a straight line (or hyperplane in multiple dimensions).

#set random seed
set.seed(123)

#fit LDA model 
lda_fit <- MASS::lda(net ~ coverage + incidence + loa + cost_per_person_treated, data = df) #outcome (net) predicted using 4 metrics
print(lda_fit) #estimates combination of predictors that best separate yes or no regions

# leave-one-out accuracy, how well classifier performs on unseen data
# remove one region, fit the model using all the other regions, predict excluded region, repeat until every region has been left out
lda_cv  <- MASS::lda(net ~ coverage + incidence + loa + cost_per_person_treated, data = df, CV = TRUE)
cat(sprintf("LDA leave-one-out accuracy: %.2f\n", mean(lda_cv$class == df$net))) #how many regions were accurately classified

# Quadratic Discriminant Analysis — quadratic (curved) boundaries; coverage's effect on NMB is non-linear
qda_fit <- MASS::qda(net ~ coverage + incidence + loa + cost_per_person_treated, data = df) #same predictors diff decision boundary (curved)
qda_cv  <- MASS::qda(net ~ coverage + incidence + loa + cost_per_person_treated, data = df, CV = TRUE) #same leave one out procedure
cat(sprintf("QDA leave-one-out accuracy: %.2f\n", mean(qda_cv$class == df$net))) #classification accuracy

## 4. (B) RANDOM FOREST: which regional characteristics are most important in predicting

set.seed(1)

#classification random forest
rf_cls <- randomForest(net ~ coverage + incidence + loa + cost_per_person_treated,
                       data = df, ntree = 500, importance = TRUE) #500 decision trees, every tree built from different set of combos, calculate how important each variable is
cat("\n=== Random forest (classify net-beneficial) ===\n"); print(rf_cls)

#regression random forest
rf_reg <- randomForest(nmb_M ~ coverage + incidence + loa + cost_per_person_treated,
                       data = df, ntree = 500, importance = TRUE) #predicts actual NMB value rather than yes/no diff outcome
cat(sprintf("RF regression on NMB ($M): %% var explained = %.1f\n", #how much variation in NMB it explains (r2 similar)
            100 * rf_reg$rsq[length(rf_reg$rsq)]))

#Variable importance plot
png(file.path(thr_dir, "rf_importance.png"), width = 1400, height = 700, res = 150)
par(mfrow = c(1, 2))
varImpPlot(rf_cls, main = "Importance — beneficial (yes/no)", type = 1) #which variables are most important in predicting if NMB is beneficial
varImpPlot(rf_reg, main = "Importance — NMB magnitude", type = 1) #which variables are most important in predicting size of NMB
dev.off()

# Partial dependence plots: how P(beneficial) / NMB move with each driver

# loa loa excluded due to low importance from other classifiers
png(file.path(thr_dir, "rf_partial_dependence.png"), width = 1500, height = 500, res = 150)
par(mfrow = c(1, 3))
partialPlot(rf_cls, pred.data = df, x.var = "coverage",        which.class = "YES", main = "Coverage") # If only coverage changes, while averaging over the other predictors, how does the probability that NMB is positive change
partialPlot(rf_cls, pred.data = df, x.var = "incidence",       which.class = "YES", main = "Incidence")

pd_cost <- partialPlot(rf_cls, pred.data = df, x.var = "cost_per_person_treated",
                       which.class = "YES", plot = FALSE)
plot(pd_cost$x, pd_cost$y, type = "l",
     main = "Cost per person treated",
     xlab = "US$ per person treated (log scale)", ylab = "",
     log = "x")
rug(df$cost_per_person_treated)
dev.off()


## 5. CLASSIFIER COMPARISON: LDA vs QDA vs RANDOM FOREST
#     identical 5x5-fold Cross Validation; report AUC / sensitivity / specificity and
#     overlay the ROC curves to show which generalises best.

df$net <- factor(df$net, levels = c("YES", "NO"))   # YES (beneficial) = define positive class, comes first
set.seed(42)
folds <- createMultiFolds(df$net, k = 5, times = 5) #creates repeated cross validation folds, 25 train/test folds
#train every model using repeated 5-fold cross validation
ctrl  <- trainControl(method = "repeatedcv", number = 5, repeats = 5, index = folds, #saves probabilities 
                      classProbs = TRUE, summaryFunction = twoClassSummary, #ROC AUC, Sensitivity and Specificity
                      savePredictions = "final") 
#Define model formula (every classifier uses same predictors)
fml   <- net ~ coverage + incidence + loa + cost_per_person_treated
#Train models
m_lda <- train(fml, df, method = "lda", metric = "ROC", trControl = ctrl)
m_qda <- train(fml, df, method = "qda", metric = "ROC", trControl = ctrl)
m_rf  <- train(fml, df, method = "rf",  metric = "ROC", trControl = ctrl,
               tuneGrid = data.frame(mtry = 2), ntree = 500) #At every tree split tree randomly considers 2 of the 4 predictors (randomness)
#Extract best performance (highest ROC)
pick <- function(m, col) m$results[[col]][which.max(m$results$ROC)]
#Build comparison table
comparison <- data.frame(
    model = c("LDA", "QDA", "RandomForest"),
    AUC   = round(c(pick(m_lda,"ROC"),  pick(m_qda,"ROC"),  pick(m_rf,"ROC")),  3),
    Sensitivity = round(c(pick(m_lda,"Sens"), pick(m_qda,"Sens"), pick(m_rf,"Sens")), 3), #how well it identifies beneficial
    Specificity = round(c(pick(m_lda,"Spec"), pick(m_qda,"Spec"), pick(m_rf,"Spec")), 3)) #how well it identifies non-beneficial
cat("\n=== Classifier comparison (5x5-fold CV; YES = beneficial) ===\n")
print(comparison, row.names = FALSE)
#Save table
write.csv(comparison, file.path(thr_dir, "model_comparison.csv"), row.names = FALSE)

# overlaid ROC curves, each model has its own ROC curve
roc_of <- function(m) pROC::roc(response = m$pred$obs, predictor = m$pred$YES,
                                levels = c("NO", "YES"), direction = "<")
r_lda <- roc_of(m_lda); r_qda <- roc_of(m_qda); r_rf <- roc_of(m_rf)
png(file.path(thr_dir, "roc_comparison.png"), width = 1100, height = 1000, res = 160)
plot(r_lda, col = "#1b9e77", lwd = 2.4, legacy.axes = TRUE)
plot(r_qda, col = "#d95f02", lwd = 2.4, add = TRUE)
plot(r_rf,  col = "#7570b3", lwd = 2.4, add = TRUE)
legend("bottomright", bty = "n", lwd = 2.4,
       col = c("#1b9e77", "#d95f02", "#7570b3"),
       legend = c(sprintf("LDA (AUC %.3f)", pick(m_lda, "ROC")),
                  sprintf("QDA (AUC %.3f)", pick(m_qda, "ROC")),
                  sprintf("RF  (AUC %.3f)", pick(m_rf,  "ROC"))))
dev.off()


## 6. HOLD-OUT 60/40 TRAIN/TEST SPLIT (cross-check alongside the CV above)
#for comparison.

set.seed(123)
#selects 60% of rows for training, keeping proprotion of outcomes, stratified splits between both datasets
idx   <- caret::createDataPartition(df$net, p = 0.60, list = FALSE)
#train and test datasets
train <- df[idx, ]; test <- df[-idx, ]
#accuracy correct/total
acc   <- function(pred, actual) mean(pred == actual)
cat(sprintf("\n=== Hold-out split: %d train / %d test (test-set results) ===\n",
            nrow(train), nrow(test)))

#fits LDA on training data, use trained model to predict test data
lda_te <- predict(MASS::lda(fml, data = train), test)$class
cat(sprintf("\nLDA  test accuracy: %.2f\n", acc(lda_te, test$net)))
print(table(Predicted = lda_te, Actual = test$net))

#fits QDA
qda_te <- predict(MASS::qda(fml, data = train), test)$class
cat(sprintf("\nQDA  test accuracy: %.2f\n", acc(qda_te, test$net)))
print(table(Predicted = qda_te, Actual = test$net))

#fits RF
rf_te <- predict(randomForest(fml, data = train, ntree = 500), test)
cat(sprintf("\nRF   test accuracy: %.2f\n", acc(rf_te, test$net)))
print(table(Predicted = rf_te, Actual = test$net))


## 7. EXTRACTABLE RULE (shallow classfication tree) transferable summary, limited to three levels

tree <- rpart(net ~ coverage + incidence + loa + cost_per_person_treated, data = df,
              method = "class", control = rpart.control(maxdepth = 3, minbucket = 6, cp = 0.01)) #at least 6 observation,cp to prevent overfitting
cat("\n=== Transferable rule (decision tree) ===\n"); print(tree)
png(file.path(thr_dir, "rule_tree.png"), width = 1500, height = 1000, res = 160)
rpart.plot(tree, type = 2, extra = 104, box.palette = "RdBu")
dev.off()

## 7b. How many are beneficial at different coverage thresholds 
# This scans a range of candidate cut-points and reports the proportion of regions that are
#  net-beneficial above and below each. If the selected split is real, the
#  separation should be sharp there and degrade smoothly on either side.
#
#  Descriptive only: nothing here feeds the reported rule, which comes from the
#  tree itself

# report the fitted root split for comparison with the scan below
root_var  <- as.character(tree$frame$var[1])
COV_SPLIT <- if (root_var == "coverage") unname(tree$splits[1, "index"]) else NA_real_
cat(sprintf("\nTree root split: %s >= %.3f\n", root_var, COV_SPLIT))


thresh_tbl <- do.call(rbind, lapply(c(0.20, 0.22, 0.30, 0.35, 0.40, 0.50), function(t) {
    hi <- df[df$coverage >= t, ]; lo <- df[df$coverage <  t, ]
    data.frame(threshold_pct   = 100 * t,
               n_at_or_above   = nrow(hi),
               pct_beneficial_above = round(100 * mean(hi$net == "YES"), 1),
               n_below         = nrow(lo),
               pct_beneficial_below = round(100 * mean(lo$net == "YES"), 1))
}))
cat("\n=== Beneficial % above/below candidate coverage thresholds ===\n")
print(thresh_tbl, row.names = FALSE)
write.csv(thresh_tbl, file.path(thr_dir, "coverage_threshold_scan.csv"), row.names = FALSE)


## 7c. COVERAGE MARGIN BAND  -> Table S5
#  The band immediately above the tree's coverage split, where the outcome is
#  NOT already determined by coverage. Below the split nothing is beneficial, so
#  a comparison there would be uninformative; within this band cost is what
#  separates the two groups. COV_SPLIT is read from the fitted tree above, so
#  this table cannot drift from the rule reported in the text. The 0.35 upper
#  bound is a presentational choice.
MARGIN_HI <- 0.35
margin <- df[df$coverage >= COV_SPLIT & df$coverage < MARGIN_HI, ]
margin_tbl <- do.call(rbind, lapply(c("YES", "NO"), function(k) {
    g <- margin[margin$net == k, ]
    data.frame(Net = k, N = nrow(g),
               median_cost_pp   = round(median(g$cost_per_person_treated), 3),
               min_cost_pp      = round(min(g$cost_per_person_treated), 3),
               max_cost_pp      = round(max(g$cost_per_person_treated), 3),
               median_incidence = round(median(g$incidence)))
}))
cat(sprintf("\n=== Coverage margin band (%.0f-%.0f%% coverage; n = %d) ===\n",
            100 * COV_SPLIT, 100 * MARGIN_HI, nrow(margin)))
print(margin_tbl, row.names = FALSE)
cat(sprintf("Fold difference in median cost per person treated: %.1fx\n",
            margin_tbl$median_cost_pp[margin_tbl$Net == "NO"] /
            margin_tbl$median_cost_pp[margin_tbl$Net == "YES"]))
write.csv(margin_tbl, file.path(thr_dir, "coverage_margin_band.csv"), row.names = FALSE)


## 7d. TERMINAL NODES OF THE TREE  -> Table S6
#  Every region is assigned to a leaf of the FITTED tree via tree$where, so this
#  table follows whatever tree was actually grown. If the tree changes - a
#  different split, a different number of leaves - the table changes with it 

df$leaf <- tree$where
node_tbl <- do.call(rbind, lapply(sort(unique(df$leaf)), function(L) {
    g <- df[df$leaf == L, ]
    data.frame(leaf = L,
               regions             = nrow(g),
               net_beneficial      = sum(g$net == "YES"),
               pct_beneficial      = round(100 * mean(g$net == "YES")),
               median_coverage_pct = round(100 * median(g$coverage), 1),
               median_cost_pp      = round(median(g$cost_per_person_treated), 3),
               median_incidence    = round(median(g$incidence)))
}))
node_tbl <- node_tbl[order(node_tbl$median_coverage_pct), ]
cat("\n=== Terminal nodes of the classification tree (n =", sum(node_tbl$regions), ") ===\n")
print(node_tbl, row.names = FALSE)
cat("\nRules defining each leaf:\n")
print(rpart.plot::rpart.rules(tree, cover = TRUE))
write.csv(node_tbl, file.path(thr_dir, "tree_terminal_nodes.csv"), row.names = FALSE)


## 8. INTERACTIVE 3D  — three epi covariates, coloured by NMB,
#    marker size = cost per person (so all 4 inputs are visible)

cmax <- as.numeric(quantile(abs(df$nmb_M), 0.95)) #95th percentiles of the absolute NMB values, colour focuses on range and ignores outliers
fig <- plot_ly(
    df, x = ~coverage * 100, y = ~incidence, z = ~loa * 100, #converts coverage and loa to percentage
    type = "scatter3d", mode = "markers",
    marker = list(size = ~4 + 12 * sqrt(pmin(cost_per_person_treated, 1)), #cost per person marker size, capped at 1 to easily view
                  color = ~nmb_M, colorscale = "RdBu", cmid = 0, #marker colour NMB
                  cmin = -cmax, cmax = cmax, showscale = TRUE, #NMB scale centred at 0
                  colorbar = list(title = "NMB (US$M)"),
                  line = list(width = 0.5, color = "grey")),
    text = ~paste0(country, " — ", region,
                   "<br>Coverage ", round(coverage*100), "%  Incidence ", round(incidence),
                   "/1k  Loa ", round(loa*100,1), "%",
                   "<br>Cost/person $", round(cost_per_person_treated,2), "  NMB $", round(nmb_M,1), "M"),
    hoverinfo = "text") %>%
    layout(title = "NMB across the three epi covariates (marker size = cost per person)",
           scene = list(xaxis = list(title = "Ivermectin coverage (%)"),
                        yaxis = list(title = "Malaria incidence (/1,000)"),
                        zaxis = list(title = "Loa prevalence (%)")))
htmlwidgets::saveWidget(fig, file.path(thr_dir, "nmb_3d.html"), selfcontained = TRUE)

write.csv(df, file.path(thr_dir, "rf_input_realistic.csv"), row.names = FALSE)
cat("\nSaved: rf_importance.png, rf_partial_dependence.png, model_comparison.csv,",
    "roc_comparison.png, rule_tree.png, coverage_band_summary.csv,",
    "tree_rule_table.csv, covariate_by_outcome.csv to", thr_dir, "\n")


## 9. TERNARY PLOTS  (three covariates at once, NMB as binned colour)
#Three covariates are min-max scaled to 0-1 to account for their different
#scales, then normalised to sum to 1, so position reflects the relative
#contribution of each covariate to a region's profile rather than its absolute
#value. This follows the standard ternary construction.
#NMB is shown in discrete bands to capture both direction and magnitude of benefit.
#Plot A: coverage / incidence / Loa loa prevalence (used in paper)
#Plot B: coverage / incidence / cost per person treated (for reference, but hard to read)

if (!requireNamespace("ggtern", quietly = TRUE)) {
    message("Package 'ggtern' not installed - skipping ternary plots. install.packages('ggtern')")
} else {
    library(ggtern)   # NOTE: ggtern masks some ggplot2 functions; kept last in the script

    # discrete NMB bands: red below zero, then a bright ramp upward
    TERN_BREAKS <- c(-Inf, -1, 0, 5, 10, 15, 20, 30, Inf)
    TERN_LABELS <- c("< -1", "-1 - 0", "0 - 5", "5 - 10",
                     "10 - 15", "15 - 20", "20 - 30", "> 30")
    TERN_PAL <- c("#B2182B", "#EF8A62", "#FDE725", "#A0DA39",
                  "#4AC16D", "#1FA187", "#277F8E", "#365C8D")
    names(TERN_PAL) <- TERN_LABELS

    # min-max rescale to 0-1
    minmax <- function(x) (x - min(x, na.rm = TRUE)) /
                          (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

    make_ternary <- function(right_var, right_lab, file_name) {
        d <- df[is.finite(df[[right_var]]) & is.finite(df$nmb_M), ]

        # min-max scale the three variables to be between 0 and 1
        plot_data_scaled <- data.frame(
            coverage  = minmax(d$coverage),
            incidence = minmax(d$incidence),
            right     = minmax(d[[right_var]]),
            nmb_M     = d$nmb_M,
            region    = d$region
        )
        # normalise so the three coordinates sum to 1
        plot_data_scaled$Sum <- plot_data_scaled$coverage +
                                plot_data_scaled$incidence +
                                plot_data_scaled$right
        plot_data_scaled$coverage  <- plot_data_scaled$coverage  / plot_data_scaled$Sum
        plot_data_scaled$incidence <- plot_data_scaled$incidence / plot_data_scaled$Sum
        plot_data_scaled$right     <- plot_data_scaled$right     / plot_data_scaled$Sum

        # NMB in discrete bands (instead of a yes/no beneficial factor)
        plot_data_scaled$nmb_band <- cut(plot_data_scaled$nmb_M, breaks = TERN_BREAKS,
                                         labels = TERN_LABELS, right = TRUE,
                                         include.lowest = TRUE)

        g <- ggtern(data = plot_data_scaled,
                    aes(x = coverage, y = incidence, z = right, color = nmb_band)) +
            geom_point(size = 3, alpha = 0.8) +
            labs(x = "", xarrow = "Ivermectin coverage",
                 y = "", yarrow = "Malaria incidence per 1000",
                 z = "", zarrow = right_lab) +
            scale_color_manual(values = TERN_PAL, drop = FALSE,
                               name = "Net monetary\nbenefit (US$M)") +
            theme_minimal() +
            theme_showarrows() +
            theme_clockwise()
        print(g)
        ggsave(file.path(thr_dir, file_name), g, width = 9, height = 7.5, dpi = 300)
        cat("Saved", file.path(thr_dir, file_name), "\n")
    }

    make_ternary("loa", "Loa loa prevalence", "ternary_nmb_loa.png")
    make_ternary("cost_per_person_treated", "Cost per person (US$)", "ternary_nmb_cost.png")

    # regions in the top band, for reference
    top <- df[df$nmb_M > 30, c("country","region","coverage","incidence",
                               "loa","cost_per_person_treated","nmb_M")]
    if (nrow(top)) {
        top <- top[order(-top$nmb_M), ]
        cat("\n=== Regions in the top NMB band (> $30M) ===\n"); print(top, row.names = FALSE)
        write.csv(top, file.path(thr_dir, "ternary_top_band_regions.csv"), row.names = FALSE)
    }
    par(mfrow = c(1, 1))   # reset plotting layout
}


## 10. REFERENCE REGION FOR THE COST-CEILING GRID
#  Observed NMB scales with population, so large regions would dominate any
#  coverage-by-incidence surface. Holding cost inputs and Loa loa at their median across the 202
#  regions, so that coverage and incidence can be swept independently of region
#  size. This reference region feeds the cost-ceiling grid in section 11.

surf_dir <- thr_dir
POP_REF  <- 1e6          # reference population (cancels out once per-capita)

# representative region: median cost inputs and Loa loa across the 202 regions
ref <- data[1, , drop = FALSE]
for (v in c("cost_inpatient_day","cost_outpatient","unit_cost_uncomplicated",
            "unit_cost_severe","dalys_malaria_per_case","loa_loa_prev",
            "planning_cost","programme_cost","est_uncomplicated","est_severe"))
    ref[[v]] <- median(data[[v]], na.rm = TRUE)
# scale the per-region cost lumps to the reference population
ref$planning_cost  <- median(data$planning_cost  / data$total_pop, na.rm = TRUE) * POP_REF
ref$programme_cost <- median(data$programme_cost / data$total_pop, na.rm = TRUE) * POP_REF
ref$total_pop      <- POP_REF

cov_grid <- seq(0, 1,   by = 0.02)     # ivermectin coverage
inc_grid <- seq(0, 450, by = 10)       # malaria incidence per 1,000

p_surf <- make_params(list(IDE = 0.125, IAEp = 1.30, ISAEp = 1.30, cfr_SAE = 0.05))


## 11. MAXIMUM AFFORDABLE DELIVERY COST  (cost ceiling grid -> Doc S2)
#  "For regions with similar coverage and incidence, what difference in cost
#   makes escalation beneficial rather than not?"
#
#  Every extra $1 per person of delivery cost reduces NMB by exactly $1 per
#  person treated, so the BREAK-EVEN cost is simply the NMB per person treated computed with all
#  delivery costs set to zero. That value is the maximum a programme can spend
#  per person treated before escalation stops being worthwhile, for a given
#  coverage and incidence. Regions below their ceiling are net-beneficial; above
#  it, net-cost.

ceil_grid <- do.call(rbind, lapply(cov_grid, function(cv) {
    d <- ref[rep(1, length(inc_grid)), ]
    d$n_treated   <- cv * POP_REF
    d$total_cases <- inc_grid / 1000 * POP_REF
    epi  <- f_epi(d, p_surf); ae <- f_AE(d, p_surf); sae <- f_SAE(d, p_surf)
    cst  <- f_cost(epi, ae, sae, d, p_surf,
                   planning_cost = 0, programme_cost = 0)   # <- zero delivery cost
    # At zero coverage nobody is treated and the ceiling is
    # undefined; it is set to NA rather than dividing by zero.
    n_tr <- cv * POP_REF
    data.frame(coverage = cv, incidence = inc_grid,
               max_cost_pp = if (n_tr > 0)
                                 (wtp * cst$net_dalys - cst$net_cost) / n_tr
                             else NA_real_)
}))
ceil_grid$max_cost_pp[!is.na(ceil_grid$max_cost_pp) &
                      ceil_grid$max_cost_pp < 0] <- 0   # never affordable

# lookup table at representative combinations
tab <- ceil_grid[ceil_grid$coverage %in% c(0.20,0.30,0.40,0.50,0.60,0.70,0.80) &
                 ceil_grid$incidence %in% c(100,200,300,400), ]
tab$coverage_pct <- 100 * tab$coverage
tab <- tab[order(tab$coverage, tab$incidence), c("coverage_pct","incidence","max_cost_pp")]
tab$max_cost_pp <- round(tab$max_cost_pp, 3)
cat("\n=== Maximum affordable delivery cost per person (US$) ===\n")
print(reshape(tab, idvar = "coverage_pct", timevar = "incidence", direction = "wide"),
      row.names = FALSE)
write.csv(ceil_grid, file.path(surf_dir, "max_affordable_cost_grid.csv"), row.names = FALSE)

# how each observed region compares with its own ceiling
near <- function(x, grid) grid[which.min(abs(grid - x))]
df$ceiling_pp <- mapply(function(cv, ii) {
    ceil_grid$max_cost_pp[ceil_grid$coverage == near(cv, cov_grid) &
                          ceil_grid$incidence == near(ii, inc_grid)][1]
}, pmin(df$coverage, 1), df$incidence)
# Like for like: ceiling_pp is the maximum affordable DELIVERY cost per person
# treated, so it must be compared against delivery cost, not against the total
# incremental cost (which also contains drug and adverse-event costs already
# accounted for inside the ceiling).
df$headroom_pp <- df$ceiling_pp - df$delivery_cost_pp            # >0 = affordable
cat(sprintf("\nRegions currently spending BELOW their affordable ceiling: %d of %d\n",
            sum(df$headroom_pp > 0, na.rm = TRUE), nrow(df)))
write.csv(df[order(df$headroom_pp),
             c("country","region","coverage","incidence",
               "delivery_cost_pp","cost_per_person_treated",
               "ceiling_pp","headroom_pp","nmb_M")],
          file.path(surf_dir, "region_cost_headroom.csv"), row.names = FALSE)



## 12. MATCHED PAIRS: same epidemiology, different delivery cost
#  Isolates the effect of cost by pairing regions with near-identical coverage
#  and incidence but opposite outcomes. Any difference in net benefit within a
#  pair is attributable to delivery cost rather than to epidemiology.
COV_TOL <- 0.05    # coverage within 5 percentage points
INC_TOL <- 25      # incidence within 25 cases per 1,000

# Restrict to regions above the tree's coverage floor: below it nothing is
# beneficial, so a matched pair there would not isolate cost. COV_SPLIT is read
# from the fitted tree in section 7b, so this cannot drift from the reported rule.
COV_FLOOR <- if (!is.na(COV_SPLIT)) COV_SPLIT else 0.22
ben  <- df[df$net == "YES" & df$coverage > COV_FLOOR, ]
cst_ <- df[df$net == "NO"  & df$coverage > COV_FLOOR, ]
pairs <- do.call(rbind, lapply(seq_len(nrow(ben)), function(i) {
    a <- ben[i, ]
    m <- cst_[abs(cst_$coverage - a$coverage) <= COV_TOL &
              abs(cst_$incidence - a$incidence) <= INC_TOL, ]
    if (!nrow(m)) return(NULL)
    do.call(rbind, lapply(seq_len(nrow(m)), function(j) {
        b <- m[j, ]
        data.frame(
            beneficial_country = a$country, beneficial_region = a$region,
            ben_coverage = round(100 * a$coverage, 1), ben_incidence = round(a$incidence),
            ben_cost_pp  = round(a$cost_per_person_treated, 4), ben_nmb_M = round(a$nmb_M, 2),
            netcost_country = b$country, netcost_region = b$region,
            nc_coverage = round(100 * b$coverage, 1), nc_incidence = round(b$incidence),
            nc_cost_pp  = round(b$cost_per_person_treated, 4), nc_nmb_M = round(b$nmb_M, 2),
            cost_ratio  = round(b$cost_per_person_treated / a$cost_per_person_treated, 1),
            match_dist  = abs(a$coverage - b$coverage) + abs(a$incidence - b$incidence) / 500)
    }))
}))
if (!is.null(pairs) && nrow(pairs)) {
    # keep only pairs where the net-cost region really is the more expensive one
    pairs <- pairs[pairs$cost_ratio > 1, ]
    pairs <- pairs[order(pairs$match_dist), ]
    cat("\n=== Matched pairs: similar coverage & incidence, opposite outcome ===\n")
    print(head(pairs[, c("beneficial_country","beneficial_region","ben_coverage","ben_incidence",
                         "ben_cost_pp","ben_nmb_M","netcost_country","netcost_region",
                         "nc_coverage","nc_incidence","nc_cost_pp","nc_nmb_M","cost_ratio")], 6),
          row.names = FALSE)
    write.csv(pairs, file.path(thr_dir, "matched_pairs_cost.csv"), row.names = FALSE)
    cat("Wrote", file.path(thr_dir, "matched_pairs_cost.csv"), "\n")
}


## 13. SUMMARY STATISTICS BEHIND THE TERNARY PLOTS
#  Both the ABSOLUTE values and the normalised ternary SHARES are reported, since the
#  ternary axes show relative position while the text quotes absolute values.

# min-max rescale, matching the ternary plots in section 9
rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) return(rep(0, length(x)))
    (x - rng[1]) / diff(rng)
}
tern_df <- df
tern_df$s_cov <- rescale01(tern_df$coverage)
tern_df$s_inc <- rescale01(tern_df$incidence)
tern_df$s_loa <- rescale01(tern_df$loa)
tot <- tern_df$s_cov + tern_df$s_inc + tern_df$s_loa
tern_df$share_cov <- tern_df$s_cov / tot
tern_df$share_inc <- tern_df$s_inc / tot
tern_df$share_loa <- tern_df$s_loa / tot

# (a) net-cost vs net-beneficial
# Medians AND means are both reported. The two tell different
# stories for cost: the median is the typical region, while the mean is pulled
# by a few regions where a largely fixed programme cost divides across very few
# treated people. Loa loa is median-zero in both groups, so only its mean is
# informative - hence both are needed.
tern_binary <- tern_df %>% group_by(net) %>%
    summarise(n = n(),
              median_coverage_pct = round(100 * median(coverage), 1),
              mean_coverage_pct   = round(100 * mean(coverage), 1),
              median_incidence    = round(median(incidence), 0),
              mean_incidence      = round(mean(incidence), 0),
              median_loa_pct      = round(100 * median(loa), 1),
              mean_loa_pct        = round(100 * mean(loa), 1),
              median_cost_pp      = round(median(cost_per_person_treated), 3),
              mean_cost_pp        = round(mean(cost_per_person_treated), 3),
              share_coverage      = round(mean(share_cov), 3),
              share_incidence     = round(mean(share_inc), 3),
              share_loa           = round(mean(share_loa), 3), .groups = "drop")
cat("\n=== Ternary summary: net-cost vs net-beneficial ===\n")
print(as.data.frame(tern_binary), row.names = FALSE)

# (b) by band of net monetary benefit
tern_df$nmb_group <- cut(tern_df$nmb_M, breaks = c(-Inf, 0, 5, 15, Inf),
                         labels = c("net-cost", "0-5", "5-15", ">15"))
tern_bands <- tern_df %>% group_by(nmb_group) %>%
    summarise(n = n(),
              mean_coverage_pct = round(100 * mean(coverage), 1),
              mean_incidence    = round(mean(incidence), 0),
              mean_loa_pct      = round(100 * mean(loa), 1),
              mean_cost_pp      = round(mean(cost_per_person_treated), 3),
              share_coverage    = round(mean(share_cov), 3),
              share_loa         = round(mean(share_loa), 3), .groups = "drop")
cat("\n=== Ternary summary by NMB band ===\n")
print(as.data.frame(tern_bands), row.names = FALSE)

# (c) high-Loa regions: how many are still beneficial
hi_loa <- tern_df[tern_df$loa > 0.20, ]
cat(sprintf("\nRegions with Loa loa prevalence > 20%%: %d, of which %d (%.0f%%) net-beneficial\n",
            nrow(hi_loa), sum(hi_loa$net == "YES"),
            100 * mean(hi_loa$net == "YES")))

write.csv(tern_binary, file.path(thr_dir, "ternary_summary_binary.csv"), row.names = FALSE)
write.csv(tern_bands,  file.path(thr_dir, "ternary_summary_bands.csv"),  row.names = FALSE)
write.csv(tern_df[, c("country","region","coverage","incidence","loa","cost_per_person_treated",
                      "nmb_M","net","share_cov","share_inc","share_loa")],
          file.path(thr_dir, "ternary_region_values.csv"), row.names = FALSE)
cat("Wrote ternary_summary_binary.csv, ternary_summary_bands.csv and ternary_region_values.csv to",
    thr_dir, "\n")

