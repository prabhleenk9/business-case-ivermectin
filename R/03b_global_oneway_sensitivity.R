#### 03b - GLOBAL (UNRESTRICTED) ONE-WAY SENSITIVITY ANALYSIS
# ------------------------------------------------------------
# Here each parameter is varied across the FULL range spanned by all three
# scenarios, with every other parameter held at its central value. This shows
# how sensitive the model is to each input in absolute terms, independent of
# any scenario framing.
#
# Outputs: net cost, net DALYs and NMB, on readable (millions / thousands) scales.

# Paths are relative to the project root. Open business-case-ivermectin.Rproj
# so the working directory is set correctly.
if (!file.exists("data2.xlsx"))
    stop("Working directory is not the project root. Open ",
         "business-case-ivermectin.Rproj, or setwd() to the folder ",
         "containing data2.xlsx.")

#Source the model
source("R/00_model_setup.R")

## 1. PACKAGES

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# output folder for figures
out_dir <- "outputs"
dir.create(out_dir, showWarnings = FALSE)

#Load data
data_g <- data   # planning_cost + programme_cost attached in 00_model_setup.R
stopifnot(sum(is.na(data_g$planning_cost)) == 0)


## 2. RUN WRAPPER 
# Planning and programme costs both applied and both scalable.

run_model_g <- function(p, data) {
    pm  <- if (!is.null(p$plan_mult)) p$plan_mult else 1
    pgm <- if (!is.null(p$prog_mult)) p$prog_mult else 1

    epi  <- f_epi(data, p)
    ae   <- f_AE(data, p)
    sae  <- f_SAE(data, p)
    cost <- f_cost(epi, ae, sae, data, p,
                   planning_cost  = pm  * data$planning_cost,
                   programme_cost = pgm * data$programme_cost)

    nc <- sum(cost$net_cost,  na.rm = TRUE)
    nd <- sum(cost$net_dalys, na.rm = TRUE)
    c(net_cost = nc, net_dalys = nd, nmb = wtp * nd - nc)
}


## 3. GLOBAL RANGES 
# Union of the three scenario ranges. Middle value = base case.

global_ranges <- list(
    IDE          = c(0.00, 0.125, 0.25),   # no extra benefit -> upper plausible efficacy
    IAEp         = c(1.00, 1.13,  2.00),   # no extra AE risk -> doubling
    ISAEp        = c(1.00, 1.13,  2.00),   # no extra SAE risk -> doubling
    cfr_SAE      = c(0.02, 0.058, 0.10),   # SAE case fatality
    plan_mult    = c(0.60, 1.00,  1.50),   # planning cost multiplier
    prog_mult    = c(0.00, 1.00,  1.50),   # programme cost multiplier
    ivm_cost_400 = c(0.00, 0.20,  0.40)    # donated -> highest procurement cost
)

# Central base case: model defaults, full delivery costs, drug donated.
base_overrides <- list(plan_mult = 1, prog_mult = 1)
base_out <- run_model_g(make_params(base_overrides), data_g)
print(base_out)

one_way_g <- do.call(rbind, lapply(names(global_ranges), function(param) {
    vals <- global_ranges[[param]]
    do.call(rbind, lapply(seq_along(vals), function(i) {
        ov  <- modifyList(base_overrides, setNames(list(vals[i]), param))
        out <- run_model_g(make_params(ov), data_g)
        data.frame(parameter = param,
                   value     = vals[i],
                   level     = c("low", "base", "high")[i],
                   t(out),
                   stringsAsFactors = FALSE)
    }))
}))

write.csv(one_way_g, file.path(out_dir, "global_oneway_sensitivity.csv"), row.names = FALSE)


## 4. TORNADO DATA 
# Rescale so the axes are readable: money in millions, DALYs in thousands.

scale_map <- c(net_cost = 1e6, net_dalys = 1e3, nmb = 1e6)
axis_lab  <- c(net_cost  = "Net cost (US$ millions)",
               net_dalys = "Net DALYs averted (thousands)",
               nmb       = "Net monetary benefit (US$ millions)")

base_df <- data.frame(output = names(base_out),
                      base   = as.numeric(base_out),
                      stringsAsFactors = FALSE)

tor <- one_way_g %>%
    pivot_longer(c(net_cost, net_dalys, nmb),
                 names_to = "output", values_to = "val") %>%
    left_join(base_df, by = "output") %>%
    filter(level != "base") %>%
    mutate(direction = ifelse(level == "low", "parameter low", "parameter high"),
           val_s     = val  / scale_map[output],
           base_s    = base / scale_map[output],
           lo        = pmin(val_s, base_s),
           hi        = pmax(val_s, base_s),
           out_lab   = factor(axis_lab[output], levels = unname(axis_lab)))

# order parameters by total swing in NMB (the primary decision metric)
ord <- tor %>%
    filter(output == "nmb") %>%
    group_by(parameter) %>%
    summarise(swing = max(val_s) - min(val_s), .groups = "drop") %>%
    arrange(swing) %>%
    pull(parameter)
tor$parameter <- factor(tor$parameter, levels = ord)

base_lines <- distinct(tor, out_lab, base_s)

# swing table (useful for quoting in the text)
swing_tbl <- tor %>%
    group_by(out_lab, parameter) %>%
    summarise(low = val_s[direction == "parameter low"][1],
              high = val_s[direction == "parameter high"][1],
              base = base_s[1],
              swing = max(val_s) - min(val_s), .groups = "drop") %>%
    arrange(out_lab, desc(swing))
write.csv(swing_tbl, file.path(out_dir, "global_oneway_swings.csv"), row.names = FALSE)
print(as.data.frame(swing_tbl))


## 5. PLOT

pal_dir <- c("parameter low" = "#3C5488FF", "parameter high" = "#E64B35FF")

tornado_global <- ggplot(tor, aes(y = parameter)) +
    geom_linerange(aes(xmin = lo, xmax = hi, colour = direction),
                   linewidth = 7, alpha = 0.95) +
    geom_vline(data = base_lines, aes(xintercept = base_s),
               linetype = "dashed", linewidth = 0.4) +
    facet_wrap(~ out_lab, scales = "free_x", nrow = 1) +
    scale_colour_manual(name = NULL, values = pal_dir) +
    scale_x_continuous(labels = label_number(accuracy = 1, big.mark = ","),
                       n.breaks = 5,
                       expand = expansion(mult = 0.10)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position  = "top",
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.spacing    = unit(1.1, "lines"),
          strip.text       = element_text(face = "bold", size = 11),
          axis.text.y      = element_text(face = "bold", size = 10),
          axis.text.x      = element_text(size = 9),
          plot.margin      = ggplot2::margin(6, 14, 6, 6))

print(tornado_global)
ggsave(file.path(out_dir, "global_oneway_tornado_all_outputs.png"),
       tornado_global, width = 12, height = 4.6, dpi = 300, bg = "white")


## 6. INDIVIDUAL PANELS 

for (tgt in names(axis_lab)) {
    # base subsetting rather than dplyr::filter(): the tidy-eval version can trip
    # dplyr's `by` typo guard on some versions when filtering on a column called
    # `output` against a loop variable. Same result, no dispatch.
    d <- tor[tor$output == tgt, ]
    p <- ggplot(d, aes(y = parameter)) +
        geom_linerange(aes(xmin = lo, xmax = hi, colour = direction),
                       linewidth = 8, alpha = 0.95) +
        geom_vline(xintercept = d$base_s[1], linetype = "dashed", linewidth = 0.4) +
        scale_colour_manual(name = NULL, values = pal_dir) +
        scale_x_continuous(labels = label_number(accuracy = 1, big.mark = ","),
                           n.breaks = 6, expand = expansion(mult = 0.10)) +
        labs(x = axis_lab[[tgt]], y = NULL) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top",
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.text.y = element_text(face = "bold"))
    print(p)
    ggsave(file.path(out_dir, paste0("global_oneway_tornado_", tgt, ".png")),
           p, width = 7, height = 4.2, dpi = 300, bg = "white")
}

message("Done. Base case: net cost ", round(base_out["net_cost"]/1e6, 1),
        "M | net DALYs ", round(base_out["net_dalys"]/1e3, 1),
        "k | NMB ", round(base_out["nmb"]/1e6, 1), "M")


## 7. IDE-CONDITIONAL ANALYSIS 
# Holding IDE fixed at successive levels, each remaining parameter is varied
# across the SAME global ranges used above. This shows which secondary
# parameters matter once efficacy is controlled for.
#
# Reported as % change from the base case AT THAT IDE LEVEL, because the base
# value differs by orders of magnitude between levels and raw units are not
# comparable across panels. 
# NOTE: net cost is negative (cost-saving), so the denominator is |base|.
# Without the absolute value the sign of the percentage change inverts and
# "parameter high" would appear to reduce cost.

IDE_levels <- c(0.00, 0.125, 0.25)
secondary  <- global_ranges[setdiff(names(global_ranges), "IDE")]

cond <- do.call(rbind, lapply(IDE_levels, function(ide) {
    base_ide <- run_model_g(make_params(modifyList(base_overrides, list(IDE = ide))), data_g)
    do.call(rbind, lapply(names(secondary), function(param) {
        vals <- secondary[[param]]
        do.call(rbind, lapply(seq_along(vals), function(i) {
            ov  <- modifyList(base_overrides, list(IDE = ide))
            ov  <- modifyList(ov, setNames(list(vals[i]), param))
            out <- run_model_g(make_params(ov), data_g)
            data.frame(IDE       = ide,
                       parameter = param,
                       value     = vals[i],
                       level     = c("low", "base", "high")[i],
                       output    = names(out),
                       val       = unname(out),
                       base_val  = unname(base_ide[names(out)]),
                       stringsAsFactors = FALSE)
        }))
    }))
}))

cond <- cond %>%
    mutate(pct       = 100 * (val - base_val) / abs(base_val),
           direction = ifelse(level == "low", "parameter low", "parameter high"),
           IDE_lab   = factor(paste0("IDE = ", IDE),
                              levels = paste0("IDE = ", IDE_levels)))

write.csv(cond, file.path(out_dir, "global_conditional_ide_all_outputs.csv"), row.names = FALSE)

cond_plot_df <- cond[cond$level != "base", ]

pct_lab <- c(net_cost  = "Change in net cost from base case at that efficacy level (%)",
             net_dalys = "Change in net DALYs averted from base case at that efficacy level (%)",
             nmb       = "Change in net monetary benefit from base case at that efficacy level (%)")

for (tgt in names(pct_lab)) {

    d <- cond_plot_df[cond_plot_df$output == tgt, ]

    # order by largest absolute effect anywhere in this grid
    ord_c <- d %>%
        group_by(parameter) %>%
        summarise(m = max(abs(pct)), .groups = "drop") %>%
        arrange(m) %>%
        pull(parameter)
    d$parameter <- factor(d$parameter, levels = ord_c)

    # shared x scale across facets so magnitudes are comparable between IDE levels
    p_cond <- ggplot(d, aes(y = parameter)) +
        geom_linerange(aes(xmin = pmin(0, pct), xmax = pmax(0, pct), colour = direction),
                       linewidth = 7, alpha = 0.95) +
        geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
        facet_wrap(~ IDE_lab, nrow = 1) +
        scale_colour_manual(name = NULL, values = pal_dir) +
        scale_x_continuous(labels = label_number(accuracy = 1, suffix = "%"),
                           n.breaks = 5, expand = expansion(mult = 0.10)) +
        labs(x = pct_lab[[tgt]], y = NULL) +
        theme_minimal(base_size = 12) +
        theme(legend.position    = "top",
              panel.grid.minor   = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.spacing      = unit(1.1, "lines"),
              strip.text         = element_text(face = "bold", size = 11),
              axis.text.y        = element_text(face = "bold", size = 10),
              axis.title.x       = element_text(size = 10),
              plot.margin        = ggplot2::margin(6, 14, 6, 6))

    print(p_cond)
    ggsave(file.path(out_dir, paste0("global_conditional_ide_tornado_", tgt, ".png")),
           p_cond, width = 11, height = 4.2, dpi = 300, bg = "white")
}

# largest absolute % effect per parameter, per outcome, per IDE level
cond_summary <- cond_plot_df %>%
    group_by(output, IDE, parameter) %>%
    summarise(max_abs_pct = max(abs(pct)), .groups = "drop") %>%
    arrange(output, IDE, desc(max_abs_pct))
write.csv(cond_summary, file.path(out_dir, "global_conditional_ide_summary.csv"), row.names = FALSE)
print(as.data.frame(cond_summary))

