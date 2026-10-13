#### 09 - SPATIAL MAP OF NET BENEFIT and COVARIATES AT ADMIN-1 
# ------------------------------------------------------------
#  Shapefile, copied into this project:
#     maps/inputs/ESPEN_IU_2023/     -> IU polygons (has ADMIN0/ADMIN1)
#     maps/inputs/ESPEN_ADM0_2023/   -> country outlines + labels
#
#  Net benefit data: outputs/nmb_subnational region_scenarios.csv from 05,
#     columns country, subnational region, scenario, nmb_median, nmb_lo, nmb_hi
#     (per-region PSA summary). 

##### NET BENEFIT 

# Paths are relative to the project root. Open business-case-ivermectin.Rproj
# so the working directory is set correctly.
if (!file.exists("data2.xlsx"))
    stop("Working directory is not the project root. Open ",
         "business-case-ivermectin.Rproj, or setwd() to the folder ",
         "containing data2.xlsx.")

## 1. PACKAGES
# install.packages(c("sf", "dplyr", "stringi", "ggplot2", "ggrepel"))
library("sf")
library("dplyr")
library("ggplot2")
library("ggrepel")

# constants
IN_DIR  <- file.path("maps", "inputs")
OUT_DIR <- file.path("outputs", "maps")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

#For realistic (median) map used for results
FILE_nmb   <- file.path("outputs", "nmb_subnational_region_scenarios.csv") #input
NB_VALUE   <- "nmb_median"     # central net-benefit estimate to map (PSA median)
NB_SCEN    <- "Realistic"      # which scenario to map (Optimistic/Realistic/Pessimistic)
LEGEND_TTL <- "Net monetary\nbenefit (US$ millions)"

# fixed, round legend breaks; tails caught by +/-Inf
MAP_BREAKS  <- c(-Inf, -1, 0, 5, 10, 15, 20, 30, Inf) #create bins
# pastel diverging: pink (negative) -> soft blue (positive)
#   2 pink bins below 0, 6 blue bins above 0 (matches the 8 breaks above).

PAL <- c(
    "#D45B7A",   # < -1     
    "#F4A9C0",   # -1 - 0   
    "#DCEBF7",   # 0 - 5    
    "#B9D6EE",   # 5 - 10
    "#93BFE3",   # 10 - 15
    "#6BA3D6",   # 15 - 20
    "#4785C4",   # 20 - 30
    "#2A66A8"    # > 30     
)
PAL_LABELS <- c("< -1", "-1 – 0", "0 – 5", "5 – 10", #legend labels
                "10 – 15", "15 – 20", "20 – 30", "> 30")
NA_LABEL   <- "No MDA programme"   # white = admin-1 units not in the 202 subnational regions


## 2. NET BENEFIT DATA (admin-1 = subnational region)

nmb <- read.csv(FILE_nmb, header = TRUE, stringsAsFactors = FALSE)
to_plot <- nmb[nmb$scenario == NB_SCEN, c("country", "subnational_region", NB_VALUE)] #filter to scenario you want defined in NB_SCEN
names(to_plot) <- c("nb_country", "nb_subnational_region", "nb_value") #NB_VALUE also defined (median)
cat("Net-benefit rows for scenario '", NB_SCEN, "': ",
    nrow(to_plot), " admin-1 units\n", sep = "")


## 3. NAME NORMALISER  (accent-strip, lowercase, alphanumeric only)
#    handles non-breaking spaces (Uganda) and diacritics (CIV, etc.)
clean <- function(x) {
    x <- gsub("\u00a0", " ", x)                      # nbsp -> space
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")      # remove accents and special characters 
    gsub("[^a-z0-9]", "", tolower(trimws(x))) #removes everything except letters and numbers
}


## 4. COUNTRY MATCHING  (country name -> ESPEN ADMIN0 name)
#    Only the mismatches need listing, everything else matches as-is.

country_map <- c(
    "Democratic Republic of the Congo" = "Congo, DRC",
    "Sierra Leonne"                    = "Sierra Leone",        
    "Tanzania"                         = "Tanzania (Mainland);Tanzania (Zanzibar)"
)
# Create a lookup table: ESPEN ADMIN0 -> country name (to match)
espen0_to_nb <- list() #if countries appear in (country_map) use translated ESPEN name(s)
for (nbc in unique(to_plot$nb_country)) {
    esps <- if (nbc %in% names(country_map)) strsplit(country_map[[nbc]], ";")[[1]] else nbc #split countries with multiple entries
    for (e in esps) espen0_to_nb[[e]] <- nbc #fill the lookup table 
} 
sel_countries_espen <- names(espen0_to_nb) #extract all ESPEN country names that will be searched for in the shapefile


## 5. ADMIN-1 MATCHING  (ESPEN "ADMIN0|ADMIN1" -> subnational region name)
#    Covers: typos, "Region" suffixes, and Cote d'Ivoire's 31 regions
#    which roll up for 14 subnational regions. All keys/values are matched
#    by clean() so accents/case/spacing don't matter.

adm1_override <- c(
    # Guinea-Bissau: ESPEN splits Bijagos & Bolama; this unit combines them
    "Guinea-Bissau|Bijagos"                 = "Bijagos (Bolama)",
    "Guinea-Bissau|Bolama"                  = "Bijagos (Bolama)",
    # Liberia spelling
    "Liberia|Gbarpolu"                      = "Gbapolu",
    # Malawi "Region" suffix
    "Malawi|Central"                        = "Central Region",
    "Malawi|Southern"                       = "Southern Region",
    # Nigeria FCT
    "Nigeria|Federal Capital Territory"     = "Abuja (Federal Capital )",
    # Sierra Leone directions
    "Sierra Leone|East"                     = "Eastern",
    "Sierra Leone|North"                    = "Northern",
    "Sierra Leone|South"                    = "Southern",
    # South Sudan spelling
    "South Sudan|Warraps"                   = "Warrap",
    # Uganda "Region" suffix 
    "Uganda|Northern Region"                = "Northern",
    "Uganda|Western Region"                 = "Western",
    # Cote d'Ivoire: 31 ESPEN regions -> we have 14 subnational regions 
    "Cote d'Ivoire|Gbokle"                  = "Bas-Sassandra",
    "Cote d'Ivoire|Nawa"                    = "Bas-Sassandra",
    "Cote d'Ivoire|San Pedro"               = "Bas-Sassandra",
    "Cote d'Ivoire|Indenie-Djuablin"        = "Comoe",
    "Cote d'Ivoire|Sud-Comoe"               = "Comoe",
    "Cote d'Ivoire|Folon"                   = "Denguele",
    "Cote d'Ivoire|Kabadougou"              = "Denguele",
    "Cote d'Ivoire|Goh"                     = "Goh-Djiboua",
    "Cote d'Ivoire|Loh-Djiboua"             = "Goh-Djiboua",
    "Cote d'Ivoire|Belier"                  = "Lacs",
    "Cote d'Ivoire|Iffou"                   = "Lacs",
    "Cote d'Ivoire|Moronou"                 = "Lacs",
    "Cote d'Ivoire|N'Zi"                    = "Lacs",
    "Cote d'Ivoire|Agneby-Tiassa"           = "Lagunes",
    "Cote d'Ivoire|Grands Ponts"            = "Lagunes",
    "Cote d'Ivoire|Me"                      = "Lagunes",
    "Cote d'Ivoire|Cavally"                 = "Montagnes",
    "Cote d'Ivoire|Guemon"                  = "Montagnes",
    "Cote d'Ivoire|Tonkpi"                  = "Montagnes",
    "Cote d'Ivoire|Haut-Sassandra"          = "Sassandra-Marahoue",
    "Cote d'Ivoire|Marahoue"                = "Sassandra-Marahoue",
    "Cote d'Ivoire|Bagoue"                  = "Savanes",
    "Cote d'Ivoire|Poro"                    = "Savanes",
    "Cote d'Ivoire|Tchologo"                = "Savanes",
    "Cote d'Ivoire|Gbeke"                   = "Vallee du Bandama",
    "Cote d'Ivoire|Hambol"                  = "Vallee du Bandama",
    "Cote d'Ivoire|Bafing"                  = "Woroba",
    "Cote d'Ivoire|Bere"                    = "Woroba",
    "Cote d'Ivoire|Worodougou"              = "Woroba",
    "Cote d'Ivoire|Bounkani"                = "Zanzan",
    "Cote d'Ivoire|Gontougo"                = "Zanzan",
    "Cote d'Ivoire|District Autonome De Yamoussoukro" = "Yamoussoukro (Autonomous District)"
)
# clean() the override keys/values once
ovr_keys <- vapply(strsplit(names(adm1_override), "\\|"), #Splits Liberia|Gbarpolu into separate 
                   function(p) paste(clean(p[1]), clean(p[2]), sep = "|"),
                   character(1)) #converts into standard format
ovr_vals <- clean(unname(adm1_override))
override_lookup <- setNames(ovr_vals, ovr_keys) #Build lookup table


## 6. LOAD ESPEN SHAPEFILE + FILTER TO COUNTRIES

sf_iu <- sf::read_sf(file.path(IN_DIR, "ESPEN_IU_2023"), "ESPEN_IU_2023") #Load ESPEN shapefile
sf_iu <- sf_iu[sf_iu$ADMIN0 %in% sel_countries_espen, ] #Keep only your countries

# dissolve IUs up to ADMIN0 + ADMIN1 (one polygon per admin-1 unit)
sf::sf_use_s2(FALSE)   # avoids the s2 oriented-loop error on some geoms
sf_adm1 <- sf_iu %>% #merges IUs within the same Admin-1 region into a single polygon
    group_by(ADMIN0, ADMIN1) %>%
    summarise(.groups = "drop")

# match each ESPEN admin-1 -> country, subnational region 
sf_adm1$nb_country <- unlist(espen0_to_nb[sf_adm1$ADMIN0])
key <- paste(clean(sf_adm1$ADMIN0), clean(sf_adm1$ADMIN1), sep = "|")
mapped <- override_lookup[key]  # checks whether each Admin-1 regions appears in lookups, if not result is NA
sf_adm1$dist_clean <- ifelse(is.na(mapped), clean(sf_adm1$ADMIN1), mapped) #creates final cleaned region name used for matching, 
#and if override of names is needed



## 7. JOIN NET BENEFIT ONTO POLYGONS

to_plot$ctry_clean <- clean(to_plot$nb_country) #clean country names in data (used for matching)
to_plot$dist_clean <- clean(to_plot$nb_subnational_region) #cleaned version of subnational region names
sf_adm1$ctry_clean <- clean(sf_adm1$nb_country) #clean country names in shape file (earlier,translated ESPEN names to names used in data set (nb_country))

#Match each polygon with the NMB value
m <- merge(
    sf_adm1, to_plot[, c("ctry_clean", "dist_clean", "nb_value")],
    by = c("ctry_clean", "dist_clean"), all.x = TRUE #if polygon has no match, it remains within dataset with NA (i.e NO MDA programme)
)

# DIAGNOSTIC: any subnational regions that did not find polygons (check)
matched_keys <- unique(paste(m$ctry_clean, m$dist_clean)[!is.na(m$nb_value)]) #find those that matched
your_keys    <- paste(to_plot$ctry_clean, to_plot$dist_clean) #list all regions in NMB dataset
unmatched    <- to_plot[!(your_keys %in% matched_keys), c("nb_country", "nb_subnational_region")] #Find any unmatched
cat("\n=====================================================\n")
cat("Net-benefit admin-1 units with NO boundary match:", nrow(unmatched), "\n")
cat("=====================================================\n")
if (nrow(unmatched) > 0) print(unmatched[order(unmatched$nb_country), ]) #Print unmatched regions


## 8. COUNTRY OUTLINES + label anchor points

sf_adm0_all <- sf::read_sf(file.path(IN_DIR, "ESPEN_ADM0_2023"), "ESPEN_ADM0_2023") #load Admin-0 shapefile (country boundaries)
sf_ctry_all <- sf_adm0_all %>% group_by(ADMIN0) %>% summarise(.groups = "drop") #dissolve country polygons, one country-level shape

# background = selected countries + neighbours out to N_RINGS
#   (1 = immediately adjacent, 2 = also one country over). Drops distant
#   countries and islands so the map stays focused.
N_RINGS <- 2
sel_i    <- which(sf_ctry_all$ADMIN0 %in% sel_countries_espen) #Find selected countries
adj      <- sf::st_intersects(sf_ctry_all)          # find neighbouring countries
keep     <- sel_i; frontier <- sel_i #for also one country over + neighbours
for (r in seq_len(N_RINGS)) {
    frontier <- setdiff(unique(unlist(adj[frontier])), keep) #The neighbours of ring 1
    keep     <- union(keep, frontier)
}
sf_bg <- sf_ctry_all[keep, ]                          # Create map background

# selected countries only (bold border + labels)
sf_adm0 <- sf_adm0_all[sf_adm0_all$ADMIN0 %in% sel_countries_espen, ]

# tidy display names (labels sit OUTSIDE the country)
sf_adm0$cname <- sf_adm0$ADMIN0
sf_adm0$cname[sf_adm0$cname %in% c("Tanzania (Mainland)", "Tanzania (Zanzibar)")] <- "Tanzania"
sf_adm0$cname[sf_adm0$cname == "Congo, DRC"]    <- "DR Congo"
sf_adm0$cname[sf_adm0$cname == "Cote d'Ivoire"] <- "Côte d'Ivoire"

# dissolve to one polygon per country -> single border + single label each
sf_ctry <- sf_adm0 %>% group_by(cname) %>% summarise(.groups = "drop")

# Label positions: one anchor point per country 
lab_pts <- sf::st_point_on_surface(sf_ctry) #a point guaranteed to be inside the polygon
lab_xy  <- as.data.frame(sf::st_coordinates(lab_pts)) #coordinations where label will be placed
lab_xy$cname <- sf_ctry$cname #adds country name to label


## 9. BIN NET BENEFIT + PLOT

# cut into the fixed bins; NA (no MDA programme) becomes white
m$nb_bin <- cut(m$nb_value, breaks = MAP_BREAKS, labels = PAL_LABELS, #puts number into bins and then labels/colours bins
                right = TRUE, include.lowest = TRUE) #puts NMB values into bins categorised earlier (MAP_BREAKS), intervals </= right closed
m$nb_bin <- as.character(m$nb_bin)
m$nb_bin[is.na(m$nb_bin)] <- NA_LABEL #polygons with missing NMB values = no mda programme 
m$nb_bin <- factor(m$nb_bin, levels = c(PAL_LABELS, NA_LABEL)) #legend in sequential number order

fill_values <- c(setNames(PAL, PAL_LABELS), setNames("white", NA_LABEL)) #fills bins with colours, NA as white)

# add an empty-geometry row for any bin absent from a given map, so the legend
# shows ALL bins in the right colour (geom_sf otherwise leaves an unused bin's key
# blank/white). Empty geometries draw nothing, they only populate the legend.
add_missing_bins <- function(x) {
    lvls <- c(PAL_LABELS, NA_LABEL)
    miss <- setdiff(lvls, as.character(x$nb_bin))
    if (length(miss)) {
        d <- x[rep(1L, length(miss)), , drop = FALSE]
        d$nb_bin <- factor(miss, levels = lvls)
        sf::st_geometry(d) <- sf::st_sfc(lapply(seq_along(miss), function(i) sf::st_polygon()),
                                         crs = sf::st_crs(x))
        x <- rbind(x, d)
    }
    x
}
m <- add_missing_bins(m)

# Map zoom limits
# increase pad to reveal more of the surrounding ring, decrease to zoom in further
bb <- sf::st_bbox(sf_ctry)
xpad <- 0.03 * (bb["xmax"] - bb["xmin"]) #3% border around map
ypad <- 0.03 * (bb["ymax"] - bb["ymin"])

#Build map: plot function
p <- ggplot() +
    # surrounding countries: outline only, no fill, no labels (context backdrop)
    geom_sf(data = sf_bg, fill = "white", colour = "grey75", linewidth = 0.2) +
    geom_sf(data = m, aes(fill = nb_bin), colour = "grey45", linewidth = 0.12) +
    geom_sf(data = sf_ctry, fill = NA, colour = "black", linewidth = 0.45) + #add borders
    ggrepel::geom_label_repel( #labels, prevents overlapping
        data = lab_xy, aes(X, Y, label = cname), #cleaned names
        size = 3, fontface = "bold",
        fill = "white", alpha = 0.9, label.size = 0.15,
        label.padding = unit(0.12, "lines"),
        segment.colour = "grey30", segment.size = 0.3,
        min.segment.length = 0, box.padding = 0.6,
        max.overlaps = Inf, seed = 1 #labels reproducible for every run
    ) +
    scale_fill_manual(values = fill_values, drop = FALSE, limits = c(PAL_LABELS, NA_LABEL), name = LEGEND_TTL, #custom colours and legend title
                      guide = guide_legend(override.aes = list(colour = "grey60", linewidth = 0.3))) +
    coord_sf(xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad), #Crop map
             ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
             expand = FALSE, clip = "off") +
    theme_void(base_size = 13) + #Remove normal plot formatting (axes, gridlines, background, margins)
    theme(
        legend.position   = c(0.10, 0.18),   # inside, bottom-left (empty ocean) O=left/bottom, 1= right/top
        legend.justification = c(0, 0),
        legend.background = element_rect(fill = "white", colour = "grey70", linewidth = 0.3),
        legend.key.size   = unit(0.9, "lines"),
        plot.margin       = ggplot2::margin(8, 8, 8, 8)
    )

#Print map
p

# Save spatial map 
ggsave(
    file.path(OUT_DIR, paste0("nmb_espen_admin1_", tolower(NB_SCEN), ".png")),
    p, width = 13, height = 10, dpi = 300, bg = "white"
)
cat("\nSaved map to",
    file.path(OUT_DIR, paste0("nmb_espen_admin1_", tolower(NB_SCEN), ".png")), "\n")


## 10. ALL SCENARIOS x {lower 95% CrI, median, upper 95% CrI}  -> 9 maps
# Reuses the geometry built above and the
# median + 2.5th/97.5th percentile columns already in the CSV (95% CrI), consistent
# with the forest plot and the country/scenario summaries.
make_map <- function(scen, value_col, stat_label) {
    tp <- nmb[nmb$scenario == scen, c("country", "subnational_region", value_col)] #Extract one scenario and one uncertainty estimate
    names(tp) <- c("nb_country", "nb_subnational_region", "nb_value")
    tp$ctry_clean <- clean(tp$nb_country)
    tp$dist_clean <- clean(tp$nb_subnational_region)
    mm <- merge(sf_adm1, tp[, c("ctry_clean", "dist_clean", "nb_value")],
                by = c("ctry_clean", "dist_clean"), all.x = TRUE)
    mm$nb_bin <- cut(mm$nb_value, breaks = MAP_BREAKS, labels = PAL_LABELS,
                     right = TRUE, include.lowest = TRUE)
    mm$nb_bin <- as.character(mm$nb_bin); mm$nb_bin[is.na(mm$nb_bin)] <- NA_LABEL
    mm$nb_bin <- factor(mm$nb_bin, levels = c(PAL_LABELS, NA_LABEL))
    mm <- add_missing_bins(mm)   # every bin present
    g <- ggplot() +
        geom_sf(data = sf_bg,   fill = "white", colour = "grey75", linewidth = 0.2) +
        geom_sf(data = mm, aes(fill = nb_bin), colour = "grey45", linewidth = 0.12) +
        geom_sf(data = sf_ctry, fill = NA, colour = "black", linewidth = 0.45) +
        ggrepel::geom_label_repel(data = lab_xy, aes(X, Y, label = cname),
            size = 3, fontface = "bold", fill = "white", alpha = 0.9, label.size = 0.15,
            label.padding = unit(0.12, "lines"), segment.colour = "grey30", segment.size = 0.3,
            min.segment.length = 0, box.padding = 0.6, max.overlaps = Inf, seed = 1) +
        scale_fill_manual(values = fill_values, drop = FALSE, limits = c(PAL_LABELS, NA_LABEL), name = LEGEND_TTL,
            guide = guide_legend(override.aes = list(colour = "grey60", linewidth = 0.3))) +
        coord_sf(xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
                 ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad), expand = FALSE, clip = "off") +
        theme_void(base_size = 13) +
        theme(legend.position = c(0.10, 0.18), legend.justification = c(0, 0),
              legend.background = element_rect(fill = "white", colour = "grey70", linewidth = 0.3),
              legend.key.size = unit(0.9, "lines"), plot.margin = ggplot2::margin(8, 8, 8, 8),
              plot.title = element_text(face = "bold", hjust = 0.5))
    fn <- file.path(OUT_DIR, paste0("nmb_espen_admin1_", tolower(scen), "_", value_col, ".png"))
    ggsave(fn, g, width = 13, height = 10, dpi = 300, bg = "white")
    cat("Saved", fn, "\n")
}

#Define percentiles
stat_cols <- c(nmb_lo = "lower 95% CrI (2.5th pct)", nmb_median = "median",
               nmb_hi = "upper 95% CrI (97.5th pct)")
#Run through for each scenario and percentiles
for (scen in c("Optimistic", "Realistic", "Pessimistic")) #for each scenario
    for (vc in names(stat_cols)) #run three times for each percentile
        make_map(scen, vc, stat_cols[[vc]])
cat("\nSaved 9 scenario x percentile maps to", OUT_DIR, "\n")


##### COVARIATE MAPS  —  malaria incidence and ivermectin coverage

#     * incidence -> sequential ORANGE-RED (YlOrRd), high burden = dark red
#     * coverage  -> sequential GREEN       (YlGn),   high coverage = dark green
#  Both use fixed, round bins with the tails caught by +/-Inf, and share the
#  polygons, country outlines and labels built above.

if (!exists("data") || !all(c("n_treated", "incidence_rate") %in% names(data))) {
    source("R/00_model_setup.R")
}

# covariates keyed the same way as the net-benefit table
cov_tbl <- data.frame(
    nb_country            = data$country,
    nb_subnational_region = data$subnational_region,
    incidence             = data$incidence_rate,                  # cases per 1,000
    coverage              = 100 * data$n_treated / data$total_pop, # % of total population
    stringsAsFactors      = FALSE
)
cov_tbl$ctry_clean <- clean(cov_tbl$nb_country)
cov_tbl$dist_clean <- clean(cov_tbl$nb_subnational_region)

# bin edges chosen to match the observed spread: incidence runs 58-446 per 1,000
# (so no bin below 100 is needed beyond a single tail), coverage runs 0-127%
# (eight regions report treating more people than the population estimate).
INC_BREAKS <- c(-Inf, 100, 150, 200, 250, 300, 350, 400, Inf)
INC_LABELS <- c("< 100", "100 – 150", "150 – 200", "200 – 250",
                "250 – 300", "300 – 350", "350 – 400", "> 400")
INC_PAL    <- c("#FFF7BC", "#FEE391", "#FEC44F", "#FE9929",
                "#EC7014", "#CC4C02", "#993404", "#662506")

COV_BREAKS <- c(-Inf, 10, 20, 30, 40, 60, 80, 100, Inf)
COV_LABELS <- c("< 10", "10 – 20", "20 – 30", "30 – 40",
                "40 – 60", "60 – 80", "80 – 100", "> 100")
COV_PAL    <- c("#F7FCB9", "#E0F3A8", "#C7E9B4", "#A1D99B",
                "#74C476", "#41AB5D", "#238B45", "#005A32")

make_cov_map <- function(varname, breaks, labels, pal, legend_ttl, file_name) {

    mm <- merge(sf_adm1, cov_tbl[, c("ctry_clean", "dist_clean", varname)],
                by = c("ctry_clean", "dist_clean"), all.x = TRUE)

    mm$bin <- cut(mm[[varname]], breaks = breaks, labels = labels,
                  right = TRUE, include.lowest = TRUE)
    mm$bin <- as.character(mm$bin)
    mm$bin[is.na(mm$bin)] <- NA_LABEL
    mm$bin <- factor(mm$bin, levels = c(labels, NA_LABEL))

    # populate every legend key, as for the NMB maps
    lvls <- c(labels, NA_LABEL)
    miss <- setdiff(lvls, as.character(mm$bin))
    if (length(miss)) {
        d <- mm[rep(1L, length(miss)), , drop = FALSE]
        d$bin <- factor(miss, levels = lvls)
        sf::st_geometry(d) <- sf::st_sfc(lapply(seq_along(miss), function(i) sf::st_polygon()),
                                         crs = sf::st_crs(mm))
        mm <- rbind(mm, d)
    }

    fills <- c(setNames(pal, labels), setNames("white", NA_LABEL))

    g <- ggplot() +
        geom_sf(data = sf_bg, fill = "white", colour = "grey75", linewidth = 0.2) +
        geom_sf(data = mm, aes(fill = bin), colour = "grey45", linewidth = 0.12) +
        geom_sf(data = sf_ctry, fill = NA, colour = "black", linewidth = 0.45) +
        ggrepel::geom_label_repel(
            data = lab_xy, aes(X, Y, label = cname),
            size = 3, fontface = "bold",
            fill = "white", alpha = 0.9, label.size = 0.15,
            label.padding = unit(0.12, "lines"),
            segment.colour = "grey30", segment.size = 0.3,
            min.segment.length = 0, box.padding = 0.6,
            max.overlaps = Inf, seed = 1
        ) +
        scale_fill_manual(values = fills, drop = FALSE,
                          limits = c(labels, NA_LABEL), name = legend_ttl,
                          guide = guide_legend(override.aes = list(colour = "grey60",
                                                                   linewidth = 0.3))) +
        coord_sf(xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
                 ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
                 expand = FALSE, clip = "off") +
        theme_void(base_size = 13) +
        theme(
            legend.position   = c(0.10, 0.18),
            legend.justification = c(0, 0),
            legend.background = element_rect(fill = "white", colour = "grey70", linewidth = 0.3),
            legend.key.size   = unit(0.9, "lines"),
            plot.margin       = ggplot2::margin(8, 8, 8, 8)
        )

    print(g)
    ggsave(file.path(OUT_DIR, file_name), g, width = 13, height = 10, dpi = 300, bg = "white")
    cat("Saved", file.path(OUT_DIR, file_name), "\n")
}

make_cov_map("incidence", INC_BREAKS, INC_LABELS, INC_PAL,
             "Malaria incidence\n(cases per 1,000)", "map_incidence_admin1.png")

make_cov_map("coverage",  COV_BREAKS, COV_LABELS, COV_PAL,
             "Ivermectin coverage\n(% of population)", "map_coverage_admin1.png")
