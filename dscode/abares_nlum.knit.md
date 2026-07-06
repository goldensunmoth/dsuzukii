---
title: "ABARES land use data separation to identify commodiy host distributions"
date: "2026-07-03"
output:
  html_document:
    theme: cerulean
    highlight: tango
bibliography: references.bib
csl: harvard-cite-them-right.csl
---

<style type="text/css">
.csl-entry { margin-bottom: 1.5em; }
</style>

### Rmd purposes

**Aim and context:** This file is used to subset the ABARES NLUM dataset into relevant agricultural commodities and to plot their extent across Australia. 
**Outputs:** Map of relevant land use types as commodity hosts, and a raster of the outputs

### Libraries setup


``` r
library(terra)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ozmaps)
library(sf)
```

### Data files

- Read in Australian boundaries
- Download and read in the most recent 2020-21 ABARES NLUM dataset from <https://www.agriculture.gov.au/abares/aclump/land-use/land-use-of-australia-2010-11-to-2020-21#downloads>
- CSV attribute data is used to map integer raster codes to land-use category labels
- TIF provides the native-resolution (250 m) categorical land-use raster for Australia

``` r
data("ozmap_states")                                  # load Australian boundaries
abares_csv <- read.csv("../dsdata/NLUM_v7_250_INPUTS_2020_21_geo.csv") # read in csv
abares_tif <- rast("../dsdata/NLUM_v7_250_INPUTS_2020_21_geo.tif")     # read in tif 
```

### Define and apply Australian Albers Equal Area projection

- Set Australian boundaries as an object, then convert to a vector and reproject to Australian Albers
- Set target raster resolution to 10 arc minutes, and the extent to the approximate bounding box of mainland Australia + Tasmania

``` r
aus_states <- ozmap_states
aus_states <- vect(aus_states)
aus_states_albers <- project(aus_states, "EPSG:3577") 

target_res <- rast()
res(target_res) <- 10/60
ext(target_res) <- ext(113, 154, -44, -10)
```

### Subset ABARES categories to relevant land use types

- Collapse irrigated horticulture codes into their non-irrigated equivalents
- Apply the merged attribute table to the raster and make it the active category, so raster values now resolve to TERTV8_merged labels
- Identify land use categories to retain

``` r
abares_csv_merged <- abares_csv %>%
  mutate(TERTV8_merged = case_when(
    TERTV8 == "4.4.1 Irrigated tree fruits" ~ "3.4.1 Tree fruits",
    TERTV8 == "4.4.8 Irrigated citrus" ~ "3.4.8 Citrus",
    TERTV8 == "4.4.9 Irrigated grapes" ~ "3.4.9 Grapes",
    TERTV8 == "4.4.5 Irrigated shrub berries and fruits" ~ "3.4.5 Shrub berries and fruits",
    TERTV8 == "4.4.0 Irrigated perennial horticulture" ~ "3.4.0 Perennial horticulture",
    TRUE ~ TERTV8 ))

levels(abares_tif) <- abares_csv_merged
activeCat(abares_tif) <- "TERTV8_merged"

categories_to_keep <- c("3.4.0 Perennial horticulture",
                        "3.4.1 Tree fruits",
                        "3.4.5 Shrub berries and fruits", 
                        "3.4.8 Citrus",
                        "3.4.9 Grapes",
                        "5.1.0 Intensive horticulture",
                        "5.1.2 Shadehouses",
                        "5.1.3 Glasshouses")
```

### Analysis

- Look up the integer raster codes ("Value") corresponding to the categories_to_keep labels via the merged attribute table
- Keep only pixels in categories_to_keep, everything else becomes NA
- Reproject to Australian Albers
- Keep categorical attribute table attached at each step


``` r
level_table <- levels(abares_tif)[[1]]
values_to_keep <- level_table$Value[level_table$TERTV8_merged %in% categories_to_keep]
abares_filtered <- classify(abares_tif, cbind(values_to_keep, values_to_keep), others = NA) # drop all cells not specified to keep
```

```
## |---------|---------|---------|---------|=========================================                                          
```

``` r
levels(abares_filtered) <- abares_csv_merged        # ensure categorical attribute table is kept
activeCat(abares_filtered) <- "TERTV8_merged"

abares_wgs84 <- project(abares_filtered, "EPSG:4326", method = "near")
```

```
## |---------|---------|---------|---------|=========================================                                          
```

``` r
agg_factor <- round((10/60) / res(abares_wgs84)[1]) # each output cell takes the most frequent (modal) code of kept land use pixels

abares_agg <- aggregate(abares_wgs84, fact = agg_factor, fun = "modal", na.rm = TRUE)
abares_modal <- resample(abares_agg, target_res, method = "near")

levels(abares_modal) <- abares_csv_merged           # ensure categorical attribute table is kept
activeCat(abares_modal) <- "TERTV8_merged"

abares_modal_albers <- project(abares_modal, "EPSG:3577", method = "near") # project to Australian Albers
levels(abares_modal_albers) <- abares_csv_merged    # restore full category table
activeCat(abares_modal_albers) <- "TERTV8_merged"
```

### Set mapping aesthetics

- The TERTV8 codes aren't necessary for the map outputs, so these can be dropped for visual clarity

``` r
lu_table <- levels(abares_modal_albers)[[1]] # build colour table
cols <- rep("#d9d9d9", nrow(lu_table))
base_cols <- c(
  "3.4.0 Perennial horticulture" = "#d76a4c", 
  "3.4.1 Tree fruits" = "#c9d763",
  "3.4.5 Shrub berries and fruits" = "#6e1e3a", 
  "3.4.8 Citrus" = "#ffd932", 
  "3.4.9 Grapes" = "#e9b0c8",
  "5.1.0 Intensive horticulture" = "#25533f",
  "5.1.2 Shadehouses" = "#274f8b", 
  "5.1.3 Glasshouses" = "#a4bde0")
names(base_cols) <- categories_to_keep

idx <- match(lu_table$TERTV8_merged, categories_to_keep)
cols[!is.na(idx)] <- base_cols[idx[!is.na(idx)]]
coltab(abares_modal_albers) <- data.frame(value = lu_table$Value, col = cols)

category_labels <- c(
  "3.4.0 Perennial horticulture" = "Perennial horticulture",
  "3.4.1 Tree fruits" = "Tree fruits",
  "3.4.5 Shrub berries and fruits" = "Shrub berries and fruits",
  "3.4.8 Citrus" = "Citrus",
  "3.4.9 Grapes" = "Grapes",
  "5.1.0 Intensive horticulture" = "Intensive horticulture",
  "5.1.2 Shadehouses" = "Shadehouses",
  "5.1.3 Glasshouses" = "Glasshouses")
```

### Create map

``` r
png("../dsoutputs/D.suzukii_hosts_aus.png", units="in", width=8, height=4, res=700)
par(oma=c(0, 0, 0, 1))
par(mar=c(0, 0, 0, 1))

plot(abares_modal_albers, axes = FALSE, plg = FALSE, legend = FALSE,
     main = expression('Australian host distribution for '*italic(Drosophila~suzukii)*''))
lines(aus_states_albers,   col = "black", lwd = 1)

par(xpd = NA)
legend("right", inset = c(-0.05, 0),
       legend = category_labels[categories_to_keep],
       fill = base_cols[categories_to_keep],
       bty = "n", cex = 0.9)
invisible(dev.off())
```

### Export raster of all land use types
- No separation of host commodities within the raster

``` r
writeRaster(abares_modal, filename = "../dsdata/abares_modal_hosts.tif", filetype = "GTiff", overwrite = TRUE)
```
