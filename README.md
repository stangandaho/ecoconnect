
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ecoconnect

<!-- badges: start -->

[![R-CMD-check](https://github.com/stangandaho/ecoconnect/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stangandaho/ecoconnect/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of **ecoconnect** is to provide a comprehensive toolkit for
assessing and quantifying ecological connectivity in protected area
networks. The package implements a suite of metrics and methods for
evaluating landscape connectivity, including the Probability of
Connectivity (PC), Equivalent Connected Area (ECA), ProtConn fractions,
and more. It is designed to facilitate reproducible connectivity
analyses for conservation planning, research, and policy support.

## Installation

You can install the development version of ecoconnect from
[GitHub](https://github.com/stangandaho/ecoconnect) with:

``` r
# install.packages("pak")
pak::pkg_install("stangandaho/ecoconnect")
```

## Example

Load the package and explore the main functions:

``` r
library(ecoconnect)
library(sf)
#> Linking to GEOS 3.11.2, GDAL 3.8.2, PROJ 9.3.1; sf_use_s2() is TRUE

# Load example data
data("pas")    # Protected areas
data("fiwac")  # Five West African Country Boundaries

# Benin - treated as single ecoregion
benin <- fiwac %>%
  dplyr::filter(ISO3_CODE == "BEN")

# Prepare data first
prepared_data <- ec_prepare_data(
  pa = pas,
  ecoregion = benin,
  min_area = 1
)

# Add Transboundary Protected Areas to Analysis
complete_pa <- ec_add_transboundary_pa(prepared_data, buffer_km = 70)

# Calculate Inter-Protected Area Distances
dist_matrix <- ec_pa_distances(complete_pa, method = "edge")

# Calculate Dispersal Probabilities Between Protected Areas
prob_matrix <- ec_dispersal_probability(dist_matrix, median_dispersal = 30)

# Calculate Maximum Product Probability Considering Stepping-Stone Paths
max_prob <- ec_max_product_probability(prob_matrix, max_steps = 3)

# Calculate ECA including transboundary effects
eca <- ec_eca(
  pa = complete_pa,
  max_prob_matrix = max_prob,
  with_transboundary = TRUE
)
```

## Contributing

Contributions, bug reports, and feature requests are welcome! Please
open an issue or submit a pull request via
[GitHub](https://github.com/stangandaho/ecoconnect).
