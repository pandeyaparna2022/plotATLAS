
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plotATLAS

<!-- badges: start -->
<!-- badges: end -->

The goal of plotATLAS is to plot data from various outputs produced from
the ATLAS (Analysis Tools for Low-coverage and Ancient Samples)
pipleline.

## Installation

You can install the development version of plotJSON from
[GitHub](https://github.com/pandeyaparna2022/plotATLAS) with:

``` r
# install.packages("devtools")
devtools::install_github("pandeyaparna2022/plotATLAS")
```

Alternatively you can also use the `pak` package:

``` r
# install.packages("pak")
pak::pak("pandeyaparna2022/plotATLAS")
```

Or the `remotes` package:

``` r
# install.packages("remotes")
remotes::install_github("pandeyaparna2022/plotATLAS", build_vignettes = TRUE)
```

## Dependencies

- rjson
- ggplot2
- rlang
- stringr

## Usage

### Plot postmortem damage

``` r
library(plotATLAS)

# Assign path to the input file e.g path = "path/to/folder/ATLAS_data_RGInfo.json"

path <- list_example("test.json") #here the path is the internal test data provided with the package

# define readGroups if you wish to plot only certain readGroups
readGroups <- c("ERR8666961", "ERR8684188")

# define Side 5, 3 or both. Default is both.
side = 5

# plot PMD
plot_PMD(path, side, readGroups=readGroups)
```

<img src="man/figures/README_usage_PMD_-1.png" width="100%" />

``` r
plot_PMD(path, readGroups=readGroups)
```

<img src="man/figures/README_usage_PMD_-2.png" width="100%" />

``` r
plot_PMD(path)
```

<img src="man/figures/README_usage_PMD_-3.png" width="100%" />

### Plot Sequencing Error Covariates

``` r
library(plotATLAS)

# Assign path to the input file e.g path = "path/to/folder/ATLAS_data_RGInfo.json"
path <- list_example("test.json")
# define readGroups if you wish to plot only certain readGroups
readGroups <- c("ERR8666961", "ERR8684188")
#Plot covariates
Covariates = list('quality','position','context','fragmentLength','mappingQuality','rho')
for (i in Covariates){
plot_SeqError_covariate(path,i,1);

}
```

<img src="man/figures/README_usage_covariates_-1.png" width="100%" /><img src="man/figures/README_usage_covariates_-2.png" width="100%" /><img src="man/figures/README_usage_covariates_-3.png" width="100%" /><img src="man/figures/README_usage_covariates_-4.png" width="100%" /><img src="man/figures/README_usage_covariates_-5.png" width="100%" /><img src="man/figures/README_usage_covariates_-6.png" width="100%" />
\### Plot BAMDiagnostic Histograms

``` r
library(plotATLAS)
# Assign path to the directory with input files e.g path = "path/to/folder/Bamdiagnostic"
path <- list_example_folder()
plot_BAMdiagnostic(path, output_directory_name = getwd())
#> [1] "The following  1  file/s are excluded because either they are empty or do not align with the required format."
#> [1] "C:\\Users\\pandapar\\AppData\\Local\\R\\win-library\\4.4\\plotATLAS\\extdata/modern_2_fragmentLengthHistogram.txt"
```

<img src="man/figures/README_usage_BAMDiagnostics_-1.png" width="100%" /><img src="man/figures/README_usage_BAMDiagnostics_-2.png" width="100%" /><img src="man/figures/README_usage_BAMDiagnostics_-3.png" width="100%" /><img src="man/figures/README_usage_BAMDiagnostics_-4.png" width="100%" /><img src="man/figures/README_usage_BAMDiagnostics_-5.png" width="100%" />

    #> png 
    #>   2
