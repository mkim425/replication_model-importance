
# Replication package for “Beyond forecast leaderboards: Measuring individual model importance based on contribution to ensemble accuracy”

This repository contains code and data to reproduce the results in the
manuscript:

- *Title: Beyond forecast leaderboards: Measuring individual model
  importance based on contribution to ensemble accuracy*
- *Authors: Minsu Kim, Evan L. Ray, Nicholas G. Reich*

## Contents

The repository is organized as follows:

- `data-raw/`
  - Scripts for downloading and preprocessing raw data, along with the
    corresponding data files.
  - Scripts for simulating data for simulation studies.
- `data/`: Preprocessed data used in the analysis.
  - `calculate-importance/`: scripts to calculate model importance
    scores (see [Notes – Item 1](#1-Calculating-model-importance-scores)
    for details).
  - `forecast-data/`: forecast data split by horizon and forecast date
    for parallel computation of model importance scores.
  - `calc-result/`: calculated model importance scores are stored in
    `lasomo-output/` and `lomo-output/` by horizon and forecast date.
  - `combine-arrays-*.R`: scripts to combine files in `calc-result/`
    across forecast dates for each horizon per alogorithm (LOMO and
    LASOMO).
- `Rcode/`: R scripts to generate figures and tables in the manuscript.
  - `main-figure_[xx]_*.R`: R scripts to generate figures in the main
    text of the manuscript.
  - `main-table_[xx]_*.R`: R scripts to generate tables in the main text
    of the manuscript.
  - `supp-figure_[xx]_*.R`: R scripts to generate figures in the
    supplementary material.
  - `supp-table_[xx]_*.R`: R scripts to generate tables in the
    supplementary material.
- `plots/`: Generated figures saved as PDF files (`.pdf`).
- `tables/`: Generated tables saved as text files (`.txt`).
- `utils/`: Utility functions used for data simulation and analysis.

## Required Packages

The following R packages are required to run the code in this
repository:

- for accessing forecast data: `covidData`, `covidHubUtils` (version
  0.1.8)
- for data manipulation and visualization: `dplyr`, `tidyverse`,
  `tidyr`, `stringr`, `ggplot2`, `ggrepel`, `ggalt` (version 0.6.1),
  `scoringutils` (version 2.1.2), `hubVis` (version 0.1.3)
- for calculating model importance scores: `covidHubUtils` (version
  0.1.8), `hubEnsembles`, `dplyr`, `tidyverse`, `magrittr`, `lubridate`,
  `modelimportance` (see [Notes – Item
  2](#2-Use-of-%60modelimportance%60-package) for details)

*Comment.* *Versions are provided for selected packages; for the
remaining packages, the exact versions of the packages without indicated
version used during the original computations are not recorded. The
script should run with the latest versions of the packages as of
December 2025.*

### Installation

The `covidHubUtils` package is available on GitHub. The following code
installs the package along with some packages it depends on using the
following code:

``` r
devtools::install_github("reichlab/zoltr", force = TRUE)
remotes::install_github("epiforecasts/scoringutils", dependencies = TRUE)
remotes::install_github("reichlab/covidData")
remotes::install_github("reichlab/covidHubUtils")
```

The `ggalt`, `modelimportance`, and `hubVis` packages are also available
on GitHub. You can install the packages using the following code:

``` r
devtools::install_github("hrbrmstr/ggalt")
devtools::install_github("mkim425/modelimportance")
install.packages("hubVis", repos = c("https://hubverse-org.r-universe.dev", "https://cloud.r-project.org"))
```

Other required packages can be installed from CRAN.

## Notes

#### 1. Calculating model importance scores

Model importance scores were calculated using two algorithms, LOMO and
LASOMO. Parallel computation was performed on the Unity cluster at the
Massachusetts Green High Performance Computing Center (MGHPCC), using
the following scripts:

- `split_forecast_data.R`: script to split the whole forecast data by
  horizon and forecast date for parallel computation.
- `build_array_*.R`: script including a function to build an array to
  store model importance scores based on the corresponding algorithm.
- `importance_score_*.R`: script including a function to calculate
  importance scores based on the corresponding algorithm.
- `calc_importance_*.R`: script to calculate model importance scores
  based on the corresponding algorithm.

*Remark.* *Job submission scripts specific to the cluster environment
are not included in this repository, as they contain user-specific
settings.*

#### 2. Use of `modelimportance` package

Additional supplementary analyses were performed using the
`modelimportance` package, developed by the authors to support use by
external collaborators across a variety of scenarios. The package is
planned for submission to CRAN.

------------------------------------------------------------------------

README prepared on December 13, 2025.
