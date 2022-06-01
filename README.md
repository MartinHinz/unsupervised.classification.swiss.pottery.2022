
<!-- README.md is generated from README.Rmd. Please edit that file -->

# unsupervised.classification.swiss.pottery.2022

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MartinHinz/unsupervised.classification.swiss.pottery.2022/master?urlpath=rstudio)

This repository contains the data and code for our paper:

> Hinz, M., Heitz, C., (2022). *Unsupervised Classification of Neolithic
> Pottery from the northern Alpine Space using t-SNE and HDBSCAN*. Name
> of journal/book <https://doi.org/xxx/xxx>

Our pre-print is online here:

> Hinz, M., Heitz, C., (2022). *Unsupervised Classification of Neolithic
> Pottery from the northern Alpine Space using t-SNE and HDBSCAN*. Name
> of journal/book, Accessed 01 Jun 2022. Online at
> <https://doi.org/xxx/xxx>

## Pre-Release

This prerelease contains not the actual dataset used in the analysis of
that paper, but a dummy dataset based on iberian Bell Beakers.

**Do not use this dummy data for real research analysis!!!**

### How to cite

Please cite this compendium as:

> Hinz, M., Heitz, C., (2022). *Compendium of R code and data for
> Unsupervised Classification of Neolithic Pottery from the northern
> Alpine Space using t-SNE and HDBSCAN*. Accessed 01 Jun 2022. Online at
> <https://doi.org/xxx/xxx>

## Contents

The **analysis** directory contains:

-   [:file_folder: code](/analysis/code): The code to run the analysis.
    You can run ´run_all_scripts.R´ to do the full analysis based on the
    input data. You can also run the individual scripts sequentially to
    see the inbetween results. Functions used in the analysis are
    defined in the ´00_helper_functions´ folder.
-   [:file_folder: data](/analysis/data): Data used in the analysis. For
    this pre-release, this folder contains **Dummy data!!!**

## How to run in your browser or download and run locally

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

The simplest way to explore the text, code and data is to click on
[binder](https://mybinder.org/v2/gh/MartinHinz/unsupervised.classification.swiss.pottery.2022/master?urlpath=rstudio)
to open an instance of RStudio in your browser, which will have the
compendium files ready to work with. Binder uses rocker-project.org
Docker images to ensure a consistent and reproducible computational
environment. These Docker images can also be used locally.

You can download the compendium as a zip from from this URL:
[master.zip](/archive/master.zip). After unzipping: - open the `.Rproj`
file in RStudio - run `devtools::install()` to ensure you have the
packages this analysis depends on (also listed in the
[DESCRIPTION](/DESCRIPTION) file). - finally, open
`analysis/paper/paper.Rmd` and knit to produce the `paper.docx`, or run
`rmarkdown::render("analysis/paper/paper.Rmd")` in the R console

### Licenses

**Text, Code and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
