
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covid-19-behavior-sem

<!-- badges: start -->
<!-- badges: end -->

This is repository for the data examination and model estimation for the
January-February 2021 study on predictors for behavioral change in the
contexts of the climate crisis and the COVID-19 pandemic.

This repository contains all materials to reproduce our analysis for the
paper 'Perceptions of behaviour efficacy, not perceptions of threat, 
drive COVID-19 protective behaviour in Germany'.

## Folder structure

-   `additional analyses` contains descriptive analyses and inference
    statiscs (except structural model). Also generates some figures for
    our documents
-   `data` contains all data required for this project and code to
    handle data perparation, anonymization, and OSF up/downloads
-   `docs` contains the website data
-   `external-data` contains external data used in the project analysis  
-   `figures` contains generated figures
-   `renv` contains information for libraries used
-   `sem-covid-19` The Covid-19 model
-   `templates` contains template documents to conduct our analyses
-   `R` contains scripts with helper functions

## Instructions for reproducibility

This project uses `renv` for reproducibility. You can install the
libraries we used by calling `renv::restore()` from the R terminal.
