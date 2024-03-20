# mean-var-ts-classify
This repository holds analysis for our [working paper](https://arxiv.org/abs/2303.17809) on simple feature set performance on time-series classification problems.

## Reproducibility

This project is set up using a modular structure. As it uses an `R` project as its basis, the entire analysis can be reproduced by running `driver.R` which calls the necessary scripts in order to build the project end-to-end. Note that some of the scripts require data to be downloaded and within a specific filepath (such as `analysis/compute-features.R` which calls a function defined in `R/tidy_arff_files.R` that requires the datasets to be in the `data/` folder) so please check you have these first before sourcing `driver.R`.

For the neuroimaging case study involving schizophrenia (SCZ) case--control classification, all analysis and visualization can be reproduced using the interactive Jupyter notebook [schizophrenia_case_study.ipynb](https://github.com/hendersontrent/mean-var-ts-classify/blob/main/analysis/schizophrenia_case_study.ipynb).
