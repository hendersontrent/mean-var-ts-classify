# mean-var-ts-classify
This repository holds analysis for our [working paper](https://arxiv.org/abs/2303.17809) on simple feature set performance on time-series classification problems.

## Reproducibility

For the neuroimaging case study involving schizophrenia (SCZ) case--control classification, all analysis and visualization can be reproduced using the interactive Jupyter notebook [schizophrenia_case_study.ipynb](https://github.com/hendersontrent/mean-var-ts-classify/blob/main/analysis/schizophrenia_case_study.ipynb).

For the UEA/UCR Repository analysis, see below.

### FTM UEA/UCR Repository analysis

The repository is organised into sub folders, each of which contains a discrete part of this analysis:

* `data` -- contains the time-series data and class labels for the train and test splits for each problem (i.e., the outputs of `get-uea-ucr-datasets.py`). *NOTE: This folder is not pushed to git for size reasons.*
* `feature-calculations` -- contains R scripts to calculate features for each problem as well as the resulting feature objects stored as `.Rda` files
* `classification-models` -- contains R scripts to calculate classification performance of each feature set for each problem as well as the resulting classification objects stored as `.csv` files
* `interpretation` -- contains R scripts which analyse the results computed in `feature-calculations` and `classification-models`

Scripts should be run in the following order:

1. `get-uea-ucr-datasets.py` (Python)
2. `feature-calculations/calculate-features.R` (R)
3. `classification-models/fit-models.R` (R)
4. `interpretation/moments-vs-chance.R` (R)
5. `interpretation/moments-vs-catch24.R` (R)
6. `interpretation/case-studies.R` (R)
7. `interpretation/moments-vs-catch24-avg-abs.R` (R)
