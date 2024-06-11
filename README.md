# Association of Human Mobility and Weather Conditions on Dengue Mosquito in Hong Kong during the COVID-19 Pandemic

**DOI:** [https://doi.org/10.1101/2024.04.17.24306004](https://doi.org/10.1101/2024.04.17.24306004)

## Paper Authors

Yufan Zheng, Keqi Yue, Eric W. M. Wong, Hsiang-Yu Yuan

## Research Aim

This study aimed to assess the influence of human mobility on the abundance and extensiveness of *Aedes albopictus*, taking account of the lagged and nonlinear effects of total rainfall and mean temperature based on distributed-lagged non-linear models (DLNM).

## Data and Code sources

**Data:** a folder containing the processed data, including mosquito activity, weather data, and human mobility data.

**Model:** a folder containing all fitted models and sensitivity analysis models. In addition, summarizes the fitted model results and sensitivity analysis results.

**Paper:** a folder containing all figures in the paper.

**results:** a folder containing fitted, leave-one-out cross-validation (LOOCV), and projection results.

**1_Load_packages_data.R:** R script to load data, and prepare the input variables.

**2_Model_Construction.R:** R script to construct models and record results.

**3_Model_Sensibility.R:** R script to analyze the model parameter sensibility.

**4_LOOCV.R:** R script to validate the performance of models by the LOOCV.

**5_Standardized.R:** R script to standardize the fitted results (output).

**6_Projection.R:** R script to project the mosquito activity with different human mobility scenarios.

**7_Figures_of_paper.R:** R script to draw all figures in the paper.
