# Association of Human Mobility and Weather Conditions on Dengue Mosquito in Hong Kong during the COVID-19 Pandemic

**DOI:** [https://doi.org/10.1101/2024.04.17.24306004](https://doi.org/10.1101/2024.04.17.24306004)

## Paper Authors

Yufan Zheng, Keqi Yue, Eric W. M. Wong, Hsiang-Yu Yuan

## Research Aim

This study aimed to assess the influence of human mobility on the abundance and extensiveness of *Aedes albopictus*, taking account of the lagged and nonlinear effects of total rainfall and mean temperature based on distributed-lagged non-linear models (DLNM).


## Data Description

The data folder contains the following datasets:
1. **Mosquito Activity Data**:
   - Records of *Aedes albopictus* abundance in Hong Kong.
   - Aggregated at regular intervals to reflect mosquito activity trends.
2. **Weather Data**:
   - Total rainfall and mean temperature measurements.
   - Time series format with daily/weekly resolution.
3. **Human Mobility Data**:
   - Derived from mobility tracking systems, capturing relative movement levels during the COVID-19 pandemic.
   - Processed to match the temporal and spatial resolution of mosquito activity data.
  
### Table 1: The Monitoring Sites of Mosquito Activity Data in Three Areas in Hong Kong

| Area                     | Site                                                                                                                                                                                                                                                                                                                                                                               |
|--------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Hong Kong Island & Kowloon** | Chai Wan West, Tin Hau, Shau Kei Wan & Sai Wan Ho, Wan Chai North, Happy Valley, Central, Sheung Wan and Sai Ying Pun, Sai Wan, North Point, Aberdeen and Ap Lei Chau, Pokfulam, Deep Water Bay & Repulse Bay, Cheung Chau, Tung Chung, Tsim Sha Tsui, Mong Kok, Lai Chi Kok, Sham Shui Po East, Cheung Sha Wan, Kowloon City North, Hung Hom, Ho Man Tin, Lok Fu West, Kai Tak North, Wong Tai Sin Central, Diamond Hill, Ngau Chi Wan, Kwun Tong Central, Lam Tin, Kowloon Bay |
| **New Territories East** | Tseung Kwan O South, Sai Kung Town, Tseung Kwan, Ma On Shan, Lek Yuen, Yuen Chau Kok, Tai Wai, Wo Che, Tai Po, Fanling, Sheung Shui                                                                                                                                                                                                                                              |
| **New Territories West** | Tin Shui Wai, Yuen Kong, Yuen Long Town, Tuen Mun (S), Tuen Mun (N), Tuen Mun West, So Kwun Wat, Tsuen Wan Town, Tsuen Wan West, Ma Wan, Sheung Kwai Chung, Kwai Chung, Lai King, Tsing Yi, Tsing Yi South, Tsing Yi North                                                                                                                                                          |

### Table 2: The Weather Stations in Three Areas of Hong Kong

| Areas                    | Weather Stations (temperature)                                   | Weather Stations (rainfall)                                      |
|--------------------------|------------------------------------------------------------------|------------------------------------------------------------------|
| **Hong Kong Island & Kowloon** | King’s Park, Happy Valley, Wong Chuk Hang                      | Quarry Bay, Cape D'Aguilar, Happy Valley, King’s Park            |
| **New Territories East** | Ta Kwu Ling, Sha Tin, Tai Mei Tuk                               | Ta Kwu Ling, Sha Tin, Tai Mei Tuk                               |
| **New Territories West** | New Tsing Yi Station, Sha Lo Wan, Cheung Chau, Tuen Mun Children and Juvenile Home, Wetland Park | Sha Lo Wan, Cheung Chau, Tuen Mun Children and Juvenile Home, Wetland Park |

All data have been pre-processed for compatibility with the scripts in this repository.

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


## How to Run the Code

Follow these steps to reproduce the results:

1. **Load Required Packages and Data**:
   - Run `1_Load_packages_data.R` to install necessary R packages and prepare input data.

2. **Construct Models**:
   - Run `2_Model_Construction.R` to fit the distributed-lagged non-linear models (DLNM).

3. **Sensitivity Analysis**:
   - Run `3_Model_Sensibility.R` to analyze the sensitivity of the model parameters.

4. **Model Validation**:
   - Run `4_LOOCV.R` to validate model performance using leave-one-out cross-validation.

5. **Standardize Results**:
   - Run `5_Standardized.R` to standardize and prepare results for further analysis.

6. **Projection Scenarios**:
   - Run `6_Projection.R` to generate projections for mosquito activity under various human mobility scenarios.

7. **Generate Figures**:
   - Run `7_Figures_of_paper.R` to create all figures presented in the study.

## Notes

- Ensure all required R packages are installed before running the scripts.
- Adjust file paths in the scripts to match your local directory structure if necessary.
