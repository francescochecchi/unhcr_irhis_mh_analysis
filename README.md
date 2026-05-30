## Forced displacement and mental health-related diagnoses: a secondary analysis of United Nations High Commissioner for Refugees data
### Description of input datasets and R analysis scripts
May 2026

## General description
This repository contains data and R scripts needed to replicate the above analysis. However, it does not contain the main UNHCR health information system (iRHIS) datasets used in the study (only dummy versions of these are included here, with the actual column names but na data rows). These need to be requested from UNHCR's Public Health Section. All the code is found in the `\code` folder. To replicate the analysis once UNHCR source datasets are obtained, follow these steps:
* Download and unzip the repository to any folder in your computer (other than the Downloads folder, which usually gets wiped automatically). The folder is identified automatically when the code is run.
* Replace the dummy datasets in the `in` folder with the actual UNHCR source datasets: these should have the same file names and column names/format.
* Download R and RStudio (see download links on [https://posit.co/download/rstudio-desktop/]). While R is sufficient to run the analysis, it is recommended to instead run the scripts from the RStudio interface.
* Open and run the entire `00_master_script.R` script (just press Alt+Ctrl+R). This will create an `\out` folder with further sub-folders, to which output tables and graphs will be saved automatically. As this scripts calls all the others, it alone is sufficient to replicate the analysis, but note below steps if you wish to alter the analysis.

## Description of input files
* `demographics_residing_world.csv` contains UNHCR population data, as downloaded from [https://data.humdata.org/dataset/unhcr-population-data-for-world]. Only the year 2024 is included.
* `HDR25_Statistical_Annex_HDI_Table.xlsx` is the UNDP's Human Development Index dataset, as downloaded from [https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Statistical_Annex_HDI_Table.xlsx].
* `UNHCR_OPD_consultation_indicators_Jan_2024-June_2025.csv`, a dummy dataset, should contain data on clinic functionality from each UNHCR site-month included in the iRHIS.
* `UNHCR_OPD_consultations_Jan_2024-June_2025.csv`, also a dummy dataset, should contain data on clinic utilisations.
* `UNHCR_OPD_MH_morbidity_Q1-Q2_2024.csv`, `UNHCR_OPD_MH_morbidity_Q3-Q4_2024.csv` and `UNHCR_OPD_MH_morbidity_Q1-Q2_2025.csv` each contain a semester's worth of data from the iRHIS on mental health-related service utilisations.
* `UNHCR_Refugees_pop_Jan_2024-June_2025.csv` is a UNHCR dataset of population per site.
* `who_mh_consultations.csv`, `who_mh_expenditure.csv` and `who_mh_psychiatrists.csv` are datasets of refugee host country-level mental health system indicators, drawn from WHO's Global Health Observatory.

## Description of R scripts
* `00_master_script.R` loads necessary packages, sets a few general parameters and calls other scripts.
* `01_process_data.R` pre-processes the datasets, cleaning implausible values and reshaping/merging datasets together.
* `02_visualise_patterns.R` generates visualisations of different patterns in mental health-related service utilisations among refugees and host nationals utilising UNHCR-supported facilities.
* `03_fit_models.R` prepares data, explores univariate correlations and fits multivariate models of the association between clinician availability and the proportion of mental health-related consultations / the category of mental health-related consultation.

