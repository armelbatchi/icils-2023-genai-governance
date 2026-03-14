# icils-2023-genai-governance

R code for data cleaning, variable construction, analysis, and figure generation for the ICILS 2023 GenAI governance readiness study.

## Overview
fkshj
This repository contains the analytical code used to examine school-level generative AI governance readiness in the 12 ICILS 2023 systems that administered the optional principal questionnaire GenAI module. The code is organized into a reproducible workflow covering data import, cleaning, variable construction, analysis tables, and figure production.

## Repository contents

- `01_load_and_clean_data.R`  
  Imports the raw ICILS school files, harmonizes variables, applies initial cleaning, and prepares a working analytic file.

- `02_construct_variables.R`  
  Constructs governance, permission, restriction, curriculum, and readiness indicators used in the study.

- `03_analysis_tables.R`  
  Produces descriptive analytical tables and country-level summaries.

- `04_make_figures.R`  
  Generates the manuscript and supplementary figures.

- `session_info.txt`  
  Records the R session and package versions used to run the analyses.

## Data availability

This repository does **not** redistribute ICILS 2023 raw data. Users must obtain the relevant ICILS 2023 school/principal files through the appropriate authorized source and place them locally before running the scripts.

## Expected input data

The code assumes access to the ICILS 2023 school-level files containing, at minimum, the variables used in the manuscript, including:

- `CNTRY3`
- `IDSCHOOL`
- `TOTWGTC`
- `CRWGT01` to `CRWGT75`
- `IA3G02A`, `IA3G02B`
- `IA3G03A`, `IA3G03B`
- `IA3G04A`, `IA3G04B`
- `IA3G05`, `IA3G06`
- `IA3G07A`, `IA3G07B`, `IA3G07C`
- `P_CIMPOS`, `P_CONEG`

## Reproducibility workflow

Run the scripts in the following order:

1. `01_load_and_clean_data.R`
2. `02_construct_variables.R`
3. `03_analysis_tables.R`
4. `04_make_figures.R`

## Output

The scripts produce:

- cleaned analytic data objects
- derived governance and readiness indicators
- descriptive tables
- manuscript figures
- supplementary figures

Outputs are written to the local output directory defined within the scripts.

## Analytical notes

The analyses use weighted estimates and jackknife repeated replication (JRR) variance estimation with the ICILS replicate weights. Country-level summaries and figure outputs are based on the school-level principal questionnaire data for the 12 participating systems in the GenAI module.

## Software

Analyses were conducted in R. Package versions are documented in `session_info.txt`.

## Citation

If you use or adapt this code, please cite the associated manuscript once published.

## Contact

For questions about the code, please contact the repository owner through GitHub.
