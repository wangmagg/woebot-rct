# Woebot

Statistical analyses for Phase II W-SUD trial.

## Set-up
__Installation__ <br />
To clone this repository, run the following <br />
```
git clone https://github.com/wangmagg/woebot-share.git
cd woebot-share
```

__R library dependencies__ <br />
Library dependencies are specified in `renv.lock`. To install these dependencies, run the following <br />
```
renv::restore()
```

## File Descriptions

<details>
<summary><b>Data </b></summary>

  -   `dat_analysis_dictionary.xlsx`: contains definition for each variable, the possible values and ranges it can take on, and how it got calculated (if it was not imported from the questionnaires directly)
  -   `dat_analysis_share.csv`: analytic dataset

</details>


<details>
  <summary><b>Analysis </b></summary>
  **`run-analysis.sh`**: bash script that executes all analyses
  
  **`config.R`**: loads all necessary libraries and defines variable constants that are shared across several scripts
  
  **`0-Functions`**
-   `1a-data-fns-filling.R`: functions for imputing or replacing data values
-   `1b-data-fns-loading.R`: functions for loading and merging raw data files
-   `1c-data-fns-recoding.R`: functions for recoding (reverse coding, collapsing small categories) and for creating composite variables
-   `2a-analysis-fns-balance.R`: functions used to assess balance across groups (invoked in constructing balance table)
-   `2b-analysis-fns-subgroups.R`: functions used to define subgroups (invoked for moderation analysis)
-   `2c-analysis-fns-summaries.R`: functions used to perform descriptive summaries of variables
-   `3a-analysis-fns-regressions.R`: functions used to run regression analyses (invoked for estimating treatment effects for primary and secondary outcome)
-   `3b-analysis-fns-ttest.R`: functions used to perform t-tests (invoked for estimating treatment effects for primary and secondary outcomes, without covariate adjustment)
-   `3c-analysis-fns-variable-selection.R`: functions used to select variables included in the regression analyses

  **`1-DataProcessing`**
-   `run-data-processing.R`: constructs derivative datasets from `dat_analysis_shared.csv` for ease of use in analyses

  **`2-Descriptives`**

-   `run-descriptives.R`: runs all descriptive analysis (1-3 detailed below)
-   `1-balance-table.R`: creates and saves balance tables
-   `2-outcome-vars-summary.R`: creates and saves descriptive summaries of outcome variables
-   `3-differential-dropout-check.R`: compares participants who were randomized to those who were removed or withdrawn from the study

**`3-Effects`**

-   `run-effects.R`: runs all effect estimation analysis (1-3 detailed below)
-   `1-assumptions.R`: visual checks on linear regression assumptions
-   `2a-between-group-primary.R`: estimates treatment effects for primary outcome (past 30-day substance use days) and tests for treatment effect heterogeneity
-   `2b-between-group-secondary.R`: estimates treatment effects for secondary outcomes and tests for treatment effect heterogeneity
-   `2c-pval-adjust.R`: adjusts secondary outcome p-values
-   `3-within-group`: calculates within group "effect size" for primary and secondary outcomes by comparing pre- and post- data within intervention arms

**`4-Engagement`**

-   `run-engagement.R`: runs all engagement analysis (1-3 detailed below)
-   `1-acc-fea.R`: summarizes accessibility and feasibility scores and estimates correlation with past 30-day substance use days
-   `2-control-pdf.R`: summarizes control group's self-reported and submission-tracked engagement with psychoeducational PDFs

**`5-BivarCorrelations`**

-   `run-bivariate-corr.R`: calculates bivariate correlations across subset of outcome variables and compares correlations across arms

## Running

To run **all analyses**: </br>
```
bash ./Analysis/run-analysis.sh
```
