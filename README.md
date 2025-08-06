# W-SUD Randomized Controlled Trial Statistical Analysis

## Set-up
__Installation__ <br />
To clone this repository, run the following <br />
```
git clone https://github.com/wangmagg/woebot-rct.git
cd woebot-rct
```

__R library dependencies__ <br />
Library dependencies are specified in `renv.lock`. To install these dependencies, run the following <br />
```
renv::restore()
```

## File Descriptions

### Data

  -   `dat_analysis_dictionary.xlsx`: contains definition for each variable, the possible values and ranges it can take on, and how it got calculated (if it was not imported from the questionnaires directly)
  -   `dat_analysis_share.csv`: analytic dataset

### Analysis
  - `run-analysis.sh`: bash script that executes all analyses
  - `config.R`: loads all necessary libraries and defines variable constants that are shared across several scripts

  <details>
  <summary><b> 0-Functions </b></summary>
    
  -   `1a-data-fns-filling.R`: functions for imputing or replacing data values
  -   `1b-data-fns-loading.R`: functions for loading and merging raw data files
  -   `1c-data-fns-recoding.R`: functions for recoding (reverse coding, collapsing small categories) and for creating composite variables
  -   `2a-analysis-fns-balance.R`: functions used to assess balance across groups (invoked in constructing balance table)
  -   `2b-analysis-fns-subgroups.R`: functions used to define subgroups (invoked for moderation analysis)
  -   `2c-analysis-fns-summaries.R`: functions used to perform descriptive summaries of variables
  -   `3a-analysis-fns-regressions.R`: functions used to run regression analyses (invoked for estimating treatment effects for primary and secondary outcome)
  -   `3b-analysis-fns-ttest.R`: functions used to perform t-tests (invoked for estimating treatment effects for primary and secondary outcomes, without covariate adjustment)
  -   `3c-analysis-fns-variable-selection.R`: functions used to select variables included in the regression analyses
  
  </details>

  <details>
  <summary><b> 1-DataProcessing </b></summary>
    
  -   `run-data-processing.R`: constructs derivative datasets from `dat_analysis_shared.csv` for ease of use in analyses

  </details>

  <details>
  <summary><b> 2-Descriptives </b></summary>

  -   `run-descriptives.R`: runs all descriptive analysis (1-3 detailed below)
  -   `1-balance-table.R`: creates and saves balance tables
  -   `2-outcome-vars-summary.R`: creates and saves descriptive summaries of outcome variables
  -   `3-differential-dropout-check.R`: compares participants who were randomized to those who were removed or withdrawn from the study

  </details>

  <details>
  <summary><b> 3-Effects </b></summary>

-   `run-effects.R`: runs all effect estimation analysis (1-3 detailed below)
-   `1-assumptions.R`: visual checks on linear regression assumptions
-   `2a-between-group-primary.R`: estimates treatment effects for primary outcome (past 30-day substance use days) and tests for treatment effect heterogeneity
-   `2b-between-group-secondary.R`: estimates treatment effects for secondary outcomes and tests for treatment effect heterogeneity
-   `2c-pval-adjust.R`: adjusts secondary outcome p-values
-   `3-within-group`: calculates within group "effect size" for primary and secondary outcomes by comparing pre- and post- data within intervention arms
  
  </details>

  <details>
  <summary><b> 4-Engagement </b></summary>

  -   `run-engagement.R`: runs all engagement analysis (1-3 detailed below)
  -   `1-acc-fea.R`: summarizes accessibility and feasibility scores and estimates correlation with past 30-day substance use days
  -   `2-control-pdf.R`: summarizes control group's self-reported and submission-tracked engagement with psychoeducational PDFs

  </details>


  <details>
  <summary><b> 5-BivarCorrelations </b></summary>

  -   `run-bivariate-corr.R`: calculates bivariate correlations across subset of outcome variables and compares correlations across arms

  </details>


## Running

To run all analyses: </br>
```
bash ./Analysis/run-analysis.sh
```
