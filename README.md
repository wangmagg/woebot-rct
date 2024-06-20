# Woebot

Statistical analyses for Phase II W-SUD trial.

## Set-up

## File Descriptions

### `0-Config/`:

-   `0-config.R`: sources all config files

**`0-Files/`**

-   `dat_analysis_balance-table_dictionary.xlsx`: contains definition for each variable, the possible values and ranges it can take on, and how it got calculated (if it was not imported from the questionnaires directly)
-   `var_name_mapping.xlsx`: maps variable names from how they appear in the screening and survey questionnaires to ones that are more interpretable

**`1-Setup/`**

-   `1-libraries.R`: loads all necessary libraries
-   `2-input-paths.R`: pathnames for where data is stored (data contains PHI, so it is not uploaded in this repo)
-   `3-output-paths.R`: folder names for where script outputs should be saved
-   `4-constants.R`: variable constants that are shared across several scripts

**`2-Functions/`**

-   `1a-data-fns-filling.R`: functions for imputing or replacing data values
-   `1b-data-fns-loading.R`: functions for loading and merging raw data files
-   `1c-data-fns-recoding.R`: functions for recoding (reverse coding, collapsing small categories) and for creating composite variables
-   `2a-analysis-fns-balance.R`: functions used to assess balance across groups (invoked in constructing balance table)
-   `2b-analysis-fns-subgroups.R`: functions used to define subgroups (invoked for moderation analysis)
-   `2c-analysis-fns-summaries.R`: functions used to perform descriptive summaries of variables
-   `3a-analysis-fns-regressions.R`: functions used to run regression analyses (invoked for estimating treatment effects for primary and secondary outcome)
-   `3b-analysis-fns-ttest.R`: functions used to perform t-tests (invoked for estimating treatment effects for primary and secondary outcomes, without covariate adjustment)
-   `3c-analysis-fns-variable-selection.R`: functions used to select variables included in the regression analyses
-   `4-latexify-fns`: functions used to convert results into LaTeX table syntax (invoked for creating the tables in reports to the W-SUDS team)

### `1-Data-Curation/`:

-   `0-prep-datasets.R`: loads raw data files and constructs analytic datasets

### `2-Analysis/`:

- `0-run-analysis.sh`: bash script that executes all analyses

**`1-Descriptives/`**

-   `0-run-descriptives.R`: runs all descriptive analysis (1a-3b detailed below)
-   `1a-balance-table.R`: creates and saves balance tables
-   `1b-tobacco-balance-table.R`: creates and saves balance tables and other descriptives for tobacco users
-   `2a-outcome-vars-visualization.R`: creates and saves histograms, scatterplots, boxplots, and correlation maps of outcome variables
-   `2b-outcome-vars-summary.R`: creates and saves descriptive summaries of outcome variables
-   `3a-missingness.R`: creates and saves summaries of data missingness
-   `3b-outliers.R`: creates and saves descriptive summary of participants who have high (\> 90 days) past 30-day substance us

**`2-Effects/`**

-   `0-run-effects.R`: runs all effect estimation analysis (1-3 detailed below)
-   `1-assumptions.R`: visual checks on linear regression assumptions
-   `2a-between-group-primary.R`: estimates treatment effects for primary outcome (past 30-day substance use days) and tests for treatment effect heterogeneity
-   `2b-between-group-secondary.R`: estimates treatment effects for primary outcomes and tests for treatment effect heterogeneity
-   `3-within-group`: calculates within group "effect size" for primary and secondary outcomes by comparing pre- and post- data within intervention arms

**`3-Engagement/`**

-   `0-run-engagement.R`: runs all engagement analysis (1-3 detailed below)
-   `1-acc-fea.R`: summarizes accessibility and feasibility scores and estimates correlation with past 30-day substance use days
-   `2-control-pdf.R`: summarizes control group's self-reported and submission-tracked engagement with psychoeducational PDFs
-   `3-app-engage-summary.R`: summarizes WSUDs group's app engagement metrics

**`4-BivarCorrelations/`**

-   `0-bivariate-corr.R`: calculates bivariate correlations across subset of outcome variables

### `3-Latexify/`:

-   `0-latexify.sh`: runs all latexify scripts (1-2 below)
-   `1a-to-latex-itt.R`: saves results as LaTeX strings to make it easier to create the tables in the reports shared with the W-SUDS team (ITT analysis)
-   `1b-to-latex-pp.R`: saves results as LaTeX strings to make it easier to create the tables in the reports shared with the W-SUDS team (per-protocol analysis)

## Running

To run all data curation, analysis, and latexify scripts, excecute the following command: `bash ./0-run-woebot.sh`\
To run data curation only, execute the following command: `Rscript 1-Data-Curation/0-prep-datasets.R`\
To run analysis only, execute the following command: `bash 2-Analysis/0-run-analysis.sh`\
To run latexification only, execute the following command: `bash 3-Latexify/0-latexify.sh`
