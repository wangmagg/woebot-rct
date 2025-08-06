#!/bin/bash
echo "Analysis/1-DataProcessing/run-data-processing.R"
Rscript Analysis/1-DataProcessing/run-data-processing.R

echo "Analysis/2-Descriptives/0-run-descriptives.R"
Rscript Analysis/2-Descriptives/run-descriptives.R

echo "Analysis/3-Effects/0-run-effects.R"
Rscript Analysis/3-Effects/run-effects.R

echo "Analysis/4-Engagement/0-run-engagement.R"
Rscript Analysis/4-Engagement/run-engagement.R

echo "Analysis/5-BivarCorrelations/run-bivariate-corr.R"
Rscript Analysis/5-BivarCorrelations/run-bivariate-corr.R
