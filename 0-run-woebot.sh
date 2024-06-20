#!/bin/bash

echo "1-Data-Curation/0-prep-datasets.R"
Rscript 1-Data-Curation/0-prep-datasets.R

echo "2-Analysis/0-run-analysis.sh"
bash 2-Analysis/0-run-analysis.sh

echo "3-Latexify/0-latexify.sh"
bash 3-Latexify/0-latexify.sh