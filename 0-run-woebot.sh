#!/bin/bash

echo "1-Data-Curation/0-data.sh"
bash 1-Data-Curation/0-data.sh

echo "2-Analysis/0-run-analysis.sh"
bash 2-Analysis/0-run-analysis.sh

echo "3-Latexify/0-latexify.sh"
bash 3-Latexify/0-latexify.sh