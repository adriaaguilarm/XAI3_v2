# XAI3_v2

Random forest partial dependence analysis for a university explainable AI assignment.

## Repository Structure

- `src/`: reproducible R analysis code.
- `outputs/figures/`: generated PDP figures.
- `outputs/tables/`: generated model metrics, feature importance, and PDP data tables.
- `report/`: LaTeX report source and compiled PDF.
- `references/`: assignment brief.
- `day.csv`: daily bike rental dataset.
- `kc_house_data.csv`: house price dataset.

## Reproduce

Install the required R packages:

```r
install.packages(c("dplyr", "ggplot2", "ranger", "readr", "scales", "tidyr"))
```

Run the complete analysis:

```bash
Rscript src/01_pdp_random_forest.R
```

Compile the report:

```bash
cd report
latexmk -pdf -interaction=nonstopmode -halt-on-error main.tex
```

## Expected Outputs

The analysis writes figures to `outputs/figures/` and CSV tables to `outputs/tables/`. The final report is `report/main.pdf`.
