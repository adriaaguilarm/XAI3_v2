# XAI3 v2

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

Generated figure files:

- `outputs/figures/bike_pdp_days_since_2011.png`
- `outputs/figures/bike_pdp_temperature.png`
- `outputs/figures/bike_pdp_humidity.png`
- `outputs/figures/bike_pdp_windspeed.png`
- `outputs/figures/bike_pdp_2d_temperature_humidity_heatmap.png`
- `outputs/figures/house_pdp_bedrooms.png`
- `outputs/figures/house_pdp_bathrooms.png`
- `outputs/figures/house_pdp_sqft_living.png`
- `outputs/figures/house_pdp_floors.png`

Generated table files:

- `outputs/tables/model_metrics.csv`
- `outputs/tables/feature_importance.csv`
- `outputs/tables/pdp_summary.csv`
- `outputs/tables/bike_pdp_1d.csv`
- `outputs/tables/bike_pdp_temp_hum_2d.csv`
- `outputs/tables/house_pdp_1d.csv`

## Analysis Notes

- The bike rental model is trained on all 731 daily records.
- Bike PDPs use exactly 50 sampled observations.
- Bike weather PDP axes use the original normalized dataset scales for `temp`, `hum`, and `windspeed`.
- The house price model and house PDPs use a 1,500-row sample.
- House records with more than eight bedrooms are removed before sampling, which removes the anomalous 33-bedroom observation.
- A fixed random seed is used for reproducibility.
