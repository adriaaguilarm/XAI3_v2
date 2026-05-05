library(dplyr)
library(ranger)
library(readr)
library(tidyr)

set.seed(20260505)

house_sample_size <- 1500

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

model_metrics <- function(model, dataset_name, target, n_training, n_pdp) {
  tibble(
    dataset = dataset_name,
    target = target,
    n_training = n_training,
    n_pdp_sample = n_pdp,
    trees = model$num.trees,
    mtry = model$mtry,
    min_node_size = model$min.node.size,
    oob_mse = model$prediction.error,
    oob_rmse = sqrt(model$prediction.error),
    oob_r2 = model$r.squared
  )
}

importance_table <- function(model, dataset_name) {
  tibble(
    dataset = dataset_name,
    feature = names(model$variable.importance),
    importance = as.numeric(model$variable.importance)
  ) %>%
    arrange(dataset, desc(importance))
}

bike <- read_csv("day.csv", show_col_types = FALSE) %>%
  mutate(
    dteday = as.Date(dteday),
    days_since_2011 = as.integer(dteday - as.Date("2011-01-01")) + 1,
    season = factor(season),
    holiday = factor(holiday),
    weekday = factor(weekday),
    workingday = factor(workingday),
    weathersit = factor(weathersit)
  )

bike_features <- c(
  "days_since_2011",
  "season",
  "holiday",
  "weekday",
  "workingday",
  "weathersit",
  "temp",
  "hum",
  "windspeed"
)

bike_train <- bike %>%
  select(cnt, all_of(bike_features))

bike_model <- ranger(
  cnt ~ .,
  data = as.data.frame(bike_train),
  num.trees = 500,
  mtry = 3,
  min.node.size = 5,
  importance = "permutation",
  seed = 20260505,
  respect.unordered.factors = "order"
)

house_features <- c(
  "bedrooms",
  "bathrooms",
  "sqft_living",
  "sqft_lot",
  "floors",
  "yr_built"
)

house <- read_csv("kc_house_data.csv", show_col_types = FALSE) %>%
  filter(bedrooms <= 8) %>%
  select(price, all_of(house_features)) %>%
  drop_na()

if (nrow(house) < house_sample_size) {
  stop("The house dataset does not contain enough rows after cleaning.")
}

house_train <- house %>%
  slice_sample(n = house_sample_size)

house_model <- ranger(
  price ~ .,
  data = as.data.frame(house_train),
  num.trees = 500,
  mtry = 3,
  min.node.size = 5,
  importance = "permutation",
  seed = 20260505
)

metrics <- bind_rows(
  model_metrics(bike_model, "Bike rentals", "cnt", nrow(bike_train), 50),
  model_metrics(house_model, "House prices", "price", nrow(house_train), nrow(house_train))
)

feature_importance <- bind_rows(
  importance_table(bike_model, "Bike rentals"),
  importance_table(house_model, "House prices")
)

write_csv(metrics, "outputs/tables/model_metrics.csv")
write_csv(feature_importance, "outputs/tables/feature_importance.csv")
