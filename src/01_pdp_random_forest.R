library(dplyr)
library(ggplot2)
library(ranger)
library(readr)
library(scales)
library(tidyr)

set.seed(20260505)

bike_pdp_sample_size <- 50
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

predict_ranger <- function(model, newdata) {
  predict(model, data = as.data.frame(newdata))$predictions
}

sample_exact_rows <- function(data, n, label) {
  if (nrow(data) < n) {
    stop(label, " requires exactly ", n, " rows, but only ", nrow(data), " are available.")
  }

  sampled <- slice_sample(data, n = n)

  if (nrow(sampled) != n) {
    stop(label, " sampling failed to return exactly ", n, " rows.")
  }

  sampled
}

grid_values <- function(x, n = 60, discrete_limit = 30) {
  x <- x[!is.na(x)]
  lower <- as.numeric(quantile(x, 0.01, names = FALSE))
  upper <- as.numeric(quantile(x, 0.99, names = FALSE))
  x_trimmed <- x[x >= lower & x <= upper]
  unique_values <- sort(unique(x_trimmed))

  if (length(unique_values) <= discrete_limit) {
    unique_values
  } else {
    seq(lower, upper, length.out = n)
  }
}

pdp_1d <- function(model, sample_data, feature, grid) {
  bind_rows(lapply(grid, function(value) {
    newdata <- sample_data
    newdata[[feature]] <- value
    tibble(
      feature = feature,
      value = value,
      prediction = mean(predict_ranger(model, newdata))
    )
  }))
}

pdp_2d <- function(model, sample_data, feature_x, feature_y, grid_x, grid_y) {
  pdp_grid <- expand.grid(
    value_x = grid_x,
    value_y = grid_y,
    KEEP.OUT.ATTRS = FALSE
  )

  bind_rows(lapply(seq_len(nrow(pdp_grid)), function(i) {
    newdata <- sample_data
    newdata[[feature_x]] <- pdp_grid$value_x[i]
    newdata[[feature_y]] <- pdp_grid$value_y[i]
    tibble(
      !!feature_x := pdp_grid$value_x[i],
      !!feature_y := pdp_grid$value_y[i],
      prediction = mean(predict_ranger(model, newdata))
    )
  }))
}

pdp_summary_1d <- function(pdp_data, dataset_name) {
  pdp_data %>%
    group_by(feature) %>%
    arrange(value, .by_group = TRUE) %>%
    summarise(
      dataset = dataset_name,
      min_prediction = min(prediction),
      value_at_min = value[which.min(prediction)],
      max_prediction = max(prediction),
      value_at_max = value[which.max(prediction)],
      first_grid_prediction = first(prediction),
      last_grid_prediction = last(prediction),
      edge_delta = last(prediction) - first(prediction),
      .groups = "drop"
    ) %>%
    select(dataset, everything())
}

pdp_summary_2d <- function(pdp_data, dataset_name, feature_x, feature_y) {
  min_index <- which.min(pdp_data$prediction)
  max_index <- which.max(pdp_data$prediction)

  tibble(
    dataset = dataset_name,
    feature = paste(feature_x, feature_y, sep = "_"),
    min_prediction = pdp_data$prediction[min_index],
    value_at_min = NA_real_,
    max_prediction = pdp_data$prediction[max_index],
    value_at_max = NA_real_,
    first_grid_prediction = NA_real_,
    last_grid_prediction = NA_real_,
    edge_delta = NA_real_,
    min_feature_x_value = pdp_data[[feature_x]][min_index],
    min_feature_y_value = pdp_data[[feature_y]][min_index],
    max_feature_x_value = pdp_data[[feature_x]][max_index],
    max_feature_y_value = pdp_data[[feature_y]][max_index]
  )
}

theme_pdp <- function() {
  theme_minimal(base_size = 13) +
    theme(
      axis.title = element_text(color = "#2f2f2f"),
      axis.text = element_text(color = "#555555"),
      panel.grid.major = element_line(color = "#e7e7e7", linewidth = 0.7),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
}

theme_pdp_heatmap <- function() {
  theme_minimal(base_size = 13) +
    theme(
      axis.title = element_text(color = "#2f2f2f"),
      axis.text = element_text(color = "#555555"),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}

save_pdp_plot <- function(plot, filename, width = 9, height = 5.5) {
  ggsave(
    file.path("outputs/figures", filename),
    plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

plot_pdp_1d <- function(
    pdp_data,
    sample_data,
    feature_name,
    x_label,
    y_label) {
  ggplot(filter(pdp_data, feature == feature_name), aes(value, prediction)) +
    geom_line(color = "#1f4e79", linewidth = 1.15) +
    geom_rug(
      data = sample_data,
      aes(x = .data[[feature_name]]),
      inherit.aes = FALSE,
      sides = "b",
      alpha = 0.42,
      color = "#705d48"
    ) +
    scale_y_continuous(labels = label_comma()) +
    labs(
      x = x_label,
      y = y_label
    ) +
    theme_pdp()
}

tile_step <- function(x) {
  unique_values <- sort(unique(x))
  min(diff(unique_values))
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

set.seed(20260505)
bike_pdp_sample <- bike_train %>%
  sample_exact_rows(bike_pdp_sample_size, "Bike PDP sample")

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

set.seed(20260505)
house_train <- house %>%
  sample_exact_rows(house_sample_size, "House training and PDP sample")

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
  model_metrics(bike_model, "Bike rentals", "cnt", nrow(bike_train), nrow(bike_pdp_sample)),
  model_metrics(house_model, "House prices", "price", nrow(house_train), nrow(house_train))
)

feature_importance <- bind_rows(
  importance_table(bike_model, "Bike rentals"),
  importance_table(house_model, "House prices")
)

write_csv(metrics, "outputs/tables/model_metrics.csv")
write_csv(feature_importance, "outputs/tables/feature_importance.csv")

bike_pdp_features <- c("days_since_2011", "temp", "hum", "windspeed")

bike_pdp_1d <- bind_rows(lapply(bike_pdp_features, function(feature) {
  pdp_1d(
    bike_model,
    bike_pdp_sample %>% select(all_of(bike_features)),
    feature,
    grid_values(bike_train[[feature]], n = 60)
  )
}))

bike_pdp_temp_hum_2d <- pdp_2d(
  bike_model,
  bike_pdp_sample %>% select(all_of(bike_features)),
  "temp",
  "hum",
  grid_values(bike_train$temp, n = 45),
  grid_values(bike_train$hum, n = 45)
)

house_pdp_features <- c("bedrooms", "bathrooms", "sqft_living", "floors")

house_pdp_1d <- bind_rows(lapply(house_pdp_features, function(feature) {
  pdp_1d(
    house_model,
    house_train %>% select(all_of(house_features)),
    feature,
    grid_values(house_train[[feature]], n = 60)
  )
}))

pdp_summary <- bind_rows(
  pdp_summary_1d(bike_pdp_1d, "Bike rentals"),
  pdp_summary_2d(bike_pdp_temp_hum_2d, "Bike rentals", "temp", "hum"),
  pdp_summary_1d(house_pdp_1d, "House prices")
)

write_csv(bike_pdp_1d, "outputs/tables/bike_pdp_1d.csv")
write_csv(bike_pdp_temp_hum_2d, "outputs/tables/bike_pdp_temp_hum_2d.csv")
write_csv(house_pdp_1d, "outputs/tables/house_pdp_1d.csv")
write_csv(pdp_summary, "outputs/tables/pdp_summary.csv")

save_pdp_plot(
  plot_pdp_1d(
    bike_pdp_1d,
    bike_pdp_sample,
    "days_since_2011",
    "Days since 2011",
    "Predicted daily rentals"
  ),
  "bike_pdp_days_since_2011.png"
)

save_pdp_plot(
  plot_pdp_1d(
    bike_pdp_1d,
    bike_pdp_sample,
    "temp",
    "Temperature (normalized dataset scale)",
    "Predicted daily rentals"
  ),
  "bike_pdp_temperature.png"
)

save_pdp_plot(
  plot_pdp_1d(
    bike_pdp_1d,
    bike_pdp_sample,
    "hum",
    "Humidity (normalized dataset scale)",
    "Predicted daily rentals"
  ),
  "bike_pdp_humidity.png"
)

save_pdp_plot(
  plot_pdp_1d(
    bike_pdp_1d,
    bike_pdp_sample,
    "windspeed",
    "Wind speed (normalized dataset scale)",
    "Predicted daily rentals"
  ),
  "bike_pdp_windspeed.png"
)

bike_temp_hum_heatmap <- ggplot(
  bike_pdp_temp_hum_2d,
  aes(temp, hum, fill = prediction)
) +
  geom_tile(
    width = tile_step(bike_pdp_temp_hum_2d$temp) * 1.02,
    height = tile_step(bike_pdp_temp_hum_2d$hum) * 1.02
  ) +
  geom_rug(
    data = bike_pdp_sample,
    aes(x = temp, y = hum),
    inherit.aes = FALSE,
    sides = "bl",
    alpha = 0.35,
    color = "#705d48"
  ) +
  scale_fill_gradientn(
    colours = c("#2d2233", "#374b83", "#5f87d9", "#7fb0ff"),
    labels = label_comma(),
    name = "Predicted\nrentals"
  ) +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  coord_cartesian(expand = FALSE) +
  labs(
    x = "Temperature (normalized dataset scale)",
    y = "Humidity (normalized dataset scale)"
  ) +
  theme_pdp_heatmap()

save_pdp_plot(
  bike_temp_hum_heatmap,
  "bike_pdp_2d_temperature_humidity_heatmap.png",
  width = 9,
  height = 6.5
)

save_pdp_plot(
  plot_pdp_1d(
    house_pdp_1d,
    house_train,
    "bedrooms",
    "Bedrooms",
    "Predicted price"
  ),
  "house_pdp_bedrooms.png"
)

save_pdp_plot(
  plot_pdp_1d(
    house_pdp_1d,
    house_train,
    "bathrooms",
    "Bathrooms",
    "Predicted price"
  ),
  "house_pdp_bathrooms.png"
)

save_pdp_plot(
  plot_pdp_1d(
    house_pdp_1d,
    house_train,
    "sqft_living",
    "Living area (sqft)",
    "Predicted price"
  ),
  "house_pdp_sqft_living.png"
)

save_pdp_plot(
  plot_pdp_1d(
    house_pdp_1d,
    house_train,
    "floors",
    "Floors",
    "Predicted price"
  ),
  "house_pdp_floors.png"
)
