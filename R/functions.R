

# Load necessary libraries within functions for robustness,
# or ensure they are loaded globally in _targets.R.
# Using package::function() is generally safer in functions for targets.

# Function to connect to the database and read raw tables
f_read_raw_data <- function() {
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("PGRDATABASE"),
    host = Sys.getenv("PGRHOST"),
    user = Sys.getenv("PGRUSER"),
    password = Sys.getenv("PGRPASSWORD"),
    port = Sys.getenv("PGRPORT")
  )
  
  if (!DBI::dbIsValid(conn)) {
    stop("Database connection is not valid.")
  }
  
  casualties <- DBI::dbReadTable(conn, "stats19_casualties")
  accidents <- DBI::dbReadTable(conn, "stats19_accidents")
  vehicles <- DBI::dbReadTable(conn, "stats19_vehicles")
  
  DBI::dbDisconnect(conn)
  
  list(
    casualties = casualties,
    accidents = accidents,
    vehicles = vehicles
  )
}

# Function to filter for pedestrian casualties and join tables
f_prepare_pedestrian_data <- function(raw_data_list) {
  casualties <- raw_data_list$casualties
  accidents <- raw_data_list$accidents
  vehicles <- raw_data_list$vehicles
  
  casualty_pedestrian <- casualties %>%
    dplyr::filter(casualty_type == 0)
  
  pedestrian_data <- casualty_pedestrian %>%
    dplyr::inner_join(accidents, by = "accident_index") %>%
    dplyr::inner_join(vehicles, by = c("accident_index", "vehicle_reference"))
  
  pedestrian_data
}

# Function to select relevant columns and clean/handle missing values
f_clean_pedestrian_data <- function(pedestrian_data) {
  pedestrian_selected <- pedestrian_data %>%
    dplyr::select(
      age_of_casualty,
      sex_of_casualty,
      light_conditions,
      weather_conditions,
      urban_or_rural_area,
      speed_limit_mph,
      vehicle_type,
      age_of_driver,
      pedestrian_location,
      casualty_severity
    )
  
  pedestrian_clean <- pedestrian_selected %>%
    dplyr::filter(!is.na(casualty_severity)) %>%
    dplyr::filter(
      !is.na(age_of_casualty),
      !is.na(sex_of_casualty),
      !is.na(light_conditions),
      !is.na(weather_conditions),
      !is.na(urban_or_rural_area),
      !is.na(speed_limit_mph),
      !is.na(vehicle_type),
      !is.na(age_of_driver),
      !is.na(pedestrian_location)
    ) %>%
    dplyr::mutate(
      sex_of_casualty = as.factor(sex_of_casualty),
      light_conditions = as.factor(light_conditions),
      weather_conditions = as.factor(weather_conditions),
      urban_or_rural_area = as.factor(urban_or_rural_area),
      vehicle_type = as.factor(vehicle_type),
      pedestrian_location = as.factor(pedestrian_location),
      casualty_severity = forcats::fct_relevel(as.factor(casualty_severity), "Slight", "Serious", "Fatal")
    )
  pedestrian_clean
}

# Function to split data into training and testing sets
f_split_data <- function(pedestrian_clean, seed = 123) {
  set.seed(seed)
  data_split <- rsample::initial_split(pedestrian_clean, prop = 0.8, strata = casualty_severity)
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)
  list(train = train_data, test = test_data)
}

# Function to define the preprocessing recipe
f_define_recipe <- function(train_data) {
  recipe(casualty_severity ~ ., data = train_data) %>%
    recipes::step_normalize(recipes::all_numeric_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal_predictors())
}

# R/functions.R

# ... (previous functions like f_read_raw_data, f_prepare_pedestrian_data, f_clean_pedestrian_data, f_split_data, f_define_recipe) ...

# Function to fit Decision Tree model
f_fit_tree_model <- function(train_data, recipe) {
  tree_model <- parsnip::decision_tree(mode = "classification") %>%
    parsnip::set_engine("rpart")
  tree_wf <- workflows::workflow() %>%
    workflows::add_model(tree_model) %>%
    workflows::add_recipe(recipe)
  tree_fit <- tree_wf %>% parsnip::fit(data = train_data)
  tree_fit
}

# Function to fit KNN model
f_fit_knn_model <- function(train_data, recipe) {
  knn_model <- parsnip::nearest_neighbor(mode = "classification", neighbors = 5) %>%
    parsnip::set_engine("kknn")
  knn_wf <- workflows::workflow() %>%
    workflows::add_model(knn_model) %>%
    workflows::add_recipe(recipe)
  knn_fit <- knn_wf %>% parsnip::fit(data = train_data)
  knn_fit
}

# Function to fit Random Forest model
f_fit_rf_model <- function(train_data, recipe) {
  rf_model <- parsnip::rand_forest(mode = "classification", trees = 500) %>%
    parsnip::set_engine("ranger")
  rf_wf <- workflows::workflow() %>%
    workflows::add_model(rf_model) %>%
    workflows::add_recipe(recipe)
  rf_fit <- rf_wf %>% parsnip::fit(data = train_data)
  rf_fit
}

# Function to make predictions
f_predict_model <- function(model_fit, test_data) {
  predictions <- predict(model_fit, test_data, type = "prob") %>%
    dplyr::bind_cols(predict(model_fit, test_data)) %>%
    dplyr::bind_cols(test_data) # Keep true labels for evaluation
  predictions
}

# --- NEW/MODIFIED METRICS FUNCTIONS ---

# Function to calculate class-based metrics (accuracy, precision, recall, f_meas)
f_calculate_class_metrics <- function(predictions) {
  metrics_tbl <- yardstick::metric_set(
    yardstick::accuracy,
    yardstick::precision,
    yardstick::recall,
    yardstick::f_meas
  )(
    predictions,
    truth = casualty_severity,
    estimate = .pred_class
  )
  metrics_tbl
}

# Function to calculate ROC AUC specifically (requires probability columns)
f_calculate_roc_auc <- function(predictions) {
  yardstick::roc_auc(
    predictions,
    truth = casualty_severity,
    .pred_Slight, .pred_Serious, .pred_Fatal
  )
}

# Function to get the Confusion Matrix object (not a single metric)
f_get_conf_mat <- function(predictions) {
  yardstick::conf_mat(
    predictions,
    truth = casualty_severity,
    estimate = .pred_class
  )
}

# --- PLOTTING FUNCTIONS (already mostly correct, just ensure they use the correct input) ---

# Function to plot ROC curve
f_plot_roc <- function(predictions, model_name) {
  roc_curve(predictions, truth = casualty_severity,
            .pred_Slight, .pred_Serious, .pred_Fatal) %>%
    ggplot2::autoplot() +
    ggplot2::labs(
      title = paste("ROC Curve for", model_name, "Model"),
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    ggplot2::theme_minimal()
}

# Function to plot Confusion Matrix (takes the conf_mat object directly)
f_plot_confusion_matrix <- function(conf_mat_object, model_name) {
  ggplot2::autoplot(conf_mat_object, type = "heatmap") +
    ggplot2::labs(
      title = paste("Confusion Matrix for", model_name, "Model"),
      x = "Predicted",
      y = "Actual"
    ) +
    ggplot2::theme_minimal()
}

# Function to create sex_of_casualty bar chart
f_plot_sex_of_casualty <- function(pedestrian_clean) {
  pedestrian_clean %>%
    dplyr::count(sex_of_casualty) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    ggplot2::ggplot(aes(x = sex_of_casualty, y = n, fill = sex_of_casualty)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(aes(label = scales::percent(prop)), vjust = -0.5) +
    ggplot2::labs(
      title = "Distribution of Sex of Casualty (Pedestrians)",
      x = "Sex of Casualty",
      y = "Count"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}

# Function to create casualty_severity distribution bar chart
f_plot_casualty_severity <- function(pedestrian_clean) {
  pedestrian_clean %>%
    dplyr::count(casualty_severity) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    ggplot2::ggplot(aes(x = casualty_severity, y = n, fill = casualty_severity)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(aes(label = scales::percent(prop)), vjust = -0.5) +
    ggplot2::labs(
      title = "Distribution of Pedestrian Casualty Severity",
      x = "Casualty Severity",
      y = "Count"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}
