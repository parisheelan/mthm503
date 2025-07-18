# R/functions.R

# Function to connect to the database and read raw data tables
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

# Function to filter for pedestrian casualties and join raw tables
f_prepare_pedestrian_data <- function(raw_data_list) {
  casualties <- raw_data_list$casualties
  accidents <- raw_data_list$accidents
  vehicles <- raw_data_list$vehicles
  
  casualty_pedestrian <- casualties %>%
    dplyr::filter(casualty_type == 0) # Filter for pedestrian casualties
  
  pedestrian_data <- casualty_pedestrian %>%
    dplyr::inner_join(accidents, by = "accident_index") %>%
    dplyr::inner_join(vehicles, by = c("accident_index", "vehicle_reference"))
  
  pedestrian_data
}

# Function to select relevant columns, handle missing values, and convert data types
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
    dplyr::filter(!is.na(casualty_severity)) %>% # Remove rows with NA in target variable
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
      # Convert selected columns to factors
      sex_of_casualty = as.factor(sex_of_casualty),
      light_conditions = as.factor(light_conditions),
      weather_conditions = as.factor(weather_conditions),
      urban_or_rural_area = as.factor(urban_or_rural_area),
      vehicle_type = as.factor(vehicle_type),
      pedestrian_location = as.factor(pedestrian_location),
      # Relevel casualty_severity for consistent ordering in plots/models
      casualty_severity = forcats::fct_relevel(as.factor(casualty_severity), "Slight", "Serious", "Fatal")
    )
  pedestrian_clean
}

# Function to split data into training and testing sets
f_split_data <- function(pedestrian_clean, seed = 123) {
  set.seed(seed)
  # Stratified split to maintain casualty_severity proportions
  data_split <- rsample::initial_split(pedestrian_clean, prop = 0.8, strata = casualty_severity)
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)
  list(train = train_data, test = test_data)
}

# Function to define the data preprocessing recipe, including SMOTE for class imbalance
f_define_recipe <- function(train_data) {
  recipe(casualty_severity ~ ., data = train_data) %>%
    recipes::step_other(recipes::all_nominal_predictors(), threshold = 0.01) %>% # Group infrequent nominal levels
    recipes::step_dummy(recipes::all_nominal_predictors()) %>% # Create dummy variables for nominal predictors
    recipes::step_normalize(recipes::all_numeric_predictors()) %>% # Normalize numeric predictors
    recipes::step_nzv(recipes::all_predictors()) %>% # Remove near-zero variance predictors
    recipes::step_corr(recipes::all_numeric_predictors(), threshold = 0.9) %>% # Remove highly correlated numeric predictors
    themis::step_smote(casualty_severity, over_ratio = 1, neighbors = 5) # SMOTE for class imbalance
}

# Function to fit a Decision Tree classification model
f_fit_tree_model <- function(train_data, recipe) {
  tree_model <- parsnip::decision_tree(mode = "classification") %>%
    parsnip::set_engine("rpart")
  tree_wf <- workflows::workflow() %>%
    workflows::add_model(tree_model) %>%
    workflows::add_recipe(recipe)
  tree_fit <- tree_wf %>% parsnip::fit(data = train_data)
  tree_fit
}

# Function to fit a K-Nearest Neighbors (KNN) classification model
f_fit_knn_model <- function(train_data, recipe) {
  knn_model <- parsnip::nearest_neighbor(mode = "classification", neighbors = 5) %>%
    parsnip::set_engine("kknn")
  knn_wf <- workflows::workflow() %>%
    workflows::add_model(knn_model) %>%
    workflows::add_recipe(recipe)
  knn_fit <- knn_wf %>% parsnip::fit(data = train_data)
  knn_fit
}

# Function to fit a Random Forest classification model
f_fit_rf_model <- function(train_data, recipe) {
  rf_model <- parsnip::rand_forest(mode = "classification", trees = 500) %>%
    parsnip::set_engine("ranger")
  rf_wf <- workflows::workflow() %>%
    workflows::add_model(rf_model) %>%
    workflows::add_recipe(recipe)
  rf_fit <- rf_wf %>% parsnip::fit(data = train_data)
  rf_fit
}

# Function to generate predictions (class and probabilities) from a fitted model
f_predict_model <- function(model_fit, test_data) {
  predictions <- predict(model_fit, test_data, type = "prob") %>% # Get class probabilities
    dplyr::bind_cols(predict(model_fit, test_data)) %>% # Get predicted class
    dplyr::bind_cols(test_data) # Bind original test data for evaluation
  predictions
}

# Function to calculate class-based classification metrics
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

# Function to calculate Area Under the Receiver Operating Characteristic (ROC AUC)
f_calculate_roc_auc <- function(predictions) {
  yardstick::roc_auc(
    predictions,
    truth = casualty_severity,
    .pred_Slight, .pred_Serious, .pred_Fatal # Specify probability columns for multi-class ROC AUC
  )
}

# Function to generate a confusion matrix object
f_get_conf_mat <- function(predictions) {
  yardstick::conf_mat(
    predictions,
    truth = casualty_severity,
    estimate = .pred_class
  )
}

# Function to analyze and summarize misclassified predictions
f_analyze_misclassifications <- function(predictions, model_name) {
  misclassified_df <- predictions %>%
    dplyr::filter(casualty_severity != .pred_class) # Filter for incorrect predictions
  
  misclass_summary <- misclassified_df %>%
    dplyr::count(truth = casualty_severity, predicted = .pred_class) %>% # Count misclassifications by true/predicted class
    dplyr::arrange(truth, predicted)
  
  total_test_samples <- nrow(predictions)
  overall_misclassification_rate <- nrow(misclassified_df) / total_test_samples
  
  misclass_counts_per_true_class <- predictions %>%
    dplyr::group_by(casualty_severity) %>%
    dplyr::summarise(
      total = n(),
      misclassified = sum(casualty_severity != .pred_class),
      misclassification_rate = misclassified / total
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(True_Severity = casualty_severity)
  
  misclass_rate_plot <- misclass_counts_per_true_class %>%
    ggplot2::ggplot(aes(x = True_Severity, y = misclassification_rate, fill = True_Severity)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(aes(label = scales::percent(misclassification_rate, accuracy = 0.1)), vjust = -0.5) +
    ggplot2::labs(
      title = paste("Misclassification Rate by True Severity (", model_name, ")"),
      x = "True Casualty Severity",
      y = "Misclassification Rate"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  list(
    summary_table = misclass_summary,
    overall_rate = overall_misclassification_rate,
    rate_plot = misclass_rate_plot
  )
}

# Function to plot the Receiver Operating Characteristic (ROC) curve
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

# Function to plot a confusion matrix heatmap
f_plot_confusion_matrix <- function(conf_mat_object, model_name) {
  ggplot2::autoplot(conf_mat_object, type = "heatmap") +
    ggplot2::labs(
      title = paste("Confusion Matrix for", model_name, "Model"),
      x = "Predicted",
      y = "Actual"
    ) +
    ggplot2::theme_minimal()
}

# Function to plot the distribution of 'sex_of_casualty'
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

# Function to plot the distribution of 'casualty_severity'
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