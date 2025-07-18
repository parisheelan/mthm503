library(dplyr)
library(skimr)
library(tidyr)
library(forcats)

readRenviron(".Renviron.R")
Sys.getenv("PGRUSER")

conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("PGRDATABASE"),
  host = Sys.getenv("PGRHOST"),
  user = Sys.getenv("PGRUSER"),
  password = Sys.getenv("PGRPASSWORD"),
  port = Sys.getenv("PGRPORT")
)

DBI::dbIsValid(conn)

tables <- DBI::dbListTables(conn)
tables

casualties <- DBI::dbReadTable(conn, "stats19_casualties")
accidents <- DBI::dbReadTable(conn, "stats19_accidents")
vehicles <- DBI::dbReadTable(conn, "stats19_vehicles")
names(casualties)
names(accidents)
names(vehicles)

DBI::dbDisconnect(conn)


casualty_pedestrian <- casualties %>%
  filter(casualty_type == 0)
pedestrian_data <- casualty_pedestrian %>%
  inner_join(accidents, by = "accident_index") %>%
  inner_join(vehicles, by = c("accident_index", "vehicle_reference"))
glimpse(pedestrian_data)


pedestrian_selected <- pedestrian_data %>%
  select(
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




# Step 4: Clean and handle missing values
pedestrian_clean <- pedestrian_selected %>%
  # Drop rows with missing target variable
  filter(!is.na(casualty_severity)) %>%
  
  # Remove rows with any NA in predictor columns 
  filter(
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
  
  # Convert categorical features to factors (if not already)
  mutate(
    sex_of_casualty = as.factor(sex_of_casualty),
    light_conditions = as.factor(light_conditions),
    weather_conditions = as.factor(weather_conditions),
    urban_or_rural_area = as.factor(urban_or_rural_area),
    vehicle_type = as.factor(vehicle_type),
    pedestrian_location = as.factor(pedestrian_location),
    casualty_severity = fct_relevel(as.factor(casualty_severity), "Slight", "Serious", "Fatal")
  )
# Peek at result
glimpse(pedestrian_clean)
skim(pedestrian_clean)


library(tidymodels)

set.seed(123)
data_split <- initial_split(pedestrian_clean, prop = 0.8, strata = casualty_severity)
train_data <- training(data_split)
test_data <- testing(data_split)


pedestrian_recipe <- recipe(casualty_severity ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

tree_model <- decision_tree(mode = "classification") %>%
  set_engine("rpart")

knn_model <- nearest_neighbor(mode = "classification", neighbors = 5) %>%
  set_engine("kknn")

tree_wf <- workflow() %>%
  add_model(tree_model) %>%
  add_recipe(pedestrian_recipe)

knn_wf <- workflow() %>%
  add_model(knn_model) %>%
  add_recipe(pedestrian_recipe)

library(kknn)
tree_fit <- tree_wf %>% fit(data = train_data)
knn_fit <- knn_wf %>% fit(data = train_data)


# Predict
tree_preds <- predict(tree_fit, test_data, type = "prob") %>%
  bind_cols(predict(tree_fit, test_data)) %>%
  bind_cols(test_data)

knn_preds <- predict(knn_fit, test_data, type = "prob") %>%
  bind_cols(predict(knn_fit, test_data)) %>%
  bind_cols(test_data)

# Accuracy
metrics(tree_preds, truth = casualty_severity, estimate = .pred_class)
metrics(knn_preds, truth = casualty_severity, estimate = .pred_class)

# ROC-AUC (multiclass AUC requires one-vs-rest)
roc_auc(tree_preds, truth = casualty_severity, .pred_Slight, .pred_Serious, .pred_Fatal)
roc_auc(knn_preds, truth = casualty_severity, .pred_Slight, .pred_Serious, .pred_Fatal)

conf_mat(knn_preds, truth = casualty_severity, estimate = .pred_class)
conf_mat(tree_preds, truth = casualty_severity, estimate = .pred_class)

rf_model <- rand_forest(mode = "classification", trees = 500) %>%
  set_engine("ranger")  # fast + supports probabilities

rf_wf <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(pedestrian_recipe)

rf_fit <- rf_wf %>% fit(data = train_data)




# Predict on test set
rf_preds <- predict(rf_fit, test_data, type = "prob") %>%
  bind_cols(predict(rf_fit, test_data)) %>%
  bind_cols(test_data)

# Accuracy
metrics(rf_preds, truth = casualty_severity, estimate = .pred_class)

# ROC AUC
roc_auc(rf_preds, truth = casualty_severity, .pred_Slight, .pred_Serious, .pred_Fatal)

# Confusion Matrix
conf_mat(rf_preds, truth = casualty_severity, estimate = .pred_class)

library(ggplot2) # Ensure ggplot2 is loaded for autoplot

roc_curve(tree_preds, truth = casualty_severity,
          .pred_Slight, .pred_Serious, .pred_Fatal) %>%
  autoplot() +
  labs(
    title = "ROC Curve for Decision Tree Model",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal()


roc_curve(knn_preds, truth = casualty_severity,
          .pred_Slight, .pred_Serious, .pred_Fatal) %>%
  autoplot() +
  labs(
    title = "ROC Curve for K-Nearest Neighbors (KNN) Model",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal()

library(yardstick) # Ensure yardstick is loaded for conf_mat

conf_mat(knn_preds, truth = casualty_severity, estimate = .pred_class) %>%
  autoplot(type = "heatmap") +
  labs(
    title = "Confusion Matrix for K-Nearest Neighbors (KNN) Model",
    x = "Predicted",
    y = "Actual"
  ) +
  theme_minimal()


roc_curve(rf_preds, truth = casualty_severity,
          .pred_Slight, .pred_Serious, .pred_Fatal) %>%
  autoplot() +
  labs(
    title = "ROC Curve for Random Forest Model",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal()



conf_mat(rf_preds, truth = casualty_severity, estimate = .pred_class) %>%
  autoplot(type = "heatmap") +
  labs(
    title = "Confusion Matrix for Random Forest Model",
    x = "Predicted",
    y = "Actual"
  ) +
  theme_minimal()



# Create the bar chart for sex_of_casualty
pedestrian_clean %>%
  count(sex_of_casualty) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = sex_of_casualty, y = n, fill = sex_of_casualty)) +
  geom_col(show.legend = FALSE) + # geom_col is for pre-computed counts
  geom_text(aes(label = scales::percent(prop)), vjust = -0.5) + # Add percentage labels
  labs(
    title = "Distribution of Sex of Casualty (Pedestrians)",
    x = "Sex of Casualty",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

# Create the bar chart for casualty_severity distribution
pedestrian_clean %>%
  count(casualty_severity) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = casualty_severity, y = n, fill = casualty_severity)) +
  geom_col(show.legend = FALSE) + # geom_col is for pre-computed counts
  geom_text(aes(label = scales::percent(prop)), vjust = -0.5) + # Add percentage labels
  labs(
    title = "Distribution of Pedestrian Casualty Severity",
    x = "Casualty Severity",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

