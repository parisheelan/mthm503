library(dplyr)
library(ggplot2)
library(tidyr)
library(skimr)
library(kableExtra)
library(vtable)
library(mgcv)
library(nnet)
library(caret)
library(testthat)

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

extrication <-  DBI::dbReadTable(conn, "fire_rescue_extrication_casualties")
names(extrication)
table(extrication$extrication)
table(extrication$age_band)
table(extrication$financial_year)
table(extrication$sex)
head(extrication)


# --- Data Preparation and Preprocessing ---

extrication_clean <- extrication %>%
  mutate(
    # Convert to factor
    financial_year = as.factor(financial_year),
    
    sex = as.factor(sex), 
    age_band = factor(age_band,
                      levels = c("0-16", "17-24", "25-39", "40-64", "65+"),
                      ordered = TRUE),
    # Create the response variable as a factor from the original 'extrication' column
    extrication = as.factor(extrication)
  ) %>%
  # Remove rows with NA values that were introduced by 'Unknown' or other missing data
  drop_na(extrication, n_casualties, sex, age_band)

# --- Unit Tests for Data Preparation (using testthat) ---
cat("\n--- Running Unit Tests for Data Preparation ---\n")

test_that("extrication_clean has no NA values in critical modeling columns", {
  expect_true(all(!is.na(extrication_clean$extrication)), info = "NA found in extrication")
  expect_true(all(!is.na(extrication_clean$n_casualties)), info = "NA found in n_casualties")
  expect_true(all(!is.na(extrication_clean$sex)), info = "NA found in sex")
  expect_true(all(!is.na(extrication_clean$age_band)), info = "NA found in age_band")
})

test_that("extrication (response) is a factor with more than one level", {
  expect_true(is.factor(extrication_clean$extrication))
  expect_gt(nlevels(extrication_clean$extrication), 1) # Ensure it's not degenerate
})

test_that("age_band is an ordered factor with specified levels", {
  expect_true(is.ordered(extrication_clean$age_band))
  expect_equal(levels(extrication_clean$age_band), c("0-16", "17-24", "25-39", "40-64", "65+"))
})

test_that("financial_year is a factor", {
  expect_true(is.factor(extrication_clean$financial_year))
})

test_that("sex is a factor", {
  expect_true(is.factor(extrication_clean$sex))
})

cat("--- Unit Tests for Data Preparation Complete ---\n\n")

# Verify the changes
vtable(extrication_clean)

# --- Detailed EDA Plots for Balance and Distribution ---

# Overall Distribution of Extrication Methods
ggplot(extrication_clean, aes(x = extrication)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Overall Distribution of Extrication Methods", x = "Extrication Method", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Overall Distribution of Sex
ggplot(extrication_clean, aes(x = sex)) +
  geom_bar(fill = "lightcoral") +
  labs(title = "Overall Distribution of Sex", x = "Sex", y = "Count") +
  theme_minimal()

# Overall Distribution of Age Band
ggplot(extrication_clean, aes(x = age_band)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Overall Distribution of Age Bands", x = "Age Band", y = "Count") +
  theme_minimal()

# Overall Distribution of Financial Year
ggplot(extrication_clean, aes(x = financial_year)) +
  geom_bar(fill = "darkseagreen") +
  labs(title = "Overall Distribution of Financial Years", x = "Financial Year", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Proportion of Extrication Methods by Sex
ggplot(extrication_clean, aes(x = sex, fill = extrication)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Extrication Method by Sex", y = "Proportion", fill = "Method") +
  theme_minimal()

# Proportion of Extrication Methods by Age Band
ggplot(extrication_clean, aes(x = age_band, fill = extrication)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Extrication Method by Age Band", y = "Proportion", fill = "Method") +
  theme_minimal()

# Proportion of Extrication Methods by Financial Year
ggplot(extrication_clean, aes(x = financial_year, fill = extrication)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Extrication Method by Financial Year", y = "Proportion", fill = "Method") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Histogram for n_casualties
ggplot(extrication_clean, aes(x = n_casualties)) +
  geom_histogram(binwidth = 10, fill = "darkorchid", color = "black") +
  labs(title = "Histogram of Number of Casualties (Binwidth = 5)", x = "Number of Casualties", y = "Frequency") +
  theme_minimal()

# --- Data Splitting (Training and Test Sets) ---
set.seed(42)
trainIndex <- createDataPartition(extrication_clean$extrication, p = 0.7, list = FALSE)
train_data <- extrication_clean[trainIndex, ]
test_data <- extrication_clean[-trainIndex, ]

cat("Training data rows:", nrow(train_data), "\n")
cat("Test data rows:", nrow(test_data), "\n")
cat("Proportion of response classes in training data:\n")
print(prop.table(table(train_data$extrication)))

# --- Model Building and Evaluation Helper Function ---
evaluate_model <- function(model, test_data, model_name) {
  cat(paste0("\n--- Evaluating ", model_name, " ---\n"))
  cat("\nModel Summary:\n")
  print(summary(model))
  
  # Get predicted classes
  if (inherits(model, "multinom")) { # nnet::multinom model
    test_predictions_class <- predict(model, newdata = test_data, type = "class")
  } else if (inherits(model, "gam") && model$family$family == "multinomial") { # mgcv::gam multinomial model
    test_predictions_prob_matrix <- predict(model, newdata = test_data, type = "response")
    test_predictions_class <- factor(colnames(test_predictions_prob_matrix)[apply(test_predictions_prob_matrix, 1, which.max)],
                                     levels = levels(test_data$extrication))
  } else {
    stop(paste("Unsupported model type for prediction:", class(model)))
  }
  
  cat("\nConfusion Matrix:\n")
  levels_response <- levels(test_data$extrication)
  print(confusionMatrix(factor(test_predictions_class, levels = levels_response),
                        factor(test_data$extrication, levels = levels_response)))
  
  if (inherits(model, "gam")) {
    cat("\nPlots of Smooth Terms:\n")
    plot(model, pages = 1, seWithMean = TRUE)
  }
  cat("\n--- End of Evaluation for ", model_name, " ---\n")
}


# --- Model : Multinomial GLM (With Sex-Age_Band Interaction) ---
cat("\n--- Building Multinomial GLM: With Sex-Age_Band Interaction ---\n")
glm_model_interaction <- nnet::multinom(extrication ~ sex * age_band + n_casualties + financial_year,
                                        data = train_data, MaxNWts = 5000, trace = FALSE)
evaluate_model(glm_model_interaction, test_data, "Multinomial GLM (Sex-Age_Band Interaction)")



# --- Visualize Confusion Matrix as a Heatmap ---

# Get predicted classes from the GLM model
test_predictions_class <- predict(glm_model_interaction, newdata = test_data, type = "class")

# Create the confusion matrix object
levels_response <- levels(test_data$extrication)
cm <- confusionMatrix(factor(test_predictions_class, levels = levels_response),
                      factor(test_data$extrication, levels = levels_response))

# Convert the confusion matrix table to a data frame for ggplot
cm_df <- as.data.frame(cm$table)
colnames(cm_df) <- c("Prediction", "Reference", "Count")

# Create the heatmap plot
confusion_heatmap <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Count)) +
  geom_tile(color = "white") + # Add white borders to tiles
  scale_fill_gradient(low = "white", high = "steelblue") + # Color gradient for counts
  geom_text(aes(label = Count), vjust = 1, size = 4, color = "black") + # Add count numbers
  labs(title = "Confusion Matrix Heatmap for Multinomial GLM",
       x = "Actual Extrication Method",
       y = "Predicted Extrication Method") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), # Rotate x-axis labels
        plot.title = element_text(hjust = 0.5)) + # Center plot title
  coord_fixed() # Ensure tiles are square

print(confusion_heatmap)
