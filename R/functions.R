
# Function to establish a database connection.
f_read_extrication_data <- function() {
  readRenviron(".Renviron.R") 
  
  conn <- NULL 
  tryCatch({
    conn <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("PGRDATABASE"),
      host = Sys.getenv("PGRHOST"),
      user = Sys.getenv("PGRUSER"),
      password = Sys.getenv("PGRPASSWORD"),
      port = Sys.getenv("PGRPORT")
    )
    
    if (!DBI::dbIsValid(conn)) {
      stop("Database connection is not valid. Check credentials or network.")
    }
    
    extrication_data <- DBI::dbReadTable(conn, "fire_rescue_extrication_casualties")
    
    return(extrication_data)
  }, finally = {
    if (!is.null(conn) && DBI::dbIsValid(conn)) {
      DBI::dbDisconnect(conn)
      message("Database connection disconnected.")
    }
  })
}

# Function to perform data cleaning and preprocessing.
clean_data <- function(extrication_raw) {
  extrication_clean <- extrication_raw %>%
    mutate(
      financial_year = as.factor(financial_year),
      sex = as.factor(sex),
      age_band = factor(age_band,
                        levels = c("0-16", "17-24", "25-39", "40-64", "65+"),
                        ordered = TRUE),
      extrication = as.factor(extrication)
    ) %>%
    drop_na(extrication, n_casualties, sex, age_band)
  return(extrication_clean)
}

# Function to run unit tests for data quality after cleaning.
run_data_tests <- function(extrication_clean) {
  cat("\n--- Running Unit Tests for Data Preparation ---\n")
  test_that("extrication_clean has no NA values in critical modeling columns", {
    expect_true(all(!is.na(extrication_clean$extrication)), info = "NA found in extrication")
    expect_true(all(!is.na(extrication_clean$n_casualties)), info = "NA found in n_casualties")
    expect_true(all(!is.na(extrication_clean$sex)), info = "NA found in sex")
    expect_true(all(!is.na(extrication_clean$age_band)), info = "NA found in age_band")
  })
  test_that("extrication (response) is a factor with more than one level", {
    expect_true(is.factor(extrication_clean$extrication))
    expect_gt(nlevels(extrication_clean$extrication), 1)
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
  invisible(NULL)
}

# Function to generate and print exploratory data analysis plots.
generate_eda_plots <- function(data) {
  p1 <- ggplot(data, aes(x = extrication)) +
    geom_bar(fill = "steelblue") +
    labs(title = "Overall Distribution of Extrication Methods", x = "Extrication Method", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p1)
  
  p2 <- ggplot(data, aes(x = sex)) +
    geom_bar(fill = "lightcoral") +
    labs(title = "Overall Distribution of Sex", x = "Sex", y = "Count") +
    theme_minimal()
  print(p2)
  
  p3 <- ggplot(data, aes(x = age_band)) +
    geom_bar(fill = "lightgreen") +
    labs(title = "Overall Distribution of Age Bands", x = "Age Band", y = "Count") +
    theme_minimal()
  print(p3)
  
  p4 <- ggplot(data, aes(x = financial_year)) +
    geom_bar(fill = "darkseagreen") +
    labs(title = "Overall Distribution of Financial Years", x = "Financial Year", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p4)
  
  p5 <- ggplot(data, aes(x = sex, fill = extrication)) +
    geom_bar(position = "fill") +
    labs(title = "Proportion of Extrication Method by Sex", y = "Proportion", fill = "Method") +
    theme_minimal()
  print(p5)
  
  p6 <- ggplot(data, aes(x = age_band, fill = extrication)) +
    geom_bar(position = "fill") +
    labs(title = "Proportion of Extrication Method by Age Band", y = "Proportion", fill = "Method") +
    theme_minimal()
  print(p6)
  
  p7 <- ggplot(data, aes(x = financial_year, fill = extrication)) +
    geom_bar(position = "fill") +
    labs(title = "Proportion of Extrication Method by Financial Year", y = "Proportion", fill = "Method") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p7)
  
  p8 <- ggplot(data, aes(x = n_casualties)) +
    geom_histogram(binwidth = 10, fill = "darkorchid", color = "black") +
    labs(title = "Histogram of Number of Casualties (Binwidth = 5)", x = "Number of Casualties", y = "Frequency") +
    theme_minimal()
  print(p8)
  list(p1 = p1,
       p2 = p2,
       p3 = p3,
       p4 = p4,
       p5 = p5,
       p6 = p6,
       p7 = p7,
       p8 = p8
       )
}

# Function to split data into training and testing sets.
split_data <- function(extrication_clean, seed = 42, p = 0.7) {
  set.seed(seed)
  trainIndex <- createDataPartition(extrication_clean$extrication, p = p, list = FALSE)
  train_data <- extrication_clean[trainIndex, ]
  test_data <- extrication_clean[-trainIndex, ]
  
  cat("Training data rows:", nrow(train_data), "\n")
  cat("Test data rows:", nrow(test_data), "\n")
  cat("Proportion of response classes in training data:\n")
  print(prop.table(table(train_data$extrication)))
  
  return(list(train_data = train_data, test_data = test_data))
}

# Function to build the Multinomial GLM.
build_glm_model <- function(train_data) {
  cat("\n--- Building Multinomial GLM: With Sex-Age_Band Interaction ---\n")
  glm_model_interaction <- nnet::multinom(extrication ~ sex * age_band + n_casualties + financial_year,
                                          data = train_data, MaxNWts = 5000, trace = FALSE)
  return(glm_model_interaction)
}

# Function to evaluate a given model and print its summary and confusion matrix.
evaluate_model <- function(model, test_data, model_name) {
  cat(paste0("\n--- Evaluating ", model_name, " ---\n"))
  cat("\nModel Summary:\n")
  print(summary(model))
  
  if (inherits(model, "multinom")) {
    test_predictions_class <- predict(model, newdata = test_data, type = "class")
  } else if (inherits(model, "gam") && model$family$family == "multinomial") {
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
  summary(model)
}

# Function to plot the confusion matrix as a heatmap.
plot_confusion_matrix_heatmap <- function(model, test_data) {
  test_predictions_class <- predict(model, newdata = test_data, type = "class")
  levels_response <- levels(test_data$extrication)
  cm <- confusionMatrix(factor(test_predictions_class, levels = levels_response),
                        factor(test_data$extrication, levels = levels_response))
  
  cm_df <- as.data.frame(cm$table)
  colnames(cm_df) <- c("Prediction", "Reference", "Count")
  
  confusion_heatmap <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(label = Count), vjust = 1, size = 4, color = "black") +
    labs(title = "Confusion Matrix Heatmap for Multinomial GLM",
         x = "Actual Extrication Method",
         y = "Predicted Extrication Method") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    coord_fixed()
  
  confusion_heatmap
}