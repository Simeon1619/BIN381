# -----------------------------------------------------------------------------
# Public Health Indicator Prediction Tool - Shiny Application
# -----------------------------------------------------------------------------

# --- 1. Load Required Libraries ---
library(shiny)
library(shinydashboard)
library(randomForest)
library(ggplot2)
library(dplyr)


# --- 2. Load Pre-trained Models ---
# IMPORTANT: Place your saved model files in the same directory as this app.R file.
# These models are the outputs of your modeling pipeline.

# In a real scenario, you would load your models like this:
# rf_model <- readRDS("rf_final_model.rds")
# mlr_model <- readRDS("mlr_final_model.rds")

# --- FOR DEMONSTRATION PURPOSES: Create dummy models if real ones are not found ---
# This section creates functional placeholder models so the app can run.
# In your actual deployment, you should REMOVE this section and use the readRDS() lines above.
if (!file.exists("rf_final_model.rds")) {
  # Create sample data mirroring the project's structure
  set.seed(123)
  sample_data <- data.frame(
    Value = runif(100, 30, 90),
    value_z = scale(runif(100, 30, 90)),
    value_lag1 = runif(100, 30, 90),
    facility_public = rbinom(100, 1, 0.6),
    facility_private = rbinom(100, 1, 0.2),
    provider_nurse = rbinom(100, 1, 0.5),
    provider_midwife = rbinom(100, 1, 0.4)
  )
  sample_data$facility_home <- 1 - (sample_data$facility_public + sample_data$facility_private)
  sample_data$facility_home[sample_data$facility_home < 0] <- 0
  
  # Train dummy models
  mlr_model <- lm(Value ~ value_z + value_lag1 + facility_public + facility_private + provider_nurse + provider_midwife, data = sample_data)
  rf_model <- randomForest(Value ~ value_z + value_lag1 + facility_public + facility_private + provider_nurse + provider_midwife, data = sample_data, ntree = 100, importance = TRUE)
  
  # Create a dummy variable importance data frame for the plot
  importance_df <- as.data.frame(importance(rf_model)) %>%
    mutate(variable = rownames(.)) %>%
    arrange(desc(`%IncMSE`))
} else {
  # Load the real models if they exist
  rf_model <- readRDS("rf_final_model.rds")
  mlr_model <- readRDS("mlr_final_model.rds")
  # Extract variable importance for plotting
  importance_df <- as.data.frame(importance(rf_model)) %>%
    mutate(variable = rownames(.)) %>%
    arrange(desc(`%IncMSE`))
}
# --- End of Demonstration Section ---


# -----------------------------------------------------------------------------
# --- 3. User Interface (UI) Definition ---
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Health Indicator Prediction"),
  
  # --- Sidebar with User Inputs ---
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prediction Tool", tabName = "dashboard", icon = icon("dashboard")),
      
      h4("Set Predictor Values", style = "padding-left: 15px;"),
      
      # Input for the previous year's value (Lag 1)
      sliderInput("value_lag1", "Previous Year's Indicator Value:",
                  min = 0, max = 100, value = 50, step = 1),
      
      # Input for place of delivery
      selectInput("place_of_delivery", "Primary Place of Delivery:",
                  choices = c("Public Facility" = "Public",
                              "Private Facility" = "Private",
                              "Home" = "Home"),
                  selected = "Public"),
      
      # Input for antenatal care provider
      selectInput("antenatal_care_provider", "Primary Antenatal Care Provider:",
                  choices = c("Nurse", "Midwife", "Doctor", "Other"),
                  selected = "Nurse")
    )
  ),
  
  # --- Main Panel with Outputs ---
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                # --- Prediction Output Box ---
                valueBoxOutput("predictionBox", width = 12)
              ),
              fluidRow(
                # --- Tabs for Interpretation and Details ---
                tabBox(
                  title = "Model Insights",
                  id = "tabset1",
                  width = 12,
                  
                  # --- MLR Interpretation Tab ---
                  tabPanel("Policy Interpretation (from MLR)",
                           h4("How do these factors influence the outcome?"),
                           p("The Multiple Linear Regression (MLR) model provides interpretable coefficients to understand the linear relationships between service delivery factors and the health outcome. While less accurate than the Random Forest model, it helps explain the 'why' behind the prediction."),
                           verbatimTextOutput("mlr_interpretation")
                  ),
                  
                  # --- RF Variable Importance Tab ---
                  tabPanel("Predictor Importance (from RF)",
                           h4("What are the most influential predictors?"),
                           p("The Random Forest (RF) model ranks predictors by their contribution to predictive accuracy (%IncMSE - Percent Increase in Mean Squared Error when the variable is randomly shuffled). Higher values indicate greater importance."),
                           plotOutput("importance_plot")
                  )
                )
              )
      )
    )
  )
)


# -----------------------------------------------------------------------------
# --- 4. Server-side Logic ---
# -----------------------------------------------------------------------------
server <- function(input, output) {
  
  # --- Reactive Data Frame ---
  # Creates a data frame from user inputs that can be fed to the models
  input_data <- reactive({
    
    # Create the base features
    data <- data.frame(
      value_lag1 = as.numeric(input$value_lag1)
    )
    
    # One-hot encode the 'place_of_delivery' input
    data$facility_public <- ifelse(input$place_of_delivery == "Public", 1, 0)
    data$facility_private <- ifelse(input$place_of_delivery == "Private", 1, 0)
    data$facility_home <- ifelse(input$place_of_delivery == "Home", 1, 0)
    
    # One-hot encode the 'antenatal_care_provider' input
    data$provider_nurse <- ifelse(input$antenatal_care_provider == "Nurse", 1, 0)
    data$provider_midwife <- ifelse(input$antenatal_care_provider == "Midwife", 1, 0)
    data$provider_doctor <- ifelse(input$antenatal_care_provider == "Doctor", 1, 0)
    
    # Calculate the standardized value (value_z) based on training data mean/sd
    # This is a simplification; in a real app, these values should be saved from the training set
    mean_val <- 65 
    sd_val <- 15
    data$value_z <- (data$value_lag1 - mean_val) / sd_val
    
    return(data)
  })
  
  # --- Generate Predictions ---
  rf_prediction <- reactive({
    predict(rf_model, newdata = input_data())
  })
  
  # --- Render the Prediction Value Box ---
  output$predictionBox <- renderValueBox({
    valueBox(
      value = paste0(round(rf_prediction(), 2), "%"),
      subtitle = "Predicted Health Indicator Value (Using Random Forest)",
      icon = icon("bullseye"),
      color = "aqua"
    )
  })
  
  # --- Render the MLR Interpretation ---
  output$mlr_interpretation <- renderPrint({
    cat("MLR Model Equation:\n")
    print(summary(mlr_model))
    
    cat("\n--- Interpretation based on your selections ---\n")
    prediction <- predict(mlr_model, newdata = input_data(), interval = "confidence")
    cat(sprintf("For the selected inputs, the MLR model predicts a value of %.2f.\n", prediction[1,1]))
    cat(sprintf("The 95%% confidence interval for this prediction is [%.2f, %.2f].\n", prediction[1,2], prediction[1,3]))
  })
  
  # --- Render the Variable Importance Plot ---
  output$importance_plot <- renderPlot({
    ggplot(importance_df, aes(x = reorder(variable, `%IncMSE`), y = `%IncMSE`)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Random Forest - Predictor Importance",
        x = "Predictor Variable",
        y = "Importance (% Increase in MSE)"
      ) +
      theme_minimal(base_size = 14)
  })
}

# --- 5. Run the Shiny Application ---
shinyApp(ui, server)