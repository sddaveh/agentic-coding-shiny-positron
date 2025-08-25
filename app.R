library(shiny)
library(bslib)
library(fpp3)
library(gt)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load tourism data
data("tourism")

# Prepare data
tourism_data <- tourism |> 
  group_by(Purpose) |>
  summarise(Trips = sum(Trips), .groups = "drop")

# Get unique purposes and date range
purposes <- unique(tourism_data$Purpose)
date_range <- range(tourism_data$Quarter)

ui <- page_navbar(
  title = "Australian Tourism Forecast",
  
  # Tab 1 - Visualization
  nav_panel(
    title = "Visualization",
    page_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(
          "selected_purposes",
          "Select Purposes:",
          choices = setNames(purposes, purposes),
          selected = purposes  # Default to all purposes
        ),
        dateRangeInput(
          "date_range",
          "Training/Forecast Range:",
          start = as.Date(yearquarter("2016 Q1")),
          end = as.Date(date_range[2]),
          min = as.Date(date_range[1]),
          max = as.Date(date_range[2])
        ),
        p("First date: Training cutoff (default 1 year before end)"),
        p("Second date: End of forecast horizon")
      ),
      card(
        card_header("Tourism Data Visualization"),
        plotOutput("tourism_plot", height = "700px")
      )
    )
  ),
  
  # Tab 2 - Model Building
  nav_panel(
    title = "Model Building",
    page_sidebar(
      sidebar = sidebar(
        checkboxInput(
          "show_model_spec",
          "Show Model Specifications",
          value = FALSE
        ),
        checkboxInput(
          "show_training_accuracy",
          "Show Training Accuracy",
          value = FALSE
        )
      ),
      layout_columns(
        card(
          card_header("Model Specifications"),
          conditionalPanel(
            condition = "input.show_model_spec",
            verbatimTextOutput("model_specs")
          )
        ),
        card(
          card_header("Training Accuracy"),
          conditionalPanel(
            condition = "input.show_training_accuracy",
            gt_output("training_accuracy")
          )
        ),
        card(
          card_header("Forecast Accuracy"),
          gt_output("forecast_accuracy")
        )
      )
    )
  ),
  
  # Tab 3 - Forecast
  nav_panel(
    title = "Forecast",
    page_sidebar(
      sidebar = sidebar(
        radioButtons(
          "selected_model",
          "Select Model for Forecast Table:",
          choices = c("ARIMA" = "arima", "ETS" = "ets"),
          selected = "arima"
        )
      ),
      layout_columns(
        card(
          card_header("Forecast Visualization"),
          plotOutput("forecast_plot", height = "600px")
        ),
        card(
          card_header("Forecast Table"),
          gt_output("forecast_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    req(input$selected_purposes)
    
    tourism_data |>
      filter(Purpose %in% input$selected_purposes)
  })
  
  # Training cutoff date
  train_cutoff <- reactive({
    req(input$date_range)
    yearquarter(input$date_range[1])
  })
  
  # Training data
  training_data <- reactive({
    filtered_data() |>
      filter(Quarter < train_cutoff())
  })
  
  # Fitted models
  fitted_models <- reactive({
    req(nrow(training_data()) > 0)
    
    training_data() |>
      model(
        arima = ARIMA(Trips),
        ets = ETS(Trips)
      )
  })
  
  # Forecasts
  forecasts <- reactive({
    req(fitted_models())
    
    end_date <- yearquarter(input$date_range[2])
    h_periods <- as.numeric(end_date - train_cutoff()) + 1
    
    fitted_models() |>
      forecast(h = h_periods)
  })
  
  # Tab 1 - Visualization plot
  output$tourism_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    
    filtered_data() |>
      autoplot(Trips) +
      geom_vline(
        xintercept = as.numeric(train_cutoff()), 
        color = "red", 
        linetype = "dashed",
        size = 0.8
      ) +
      labs(
        y = "Trips", 
        title = "Tourism Data with Training Cutoff",
        caption = "Red dashed line indicates training cutoff"
      ) +
      facet_wrap(~Purpose, ncol = 1, scales = "free_y") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        strip.text = element_text(face = "bold", size = 12),
        panel.grid.minor = element_blank()
      )
  })
  
  # Tab 2 - Model specifications
  output$model_specs <- renderPrint({
    req(fitted_models())
    fitted_models()
  })
  
  # Tab 2 - Training accuracy
  output$training_accuracy <- render_gt({
    req(fitted_models())
    
    fitted_models() |>
      accuracy() |>
      select(Purpose, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE) |>
      gt() |>
      fmt_number(decimals = 2) |>
      tab_header(title = "Training Accuracy Metrics")
  })
  
  # Tab 2 - Forecast accuracy
  output$forecast_accuracy <- render_gt({
    req(forecasts())
    
    forecasts() |>
      accuracy(filtered_data()) |>
      select(Purpose, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE) |>
      gt() |>
      fmt_number(decimals = 2) |>
      tab_header(title = "Forecast Accuracy Metrics")
  })
  
  # Tab 3 - Forecast plot
  output$forecast_plot <- renderPlot({
    req(forecasts())
    
    # Show last 2 years of training data plus forecasts
    display_start <- yearquarter(as.Date(train_cutoff()) - years(2))
    
    forecasts() |>
      autoplot(
        filtered_data() |> filter(Quarter >= display_start)
      ) +
      labs(
        y = "Trips", 
        title = "Tourism Forecasts",
        caption = "Showing last 2 years of training data plus forecasts"
      ) +
      facet_wrap(Purpose ~ .model, ncol = 2) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        strip.text = element_text(face = "bold", size = 10),
        panel.grid.minor = element_blank()
      )
  })
  
  # Tab 3 - Forecast table
  output$forecast_table <- render_gt({
    req(forecasts(), input$selected_model)
    
    forecasts() |>
      filter(.model == input$selected_model) |>
      as_tibble() |>
      select(Purpose, .model, Quarter, .mean) |>
      pivot_wider(names_from = Quarter, values_from = .mean) |>
      gt() |>
      fmt_number(decimals = 0) |>
      tab_header(
        title = paste("Forecast Table -", toupper(input$selected_model))
      )
  })
}

shinyApp(ui = ui, server = server)