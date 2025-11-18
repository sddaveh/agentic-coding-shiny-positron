library(shiny)
library(bslib)
library(fpp3)
library(gt)
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)

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
  
  # Tab 1 - Visualization.
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
      p("Second date: End of forecast horizon"),
      checkboxInput(
        "free_y_scale",
        "Free Y Scale",
        value = TRUE
      )
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
        value = TRUE  # Changed from FALSE to TRUE
      )
    ),
    # First row - Conditional Model Specifications card
    conditionalPanel(
      condition = "input.show_model_spec",
      card(
        card_header("Model Specifications"),
        verbatimTextOutput("model_specs")
      )
    ),
    
    # Second row - Training and Forecast Accuracy side by side
    layout_columns(
      # Conditional Training Accuracy card  
      conditionalPanel(
        condition = "input.show_training_accuracy",
        card(
          card_header("Training Accuracy"),
          htmlOutput("training_accuracy")
        )
      ),
      
      # Forecast Accuracy card (always shown)
      card(
        card_header("Forecast Accuracy"),
        htmlOutput("forecast_accuracy_out")
      )
    )
  )
),
  
  # Tab 3 - Forecast
  nav_panel(
    title = "Forecast",
    page_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(
          "selected_models",
          "Select Models for Forecast:",
          choices = c("ARIMA" = "arima", "ETS" = "ets"),
          selected = c("arima", "ets")  # Default to both models
        )
      ),
      layout_columns(
        card(
          card_header("Forecast Visualization"),
          plotOutput("forecast_plot", height = "600px")
        ),
        card(
          card_header("Forecast Tables"),
          htmlOutput("forecast_table_out")
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
    yearquarter(paste(year(input$date_range[1]), "Q", quarter(input$date_range[1]), sep=""))
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
    
    end_date <- yearquarter(paste(year(input$date_range[2]), "Q", quarter(input$date_range[2]), sep=""))
    h_periods <- as.numeric(end_date - train_cutoff()) + 1
    
    fitted_models() |>
      forecast(h = h_periods)
  })
  
  # Tab 1 - Visualization plot
output$tourism_plot <- renderPlot({
  req(nrow(filtered_data()) > 0, input$date_range)
  
  # Convert dates properly to yearquarter
  train_cutoff_date <- yearquarter(paste(year(input$date_range[1]), "Q", quarter(input$date_range[1]), sep=""))
  forecast_end_date <- yearquarter(paste(year(input$date_range[2]), "Q", quarter(input$date_range[2]), sep=""))
  
  # Determine scales based on user input
  scales_option <- if(input$free_y_scale) "free_y" else "fixed"
  
  filtered_data() |>
    autoplot(Trips) +
    # Add gray shading for forecast horizon
    annotate(
      "rect",
      xmin = as.Date(train_cutoff_date),
      xmax = as.Date(forecast_end_date),
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.2,
      fill = "lightgray"
    ) +
    # Add vertical red line for training cutoff
    geom_vline(
      xintercept = as.Date(train_cutoff_date), 
      color = "red", 
      linetype = "solid",
      size = 0.8
    ) +
    labs(
      y = "Trips", 
      title = "Tourism Data with Training Cutoff and Forecast Horizon",
      caption = "Red line indicates training cutoff; gray shading shows forecast horizon"
    ) +
    facet_wrap(~Purpose, ncol = 1, scales = scales_option) +
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
  output$training_accuracy <- renderUI({
    req(fitted_models())
    
    fitted_models() |>
      accuracy() |>
      select(Purpose, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE) |>
      gt() |>
      fmt_number(decimals = 2) |>
      tab_header(title = "Training Accuracy Metrics") |>
      as_raw_html()
  })
  
  # Tab 2 - Forecast accuracy
  output$forecast_accuracy_out <- renderUI({
    req(forecasts())
    
    forecasts() |>
      accuracy(filtered_data()) |>
      select(Purpose, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE) |>
      gt() |>
      fmt_number(decimals = 2) |>
      tab_header(title = "Forecast Accuracy Metrics") |>
      as_raw_html()
  })
  
  # Tab 3 - Forecast plot (filtered by selected models)
  output$forecast_plot <- renderPlot({
    req(forecasts(), input$selected_models)
    
    # Show last 2 years of training data plus forecasts
    display_start <- yearquarter(as.Date(train_cutoff()) - years(2))
    
    # Filter forecasts by selected models
    filtered_forecasts <- forecasts() |>
      filter(.model %in% input$selected_models)
    
    # Calculate number of columns based on number of selected models
    n_models <- length(input$selected_models)
    
    filtered_forecasts |>
      autoplot(
        filtered_data() |> filter(Quarter >= display_start)
      ) +
      labs(
        y = "Trips", 
        title = "Tourism Forecasts",
        caption = "Showing last 2 years of training data plus forecasts"
      ) +
      facet_wrap(Purpose ~ .model, ncol = n_models, scales = if(input$free_y_scale) "free_y" else "fixed") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        strip.text = element_text(face = "bold", size = 10),
        panel.grid.minor = element_blank()
      )
  })
  
  # Tab 3 - Forecast tables (separate table for each selected model)
  output$forecast_table_out <- renderUI({
    req(forecasts(), input$selected_models)
    
    # Create a list to hold all tables
    table_list <- map(input$selected_models, function(model) {
      
      table_data <- forecasts() |>
        filter(.model == model) |>
        as_tibble() |>
        select(Purpose, .model, Quarter, .mean) |>
        pivot_wider(names_from = Quarter, values_from = .mean)
      
      # Create gt table
      gt_table <- table_data |>
        gt() |>
        fmt_number(decimals = 0) |>
        tab_header(
          title = paste("Forecast Table -", toupper(model))
        ) |>
        as_raw_html()
      
      # Wrap in a div for styling
      div(
        style = "margin-bottom: 30px;",
        HTML(gt_table)
      )
    })
    
    # Return all tables as a tagList
    do.call(tagList, table_list)
  })
}

shinyApp(ui = ui, server = server)