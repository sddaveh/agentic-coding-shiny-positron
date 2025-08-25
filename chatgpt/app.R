# app.R
# Australian Tourism Forecast — Shiny app
# Requires: shiny, fpp3, gt, bslib
# Run with: shiny::runApp()

library(shiny)
library(bslib)
library(fpp3)
library(gt)

ui <- navbarPage(
  title = "Australian Tourism Forecast",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # ---- TAB 1: Explore ----
  tabPanel(
    "Explore",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("state", "State", choices = c("All", unique(tourism$State)), selected = "All"),
        selectInput("purpose", "Purpose", choices = c("All", unique(tourism$Purpose)), selected = "All"),
        dateRangeInput("date_range", "Quarter range",
                       start = min(as.Date(tourism$Quarter)),
                       end   = max(as.Date(tourism$Quarter))),
        checkboxInput("facet_regions", "Facet by Region", value = FALSE)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Time plot", plotOutput("p_explore", height = 420)),
          tabPanel("Summary", tableOutput("t_summary"))
        )
      )
    )
  ),
  
  # ---- TAB 2: Train & Diagnose ----
  tabPanel(
    "Train & Diagnose",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons("model_type", "Model",
                     choices = c("ETS (auto)" = "ets", "ARIMA (auto)" = "arima", "STL + ETS" = "stl_ets"),
                     selected = "arima"),
        dateInput("train_cut", "Train until (last included quarter)",
                  value = as.Date(yearquarter("2014 Q4"))),
        numericInput("h", "Horizon (quarters)", value = 8, min = 1, max = 24, step = 1),
        checkboxInput("by_purpose", "Fit separate models per Purpose", value = TRUE)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Residuals", plotOutput("p_resid", height = 420)),
          tabPanel("ACF", plotOutput("p_acf", height = 420)),
          tabPanel("Forecast vs Actual", plotOutput("p_fc_diag", height = 420))
        )
      )
    )
  ),
  
  # ---- TAB 3: Forecast ----
  tabPanel(
    "Forecast",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons("fc_model", "Forecast Model",
                     choices = c("ETS (auto)" = "ets", "ARIMA (auto)" = "arima", "STL + ETS" = "stl_ets"),
                     selected = "arima"),
        numericInput("fc_h", "Horizon (quarters)", value = 8, min = 1, max = 24, step = 1),
        checkboxInput("show_ci", "Show 80/95% intervals", value = TRUE)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput("p_fc", height = 420)),
          tabPanel("Table", gt_output("gt_fc"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # ---- Helpers ----
  data_filtered <- reactive({
    d <- tourism
    if (input$state != "All") d <- d |> filter(State == input$state)
    if (input$purpose != "All") d <- d |> filter(Purpose == input$purpose)
    d |> filter(Quarter >= yearquarter(input$date_range[1]),
                Quarter <= yearquarter(input$date_range[2]))
  })
  
  key_spec <- reactive({
    if (isTRUE(input$by_purpose)) c(State, Region, Purpose) else c(State, Region)
  })
  
  train_ts <- reactive({
    data_filtered() |> filter(Quarter <= yearquarter(input$train_cut))
  })
  test_ts <- reactive({
    data_filtered() |> filter(Quarter > yearquarter(input$train_cut))
  })
  
  fit_model <- function(dat, model_type, key_cols) {
    dat_keyed <- dat |> as_tsibble(index = Quarter, key = key_cols)
    if (model_type == "ets") {
      mdl <- dat_keyed |> model(ets = ETS(Trips))
    } else if (model_type == "arima") {
      mdl <- dat_keyed |> model(arima = ARIMA(Trips))
    } else {
      mdl <- dat_keyed |>
        model(stl_ets = decomposition_model(
          STL(Trips ~ season(window = "periodic")),
          ETS(season_adjust))
        )
    }
    mdl
  }
  
  # ---- Explore outputs ----
  output$p_explore <- renderPlot({
    d <- data_filtered()
    p <- d |> autoplot(Trips) +
      labs(y = "Trips", x = NULL, title = "Australian Tourism — Trips") +
      theme_minimal()
    if (isTRUE(input$facet_regions)) {
      p <- p + facet_wrap(vars(Region), scales = "free_y")
    }
    p
  })
  
  output$t_summary <- renderTable({
    data_filtered() |> as_tibble() |>
      group_by(State, Region, Purpose) |>
      summarise(start = min(Quarter), end = max(Quarter),
                mean_trips = mean(Trips), sd_trips = sd(Trips), .groups = "drop") |>
      arrange(State, Region, Purpose)
  })
  
  # ---- Train & Diagnose outputs ----
  fit_train <- reactive({
    fit_model(train_ts(), input$model_type, key_spec())
  })
  
  fc_diag <- reactive({
    fit_train() |> forecast(h = input$h)
  })
  
  output$p_resid <- renderPlot({
    fit_train() |> gg_tsresiduals()
  })
  
  output$p_acf <- renderPlot({
    fit_train() |> augment() |> ACF(.resid) |> autoplot()
  })
  
  output$p_fc_diag <- renderPlot({
    trn <- train_ts()
    tst <- test_ts()
    f <- fc_diag()
    trn_show <- trn |> filter(Quarter >= max(trn$Quarter, na.rm = TRUE) - 4)
    autoplot(trn_show, Trips) +
      autolayer(f, level = if (input$show_ci) c(80, 95) else NULL) +
      autolayer(tst, Trips, colour = "black") +
      labs(y = "Trips", title = "Forecast vs Actuals (diagnostics)") +
      theme_minimal()
  })
  
  # ---- Forecast outputs ----
  fit_all <- reactive({
    fit_model(data_filtered(), input$fc_model, key_spec())
  })
  
  fc_out <- reactive({
    fit_all() |> forecast(h = input$fc_h)
  })
  
  output$p_fc <- renderPlot({
    d <- data_filtered()
    f <- fc_out()
    d_show <- d |> filter(Quarter >= max(d$Quarter, na.rm = TRUE) - 4)
    autoplot(d_show, Trips) +
      autolayer(f, level = if (input$show_ci) c(80, 95) else NULL) +
      labs(y = "Trips", title = "Tourism data forecasts") +
      facet_wrap(vars(Purpose), ncol = 2) +
      theme_minimal()
  })
  
  output$gt_fc <- render_gt({
    f <- fc_out() |> as_tibble() |>
      select(State, Region, Purpose, .model, Quarter, .mean) |>
      mutate(Quarter = as.character(Quarter)) |>
      pivot_wider(names_from = Quarter, values_from = .mean) |>
      arrange(State, Region, Purpose)
    
    gt(f) |> fmt_number(decimals = 0)
  })
}

shinyApp(ui, server)
