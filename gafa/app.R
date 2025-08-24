library(shiny)
library(bslib)
library(fpp3)
library(dplyr)
library(ggplot2)

# Load the GAFA stock data
data("gafa_stock")

# Get unique stock symbols and date range
stock_symbols <- unique(gafa_stock$Symbol)
date_range <- range(gafa_stock$Date)

ui <- page_sidebar(
  title = "GAFA Stock Analysis",
  sidebar = sidebar(
    checkboxGroupInput(
      "selected_stocks",
      "Select Stocks:",
      choices = setNames(stock_symbols, stock_symbols),
      selected = stock_symbols[1:2]  # Default to first two stocks
    ),
    dateRangeInput(
      "date_range",
      "Select Date Range:",
      start = date_range[1],
      end = date_range[2],
      min = date_range[1],
      max = date_range[2]
    ),
    selectInput(
      "price_type",
      "Price Type:",
      choices = c("Close" = "Close", 
                 "Open" = "Open", 
                 "High" = "High", 
                 "Low" = "Low"),
      selected = "Close"
    )
  ),
  card(
    card_header("Stock Price Time Series"),
    plotOutput("stock_plot", height = "600px")
  )
)

server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    req(input$selected_stocks, input$date_range)
    
    gafa_stock %>%
      filter(
        Symbol %in% input$selected_stocks,
        Date >= input$date_range[1],
        Date <= input$date_range[2]
      )
  })
  
  # Create the faceted plot
  output$stock_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    
    # Get the selected price column
    price_col <- input$price_type
    
    filtered_data() %>%
      ggplot(aes(x = Date, y = .data[[price_col]])) +
      geom_line(color = "#2E86AB", size = 0.8) +
      facet_wrap(~Symbol, scales = "free_y", ncol = 2) +
      labs(
        title = paste("GAFA Stock Prices -", price_col),
        x = "Date",
        y = paste(price_col, "Price ($)"),
        caption = "Data source: fpp3 package"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        strip.text = element_text(face = "bold", size = 12),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
}

shinyApp(ui = ui, server = server)
