# --- START FILE: history_module.R ---

# =============================================================
# UI function for the History Module
# =============================================================
history_ui <- function(id) {
  # ns is a helper function that namespaces the IDs
  ns <- NS(id)
  
  tabItem(tabName="history",
          h2("Historical Stock Viewer"),
          p("Select a timestamp to see a snapshot of the stock at that specific moment."),
          
          fluidRow(
            box(
              title = "History Controls", status = "primary", solidHeader = TRUE, width = 12,
              # This UI will be generated on the server
              uiOutput(ns("history_timestamp_selector_ui"))
            )
          ),
          
          # Recreate the dashboard layout for historical data
          fluidRow(
            box(title="Seed Stock (Historical)", status="info", solidHeader=TRUE, width=6, uiOutput(ns("history_seed_stock_ui"))),
            box(title="Gear Stock (Historical)", status="info", solidHeader=TRUE, width=6, uiOutput(ns("history_gear_stock_ui")))
          ),
          fluidRow(
            box(title="Egg Stock (Historical)", status="warning", solidHeader=TRUE, width=6, uiOutput(ns("history_egg_stock_ui"))),
            box(title="Cosmetics Stock (Historical)", status="warning", solidHeader=TRUE, width=6, uiOutput(ns("history_cosmetics_stock_ui")))
          ),
          fluidRow(
            box(title="Honey Stock (Historical)", status="success", solidHeader=TRUE, width=6, uiOutput(ns("history_honey_stock_ui"))),
            box(title="Night Stock (Historical)", status="primary", solidHeader=TRUE, width=6, uiOutput(ns("history_night_stock_ui")))
          ),
          
          # The original raw data table
          fluidRow(
            box(title="Complete Raw History Log", status="primary", solidHeader=TRUE, width=12, collapsible = TRUE, collapsed = TRUE,
                p("This table contains the raw, line-by-line history data."),
                DTOutput(ns("history_table"))
            )
          )
  )
}


# =============================================================
# Server function for the History Module
# =============================================================
# It takes an 'id' and the main 'history_data' reactive from the main app
history_server <- function(id, history_data) {
  moduleServer(id, function(input, output, session) {
    
    # 1. RENDER THE TIMESTAMP SELECTOR UI
    output$history_timestamp_selector_ui <- renderUI({
      # We depend on history_data() so this updates whenever the data does
      timestamps <- unique(history_data()$timestamp)
      
      # Create the selectInput dropdown
      selectInput(session$ns("selected_timestamp"), 
                  "Select a Historical Snapshot:",
                  choices = timestamps,
                  width = "100%")
    })
    
    # 2. CREATE A REACTIVE DATA FRAME FOR THE SELECTED HISTORY
    selected_history_data <- reactive({
      req(input$selected_timestamp) # Ensure a selection is made
      history_data() %>%
        filter(timestamp == input$selected_timestamp)
    })
    
    # 3. HELPER FUNCTION TO CREATE HISTORICAL GRIDS
    create_historical_item_grid <- function(category_name) {
      renderUI({
        # Depend on the filtered historical data
        historical_data <- selected_history_data()
        req(historical_data)
        
        # Filter for the specific category we want to display
        df <- historical_data %>% filter(category == category_name)
        
        if (is.null(df) || nrow(df) == 0) {
          return(p(style="padding: 20px; text-align: center;", "No items were in stock for this category at the selected time."))
        }
        
        # Use lapply to build the HTML for each item
        item_html <- lapply(1:nrow(df), function(i) {
          tags$div(class="stock-item", 
                   tags$img(src=df$image_url[i], alt=df$item_name[i]), 
                   tags$p(df$item_name[i]),
                   tags$p(class="quantity", paste("x", df$quantity[i]))
          )
        })
        
        tags$div(style="display:flex; flex-wrap:wrap; justify-content:center;", item_html)
      })
    }
    
    # 4. CONNECT THE HELPER TO THE UI OUTPUTS
    output$history_seed_stock_ui      <- create_historical_item_grid("Seed Stock")
    output$history_gear_stock_ui      <- create_historical_item_grid("Gear Stock")
    output$history_egg_stock_ui       <- create_historical_item_grid("Egg Stock")
    output$history_cosmetics_stock_ui <- create_historical_item_grid("Cosmetics Stock")
    output$history_honey_stock_ui     <- create_historical_item_grid("Honey Stock")
    output$history_night_stock_ui     <- create_historical_item_grid("Night Stock")
    
    # 5. RENDER THE RAW DATA TABLE
    output$history_table <- renderDT({
      history_data()
    }, options=list(pageLength=25, order=list(list(0, 'desc'))), rownames=FALSE)
    
  })
}

# --- END FILE: history_module.R ---