# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               Grow a Garden - Live Stock Dashboard
#                      Full and Complete app.R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# =============================================================
# PART 1: LOAD LIBRARIES
# =============================================================
library(shiny)
library(shinydashboard)
library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(shinyjs)
library(purrr)
library(DT)


# =============================================================
# PART 2: GLOBAL OBJECTS & THE SCRAPER FUNCTION
# =============================================================

# --- History file path ---
HISTORY_FILE <- "grow_a_garden_stock_history.csv"

# --- The Web Scraper Function ---
scrape_garden_stocks <- function() {
  
  # The URL of the target page
  url <- "https://growagarden.gg/stocks"
  
  tryCatch({
    # Read the HTML from the page
    page <- read_html(url)
    
    # --- 1. Scrape the Countdown Timer ---
    countdown_text <- page %>%
      html_element("h1 + p") %>%
      html_text() %>%
      str_extract("\\d{2}:\\d{2}:\\d{2}")
    
    # --- 2. Scrape the Stock Items ---
    all_stocks <- list()
    sections <- page %>% html_elements("section.py-16")
    
    for (section in sections) {
      category_title <- section %>%
        html_element("h2") %>%
        html_text() %>%
        str_trim()
      
      items <- section %>% html_elements("div.item")
      
      if (length(items) > 0) {
        item_data <- tibble(
          name = items %>% html_element("p.font-bold") %>% html_text(),
          image_url = items %>% html_element("img") %>% html_attr("src")
        )
        all_stocks[[category_title]] <- item_data
      }
    }
    
    # --- 3. Return a structured list ---
    return(list(
      stocks = all_stocks,
      countdown_str = countdown_text,
      timestamp = Sys.time(),
      error = NULL
    ))
    
  }, error = function(e) {
    # If the website is down or changes, return an error message
    return(list(stocks = list(), countdown_str = NA, timestamp = Sys.time(), error = as.character(e)))
  })
}


# =============================================================
# PART 3: UI (USER INTERFACE)
# =============================================================
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Grow a Garden Stocks"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Live Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Stock History", tabName = "history", icon = icon("history"))
    )
  ),
  
  dashboardBody(
    # --- Add shinyjs and custom CSS ---
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* Custom styling for the item boxes */
        .stock-item {
          text-align: center;
          padding: 10px;
          border: 1px solid #ddd;
          border-radius: 5px;
          margin: 5px;
          background-color: #f9f9f9;
          width: 120px; /* Fixed width for alignment */
          height: 140px; /* Fixed height for alignment */
          display: flex;
          flex-direction: column;
          justify-content: space-between;
        }
        .stock-item img {
          max-width: 80px;
          max-height: 80px;
          margin: 0 auto; /* Center image */
        }
        .stock-item p {
          margin-top: 5px;
          font-weight: bold;
          flex-grow: 1;
        }
      "))
    ),
    
    tabItems(
      # --- First tab content: Our original dashboard ---
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Live Status", status = "success", solidHeader = TRUE, width = 12,
                  column(6,
                         h3("Next Stock Update In:"),
                         h3(uiOutput("countdown_display"))
                  ),
                  column(6, align = "right", br(),
                         actionButton("refresh_button", "Refresh Now", icon = icon("refresh"), class = "btn-lg btn-success"),
                         p(textOutput("last_updated_text"))
                  )
                )
              ),
              fluidRow(
                box(title = "Seeds", status = "primary", solidHeader = TRUE, width = 6, uiOutput("seeds_ui")),
                box(title = "Gears", status = "primary", solidHeader = TRUE, width = 6, uiOutput("gears_ui"))
              ),
              fluidRow(
                box(title = "Eggs", status = "warning", solidHeader = TRUE, width = 6, uiOutput("eggs_ui")),
                box(title = "Cosmetics", status = "warning", solidHeader = TRUE, width = 6, uiOutput("cosmetics_ui"))
              ),
              fluidRow(
                box(title = "Weather", status = "info", solidHeader = TRUE, width = 6, uiOutput("weather_ui")),
                box(title = "Honey", status = "info", solidHeader = TRUE, width = 6, uiOutput("honey_ui"))
              )
      ),
      
      # --- Second tab content: The history table ---
      tabItem(tabName = "history",
              h2("Complete Stock History Log"),
              p("This log starts from the first time the app was run and updates on every refresh."),
              fluidRow(
                box(
                  title = "History Data", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("history_table")
                )
              )
      )
    )
  )
)


# =============================================================
# PART 4: SERVER (APPLICATION LOGIC)
# =============================================================
server <- function(input, output, session) {
  
  # --- 1. Reactive Values ---
  scraped_data <- reactiveVal()
  countdown_seconds <- reactiveVal(0)
  
  # --- 2. Data Fetch & Save Function ---
  fetch_data <- function() {
    showNotification("Fetching latest stock data...", type = "message", duration = 3)
    data <- scrape_garden_stocks()
    
    # Handle potential scraping errors
    if (!is.null(data$error)) {
      showNotification(paste("Scraping Error:", data$error), type = "error", duration = 10)
      return()
    }
    
    scraped_data(data)
    
    if (!is.na(data$countdown_str)) {
      time_parts <- as.numeric(str_split(data$countdown_str, ":", simplify = TRUE))
      total_seconds <- time_parts[1]*3600 + time_parts[2]*60 + time_parts[3]
      countdown_seconds(total_seconds)
    }
    
    # Save the scraped data to our history file
    if (length(data$stocks) > 0) {
      history_df <- map_dfr(data$stocks, ~as.data.frame(.x), .id = "category") %>%
        mutate(timestamp = data$timestamp) %>%
        select(timestamp, category, item_name = name, image_url)
      
      write.table(history_df, 
                  HISTORY_FILE, 
                  sep = ",",
                  row.names = FALSE, 
                  append = file.exists(HISTORY_FILE), 
                  col.names = !file.exists(HISTORY_FILE))
      
      showNotification("History log updated.", type = "message", duration = 2)
    }
  }
  
  # Fetch data for the very first time when the app starts
  fetch_data()
  
  # --- 3. Countdown Timer Logic ---
  observe({
    invalidateLater(1000, session)
    isolate({
      current_seconds <- countdown_seconds()
      if (current_seconds > 0) {
        countdown_seconds(current_seconds - 1)
      } else if (current_seconds == 0) {
        showNotification("Countdown finished! Auto-refreshing...", type = "warning", duration = 5)
        countdown_seconds(-1)
        shinyjs::click("refresh_button")
      }
    })
  })
  
  # --- 4. Manual Refresh Logic ---
  observeEvent(input$refresh_button, {
    fetch_data()
  })
  
  # --- 5. Rendering Outputs for the UI ---
  output$countdown_display <- renderUI({
    seconds <- countdown_seconds()
    req(seconds >= 0)
    
    formatted_time <- sprintf("%02d:%02d:%02d", 
                              floor(seconds / 3600), 
                              floor(seconds %% 3600 / 60), 
                              seconds %% 60)
    tags$span(class = "h2 font-weight-bold text-success", formatted_time)
  })
  
  output$last_updated_text <- renderText({
    req(scraped_data())
    paste("Last updated:", format(scraped_data()$timestamp, "%I:%M:%S %p"))
  })
  
  create_item_grid <- function(category_name) {
    renderUI({
      req(scraped_data())
      df <- scraped_data()$stocks[[category_name]]
      
      if (is.null(df) || nrow(df) == 0) {
        return(p("No items in this category right now."))
      }
      
      item_html <- lapply(1:nrow(df), function(i) {
        tags$div(class = "stock-item",
                 tags$img(src = df$image_url[i], alt = df$name[i]),
                 tags$p(df$name[i])
        )
      })
      
      tags$div(style = "display: flex; flex-wrap: wrap; justify-content: center;", item_html)
    })
  }
  
  output$seeds_ui      <- create_item_grid("Seeds")
  output$gears_ui      <- create_item_grid("Gears")
  output$eggs_ui       <- create_item_grid("Eggs")
  output$cosmetics_ui  <- create_item_grid("Cosmetics")
  output$weather_ui    <- create_item_grid("Weather")
  output$honey_ui      <- create_item_grid("Honey")
  
  output$history_table <- renderDT({
    req(input$refresh_button)
    isolate({
      if (file.exists(HISTORY_FILE)) {
        read.csv(HISTORY_FILE) %>%
          mutate(timestamp = ymd_hms(timestamp, tz = Sys.timezone()) %>% 
                   format("%Y-%m-%d %I:%M:%S %p")) %>%
          arrange(desc(timestamp))
      } else {
        data.frame(timestamp = character(), category = character(), item_name = character()) 
      }
    })
  }, options = list(pageLength = 25, order = list(list(0, 'desc'))), rownames = FALSE)
}


# =============================================================
# PART 5: RUN THE APP
# =============================================================
shinyApp(ui = ui, server = server)