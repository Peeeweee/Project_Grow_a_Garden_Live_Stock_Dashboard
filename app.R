# --- START FILE: app.R ---

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    Grow a Garden Live Stock Monitoring Dashboard (FINAL)
#         ### FINAL VERSION: All features and modules integrated and fixed.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# =============================================================
# PART 1: LOAD LIBRARIES & MODULES
# =============================================================
library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
library(shinyjs)
library(DT)
library(purrr)
library(rvest)

# SOURCE THE MODULES
source("history_module.R")
source("calculator_module.R")


# =============================================================
# PART 2: GLOBAL OBJECTS & DATA FUNCTIONS
# =============================================================

HISTORY_FILE <- "vulcan_stock_history.csv"
FRUIT_VALUES_FILE <- "fruit_values.csv"


# Fetches item details for the pop-up modals.
fetch_item_info <- function() {
  message("--- Fetching Item Info Encyclopedia (once) ---")
  item_info_url <- "https://growagardenapi.vercel.app/api/Item-Info"
  tryCatch({
    response <- GET(item_info_url)
    stop_for_status(response, "fetch item info")
    item_data <- content(response, "parsed")
    encyclopedia <- setNames(item_data, sapply(item_data, `[[`, "name"))
    message("   Item encyclopedia fetched successfully with ", length(encyclopedia), " items.")
    return(encyclopedia)
  }, error = function(e) {
    message("   ERROR fetching item encyclopedia: ", e$message)
    return(list())
  })
}

item_encyclopedia <- fetch_item_info()


# Scraper for gamerant.com
fetch_fruit_values <- function() {
  message("--- Fetching DETAILED Fruit Values from gamerant.com ---")
  values_url <- "https://gamerant.com/roblox-grow-a-garden-all-crops-how-to-get-them/"
  tryCatch({
    page_html <- read_html(values_url)
    fruit_table <- page_html %>% html_node("table")
    df <- fruit_table %>% html_table()
    df <- df %>%
      rename(
        name = `All Crops`,
        rarity = Rarity,
        type = Type,
        sell_value = `Sell Price (Average per Unit)`,
        how_to_get = `How to Get Them`
      ) %>%
      mutate(
        sell_value = str_replace_all(sell_value, "Shuckles", ""),
        sell_value = str_replace_all(sell_value, ",", ""),
        sell_value = str_trim(sell_value)
      )
    message("   Detailed fruit values scraped successfully for ", nrow(df), " fruits.")
    return(df)
  }, error = function(e) {
    message("   ERROR fetching fruit values from gamerant: ", e$message)
    return(tibble())
  })
}


# Primary data function for the live dashboard, using the official APIs.
fetch_public_api_data <- function() {
  message("--- Starting fetch_public_api_data() ---")
  stock_url <- "https://growagardenapi.vercel.app/api/stock/GetStock"
  timer_url <- "https://growagardenapi.vercel.app/api/stock/Restock-Time"
  weather_url <- "https://growagardenapi.vercel.app/api/GetWeather"
  tryCatch({
    message("1. Fetching stock data from public API...")
    stock_response <- GET(stock_url); stop_for_status(stock_response, "fetch stock data")
    stock_data <- content(stock_response, "parsed")
    message("   Stock data fetched successfully.")
    process_stock_list <- function(stock_list) {
      if (is.null(stock_list) || length(stock_list) == 0) return(tibble(name=character(), image_url=character(), quantity=integer()))
      map_dfr(stock_list, ~tibble(name = .x$name %||% "Unknown", image_url = .x$image %||% "", quantity = .x$value %||% NA_integer_))
    }
    all_stocks <- list(
      "Seed Stock" = process_stock_list(stock_data$seedsStock), "Gear Stock" = process_stock_list(stock_data$gearStock),
      "Egg Stock" = process_stock_list(stock_data$eggStock), "Honey Stock" = process_stock_list(stock_data$honeyStock),
      "Cosmetics Stock" = process_stock_list(stock_data$cosmeticsStock), "Night Stock" = process_stock_list(stock_data$nightStock) 
    )
    message("2. Fetching timer data from public API...")
    timer_response <- GET(timer_url); stop_for_status(timer_response, "fetch timer data")
    timer_data <- content(timer_response, "parsed")
    message("   Timer data fetched successfully.")
    parse_countdown_to_seconds <- function(s) {
      if (is.null(s) || !is.character(s) || nchar(s)==0) return(NA_integer_)
      h <- as.numeric(str_extract(s, "\\d+(?=h)")); h <- ifelse(is.na(h), 0, h)
      m <- as.numeric(str_extract(s, "\\d+(?=m)")); m <- ifelse(is.na(m), 0, m)
      s <- as.numeric(str_extract(s, "\\d+(?=s)")); s <- ifelse(is.na(s), 0, s)
      h*3600 + m*60 + s
    }
    egg_s <- parse_countdown_to_seconds(timer_data$egg$countdown); honey_s <- parse_countdown_to_seconds(timer_data$honey$countdown)
    if (is.na(honey_s)) honey_s <- egg_s; if (is.na(egg_s)) egg_s <- honey_s
    timers_seconds <- list(
      "Seed Stock" = parse_countdown_to_seconds(timer_data$seeds$countdown), "Gear Stock" = parse_countdown_to_seconds(timer_data$gear$countdown),
      "Egg Stock" = egg_s, "Honey Stock" = honey_s, "Cosmetics Stock" = parse_countdown_to_seconds(timer_data$cosmetic$countdown),
      "Night Stock" = parse_countdown_to_seconds(timer_data$Event$countdown)
    )
    message("3. Fetching weather data from public API...")
    weather_response <- GET(weather_url); stop_for_status(weather_response, "fetch weather data")
    weather_data_raw <- content(weather_response, "parsed")
    current_unix_time <- as.numeric(Sys.time())
    truly_active_weather <- Filter(function(w) (w$start_duration_unix %||% 0) <= current_unix_time && (w$end_duration_unix %||% 0) >= current_unix_time, weather_data_raw$weather)
    weather_names <- if (length(truly_active_weather) > 0) truly_active_weather[[1]]$weather_name else "Clear Skies"
    message("   Weather data fetched successfully: ", weather_names)
    message("--- API fetch finished successfully. ---")
    return(list(stocks = all_stocks, timers = timers_seconds, weather = weather_names, timestamp = Sys.time(), error = NULL))
  }, error = function(e) {
    message("--- An ERROR occurred in the API fetch ---"); message(as.character(e))
    return(list(stocks=list(), timers=list(), weather="Error", timestamp=Sys.time(), error=as.character(e)))
  })
}


# =============================================================
# PART 3: UI (USER INTERFACE)
# =============================================================
ui <- dashboardPage(
  title = "Grow a Garden Dashboard",
  skin = "purple",
  dashboardHeader(title = "Grow a Garden Dashboard"),
  dashboardSidebar(
    sidebarMenu(id="tabs", 
                menuItem("Live Dashboard", tabName="dashboard", icon=icon("dashboard")), 
                menuItem("Stock History", tabName="history", icon=icon("history")),
                menuItem("Fruit Calculator", tabName="calculator", icon=icon("calculator"))
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML("
      .stock-item-link { color: inherit; text-decoration: none; }
      .stock-item {text-align:center; padding:10px; border:1px solid #ddd; border-radius:5px; margin:5px; background-color:#f9f9f9; width:120px; height:150px; display:flex; flex-direction:column; justify-content:space-between; box-shadow: 0 2px 4px rgba(0,0,0,0.1); transition: transform 0.2s, box-shadow 0.2s;}
      .stock-item:hover { transform: translateY(-3px); box-shadow: 0 4px 8px rgba(0,0,0,0.2); cursor: pointer; }
      .stock-item img {max-width:80px; max-height:80px; margin:0 auto;}
      .stock-item p {margin-top:5px; font-weight:bold; font-size: 0.9em;}
      .stock-item .quantity {font-size: 1.1em; color: #2c3e50; font-weight: bold;}
      #loading-overlay { position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(0,0,0,0.5); z-index: 10000; display: flex; justify-content: center; align-items: center; }
      .loader { border: 8px solid #f3f3f3; border-top: 8px solid #8E44AD; border-radius: 50%; width: 60px; height: 60px; animation: spin 1s linear infinite; }
      @keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }
      @media (max-width: 767px) { .main-header .logo { font-size: 12px; overflow: hidden; text-overflow: ellipsis; } }
      
      /* *** NEW: CSS for the styled text boxes *** */
      .formula-box {
        background-color: #00c0ef; /* shinydashboard info-box blue */
        color: white;
        padding: 10px;
        border-radius: 3px;
        text-align: center;
        font-weight: bold;
      }
      .solution-box {
        background-color: #f39c12; /* shinydashboard warning-box orange */
        color: white;
        padding: 10px;
        border-radius: 3px;
        text-align: center;
        font-weight: bold;
        font-family: monospace;
        font-size: 1.2em;
        word-wrap: break-word;
      }

    "))),
    tags$div(id = "loading-overlay", style = "display: none;", div(class="loader")),
    tabItems(
      tabItem(tabName="dashboard",
              fluidRow(
                column(8,
                       fluidRow(
                         valueBoxOutput("seed_timer_box", width=4), valueBoxOutput("gear_timer_box", width=4), valueBoxOutput("egg_timer_box", width=4)
                       ),
                       fluidRow(
                         valueBoxOutput("honey_timer_box", width=4), valueBoxOutput("cosmetics_timer_box", width=4), valueBoxOutput("night_timer_box", width=4)
                       )
                ),
                column(4,
                       valueBoxOutput("weather_box", width=12),
                       box(title = "Control Panel", status = "primary", solidHeader = TRUE, width = 12, align = "center",
                           actionButton("refresh_button", "Refresh Now", icon = icon("refresh"), class = "btn-lg btn-primary"), br(),br(),
                           p(textOutput("last_updated_text"))
                       )
                )
              ),
              fluidRow(
                box(title="Seed Stock", status="info", solidHeader=TRUE, width=6, uiOutput("seed_stock_ui")),
                box(title="Gear Stock", status="info", solidHeader=TRUE, width=6, uiOutput("gear_stock_ui"))
              ),
              fluidRow(
                box(title="Egg Stock", status="warning", solidHeader=TRUE, width=6, uiOutput("egg_stock_ui")),
                box(title="Cosmetics Stock", status="warning", solidHeader=TRUE, width=6, uiOutput("cosmetics_stock_ui"))
              ),
              fluidRow(
                box(title="Honey Stock", status="success", solidHeader=TRUE, width=6, uiOutput("honey_stock_ui")),
                box(title="Night Stock", status="primary", solidHeader=TRUE, width=6, uiOutput("night_stock_ui"))
              )
      ),
      history_ui("history_module"),
      calculator_ui("calculator")
    )
  )
)

# =============================================================
# PART 4: SERVER (APPLICATION LOGIC)
# =============================================================
server <- function(input, output, session) {
  message("Shiny server function initialized.")
  
  SharedState <- reactiveValues(status = "idle")
  scraped_data <- reactiveVal(); timers_rv <- reactiveVal(list())
  
  fruit_data_rv <- reactiveVal()
  
  fetch_and_save_fruit_data <- function() {
    shinyjs::show("loading-overlay")
    showNotification("Fetching latest fruit data from gamerant.com...", type = "message", duration = 5)
    
    new_data <- fetch_fruit_values()
    
    if (nrow(new_data) > 0) {
      write.csv(new_data, FRUIT_VALUES_FILE, row.names = FALSE)
      fruit_data_rv(new_data) # Update the reactive value
      showNotification("Successfully updated and saved fruit data!", type = "message", duration = 5)
    } else {
      showNotification("Failed to fetch new fruit data. Using existing data if available.", type = "error", duration = 8)
    }
    shinyjs::hide("loading-overlay")
    return(new_data)
  }
  
  if (file.exists(FRUIT_VALUES_FILE)) {
    message("Loading fruit data from local CSV file.")
    fruit_data_rv(read.csv(FRUIT_VALUES_FILE, check.names = FALSE))
  } else {
    message("No local fruit data found. Fetching for the first time.")
    fetch_and_save_fruit_data()
  }
  
  
  fetch_data <- function() {
    if (isolate(SharedState$status) == "fetching") return()
    SharedState$status <- "fetching"; shinyjs::show("loading-overlay")
    message("fetch_data() triggered. Status: fetching")
    data <- fetch_public_api_data()
    if (!is.null(data$error)) showNotification(paste("API Error:", data$error), type="error", duration=10)
    scraped_data(data); timers_rv(data$timers)
    if (is.null(data$error) && length(data$stocks) > 0) {
      history_df <- map_dfr(data$stocks, ~as.data.frame(.x), .id="category") %>%
        filter(nrow(.) > 0) %>%
        mutate(timestamp = data$timestamp, quantity = as.character(quantity)) %>%
        select(timestamp, category, item_name = name, quantity, image_url)
      if(nrow(history_df)>0) write.table(history_df, HISTORY_FILE, sep=",", row.names=F, append=file.exists(HISTORY_FILE), col.names=!file.exists(HISTORY_FILE))
    }
    SharedState$status <- "idle"; shinyjs::hide("loading-overlay")
    message("Fetch complete. Status: idle")
  }
  fetch_data()
  
  observe({ # Timer countdown logic
    req(SharedState$status == "idle"); invalidateLater(1000, session)
    if (!is.null(isolate(scraped_data()$error))) return()
    temp_timers <- isolate(timers_rv()); if (length(temp_timers) == 0) return()
    refresh_needed <- FALSE
    for (name in names(temp_timers)) {
      if (is.numeric(temp_timers[[name]]) && !is.na(temp_timers[[name]]) && temp_timers[[name]] > 0) {
        temp_timers[[name]] <- temp_timers[[name]] - 1
        if (temp_timers[[name]] <= 0) refresh_needed <- TRUE
      }
    }
    if (refresh_needed) { message("A timer hit zero. Triggering auto-refresh."); showNotification("A timer finished! Auto-refreshing...", type="warning", duration=5); fetch_data()
    } else { timers_rv(temp_timers) }
  })
  
  observeEvent(input$refresh_button, { message("Manual refresh triggered."); fetch_data() })
  
  observe({ # Scheduled 5-minute refresher
    invalidateLater(300000, session) 
    if (isolate(SharedState$status) == "idle") {
      message("Performing scheduled 5-minute refresh."); showNotification("Performing scheduled 5-minute refresh...", type="message", duration=4)
      fetch_data()
    }
  })
  
  output$last_updated_text <- renderText({
    data <- scraped_data(); req(data)
    if (!is.null(data$error)) return(paste("Last attempt failed:", format(data$timestamp, "%I:%M:%S %p")))
    paste("Last updated:", format(data$timestamp, "%I:%M:%S %p"))
  })
  
  create_timer_output <- function(timer_name, icon_name, color) {
    renderValueBox({
      req(scraped_data()); if (!is.null(scraped_data()$error)) return(valueBox("ERROR", timer_name, icon=icon("exclamation-triangle"), color="red"))
      seconds <- timers_rv()[[timer_name]]
      if (is.null(seconds) || is.na(seconds)) return(valueBox("N/A", timer_name, icon=icon("question-circle"), color="maroon"))
      h <- floor(seconds/3600); m <- floor((seconds%%3600)/60); s <- seconds%%60
      formatted_time <- if(h>0) sprintf("%d:%02d:%02d",h,m,s) else sprintf("%02d:%02d",m,s)
      valueBox(formatted_time, timer_name, icon=icon(icon_name), color=color)
    })
  }
  output$seed_timer_box <- create_timer_output("Seed Stock", "seedling", "aqua")
  output$gear_timer_box <- create_timer_output("Gear Stock", "cogs", "blue")
  output$egg_timer_box <- create_timer_output("Egg Stock", "egg", "orange")
  output$honey_timer_box <- create_timer_output("Honey Stock", "tint", "yellow")
  output$cosmetics_timer_box <- create_timer_output("Cosmetics Stock", "gem", "purple")
  output$night_timer_box <- create_timer_output("Night Stock", "moon", "navy")
  
  output$weather_box <- renderValueBox({
    data <- scraped_data(); req(data)
    if (!is.null(data$error)) return(valueBox("API OFFLINE", "Current Weather", icon=icon("exclamation-triangle"), color="red"))
    valueBox(data$weather, "Current Weather", icon=icon("cloud-sun-rain"), color="teal")
  })
  
  show_item_modal <- function(item_name) {
    info <- item_encyclopedia[[item_name]]
    live_quantity <- "Not in stock"
    for (category in scraped_data()$stocks) { if (item_name %in% category$name) { live_quantity <- paste("x", category$quantity[category$name == item_name][1]); break } }
    modal_ui <- if(is.null(info)) { p("Sorry, detailed information for this item is not available.") } else {
      fluidPage(fluidRow(
        column(4,align="center", tags$img(src=info$image,width="100%",style="max-width:150px;"), tags$h3(info$name), tags$p(strong("Current Stock: "), live_quantity)),
        column(8, tags$h4("Item Details"), tags$p(strong("Category: "), info$category %||% "N/A"), tags$p(strong("Rarity Tier: "), info$metadata$tier %||% "N/A"),
               tags$p(strong("Buy Price: "), info$metadata$buyPrice %||% "N/A"), tags$p(strong("Sell Value: "), info$metadata$sellValue %||% "N/A"),
               tags$p(strong("Amount in Shop: "), info$metadata$amountInShop %||% "N/A"), tags$p(strong("Tradeable: "), if(isTRUE(info$metadata$tradeable)) "Yes" else "No"))
      ))
    }
    showModal(modalDialog(title = item_name, modal_ui, easyClose = TRUE, footer = modalButton("Close")))
  }
  
  if(length(item_encyclopedia) > 0) {
    lapply(names(item_encyclopedia), function(item_name) {
      button_id <- paste0("show_details_", gsub("[^A-Za-z0-9]", "", item_name))
      observeEvent(input[[button_id]], {
        show_item_modal(item_name)
      })
    })
  }
  
  create_item_grid <- function(category_name) {
    renderUI({
      data <- scraped_data(); req(data); if (!is.null(data$error)) return(p("Could not load stock data.", style="color:red; text-align:center;"))
      df <- data$stocks[[category_name]]; if (is.null(df) || nrow(df)==0) return(p("No items in this category right now.", style="text-align:center;"))
      item_html <- lapply(1:nrow(df), function(i) {
        item_name <- df$name[i]; button_id <- paste0("show_details_",gsub("[^A-Za-z0-9]","",item_name))
        label_div <- tags$div(class="stock-item", tags$img(src=df$image_url[i],alt=item_name), tags$p(item_name), tags$p(class="quantity",paste("x",df$quantity[i])))
        if (item_name %in% names(item_encyclopedia)) actionLink(inputId=button_id, label=label_div, class="stock-item-link") else label_div
      })
      tags$div(style="display:flex; flex-wrap:wrap; justify-content:center;", item_html)
    })
  }
  output$seed_stock_ui <- create_item_grid("Seed Stock"); output$gear_stock_ui <- create_item_grid("Gear Stock");
  output$egg_stock_ui <- create_item_grid("Egg Stock"); output$cosmetics_stock_ui <- create_item_grid("Cosmetics Stock");
  output$honey_stock_ui <- create_item_grid("Honey Stock"); output$night_stock_ui <- create_item_grid("Night Stock")
  
  history_data <- eventReactive(scraped_data(), {
    if (!file.exists(HISTORY_FILE)) return(data.frame()) 
    read.csv(HISTORY_FILE) %>% mutate(timestamp_val = ymd_hms(timestamp, tz=Sys.timezone())) %>%
      arrange(desc(timestamp_val)) %>% mutate(timestamp = format(timestamp_val, "%Y-%m-%d %I:%M:%S %p")) %>% select(-timestamp_val)
  }, ignoreNULL = FALSE)
  
  # --- Module Servers ---
  
  fruit_value_list_rv <- reactive({
    df <- fruit_data_rv()
    req(df, nrow(df) > 0)
    df_calc <- df %>%
      mutate(
        sell_value_numeric = as.numeric(str_extract(sell_value, "\\d+"))
      )
    setNames(as.list(df_calc$sell_value_numeric), df_calc$name)
  })
  
  calculator_return <- calculator_server("calculator", fruit_data_rv = fruit_data_rv, fruit_value_list_rv = fruit_value_list_rv)
  
  observeEvent(calculator_return$fetch_data(), {
    req(calculator_return$fetch_data() > 0) 
    fetch_and_save_fruit_data()
  }, ignoreInit = TRUE) 
  
  history_server("history_module", history_data = history_data)
}

# =============================================================
# PART 5: RUN THE APP
# =============================================================
shinyApp(ui = ui, server = server)

# --- END FILE: app.R ---