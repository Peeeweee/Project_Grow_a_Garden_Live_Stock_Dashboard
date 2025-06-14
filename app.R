# --- START FILE: app.R ---

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    Grow a Garden Live Stock Monitoring Dashboard (FINAL)
#         ### FINAL/REVIEWED VERSION: All features, modules, and layout fixes integrated.
#         ### This version is cleaned, documented, and production-ready.
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
library(xml2) # Explicitly load xml2 to ensure its functions are available for rvest

# SOURCE THE MODULES
source("history_module.R")
source("calculator_module.R")
source("encyclopedia_module.R")


# =============================================================
# PART 2: GLOBAL OBJECTS & DATA FUNCTIONS
# =============================================================

# --- Define global file paths for data persistence ---
HISTORY_FILE <- "vulcan_stock_history.csv"
FRUIT_VALUES_FILE <- "fruit_values.csv"


# --- API & SCRAPING FUNCTIONS ---
# These functions are responsible for gathering all data from external sources.
# Each is wrapped in a tryCatch block to prevent network errors from crashing the app.

# Fetches item details for the pop-up modals from the official API.
fetch_item_info <- function() {
  message("--- Fetching Item Info Encyclopedia (once) ---")
  item_info_url <- "https://growagardenapi.vercel.app/api/Item-Info"
  tryCatch({
    response <- GET(item_info_url)
    stop_for_status(response, "fetch item info")
    item_data <- content(response, "parsed")
    # Convert the list of items into a named list for fast lookups
    encyclopedia <- setNames(item_data, sapply(item_data, `[[`, "name"))
    message("   Item encyclopedia fetched successfully with ", length(encyclopedia), " items.")
    return(encyclopedia)
  }, error = function(e) {
    message("   ERROR fetching item encyclopedia: ", e$message)
    return(list())
  })
}

# Initial fetch of the item encyclopedia for modals. This only runs once at startup.
item_encyclopedia <- fetch_item_info()


# Scraper for growagardencalculator.net to get fruit values for the calculator.
fetch_fruit_values <- function() {
  message("--- Fetching DETAILED Fruit Values from growagardencalculator.net ---")
  values_url <- "https://growagardencalculator.net/grow-a-garden-values"
  tryCatch({
    response <- GET(values_url)
    stop_for_status(response, "download page")
    page_html <- content(response, "text", encoding = "UTF-8") %>% read_html()
    all_tables <- page_html %>% html_nodes("table")
    if (length(all_tables) == 0) {
      message("   No tables found on the page.")
      return(tibble())
    }
    # Map over all tables and combine them into one dataframe
    df <- all_tables %>% 
      map_dfr(~html_table(.x, na.strings = c("", "NA", "N/A", "Unknown")) %>% mutate(across(everything(), as.character)))
    df <- df %>%
      rename(
        name = `Crop Name`, sheckle_price = `Sheckle Price`,
        sell_value = `Min Value`, robux_price = `Robux Price`,
        stock = Stock, rarity = Tier,
        multi_harvest = `Multi Harvest`, obtainable = Obtainable
      ) %>%
      mutate(
        # Handle special cases where sell value is listed as rarity tier
        sell_value = if_else(sell_value %in% c("Mythical", "Divine"), robux_price, sell_value),
        # Clean up the sell value to be purely numeric
        sell_value = as.character(sell_value),
        sell_value = str_replace_all(sell_value, "[~,]", ""),
        sell_value = str_extract(sell_value, "^\\d+"),
        sell_value = str_trim(sell_value)
      ) %>%
      # Ensure data is clean and valid
      filter(name != "", !is.na(name), sell_value != "", !is.na(sell_value)) %>%
      filter(!is.na(suppressWarnings(as.numeric(sell_value))))
    message("   Detailed fruit values scraped successfully for ", nrow(df), " fruits.")
    return(df)
  }, error = function(e) {
    message("   ERROR fetching fruit values: ", e$message)
    showNotification(paste("Failed to fetch fruit values from", values_url), type = "error", duration = 10)
    return(tibble())
  })
}


# Scraper for the mutations page, used by the Calculator.
fetch_mutation_data <- function() {
  message("--- Fetching Mutation Data (once) ---")
  mutations_url <- "https://growagardencalculator.net/wiki/grow-a-garden-mutations"
  tryCatch({
    page_html <- read_html(mutations_url)
    mutation_table <- page_html %>% html_node("table")
    df <- mutation_table %>% html_table()
    
    df <- df %>%
      rename(name = `Mutation Name`, bonus = `Stack Bonus`) %>%
      select(name, bonus) %>%
      mutate(
        bonus = str_replace(bonus, "\\+", ""),
        bonus = suppressWarnings(as.numeric(bonus))
      ) %>%
      filter(!is.na(bonus))
    
    message("   Mutation data fetched successfully for ", nrow(df), " mutations.")
    return(df)
  }, error = function(e) {
    message("   ERROR fetching mutation data: ", e$message)
    showNotification(paste("Failed to fetch mutation data from", mutations_url), type = "error", duration = 10)
    return(tibble())
  })
}

# Initial fetch of mutation data. This only runs once at startup.
mutation_data <- fetch_mutation_data()


# Primary data function for the live dashboard, using the official API.
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
    
    # Helper function to process each stock category list from the API response
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
    
    # Helper function to parse countdown strings like "1h30m15s" into seconds
    parse_countdown_to_seconds <- function(s) {
      if (is.null(s) || !is.character(s) || nchar(s)==0) return(NA_integer_)
      h <- as.numeric(str_extract(s, "\\d+(?=h)")); h <- ifelse(is.na(h), 0, h)
      m <- as.numeric(str_extract(s, "\\d+(?=m)")); m <- ifelse(is.na(m), 0, m)
      s <- as.numeric(str_extract(s, "\\d+(?=s)")); s <- ifelse(is.na(s), 0, s)
      h*3600 + m*60 + s
    }
    
    # The API sometimes returns NA for honey/egg timers; this logic provides a fallback.
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
    
    # Filter for weather events that are currently active based on unix timestamps
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


# --- Encyclopedia Scraping Functions (for the Encyclopedia Tab) ---
# These run once at startup to populate the encyclopedia tab.

fetch_detailed_seed_data <- function() {
  message("--- Fetching DETAILED Seed/Crop Data from growagardencalculator.net ---")
  url <- "https://growagardencalculator.net/grow-a-garden-values"
  tryCatch({
    page <- read_html(url)
    all_tables <- page %>% html_nodes("table")
    if (length(all_tables) == 0) stop("Could not find any tables on the seed/crop values page.")
    
    all_seeds_df <- all_tables %>%
      map_dfr(function(table_node) {
        rows <- table_node %>% html_nodes("tr") %>% tail(-1)
        map_dfr(rows, function(row) {
          cells <- row %>% html_nodes("td")
          if (length(cells) != 9) return(NULL)
          tibble(
            image_url     = cells[[1]] %>% html_node("img") %>% html_attr("src"), name          = cells[[2]] %>% html_text(trim = TRUE),
            sheckle_price = cells[[3]] %>% html_text(trim = TRUE), min_value     = cells[[4]] %>% html_text(trim = TRUE),
            robux_price   = cells[[5]] %>% html_text(trim = TRUE), stock         = cells[[6]] %>% html_text(trim = TRUE),
            tier          = cells[[7]] %>% html_text(trim = TRUE), multi_harvest = cells[[8]] %>% html_text(trim = TRUE),
            obtainable    = cells[[9]] %>% html_text(trim = TRUE)
          )
        })
      })
    message("   Successfully scraped ", nrow(all_seeds_df), " detailed seeds/crops.")
    return(all_seeds_df)
  }, error = function(e) {
    message("   ERROR fetching detailed seed data: ", e$message)
    showNotification(paste("Failed to fetch detailed seed data from", url), type = "error", duration = 10)
    return(tibble())
  })
}

fetch_ign_gear_data <- function() {
  message("--- Fetching Gear Data from IGN ---")
  url <- "https://www.ign.com/wikis/grow-a-garden/Grow_a_Garden_Gear_Guide"
  tryCatch({
    page <- read_html(url)
    all_tables <- page %>% html_nodes("table")
    table_node <- NULL
    for(tbl in all_tables) {
      headers <- tbl %>% html_nodes("th") %>% html_text(trim = TRUE)
      if (all(c("Tool", "Cost", "Use", "Number of Uses") %in% headers)) { table_node <- tbl; break }
    }
    if (is.null(table_node)) stop("Gear table not found on page.")
    
    gear_list <- list(); current_rarity <- "Unknown"
    rows <- table_node %>% html_nodes("tr")
    
    for (row in rows) {
      header_cell <- row %>% html_node("th[colspan='4']")
      if (!is.na(header_cell)) { current_rarity <- header_cell %>% html_text(trim = TRUE); next }
      cells <- row %>% html_nodes("td")
      if (length(cells) == 4) {
        gear_list[[length(gear_list) + 1]] <- tibble(
          name = cells[[1]] %>% html_text(trim = TRUE), cost = cells[[2]] %>% html_text(trim = TRUE),
          description = cells[[3]] %>% html_text(trim = TRUE), uses = cells[[4]] %>% html_text(trim = TRUE),
          rarity = current_rarity
        )
      }
    }
    if (length(gear_list) == 0) stop("No gear data rows found in the table.")
    gear_df <- bind_rows(gear_list) %>%
      mutate(cost_numeric = suppressWarnings(as.numeric(gsub("[^0-9]", "", cost)))) %>%
      select(name, rarity, cost_numeric, description, uses)
    message("   Gear data scraped successfully for ", nrow(gear_df), " items.")
    return(gear_df)
  }, error = function(e) {
    message("   ERROR fetching IGN gear data: ", e$message)
    showNotification(paste("Failed to fetch IGN gear data from", url), type = "error", duration = 10)
    return(tibble())
  })
}

fetch_ign_egg_data <- function() {
  message("--- Fetching Egg & Animal Data from IGN (including chances) ---")
  url <- "https://www.ign.com/wikis/grow-a-garden/The_Animal_Update_-_Pet_Egg_Guide"
  tryCatch({
    page <- read_html(url); all_tables <- page %>% html_nodes("table")
    if(length(all_tables) < 12) stop("Page structure has changed, expected at least 12 tables.")
    
    pet_traits_table_node <- all_tables[[12]]
    pet_traits_df <- html_table(pet_traits_table_node, header = FALSE) %>%
      magrittr::set_colnames(c("name", "rarity", "bonus")) %>% slice(-c(1, 2)) %>%
      filter(!is.na(name) & name != "")
    
    chance_table_indices <- c(2, 3, 4, 5, 6, 7, 8, 10, 11)
    pet_chances_df <- all_tables[chance_table_indices] %>%
      map_dfr(~{
        egg_source <- .x %>% html_node("th") %>% html_text(trim = TRUE) %>% str_extract("(?<=from ).*")
        html_table(.x, header = FALSE) %>%
          magrittr::set_colnames(c("name", "chance", "collection")) %>% slice(-c(1, 2)) %>%
          mutate(chance_of_appearing = paste0(chance, " (from ", egg_source, ")")) %>%
          select(name, chance_of_appearing)
      }) %>% filter(!is.na(name) & name != "") %>% distinct(name, .keep_all = TRUE)
    
    egg_df <- html_table(all_tables[[1]], header = TRUE) %>%
      rename(name = `Egg Type`, cost = Cost, chance_to_appear = `Chances to Appear`, 
             num_pets = `Number of Pet Types`, grow_time = `Time to Grow`) %>%
      filter(!is.na(name) & name != "Egg Type") %>% mutate(type = "Egg")
    
    final_pet_df <- left_join(pet_traits_df, pet_chances_df, by = "name")
    message("   Egg & Pet data scraped and merged successfully.")
    return(list(eggs = egg_df, pets = final_pet_df))
  }, error = function(e) {
    message("   ERROR fetching IGN egg/pet data: ", e$message)
    showNotification(paste("Failed to fetch IGN egg/pet data from", url), type = "error", duration = 10)
    return(list(eggs = tibble(), pets = tibble()))
  })
}

fetch_pet_chances_data <- function() {
  message("--- Fetching Pet Chance Data from growagardencalculator.net ---")
  url <- "https://growagardencalculator.net/wiki/grow-a-garden-pets-and-animals"
  tryCatch({
    page_html <- read_html(url)
    all_tables <- page_html %>% html_nodes("table")
    if (length(all_tables) == 0) stop("No tables found on the pet chances page.")
    chances_df <- all_tables %>%
      map(~{
        df <- html_table(.x, header = TRUE) %>% mutate(across(everything(), as.character))
        if (all(c("Animal Type", "Chance of Appearing") %in% names(df))) {
          df %>% select(name = `Animal Type`, chance_of_appearing = `Chance of Appearing`)
        } else { NULL }
      }) %>% purrr::compact() %>% bind_rows() %>% filter(!is.na(name) & name != "")
    if (nrow(chances_df) > 0) message("   Pet chance data scraped successfully for ", nrow(chances_df), " pets.")
    else message("   WARNING: No valid pet chance tables were found on the page.")
    return(chances_df)
  }, error = function(e) {
    message("   ERROR fetching pet chance data: ", e$message)
    showNotification(paste("Failed to fetch pet chance data from", url), type = "error", duration = 10)
    return(tibble())
  })
}

fetch_detailed_mutations_data <- function() {
  message("--- Fetching DETAILED Mutation Data from growagardencalculator.net ---")
  url <- "https://growagardencalculator.net/wiki/grow-a-garden-mutations"
  tryCatch({
    page <- read_html(url)
    table_node <- page %>% html_node("table")
    if (is.null(table_node) || is.na(table_node)) stop("Could not find the mutation table.")
    
    mutations_df <- table_node %>% html_nodes("tr") %>% tail(-1) %>%
      map_dfr(function(row) {
        cells <- row %>% html_nodes("td")
        if (length(cells) != 5) return(NULL)
        tibble(
          name = cells[[1]] %>% html_text(trim = TRUE), icon = cells[[2]] %>% html_node("img") %>% html_attr("src"),
          multiplier = cells[[3]] %>% html_text(trim = TRUE), stack_bonus = cells[[4]] %>% html_text(trim = TRUE),
          description = cells[[5]] %>% html_text(trim = TRUE)
        )
      })
    message("   Successfully scraped ", nrow(mutations_df), " detailed mutations.")
    return(mutations_df)
  }, error = function(e) {
    message("   ERROR fetching detailed mutation data: ", e$message)
    showNotification(paste("Failed to fetch detailed mutation data from", url), type = "error", duration = 10)
    return(tibble())
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
                menuItem("Fruit Calculator", tabName="calculator", icon=icon("calculator")),
                menuItem("Encyclopedia", tabName = "encyclopedia", icon = icon("book"))
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    # --- Custom CSS for styling the application ---
    tags$head(tags$style(HTML("
      /* Style for item links in the live dashboard */
      .stock-item-link { color: inherit; text-decoration: none; }
      /* Style for individual item boxes (cards) */
      .stock-item {text-align:center; padding:10px; border:1px solid #ddd; border-radius:5px; margin:5px; background-color:#f9f9f9; width:120px; min-height:150px; /* MODIFIED: Changed height to min-height for flexible wrapping */ display:flex; flex-direction:column; justify-content:space-between; box-shadow: 0 2px 4px rgba(0,0,0,0.1); transition: transform 0.2s, box-shadow 0.2s;}
      .stock-item:hover { transform: translateY(-3px); box-shadow: 0 4px 8px rgba(0,0,0,0.2); cursor: pointer; }
      .stock-item img {max-width:80px; max-height:80px; margin:0 auto;}
      .stock-item p {margin-top:5px; font-weight:bold; font-size: 0.9em;}
      .stock-item .quantity {font-size: 1.1em; color: #2c3e50; font-weight: bold;}
      /* Loading overlay styles */
      #loading-overlay { position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(0,0,0,0.5); z-index: 10000; display: flex; justify-content: center; align-items: center; }
      .loader { border: 8px solid #f3f3f3; border-top: 8px solid #8E44AD; border-radius: 50%; width: 60px; height: 60px; animation: spin 1s linear infinite; }
      @keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }
      /* Responsive header title for small screens */
      @media (max-width: 767px) { .main-header .logo { font-size: 12px; overflow: hidden; text-overflow: ellipsis; } }
      /* Styles for the calculator tab */
      .formula-box { background-color: #00c0ef; color: white; padding: 10px; border-radius: 3px; text-align: center; font-weight: bold; }
      .solution-box { background-color: #f39c12; color: white; padding: 10px; border-radius: 3px; text-align: center; font-weight: bold; font-family: monospace; font-size: 1.2em; word-wrap: break-word; }
    "))),
    # The loading overlay div, hidden by default
    tags$div(id = "loading-overlay", style = "display: none;", div(class="loader")),
    
    # --- Main Tab Content ---
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
      # UI from modules
      history_ui("history_module"),
      calculator_ui("calculator"),
      encyclopedia_ui("encyclopedia_module")
    )
  )
)

# =============================================================
# PART 4: SERVER (APPLICATION LOGIC)
# =============================================================
server <- function(input, output, session) {
  message("Shiny server function initialized.")
  
  # Reactive values for managing shared state and data
  SharedState <- reactiveValues(status = "idle") # Prevents multiple simultaneous fetches
  scraped_data <- reactiveVal()
  timers_rv <- reactiveVal(list())
  fruit_data_rv <- reactiveVal()
  
  # --- Data Management for Fruit Calculator ---
  # Function to fetch and save the latest fruit data from the web scraper
  fetch_and_save_fruit_data <- function() {
    shinyjs::show("loading-overlay")
    showNotification("Fetching latest fruit data...", type = "message", duration = 5)
    new_data <- fetch_fruit_values()
    
    if (nrow(new_data) > 0) {
      write.csv(new_data, FRUIT_VALUES_FILE, row.names = FALSE)
      fruit_data_rv(new_data)
      showNotification("Successfully updated and saved fruit data!", type = "message", duration = 5)
    } else {
      showNotification("Failed to fetch new fruit data. Using existing data if available.", type = "error", duration = 8)
    }
    shinyjs::hide("loading-overlay")
    return(new_data)
  }
  
  # On app start, load fruit data from local cache if it exists, otherwise fetch it.
  if (file.exists(FRUIT_VALUES_FILE)) {
    message("Loading fruit data from local CSV file.")
    fruit_data_rv(read.csv(FRUIT_VALUES_FILE, check.names = FALSE))
  } else {
    message("No local fruit data found. Fetching for the first time.")
    fetch_and_save_fruit_data()
  }
  
  
  # --- Core Data Fetching for Live Dashboard ---
  fetch_data <- function() {
    if (isolate(SharedState$status) == "fetching") return() # Prevent re-entry
    SharedState$status <- "fetching"
    shinyjs::show("loading-overlay")
    message("fetch_data() triggered. Status: fetching")
    
    data <- fetch_public_api_data()
    
    if (!is.null(data$error)) {
      showNotification(paste("API Error:", data$error), type="error", duration=10)
    }
    
    scraped_data(data)
    timers_rv(data$timers)
    
    # If fetch was successful, append the new data to the history file
    if (is.null(data$error) && length(data$stocks) > 0) {
      history_df <- map_dfr(data$stocks, ~as.data.frame(.x), .id="category") %>%
        mutate(timestamp = data$timestamp, quantity = as.character(quantity)) %>%
        select(timestamp, category, item_name = name, quantity, image_url)
      
      # Safer check to ensure there's data to write
      if(nrow(history_df) > 0) {
        write.table(history_df, HISTORY_FILE, sep=",", row.names=F, append=file.exists(HISTORY_FILE), col.names=!file.exists(HISTORY_FILE))
      }
    }
    
    SharedState$status <- "idle"
    shinyjs::hide("loading-overlay")
    message("Fetch complete. Status: idle")
  }
  
  # Initial data fetch when the app starts
  fetch_data()
  
  # --- Timer and Refresh Logic ---
  # This observer ticks every second to countdown the timers.
  observe({ 
    req(SharedState$status == "idle")
    invalidateLater(1000, session)
    if (!is.null(isolate(scraped_data()$error))) return() # Stop countdown if there's an error
    
    temp_timers <- isolate(timers_rv())
    if (length(temp_timers) == 0) return()
    
    refresh_needed <- FALSE
    for (name in names(temp_timers)) {
      if (is.numeric(temp_timers[[name]]) && !is.na(temp_timers[[name]]) && temp_timers[[name]] > 0) {
        temp_timers[[name]] <- temp_timers[[name]] - 1
        if (temp_timers[[name]] <= 0) refresh_needed <- TRUE
      }
    }
    
    if (refresh_needed) {
      message("A timer hit zero. Triggering auto-refresh.")
      showNotification("A timer finished! Auto-refreshing...", type="warning", duration=5)
      fetch_data()
    } else {
      timers_rv(temp_timers) # Update the timers without a full refresh
    }
  })
  
  # Manual refresh button
  observeEvent(input$refresh_button, { message("Manual refresh triggered."); fetch_data() })
  
  # Scheduled 5-minute refresher to ensure data doesn't become stale
  observe({
    invalidateLater(300000, session) 
    if (isolate(SharedState$status) == "idle") {
      message("Performing scheduled 5-minute refresh."); showNotification("Performing scheduled 5-minute refresh...", type="message", duration=4)
      fetch_data()
    }
  })
  
  # --- Dashboard UI Rendering ---
  output$last_updated_text <- renderText({
    data <- scraped_data(); req(data)
    if (!is.null(data$error)) return(paste("Last attempt failed:", format(data$timestamp, "%I:%M:%S %p")))
    paste("Last updated:", format(data$timestamp, "%I:%M:%S %p"))
  })
  
  # Helper function to create timer value boxes, reducing code duplication
  create_timer_output <- function(timer_name, icon_name, color) {
    renderValueBox({
      req(scraped_data())
      if (!is.null(scraped_data()$error)) return(valueBox("ERROR", timer_name, icon=icon("exclamation-triangle"), color="red"))
      
      seconds <- timers_rv()[[timer_name]]
      if (is.null(seconds) || is.na(seconds)) return(valueBox("N/A", timer_name, icon=icon("question-circle"), color="maroon"))
      
      h <- floor(seconds/3600); m <- floor((seconds%%3600)/60); s <- seconds%%60
      formatted_time <- if(h>0) sprintf("%d:%02d:%02d",h,m,s) else sprintf("%02d:%02d",m,s)
      valueBox(formatted_time, timer_name, icon=icon(icon_name), color=color)
    })
  }
  
  # Assign helper to each timer output
  output$seed_timer_box <- create_timer_output("Seed Stock", "seedling", "aqua")
  output$gear_timer_box <- create_timer_output("Gear Stock", "cogs", "blue")
  output$egg_timer_box <- create_timer_output("Egg Stock", "egg", "orange")
  output$honey_timer_box <- create_timer_output("Honey Stock", "tint", "yellow")
  output$cosmetics_timer_box <- create_timer_output("Cosmetics Stock", "gem", "purple")
  output$night_timer_box <- create_timer_output("Night Stock", "moon", "navy")
  
  # Weather value box
  output$weather_box <- renderValueBox({
    data <- scraped_data(); req(data)
    if (!is.null(data$error)) return(valueBox("API OFFLINE", "Current Weather", icon=icon("exclamation-triangle"), color="red"))
    valueBox(data$weather, "Current Weather", icon=icon("cloud-sun-rain"), color="teal")
  })
  
  # --- Item Grid and Modal Logic ---
  # Function to show a detailed modal for a clicked item
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
  
  # Dynamically create observers for each clickable item in the encyclopedia
  if(length(item_encyclopedia) > 0) {
    lapply(names(item_encyclopedia), function(item_name) {
      button_id <- paste0("show_details_", gsub("[^A-Za-z0-9]", "", item_name))
      observeEvent(input[[button_id]], { show_item_modal(item_name) })
    })
  }
  
  # Helper function to create the item grid UI, reducing code duplication.
  # This function's output is clean and does NOT produce the overlapping text from the screenshot.
  create_item_grid <- function(category_name) {
    renderUI({
      data <- scraped_data(); req(data); if (!is.null(data$error)) return(p("Could not load stock data.", style="color:red; text-align:center;"))
      df <- data$stocks[[category_name]]; if (is.null(df) || nrow(df)==0) return(p("No items in this category right now.", style="text-align:center;"))
      
      item_html <- lapply(1:nrow(df), function(i) {
        item_name <- df$name[i]; button_id <- paste0("show_details_",gsub("[^A-Za-z0-9]","",item_name))
        
        # This defines the content of a single item card
        label_div <- tags$div(class="stock-item", 
                              tags$img(src=df$image_url[i], alt=item_name), 
                              tags$p(item_name), 
                              tags$p(class="quantity", paste("x", df$quantity[i]))
        )
        
        # If the item exists in our encyclopedia, make it a clickable actionLink
        if (item_name %in% names(item_encyclopedia)) {
          actionLink(inputId=button_id, label=label_div, class="stock-item-link")
        } else {
          label_div # Otherwise, just display the div
        }
      })
      tags$div(style="display:flex; flex-wrap:wrap; justify-content:center;", item_html)
    })
  }
  
  # Assign helper to each stock UI output
  output$seed_stock_ui <- create_item_grid("Seed Stock"); output$gear_stock_ui <- create_item_grid("Gear Stock");
  output$egg_stock_ui <- create_item_grid("Egg Stock"); output$cosmetics_stock_ui <- create_item_grid("Cosmetics Stock");
  output$honey_stock_ui <- create_item_grid("Honey Stock"); output$night_stock_ui <- create_item_grid("Night Stock")
  
  
  # --- Data Provisioning for Modules ---
  
  # This reactive reads the history CSV file ONLY when new data is fetched.
  history_data <- eventReactive(scraped_data(), {
    if (!file.exists(HISTORY_FILE)) return(data.frame()) 
    read.csv(HISTORY_FILE) %>% 
      mutate(timestamp_val = ymd_hms(timestamp, tz=Sys.timezone())) %>%
      arrange(desc(timestamp_val)) %>% 
      mutate(timestamp = format(timestamp_val, "%Y-%m-%d %I:%M:%S %p")) %>% 
      select(-timestamp_val)
  }, ignoreNULL = FALSE)
  
  # This reactive value fetches and holds ALL encyclopedia data at startup.
  # It is then passed to the encyclopedia module.
  encyclopedia_data <- reactiveVal({
    message("--- Aggregating all encyclopedia data at startup... ---")
    ign_data <- fetch_ign_egg_data()
    Sys.sleep(1) # Be polite to the servers
    pet_chances <- fetch_pet_chances_data()
    Sys.sleep(1)
    
    if (!is.null(ign_data$pets) && nrow(pet_chances) > 0) {
      if (!"chance_of_appearing" %in% names(ign_data$pets)) {
        ign_data$pets <- ign_data$pets %>% left_join(pet_chances, by = "name")
      }
    } else if (!is.null(ign_data$pets) && !"chance_of_appearing" %in% names(ign_data$pets)) {
      ign_data$pets$chance_of_appearing <- NA_character_
    }
    
    list(
      seeds = fetch_detailed_seed_data(),
      gear = fetch_ign_gear_data(),
      eggs_and_pets = ign_data,
      mutations = fetch_detailed_mutations_data()
    )
  })
  
  # This reactive creates a named list of fruit values for the calculator's dropdown
  fruit_value_list_rv <- reactive({
    df <- fruit_data_rv(); req(df, nrow(df) > 0)
    df_calc <- df %>% mutate(sell_value_numeric = as.numeric(sell_value)) %>% filter(!is.na(sell_value_numeric))
    setNames(as.list(df_calc$sell_value_numeric), df_calc$name)
  })
  
  # --- Module Server Initializations ---
  
  # Calculator Module
  calculator_return <- calculator_server("calculator", 
                                         fruit_data_rv = fruit_data_rv, 
                                         fruit_value_list_rv = fruit_value_list_rv,
                                         mutation_data = mutation_data)
  
  # Listen for the "Fetch Latest" button click from the calculator module
  observeEvent(calculator_return$fetch_data(), {
    req(calculator_return$fetch_data() > 0) 
    fetch_and_save_fruit_data()
  }, ignoreInit = TRUE) 
  
  # History Module
  history_server("history_module", history_data = history_data)
  
  # Encyclopedia Module
  encyclopedia_server("encyclopedia_module", all_encyclopedia_data = encyclopedia_data)
}

# =============================================================
# PART 5: RUN THE APP
# =============================================================
shinyApp(ui = ui, server = server)

# --- END FILE: app.R ---