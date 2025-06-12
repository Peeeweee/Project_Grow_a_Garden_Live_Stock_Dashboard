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
library(xml2) # Explicitly load xml2 for its functions

# SOURCE THE MODULES
source("history_module.R")
source("calculator_module.R")
source("encyclopedia_module.R")


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


# Scraper for growagardencalculator.net/grow-a-garden-values
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
        sell_value = if_else(sell_value %in% c("Mythical", "Divine"), robux_price, sell_value),
        sell_value = as.character(sell_value),
        sell_value = str_replace_all(sell_value, "[~,]", ""),
        sell_value = str_extract(sell_value, "^\\d+"),
        sell_value = str_trim(sell_value)
      ) %>%
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


# Scraper for the mutations page
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

mutation_data <- fetch_mutation_data()


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


# --- ENCYCLOPEDIA SCRAPING FUNCTIONS ---

# Scraper for the detailed seed/crop values page on growagardencalculator.net
fetch_detailed_seed_data <- function() {
  message("--- Fetching DETAILED Seed/Crop Data from growagardencalculator.net ---")
  url <- "https://growagardencalculator.net/grow-a-garden-values"
  
  tryCatch({
    page <- read_html(url)
    all_tables <- page %>% html_nodes("table")
    
    if (length(all_tables) == 0) {
      stop("Could not find any tables on the seed/crop values page.")
    }
    
    # Iterate over every table on the page
    all_seeds_df <- all_tables %>%
      map_dfr(function(table_node) {
        # Get all rows from the current table, skipping the header
        rows <- table_node %>% html_nodes("tr") %>% tail(-1)
        
        # Iterate over each row to extract data cell by cell
        map_dfr(rows, function(row) {
          cells <- row %>% html_nodes("td")
          if (length(cells) != 9) return(NULL) # Safety check for correct number of columns
          
          tibble(
            image_url     = cells[[1]] %>% html_node("img") %>% html_attr("src"),
            name          = cells[[2]] %>% html_text(trim = TRUE),
            sheckle_price = cells[[3]] %>% html_text(trim = TRUE),
            min_value     = cells[[4]] %>% html_text(trim = TRUE),
            robux_price   = cells[[5]] %>% html_text(trim = TRUE),
            stock         = cells[[6]] %>% html_text(trim = TRUE),
            tier          = cells[[7]] %>% html_text(trim = TRUE),
            multi_harvest = cells[[8]] %>% html_text(trim = TRUE),
            obtainable    = cells[[9]] %>% html_text(trim = TRUE)
          )
        })
      })
    
    message("   Successfully scraped ", nrow(all_seeds_df), " detailed seeds/crops.")
    return(all_seeds_df)
    
  }, error = function(e) {
    message("   ERROR fetching detailed seed data: ", e$message)
    showNotification(paste("Failed to fetch detailed seed data from", url), type = "error", duration = 10)
    return(tibble()) # Return an empty tibble on failure
  })
}

# --- FIXED & UPDATED ---
fetch_ign_gear_data <- function() {
  message("--- Fetching Gear Data from IGN ---")
  url <- "https://www.ign.com/wikis/grow-a-garden/Grow_a_Garden_Gear_Guide"
  tryCatch({
    page <- read_html(url)
    
    # Find the correct table by looking for its specific column headers
    all_tables <- page %>% html_nodes("table")
    table_node <- NULL
    for(tbl in all_tables) {
      headers <- tbl %>% html_nodes("th") %>% html_text(trim = TRUE)
      if (all(c("Tool", "Cost", "Use", "Number of Uses") %in% headers)) {
        table_node <- tbl
        break
      }
    }
    
    if (is.null(table_node)) stop("Gear table not found on page.")
    
    gear_list <- list()
    current_rarity <- "Unknown"
    rows <- table_node %>% html_nodes("tr")
    
    for (row in rows) {
      header_cell <- row %>% html_node("th[colspan='4']")
      if (!is.na(header_cell)) {
        current_rarity <- header_cell %>% html_text(trim = TRUE)
        next
      }
      
      cells <- row %>% html_nodes("td")
      if (length(cells) == 4) {
        gear_list[[length(gear_list) + 1]] <- tibble(
          name = cells[[1]] %>% html_text(trim = TRUE),
          cost = cells[[2]] %>% html_text(trim = TRUE),
          description = cells[[3]] %>% html_text(trim = TRUE),
          uses = cells[[4]] %>% html_text(trim = TRUE),
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
    page <- read_html(url)
    all_tables <- page %>% html_nodes("table")
    
    if(length(all_tables) < 12) {
      stop("Page structure has changed, expected at least 12 tables.")
    }
    
    # --- Scrape Pet Traits Table (by position, with manual cleaning) ---
    message("   Scraping the main 'Pet Traits' table (Table #12)...")
    pet_traits_table_node <- all_tables[[12]]
    pet_traits_df <- html_table(pet_traits_table_node, header = FALSE) %>%
      magrittr::set_colnames(c("name", "rarity", "bonus")) %>%
      slice(-c(1, 2)) %>%
      filter(!is.na(name) & name != "")
    message("   SUCCESS: Scraped ", nrow(pet_traits_df), " pets from the main traits table.")
    
    # --- Scrape all Pet Chance tables (by position) ---
    message("   Scraping all 'Pet Chance' tables...")
    chance_table_indices <- c(2, 3, 4, 5, 6, 7, 8, 10, 11)
    chance_tables_list <- all_tables[chance_table_indices]
    
    pet_chances_df <- chance_tables_list %>%
      map_dfr(~{
        egg_source <- .x %>% html_node("th") %>% html_text(trim = TRUE) %>%
          str_extract("(?<=from ).*")
        
        html_table(.x, header = FALSE) %>%
          magrittr::set_colnames(c("name", "chance", "collection")) %>%
          slice(-c(1, 2)) %>%
          mutate(chance_of_appearing = paste0(chance, " (from ", egg_source, ")")) %>%
          select(name, chance_of_appearing)
      }) %>%
      filter(!is.na(name) & name != "") %>%
      distinct(name, .keep_all = TRUE)
    message("   SUCCESS: Scraped and combined ", nrow(pet_chances_df), " pet chances.")
    
    # --- Scrape Egg Info Table (by position) ---
    message("   Scraping the 'Egg Info' table (Table #1)...")
    egg_df <- html_table(all_tables[[1]], header = TRUE) %>%
      rename(
        name = `Egg Type`, 
        cost = Cost, 
        chance_to_appear = `Chances to Appear`, 
        num_pets = `Number of Pet Types`, 
        grow_time = `Time to Grow`
      ) %>%
      filter(!is.na(name) & name != "Egg Type") %>%
      mutate(type = "Egg")
    
    # --- Join the datasets ---
    message("   Joining datasets...")
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
    
    list_of_dfs <- all_tables %>%
      map(~{
        df <- html_table(.x, header = TRUE) %>% mutate(across(everything(), as.character))
        if (all(c("Animal Type", "Chance of Appearing") %in% names(df))) {
          df %>%
            select(name = `Animal Type`, chance_of_appearing = `Chance of Appearing`)
        } else {
          NULL
        }
      })
    
    chances_df <- list_of_dfs %>%
      purrr::compact() %>%
      bind_rows() %>%
      filter(!is.na(name) & name != "")
    
    if (nrow(chances_df) > 0) {
      message("   Pet chance data scraped successfully for ", nrow(chances_df), " pets.")
    } else {
      message("   WARNING: No valid pet chance tables were found on the page.")
    }
    
    return(chances_df)
    
  }, error = function(e) {
    message("   ERROR fetching pet chance data: ", e$message)
    showNotification(paste("Failed to fetch pet chance data from", url), type = "error", duration = 10)
    return(tibble()) # Return empty tibble on error
  })
}

# Scraper for the detailed mutations page on growagardencalculator.net
fetch_detailed_mutations_data <- function() {
  message("--- Fetching DETAILED Mutation Data from growagardencalculator.net ---")
  url <- "https://growagardencalculator.net/wiki/grow-a-garden-mutations"
  
  tryCatch({
    page <- read_html(url)
    table_node <- page %>% html_node("table")
    
    if (is.null(table_node) || is.na(table_node)) {
      stop("Could not find the mutation table on the page.")
    }
    
    # Process rows to extract text and image URLs correctly
    rows <- table_node %>% html_nodes("tr")
    
    mutations_df <- rows %>%
      tail(-1) %>% # Skip header row
      map_dfr(function(row) {
        cells <- row %>% html_nodes("td")
        
        if (length(cells) != 5) return(NULL) # Skip malformed rows
        
        tibble(
          name = cells[[1]] %>% html_text(trim = TRUE),
          icon = cells[[2]] %>% html_node("img") %>% html_attr("src"),
          multiplier = cells[[3]] %>% html_text(trim = TRUE),
          stack_bonus = cells[[4]] %>% html_text(trim = TRUE),
          description = cells[[5]] %>% html_text(trim = TRUE)
        )
      })
    
    message("   Successfully scraped ", nrow(mutations_df), " detailed mutations.")
    return(mutations_df)
    
  }, error = function(e) {
    message("   ERROR fetching detailed mutation data: ", e$message)
    showNotification(paste("Failed to fetch detailed mutation data from", url), type = "error", duration = 10)
    return(tibble()) # Return an empty tibble on failure
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
      
      .formula-box {
        background-color: #00c0ef;
        color: white;
        padding: 10px;
        border-radius: 3px;
        text-align: center;
        font-weight: bold;
      }
      .solution-box {
        background-color: #f39c12;
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
  
  SharedState <- reactiveValues(status = "idle")
  scraped_data <- reactiveVal(); timers_rv <- reactiveVal(list())
  
  fruit_data_rv <- reactiveVal()
  
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
  
  # Fetch and store all encyclopedia data at startup
  encyclopedia_data <- reactiveVal({
    # Add a small delay between requests to be more polite to the servers
    ign_data <- fetch_ign_egg_data()
    Sys.sleep(1) 
    pet_chances <- fetch_pet_chances_data()
    Sys.sleep(1)
    
    # Safely join the pet chances to the main pet data
    if (!is.null(ign_data$pets) && nrow(pet_chances) > 0) {
      # Note: We only join the 'chance_of_appearing' if it's not already present from the IGN scrape
      if (!"chance_of_appearing" %in% names(ign_data$pets)) {
        ign_data$pets <- ign_data$pets %>%
          left_join(pet_chances, by = "name")
      }
    } else if (!is.null(ign_data$pets) && !"chance_of_appearing" %in% names(ign_data$pets)) {
      # If the scrape fails, add an empty column to prevent errors
      ign_data$pets$chance_of_appearing <- NA_character_
    }
    
    list(
      seeds = fetch_detailed_seed_data(),
      gear = fetch_ign_gear_data(),
      eggs_and_pets = ign_data, # Use the merged data
      mutations = fetch_detailed_mutations_data()
    )
  })
  
  # --- Module Servers ---
  
  fruit_value_list_rv <- reactive({
    df <- fruit_data_rv()
    req(df, nrow(df) > 0, "name" %in% names(df), "sell_value" %in% names(df))
    
    df_calc <- df %>%
      mutate(
        sell_value_numeric = as.numeric(sell_value)
      ) %>% 
      filter(!is.na(sell_value_numeric))
    
    setNames(as.list(df_calc$sell_value_numeric), df_calc$name)
  })
  
  calculator_return <- calculator_server("calculator", 
                                         fruit_data_rv = fruit_data_rv, 
                                         fruit_value_list_rv = fruit_value_list_rv,
                                         mutation_data = mutation_data)
  
  observeEvent(calculator_return$fetch_data(), {
    req(calculator_return$fetch_data() > 0) 
    fetch_and_save_fruit_data()
  }, ignoreInit = TRUE) 
  
  history_server("history_module", history_data = history_data)
  encyclopedia_server("encyclopedia_module", all_encyclopedia_data = encyclopedia_data)
}

# =============================================================
# PART 5: RUN THE APP
# =============================================================
shinyApp(ui = ui, server = server)

# --- END FILE: app.R ---