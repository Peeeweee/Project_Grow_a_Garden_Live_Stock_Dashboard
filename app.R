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
DEBUG_HTML_FILE <- "debug_page_content.html"


# --- API & SCRAPING FUNCTIONS ---

fetch_item_info <- function() {
  message("--- Fetching Item Info Encyclopedia (v2) ---")
  item_info_url <- "https://growagardenapi.vercel.app/api/Item-Info"
  tryCatch({
    response <- GET(item_info_url)
    stop_for_status(response, "fetch item info")
    item_data <- content(response, "parsed")
    encyclopedia <- setNames(item_data, tolower(sapply(item_data, `[[`, "name")))
    message("   Item encyclopedia fetched successfully with ", length(encyclopedia), " items.")
    return(encyclopedia)
  }, error = function(e) {
    message("   ERROR fetching item encyclopedia: ", e$message)
    return(list())
  })
}

item_encyclopedia <- fetch_item_info()


fetch_master_seed_list <- function() {
  message("--- Fetching MASTER Seed/Crop List from growagardencalculator.net ---")
  url <- "https://growagardencalculator.net/grow-a-garden-values"
  tryCatch({
    page_html <- read_html(url)
    main_content <- page_html %>% html_nodes("div.space-y-6")
    if (length(main_content) == 0) stop("Could not find main content container.")
    
    all_seeds_df <- main_content %>% 
      html_nodes("div.space-y-4") %>%
      map_dfr(function(category_block) {
        category_name <- category_block %>% html_node("h2") %>% html_text(trim = TRUE)
        table_node <- category_block %>% html_node("table")
        if (is.na(table_node)) return(NULL)
        
        rows <- table_node %>% html_nodes("tr") %>% tail(-1)
        map_dfr(rows, function(row) {
          cells <- row %>% html_nodes("td")
          if (length(cells) < 6) return(NULL)
          
          tibble(
            image_url = cells[[1]] %>% html_node("img") %>% html_attr("src") %>% na_if(""),
            name = cells[[2]] %>% html_text(trim = TRUE),
            sheckle_price = cells[[3]] %>% html_text(trim = TRUE),
            sell_value = cells[[4]] %>% html_text(trim = TRUE),
            robux_price = cells[[5]] %>% html_text(trim = TRUE),
            rarity = cells[[6]] %>% html_text(trim = TRUE),
            stock = "N/A", multi_harvest = "Unknown", obtainable = "Unknown", source_category = category_name
          )
        })
      })
    
    if (nrow(all_seeds_df) == 0) stop("Found content container, but failed to parse any valid crop rows.")
    
    df_cleaned <- all_seeds_df %>%
      mutate(
        sell_value = if_else(sell_value %in% c("Mythical", "Divine"), robux_price, sell_value),
        sell_value = str_replace_all(sell_value, "[~,]", ""),
        sell_value = str_extract(sell_value, "^\\d+"),
        sell_value = str_trim(sell_value)
      ) %>%
      filter(!is.na(name) & name != "", !is.na(sell_value) & sell_value != "") %>%
      filter(!is.na(suppressWarnings(as.numeric(sell_value))))
    
    message("   Master seed list scraped successfully for ", nrow(df_cleaned), " fruits.")
    return(df_cleaned)
    
  }, error = function(e) {
    message("   ERROR fetching master seed list: ", e$message)
    showNotification(paste("Critical Scraper Failure:", e$message), type = "error", duration = 15)
    return(tibble())
  })
}

fetch_mutation_data <- function() {
  message("--- Fetching Mutation Data (once) ---")
  url <- "https://growagardencalculator.net/wiki/grow-a-garden-mutations"
  tryCatch({
    page_html <- read_html(url)
    mutation_table <- page_html %>% html_node("table")
    df <- mutation_table %>% html_table()
    
    df <- df %>%
      rename(name = `Mutation Name`, bonus = `Stack Bonus`) %>%
      select(name, bonus) %>%
      mutate(bonus = str_replace(bonus, "\\+", ""), bonus = suppressWarnings(as.numeric(bonus))) %>%
      filter(!is.na(bonus))
    
    message("   Mutation data fetched successfully for ", nrow(df), " mutations.")
    return(df)
  }, error = function(e) {
    message("   ERROR fetching mutation data: ", e$message)
    showNotification(paste("Failed to fetch mutation data from", url), type = "error", duration = 10)
    return(tibble())
  })
}

mutation_data <- fetch_mutation_data()


# --- DEFINITIVE VERSION: Handles partial API failures and multiple API formats gracefully ---
fetch_public_api_data <- function() {
  message("--- Starting data fetch from live API endpoints ---")
  
  stock_url <- "https://growagardenapi.vercel.app/api/stock/GetStock"
  timers_url <- "https://growagardenapi.vercel.app/api/stock/Restock-Time"
  weather_url <- "https://growagardenapi.vercel.app/api/GetWeather"
  
  # --- 1. Fetch Stock Data ---
  all_stocks <- tryCatch({
    message("   Fetching stock data...")
    stock_response <- GET(stock_url)
    stop_for_status(stock_response, "fetch stock data")
    stock_data_raw <- content(stock_response, "parsed")$Data
    
    if (is.null(stock_data_raw)) stop("Stock API did not return a 'Data' object.")
    
    process_stock_category <- function(category_data) {
      if (is.null(category_data) || length(category_data) == 0) return(tibble())
      bind_rows(category_data) %>%
        rename(quantity = stock) %>%
        mutate(
          quantity = as.integer(quantity),
          image_url = map_chr(name, ~ (item_encyclopedia[[tolower(.x)]]$image %||% "https://i.imgur.com/8fP6y2d.png"))
        )
    }
    
    stocks_list <- list(
      "Seed Stock" = process_stock_category(stock_data_raw$seeds),
      "Gear Stock" = process_stock_category(stock_data_raw$gear),
      "Egg Stock" = process_stock_category(stock_data_raw$egg),
      "Honey Stock" = process_stock_category(stock_data_raw$honey),
      "Cosmetics Stock" = process_stock_category(stock_data_raw$cosmetics),
      "Night Stock" = process_stock_category(stock_data_raw$night) 
    )
    message("   Stock data processed successfully.")
    stocks_list
  }, error = function(e) {
    message("   ! ERROR fetching stock data: ", e$message)
    list()
  })
  
  # --- 2. Fetch Timer Data ---
  all_timers <- tryCatch({
    message("   Fetching timer data...")
    timers_response <- GET(timers_url)
    stop_for_status(timers_response, "fetch timer data")
    timers_raw <- content(timers_response, "parsed")
    
    parse_countdown <- function(timer_obj) {
      if(is.null(timer_obj) || is.null(timer_obj$countdown)) return(NA_integer_)
      
      countdown_str <- timer_obj$countdown
      h_val <- as.integer(str_extract(countdown_str, "\\d+(?=h)"))
      h <- if (is.na(h_val)) 0 else h_val
      
      m_val <- as.integer(str_extract(countdown_str, "\\d+(?=m)"))
      m <- if (is.na(m_val)) 0 else m_val
      
      s_val <- as.integer(str_extract(countdown_str, "\\d+(?=s)"))
      s <- if (is.na(s_val)) 0 else s_val
      
      return(h * 3600 + m * 60 + s)
    }
    
    seed_timer_val <- parse_countdown(timers_raw$seeds)
    gear_timer_val <- parse_countdown(timers_raw$gear)
    egg_timer_val <- parse_countdown(timers_raw$egg)
    cosmetics_timer_val <- parse_countdown(timers_raw$cosmetic)
    night_timer_val <- parse_countdown(timers_raw$Event)
    
    if (is.na(seed_timer_val) || is.na(gear_timer_val)) {
      message("   > Seed/Gear timer missing from API, calculating dynamic default.")
      current_time <- Sys.time()
      seconds_into_cycle <- (minute(current_time) %% 5) * 60 + second(current_time)
      dynamic_default <- floor(300 - seconds_into_cycle)
      
      if(is.na(seed_timer_val)) seed_timer_val <- dynamic_default
      if(is.na(gear_timer_val)) gear_timer_val <- dynamic_default
    }
    
    timers_list <- list(
      "Seed Stock" = seed_timer_val,
      "Gear Stock" = gear_timer_val,
      "Egg Stock" = egg_timer_val,
      "Honey Stock" = egg_timer_val,
      "Cosmetics Stock" = cosmetics_timer_val,
      "Night Stock" = night_timer_val
    )
    
    message("   Timer data fetched and processed.")
    timers_list
  }, error = function(e) {
    message("   ! ERROR fetching timer data: ", e$message)
    list()
  })
  
  # --- 3. Fetch Weather Data ---
  current_weather <- tryCatch({
    message("   Fetching weather data...")
    weather_response <- GET(weather_url)
    stop_for_status(weather_response, "fetch weather data")
    weather_data_raw <- content(weather_response, "parsed")
    
    active_weather <- "Clear" 
    if (!is.null(weather_data_raw$weather) && length(weather_data_raw$weather) > 0) {
      active_weather_df <- bind_rows(weather_data_raw$weather) %>% filter(active == TRUE)
      if (nrow(active_weather_df) > 0) {
        active_weather <- active_weather_df$weather_name[1]
      }
    }
    
    message("   Weather data fetched: ", active_weather)
    active_weather
  }, error = function(e) {
    message("   ! ERROR fetching weather data: ", e$message)
    "API Error"
  })
  
  # --- 4. Assemble Final Data Packet ---
  list(
    stocks = all_stocks, timers = all_timers, weather = current_weather,
    timestamp = Sys.time(), error = NULL
  )
}


# FALLBACK FUNCTIONS (kept as a safety measure)
parse_debug_html <- function(file_path = DEBUG_HTML_FILE) {
  message("--- Parsing fallback data from debug_page_content.html ---")
  if (!file.exists(file_path)) return(NULL)
  
  tryCatch({
    page_html <- read_html(file_path)
    
    parse_section <- function(section_header_text) {
      article_node <- page_html %>% 
        html_node(xpath = paste0("//h2[normalize-space()='", section_header_text, "']//ancestor::article"))
      if(is.na(article_node)) return(tibble())
      
      items <- article_node %>% html_nodes("article.group")
      if(length(items) == 0) return(tibble())
      
      map_dfr(items, function(item_node) {
        name <- item_node %>% html_node("h3") %>% html_text(trim=TRUE)
        quantity <- item_node %>% html_node("data") %>% html_attr("value") %>% as.integer()
        if(is.na(name) || is.na(quantity)) return(NULL)
        tibble(name = name, quantity = quantity)
      })
    }
    
    add_images <- function(df) {
      if(is.null(df) || nrow(df) == 0) return(tibble())
      df %>%
        mutate(
          image_url = map_chr(name, ~ (item_encyclopedia[[tolower(.x)]]$image %||% "https://i.imgur.com/8fP6y2d.png"))
        )
    }
    
    fallback_stocks <- list(
      "Seed Stock" = add_images(parse_section("Seeds Stock")), "Gear Stock" = add_images(parse_section("Gear Stock")),
      "Egg Stock" = add_images(parse_section("Egg Stock")), "Cosmetics Stock" = add_images(parse_section("Cosmetics Stock")), 
      "Honey Stock" = add_images(parse_section("Honey Stock")), "Night Stock" = add_images(parse_section("Night Stock"))
    )
    
    message("   Fallback data parsed successfully.")
    
    return(list(
      stocks = fallback_stocks, timers = list(), weather = "N/A (Static)",
      timestamp = file.info(file_path)$mtime, error = "FALLBACK_DATA" 
    ))
  }, error = function(e) {
    message("   ERROR parsing fallback HTML: ", e$message)
    return(NULL) 
  })
}


# --- Encyclopedia Scraping Functions (no changes needed) ---
fetch_ign_gear_data <- function() {
  message("--- Fetching Gear Data from growagardencalculator.net (v5 scraper) ---")
  url <- "https://growagardencalculator.net/wiki/grow-a-garden-gears"
  tryCatch({
    page_html <- read_html(url)
    main_table <- page_html %>% html_node("main table")
    if (is.na(main_table)) stop("Could not find the main gear table.")
    rows <- main_table %>% html_nodes("tbody tr")
    gear_df <- map_dfr(rows, function(row) {
      cells <- row %>% html_nodes("td")
      if (length(cells) != 6) return(NULL)
      tibble(
        image_url   = cells[[1]] %>% html_node("img") %>% html_attr("src"),
        name        = cells[[2]] %>% html_text(trim = TRUE),
        description = cells[[3]] %>% html_text(trim = TRUE),
        cost        = cells[[4]] %>% html_text2(),
        rarity      = cells[[5]] %>% html_text(trim = TRUE),
        obtainable  = cells[[6]] %>% html_text(trim = TRUE)
      )
    })
    if (nrow(gear_df) == 0) stop("Found the gear table, but failed to parse any rows.")
    message("   Gear data scraped successfully for ", nrow(gear_df), " items.")
    return(gear_df)
  }, error = function(e) {
    message("   ERROR fetching new gear data: ", e$message)
    showNotification(paste("Failed to fetch new gear data:", e$message), type = "error", duration = 10)
    return(tibble())
  })
}

fetch_ign_egg_data <- function() {
  message("--- Fetching Egg Data from IGN ---")
  url <- "https://www.ign.com/wikis/grow-a-garden/The_Animal_Update_-_Pet_Egg_Guide"
  tryCatch({
    page <- read_html(url); all_tables <- page %>% html_nodes("table")
    if(length(all_tables) < 1) stop("Page structure has changed.")
    egg_df <- html_table(all_tables[[1]], header = TRUE) %>%
      rename(name = `Egg Type`, cost = Cost, chance_to_appear = `Chances to Appear`, 
             num_pets = `Number of Pet Types`, grow_time = `Time to Grow`) %>%
      filter(!is.na(name) & name != "Egg Type") %>% mutate(type = "Egg")
    message("   Egg data scraped successfully.")
    return(list(eggs = egg_df))
  }, error = function(e) {
    message("   ERROR fetching IGN egg data: ", e$message)
    showNotification(paste("Failed to fetch IGN egg data from", url), type = "error", duration = 10)
    return(list(eggs = tibble()))
  })
}

fetch_hatchable_pets_from_wiki <- function() {
  message("--- Fetching Hatchable Pets Data from new Wiki source ---")
  url <- "https://growagardencalculator.net/wiki/grow-a-garden-pets"
  tryCatch({
    page_html <- read_html(url)
    main_pet_table <- page_html %>% html_node("h2:contains('All Pets in Grow a Garden') ~ table")
    if (is.na(main_pet_table)) stop("Could not find the main pet table.")
    rows <- main_pet_table %>% html_nodes("tbody tr")
    all_pets_df <- map_dfr(rows, function(row) {
      cells <- row %>% html_nodes("td")
      if (length(cells) != 5) return(NULL)
      tibble(
        image_url   = cells[[1]] %>% html_node("img") %>% html_attr("src"),
        name        = cells[[2]] %>% html_text(trim = TRUE),
        description = cells[[3]] %>% html_text(trim = TRUE),
        rarity      = cells[[4]] %>% html_text(trim = TRUE),
        obtainable  = cells[[5]] %>% html_text(trim = TRUE)
      )
    })
    if (nrow(all_pets_df) == 0) stop("Found the pet table, but failed to parse any rows.")
    base_url <- "https://growagardencalculator.net"
    all_pets_df <- all_pets_df %>%
      mutate(image_url = if_else(str_starts(image_url, "/"), paste0(base_url, image_url), image_url))
    message("   Hatchable pets scraped successfully for ", nrow(all_pets_df), " pets.")
    return(all_pets_df)
  }, error = function(e) {
    message("   ERROR fetching new hatchable pets data: ", e$message)
    showNotification(paste("Failed to fetch pet data:", e$message), type = "error", duration = 10)
    return(tibble())
  })
}

fetch_detailed_mutations_data <- function() {
  message("--- Fetching DETAILED Mutation Data from growagardencalculator.net ---")
  url <- "https://growagardencalculator.net/wiki/grow-a-garden-mutations"
  tryCatch({
    page <- read_html(url)
    table_node <- page %>% html_node("table")
    if (is.null(table_node)) stop("Could not find the mutation table.")
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
  dashboardHeader(title = "Grow a Garden"),
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
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
      tags$link(rel = "stylesheet", type = "text/css", href = paste0("styles.css?v=", as.numeric(Sys.time())))
    ),
    tags$div(id = "loading-overlay", style = "display: none;", div(class="loader")),
    tabItems(
      tabItem(tabName="dashboard",
              h2(class="animated-gradient-text", "Live Dashboard"),
              p("Real-time stock, timer, and weather information from the game."),
              fluidRow(
                column(8,
                       fluidRow(
                         # --- WRAPPED: ValueBoxes are now wrapped in clickable divs ---
                         div(id = "clickable_seed_timer", style="cursor:pointer;", valueBoxOutput("seed_timer_box", width=4)), 
                         div(id = "clickable_gear_timer", style="cursor:pointer;", valueBoxOutput("gear_timer_box", width=4)), 
                         div(id = "clickable_egg_timer", style="cursor:pointer;", valueBoxOutput("egg_timer_box", width=4))
                       ),
                       fluidRow(
                         div(id = "clickable_honey_timer", style="cursor:pointer;", valueBoxOutput("honey_timer_box", width=4)), 
                         div(id = "clickable_cosmetics_timer", style="cursor:pointer;", valueBoxOutput("cosmetics_timer_box", width=4)), 
                         div(id = "clickable_night_timer", style="cursor:pointer;", valueBoxOutput("night_timer_box", width=4))
                       )
                ),
                column(4,
                       valueBoxOutput("weather_box", width=12),
                       box(title = "Control Panel", status = "primary", solidHeader = TRUE, width = 12, align = "center",
                           collapsible = TRUE, collapsed = TRUE,
                           actionButton("refresh_button", "Refresh Now", icon = icon("refresh"), class = "btn-lg btn-primary"), br(),br(),
                           p(textOutput("last_updated_text"))
                       )
                )
              ),
              # --- WRAPPED: Stock boxes are now wrapped in divs with anchor IDs ---
              fluidRow(
                div(id = "seed_stock_anchor", box(title="Seed Stock", status="info", solidHeader=TRUE, width=6, uiOutput("seed_stock_ui"))),
                div(id = "gear_stock_anchor", box(title="Gear Stock", status="info", solidHeader=TRUE, width=6, uiOutput("gear_stock_ui")))
              ),
              fluidRow(
                div(id = "egg_stock_anchor", box(title="Egg Stock", status="warning", solidHeader=TRUE, width=6, uiOutput("egg_stock_ui"))),
                div(id = "cosmetics_stock_anchor", box(title="Cosmetics Stock", status="warning", solidHeader=TRUE, width=6, uiOutput("cosmetics_stock_ui")))
              ),
              fluidRow(
                div(id = "honey_stock_anchor", box(title="Honey Stock", status="success", solidHeader=TRUE, width=6, uiOutput("honey_stock_ui"))),
                div(id = "night_stock_anchor", box(title="Night Stock", status="primary", solidHeader=TRUE, width=6, uiOutput("night_stock_ui")))
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
  scraped_data <- reactiveVal()
  timers_rv <- reactiveVal(list())
  fruit_data_rv <- reactiveVal()
  mid_cycle_refresh_done <- reactiveVal(FALSE)
  
  fetch_and_save_fruit_data <- function() {
    shinyjs::show("loading-overlay")
    showNotification("Fetching latest fruit data...", type = "message", duration = 5)
    new_data <- fetch_master_seed_list()
    
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
    SharedState$status <- "fetching"
    shinyjs::show("loading-overlay")
    message("fetch_data() triggered. Status: fetching")
    
    data <- fetch_public_api_data()
    
    if (length(data$stocks) == 0 || all(sapply(data$stocks, nrow) == 0)) {
      message("Live stock API failed or returned empty. Attempting to use fallback data.")
      showNotification("Live API is unresponsive. Using static fallback data.", type="warning", duration=8)
      fallback_data <- parse_debug_html()
      
      if(!is.null(fallback_data)) {
        data$stocks <- fallback_data$stocks
        if(length(data$timers) == 0) data$timers <- fallback_data$timers
        if(data$weather == "API Error") data$weather <- fallback_data$weather
        data$error <- "FALLBACK_DATA"
      }
    }
    
    scraped_data(data)
    timers_rv(data$timers)
    
    if (is.null(data$error) && length(data$stocks) > 0) {
      history_df_list <- list()
      for (category_name in names(data$stocks)) {
        df <- data$stocks[[category_name]]
        
        if (is.data.frame(df) && nrow(df) > 0 && all(c("name", "quantity", "image_url") %in% names(df))) {
          history_df_list[[category_name]] <- df %>%
            select(item_name = name, quantity, image_url) %>%
            mutate(category = category_name)
        }
      }
      
      if (length(history_df_list) > 0) {
        history_df <- bind_rows(history_df_list) %>%
          mutate(
            timestamp = data$timestamp,
            quantity = as.character(quantity)
          ) %>%
          select(timestamp, category, item_name, quantity, image_url)
        
        if(nrow(history_df) > 0) {
          write.table(history_df, HISTORY_FILE, sep=",", row.names=F, append=file.exists(HISTORY_FILE), col.names=!file.exists(HISTORY_FILE))
        }
      }
    }
    
    SharedState$status <- "idle"
    shinyjs::hide("loading-overlay")
    message("Fetch complete. Status: idle")
  }
  
  fetch_data()
  
  observe({
    req(SharedState$status == "idle")
    if (!is.null(isolate(scraped_data()$error))) return()
    
    invalidateLater(1000, session)
    
    current_timers <- isolate(timers_rv())
    if (length(current_timers) == 0) return()
    
    was_running <- unlist(current_timers) > 0
    
    decremented_timers <- current_timers
    for (name in names(decremented_timers)) {
      if (is.numeric(decremented_timers[[name]]) && !is.na(decremented_timers[[name]]) && decremented_timers[[name]] > 0) {
        decremented_timers[[name]] <- decremented_timers[[name]] - 1
      }
    }
    timers_rv(decremented_timers)
    
    is_finished <- unlist(decremented_timers) <= 0
    
    if (any(was_running & is_finished, na.rm = TRUE)) {
      message("A timer hit zero. Triggering main auto-refresh.")
      showNotification("A timer finished! Refreshing...", type = "warning", duration = 5)
      mid_cycle_refresh_done(FALSE)
      fetch_data()
      
      shinyjs::delay(1500, {
        message("Performing scheduled double-tap refresh to ensure data is current.")
        fetch_data()
      })
    }
    
    seed_timer_val <- decremented_timers[["Seed Stock"]]
    if (!is.null(seed_timer_val) && seed_timer_val == 260 && !isolate(mid_cycle_refresh_done())) {
      message("Seed/Gear timer hit 4:20. Triggering mid-cycle refresh to catch updates.")
      showNotification("Refreshing to catch stock updates...", type = "message", duration = 4)
      fetch_data()
      mid_cycle_refresh_done(TRUE) 
    }
  })
  
  observeEvent(input$refresh_button, { message("Manual refresh triggered."); fetch_data() })
  
  observe({
    invalidateLater(300000, session) 
    if (isolate(SharedState$status) == "idle") {
      message("Performing scheduled 5-minute refresh."); showNotification("Performing scheduled 5-minute refresh...", type="message", duration=4)
      fetch_data()
    }
  })
  
  # --- NEW: Observer to handle scrolling when timers are clicked ---
  observe({
    shinyjs::onclick("clickable_seed_timer", shinyjs::runjs("document.getElementById('seed_stock_anchor').scrollIntoView({ behavior: 'smooth' });"))
    shinyjs::onclick("clickable_gear_timer", shinyjs::runjs("document.getElementById('gear_stock_anchor').scrollIntoView({ behavior: 'smooth' });"))
    shinyjs::onclick("clickable_egg_timer", shinyjs::runjs("document.getElementById('egg_stock_anchor').scrollIntoView({ behavior: 'smooth' });"))
    shinyjs::onclick("clickable_honey_timer", shinyjs::runjs("document.getElementById('honey_stock_anchor').scrollIntoView({ behavior: 'smooth' });"))
    shinyjs::onclick("clickable_cosmetics_timer", shinyjs::runjs("document.getElementById('cosmetics_stock_anchor').scrollIntoView({ behavior: 'smooth' });"))
    shinyjs::onclick("clickable_night_timer", shinyjs::runjs("document.getElementById('night_stock_anchor').scrollIntoView({ behavior: 'smooth' });"))
  })
  
  output$last_updated_text <- renderText({
    data <- scraped_data(); req(data)
    if (!is.null(data$error)) {
      if (data$error == "FALLBACK_DATA") {
        return(paste("Displaying static data from:", format(data$timestamp, "%b %d, %Y")))
      } else {
        return(paste("Last attempt failed:", format(data$timestamp, "%I:%M:%S %p")))
      }
    }
    paste("Last updated:", format(data$timestamp, "%I:%M:%S %p"))
  })
  
  create_timer_output <- function(timer_name, icon_name, color) {
    renderValueBox({
      req(scraped_data())
      seconds <- timers_rv()[[timer_name]]
      
      if (is.null(seconds) || is.na(seconds)) {
        return(valueBox("ERROR", timer_name, icon=icon("exclamation-triangle"), color="red"))
      }
      
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
    if (identical(data$weather, "API Error")) {
      return(valueBox("API OFFLINE", "Current Weather", icon=icon("exclamation-triangle"), color="red"))
    }
    valueBox(data$weather, "Current Weather", icon=icon("cloud-sun-rain"), color="teal")
  })
  
  show_item_modal <- function(item_name) {
    info <- item_encyclopedia[[tolower(item_name)]]
    live_quantity <- "Not in stock"
    if(!is.null(scraped_data()$stocks)) {
      for (category in scraped_data()$stocks) { 
        if (item_name %in% category$name) { 
          live_quantity <- paste("x", category$quantity[category$name == item_name][1]); break 
        } 
      }
    }
    
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
  
  observe({
    req(length(item_encyclopedia) > 0)
    proper_names <- sapply(item_encyclopedia, `[[`, "name")
    
    lapply(seq_along(proper_names), function(i) {
      item_name_proper <- proper_names[i]
      button_id <- paste0("show_details_", gsub("[^A-Za-z0-9]", "", item_name_proper))
      observeEvent(input[[button_id]], { 
        show_item_modal(item_name_proper) 
      })
    })
  })
  
  create_item_grid <- function(category_name) {
    renderUI({
      data <- scraped_data(); req(data); 
      
      df <- data$stocks[[category_name]]; 
      if (is.null(df) || nrow(df) == 0) {
        msg <- if (!is.null(data$error) && data$error == "FALLBACK_DATA") {
          "No data for this category in the fallback file."
        } else {
          "No items in this category right now."
        }
        return(p(msg, style="text-align:center; color: var(--text-secondary);"))
      }
      
      item_html <- lapply(1:nrow(df), function(i) {
        item_name <- df$name[i]
        button_id <- paste0("show_details_",gsub("[^A-Za-z0-9]","",item_name))
        
        image_url <- df$image_url[i]
        if (is.na(image_url) || image_url == "") {
          image_url <- "https://i.imgur.com/8fP6y2d.png"
        }
        
        label_div <- tags$div(class="stock-item", 
                              tags$img(src=image_url, alt=item_name), 
                              tags$p(item_name), 
                              tags$p(class="quantity", paste("x", df$quantity[i]))
        )
        
        if (tolower(item_name) %in% names(item_encyclopedia)) {
          actionLink(inputId=button_id, label=label_div, class="stock-item-link")
        } else {
          label_div
        }
      })
      tags$div(class="stock-grid-container", item_html)
    })
  }
  
  output$seed_stock_ui <- create_item_grid("Seed Stock"); output$gear_stock_ui <- create_item_grid("Gear Stock");
  output$egg_stock_ui <- create_item_grid("Egg Stock"); output$cosmetics_stock_ui <- create_item_grid("Cosmetics Stock");
  output$honey_stock_ui <- create_item_grid("Honey Stock"); output$night_stock_ui <- create_item_grid("Night Stock")
  
  
  # --- Data Provisioning for Modules ---
  
  history_data <- eventReactive(scraped_data(), {
    if (!file.exists(HISTORY_FILE)) return(data.frame()) 
    read.csv(HISTORY_FILE) %>% 
      mutate(timestamp_val = ymd_hms(timestamp, tz=Sys.timezone())) %>%
      arrange(desc(timestamp_val)) %>% 
      mutate(timestamp = format(timestamp_val, "%Y-%m-%d %I:%M:%S %p")) %>% 
      select(-timestamp_val)
  }, ignoreNULL = FALSE)
  
  encyclopedia_data <- reactiveVal({
    message("--- Aggregating all encyclopedia data at startup (politely)... ---")
    
    seeds_data <- fetch_master_seed_list()
    Sys.sleep(1.5)
    
    gear_data <- fetch_ign_gear_data()
    Sys.sleep(1.5)
    
    hatchable_pets_data <- fetch_hatchable_pets_from_wiki()
    Sys.sleep(1.5)
    
    mutations_data <- fetch_detailed_mutations_data()
    Sys.sleep(1.5)
    
    ign_data <- fetch_ign_egg_data() 
    
    list(
      seeds = seeds_data,
      gear = gear_data,
      eggs = ign_data$eggs,
      pets = hatchable_pets_data,
      mutations = mutations_data
    )
  })
  
  fruit_value_list_rv <- reactive({
    df <- fruit_data_rv(); req(df, nrow(df) > 0)
    df_calc <- df %>% mutate(sell_value_numeric = as.numeric(sell_value)) %>% filter(!is.na(sell_value_numeric))
    setNames(as.list(df_calc$sell_value_numeric), df_calc$name)
  })
  
  # --- Module Server Initializations ---
  
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