# --- START FILE: encyclopedia_module.R ---

# =============================================================
# UI function for the Encyclopedia Module
# =============================================================
encyclopedia_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "encyclopedia",
          tags$head(
            tags$style(HTML("
              /* --- Card Styles (for Gear, Eggs, etc.) --- */
              .encyclopedia-card {
                background-color: #ffffff; border: 1px solid #eaeaea; border-radius: 8px;
                padding: 15px; margin-bottom: 20px; box-shadow: 0 4px 8px rgba(0,0,0,0.08);
                transition: all 0.3s ease-in-out; display: flex; flex-direction: column;
                align-items: center; text-align: center; height: 100%;
              }
              .encyclopedia-card:hover { transform: translateY(-5px); box-shadow: 0 8px 16px rgba(0,0,0,0.12); }
              .encyclopedia-card h4 {
                color: #8E44AD; margin-top: 0; margin-bottom: 15px; border-bottom: 2px solid #f4f4f4;
                padding-bottom: 10px; width: 100%; font-weight: 600;
              }
              .encyclopedia-card p { margin: 5px 0; width: 100%; text-align: left; }
              .card-details { font-size: 0.9em; color: #555; }
              .card-details strong { color: #333; }
              .encyclopedia-section-header {
                margin-top: 20px; margin-bottom: 10px; border-left: 4px solid #8E44AD; padding-left: 15px;
              }

              /* --- Table Styles --- */
              .wiki-table {
                width: 100%; border-collapse: collapse; margin-bottom: 25px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
              }
              .wiki-table th, .wiki-table td { border: 1px solid #ddd; padding: 10px 12px; text-align: left; }
              .wiki-table thead th { color: white; font-weight: bold; font-size: 1.1em; text-align: center; }
              .wiki-table .rarity-header-Common { background-color: #A9A9A9; }
              .wiki-table .rarity-header-Uncommon { background-color: #27AE60; }
              .wiki-table .rarity-header-Rare { background-color: #2980B9; }
              .wiki-table .rarity-header-Legendary { background-color: #F1C40F; color: #333; }
              .wiki-table .rarity-header-Mythical { background-color: #E67E22; }
              .wiki-table .rarity-header-Divine { background-color: #C0392B; }
              .wiki-table .rarity-header-Prismatic { background: linear-gradient(45deg, #f3ec78, #af4261); }
              .wiki-table .rarity-header-Unavailable { background-color: #333333; color: white; border: 1px solid #555; } /* NEW STYLE */
              .wiki-table tbody tr:nth-child(even) { background-color: #f9f9f9; }
            "))
          ),
          
          h2("Grow a Garden Encyclopedia of Knowledge"),
          p("An exhaustive collection of all items, creatures, and phenomena in the game, sourced from the IGN Grow a Garden Wiki."),
          
          tabBox(
            id = ns("encyclopedia_tabs"),
            width = 12,
            title = "Categories",
            
            tabPanel("Seeds & Crops", icon = icon("seedling"),
                     div(class="encyclopedia-section-header", h3("Available Seeds & Crops in Store")),
                     p("This section shows seeds that are available for purchase with Sheckles in the main Seed Shop."),
                     uiOutput(ns("store_seed_tables_ui")),
                     hr(),
                     div(class="encyclopedia-section-header", h3("List of All Seed Packs")),
                     p("This is a reference list of seeds obtainable from Seed Packs and special events."),
                     uiOutput(ns("pack_seed_tables_ui"))
            ),
            # --- UI CHANGE: Gear tab now uses a simple uiOutput for the new tables ---
            tabPanel("Gear", icon = icon("cogs"),
                     uiOutput(ns("gear_tables_ui"))
            ),
            tabPanel("Eggs & Animals", icon = icon("egg"),
                     div(class="encyclopedia-section-header", h3("Pet Eggs")),
                     p("Eggs that can be purchased or found to hatch into helpful pets."),
                     uiOutput(ns("egg_cards_ui")),
                     hr(),
                     div(class="encyclopedia-section-header", h3("Hatchable Pets")),
                     p("Pets that can be hatched from the eggs listed above, providing unique bonuses."),
                     uiOutput(ns("pet_cards_ui"))
            ),
            tabPanel("Weather & Mutations", icon = icon("cloud-sun-rain"),
                     div(class="encyclopedia-section-header", h3("Weather Events")),
                     p("Weather events that can occur, affecting crop growth and mutation chances."),
                     DTOutput(ns("weather_table")),
                     hr(),
                     div(class="encyclopedia-section-header", h3("Mutations")),
                     p("All possible mutations and their effects on your crops."),
                     DTOutput(ns("mutations_table"))
            )
          )
  )
}


# =============================================================
# Server function for the Encyclopedia Module
# =============================================================
encyclopedia_server <- function(id, all_encyclopedia_data) {
  moduleServer(id, function(input, output, session) {
    
    # --- SEED & CROP LOGIC (Two-Table Layout) ---
    
    # 1. UI for Available Store Seeds
    output$store_seed_tables_ui <- renderUI({
      df <- all_encyclopedia_data()$seeds
      req(df)
      df_store <- df %>% filter(!is.na(cost_numeric))
      if (nrow(df_store) == 0) return(p("No store-based seeds found.", style = "text-align:center; color: #999;"))
      
      rarity_levels <- c("Common", "Uncommon", "Rare", "Legendary", "Mythical", "Divine", "Prismatic")
      df_store$rarity <- factor(df_store$rarity, levels = intersect(rarity_levels, unique(df_store$rarity)))
      
      grouped_df <- df_store %>% arrange(rarity, cost_numeric) %>% group_by(rarity) %>% summarise(data = list(cur_data()), .groups = 'drop')
      
      tables_list <- lapply(1:nrow(grouped_df), function(i) {
        current_rarity <- grouped_df$rarity[[i]]; current_data <- grouped_df$data[[i]]
        header_class <- paste0("rarity-header-", current_rarity)
        table_rows <- lapply(1:nrow(current_data), function(j) {
          tags$tr(
            tags$td(current_data$name[j]),
            tags$td(paste(format(current_data$cost_numeric[j], big.mark = ","), "Sheckles")),
            tags$td(current_data$harvest_type[j])
          )
        })
        tags$table(class = "wiki-table",
                   tags$thead(
                     tags$tr(tags$th(class = header_class, colspan = "3", paste("All", current_rarity, "Seeds"))),
                     tags$tr(tags$th("Name of Seed"), tags$th("Cost"), tags$th("Amount of Harvests"))
                   ),
                   tags$tbody(table_rows)
        )
      })
      tagList(tables_list)
    })
    
    # 2. UI for Seed Pack Table
    output$pack_seed_tables_ui <- renderUI({
      df <- all_encyclopedia_data()$seeds
      req(df)
      
      # SPLIT the data into normal and unavailable pack seeds
      df_packs_normal <- df %>% filter(is.na(cost_numeric) & rarity != "N/A")
      df_packs_unavailable <- df %>% filter(is.na(cost_numeric) & rarity == "N/A")
      
      normal_tables_list <- NULL
      if (nrow(df_packs_normal) > 0) {
        rarity_levels <- c("Common", "Uncommon", "Rare", "Legendary", "Mythical", "Divine", "Prismatic")
        df_packs_normal$rarity <- factor(df_packs_normal$rarity, levels = intersect(rarity_levels, unique(df_packs_normal$rarity)))
        grouped_df <- df_packs_normal %>% arrange(rarity) %>% group_by(rarity) %>% summarise(data = list(cur_data()), .groups = 'drop')
        
        normal_tables_list <- lapply(1:nrow(grouped_df), function(i) {
          current_rarity <- grouped_df$rarity[[i]]; current_data <- grouped_df$data[[i]]
          header_class <- paste0("rarity-header-", current_rarity)
          table_rows <- lapply(1:nrow(current_data), function(j) {
            tags$tr(
              tags$td(current_data$name[j]),
              tags$td(current_data$harvest_type[j]),
              tags$td(current_data$source[j])
            )
          })
          tags$table(class = "wiki-table",
                     tags$thead(
                       tags$tr(tags$th(class = header_class, colspan = "3", paste("All", current_rarity, "Seeds"))),
                       tags$tr(tags$th("Name of Seed"), tags$th("Amount of Harvests"), tags$th("Source"))
                     ),
                     tags$tbody(table_rows)
          )
        })
      }
      
      unavailable_table <- NULL
      if (nrow(df_packs_unavailable) > 0) {
        table_rows <- lapply(1:nrow(df_packs_unavailable), function(j) {
          tags$tr(
            tags$td(df_packs_unavailable$name[j]),
            tags$td(df_packs_unavailable$harvest_type[j]),
            tags$td(df_packs_unavailable$source[j])
          )
        })
        unavailable_table <- tags$table(class = "wiki-table",
                                        tags$thead(
                                          tags$tr(tags$th(class = "rarity-header-Unavailable", colspan = "3", "All Unavailable Seeds")),
                                          tags$tr(tags$th("Name of Seed"), tags$th("Amount of Harvests"), tags$th("Source"))
                                        ),
                                        tags$tbody(table_rows)
        )
      }
      
      if (is.null(normal_tables_list) && is.null(unavailable_table)) {
        return(p("No pack-based seeds found.", style = "text-align:center; color: #999;"))
      }
      
      tagList(normal_tables_list, unavailable_table)
    })
    
    # --- SERVER CHANGE: Replaced gear card logic with gear table logic ---
    output$gear_tables_ui <- renderUI({
      df <- all_encyclopedia_data()$gear
      req(df, nrow(df) > 0)
      
      # Define rarity order for correct sorting
      rarity_levels <- c("Common", "Uncommon", "Rare", "Legendary", "Mythical", "Divine", "Prismatic")
      df$rarity <- factor(df$rarity, levels = intersect(rarity_levels, unique(df$rarity)))
      
      # Group data by rarity
      grouped_df <- df %>% 
        arrange(rarity, cost_numeric) %>% 
        group_by(rarity) %>% 
        summarise(data = list(cur_data()), .groups = 'drop')
      
      # Loop through each rarity group and create a table
      tables_list <- lapply(1:nrow(grouped_df), function(i) {
        current_rarity <- grouped_df$rarity[[i]]
        current_data <- grouped_df$data[[i]]
        header_class <- paste0("rarity-header-", current_rarity)
        
        # Create the rows for the current table
        table_rows <- lapply(1:nrow(current_data), function(j) {
          tags$tr(
            tags$td(current_data$name[j]),
            tags$td(
              if (!is.na(current_data$cost_numeric[j])) {
                paste(format(current_data$cost_numeric[j], big.mark = ","), "Sheckles")
              } else {
                "N/A"
              }
            ),
            tags$td(current_data$description[j]),
            tags$td(current_data$uses[j])
          )
        })
        
        # Assemble the full table
        tags$table(class = "wiki-table",
                   tags$thead(
                     tags$tr(tags$th(class = header_class, colspan = "4", current_rarity)),
                     tags$tr(
                       tags$th("Tool"), 
                       tags$th("Cost"), 
                       tags$th("Use"), 
                       tags$th("Number of Uses")
                     )
                   ),
                   tags$tbody(table_rows)
        )
      })
      
      tagList(tables_list)
    })
    
    # --- EGG & ANIMAL LOGIC ---
    output$egg_cards_ui <- renderUI({
      df <- all_encyclopedia_data()$eggs_and_pets$eggs
      req(df, nrow(df) > 0)
      
      # Note: This UI expects columns that may not be in the scraped data.
      # We use `%||% "N/A"` to prevent errors if a column is missing.
      cards <- lapply(1:nrow(df), function(i) {
        tags$div(class = "col-md-3",
                 tags$div(class = "encyclopedia-card",
                          tags$h4(df$name[i]),
                          tags$p(class = "card-details", strong("Cost: "), df$cost[i] %||% "N/A"),
                          tags$p(class = "card-details", strong("Appear Chance: "), df$chance_to_appear[i] %||% "N/A"),
                          tags$p(class = "card-details", strong("Grow Time: "), df$grow_time[i] %||% "N/A")
                 )
        )
      })
      fluidRow(cards)
    })
    
    output$pet_cards_ui <- renderUI({
      df <- all_encyclopedia_data()$eggs_and_pets$pets
      req(df, nrow(df) > 0)
      
      cards <- lapply(1:nrow(df), function(i) {
        tags$div(class = "col-md-4",
                 tags$div(class = "encyclopedia-card",
                          tags$h4(df$name[i]),
                          tags$p(class = "card-details", strong("Rarity: "), df$rarity[i] %||% "N/A"),
                          tags$p(class = "card-details", style = "margin-top: 10px;", 
                                 strong("Bonus: "), df$bonus[i] %||% "N/A")
                 )
        )
      })
      fluidRow(cards)
    })
    
    # --- WEATHER & MUTATION LOGIC ---
    output$weather_table <- renderDT({
      df <- all_encyclopedia_data()$weather_and_mutations$weather
      req(df)
      datatable(df, options = list(dom = 't', paging = FALSE, ordering = FALSE), rownames = FALSE,
                colnames = c("Weather Event", "Effect"))
    })
    
    output$mutations_table <- renderDT({
      df <- all_encyclopedia_data()$weather_and_mutations$mutations
      req(df)
      datatable(df, options = list(pageLength = 10, dom = 'ftp'), rownames = FALSE,
                colnames = c("Mutation Name", "Bonus", "Effect"))
    })
    
  })
}

# --- END FILE: encyclopedia_module.R ---