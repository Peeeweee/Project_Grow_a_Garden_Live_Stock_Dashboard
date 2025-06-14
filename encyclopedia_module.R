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
              
              .wiki-table thead th { color: black; font-weight: bold; font-size: 1.1em; text-align: center; }
              
              .wiki-table .rarity-header-Common { background-color: #A9A9A9; color: white !important; }
              .wiki-table .rarity-header-Uncommon { background-color: #27AE60; color: white !important; }
              .wiki-table .rarity-header-Rare { background-color: #2980B9; color: white !important; }
              .wiki-table .rarity-header-Legendary { background-color: #F1C40F; color: #333 !important; }
              .wiki-table .rarity-header-Mythical { background-color: #E67E22; color: white !important; }
              .wiki-table .rarity-header-Divine { background-color: #C0392B; color: white !important; }
              .wiki-table .rarity-header-Prismatic { background: linear-gradient(45deg, #f3ec78, #af4261); color: white !important; }
              .wiki-table .rarity-header-Unavailable { background-color: #333333; color: white !important; border: 1px solid #555; }
              
              .wiki-table tbody tr:nth-child(even) { background-color: #f9f9f9; }
            "))
          ),
          
          h2("Grow a Garden Encyclopedia of Knowledge"),
          p("An exhaustive collection of all items, creatures, and phenomena in the game."),
          
          tabBox(
            id = ns("encyclopedia_tabs"),
            width = 12,
            title = "Categories",
            
            tabPanel("Seeds & Crops", icon = icon("seedling"),
                     div(class="encyclopedia-section-header", h3("Master Seed & Crop List")),
                     p("A comprehensive list of all seeds and crops. Sourced from growagardencalculator.net."),
                     DTOutput(ns("seeds_table_detailed"))
            ),
            tabPanel("Gear", icon = icon("cogs"),
                     uiOutput(ns("gear_tables_ui"))
            ),
            tabPanel("Eggs & Animals", icon = icon("egg"),
                     div(class="encyclopedia-section-header", h3("Pet Eggs")),
                     p("Eggs that can be purchased or found to hatch into helpful pets."),
                     uiOutput(ns("egg_table_ui")),
                     hr(),
                     div(class="encyclopedia-section-header", h3("Hatchable Pets")),
                     p("Pets that can be hatched from the eggs listed above, providing unique bonuses."),
                     uiOutput(ns("pet_tables_ui"))
            ),
            tabPanel("Weather & Mutations", icon = icon("dna"),
                     div(class="encyclopedia-section-header", h3("Mutation List")),
                     p("A comprehensive list of all mutations, their effects, and bonuses. Sourced from growagardencalculator.net."),
                     DTOutput(ns("mutations_table_detailed"))
            )
          )
  )
}


# =============================================================
# Server function for the Encyclopedia Module
# =============================================================
encyclopedia_server <- function(id, all_encyclopedia_data) {
  moduleServer(id, function(input, output, session) {
    
    # --- Seeds & Crops Table ---
    output$seeds_table_detailed <- renderDT({
      df <- all_encyclopedia_data()$seeds
      req(df)
      
      df_display <- df %>%
        mutate(
          Image = paste0('<img src="', image_url, '" height="50" alt="', name, '">'),
          sheckle_price_num = suppressWarnings(as.numeric(gsub(",", "", sheckle_price))),
          min_value_num = suppressWarnings(as.numeric(gsub(",", "", min_value))),
          robux_price_num = suppressWarnings(as.numeric(gsub(",", "", robux_price)))
        ) %>%
        select(
          Image,
          `Crop Name` = name, `Sheckle Price` = sheckle_price, `Min Value` = min_value,
          `Robux Price` = robux_price, Stock = stock, Tier = tier,
          `Multi Harvest` = multi_harvest, Obtainable = obtainable,
          sheckle_price_num, min_value_num, robux_price_num
        )
      
      datatable(
        df_display,
        escape = FALSE,
        # *** MODIFICATION START: Disable pagination and add vertical scroll ***
        options = list(
          paging = FALSE,       # Disable pagination to show all rows
          dom = 'ft',           # Show only filter and table
          scrollY = "600px",    # Add vertical scroll
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = c(0, 5, 6, 7, 8)),
            list(visible = FALSE, targets = c("sheckle_price_num", "min_value_num", "robux_price_num"))
          )
        ),
        # *** MODIFICATION END ***
        rownames = FALSE
      ) %>%
        formatStyle(
          'Tier',
          backgroundColor = styleEqual(
            c("Common", "Uncommon", "Rare", "Legendary", "Mythical", "Divine", "Prismatic"),
            c('#D3D3D3', '#A9DFBF', '#AED6F1', '#F9E79F', '#F5CBA7', '#EDBB99', '#D2B4DE')
          )
        ) %>%
        formatStyle(
          c('Multi Harvest', 'Obtainable'),
          color = styleEqual(c("Yes", "No"), c('green', 'red')),
          fontWeight = styleEqual(c("Yes", "No"), c('bold', 'normal'))
        )
    })
    
    # --- Gear table logic (No changes needed, already shows all) ---
    output$gear_tables_ui <- renderUI({
      df <- all_encyclopedia_data()$gear
      req(df, nrow(df) > 0)
      
      rarity_levels <- c("Common", "Uncommon", "Rare", "Legendary", "Mythical", "Divine", "Prismatic")
      df$rarity <- factor(df$rarity, levels = intersect(rarity_levels, unique(df$rarity)))
      
      grouped_df <- df %>% 
        arrange(rarity, cost_numeric) %>% 
        group_by(rarity) %>% 
        summarise(data = list(cur_data()), .groups = 'drop')
      
      tables_list <- lapply(1:nrow(grouped_df), function(i) {
        current_rarity <- grouped_df$rarity[[i]]
        current_data <- grouped_df$data[[i]]
        header_class <- paste0("rarity-header-", current_rarity)
        
        table_rows <- lapply(1:nrow(current_data), function(j) {
          tags$tr(
            tags$td(current_data$name[j]),
            tags$td(
              if (!is.na(current_data$cost_numeric[j])) {
                paste(format(current_data$cost_numeric[j], big.mark = ","), "Sheckles")
              } else { "N/A" }
            ),
            tags$td(current_data$description[j]),
            tags$td(current_data$uses[j])
          )
        })
        
        tags$table(class = "wiki-table",
                   tags$thead(
                     tags$tr(tags$th(class = header_class, colspan = "4", current_rarity)),
                     tags$tr(
                       tags$th("Tool"), tags$th("Cost"), 
                       tags$th("Use"), tags$th("Number of Uses")
                     )
                   ),
                   tags$tbody(table_rows)
        )
      })
      
      tagList(tables_list)
    })
    
    # --- EGG & ANIMAL LOGIC (No changes needed, already shows all) ---
    output$egg_table_ui <- renderUI({
      df <- all_encyclopedia_data()$eggs_and_pets$eggs
      req(df, nrow(df) > 0)
      tags$table(class = "wiki-table",
                 tags$thead(tags$tr(
                   tags$th("Egg Type"), tags$th("Cost"), tags$th("Chances to Appear"),
                   tags$th("Number of Pet Types"), tags$th("Time to Grow")
                 )),
                 tags$tbody(lapply(1:nrow(df), function(i) {
                   tags$tr(
                     tags$td(df$name[i]), tags$td(HTML(df$cost[i])), tags$td(df$chance_to_appear[i]),
                     tags$td(df$num_pets[i]), tags$td(df$grow_time[i])
                   )
                 }))
      )
    })
    
    output$pet_tables_ui <- renderUI({
      df <- all_encyclopedia_data()$eggs_and_pets$pets
      req(df, nrow(df) > 0)
      
      rarity_levels <- c("Common", "Uncommon", "Rare", "Legendary", "Mythical", "Divine", "Prismatic")
      df$rarity <- factor(df$rarity, levels = intersect(rarity_levels, unique(df$rarity)))
      
      grouped_df <- df %>%
        filter(!is.na(rarity)) %>% arrange(rarity) %>%
        group_by(rarity) %>% summarise(data = list(cur_data()), .groups = 'drop')
      
      tables_list <- lapply(1:nrow(grouped_df), function(i) {
        current_rarity <- as.character(grouped_df$rarity[[i]])
        current_data <- grouped_df$data[[i]]
        header_class <- paste0("rarity-header-", current_rarity)
        
        table_rows <- lapply(1:nrow(current_data), function(j) {
          chance_value <- current_data$chance_of_appearing[j]
          display_chance <- if (is.na(chance_value) || chance_value == "") "Not Listed" else chance_value
          tags$tr(
            tags$td(current_data$name[j]),
            tags$td(current_data$bonus[j]),
            tags$td(display_chance)
          )
        })
        
        tags$table(class = "wiki-table",
                   tags$thead(
                     tags$tr(tags$th(class = header_class, colspan = "3", paste("All", current_rarity, "Pets"))),
                     tags$tr(tags$th("Pet Name"), tags$th("Bonus"), tags$th("Chance of Appearing"))
                   ),
                   tags$tbody(table_rows)
        )
      })
      
      tagList(tables_list)
    })
    
    # --- Mutations Table ---
    output$mutations_table_detailed <- renderDT({
      df <- all_encyclopedia_data()$mutations
      req(df)
      
      df_display <- df %>%
        mutate(
          Icon = paste0('<img src="', icon, '" height="40" alt="', name, '">')
        ) %>%
        select(
          `Mutation Name` = name, Icon, Multiplier = multiplier,
          `Stack Bonus` = stack_bonus, Description = description
        )
      
      datatable(
        df_display,
        escape = FALSE, 
        # *** MODIFICATION START: Disable pagination and add vertical scroll ***
        options = list(
          paging = FALSE,       # Disable pagination to show all rows
          dom = 'ft',           # Show only filter and table
          scrollY = "600px",    # Add vertical scroll
          columnDefs = list(
            list(className = 'dt-center', targets = 1)
          )
        ),
        # *** MODIFICATION END ***
        rownames = FALSE
      )
    })
    
  })
}

# --- END FILE: encyclopedia_module.R ---