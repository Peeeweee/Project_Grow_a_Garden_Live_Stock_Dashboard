# --- START FILE: encyclopedia_module.R ---

# =============================================================
# UI function for the Encyclopedia Module
# =============================================================
encyclopedia_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "encyclopedia",
          # Updated internal CSS for custom tables to match the theme
          tags$head(
            tags$style(HTML("
              .encyclopedia-section-header {
                margin-top: 20px; margin-bottom: 10px; border-left: 4px solid var(--accent-yellow); 
                padding-left: 15px;
              }
              /* ### MODIFICATION: Added a wrapper class for mobile scrolling */
              .wiki-table-wrapper {
                overflow-x: auto;
                width: 100%;
              }
              .wiki-table {
                width: 100%; border-collapse: collapse; margin-bottom: 25px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                background-color: var(--bg-card);
                border: 1px solid var(--border-color);
              }
              .wiki-table th, .wiki-table td { 
                border: 1px solid var(--border-color); 
                padding: 10px 12px; 
                text-align: left;
                color: var(--text-primary);
              }
              .wiki-table thead th {
                background-color: #153318 !important;
                color: var(--accent-yellow) !important;
                font-weight: bold; font-size: 1.1em; text-align: center; 
              }
              /* Style the custom rarity headers */
              .rarity-header-Common, .rarity-header-Uncommon, .rarity-header-Rare, 
              .rarity-header-Legendary, .rarity-header-Mythical, .rarity-header-Divine, 
              .rarity-header-Prismatic, .rarity-header-Unavailable {
                  color: white !important;
              }
              .rarity-header-Common { background-color: #7f8c8d !important; }
              .rarity-header-Uncommon { background-color: var(--accent-green) !important; }
              .rarity-header-Rare { background-color: #2980B9 !important; }
              .rarity-header-Legendary { background-color: var(--accent-yellow) !important; color: #153318 !important; }
              .rarity-header-Mythical { background-color: var(--accent-orange) !important; }
              .rarity-header-Divine { background-color: var(--accent-red) !important; }
              .rarity-header-Prismatic { background: linear-gradient(45deg, #a96ded, #FFC107); }

              .wiki-table tbody tr:nth-child(even) { background-color: rgba(0,0,0,0.2); }
              .wiki-table tbody tr:hover { background-color: rgba(255, 193, 7, 0.1); }
            "))
          ),
          
          h2(class = "animated-gradient-text", "Grow a Garden Encyclopedia"),
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
                     hr(style="border-color: var(--border-color);"),
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
      req(df, nrow(df) > 0)
      
      df_display <- df %>%
        mutate(
          Image = paste0('<img src="', image_url, '" height="50" alt="', name, '">'),
          sheckle_price_num = suppressWarnings(as.numeric(gsub(",", "", sheckle_price))),
          sell_value_num = suppressWarnings(as.numeric(gsub(",", "", sell_value))),
          robux_price_num = suppressWarnings(as.numeric(gsub(",", "", robux_price)))
        ) %>%
        select(
          Image, `Crop Name` = name, `Sheckle Price` = sheckle_price, 
          `Sell Value` = sell_value, `Robux Price` = robux_price, 
          Stock = stock, Tier = rarity, `Multi Harvest` = multi_harvest, 
          Obtainable = obtainable, sheckle_price_num, sell_value_num, robux_price_num
        )
      
      datatable(
        df_display,
        escape = FALSE,
        extensions = 'Buttons',
        class = 'display compact hover',
        options = list(
          paging = FALSE,
          dom = 'Bfrt',
          scrollY = "600px",
          scrollX = TRUE,
          buttons = c('copy', 'csv', 'excel', 'pdf'),
          columnDefs = list(
            list(className = 'dt-center', targets = c(0, 5, 6, 7, 8)),
            list(visible = FALSE, targets = c("sheckle_price_num", "sell_value_num", "robux_price_num"))
          )
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          'Tier',
          color = styleEqual(
            c("Common", "Uncommon", "Rare", "Legendary", "Mythical", "Divine", "Prismatic"),
            c('#FFFFFF', '#FFFFFF', '#FFFFFF', '#153318', '#FFFFFF', '#FFFFFF', '#153318')
          ),
          backgroundColor = styleEqual(
            c("Common", "Uncommon", "Rare", "Legendary", "Mythical", "Divine", "Prismatic"),
            c('#7f8c8d', '#347433', '#2980B9', '#FFC107', '#FF6F3C', '#B22222', '#a96ded')
          ),
          fontWeight = 'bold',
          borderRadius = '4px',
          padding = '3px 8px'
        ) %>%
        formatStyle(
          c('Multi Harvest', 'Obtainable'),
          color = styleEqual(c("Yes", "No", "✓", "✗", "Unknown"), c('#2ECC71', '#E74C3C', '#2ECC71', '#E74C3C', 'grey')),
          fontWeight = 'bold'
        )
    })
    
    # --- Gear table logic (UI styled via internal CSS block) ---
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
        
        table_html <- tags$table(class = "wiki-table",
                                 tags$thead(
                                   tags$tr(tags$th(class = header_class, colspan = "4", current_rarity)),
                                   tags$tr(
                                     tags$th("Tool"), tags$th("Cost"), 
                                     tags$th("Use"), tags$th("Number of Uses")
                                   )
                                 ),
                                 tags$tbody(lapply(1:nrow(current_data), function(j) {
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
                                 }))
        )
        ### MODIFICATION: Wrap the custom HTML table in a scrollable div for mobile.
        div(class="wiki-table-wrapper", table_html)
      })
      
      tagList(tables_list)
    })
    
    # --- EGG & ANIMAL LOGIC (UI styled via internal CSS block) ---
    output$egg_table_ui <- renderUI({
      df <- all_encyclopedia_data()$eggs_and_pets$eggs
      req(df, nrow(df) > 0)
      table_html <- tags$table(class = "wiki-table",
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
      ### MODIFICATION: Wrap the custom HTML table in a scrollable div for mobile.
      div(class = "wiki-table-wrapper", table_html)
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
        
        table_html <- tags$table(class = "wiki-table",
                                 tags$thead(
                                   tags$tr(tags$th(class = header_class, colspan = "3", paste("All", current_rarity, "Pets"))),
                                   tags$tr(tags$th("Pet Name"), tags$th("Bonus"), tags$th("Chance of Appearing"))
                                 ),
                                 tags$tbody(lapply(1:nrow(current_data), function(j) {
                                   chance_value <- current_data$chance_of_appearing[j]
                                   display_chance <- if (is.na(chance_value) || chance_value == "") "Not Listed" else chance_value
                                   tags$tr(
                                     tags$td(current_data$name[j]),
                                     tags$td(current_data$bonus[j]),
                                     tags$td(display_chance)
                                   )
                                 }))
        )
        ### MODIFICATION: Wrap the custom HTML table in a scrollable div for mobile.
        div(class = "wiki-table-wrapper", table_html)
      })
      
      tagList(tables_list)
    })
    
    # --- Mutations Table (Styled by global styles.css) ---
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
        options = list(
          paging = FALSE,
          dom = 'ft',
          scrollY = "600px",
          columnDefs = list(
            list(className = 'dt-center', targets = 1)
          )
        ),
        rownames = FALSE
      )
    })
    
  })
}

# --- END FILE: encyclopedia_module.R ---