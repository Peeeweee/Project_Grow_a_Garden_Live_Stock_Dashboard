# --- START FILE: encyclopedia_module.R ---

# =============================================================
# UI function for the Encyclopedia Module
# =============================================================
encyclopedia_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "encyclopedia",
          # Updated internal CSS to ensure DT headers are styled correctly
          tags$head(
            tags$style(HTML("
              .encyclopedia-section-header {
                margin-top: 20px; margin-bottom: 10px; border-left: 4px solid var(--accent-yellow); 
                padding-left: 15px;
              }
              .wiki-table-wrapper {
                overflow-x: auto;
                width: 100%;
              }
              /* Ensure custom DT containers and their tables are styled */
              .dataTables_wrapper.no-footer .dataTables_scrollBody {
                  border: 1px solid var(--border-color) !important;
              }
              .wiki-table, table.dataTable {
                width: 100% !important; border-collapse: collapse; margin-bottom: 25px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                background-color: var(--bg-card);
                border: 1px solid var(--border-color);
              }
              .wiki-table th, .wiki-table td, table.dataTable th, table.dataTable td { 
                border: 1px solid var(--border-color) !important; 
                padding: 10px 12px; 
                text-align: left;
                color: var(--text-primary);
              }
              .wiki-table thead th, table.dataTable thead th {
                background-color: #153318 !important;
                color: var(--accent-yellow) !important;
                font-weight: bold; font-size: 1.1em; text-align: center; 
              }
              /* Style the custom rarity headers */
              .rarity-header-Common, .rarity-header-Uncommon, .rarity-header-Rare, 
              .rarity-header-Legendary, .rarity-header-Mythical, .rarity-header-Divine, 
              .rarity-header-Prismatic, .rarity-header-Unavailable {
                  color: white !important;
                  text-align: left !important; /* Align the main title left */
                  padding-left: 15px !important;
              }
              .rarity-header-Common { background-color: #7f8c8d !important; }
              .rarity-header-Uncommon { background-color: var(--accent-green) !important; }
              .rarity-header-Rare { background-color: #2980B9 !important; }
              .rarity-header-Legendary { background-color: var(--accent-yellow) !important; color: #153318 !important; }
              .rarity-header-Mythical { background-color: var(--accent-orange) !important; }
              .rarity-header-Divine { background-color: var(--accent-red) !important; }
              .rarity-header-Prismatic { background: linear-gradient(45deg, #a96ded, #FFC107); }
              .rarity-header-Unavailable { background-color: #4d5658 !important; }

              .wiki-table tbody tr:nth-child(even), table.dataTable tbody tr:nth-child(even) { background-color: rgba(0,0,0,0.2); }
              .wiki-table tbody tr:hover, table.dataTable tbody tr:hover { background-color: rgba(255, 193, 7, 0.1); }
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
                     div(class="encyclopedia-section-header", h3("Gear & Tools List")),
                     p("A comprehensive list of all gear and tools. Sourced from growagardencalculator.net."),
                     DTOutput(ns("gear_table_detailed"))
            ),
            tabPanel("Eggs & Animals", icon = icon("egg"),
                     div(class="encyclopedia-section-header", h3("Pet Eggs")),
                     p("Eggs that can be purchased or found to hatch into helpful pets."),
                     DTOutput(ns("egg_table_dt")),
                     hr(style="border-color: var(--border-color);"),
                     div(class="encyclopedia-section-header", h3("Hatchable Pets")),
                     p("Pets that can be hatched from the eggs listed above, providing unique bonuses."),
                     uiOutput(ns("pet_tables_ui_dt"))
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
          paging = FALSE, dom = 'Bfrt', scrollY = "600px", scrollX = TRUE,
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
          fontWeight = 'bold', borderRadius = '4px', padding = '3px 8px'
        ) %>%
        formatStyle(
          c('Multi Harvest', 'Obtainable'),
          color = styleEqual(c("Yes", "No", "✓", "✗", "Unknown"), c('#2ECC71', '#E74C3C', '#2ECC71', '#E74C3C', 'grey')),
          fontWeight = 'bold'
        )
    })
    
    # --- Gear Table ---
    output$gear_table_detailed <- renderDT({
      df <- all_encyclopedia_data()$gear
      req(df, nrow(df) > 0)
      
      df_display <- df %>%
        mutate(
          Image = paste0('<img src="', image_url, '" height="50" alt="', name, '">'),
          Cost_HTML = gsub("\\n", "<br>", cost)
        ) %>%
        select(Image, Tool = name, Description = description, Cost = Cost_HTML, Rarity = rarity, Obtainable = obtainable)
      
      datatable(
        df_display,
        escape = FALSE,
        extensions = 'Buttons', class = 'display compact hover',
        options = list(
          paging = FALSE, dom = 'Bfrt', scrollY = "600px", scrollX = TRUE,
          buttons = c('copy', 'csv', 'excel', 'pdf'),
          columnDefs = list(list(className = 'dt-center', targets = c(0, 4, 5)))
        ),
        rownames = FALSE
      ) %>%
        formatStyle('Rarity',
                    color = styleEqual(c("Common", "Uncommon", "Rare", "Legendary", "Mythical", "Divine", "Prismatic", "Unknown", "-"), c('#FFFFFF', '#FFFFFF', '#FFFFFF', '#153318', '#FFFFFF', '#FFFFFF', '#153318', '#FFFFFF', '#FFFFFF')),
                    backgroundColor = styleEqual(c("Common", "Uncommon", "Rare", "Legendary", "Mythical", "Divine", "Prismatic", "Unknown", "-"), c('#7f8c8d', '#347433', '#2980B9', '#FFC107', '#FF6F3C', '#B22222', '#a96ded', '#4d5658', '#4d5658')),
                    fontWeight = 'bold', borderRadius = '4px', padding = '3px 8px'
        ) %>%
        formatStyle('Obtainable', color = styleEqual(c("✓", "✗"), c('#2ECC71', '#E74C3C')), fontWeight = 'bold')
    })
    
    # --- EGG & ANIMAL LOGIC (Using New Data Source for Pets) ---
    
    # Egg Table - This reads from the original egg data source.
    output$egg_table_dt <- renderDT({
      df <- all_encyclopedia_data()$eggs
      req(df, nrow(df) > 0)
      
      df_display <- df %>% select(
        `Egg Type` = name, Cost = cost, `Chances to Appear` = chance_to_appear,
        `Number of Pet Types` = num_pets, `Time to Grow` = grow_time
      )
      datatable(df_display, class = "wiki-table", escape = FALSE,
                options = list(paging = FALSE, dom = 'ft', scrollY = "400px"),
                rownames = FALSE)
    })
    
    # Pet Tables - This block is revised to re-add the rarity column before selecting.
    output$pet_tables_ui_dt <- renderUI({
      df <- all_encyclopedia_data()$pets
      req(df, nrow(df) > 0)
      
      rarity_levels <- c("Common", "Uncommon", "Rare", "Legendary", "Mythical", "Divine", "Prismatic", "-")
      df$rarity <- factor(df$rarity, levels = intersect(rarity_levels, unique(df$rarity)))
      
      grouped_df <- df %>%
        filter(!is.na(rarity)) %>% arrange(rarity) %>%
        group_by(rarity) %>% summarise(data = list(cur_data()), .groups = 'drop')
      
      tables_list <- lapply(1:nrow(grouped_df), function(i) {
        current_rarity_str <- as.character(grouped_df$rarity[[i]])
        current_rarity_label <- if (current_rarity_str %in% rarity_levels[1:7]) current_rarity_str else "Unavailable"
        current_data <- grouped_df$data[[i]]
        
        # Correctly select and rename columns to match the new data and target image.
        df_display <- current_data %>%
          mutate(
            Image = paste0('<img src="', image_url, '" height="50" alt="', name, '">'),
            rarity = current_rarity_str # Add the rarity column back in
          ) %>%
          # Now the select() call will work because the 'rarity' column exists.
          select(Img = Image, Name = name, Desc = description, Tier = rarity, Obtainable = obtainable)
        
        # Create the custom two-level header container
        table_container <- htmltools::withTags(table(
          class = "wiki-table",
          thead(
            tr(
              th(colspan = "5", class = paste0("rarity-header-", current_rarity_label), paste("All", current_rarity_label, "Pets"))
            ),
            tr(lapply(colnames(df_display), th))
          )
        ))
        
        dt_output <- datatable(df_display,
                               container = table_container,
                               escape = FALSE,
                               options = list(
                                 dom = 'ft', paging = FALSE, scrollY = "600px", scrollCollapse = TRUE,
                                 columnDefs = list(
                                   list(className = 'dt-center', targets = c(0, 3, 4)),
                                   list(width = '40%', targets = 2) # Give more space to Description
                                 )
                               ),
                               rownames = FALSE) %>%
          formatStyle(
            'Tier',
            color = styleEqual(
              c("Common", "Uncommon", "Rare", "Legendary", "Mythical", "Divine", "Prismatic", "-"),
              c('#FFFFFF', '#FFFFFF', '#FFFFFF', '#153318', '#FFFFFF', '#FFFFFF', '#153318', '#FFFFFF')
            ),
            backgroundColor = styleEqual(
              c("Common", "Uncommon", "Rare", "Legendary", "Mythical", "Divine", "Prismatic", "-"),
              c('#7f8c8d', '#347433', '#2980B9', '#FFC107', '#FF6F3C', '#B22222', '#a96ded', '#4d5658')
            ),
            fontWeight = 'bold', borderRadius = '4px', padding = '3px 8px'
          ) %>%
          formatStyle(
            'Obtainable',
            color = styleEqual(c("✓", "✗"), c('#2ECC71', '#E74C3C')),
            fontWeight = 'bold'
          )
        
        div(class = "wiki-table-wrapper", dt_output)
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
        select(`Mutation Name` = name, Icon, Multiplier = multiplier, `Stack Bonus` = stack_bonus, Description = description)
      
      datatable(
        df_display, escape = FALSE,
        options = list(paging = FALSE, dom = 'ft', scrollY = "600px", columnDefs = list(list(className = 'dt-center', targets = 1))),
        rownames = FALSE
      )
    })
    
  })
}

# --- END FILE: encyclopedia_module.R ---