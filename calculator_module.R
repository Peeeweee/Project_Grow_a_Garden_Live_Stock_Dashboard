# --- START FILE: calculator_module.R ---

# =============================================================
# UI function for the Calculator Module
# =============================================================
calculator_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "calculator",
          # Add custom CSS for button glow effect
          tags$head(
            tags$style(HTML("
              .button-glow {
                box-shadow: 0 0 15px #FFC107 !important; /* Matches new theme's accent */
                transition: box-shadow 0.2s ease-in-out;
              }
            "))
          ),
          
          # --- MODIFICATION: Added animated class to the title ---
          h2(class = "animated-gradient-text", "Grow a Garden Fruit Value Calculator"),
          p("Select a crop and its mutations to calculate the final sell value based on the official formula."),
          
          fluidRow(
            # --- INPUTS COLUMN ---
            column(width = 8,
                   # -- Crop Selection Box --
                   box(
                     title = "1. Select Crop", status = "primary", solidHeader = TRUE, width = 12,
                     fluidRow(
                       column(width = 9,
                              uiOutput(ns("crop_selector_ui"))
                       ),
                       column(width = 3, align = "center",
                              actionButton(ns("fetch_fruit_data"), "Fetch Latest", icon = icon("sync"), class = "btn-primary", style = "margin-top: 25px;")
                       )
                     )
                   ),
                   # -- Mutations Box --
                   box(
                     title = "2. Select Mutations", status = "primary", solidHeader = TRUE, width = 12,
                     
                     fluidRow(
                       column(12, align = "right", style = "margin-bottom: 15px;",
                              actionButton(ns("reset_mutations_button"), "Reset", icon = icon("undo"), class = "btn-sm btn-default"),
                              actionButton(ns("max_mutations_button"), "Maximize All", icon = icon("rocket"), class = "btn-sm btn-warning")
                       )
                     ),
                     
                     fluidRow(
                       column(width = 6,
                              h4("Growth Mutations", style="text-align:center; border-bottom: 1px solid var(--border-color); padding-bottom: 5px; margin-bottom: 10px;"),
                              radioButtons(ns("growth_mutation"), label = NULL,
                                           choices = c("None (x1)" = 1, "Golden (x20)" = 20, "Rainbow (x50)" = 50),
                                           selected = 1),
                              
                              br(), 
                              
                              h4("Temperature Mutations", style="text-align:center; border-bottom: 1px solid var(--border-color); padding-bottom: 5px; margin-bottom: 10px;"),
                              radioButtons(ns("temp_mutation"), label = NULL,
                                           choices = c("None (+0)" = 0, "Wet (+1)" = 1, "Chilled (+1)" = 1, "Frozen (+9)" = 9),
                                           selected = 0)
                       ),
                       column(width = 6,
                              h4("Environmental Mutations", style="text-align:center; border-bottom: 1px solid var(--border-color); padding-bottom: 5px; margin-bottom: 10px;"),
                              p(em("You can select multiple."), style="text-align:center; font-size:0.9em; margin-top: -5px;"),
                              uiOutput(ns("env_mutations_ui"))
                       )
                     )
                   )
            ),
            
            # --- OUTPUTS COLUMN ---
            column(width = 4,
                   box(
                     title = "Calculated Value", status = "success", solidHeader = TRUE, width = 12,
                     div(style="text-align: center;",
                         valueBoxOutput(ns("calculated_value_box"), width = 12)
                     )
                   ),
                   box(
                     title = "Formula", status = "info", solidHeader = TRUE, width = 12,
                     div(class = "formula-box",
                         "Final Value = Base Value × Growth × (1 + Temp + Environment)"
                     )
                   ),
                   box(
                     title = "Calculation Breakdown", status = "warning", solidHeader = TRUE, width = 12,
                     uiOutput(ns("solution_ui"))
                   )
            )
          ),
          
          # The detailed data table display
          fluidRow(
            box(
              title = "Detailed Fruit Data (from local cache)", status = "primary", solidHeader = TRUE, width = 12,
              collapsible = TRUE, collapsed = FALSE, 
              p("This table shows the data currently loaded in the app. Use the 'Fetch Latest' button to update it from the website."),
              DTOutput(ns("fruit_details_table"))
            )
          )
  )
}


# =============================================================
# Server function for the Calculator Module
# =============================================================
calculator_server <- function(id, fruit_data_rv, fruit_value_list_rv, mutation_data) {
  moduleServer(id, function(input, output, session) {
    
    # --- Render UI Elements ---
    output$crop_selector_ui <- renderUI({
      fruit_list <- fruit_value_list_rv()
      
      if (is.null(fruit_list) || length(fruit_list) == 0) {
        return(
          tags$div(class = "alert alert-danger", role = "alert",
                   style = "padding: 10px; text-align: center;",
                   tags$strong("Data Not Found"),
                   tags$p("Local fruit data is missing. Please use the 'Fetch Latest' button to load it from the website.")
          )
        )
      }
      
      crop_names <- sort(names(fruit_list))
      
      selectInput(session$ns("selected_crop"),
                  label = "Choose a crop:",
                  choices = c("Please select a crop" = "", crop_names),
                  width = "100%")
    })
    
    output$env_mutations_ui <- renderUI({
      non_env_mutations <- c("Wet", "Chilled", "Frozen", "Golden", "Rainbow")
      req(mutation_data)
      env_muts <- mutation_data %>%
        filter(!name %in% non_env_mutations)
      choices_list <- setNames(as.list(env_muts$bonus), 
                               paste0(env_muts$name, " (+", env_muts$bonus, ")"))
      
      checkboxGroupInput(session$ns("env_mutations"), 
                         label = NULL,
                         choices = choices_list)
    })
    
    # --- Button Logic ---
    observeEvent(input$max_mutations_button, {
      shinyjs::addClass(id = "max_mutations_button", class = "button-glow")
      
      updateRadioButtons(session, "growth_mutation", selected = 50)
      updateRadioButtons(session, "temp_mutation", selected = 9)
      
      non_env_mutations <- c("Wet", "Chilled", "Frozen", "Golden", "Rainbow")
      env_muts <- mutation_data %>% filter(!name %in% non_env_mutations)
      all_env_values <- env_muts$bonus
      updateCheckboxGroupInput(session, "env_mutations", selected = all_env_values)
      
      delay(300, shinyjs::removeClass(id = "max_mutations_button", class = "button-glow"))
    })
    
    observeEvent(input$reset_mutations_button, {
      updateRadioButtons(session, "growth_mutation", selected = 1)
      updateRadioButtons(session, "temp_mutation", selected = 0)
      updateCheckboxGroupInput(session, "env_mutations", selected = character(0))
    })
    
    
    # --- Calculation Logic ---
    final_value <- reactive({
      req(input$selected_crop, nzchar(input$selected_crop))
      
      fruit_list <- fruit_value_list_rv()
      base_value <- fruit_list[[input$selected_crop]] %||% 0
      
      growth_multiplier <- as.numeric(input$growth_mutation)
      temp_bonus <- as.numeric(input$temp_mutation)
      env_bonuses <- if (is.null(input$env_mutations)) 0 else sum(as.numeric(input$env_mutations))
      calculated_value <- base_value * growth_multiplier * (1 + temp_bonus + env_bonuses)
      
      paste0("$", format(round(calculated_value, 0), big.mark = ",", scientific = FALSE))
    })
    
    # --- Render Outputs ---
    output$calculated_value_box <- renderValueBox({
      req(input$selected_crop, nzchar(input$selected_crop))
      
      valueBox(
        value = final_value(),
        subtitle = paste("Value for:", input$selected_crop),
        icon = icon("dollar-sign"),
        # --- MODIFICATION: Changed color to match new theme ---
        color = "yellow"
      )
    })
    
    output$solution_ui <- renderUI({
      req(input$selected_crop, nzchar(input$selected_crop))
      
      fruit_list <- fruit_value_list_rv()
      base_value <- fruit_list[[input$selected_crop]] %||% 0
      growth_multiplier <- as.numeric(input$growth_mutation)
      temp_bonus <- as.numeric(input$temp_mutation)
      env_bonuses_sum <- if (is.null(input$env_mutations)) 0 else sum(as.numeric(input$env_mutations))
      
      solution_string <- paste0(
        format(base_value, big.mark = ","), " × ",
        growth_multiplier, " × (1 + ",
        temp_bonus, " + ",
        env_bonuses_sum, ") = ",
        final_value()
      )
      
      div(class = "solution-box", solution_string)
    })
    
    output$fruit_details_table <- renderDT({
      df <- fruit_data_rv()
      req(df)
      
      df_display <- df %>%
        mutate(
          sell_value_num = suppressWarnings(as.numeric(sell_value)),
          sheckle_price_num = suppressWarnings(as.numeric(gsub(",", "", sheckle_price))),
          robux_price_num = suppressWarnings(as.numeric(gsub(",", "", robux_price)))
        ) %>%
        select(
          "Crop Name" = name,
          "Sheckle Price" = sheckle_price,
          "Sell Value" = sell_value,
          "Robux Price" = robux_price,
          "Stock" = stock,
          "Rarity Tier" = rarity,
          "Multi Harvest" = multi_harvest,
          "Obtainable" = obtainable,
          sell_value_num,
          sheckle_price_num, 
          robux_price_num
        ) %>%
        mutate(across(
          c("Sheckle Price", "Sell Value", "Robux Price", "Stock"),
          ~ if_else(is.na(.) | . %in% c("", "0", "None", "N/A"), "Unknown", as.character(.))
        ))
      
      # --- MODIFICATION: Updated datatable with new theme-friendly styles ---
      datatable(
        df_display,
        extensions = 'Buttons',
        class = 'display compact hover',
        rownames = FALSE,
        options = list(
          paging = FALSE,
          dom = 'Bfrt',
          scrollY = "600px",
          scrollX = TRUE,
          buttons = c('copy', 'csv', 'excel', 'pdf'),
          columnDefs = list(
            list(visible = FALSE, targets = c("sell_value_num", "sheckle_price_num", "robux_price_num"))
          )
        )
      ) %>%
        formatStyle(
          'Rarity Tier',
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
          'Multi Harvest',
          color = styleEqual(c("Yes", "No", "✓", "✗", "Unknown"), c('#2ECC71', '#E74C3C', '#2ECC71', '#E74C3C', 'grey')),
          fontWeight = 'bold'
        ) %>%
        formatStyle(
          'Obtainable',
          color = styleEqual(c("Yes", "No", "✓", "✗", "Unknown"), c('#2ECC71', '#E74C3C', '#2ECC71', '#E74C3C', 'grey')),
          fontWeight = 'bold'
        ) %>%
        formatCurrency('sheckle_price_num', currency = "", digits = 0) %>%
        formatCurrency('sell_value_num', currency = "", digits = 0) %>%
        formatCurrency('robux_price_num', currency = "", digits = 0)
    })
    
    # Return the button click as a reactive event
    return(
      list(
        fetch_data = reactive(input$fetch_fruit_data)
      )
    )
    
  })
}
# --- END FILE: calculator_module.R ---