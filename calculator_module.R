# --- START FILE: calculator_module.R ---

# =============================================================
# UI function for the Calculator Module
# =============================================================
calculator_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "calculator",
          h2("Grow a Garden Fruit Value Calculator"),
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
                       column(width = 4,
                              h4("Growth Mutations", style="text-align:center;"),
                              radioButtons(ns("growth_mutation"), label = NULL,
                                           choices = c("None (x1)" = 1, "Golden (x20)" = 20, "Rainbow (x50)" = 50),
                                           selected = 1)
                       ),
                       column(width = 4,
                              h4("Temperature Mutations", style="text-align:center;"),
                              radioButtons(ns("temp_mutation"), label = NULL,
                                           choices = c("None (+0)" = 0, "Wet (+1)" = 1, "Chilled (+1)" = 1, "Frozen (+9)" = 9),
                                           selected = 0)
                       ),
                       column(width = 4,
                              h4("Environmental Mutations", style="text-align:center;"),
                              p(em("You can select multiple."), style="text-align:center; font-size:0.9em;"),
                              checkboxGroupInput(ns("env_mutations"), label = NULL,
                                                 choices = c("Chocolate (+1)" = 1, "Moonlit (+1)" = 1, "Pollinated (+2)" = 2,
                                                             "Bloodlit (+3)" = 3, "Plasma (+4)" = 4, "Honey Glazed (+4)" = 4,
                                                             "Zombified (+24)" = 24, "Shocked (+99)" = 99, "Celestial (+119)" = 119,
                                                             "Disco (+124)" = 124, "Voidtouched (+134)" = 134))
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
                   # *** MODIFIED: Using the new CSS class for the formula box ***
                   box(
                     title = "Formula", status = "info", solidHeader = TRUE, width = 12,
                     div(class = "formula-box",
                         "Final Value = Base Value × Growth × (1 + Temp + Env)"
                     )
                   ),
                   # *** MODIFIED: Using the new CSS class for the solution box ***
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
              collapsible = TRUE, collapsed = TRUE,
              p("This table shows the data currently loaded in the app. Use the 'Fetch Latest' button to update it from the website."),
              DTOutput(ns("fruit_details_table"))
            )
          )
  )
}


# =============================================================
# Server function for the Calculator Module
# =============================================================
calculator_server <- function(id, fruit_data_rv, fruit_value_list_rv) {
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
        color = "green"
      )
    })
    
    # *** MODIFIED: Render the solution breakdown UI with the new class ***
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
      
      # Return the string inside the styled div
      div(class = "solution-box", solution_string)
    })
    
    output$fruit_details_table <- renderDT({
      df <- fruit_data_rv()
      req(df)
      
      datatable(
        df,
        rownames = FALSE,
        colnames = c("Crop", "Rarity", "Type", "Sell Price", "How to Get"),
        options = list(pageLength = 10, searching = TRUE, scrollX = TRUE)
      )
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