# Data Selection and visualization module #######################################
################################################################################
data_overview_ui <- function(){
  
  tabPanel("Data Selection and Visualization",
           # Sidebar with input options
           sidebarLayout(
             sidebarPanel(
               #keep the sidebarPanel fixed when scrolling through mainPanel
               style = "position: fixed; top: 20; height: calc(100vh - 80px); overflow-y: auto; width: 30%;",
               #upload Excel file
               conditionalPanel(condition = "input.tabselected=='Table & Graph'",
                                #data upload option
                                fileInput("data_file", "Choose a file with data"),
                                conditionalPanel(
                                  condition = "output.data_file_uploaded",
                                  #picket for variable selection
                                  pickerInput("var_select", "Select variables to display:",
                                              choices = list(),
                                              options = list(`actions-box` = TRUE),
                                              multiple = TRUE
                                  )
                                )
                                
               ),
               #options for plotting data within reactive table
               conditionalPanel(
                 condition = "input.tabselected == 'Table & Graph' && input.var_select.length > 0",
                 hr(),
                 h4("Plot"),
                 selectInput("choose_table", "Select plot type",
                                      choices = list("Scatter",
                                                     "Bar Chart",
                                                     "Box-plot",
                                                     "Violin",
                                                     "Raincloud"), selected = NULL, multiple = FALSE
                   ),
                   selectInput("x_axis", "Select X-axis variable:", choices = NULL),
                   # for group_level selection
                   conditionalPanel(condition = "input.x_axis.length > 0 && input.y_axis.length > 0",
                                    div(style = "margin-left: 20px;",
                                        selectInput(inputId = "group_order_table", label = "Select order and variables to show", choices = NULL, multiple = TRUE, selected = NULL)
                                      )
                                    ),
                   selectInput("y_axis", "Select Y-axis variable:", choices = NULL),
                  # for group_level selection
                  conditionalPanel(condition = "input.x_axis.length > 0 && input.y_axis.length > 0",
                                      checkboxInput(inputId = "show_additional_group_legend_table", label = "Select additional grouping variable", value = FALSE),
                    conditionalPanel(
                      condition = "input.show_additional_group_legend_table == true",
                      div(style = "margin-left: 20px;",
                          selectInput("y_var_table", "Select the column for additional grouping", ""),
                        #for additional group selection
                        selectInput(
                          inputId = "additional_group_order_table",
                          label = "Select order and variables to show",
                          choices = NULL,
                          multiple = TRUE,
                          selected = NULL
                        )
                      )
                    ),
                    # for paired observations
                    checkboxInput("repeated_observations_table", "Paired observations?", value = FALSE),
                    div(style = "margin-left: 20px;",
                          conditionalPanel(
                        condition = "input.repeated_observations_table == true",
                        selectInput("paired_observations_table", "Select column for paired observations", "")
                      )
                    ),
                    # additional options for scatter plot
                    conditionalPanel(condition = "input.choose_table == 'Scatter'",
                                     h5("Scatter plot additional options"),
                                     div(style = "margin-left: 20px;",
                                         checkboxInput(inputId = "maginal_plot_table", label = "Show marginal distribution plots", value = FALSE),
                                         checkboxInput(inputId = "geom_smooth_scatter_table", label = "Show trendline", value = FALSE)
                                     )
                                     
                    ),
                    actionButton(inputId = "submit_plot_table", label = "Plot", icon = icon("chart-simple")),
                    actionButton(inputId = "reset_plot_table", label = "Clear", icon = icon("trash")),
                    actionButton(inputId = "options_table", label = "Plot Options", icon = icon("cog")) # fixed to try to see modal
                  ),
                )
             ),
             # Main panel display
             mainPanel(
               tabsetPanel(
                 id = "tabselected",
                 tabPanel("Table & Graph",
                          #shows error message if uploaded file is not .xls, .xlsx or .csv format
                          verbatimTextOutput("uploaded_file_extension_error"),
                          # displays page length options and rendered datatable
                          conditionalPanel(condition = "input.tabselected == 'Table & Graph' && input.var_select.length > 0",
                          fluidRow(
                            column(width = 12,
                                   br(),
                                   selectInput("page_len", "Choose number of rows per page", choices = c(10, 25, 50, 100, 200, 500, 10000), selected = 10),
                                   DTOutput("data_table")
                                   )
                            ),
                          fluidRow(
                            column(width = 12,
                                   # adds error message if for some groups ICC cannot be calculated
                                   verbatimTextOutput("table_error_message"),
                                   withSpinner(plotOutput("table_plot", height = 'auto', width = 'auto', brush = "table_plot_brush"), type = 5, hide.ui = FALSE),
                                   # for brushed points
                                   tableOutput("table_brush_selection"),
                                   #adds modal
                                   plot_modal("table"),
                                   #removes additional close button
                                   tags$head(tags$style("#modal_table .modal-footer{ display:none}"))
                              )
                            ),
                          # numeric input to adjust plot size
                          conditionalPanel(condition = "input.submit_plot_table >0",
                            fluidRow(
                              column(
                                width = 2, # Adjust the width as needed (1-12), 6 will take half the available width
                                numericInput(inputId = "height_plot_table", label = "Plot height (pixels)", value = 500)
                              ),
                              column(
                                width = 2, # Adjust the width as needed (1-12), 6 will take half the available width
                                numericInput(inputId = "width_plot_table", label = "Plot width (pixels)", value = 800)
                              ),
                            )
                          )
                        )
                 )
               )
             )
           )
  )
}

