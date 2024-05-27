#Tools for assessing variability components ####################################
################################################################################
tools_variability_ui <- function (){
  
  #option to display selectable tools to assess variability
  tabPanel("Intraclass correlation & Linear Mixed Models",
             #adds ICC to Intraclass correlation coefficient
             tabPanel(title = NULL,#"ICC plots",
                      sidebarLayout(
                        sidebarPanel(
                          #conditional clause for Plot
                          conditionalPanel(condition = "input.tabselected2=='Intraclass Correlation Analysis (2 Levels)' || input.tabselected2 == 'Linear Mixed Model'",
                                           #data upload option
                                           fileInput("data_file_icc", "Choose a file with data"),
                                           conditionalPanel(
                                             condition = "output.data_file_icc_uploaded",
                                             # adds plot type selection
                                             h4("Plot"),
                                             # adds error message if anything goes wrong with the data selection
                                             verbatimTextOutput("error_message_icc_data"),
                                             
                                            checkboxInput(inputId = "show_additional_group_legend_icc", label = "Select additional grouping variable", value = TRUE),
                                            checkboxInput(inputId = "show_additional_group_legend_icc_two", label = "Select additional grouping variable", value = TRUE), 
                                             # for 2 level icc
                                             conditionalPanel(
                                               condition = "input.tabselected2=='Intraclass Correlation Analysis (2 Levels)'",
                                               selectInput("choose_icc", "Select plot type",
                                                           choices = list("Scatter",
                                                                          "Bar Chart",
                                                                          "Box-plot",
                                                                          "Violin",
                                                                          "Raincloud"), selected = NULL, multiple = FALSE
                                               )
                                             ),
                                             conditionalPanel(
                                               condition = "input.tabselected2 == 'Linear Mixed Model'",
                                               selectInput("choose_icc_two", "Select plot type",
                                                           choices = list("Scatter",
                                                                          "Bar Chart",
                                                                          "Box-plot",
                                                                          "Violin",
                                                                          "Raincloud"), selected = NULL, multiple = FALSE
                                               )
                                             ),
                                             # to select grouping variable (e.g. genotype)
                                             selectInput("group_var_icc", "Select the grouping variable", ""),
                                            # for group_level selection for Intraclass Correlation Analysis (2 Levels)
                                            conditionalPanel(condition = "input.group_var_icc.length > 0 && input.tabselected2=='Intraclass Correlation Analysis (2 Levels)'",
                                                             div(style = "margin-left: 20px;",
                                                                 selectInput(inputId = "additional_group_order_icc", label = "Select order and variables to show", choices = NULL, multiple = TRUE, selected = NULL)
                                                             )
                                            ),
                                            # for group_level selection for Linear Mixed Model
                                            conditionalPanel(condition = "input.group_var_icc.length > 0 && input.tabselected2=='Linear Mixed Model'",
                                                             div(style = "margin-left: 20px;",
                                                                 selectInput(inputId = "group_order_icc_two", label = "Select order and variables to show", choices = NULL, multiple = TRUE, selected = NULL)
                                                             )
                                            ),
                                             # select the number of hierarchical levels (1-3) in the data
                                             selectInput("num_levels", "Select the number of levels in your data", choices = c(1, 2, 3, 4))
                                           ),
                                           div(style = "margin-left: 20px;",
                                             conditionalPanel(
                                               condition = "input.num_levels >= 2 && (input.tabselected2=='Intraclass Correlation Analysis (2 Levels)' || input.tabselected2 == 'Linear Mixed Model')",
                                               selectInput("x_var_icc", "Select the column for level 1", ""),
                                               selectInput("y_var_icc", "Select the column for level 2", "")
                                             ),
                                             conditionalPanel(
                                               condition = "input.num_levels >= 3 && (input.tabselected2 == 'Linear Mixed Model')",
                                               selectInput("z_var_icc", "Select the column for level 3", "")
                                             ),
                                             conditionalPanel(
                                               condition = "input.num_levels == 4 && (input.tabselected2 == 'Linear Mixed Model')",
                                               selectInput("w_var_icc", "Select the column for level 4", "")
                                             )
                                          ),
                                          # additional options for scatter plot for Intraclass Correlation Analysis (2 Levels)
                                          conditionalPanel(condition = "input.tabselected2 == 'Intraclass Correlation Analysis (2 Levels)' && input.choose_icc == 'Scatter' && input.num_levels >= 2", 
                                                           h5("Scatter plot additional options"),
                                                           div(style = "margin-left: 20px;",
                                                               checkboxInput(inputId = "maginal_plot_icc", label = "Show marginal distribution plots", value = FALSE),
                                                                checkboxInput(inputId = "geom_smooth_scatter_icc", label = "Show trendline", value = FALSE)
                                                           )
                                                           
                                          ),
                                          # additional options for scatter plot for Linear Mixed Model
                                          conditionalPanel(condition = "input.tabselected2 == 'Linear Mixed Model' && input.choose_icc_two == 'Scatter' && input.num_levels >= 2", 
                                                           h5("Scatter plot additional options"),
                                                           div(style = "margin-left: 20px;",
                                                               checkboxInput(inputId = "maginal_plot_icc_two", label = "Show marginal distribution plots", value = FALSE),
                                                               checkboxInput(inputId = "geom_smooth_scatter_icc_two", label = "Show trendline", value = FALSE)
                                                           )
                                          ),
                                           # button selection for Intraclass Correlation Analysis (2 Levels)
                                           conditionalPanel(
                                             condition = "input.tabselected2=='Intraclass Correlation Analysis (2 Levels)' && input.num_levels >= 2",
                                             actionButton("submit_plot_icc", "Calculate ICC", icon = icon("chart-simple")),
                                             actionButton("clear_icc", "Clear", icon = icon("trash")),
                                             actionButton(inputId = "options_icc", label = "Plot Options", icon = icon("cog"))
                                           ),
                                           # button selection for Linear Mixed Model
                                           conditionalPanel(
                                             condition = "input.tabselected2 == 'Linear Mixed Model' && input.num_levels >= 2",
                                             actionButton("submit_plot_icc_two", "Calculate LMM", icon = icon("chart-simple")),
                                             actionButton("clear_icc_two", "Clear", icon = icon("trash")),
                                             actionButton(inputId = "options_icc_two", label = "Plot Options", icon = icon("cog"))
                                           ),
                                           hr(),
                                           # panel options for LMM
                                           conditionalPanel(
                                             condition = "input.tabselected2 == 'Linear Mixed Model' && output.data_file_icc_uploaded && input.num_levels >= 2",
                                             # adds LMM options 
                                             h4("Linear Mixed Model"),
                                             # to select control group
                                             selectInput("control_group_var_icc", "Select control group", ""),
                                             # check box for repeated measurements
                                             checkboxInput("repeated_var_icc_selection", "Repeated measures?", value = FALSE),
                                             div(style = "margin-left: 20px;",
                                                 conditionalPanel(
                                                  condition = "input.repeated_var_icc_selection == true",
                                                  selectInput("repeated_var_icc", "Selected repeated measures column", "")
                                                )
                                              ),
                                             checkboxInput("manual_lmm_text", "Write lmm formula?", value = FALSE),
                                             #allows to input lmm manually
                                             div(style = "margin-left: 20px;",
                                               conditionalPanel(
                                                 condition = "input.manual_lmm_text == true",
                                                 textInput("manual_lmm", label = h5("Define lmer manually",  bsButton("manual_lmm_text_popover", label = "?", icon = NULL, style = "info",  title = "About lmer notation", block = FALSE,  href = NULL,  dataToggle = NULL,  dataPlacement = "right",  options = NULL
                                                 )), value = ""),
                                                 bsPopover("manual_lmm_text_popover", "popover", 
                                                           content = "- Write the exact names of the select variables displayed above <br> + adds terms to the model <br> * and : can be used for interaction <br> | is used to specify random effects <br> ( ) group terms together <br> <br> Example <br> cell_diameter ~ time_pont * treatment + ( 1 | replica) ",
                                                           placement = "bottom", trigger = "hover", 
                                                           options = list(
                                                             html = TRUE)
                                                 )
                                               )
                                              ),
                                             # to select effect size for LMM
                                             selectInput(inputId = "effect_size_lmm", 
                                                         label = "Select effect size for LMM fixed effects",
                                                         choices = c("Partial eta squared" = "eta2", "Partial omega squared" = "omega2", "Partial epsilon squared" = "epsilon2", "Cohen's f squared" = "cohen_f", "Cohen's d" = "d", "Partial r" = "r"),
                                                         multiple = FALSE,
                                                         selected = "eta2"
                                                         )
                                           ),
                          )
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            id = "tabselected2",
                            tabPanel("Intraclass Correlation Analysis (2 Levels)",
                                     #shows error message if uploaded file is not .xls, .xlsx or .csv format
                                     verbatimTextOutput("uploaded_icc_file_extension_error"),
                                     fluidRow(
                                       column(width = 12,
                                              withSpinner(plotOutput("icc_plot", height = 'auto', width = 'auto', brush = brushOpts("icc_plot_brush")), type = 5),
                                              # for brushed points
                                              tableOutput("icc_brush_selection"),
                                              # adds error message if for some groups ICC cannot be calculated
                                              verbatimTextOutput("icc_error_message"),
                                              plot_modal("icc"),
                                              #removes additional close button
                                              tags$head(tags$style("#modal_icc .modal-footer{ display:none}"))
                                       ),
                                       style = "margin-bottom: 2px;"
                                     ),
                                     # numeric input to adjust plot size
                                     conditionalPanel(condition = "input.submit_plot_icc >0",
                                                      fluidRow(
                                                        column(
                                                          width = 2, # Adjust the width as needed (1-12), 6 will take half the available width
                                                          numericInput(inputId = "height_plot_icc", label = "Plot height (pixels)", value = 500)
                                                        ),
                                                        column(
                                                          width = 2, # Adjust the width as needed (1-12), 6 will take half the available width
                                                          numericInput(inputId = "width_plot_icc", label = "Plot width (pixels)", value = 800)
                                                        ),
                                                      )
                                     ),
                                     # download buttons
                                     fluidRow(
                                       downloadButton("download_data_table_icc", "Download ICC summary"),
                                       downloadButton("download_plot_icc", "Download plot"),
                                       tableOutput("data_table_icc")
                                     )
                            ),
                            tabPanel("Linear Mixed Model",
                                     # adds error message if for some groups ICC cannot be calculated
                                     verbatimTextOutput("lmm_error_message"),
                                     fluidRow(
                                       column(width = 12,
                                              withSpinner(plotOutput("icc_two_plot", height = 'auto', width = 'auto', brush = brushOpts("icc_two_plot_brush")), type = 5),
                                              p(id = "loading-message", "The computation will take longer than usual to load due to large number of observations. Please be patient.", style = "display:none;"),
                                              # for brushed points
                                              tableOutput("icc_two_brush_selection"),
                                              #adds modal
                                              plot_modal("icc_two"),
                                              #removes additional close button
                                              tags$head(tags$style("#modal_icc_two .modal-footer{ display:none}"))
                                       ),
                                       style = "margin-bottom: 2px;"
                                     ),
                                     # numeric input to adjust plot size
                                     conditionalPanel(condition = "input.submit_plot_icc_two >0",
                                                      fluidRow(
                                                        column(
                                                          width = 2, # Adjust the width as needed (1-12), 6 will take half the available width
                                                          numericInput(inputId = "height_plot_icc_two", label = "Plot height (pixels)", value = 500)
                                                        ),
                                                        column(
                                                          width = 2, # Adjust the width as needed (1-12), 6 will take half the available width
                                                          numericInput(inputId = "width_plot_icc_two", label = "Plot width (pixels)", value = 800)
                                                        ),
                                                      )
                                     ),
                                     # download buttons
                                     fluidRow(
                                       downloadButton("download_data_table_icc_two_lmm", "Download LMM summary"),
                                       downloadButton("download_plot_icc_two", "Download plot"),
                                       tableOutput("data_table_icc_two_lmm")
                                     )
                            )
                          )
                        )
                      )
             )
  )
}