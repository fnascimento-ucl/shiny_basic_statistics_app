##########################Plots for data representation module##################
################################################################################
plots_data_representation_superplot_ui <- function() {
  tabPanel("Superplot",
           # Sidebar with input options
           sidebarPanel(
             #upload Excel file
            fileInput("data_file_superplot", "Choose a file with data"),
             conditionalPanel(
               condition = "input.superplot_tab == 'Superplot' && output.data_file_superplot_uploaded",
               h4("Plot"),
               # adds error message if anything goes wrong with the data selection
               verbatimTextOutput("error_message_superplot_data"),
               # select type of plot and variables
               selectInput(
                 inputId = "choose_superplot", 
                 label = "Select type of plot:", 
                 choices = list("Scatter",
                                "Raincloud"),
                 multiple = FALSE, selected = FALSE),
               # to select grouping variable
               selectInput("group_var_superplot", "Select the grouping variable", ""),
               # allows to filter group selection
               conditionalPanel(condition = "input.group_var_superplot.length > 0 && input.x_var_superplot.length > 0",
                                div(style = "margin-left: 20px;",
                                    selectInput(inputId = "group_order_superplot", label = "Select order and variables to show", choices = NULL, multiple = TRUE, selected = NULL)
                                )
               ),
               # select the variables for the number of hierarchical levels (2 levels for superplot) in the data
               selectInput("x_var_superplot", "Select the column for level 1", ""),
               # for additional group_level selection and marginal plots
               conditionalPanel(condition = "input.group_var_superplot.length > 0 && input.x_var_superplot.length > 0",
                                checkboxInput(inputId = "show_additional_group_legend_superplot", label = "Select additional grouping variable", value = TRUE)
                                ),
                                # additional variable selection
                                conditionalPanel(
                                  condition = "input.show_additional_group_legend_superplot == true && input.submit_plot_superplot > 0",
                                  div(style = "margin-left: 20px;",
                                      #for additional group selection
                                      selectInput(
                                        inputId = "additional_group_order_superplot",
                                        label = "Select order and variables to show",
                                        choices = NULL,
                                        multiple = TRUE,
                                        selected = NULL
                                      )
                                  )
                                ),
               
               selectInput("y_var_superplot", "Select the column for level 2", ""),
               fluidRow(
                 column(width = 7, div(actionButton("submit_plot_superplot", "Plot", icon = icon("chart-simple")), style = "margin-right: 10px;")),
                 column(width = 2, div(actionButton("reset_superplot", "Clear plot", icon = icon("trash")), style = "margin-left: 10px;"))
               ),
               
               
               #adds mean or median choices for plotting
               radioButtons("mean_or_median_superplot","Choose statistic to display:",               
                            choices = list("Mean with SD" = "mean",
                                           "Median with IQR" = "median"),
                            selected = "mean")
            ),
            # bootstrapped effect size options
            conditionalPanel(
              condition = "input.superplot_tab == 'Superplot' && output.data_file_superplot_uploaded && input.group_var_superplot.length > 0 && input.x_var_superplot.length > 0",
              hr(),
              h4('Effect size options'), 
              # adds choices for effect sizes
              selectInput("number_effect_sizes_superplot", "Number of effect sizes to display:", choices =c(0,1,2), multiple = FALSE, selected = 0),
              # allows to switch between hierarchical and simple bootstrap
              conditionalPanel(
                condition = "input.number_effect_sizes_superplot == 1 || input.number_effect_sizes_superplot == 2",
                radioButtons("bootstrap_option_superplot", "Choose type of bootstrap:",
                             choices = list ("Hierarchical" = "hierarchical_boot",
                                             "Simple Bootstrap" = "simple_boot"),
                             selected = "simple_boot"),
                numericInput("n_boot_superplot", "Number of boostrap samples:", value = NULL),
                
                # adds checkbox for paired observations
                checkboxInput("repeated_observations_superplot", "Paired observations?", value = FALSE),
                conditionalPanel(
                  condition = "input.repeated_observations_superplot == true",
                  selectInput("paired_observations_superplot", "Select column for paired observations", "")
                ),
                
                #select control and test groups
                selectInput("control_group_superplot", "Select control group", choices = NULL, multiple = FALSE, selected = NULL),
                selectInput("test_groups_superplot", "Select test group(s) and their order", choices = NULL, multiple = TRUE, selected = NULL)
                ),
              # choose median or mean effect size
              conditionalPanel(
                condition = "input.number_effect_sizes_superplot == 1 || input.number_effect_sizes_superplot == 2",
                selectInput("effect_size_mean_median_superplot", "Select effect sizes to display:", choices = list("Mean difference", "Median difference"), multiple = FALSE)
                ),
              conditionalPanel(
                condition = "input.number_effect_sizes_superplot == 2",
              # choose hedges, cohen or glass delta
              selectInput("effect_sizes_hedges_cohen_glass_superplot", "Select effect sizes to display", choices = list("Hedges' g","Cohen's d", "Glass' delta"), multiple = FALSE)
              ),
              conditionalPanel(
                condition = "input.number_effect_sizes_superplot == 1 || input.number_effect_sizes_superplot == 2",
              selectInput("position_plot_mean_median_diff_superplot", "Select position for mean/median diff effect size:", choices = list("Aligned with mean/median", "Below data")),
            ),
            conditionalPanel(
              condition = "input.submit_plot_superplot > 0 && input.number_effect_sizes_superplot >=1",
              fluidRow(
                column(width = 7, div(actionButton("submit_superplot_with_effect_sizes", "Plot data with effect sizes", icon = icon("ranking-star")), style = "margin-right: 10px;")),
                column(width = 2, div(actionButton("reset_superplot_with_effect_sizes", "Clear plot with effect sizes", icon = icon("trash-can")), style = "margin-left: 10px;"))
              )
            )
           )
           ),
           
           # Main panel display
           mainPanel(
             tabsetPanel(
               id = "superplot_tab",
               tabPanel("Superplot",
                        conditionalPanel(condition = "input.submit_plot_superplot > 0",
                                         # renders plots
                                         # without effect size
                                         plotOutput("superplot_plot", height = 'auto', width = 'auto', brush = brushOpts("plot_brush_superplot_plot")),
                                         # with effect size
                                         withSpinner(plotOutput("superplot_plot_with_effect_sizes", height = 'auto', width = 'auto'), type = 5),
                                         # table for brushed points
                                         tableOutput("superplot_brush_selection"),
                        ),
                        # table for summary data (STILL WORKING ON IT)
                        tableOutput("data_superplot_rendered"),
                        # error message if plot is not displayed
                        verbatimTextOutput("superplot_error_message"),
                        # adds numeric inputs for plot
                        conditionalPanel(
                          condition = "input.submit_plot_superplot > 0", 
                          fluidRow(
                            column(
                              width = 2, 
                              numericInput(inputId = "height_plot_superplot", label = "Plot height (pixels)", value = 800)
                            ),
                            column(
                              width = 2, 
                              numericInput(inputId = "width_plot_superplot", label = "Plot width (pixels)", value = 1000)
                            )
                          ),
                          # download buttons for plot
                          downloadButton("download_superplot", "Download plot"),
                          # download buttons for plot and results
                          conditionalPanel(
                            condition = "input.submit_superplot_with_effect_sizes > 0",
                            downloadButton("download_superplot_effect_sizes", "Download plot with effect sizes"),
                            downloadButton("download_superplot_table_all", "Download plotted data"),
                          )
                        ),
                        #Modal for
                        conditionalPanel(
                          condition = "input.submit_plot_superplot > 0 || input.submit_superplot_with_effect_sizes > 0",
                          actionButton("options_superplot", label = "Plot Options", icon = icon("cog"))
                        ),
                        plot_modal("superplot"),
                        #removes additional close button
                        tags$head(tags$style("#modal_superplot .modal-footer{ display:none}")),
                        tableOutput("superplot_effect_size_table")
               )
             )
           )
  )
}