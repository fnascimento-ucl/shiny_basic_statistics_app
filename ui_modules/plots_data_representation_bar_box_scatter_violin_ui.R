##########################Plots for data representation module##################
################################################################################

shinyjs::useShinyjs()
plots_data_representation_bar_box_scatter_violin_ui <- function() {
  tabPanel("Box, Bar, Scatter, Violin and Raincloud",
           # Sidebar with input options
           sidebarPanel(
             #upload Excel file
              fileInput("data_file_box_bar_scatter_violin", "Choose a file with data"),
             # options for plotting data
             conditionalPanel(
               condition = "input.box_bar_scatter_violin_tab == 'Box, Bar, Scatter, Violin and Raincloud' && output.data_file_box_bar_scatter_violin_uploaded",
               h4("Plot"),
               # adds error message if anything goes wrong with the data selection
               verbatimTextOutput("error_message_box_bar_scatter_violin_data"),
               # select type of plot and variables
               selectInput(
                 inputId = "choose_box_bar_scatter_violin", 
                 label = "Select type of plot:", 
                 choices = list("Scatter",
                                         "Bar Chart",
                                         "Box-plot",
                                         "Violin",
                                         "Raincloud"),
                 multiple = FALSE, selected = FALSE),
               # for group_level selection
               selectInput("group_var_box_bar_scatter_violin", "Select the grouping variable", ""),
               # allows to filter group selection
               conditionalPanel(condition = "input.group_var_box_bar_scatter_violin.length > 0 && input.x_var_box_bar_scatter_violin.length > 0",
                                div(style = "margin-left: 20px;",
                                  selectInput(inputId = "group_order_box_bar_scatter_violin", label = "Select order and variables to show", choices = NULL, multiple = TRUE, selected = NULL)
                                )
               ),
               # select the number of hierarchical levels (1-4) in the data
               selectInput("num_levels_box_bar_scatter_violin", "Select the number of levels in your data", choices = c(1, 2, 3, 4)),
               div(style = "margin-left: 20px;",
                 conditionalPanel(
                   condition = "input.num_levels_box_bar_scatter_violin >= 1",
                   selectInput("x_var_box_bar_scatter_violin", "Select the column for level 1", "")
                 ),
                 conditionalPanel(
                   condition = "input.num_levels_box_bar_scatter_violin >= 2",
                   selectInput("y_var_box_bar_scatter_violin", "Select the column for level 2", "")
                 ),
                 conditionalPanel(
                   condition = "input.num_levels_box_bar_scatter_violin >= 3",
                   selectInput("z_var_box_bar_scatter_violin", "Select the column for level 3", "")
                 ),
                 conditionalPanel(
                   condition = "input.num_levels_box_bar_scatter_violin == 4",
                   selectInput("w_var_box_bar_scatter_violin", "Select the column for level 4", "")
                 )
                ),
               # for additional group_level selection and marginal plots
               conditionalPanel(condition = "input.group_var_box_bar_scatter_violin.length > 0 && input.x_var_box_bar_scatter_violin.length > 0",
                                checkboxInput(inputId = "show_additional_group_legend_box_bar_scatter_violin", label = "Select additional grouping variable", value = FALSE),
                                conditionalPanel(condition = "input.show_additional_group_legend_box_bar_scatter_violin == true",
                                                 div(style = "margin-left: 20px;",
                                                     selectInput("additional_variable_box_bar_scatter_violin", "Select the column for additional grouping variable", "")
                                                 )
                                ),
                                # additional variable selection
                                conditionalPanel(
                                  condition = "input.show_additional_group_legend_box_bar_scatter_violin == true && input.submit_plot_box_bar_scatter_violin > 0",
                                  div(style = "margin-left: 20px;",
                                      #for additional group selection
                                      selectInput(
                                        inputId = "additional_group_order_box_bar_scatter_violin",
                                        label = "Select order and variables to show",
                                        choices = NULL,
                                        multiple = TRUE,
                                        selected = NULL
                                      )
                                  )
                                ),
                                # adds box input for marginal plots for scatter graph
                                conditionalPanel(
                                  condition = "input.choose_box_bar_scatter_violin == 'Scatter'",
                                  h5("Scatter plot additional options"),
                                  div(style = "margin-left: 20px;",
                                      checkboxInput(inputId = "maginal_plot_box_bar_scatter_violin", label = "Show marginal distribution plots", value = FALSE),
                                      checkboxInput(inputId = "geom_smooth_scatter_box_bar_scatter_violin", label = "Show trendline", value = FALSE)
                                      ),
                                )
                              ),
               # action buttons for Plot
               fluidRow(
                 column(width = 7, div(actionButton("submit_plot_box_bar_scatter_violin", "Plot", icon = icon("chart-simple")), style = "margin-right: 10px;")),
                 column(width = 2, div(actionButton("reset_box_bar_scatter_violin", "Clear plot", icon = icon("trash")), style = "margin-left: 10px;"))
               )
             ),
               
               # bsTooltip(id ="group_order_box_bar_scatter_violin",
               #           title = "Bonjour!",
               #           placement = "right",
               #           trigger = "hover"),

            
             
             # bootstrapped effect size options
             conditionalPanel(
               condition = "input.box_bar_scatter_violin_tab == 'Box, Bar, Scatter, Violin and Raincloud' && output.data_file_box_bar_scatter_violin_uploaded && input.group_var_box_bar_scatter_violin.length > 0 && input.x_var_box_bar_scatter_violin.length > 0",
               hr(),
               h4('Effect size options'), 
               # adds choices for effect sizes
               selectInput("number_effect_sizes_box_bar_scatter_violin", "Number of effect sizes to display:", choices =c(0,1,2), multiple = FALSE, selected = 0),
               # allows to switch between hierarchical and simple bootstrap
               conditionalPanel(
                 condition = "input.number_effect_sizes_box_bar_scatter_violin == 1 || input.number_effect_sizes_box_bar_scatter_violin == 2",
                 radioButtons("bootstrap_option_box_bar_scatter_violin", "Choose type of bootstrap:",
                              choices = list ("Hierarchical" = "hierarchical_boot",
                                              "Simple Bootstrap" = "simple_boot"),
                              selected = "simple_boot"),
                 numericInput("n_boot_box_bar_scatter_violin", "Number of boostrap samples:", value = NULL),
                 # adds checkbox for paired observations
                 checkboxInput("repeated_observations_box_bar_scatter_violin", "Paired observations?", value = FALSE),
                   conditionalPanel(
                     condition = "input.repeated_observations_box_bar_scatter_violin == true",
                     selectInput("paired_observations_box_bar_scatter_violin", "Select column for paired observations", "")
                   ),
                 #select control and test groups
                 selectInput("control_group_box_bar_scatter_violin", "Select control group", choices = NULL, multiple = FALSE, selected = NULL),
                 selectInput("test_groups_box_bar_scatter_violin", "Select test group(s) and their order", choices = NULL, multiple = TRUE, selected = NULL)
               ),
               #choose median or mean effect size
               conditionalPanel(
                 condition = "input.number_effect_sizes_box_bar_scatter_violin == 1 || input.number_effect_sizes_box_bar_scatter_violin == 2",
                 selectInput("effect_size_mean_median_box_bar_scatter_violin", "Select effect sizes to display:", choices = list("Mean difference", "Median difference"), multiple = FALSE)
               ),
               
               #choose hedges, cohen or glass delta
               conditionalPanel(
                 condition = "input.number_effect_sizes_box_bar_scatter_violin == 2",
                 selectInput("effect_sizes_hedges_cohen_glass_box_bar_scatter_violin", "Select effect sizes to display", choices = list("Hedges' g","Cohen's d", "Glass' delta"), multiple = FALSE)
               ),
               # choose position of the effect size
               conditionalPanel(
                 condition = "input.number_effect_sizes_box_bar_scatter_violin == 1 || input.number_effect_sizes_box_bar_scatter_violin == 2",
                 selectInput("position_plot_mean_median_diff_box_bar_scatter_violin", "Select position for mean/median diff effect size:", choices = list("Aligned with mean/median", "Below data")),
               ),
               
               conditionalPanel(
                 condition = "input.submit_plot_box_bar_scatter_violin > 0 && input.number_effect_sizes_box_bar_scatter_violin >= 1",
                 fluidRow(
                   column(width = 7, div(actionButton("submit_box_bar_scatter_violin_with_effect_sizes", "Plot data with effect sizes", icon = icon("ranking-star")), style = "margin-right: 10px;")),
                   column(width = 2, div(actionButton("reset_box_bar_scatter_violin_with_effect_sizes", "Clear plot with effect sizes", icon = icon("trash-can")), style = "margin-left: 10px;"))
                 )
               )
             )
           ),
           # Main panel display
           mainPanel(
             tabsetPanel(
               id = "box_bar_scatter_violin_tab",
               tabPanel(
                 "Box, Bar, Scatter, Violin and Raincloud",
                 conditionalPanel(
                 condition = "input.submit_plot_box_bar_scatter_violin > 0",
                 # renders plots
                 # without effect size
                 plotOutput("box_bar_scatter_violin_plot", height = 'auto', width = 'auto', brush = brushOpts("plot_brush_box_bar_scatter_violin_plot")),
                 # with effect size
                 withSpinner(plotOutput("box_bar_scatter_violin_plot_with_effect_size", height = 'auto', width = 'auto'), type = 5),
                 # table for brushed points
                 tableOutput("box_bar_scatter_violin_brush_selection")
                 ),
                 # # table for summary data (STILL WORKING ON IT)
                 # tableOutput("data_box_bar_scatter_violin_plot_rendered"),
                 # error message if plot is not displayed
                 verbatimTextOutput("box_bar_scatter_violin_error_message"),
                 # adds numeric inputs for plot
                 conditionalPanel(
                   condition = "input.submit_plot_box_bar_scatter_violin > 0", 
                   fluidRow(
                     column(
                     width = 2, 
                     numericInput(inputId = "height_plot_box_bar_scatter_violin", label = "Plot height (pixels)", value = 800)
                   ),
                   column(
                     width = 2, 
                     numericInput(inputId = "width_plot_box_bar_scatter_violin", label = "Plot width (pixels)", value = 1000)
                    )
                   ),
                   # download buttons for plot
                   downloadButton("download_box_bar_scatter_violin", "Download plot"),
                   # download buttons for plot and results
                   conditionalPanel(
                     condition = "input.submit_box_bar_scatter_violin_with_effect_sizes > 0",
                     downloadButton("download_box_bar_scatter_violin_effect_sizes", "Download plot with effect sizes"),
                     downloadButton("download_box_bar_scatter_violin_table_all", "Download plotted data"),
                   )
                 ),
                 #Modal for
                 conditionalPanel(
                   condition = "input.submit_plot_box_bar_scatter_violin > 0 || input.submit_box_bar_scatter_violin_with_effect_sizes > 0",
                   actionButton("options_box_bar_scatter_violin", label = "Plot Options", icon = icon("cog"))
                   ),
                plot_modal("box_bar_scatter_violin"),
                 #removes additional close button
                 tags$head(tags$style("#modal_box_bar_scatter_violin .modal-footer{ display:none}")),
                tableOutput("box_bar_scatter_violin_effect_size_table")
               )
              )
           )
  )
}