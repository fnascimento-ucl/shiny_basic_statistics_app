################################################################################
#########################Modal for plots########################################
################################################################################

plot_modal <- function(tab_id) {
  
    #################MODAL STARTS###############################################
    ############################################################################
    bsModal(id = paste0("modal_", tab_id), title = "Plot options", trigger = paste0("options_", tab_id), size = "large",
            
            tabsetPanel(id = paste0("panels_options_", tab_id),
                        tabPanel("Lines and Ticks Box",
                                 fluidRow(
                                   #inputs for x axis
                                   column(width = 6,
                                          h3("X-axis"),
                                          numericInput(paste0("x_line_thickness_", tab_id), "Line thickness:", value = 1),
                                          colourInput(paste0("x_line_colour_", tab_id), "Line color:", value = "black"),
                                          numericInput(paste0("x_tick_thickness_", tab_id), "Major tick thickness:", value = 1),
                                          colourInput(paste0("x_tick_colour_", tab_id), "Ticks color:", value = "black"),
                                          numericInput(paste0("x_tick_length_", tab_id), "Major tick length:", value = 5),
                                          numericInput(paste0("x_tick_minor_length_", tab_id), "Minor Tick length:", value = 3),
                                          # shows x axis major and minor ticks interval selection for linear scale
                                          conditionalPanel(
                                            condition = paste0("input.x_scale_type_", tab_id, " == 'linear'"),
                                            numericInput(paste0("x_major_ticks_interval_", tab_id), "Major tick interval (set x axis range first):", value = NULL), min = 0.01,
                                            numericInput(paste0("x_minor_ticks_interval_", tab_id), "Minor tick interval (set x axis range first):", value = NULL, min = 0.01)
                                          ),
                                          numericInput(paste0("x_text_size_", tab_id), "Text size:", value = 12),
                                          colourInput(paste0("x_text_colour_", tab_id), "Text color:", value = "black"),
                                          selectInput(paste0("x_text_font_", tab_id), "Text font:", choices = c("Arial", "Times New Roman", "Helvetica", "Calibri", "Verdana", "Georgia", "Courier New", "Palatino Linotype"), selected = "Arial")
                                   ),
                                   #inputs for y axis
                                   column(width = 6,
                                          h3("Y-axis"),
                                          numericInput(paste0("y_line_thickness_", tab_id), "Line thickness:", value = 1),
                                          colourInput(paste0("y_line_colour_", tab_id), "Line color:", value = "black"),
                                          numericInput(paste0("y_tick_thickness_", tab_id), "Tick thickness:", value = 1),
                                          colourInput(paste0("y_tick_colour_", tab_id), "Tick color:", value = "black"),
                                          numericInput(paste0("y_tick_length_", tab_id), "Major tick length:", value = 5),
                                          numericInput(paste0("y_tick_minor_length_", tab_id), "Minor Tick length:", value = 3),
                                          # shows y axis major and minor ticks interval selection for linear scale
                                          conditionalPanel(
                                            condition = paste0("input.y_scale_type_", tab_id, " == 'linear'"),
                                            numericInput(paste0("y_major_ticks_interval_", tab_id), "Major tick interval (set y axis range first):", value = NULL, min = 0.01),
                                            numericInput(paste0("y_minor_ticks_interval_", tab_id), "Minor tick interval (set y axis range first):", value = NULL, min = 0.01)
                                          ),
                                          numericInput(paste0("y_text_size_", tab_id), "Text size:", value = 12),
                                          colourInput(paste0("y_text_colour_", tab_id), "Text color:", value = "black"),
                                          selectInput(paste0("y_text_font_", tab_id), "Text font:", choices = c("Arial", "Times New Roman", "Helvetica", "Calibri", "Verdana", "Georgia", "Courier New", "Palatino Linotype"), selected = "Arial")
                                   )
                                 )
                        ),
                        tabPanel("Scales Box",
                                 #enter numeric values for scales
                                 numericInput(paste0("x_start_range_", tab_id), "X-axis Start:", value = ""),
                                 numericInput(paste0("x_end_range_", tab_id), "X-axis End:", value = ""),
                                 # adds error message if x scale selection is not appropriate
                                 verbatimTextOutput(paste0("error_message_x_scale_", tab_id)),
                                 numericInput(paste0("y_start_range_", tab_id), "Y-axis Start:", value = ""),
                                 numericInput(paste0("y_end_range_", tab_id), "Y-axis End:", value = ""),
                                 # adds error message if y scale selection is not appropriate
                                 verbatimTextOutput(paste0("error_message_y_scale_", tab_id)),
                                 #buttons for type of scales X axis
                                 radioButtons(paste0("x_scale_type_", tab_id), "X-axis Scale Type:",
                                              choices = c("Linear" = "linear", "Log10" = "log10", "Log2" = "log2"),
                                              selected = "linear"),
                                 #buttons for type of scales Y axis
                                 radioButtons(paste0("y_scale_type_", tab_id), "Y-axis Scale Type:",
                                              choices = c("Linear" = "linear", "Log10" = "log10", "Log2" = "log2"),
                                              selected = "linear"),
                                 #allows selection of log scale display
                                 conditionalPanel(
                                   condition = paste0("input.x_scale_type_", tab_id, " == 'log10' || input.x_scale_type_", tab_id, " == 'log2' || input.y_scale_type_", tab_id, " == 'log10' || input.y_scale_type_", tab_id, " == 'log2'"),
                                   selectInput(paste0("numeric_display_type_y_axis_", tab_id), "Numeric display", choices = c("Decimal", "Scientific"))
                                 ),
                                 # input for scale breaks
                                 numericInput(paste0("x_num_breaks_", tab_id), "Number of X-axis Breaks:", value = NULL),
                                 textInput(paste0("x_break_values_", tab_id), "X-axis Break Values:", value = NULL),
                                 numericInput(paste0("y_num_breaks_", tab_id), "Number of Y-axis Breaks:", value = NULL),
                                 textInput(paste0("y_break_values_", tab_id), "Y-axis Break Values:", value = NULL)
                                 
                        ),
                        #options for customizing axis labels
                        tabPanel("Labels and Titles Box",
                                 fluidRow(
                                   #x axis label
                                   h5("X-axis"),
                                   column(width = 6,
                                          numericInput(inputId = paste0("x_axis_text_size_", tab_id), label = "Text size", value = 12),
                                          colourInput(inputId = paste0("x_axis_text_colour_", tab_id), label = "Select text colour", value = "black"),
                                          selectInput(inputId = paste0("x_axis_text_face_", tab_id), label = "Text face", choices = c("plain", "italic", "bold", "bold.italic"), selected = "plain"),
                                          selectInput(inputId = paste0("x_axis_text_font_", tab_id), label = "Text fount", choices = c("Arial", "Times New Roman", "Helvetica", "Calibri", "Verdana", "Georgia", "Courier New", "Palatino Linotype"), selected = "Arial"),
                                          sliderInput(inputId = paste0("x_axis_text_justification_", tab_id), label = "Text justification", min = 0, max =1, value =0.5),
                                          numericInput(inputId = paste0("x_axis_text_margin_", tab_id), label = "Margin", value = 10),
                                          textInput(inputId = paste0("x_axis_text_title_", tab_id), label = "Text title", value = "")
                                   ),
                                   #y axis label
                                   h5("Y-axis"),
                                   column(width = 6,
                                          numericInput(inputId = paste0("y_axis_text_size_", tab_id), label = "Text size", value = 12),
                                          colourInput(inputId = paste0("y_axis_text_colour_", tab_id), label = "Select text colour", value = "black"),
                                          selectInput(inputId = paste0("y_axis_text_face_", tab_id), label = "Text face", choices = c("plain", "italic", "bold", "bold.italic"), selected = "plain"),
                                          selectInput(inputId = paste0("y_axis_text_font_", tab_id), label = "Text fount", choices = c("Arial", "Times New Roman", "Helvetica", "Calibri", "Verdana", "Georgia", "Courier New", "Palatino Linotype"), selected = "Arial"),
                                          sliderInput(inputId = paste0("y_axis_text_justification_", tab_id), label = "Text justification", min = 0, max = 1, value = 0.5),
                                          numericInput(inputId = paste0("y_axis_text_margin_", tab_id), label = "Margin", value = 10),
                                          textInput(inputId = paste0("y_axis_text_title_", tab_id), label = "Text title", value = "")
                                   )
                                 )
                        ),
                        tabPanel("Symbols Box",
                                 #input to change symbol color
                                 column(width = 6,
                                        h3("Symbols"),
                                        #select colour group action button
                                        selectInput(inputId = paste0("geom_jitter_colour_fill_by_group_", tab_id), label = "", choices = c("Colour by Group - discrete", "Colour by Group - continuous", "Single Colour"), selected = "Single Colour"),
                                        verbatimTextOutput(paste0("symbol_gradient_error_", tab_id)),
                                        h6("Border colour"),
                                        #input for symbol colour
                                        colourInput(inputId = paste0("color_var_", tab_id), label = "Select border color", value = "black"),
                                        #UI input for colour symbol selection based on grouping variable
                                        div(
                                          style = "max-height: 300px; overflow-y: auto;",
                                          uiOutput(paste0("colour_symbol_inputs_", tab_id))
                                        ),
                                        #inputs for gradient colour for border and fill
                                        # for border colour
                                        colourInput(inputId = paste0("color_var_gradient_low_", tab_id), label = "Select low gradient border colour", value = "blue"),
                                        colourInput(inputId = paste0("color_var_gradient_high_", tab_id), label = "Select high gradient border colour", value = "white"),
                                        h6("Fill colour"),
                                        #input to change symbol color fill
                                        colourInput(inputId = paste0("symbol_fill_", tab_id), label = "Select fill color", value = "black"),
                                        #UI input for colour symbol selection based on grouping variable
                                        div(
                                          style = "max-height: 300px; overflow-y: auto;",
                                          uiOutput(paste0("colour_symbol_fill_inputs_", tab_id))
                                          ),
                                        #inputs for gradient colour for border and fill
                                        # for symbol fill
                                        colourInput(inputId = paste0("symbol_fill_gradient_low_", tab_id), label = "Select low gradient fill colour", value = "blue"),
                                        colourInput(inputId = paste0("symbol_fill_gradient_high_", tab_id), label = "Select high gradient fill colour", value = "white"),
                                        #input to change symbol shape
                                        selectInput(inputId = paste0("symbol_shape_", tab_id), label = "Shape", choices = list(
                                          "Basic Shapes" = c(
                                            "square" = 0, "circle" = 1, "triangle point up" = 2,
                                            "cross" = 4, "diamond" = 5, "triangle point down" = 6,
                                            "square cross" = 7, "star" = 8, "diamond plus" = 9,
                                            "circle plus" = 10, "triangles up and down" = 11, "square plus" = 12,
                                            "circle cross" = 13, "square and triangle up" = 14
                                          ),
                                          "Filled Shapes" = c(
                                            "filled square" = 15,
                                            "filled circle" = 16,
                                            "filled triangle point up" = 17,
                                            "filled diamond" = 19,
                                            "solid circle" = 19,
                                            "bullet" = 20
                                          ),
                                          "Colored Shapes" = c(
                                            "filled color circle" = 21,
                                            "filled color square" = 22,
                                            "filled color diamond" = 23,
                                            "filled color triangle point up" = 24,
                                            "filled color triangle point down" = 25
                                          )
                                        ),
                                        selected = 21),
                                        #input to change symbol size
                                        numericInput(inputId = paste0("symbol_size_", tab_id), label = "Size", value = 1.5),
                                          # symbol size for superplot large dots
                                          div(style = "margin-left: 20px;",
                                              numericInput(inputId = paste0("symbol_size_superplot_", tab_id), label = "Size for mean/median dot", value = 5)
                                            ),
                                        #input to change symbol edge thickness
                                        numericInput(inputId = paste0("symbol_edge_thickness_", tab_id), label = "Edge thickness", value = 1),
                                          # symbol edge thickness for superplot large dots
                                        div(style = "margin-left: 20px;",
                                              numericInput(inputId = paste0("symbol_edge_thickness_superplot_", tab_id), label = "Edge thickness for mean/median dot", value = 1.5)
                                            ),
                                        #inputs to change symbol jitter
                                        numericInput(inputId = paste0("symbol_jitter_", tab_id), label = "Jitter", value = 0.4, step = 0.1),
                                          # symbol jitter for superplot large dots
                                        div(style = "margin-left: 20px;",
                                              numericInput(inputId = paste0("symbol_jitter_superplot_", tab_id), label = "Jitter for mean/median dot", value = 0.4, step = 0.1)
                                            ),
                                        #slider for symbol transparency
                                        sliderInput(inputId = paste0("symbol_transparency_", tab_id), label = "Transparency", min = 0, max = 1, value = 1),
                                          # symbol transparency for superplot large dots
                                          div(style = "margin-left: 20px;",
                                            sliderInput(inputId = paste0("symbol_transparency_superplot_", tab_id), label = "Transparency for mean/median dot", min = 0, max = 1, value = 1)
                                            ),
                                        # for raincloud plot dots
                                        conditionalPanel(
                                          condition = paste0("input.choose_", tab_id, " == 'Raincloud'"),
                                          selectInput(inputId = paste0("stat_dots_side_", tab_id), label = "Select side for dot display", choices = c("left", "bottomleft", "topleft", "bottom", "right", "bottomright", "topright", "both"), selected = "left"),
                                          numericInput(inputId = paste0("stat_dots_bindwidth_", tab_id), label = "Select dot binwidth", value = 2, step = 1),
                                          numericInput(inputId = paste0("stat_dots_justification_", tab_id), label = "Dot justification", value = 1.4, step = 0.1)
                                        )
                                 ),
                                 column(width=6,
                                        #adds geom_smooth if selected
                                        conditionalPanel(
                                          condition = paste0("input.geom_smooth_scatter_", tab_id, " == true && input.choose_", tab_id, " == 'Scatter'"),
                                          #condition = paste0("input.show_additional_group_legend_", tab_id, " == true"),
                                          
                                          h3("Trendline"),
                                                         selectInput(inputId = paste0("method_geom_smooth_scatter_", tab_id), label = "Method", choices = c("Linear" = "lm", "General linear model" = "glm", "Generalized Additive Model" = "gam", "Locally Weighted Least Squares Regression" = "loess"), selected = "loess"),
                                                         numericInput(inputId = paste0("line_thickness_geom_smooth_scatter_", tab_id), label = "Line thickness", value = 1, step =0.1),
                                                         selectInput(inputId = paste0("line_type_geom_smooth_scatter_", tab_id), label = "Line type", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid"),
                                                         numericInput(inputId = paste0("confidence_interval_geom_smooth_scatter_", tab_id), label = "Confidence interval", value = 0.95, step = 0.01),

                                                         conditionalPanel(
                                                           condition = paste0("input.method_geom_smooth_scatter_", tab_id, " == 'loess'"),
                                                           numericInput(inputId = paste0("span_geom_smooth_scatter_", tab_id), label = "Smoothing", value = 0.3, step =0.05)
                                                         ),
                                                         sliderInput(inputId = paste0("transparency_geom_smooth_scatter_", tab_id), label = "Confidence interval transparency", min = 0, max = 1, value = 0.3),
                                                         conditionalPanel(
                                                           condition = paste0("input.show_additional_group_legend_", tab_id, " == false"),
                                                           colourInput(inputId = paste0("line_colour_geom_smooth_scatter_", tab_id), label = "Select line colour", value = "purple"),
                                                                          colourInput(inputId = paste0("fill_colour_geom_smooth_scatter_", tab_id), label = "Select line colour", value = "grey"),
                                                         ),
                                          h3("Correlation coefficient"),
                                          # for correlation coefficient
                                          checkboxInput(inputId = paste0("correlation_coefficient_show_legend_", tab_id), label = "Show coefficient", value = TRUE),
                                          selectInput(inputId = paste0("correlation_coefficient_method_", tab_id), label = "Type of correlation test", choices = c("spearman", "pearson", "kendall"), selected = "spearman"),
                                          selectInput(inputId = paste0("correlation_coefficient_name_", tab_id), label = "Type of correlation coefficient", choices = c("R", "rho", "tau"), selected = "rho"),
                                          # for defining position of the legend
                                            column(width = 6,
                                                   selectInput(inputId = paste0("correlation_coefficient_legend_position_x_", tab_id), label = "horizontal vector",  choices = c('right', 'left', 'center', 'middle'), selected = 'left')
                                            ),
                                            column(width = 6,
                                                   selectInput(inputId = paste0("correlation_coefficient_legend_position_y_", tab_id), label = "vertical vector", choices = c( 'bottom', 'top', 'center', 'middle'), selected = 'top')
                                            ),
                                          numericInput(inputId = paste0("correlation_coefficient_text_size_", tab_id), label = "Text size", value = 4),
                                          selectInput(inputId = paste0("correlation_coefficient_text_font_", tab_id), label = "Text fount", choices = c("Arial", "Times New Roman", "Helvetica", "Calibri", "Verdana", "Georgia", "Courier New", "Palatino Linotype"), selected = "Arial")
                                          ),
                                        #for box plots and bar chart
                                        conditionalPanel(condition = paste0("input.submit_plot_", tab_id," > 0 && (input.choose_", tab_id, " == 'Box-plot' || input.choose_", tab_id, " == 'Raincloud' || input.choose_", tab_id, " == 'Bar Chart' || input.choose_", tab_id, " == 'Violin')"),
                                                         conditionalPanel(condition = paste0("input.choose_", tab_id, " == 'Box-plot' || input.choose_", tab_id, " == 'Raincloud'"), h3("Box-plot")),
                                                         conditionalPanel(condition = paste0("input.choose_", tab_id, " == 'Bar Chart'"), h3("Bar")),
                                                         conditionalPanel(condition = paste0("input.choose_", tab_id, " == 'Violin'"), h3("Violin")),
                                                         #select colour group action button
                                                         selectInput(inputId = paste0("geom_boxplot_colour_fill_by_group_", tab_id), label = "", choices = c("Colour by Group", "Single Colour"), selected = "Single Colour"),
                                                         h6("Border colour"),
                                                         #input for box line colour
                                                         colourInput(inputId = paste0("box_line_", tab_id), label = "Box line colour", value = "black"),
                                                         #UI input for colour boxplot selection based on grouping variable
                                                         div(
                                                           style = "max-height: 300px; overflow-y: auto;",
                                                           uiOutput(paste0("colour_boxplot_line_inputs_", tab_id))
                                                           ),
                                                         h6("Fill colour"),
                                                         #input for box fill colour
                                                         colourInput(inputId = paste0("box_fill_", tab_id), label = "Box fill colour", value = "lightblue"),
                                                         #UI input for colour boxplot selection based on grouping variable
                                                         div(
                                                           style = "max-height: 300px; overflow-y: auto;",
                                                           uiOutput(paste0("colour_boxplot_fill_inputs_", tab_id))
                                                           ),
                                                         numericInput(inputId = paste0("box_line_thickness_", tab_id), label = "Box line thickness", value = 1, step = 0.1),
                                                         conditionalPanel(
                                                           condition = paste0("input.choose_", tab_id, " != 'Raincloud'"),
                                                           numericInput(inputId = paste0("box_width_", tab_id), label = "Box width", value = 0.8, step = 0.1)
                                                         ),
                                                         conditionalPanel(
                                                           condition = paste0("input.choose_", tab_id, " == 'Raincloud'"),
                                                           numericInput(inputId = paste0("box_raincloud_width_", tab_id), label = "Box width", value = 0.2, step = 0.1)
                                                         ),
                                                         sliderInput(inputId = paste0("box_transparency_", tab_id), label = "Box transparency", min = 0, max = 1, value = 0.8),
                                                         conditionalPanel(condition = paste0("input.choose_", tab_id, " == 'Bar Chart' || input.choose_", tab_id, " == 'Violin'"),
                                                                          radioButtons(inputId = paste0("sd_se_bar_chart_", tab_id), label = "Select errorbar display", choices = c("Standard deviation", "Standard error of mean", "None"))
                                                         )
                                        )
                                 )
                        ),
                        #options for customizing plot and panel background
                        tabPanel("Background Box",
                                 column(width = 6,
                                        h3("Plot background and panel"),
                                        colourInput(inputId = paste0("colour_background_", tab_id), label = "Background colour", value = "white"),
                                        colourInput(inputId = paste0("colour_background_border_", tab_id), label = "Background border colour", value = "white"),
                                        numericInput(inputId = paste0("colour_background_border_thickness_", tab_id), label = "Background border thickness", value = 0.5),
                                        colourInput(inputId = paste0("colour_panel_", tab_id), label = "Panel colour", value = "white")
                                 ),
                                 column(width = 6,
                                        h3("X axis major and minor grids"),
                                        colourInput(inputId = paste0("colour_major_x_grid_", tab_id), label = "X axis major grid colour", value = "white"),
                                        numericInput(inputId = paste0("thickness_major_x_grid_", tab_id), label = "X axis major grid thickness", value = 0.2),
                                        colourInput(inputId = paste0("colour_minor_x_grid_", tab_id), label = "X axis minor grid colour", value = "white"),
                                        numericInput(inputId = paste0("thickness_minor_x_grid_", tab_id), label = "X axis minor grid thickness", value = 0.1),
                                        h3("Y axis major and minor grids"),
                                        colourInput(inputId = paste0("colour_major_y_grid_", tab_id), label = "Y axis major grid colour", value = "white"),
                                        numericInput(inputId = paste0("thickness_major_y_grid_", tab_id), label = "Y axis major grid thickness", value = 0.2),
                                        colourInput(inputId = paste0("colour_minor_y_grid_", tab_id), label = "Y axis minor grid colour", value = "white"),
                                        numericInput(inputId = paste0("thickness_minor_y_grid_", tab_id), label = "Y axis minor grid thickness", value = 0.1),
                                 )
                        ),
                        #options for customizing marginal Plots for Scatter
                        tabPanel(title = "Legend",
                                   #uiOutput(paste0("legend_marginal_plot_title_", tab_id)),
                                 column(width = 6,
                                        h3(""),
                                        # checkbox for legend outside main plot for effect size plot_grid
                                        checkboxInput(inputId = paste0("legend_outside_main_plot_", tab_id), label = "Display legend outside main plot?", value = FALSE),
                                        checkboxGroupInput(inputId = paste0("legends_selected_", tab_id), label = "Select legends to show", choices = c("Dots", "Box", "Violin", "Bar", "Cloud"), selected = c("Dots", "Box", "Violin", "Bar", "Cloud")),

                                          selectInput(inputId = paste0("maginal_plot_legend_", tab_id), label = "Legend position", choices = c("none", "left","top", "right", "bottom", "custom"), selected = "none"),
                                          conditionalPanel(
                                            condition = paste0("input.maginal_plot_legend_", tab_id, " == 'custom'"),
                                            column(width = 6,
                                                   numericInput(inputId = paste0("marginal_plot_legend_position_x_", tab_id), label = "horizontal vector", value = 1, step = 0.1)
                                            ),
                                            column(width = 6,
                                                   numericInput(inputId = paste0("marginal_plot_legend_position_y_", tab_id), label = "vertical vector", value = 1, step = 0.1)
                                            )
                                          )
                                        ),
                                 column(width = 6,
                                        conditionalPanel(
                                          condition = paste0("input.maginal_plot_legend_", tab_id, " != 'none'"),
                                          div(style = "margin-left: 20px;",
                                              
                                          ),
                                          checkboxInput(inputId = paste0("marginal_plot_legend_title_", tab_id), label = "Show legend title", value = TRUE),
                                          numericInput(inputId = paste0("marginal_plot_legend_text_size_", tab_id), label = "Text size", value = 12),
                                          selectInput(inputId = paste0("marginal_plot_legend_text_face_", tab_id), label = "Text face", choices = c("plain", "italic", "bold", "bold.italic"), selected = "plain"),
                                          selectInput(inputId = paste0("marginal_plot_legend_text_font_", tab_id), label = "Text fount", choices = c("Arial", "Times New Roman", "Helvetica", "Calibri", "Verdana", "Georgia", "Courier New", "Palatino Linotype"), selected = "Arial")
                                        )
                                 )
                        ),
                        #for marginal plots
                        tabPanel(title = "Marginal Plots",
                                  column(width = 6,
                                   selectInput(inputId = paste0("marginal_plot_type_", tab_id), label = "Choose type of marginal plot", choices = c("density", "histogram", "boxplot", "violin", "densigram"), selected = "density"),
                                   selectInput(inputId = paste0("marginal_plot_margins_", tab_id), label = "Choose axis to display plots", choices = c("x", "y", "both"), selected = "both"),
                                   numericInput(inputId = paste0("marginal_plot_size_", tab_id), label = "Marginal plot size", value = 10, step = 1),
                                   #selectInput(inputId = paste0("marginal_plot_legend_", tab_id), label = "Legend position", choices = c("none", "left","top", "right", "bottom", "custom"), selected = "none"),
                                   conditionalPanel(
                                     condition = paste0("input.marginal_plot_legend_", tab_id, " == 'custom'"),
                                     column(width = 6,
                                            numericInput(inputId = paste0("marginal_plot_legend_position_x_", tab_id), label = "horizontal vector", value = 1, step = 0.1)
                                     ),
                                     column(width = 6,
                                            numericInput(inputId = paste0("marginal_plot_legend_position_y_", tab_id), label = "vertical vector", value = 1, step = 0.1)
                                     )
                                   )
                                ),
                        column(width = 6,
                               conditionalPanel(
                                 condition = paste0("input.marginal_plot_margins_", tab_id, " == 'x' || input.marginal_plot_margins_", tab_id, " == 'both'"),
                                 h3("X marginal plot"),
                                 conditionalPanel(
                                   condition = paste0("input.marginal_plot_type_", tab_id, " == 'histogram' || input.marginal_plot_type_", tab_id, " == 'densigram'"),
                                   numericInput(inputId = paste0("marginal_plot_bindwidth_x_", tab_id), label = "Select binwidth", value = 10)
                                 ),
                                 conditionalPanel(
                                   condition = paste0("input.show_additional_group_legend_", tab_id, " == false && input.maginal_plot_", tab_id, " == true"),
                                   colourInput(inputId = paste0("marginal_plot_border_colour_x_", tab_id), label = "Choose line colour", value = "black"),
                                   colourInput(inputId = paste0("marginal_plot_fill_colour_x_", tab_id), label = "Choose fill colour", value = "lightgray")
                                 ),
                                   numericInput(inputId = paste0("marginal_plot_line_width_x_", tab_id), label = "Line width", value = 1, step = 0.1),
                                   sliderInput(inputId = paste0("marginal_plot_transparency_x_", tab_id), label = "Transparency", min = 0, max = 1, value = 0.5)
                                 ),
                                 conditionalPanel(
                                   condition = paste0("input.marginal_plot_margins_", tab_id," == 'y' || input.marginal_plot_margins_", tab_id, " == 'both'"),
                                   h3("Y marginal plot"),
                                   conditionalPanel(
                                     condition = paste0("input.marginal_plot_type_", tab_id, " == 'histogram' || input.marginal_plot_type_", tab_id, " == 'densigram'"),
                                     numericInput(inputId = paste0("marginal_plot_bindwidth_y_", tab_id), label = "Select binwidth", value = 10)
                                   ),
                                   conditionalPanel(
                                     condition = paste0("input.show_additional_group_legend_", tab_id, " == false && input.maginal_plot_", tab_id, " == true"),
                                     colourInput(inputId = paste0("marginal_plot_border_colour_y_", tab_id), label = "Choose line colour", value = "black"),
                                     colourInput(inputId = paste0("marginal_plot_fill_colour_y_", tab_id), label = "Choose fill colour", value = "lightgray")
                                   ),
                                   numericInput(inputId = paste0("marginal_plot_line_width_y_", tab_id), label = "Line width", value = 1, step = 0.1),
                                   sliderInput(inputId = paste0("marginal_plot_transparency_y_", tab_id), label = "Transparency", min = 0, max = 1, value = 0.5)
                                 )
                              )
                            ),
                        # for raincloud plot               
                        tabPanel(title = "Raincloud halfeyes",
                                 numericInput(inputId = paste0("raincloud_halfeye_width_", tab_id), label = "Cloud width", value = 0.5, step = 0.1),
                                 numericInput(inputId = paste0("raincloud_halfeye_line_thickness_", tab_id), label = "Bandwith", value = 0.5, step = 0.1),
                                 numericInput(inputId = paste0("raincloud_halfeye_justification_", tab_id), label = "Justification", value = -0.3, step = 0.1),
                                 sliderInput(inputId = paste0("raincloud_halfeye_transparency_", tab_id), label = "Transparency", min = 0, max = 1, value = 0.5)
                        ),
                        # for effect size plots
                        tabPanel(title = "Effect size plots",
                                 fluidRow(
                                   column(width = 6,
                                          h3("Distribution"),
                                          colourInput(inputId = paste0("effect_size_distribution_fill_colour_", tab_id), label = "Fill colour", value = "#FDC1C1"),
                                          colourInput(inputId = paste0("effect_size_distribution_line_colour_", tab_id), label = "Line colour", value = "#FDC1C1"),
                                          sliderInput(inputId = paste0("effect_size_distribution_transparency_", tab_id), label = "Transparency", min = 0, max = 1, value = 0.5),
                                          selectInput(inputId = paste0("effect_size_distribution_line_type_", tab_id), label = "Line type", 
                                                      choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "blank"),
                                          numericInput(inputId = paste0("effect_size_distribution_line_width_", tab_id), label = "Line width", value = 1, step = 0.1),
                                          numericInput(inputId = paste0("effect_size_distribution_bandwith_adjustment_", tab_id), label = "Bandwith multiplier", value = 1, step = 0.1),
                                          # adds optional lines connecting mean to axis
                                          checkboxInput(inputId = paste0("effect_size_line_per_group_", tab_id), label = "Show line connecting mean(s) to Y axis?", value = FALSE),
                                          colourInput(inputId = paste0("effect_size_line_per_group_colour_", tab_id), label = "Line colour", value = "#FDC1C1"),
                                          sliderInput(inputId = paste0("effect_size_line_per_group_transparency_", tab_id), label = "Transparency", min = 0, max = 1, value = 0.5),
                                          selectInput(inputId = paste0("effect_size_line_per_group_type_", tab_id), label = "Line type", 
                                                      choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "blank")             
                                   ),
                                   column(width = 6,
                                          h3("Mean/Median Dot"),
                                          colourInput(inputId = paste0("effect_size_mean_colour_", tab_id), label = "Dot colour", value = "black"),
                                          numericInput(inputId = paste0("effect_size_mean_dot_size_", tab_id), label = "Dot size", value = 3, step = 1),
                                          h3("Confidence Interval line"),
                                          colourInput(inputId = paste0("effect_size_CI_colour_", tab_id), label = "Line colour", value = "black"),
                                          numericInput(inputId = paste0("effect_size_CI_line_width_", tab_id), label = "Line width", value = 1, step = 0.1),
                                          h3("Zero line"),
                                          colourInput(inputId = paste0("effect_size_zero_colour_", tab_id), label = "Line colour", value = "black"),
                                          selectInput(inputId = paste0("effect_size_zero_line_type_", tab_id), label = "Line type", 
                                                      choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "dotted"),
                                          numericInput(inputId = paste0("effect_size_zero_line_width_", tab_id), label = "Line width", value = 0.5, step = 0.1)
                                   )
                                 )
                        )
            ),
            tabsetPanel(
              footer = tagList(
                actionButton(inputId = paste0("update_options_", tab_id), label = "Update", icon = icon("refresh")),
                downloadButton(paste0("save_options_", tab_id), "Save options"),
                tags$div(
                  style = "display: none;",
                  fileInput(paste0("load_options_", tab_id), label = NULL, multiple = FALSE, accept = ".rds")
                ),
                actionButton(inputId = paste0("load_options_click_", tab_id), label = "Load", icon = icon("folder-open")),
                modalButton(label = "Close", icon = icon("times"))
              )
            )
  )
    ##end of modal
}