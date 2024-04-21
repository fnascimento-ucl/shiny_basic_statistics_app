################################################################################                                   
################################################################################

#load packages
#source("C:/Users/filip/Documents/Basic_statistics_for_basic_neuroscientists/packages.R")
source("packages.R")

################################################################################                                   
################################################################################

# User Interface (UI)
ui <- bootstrapPage(
  shinyjs::useShinyjs(),
  useShinyToastify(),
  shinyFeedback::useShinyFeedback(),
    # adds start up spinner for when the app is loaded
    busy_start_up(
      loader = tags$img(
        #src = "https://media0.giphy.com/media/v1.Y2lkPTc5MGI3NjExMjNwMWVpZ3lqdG13aWR2azhkbGg1d2FxODJqNm5uc3FxdGE5eGptMyZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/xT1XGN5XmSJyqv3vJ6/giphy.gif",
        src='ghipy_neuron.webp',
        width = 480,
        height = 270
      ),
    text = "Loading...",
    timeout = 3000,
    color = "#FFF",
    background = "black"
  ),
  # navigation page with title and theme
  navbarPage(
    title = HTML(paste0('<i class="fa fa-brain" aria-hidden="true"></i> Basic Statistics for Basic Neuroscientists')),
    id = "navpage",
    collapsible = TRUE,
    fluid = TRUE,
    theme = shinytheme("yeti"),
    windowTitle = "Basic statistics for basic neuroscientists",
    
    #first selectable Tab for data selection
    navbarMenu("Data Overview",
               data_overview_ui()
    ),
    # second selectable tab for ICC and LMM calculation
    navbarMenu("Variance Components Explorer",
               tools_variability_ui(),
    ),
    # third selectable tab for data plot generation with effect sizes
    navbarMenu("Plots with effect sizes",
              plots_data_representation_superplot_ui(),
              plots_data_representation_bar_box_scatter_violin_ui()
    ),
    # fourth selected tab for information about the app
    navbarMenu("About",
               app_overview_ui(),
               contact_ui()
    )
  )
)
                                  
################################################################################                                   
################################################################################

# Define server logic
server <- function(input, output, session) {
  
  ##############################################################################
  #For generating a dynamic table in "Data selection and visualization"
    # Read in data from excel or csv file
    data_table <- reactive({
      req(input$data_file)
        # Determine the file extension
        file_ext <- tools::file_ext(input$data_file$name)
        if (tolower(file_ext) == "xlsx" || tolower(file_ext) == "xls") {
          # Read a sheet from an Excel file
            # finds names of sheets
            # sheet_names <- excel_sheets(input$data_file$datapath)
            # 
            # # updates select input for sheet selection
            # updateSelectInput(session, "selected_sheet_table", choices = sheet_names, selected = sheet_names[1])
            # 
            # if (!any(input$selected_sheet_table %in% sheet_names)){
            #   
            # }
            # # input for selected sheet
            # selected_sheet <- input$selected_sheet_table
            # 
            # read_excel(input$data_file$datapath, sheet = selected_sheet) %>%
              read_excel(input$data_file$datapath) %>%
              mutate_if(is.character, as.factor) # Convert character columns to factor
        } else if (tolower(file_ext) == "csv") {
          # Read a CSV file
          read.csv(input$data_file$datapath) %>%
            mutate_if(is.character, as.factor) # Convert character columns to factor
        } else {
          # Display a message for unsupported file types
          output$uploaded_file_extension_error <- renderText({"Wrong file uploaded, please upload an Excel (.xlsx or .xls) or CSV (.csv) file"})
        }
    })
  
    # resets plot and display options everytime a new file is uploaded
    observeEvent(input$data_file,{
      # removes previous plot
      output$table_plot <- renderPlot({NULL})
      # hides numeric inputs for plot size
      shinyjs::hide("height_plot_table")
      shinyjs::hide("width_plot_table")
      
      # hides brushoptions
      shinyjs::hide("table_brush_selection")
      
      # restores other panel inputs
      updateCheckboxInput(session, "show_additional_group_legend_table", value = FALSE)
      updateCheckboxInput(session, "repeated_observations_table", value = FALSE)
      updateCheckboxInput(session, "maginal_plot_table", value = FALSE)
      updateCheckboxInput(session, "geom_smooth_table", value = FALSE)
      shinyjs::reset("additional_group_order_table")
      shinyjs::reset("group_order_table")
      
      
      # disables options
      shinyjs::disable("options_table")
    })
    
    # hides picker input until file is uploaded
    output$data_file_uploaded <- reactive({
      return(!is.null(input$data_file))
      })
    
    outputOptions(output, 'data_file_uploaded', suspendWhenHidden=FALSE)
    
    # Update variable selection dropdown and menu display when correct data_file is uploaded
    observe({
        req(data_table())
        updatePickerInput(session, "var_select", choices = colnames(data_table()))
        if (is.data.frame(data_table())){
          output$uploaded_file_extension_error <- renderText({NULL})
          shinyjs::show("var_select")
          shinyjs::show("plot_type")
          shinyjs::show("x-axis")
          shinyjs::show("y-axis")
        } else if (!is.data.frame(data_table())) {
          shinyjs::hide("var_select")
          shinyjs::hide("plot_type")
          shinyjs::hide("x_axis")
          shinyjs::hide("y_axis")
        }
      })
    
    # Create reactive data for selected variables
    selected_vars <- reactive({
      req(data_table(), input$var_select, input$data_file)
      #check if  selected variables are in uploaded data (avoids error when new file is uploaded)
      if (all(input$var_select %in% colnames(data_table()))) {
        selected_data <- data_table() %>%
          dplyr::select(input$var_select)
      } else {
        return(0)
      }
    })
    
    # Create edited data for plot and table update when filters are used
    edited_data <- reactiveVal()
    
    # Datatable coerce function
    myCoerceValue <- function(x, y) {
      if (is.factor(y)) {
        if (x %in% levels(y)) {
          factor(x, levels = levels(y))
        } else {
          factor(c(levels(y), x), levels = NULL)
        }
      } else if (is.integer(y)) {
        as.integer(x)
      } else {
        x
      }
    }
    
    # Edit data table update for individual cell changes
    observeEvent(input$data_table_cell_edit, {
      info <- input$data_table_cell_edit
      i <- info$row
      j <- info$col + 1
      v <- info$value
      new_edited_data <- edited_data()
      if (is.null(new_edited_data)) {
        # initialize edited_data if it is null
        new_edited_data <- data_table()
      }
      #converts factor to character to allow coercion
      new_edited_data <- as.data.frame(lapply(new_edited_data, function(x) if(is.factor(x)) as.character(x) else x))
      new_edited_data[i, j] <- myCoerceValue(v, new_edited_data[i, j])
      #converts characters to factors before updating table
      new_edited_data <- as.data.frame(lapply(new_edited_data, function(x) if(is.character(x)) as.factor(x) else x))
      edited_data(new_edited_data)
    })
    
    # Render data table
    output$data_table <- renderDT({
      req(data_table(), selected_vars)
      if (!is.null(input$var_select) && all(input$var_select %in% colnames(data_table()))) {
        selected_cols <- c(match(input$var_select, colnames(data_table())))
        if (!is.null(edited_data())) {
          # if edited_data is not NULL, display edited_data
          datatable(edited_data(), 
                    options = list(pageLength = as.numeric(input$page_len), #number of rows to display
                                   fixedHeader = TRUE, # header stays 
                                   autoFill = TRUE, # allows automatic filling
                                   lengthChange = TRUE, # changes the length of the columns displayed
                                   searchHighlight=TRUE, # search options
                                   dom = 'Bfrtip',
                                   buttons = list(
                                     list(
                                       extend = 'collection',
                                       buttons = c('copy', 'print', 'excel', 'csv', 'pdf'), # buttons for export options
                                       text = 'Export options',
                                       align = 'left'
                                     ),
                                     list(
                                       extend = 'collection',
                                       buttons = c('selectAll', 'selectNone', 'selectRows', 'selectColumns', 'selectCells'), # buttons for selection options
                                       text = 'Selection options',
                                       align = 'left'
                                     )
                                   ),
                                   colReorder = TRUE, # allows to move order of the columns
                                   keys = TRUE, #allows navigation cell by cell (like Excel)
                                   scrollX = TRUE, #allows to scroll horizontally
                                   fixedColumns = TRUE #allows to scroll horizontally
                    ),
                    rownames = FALSE,
                    filter = 'top', # search filter located on top
                    class = "cell-border stripe",
                    extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'AutoFill', 'KeyTable', 'RowReorder', 'Scroller', 'FixedColumns', 'Select'),
                    selection = "none", #none to allow 'Select' extension to run
                    editable = TRUE)
        } else {
          # if edited_data is NULL, display original data
          datatable(data_table()[, selected_cols], 
                    options = list(pageLength = as.numeric(input$page_len), 
                                   select = list(style = 'multiple', items = 'row'),
                                   fixedHeader = TRUE,
                                   autoFill = TRUE, 
                                   lengthChange = TRUE, 
                                   searchHighlight=TRUE,
                                   dom = 'Bfrtip',
                                   buttons = list(
                                     list(
                                       extend = 'collection',
                                       buttons = c('copy', 'print', 'excel', 'csv', 'pdf'),
                                       text = 'Export options',
                                       align = 'left'
                                     ),
                                     list(
                                       extend = 'collection',
                                       buttons = c('selectAll', 'selectNone', 'selectRows', 'selectColumns', 'selectCells'),
                                       text = 'Selection options',
                                       align = 'left'
                                     )
                                   ),
                                   colReorder = TRUE,
                                   keys = TRUE, #allows navigation cell by cell (like Excel)
                                   scrollX = TRUE, #allows to scroll horizontally
                                   fixedColumns = TRUE #allows to scroll horizontally 
                    ),
                    rownames = FALSE,
                    filter = 'top',
                    class = "cell-border stripe",
                    extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'AutoFill', 'KeyTable', 'RowReorder', 'Scroller', 'FixedColumns', 'Select'),
                    selection = "none",
                    editable = TRUE)
        }
      }
    }, server = FALSE)
    
  #Plotting filtered and unfiltered data underneath reactive table in "Table" tab
    # Reactive data for selected variables for plotting
    selected_vars_plot <- reactive({
      #checks if names of variables are in data (helps to avoid error when new file is uploaded)
      validate(
        need(!is.null(data_table()) && !is.null(input$x_axis) && !is.null(input$y_axis) &&
               (input$x_axis %in% names(data_table()) || input$x_axis %in% names(edited_data())) &&
               (input$y_axis %in% names(data_table()) || input$y_axis %in% names(edited_data())), label = ""
        )
      )
      
      #gets variables from selection
      if (is.null(edited_data()) && input$y_var_table != "" && input$y_var_table %in% names(data_table())) {
        data <- data_table()[input$data_table_rows_all, ] %>%
          dplyr::select(input$x_axis, input$y_axis, input$y_var_table)
        names(data) <- c(input$x_axis, input$y_axis, input$y_var_table)
      } else if (!is.null(edited_data()) && input$y_var_table != "" && input$y_var_table %in% names(edited_data())){
        data <- edited_data()[input$data_table_rows_all, ] %>%
          dplyr::select(input$x_axis, input$y_axis, input$y_var_table)
        names(data) <- c(input$x_axis, input$y_axis, input$y_var_table)
      } else if (is.null(edited_data()) && (input$y_var_table == "" || !input$y_var_table %in% names(data_table()))){
        data <- data_table()[input$data_table_rows_all, ] %>%
          dplyr::select(input$x_axis, input$y_axis)
        names(data) <- c(input$x_axis, input$y_axis)
      } else if (!is.null(edited_data()) && (input$y_var_table == "" || !input$y_var_table %in% names(edited_data()))){
        data <- edited_data()[input$data_table_rows_all, ] %>%
          dplyr::select(input$x_axis, input$y_axis)
        names(data) <- c(input$x_axis, input$y_axis)
      }
      
      data
      # adds paired_observations column if not already present
      if (is.null(edited_data()) && input$repeated_observations_table == TRUE && !is.null(input$paired_observations_table) && input$paired_observations_table != ""){
        # Check if paired_observations_table column already exists
        if (!(input$paired_observations_table %in% names(data))) {
          # adds additional column to data
          paired_data <- data_table()[input$data_table_rows_all, ] %>%
            dplyr::select(input$paired_observations_table)
          names(paired_data) <- input$paired_observations_table
          data <- cbind(data, paired_data)
        }
      } else if (!is.null(edited_data()) && input$repeated_observations_table == TRUE && !is.null(input$paired_observations_table && input$paired_observations_table != "")){
        # Check if paired_observations_table column already exists
        if (!(input$paired_observations_table %in% names(data))) {
          paired_data <- edited_data()[input$data_table_rows_all, ] %>%
            dplyr::select(input$paired_observations_table)
          names(paired_data) <- input$paired_observations_table
          data <- cbind(data, paired_data)
        }
        data
      }
        
      #returns "data"
      data
    })
    
    # Populate x_axis and y_axis variables for filtered data
    observe({
      req(edited_data(), input$var_select)
      all_vars <- colnames(selected_vars())
      updateSelectInput(session, "x_axis", choices = all_vars, selected = input$x_axis)
      updateSelectInput(session, "y_axis", choices = setdiff(all_vars, input$x_axis), selected = input$y_axis)
    })
    
    # Populate x_axis and y_axis variables for unfiltered data
    observe({
      req(data_table(), input$var_select)
      all_vars <- colnames(selected_vars())
      updateSelectInput(session, "x_axis", choices = all_vars, selected = input$x_axis)
      updateSelectInput(session, "y_axis", choices = setdiff(all_vars, input$x_axis), selected = input$y_axis)
    })
    
    # Populate x_axis group order selection
    observe({
      req(input$var_select, input$x_axis)
      
      # selects data to be used
      if (is.null(edited_data())){
        data <- data_table()
      } else if (!is.null(edited_data())) {
        data <- edited_data()
      }
      updateSelectInput(session, "group_order_table", "Select order and variables to show", choices = na.omit(data[[input$x_axis]]))
    })
    
    # Populate  repeated variables for repeated observations
    observe({
      req(data_table(), input$var_select)
      updateSelectInput(session, "paired_observations_table", "Select column for paired observations", choices = colnames(selected_vars()), selected = input$paired_observations_table)
    })
    
    # Populate additional group order selection
    observe({
      req(input$var_select, input$x_axis, input$y_axis)
      
        # selects data to be used
        if (is.null(edited_data())){
          data <- data_table()
        } else if (!is.null(edited_data())) {
          data <- edited_data()
        }
      req(input$x_axis %in% names(data) && input$y_axis %in% names(data))
      # filters data based on previous selection
      if (input$show_additional_group_legend_table == TRUE) {
        # filters data based on group_order
        if (!is.null(input$group_order_table)) {
          filtered_data <- data %>%
            filter(!!as.name(input$x_axis) %in% input$group_order_table)
        } else {
          filtered_data <- data 
        }
        # Populate additional group selection
        updateSelectInput(session, "y_var_table", "Select the column for additional grouping", choices = colnames(filtered_data)[colnames(filtered_data) != input$x_axis & colnames(filtered_data) != input$y_axis], selected = input$y_var_table)
        
        # Filter choices for additional group order based on selected groups
        filtered_choices <- unique(na.omit(filtered_data[[input$y_var_table]]))
        updateSelectInput(session, "additional_group_order_table", "Select order and variables to shows", choices = filtered_choices)
      }
    })
    
    # runs submit_plot_table action button when additional variable is changed
    observeEvent(c(input$y_var_table, input$additional_group_order_table),{
      if (input$submit_plot_table[1] != 0) {
      runjs('$("#submit_plot_table").click();')
      }
    })
    
    # Clear button
    observeEvent(input$reset_plot_table,{
      # removes plot
      output$table_plot <- renderPlot({return(NULL)})
      # hides brush selection
      shinyjs::hide("table_brush_selection")
      # disables options
      shinyjs::disable("options_table")
    })
    
    # To avoid ongoing spinner when the table is first generated
    observe({
      if (input$submit_plot_table[1] == 0) {
        output$table_plot <- renderPlotly({return(NULL)})
      }
    })
    
    
    #Resets y and x axis labels when variables are changed
      observeEvent(input$y_axis,{
        updateTextInput(session, "y_axis_text_title_table", value = input$y_axis)
      })
      observeEvent(input$x_axis,{
        updateTextInput(session, "x_axis_text_title_table", value = input$x_axis)
      })
    
    #Generates plots
    observeEvent(input$submit_plot_table,{
      tryCatch({
        # if modal is open
        if (!is.null(plot_for_all_table()) && input$modal_table == TRUE &&  click_count_table() > 0) {
          # Check if update button is clicked
          output$table_plot <- renderPlot({
            if (!is.ggplot(plot_for_all_table())){
              plot_for_all_table()[[2]]
              } else {
              plot_for_all_table()    
              }
          }, height = input$height_plot_table, width = input$width_plot_table)
          
        # if modal is not open  
        } else if (input$modal_table != TRUE && !is.null(plot_for_all_table()) && click_count_table() == 0 && input$navpage == 'Data Selection and Visualization' && input$tabselected =='Table & Graph') {
          output$table_plot <- renderPlot({
            req(plot_for_all_table())
            if (!is.ggplot(plot_for_all_table())){
              plot_for_all_table()[[2]]
            } else {
              plot_for_all_table()    
            }
            }, height = input$height_plot_table, width = input$width_plot_table)
        }
        
        # enables options button
        shinyjs::enable("options_table")
        
        ## disables plot button
        #shinyjs::disable("submit_plot_table")
        
        
        # for brushed points when plotting
        # output$table_brush_selection <- renderTable({
        #   brushedPoints(brushed_points_table(), input$table_plot_brush, xvar = input$x_axis, yvar = input$y_axis)
        # })
        
        
        
        # renders warning text null
        output$uploaded_file_extension_error <- renderText({NULL})
        output$table_error_message <- renderText({NULL})
        
        #shows brush table selection and plot height and width selections
        shinyjs::show("table_brush_selection")
        
        shinyjs::show("height_plot_table")
        shinyjs::show("width_plot_table")
        
      }, error = function(e) {
        output$table_error_message <- renderText({"Plot could not be generated. Please check data and format selections"})
        
      })
    })
    
    # runs submit_plot_table action button so that click count is updated
    observeEvent(input$update_options_table, {
      runjs('$("#submit_plot_table").click();')
    }) 
    
  #############################################################################  
  #############################################################################
  # For Intraclass correlation coefficient (ICC) and Linear Mixed Model (LMM) calculations
    #Read data from excel file
    data_icc <- reactive({
      tryCatch({
        req(input$data_file_icc)
        # Determine the file extension
        file_ext <- tools::file_ext(input$data_file_icc$name)
        if (tolower(file_ext) == "xlsx" || tolower(file_ext) == "xls") {
          # Read an Excel file
          read_excel(input$data_file_icc$datapath) %>%
            mutate_if(is.character, as.factor) # Convert character columns to factor
        } else if (tolower(file_ext) == "csv") {
          # Read a CSV file
          read.csv(input$data_file_icc$datapath) %>%
            mutate_if(is.character, as.factor) # Convert character columns to factor
        } else {
          # Display a message for unsupported file types
          output$uploaded_icc_file_extension_error <- renderText({"Wrong file uploaded, please upload an Excel (.xlsx or .xls) or CSV (.csv) file"})
        }
      }, error = function(e) {
        # Shows message if error occurs when uploading file
        output$error_message_icc_data <- renderText({"An error ocurred with your data file upload. Please check your data structure and column names"})
      })
    })
    
    # resets plot and display options everytime a new file is uploaded
    observeEvent(input$data_file_icc,{
      # hides inputs and resets plots and error text messages when new file is uploaded
      output$icc_plot <- renderPlot({return(NULL)})
      output$icc_two_plot <- renderPlot({return(NULL)})
      output$data_table_icc_two_lmm <- renderUI({return(NULL)})
      output$data_table_icc <- renderUI({return(NULL)})
      
      output$icc_error_message <- renderText({NULL})
      output$error_message_icc_data <- renderText({NULL})
      
      # hides inputs that are not used but are required for conditions in plot code
      shinyjs::hide("show_additional_group_legend_icc")
      shinyjs::hide("show_additional_group_legend_icc_two")
      
      # hides download buttons
      shinyjs::hide("download_plot_icc")
      shinyjs::hide("download_plot_icc_two")
      shinyjs::hide("download_data_table_icc_two_lmm")
      shinyjs::hide("download_data_table_icc")
      
      # hides numeric inputs for plot size
      shinyjs::hide("height_plot_icc")
      shinyjs::hide("width_plot_icc")
      
      # hides brushoptions
      shinyjs::hide("icc_brush_selection")
      shinyjs::hide("icc_two_brush_selection")
    })
    
    # hides group and level section until file is uploaded
    output$data_file_icc_uploaded <- reactive({
      return(!is.null(input$data_file_icc))
    })
    
    outputOptions(output, 'data_file_icc_uploaded', suspendWhenHidden=FALSE)
    
    # Reactive data for selected variables for plotting
    selected_vars_plot_icc <- reactive({
      tryCatch({
        # Clears previous error message if present
        output$error_message_icc_data = renderText({NULL})
        
        #checks if names of variables are in data (helps to avoid error when new file is uploaded)
        validate(
          need(!is.null(data_icc()) && !is.null(input$group_var_icc) && !is.null(input$x_var_icc) && !is.null(input$y_var_icc) &&
                 (input$group_var_icc %in% names(data_icc()) && input$x_var_icc %in% names(data_icc())) && 
                 (input$y_var_icc %in% names(data_icc())), label = ""
          )
        )
        #gets variables from selection
        if (input$num_levels == 2){
          data <- data_icc() %>%
            dplyr::select(input$group_var_icc, input$x_var_icc, input$y_var_icc)
          names(data) <- c(input$group_var_icc, input$x_var_icc, input$y_var_icc)
          data
        } else if (input$num_levels == 3) {
          data <- data_icc() %>%
            dplyr::select(input$group_var_icc, input$x_var_icc, input$y_var_icc, input$z_var_icc)
          names(data) <- c(input$group_var_icc, input$x_var_icc, input$y_var_icc, input$z_var_icc)
          data
        } else if (input$num_levels == 4) {
          data <- data_icc() %>%
            dplyr::select(input$group_var_icc, input$x_var_icc, input$y_var_icc, input$z_var_icc, input$w_var_icc)
          names(data) <- c(input$group_var_icc, input$x_var_icc, input$y_var_icc, input$z_var_icc, input$w_var_icc)
          data
        }
      }, error = function(e) {
        # Shows message if error occurs
        #output$error_message_icc_data <- renderText({"An error ocurred with variable selection. Please check your data"})
      })
    }) 
    
    # Download buttons for tables start hidden
    observe({
      if(is.null(input$data_file_icc)){
        shinyjs::hide("download_data_table_icc_two_lmm")
        shinyjs::hide("download_data_table_icc")
        shinyjs::hide("download_plot_icc")
        shinyjs::hide("download_plot_icc_two")
      }
    })
    
    # Populate grouping variable dropdown menu
    observe({
      req(!is.null(data_icc()))
      updateSelectInput(session, "group_var_icc", "Select the grouping variable", choices = colnames(data_icc()), selected = input$group_var_icc)
      
      # # shows message if column is empty
      # if (input$group_var_icc == ""){
      #   shinyFeedback::feedbackWarning(inputId = "group_var_icc", session = shiny::getDefaultReactiveDomain(), text = "This variable is empty or has no column name assigned", show = TRUE)
      # } else {
      #   hideFeedback("group_var_icc")
      # }
    })
    
    # Populate x_axis group order selection
    observe({
      req(input$data_file_icc, input$group_var_icc)
      updateSelectInput(session, "additional_group_order_icc", "Select order and variables to show", choices = na.omit(data_icc()[[input$group_var_icc]]))
      updateSelectInput(session, "group_order_icc_two", "Select order and variables to show", choices = na.omit(data_icc()[[input$group_var_icc]]))
    })
    
    # Populate level 1 column dropdown menu
    observe({
      req(data_icc(), input$group_var_icc)
      updateSelectInput(session, "x_var_icc", "Select the column for level 1", choices = colnames(data_icc())[colnames(data_icc()) != input$group_var_icc], selected = input$x_var_icc)
      
      # # shows message if column is empty
      # if (input$x_var_icc == ""){
      #   shinyFeedback::feedbackWarning(inputId = "x_var_icc", session = shiny::getDefaultReactiveDomain(), text = "This variable is empty or has no column name assigned", show = TRUE)
      # } else {
      #   hideFeedback("x_var_icc")
      # }
    })
    
    # Populate level 2 column dropdown menu
    observe({
      req(data_icc(), input$group_var_icc, input$x_var_icc)
      updateSelectInput(session, "y_var_icc", "Select the column for level 2", choices = colnames(data_icc())[colnames(data_icc()) != input$group_var_icc & colnames(data_icc()) != input$x_var_icc], selected = input$y_var_icc)
      
      # # shows message if column is empty
      # if (input$y_var_icc == ""){
      #   shinyFeedback::feedbackWarning(inputId = "y_var_icc", text = "This variable is empty or has no column name assigned", show = TRUE)
      # } else {
      #   hideFeedback("y_var_icc")
      # }
    })
    
    # Populate level 3 column dropdown menu
    observe({
      req(data_icc(), input$group_var_icc, input$x_var_icc, input$y_var_icc)
      updateSelectInput(session, "z_var_icc", "Select the column for level 3", choices = colnames(data_icc())[colnames(data_icc()) != input$group_var_icc & colnames(data_icc()) != input$x_var_icc & colnames(data_icc()) != input$y_var_icc], selected = input$z_var_icc)
      
      # # shows message if column is empty
      # if (input$z_var_icc == ""){
      #   shinyFeedback::feedbackWarning(inputId = "z_var_icc", text = "This variable is empty or has no column name assigned", show = TRUE)
      # } else {
      #   hideFeedback("z_var_icc")
      # }
    })
    
    # Populate level 4 column dropdown menu
    observe({
      req(data_icc(), input$group_var_icc, input$x_var_icc, input$y_var_icc, input$z_var_icc)
      updateSelectInput(session, "w_var_icc", "Select the column for level 4", choices = colnames(data_icc())[colnames(data_icc()) != input$group_var_icc & colnames(data_icc()) != input$x_var_icc & colnames(data_icc()) != input$y_var_icc & colnames(data_icc()) != input$z_var_icc], selected = input$w_var_icc)
      
      # # shows message if column is empty
      # if (input$w_var_icc == ""){
      #   shinyFeedback::feedbackWarning(inputId = "w_var_icc", text = "This variable is empty or has no column name assigned", show = TRUE)
      # } else {
      #   hideFeedback("w_var_icc")
      # }
    })
    
    # Adjusts choices for number of levels selected for each tab
    observe({
      req(data_icc())
        if (input$num_levels >= 3 && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)') {
          # warning message in case num_levels larger than 2
          shinyFeedback::feedbackWarning(inputId = "num_levels", text = "Only 2 levels can be considered for ICC (2 Levels)", show = TRUE)
        } else if (input$num_levels <= 2 && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)'){
          # hides feedback message
          hideFeedback("num_levels")
        }
    })
    
    # Populate grouping variable control group selection for LMM
    observe({
      req(data_icc(), input$group_var_icc, input$x_var_icc, input$y_var_icc)
      # if argument avoids error when new file is uploaded
      if (input$group_var_icc %in% colnames(data_icc()) && !is.null(input$additional_group_order_icc_two)) {
        updateSelectInput(session, "control_group_var_icc", "Select control group", choices = input$additional_group_order_icc_two)
      } else if (input$group_var_icc %in% colnames(data_icc()) && is.null(input$additional_group_order_icc_two)) {
        updateSelectInput(session, "control_group_var_icc", "Select control group", choices = na.omit(unique(data_icc()[, input$group_var_icc])))        
      } else {
        updateSelectInput(session, "control_group_var_icc", "Select control group", choices = character(0))
      }
      
    })
    
    # Populate repeated measures dropdown selection for LMM
    observe({
      (req(data_icc(), input$group_var_icc))
      if(isTRUE(input$repeated_var_icc_selection)){
        updateSelectInput(session, "repeated_var_icc", "Select repeated measures column", choices = colnames(data_icc())[colnames(data_icc()) != input$group_var_icc & colnames(data_icc()) != input$x_var_icc])
      }
    })
    
    # creates variables to store ICC and LMM values to avoid running the same code
    # for names of variables
    group_var_icc_lmm <- reactiveVal(0)
    x_var_icc_lmm <- reactiveVal(0)
    y_var_icc_lmm <- reactiveVal(0)
    z_var_icc_lmm <- reactiveVal(0)
    w_var_icc_lmm <- reactiveVal(0)
    group_order_icc <- reactiveVal(0)
    group_order_lmm <- reactiveVal(0)
    data_icc_lmm <- reactiveVal(0)   
    icc_table_icc_lmm <- reactiveVal(0)
    manual_lmm_formula <- reactiveVal(0)
    repeated_lmm <- reactiveVal(0)
    control_group_lmm <- reactiveVal(0)
    effect_size_lmm <- reactiveVal(0)
    
    # For ICC
      #Clear ICC plot
      observeEvent(input$clear_icc,{
        
        # hides plots, buttons, error messages and tables
        output$icc_plot <- renderPlot({return(NULL)})
        output$data_table_icc <- renderUI({return(NULL)})
        shinyjs::hide("download_data_table_icc")
        shinyjs::hide("download_plot_icc")
        output$icc_error_message <- renderText({NULL})
        output$error_message_icc_data <- renderText({NULL})
        shinyjs::hide("height_plot_icc")
        shinyjs::hide("width_plot_icc")
        shinyjs::hide("icc_brush_selection")
      })
      
      # To avoid ongoing spinner when the table is first generated
      observe({
        if (input$submit_plot_icc[1] == 0) {
          output$icc_plot <- renderPlot({return(NULL)})
        }
      })
      
      #Resets y and x axis labels when variables are changed
      observeEvent(input$y_var_icc,{
        updateTextInput(session, "y_axis_text_title_icc", value = input$y_var_icc)
      })
      observeEvent(input$x_var_icc,{
        if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)') {
          updateTextInput(session, "x_axis_text_title_icc", value = input$x_var_icc)
        }
      })
      
      # For ICC calculation and plot and Design effect calculation
      observeEvent(input$submit_plot_icc,{
        # adds tryCatch for any error that may occur
        tryCatch({ 
          #Renders Plot
          # if modal is open, requires click to update pot
          if (!is.null(plot_for_all_icc()) && input$modal_icc == TRUE &&  click_count_icc() > 0) {
            # Check if update button is clicked
            output$icc_plot <- renderPlot({
              if (!is.ggplot(plot_for_all_icc())){
                plot_for_all_icc()[[2]]
              } else {
                plot_for_all_icc()    
              }
            }, height = input$height_plot_icc, width = input$width_plot_icc
            )
            # #sets click count to zero
            # click_count_icc(0)
            # shows numeric inputs for plot size
            shinyjs::show("height_plot_icc")
            shinyjs::show("width_plot_icc")
            shinyjs::enable("options_icc")

          # if modal is not open
          } else if (input$modal_icc != TRUE && !is.null(plot_for_all_icc()) && click_count_icc() == 0) {
            output$icc_plot <- renderPlot({
              if (!is.ggplot(plot_for_all_icc())){
                plot_for_all_icc()[[2]]
              } else {
                plot_for_all_icc()    
              }
            }, height = input$height_plot_icc, width = input$width_plot_icc
            )
            # #sets click count to zero
            # click_count_icc(0)
            # shows numeric inputs for plot size
            shinyjs::show("height_plot_icc")
            shinyjs::show("width_plot_icc")
            shinyjs::enable("options_icc")
          }
          
          # checks if alterations occurred on data and data selection
          if (group_var_icc_lmm() != input$group_var_icc || x_var_icc_lmm() != input$x_var_icc ||
              y_var_icc_lmm() != input$y_var_icc || !identical(group_order_icc(),input$additional_group_order_icc) ||
              isTRUE(data_icc_lmm() != data_icc())) {
            
            # updates names of variables
            group_var_icc_lmm(input$group_var_icc) 
            x_var_icc_lmm(input$x_var_icc)
            y_var_icc_lmm() != input$y_var_icc
            data_icc_lmm(data_icc())
            group_order_icc(input$additional_group_order_icc)
            
            # filters data based on group_order
            if (!is.null(input$additional_group_order_icc)) {
              data_icc_2_levels <- data_icc() %>%
                filter(!!as.name(input$group_var_icc) %in% input$additional_group_order_icc)
            } else {
              data_icc_2_levels <- data_icc()
            }

            # Extract grouping variable names from data
            group_levels <- unique(data_icc_2_levels[[input$group_var_icc]])
            
            # Calculate ICC for each group
            icc_list <- list()
            icc_error_groups <- list()
            for (group_level in group_levels) {
              # Subset data for current group
              group_data <- subset(data_icc_2_levels, data_icc_2_levels[[input$group_var_icc]] == group_level)
              
              # Check that there are at least two observations per group
              if (length(group_data[[input$y_var_icc]][!is.na(group_data[[input$y_var_icc]])]) < 2 | length(group_data[[input$x_var_icc]][!is.na(group_data[[input$x_var_icc]])]) < 2 ) {
                
                # Store names of group levels for which icc cannot be calculated
                icc_error_groups <- c(icc_error_groups, group_level)
                
                #runs icc for groups in which there are at least two observations per group
              } else if (length(group_data[[input$y_var_icc]][!is.na(group_data[[input$y_var_icc]])]) > 2 | length(group_data[[input$x_var_icc]][!is.na(group_data[[input$x_var_icc]])]) > 2 ) {
                
                tryCatch({ 
                  # Calculate ICC for current group there are at least two observations per group
                  icc <- ICCest(x = factor(group_data[[input$x_var_icc]]), alpha = 0.05, y = group_data[[input$y_var_icc]], data = group_data)
                  
                  # Calculates Design Effect and effective sample size
                  N_total <- sum(table(group_data[[input$y_var_icc]]))
                  cluster_size <- mean(table(group_data[[input$x_var_icc]]))
                  deff <- 1 + (cluster_size - 1) * icc[[1]]
                  sample_size <- sum(table(group_data[[input$x_var_icc]]))
                  ess <- sample_size / deff
                  
                  # Store both ICC result and group_level in the list
                  icc_list[[group_level]] <- list(ICC_result = icc, N_total = N_total, Deff = deff, Ess = ess, Group_Level = group_level)
                }, error = function(e) {
                  cat("Error Message:", conditionMessage(e), "\n")
                  # Handle the error (e.g., print a message)
                  output$icc_error_message <- renderText({
                    ("ICC could not be calculated. Please check data selection")
                  })
                  output$icc_plot <- renderPlot({NULL})
                  shinyjs::hide("download_data_table_icc")
                  shinyjs::hide("download_plot_icc")
                  shinyjs::hide("height_plot_icc")
                  shinyjs::hide("width_plot_icc")
                  output$data_table_icc <- renderUI({NULL})
                  shinyjs::disable("options_icc")
                  shinyjs::hide("icc_brush_selection")
                  
                  # checks if plotting variable is not numerical and warns user
                  if (!is.numeric(data_icc_2_levels[[input$y_var_icc]])){
                    shinyFeedback::feedbackWarning(inputId = "y_var_icc", text = "Please select a numeric variable for plotting", show = TRUE)
                  } else if (is.numeric(data_icc_2_levels[[input$y_var_icc]])) {
                    hideFeedback("y_var_icc")
                  }
                })
              }
            }
            
            # runs the rest of the code only if icc_list is not empty
            if (!is_empty(icc_list)){
              # deletes previous error message
              output$icc_error_message <- renderText({NULL})
              
              # Remove NULL entries from icc_list
              icc_list <- icc_list[sapply(icc_list, Negate(is.null))]
              
              # Display the list of problematic groups in a text output
              if (!is_empty(icc_error_groups)){
                output$icc_error_message <- renderText({
                  paste("The following groups have less than 2 observations:", paste(icc_error_groups, collapse = ", "))
                })
              }
              
              # Remove NULL entries from icc_list and replace NA with 0
              icc_list <- lapply(icc_list, function(x) if (!is.null(x)) {
                x[is.na(x)] <- 0
                x
              })
              
              # Extract ICC results and group levels from the list
              icc_df <- data.frame(
                Group = sapply(icc_list, function(x) if (!is.null(x)) x$Group_Level else ""),
                ICC = sapply(icc_list, function(x) if (!is.null(x)) x$ICC_result$ICC else 0),
                Lower = sapply(icc_list, function(x) if (!is.null(x)) x$ICC_result$LowerCI else 0),
                Upper = sapply(icc_list, function(x) if (!is.null(x)) x$ICC_result$UpperCI else 0),
                N = sapply(icc_list, function(x) if (!is.null(x)) x$ICC_result$N else 0),
                N_total = sapply(icc_list, function(x) if (!is.null(x)) x$N_total else 0),
                Deff = sapply(icc_list, function(x) if (!is.null(x)) x$Deff else 0),
                Ess = sapply(icc_list, function(x) if (!is.null(x)) x$Ess else 0)
              )
              
              # generates table with values
              icc_df_table <- data.frame(
                Group = icc_df$Group,
                Observations = icc_df$N_total,
                Subgroup = icc_df$N,
                ICC = signif(icc_df$ICC, 3),
                `ICC 95 % CI` = paste0("[", signif(icc_df$Lower, 3), ", ", signif(icc_df$Upper, 3), "]"),
                `Design effect` = signif(icc_df$Deff, 3),
                `Effective sample size` = signif(icc_df$Ess, 3),
                check.names = FALSE
              )
              
              # re-organizes table order to match group order
              icc_df_table <- icc_df_table[order(match(icc_df_table$Group, input$additional_group_order_icc)), ]
              
              colnames(icc_df_table)[which(names(icc_df_table) == "Subgroup")] <- paste("Subgroup (", input$x_var_icc, ")")
              
              
              #highlights rows for which ICC is larger than 0.5
              icc_table <- kable(icc_df_table, digits = 3, align = "lcccccc", caption = "Summary", valign = 't', table.attr = "class=\"table table-striped table-auto\"", format = "html")%>%
                kable_styling() %>%
                row_spec(which(icc_df_table$ICC >0.5), bold = T, color = "white", background = "red")
              
              # Displays the table
              output$data_table_icc <- renderUI({
                HTML(icc_table)
              })
              
              #shows table download button
              shinyjs::show("download_data_table_icc")
              
              # downloads generated table as .xlsx
              output$download_data_table_icc <- downloadHandler(
                filename = "icc_table.xlsx",
                content = function(file) {
                  # Create a workbook
                  wb <- createWorkbook()
                  # Add sheets to the workbook
                  addWorksheet(wb, sheetName = "icc")
                  writeData(wb, sheet = "icc", icc_df_table)
                  # Save the workbook
                  saveWorkbook(wb, file = file, overwrite = TRUE)
                }
              )
              
              # For brushed points when plotting
              shinyjs::show("icc_brush_selection")
              
              # shows plot download button
              shinyjs::show("download_plot_icc")
              
              # downloads generated plot as .png
              output$download_plot_icc <- downloadHandler(
                filename = "icc.png",
                content = function(file) {
                  # Save the superplot as a PNG file
                  ggsave(file, plot = plot_for_all_icc(), device = "png")
                }
              )
            }
          }
        }, error = function(e) {
          output$icc_error_message <- renderText({
            paste("An error occured. Please check data and plot selection")
          })
        })
        
      })
      
      #observe for different colour coding when plot is generated for the first time
      observe({
        if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)') {
          tab_id = "icc"
          data <- data_icc()
          data <- selected_vars_plot_icc()
          if (input$num_levels >= 2) {
            x_var <- input$x_var_icc
            y_var <- input$y_var_icc
            z_var <- input$group_var_icc
          }

         if (input$submit_plot_icc[1] == 0 && input$num_levels >= 2) {
          updateSelectInput(session, paste0("geom_jitter_colour_fill_by_group_", tab_id), choices = list("Single Colour", "Colour by Group - discrete" = c(x_var, z_var), "Colour by Group - gradient"= c(paste0(x_var, " "), paste0(z_var, " "))), selected = z_var)
           updateSelectInput(session, paste0("geom_boxplot_colour_fill_by_group_", tab_id), choices = list("Single Colour", "Colour by Group - discrete" = c(x_var, z_var), "Colour by Group - gradient"= c(paste0(x_var, " "), paste0(z_var, " "))), selected = z_var)
           # generates random colours symbols
           random_colours <- randomColor(length(unique(data[[z_var]])))
           # assigns random colours for symbol border and fill
           selected_colors_icc$colors <- random_colours
           selected_colours_symbol_fill_icc$colors <- random_colours
           # assigns random colours for box/violin/bar border and fill
           selected_colors_boxplot_line_icc$colors <- random_colours
           selected_colors_boxplot_fill_icc$colors <- random_colours
           # sets legend in place
           updateSelectInput(session, paste0("maginal_plot_legend_", tab_id), label = "Legend position", choices = c("none", "left","top", "right", "bottom", "custom"), selected = "right")
         }
          if (input$submit_plot_icc[1] >= 0 && input$num_levels >= 2 && length(selected_colors_icc$colors) != length(unique(data[[z_var]]))) {
            observeEvent(input$group_var_icc,{
              updateSelectInput(session, paste0("geom_jitter_colour_fill_by_group_", tab_id), choices = list("Single Colour", "Colour by Group - discrete" = c(x_var, z_var), "Colour by Group - gradient"= c(paste0(x_var, " "), paste0(z_var, " "))), selected = z_var)
              updateSelectInput(session, paste0("geom_boxplot_colour_fill_by_group_", tab_id), choices = list("Single Colour", "Colour by Group - discrete" = c(x_var, z_var), "Colour by Group - gradient"= c(paste0(x_var, " "), paste0(z_var, " "))), selected = z_var)
              # generates random colours symbols
              random_colours <- randomColor(length(unique(data[[z_var]])))
              # assigns random colours for symbol border and fill
              selected_colors_icc$colors <- random_colours
              selected_colours_symbol_fill_icc$colors <- random_colours
              # assigns random colours for box/violin/bar border and fill
              selected_colors_boxplot_line_icc$colors <- random_colours
              selected_colors_boxplot_fill_icc$colors <- random_colours
              # sets legend in place
              updateSelectInput(session, paste0("maginal_plot_legend_", tab_id), label = "Legend position", choices = c("none", "left","top", "right", "bottom", "custom"), selected = "right")
              
            })
          }
        }
      })
      
      # runs submit_plot_icc action button so that click count is updated
      observeEvent(input$update_options_icc, {
        runjs('$("#submit_plot_icc").click();')
      }) 
      
    # For LMM
      # Clear LMM plot
      observeEvent(input$clear_icc_two,{
        # hides plots, buttons and tables
        output$lmm_error_message <- renderText({NULL})
        output$icc_two_plot <- renderPlot({return(NULL)})
        output$data_table_icc_two_lmm <- renderUI({return(NULL)})
        shinyjs::hide("download_data_table_icc_two_lmm")
        shinyjs::hide("download_plot_icc_two")
        output$icc_error_message <- renderText({NULL})
        output$error_message_icc_data <- renderText({NULL})
        shinyjs::hide("height_plot_icc_two")
        shinyjs::hide("width_plot_icc_two")
        shinyjs::hide("icc_two_brush_selection")
      })

      # To avoid ongoing spinner when the table is first generated
      observe({
        if (input$submit_plot_icc_two[1] == 0) {
          output$icc_two_plot <- renderPlot({return(NULL)})
        }
      })
      
      #Resets y and x axis labels when variables are changed
      observeEvent(input$y_var_icc,{
          if (input$num_levels == 2 && input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
          updateTextInput(session, "y_axis_text_title_icc_two", value = input$y_var_icc)
          }
        })
        
      observeEvent(input$z_var_icc,{
          if (input$num_levels == 3 && input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
            updateTextInput(session, "y_axis_text_title_icc_two", value = input$z_var_icc)
          }
        })
        
      observeEvent(input$w_var_icc,{
          if (input$num_levels == 4 && input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
            updateTextInput(session, "y_axis_text_title_icc_two", value = input$w_var_icc)
          }
        })
      
      observeEvent(input$group_var_icc,{
        if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
          updateTextInput(session, "x_axis_text_title_icc_two", value = input$group_var_icc)
        }
      })
    
      # For linear mixed-model
      observeEvent(input$submit_plot_icc_two,{

        #begins with error detection
        tryCatch({
          
          # enables buttons
          shinyjs::show("download_data_table_icc_two_lmm")
          shinyjs::show("download_plot_icc_two")
          shinyjs::show("icc_two_brush_selection")
          shinyjs::enable("options_icc_two")
          output$lmm_error_message <- renderText({NULL})
          output$error_message_icc_data <- renderText({NULL})

          #Renders Plot
          # if modal is open, requires click to update pot
          if (!is.null(plot_for_all_icc_two()) && input$modal_icc_two == TRUE &&  click_count_icc_two() > 0) {
            # Check if update button is clicked
            output$icc_two_plot <- renderPlot({
              if (!is.ggplot(plot_for_all_icc_two())){
                plot_for_all_icc_two()[[2]]
              } else {
                plot_for_all_icc_two()    
              }
            }, height = input$height_plot_icc_two, width = input$width_plot_icc_two
            )
            # #sets click count to zero
            # click_count_icc_two(0)
            # shows numeric inputs for plot size
            shinyjs::show("height_plot_icc_two")
            shinyjs::show("width_plot_icc_two")
            shinyjs::enable("options_icc_two")
            
            # if modal is not open
          } else if (input$modal_icc_two != TRUE && !is.null(plot_for_all_icc_two()) && click_count_icc_two() == 0) {
            output$icc_two_plot <- renderPlot({
              if (!is.ggplot(plot_for_all_icc_two())){
                plot_for_all_icc_two()[[2]]
              } else {
                plot_for_all_icc_two()    
              }
            }, height = input$height_plot_icc_two, width = input$width_plot_icc_two
            )
            # #sets click count to zero
            # click_count_icc_two(0)
            # shows numeric inputs for plot size
            shinyjs::show("height_plot_icc_two")
            shinyjs::show("width_plot_icc_two")
            shinyjs::enable("options_icc_two")
          }  
          
        # checks if alterations occurred on data and data selection
        if (group_var_icc_lmm() != input$group_var_icc || x_var_icc_lmm() != input$x_var_icc ||
            y_var_icc_lmm() != input$y_var_icc ||  z_var_icc_lmm() != input$z_var_icc ||
            w_var_icc_lmm() != input$w_var_icc || (isTRUE(input$repeated_var_icc_selection) && manual_lmm_formula() != input$manual_lmm) ||
            repeated_lmm() != input$repeated_var_icc || control_group_lmm() != input$control_group_var_icc ||
            effect_size_lmm() != input$effect_size_lmm || #isTRUE(group_order_lmm() != input$additional_group_order_icc_two) ||
            isFALSE(any(group_order_lmm() %in% input$group_order_icc_two)) ||
            !identical(group_order_lmm(), input$additional_group_order_icc_two) ||
            #(!is.null(input$additional_group_order_icc_two) && !is.null(group_order_lmm()) && isTRUE(group_order_lmm() != input$additional_group_order_icc_two)) ||
            isTRUE(data_icc_lmm() != data_icc()))  {
        
          # updates names of variables
          group_var_icc_lmm(input$group_var_icc) 
          x_var_icc_lmm(input$x_var_icc)
          y_var_icc_lmm(input$y_var_icc)
          z_var_icc_lmm(input$z_var_icc)
          w_var_icc_lmm(input$w_var_icc)
          data_icc_lmm(data_icc())
          group_order_lmm(input$group_order_icc_two)
          manual_lmm_formula(input$manual_lmm)
          repeated_lmm(input$repeated_var_icc)
          control_group_lmm(input$control_group_var_icc)
          effect_size_lmm(input$effect_size_lmm)
          
          # filters data based on group_order
          if (!is.null(input$additional_group_order_icc_two)) {
            data_lmm <- data_icc() %>%
              filter(!!as.name(input$group_var_icc) %in% input$group_order_icc_two)
          } else {
            data_lmm <- data_icc()
          }
          
          # gets data for plot and adjusts control group based on selection
          if (is.factor(data_lmm[[input$group_var_icc]])){
            data_lmm[[input$group_var_icc]] <- relevel(data_lmm[[input$group_var_icc]], ref = input$control_group_var_icc)
          }
          
          # Converts names of inputs columns into strings that can be used for lmer
          names(data_lmm)[names(data_lmm) == input$group_var_icc] <- "group_var"
          names(data_lmm)[names(data_lmm) == input$x_var_icc] <- "x_var"
          names(data_lmm)[names(data_lmm) == input$y_var_icc] <- "y_var"
          
          # if 3 levels or more are selected
          if (input$num_levels >=3 ){
            names(data_lmm)[names(data_lmm) == input$z_var_icc] <- "z_var"
          }
          
          # if 4 levels are selected
          if (input$num_levels >=4 ){
            names(data_lmm)[names(data_lmm) == input$w_var_icc] <- "w_var"
          }
         
          # Runs linear mixed-model for 1 fixed effect variables and 2, 3, or 4 random variables without interaction
          if (input$num_levels == 2) {
            mixed <- lmer(y_var ~ 1 + group_var + (1 | x_var), data_lmm)
          } else if (input$num_levels == 3) {
            mixed <- lmer(z_var ~ 1 + group_var + (1 | y_var) + (1 | x_var), data_lmm)
          } else if (input$num_levels == 4) {
            mixed <- lmer(w_var ~ 1 + group_var + (1 | z_var) + (1 | y_var) + (1 | x_var), data_lmm)
          } else {
            print("Less than 2 levels, LMM cannot calculate variance component")
          }
          
          # If repeated measures are selected
          if (isTRUE(input$repeated_var_icc_selection)){ 
            names(data_lmm)[names(data_lmm) == input$repeated_var_icc] <- "repeated_var"
            
            # Runs linear mixed-model for 2 fixed effect variables and 2, 3, or 4 random variables without interaction
            if (input$num_levels == 2) {
              mixed <- lmer(y_var ~ 1 + group_var * repeated_var + (1 | x_var), data_lmm)
            } else if (input$num_levels == 3) {
              mixed <- lmer(z_var ~ 1 + group_var * repeated_var + (1 | y_var) + (1 | x_var), data_lmm)
            } else if (input$num_levels == 4) {
              mixed <- lmer(w_var ~ 1 + group_var * repeated_var + (1 | z_var) + (1 | y_var) + (1 | x_var), data_lmm)
            } else {
              print("Less than 2 levels, LMM cannot calculate variance component")
            }
          }
          
          if (isTRUE(input$manual_lmm_text)) {
            # for manual lmer
            formula <- input$manual_lmm
            formula <- gsub(input$group_var_icc, "group_var", formula)
            formula <- gsub(input$x_var_icc, "x_var", formula)
            formula <- gsub(input$y_var_icc, "y_var", formula)
            formula <- gsub(input$z_var_icc, "z_var", formula)
            formula <- gsub(input$w_var_icc, "w_var", formula)
            
            formula_lmer <- as.formula(formula)
            mixed <- lmer(formula_lmer, data = data_lmm)
          }
          
          # adds warning message if number of observations is too big for fast computation of model_parameters
          if (nobs(mixed) > 500) {
            shinyjs::show("loading-message")
          }
          
          #Extracts fixed and random effects
          parameters_mixed <- parameters::model_parameters(mixed, effects = "all", dff = "Wald")
          
            #removes standard error from table
            parameters_mixed <- subset(parameters_mixed, select = -c(SE))
            
            #estimates variance of random effects by squaring standard deviation
            parameters_mixed <- parameters_mixed %>%
              mutate(
                Coefficient = ifelse(Effects == "random", Coefficient^2, Coefficient),
                CI_low = ifelse(Effects == "random", CI_low^2, CI_low),
                CI_high = ifelse(Effects == "random", CI_high^2, CI_high)
              )
            
            # Extract the number of observations per random effect
            observations <- ranef(mixed)
            
            #calculates number of observations per random effect and stores in a data.frame
            num_observations <- lapply(observations, function(x) nrow(x))
            
            observations_df <- data.frame(
              Random_Effect = names(observations),
              Num_Observations = unlist(num_observations)
            )
          
          # Estimates variance component of random effects
            # total variance
            var_total <- sum(parameters_mixed$Coefficient[parameters_mixed$Effects == "random"])
            
            # Compute variance components for each random effect
            var_components <- data.frame(var_name = character(), variance = numeric(), lower = numeric(), upper = numeric(), stringsAsFactors = FALSE)
            for (i in 1:nrow(parameters_mixed)) {
              effect <- parameters_mixed[i, "Effects"]
              if (effect == "random") {
                var_comp <- parameters_mixed[i, "Coefficient"] / var_total
                lower_comp <- parameters_mixed[i, "CI_low"] / var_total
                upper_comp <- parameters_mixed[i, "CI_high"] / var_total
                var_components <- rbind(var_components, data.frame(var_name = parameters_mixed[i, "Group"], variance = var_comp, lower = lower_comp, upper = upper_comp))
              }
            }
            
            # Adds random effect variance components and number of observations to summary results
            for (i in 1:nrow(parameters_mixed)) {
              param_name <- parameters_mixed[i, "Group"]
              row_idx <- which(var_components[, "var_name"] == param_name)
              if (length(row_idx) > 0 && param_name != "") {
                parameters_mixed[i, "var_component"] <- var_components[row_idx, "variance"]
                parameters_mixed[i, "CI_low_var_component"] <- var_components[row_idx, "lower"]
                parameters_mixed[i, "CI_high_var_component"] <- var_components[row_idx, "upper"]
                parameters_mixed[i, "Parameter"] <- param_name
                parameters_mixed[i, "Observations"] <- observations_df[row_idx, "Num_Observations"]
              }
            }
          
          # Initialize an empty matrix to store effect size results
          effect_size_matrix <- matrix(nrow = nrow(parameters_mixed), ncol = 4, dimnames = list(NULL, c("Parameter", "effect_size", "CI_low_effect_size", "CI_high_effect_size")))
          
          # Apply the function using a for loop
          for (i in 1:nrow(parameters_mixed)) {
            param_name <- parameters_mixed[i, "Parameter"]
            
            # Skips 'Intercept'
              if (param_name != "(Intercept)") {  
                t_val <- parameters_mixed[i, "t"]
                df <- parameters_mixed[i, "df_error"]
                
                #calculates selected effect size from t statistic
                if (!is.na(t_val)){ #avoids error when t_val might is NA
                  if (input$effect_size_lmm == "eta2"){
                    effect_size <- t_to_eta2(t_val, df, ci = 0.90, alternative = "two.sided")
                  } else if (input$effect_size_lmm == "omega2") {
                    effect_size <- t_to_omega2(t_val, df, ci = 0.90, alternative = "two.sided")
                  } else if (input$effect_size_lmm == "epsilon2") {
                    effect_size <- t_to_epsilon2(t_val, df, ci = 0.90, alternative = "two.sided")
                  } else if (input$effect_size_lmm == "cohen_f") {
                    effect_size <- t_to_f2(t_val, df, ci = 0.90, alternative = "two.sided")
                  } else if (input$effect_size_lmm == "d") {
                    effect_size <- t_to_d(t_val, df, ci = 0.90, alternative = "two.sided")
                  } else if (input$effect_size_lmm == "r") {
                    effect_size <- t_to_r(t_val, df, ci = 0.90, alternative = "two.sided")
                  } else {
                    effect_size <- t_to_eta2(t_val, df, ci = 0.90, alternative = "two.sided")
                  }
                  effect_size_matrix[i, "Parameter"] <- param_name
                  effect_size_matrix[i, "effect_size"] <- round(effect_size[1,1], 3)
                  effect_size_matrix[i, "CI_low_effect_size"] <- round(effect_size$CI_low, 3)
                  effect_size_matrix[i, "CI_high_effect_size"] <- round(effect_size$CI_high, 3)
              }
            }
          }
          
          # Adding effect size values to parameters_mixed
          for (i in 1:nrow(parameters_mixed)) {
            param_name <- parameters_mixed[i, "Parameter"]
            row_idx <- which(effect_size_matrix[, "Parameter"] == param_name)
            if (length(row_idx) > 0) {
              parameters_mixed[i, "effect_size"] <- effect_size_matrix[row_idx, "effect_size"]
              parameters_mixed[i, "CI_low_effect_size"] <- effect_size_matrix[row_idx, "CI_low_effect_size"]
              parameters_mixed[i, "CI_high_effect_size"] <- effect_size_matrix[row_idx, "CI_high_effect_size"]
            }
          }
          
          # Restores the original names to the variables from the LMM
          parameters_mixed$Parameter <- gsub("group_var", paste0("[", input$group_var_icc, "]"), parameters_mixed$Parameter)
          parameters_mixed$Parameter <- gsub("x_var", input$x_var_icc, parameters_mixed$Parameter)
          parameters_mixed$Parameter <- gsub("y_var", input$y_var_icc, parameters_mixed$Parameter)
          parameters_mixed$Parameter <- gsub("z_var", input$z_var_icc, parameters_mixed$Parameter)
          parameters_mixed$Parameter <- gsub("w_var", input$w_var_icc, parameters_mixed$Parameter)
          if (isTRUE(input$repeated_var_icc_selection)){ 
            parameters_mixed$Parameter <- gsub("repeated_var", input$repeated_var_icc, parameters_mixed$Parameter)
          }
          
          parameters_mixed$Group <- gsub("x_var", input$x_var_icc, parameters_mixed$Group)
          parameters_mixed$Group <- gsub("y_var", input$y_var_icc, parameters_mixed$Group)
          parameters_mixed$Group <- gsub("z_var", input$z_var_icc, parameters_mixed$Group)
          
          
          #builds HTML tables from parameters_mixed
            # for fixed effects
            fixed_effects_df <- data.frame(
              Parameter = parameters_mixed$Parameter[parameters_mixed$Effects == "fixed"],
              Prediction = signif(parameters_mixed$Coefficient[parameters_mixed$Effects == "fixed"], 3),
              `Prediction 95 % CI` = paste0("[", signif(parameters_mixed$CI_low[parameters_mixed$Effects == "fixed"], 3), ", ", signif(parameters_mixed$CI_high[parameters_mixed$Effects == "fixed"], 3), "]"),
              check.names = FALSE
            )
            
            # Setting t column name dynamically
            t_val <- as.numeric(parameters_mixed$df_error[1])
            col_name <- paste("t(", t_val, ")", sep = "")
            fixed_effects_df[, col_name] <- c(signif(parameters_mixed$t[parameters_mixed$Effects == "fixed"], 3))
            
            # Adds the remaining columns
            fixed_effects_df$p <- c(signif(parameters_mixed$p[parameters_mixed$Effects == "fixed"]))
            if (input$effect_size_lmm == "eta2"){
                fixed_effects_df$`ηₚ²` <- c(parameters_mixed$effect_size[parameters_mixed$Effects == "fixed"])
                fixed_effects_df$`ηₚ² 90% CI` <- paste0("[", parameters_mixed$CI_low_effect_size[parameters_mixed$Effects == "fixed"], ", ",parameters_mixed$CI_high_effect_size[parameters_mixed$Effects == "fixed"], "]")
                # blanks effect size confidence interval for further table display
                fixed_effects_df$`ηₚ² 90% CI`[fixed_effects_df$Parameter == '(Intercept)'] = ""
              } else if (input$effect_size_lmm == "omega2") {
                fixed_effects_df$`ωₚ²` <- c(parameters_mixed$effect_size[parameters_mixed$Effects == "fixed"])
                fixed_effects_df$`ωₚ² 90% CI` <- paste0("[", parameters_mixed$CI_low_effect_size[parameters_mixed$Effects == "fixed"], ", ",parameters_mixed$CI_high_effect_size[parameters_mixed$Effects == "fixed"], "]")
                # blanks effect size confidence interval for further table display
                fixed_effects_df$`ωₚ² 90% CI`[fixed_effects_df$Parameter == '(Intercept)'] = ""
              } else if (input$effect_size_lmm == "epsilon2") {
                fixed_effects_df$`εₚ²` <- c(parameters_mixed$effect_size[parameters_mixed$Effects == "fixed"])
                fixed_effects_df$`εₚ² 90% CI` <- paste0("[", parameters_mixed$CI_low_effect_size[parameters_mixed$Effects == "fixed"], ", ",parameters_mixed$CI_high_effect_size[parameters_mixed$Effects == "fixed"], "]")
                # blanks effect size confidence interval for further table display
                fixed_effects_df$`εₚ² 90% CI`[fixed_effects_df$Parameter == '(Intercept)'] = ""
              } else if (input$effect_size_lmm == "cohen_f") {
                fixed_effects_df$`fₚ²` <- c(parameters_mixed$effect_size[parameters_mixed$Effects == "fixed"])
                fixed_effects_df$`fₚ² 90% CI` <- paste0("[", parameters_mixed$CI_low_effect_size[parameters_mixed$Effects == "fixed"], ", ",parameters_mixed$CI_high_effect_size[parameters_mixed$Effects == "fixed"], "]")
                # blanks effect size confidence interval for further table display
                fixed_effects_df$`fₚ² 90% CI`[fixed_effects_df$Parameter == '(Intercept)'] = ""
              } else if (input$effect_size_lmm == "d") {
                fixed_effects_df$d <- c(parameters_mixed$effect_size[parameters_mixed$Effects == "fixed"])
                fixed_effects_df$`d 90% CI` <- paste0("[", parameters_mixed$CI_low_effect_size[parameters_mixed$Effects == "fixed"], ", ",parameters_mixed$CI_high_effect_size[parameters_mixed$Effects == "fixed"], "]")
                # blanks effect size confidence interval for further table display
                fixed_effects_df$`d 90% CI`[fixed_effects_df$Parameter == '(Intercept)'] = ""
              } else if (input$effect_size_lmm == "r") {
                fixed_effects_df$rₚ <- c(parameters_mixed$effect_size[parameters_mixed$Effects == "fixed"])
                fixed_effects_df$`rₚ 90% CI` <- paste0("[", parameters_mixed$CI_low_effect_size[parameters_mixed$Effects == "fixed"], ", ",parameters_mixed$CI_high_effect_size[parameters_mixed$Effects == "fixed"], "]")
                # blanks effect size confidence interval for further table display
                fixed_effects_df$`rₚ 90% CI`[fixed_effects_df$Parameter == '(Intercept)'] = ""
              }
            
            #for random effects
            random_effects_df <- data.frame(
              Parameter = parameters_mixed$Parameter[parameters_mixed$Effects == "random"],
              Observations = parameters_mixed$Observations[parameters_mixed$Effects == "random"],
              `σ²` = signif(parameters_mixed$Coefficient[parameters_mixed$Effects == "random"], 3),
              `σ² 95 % CI` = paste0("[", signif(parameters_mixed$CI_low[parameters_mixed$Effects == "random"], 3), ", ", signif(parameters_mixed$CI_high[parameters_mixed$Effects == "random"], 3), "]"),
              `σ² component` = signif(parameters_mixed$var_component[parameters_mixed$Effects == "random"],2),
              `σ² component 95 % CI` = paste0("[", signif(parameters_mixed$CI_low_var_component[parameters_mixed$Effects == "random"], 2), ", ", signif(parameters_mixed$CI_high_var_component[parameters_mixed$Effects == "random"], 2), "]"),
              check.names = FALSE
            )
            
            # gets marginal and conditional R squared 
            r_squared_model <- as.data.frame(r.squaredGLMM(mixed))
            r_squared_df <- data.frame(
              `Marginal R²` = signif(r_squared_model[1,1], 3),
              `Conditional R²` = signif(r_squared_model[1,2], 3),
              check.names = FALSE
            )
            
          
          # Generates table with fixed and random effects
          opts <- options(knitr.kable.NA = "")
          fixed_effects <- kable(fixed_effects_df, digits = 5, align = "lcccccc", caption = "Fixed effects", valign = 't', table.attr = "class=\"table table-striped table-auto\"", format = "html")
          random_effects <- kable(random_effects_df, digits = 5, align = "lcccccc", caption = "Random effects", valign = 't', table.attr = "class=\"table table-striped table-auto\"", format = "html")
          r_squared <- kable(r_squared_df, digits = 5, align = "lcccccc", caption = "R squared", valign = 't', table.attr = "class=\"table table-striped table-auto\"", format = "html")
          combined_table <- kables(list(fixed_effects, random_effects, r_squared), caption = 'Two tables side by side')
          combined_table <- paste(fixed_effects, random_effects, r_squared, sep = "<br>")
          
          # Prints the table
          output$data_table_icc_two_lmm <- renderUI({
            HTML(combined_table)
          })
          
          # downloads generated table as .xlsx
          output$download_data_table_icc_two_lmm <- downloadHandler(
            filename = "combined_table.xlsx",
            content = function(file) {
              # Create a workbook
              wb <- createWorkbook()
              # Add sheets to the workbook
              addWorksheet(wb, sheetName = "Fixed Effects")
              writeData(wb, sheet = "Fixed Effects", fixed_effects_df)
              
              addWorksheet(wb, sheetName = "Random Effects")
              writeData(wb, sheet = "Random Effects", random_effects_df)
              
              addWorksheet(wb, sheetName = "R Squared")
              writeData(wb, sheet = "R Squared", r_squared_df)
              
              # Save the workbook
              saveWorkbook(wb, file = file, overwrite = TRUE)
            }
          )
          
          # downloads generated plot as .png
          output$download_plot_icc_two <- downloadHandler(
            filename = "icc.png",
            content = function(file) {
              # Save the superplot as a PNG file
              ggsave(file, plot = plot_for_all_icc_two(), device = "png")
            }
          )
          
          # For brushed points when plotting
          shinyjs::show("icc_two_brush_selection")
          
          # hides loading message if applicable
          shinyjs::hide("loading-message")
          
        }
        }, error = function(e) {
          # Handle the error (e.g., print a message)
          output$lmm_error_message <- renderText({
            ("LMM could not be calculated. Please check data selection")
          })
          output$icc_two_plot <- renderPlot({NULL})
          shinyjs::hide("download_data_table_icc_two_lmm")
          shinyjs::hide("download_plot_icc_two")
          shinyjs::hide("height_plot_icc_two")
          shinyjs::hide("width_plot_icc_two")
          output$data_table_icc_two <- renderUI({NULL})
          shinyjs::disable("options_icc_two")
          shinyjs::hide("icc_two_brush_selection")
        })
      })
      
      # runs submit_plot_icc_two action button so that click count is updated
      observeEvent(input$update_options_icc_two, {
        runjs('$("#submit_plot_icc_two").click();')
      }) 
  ##############################################################################
  ##############################################################################
  #Superplots and Bar/Box/Scatter/Violin plots
    #For data input
    data_plot <- reactive({
      tryCatch({
        req(!is.null(input$data_file_superplot) || !is.null(input$data_file_box_bar_scatter_violin))
        
        # hides inputs and resets plots and error text messages when new file is uploaded
        output$box_bar_scatter_violin_plot <- renderPlot({return(NULL)})
        output$box_bar_scatter_violin_plot_with_effect_size <- renderPlot({return(NULL)})
        shinyjs::hide("download_box_bar_scatter_violin")
        shinyjs::hide("download_box_bar_scatter_violin_effect_sizes")
        shinyjs::hide("download_box_bar_scatter_violin_table_all")
        output$error_message_box_bar_scatter_violin_data <- renderText({NULL})
        output$box_bar_scatter_violin_error_message <- renderText({NULL})
        output$box_bar_scatter_violin_effect_size_table <- renderUI({NULL})
        shinyjs::hide("show_additional_group_legend_superplot")
        
        #resets inputs and plot when new file is uploaded
        for (input_id in input_ids_to_reset()) {
          shinyjs::reset(input_id)
        }
        bar_box_scatter_violin_plot <- NULL
        output$box_bar_scatter_violin_plot_rendered = NULL
        #For superplot and box/bar/violin/raincloud plot
        if (!is.null(input$data_file_superplot)) {
          read_excel(input$data_file_superplot$datapath)%>%
            mutate_if(is.character, as.factor) # Convert character columns to factor
        } else if (!is.null(input$data_file_box_bar_scatter_violin)) {
          read_excel(input$data_file_box_bar_scatter_violin$datapath)%>%
            mutate_if(is.character, as.factor)
        }
      }, error = function(e) {
        output$error_message_box_bar_scatter_violin_data <- renderText({"An error ocurred with your data file upload. Please check your data structure and column names"})
      })
    })
    
    # For superplot 
      #resets inputs and plot when new file is uploaded  
      observeEvent(input$data_file_superplot,{
        # renders all plots, messages and tables as NULL
        output$superplot_plot <- renderPlot({return(NULL)})
        output$superplot_plot_with_effect_sizes <- renderPlot({return(NULL)})
        #output$data_superplot_plot_rendered <- renderUI({return(NULL)})
        output$error_message_superplot_data <- renderText({NULL})
        output$superplot_error_message <- renderText({NULL})
        output$superplot_effect_size_table <- renderUI({NULL})
        
        # hides buttons
        shinyjs::hide("download_data_superplot_plot_rendered")
        shinyjs::hide("download_data_superplot_plot_rendered_effect_sizes")
        shinyjs::hide("download_box_bar_scatter_violin_table_all")
        shinyjs::hide("show_additional_group_legend_superplot")
        shinyjs::hide("superplot_brush_selection")
        shinyjs::hide("options_superplot")
        shinyjs::reset("test_groups_superplot")
        shinyjs::hide("height_plot_superplot")
        shinyjs::hide("width_plot_superplot")
        
        # # restores selection
        # updateSelectInput(session, "num_levels_box_bar_scatter_violin", "Select the number of levels in your data", choices = c(1, 2, 3, 4))
        updateSelectInput(session, "number_effect_sizes_box_bar_scatter_violin",  "Number of effect sizes to display:", choices =c(0,1,2), selected = 0)
        # updateNumericInput(session, "n_boot_box_bar_scatter_violin", "Number of boostrap samples:", value = NULL)
        # updateCheckboxInput(session, "repeated_observations_box_bar_scatter_violin", "Paired observations?", value = FALSE)
        # updateSelectInput(session, "paired_observations_box_bar_scatter_violin", "Select column for paired observations", "")
        # updateSelectInput(session, "control_group_box_bar_scatter_violin", "Select control group", choices = NULL, selected = NULL)
        # updateSelectInput(session, "test_groups_box_bar_scatter_violin", "Select test group(s) and their order", choices = NULL, selected = NULL)
        
      })
      
      # hides selection for superplot until file is uploaded
      output$data_file_superplot_uploaded <- reactive({
        return(!is.null(input$data_file_superplot))
      })
      
      outputOptions(output, 'data_file_superplot_uploaded', suspendWhenHidden=FALSE)
      
      #Populate grouping variable dropdown menu
      observe({
        req(data_plot())
        updateSelectInput(session, "group_var_superplot", "Select the grouping variable", choices = colnames(data_plot()))
      })
      
      # Populate level 1 column dropdown menu
      observe({
        req(data_plot(), input$group_var_superplot)
        updateSelectInput(session, "x_var_superplot", "Select the column for level 1", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_superplot])
      })
      
      # Populate level 2 column dropdown menu
      observe({
        req(data_plot(), input$group_var_superplot, input$x_var_superplot)
        updateSelectInput(session, "y_var_superplot", "Select the column for level 2", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_superplot & colnames(data_plot()) != input$x_var_superplot])
        updateSelectInput(session, "y_var_box_bar_scatter_violin", "Select the column for additional grouping", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_box_bar_scatter_violin & colnames(data_plot()) != input$x_var_box_bar_scatter_violin])
      })
      
      # Populate group order
      observe({
        req(input$submit_plot_superplot, data_plot(), input$group_var_superplot, input$x_var_superplot)
        updateSelectInput(session, "group_order_superplot", "Select order and variables to show", choices = na.omit(data_plot()[[input$group_var_superplot]]))
      })
      
      # # Populate additional group selection
      # observe({
      #   req(input$submit_plot_superplot, data_plot(), input$group_var_superplot, input$x_var_superplot)
      #     updateSelectInput(session, "additional_variable_superplot", "Select the column for additional grouping variable", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_superplot & colnames(data_plot()) != input$x_var_superplot & colnames(data_plot()) != input$y_var_superplot])
      # })
      
      # Populate additional group order
      observe({
        req(input$submit_plot_superplot, data_plot(), input$group_var_superplot, input$x_var_superplot)
        
        # filters data based on previous selection
        if (input$show_additional_group_legend_superplot == TRUE) {
          data <- data_plot()
          # filters data based on group_order
          if (!is.null(input$group_order_superplot)) {
            filtered_data <- data %>%
              filter(!!as.name(input$group_var_superplot) %in% input$group_order_superplot)
          } else {
            filtered_data <- data
          }
          
          # updates checkbox name
          # updateCheckboxInput(session, "show_additional_group_legend_superplot", label = paste0("Select additional grouping for ", input$x_var_superplot))
          # Filter choices for additional group order based on selected groups
          filtered_choices <- unique(na.omit(filtered_data[[input$x_var_superplot]]))
          updateSelectInput(session, "additional_group_order_superplot", "Select order and variables to shows", choices = filtered_choices)
        }
      })
      
      # Populate repeated measures variable
      observe({
        req(input$submit_plot_superplot, data_plot(), input$group_var_superplot, input$x_var_superplot)
        updateSelectInput(session, "paired_observations_box_bar_scatter_violin", "Select column for paired observations", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_superplot & colnames(data_plot()) != input$x_var_superplot & colnames(data_plot()) != input$y_var_superplot])
      })
      
      # observe for different colour coding when plot is generated for the first time
      observeEvent(input$x_var_superplot,{
        #req(input$x_var_superplot >0)
        if (input$navpage == 'Superplot') {
          tab_id = "superplot"
          data <- data_plot()
          x_var <- input$group_var_superplot
          y_var <- input$y_var_superplot
          z_var <- input$x_var_superplot
          
          if (length(selected_colors_superplot$colors) !=  length(unique(data[[z_var]]))) {
            updateSelectInput(session, paste0("geom_jitter_colour_fill_by_group_", tab_id), choices = list("Single Colour", "Colour by Group - discrete" = c(x_var, z_var), "Colour by Group - gradient"= c(paste0(x_var, " "), paste0(z_var, " "))), selected = z_var)
            updateSelectInput(session, paste0("geom_boxplot_colour_fill_by_group_", tab_id), choices = list("Single Colour", "Colour by Group - discrete" = c(x_var, z_var), "Colour by Group - gradient"= c(paste0(x_var, " "), paste0(z_var, " "))), selected = z_var)
            # generates random colours symbols
            random_colours <- randomColor(length(unique(data[[z_var]])))
            # assigns random colours for symbol border and fill
            selected_colors_superplot$colors <- random_colours
            selected_colours_symbol_fill_superplot$colors <- random_colours
            # assigns random colours for box/violin/bar border and fill
            selected_colors_boxplot_line_superplot$colors <- random_colours
            selected_colors_boxplot_fill_superplot$colors <- random_colours
            # sets legend in place
            updateSelectInput(session, paste0("maginal_plot_legend_", tab_id), label = "Legend position", choices = c("none", "left","top", "right", "bottom", "custom"), selected = "right")
          }
        }
      })
      
      # Populate select and test groups for effect size calculation
      observe({
        req(data_plot(), input$group_var_superplot, input$x_var_superplot, tab_id() == "superplot")
        
        # filters data based on group_order
        if (!is.null(input$group_order_superplot)) {
          filtered_data <- data_plot() %>%
            filter(!!as.name(input$group_var_superplot) %in% input$group_order_superplot)
        } else {
          filtered_data <- data_plot()
        }
        
        
        updateSelectInput(session, "control_group_superplot", "Select control group", choices = filtered_data[[input$group_var_superplot]])
        updateSelectInput(session, "test_groups_superplot", "Select test group(s) and their order", choices = setdiff(filtered_data[[input$group_var_superplot]], input$control_group_superplot))
      })
      
      #Resets y and x axis labels when variables are changed
      observeEvent(input$y_var_superplot,{
        if (input$num_levels_box_bar_scatter_violin == 2) {
          updateTextInput(session, "y_axis_text_title_superplot", value = input$y_var_superplot)
        }
      })
      
      observeEvent(input$group_var_superplot,{
        updateTextInput(session, "x_axis_text_title_superplot", value = input$group_var_superplot)
      })
      
      #Renders superplot after click
      observeEvent(input$submit_plot_superplot, {
        # adds tryCatch for any error that may occur
        tryCatch({ 
          #Renders Plot
          # if modal is open, requires click to update pot
          if (!is.null(plot_for_all_superplot()) && input$modal_superplot == TRUE &&  click_count_superplot() > 0) {
            # Check if update button is clicked
            output$superplot_plot <- renderPlot({
              plot_for_all_superplot()
            }, height = input$height_plot_superplot, width = input$width_plot_superplot
            )
            
            # shows numeric inputs for plot size
            shinyjs::show("height_plot_superplot")
            shinyjs::show("width_plot_superplot")
            shinyjs::enable("options_superplot")
            
            # if modal is not open
          } else if (input$modal_superplot != TRUE && !is.null(plot_for_all_superplot()) && click_count_superplot() == 0) {
            output$superplot_plot <- renderPlot({
              plot_for_all_superplot()
            }, height = input$height_plot_superplot, width = input$width_plot_superplot
            )
            
            # shows numeric inputs and download buttons for plot
            shinyjs::show("height_plot_superplot")
            shinyjs::show("width_plot_superplot")
            shinyjs::show("options_superplot")
            shinyjs::show("download_superplot")
            shinyjs::enable("options_superplot")
          }
          
          # hides plot with effect size
          shinyjs::hide("superplot_plot_with_effect_sizes")
          
          reset_info_superplot(0)
          
          # shows plot if hidden
          shinyjs::show("superplot_plot")
          
          # resets text messages
          output$superplot_error_message <- renderText({NULL})
          output$error_message_superplot_data <- renderText({NULL})
          
          
          # Download the superplot
          output$download_superplot <- downloadHandler(
            filename = "plot.png",
            content = function(file) {
              # Save the superplot as a PNG file
              ggsave(file, plot = plot_for_all_superplot(), device = "png")
            }
          )
          
          # For brushed points when plotting (NEEDS FIXING)
          shinyjs::show("superplot_brush_selection")
          
          
        },  error = function(e) {
          # disables plots, and buttons if plot is not rendered
          output$superplot_plot <- renderPlot({NULL})
          shinyjs::hide("download_superplot")
          shinyjs::hide("height_plot_superplot")
          shinyjs::hide("width_plot_superplot")
          #table with data and results
          output$data_superplot_plot_rendered <- renderUI({NULL})
          shinyjs::disable("options_superplot")
          shinyjs::hide("superplot_brush_selection")
          # if effect sizes are selected
          shinyjs::hide("download_superplot_effect_sizes")
          shinyjs::hide("download_superplot_table_all")
          
          output$superplot_plot_with_effect_sizes <- renderPlot({NULL})
          
          if (input$submit_plot_superplot[1] != 0){
            # Handle the error
            output$superplot_error_message <- renderText({
              ("Plot could not be generated. Please check data and plot selection")
            }) 
          }
        })
      })
      
      # Download the superplot with effect sizes
      output$download_superplot_effect_sizes <- downloadHandler(
        filename = "superplot_with_effect_sizes.png",
        content = function(file) {
          # Save the superplot with effect sizes as a PNG file
          ggsave(file, plot = superplot_with_effect_size_plot() , device = "png")
        }
      )
      
      # Reset superplot button
      observeEvent(input$reset_superplot,{
        tryCatch({
          
          # disables plots, and buttons if plot is not rendered
          output$superplot_plot <- renderPlot({NULL})
          shinyjs::hide("download_superplot")
          shinyjs::hide("height_plot_superplot")
          shinyjs::hide("width_plot_superplot")
          
          #table with data and results
          output$data_superplot_plot_rendered <- renderUI({NULL})
          shinyjs::hide("options_superplot")
          shinyjs::hide("superplot_brush_selection")
          
          #sets click count to zero
          click_count_superplot(0)
          
          # if effect sizes are selected
          output$superplot_plot_with_effect_sizes <- renderPlot({NULL})
          shinyjs::hide("download_superplot_effect_sizes")
          shinyjs::hide("download_superplot_table_all")
          output$superplot_effect_size_table <- renderUI({NULL})
          
          # Reset specific input values using shinyjs::reset
          for (input_id in input_ids_to_reset()) {
            shinyjs::reset(input_id)
          }
          
            # # for symbol colour and fill
            # for (i in seq_along(selected_colors_superplot$colors)) {updateColourInput(session, paste0("color_var_superplot", i), value = "black")}
            # selected_colors_superplot$colors <- rep("black", length(selected_colors_superplot$colors))
            # for (i in seq_along(selected_colours_symbol_fill_superplot$colors)) {updateColourInput(session, paste0("color_symbol_fill_var_superplot", i), value = "black")}
            # selected_colours_symbol_fill_superplot$colors <- rep("black", length(selected_colours_symbol_fill_superplot$colors))
            # updateSelectInput(session, "geom_jitter_colour_fill_by_group_superplot", selected = "Single Colour")
        })
      })
      
      # Generates the combined plot with superplot and effect sizes
      superplot_with_effect_size_plot <- reactive({
        if ((input$navpage == 'Superplot' && !is.null(input$test_groups_superplot) && input$n_boot_superplot >= 2)){
          req(mean_median_effect_size_plot(), plot_for_all_superplot())
          
          # gets legend
          legend_for_all_superplot <- cowplot::get_legend(plot_for_all_superplot())   
          
          if (input$number_effect_sizes_superplot == 1 && input$position_plot_mean_median_diff_superplot == "Aligned with mean/median") {
            if (input$legend_outside_main_plot_superplot == TRUE){
              main_plot <-cowplot::plot_grid(plot_for_all_superplot() + theme(legend.position = "none"), mean_median_effect_size_plot(), align = "hv",  axis = "tb", rel_widths = c(4, 2))
              cowplot::plot_grid(main_plot,  legend_for_all_superplot, align = "hv",  axis = "tb", rel_widths = c(4, 1))
            } else if (input$legend_outside_main_plot_superplot == FALSE) {
              cowplot::plot_grid(plot_for_all_superplot() , mean_median_effect_size_plot(),  align = "hv",  axis = "tb", rel_widths = c(4, 2))
            }
            
          } else if (input$number_effect_sizes_superplot == 1 && input$position_plot_mean_median_diff_superplot == "Below data") {
            if (input$legend_outside_main_plot_superplot == TRUE){
              main_plot <- cowplot::plot_grid(plot_for_all_superplot() + theme(legend.position = "none"), mean_median_effect_size_plot(),  align = "v", axis = "l", rel_heights = c(4, 2), ncol = 1)
              legend_plot <- cowplot::plot_grid(legend_for_all_superplot, NULL,  align = "v", axis = "l", rel_heights = c(4, 2), ncol = 1)
              cowplot::plot_grid(main_plot, legend_plot, nrow = 1, ncol = 2, rel_widths = c(4, 1), rel_heights = c(1, 0.5), align = 'h', axis = 't')
            } else if (input$legend_outside_main_plot_superplot == FALSE) {
              cowplot::plot_grid(plot_for_all_superplot() , mean_median_effect_size_plot(),  align = "v", axis = "l", rel_heights = c(4, 2), ncol = 1)
            }
          } else if (input$number_effect_sizes_superplot == 2 && input$position_plot_mean_median_diff_superplot == "Aligned with mean/median") {
            if (input$legend_outside_main_plot_superplot == TRUE){
              # adjusts plots without legend shown in main plot
              #first align data plot with effect size on the bottom panel
              plots <- cowplot::align_plots(plot_for_all_superplot() + theme(legend.position = "none"), hedges_cohen_glass_effect_size_plot(), align = 'v', axis = 'l')
              top_row <- cowplot::plot_grid(plots[[1]], mean_median_effect_size_plot(), align = 'h', axis = 'tb', rel_widths = c(4,2))
              bottom_row <- cowplot::plot_grid(plots[[2]], ggplot() + theme_void(), align = "hv", axis = "tb", rel_widths = c(4,2))
              legend_plot <- cowplot::plot_grid(legend_for_all_superplot, NULL, align = "v", axis = "l", ncol =1, nrow = 2, rel_heights = c(2,1), rel_widths = c(2,1))
              main_plot <- cowplot::plot_grid(top_row, bottom_row, align = "v", axis = "l", ncol =1, nrow = 2, rel_heights = c(2,1), rel_widths = c(2,1))
              cowplot::plot_grid(main_plot, legend_plot, nrow = 1, ncol = 2, rel_widths = c(4, 1), rel_heights = c(1, 1), align = 'h', axis = 't')
              
            } else if (input$legend_outside_main_plot_superplot == FALSE) {
              #first align data plot with effect size on the bottom panel
              plots <- cowplot::align_plots(plot_for_all_superplot(), hedges_cohen_glass_effect_size_plot(), align = 'v', axis = 'l')
              top_row <- cowplot::plot_grid(plots[[1]], mean_median_effect_size_plot(), align = 'h', axis = 'tb', rel_widths = c(4,2))
              bottom_row <- cowplot::plot_grid(plots[[2]], ggplot() + theme_void(), align = "hv", axis = "tb", rel_widths = c(4,2))
              cowplot::plot_grid(top_row, bottom_row, align = "v", axis = "l", ncol =1, nrow = 2, rel_heights = c(2,1), rel_widths = c(2,1))
            }
            
          }else if (input$number_effect_sizes_superplot == 2 && input$position_plot_mean_median_diff_superplot == "Below data") {
            if (input$legend_outside_main_plot_superplot == TRUE){
              main_plot <- cowplot::plot_grid(plot_for_all_superplot() + theme(legend.position = "none"), mean_median_effect_size_plot(), hedges_cohen_glass_effect_size_plot(), align = "v", axis = "l", ncol = 1, nrow = 3, rel_heights = c(3, 2, 2), 
                                              height = 1000, width = 800)
              legend_plot <- cowplot::plot_grid(legend_for_all_superplot, NULL, NULL, align = "v", axis = "l", ncol = 1, nrow = 3, rel_heights = c(3, 2, 2))
              cowplot::plot_grid(main_plot, legend_plot, nrow = 1, ncol = 2, rel_widths = c(4, 1), rel_heights = c(1, 1), align = 'h', axis = 't')
            } else if (input$legend_outside_main_plot_superplot == FALSE) {
              cowplot::plot_grid(plot_for_all_superplot() , mean_median_effect_size_plot(), hedges_cohen_glass_effect_size_plot(), align = "v", axis = "l", ncol = 1, nrow = 3, rel_heights = c(3, 2, 2), 
                                 height = 1000, width = 800)
            }
          }
        }
      })  
      
      #Renders superplot with effect sizes after click
      observeEvent(input$submit_superplot_with_effect_sizes,{
        if ((input$navpage == 'Superplot' && !is.null(input$test_groups_superplot) && input$n_boot_superplot >= 2)){
          
          # adds tryCatch for any error that may occur
          tryCatch({
            
            #Renders Plot
            # if modal is open, requires click to update plot
            if (!is.null(plot_for_all_superplot()) && input$modal_superplot == TRUE &&  click_count_superplot() > 0) {
              # Check if update button is clicked
              output$superplot_plot_with_effect_sizes <- renderPlot({
                superplot_with_effect_size_plot()
              }, height = input$height_plot_superplot, width = input$width_plot_superplot
              )
              
              # shows numeric inputs for plot size
              shinyjs::show("height_superplot")
              shinyjs::show("width_superplot")
              shinyjs::show("options_superplot")
              shinyjs::show("download_superplot_effect_sizes")
              shinyjs::show("download_superplot_table_all")
              
              # if modal is not open
            } else if (input$modal_superplot != TRUE && !is.null(plot_for_all_superplot()) && click_count_superplot() == 0) {
              output$superplot_plot_with_effect_sizes <- renderPlot({
                superplot_with_effect_size_plot()
              }, height = input$height_plot_superplot, width = input$width_plot_superplot
              )
              
              # shows numeric inputs and download buttons for plot
              shinyjs::show("height_plot_superplot")
              shinyjs::show("width_plot_superplot")
              shinyjs::show("options_superplot")
              shinyjs::show("download_superplot_effect_sizes")
              shinyjs::show("download_superplot_table_all")
            }
            
            
            # hides plot without effect size
            shinyjs::hide("superplot_plot")
            
            # shows plot if hidden
            shinyjs::show("superplot_plot_with_effect_sizes")
            
            # resets previous plot
            output$superplot_plot <- renderPlot({NULL})
            
            # adds information about previous reset for update_button
            reset_info_superplot(1)
            
            # resets text messages
            output$superplot_error_message <- renderText({NULL})
            output$error_message_superplot_data <- renderText({NULL})
            
            # hides plot without effect size
            shinyjs::hide("superplot_plot")
            
            # Download the bar/box/scatter/violin
            output$download_superplot_effect_sizes <- downloadHandler(
              filename = "plot.png",
              content = function(file) {
                # Save the superplot as a PNG file
                ggsave(file, plot = bar_box_scatter_violin_with_effect_size_plot(), device = "png")
              }
            )
            
            #Download data descriptive statistics and effect sizes
            output$download_superplot_table_all <- downloadHandler(
              filename = "list_of_effect_size_results.xlsx",
              content = function(file) {
                saveWorkbook(list_of_effect_size_results(), file = file, overwrite = TRUE)
              }
            )
            
            # For brushed points when plotting
            shinyjs::hide("superplot_brush_selection")
            
          },  error = function(e) {
            # disables plots, and buttons if plot is not rendered
            output$superplot_plot_with_effect_sizes <- renderPlot({NULL})
            shinyjs::hide("download_superplot")
            shinyjs::hide("height_plot_superplot")
            shinyjs::hide("width_plot_superplot")
            #table with data and results
            output$data_superplot_plot_rendered <- renderUI({NULL})
            shinyjs::disable("options_superplot")
            shinyjs::hide("superplot_brush_selection")
            # if effect sizes are selected
            shinyjs::hide("download_superplot_effect_sizes")
            shinyjs::hide("download_superplot_table_all")
            
            output$superplot_plot <- renderPlot({NULL})
            
            # Handle the error
            if (input$submit_plot_superplot[1] != 0){
              output$superplot_error_message <- renderText({
                ("Plot could not be generated. Please check data and plot selection")
              })
            }
          })
        } 
      })
      
      # runs submit_superplot action button so that click count is updated
      observeEvent(input$update_options_superplot, {
        runjs('$("#submit_plot_superplot").click();')
      })
      
      # shows warning messages if applicable
      observeEvent(input$submit_superplot_with_effect_sizes,{
        # for number of bootstrap replicas
        if (is.na(input$n_boot_superplot) || input$n_boot_superplot <= 2) {
          shinyFeedback::feedbackWarning(inputId = "n_boot_superplot", text = "Please select a larger number of samples", show = TRUE)
        } else {
          hideFeedback("n_boot_superplot")
        }
        
        # for test group selection
        if (is.null(input$test_groups_superplot)){
          shinyFeedback::feedbackWarning(inputId = "test_groups_superplot", text = "Please select a test group", show = TRUE)
        } else {
          hideFeedback("test_groups_superplot")
        }
      })
      
      # runs submit_box_bar_scatter_violin_with_effect_sizes action button so that click count is updated
      observeEvent(input$update_options_superplot, {
        if (reset_info_superplot() >= 1){
          runjs('$("#submit_superplot_with_effect_sizes").click();')
        }
      })
    
    ############################################################################  
    # For bar/box/scatter/violin plot
    
      #resets inputs and plot when new file is uploaded  
      observeEvent(input$data_file_box_bar_scatter_violin,{
        # renders all plots, messages and tables as NULL
        output$box_bar_scatter_violin_plot <- renderPlot({return(NULL)})
        output$box_bar_scatter_violin_plot_with_effect_size <- renderPlot({return(NULL)})
        output$error_message_box_bar_scatter_violin_data <- renderText({NULL})
        output$box_bar_scatter_violin_error_message <- renderText({NULL})
        output$box_bar_scatter_violin_effect_size_table <- renderUI({NULL})
        
        # hides buttons
        shinyjs::hide("download_box_bar_scatter_violin")
        shinyjs::hide("download_box_bar_scatter_violin_effect_sizes")
        shinyjs::hide("download_box_bar_scatter_violin_table_all")
        shinyjs::hide("show_additional_group_legend_superplot")
        shinyjs::hide("box_bar_scatter_violin_brush_selection")
        shinyjs::hide("options_box_bar_scatter_violin")
        shinyjs::reset("test_groups_box_bar_scatter_violin")
        shinyjs::hide("height_plot_box_bar_scatter_violin")
        shinyjs::hide("width_plot_box_bar_scatter_violin")
          
        # # restores selection
        # updateSelectInput(session, "num_levels_box_bar_scatter_violin", "Select the number of levels in your data", choices = c(1, 2, 3, 4))
         updateSelectInput(session, "number_effect_sizes_box_bar_scatter_violin",  "Number of effect sizes to display:", choices =c(0,1,2), selected = 0)
         

        # updateNumericInput(session, "n_boot_box_bar_scatter_violin", "Number of boostrap samples:", value = NULL)
        # updateCheckboxInput(session, "repeated_observations_box_bar_scatter_violin", "Paired observations?", value = FALSE)
        # updateSelectInput(session, "paired_observations_box_bar_scatter_violin", "Select column for paired observations", "")
        # updateSelectInput(session, "control_group_box_bar_scatter_violin", "Select control group", choices = NULL, selected = NULL)
        # updateSelectInput(session, "test_groups_box_bar_scatter_violin", "Select test group(s) and their order", choices = NULL, selected = NULL)
        
      })
      
      # hides selection for box, bar, scatter, violin until file is uploaded
      output$data_file_box_bar_scatter_violin_uploaded <- reactive({
        return(!is.null(input$data_file_box_bar_scatter_violin))
      })
      
      outputOptions(output, 'data_file_box_bar_scatter_violin_uploaded', suspendWhenHidden=FALSE)
      
      # Populate grouping variable dropdown menu
      observe({
        req(!is.null(data_plot()))
        updateSelectInput(session, "group_var_box_bar_scatter_violin", "Select the grouping variable", choices = colnames(data_plot()))
      })
      
      # Populate level 1 column dropdown menu
      observe({
        req(data_plot(), input$group_var_box_bar_scatter_violin)
        updateSelectInput(session, "x_var_box_bar_scatter_violin", "Select the column for level 1", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_box_bar_scatter_violin])
      })
      
      # Populate level 2 column dropdown menu
      observe({
        req(data_plot(), input$group_var_box_bar_scatter_violin, input$x_var_box_bar_scatter_violin)
        updateSelectInput(session, "y_var_box_bar_scatter_violin", "Select the column for level 2", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_box_bar_scatter_violin & colnames(data_plot()) != input$x_var_box_bar_scatter_violin])
      })
      
      # Populate level 3 column dropdown menu
      observe({
        req(data_plot(), input$group_var_box_bar_scatter_violin, input$x_var_box_bar_scatter_violin, input$y_var_box_bar_scatter_violin)
        updateSelectInput(session, "z_var_box_bar_scatter_violin", "Select the column for level 3", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_box_bar_scatter_violin & colnames(data_plot()) != input$x_var_box_bar_scatter_violin & colnames(data_plot()) != input$y_var_box_bar_scatter_violin])
      })
      
      # Populate level 4 column dropdown menu
      observe({
        req(data_plot(), input$group_var_box_bar_scatter_violin, input$x_var_box_bar_scatter_violin, input$y_var_box_bar_scatter_violin, input$z_var_box_bar_scatter_violin)
        updateSelectInput(session, "w_var_box_bar_scatter_violin", "Select the column for level 4", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_box_bar_scatter_violin & colnames(data_plot()) != input$x_var_box_bar_scatter_violin & colnames(data_plot()) != input$y_var_box_bar_scatter_violin & colnames(data_plot()) != input$z_var_box_bar_scatter_violin])
      })
      
      # Populate group order
      observe({
        req(data_plot(), input$group_var_box_bar_scatter_violin, input$x_var_box_bar_scatter_violin, tab_id() == "box_bar_scatter_violin")
        updateSelectInput(session, "group_order_box_bar_scatter_violin", "Select order and variables to show", choices = na.omit(data_plot()[[input$group_var_box_bar_scatter_violin]]))
      })
      
      # Populate additional group selection
      observe({
        req(input$submit_plot_box_bar_scatter_violin, data_plot(), input$num_levels_box_bar_scatter_violin, input$group_var_box_bar_scatter_violin, input$x_var_box_bar_scatter_violin)
        if (input$num_levels_box_bar_scatter_violin == 1) {
          updateSelectInput(session, "additional_variable_box_bar_scatter_violin", "Select the column for additional grouping variable", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_box_bar_scatter_violin & colnames(data_plot()) != input$x_var_box_bar_scatter_violin])
        } else if (input$num_levels_box_bar_scatter_violin == 2) {
          updateSelectInput(session, "additional_variable_box_bar_scatter_violin", "Select the column for additional grouping variable", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_box_bar_scatter_violin & colnames(data_plot()) != input$x_var_box_bar_scatter_violin & colnames(data_plot()) != input$y_var_box_bar_scatter_violin])
        } else if (input$num_levels_box_bar_scatter_violin == 3) {
          updateSelectInput(session, "additional_variable_box_bar_scatter_violin", "Select the column for additional grouping variable", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_box_bar_scatter_violin & colnames(data_plot()) != input$x_var_box_bar_scatter_violin & colnames(data_plot()) != input$y_var_box_bar_scatter_violin & colnames(data_plot()) != input$z_var_box_bar_scatter_violin])
        } else if (input$num_levels_box_bar_scatter_violin == 4) { 
          updateSelectInput(session, "additional_variable_box_bar_scatter_violin", "Select the column for additional grouping variable", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_box_bar_scatter_violin & colnames(data_plot()) != input$x_var_box_bar_scatter_violin & colnames(data_plot()) != input$y_var_box_bar_scatter_violin & colnames(data_plot()) != input$z_var_box_bar_scatter_violin & colnames(data_plot()) != input$w_var_box_bar_scatter_violin])
        }
        
      })
      
      # Populate additional group order
      observe({
        req(input$submit_plot_box_bar_scatter_violin, data_plot(), input$num_levels_box_bar_scatter_violin, input$group_var_box_bar_scatter_violin, input$x_var_box_bar_scatter_violin)
        
        # filters data based on previous selection
        if (input$show_additional_group_legend_box_bar_scatter_violin == TRUE) {
          data <- data_plot()
          # filters data based on group_order
          if (!is.null(input$group_order_box_bar_scatter_violin)) {
            filtered_data <- data %>%
              filter(!!as.name(input$group_var_box_bar_scatter_violin) %in% input$group_order_box_bar_scatter_violin)
          } else {
            filtered_data <- data
          }
          
          # updates checkbox name
          updateCheckboxInput(session, "show_additional_group_legend_box_bar_scatter_violin", label = paste0("Select additional grouping for ", input$additional_variable_box_bar_scatter_violin))
          # Filter choices for additional group order based on selected groups
          filtered_choices <- unique(na.omit(filtered_data[[input$additional_variable_box_bar_scatter_violin]]))
          updateSelectInput(session, "additional_group_order_box_bar_scatter_violin", "Select order and variables to shows", choices = filtered_choices)
        }
      })
      
      # Populate repeated measures variable
      observe({
        req(input$submit_plot_box_bar_scatter_violin, data_plot(), input$num_levels_box_bar_scatter_violin, input$group_var_box_bar_scatter_violin, input$x_var_box_bar_scatter_violin)
        if (input$num_levels_box_bar_scatter_violin == 1) {
          updateSelectInput(session, "paired_observations_box_bar_scatter_violin", "Select column for paired observations", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_box_bar_scatter_violin & colnames(data_plot()) != input$x_var_box_bar_scatter_violin])
        } else if (input$num_levels_box_bar_scatter_violin == 2) {
          updateSelectInput(session, "paired_observations_box_bar_scatter_violin", "Select column for paired observations", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_box_bar_scatter_violin & colnames(data_plot()) != input$x_var_box_bar_scatter_violin & colnames(data_plot()) != input$y_var_box_bar_scatter_violin])
        } else if (input$num_levels_box_bar_scatter_violin == 3) {
          updateSelectInput(session, "paired_observations_box_bar_scatter_violin", "Select column for paired observations", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_box_bar_scatter_violin & colnames(data_plot()) != input$x_var_box_bar_scatter_violin & colnames(data_plot()) != input$y_var_box_bar_scatter_violin & colnames(data_plot()) != input$z_var_box_bar_scatter_violin])
        } else if (input$num_levels_box_bar_scatter_violin == 4) { 
          updateSelectInput(session, "paired_observations_box_bar_scatter_violin", "Select column for paired observations", choices = colnames(data_plot())[colnames(data_plot()) != input$group_var_box_bar_scatter_violin & colnames(data_plot()) != input$x_var_box_bar_scatter_violin & colnames(data_plot()) != input$y_var_box_bar_scatter_violin & colnames(data_plot()) != input$z_var_box_bar_scatter_violin & colnames(data_plot()) != input$w_var_box_bar_scatter_violin])
        }
      })
      
      # Populate select and test groups for effect size calculation
      observe({
        req(data_plot(), input$group_var_box_bar_scatter_violin, input$x_var_box_bar_scatter_violin, tab_id() == "box_bar_scatter_violin")
        
        # filters data based on group_order
        if (!is.null(input$group_order_box_bar_scatter_violin) && all(input$group_order_box_bar_scatter_violin %in% data_plot()[[input$group_var_box_bar_scatter_violin]])) {
          filtered_data <- data_plot() %>%
            filter(!!as.name(input$group_var_box_bar_scatter_violin) %in% input$group_order_box_bar_scatter_violin)
        } else {
          filtered_data <- data_plot()
        }
        
        
        updateSelectInput(session, "control_group_box_bar_scatter_violin", "Select control group", choices = na.omit(filtered_data[[input$group_var_box_bar_scatter_violin]]), selected = input$control_group_box_bar_scatter_violin)
        updateSelectInput(session, "test_groups_box_bar_scatter_violin", "Select test group(s) and their order", choices = setdiff(filtered_data[[input$group_var_box_bar_scatter_violin]], input$control_group_box_bar_scatter_violin))
        
      })
      
      #Resets y and x axis labels when variables are changed
      # for 2 levels
      observeEvent(input$y_var_box_bar_scatter_violin,{
        if (input$num_levels_box_bar_scatter_violin == 2) {
          updateTextInput(session, "y_axis_text_title_box_bar_scatter_violin", value = input$y_var_box_bar_scatter_violin)
        }
      })
      # for 3 levels
      observeEvent(input$z_var_box_bar_scatter_violin,{
        if (input$num_levels_box_bar_scatter_violin == 3) {
          updateTextInput(session, "y_axis_text_title_box_bar_scatter_violin", value = input$z_var_box_bar_scatter_violin)
        }
      })
      # for 4 levels
      observeEvent(input$w_var_box_bar_scatter_violin,{
        if (input$num_levels_box_bar_scatter_violin == 4) {
          updateTextInput(session, "y_axis_text_title_box_bar_scatter_violin", value = input$w_var_box_bar_scatter_violin)
        }
      })
      
      observeEvent(input$group_var_box_bar_scatter_violin,{
        updateTextInput(session, "x_axis_text_title_box_bar_scatter_violin", value = input$group_var_box_bar_scatter_violin)
      })
      
      #Updates colour by group to single colour when variables are updated
      observeEvent(input$group_var_box_bar_scatter_violin,{
        # Reset the symbol color option to "Single Colour"
        updateSelectInput(session, "geom_jitter_colour_fill_by_group", selected = "Single Colour")
        # Reset the boxplot color option to "Single Colour"
        updateSelectInput(session, "geom_boxplot_colour_fill_by_group", selected = "Single Colour")
      })
      
      #update colour selection UIs
      observeEvent(input$show_additional_group_legend_box_bar_scatter_violin, {
        if (input$show_additional_group_legend_box_bar_scatter_violin == FALSE){
          updateSelectInput(session, "geom_jitter_colour_fill_by_group", choices = list("Single Colour", "Colour by Group" = c(input$group_var_box_bar_scatter_violin,"")), selected = input$geom_jitter_colour_fill_by_group)
        } else if (input$show_additional_group_legend_box_bar_scatter_violin == TRUE) {
          updateSelectInput(session, "geom_jitter_colour_fill_by_group", choices = list("Single Colour", "Colour by Group" = c(input$group_var_box_bar_scatter_violin, input$y_var_box_bar_scatter_violin)), selected = input$geom_jitter_colour_fill_by_group)
        }
      })
      
      #Renders bar/box/scatter/violin plot
      observeEvent(input$submit_plot_box_bar_scatter_violin, {
        # adds tryCatch for any error that may occur
        tryCatch({ 
          #Renders Plot
          # if modal is open, requires click to update pot
          if (!is.null(plot_for_all_box_bar_scatter_violin()) && input$modal_box_bar_scatter_violin == TRUE &&  click_count_box_bar_scatter_violin() > 0) {
            # Check if update button is clicked
            output$box_bar_scatter_violin_plot <- renderPlot({
              if (!is.ggplot(plot_for_all_box_bar_scatter_violin())){
                plot_for_all_box_bar_scatter_violin()[[2]]
              } else {
                plot_for_all_box_bar_scatter_violin()    
              }
            }, height = input$height_plot_box_bar_scatter_violin, width = input$width_plot_box_bar_scatter_violin
            )
            
            # shows numeric inputs for plot size
            shinyjs::show("height_plot_box_bar_scatter_violin")
            shinyjs::show("width_plot_box_bar_scatter_violin")
            shinyjs::enable("options_box_bar_scatter_violin")
            
            # if modal is not open
          } else if (input$modal_box_bar_scatter_violin != TRUE && !is.null(plot_for_all_box_bar_scatter_violin()) && click_count_box_bar_scatter_violin() == 0) {
            output$box_bar_scatter_violin_plot <- renderPlot({
              if (!is.ggplot(plot_for_all_box_bar_scatter_violin())){
                plot_for_all_box_bar_scatter_violin()[[2]]
              } else {
                plot_for_all_box_bar_scatter_violin()    
              }
            }, height = input$height_plot_box_bar_scatter_violin, width = input$width_plot_box_bar_scatter_violin
            )
            
            # shows numeric inputs and download buttons for plot
            shinyjs::show("height_plot_box_bar_scatter_violin")
            shinyjs::show("width_plot_box_bar_scatter_violin")
            shinyjs::show("options_box_bar_scatter_violin")
            shinyjs::show("download_box_bar_scatter_violin")
            shinyjs::enable("options_box_bar_scatter_violin")
          }
          
          # hides plot with effect size and stops its spinner
          #shinyjs::toggleSpinner("box_bar_scatter_violin_plot_with_effect_size", false)  # Hide the spinner
          shinyjs::hide("box_bar_scatter_violin_plot_with_effect_size")
          hide_spinner("box_bar_scatter_violin_plot_with_effect_size")
          
          # hides effect size information
          shinyjs::hide("download_box_bar_scatter_violin_effect_sizes")
          shinyjs::hide("download_box_bar_scatter_violin_table_all")
          shinyjs::hide("box_bar_scatter_violin_effect_size_table")
          
          reset_info_box_bar_scatter_violin_plot(0)
          
          # shows plot if hidden
          shinyjs::show("box_bar_scatter_violin_plot")
          
          # resets text messages
          output$box_bar_scatter_violin_error_message <- renderText({NULL})
          output$error_message_box_bar_scatter_violin_data <- renderText({NULL})
          
          
          # Download the bar/box/scatter/violin
          output$download_box_bar_scatter_violin <- downloadHandler(
            filename = "plot.png",
            content = function(file) {
              # Save the superplot as a PNG file
              ggsave(file, plot = plot_for_all_box_bar_scatter_violin(), device = "png")
            }
          )
          
          # For brushed points when plotting (NEEDS FIXING)
          shinyjs::show("box_bar_scatter_violin_brush_selection")
          
        },  error = function(e) {
          # disables plots, and buttons if plot is not rendered
          output$box_bar_scatter_violin_plot <- renderPlot({NULL})
          shinyjs::hide("download_box_bar_scatter_violin")
          shinyjs::hide("height_plot_box_bar_scatter_violin")
          shinyjs::hide("width_plot_box_bar_scatter_violin")
          #table with data and results
          output$box_bar_scatter_violin_effect_size_table <- renderUI({NULL})
          shinyjs::disable("options_box_bar_scatter_violin")
          shinyjs::hide("box_bar_scatter_violin_brush_selection")
          # if effect sizes are selected
          shinyjs::hide("download_box_bar_scatter_violin_effect_sizes")
          shinyjs::hide("download_box_bar_scatter_violin_table_all")
          
          output$box_bar_scatter_violin_plot_with_effect_size <- renderPlot({NULL})
          
          if (input$submit_plot_box_bar_scatter_violin[1] != 0){
            # Handle the error
            output$box_bar_scatter_violin_error_message <- renderText({
              ("Plot could not be generated. Please check data and plot selection")
            }) 
          }
        })
      })
      
      # runs submit_plot_box_bar_scatter_violin action button so that click count is updated
      observeEvent(input$update_options_box_bar_scatter_violin, {
        runjs('$("#submit_plot_box_bar_scatter_violin").click();')
      })
      
      # Reset bar/box/scatter/violin download button
      observeEvent(c(input$reset_box_bar_scatter_violin, input$reset_box_bar_scatter_violin_with_effect_sizes),{
        tryCatch({
          
          # disables plots, and buttons if plot is not rendered
          output$box_bar_scatter_violin_plot <- renderPlot({NULL})
          shinyjs::hide("download_box_bar_scatter_violin")
          shinyjs::hide("height_plot_box_bar_scatter_violin")
          shinyjs::hide("width_plot_box_bar_scatter_violin")
          
          #table with data and results
          output$box_bar_scatter_violin_effect_size_table <- renderUI({NULL})
          shinyjs::hide("options_box_bar_scatter_violin")
          shinyjs::hide("box_bar_scatter_violin_brush_selection")
          
          #sets click count to zero
          click_count_box_bar_scatter_violin(0)
          
          #  if effect sizes are present
          output$box_bar_scatter_violin_plot_with_effect_size <- renderPlot({NULL})
          shinyjs::hide("download_box_bar_scatter_violin_effect_sizes")
          shinyjs::hide("download_box_bar_scatter_violin_table_all")
          output$box_bar_scatter_violin_effect_size_table <- renderUI({NULL})
           
          
          # Reset specific input values using shinyjs::reset
          for (input_id in input_ids_to_reset()) {
            shinyjs::reset(input_id)
          }
          
          # Reset the colors and populate them with new default colors (needs fixing)
            # for symbol colour and fill
            for (i in seq_along(selected_colors_box_bar_scatter_violin$colors)) {updateColourInput(session, paste0("color_var_box_bar_scatter_violin", i), value = "black")}
            selected_colors_box_bar_scatter_violin$colors <- rep("black", length(selected_colors_box_bar_scatter_violin$colors))
            for (i in seq_along(selected_colours_symbol_fill_box_bar_scatter_violin$colors)) {updateColourInput(session, paste0("color_symbol_fill_var_box_bar_scatter_violin", i), value = "black")}
            selected_colours_symbol_fill_box_bar_scatter_violin$colors <- rep("black", length(selected_colours_symbol_fill_box_bar_scatter_violin$colors))
            updateSelectInput(session, "geom_jitter_colour_fill_by_group_box_bar_scatter_violin", selected = "Single Colour")
            
            # for box line and fill
            for (i in seq_along(selected_colors_boxplot_line_box_bar_scatter_violin$colors)) {updateColourInput(session, paste0("color_boxplot_line_var_box_bar_scatter_violin", i), value = "black")} 
            selected_colors_boxplot_line_box_bar_scatter_violin$colors <- rep("black", length(selected_colors_boxplot_line_box_bar_scatter_violin$colors))
            for (i in seq_along(selected_colors_boxplot_fill_box_bar_scatter_violin$colors)) {updateColourInput(session, paste0("color_boxplot_fill_var_box_bar_scatter_violin", i), value = "lightblue")}
            selected_colors_boxplot_fill_box_bar_scatter_violin$colors <- rep("black", length(selected_colors_boxplot_fill_box_bar_scatter_violin$colors))
            updateSelectInput(session, "geom_boxplot_colour_fill_by_group_box_bar_scatter_violin", selected = "Single Colour")
        })
      })
      
      # Generates the combined plot with box, bar, scatter and effect sizes
      bar_box_scatter_violin_with_effect_size_plot <- reactive({
            #bar_box_scatter_violin_with_effect_size_plot(
            if ((input$navpage == 'Box, Bar, Scatter, Violin and Raincloud' && !is.null(input$test_groups_box_bar_scatter_violin) && input$n_boot_box_bar_scatter_violin >= 2)
                || (input$navpage == 'Superplot' && !is.null(input$test_groups_superplot) && input$n_boot_superplot >= 2)){
              req(mean_median_effect_size_plot(), plot_for_all_box_bar_scatter_violin())
              # gets plots depending on selection (to avoid error with ggMarginal)
              if (!is.ggplot(plot_for_all_box_bar_scatter_violin())){
                plot_for_all_box_bar_scatter_violin <- plot_for_all_box_bar_scatter_violin()[[2]]
              } else {
                plot_for_all_box_bar_scatter_violin <- plot_for_all_box_bar_scatter_violin()    
              }
              
              # gets legend
              legend_for_all_superplot <- cowplot::get_legend(plot_for_all_box_bar_scatter_violin)   
              
              if (input$number_effect_sizes_box_bar_scatter_violin == 1 && input$position_plot_mean_median_diff_box_bar_scatter_violin == "Aligned with mean/median") {
                if (input$legend_outside_main_plot_box_bar_scatter_violin == TRUE){
                  main_plot <-cowplot::plot_grid(plot_for_all_box_bar_scatter_violin + theme(legend.position = "none"), mean_median_effect_size_plot(), align = "hv",  axis = "tb", rel_widths = c(4, 2))
                  cowplot::plot_grid(main_plot,  legend_for_all_superplot, align = "hv",  axis = "tb", rel_widths = c(4, 1))
                } else if (input$legend_outside_main_plot_box_bar_scatter_violin == FALSE) {
                  cowplot::plot_grid(plot_for_all_box_bar_scatter_violin, mean_median_effect_size_plot(),  align = "hv",  axis = "tb", rel_widths = c(4, 2))
                }
                
              } else if (input$number_effect_sizes_box_bar_scatter_violin == 1 && input$position_plot_mean_median_diff_box_bar_scatter_violin == "Below data") {
                if (input$legend_outside_main_plot_box_bar_scatter_violin == TRUE){
                  main_plot <- cowplot::plot_grid(plot_for_all_box_bar_scatter_violin + theme(legend.position = "none"), mean_median_effect_size_plot(),  align = "v", axis = "l", rel_heights = c(4, 2), ncol = 1)
                  legend_plot <- cowplot::plot_grid(legend_for_all_superplot, NULL,  align = "v", axis = "l", rel_heights = c(4, 2), ncol = 1)
                  cowplot::plot_grid(main_plot, legend_plot, nrow = 1, ncol = 2, rel_widths = c(4, 1), rel_heights = c(1, 0.5), align = 'h', axis = 't')
                } else if (input$legend_outside_main_plot_box_bar_scatter_violin == FALSE) {
                  cowplot::plot_grid(plot_for_all_box_bar_scatter_violin, mean_median_effect_size_plot(), align = "v", axis = "l", rel_heights = c(4, 2), ncol = 1)
                }
                
              } else if (input$number_effect_sizes_box_bar_scatter_violin == 2 && input$position_plot_mean_median_diff_box_bar_scatter_violin == "Aligned with mean/median") {
                if (input$legend_outside_main_plot_box_bar_scatter_violin == TRUE){
                  #first align data plot with effect size on the bottom panel
                  plots <- cowplot::align_plots(plot_for_all_box_bar_scatter_violin + theme(legend.position = "none"), hedges_cohen_glass_effect_size_plot(), align = 'v', axis = 'l')
                  top_row <- cowplot::plot_grid(plots[[1]], mean_median_effect_size_plot(), align = 'h', axis = 'tb', rel_widths = c(4,2))
                  bottom_row <- cowplot::plot_grid(plots[[2]], ggplot() + theme_void(), align = "hv", axis = "tb", rel_widths = c(4,2))
                  legend_plot <- cowplot::plot_grid(legend_for_all_superplot, NULL, align = "v", axis = "l", ncol =1, nrow = 2, rel_heights = c(2,1), rel_widths = c(2,1))
                  main_plot <- cowplot::plot_grid(top_row, bottom_row, align = "v", axis = "l", ncol =1, nrow = 2, rel_heights = c(2,1), rel_widths = c(2,1))
                  cowplot::plot_grid(main_plot, legend_plot, nrow = 1, ncol = 2, rel_widths = c(4, 1), rel_heights = c(1, 1), align = 'h', axis = 't')
                } else if (input$legend_outside_main_plot_box_bar_scatter_violin == FALSE) {
                  #first align data plot with effect size on the bottom panel
                  plots <- cowplot::align_plots(plot_for_all_box_bar_scatter_violin, hedges_cohen_glass_effect_size_plot(), align = 'v', axis = 'l')
                  top_row <- cowplot::plot_grid(plots[[1]], mean_median_effect_size_plot(), align = 'h', axis = 'tb', rel_widths = c(4,2))
                  bottom_row <- cowplot::plot_grid(plots[[2]], ggplot() + theme_void(), align = "hv", axis = "tb", rel_widths = c(4,2))
                  cowplot::plot_grid(top_row, bottom_row, align = "v", axis = "l", ncol =1, nrow = 2, rel_heights = c(2,1), rel_widths = c(2,1))
                }
                
              }else if (input$number_effect_sizes_box_bar_scatter_violin == 2 && input$position_plot_mean_median_diff_box_bar_scatter_violin == "Below data") {
                if (input$legend_outside_main_plot_box_bar_scatter_violin == TRUE){
                  main_plot <- cowplot::plot_grid(plot_for_all_box_bar_scatter_violin + theme(legend.position = "none"), mean_median_effect_size_plot(), hedges_cohen_glass_effect_size_plot(), align = "v", axis = "l", ncol = 1, nrow = 3, rel_heights = c(3, 2, 2), 
                                                  height = 1000, width = 800)
                  legend_plot <- cowplot::plot_grid(legend_for_all_superplot, NULL, NULL, align = "v", axis = "l", ncol = 1, nrow = 3, rel_heights = c(3, 2, 2))
                } else if (input$legend_outside_main_plot_box_bar_scatter_violin == FALSE) {
                  cowplot::plot_grid(plot_for_all_box_bar_scatter_violin, mean_median_effect_size_plot(), hedges_cohen_glass_effect_size_plot(), align = "v", axis = "l", ncol = 1, nrow = 3, rel_heights = c(3, 2, 2), 
                                     height = 1000, width = 800)
                }
              }
            }
          })
      
      #Renders bar/box/scatter/violin plot with effect sizes
      observeEvent(input$submit_box_bar_scatter_violin_with_effect_sizes,{
        if ((input$navpage == 'Box, Bar, Scatter, Violin and Raincloud' && !is.null(input$test_groups_box_bar_scatter_violin) && input$n_boot_box_bar_scatter_violin >= 2)){
          
          # adds tryCatch for any error that may occur
          tryCatch({
            # shows plot if hidden
            shinyjs::show("box_bar_scatter_violin_plot_with_effect_size")
            #Renders Plot
            # if modal is open, requires click to update plot
            if (!is.null(plot_for_all_box_bar_scatter_violin()) && input$modal_box_bar_scatter_violin == TRUE &&  click_count_box_bar_scatter_violin() > 0) {
              # Check if update button is clicked
              output$box_bar_scatter_violin_plot_with_effect_size <- renderPlot({
                bar_box_scatter_violin_with_effect_size_plot()
              }, height = input$height_plot_box_bar_scatter_violin, width = input$width_plot_box_bar_scatter_violin
              )
              # #sets click count to zero
               #click_count_box_bar_scatter_violin(0)
              # shows numeric inputs for plot size
              shinyjs::show("height_plot_box_bar_scatter_violin")
              shinyjs::show("width_plot_box_bar_scatter_violin")
              shinyjs::show("options_box_bar_scatter_violin")
              shinyjs::show("download_box_bar_scatter_violin_effect_sizes")
              shinyjs::show("download_box_bar_scatter_violin_table_all")
              shinyjs::show("box_bar_scatter_violin_effect_size_table")
              
              
              # if modal is not open
            } else if (input$modal_box_bar_scatter_violin != TRUE && !is.null(plot_for_all_box_bar_scatter_violin()) && click_count_box_bar_scatter_violin() == 0) {
              output$box_bar_scatter_violin_plot_with_effect_size <- renderPlot({
                bar_box_scatter_violin_with_effect_size_plot()
              }, height = input$height_plot_box_bar_scatter_violin, width = input$width_plot_box_bar_scatter_violin
              )
              # #sets click count to zero
               #click_count_box_bar_scatter_violin(0)
              # shows numeric inputs and download buttons for plot
              shinyjs::show("height_plot_box_bar_scatter_violin")
              shinyjs::show("width_plot_box_bar_scatter_violin")
              shinyjs::show("options_box_bar_scatter_violin")
              shinyjs::show("download_box_bar_scatter_violin_effect_sizes")
              shinyjs::show("download_box_bar_scatter_violin_table_all")
              shinyjs::show("box_bar_scatter_violin_effect_size_table")
            }
            
            
            # hides plot without effect size
            shinyjs::hide("box_bar_scatter_violin_plot")
            
            # resets previous plot
            output$box_bar_scatter_violin_plot <- renderPlot({NULL})
            
            # adds information about previous reset for update_button
            reset_info_box_bar_scatter_violin_plot(1)
            #reset_info_box_bar_scatter_violin_plot(1)
            
            # resets text messages
            output$box_bar_scatter_violin_error_message <- renderText({NULL})
            output$error_message_box_bar_scatter_violin_data <- renderText({NULL})
            
            # hides plot without effect size
            shinyjs::hide("box_bar_scatter_violin_plot")
            
            # Download the bar/box/scatter/violin
            output$download_box_bar_scatter_violin_effect_sizes <- downloadHandler(
              filename = "plot.png",
              content = function(file) {
                # Save the superplot as a PNG file
                ggsave(file, plot = bar_box_scatter_violin_with_effect_size_plot(), device = "png")
              }
            )
            
            #Download data descriptive statistics and effect sizes
            output$download_box_bar_scatter_violin_table_all <- downloadHandler(
              filename = "list_of_effect_size_results.xlsx",
              content = function(file) {
                saveWorkbook(list_of_effect_size_results(), file = file, overwrite = TRUE)
              }
            )
            
            # For brushed points when plotting
            shinyjs::hide("box_bar_scatter_violin_brush_selection")
            # output$box_bar_scatter_violin_brush_selection <- renderTable({
            #   brushedPoints(data_plot(), input$plot_brush_box_bar_scatter_violin_with_effect_size, xvar = input$group_var_box_bar_scatter_violin, yvar = input$x_var_box_bar_scatter_violin)
            # })
            
            
          },  error = function(e) {
            # disables plots, and buttons if plot is not rendered
            output$box_bar_scatter_violin_plot_with_effect_size <- renderPlot({NULL})
            shinyjs::hide("download_box_bar_scatter_violin")
            shinyjs::hide("height_plot_box_bar_scatter_violin")
            shinyjs::hide("width_plot_box_bar_scatter_violin")
            #table with data and results
            output$box_bar_scatter_violin_effect_size_table <- renderUI({NULL})
            shinyjs::disable("options_box_bar_scatter_violin")
            shinyjs::hide("box_bar_scatter_violin_brush_selection")
            # if effect sizes are selected
            shinyjs::hide("download_box_bar_scatter_violin_effect_sizes")
            shinyjs::hide("download_box_bar_scatter_violin_table_all")
            
            output$box_bar_scatter_violin_plot <- renderPlot({NULL})
            
            # Handle the error
            if (input$submit_plot_box_bar_scatter_violin[1] != 0){
              output$box_bar_scatter_violin_error_message <- renderText({
                ("Plot could not be generated. Please check data and plot selection")
              })
            }
          })
        } 
      })
      
      # shows warning messages if applicable
      observeEvent(input$submit_box_bar_scatter_violin_with_effect_sizes,{
        # for number of bootstrap replicas
        if (is.na(input$n_boot_box_bar_scatter_violin) || input$n_boot_box_bar_scatter_violin <= 2) {
          shinyFeedback::feedbackWarning(inputId = "n_boot_box_bar_scatter_violin", text = "Please select a larger number of samples", show = TRUE)
        } else {
          hideFeedback("n_boot_box_bar_scatter_violin")
        }
        
        # for test group selection
        if (is.null(input$test_groups_box_bar_scatter_violin)){
          shinyFeedback::feedbackWarning(inputId = "test_groups_box_bar_scatter_violin", text = "Please select a test group", show = TRUE)
        } else {
          hideFeedback("test_groups_box_bar_scatter_violin")
        }
      })
      
      # runs submit_box_bar_scatter_violin_with_effect_sizes action button so that click count is updated
      observeEvent(input$update_options_box_bar_scatter_violin, {
        if (reset_info_box_bar_scatter_violin_plot() == 1){
          runjs('$("#submit_box_bar_scatter_violin_with_effect_sizes").click();')
        }
      })
      
      
    # Code for effect size
      # reset reactive val for update options for plots with effect sizes
      reset_info_box_bar_scatter_violin_plot <- reactiveVal(0)
      reset_info_superplot <- reactiveVal(0)
      
      # creates variables to store bootstrapp options to avoid repeating the sampling everytime plot is updated
      num_effect_sizes <- reactiveVal(0)
      type_of_boostrap <- reactiveVal(0)
      num_boostrap_samples <- reactiveVal(0)
      
      control_group_bootstrap <- reactiveVal(0)
      test_group_bootstrap <- reactiveVal(0)
      mean_median_diff_display <- reactiveVal(0)
      hedges_cohen_glass_display <- reactiveVal(0)
      mean_median_diff_alignment <- reactiveVal(0)
      #)
      
      # for names of variables
      group_var_control <- reactiveVal(0)
      x_var_control <- reactiveVal(0)
      y_var_control <- reactiveVal(0)
      z_var_control <- reactiveVal(0)
      w_var_control <- reactiveVal(0)
      extra_var_control <- reactiveVal(0)
      
      # reactive value that stores effect size results
      mean_median_effect_size_plot <- reactiveVal(0)
      hedges_cohen_glass_effect_size_plot <- reactiveVal(0)
      list_of_effect_size_results <- reactiveVal(0)
      
      hedges_cohen_glass_calculated_mean <- reactiveVal(0)
      hedges_cohen_glass_calculated_CI <- reactiveVal(0)
      hedges_cohen_glass_density_list <- reactiveVal(0)
      mean_median_table_mean_median_diff <-reactiveVal(0)
      
      mean_median_calculated_mean <- reactiveVal(0)
      mean_median_calculated_CI <- reactiveVal(0)
      mean_median_density_list <-reactiveVal(0)
      hedges_cohen_glass_table_hedges_cohen <-reactiveVal(0)
      
      # Get effect bootstrapped effect sizes
      observeEvent(c(input$submit_box_bar_scatter_violin_with_effect_sizes, input$update_options_box_bar_scatter_violin, input$submit_superplot_with_effect_sizes, input$update_options_superplot),{
        if ((input$navpage == 'Box, Bar, Scatter, Violin and Raincloud' && !is.null(input$test_groups_box_bar_scatter_violin) && input$n_boot_box_bar_scatter_violin >= 2)
            || (input$navpage == 'Superplot' && !is.null(input$test_groups_superplot) && input$n_boot_superplot >= 2)){
          
          # uses tryCatch to detect any error
          tryCatch({
            
            # selects variables based on tab
            if (input$navpage == 'Superplot') {
              tab_id <- "superplot"
              
              #Defines bootstrap option
              bootstrap_option <- input$bootstrap_option_superplot
              
              # selects data to use
              data <- data_plot()
              
              # filters data based on group_order
              if (!is.null(input$group_order_superplot)) {
                filtered_data <- data_plot() %>%
                  filter(!!as.name(input$group_var_superplot) %in% input$group_order_superplot)
              } else {
                filtered_data <- data_plot()
              }
              
              # filters data based on additional group order
              if (input$show_additional_group_legend_superplot == TRUE && !is.null(input$additional_group_order_superplot)) {
                data <- unique(filtered_data[filtered_data[[input$x_var_superplot]] %in% input$additional_group_order_superplot, ])
              } else if (input$show_additional_group_legend_superplot == FALSE) {
                data <- filtered_data
              }
              
              # names all the possible variables
              group_var <- input$group_var_superplot
              x_var <- input$x_var_superplot
              y_var <- input$y_var_superplot
              
              # number of bootstrap replicas
              n_boot <- input$n_boot_superplot
              
              # defines index for repeated observations
              if (input$repeated_observations_superplot == TRUE) {
                paired = input$paired_observations_superplot
              } else {
                paired = NULL
              }
              
              # defines control and test groups
              control_group <- input$control_group_superplot
              test_group <- input$test_groups_superplot
              
              # selects effect sizes to run
              #for mean/median difference
              mean_median_effect_size <- input$effect_size_mean_median_superplot
              # for hedges g, 
              hedges_cohen_glass_effect_size <- input$effect_sizes_hedges_cohen_glass_superplot
              
              # selects plot to use for effect size comparison
              plot_selected <-  plot_for_all_superplot()
              
              # position for plots
              positioning_plot_mean_median_diff <- input$position_plot_mean_median_diff_superplot
              
            } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud'){
              tab_id <- "box_bar_scatter_violin"
              
              #Defines bootstrap option
              bootstrap_option <- input$bootstrap_option_box_bar_scatter_violin
              
              # filters data based on group_order
              if (!is.null(input$group_order_box_bar_scatter_violin)) {
                filtered_data <- data_plot() %>%
                  filter(!!as.name(input$group_var_box_bar_scatter_violin) %in% input$group_order_box_bar_scatter_violin)
              } else {
                filtered_data <- data_plot()
              }
              
              # filters data based on additional group order
              if (input$show_additional_group_legend_box_bar_scatter_violin == TRUE && !is.null(input$additional_group_order_box_bar_scatter_violin)) {
                data <- unique(filtered_data[filtered_data[[input$additional_variable_box_bar_scatter_violin]] %in% input$additional_group_order_box_bar_scatter_violin, ])
              } else if (input$show_additional_group_legend_box_bar_scatter_violin == FALSE) {
                data <- filtered_data
              }
              
              # names all the possible variables
              group_var <- input$group_var_box_bar_scatter_violin
              x_var <- input$x_var_box_bar_scatter_violin
              y_var <- input$y_var_box_bar_scatter_violin
              z_var <- input$z_var_box_bar_scatter_violin
              w_var <- input$w_var_box_bar_scatter_violin
              extra_var <- input$additional_variable_box_bar_scatter_violin
              
              # number of bootstrap replicas
              n_boot <- input$n_boot_box_bar_scatter_violin
              
              # defines index for repeated observations
              if (input$repeated_observations_box_bar_scatter_violin == TRUE) {
                paired = input$paired_observations_box_bar_scatter_violin
              } else {
                paired = NULL
              }
              
              
              # defines control and test groups
              control_group <- input$control_group_box_bar_scatter_violin
              test_group <- input$test_groups_box_bar_scatter_violin
              
              # selects effect sizes to run
              #for mean/median difference
              mean_median_effect_size <- input$effect_size_mean_median_box_bar_scatter_violin
              # for hedges g, 
              hedges_cohen_glass_effect_size <- input$effect_sizes_hedges_cohen_glass_box_bar_scatter_violin
              
              # selects plot to use for effect size comparison
              if (!is.ggplot(plot_for_all_box_bar_scatter_violin())){
                plot_selected <- plot_for_all_box_bar_scatter_violin()[[2]]
              } else {
                plot_selected <- plot_for_all_box_bar_scatter_violin()    
              }
              
              # position for plots
              positioning_plot_mean_median_diff <- input$position_plot_mean_median_diff_box_bar_scatter_violin
            }
            
            # checks if changes have ocurred in the effect size selection
            if (num_effect_sizes() != input[[paste0("number_effect_sizes_", tab_id)]] || type_of_boostrap() != bootstrap_option ||
                num_boostrap_samples() != n_boot || control_group_bootstrap() != control_group || !identical(test_group_bootstrap(), test_group) ||
                mean_median_diff_display() != mean_median_effect_size || hedges_cohen_glass_display() != hedges_cohen_glass_effect_size ||
                group_var_control() != group_var || x_var_control() != x_var ||
                y_var_control() != y_var || z_var_control() != input$z_var_box_bar_scatter_violin || 
                w_var_control() != input$w_var_box_bar_scatter_violin || extra_var_control() != input$additional_variable_box_bar_scatter_violin) {
              
              #updates changes in effect size parameters
              num_effect_sizes(input[[paste0("number_effect_sizes_", tab_id)]])
              type_of_boostrap(bootstrap_option)
              num_boostrap_samples(n_boot)
              control_group_bootstrap(control_group)
              test_group_bootstrap(test_group)
              mean_median_diff_display(mean_median_effect_size)
              hedges_cohen_glass_display(hedges_cohen_glass_effect_size)
              
              group_var_control(group_var)
              x_var_control(x_var)
              y_var_control(y_var)
              z_var_control(input$z_var_box_bar_scatter_violin)
              w_var_control(input$w_var_box_bar_scatter_violin)
              extra_var_control(input$additional_variable_box_bar_scatter_violin)
              
              
              # Selects which bootstrap to run
              if (bootstrap_option == "hierarchical_boot") {
                # calculates 2 level hierarchical bootstrap for selected samples and grouping variable
                if (input$navpage == "Superplot") {
                  boot_samples <- hierarchical_boot(data, x_var, y_var, level_3_var = NA, level_4_var = NA, group_var, n_boot)
                } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
                  # adjusts hierarchical boot based on number of levels selected
                  if (input$num_levels_box_bar_scatter_violin == 2) {
                    boot_samples <- hierarchical_boot(data, x_var, y_var, level_3_var = NA, level_4_var = NA, group_var, n_boot) 
                  } else if (input$num_levels_box_bar_scatter_violin == 3){
                    boot_samples <- hierarchical_boot(data, x_var, y_var, level_3_var = z_var, level_4_var = NA, group_var, n_boot)
                  } else if (input$num_levels_box_bar_scatter_violin == 4){
                    boot_samples <- hierarchical_boot(data_plot(), x_var, y_var, level_3_var = z_var, level_4_var = w_var, group_var, n_boot)
                  }
                }
                # calculates a simple bootstrap for selected samples and grouping variable
              } else if (bootstrap_option == "simple_boot") {
                if (input$navpage == "Superplot") {
                  boot_samples <- boot_one_level(data, y_var, group_var, paired, n_boot)
                } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
                  # adjust simple boot based on number of levels selected
                  if (input$num_levels_box_bar_scatter_violin == 1) {
                    boot_samples <- boot_one_level(data, x_var, group_var, paired, n_boot)
                  } else if (input$num_levels_box_bar_scatter_violin == 2){
                    boot_samples <- boot_one_level(data, y_var, group_var, paired, n_boot)
                  } else if (input$num_levels_box_bar_scatter_violin == 3) {
                    boot_samples <- boot_one_level(data, z_var, group_var, paired, n_boot)
                  } else if (input$num_levels_box_bar_scatter_violin == 4) {
                    boot_samples <- boot_one_level(data, w_var, group_var, paired, n_boot)
                  }
                }
              }
              
              # Initialize empty list to store the effect sizes for hedges, cohen or cliff
              hedges_cohen_glass <- vector("list", length(test_group))
              names(hedges_cohen_glass) <- test_group  # Set the names of the list elements to the test_group
              
              # Run selected effect sizes for each test variable
              for (i in seq_along(test_group)) {
                if (hedges_cohen_glass_effect_size == "Hedges' g") {
                  # Calculate Hedges' g for current test variable
                  hedges_cohen_glass[[test_group[i]]] <- hedges_g(control_group, test_group[i], boot_samples)
                } else if (hedges_cohen_glass_effect_size == "Cohen's d") {
                  # Calculate Cohen's d for current test variable
                  hedges_cohen_glass[[test_group[i]]] <- cohens_d(control_group, test_group[i], boot_samples)
                } else if (hedges_cohen_glass_effect_size == "Glass' delta") {
                  hedges_cohen_glass[[test_group[i]]] <- glass_delta(control_group, test_group[i], boot_samples)
                }
              }
              
              # Initialize empty list to store the effect sizes for hedges, cohen or cliff
              mean_median_diff <- vector("list", length(test_group))
              names(mean_median_diff) <- test_group  # Set the names of the list elements to the test_group
              
              
              # Run mean or median difference
              if (mean_median_effect_size == "Mean difference") {
                # Extract the mean component from the result list
                mean_median_values <- boot_samples$mean
                # Calculate mean difference for each variable in test_column
                for (i in seq_along(test_group)) {
                  mean_median_diff[[test_group[i]]] <- mean_median_values[, test_group[i]] - mean_median_values[, control_group]
                }  
              } else if (mean_median_effect_size == "Median difference") {
                # Extract the median component from the result list
                mean_median_values <- boot_samples$median
                # Calculate median difference for each variable in test_column
                for (i in seq_along(test_group)) {
                  mean_median_diff[[test_group[i]]] <- mean_median_values[, test_group[i]] - mean_median_values[, control_group]
                } 
              }
              
              # Initialize empty lists to store the results for each group
              #for mean or median
              density_list_mean_median <- list()
              mean_list_mean_median <- list()
              CI_list_mean_median <- list()
              
              #for hedges, cohen or cliff
              density_list_hedges_cohen <- list()
              mean_list_hedges_cohen <- list()
              CI_list_hedges_cohen <- list()
              
              # Iterate over each group in mean_median_diff
              for (i in seq_along(mean_median_diff)) {
                # Get the name of the current group
                group_name <- test_group[i]
                
                # Calculate density for current group
                density_mean_median_diff <- density(mean_median_diff[[i]])
                
                # Calculate mean for current group
                mean_mean_median_diff <- mean(mean_median_diff[[i]])
                
                # Set the confidence interval
                confidence_level = 0.95
                # Calculate confidence interval for current group
                CI_mean_median_diff <- c(quantile(mean_median_diff[[i]], c((1 - confidence_level) / 2, 1 - (1 - confidence_level) / 2))[[1]], quantile(mean_median_diff[[i]], c((1 - confidence_level) / 2, 1 - (1 - confidence_level) / 2))[[2]])
                
                # Store the results for the current group with the corresponding name
                density_list_mean_median[[group_name]] <-  density_mean_median_diff
                mean_list_mean_median[[group_name]] <-  mean_mean_median_diff
                CI_list_mean_median[[group_name]] <- CI_mean_median_diff
                
              }
              
              # Create a table for mean/median diff using the stored variables
              table_mean_median_diff <- data.frame(
                Group = rep(test_group, each = lengths(mean_median_diff)),
                Mean_Median_Diff = unlist(mean_median_diff),
                Mean = rep(unlist(mean_list_mean_median), each = lengths(mean_median_diff)),
                CI_Lower = rep(sapply(CI_list_mean_median, `[[`, 1), each = lengths(mean_median_diff)),
                CI_Upper = rep(sapply(CI_list_mean_median, `[[`, 2), each = lengths(mean_median_diff))
              )
              
              # Order the data frame by test_group
              table_mean_median_diff <- table_mean_median_diff[order(table_mean_median_diff$Group), ]
              
              # Iterate over each group in hedges_cohen_glass
              for (i in seq_along(hedges_cohen_glass)) {
                # Get the name of the current group
                group_name <- test_group[i]
                
                # Calculate density for current group
                density_hedges_cohen <- density(hedges_cohen_glass[[i]])
                
                # Calculate mean for current group
                mean_hedges_cohen <- mean(hedges_cohen_glass[[i]])
                
                # Calculate confidence interval for current group
                CI_hedges_cohen <- quantile(hedges_cohen_glass[[i]], c((1 - confidence_level) / 2, 1 - (1 - confidence_level) / 2))
                
                # Store the results for the current group with the corresponding name
                density_list_hedges_cohen[[group_name]] <- density_hedges_cohen
                mean_list_hedges_cohen[[group_name]] <- mean_hedges_cohen
                CI_list_hedges_cohen[[group_name]] <- CI_hedges_cohen
              }  
              # Create a table for mean/median diff using the stored variables
              table_hedges_cohen <- data.frame(
                Group = rep(test_group, each = lengths(hedges_cohen_glass)),
                Hedges_cohen_cliff = unlist(hedges_cohen_glass),
                Mean = rep(unlist(mean_list_hedges_cohen), each = lengths(hedges_cohen_glass)),
                CI_Lower = rep(sapply(CI_list_hedges_cohen, `[[`, 1), each = lengths(hedges_cohen_glass)),
                CI_Upper = rep(sapply(CI_list_hedges_cohen, `[[`, 2), each = lengths(hedges_cohen_glass))
              )
              
              #Creates workbook for data and effect sizes
              workbook_data_effect_sizes <- createWorkbook()
              
              # Create an empty data frame to store the descriptive statistics
              summary_descriptive <- data.frame(Group = character(),
                                                Mean = numeric(),
                                                Median = numeric(),
                                                SD = numeric(),
                                                n = numeric()
              )
              
              # Join control and test groups into a single variable
              all_groups <- c(control_group, test_group)
              
              # Iterate over each group in input$group_var_superplot
              for (i in seq_along(all_groups)) {
                # Get the name of the current group
                group_name <- all_groups[i]
                
                if (input$navpage == "Superplot") {
                  # Subset the data for the current group
                  group_data <- subset(data_plot(), data_plot()[[input$group_var_superplot]] == group_name)
                  
                  # Calculate the mean, median, standard deviation, and total observations for the current group
                  mean_val <- mean(group_data[[input$y_var_superplot]], na.rm = TRUE)
                  median_val <- median(group_data[[input$y_var_superplot]], na.rm = TRUE)
                  sd_val <- sd(group_data[[input$y_var_superplot]], na.rm = TRUE)
                  total_observations <- length(group_data[[input$y_var_superplot]])
                  
                  # Calculate the total observations organized by the level input$x_var_superplot
                  total_observations_by_level <- aggregate(group_data[[input$x_var_superplot]], 
                                                           by = list(group_data[[input$group_var_superplot]]),
                                                           FUN = function(x) length(unique(na.omit(x))))
                  
                } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
                  # Subset the data for the current group
                  group_data <- subset(data, data[[group_var]] == group_name)
                  
                  # Calculate the mean, median, standard deviation, and total observations for the current group
                  # selects correct variable based on number of levels selected
                  if (input$num_levels_box_bar_scatter_violin == 1) {
                    select_var_for_stats <- input$x_var_box_bar_scatter_violin
                    extra_select_var_for_stats <- NULL
                  } else if (input$num_levels_box_bar_scatter_violin == 2) {
                    select_var_for_stats <- input$y_var_box_bar_scatter_violin
                    extra_select_var_for_stats <- input$x_var_box_bar_scatter_violin
                    
                    # list variables for table arrangement
                    variable_names <- c(x_var)
                    
                    # Calculate the total observations organized by levels
                    total_observations_by_level <- aggregate(group_data[!is.na(group_data[[y_var]]), ][[x_var]], 
                                                             by = list(group_data[!is.na(group_data[[y_var]]), ][[group_var]]),
                                                             FUN = function(x) length(unique(na.omit(x))))
                    
                  } else if (input$num_levels_box_bar_scatter_violin == 3) {
                    select_var_for_stats <- input$z_var_box_bar_scatter_violin
                    extra_select_var_for_stats <- input$y_var_box_bar_scatter_violin
                    
                    # list variables for table arrangement
                    variable_names <- c(x_var, y_var)
                    
                    # Calculate the total observations organized by levels
                    total_observations_by_level <- aggregate(cbind(group_data[!is.na(group_data[[z_var]]), ][[x_var]], group_data[!is.na(group_data[[z_var]]), ][[y_var]]),
                                                             by = list(group_data[!is.na(group_data[[z_var]]), ][[group_var]]),
                                                             FUN = function(x) length(unique(na.omit(x))))
                    
                  } else if (input$num_levels_box_bar_scatter_violin == 4){
                    select_var_for_stats <- input$w_var_box_bar_scatter_violin
                    extra_select_var_for_stats <- input$z_var_box_bar_scatter_violin
                    
                    # list variables for table arrangement
                    variable_names <- c(x_var, y_var, z_var)
                    
                    # Calculate the total observations organized by levels
                    total_observations_by_level <- aggregate(cbind(group_data[!is.na(group_data[[w_var]]), ][[x_var]], group_data[!is.na(group_data[[w_var]]), ][[y_var]], group_data[!is.na(group_data[[w_var]]), ][[z_var]]),
                                                             by = list(group_data[!is.na(group_data[[w_var]]), ][[group_var]]),
                                                             FUN = function(x) length(unique(na.omit(x))))
                  }
                  
                  mean_val <- mean(group_data[[select_var_for_stats]], na.rm = TRUE)
                  median_val <- median(group_data[[select_var_for_stats]], na.rm = TRUE)
                  sd_val <- sd(group_data[[select_var_for_stats]], na.rm = TRUE)
                  total_observations <- sum(table(group_data[[select_var_for_stats]]))
                }
                
                
                # Store the descriptive statistics in the summary_descriptive data frame
                summary_descriptive[i, "Group"] <- group_name
                summary_descriptive[i, "Mean"] <- mean_val
                summary_descriptive[i, "Median"] <- median_val
                summary_descriptive[i, "SD"] <- sd_val
                summary_descriptive[i, "n"] <- total_observations
                
                # for additional levels
                if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud' && input$num_levels_box_bar_scatter_violin >= 2){
                  #stores names of variables
                  col_name <- paste("n (", variable_names, ")", sep = "")
                  summary_descriptive[i, col_name] <- total_observations_by_level[, -1]
                }
              }
              
              # Add each table to a separate sheet in the workbook
              addWorksheet(workbook_data_effect_sizes, "Descriptive")
              writeData(workbook_data_effect_sizes, "Descriptive", summary_descriptive)
              #for mean/median effect size
              addWorksheet(workbook_data_effect_sizes, "Mean_median_difference")
              writeData(workbook_data_effect_sizes, "Mean_median_difference", table_mean_median_diff)
              #for hedges/cohen/cliff
              addWorksheet(workbook_data_effect_sizes, "Hedges_cohen_cliff")
              writeData(workbook_data_effect_sizes, "Hedges_cohen_cliff", table_hedges_cohen)
              
              list_of_effect_size_results(workbook_data_effect_sizes)
              
              
              #Summary HTML table with results
              #summarize effect sizes frame for table merging
              # Mean or median effect size
              table_mean_median_diff_summary = data.frame(
                Group = unique(table_mean_median_diff$Group),
                `Mean_diff`= paste0(signif(unique(table_mean_median_diff$Mean), 3)),
                `Mean_diff [95% CI]`= paste0("[", signif(unique(table_mean_median_diff$CI_Lower),3), ",", signif(unique(table_mean_median_diff$CI_Upper),3), "]"),
                check.names = FALSE
              )
              
              #renames effect size column if applicable
              if (mean_median_effect_size == "Median difference") {
                names(table_mean_median_diff_summary)[names(table_mean_median_diff_summary) == "Mean_diff"] = "Median_diff"
                names(table_mean_median_diff_summary)[names(table_mean_median_diff_summary) == "Mean_diff [95% CI]"] = "Median_diff [95% CI]"
              }
              
              #Hedges g', Cohen's d or Glass' delta    
              table_hedges_cohen_summary = data.frame(
                Group = unique(table_hedges_cohen$Group),
                `Hedges' g`= paste0(signif(unique(table_hedges_cohen$Mean), 3)),
                `Hedges' g [95% CI]`= paste0("[", signif(unique(table_hedges_cohen$CI_Lower),3), ",", signif(unique(table_hedges_cohen$CI_Upper),3), "]"),
                check.names = FALSE
              )
              
              #renames effect size column if applicable
              if (hedges_cohen_glass_effect_size == "Cohen's d") {
                names(table_hedges_cohen_summary)[names(table_hedges_cohen_summary) == "Hedges' g [95% CI]"] = "Cohen's d [95% CI]"
                names(table_hedges_cohen_summary)[names(table_hedges_cohen_summary) == "Hedges' g"] = "Cohen's d"
              } else if (hedges_cohen_glass_effect_size == "Glass' delta") {
                names(table_hedges_cohen_summary)[names(table_hedges_cohen_summary) == "Hedges' g [95% CI]"] = "Glass' delta [95% CI]"
                names(table_hedges_cohen_summary)[names(table_hedges_cohen_summary) == "Hedges' g"] = "Glass' delta"
              }
              
              # summary stats
              summary_all = data.frame(
                Group = summary_descriptive$Group,
                Mean = signif(summary_descriptive$Mean, 3),
                Median = signif(summary_descriptive$Median, 3),
                `Standard Deviation` = signif(summary_descriptive$SD, 3),
                n = summary_descriptive$n
              )
              
              if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud' && input$num_levels_box_bar_scatter_violin >= 2){
                # # Loop through each column starting from the 6th column in summary_descriptive
                for (i in 6:ncol(summary_descriptive)) {
                  # gets the column to add
                  col_name <- colnames(summary_descriptive)[i]
                  # inserts column
                  summary_all[[col_name]] <- summary_descriptive[[col_name]]
                }
              }
              
              #merges all dataframes into one
              if ((input$number_effect_sizes_superplot == 1 && input$navpage == "Superplot") || (input$number_effect_sizes_box_bar_scatter_violin == 1 && input$navpage == 'Box, Bar, Scatter, Violin and Raincloud')){
                merged_summary_df <- data_merge(summary_all, table_mean_median_diff_summary, join = "full",  by = "Group")
              } else if ((input$number_effect_sizes_superplot == 2 && input$navpage == "Superplot") || (input$number_effect_sizes_box_bar_scatter_violin == 2 && input$navpage == 'Box, Bar, Scatter, Violin and Raincloud')){
                merged_summary_df <- data_merge(data_merge(summary_all, table_mean_median_diff_summary, join = "full",  by = "Group"), table_hedges_cohen_summary, join = "full",  by = "Group")
              }
              
              #creates html table
              merged_summary <- kable(merged_summary_df, digits = 5, align = "lcccccc", caption = "Results summary", valign = 't', table.attr = "class=\"table table-striped table-auto\"", format = "html", row.names = FALSE )
              
              # prints html table with results
              if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
                output$box_bar_scatter_violin_effect_size_table <- renderUI({
                  HTML(merged_summary)
                })
              } else if (input$navpage == 'Superplot') {
                output$superplot_effect_size_table <- renderUI({
                  HTML(merged_summary)
                })
              }
              
              
              # returns list with mean/median and cohen/hedges/cliff effect sizes for plot
              mean_median_calculated_mean(mean_list_mean_median)
              mean_median_calculated_CI(CI_list_mean_median)
              mean_median_density_list(density_list_mean_median)
              mean_median_table_mean_median_diff(table_mean_median_diff)
              
              hedges_cohen_glass_calculated_mean(mean_list_hedges_cohen)
              hedges_cohen_glass_calculated_CI(CI_list_hedges_cohen)
              hedges_cohen_glass_density_list(density_list_hedges_cohen)
              hedges_cohen_glass_table_hedges_cohen(table_hedges_cohen)
              
              # end of checkpoint if 
            }
            output$box_bar_scatter_violin_error_message <- renderText({NULL})
            output$superplot_error_message <- renderText({NULL})
            
          }, error = function(e) {
            # Handle the error
            if (input[[paste0("submit_plot_", tab_id())]][1] != 0){
              # Shows message if error occurs when uploading file
              output[[paste0(tab_id(), "_error_message")]] <- renderText({"Effect sizes could not be calculated. Please check data selection"})
            }
            # checks if previous effect sizes existed
            if (!is.null(mean_median_table_mean_median_diff()) && input[[paste0("submit_plot_", tab_id())]][1] != 0) {
              output[[paste0(tab_id(), "_error_message")]] <- renderText({"New effect sizes could not be calculated. Please check data selection"})
              cat("Error Message:", conditionMessage(e), "\n")
              # Print the call stack trace
              traceback()
            }
          })
        }
      })
      
      # Generates effect size plots
      observe({
        if ((input$navpage == 'Box, Bar, Scatter, Violin and Raincloud' && !is.null(input$test_groups_box_bar_scatter_violin) && input$n_boot_box_bar_scatter_violin >= 2)
            || (input$navpage == 'Superplot' && !is.null(input$test_groups_superplot) && input$n_boot_superplot >= 2)){
          # uses trCatch to detect any error
          tryCatch({
            req(input$submit_box_bar_scatter_violin_with_effect_sizes || input$submit_superplot_with_effect_sizes)
            
            # selects variables based on tab
            if (input$navpage == 'Superplot') {
              tab_id <- "superplot"
              
              #Defines bootstrap option
              bootstrap_option <- input$bootstrap_option_superplot
              
              # selects data to use
              data <- data_plot()
              # filters data based on group_order
              if (!is.null(input$group_order_superplot)) {
                filtered_data <- data_plot() %>%
                  filter(!!as.name(input$group_var_superplot) %in% input$group_order_superplot)
              } else {
                filtered_data <- data_plot()
              }
              
              # filters data based on additional group order
              if (input$show_additional_group_legend_superplot == TRUE && !is.null(input$additional_group_order_superplot)) {
                data <- unique(filtered_data[filtered_data[[input$x_var_superplot]] %in% input$additional_group_order_superplot, ])
              } else if (input$show_additional_group_legend_superplot == FALSE || is.null(input$additional_group_order_superplot)) {
                data <- filtered_data
              }
              
              # names all the possible variables
              group_var <- input$group_var_superplot
              x_var <- input$x_var_superplot
              y_var <- input$y_var_superplot
              
              # defines control and test groups
              control_group <- input$control_group_superplot
              test_group <- input$test_groups_superplot
              
              # selects effect sizes to run
              #for mean/median difference
              mean_median_effect_size <- input$effect_size_mean_median_superplot
              # for hedges g, 
              hedges_cohen_glass_effect_size <- input$effect_sizes_hedges_cohen_glass_superplot
              
              # selects plot to use for effect size comparison
              plot_selected <- plot_for_all_superplot()
              
              # position for plots
              positioning_plot_mean_median_diff <- input$position_plot_mean_median_diff_superplot
              
            } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud'){
              tab_id <- "box_bar_scatter_violin"
              
              #Defines bootstrap option
              bootstrap_option <- input$bootstrap_option_box_bar_scatter_violin
              
              # filters data based on group_order
              if (!is.null(input$group_order_box_bar_scatter_violin)) {
                filtered_data <- data_plot() %>%
                  filter(!!as.name(input$group_var_box_bar_scatter_violin) %in% input$group_order_box_bar_scatter_violin)
              } else {
                filtered_data <- data_plot()
              }
              
              # filters data based on additional group order
              if (input$show_additional_group_legend_box_bar_scatter_violin == TRUE && !is.null(input$additional_group_order_box_bar_scatter_violin)) {
                data <- unique(filtered_data[filtered_data[[input$additional_variable_box_bar_scatter_violin]] %in% input$additional_group_order_box_bar_scatter_violin, ])
              } else if (input$show_additional_group_legend_box_bar_scatter_violin == FALSE || is.null(input$additional_group_order_box_bar_scatter_violin)) {
                data <- filtered_data
              }
              
              # names all the possible variables
              group_var <- input$group_var_box_bar_scatter_violin
              x_var <- input$x_var_box_bar_scatter_violin
              y_var <- input$y_var_box_bar_scatter_violin
              z_var <- input$z_var_box_bar_scatter_violin
              w_var <- input$w_var_box_bar_scatter_violin
              extra_var <- input$additional_variable_box_bar_scatter_violin
              
              # number of bootstrap replicas
              n_boot <- input$n_boot_box_bar_scatter_violin
              
              # defines control and test groups
              control_group <- input$control_group_box_bar_scatter_violin
              test_group <- input$test_groups_box_bar_scatter_violin
              
              # selects effect sizes to run
              #for mean/median difference
              mean_median_effect_size <- input$effect_size_mean_median_box_bar_scatter_violin
              
              # for hedges g, 
              hedges_cohen_glass_effect_size <- input$effect_sizes_hedges_cohen_glass_box_bar_scatter_violin
              
              # selects plot to use for effect size comparison
              # plot_selected <- plot_for_all_box_bar_scatter_violin()
              if (!is.ggplot(plot_for_all_box_bar_scatter_violin())){
                plot_selected <- plot_for_all_box_bar_scatter_violin()[[2]]
              } else {
                plot_selected <- plot_for_all_box_bar_scatter_violin()    
              }
              
              # position for plots
              positioning_plot_mean_median_diff <- input$position_plot_mean_median_diff_box_bar_scatter_violin
            }
            
            # Extracts stored effect size data
            # for mean or median
            mean_list_mean_median <- mean_median_calculated_mean()
            CI_list_mean_median <-  mean_median_calculated_CI()
            density_list_mean_median <- mean_median_density_list()
            table_mean_median_diff <- mean_median_table_mean_median_diff()
            
            # for hedges, cohen or glass
            mean_list_hedges_cohen <- hedges_cohen_glass_calculated_mean()
            CI_list_hedges_cohen<- hedges_cohen_glass_calculated_CI()
            density_list_hedges_cohen<- hedges_cohen_glass_density_list()
            table_hedges_cohen <- hedges_cohen_glass_table_hedges_cohen()
            
            # sets group order
            group_name <- test_group
            
            table_mean_median_diff$Group <- factor(table_mean_median_diff$Group, levels = test_group)
            table_mean_median_diff <- table_mean_median_diff[order(table_mean_median_diff$Group), ]
            
            table_hedges_cohen$Group <- factor(table_hedges_cohen$Group, levels = test_group)
            table_hedges_cohen <- table_hedges_cohen[order(table_hedges_cohen$Group), ]
            
            # For mean or median effect size plot
            # mean from control group and ylimits from superplot
            if (input$navpage == 'Superplot') {
              mean_control_data <- mean(data_plot()[[input$y_var_superplot]][data_plot()[[input$group_var_superplot]] == control_group], na.rm = TRUE)
              ylim_superplot <-  ggplot_build(plot_selected)$layout$panel_scales_y[[1]]$range$range
            } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
              if (input$num_levels_box_bar_scatter_violin == 1) {
                mean_control_data <- mean(data[[x_var]][data[[group_var]] == control_group], na.rm = TRUE)
              } else if (input$num_levels_box_bar_scatter_violin == 2){
                mean_control_data <- mean(data[[y_var]][data[[group_var]] == control_group], na.rm = TRUE)
              } else if (input$num_levels_box_bar_scatter_violin == 3){
                mean_control_data <- mean(data[[z_var]][data[[group_var]] == control_group], na.rm = TRUE)
              } else if (input$num_levels_box_bar_scatter_violin == 4){
                mean_control_data <- mean(data[[w_var]][data[[group_var]] == control_group], na.rm = TRUE)
              }
              ylim_superplot <-  ggplot_build(plot_selected)$layout$panel_scales_y[[1]]$range$range
            }
            
            #Calculates maximum mean/median difference CI
            ylim_max_mean_median_diff <- 1.5 * max(abs(unlist(CI_list_mean_median)))
            ylim_min_mean_median_diff <- -1.5 * max(abs(unlist(CI_list_mean_median)))
            
            # sets y limits depending on plot position (aligned with mean or below plot)
            if (positioning_plot_mean_median_diff == "Aligned with mean/median") {
              coord <- coord_cartesian(
                ylim = c(-(mean_control_data - ylim_superplot[1]), -(mean_control_data - ylim_superplot[2])),
                xlim = c(max(sapply(density_list_mean_median, function(x) max(x$y))) * -2, 
                         max(sapply(density_list_mean_median, function(x) max(x$y))) * 2)
              )
              position_plot_mean_median_diff = "right"
            } else if (positioning_plot_mean_median_diff == "Below data") {
              coord <- coord_cartesian(
                ylim = c(ylim_min_mean_median_diff, ylim_max_mean_median_diff),
                xlim = c(max(sapply(density_list_mean_median, function(x) max(x$y))) * -2, 
                         max(sapply(density_list_mean_median, function(x) max(x$y))) * 2)
              )
              position_plot_mean_median_diff = "left"
            }
            
            # adjust names for effect size plot
            if (positioning_plot_mean_median_diff == "Below data") {
              # defines axis text for left side y axis
              y_axis_left <- element_text(size = prev_input_values$y_axis_text_size, color = prev_input_values$y_axis_text_colour, face = prev_input_values$y_axis_text_face, family = prev_input_values$y_axis_text_font, hjust = prev_input_values$y_axis_text_justification, margin = margin(l = prev_input_values$y_axis_text_margin))
              
              # adds empty row to blank space for control group
              empty_row <- data.frame(Group = " ", Mean_Median_Diff = NA, Mean = NA, CI_Lower = NA, CI_Upper = NA)
              table_mean_median_diff <- rbind(empty_row, table_mean_median_diff)
            }
            
            # Create a new column named 'Label' as a factor
            unique_labels <- unique(paste0(table_mean_median_diff$Group, "\nminus\n", control_group))
            table_mean_median_diff$Label <- factor(paste0(table_mean_median_diff$Group, "\nminus\n", control_group), levels = unique_labels)
            
            # Replace values in 'Label' column where empty value is supposed to exist
            if (positioning_plot_mean_median_diff == "Below data") {
              # converts 'Label' column to character
              table_mean_median_diff$Label <- as.character(table_mean_median_diff$Label)
              
              # replaces values in 'Label' column where empty value is supposed to exist
              table_mean_median_diff$Label[table_mean_median_diff$Label == paste0(" " , "\nminus\n", control_group)] <- " "
              
              # convert 'Label' column back to factor again
              table_mean_median_diff$Label <- as.factor(table_mean_median_diff$Label)
              
              # re-adjusts variable order again
              table_mean_median_diff$Label <- factor(table_mean_median_diff$Label, levels = unique(table_mean_median_diff$Label[order(table_mean_median_diff$Group)]))
              table_mean_median_diff$Label <- table_mean_median_diff$Label[order(table_mean_median_diff$Group)]
            }
            
            table_mean_median_diff$Label <- factor(table_mean_median_diff$Label, levels = unique(table_mean_median_diff$Label[order(table_mean_median_diff$Group)]))
            table_mean_median_diff$Label <- table_mean_median_diff$Label[order(table_mean_median_diff$Group)]
            
            
            # generates mean/median difference plot
            plot_effect_mean_median <- ggplot(table_mean_median_diff, aes (y = Mean_Median_Diff)) +
              #sets fill for density
              geom_density(fill = prev_input_values$distribution_fill_colour, 
                           alpha = prev_input_values$distribution_transparency, 
                           color = prev_input_values$distribution_line_colour, 
                           linetype = prev_input_values$distribution_line_type, 
                           size = prev_input_values$distribution_line_width, 
                           adjust= prev_input_values$distribution_bandwith_adjustment,
                           na.rm = TRUE) +
              #sets mean for effect size
              geom_point(y = table_mean_median_diff$Mean, x = 0, 
                         size = prev_input_values$mean_dot_size, 
                         color =  prev_input_values$mean_dot_colour, 
                         na.rm = TRUE) +
              #sets confident interval line
              geom_segment(aes(x = 0, y = CI_Lower, xend = 0, yend = CI_Upper), 
                           color = prev_input_values$CI_line_colour, 
                           size = prev_input_values$CI_line_width) +
              #sets dotted line from y=0
              geom_hline(yintercept = 0, 
                         linetype =  prev_input_values$zero_line_type, 
                         color = prev_input_values$zero_line_colour, 
                         size = prev_input_values$zero_line_width) +
              # adds dotted lines connecting mean to axis
              (if (paste0("effect_size_line_per_group_", tab_id) == TRUE) {
                geom_hline(yintercept = c(table_mean_median_diff$Mean), 
                           linetype =  prev_input_values$zero_line_type, 
                           color = prev_input_values$zero_line_colour, 
                           size = prev_input_values$zero_line_width)
              }) +
              
              coord +
              # move y-axis to left side
              scale_y_continuous(position = position_plot_mean_median_diff,labels = waiver()) + 
              scale_x_continuous(breaks=c(0)) +
              # adjusts axes elements
              theme(
                # Change plot background
                panel.background = element_rect(fill = prev_input_values$colour_background, color = prev_input_values$colour_background_border, size = prev_input_values$colour_background_border_thickness),
                plot.background = element_rect(fill = prev_input_values$colour_panel),
                # Change X axis elements
                axis.text.x = element_blank(),
                axis.line.x = element_line(color =  prev_input_values$x_line_colour, size =  prev_input_values$x_line_thickness), #line
                axis.ticks.x = element_line(color = prev_input_values$x_tick_colour, size = prev_input_values$x_tick_thickness), #ticks
                axis.ticks.length.x = unit(prev_input_values$x_tick_length, "pt"), # Change the length of minor ticks
                # Change Y axis elements
                axis.line.y = element_line(color =  prev_input_values$y_line_colour, size =  prev_input_values$y_line_thickness), #line
                axis.text.y = element_text(size = prev_input_values$y_text_size, color = prev_input_values$y_text_colour, family = prev_input_values$y_text_font),
                axis.ticks.length.y = unit(prev_input_values$y_tick_length, "pt"), # Change the length of minor ticks
                axis.ticks.y = element_line(color = prev_input_values$y_tick_colour, size = prev_input_values$y_tick_thickness), #ticks
                # Change x and y axes text
                axis.title.x = element_text(size = prev_input_values$x_axis_text_size, color = prev_input_values$x_axis_text_colour, face = prev_input_values$x_axis_text_face, family = prev_input_values$x_axis_text_font, hjust = prev_input_values$x_axis_text_justification, margin = margin(t = prev_input_values$x_axis_text_margin)),
                axis.title.y.right = element_text(size = prev_input_values$y_axis_text_size, color = prev_input_values$y_axis_text_colour, face = prev_input_values$y_axis_text_face, family = prev_input_values$y_axis_text_font, hjust = prev_input_values$y_axis_text_justification, margin = margin(l = prev_input_values$y_axis_text_margin)),
                axis.title.y.left = element_text(size = prev_input_values$y_axis_text_size, color = prev_input_values$y_axis_text_colour, face = prev_input_values$y_axis_text_face, family = prev_input_values$y_axis_text_font, hjust = prev_input_values$y_axis_text_justification, margin = margin(r = prev_input_values$y_axis_text_margin)),
                # effect size legend
                strip.text.x = element_text(size = prev_input_values$x_text_size, color = prev_input_values$x_text_colour, family = prev_input_values$x_text_font, vjust = 0, hjust = 0.5),
                strip.background.x = element_blank(),
                # sets names below scale
                strip.placement = 'outside',
                # sets spacing between panels
                panel.spacing=unit(0.1, "lines")
              ) +
              # creates grid based on Group
              facet_grid(~Label, scales = "free_x", switch = "x")  +
              labs(x = NULL, y =mean_median_effect_size) +
              # changes colour of initial x axis label when plot is below main data plot
              facetted_pos_scales(x = list(Label == " " ~ scale_x_continuous(guide = guide_axis_colour(colour = "white"))))
            
            
            # For hedges, cohen or glass effect size plot
            # calculates maximum CI
            ylim_max_hedges_cohen <- 1.5 * max(abs(unlist(CI_list_hedges_cohen)))
            ylim_min_hedges_cohen <- -1.5 * max(abs(unlist(CI_list_hedges_cohen)))
            
            # adds empty row to blank space for control group
            empty_row <- data.frame(Group = " ", Hedges_cohen_cliff = NA, Mean = NA, CI_Lower = NA, CI_Upper = NA)            
            table_hedges_cohen <- rbind(empty_row, table_hedges_cohen)
            
            # creates a new column named 'Label' as a factor
            unique_labels <- unique(paste0(table_hedges_cohen$Group, "\nminus\n", control_group))
            table_hedges_cohen$Label <- factor(paste0(table_hedges_cohen$Group, "\nminus\n", control_group), levels = unique_labels)
            
            # replaces values in 'Label' column where empty value is supposed to exist
            # converts 'Label' column to character
            table_hedges_cohen$Label <- as.character(table_hedges_cohen$Label)
            
            # replaces values in 'Label' column where empty value is supposed to exist
            table_hedges_cohen$Label[table_hedges_cohen$Label == paste0(" " , "\nminus\n", control_group)] <- " "
            
            # converts 'Label' column back to factor again
            table_hedges_cohen$Label <- as.factor(table_hedges_cohen$Label)
            
            # re-adjusts variable order again
            table_hedges_cohen$Group <- factor(table_hedges_cohen$Group, levels = c(" ", test_group))
            table_hedges_cohen$Label <- factor(table_hedges_cohen$Label, levels = unique(table_hedges_cohen$Label[order(table_hedges_cohen$Group)]))
            
            # generates hedges, cohen or glass effect size plot
            plot_effect_hedges_cohen <- ggplot(table_hedges_cohen, aes (y = Hedges_cohen_cliff)) +
              #sets fill for density
              geom_density(fill = prev_input_values$distribution_fill_colour, 
                           alpha = prev_input_values$distribution_transparency, 
                           color = prev_input_values$distribution_line_colour, 
                           linetype = prev_input_values$distribution_line_type, 
                           size = prev_input_values$distribution_line_width, 
                           adjust= prev_input_values$distribution_bandwith_adjustment,
                           na.rm = TRUE) +
              #sets mean for effect size
              geom_point(y = table_hedges_cohen$Mean,
                         x = 0,
                         size = prev_input_values$mean_dot_size, 
                         color =  prev_input_values$mean_dot_colour, 
                         na.rm = TRUE) +
              #sets confident interval line
              geom_segment(aes(x = 0, y = CI_Lower, xend = 0, yend = CI_Upper), 
                           color = prev_input_values$CI_line_colour, 
                           size = prev_input_values$CI_line_width) +
              #sets dotted line from y=0
              geom_hline(yintercept = 0, 
                         linetype =  prev_input_values$zero_line_type, 
                         color = prev_input_values$zero_line_colour, 
                         size = prev_input_values$zero_line_width) +
              #sets y limits to align with control mean (needs ajustment)
              coord_cartesian(ylim = c(ylim_min_hedges_cohen,ylim_max_hedges_cohen),
                              c(max(sapply(density_list_hedges_cohen, function(x) max(x$y))) * -2, 
                                max(sapply(density_list_hedges_cohen, function(x) max(x$y))) * 2)) +
              # move y-axis to left side
              scale_y_continuous(position = "left",labels = waiver()) + 
              scale_x_continuous(breaks=c(0)) +
              
              #remove x-axis
              theme(
                # Change plot background
                panel.background = element_rect(fill = prev_input_values$colour_background, color = prev_input_values$colour_background_border, size = prev_input_values$colour_background_border_thickness),
                plot.background = element_rect(fill = prev_input_values$colour_panel),
                # Change X axis elements
                axis.text.x = element_blank(),
                axis.line.x = element_line(color =  prev_input_values$x_line_colour, size =  prev_input_values$x_line_thickness), #line
                axis.ticks.x = element_line(color = prev_input_values$x_tick_colour, size = prev_input_values$x_tick_thickness), #ticks
                axis.ticks.length.x = unit(prev_input_values$x_tick_length, "pt"), # Change the length of minor ticks
                # Change Y axis elements
                axis.line.y = element_line(color =  prev_input_values$y_line_colour, size =  prev_input_values$y_line_thickness), #line
                axis.text.y = element_text(size = prev_input_values$y_text_size, color = prev_input_values$y_text_colour, family = prev_input_values$y_text_font),
                axis.ticks.length.y = unit(prev_input_values$y_tick_length, "pt"), # Change the length of minor ticks
                axis.ticks.y = element_line(color = prev_input_values$y_tick_colour, size = prev_input_values$y_tick_thickness), #ticks
                # Change x and y axes text
                axis.title.x = element_text(size = prev_input_values$x_axis_text_size, color = prev_input_values$x_axis_text_colour, face = prev_input_values$x_axis_text_face, family = prev_input_values$x_axis_text_font, hjust = prev_input_values$x_axis_text_justification, margin = margin(t = prev_input_values$x_axis_text_margin)),
                axis.title.y.left = element_text(size = prev_input_values$y_axis_text_size, color = prev_input_values$y_axis_text_colour, face = prev_input_values$y_axis_text_face, family = prev_input_values$y_axis_text_font, hjust = prev_input_values$y_axis_text_justification, margin = margin(r = prev_input_values$y_axis_text_margin)),
                # effect size legend
                strip.text.x = element_text(size = prev_input_values$x_text_size, color = prev_input_values$x_text_colour, family = prev_input_values$x_text_font, vjust = 0, hjust = 0.5),
                strip.background.x = element_blank(),
                # sets names below scale
                strip.placement = 'outside',
                # sets spacing between panels
                panel.spacing=unit(0.1, "lines")
              ) +
              # creates grid based on Group
              facet_grid(~Label, scales = "free_x", switch = "x")  +
              labs(x = NULL, y = hedges_cohen_glass_effect_size) +
              # changes colour of initial x axis label when plot is below main data plot
              facetted_pos_scales(x = list(Label == " " ~ scale_x_continuous(guide = guide_axis_colour(colour = "white"))))
            #######################################################################################################################
            
            mean_median_effect_size_plot(plot_effect_mean_median)
            hedges_cohen_glass_effect_size_plot(plot_effect_hedges_cohen)  
            
          }, error = function(e) {
            
          })
        }
      })
      
 ################################################################################
      ##########################################################################
      # General code for reactive plot
      ###################################################
      ###################################################
     
      # creates brushed points variable
      brushed_points_table <- reactiveVal()
      brushed_points_icc <- reactiveVal(0)
      brushed_points_icc_two <- reactiveVal(0)
      brushed_points_superplot <- reactiveVal(0)
      brushed_points_box_bar_scatter_violin <- reactiveVal(0)
      
      # creating code for saving options
        #generates reactive val to store information
        saved_options <- reactiveVal(0)

      # load plot saved options (action button triggers fileinput)
        # for table
        observeEvent(input$load_options_click_table,{
          runjs('$("#load_options_table").click();')
           })
        
        # for icc
        observeEvent(input$load_options_click_icc,{
          runjs('$("#load_options_icc").click();')
        })
        
        # for icc2
        observeEvent(input$load_options_click_icc_two,{
          runjs('$("#load_options_icc_two").click();')
        })
        
        # for bar, box, scatter, violin
        observeEvent(input$load_options_click_box_bar_scatter_violin,{
          runjs('$("#load_options_box_bar_scatter_violin").click();')
        })
        
        # for superplot
        observeEvent(input$load_options_click_superplot,{
          runjs('$("#load_options_superplot").click();')
        })
      
      # stores and uploads plot information 
      observe({
        req(input$navpage != 'Developers & Contact' && input$navpage != 'App Overview')
       # finds correct tab_id
        if (input$navpage == 'Data Selection and Visualization' && input$tabselected =='Table & Graph') {
          tab_id = "table"
          data <- selected_vars_plot()
          x_var <- input$x_axis
          y_var <- input$y_axis
          z_var <- input$y_var_table
          
        } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)') {
          tab_id = "icc"
          data <- data_icc()
          data <- selected_vars_plot_icc()
          if (input$num_levels >= 2) {
            x_var <- input$x_var_icc
            y_var <- input$y_var_icc
            z_var <- input$group_var_icc
          }
        } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
          tab_id = "icc_two"
          data <- selected_vars_plot_icc()
          shinyjs::hide("show_additional_group_legend_icc_two")
          if (input$num_levels == 2) {
            x_var <- input$group_var_icc
            y_var <- input$y_var_icc
            z_var <- input$x_var_icc
          } else if (input$num_levels == 3){
            x_var <- input$group_var_icc
            y_var <- input$z_var_icc
            z_var <- input$y_var_icc
            w_var <- input$x_var_icc
          } else if (input$num_levels == 4){
            x_var <- input$group_var_icc
            y_var <- input$w_var_icc
            z_var <- input$z_var_icc
            w_var <- input$x_var_icc
            extra_var <- input$y_var_icc
          }
          
        } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
          tab_id = "box_bar_scatter_violin"
          data <- data_plot()
          # names correctly based on number of levels
          if (input$num_levels_box_bar_scatter_violin == 1){
            x_var <- input$group_var_box_bar_scatter_violin
            y_var <- input$x_var_box_bar_scatter_violin
            z_var <- "£$%^&*"
          }else if (input$num_levels_box_bar_scatter_violin == 2) {
            x_var <- input$group_var_box_bar_scatter_violin
            y_var <- input$y_var_box_bar_scatter_violin
            z_var <- input$x_var_box_bar_scatter_violin
          } else if (input$num_levels_box_bar_scatter_violin == 3){
            x_var <- input$group_var_box_bar_scatter_violin
            y_var <- input$z_var_box_bar_scatter_violin
            z_var <- input$y_var_box_bar_scatter_violin
          } else if (input$num_levels_box_bar_scatter_violin == 4){
            x_var <- input$group_var_box_bar_scatter_violin
            y_var <- input$w_var_box_bar_scatter_violin
            z_var <- input$z_var_box_bar_scatter_violin
          }
        } else if (input$navpage == 'Superplot'){
          tab_id = "superplot"
          data <- data_plot()
          x_var <- input$group_var_superplot
          y_var <- input$y_var_superplot
          z_var <- input$x_var_superplot
        }
        
        
        # saves input information and colours
       observeEvent(input[[paste0("modal_", tab_id)]], {
         saved_options(
           list(
             # Gather the relevant input values that you want to save
             list(
               # X axis
               x_line_thickness = input[[paste0("x_line_thickness_", tab_id)]],
               x_line_colour = input[[paste0("x_line_colour_", tab_id)]],
               x_tick_thickness = input[[paste0("x_tick_thickness_", tab_id)]],
               x_tick_colour = input[[paste0("x_tick_colour_", tab_id)]],
               x_tick_length = input[[paste0("x_tick_length_", tab_id)]],
               x_tick_minor_length = input[[paste0("x_tick_minor_length_", tab_id)]],
               x_major_ticks_interval = input[[paste0("x_major_ticks_interval_", tab_id)]],
               x_minor_ticks_interval = input[[paste0("x_minor_ticks_interval_", tab_id)]],
               x_text_size = input[[paste0("x_text_size_", tab_id)]],
               x_text_colour = input[[paste0("x_text_colour_", tab_id)]],
               x_text_font = as.character(input[[paste0("x_text_font_", tab_id)]]),
               
               # Y axis
               y_line_thickness = input[[paste0("y_line_thickness_", tab_id)]],
               y_line_colour = input[[paste0("y_line_colour_", tab_id)]],
               y_tick_thickness = input[[paste0("y_tick_thickness_", tab_id)]],
               y_tick_colour = input[[paste0("y_tick_colour_", tab_id)]],
               y_tick_length = input[[paste0("y_tick_length_", tab_id)]],
               y_tick_minor_length = input[[paste0("y_tick_minor_length_", tab_id)]],
               y_major_ticks_interval = input[[paste0("y_major_ticks_interval_", tab_id)]],
               y_minor_ticks_interval = input[[paste0("y_minor_ticks_interval_", tab_id)]],
               y_text_size = input[[paste0("y_text_size_", tab_id)]],
               y_text_colour = input[[paste0("y_text_colour_", tab_id)]],
               y_text_font = as.character(input[[paste0("y_text_font_", tab_id)]]),
               
               #Scales Box
               x_start_range = input[[paste0("x_start_range_", tab_id)]],
               x_end_range = input[[paste0("x_end_range_", tab_id)]],
               y_start_range = input[[paste0("y_start_range_", tab_id)]],
               y_end_range = input[[paste0("y_end_range_", tab_id)]],
               x_scale_type = input[[paste0("x_scale_type_", tab_id)]],
               y_scale_type = input[[paste0("y_scale_type_", tab_id)]],
               numeric_display_type_y_axis = input[[paste0("numeric_display_type_y_axis_", tab_id)]],
               ## add scale breaks###
               ######################
               
               #Labels and Titles Box
               x_axis_text_size = input[[paste0("x_axis_text_size_", tab_id)]],
               x_axis_text_colour = input[[paste0("x_axis_text_colour_", tab_id)]],
               x_axis_text_face = input[[paste0("x_axis_text_face_", tab_id)]],
               x_axis_text_font = input[[paste0("x_axis_text_font_", tab_id)]],
               x_axis_text_justification = input[[paste0("x_axis_text_justification_", tab_id)]],
               x_axis_text_margin = input[[paste0("x_axis_text_margin_", tab_id)]],
               x_axis_text_title = input[[paste0("x_axis_text_title_", tab_id)]],
               y_axis_text_size = input[[paste0("y_axis_text_size_", tab_id)]],
               y_axis_text_colour = input[[paste0("y_axis_text_colour_", tab_id)]],
               y_axis_text_face = input[[paste0("y_axis_text_face_", tab_id)]],
               y_axis_text_font = input[[paste0("y_axis_text_font_", tab_id)]],
               y_axis_text_justification = input[[paste0("y_axis_text_justification_", tab_id)]],
               y_axis_text_margin = input[[paste0("y_axis_text_margin_", tab_id)]],
               y_axis_text_title = input[[paste0("y_axis_text_title_", tab_id)]],
               
               #Symbols Box
               geom_jitter_colour_fill_by_group = input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]],
               color_var = input[[paste0("color_var_", tab_id)]],
               symbol_fill = input[[paste0("symbol_fill_", tab_id)]],
               color_var_gradient_low = input[[paste0("color_var_gradient_low_", tab_id)]],
               color_var_gradient_high = input[[paste0("color_var_gradient_high_", tab_id)]],
               symbol_fill_gradient_low = input[[paste0("symbol_fill_gradient_low_", tab_id)]],
               symbol_fill_gradient_high = input[[paste0("symbol_fill_gradient_high_", tab_id)]],
               
               symbol_shape = input[[paste0("symbol_shape_", tab_id)]],
               symbol_size = input[[paste0("symbol_size_", tab_id)]],
               symbol_edge_thickness = input[[paste0("symbol_edge_thickness_", tab_id)]],
               symbol_jitter = input[[paste0("symbol_jitter_", tab_id)]],
               symbol_transparency = input[[paste0("symbol_transparency_", tab_id)]],
               
               #for Raincloud
               stat_dots_side = input[[paste0("stat_dots_side_", tab_id)]],
               stat_dots_bindwidth = input[[paste0("stat_dots_bandwidth_", tab_id)]],
               stat_dots_justification = input[[paste0("stat_dots_justification_", tab_id)]],
               
               #Trendline
               method_geom_smooth_scatter = input[[paste0("method_geom_smooth_scatter_", tab_id)]],
               line_thickness_geom_smooth = input[[paste0("line_thickness_geom_smooth_scatter_", tab_id)]],
               line_type_geom_smooth = input[[paste0("line_type_geom_smooth_scatter_", tab_id)]],
               confidence_interval_geom_smooth = input[[paste0("confidence_interval_geom_smooth_scatter_", tab_id)]],
               span_geom_smooth = input[[paste0("span_geom_smooth_scatter_", tab_id)]],
               transparency_geom_smooth = input[[paste0("transparency_geom_smooth_scatter_", tab_id)]],
               line_colour_geom_smooth = input[[paste0("line_colour_geom_smooth_scatter_", tab_id)]],
               fill_colour_geom_smooth = input[[paste0("fill_colour_geom_smooth_scatter_", tab_id)]],
               
               #Correlation coefficient
               correlation_coefficient_show = input[[paste0("correlation_coefficient_show_legend_", tab_id)]],
               correlation_coefficient_method = input[[paste0("correlation_coefficient_method_", tab_id)]],
               correlation_coefficient_name = input[[paste0("correlation_coefficient_name_", tab_id)]],
               correlation_coefficient_legend_position_x = input[[paste0("correlation_coefficient_legend_position_x_", tab_id)]],
               correlation_coefficient_legend_position_y = input[[paste0("correlation_coefficient_legend_position_y_", tab_id)]],
               correlation_coefficient_text_size = input[[paste0("correlation_coefficient_text_size_", tab_id)]],
               correlation_coefficient_text_font = input[[paste0("correlation_coefficient_text_font_", tab_id)]],
               
               #Box and Bar-charts
               geom_boxplot_colour_fill_by_group = input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]],
               box_line = input[[paste0("box_line_", tab_id)]],
               box_fill = input[[paste0("box_fill_", tab_id)]],
               box_line_thickness = input[[paste0("box_line_thickness_", tab_id)]],
               box_width = input[[paste0("box_width_", tab_id)]],
               box_raincloud_width = input[[paste0("box_raincloud_width_", tab_id)]],
               box_transparency = input[[paste0("box_transparency_", tab_id)]],
               sd_se_bar_chart = input[[paste0("sd_se_bar_chart_", tab_id)]],
               
               #Background Box
               colour_background = input[[paste0("colour_background_", tab_id)]],
               colour_background_border = input[[paste0("colour_background_border_", tab_id)]],
               colour_background_border_thickness = input[[paste0("colour_background_border_thickness_", tab_id)]],
               colour_panel = input[[paste0("colour_panel_", tab_id)]],
               colour_major_x_grid = input[[paste0("colour_major_x_grid_", tab_id)]],
               thickness_major_x_grid = input[[paste0("thickness_major_x_grid_", tab_id)]],
               colour_minor_x_grid = input[[paste0("colour_minor_x_grid_", tab_id)]],
               thickness_minor_x_grid = input[[paste0("thickness_minor_x_grid_", tab_id)]],
               colour_major_y_grid = input[[paste0("colour_major_y_grid_", tab_id)]],
               thickness_major_y_grid = input[[paste0("thickness_major_y_grid_", tab_id)]],
               colour_minor_y_grid = input[[paste0("colour_minor_y_grid_", tab_id)]],
               thickness_minor_y_grid = input[[paste0("thickness_minor_y_grid_", tab_id)]],
               
               #Legend
               maginal_plot_legend = input[[paste0("maginal_plot_legend_", tab_id)]],
               marginal_plot_legend_position_x = input[[paste0("marginal_plot_legend_position_x_", tab_id)]],
               marginal_plot_legend_position_y = input[[paste0("marginal_plot_legend_position_y_", tab_id)]],
               marginal_plot_legend_title = input[[paste0("marginal_plot_legend_title_", tab_id)]],
               marginal_plot_legend_text_size = input[[paste0("marginal_plot_legend_text_size_", tab_id)]],
               marginal_plot_legend_text_face = input[[paste0("marginal_plot_legend_text_face_", tab_id)]],
               marginal_plot_legend_text_font = input[[paste0("marginal_plot_legend_text_font_", tab_id)]],
               
               #Marginal Plots
               marginal_plot_type = input[[paste0("marginal_plot_type_", tab_id)]],
               marginal_plot_margins = input[[paste0("marginal_plot_margins_", tab_id)]],
               marginal_plot_size = input[[paste0("marginal_plot_size_", tab_id)]],
               marginal_plot_legend_position_x = input[[paste0("marginal_plot_legend_position_x_", tab_id)]],
               marginal_plot_legend_position_y = input[[paste0("marginal_plot_legend_position_y_", tab_id)]],
               marginal_plot_bindwidth_x = input[[paste0("marginal_plot_bindwidth_x_", tab_id)]],
               marginal_plot_border_colour_x = input[[paste0("marginal_plot_border_colour_x_", tab_id)]],
               marginal_plot_fill_colour_x = input[[paste0("marginal_plot_fill_colour_x_", tab_id)]],
               marginal_plot_line_width_x = input[[paste0("marginal_plot_line_width_x_", tab_id)]],
               marginal_plot_transparency_x = input[[paste0("marginal_plot_transparency_x_", tab_id)]],
               marginal_plot_bindwidth_y = input[[paste0("marginal_plot_bindwidth_y_", tab_id)]],
               marginal_plot_border_colour_y = input[[paste0("marginal_plot_border_colour_y_", tab_id)]],
               marginal_plot_fill_colour_y = input[[paste0("marginal_plot_fill_colour_y_", tab_id)]],
               marginal_plot_line_width_y = input[[paste0("marginal_plot_line_width_y_", tab_id)]],
               marginal_plot_transparency_y = input[[paste0("marginal_plot_transparency_y_", tab_id)]],
               
               #Raincloud halfeye
               raincloud_box_width = input[[paste0("box_raincloud_width_", tab_id)]],
               raincloud_halfeye_width = input[[paste0("raincloud_halfeye_width_", tab_id)]],
               raincloud_halfeye_line_thickness = input[[paste0("raincloud_halfeye_line_thickness_", tab_id)]],
               raincloud_halfeye_justification = input[[paste0("raincloud_halfeye_justification_", tab_id)]],
               raincloud_halfeye_transparency = input[[paste0("raincloud_halfeye_transparency_", tab_id)]],
               
               # Effect size plots
               distribution_fill_colour = input[[paste0("effect_size_distribution_fill_colour_", tab_id)]],
               distribution_line_colour = input[[paste0("effect_size_distribution_line_colour_", tab_id)]],
               distribution_transparency = input[[paste0("effect_size_distribution_transparency_", tab_id)]],
               distribution_line_type = input[[paste0("effect_size_distribution_line_type_", tab_id)]],
               distribution_line_width = input[[paste0("effect_size_distribution_line_width_", tab_id)]],
               distribution_bandwith_adjustment = input[[paste0("effect_size_distribution_bandwith_adjustment_", tab_id)]],
               effect_size_line_per_group_colour = input[[paste0("effect_size_line_per_group_colour_", tab_id)]],
               effect_size_line_per_group_transparency = input[[paste0("effect_size_line_per_group_transparency_", tab_id)]],
               effect_size_line_per_group = input[[paste0("effect_size_line_per_group_", tab_id)]],
               effect_size_line_per_group_type = input[[paste0("effect_size_line_per_group_type_", tab_id)]],
               mean_dot_colour = input[[paste0("effect_size_mean_colour_", tab_id)]],
               mean_dot_size = input[[paste0("effect_size_mean_dot_size_", tab_id)]],
               CI_line_colour = input[[paste0("effect_size_CI_colour_", tab_id)]],
               CI_line_width = input[[paste0("effect_size_CI_line_width_", tab_id)]],
               zero_line_colour = input[[paste0("effect_size_zero_colour_", tab_id)]],
               zero_line_type = input[[paste0("effect_size_zero_line_type_", tab_id)]],
               zero_line_width = input[[paste0("effect_size_zero_line_width_", tab_id)]]
             ),
             # saves colours for geom_jitter and box
             list(
               eval(parse(text = paste0("selected_colors_", tab_id, "$colors"))),
               eval(parse(text = paste0("selected_colours_symbol_fill_", tab_id, "$colors"))),
               eval(parse(text = paste0("selected_colors_boxplot_line_", tab_id, "$colors"))),
               eval(parse(text = paste0("selected_colors_boxplot_fill_", tab_id, "$colors")))
               )
             )
         )
         
         # save plot options
         output[[paste0("save_options_", tab_id)]] <- downloadHandler(
           filename = "saved_plot_options.rds",
           content = function(file) {
             saveRDS(saved_options(), file = file)
           }
         ) 
        
         # observeEvent for file upload
         observeEvent(input[[paste0("load_options_", tab_id)]],{
           # Get the file path
           file_path <- input[[paste0("load_options_", tab_id)]][["datapath"]]
           
           # Load the saved options from the file
           saved_options(readRDS(file_path))

           # updates color lists
           eval(parse(text = paste0("selected_colors_", tab_id, "$colors <- saved_options()[[2]][1][[1]]")))
           eval(parse(text = paste0("selected_colours_symbol_fill_", tab_id, "$colors <- saved_options()[[2]][2][[1]]")))
           eval(parse(text = paste0("selected_colors_boxplot_line_", tab_id, "$colors <- saved_options()[[2]][3][[1]]")))
           eval(parse(text = paste0("selected_colors_boxplot_fill_", tab_id, "$colors <- saved_options()[[2]][4][[1]]")))
           
           # updates inputs
            # X axis 
           updateNumericInput(session, paste0("x_line_thickness_", tab_id), "Line thickness:", value = saved_options()[[1]]$x_line_thickness)
           updateColourInput(session, paste0("x_line_colour_", tab_id), "Line color:", value = saved_options()[[1]]$x_line_colour)
           updateNumericInput(session, paste0("x_tick_thickness_", tab_id), "Major tick thickness:", value = saved_options()[[1]]$x_tick_thickness)
           updateColourInput(session, paste0("x_tick_colour_", tab_id), "Ticks color:", value = saved_options()[[1]]$x_tick_colour)
           updateNumericInput(session, paste0("x_tick_length_", tab_id), "Major tick length:", value =  saved_options()[[1]]$x_tick_length)
           updateNumericInput(session, paste0("x_tick_minor_length_", tab_id), "Minor Tick length:", value = saved_options()[[1]]$x_tick_minor_length)
           updateNumericInput(session, paste0("x_major_ticks_interval_", tab_id), "Major tick interval (set x axis range first):", value = saved_options()[[1]]$x_major_ticks_interval, min = 0.01)
          updateNumericInput(session, paste0("x_major_ticks_interval_", tab_id), "Major tick interval (set x axis range first):", value = saved_options()[[1]]$x_minor_ticks_interval, min = 0.01)
          updateNumericInput(session, paste0("x_text_size_", tab_id), "Text size:", value = saved_options()[[1]]$x_text_size)
          updateNumericInput(session, paste0("x_text_colour_", tab_id), "Text color:", value = saved_options()[[1]]$x_text_colour)
          updateSelectInput(session, paste0("x_text_font_", tab_id), "Text font:", choices = c("Arial", "Times New Roman", "Helvetica", "Calibri", "Verdana", "Georgia", "Courier New", "Palatino Linotype"), selected = saved_options()[[1]]$x_text_font)
          
          #Y axis
          updateNumericInput(session, paste0("y_line_thickness_", tab_id), "Line thickness:", value = saved_options()[[1]]$y_line_thickness)
          updateColourInput(session, paste0("y_line_colour_", tab_id), "Line color:", value = saved_options()[[1]]$y_line_colour)
          updateNumericInput(session, paste0("y_tick_thickness_", tab_id), "Major tick thickness:", value = saved_options()[[1]]$y_tick_thickness)
          updateColourInput(session, paste0("y_tick_colour_", tab_id), "Ticks color:", value = saved_options()[[1]]$y_tick_colour)
          updateNumericInput(session, paste0("y_tick_length_", tab_id), "Major tick length:", value =  saved_options()[[1]]$y_tick_length)
          updateNumericInput(session, paste0("y_tick_minor_length_", tab_id), "Minor Tick length:", value = saved_options()[[1]]$y_tick_minor_length)
          updateNumericInput(session, paste0("y_major_ticks_interval_", tab_id), "Major tick interval (set x axis range first):", value = saved_options()[[1]]$y_major_ticks_interval, min = 0.01)
          updateNumericInput(session, paste0("y_major_ticks_interval_", tab_id), "Major tick interval (set x axis range first):", value = saved_options()[[1]]$y_minor_ticks_interval, min = 0.01)
          updateNumericInput(session, paste0("y_text_size_", tab_id), "Text size:", value = saved_options()[[1]]$y_text_size)
          updateNumericInput(session, paste0("y_text_colour_", tab_id), "Text color:", value = saved_options()[[1]]$y_text_colour)
          updateSelectInput(session, paste0("y_text_font_", tab_id), "Text font:", choices = c("Arial", "Times New Roman", "Helvetica", "Calibri", "Verdana", "Georgia", "Courier New", "Palatino Linotype"), selected = saved_options()[[1]]$y_text_font)
          
          # Scales box
          updateNumericInput(session, paste0("x_start_range_", tab_id), "X-axis Start:", value = saved_options()[[1]]$x_start_range)
          updateNumericInput(session, paste0("x_end_range_", tab_id), "X-axis End:", value = saved_options()[[1]]$x_end_range)
          updateNumericInput(session, paste0("y_start_range_", tab_id), "Y-axis Start:", value = saved_options()[[1]]$y_start_range)
          updateNumericInput(session, paste0("y_end_range_", tab_id), "Y-axis End:", value = saved_options()[[1]]$y_end_range)
          updateRadioButtons(session, paste0("x_scale_type_", tab_id), "X-axis Scale Type:",
                                choices = c("Linear" = "linear", "Log10" = "log10", "Log2" = "log2"),
                                selected = saved_options()[[1]]$x_scale_type)
          updateRadioButtons(session, paste0("y_scale_type_", tab_id), "Y-axis Scale Type:",
                                choices = c("Linear" = "linear", "Log10" = "log10", "Log2" = "log2"),
                                selected = saved_options()[[1]]$y_scale_type)
          updateSelectInput(session, paste0("numeric_display_type_y_axis_", tab_id), "Numeric display", choices = c("Decimal", "Scientific"), selected = saved_options()[[1]]$numeric_display_type_y_axis)
          # updateNumericInput(paste0("x_num_breaks_", tab_id), "Number of X-axis Breaks:", value = NULL)
          # updateTextInput(paste0("x_break_values_", tab_id), "X-axis Break Values:", value = NULL)
          # updateNumericInput(paste0("y_num_breaks_", tab_id), "Number of Y-axis Breaks:", value = NULL)
          # updateTextInput(paste0("y_break_values_", tab_id), "Y-axis Break Values:", value = NULL)
          
          # Labels and titles box
          updateNumericInput(session, inputId = paste0("x_axis_text_size_", tab_id), label = "Text size", value = saved_options()[[1]]$x_axis_text_size)
          updateColourInput(session, inputId = paste0("x_axis_text_colour_", tab_id), label = "Select text colour", value = saved_options()[[1]]$x_axis_text_colour)
          updateSelectInput(session, inputId = paste0("x_axis_text_face_", tab_id), label = "Text face", choices = c("plain", "italic", "bold", "bold.italic"), selected = saved_options()[[1]]$x_axis_text_face)
          updateSelectInput(session, inputId = paste0("x_axis_text_font_", tab_id), label = "Text fount", choices = c("Arial", "Times New Roman", "Helvetica", "Calibri", "Verdana", "Georgia", "Courier New", "Palatino Linotype"), selected = saved_options()[[1]]$x_axis_text_font)
          updateSliderInput(session, inputId = paste0("x_axis_text_justification_", tab_id), label = "Text justification", min = 0, max =1, value = saved_options()[[1]]$x_axis_text_justification)
          updateNumericInput(session, inputId = paste0("x_axis_text_margin_", tab_id), label = "Margin", value = saved_options()[[1]]$x_axis_text_margin)
          updateTextInput(session, inputId = paste0("x_axis_text_title_", tab_id), label = "Text title", value = saved_options()[[1]]$x_axis_text_title)
          updateNumericInput(session, inputId = paste0("y_axis_text_size_", tab_id), label = "Text size", value = saved_options()[[1]]$y_axis_text_size)
          updateColourInput(session, inputId = paste0("y_axis_text_colour_", tab_id), label = "Select text colour", value = saved_options()[[1]]$y_axis_text_colour)
          updateSelectInput(session, inputId = paste0("y_axis_text_face_", tab_id), label = "Text face", choices = c("plain", "italic", "bold", "bold.italic"), selected = saved_options()[[1]]$y_axis_text_face)
          updateSelectInput(session, inputId = paste0("y_axis_text_font_", tab_id), label = "Text fount", choices = c("Arial", "Times New Roman", "Helvetica", "Calibri", "Verdana", "Georgia", "Courier New", "Palatino Linotype"), selected = saved_options()[[1]]$y_axis_text_font)
          updateSliderInput(session, inputId = paste0("y_axis_text_justification_", tab_id), label = "Text justification", min = 0, max =1, value = saved_options()[[1]]$y_axis_text_justification)
          updateNumericInput(session, inputId = paste0("y_axis_text_margin_", tab_id), label = "Margin", value = saved_options()[[1]]$y_axis_text_margin)
          updateTextInput(session, inputId = paste0("y_axis_text_title_", tab_id), label = "Text title", value = saved_options()[[1]]$y_axis_text_title)
       
          # Symbols Box (if selected variable for colour is not present it returns 'Single Colour')
            if (any(saved_options()[[1]]$geom_jitter_colour_fill_by_group %in% c(x_var, z_var, paste0(x_var, " "), paste0(z_var, " ")))) {
              updateSelectInput(session, paste0("geom_jitter_colour_fill_by_group_", tab_id), choices = list("Single Colour", "Colour by Group - discrete" = c(x_var, z_var), "Colour by Group - gradient"= c(paste0(x_var, " "), paste0(z_var, " "))), selected = saved_options()[[1]]$geom_jitter_colour_fill_by_group)
            } else {
              updateSelectInput(session, paste0("geom_jitter_colour_fill_by_group_", tab_id), choices = list("Single Colour", "Colour by Group - discrete" = c(x_var, z_var), "Colour by Group - gradient"= c(paste0(x_var, " "), paste0(z_var, " "))), selected = "Single Colour")
            }
          
          #updateSelectInput(session, inputId = paste0("geom_jitter_colour_fill_by_group_", tab_id), label = "", choices = c("Colour by Group - discrete", "Colour by Group - continuous", "Single Colour"), selected = saved_options()[[1]]$geom_jitter_colour_fill_by_group)
          updateColourInput(session, inputId = paste0("color_var_", tab_id), label = "Select border color", value = saved_options()[[1]]$color_var)
          updateColourInput(session, inputId = paste0("color_var_gradient_low_", tab_id), label = "Select low gradient border colour", value = saved_options()[[1]]$color_var_gradient_low)
          updateColourInput(session, inputId = paste0("color_var_gradient_high_", tab_id), label = "Select high gradient border colour", value = saved_options()[[1]]$color_var_gradient_high)
          updateColourInput(session, inputId = paste0("symbol_fill_", tab_id), label = "Select fill color", value = saved_options()[[1]]$symbol_fill)
          updateColourInput(session, inputId = paste0("symbol_fill_gradient_low_", tab_id), label = "Select low gradient fill colour", value = saved_options()[[1]]$symbol_fill_gradient_low)
          updateColourInput(session, inputId = paste0("symbol_fill_gradient_high_", tab_id), label = "Select high gradient fill colour", value = saved_options()[[1]]$symbol_fill_gradient_high)
          updateSelectInput(session, inputId = paste0("symbol_shape_", tab_id), label = "Shape", choices = list(
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
          selected = saved_options()[[1]]$symbol_shape)
          updateNumericInput(session, inputId = paste0("symbol_size_", tab_id), label = "Size", value = saved_options()[[1]]$symbol_size)
          updateNumericInput(session, inputId = paste0("symbol_edge_thickness_", tab_id), label = "Edge thickness", value = saved_options()[[1]]$symbol_edge_thickness)
          updateNumericInput(session, inputId = paste0("symbol_jitter_", tab_id), label = "Jitter", value = saved_options()[[1]]$symbol_jitter, step = 0.1)
          updateSliderInput(session, inputId = paste0("symbol_transparency_", tab_id), label = "Transparency", min = 0, max = 1, value = saved_options()[[1]]$symbol_transparency)
          
          # raincloud dots
          updateSelectInput(session, inputId = paste0("stat_dots_side_", tab_id), label = "Select side for dot display", choices = c("left", "bottomleft", "topleft", "bottom", "right", "bottomright", "topright", "both"), selected = saved_options()[[1]]$stat_dots_side)
          updateNumericInput(session, inputId = paste0("stat_dots_bindwidth_", tab_id), label = "Select dot binwidth", value = saved_options()[[1]]$stat_dots_bindwidth, step = 1)
          updateNumericInput(session, inputId = paste0("stat_dots_justification_", tab_id), label = "Dot justification", value = saved_options()[[1]]$stat_dots_justification, step = 0.1)
         
          # trendline
          updateSelectInput(session, inputId = paste0("method_geom_smooth_scatter_", tab_id), label = "Method", choices = c("Linear" = "lm", "General linear model" = "glm", "Generalized Additive Model" = "gam", "Locally Weighted Least Squares Regression" = "loess"), selected = saved_options()[[1]]$method_geom_smooth_scatter)
          updateNumericInput(session, inputId = paste0("line_thickness_geom_smooth_scatter_", tab_id), label = "Line thickness", value = saved_options()[[1]]$line_thickness_geom_smooth, step =0.1)
          updateSelectInput(session, inputId = paste0("line_type_geom_smooth_scatter_", tab_id), label = "Line type", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = saved_options()[[1]]$line_type_geom_smooth)
          updateNumericInput(session, inputId = paste0("confidence_interval_geom_smooth_scatter_", tab_id), label = "Confidence interval", value = saved_options()[[1]]$confidence_interval_geom_smooth, step = 0.01)
          updateNumericInput(session, inputId = paste0("span_geom_smooth_scatter_", tab_id), label = "Smoothing", value = saved_options()[[1]]$span_geom_smooth, step =0.05)
          updateSliderInput(session, inputId = paste0("transparency_geom_smooth_scatter_", tab_id), label = "Confidence interval transparency", min = 0, max = 1, value = saved_options()[[1]]$transparency_geom_smooth)
          updateColourInput(session, inputId = paste0("line_colour_geom_smooth_scatter_", tab_id), label = "Select line colour", value = saved_options()[[1]]$line_colour_geom_smooth)
          updateColourInput(session, inputId = paste0("fill_colour_geom_smooth_scatter_", tab_id), label = "Select line colour", value = saved_options()[[1]]$fill_colour_geom_smooth)
          
          # correlation coefficient
          updateCheckboxInput(session, inputId = paste0("correlation_coefficient_show_legend_", tab_id), label = "Show coefficient", value = saved_options()[[1]]$correlation_coefficient_show)
          updateSelectInput(session, inputId = paste0("correlation_coefficient_method_", tab_id), label = "Type of correlation test", choices = c("spearman", "pearson", "kendall"), selected = saved_options()[[1]]$correlation_coefficient_method)
          updateSelectInput(session, inputId = paste0("correlation_coefficient_name_", tab_id), label = "Type of correlation coefficient", choices = c("R", "rho", "tau"), selected = saved_options()[[1]]$correlation_coefficient_name)
          updateSelectInput(session, inputId = paste0("correlation_coefficient_legend_position_x_", tab_id), label = "horizontal vector",  choices = c('right', 'left', 'center', 'middle'), selected = saved_options()[[1]]$correlation_coefficient_legend_position_x)
          updateSelectInput(session, inputId = paste0("correlation_coefficient_legend_position_y_", tab_id), label = "vertical vector", choices = c( 'bottom', 'top', 'center', 'middle'), selected = saved_options()[[1]]$correlation_coefficient_legend_position_y)
          updateNumericInput(session, inputId = paste0("correlation_coefficient_text_size_", tab_id), label = "Text size", value = saved_options()[[1]]$correlation_coefficient_text_size)
          updateSelectInput(session, inputId = paste0("correlation_coefficient_text_font_", tab_id), label = "Text fount", choices = c("Arial", "Times New Roman", "Helvetica", "Calibri", "Verdana", "Georgia", "Courier New", "Palatino Linotype"), selected = saved_options()[[1]]$correlation_coefficient_text_font)
          
          # box and bar-charts
          # Symbols Box (if selected variable for colour is not present it returns 'Single Colour')
            if (any(saved_options()[[1]]$geom_boxplot_colour_fill_by_group %in% c(x_var, z_var, paste0(x_var, " "), paste0(z_var, " ")))) {
              updateSelectInput(session, paste0("geom_boxplot_colour_fill_by_group_", tab_id), choices = list("Single Colour", "Colour by Group - discrete" = c(x_var, z_var), "Colour by Group - gradient"= c(paste0(x_var, " "), paste0(z_var, " "))), selected = saved_options()[[1]]$geom_boxplot_colour_fill_by_group)
            } else {
              updateSelectInput(session, paste0("geom_boxplot_colour_fill_by_group_", tab_id), choices = list("Single Colour", "Colour by Group - discrete" = c(x_var, z_var), "Colour by Group - gradient"= c(paste0(x_var, " "), paste0(z_var, " "))), selected = "Single Colour")
            }
          
          updateColourInput(session, inputId = paste0("box_line_", tab_id), label = "Box line colour", value = saved_options()[[1]]$box_line)
          updateColourInput(session, inputId = paste0("box_fill_", tab_id), label = "Box fill colour", value = saved_options()[[1]]$box_fill)
          updateNumericInput(session, inputId = paste0("box_line_thickness_", tab_id), label = "Box line thickness", value = saved_options()[[1]]$box_line_thickness, step = 0.1)
          updateNumericInput(session, inputId = paste0("box_width_", tab_id), label = "Box width", value = saved_options()[[1]]$box_width, step = 0.1)
          updateNumericInput(session, inputId = paste0("box_raincloud_width_", tab_id), label = "Box width", value = saved_options()[[1]]$box_raincloud_width, step = 0.1)
          updateSliderInput(session, inputId = paste0("box_transparency_", tab_id), label = "Box transparency", min = 0, max = 1, value = saved_options()[[1]]$box_transparency)
          updateRadioButtons(session, inputId = paste0("sd_se_bar_chart_", tab_id), label = "Select errorbar display", choices = c("Standard deviation", "Standard error of mean", "None"), selected = saved_options()[[1]]$sd_se_bar_chart)
          
          #Background Box
          updateColourInput(session, inputId = paste0("colour_background_", tab_id), label = "Background colour", value = saved_options()[[1]]$colour_background)
          updateColourInput(session, inputId = paste0("colour_background_border_", tab_id), label = "Background border colour", value = saved_options()[[1]]$colour_background_border)
          updateNumericInput(session, inputId = paste0("colour_background_border_thickness_", tab_id), label = "Background border thickness", value = saved_options()[[1]]$colour_background_border_thickness)
          updateColourInput(session, inputId = paste0("colour_panel_", tab_id), label = "Panel colour", value = saved_options()[[1]]$colour_panel)
          updateColourInput(session, inputId = paste0("colour_major_x_grid_", tab_id), label = "X axis major grid colour", value = saved_options()[[1]]$colour_major_x_grid)
          updateNumericInput(session, inputId = paste0("thickness_major_x_grid_", tab_id), label = "X axis major grid thickness", value = saved_options()[[1]]$thickness_major_x_grid)
          updateColourInput(session, inputId = paste0("colour_minor_x_grid_", tab_id), label = "X axis minor grid colour", value = saved_options()[[1]]$colour_minor_x_grid)
          updateNumericInput(session, inputId = paste0("thickness_minor_x_grid_", tab_id), label = "X axis minor grid thickness", value = saved_options()[[1]]$thickness_minor_x_grid)
          updateColourInput(session, inputId = paste0("colour_major_y_grid_", tab_id), label = "Y axis major grid colour", value = saved_options()[[1]]$colour_major_y_grid)
          updateNumericInput(session, inputId = paste0("thickness_major_y_grid_", tab_id), label = "Y axis major grid thickness", value = saved_options()[[1]]$thickness_major_y_grid)
          updateColourInput(session, inputId = paste0("colour_minor_y_grid_", tab_id), label = "Y axis minor grid colour", value = saved_options()[[1]]$colour_minor_y_grid)
          updateNumericInput(session, inputId = paste0("thickness_minor_y_grid_", tab_id), label = "Y axis minor grid thickness", value = saved_options()[[1]]$thickness_minor_y_grid)
          
          # Legend
          updateSelectInput(session, inputId = paste0("maginal_plot_legend_", tab_id), label = "Legend position", choices = c("none", "left","top", "right", "bottom", "custom"), selected = saved_options()[[1]]$maginal_plot_legend)
          updateNumericInput(session, inputId = paste0("marginal_plot_legend_position_x_", tab_id), label = "horizontal vector", value = saved_options()[[1]]$marginal_plot_legend_position_x, step = 0.1)
          updateNumericInput(session, inputId = paste0("marginal_plot_legend_position_y_", tab_id), label = "vertical vector", value = saved_options()[[1]]$marginal_plot_legend_position_y, step = 0.1)
          updateCheckboxInput(session, inputId = paste0("marginal_plot_legend_title_", tab_id), label = "Show legend title", value = saved_options()[[1]]$marginal_plot_legend_title)
          updateNumericInput(session, inputId = paste0("marginal_plot_legend_text_size_", tab_id), label = "Text size", value = saved_options()[[1]]$marginal_plot_legend_text_size)
          updateSelectInput(session, inputId = paste0("marginal_plot_legend_text_face_", tab_id), label = "Text face", choices = c("plain", "italic", "bold", "bold.italic"), selected = saved_options()[[1]]$marginal_plot_legend_text_face)
          updateSelectInput(session, inputId = paste0("marginal_plot_legend_text_font_", tab_id), label = "Text fount", choices = c("Arial", "Times New Roman", "Helvetica", "Calibri", "Verdana", "Georgia", "Courier New", "Palatino Linotype"), selected = saved_options()[[1]]$marginal_plot_legend_text_font)
          
          # Marginal Plots
          updateSelectInput(session, inputId = paste0("marginal_plot_type_", tab_id), label = "Choose type of marginal plot", choices = c("density", "histogram", "boxplot", "violin", "densigram"), selected = saved_options()[[1]]$marginal_plot_type)
          updateSelectInput(session, inputId = paste0("marginal_plot_margins_", tab_id), label = "Choose axis to display plots", choices = c("x", "y", "both"), selected = saved_options()[[1]]$marginal_plot_margins)
          updateNumericInput(session, inputId = paste0("marginal_plot_size_", tab_id), label = "Marginal plot size", value = saved_options()[[1]]$marginal_plot_size, step = 1)
          updateNumericInput(session, inputId = paste0("marginal_plot_legend_position_x_", tab_id), label = "horizontal vector", value = saved_options()[[1]]$marginal_plot_legend_position_x, step = 0.1)
          updateNumericInput(session, inputId = paste0("marginal_plot_legend_position_y_", tab_id), label = "vertical vector", value = saved_options()[[1]]$marginal_plot_legend_position_y, step = 0.1)
          updateNumericInput(session, inputId = paste0("marginal_plot_bindwidth_x_", tab_id), label = "Select binwidth", value = saved_options()[[1]]$marginal_plot_bindwidth_x)
          updateColourInput(session, inputId = paste0("marginal_plot_border_colour_x_", tab_id), label = "Choose line colour", value = saved_options()[[1]]$marginal_plot_border_colour_x)
          updateColourInput(session, inputId = paste0("marginal_plot_fill_colour_x_", tab_id), label = "Choose fill colour", value = saved_options()[[1]]$marginal_plot_fill_colour_x)
          updateNumericInput(session, inputId = paste0("marginal_plot_line_width_x_", tab_id), label = "Line width", value = saved_options()[[1]]$marginal_plot_line_width_x, step = 0.1)
          updateSliderInput(session, inputId = paste0("marginal_plot_transparency_x_", tab_id), label = "Transparency", min = 0, max = 1, value = saved_options()[[1]]$marginal_plot_transparency_x)
          updateNumericInput(session, inputId = paste0("marginal_plot_bindwidth_y_", tab_id), label = "Select binwidth", value = saved_options()[[1]]$marginal_plot_bindwidth_y)
          updateColourInput(session, inputId = paste0("marginal_plot_border_colour_y_", tab_id), label = "Choose line colour", value = saved_options()[[1]]$marginal_plot_border_colour_y)
          updateColourInput(session, inputId = paste0("marginal_plot_fill_colour_y_", tab_id), label = "Choose fill colour", value = saved_options()[[1]]$marginal_plot_fill_colour_y)
          updateNumericInput(session, inputId = paste0("marginal_plot_line_width_y_", tab_id), label = "Line width", value = saved_options()[[1]]$marginal_plot_line_width_y, step = 0.1)
          updateSliderInput(session, inputId = paste0("marginal_plot_transparency_y_", tab_id), label = "Transparency", min = 0, max = 1, value = saved_options()[[1]]$marginal_plot_transparency_y)
          
          # Raincloud halfeyes
          updateNumericInput(session, inputId = paste0("raincloud_halfeye_width_", tab_id), label = "Cloud width", value = saved_options()[[1]]$raincloud_box_width, step = 0.1)
          updateNumericInput(session, inputId = paste0("raincloud_halfeye_line_thickness_", tab_id), label = "Bandwith", value = saved_options()[[1]]$raincloud_halfeye_width, step = 0.1)
          updateNumericInput(session, inputId = paste0("raincloud_halfeye_justification_", tab_id), label = "Justification", value = saved_options()[[1]]$raincloud_halfeye_line_thickness, step = 0.1)
          updateSliderInput(session, inputId = paste0("raincloud_halfeye_transparency_", tab_id), label = "Transparency", min = 0, max = 1, value = saved_options()[[1]]$raincloud_halfeye_transparency)
          
          # Effect size plots
          colourInput(inputId = paste0("effect_size_distribution_fill_colour_", tab_id), label = "Fill colour", value = saved_options()[[1]]$distribution_fill_colour)
          colourInput(inputId = paste0("effect_size_distribution_line_colour_", tab_id), label = "Line colour", value = saved_options()[[1]]$distribution_line_colour)
          sliderInput(inputId = paste0("effect_size_distribution_transparency_", tab_id), label = "Transparency", min = 0, max = 1, value = saved_options()[[1]]$distribution_transparency)
          selectInput(inputId = paste0("effect_size_distribution_line_type_", tab_id), label = "Line type", 
                      choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = saved_options()[[1]]$distribution_line_type)
          numericInput(inputId = paste0("effect_size_distribution_line_width_", tab_id), label = "Line width", value = saved_options()[[1]]$distribution_line_width, step = 0.1)
          numericInput(inputId = paste0("effect_size_distribution_bandwith_adjustment_", tab_id), label = "Bandwith multiplier", value = saved_options()[[1]]$distribution_bandwith_adjustment, step = 0.1)
          checkboxInput(inputId = paste0("effect_size_line_per_group_", tab_id), label = "Show line connecting mean(s) to Y axis?", value = saved_options()[[1]]$effect_size_line_per_group)
          colourInput(inputId = paste0("effect_size_line_per_group_colour_", tab_id), label = "Line colour", value = saved_options()[[1]]$effect_size_line_per_group_colour)
          sliderInput(inputId = paste0("effect_size_line_per_group_transparency_", tab_id), label = "Transparency", min = 0, max = 1, value = saved_options()[[1]]$effect_size_line_per_group_transparency)
          selectInput(inputId = paste0("effect_size_line_per_group_type_", tab_id), label = "Line type", 
                      choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = saved_options()[[1]]$effect_size_line_per_group_type)
         colourInput(inputId = paste0("effect_size_mean_colour_", tab_id), label = "Dot colour", value = saved_options()[[1]]$mean_dot_colour)
         numericInput(inputId = paste0("effect_size_mean_dot_size_", tab_id), label = "Dot size", value = saved_options()[[1]]$mean_dot_size, step = 1)
         colourInput(inputId = paste0("effect_size_CI_colour_", tab_id), label = "Line colour", value = saved_options()[[1]]$CI_line_colour)
         numericInput(inputId = paste0("effect_size_CI_line_width_", tab_id), label = "Line width", value = saved_options()[[1]]$CI_line_width, step = 0.1)
         colourInput(inputId = paste0("effect_size_zero_colour_", tab_id), label = "Line colour", value = saved_options()[[1]]$zero_line_colour)
         selectInput(inputId = paste0("effect_size_zero_line_type_", tab_id), label = "Line type", 
                     choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = saved_options()[[1]]$zero_line_type)
         numericInput(inputId = paste0("effect_size_zero_line_width_", tab_id), label = "Line width", value = saved_options()[[1]]$zero_line_width, step = 0.1)
         })
       })
     })
      
      #List of future inputs to reset when pressing clear button (works for every plot)
      input_ids_to_reset <- reactive({
        # defines tab_id based on tab selected
        #Gets data for each selected tab
        if (input$navpage == 'Data Selection and Visualization' && input$tabselected =='Table & Graph') {
          tab_id = "table"
        } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)') {
          tab_id = "icc"
        } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
          tab_id = "icc_two"
        } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
          tab_id = "box_bar_scatter_violin"
        } else if (input$navpage == 'Superplot') {
          tab_id = "superplot"
        }
        
        list(
          #inputs for x axis
          paste0("x_line_thickness_", tab_id),
          paste0("x_line_colour_", tab_id),
          paste0("x_tick_thickness_", tab_id),
          paste0("x_tick_colour_", tab_id),
          paste0("x_tick_length_", tab_id),
          paste0("x_tick_minor_length_", tab_id),
          paste0("x_major_ticks_interval_", tab_id),
          paste0("x_minor_ticks_interval_", tab_id),
          paste0("x_text_size_", tab_id),
          paste0("x_text_colour_", tab_id),
          paste0("x_text_font_", tab_id),
          #inputs for y axis
          paste0("y_line_thickness_", tab_id),
          paste0("y_line_colour_", tab_id),
          paste0("y_tick_thickness_", tab_id),
          paste0("y_tick_colour_", tab_id),
          paste0("y_tick_length_", tab_id),
          paste0("y_tick_minor_length_", tab_id),
          # shows y axis major and minor ticks interval selection for linear scale
          paste0("y_major_ticks_interval_", tab_id),
          paste0("y_minor_ticks_interval_", tab_id), 
          paste0("y_text_size_", tab_id),
          paste0("y_text_colour_", tab_id),
          paste0("y_text_font_", tab_id),
          #"Scales Box",
          #enter numeric values for scales
          paste0("x_start_range_", tab_id),
          paste0("x_end_range_", tab_id),
          # adds error message if x scale selection is not appropriate
          paste0("error_message_x_scale_", tab_id),
          paste0("y_start_range_", tab_id),
          paste0("y_end_range_", tab_id),
          # adds error message if y scale selection is not appropriate
          paste0("error_message_y_scale_", tab_id),
          #buttons for type of scales X axis
          paste0("x_scale_type_", tab_id),
          #buttons for type of scales Y axis
          paste0("y_scale_type_", tab_id),
          #allows selection of log scale display
          paste0("numeric_display_type_y_axis_", tab_id),
          # input for scale breaks
          paste0("x_num_breaks_", tab_id),
          paste0("x_break_values_", tab_id),
          paste0("y_num_breaks_", tab_id),
          paste0("y_break_values_", tab_id),
          #"Labels and Titles Box"
          #x axis label
          paste0("x_axis_text_size_", tab_id),
          paste0("x_axis_text_colour_", tab_id),
          paste0("x_axis_text_face_", tab_id), 
          paste0("x_axis_text_font_", tab_id),
          paste0("x_axis_text_justification_", tab_id),
          paste0("x_axis_text_margin_", tab_id),
          paste0("x_axis_text_title_", tab_id),
          #y axis label
          paste0("y_axis_text_size_", tab_id),
          paste0("y_axis_text_colour_", tab_id),
          paste0("y_axis_text_face_", tab_id),
          paste0("y_axis_text_font_", tab_id),
          paste0("y_axis_text_justification_", tab_id),
          paste0("y_axis_text_margin_", tab_id),
          paste0("y_axis_text_title_", tab_id),
          #"Symbols"
          paste0("geom_jitter_colour_fill_by_group_", tab_id),
          paste0("symbol_gradient_error_", tab_id),
          #input for symbol colour
          paste0("color_var_", tab_id),
          paste0("colour_symbol_inputs_", tab_id),
          #inputs for gradient colour for border and fill
          # for border colour
          paste0("color_var_gradient_low_", tab_id),
          paste0("color_var_gradient_high_", tab_id),
          # input to change symbol color fill
          paste0("symbol_fill_", tab_id),
          paste0("colour_symbol_fill_inputs_", tab_id),
          #inputs for gradient colour for border and fill
          # for symbol fill
          paste0("symbol_fill_gradient_low_", tab_id),
          paste0("symbol_fill_gradient_high_", tab_id),
          #input to change symbol shape
          paste0("symbol_shape_", tab_id),
          paste0("symbol_size_", tab_id),
          #input to change symbol edge thickness
          paste0("symbol_edge_thickness_", tab_id),
          #inputs to change symbol jitter
          paste0("symbol_jitter_", tab_id),
          #slider for symbol transparency
          paste0("symbol_transparency_", tab_id),
          # for raincloud plot dots
          paste0("stat_dots_side_", tab_id),
          paste0("stat_dots_bindwidth_", tab_id),
          paste0("stat_dots_justification_", tab_id), 
          #adds geom_smooth if selected
          paste0("method_geom_smooth_scatter_", tab_id), 
          paste0("line_thickness_geom_smooth_scatter_", tab_id),
          paste0("line_type_geom_smooth_scatter_", tab_id),
          paste0("confidence_interval_geom_smooth_scatter_", tab_id),
          paste0("span_geom_smooth_scatter_", tab_id),
          paste0("transparency_geom_smooth_scatter_", tab_id),
          paste0("line_colour_geom_smooth_scatter_", tab_id),
          paste0("fill_colour_geom_smooth_scatter_", tab_id),
          #for box plots and bar chart
          paste0("geom_boxplot_colour_fill_by_group_", tab_id),
          #input for box line colour
          paste0("box_line_", tab_id),
          #UI input for colour boxplot selection based on grouping variable
          paste0("colour_boxplot_line_inputs_", tab_id),
          #input for box fill colour
          paste0("box_fill_", tab_id),
          #UI input for colour boxplot selection based on grouping variable
          paste0("colour_boxplot_fill_inputs_", tab_id),
          paste0("box_line_thickness_", tab_id),
          paste0("box_width_", tab_id),
          paste0("box_raincloud_width_", tab_id),
          paste0("box_transparency_", tab_id),
          paste0("sd_se_bar_chart_", tab_id),
          #options for customizing plot and panel background
          paste0("colour_background_", tab_id),
          paste0("colour_background_border_", tab_id),
          paste0("colour_background_border_thickness_", tab_id),
          paste0("colour_panel_", tab_id),
          paste0("colour_major_x_grid_", tab_id),
          paste0("thickness_major_x_grid_", tab_id),
          paste0("colour_minor_x_grid_", tab_id),
          paste0("thickness_minor_x_grid_", tab_id), 
          paste0("colour_major_y_grid_", tab_id),
          paste0("thickness_major_y_grid_", tab_id),
          paste0("colour_minor_y_grid_", tab_id),
          paste0("thickness_minor_y_grid_", tab_id),
          #options for customizing marginal Plots for Scatter
          paste0("maginal_plot_legend_", tab_id), 
          paste0("marginal_plot_legend_position_x_", tab_id),
          paste0("marginal_plot_legend_position_y_", tab_id),
          paste0("marginal_plot_legend_title_", tab_id), 
          paste0("marginal_plot_legend_text_size_", tab_id),
          paste0("marginal_plot_legend_text_face_", tab_id),
          paste0("marginal_plot_legend_text_font_", tab_id), 
          #for marginal plots and legend
          paste0("marginal_plot_type_", tab_id), 
          paste0("marginal_plot_margins_", tab_id), 
          paste0("marginal_plot_size_", tab_id),
          paste0("marginal_plot_legend_position_x_", tab_id), 
          paste0("marginal_plot_legend_position_y_", tab_id), 
          paste0("legends_selected_", tab_id),
          paste0("legends_outside_main_plot_", tab_id),
          paste0("marginal_plot_bindwidth_x_", tab_id), 
          paste0("marginal_plot_fill_colour_x_", tab_id),
          paste0("marginal_plot_line_width_x_", tab_id), 
          paste0("marginal_plot_transparency_x_", tab_id), 
          paste0("marginal_plot_bindwidth_y_", tab_id), 
          paste0("marginal_plot_border_colour_y_", tab_id), 
          paste0("marginal_plot_fill_colour_y_", tab_id), 
          paste0("marginal_plot_line_width_y_", tab_id), 
          paste0("marginal_plot_transparency_y_", tab_id),
          # for raincloud plot               
          paste0("raincloud_halfeye_width_", tab_id), 
          paste0("raincloud_halfeye_line_thickness_", tab_id), 
          paste0("raincloud_halfeye_justification_", tab_id), 
          paste0("raincloud_halfeye_transparency_", tab_id),
          # for effects sizes
          paste0("effect_size_distribution_fill_colour_", tab_id),
          paste0("effect_size_distribution_line_colour_", tab_id),
          paste0("effect_size_distribution_transparency_", tab_id),
          paste0("effect_size_distribution_line_type_", tab_id),
          paste0("effect_size_distribution_line_width_", tab_id),
          paste0("effect_size_distribution_bandwith_adjustment_", tab_id),
          paste0("effect_size_mean_colour_", tab_id), 
          paste0("effect_size_mean_dot_size_", tab_id),
          paste0("effect_size_CI_colour_", tab_id), 
          paste0("effect_size_CI_line_width_", tab_id), 
          paste0("effect_size_zero_colour_", tab_id), 
          paste0("effect_size_zero_line_type_", tab_id),
          paste0("effect_size_zero_line_width_", tab_id)
        )
      })
      
      # shows warning messages in modal applicable
      observeEvent(input[[paste0("update_options_", tab_id())]],{
        req(input$navpage != "App Overview" && input$navpage != "Developers & Contact")
        if (input$navpage == 'Data Selection and Visualization' && input$tabselected =='Table & Graph') {
          tab_id <- "table"
        } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)') {
          tab_id <- "icc"
        } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
          tab_id <- "icc_two"
        } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
          tab_id <- "box_bar_scatter_violin"
        } else if (input$navpage == 'Superplot'){
          tab_id <- "superplot"
        }
       
        excluded_inputs <- list(
          # x and y axes tick length
          paste0("x_tick_length_", tab_id),
          paste0("x_tick_minor_length_", tab_id),
          paste0("y_tick_length_", tab_id),
          paste0("y_tick_minor_length_", tab_id),
          paste0("x_start_range_", tab_id),
          paste0("x_end_range_", tab_id),
          paste0("y_start_range_", tab_id),
          paste0("y_end_range_", tab_id),
          paste0("x_axis_text_justification_", tab_id),
          paste0("x_axis_text_margin_", tab_id),
          paste0("x_axis_text_title_", tab_id),
          paste0("y_axis_text_justification_", tab_id),
          paste0("y_axis_text_margin_", tab_id),
          paste0("y_axis_text_title_", tab_id),
          paste0("transparency_geom_smooth_scatter_", tab_id),
          paste0("marginal_plot_legend_position_x_", tab_id), 
          paste0("marginal_plot_legend_position_y_", tab_id), 
          paste0("marginal_plot_transparency_x_", tab_id), 
          paste0("marginal_plot_transparency_y_", tab_id), 
          paste0("box_transparency_", tab_id),
          paste0("raincloud_halfeye_justification_", tab_id),
          paste0("effect_size_distribution_transparency_", tab_id)
        )
        for (input_id in input_ids_to_reset()) {
          if (!(input_id %in% excluded_inputs) && is.numeric(input[[input_id]]) && input[[input_id]] <= 0) {
            shinyFeedback::feedbackWarning(inputId = input_id, text = "Please select a larger number", show = TRUE)
          } else {
            hideFeedback(input_id)
          }
        }
      })
      
      # Create reactiveValues to store previous values of inputs for plot_all
      prev_input_values <- reactiveValues(
        #For lines and ticks 
          # X axis
          x_line_thickness = 1,
          x_line_colour = "black",
          x_tick_thickness = 1,
          x_tick_colour = "black",
          x_tick_length = 5,
          x_tick_minor_length = 3,
          x_major_ticks_interval = NULL,
          x_minor_ticks_interval = NULL,
          x_text_size = 12,
          x_text_colour = "black",
          x_text_font = "Arial",
          
          # Y axis
          y_line_thickness = 1,
          y_line_colour = "black",
          y_tick_thickness = 1,
          y_tick_colour = "black",
          y_tick_length = 5,
          y_tick_minor_length = 3,
          y_major_ticks_interval = NULL,
          y_minor_ticks_interval = NULL,
          y_text_size = 12,
          y_text_colour = "black",
          y_text_font = "Arial",
          
          #Scales Box
          x_start_range = NA,
          x_end_range = NA,
          y_start_range = NA,
          y_end_range = NA,
          x_scale_type = "linear",
          y_scale_type = "linear",
          numeric_display_type_y_axis = "Decimal",
          
          #Labels and Titles Box
          x_axis_text_size = 12,
          x_axis_text_colour = "black",
          x_axis_text_face = "plain",
          x_axis_text_font = "Arial",
          x_axis_text_justification = 0.5,
          x_axis_text_margin = 10,
          x_axis_text_title = "",
          y_axis_text_size = 12,
          y_axis_text_colour = "black",
          y_axis_text_face = "plain",
          y_axis_text_font = "Arial",
          y_axis_text_justification = 0.5,
          y_axis_text_margin = 10,
          y_axis_text_title = "",
          
          #Symbols Box
          geom_jitter_colour_fill_by_group = "Single Colour",
          color_var = "black",
          symbol_fill = "black",
          color_var_gradient_low = "blue",
          color_var_gradient_high = "white",
          symbol_fill_gradient_low = "blue",
          symbol_fill_gradient_high = "white",
          symbol_shape = 21,
          symbol_size = 1.5,
          symbol_edge_thickness = 1,
          symbol_jitter = 0.1,
          symbol_transparency = 1,
          
            # superplot symbols
            symbol_size_superplot = 5,
            symbol_edge_thickness_superplot = 1.5,
            symbol_jitter_superplot = 0.1,
            symbol_transparency_superplot = 1,
          
          #for Raincloud
          stat_dots_side = "left",
          stat_dots_bindwidth = 1,
          stat_dots_justification = 1.4,
          
          #Trendline
          method_geom_smooth_scatter = "loess",
          line_thickness_geom_smooth = 1,
          line_type_geom_smooth = "solid",
          confidence_interval_geom_smooth = 0.95,
          span_geom_smooth = 0.3,
          transparency_geom_smooth = 0.3,
          line_colour_geom_smooth = "purple",
          fill_colour_geom_smooth = "grey",
          
          #Correlation coefficient
          correlation_coefficient_show = TRUE,
          correlation_coefficient_method = "spearman",
          correlation_coefficient_name = "rho",
          correlation_coefficient_legend_position_x = "center",
          correlation_coefficient_legend_position_y = "center",
          correlation_coefficient_text_size = 4,
          correlation_coefficient_text_font = "Arial",
          
          
          #Legend
          maginal_plot_legend = "",
          marginal_plot_legend_position_x = 1,
          marginal_plot_legend_position_y = 1,
          marginal_plot_legend_title = TRUE,
          marginal_plot_legend_text_size = 12,
          marginal_plot_legend_text_face = "plain",
          marginal_plot_legend_text_font = "Arial",
          
          #Box and Bar-charts
          geom_boxplot_colour_fill_by_group = "Single Colour",
          box_line = "black",
          box_fill = "lightblue",
          box_line_thickness = 1,
          box_width = 0.8,
          box_raincloud_width = 0.2,
          box_transparency = 0.8,
          sd_se_bar_chart = "Standard deviation",
          
          #Background Box
          colour_background = "white",
          colour_background_border = "white",
          colour_background_border_thickness = 0.5,
          colour_panel = "white",
          colour_major_x_grid= "white",
          thickness_major_x_grid = 0.2,
          colour_minor_x_grid = "white",
          thickness_minor_x_grid = 0.1,
          colour_major_y_grid = "white",
          thickness_major_y_grid = 0.2,
          colour_minor_y_grid = "white",
          thickness_minor_y_grid = 0.1,
          
          #Legend
          marginal_plot_legend = "",
          marginal_plot_legend_position_x = 1,
          marginal_plot_legend_position_y = 1,
          marginal_plot_legend_title = TRUE,
          legends_outside_plot = FALSE,
          legends = c("Dots", "Box", "Violin", "Bar", "Cloud"),
          marginal_plot_legend_text_size = 12,
          marginal_plot_legend_text_face = "plain",
          marginal_plot_legend_text_font = "Arial",
          
          #Marginal plots
          marginal_plot_type = "density",
          marginal_plot_margins = "both",
          marginal_plot_size = 10,
          marginal_plot_bindwidth_x = 10,
          marginal_plot_border_colour_x = "black",
          marginal_plot_fill_colour_x = "lightgray",
          marginal_plot_line_width_x = 1,
          marginal_plot_transparency_x = 0.5,
          marginal_plot_bindwidth_y = 10,
          marginal_plot_border_colour_y = "black",
          marginal_plot_fill_colour_y = "lightgray",
          marginal_plot_line_width_y = 1,
          marginal_plot_transparency_y = 0.5,
          
          #Raincloud halfeye
          stat_dots_side = "left",
          stat_dots_binwidth = 2,
          stat_dots_justification = 1.4,
          raincloud_box_width = 0.2,
          raincloud_halfeye_width = 0.1,
          raincloud_halfeye_line_thickness = 0.1,
          raincloud_halfeye_justification = -0.3,
          raincloud_halfeye_transparency = 0.5,
          
          # Effect size
          distribution_fill_colour = "#FDC1C1",
          distribution_line_colour = "#FDC1C1",
          distribution_transparency = 0.5,
          distribution_line_type = "blank", 
          distribution_line_width = 0.5,
          distribution_bandwith_adjustment = 1,
          mean_dot_colour = "black",
          mean_dot_size = 3,
          CI_line_colour = "black",
          CI_line_width = 1,
          zero_line_colour = "black",
          zero_line_type = "dotted",
          zero_line_width = 0.5
          
      )

      # Selects RenderUI color for geom_jitter and geom_boxplot
      observe({
        req(input$navpage != "App Overview" && input$navpage != "Developers & Contact")
          if (input$navpage == 'Data Selection and Visualization' && input$tabselected =='Table & Graph') {
            tab_id = "table"
          } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)') {
            tab_id = "icc"
          } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
            tab_id = "icc_two"
          } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
            tab_id = "box_bar_scatter_violin"
          } else if (input$navpage == 'Superplot'){
            tab_id = "superplot"
          }
          
          # Gets UI output for symbol and box colour
          output[[paste0("colour_symbol_inputs_", tab_id)]] <- symbol_ui
          output[[paste0("colour_symbol_fill_inputs_", tab_id)]] <- symbol_fill_ui
          output[[paste0("colour_boxplot_line_inputs_", tab_id)]] <- box_border_ui
          output[[paste0("colour_boxplot_fill_inputs_", tab_id)]] <- box_fill_ui
          
          observeEvent(input[[paste0("reset_plot_", tab_id)]],{
            output[[paste0(tab_id, "_plot")]] = NULL
            shinyjs::hide(paste0("height_plot_", tab_id))
            shinyjs::hide(paste0("width_plot_", tab_id))
          })
      })
      
      #Listens to Update button clicks in order to update the modal
        # creates reactive val that detects the number of clicks for table tab
        click_count_table <- reactiveVal(0)
          click_count_table_plot_symbol_border <- reactiveVal(0)
          click_count_table_plot_symbol_fill <- reactiveVal(0)
          click_count_table_plot_box_border <- reactiveVal(0)
          click_count_table_plot_box_fill <- reactiveVal(0)
        
        # creates reactive val that detects the number of clicks for icc tab
        click_count_icc <- reactiveVal(0)
          click_count_icc_plot_symbol_border <- reactiveVal(0)
          click_count_icc_plot_symbol_fill <- reactiveVal(0)
          click_count_icc_plot_box_border <- reactiveVal(0)
          click_count_icc_plot_box_fill <- reactiveVal(0)
          
        # creates reactive val that detects the number of clicks for icc two tab
        click_count_icc_two <- reactiveVal(0)
          click_count_icc_two_plot_symbol_border <- reactiveVal(0)
          click_count_icc_two_plot_symbol_fill <- reactiveVal(0)
          click_count_icc_two_plot_box_border <- reactiveVal(0)
          click_count_icc_two_plot_box_fill <- reactiveVal(0)
          
        # creates reactive val that detects the number of clicks for box_bar_scatter_violin
        click_count_box_bar_scatter_violin <- reactiveVal(0)
          click_count_box_bar_scatter_violin_plot_symbol_border <- reactiveVal(0)
          click_count_box_bar_scatter_violin_plot_symbol_fill <- reactiveVal(0)
          click_count_box_bar_scatter_violin_plot_box_border <- reactiveVal(0)
          click_count_box_bar_scatter_violin_plot_box_fill <- reactiveVal(0)  
        
        # creates reactive val that detects the number of clicks for superplot
        click_count_superplot <- reactiveVal(0) 
          click_count_superplot_plot_symbol_border <- reactiveVal(0)
          click_count_superplot_plot_symbol_fill <- reactiveVal(0)
          click_count_superplot_plot_box_border <- reactiveVal(0)
          click_count_superplot_plot_box_fill <- reactiveVal(0)  
        
        #detects if more clicks have occurred when visualizing table tab
        observeEvent(input$update_options_table,{
          click_count_diff = input$update_options_table[1] - click_count_table()
          click_count_table(click_count_diff)
          
          click_diff_symbol_border = input$update_options_table[1] - click_count_table_plot_symbol_border()
          click_count_table_plot_symbol_border(click_diff_symbol_border)
          click_diff_symbol_fill = input$update_options_table[1] - click_count_table_plot_symbol_fill()
          click_count_table_plot_symbol_fill(click_diff_symbol_fill)
          click_diff_box_border = input$update_options_table[1] - click_count_table_plot_box_border()
          click_count_table_plot_box_border(click_diff_box_border)
          click_diff_box_fill = input$update_options_table[1] - click_count_table_plot_box_fill()
          click_count_table_plot_box_fill(click_diff_box_fill)
        })
        
        #detects if more clicks have occurred when visualizing icc tab
        observeEvent(input$update_options_icc,{
          click_count_diff = input$update_options_icc[1] - click_count_icc()
          click_count_icc(click_count_diff)
          
          click_diff_symbol_border = input$update_options_icc[1] - click_count_icc_plot_symbol_border()
          click_count_icc_plot_symbol_border(click_diff_symbol_border)
          click_diff_symbol_fill = input$update_options_icc[1] - click_count_icc_plot_symbol_fill()
          click_count_icc_plot_symbol_fill(click_diff_symbol_fill)
          click_diff_box_border = input$update_options_icc[1] - click_count_icc_plot_box_border()
          click_count_icc_plot_box_border(click_diff_box_border)
          click_diff_box_fill = input$update_options_icc[1] - click_count_icc_plot_box_fill()
          click_count_icc_plot_box_fill(click_diff_box_fill)
        })
        
        #detects if more clicks have occurred when visualizing icc_two tab
        observeEvent(input$update_options_icc_two,{
          click_count_diff = input$update_options_icc_two[1] - click_count_icc_two()
          click_count_icc_two(click_count_diff)
          
          click_diff_symbol_border = input$update_options_icc_two[1] - click_count_icc_two_plot_symbol_border()
          click_count_icc_two_plot_symbol_border(click_diff_symbol_border)
          click_diff_symbol_fill = input$update_options_icc_two[1] - click_count_icc_two_plot_symbol_fill()
          click_count_icc_two_plot_symbol_fill(click_diff_symbol_fill)
          click_diff_box_border = input$update_options_icc_two[1] - click_count_icc_two_plot_box_border()
          click_count_icc_two_plot_box_border(click_diff_box_border)
          click_diff_box_fill = input$update_options_icc_two[1] - click_count_icc_two_plot_box_fill()
          click_count_icc_two_plot_box_fill(click_diff_box_fill)
        })
        
        #detects if more clicks have occurred when visualizing box_bar_scatter_violin tab
        observeEvent(input$update_options_box_bar_scatter_violin,{
          click_count_diff = input$update_options_box_bar_scatter_violin[1] - click_count_box_bar_scatter_violin()
          click_count_box_bar_scatter_violin(click_count_diff)
          
          click_diff_symbol_border = input$update_options_box_bar_scatter_violin[1] - click_count_box_bar_scatter_violin_plot_symbol_border()
          click_count_box_bar_scatter_violin_plot_symbol_border(click_diff_symbol_border)
          click_diff_symbol_fill = input$update_options_box_bar_scatter_violin[1] - click_count_box_bar_scatter_violin_plot_symbol_fill()
          click_count_box_bar_scatter_violin_plot_symbol_fill(click_diff_symbol_fill)
          click_diff_box_border = input$update_options_box_bar_scatter_violin[1] - click_count_box_bar_scatter_violin_plot_box_border()
          click_count_box_bar_scatter_violin_plot_box_border(click_diff_box_border)
          click_diff_box_fill = input$update_options_box_bar_scatter_violin[1] - click_count_box_bar_scatter_violin_plot_box_fill()
          click_count_box_bar_scatter_violin_plot_box_fill(click_diff_box_fill)
        })
        
        #detects if more clicks have occurred when visualizing superplot tab
        observeEvent(input$update_options_superplot,{
          click_count_diff = input$update_options_superplot[1] - click_count_superplot()
          click_count_superplot(click_count_diff)
          
          click_diff_symbol_border = input$update_options_superplot[1] - click_count_superplot_plot_symbol_border()
          click_count_superplot_plot_symbol_border(click_diff_symbol_border)
          click_diff_symbol_fill = input$update_options_superplot[1] - click_count_superplot_plot_symbol_fill()
          click_count_superplot_plot_symbol_fill(click_diff_symbol_fill)
          click_diff_box_border = input$update_options_superplot[1] - click_count_superplot_plot_box_border()
          click_count_superplot_plot_box_border(click_diff_box_border)
          click_diff_box_fill = input$update_options_superplot[1] - click_count_superplot_plot_box_fill()
          click_count_superplot_plot_box_fill(click_diff_box_fill)
        })

      # Observe event for modal
      observe({
        req(input$modal_table == TRUE || input$modal_icc_two == TRUE || input$modal_icc == TRUE || input$modal_box_bar_scatter_violin == TRUE || input$modal_superplot == TRUE)
        
        # defines tab_id based on tab selected
        if (input$modal_table == TRUE) {
          tab_id = "table"
          data <- selected_vars_plot()
          x_var <- input$x_axis
          y_var <- input$y_axis
          z_var <- input$y_var_table
          
        } else if (input$modal_icc_two == TRUE) {
          tab_id = "icc_two"
          data <- data_icc()
          if (input$num_levels == 2) {
            x_var <- input$group_var_icc
            y_var <- input$y_var_icc
            z_var <- input$x_var_icc
          } else if (input$num_levels == 3){
            x_var <- input$group_var_icc
            y_var <- input$z_var_icc
            z_var <- input$y_var_icc
          } else if (input$num_levels == 4){
            x_var <- input$group_var_icc
            y_var <- input$w_var_icc
            z_var <- input$z_var_icc
          }
        } else if (input$modal_icc == TRUE) {
          tab_id = "icc"
          data <- data_icc()
          x_var <- input$x_var_icc
          y_var <- input$y_var_icc
          z_var <- input$group_var_icc
        } else if (input$modal_box_bar_scatter_violin == TRUE) {
          tab_id = "box_bar_scatter_violin"
          data <- data_plot()
          
          # names correctly based on number of levels
          if (input$num_levels_box_bar_scatter_violin == 1){
            x_var <- input$group_var_box_bar_scatter_violin
            y_var <- input$x_var_box_bar_scatter_violin
            z_var <- NULL
          }else if (input$num_levels_box_bar_scatter_violin == 2) {
            x_var <- input$group_var_box_bar_scatter_violin
            y_var <- input$y_var_box_bar_scatter_violin
            z_var <- input$x_var_box_bar_scatter_violin
          } else if (input$num_levels_box_bar_scatter_violin == 3){
            x_var <- input$group_var_box_bar_scatter_violin
            y_var <- input$z_var_box_bar_scatter_violin
            z_var <- input$y_var_box_bar_scatter_violin
          } else if (input$num_levels_box_bar_scatter_violin == 4){
            x_var <- input$group_var_box_bar_scatter_violin
            y_var <- input$w_var_box_bar_scatter_violin
            z_var <- input$z_var_box_bar_scatter_violin
          }
          # x_var <- input$group_var_box_bar_scatter_violin
          # y_var <- input$x_var_box_bar_scatter_violin
          # z_var <- input$y_var_box_bar_scatter_violin
        } else if (input$modal_superplot == TRUE) {
          tab_id = "superplot"
          data <- data_plot()
          x_var <- input$group_var_superplot
          y_var <- input$y_var_superplot
          z_var <- input$x_var_superplot
        }
        
        # for symbol border colour and fill error when variable is not continuous
        if ((input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]] == paste0(x_var, " ") && !is.numeric(data[[x_var]])) || (input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]] == paste0(z_var, " ") && !is.numeric(data[[z_var]]))){
          output[[paste0("symbol_gradient_error_", tab_id)]] <- renderText({"Discrete variable selected, cannot be used for gradient scale"})
        } else if (input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]] != paste0(x_var, " ") || input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]] != paste0(z_var, " ")){
          output[[paste0("symbol_gradient_error_", tab_id)]] <- renderText({NULL})
        }
        
          # for geom_jitter
          if (input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE){
            updateSelectInput(session, paste0("geom_jitter_colour_fill_by_group_", tab_id), choices = list("Single Colour", "Colour by Group - discrete" = c(x_var,""), "Colour by Group - gradient" = c(paste0(x_var, " "), "")), selected = input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]])
          } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE) {
            updateSelectInput(session, paste0("geom_jitter_colour_fill_by_group_", tab_id), choices = list("Single Colour", "Colour by Group - discrete" = c(x_var, z_var), "Colour by Group - gradient"= c(paste0(x_var, " "), paste0(z_var, " "))), selected = input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]])
          }
          
          #for geom_boxplot
          if (input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE){
            updateSelectInput(session, paste0("geom_boxplot_colour_fill_by_group_", tab_id), choices = list("Single Colour", "Colour by Group - discrete" = c(x_var,"")), selected = input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]])
          } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE) {
            updateSelectInput(session, paste0("geom_boxplot_colour_fill_by_group_", tab_id), choices = list("Single Colour", "Colour by Group - discrete" = c(x_var, z_var)), selected = input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]])
          }
        
        #For TabPanel Marginal Plots hide and show
        if (!is.null(input[[paste0("maginal_plot_", tab_id)]])){
          if (input[[paste0("maginal_plot_", tab_id)]] == TRUE){
            showTab(inputId = paste0("panels_options_", tab_id), target = "Marginal Plots")
          } else if (input[[paste0("maginal_plot_", tab_id)]] == FALSE){
            hideTab(inputId = paste0("panels_options_", tab_id), target = "Marginal Plots")
          } 
        }
        
        # for effect size hide and show for box, bar, scatter, violin and superplot
        if (input$number_effect_sizes_box_bar_scatter_violin >= 1 || input$number_effect_sizes_superplot >= 1){
          showTab(inputId = paste0("panels_options_", tab_id), target = "Effect size plots")
        } else if (input$number_effect_sizes_box_bar_scatter_violin == 0 || input$number_effect_sizes_superplot == 0) {
          hideTab(inputId = paste0("panels_options_", tab_id), target = "Effect size plots")
        }
        
        # for effect size hide when not in effect size navpages
        if (tab_id != "box_bar_scatter_violin" && tab_id != "superplot"){
          hideTab(inputId = paste0("panels_options_", tab_id), target = "Effect size plots")
        }
        
        # for raincloud plot tab show and hide
        if (input[[paste0("choose_", tab_id)]] != 'Raincloud'){
            hideTab(inputId = paste0("panels_options_", tab_id), target = "Raincloud halfeyes")
          } else if (input[[paste0("choose_", tab_id)]] == 'Raincloud'){
            showTab(inputId = paste0("panels_options_", tab_id), target = "Raincloud halfeyes")
          }
        

        #Switch between single colour or colour by group options for symbol
          if (input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]] != "Single Colour") {
            shinyjs::toggle(paste0("colour_symbol_inputs_", tab_id), condition = TRUE)
            shinyjs::toggle(paste0("color_var_", tab_id), condition = FALSE)
            shinyjs::toggle(paste0("colour_symbol_fill_inputs_", tab_id), condition = TRUE)
            shinyjs::toggle(paste0("symbol_fill_", tab_id), condition = FALSE)
          } else if (input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]] == "Single Colour") {
            shinyjs::toggle(paste0("colour_symbol_inputs_", tab_id), condition = FALSE)
            shinyjs::toggle(paste0("color_var_", tab_id), condition = TRUE)
            shinyjs::toggle(paste0("colour_symbol_fill_inputs_", tab_id), condition = FALSE)
            shinyjs::toggle(paste0("symbol_fill_", tab_id), condition = TRUE)
          }
        
        #Switch between single colour or colour by group options for box 
        if (input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]] != "Single Colour") {
          shinyjs::toggle(paste0("colour_boxplot_line_inputs_", tab_id), condition = TRUE)
          shinyjs::toggle(paste0("colour_boxplot_fill_inputs_", tab_id), condition = TRUE)
          shinyjs::toggle(paste0("box_line_", tab_id), condition = FALSE)
          shinyjs::toggle(paste0("box_fill_", tab_id), condition = FALSE)
        } else if (input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]] == "Single Colour") {
          shinyjs::toggle(paste0("colour_boxplot_line_inputs_", tab_id), condition = FALSE)
          shinyjs::toggle(paste0("colour_boxplot_fill_inputs_", tab_id), condition = FALSE)
          shinyjs::toggle(paste0("box_line_", tab_id), condition = TRUE)
          shinyjs::toggle(paste0("box_fill_", tab_id), condition = TRUE)
        }
        
        #Shows gradient options for symbol border and fill if applicable
        if ((input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]] == c(paste0(x_var, " ")) && is.numeric(data[[x_var]])) || (input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]] == c(paste0(z_var, " ")) && is.numeric(data[[z_var]]))) {
          shinyjs::toggle(paste0("color_var_gradient_low_", tab_id), condition = TRUE)
          shinyjs::toggle(paste0("color_var_gradient_high_", tab_id), condition = TRUE)
          shinyjs::toggle(paste0("symbol_fill_gradient_low_", tab_id), condition = TRUE)
          shinyjs::toggle(paste0("symbol_fill_gradient_high_", tab_id), condition = TRUE)
          
          shinyjs::toggle(paste0("colour_symbol_inputs_", tab_id), condition = FALSE)
          shinyjs::toggle(paste0("color_var_", tab_id), condition = FALSE)
          shinyjs::toggle(paste0("symbol_fill_", tab_id), condition = FALSE)
          shinyjs::toggle(paste0("colour_symbol_fill_inputs_", tab_id), condition = FALSE)
          output[[paste0("symbol_gradient_error_", tab_id)]] <- renderText({NULL})
        } else {
          shinyjs::toggle(paste0("color_var_gradient_low_", tab_id), condition = FALSE)
          shinyjs::toggle(paste0("color_var_gradient_high_", tab_id), condition = FALSE)
          shinyjs::toggle(paste0("symbol_fill_gradient_low_", tab_id), condition = FALSE)
          shinyjs::toggle(paste0("symbol_fill_gradient_high_", tab_id), condition = FALSE)
        }
        
        # allows mean dot modal selection only for superplot
          if (tab_id != "superplot"){
            shinyjs::hide(paste0("symbol_size_superplot_", tab_id))
            shinyjs::hide(paste0("symbol_edge_thickness_superplot_", tab_id))
            shinyjs::hide(paste0("symbol_jitter_superplot_", tab_id))
            shinyjs::hide(paste0("symbol_transparency_superplot_", tab_id))
          }
       
        # adjust shows legend option display for specific plot
        shinyjs::hide(paste0("legends_selected_", tab_id))
        
        if (input[[paste0("maginal_plot_legend_", tab_id)]] != "none"){
          shinyjs::show(paste0("legends_selected_", tab_id))
          if (input[[paste0("choose_", tab_id)]] == "Scatter"){
            updateCheckboxGroupInput(session, paste0("legends_selected_", tab_id), label = "Select legends to show", choices = c("Dots"), selected = input[[paste0("legends_selected_", tab_id)]])
          } else if (input[[paste0("choose_", tab_id)]] == "Box-plot") {
            updateCheckboxGroupInput(session, paste0("legends_selected_", tab_id), label = "Select legends to show", choices = c("Dots", "Box"), selected = input[[paste0("legends_selected_", tab_id)]])
          } else if (input[[paste0("choose_", tab_id)]] == "Bar") {
            updateCheckboxGroupInput(session, paste0("legends_selected_", tab_id), label = "Select legends to show", choices = c("Dots", "Bar"), selected = input[[paste0("legends_selected_", tab_id)]])
          } else if (input[[paste0("choose_", tab_id)]] == "Violin") {
            updateCheckboxGroupInput(session, paste0("legends_selected_", tab_id), label = "Select legends to show", choices = c("Dots", "Violin"), selected = input[[paste0("legends_selected_", tab_id)]])
          } else if (input[[paste0("choose_", tab_id)]] == "Raincloud") {
            updateCheckboxGroupInput(session, paste0("legends_selected_", tab_id), label = "Select legends to show", choices = c("Dots", "Box", "Cloud"), selected = input[[paste0("legends_selected_", tab_id)]])
          }
        }
        
        
        # hides legend outside plot option (shows only for effect size plots)
        hide(paste0("legend_outside_main_plot_", tab_id))
        
        if ((tab_id == "Superplot" && !is.null(superplot_with_effect_size_plot())) || (tab_id == "box_bar_scatter_violin" && !is.null(bar_box_scatter_violin_with_effect_size_plot()))){
          show(paste0("legend_outside_main_plot_", tab_id))
        }
        
      })
      
      # Create a reactiveValues object to store selected colors
        # for table panel
        selected_colors_table <- reactiveValues()
        selected_colours_symbol_fill_table <- reactiveValues()
        selected_colors_boxplot_line_table <- reactiveValues()
        selected_colors_boxplot_fill_table <- reactiveValues()
        
        # for icc panel
        selected_colors_icc <- reactiveValues()
        selected_colours_symbol_fill_icc <- reactiveValues()
        selected_colors_boxplot_line_icc <- reactiveValues()
        selected_colors_boxplot_fill_icc <- reactiveValues()
        
        # for icc_two panel
        selected_colors_icc_two <- reactiveValues()
        selected_colours_symbol_fill_icc_two <- reactiveValues()
        selected_colors_boxplot_line_icc_two <- reactiveValues()
        selected_colors_boxplot_fill_icc_two <- reactiveValues()
        
        # for box_bar_scatter_violin
        selected_colors_box_bar_scatter_violin <- reactiveValues()
        selected_colours_symbol_fill_box_bar_scatter_violin <- reactiveValues()
        selected_colors_boxplot_line_box_bar_scatter_violin <- reactiveValues()
        selected_colors_boxplot_fill_box_bar_scatter_violin <- reactiveValues()
        
        # for superplot
        selected_colors_superplot <- reactiveValues()
        selected_colours_symbol_fill_superplot <- reactiveValues()
        selected_colors_boxplot_line_superplot <- reactiveValues()
        selected_colors_boxplot_fill_superplot <- reactiveValues()
        
      ##NEW Render UI for colour symbol selection for each group_var
        symbol_ui <- renderUI({
          req((input$geom_jitter_colour_fill_by_group_table != "Single Colour" || input$geom_jitter_colour_fill_by_group_icc != "Single Colour" || input$geom_jitter_colour_fill_by_group_icc_two != "Single Colour" || input$geom_jitter_colour_fill_by_group_box_bar_scatter_violin != "Single Colour" || input$geom_jitter_colour_fill_by_group_superplot != "Single Colour"))
          
          #Gets plots and tab information based on selection
          if (input$navpage == 'Data Selection and Visualization' && input$tabselected =='Table & Graph') {
            tab_id <- "table"
            if (!is.ggplot(plot_for_all_table())){
              plot_selected <- plot_for_all_table()[[1]]
            } else {
              plot_selected <- plot_for_all_table()    
            }
            #plot_selected <- plot_for_all_table()
            
          } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)') {
            tab_id <- "icc"
            if (!is.ggplot(plot_for_all_icc())){
              plot_selected <- plot_for_all_icc()[[1]]
            } else {
              plot_selected <- plot_for_all_icc()    
            }
            #plot_selected <- plot_for_all_icc()
            
          } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
            tab_id <- "icc_two"
            if (!is.ggplot(plot_for_all_icc_two())){
              plot_selected <- plot_for_all_icc_two()[[1]]
            } else {
              plot_selected <- plot_for_all_icc_two()    
            }
            #plot_selected <- plot_for_all_icc_two()
            
          } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
            tab_id <- "box_bar_scatter_violin"
            if (!is.ggplot(plot_for_all_box_bar_scatter_violin())){
              plot_selected <- plot_for_all_box_bar_scatter_violin()[[1]]
            } else {
              plot_selected <- plot_for_all_box_bar_scatter_violin()    
            }
            #plot_selected <- plot_for_all_box_bar_scatter_violin()
            
          } else if (input$navpage == 'Superplot') {
            tab_id <- "superplot"
            plot_selected <- plot_for_all_superplot()
          }
          
        # gets grouping variables from plots
        # for plots that are not lists ##is ggplot might be ideal###
        if (is.ggplot(plot_selected)) {
          plot_data <- plot_selected %>% pluck("data")
          group_var_levels <- unique(plot_data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]])
          # pluck does not recognize the order of factor variables if one or more are in capital letters. code below fixes that
          if (!is.numeric(group_var_levels)) {
            plot_data <- ggplot_build(plot_selected)
            group_var_levels <- plot_data$layout$panel_params[[1]]$x$limits
            # corrects group if additional group is called
            if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE && !is.null(unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]]))){
              # Corrects for wrong order in ICC tab
              if (input$navpage != 'Intraclass correlation & Linear Mixed Models'){
                # trims selected variable if it does not exist (avoids errors when switching between discrete and continuous variables)
                if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]]))){
                  group_var_levels <- unique(plot_data$plot$data[[str_trim(input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]])]])
                }else if ("levels" %in% names(attributes(unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]])))){
                  group_var_levels <- levels(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]])
                } else {
                  group_var_levels <- unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]])
                }
              } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models'){
                if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]]))){
                  group_var_levels <- levels(unique(plot_data$plot$data[[str_trim(input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]])]]))
                } else {
                  group_var_levels <- levels(unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]]))
                }
              }


            }
          }
        } else {
          # for plots within lists
          # Check if 'levels' exists 
          # if additional grouping is present
          if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE){
            if ("levels" %in% names(attributes(plot_selected[[3]]))) {
              # if 'levels' exists
              group_var_levels <- levels(plot_selected[[3]])
            } else {
              # if 'levels' does not exist
              group_var_levels <- unique(plot_selected[[3]])
            }
            # if additional grouping is NOT present
          } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE)
            if ("levels" %in% names(attributes(plot_selected[[2]]))) {
              # if 'levels' exists
              group_var_levels <- levels(plot_selected[[2]])
            } else {
              # if 'levels' does not exist
              group_var_levels <- unique(plot_selected[[2]])
            }
        }
        
        # Initialize additional colors if needed
        if (!is.null(group_var_levels) && length(group_var_levels) > 0) {
          num_colors_to_add <- length(group_var_levels) - length(get(paste0("selected_colors_", tab_id))$colors)
          
          # Initialize additional colors if needed
          if (num_colors_to_add > 0) {
            additional_colors <- rep("black", num_colors_to_add)
            # fills gaps with black colors
            eval(parse(text = paste0("selected_colors_", tab_id, "$colors <- c(get(paste0('selected_colors_', tab_id))$colors, additional_colors)")))
            
            # if (tab_id == "table") {
            #   selected_colors_table$colors <- c(get(paste0("selected_colors_", tab_id))$colors, additional_colors)
            # } else if (tab_id == "icc") {
            #   selected_colors_icc$colors <- c(get(paste0("selected_colors_", tab_id))$colors, additional_colors)
            # } else if (tab_id == "icc_two"){
            #   selected_colors_icc_two$colors <- c(get(paste0("selected_colors_", tab_id))$colors, additional_colors)
            # } else if (tab_id == "box_bar_scatter_violin"){
            #   selected_colors_box_bar_scatter_violin$colors <- c(get(paste0("selected_colors_", tab_id))$colors, additional_colors)
            # }
            # 
            
            # Reset input values
            for (i in (length(get(paste0("selected_colors_", tab_id))$colors) - num_colors_to_add + 1):length(get(paste0("selected_colors_", tab_id))$colors)) {
              updateColourInput(session, paste0("color_var_", tab_id, i), value = "black")
            }
          }
          
          # generates the UI for colour selection
          colour_inputs <- lapply(1:length(group_var_levels), function(i) {
            colourInput(inputId = paste0("color_var_", tab_id, i), label = paste("Select border color for", group_var_levels[i]), value = get(paste0("selected_colors_", tab_id))$colors[i])
          })
          
          # adds default colour if no colour is applied
          colour_inputs <- lapply(colour_inputs, function(input_element) {
            input_element$children[[2]]$attribs$`data-init-value` <- lapply(
              input_element$children[[2]]$attribs$`data-init-value`,
              function(value) if (is.na(value)) "black" else value
            )
            return(input_element)
          })
          
          #adds observe for updating variables
          observe({
            req(!is.null(plot_selected) && input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]] != "Single Colour" && eval(parse(text = paste0("click_count_", tab_id, "_plot_symbol_border()"))) > 0 && input[[paste0("update_options_", tab_id)]])
            
            
            ##gets group_vars again (avoids UI rendering error and color update)
            # gets grouping variables from plots
            # for plots that are not lists ##is ggplot might be ideal###
            if (is.ggplot(plot_selected)) {
              plot_data <- plot_selected %>% pluck("data")
              group_var_levels <- unique(plot_data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]])
              # pluck does not recognize the order of factor variables if one or more are in capital letters. code below fixes that
              if (!is.numeric(group_var_levels)) {
                plot_data <- ggplot_build(plot_selected)
                group_var_levels <- plot_data$layout$panel_params[[1]]$x$limits
                # corrects group if additional group is called
                if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE){
                  # Corrects for wrong order in ICC tab
                  if (input$navpage != 'Intraclass correlation & Linear Mixed Models'){
                    # trims selected variable if it does not exist (avoids errors when switching between discrete and continuous variables)
                    if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]]))){
                      group_var_levels <- unique(plot_data$plot$data[[str_trim(input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]])]])
                    }else {
                      group_var_levels <- unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]])
                    }
                  } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models'){
                    if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]]))){
                      group_var_levels <- levels(unique(plot_data$plot$data[[str_trim(input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]])]]))
                    }else {
                      group_var_levels <- levels(unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]]))
                    }
                  }
                }
              }
            } else {
              # for plots within lists
              # Check if 'levels' exists 
              # if additional grouping is present
              if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE){
                if ("levels" %in% names(attributes(plot_selected[[3]]))) {
                  # if 'levels' exists
                  group_var_levels <- levels(plot_selected[[3]])
                } else {
                  # if 'levels' does not exist
                  group_var_levels <- unique(plot_selected[[3]])
                }
                # if additional grouping is NOT present
              } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE)
                if ("levels" %in% names(attributes(plot_selected[[2]]))) {
                  # if 'levels' exists
                  group_var_levels <- levels(plot_selected[[2]])
                } else {
                  # if 'levels' does not exist
                  group_var_levels <- unique(plot_selected[[2]])
                }
            }
            
            if (!is.null(group_var_levels)) {
              # gets grouping variables from plot
              for (i in 1:length(group_var_levels)) {
                if (is.null(input[[paste0("color_var_", tab_id, i)]]) || (length(get(paste0("selected_colors_", tab_id))$colors) < length(group_var_levels))) {
                  #eval(parse(text = paste0("selected_colors_", tab_id, "$colors[[", i, "]] <- '", input[[paste0("color_var_", tab_id, i)]], "'")))
                  #eval(parse(text = paste0("selected_colors_", tab_id, "$colors[[", i, "]] <- 'black'")))
                  #eval(parse(text = paste0("get(selected_colors_", tab_id, ")$colors[[", i, "]] <- '", "black"[i], "'")))
                  eval(parse(text = paste0("selected_colors_", tab_id, "$colors[[", i, "]] <- 'black'")))
                  
                    
                } else if (!is.null(input[[paste0("color_var_", tab_id, i)]])) {
                  eval(parse(text = paste0("selected_colors_", tab_id, "$colors[[", i, "]] <- '", input[[paste0("color_var_", tab_id, i)]], "'")))
                  #get(paste0("selected_colors_", tab_id))$colors[[i]] <- input[[paste0("color_var_", tab_id, i)]]
                }
              }
              #resets click count
              if (input$navpage == 'Data Selection and Visualization' && input$tabselected =='Table & Graph') {
                click_count_table_plot_symbol_border(0)
              } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
                click_count_icc_two_plot_symbol_border(0)
              } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)'){
                click_count_icc_plot_symbol_border(0)
              } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud'){
                click_count_box_bar_scatter_violin_plot_symbol_border(0)
              } else if (input$navpage == 'Superplot') {
                click_count_superplot_plot_symbol_border(0)
              }
            }
          }) 
          
          #calls colour variables for UI
          do.call(tagList, c(colour_inputs))
        }
      })
        
        ##Render UI for colour symbol fill selection for each group_var
        symbol_fill_ui <- renderUI({
          req((input$geom_jitter_colour_fill_by_group_table != "Single Colour" || input$geom_jitter_colour_fill_by_group_icc != "Single Colour" || input$geom_jitter_colour_fill_by_group_icc_two != "Single Colour" || input$geom_jitter_colour_fill_by_group_box_bar_scatter_violin != "Single Colour" || input$geom_jitter_colour_fill_by_group_superplot != "Single Colour"))

          #Gets plots and tab information based on selection
          if (input$navpage == 'Data Selection and Visualization' && input$tabselected =='Table & Graph') {
            tab_id <- "table"
            if (!is.ggplot(plot_for_all_table())){
              plot_selected <- plot_for_all_table()[[1]]
            } else {
              plot_selected <- plot_for_all_table()    
            }
            
          } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)') {
            tab_id <- "icc"
            if (!is.ggplot(plot_for_all_icc())){
              plot_selected <- plot_for_all_icc()[[1]]
            } else {
              plot_selected <- plot_for_all_icc()    
            }
            
            #plot_selected <- plot_for_all_icc()
            
          } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
            tab_id <- "icc_two"
            if (!is.ggplot(plot_for_all_icc_two())){
              plot_selected <- plot_for_all_icc_two()[[1]]
            } else {
              plot_selected <- plot_for_all_icc_two()    
            }
            #plot_selected <- plot_for_all_icc_two()
            
          } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
            tab_id <- "box_bar_scatter_violin"
            if (!is.ggplot(plot_for_all_box_bar_scatter_violin())){
              plot_selected <- plot_for_all_box_bar_scatter_violin()[[1]]
            } else {
              plot_selected <- plot_for_all_box_bar_scatter_violin()    
            }
            #plot_selected <- plot_for_all_box_bar_scatter_violin()
            
          } else if (input$navpage == 'Superplot') {
            tab_id <- "superplot"
            plot_selected <- plot_for_all_superplot()
          }         
          
          # gets grouping variables from plots
          # for plots that are not lists ##is ggplot might be ideal###
          if (is.ggplot(plot_selected)) {
            plot_data <- plot_selected %>% pluck("data")
            group_var_levels <- unique(plot_data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]])
            # pluck does not recognize the order of factor variables if one or more are in capital letters. code below fixes that
            if (!is.numeric(group_var_levels)) {
              plot_data <- ggplot_build(plot_selected)
              group_var_levels <- plot_data$layout$panel_params[[1]]$x$limits
              # corrects group if additional group is called
              if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE && !is.null(unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]]))){
                # Corrects for wrong order in ICC tab
                if (input$navpage != 'Intraclass correlation & Linear Mixed Models'){
                  # trims selected variable if it does not exist (avoids errors when switching between discrete and continuous variables)
                  if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]]))){
                    group_var_levels <- unique(plot_data$plot$data[[str_trim(input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]])]])
                  }else if ("levels" %in% names(attributes(unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]])))){
                    group_var_levels <- levels(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]])
                  } else {
                    group_var_levels <- unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]])
                  }
                } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models'){
                  if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]]))){
                    group_var_levels <- levels(unique(plot_data$plot$data[[str_trim(input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]])]]))
                  } else {
                    group_var_levels <- levels(unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]]))
                  }
                }
                
                
              }
            }
          } else {
            # for plots within lists
            # Check if 'levels' exists 
            # if additional grouping is present
            if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE){
              if ("levels" %in% names(attributes(plot_selected[[3]]))) {
                # if 'levels' exists
                group_var_levels <- levels(plot_selected[[3]])
              } else {
                # if 'levels' does not exist
                group_var_levels <- unique(plot_selected[[3]])
              }
              # if additional grouping is NOT present
            } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE)
              if ("levels" %in% names(attributes(plot_selected[[2]]))) {
                # if 'levels' exists
                group_var_levels <- levels(plot_selected[[2]])
              } else {
                # if 'levels' does not exist
                group_var_levels <- unique(plot_selected[[2]])
              }
          }
          
          # Initialize additional colors if needed
          if (!is.null(group_var_levels) && length(group_var_levels) > 0) {
            num_colors_to_add <- length(group_var_levels) - length(get(paste0("selected_colours_symbol_fill_", tab_id))$colors)
            
            # Initialize additional colors if needed
            if (num_colors_to_add > 0) {
              additional_colors <- rep("black", num_colors_to_add)
              # fills gaps with black colors
              eval(parse(text = paste0("selected_colours_symbol_fill_", tab_id, "$colors <- c(get(paste0('selected_colours_symbol_fill_', tab_id))$colors, additional_colors)")))
              
              # if (tab_id == "table") {
              #   selected_colours_symbol_fill_table$colors <- c(get(paste0("selected_colours_symbol_fill_", tab_id))$colors, additional_colors)
              # } else if (tab_id == "icc") {
              #   selected_colours_symbol_fill_icc$colors <- c(get(paste0("selected_colours_symbol_fill_", tab_id))$colors, additional_colors)
              # } else if (tab_id == "icc_two"){
              #   selected_colours_symbol_fill_icc_two$colors <- c(get(paste0("selected_colours_symbol_fill_", tab_id))$colors, additional_colors)
              # } else if (tab_id == "box_bar_scatter_violin"){
              #   selected_colours_symbol_fill_box_bar_scatter_violin$colors <- c(get(paste0("selected_colours_symbol_fill_", tab_id))$colors, additional_colors)
              # }

              # Reset input values
              for (i in (length(get(paste0("selected_colours_symbol_fill_", tab_id))$colors) - num_colors_to_add + 1):length(get(paste0("selected_colours_symbol_fill_", tab_id))$colors)) {
                updateColourInput(session, paste0("symbol_fill_", tab_id, i), value = "black")
              }
            }
            
            # generates the UI for colour selection
            colour_inputs_symbol_fill <- lapply(1:length(group_var_levels), function(i) {
              colourInput(inputId = paste0("symbol_fill_", tab_id, i), label = paste("Select fill color for", group_var_levels[i]), value = get(paste0("selected_colours_symbol_fill_", tab_id))$colors[i])
            })
            
            # adds default colour if no colour is applied
            colour_inputs_symbol_fill <- lapply(colour_inputs_symbol_fill, function(input_element) {
              input_element$children[[2]]$attribs$`data-init-value` <- lapply(
                input_element$children[[2]]$attribs$`data-init-value`,
                function(value) if (is.na(value)) "black" else value
              )
              return(input_element)
            })
            
            #adds observe for updating variables
            observe({
              req(!is.null(plot_selected) && input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]] != "Single Colour" && eval(parse(text = paste0("click_count_", tab_id, "_plot_symbol_fill()"))) > 0 && input[[paste0("update_options_", tab_id)]])
              
              #gets group_var again (avoids error when rendering UI and updating colours)
              # gets grouping variables from plots
              # for plots that are not lists ##is ggplot might be ideal###
              if (is.ggplot(plot_selected)) {
                plot_data <- plot_selected %>% pluck("data")
                group_var_levels <- unique(plot_data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]])
                # pluck does not recognize the order of factor variables if one or more are in capital letters. code below fixes that
                if (!is.numeric(group_var_levels)) {
                  plot_data <- ggplot_build(plot_selected)
                  group_var_levels <- plot_data$layout$panel_params[[1]]$x$limits
                  # corrects group if additional group is called
                  if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE){
                    # trims selected variable if it does not exist (avoids errors when switching between discrete and continuous variables)
                    if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]]))){
                      group_var_levels <- unique(plot_data$plot$data[[str_trim(input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]])]])
                    }else {
                      group_var_levels <- unique(plot_data$plot$data[[input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]]])
                    }
                  }
                }
              } else {
                # for plots within lists
                # Check if 'levels' exists 
                # if additional grouping is present
                if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE){
                  if ("levels" %in% names(attributes(plot_selected[[3]]))) {
                    # if 'levels' exists
                    group_var_levels <- levels(plot_selected[[3]])
                  } else {
                    # if 'levels' does not exist
                    group_var_levels <- unique(plot_selected[[3]])
                  }
                  # if additional grouping is NOT present
                } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE)
                  if ("levels" %in% names(attributes(plot_selected[[2]]))) {
                    # if 'levels' exists
                    group_var_levels <- levels(plot_selected[[2]])
                  } else {
                    # if 'levels' does not exist
                    group_var_levels <- unique(plot_selected[[2]])
                  }
              }
              
              if (!is.null(group_var_levels)) {
                for (i in 1:length(group_var_levels)) {
                  if (is.null(input[[paste0("symbol_fill_", tab_id, i)]]) || (length(get(paste0("selected_colours_symbol_fill_", tab_id))$colors) < length(group_var_levels))) {
                    #get(paste0("selected_colours_symbol_fill_", tab_id))$colors[[i]] <- "black"[i]
                    #eval(parse(text = paste0("get(selected_colours_symbol_fill_", tab_id, ")$colors[[", i, "]] <- '", "black"[i], "'")))
                    eval(parse(text = paste0("selected_colours_symbol_fill_", tab_id, "$colors[[", i, "]] <- 'black'")))
                    
                    
                  } else if (!is.null(input[[paste0("symbol_fill_", tab_id, i)]])) {
                    #get(paste0("selected_colours_symbol_fill_", tab_id))$colors[[i]] <- input[[paste0("symbol_fill_", tab_id, i)]]
                    eval(parse(text = paste0("selected_colours_symbol_fill_", tab_id, "$colors[[", i, "]] <- '", input[[paste0("symbol_fill_", tab_id, i)]], "'")))
                  }
                }
                #resets click count
                if (input$navpage == 'Data Selection and Visualization' && input$tabselected =='Table & Graph') {
                  click_count_table_plot_symbol_fill(0)
                } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
                  click_count_icc_two_plot_symbol_fill(0)
                } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)'){
                  click_count_icc_plot_symbol_fill(0)
                } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
                  click_count_box_bar_scatter_violin_plot_symbol_fill(0)
                } else if (input$navpage == 'Superplot'){
                  click_count_superplot_plot_symbol_fill(0)
                }
              }
              
            })
            
            #calls colour variables for UI
            do.call(tagList, c(colour_inputs_symbol_fill))
          }
        })
        
        ##Render UI for colour boxplot line selection for each group_var
        box_border_ui <- renderUI({
          req((input$geom_boxplot_colour_fill_by_group_table != "Single Colour" || input$geom_boxplot_colour_fill_by_group_icc != "Single Colour" || input$geom_boxplot_colour_fill_by_group_icc_two != "Single Colour" || input$geom_boxplot_colour_fill_by_group_box_bar_scatter_violin != "Single Colour" || input$geom_boxplot_colour_fill_by_group_superplot != "Single Colour"))
          # !is.null(plot_for_all()) & 
          #Gets plots and tab information based on selection
          if (input$navpage == 'Data Selection and Visualization' && input$tabselected =='Table & Graph') {
            tab_id <- "table"
            if (!is.ggplot(plot_for_all_table())){
              plot_selected <- plot_for_all_table()[[1]]
            } else {
              plot_selected <- plot_for_all_table()    
            }
            
          } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)') {
            tab_id <- "icc"
            if (!is.ggplot(plot_for_all_icc())){
              plot_selected <- plot_for_all_icc()[[1]]
            } else {
              plot_selected <- plot_for_all_icc()    
            }
            
            #plot_selected <- plot_for_all_icc()
            
          } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
            tab_id <- "icc_two"
            if (!is.ggplot(plot_for_all_icc_two())){
              plot_selected <- plot_for_all_icc_two()[[1]]
            } else {
              plot_selected <- plot_for_all_icc_two()    
            }
            #plot_selected <- plot_for_all_icc_two()
            
          } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
            tab_id <- "box_bar_scatter_violin"
            if (!is.ggplot(plot_for_all_box_bar_scatter_violin())){
              plot_selected <- plot_for_all_box_bar_scatter_violin()[[1]]
            } else {
              plot_selected <- plot_for_all_box_bar_scatter_violin()    
            }
            #plot_selected <- plot_for_all_box_bar_scatter_violin()
            
          } else if (input$navpage == 'Superplot') {
            tab_id <- "superplot"
            plot_selected <- plot_for_all_superplot()
          }         
          
          # gets grouping variables from plots
          # for plots that are not lists ##is ggplot might be ideal###
          if (is.ggplot(plot_selected)) {
            plot_data <- plot_selected %>% pluck("data")
            group_var_levels <- unique(plot_data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])
            # pluck does not recognize the order of factor variables if one or more are in capital letters. code below fixes that
            if (!is.numeric(group_var_levels)) {
              plot_data <- ggplot_build(plot_selected)
              group_var_levels <- plot_data$layout$panel_params[[1]]$x$limits
              # corrects group if additional group is called
              if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE && !is.null(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))){
                # Corrects for wrong order in ICC tab
                if (input$navpage != 'Intraclass correlation & Linear Mixed Models'){
                  # trims selected variable if it does not exist (avoids errors when switching between discrete and continuous variables)
                  if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))){
                    group_var_levels <- unique(plot_data$plot$data[[str_trim(input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]])]])
                  }else if ("levels" %in% names(attributes(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])))){
                    group_var_levels <- levels(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])
                  } else {
                    group_var_levels <- unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])
                  }
                } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models'){
                  if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))){
                    group_var_levels <- levels(unique(plot_data$plot$data[[str_trim(input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]])]]))
                  } else {
                    group_var_levels <- levels(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))
                  }
                }
                
                
              }
            }
          } else {
            # for plots within lists
            # Check if 'levels' exists 
            # if additional grouping is present
            if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE){
              if ("levels" %in% names(attributes(plot_selected[[3]]))) {
                # if 'levels' exists
                group_var_levels <- levels(plot_selected[[3]])
              } else {
                # if 'levels' does not exist
                group_var_levels <- unique(plot_selected[[3]])
              }
              # if additional grouping is NOT present
            } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE)
              if ("levels" %in% names(attributes(plot_selected[[2]]))) {
                # if 'levels' exists
                group_var_levels <- levels(plot_selected[[2]])
              } else {
                # if 'levels' does not exist
                group_var_levels <- unique(plot_selected[[2]])
              }
          }
          
          # Initialize additional colors if needed
          if (!is.null(group_var_levels) && length(group_var_levels) > 0) {
            num_colors_to_add <- length(group_var_levels) - length(get(paste0("selected_colors_boxplot_line_", tab_id))$colors)
            
            # Initialize additional colors if needed
            if (num_colors_to_add > 0) {
              additional_colors <- rep("black", num_colors_to_add)
              eval(parse(text = paste0("selected_colors_boxplot_line_", tab_id, "$colors <- c(get(paste0('selected_colors_boxplot_line_', tab_id))$colors, additional_colors)")))
              
              # Reset input values
              for (i in (length(get(paste0("selected_colors_boxplot_line_", tab_id))$colors) - num_colors_to_add + 1):length(get(paste0("selected_colors_boxplot_line_", tab_id))$colors)) {
                updateColourInput(session, paste0("box_line_", tab_id, i), value = "black")
              }
            }
            
             #generates the UI for colour selection
            colour_inputs_boxplot_line <- lapply(1:length(group_var_levels), function(i) {
              colourInput(inputId = paste0("box_line_", tab_id, i), label = paste("Select border color for", group_var_levels[i]), value =  get(paste0("selected_colors_boxplot_line_", tab_id))$colors[i])
            })
            
            # adds default colour if no colour is applied
            colour_inputs_boxplot_line <- lapply(colour_inputs_boxplot_line, function(input_element) {
              input_element$children[[2]]$attribs$`data-init-value` <- lapply(
                input_element$children[[2]]$attribs$`data-init-value`,
                function(value) if (is.na(value)) "black" else value
              )
              return(input_element)
            })
            
            #adds observe for updating variables
            observe({
              req(input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]] != "Single Colour" && eval(parse(text = paste0("click_count_", tab_id, "_plot_box_border()"))) > 0 && input[[paste0("update_options_", tab_id)]])
              # !is.null(plot_for_all()) & 
              
              # gets grouping variables from plots
              # for plots that are not lists ##is ggplot might be ideal###
              if (is.ggplot(plot_selected)) {
                plot_data <- plot_selected %>% pluck("data")
                group_var_levels <- unique(plot_data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])
                # pluck does not recognize the order of factor variables if one or more are in capital letters. code below fixes that
                if (!is.numeric(group_var_levels)) {
                  plot_data <- ggplot_build(plot_selected)
                  group_var_levels <- plot_data$layout$panel_params[[1]]$x$limits
                  # corrects group if additional group is called
                  if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE && !is.null(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))){
                    # Corrects for wrong order in ICC tab
                    if (input$navpage != 'Intraclass correlation & Linear Mixed Models'){
                      # trims selected variable if it does not exist (avoids errors when switching between discrete and continuous variables)
                      if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))){
                        group_var_levels <- unique(plot_data$plot$data[[str_trim(input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]])]])
                      }else if ("levels" %in% names(attributes(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])))){
                        group_var_levels <- levels(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])
                      } else {
                        group_var_levels <- unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])
                      }
                    } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models'){
                      if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))){
                        group_var_levels <- levels(unique(plot_data$plot$data[[str_trim(input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]])]]))
                      } else {
                        group_var_levels <- levels(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))
                      }
                    }
                    
                    
                  }
                }
              } else {
                # for plots within lists
                # Check if 'levels' exists 
                # if additional grouping is present
                if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE){
                  if ("levels" %in% names(attributes(plot_selected[[3]]))) {
                    # if 'levels' exists
                    group_var_levels <- levels(plot_selected[[3]])
                  } else {
                    # if 'levels' does not exist
                    group_var_levels <- unique(plot_selected[[3]])
                  }
                  # if additional grouping is NOT present
                } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE)
                  if ("levels" %in% names(attributes(plot_selected[[2]]))) {
                    # if 'levels' exists
                    group_var_levels <- levels(plot_selected[[2]])
                  } else {
                    # if 'levels' does not exist
                    group_var_levels <- unique(plot_selected[[2]])
                  }
              }
              
              if (!is.null(group_var_levels)) {
                for (i in 1:length(group_var_levels)) {
                  if (is.null(input[[paste0("box_line_", tab_id, i)]]) || (length(get(paste0("selected_colors_boxplot_line_", tab_id))$colors) < length(group_var_levels))) {
                   # eval(parse(text = paste0("get(selected_colors_boxplot_line_", tab_id, ")$colors[[", i, "]] <- '", "black"[i], "'")))
                    #selected_colors_boxplot_line$colors[[i]] <- "black"[i]
                    eval(parse(text = paste0("selected_colors_boxplot_line_", tab_id, "$colors[[", i, "]] <- 'black'")))
                    
                    
                  } else if (!is.null(input[[paste0("box_line_", tab_id, i)]])) {
                    #selected_colors_boxplot_line$colors[[i]] <- input[[paste0("box_line_", tab_id, i)]]
                    eval(parse(text = paste0("selected_colors_boxplot_line_", tab_id, "$colors[[", i, "]] <- '", input[[paste0("box_line_", tab_id, i)]], "'")))
                  }
                }
                #resets click count
                if (input$navpage == 'Data Selection and Visualization') {
                  click_count_table_plot_box_border(0)
                } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
                  click_count_icc_two_plot_box_border(0)
                } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)'){
                  click_count_icc_plot_box_border(0)
                } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
                  click_count_box_bar_scatter_violin_plot_box_border(0)
                } else if (input$nvapage == 'Superplot') {
                  click_count_superplot_plot_box_border(0)
                }
              }
            })
            
            #calls colour variables for UI
            do.call(tagList, c(colour_inputs_boxplot_line))
          }
        })
        
        ##Render UI for colour boxplot fill selection for each group_var
        box_fill_ui <- renderUI({
          req((input$geom_boxplot_colour_fill_by_group_table != "Single Colour" || input$geom_boxplot_colour_fill_by_group_icc != "Single Colour" || input$geom_boxplot_colour_fill_by_group_icc_two != "Single Colour" || input$geom_boxplot_colour_fill_by_group_box_bar_scatter_violin != "Single Colour" || input$geom_boxplot_colour_fill_by_group_superplot != "Single Colour"))
          # !is.null(plot_for_all()) & 
          #Gets plots and tab information based on selection
          if (input$navpage == 'Data Selection and Visualization' && input$tabselected =='Table & Graph') {
            tab_id <- "table"
            if (!is.ggplot(plot_for_all_table())){
              plot_selected <- plot_for_all_table()[[1]]
            } else {
              plot_selected <- plot_for_all_table()    
            }
            
          } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)') {
            tab_id <- "icc"
            if (!is.ggplot(plot_for_all_icc())){
              plot_selected <- plot_for_all_icc()[[1]]
            } else {
              plot_selected <- plot_for_all_icc()    
            }
            
            #plot_selected <- plot_for_all_icc()
            
          } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
            tab_id <- "icc_two"
            if (!is.ggplot(plot_for_all_icc_two())){
              plot_selected <- plot_for_all_icc_two()[[1]]
            } else {
              plot_selected <- plot_for_all_icc_two()    
            }
            #plot_selected <- plot_for_all_icc_two()
            
          } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
            tab_id <- "box_bar_scatter_violin"
            if (!is.ggplot(plot_for_all_box_bar_scatter_violin())){
              plot_selected <- plot_for_all_box_bar_scatter_violin()[[1]]
            } else {
              plot_selected <- plot_for_all_box_bar_scatter_violin()    
            }
            #plot_selected <- plot_for_all_box_bar_scatter_violin()
            
          } else if (input$navpage == 'Superplot') {
            tab_id <- "superplot"
            plot_selected <- plot_for_all_superplot()
          }            
          
          # gets grouping variables from plots
          # for plots that are not lists ##is ggplot might be ideal###
          if (is.ggplot(plot_selected)) {
            plot_data <- plot_selected %>% pluck("data")
            group_var_levels <- unique(plot_data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])
            # pluck does not recognize the order of factor variables if one or more are in capital letters. code below fixes that
            if (!is.numeric(group_var_levels)) {
              plot_data <- ggplot_build(plot_selected)
              group_var_levels <- plot_data$layout$panel_params[[1]]$x$limits
              # corrects group if additional group is called
              if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE && !is.null(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))){
                # Corrects for wrong order in ICC tab
                if (input$navpage != 'Intraclass correlation & Linear Mixed Models'){
                  # trims selected variable if it does not exist (avoids errors when switching between discrete and continuous variables)
                  if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))){
                    group_var_levels <- unique(plot_data$plot$data[[str_trim(input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]])]])
                  }else if ("levels" %in% names(attributes(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])))){
                    group_var_levels <- levels(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])
                  } else {
                    group_var_levels <- unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])
                  }
                } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models'){
                  if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))){
                    group_var_levels <- levels(unique(plot_data$plot$data[[str_trim(input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]])]]))
                  } else {
                    group_var_levels <- levels(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))
                  }
                }
                
                
              }
            }
          } else {
            # for plots within lists
            # Check if 'levels' exists 
            # if additional grouping is present
            if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE){
              if ("levels" %in% names(attributes(plot_selected[[3]]))) {
                # if 'levels' exists
                group_var_levels <- levels(plot_selected[[3]])
              } else {
                # if 'levels' does not exist
                group_var_levels <- unique(plot_selected[[3]])
              }
              # if additional grouping is NOT present
            } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE)
              if ("levels" %in% names(attributes(plot_selected[[2]]))) {
                # if 'levels' exists
                group_var_levels <- levels(plot_selected[[2]])
              } else {
                # if 'levels' does not exist
                group_var_levels <- unique(plot_selected[[2]])
              }
          }
          
          # Initialize additional colors if needed
          if (!is.null(group_var_levels) && length(group_var_levels) > 0) {
            num_colors_to_add <- length(group_var_levels) - length(get(paste0("selected_colors_boxplot_fill_", tab_id))$colors)
            
            # Initialize additional colors if needed
            if (num_colors_to_add > 0) {
              additional_colors <- rep("lightblue", num_colors_to_add)
              eval(parse(text = paste0("selected_colors_boxplot_fill_", tab_id, "$colors <- c(get(paste0('selected_colors_boxplot_fill_', tab_id))$colors, additional_colors)")))
              #selected_colors_boxplot_fill$colors <- c( selected_colors_boxplot_fill$colors, additional_colors)
              
              # Reset input values
              for (i in (length(get(paste0("selected_colors_boxplot_fill_", tab_id))$colors) - num_colors_to_add + 1):length(get(paste0("selected_colors_boxplot_fill_", tab_id))$colors)) {
                updateColourInput(session, paste0("box_fill_", tab_id, i), value = "lightblue")
              }
            }
            
            # generates the UI for colour selection
            colour_inputs_boxplot_fill <- lapply(1:length(group_var_levels), function(i) {
              colourInput(inputId = paste0("box_fill_", tab_id, i), label = paste("Select fill color for", group_var_levels[i]), value =  get(paste0("selected_colors_boxplot_fill_", tab_id))$colors[i])
            })
            
            # adds default colour if no colour is applied
            colour_inputs_boxplot_fill <- lapply(colour_inputs_boxplot_fill, function(input_element) {
              input_element$children[[2]]$attribs$`data-init-value` <- lapply(
                input_element$children[[2]]$attribs$`data-init-value`,
                function(value) if (is.na(value)) "lightblue" else value
              )
              return(input_element)
            })
            
            #adds observe for updating variables
            observe({
              req(input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]] != "Single Colour" && eval(parse(text = paste0("click_count_", tab_id, "_plot_box_fill()"))) > 0 && input[[paste0("update_options_", tab_id)]])
              #!is.null(plot_for_all()) & 
              #gets group_var again (avoids error when rendering UI and updating colours)
              # gets grouping variables from plots
              # for plots that are not lists ##is ggplot might be ideal###
              if (is.ggplot(plot_selected)) {
                plot_data <- plot_selected %>% pluck("data")
                group_var_levels <- unique(plot_data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])
                # pluck does not recognize the order of factor variables if one or more are in capital letters. code below fixes that
                if (!is.numeric(group_var_levels)) {
                  plot_data <- ggplot_build(plot_selected)
                  group_var_levels <- plot_data$layout$panel_params[[1]]$x$limits
                  # corrects group if additional group is called
                  if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE && !is.null(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))){
                    # Corrects for wrong order in ICC tab
                    if (input$navpage != 'Intraclass correlation & Linear Mixed Models'){
                      # trims selected variable if it does not exist (avoids errors when switching between discrete and continuous variables)
                      if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))){
                        group_var_levels <- unique(plot_data$plot$data[[str_trim(input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]])]])
                      }else if ("levels" %in% names(attributes(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])))){
                        group_var_levels <- levels(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])
                      } else {
                        group_var_levels <- unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]])
                      }
                    } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models'){
                      if (is.null(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))){
                        group_var_levels <- levels(unique(plot_data$plot$data[[str_trim(input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]])]]))
                      } else {
                        group_var_levels <- levels(unique(plot_data$plot$data[[input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]]]))
                      }
                    }
                  }
                }
              } else {
                # for plots within lists
                # Check if 'levels' exists 
                # if additional grouping is present
                if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE){
                  if ("levels" %in% names(attributes(plot_selected[[3]]))) {
                    # if 'levels' exists
                    group_var_levels <- levels(plot_selected[[3]])
                  } else {
                    # if 'levels' does not exist
                    group_var_levels <- unique(plot_selected[[3]])
                  }
                  # if additional grouping is NOT present
                } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE)
                  if ("levels" %in% names(attributes(plot_selected[[2]]))) {
                    # if 'levels' exists
                    group_var_levels <- levels(plot_selected[[2]])
                  } else {
                    # if 'levels' does not exist
                    group_var_levels <- unique(plot_selected[[2]])
                  }
              }
              
              if (!is.null(group_var_levels)) {
                for (i in 1:length(group_var_levels)) {
                  if (is.null(input[[paste0("box_fill_", tab_id, i)]]) || (length(get(paste0("selected_colors_boxplot_fill_", tab_id))$colors) < length(group_var_levels))) {
                    #selected_colors_boxplot_fill$colors[[i]] <- "lightblue"[i]
                    #eval(parse(text = paste0("get(selected_colors_boxplot_fill_", tab_id, ")$colors[[", i, "]] <- '", "lightblue"[i], "'")))
                    eval(parse(text = paste0("selected_colors_boxplot_line_", tab_id, "$colors[[", i, "]] <- 'lightblue'")))
                    
                  } else if (!is.null(input[[paste0("box_fill_", tab_id, i)]])) {
                    eval(parse(text = paste0("selected_colors_boxplot_fill_", tab_id, "$colors[[", i, "]] <- '", input[[paste0("box_fill_", tab_id, i)]], "'")))
                    #selected_colors_boxplot_fill$colors[[i]] <- input[[paste0("box_fill_", tab_id, i)]]
                  }
                }
                #resets click count
                if (input$navpage == 'Data Selection and Visualization' && input$tabselected =='Table & Graph') {
                  click_count_table_plot_box_fill(0)
                } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
                  click_count_icc_two_plot_box_fill(0)
                } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)'){
                  click_count_icc_plot_box_fill(0)
                } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud'){
                  click_count_box_bar_scatter_violin_plot_box_fill(0)
                } else if (input$navpage == 'Superplot'){
                  click_count_superplot_plot_box_fill(0)
                }
              }
            })
            
            #calls colour variables for UI
            do.call(tagList, c(colour_inputs_boxplot_fill))
          }
        })
        
        ####################################
        ##### TRYING NEW STUFF############# 
        
      # creates variables to stores tab_id info, data and names of variables
      tab_id <- reactiveVal()
      
      # enables Plot button for table plot when applicable
      observeEvent(c(selected_vars_plot(),input$x_axis, input$y_axis, input$y_var_table, input$update_options_table, input$show_additional_group_legend_table),{
        
        # for geom_jitter
        if (input$show_additional_group_legend_table == FALSE && (input$geom_jitter_colour_fill_by_group_table == input$y_var_table || input$geom_jitter_colour_fill_by_group_table == paste0(input$y_var_table, " "))){
           updateSelectInput(session, "geom_jitter_colour_fill_by_group_table", selected = "Single Colour")
        }
        
        # for box_plot
        if (input$show_additional_group_legend_table == FALSE && (input$geom_boxplot_colour_fill_by_group_table == input$y_var_table)){
          updateSelectInput(session, "geom_boxplot_colour_fill_by_group_table", selected = "Single Colour")
        }
        
        shinyjs::enable("submit_plot_table")
      })
      
      # attributes correct tab_id, data and names of variables
      observeEvent(c(input$navpage, input$tabselected2),{
        if (input$navpage == 'Data Selection and Visualization' && input$tabselected =='Table & Graph') {
          tab_id("table")
        } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)') {
          tab_id("icc")
        } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
          tab_id("icc_two")
        } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
          tab_id("box_bar_scatter_violin")
        } else if (input$navpage == 'Superplot'){
          tab_id("superplot")
        } else {
          tab_id("table")
        }
      })
      
      
      #Generates plot that can be used for several graphs
      observeEvent(input[[paste0("submit_plot_", tab_id())]],{
        req(input$navpage != "App Overview" && input$navpage != "Developers & Contact")
        if (input$navpage == 'Data Selection and Visualization' && input$tabselected =='Table & Graph') {
          tab_id <- "table"
          data <- selected_vars_plot()
          x_var <- input$x_axis
          y_var <- input$y_axis
          z_var <- input$y_var_table
          
        } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Intraclass Correlation Analysis (2 Levels)') {
          tab_id <- "icc"
          data <- selected_vars_plot_icc()
          if (input$num_levels >= 2) {
            x_var <- input$x_var_icc
            y_var <- input$y_var_icc
            z_var <- input$group_var_icc
          }
        } else if (input$navpage == 'Intraclass correlation & Linear Mixed Models' && input$tabselected2 == 'Linear Mixed Model') {
          tab_id <- "icc_two"
          data <- selected_vars_plot_icc()
          shinyjs::hide("show_additional_group_legend_icc_two")
          if (input$num_levels == 2) {
            x_var <- input$group_var_icc
            y_var <- input$y_var_icc
            z_var <- input$x_var_icc
          } else if (input$num_levels == 3){
            x_var <- input$group_var_icc
            y_var <- input$z_var_icc
            z_var <- input$y_var_icc
            w_var <- input$x_var_icc
          } else if (input$num_levels == 4){
            x_var <- input$group_var_icc
            y_var <- input$w_var_icc
            z_var <- input$z_var_icc
            w_var <- input$x_var_icc
            extra_var <- input$y_var_icc
          }
          
        } else if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud') {
          tab_id <- "box_bar_scatter_violin"
          data <- data_plot()
          # names correctly based on number of levels
          if (input$num_levels_box_bar_scatter_violin == 1){
            x_var <- input$group_var_box_bar_scatter_violin
            y_var <- input$x_var_box_bar_scatter_violin
            z_var <- "£$%^&*"
          }else if (input$num_levels_box_bar_scatter_violin == 2) {
            x_var <- input$group_var_box_bar_scatter_violin
            y_var <- input$y_var_box_bar_scatter_violin
            z_var <- input$x_var_box_bar_scatter_violin
          } else if (input$num_levels_box_bar_scatter_violin == 3){
            x_var <- input$group_var_box_bar_scatter_violin
            y_var <- input$z_var_box_bar_scatter_violin
            z_var <- input$y_var_box_bar_scatter_violin
          } else if (input$num_levels_box_bar_scatter_violin == 4){
            x_var <- input$group_var_box_bar_scatter_violin
            y_var <- input$w_var_box_bar_scatter_violin
            z_var <- input$z_var_box_bar_scatter_violin
          }
        } else if (input$navpage == 'Superplot'){
          tab_id <- "superplot"
          data <- data_plot()
          x_var <- input$group_var_superplot
          y_var <- input$y_var_superplot
          z_var <- input$x_var_superplot
        }
        
        #shinyjs::disable("submit_plot_table")
        
        
        tryCatch({
        #adjust selection for icc navpage when plot is first generated
          if (tab_id == "icc" && input$submit_plot_icc[1] == 0){
          prev_input_values$geom_jitter_colour_fill_by_group = input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]
          prev_input_values$maginal_plot_legend = input[[paste0("maginal_plot_legend_", tab_id)]]
        } else if (input$navpage == 'Superplot' && input$submit_plot_superplot[1] == 0){
          prev_input_values$geom_jitter_colour_fill_by_group = input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]
          prev_input_values$maginal_plot_legend = input[[paste0("maginal_plot_legend_", tab_id)]]
        }
          observeEvent(input$group_var_icc,{
            prev_input_values$geom_jitter_colour_fill_by_group = input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]
            prev_input_values$maginal_plot_legend = input[[paste0("maginal_plot_legend_", tab_id)]]
          })
        }, error = function(e) {
          # Print or log the error message
          cat("Error Message:", conditionMessage(e), "\n")
           #Print the call stack trace
        })
        
        # Checks if update click happened when modal is open
        if (eval(parse(text = paste0("click_count_", tab_id, "()"))) > 0 && input[[paste0("modal_", tab_id)]] == TRUE) {  
          prev_input_values$x_line_thickness = input[[paste0("x_line_thickness_", tab_id)]]
          prev_input_values$x_line_colour = input[[paste0("x_line_colour_", tab_id)]]
          prev_input_values$x_tick_thickness = input[[paste0("x_tick_thickness_", tab_id)]]
          prev_input_values$x_tick_colour = input[[paste0("x_tick_colour_", tab_id)]]
          prev_input_values$x_tick_length = input[[paste0("x_tick_length_", tab_id)]]
          prev_input_values$x_tick_minor_length = input[[paste0("x_tick_minor_length_", tab_id)]]
          prev_input_values$x_major_ticks_interval = input[[paste0("x_major_ticks_interval_", tab_id)]]
          prev_input_values$x_minor_ticks_interval = input[[paste0("x_minor_ticks_interval_", tab_id)]]
          prev_input_values$x_text_size = input[[paste0("x_text_size_", tab_id)]]
          prev_input_values$x_text_colour = input[[paste0("x_text_colour_", tab_id)]]
          prev_input_values$x_text_font = as.character(input[[paste0("x_text_font_", tab_id)]])
          
          # Y axis
          prev_input_values$y_line_thickness = input[[paste0("y_line_thickness_", tab_id)]]
          prev_input_values$y_line_colour = input[[paste0("y_line_colour_", tab_id)]]
          prev_input_values$y_tick_thickness = input[[paste0("y_tick_thickness_", tab_id)]]
          prev_input_values$y_tick_colour = input[[paste0("y_tick_colour_", tab_id)]]
          prev_input_values$y_tick_length = input[[paste0("y_tick_length_", tab_id)]]
          prev_input_values$y_tick_minor_length = input[[paste0("y_tick_minor_length_", tab_id)]]
          prev_input_values$y_major_ticks_interval = input[[paste0("y_major_ticks_interval_", tab_id)]]
          prev_input_values$y_minor_ticks_interval = input[[paste0("y_minor_ticks_interval_", tab_id)]]
          prev_input_values$y_text_size = input[[paste0("y_text_size_", tab_id)]]
          prev_input_values$y_text_colour = input[[paste0("y_text_colour_", tab_id)]]
          prev_input_values$y_text_font = as.character(input[[paste0("y_text_font_", tab_id)]])
          
          #Scales Box
          prev_input_values$x_start_range = input[[paste0("x_start_range_", tab_id)]]
          prev_input_values$x_end_range = input[[paste0("x_end_range_", tab_id)]]
          prev_input_values$y_start_range = input[[paste0("y_start_range_", tab_id)]]
          prev_input_values$y_end_range = input[[paste0("y_end_range_", tab_id)]]
          prev_input_values$x_scale_type = input[[paste0("x_scale_type_", tab_id)]]
          prev_input_values$y_scale_type = input[[paste0("y_scale_type_", tab_id)]]
          prev_input_values$numeric_display_type_y_axis = input[[paste0("numeric_display_type_y_axis_", tab_id)]]
          ## add scale breaks###
          ######################
          
          #Labels and Titles Box
          prev_input_values$x_axis_text_size = input[[paste0("x_axis_text_size_", tab_id)]]
          prev_input_values$x_axis_text_colour = input[[paste0("x_axis_text_colour_", tab_id)]]
          prev_input_values$x_axis_text_face = input[[paste0("x_axis_text_face_", tab_id)]]
          prev_input_values$x_axis_text_font = input[[paste0("x_axis_text_font_", tab_id)]]
          prev_input_values$x_axis_text_justification = input[[paste0("x_axis_text_justification_", tab_id)]]
          prev_input_values$x_axis_text_margin = input[[paste0("x_axis_text_margin_", tab_id)]]
          prev_input_values$x_axis_text_title = input[[paste0("x_axis_text_title_", tab_id)]]
          prev_input_values$y_axis_text_size = input[[paste0("y_axis_text_size_", tab_id)]]
          prev_input_values$y_axis_text_colour = input[[paste0("y_axis_text_colour_", tab_id)]]
          prev_input_values$y_axis_text_face = input[[paste0("y_axis_text_face_", tab_id)]]
          prev_input_values$y_axis_text_font = input[[paste0("y_axis_text_font_", tab_id)]]
          prev_input_values$y_axis_text_justification = input[[paste0("y_axis_text_justification_", tab_id)]]
          prev_input_values$y_axis_text_margin = input[[paste0("y_axis_text_margin_", tab_id)]]
          prev_input_values$y_axis_text_title = input[[paste0("y_axis_text_title_", tab_id)]]
          
          #Symbols Box
          prev_input_values$geom_jitter_colour_fill_by_group = input[[paste0("geom_jitter_colour_fill_by_group_", tab_id)]]
          prev_input_values$color_var = input[[paste0("color_var_", tab_id)]]
          prev_input_values$symbol_fill = input[[paste0("symbol_fill_", tab_id)]]
          prev_input_values$color_var_gradient_low = input[[paste0("color_var_gradient_low_", tab_id)]]
          prev_input_values$color_var_gradient_high = input[[paste0("color_var_gradient_high_", tab_id)]]
          prev_input_values$symbol_fill_gradient_low = input[[paste0("symbol_fill_gradient_low_", tab_id)]]
          prev_input_values$symbol_fill_gradient_high = input[[paste0("symbol_fill_gradient_high_", tab_id)]]
          
          prev_input_values$symbol_shape = input[[paste0("symbol_shape_", tab_id)]]
          prev_input_values$symbol_size = input[[paste0("symbol_size_", tab_id)]]
          prev_input_values$symbol_edge_thickness = input[[paste0("symbol_edge_thickness_", tab_id)]]
          prev_input_values$symbol_jitter = input[[paste0("symbol_jitter_", tab_id)]]
          prev_input_values$symbol_transparency = input[[paste0("symbol_transparency_", tab_id)]]
          
          # superplot symbols
          prev_input_values$symbol_size_superplot = input[[paste0("symbol_size_superplot_", tab_id)]]
          prev_input_values$symbol_edge_thickness_superplot = input[[paste0("symbol_edge_thickness_superplot_", tab_id)]]
          prev_input_values$symbol_jitter_superplot = input[[paste0("symbol_jitter_superplot_", tab_id)]]
          prev_input_values$symbol_transparency_superplot = input[[paste0("symbol_transparency_superplot_", tab_id)]]
          
          #for Raincloud
          prev_input_values$stat_dots_side = input[[paste0("stat_dots_side_", tab_id)]]
          prev_input_values$stat_dots_bindwidth = input[[paste0("stat_dots_bandwidth_", tab_id)]]
          prev_input_values$stat_dots_justification = input[[paste0("stat_dots_justification_", tab_id)]]
          
          #Trendline
          prev_input_values$method_geom_smooth_scatter = input[[paste0("method_geom_smooth_scatter_", tab_id)]]
          prev_input_values$line_thickness_geom_smooth = input[[paste0("line_thickness_geom_smooth_scatter_", tab_id)]]
          prev_input_values$line_type_geom_smooth = input[[paste0("line_type_geom_smooth_scatter_", tab_id)]]
          prev_input_values$confidence_interval_geom_smooth = input[[paste0("confidence_interval_geom_smooth_scatter_", tab_id)]]
          prev_input_values$span_geom_smooth = input[[paste0("span_geom_smooth_scatter_", tab_id)]]
          prev_input_values$transparency_geom_smooth = input[[paste0("transparency_geom_smooth_scatter_", tab_id)]]
          prev_input_values$line_colour_geom_smooth = input[[paste0("line_colour_geom_smooth_scatter_", tab_id)]]
          prev_input_values$fill_colour_geom_smooth = input[[paste0("fill_colour_geom_smooth_scatter_", tab_id)]]
          
          #Correlation coefficient
          prev_input_values$correlation_coefficient_show = input[[paste0("correlation_coefficient_show_legend_", tab_id)]]
          prev_input_values$correlation_coefficient_method = input[[paste0("correlation_coefficient_method_", tab_id)]]
          prev_input_values$correlation_coefficient_name = input[[paste0("correlation_coefficient_name_", tab_id)]]
          prev_input_values$correlation_coefficient_legend_position_x = input[[paste0("correlation_coefficient_legend_position_x_", tab_id)]]
          prev_input_values$correlation_coefficient_legend_position_y = input[[paste0("correlation_coefficient_legend_position_y_", tab_id)]]
          prev_input_values$correlation_coefficient_text_size = input[[paste0("correlation_coefficient_text_size_", tab_id)]]
          prev_input_values$correlation_coefficient_text_font = input[[paste0("correlation_coefficient_text_font_", tab_id)]]
          
          #Box and Bar-charts
          prev_input_values$geom_boxplot_colour_fill_by_group = input[[paste0("geom_boxplot_colour_fill_by_group_", tab_id)]]
          prev_input_values$box_line = input[[paste0("box_line_", tab_id)]]
          prev_input_values$box_fill = input[[paste0("box_fill_", tab_id)]]
          prev_input_values$box_line_thickness = input[[paste0("box_line_thickness_", tab_id)]]
          prev_input_values$box_width = input[[paste0("box_width_", tab_id)]]
          prev_input_values$box_raincloud_width = input[[paste0("box_raincloud_width_", tab_id)]]
          prev_input_values$box_transparency = input[[paste0("box_transparency_", tab_id)]]
          prev_input_values$sd_se_bar_chart = input[[paste0("sd_se_bar_chart_", tab_id)]]
          
          #Background Box
          prev_input_values$colour_background = input[[paste0("colour_background_", tab_id)]]
          prev_input_values$colour_background_border = input[[paste0("colour_background_border_", tab_id)]]
          prev_input_values$colour_background_border_thickness = input[[paste0("colour_background_border_thickness_", tab_id)]]
          prev_input_values$colour_panel = input[[paste0("colour_panel_", tab_id)]]
          prev_input_values$colour_major_x_grid = input[[paste0("colour_major_x_grid_", tab_id)]]
          prev_input_values$thickness_major_x_grid = input[[paste0("thickness_major_x_grid_", tab_id)]]
          prev_input_values$colour_minor_x_grid = input[[paste0("colour_minor_x_grid_", tab_id)]]
          prev_input_values$thickness_minor_x_grid = input[[paste0("thickness_minor_x_grid_", tab_id)]]
          prev_input_values$colour_major_y_grid = input[[paste0("colour_major_y_grid_", tab_id)]]
          prev_input_values$thickness_major_y_grid = input[[paste0("thickness_major_y_grid_", tab_id)]]
          prev_input_values$colour_minor_y_grid = input[[paste0("colour_minor_y_grid_", tab_id)]]
          prev_input_values$thickness_minor_y_grid = input[[paste0("thickness_minor_y_grid_", tab_id)]]
          
          #Legend
          prev_input_values$legends_outside_plot = input[[paste0("legend_outside_main_plot_", tab_id)]]
          prev_input_values$legends = input[[paste0("legends_selected_", tab_id)]]
          prev_input_values$maginal_plot_legend = input[[paste0("maginal_plot_legend_", tab_id)]]
          prev_input_values$marginal_plot_legend_position_x = input[[paste0("marginal_plot_legend_position_x_", tab_id)]]
          prev_input_values$marginal_plot_legend_position_y = input[[paste0("marginal_plot_legend_position_y_", tab_id)]]
          prev_input_values$marginal_plot_legend_title = input[[paste0("marginal_plot_legend_title_", tab_id)]]
          prev_input_values$marginal_plot_legend_text_size = input[[paste0("marginal_plot_legend_text_size_", tab_id)]]
          prev_input_values$marginal_plot_legend_text_face = input[[paste0("marginal_plot_legend_text_face_", tab_id)]]
          prev_input_values$marginal_plot_legend_text_font = input[[paste0("marginal_plot_legend_text_font_", tab_id)]]
          
          #Marginal Plots
          prev_input_values$marginal_plot_type = input[[paste0("marginal_plot_type_", tab_id)]]
          prev_input_values$marginal_plot_margins = input[[paste0("marginal_plot_margins_", tab_id)]]
          prev_input_values$marginal_plot_size = input[[paste0("marginal_plot_size_", tab_id)]]
          prev_input_values$marginal_plot_bindwidth_x = input[[paste0("marginal_plot_bindwidth_x_", tab_id)]]
          prev_input_values$marginal_plot_border_colour_x = input[[paste0("marginal_plot_border_colour_x_", tab_id)]]
          prev_input_values$marginal_plot_fill_colour_x = input[[paste0("marginal_plot_fill_colour_x_", tab_id)]]
          prev_input_values$marginal_plot_line_width_x = input[[paste0("marginal_plot_line_width_x_", tab_id)]]
          prev_input_values$marginal_plot_transparency_x = input[[paste0("marginal_plot_transparency_x_", tab_id)]]
          prev_input_values$marginal_plot_bindwidth_y = input[[paste0("marginal_plot_bindwidth_y_", tab_id)]]
          prev_input_values$marginal_plot_border_colour_y = input[[paste0("marginal_plot_border_colour_y_", tab_id)]]
          prev_input_values$marginal_plot_fill_colour_y = input[[paste0("marginal_plot_fill_colour_y_", tab_id)]]
          prev_input_values$marginal_plot_line_width_y = input[[paste0("marginal_plot_line_width_y_", tab_id)]]
          prev_input_values$marginal_plot_transparency_y = input[[paste0("marginal_plot_transparency_y_", tab_id)]]
          
          #Raincloud halfeye
          prev_input_values$stat_dots_side = input[[paste0("stat_dots_side_", tab_id)]]
          prev_input_values$stat_dots_binwidth = input[[paste0("stat_dots_bindwidth_", tab_id)]]
          prev_input_values$stat_dots_justification = input[[paste0("stat_dots_justification_", tab_id)]]
          prev_input_values$raincloud_box_width = input[[paste0("box_raincloud_width_", tab_id)]]
          prev_input_values$raincloud_halfeye_width = input[[paste0("raincloud_halfeye_width_", tab_id)]]
          prev_input_values$raincloud_halfeye_line_thickness = input[[paste0("raincloud_halfeye_line_thickness_", tab_id)]]
          prev_input_values$raincloud_halfeye_justification = input[[paste0("raincloud_halfeye_justification_", tab_id)]]
          prev_input_values$raincloud_halfeye_transparency = input[[paste0("raincloud_halfeye_transparency_", tab_id)]]
          
          # Effect size plots
          prev_input_values$distribution_fill_colour = input[[paste0("effect_size_distribution_fill_colour_", tab_id)]]
          prev_input_values$distribution_line_colour = input[[paste0("effect_size_distribution_line_colour_", tab_id)]]
          prev_input_values$distribution_transparency = input[[paste0("effect_size_distribution_transparency_", tab_id)]]
          prev_input_values$distribution_line_type = input[[paste0("effect_size_distribution_line_type_", tab_id)]]
          prev_input_values$distribution_line_width = input[[paste0("effect_size_distribution_line_width_", tab_id)]]
          prev_input_values$distribution_bandwith_adjustment = input[[paste0("effect_size_distribution_bandwith_adjustment_", tab_id)]]
          prev_input_values$mean_dot_colour = input[[paste0("effect_size_mean_colour_", tab_id)]]
          prev_input_values$mean_dot_size = input[[paste0("effect_size_mean_dot_size_", tab_id)]]
          prev_input_values$CI_line_colour = input[[paste0("effect_size_CI_colour_", tab_id)]]
          prev_input_values$CI_line_width = input[[paste0("effect_size_CI_line_width_", tab_id)]]
          prev_input_values$zero_line_colour = input[[paste0("effect_size_zero_colour_", tab_id)]]
          prev_input_values$zero_line_type = input[[paste0("effect_size_zero_line_type_", tab_id)]]
          prev_input_values$zero_line_width = input[[paste0("effect_size_zero_line_width_", tab_id)]]
          
        }
        
        eval(parse(text = paste0("click_count_", tab_id, "(0)")))

        # reactive plot
            assign(paste0("plot_for_all_", tab_id), reactive({
         tryCatch({
        
          #req(input$submit_plot_table || input$submit_plot_icc_two || input$submit_plot_icc || input$submit_plot_box_bar_scatter_violin || input$submit_plot_superplot)
          
          # adjust axis titles beforehand
          prev_input_values$y_axis_text_title = input[[paste0("y_axis_text_title_", tab_id)]]
          prev_input_values$x_axis_text_title = input[[paste0("x_axis_text_title_", tab_id)]]
          
          # for Superplot
          if (tab_id == 'superplot'){
            # Group and summarize the data by group and x-variable
            if (input$mean_or_median_superplot == "mean") {
              Replicate_means_or_medians <- data %>% 
                dplyr::group_by(!!as.name(x_var), !!as.name(z_var)) %>% 
                summarise(across(everything(), mean, na.rm = TRUE))
              
              # Calculates total averages
              Total_replicates <- Replicate_means_or_medians %>% 
                summarise(across(everything(), mean, na.rm = TRUE))
            } else if (input$mean_or_median_superplot == "median") {
              Replicate_means_or_medians <- data %>% 
                dplyr::group_by(!!as.name(x_var), !!as.name(z_var)) %>% 
                summarise(across(everything(), median, na.rm = TRUE))
              
              Total_replicates <- Replicate_means_or_medians %>% 
                summarise(across(everything(), median, na.rm = TRUE))
            }
            
            #function for IQR
            median_IQR <- function(x) {
              data.frame(y = median(x), # Median
                         ymin = quantile(x)[2], # 1st quartile
                         ymax = quantile(x)[4])  # 3rd quartile
            }
          }
          
          # Get the levels of the group variable in the order chosen by the user
          # for first grouping variable
          #for numeric variables
          if (is.numeric(data[[x_var]])) {
            group_levels <- unique(data[[x_var]])
            #for non-numeric variables
          } else if (!is.numeric(data[[x_var]])) {
            group_levels <- levels(data[[x_var]])
          }
          
          # for additional grouping variable
          additional_group_levels <- NULL
          
            #for numeric variables 
            if (!is.null(z_var) && is.numeric(data[[z_var]]) && input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE) {
              additional_group_levels <- unique(data[[z_var]])
              #for non-numeric variables
            } else if (!is.null(z_var) && !is.numeric(data[[z_var]]) && input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE) {
              additional_group_levels <- levels(data[[z_var]])
            }
          
          #for group order selection
          # initial group levels
          if (!is.null(input[[paste0("group_order_", tab_id)]])) {
            group_levels <- input[[paste0("group_order_", tab_id)]]
          }
          
          #for additional group levels 
          if (!is.null(z_var) && !is.null(input[[paste0("additional_group_order_", tab_id)]])) {
            additional_group_levels <- input[[paste0("additional_group_order_", tab_id)]]
          }
          
          # Filter data based on group_levels
            if (x_var %in% colnames(data)) {
              filtered_data <- data %>%
                filter(!!as.name(x_var) %in% group_levels)
            } else {
              filtered_data <- data
            }
          
          
          # for additional variable in box plot
          if (input$navpage == 'Box, Bar, Scatter, Violin and Raincloud'){
            if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE && !is.null(input$additional_group_order_box_bar_scatter_violin)) {
              if (input$additional_variable_box_bar_scatter_violin %in% colnames(filtered_data)) {
                filtered_data <- filtered_data %>%
                  filter(!!as.name(input$additional_variable_box_bar_scatter_violin) %in% input$additional_group_order_box_bar_scatter_violin)
              } else {
                filtered_data <- filtered_data
              }
            }
          } else {
            # Check if input$y_var_etc is in data_plot()
            if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE && !is.null(z_var)) {
              if (z_var %in% colnames(filtered_data)) {
                filtered_data <- filtered_data %>%
                  filter(!!as.name(z_var) %in% additional_group_levels)
                
                if (is.factor(filtered_data[[z_var]])){
                  # relevels data based on additional group var
                  filtered_data[[z_var]] <- factor(filtered_data[[z_var]], levels = additional_group_levels)
                }
              } else {
                filtered_data <- filtered_data
              }
            }
          }
          
          
          # updates data for brushed points
          assign(paste0("brushed_points_", tab_id), reactiveVal(filtered_data))
          #brushed_points_table(filtered_data)
          #eval(parse(text = paste0("brushed_points_", tab_id, "(", filtered_data, ")")))
          
          # for brushed points when plotting
          if (tab_id == "table"){
            output$table_brush_selection <- renderTable({
              brushedPoints(filtered_data, input$table_plot_brush, xvar = input$x_axis, yvar = input$y_axis)
            })
          } else if (tab_id == "icc"){
            output$icc_brush_selection <- renderTable({
              brushedPoints(filtered_data, input$icc_plot_brush, xvar = input$x_var_icc, yvar = input$y_var_icc)
            })
          } else if (tab_id == "icc_two"){
            output$icc_two_brush_selection <- renderTable({
              if (input$num_levels == 2) {
                brushedPoints(filtered_data, input$icc_two_plot_brush, xvar = input$group_var_icc, yvar = input$y_var_icc)
              } else if (input$num_levels == 3) {
                brushedPoints(filtered_data, input$icc_two_plot_brush, xvar = input$group_var_icc, yvar = input$z_var_icc)
              } else if (input$num_levels == 4) {
                brushedPoints(filtered_data, input$icc_two_plot_brush, xvar = input$group_var_icc, yvar = input$w_var_icc)
              }
            })
          } else if (tab_id == "box_bar_scatter_violin"){
            output$box_bar_scatter_violin_brush_selection <- renderTable({
              if (input$num_levels_box_bar_scatter_violin == 1) {
                brushedPoints(filtered_data, input$plot_brush_box_bar_scatter_violin_plot, xvar = input$group_var_box_bar_scatter_violin, yvar = input$x_var_box_bar_scatter_violin)
              } else if (input$num_levels_box_bar_scatter_violin == 2) {
                brushedPoints(filtered_data, input$plot_brush_box_bar_scatter_violin_plot, xvar = input$group_var_box_bar_scatter_violin, yvar = input$y_var_box_bar_scatter_violin)
              } else if (input$num_levels_box_bar_scatter_violin == 3) {
                brushedPoints(filtered_data, input$plot_brush_box_bar_scatter_violin_plot, xvar = input$group_var_box_bar_scatter_violin, yvar = input$z_var_box_bar_scatter_violin)
              } else if(input$num_levels_box_bar_scatter_violin == 4){
                brushedPoints(filtered_data, input$plot_brush_box_bar_scatter_violin_plot, xvar = input$group_var_box_bar_scatter_violin, yvar = input$z_var_box_bar_scatter_violin)
              }
            })
          } else if (tab_id == "superplot"){
            output$superplot_brush_selection <- renderTable({
              brushedPoints(filtered_data, input$plot_brush_superplot, xvar = input$group_var_superplot, yvar = input$y_var_superplot)
            })
          }
          
          
          
          # defines colours for symbol border and fill
          # for symbol border
            # when additional grouping is not selected
            if (input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE || (input[[paste0("show_additional_group_legend_" ,tab_id)]] == TRUE && !identical(prev_input_values$geom_jitter_colour_fill_by_group, z_var))) {
              if(length(get(paste0("selected_colors_", tab_id))$colors) < length(unique(filtered_data[[x_var]])) || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour") {
                symbol_border_colour_scale <- rep(prev_input_values$color_var, length(unique(filtered_data[[x_var]])))
              } else if (length(get(paste0("selected_colors_", tab_id))$colors) > length(unique(filtered_data[[x_var]]))) {  
                symbol_border_colour_scale <- get(paste0("selected_colors_", tab_id))$colors[1:length(group_levels)]
              } else {
                symbol_border_colour_scale <- c(get(paste0("selected_colors_", tab_id))$colors)
              } 
              
              # when additional grouping is selected
            } else if (input[[paste0("show_additional_group_legend_" ,tab_id)]] == TRUE) {
              if(length(get(paste0("selected_colors_", tab_id))$colors) < length(unique(filtered_data[[z_var]])) || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour") {
                symbol_border_colour_scale <- rep(prev_input_values$color_var, length(unique(filtered_data[[z_var]])))
              } else if (length(get(paste0("selected_colors_", tab_id))$colors) > length(unique(filtered_data[[z_var]]))) {  
                symbol_border_colour_scale <- get(paste0("selected_colors_", tab_id))$colors[1:length(additional_group_levels)]
              } else {
                symbol_border_colour_scale <- c(get(paste0("selected_colors_", tab_id))$colors)
              }
            }
  
          #for symbol fill
            # when additional grouping is not selected
            if (input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE || (input[[paste0("show_additional_group_legend_" ,tab_id)]] == TRUE && !identical(prev_input_values$geom_jitter_colour_fill_by_group, z_var))) {
              if(length(get(paste0("selected_colours_symbol_fill_", tab_id))$colors) < length(unique(filtered_data[[x_var]])) || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour") {
                symbol_fill_colour_scale <- rep(prev_input_values$symbol_fill, length(unique(filtered_data[[x_var]])))
              } else if (length(get(paste0("selected_colours_symbol_fill_", tab_id))$colors) > length(unique(filtered_data[[x_var]]))) {
                symbol_fill_colour_scale <- get(paste0("selected_colours_symbol_fill_", tab_id))$colors[1:length(group_levels)]
              } else {
                symbol_fill_colour_scale <- c(get(paste0("selected_colours_symbol_fill_", tab_id))$colors)
              }
              
              # when additional grouping is selected (needs fixing)
            } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE) {
              if(length(get(paste0("selected_colours_symbol_fill_", tab_id))$colors) < length(unique(filtered_data[[z_var]])) || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour") {
                symbol_fill_colour_scale <- rep(prev_input_values$symbol_fill, length(unique(filtered_data[[z_var]])))
              } else if (length(get(paste0("selected_colours_symbol_fill_", tab_id))$colors) > length(unique(filtered_data[[z_var]]))) {
                symbol_fill_colour_scale <- get(paste0("selected_colours_symbol_fill_", tab_id))$colors[1:length(additional_group_levels)]
              } else {
                symbol_fill_colour_scale <- c(get(paste0("selected_colours_symbol_fill_", tab_id))$colors)
              }
            }
          
          # for boxplot border
            # when additional grouping is NOT selected
            if (input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE || (input[[paste0("show_additional_group_legend_" ,tab_id)]] == TRUE && !identical(prev_input_values$geom_boxplot_colour_fill_by_group, z_var))) {
              if(length(get(paste0("selected_colors_boxplot_line_", tab_id))$colors) < length(unique(filtered_data[[x_var]])) || prev_input_values$geom_boxplot_colour_fill_by_group == "Single Colour") {
                boxplot_border_colour_scale <- rep(prev_input_values$box_line, length(unique(filtered_data[[x_var]])))
              } else if (length(get(paste0("selected_colors_boxplot_line_", tab_id))$colors) > length(unique(filtered_data[[x_var]]))) {  
                boxplot_border_colour_scale <- get(paste0("selected_colors_boxplot_line_", tab_id))$colors[1:length(group_levels)]
              } else {
                boxplot_border_colour_scale <- c(get(paste0("selected_colors_boxplot_line_", tab_id))$colors)
              } 
              
              # when additional grouping IS selected (needs fixing)
            } else if (input[[paste0("show_additional_group_legend_" ,tab_id)]] == TRUE) {
              if(length(get(paste0("selected_colors_boxplot_line_", tab_id))$colors) < length(unique(filtered_data[[z_var]])) || prev_input_values$geom_boxplot_colour_fill_by_group == "Single Colour") {
                boxplot_border_colour_scale <- rep(prev_input_values$box_line, length(unique(filtered_data[[z_var]])))
              } else if (length(get(paste0("selected_colors_boxplot_line_", tab_id))$colors) > length(unique(filtered_data[[z_var]]))) {  
                boxplot_border_colour_scale <- get(paste0("selected_colors_boxplot_line_", tab_id))$colors[1:length(additional_group_levels)]
              } else {
                boxplot_border_colour_scale <- c(get(paste0("selected_colors_boxplot_line_", tab_id))$colors)
              }
            }

          #for boxplot fill
            # when additional grouping is NOT selected
            if (input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE || (input[[paste0("show_additional_group_legend_" ,tab_id)]] == TRUE && !identical(prev_input_values$geom_boxplot_colour_fill_by_group, z_var))) {
              if(length(get(paste0("selected_colors_boxplot_fill_", tab_id))$colors) < length(unique(filtered_data[[x_var]])) || prev_input_values$geom_boxplot_colour_fill_by_group == "Single Colour") {
                boxplot_fill_colour_scale <- rep(prev_input_values$box_fill, length(unique(filtered_data[[x_var]])))
              } else if (length(get(paste0("selected_colors_boxplot_fill_", tab_id))$colors) > length(unique(filtered_data[[x_var]]))) {  
                boxplot_fill_colour_scale <- get(paste0("selected_colors_boxplot_fill_", tab_id))$colors[1:length(group_levels)]
              } else {
                boxplot_fill_colour_scale <- c(get(paste0("selected_colors_boxplot_fill_", tab_id))$colors)
              } 
              
              # when additional grouping IS selected (needs fixing)
            } else if (input[[paste0("show_additional_group_legend_" ,tab_id)]] == TRUE) {
              if(length(get(paste0("selected_colors_boxplot_fill_", tab_id))$colors) < length(unique(filtered_data[[z_var]])) || prev_input_values$geom_boxplot_colour_fill_by_group == "Single Colour") {
                boxplot_fill_colour_scale <- rep(prev_input_values$box_fill, length(unique(filtered_data[[z_var]])))
              } else if (length(get(paste0("selected_colors_boxplot_fill_", tab_id))$colors) > length(unique(filtered_data[[z_var]]))) {  
                boxplot_fill_colour_scale <- get(paste0("selected_colors_boxplot_fill_", tab_id))$colors[1:length(additional_group_levels)]
              } else {
                boxplot_fill_colour_scale <- c(get(paste0("selected_colors_boxplot_fill_", tab_id))$colors)
              }
            }
          
          # generates legend option for theme
          if (prev_input_values$maginal_plot_legend != "custom") {
            legend_position_selection = prev_input_values$maginal_plot_legend
          } else if (prev_input_values$maginal_plot_legend == "custom"){
            legend_position_selection = c(as.numeric(prev_input_values$marginal_plot_legend_position_x), as.numeric(prev_input_values$marginal_plot_legend_position_y))
          }
          
          #adjust name for geom_jitter legend
          if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  identical(prev_input_values$geom_jitter_colour_fill_by_group,z_var)) {
            legend_name_jitter = z_var
          } else {
            legend_name_jitter = x_var
          }
          
          # switches between gradient and discrete scales for geom_jitter
          if ((prev_input_values$geom_jitter_colour_fill_by_group == paste0(z_var, " ") && is.numeric(data[[z_var]]))|| (prev_input_values$geom_jitter_colour_fill_by_group == paste0(x_var, " ") && is.numeric(data[[x_var]]))) {
            scale_fill_geom_jitter_jitter <- scale_fill_gradient(name = legend_name_jitter, low = prev_input_values$symbol_fill_gradient_low, high = prev_input_values$symbol_fill_gradient_high)
            scale_border_geom_jitter <- scale_color_gradient(name = legend_name_jitter, low = prev_input_values$color_var_gradient_low, high = prev_input_values$color_var_gradient_high)
          } else {
            scale_fill_geom_jitter_jitter <- scale_fill_manual(name = legend_name_jitter, values = symbol_fill_colour_scale)
            scale_border_geom_jitter <- scale_color_manual(name = legend_name_jitter, values = symbol_border_colour_scale)
          }
          
          #adjust name for geom_boxplot, geom_violin and geom_bar legend
          if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_boxplot_colour_fill_by_group == z_var) {
            legend_name_boxplot = z_var
          } else {
            legend_name_boxplot = x_var
          }
          
          # switches between gradient and discrete scales for geom_boxplot, geom_violin and geom_bar legend
          scale_fill_geom_boxplot <- scale_fill_manual(name = legend_name_boxplot, values = boxplot_fill_colour_scale)
          scale_border_geom_boxplot <- scale_color_manual(name = legend_name_boxplot, values = boxplot_border_colour_scale)
          
          ########################################################################
          # adds Box-plot if selected
           if (input$navpage != "Superplot" && input[[paste0("choose_" , tab_id)]] == "Box-plot") {
            
             neuro_plot <- 
               
              # if group_var is a factor
               (if (!is.numeric(filtered_data[[x_var]])){
                 ggplot(filtered_data, aes(x = factor(.data[[x_var]], levels = group_levels), y = .data[[y_var]]))
                 # if group_var is numeric
               } else if (is.numeric(filtered_data[[x_var]])) {
                 ggplot(filtered_data, aes(x = .data[[x_var]], y = .data[[y_var]]))
               }) +
                
             
               # adjusts boxplot parameters
                 # for additional grouping variable  
                 (if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_boxplot_colour_fill_by_group == z_var) {
                   geom_boxplot(aes(colour = factor(.data[[z_var]], levels = additional_group_levels), 
                                    fill = factor(.data[[z_var]], levels = additional_group_levels)), 
                                size = prev_input_values$box_line_thickness, 
                                alpha = prev_input_values$box_transparency, 
                                width = prev_input_values$box_width, 
                                notch = FALSE, 
                                outlier.shape = NA, 
                                coef = 1E100,
                                show.legend = ("Box" %in% prev_input_values$legends)
                                )
                   
                 } else if (prev_input_values$geom_boxplot_colour_fill_by_group == x_var || prev_input_values$geom_boxplot_colour_fill_by_group == "Single Colour"){
                   # for initial grouping variable
                   geom_boxplot(aes(colour = factor(.data[[x_var]], levels = group_levels), 
                                    fill = factor(.data[[x_var]], levels = group_levels)), 
                                size = prev_input_values$box_line_thickness, 
                                alpha = prev_input_values$box_transparency, 
                                width = prev_input_values$box_width, 
                                notch = FALSE, 
                                outlier.shape = NA, 
                                coef = 1E100,
                                show.legend = ("Box" %in% prev_input_values$legends)
                   )
                 }) +
               
               # adds colour scales for geom_boxplot
               scale_fill_geom_boxplot +
               scale_border_geom_boxplot +
               new_scale_color() + new_scale_fill() +
               
               #adjust colour, shape, fill, size, edge thickness, transparency for datapoints
               # for additional grouping variable
               (if(input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE && prev_input_values$geom_jitter_colour_fill_by_group == z_var && prev_input_values$geom_boxplot_colour_fill_by_group == z_var) {
                 geom_point(aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                                fill = factor(.data[[z_var]], levels = additional_group_levels)),
                            position = position_jitterdodge(jitter.width = prev_input_values$symbol_jitter),
                            size = prev_input_values$symbol_size,
                            shape = as.numeric(prev_input_values$symbol_shape),
                            stroke = prev_input_values$symbol_edge_thickness,
                            alpha = prev_input_values$symbol_transparency,
                            show.legend = ("Dots" %in% prev_input_values$legends)
                 )
               } else {
                 if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == z_var) {
                   geom_jitter(aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                                   fill = factor(.data[[z_var]], levels = additional_group_levels)),
                               size = prev_input_values$symbol_size,
                               shape = as.numeric(prev_input_values$symbol_shape),
                               stroke = prev_input_values$symbol_edge_thickness,
                               alpha = prev_input_values$symbol_transparency,
                               width = prev_input_values$symbol_jitter,
                               show.legend = ("Dots" %in% prev_input_values$legends)
                   )
                   # for initial grouping variable
                 } else if (prev_input_values$geom_jitter_colour_fill_by_group == x_var || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour"){
                   geom_jitter(aes(colour = factor(.data[[x_var]], levels = group_levels),
                                   fill = factor(.data[[x_var]], levels = group_levels)),
                               size = prev_input_values$symbol_size,
                               shape = as.numeric(as.numeric(prev_input_values$symbol_shape)),
                               stroke = prev_input_values$symbol_edge_thickness,
                               alpha = prev_input_values$symbol_transparency,
                               width = prev_input_values$symbol_jitter,
                               show.legend = ("Dots" %in% prev_input_values$legends)
                   )
                   
                 } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == paste0(z_var, " ")){
                   geom_jitter(aes(colour = .data[[z_var]],
                                   fill = .data[[z_var]]),
                               size = prev_input_values$symbol_size,
                               shape = as.numeric(prev_input_values$symbol_shape),
                               stroke = prev_input_values$symbol_edge_thickness,
                               alpha = prev_input_values$symbol_transparency,
                               width = prev_input_values$symbol_jitter,
                               show.legend = ("Dots" %in% prev_input_values$legends)
                   )
                 } else if (prev_input_values$geom_jitter_colour_fill_by_group == paste0(x_var, " ")) {
                   geom_jitter(aes(colour = .data[[x_var]],
                                   fill = .data[[x_var]]),
                               size = prev_input_values$symbol_size,
                               shape = as.numeric(as.numeric(prev_input_values$symbol_shape)),
                               stroke = prev_input_values$symbol_edge_thickness,
                               alpha = prev_input_values$symbol_transparency,
                               width = prev_input_values$symbol_jitter,
                               show.legend = ("Dots" %in% prev_input_values$legends)
                   )
                 }
               })+
               
               # adds line for repeated observations
               (if (input[[paste0("repeated_observations_", tab_id)]] == TRUE && input[[paste0("paired_observations_", tab_id)]] != "" && input$navpage != 'Intraclass correlation & Linear Mixed Models') {
                 geom_line(aes(group = .data[[input[[paste0("paired_observations_", tab_id)]]]], colour = factor(.data[[x_var]], levels = group_levels)))  # Connect paired observations
               }) +
               
               # adds colour scales for geom_jitter
               scale_fill_geom_jitter_jitter +
               scale_border_geom_jitter 
              
          #For Bar Chart Plot
          } else if (input$navpage != "Superplot" && input[[paste0("choose_", tab_id)]] == "Bar Chart") {
            neuro_plot <- 
              
            #if group_var is a factor
            (if (!is.numeric(filtered_data[[x_var]])){
              ggplot(filtered_data, aes(x = factor(.data[[x_var]], levels = group_levels), y = .data[[y_var]]))
              # if group_var is numeric
            } else if (is.numeric(filtered_data[[x_var]])) {
              ggplot(filtered_data, aes(x = .data[[x_var]], y = .data[[y_var]]))
            }) +
           
            # adjusts bar chart parameters
              # for additional grouping variable
              (if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_boxplot_colour_fill_by_group == z_var) {
                geom_bar(stat = "summary", position = "dodge", aes(colour = factor(.data[[z_var]], levels = additional_group_levels), fill = factor(.data[[z_var]], levels = additional_group_levels)),
                         size = prev_input_values$box_line_thickness, 
                         alpha = prev_input_values$box_transparency, 
                         width = prev_input_values$box_width,
                         show.legend = ("Bar" %in% prev_input_values$legends)
                )
              } else if (prev_input_values$geom_boxplot_colour_fill_by_group == x_var || prev_input_values$geom_boxplot_colour_fill_by_group == "Single Colour"){
                # for initial grouping variable
                geom_bar(stat = "summary", position = "dodge", aes(colour = factor(.data[[x_var]], levels = group_levels), fill = factor(.data[[x_var]], levels = group_levels)),
                         linewidth = prev_input_values$box_line_thickness, 
                         alpha = prev_input_values$box_transparency, 
                         width = prev_input_values$box_width,
                         show.legend = ("Bar" %in% prev_input_values$legends))
              }) +
            
              # adds errorbar if applicable
                (if (prev_input_values$sd_se_bar_chart == "Standard deviation" && input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_boxplot_colour_fill_by_group == z_var){
                  #stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "errorbar", color = boxplot_border_colour_scale, width = as.numeric(input$box_width_box_bar_scatter_violin * 0.5), size = input$box_line_thickness_box_bar_scatter_violin)
                  stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "errorbar",
                               aes(colour = factor(.data[[z_var]], levels = additional_group_levels)),
                               width = as.numeric(prev_input_values$box_width * 0.5),
                               size = prev_input_values$box_line_thickness)
                } else if (prev_input_values$sd_se_bar_chart == "Standard error of mean" && input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_boxplot_colour_fill_by_group == z_var){
                  geom_errorbar(stat = "summary", fun.data = "mean_se", position = "dodge",
                                aes(colour = factor(.data[[z_var]], levels = additional_group_levels)),
                                width = as.numeric(prev_input_values$box_width * 0.5),
                                size = prev_input_values$box_line_thickness)
                } else if (prev_input_values$sd_se_bar_chart == "None" && input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_boxplot_colour_fill_by_group == z_var) {
                  geom_errorbar(stat = "summary", fun.data = "none")
                } else if (prev_input_values$sd_se_bar_chart == "Standard deviation" && (prev_input_values$geom_boxplot_colour_fill_by_group == x_var || prev_input_values$geom_boxplot_colour_fill_by_group == "Single Colour")){
                  stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "errorbar",
                               aes(colour = factor(.data[[x_var]], levels = group_levels)),
                               width = as.numeric(prev_input_values$box_width * 0.5),
                               size = prev_input_values$box_line_thickness)
                } else if (prev_input_values$sd_se_bar_chart == "Standard error of mean" && (prev_input_values$geom_boxplot_colour_fill_by_group == x_var || prev_input_values$geom_boxplot_colour_fill_by_group == "Single Colour")){
                  geom_errorbar(stat = "summary", fun.data = "mean_se", position = "dodge",
                                aes(colour = factor(.data[[x_var]], levels = group_levels)),
                                width = as.numeric(prev_input_values$box_width * 0.5),
                                size = prev_input_values$box_line_thickness)
                } else if (prev_input_values$sd_se_bar_chart == "None" && (prev_input_values$geom_boxplot_colour_fill_by_group == x_var || prev_input_values$geom_boxplot_colour_fill_by_group == "Single Colour")) {
                  geom_errorbar(stat = "summary", fun.data = "none")
                }) +
              
              # adds colour scales for geom_errorbar
              scale_fill_geom_boxplot +
              scale_border_geom_boxplot +
              new_scale_color() + new_scale_fill() +
  
            
            #adjust colour, shape, fill, size, edge thickness, transparency for datapoints
            # for additional grouping variable
              (if(input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE && prev_input_values$geom_jitter_colour_fill_by_group == z_var && prev_input_values$geom_boxplot_colour_fill_by_group == z_var) {
                geom_point(aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                               fill = factor(.data[[z_var]], levels = additional_group_levels)),
                           position = position_jitterdodge(jitter.width = prev_input_values$symbol_jitter),
                           size = prev_input_values$symbol_size,
                           shape = as.numeric(prev_input_values$symbol_shape),
                           stroke = prev_input_values$symbol_edge_thickness,
                           alpha = prev_input_values$symbol_transparency,
                           show.legend = ("Dots" %in% prev_input_values$legends)
                           )
              } else {
                if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == z_var) {
  
  
                  geom_jitter(aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                                  fill = factor(.data[[z_var]], levels = additional_group_levels)),
                              size = prev_input_values$symbol_size,
                              shape = as.numeric(prev_input_values$symbol_shape),
                              stroke = prev_input_values$symbol_edge_thickness,
                              alpha = prev_input_values$symbol_transparency,
                              width = prev_input_values$symbol_jitter,
                              show.legend = ("Dots" %in% prev_input_values$legends)
                  )
                  # for initial grouping variable
                } else if (prev_input_values$geom_jitter_colour_fill_by_group == x_var || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour"){
                  geom_jitter(aes(colour = factor(.data[[x_var]], levels = group_levels),
                                  fill = factor(.data[[x_var]], levels = group_levels)),
                              size = prev_input_values$symbol_size,
                              shape = as.numeric(as.numeric(prev_input_values$symbol_shape)),
                              stroke = prev_input_values$symbol_edge_thickness,
                              alpha = prev_input_values$symbol_transparency,
                              width = prev_input_values$symbol_jitter,
                              show.legend = ("Dots" %in% prev_input_values$legends)
                              )
  
                } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == paste0(z_var, " ")){
                  geom_jitter(aes(colour = .data[[z_var]],
                                  fill = .data[[z_var]]),
                              size = prev_input_values$symbol_size,
                              shape = as.numeric(prev_input_values$symbol_shape),
                              stroke = prev_input_values$symbol_edge_thickness,
                              alpha = prev_input_values$symbol_transparency,
                              width = prev_input_values$symbol_jitter,
                              show.legend = ("Dots" %in% prev_input_values$legends)
                              )
                } else if (prev_input_values$geom_jitter_colour_fill_by_group == paste0(x_var, " ")) {
                  geom_jitter(aes(colour = .data[[x_var]],
                                  fill = .data[[x_var]]),
                              size = prev_input_values$symbol_size,
                              shape = as.numeric(as.numeric(prev_input_values$symbol_shape)),
                              stroke = prev_input_values$symbol_edge_thickness,
                              alpha = prev_input_values$symbol_transparency,
                              width = prev_input_values$symbol_jitter,
                              show.legend = ("Dots" %in% prev_input_values$legends)
                              )
                }
              })+
              
              # adds line for repeated observations
              (if (input[[paste0("repeated_observations_", tab_id)]] == TRUE && input[[paste0("paired_observations_", tab_id)]] != "" && input$navpage != 'Intraclass correlation & Linear Mixed Models') {
                geom_line(aes(group = .data[[input[[paste0("paired_observations_", tab_id)]]]], colour = factor(.data[[x_var]], levels = group_levels)))  # Connect paired observations
              }) +
              
              # adds colour scales for geom_jitter
              scale_fill_geom_jitter_jitter +
              scale_border_geom_jitter
          
            # For scatter plot
            } else if (input$navpage != "Superplot" && input[[paste0("choose_", tab_id)]] == "Scatter") { 
            
              neuro_plot <- 
                
              # if group_var is a factor
                (if (!is.numeric(filtered_data[[x_var]])){
                  ggplot(filtered_data, aes(x = factor(.data[[x_var]], levels = group_levels), y = .data[[y_var]]))
                  # if group_var is numeric
                } else if (is.numeric(filtered_data[[x_var]])) {
                  ggplot(filtered_data, aes(x = .data[[x_var]], y = .data[[y_var]]))
                }) +

              #adjust colour, shape, fill, size, edge thickness, transparency for datapoints
                # for additional grouping variable
                (if(input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE && prev_input_values$geom_jitter_colour_fill_by_group == z_var && prev_input_values$geom_boxplot_colour_fill_by_group == z_var) {
                  geom_point(aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                                 fill = factor(.data[[z_var]], levels = additional_group_levels)),
                             position = position_jitterdodge(jitter.width = prev_input_values$symbol_jitter),
                             size = prev_input_values$symbol_size,
                             shape = as.numeric(prev_input_values$symbol_shape),
                             stroke = prev_input_values$symbol_edge_thickness,
                             alpha = prev_input_values$symbol_transparency,
                             show.legend = ("Dots" %in% prev_input_values$legends)
                             )
                } else {
                  if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == z_var) {
  
  
                    geom_jitter(aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                                    fill = factor(.data[[z_var]], levels = additional_group_levels)),
                                size = prev_input_values$symbol_size,
                                shape = as.numeric(prev_input_values$symbol_shape),
                                stroke = prev_input_values$symbol_edge_thickness,
                                alpha = prev_input_values$symbol_transparency,
                                width = prev_input_values$symbol_jitter,
                                show.legend = ("Dots" %in% prev_input_values$legends)
                    )
                    # for initial grouping variable
                  } else if (prev_input_values$geom_jitter_colour_fill_by_group == x_var || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour"){
                    geom_jitter(aes(colour = factor(.data[[x_var]], levels = group_levels),
                                    fill = factor(.data[[x_var]], levels = group_levels)),
                                size = prev_input_values$symbol_size,
                                shape = as.numeric(as.numeric(prev_input_values$symbol_shape)),
                                stroke = prev_input_values$symbol_edge_thickness,
                                alpha = prev_input_values$symbol_transparency,
                                width = prev_input_values$symbol_jitter,
                                show.legend = ("Dots" %in% prev_input_values$legends)
                                )
  
                  } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == paste0(z_var, " ")){
                    geom_jitter(aes(colour = .data[[z_var]],
                                    fill = .data[[z_var]]),
                                size = prev_input_values$symbol_size,
                                shape = as.numeric(prev_input_values$symbol_shape),
                                stroke = prev_input_values$symbol_edge_thickness,
                                alpha = prev_input_values$symbol_transparency,
                                width = prev_input_values$symbol_jitter,
                                show.legend = ("Dots" %in% prev_input_values$legends)
                                )
                  } else if (prev_input_values$geom_jitter_colour_fill_by_group == paste0(x_var, " ")) {
                    geom_jitter(aes(colour = .data[[x_var]],
                                    fill = .data[[x_var]]),
                                size = prev_input_values$symbol_size,
                                shape = as.numeric(as.numeric(prev_input_values$symbol_shape)),
                                stroke = prev_input_values$symbol_edge_thickness,
                                alpha = prev_input_values$symbol_transparency,
                                width = prev_input_values$symbol_jitter,
                                show.legend = ("Dots" %in% prev_input_values$legends)
                                )
                  }
                }) +
                
                # adds line for repeated observations
                (if (input[[paste0("repeated_observations_", tab_id)]] == TRUE && input[[paste0("paired_observations_", tab_id)]] != "" && input$navpage != 'Intraclass correlation & Linear Mixed Models') {
                  geom_line(aes(group = .data[[input[[paste0("paired_observations_", tab_id)]]]], colour = factor(.data[[x_var]], levels = group_levels)))  # Connect paired observations
                }) +
                
                # adds colour scale for geom_jitter
                scale_fill_geom_jitter_jitter +
                scale_border_geom_jitter
            
          #For Violin plot
          } else if (input$navpage != "Superplot" && input[[paste0("choose_", tab_id)]] == "Violin"){
            neuro_plot <-
              
            #if group_var is a factor
              (if (!is.numeric(filtered_data[[x_var]])){
                ggplot(filtered_data, aes(x = factor(.data[[x_var]], levels = group_levels), y = .data[[y_var]]))
                # if group_var is numeric
              } else if (is.numeric(filtered_data[[x_var]])) {
                ggplot(filtered_data, aes(x = .data[[x_var]], y = .data[[y_var]]))
              }) +
           
              # adjusts violin parameters
                # for additional grouping variable
                (if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_boxplot_colour_fill_by_group == z_var) {
                  geom_violin(aes(colour = factor(.data[[z_var]], levels = additional_group_levels), 
                                   fill = factor(.data[[z_var]], levels = additional_group_levels)), 
                               size = prev_input_values$box_line_thickness, 
                               alpha = prev_input_values$box_transparency, 
                               width = prev_input_values$box_width, 
                               notch = FALSE, 
                               outlier.shape = NA, 
                               coef = 1E100,
                              show.legend = ("Violin" %in% prev_input_values$legends)
                              )
                  
                } else if (prev_input_values$geom_boxplot_colour_fill_by_group == x_var || prev_input_values$geom_boxplot_colour_fill_by_group == "Single Colour"){
                  # for initial grouping variable
                  geom_violin(aes(colour = factor(.data[[x_var]], levels = group_levels), 
                                   fill = factor(.data[[x_var]], levels = group_levels)), 
                               size = prev_input_values$box_line_thickness, 
                               alpha = prev_input_values$box_transparency, 
                               width = prev_input_values$box_width, 
                               notch = FALSE, 
                               outlier.shape = NA, 
                               coef = 1E100,
                              show.legend = ("Violin" %in% prev_input_values$legends)
                              )
                }) +
              
              
              # adds errorbar if applicable
              (if (prev_input_values$sd_se_bar_chart == "Standard deviation" && input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_boxplot_colour_fill_by_group == z_var){
                stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "errorbar",
                             aes(colour = factor(.data[[z_var]], levels = additional_group_levels)),
                             width = as.numeric(prev_input_values$box_width * 0.5),
                             size = prev_input_values$box_line_thickness)
              } else if (prev_input_values$sd_se_bar_chart == "Standard error of mean" && input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_boxplot_colour_fill_by_group == z_var){
                geom_errorbar(stat = "summary", fun.data = "mean_se", position = "dodge",
                              aes(colour = factor(.data[[z_var]], levels = additional_group_levels)),
                              width = as.numeric(prev_input_values$box_width * 0.5),
                              size = prev_input_values$box_line_thickness)
              } else if (prev_input_values$sd_se_bar_chart == "None" && input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_boxplot_colour_fill_by_group == z_var) {
                geom_errorbar(stat = "summary", fun.data = "none")
              } else if (prev_input_values$sd_se_bar_chart == "Standard deviation" && (prev_input_values$geom_boxplot_colour_fill_by_group == x_var || prev_input_values$geom_boxplot_colour_fill_by_group == "Single Colour")){
                stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "errorbar",
                             aes(colour = factor(.data[[x_var]], levels = group_levels)),
                             width = as.numeric(prev_input_values$box_width * 0.5),
                             size = prev_input_values$box_line_thickness)
              } else if (prev_input_values$sd_se_bar_chart == "Standard error of mean" && (prev_input_values$geom_boxplot_colour_fill_by_group == x_var || prev_input_values$geom_boxplot_colour_fill_by_group == "Single Colour")){
                geom_errorbar(stat = "summary", fun.data = "mean_se", position = "dodge",
                              aes(colour = factor(.data[[x_var]], levels = group_levels)),
                              width = as.numeric(prev_input_values$box_width * 0.5),
                              size = prev_input_values$box_line_thickness)
              } else if (prev_input_values$sd_se_bar_chart == "None" && (prev_input_values$geom_boxplot_colour_fill_by_group == x_var || prev_input_values$geom_boxplot_colour_fill_by_group == "Single Colour")) {
                geom_errorbar(stat = "summary", fun.data = "none")
              }) +
              
              # adds colour scale for geom_violin
              scale_fill_geom_boxplot +
              scale_border_geom_boxplot +
              new_scale_color() + new_scale_fill() +
              
              #adjust colour, shape, fill, size, edge thickness, transparency for datapoints
                # for additional grouping variable
                (if(input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE && prev_input_values$geom_jitter_colour_fill_by_group == z_var && prev_input_values$geom_boxplot_colour_fill_by_group == z_var) {
                  geom_point(aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                                 fill = factor(.data[[z_var]], levels = additional_group_levels)),
                             position = position_jitterdodge(jitter.width = prev_input_values$symbol_jitter),
                             size = prev_input_values$symbol_size,
                             shape = as.numeric(prev_input_values$symbol_shape),
                             stroke = prev_input_values$symbol_edge_thickness,
                             alpha = prev_input_values$symbol_transparency,
                             show.legend = ("Dots" %in% prev_input_values$legends)
                             )
                } else {
                  if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == z_var) {
  
  
                    geom_jitter(aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                                    fill = factor(.data[[z_var]], levels = additional_group_levels)),
                                size = prev_input_values$symbol_size,
                                shape = as.numeric(prev_input_values$symbol_shape),
                                stroke = prev_input_values$symbol_edge_thickness,
                                alpha = prev_input_values$symbol_transparency,
                                width = prev_input_values$symbol_jitter,
                                show.legend = ("Dots" %in% prev_input_values$legends)
                    )
                    # for initial grouping variable
                  } else if (prev_input_values$geom_jitter_colour_fill_by_group == x_var || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour"){
                    geom_jitter(aes(colour = factor(.data[[x_var]], levels = group_levels),
                                    fill = factor(.data[[x_var]], levels = group_levels)),
                                size = prev_input_values$symbol_size,
                                shape = as.numeric(as.numeric(prev_input_values$symbol_shape)),
                                stroke = prev_input_values$symbol_edge_thickness,
                                alpha = prev_input_values$symbol_transparency,
                                width = prev_input_values$symbol_jitter,
                                show.legend = ("Dots" %in% prev_input_values$legends)
                                )
  
                  } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == paste0(z_var, " ")){
                    geom_jitter(aes(colour = .data[[z_var]],
                                    fill = .data[[z_var]]),
                                size = prev_input_values$symbol_size,
                                shape = as.numeric(prev_input_values$symbol_shape),
                                stroke = prev_input_values$symbol_edge_thickness,
                                alpha = prev_input_values$symbol_transparency,
                                width = prev_input_values$symbol_jitter,
                                show.legend = ("Dots" %in% prev_input_values$legends)
                                )
                  } else if (prev_input_values$geom_jitter_colour_fill_by_group == paste0(x_var, " ")) {
                    geom_jitter(aes(colour = .data[[x_var]],
                                    fill = .data[[x_var]]),
                                size = prev_input_values$symbol_size,
                                shape = as.numeric(as.numeric(prev_input_values$symbol_shape)),
                                stroke = prev_input_values$symbol_edge_thickness,
                                alpha = prev_input_values$symbol_transparency,
                                width = prev_input_values$symbol_jitter,
                                show.legend = ("Dots" %in% prev_input_values$legends)
                                )
                  }
                })+
              
              # adds colour scalar for geom_jitter
              scale_fill_geom_jitter_jitter +
              scale_border_geom_jitter
            
          # For Raincloud Plot
          } else if (input$navpage != "Superplot" && input[[paste0("choose_", tab_id)]] == "Raincloud"){
            
            neuro_plot <-
              
            # if group_var is a factor
              (if (!is.numeric(filtered_data[[x_var]])){
                ggplot(filtered_data, aes(x = factor(.data[[x_var]], levels = group_levels), y = .data[[y_var]]))
                # if group_var is numeric
              } else if (is.numeric(filtered_data[[x_var]])) {
                ggplot(filtered_data, aes(x = .data[[x_var]], y = .data[[y_var]]))
              }) +
              
              # adjusts boxplot parameters
              # for additional grouping variable
              (if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_boxplot_colour_fill_by_group == z_var) {
                geom_boxplot(aes(colour = factor(.data[[z_var]], levels = additional_group_levels), 
                                 fill = factor(.data[[z_var]], levels = additional_group_levels)), 
                             size = prev_input_values$box_line_thickness, 
                             alpha = prev_input_values$box_transparency, 
                             width = prev_input_values$raincloud_box_width, 
                             notch = FALSE, 
                             outlier.shape = NA, 
                             coef = 1E100,
                             show.legend = "Box" %in% prev_input_values$legends)
                
              } else if (prev_input_values$geom_boxplot_colour_fill_by_group == x_var || prev_input_values$geom_boxplot_colour_fill_by_group == "Single Colour"){
                # for initial grouping variable
                geom_boxplot(aes(colour = factor(.data[[x_var]], levels = group_levels), 
                                 fill = factor(.data[[x_var]], levels = group_levels)), 
                             size = prev_input_values$box_line_thickness, 
                             alpha = prev_input_values$box_transparency, 
                             width = prev_input_values$raincloud_box_width, 
                             notch = FALSE, 
                             outlier.shape = NA, 
                             coef = 1E100,
                             show.legend = "Box" %in% prev_input_values$legends
                )
              }) +
              
              # colour scale for boxplot
              scale_fill_geom_boxplot +
              scale_border_geom_boxplot +
              new_scale_color() + new_scale_fill() +
            
  
              # Create the raincloud plot
                # for additional grouping variable
                (if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == z_var) {
                  # for halfeye distribution
                  stat_slab(width = prev_input_values$raincloud_halfeye_width,
                               adjust =  prev_input_values$raincloud_halfeye_line_thickness,
                               aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                                   fill = factor(.data[[z_var]], levels = additional_group_levels)),
                            alpha = prev_input_values$raincloud_halfeye_transparency,
                            justification = prev_input_values$raincloud_halfeye_justification,
                            show.legend = "Cloud" %in% prev_input_values$legends
                  ) 
                } else if (prev_input_values$geom_jitter_colour_fill_by_group == x_var || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour"){
                # for initial grouping variable
                  # for halfeye distribution
                  stat_slab(width = prev_input_values$raincloud_halfeye_width,
                               adjust =  prev_input_values$raincloud_halfeye_line_thickness,
                               aes(colour = factor(.data[[x_var]], levels = group_levels),
                                   fill = factor(.data[[x_var]], levels = group_levels)),
                           # aes( fill = factor(.data[[x_var]], levels = group_levels)),
                               alpha = prev_input_values$raincloud_halfeye_transparency,
                               justification = prev_input_values$raincloud_halfeye_justification,
                                show.legend = "Cloud" %in% prev_input_values$legends
                               ) 
                  })+
              
              # for additional grouping variable
                (if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == z_var) {
                    # for dots distribution
                    stat_dots(side = prev_input_values$stat_dots_side,
                              justification = prev_input_values$stat_dots_justification,
                              binwidth = as.numeric(prev_input_values$stat_dots_binwidth),
                              dotsize = prev_input_values$symbol_size,
                              aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                                  fill = factor(.data[[z_var]], levels = additional_group_levels)),
                              shape = as.numeric(as.numeric(prev_input_values$symbol_shape)),
                              stroke = prev_input_values$symbol_edge_thickness,
                              alpha = prev_input_values$symbol_transparency,
                              width = prev_input_values$symbol_jitter,
                              show.legend = "Dots" %in% prev_input_values$legends
                  )
                } else if (prev_input_values$geom_jitter_colour_fill_by_group == x_var || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour"){
                  # for initial grouping variable
                    # for dots distribution
                    stat_dots(side = prev_input_values$stat_dots_side,
                              justification = prev_input_values$stat_dots_justification,
                              binwidth = as.numeric(prev_input_values$stat_dots_binwidth),
                              dotsize = prev_input_values$symbol_size,
                              aes(colour = factor(.data[[x_var]], levels = group_levels),
                                  fill = factor(.data[[x_var]], levels = group_levels)),
                              shape = as.numeric(as.numeric(prev_input_values$symbol_shape)),
                              stroke = prev_input_values$symbol_edge_thickness,
                              alpha = prev_input_values$symbol_transparency,
                              width = prev_input_values$symbol_jitter#,
                              #show.legend = "Dots" %in% prev_input_values$legends
                              )
                })+
              
              # colour scaler for raincloud
              scale_fill_geom_jitter_jitter +
              scale_border_geom_jitter +
              new_scale_color() + new_scale_fill()
            
              
            
            
            
          # for Superplot Scatter
          } else if (input$navpage == "Superplot" && input[[paste0("choose_", tab_id)]] == "Scatter"){
            neuro_plot <-
              # if group_var is a factor
              (if (!is.numeric(filtered_data[[x_var]])){
                ggplot(filtered_data, aes(x = factor(.data[[x_var]], levels = group_levels), y = .data[[y_var]]))
                # if group_var is numeric
              } else if (is.numeric(filtered_data[[x_var]])) {
                ggplot(filtered_data, aes(x = .data[[x_var]], y = .data[[y_var]]))
              }) +
                
                #adjust colour, shape, fill, size, edge thickness, transparency for datapoints
                # for additional grouping variable
                (if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == z_var) {
                    geom_beeswarm(aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                                    fill = factor(.data[[z_var]], levels = additional_group_levels)),
                                size = prev_input_values$symbol_size,
                                cex = prev_input_values$symbol_jitter,
                                shape = as.numeric(prev_input_values$symbol_shape),
                                stroke = prev_input_values$symbol_edge_thickness,
                                alpha = prev_input_values$symbol_transparency,
                                method = "swarm",
                                show.legend = "Dots" %in% prev_input_values$legends
                    )
                    # for initial grouping variable
                  } else if (prev_input_values$geom_jitter_colour_fill_by_group == x_var || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour"){
                    geom_beeswarm(aes(colour = factor(.data[[x_var]], levels = group_levels),
                                    fill = factor(.data[[x_var]], levels = group_levels)),
                                size = prev_input_values$symbol_size,
                                cex = prev_input_values$symbol_jitter,
                                shape = as.numeric(as.numeric(prev_input_values$symbol_shape)),
                                stroke = prev_input_values$symbol_edge_thickness,
                                alpha = prev_input_values$symbol_transparency,
                                method = "swarm",
                                show.legend = "Dots" %in% prev_input_values$legends
                    )
                    
                  } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == paste0(z_var, " ")){
                    geom_beeswarm(aes(colour = .data[[z_var]],
                                    fill = .data[[z_var]]),
                                size = prev_input_values$symbol_size,
                                cex = prev_input_values$symbol_jitter,
                                shape = as.numeric(prev_input_values$symbol_shape),
                                stroke = prev_input_values$symbol_edge_thickness,
                                alpha = prev_input_values$symbol_transparency,
                                method = "swarm",
                                show.legend = "Dots" %in% prev_input_values$legends
                                )
                  } else if (prev_input_values$geom_jitter_colour_fill_by_group == paste0(x_var, " ")) {
                    geom_beeswarm(aes(colour = .data[[x_var]],
                                    fill = .data[[x_var]]),             
                                size = prev_input_values$symbol_size,
                                cex=prev_input_values$symbol_jitter,
                                shape = as.numeric(as.numeric(prev_input_values$symbol_shape)),
                                stroke = prev_input_values$symbol_edge_thickness,
                                alpha = prev_input_values$symbol_transparency,
                                method = "swarm",
                                show.legend = "Dots" %in% prev_input_values$legends
                                )
                  })+
               
                #adjust colour, shape, fill, size, edge thickness, transparency for averaged datapoints
                # for additional grouping variable
                (if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == z_var) {
                  geom_beeswarm(data = Replicate_means_or_medians,
                                aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                                    fill = factor(.data[[z_var]], levels = additional_group_levels)),
                                cex=prev_input_values$symbol_jitter_superplot,
                                shape = as.numeric(prev_input_values$symbol_shape),
                                stroke = prev_input_values$symbol_edge_thickness_superplot,
                                alpha = prev_input_values$symbol_transparency_superplot,
                                method = "swarm",
                                size = prev_input_values$symbol_size_superplot
                  )
                  # for initial grouping variable
                } else if (prev_input_values$geom_jitter_colour_fill_by_group == x_var || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour"){
                  geom_beeswarm(data = Replicate_means_or_medians,
                                aes(colour = factor(.data[[x_var]], levels = group_levels),
                                    fill = factor(.data[[x_var]], levels = group_levels)),
                                cex=prev_input_values$symbol_jitter_superplot,
                                shape = as.numeric(prev_input_values$symbol_shape),
                                stroke = prev_input_values$symbol_edge_thickness_superplot,
                                alpha = prev_input_values$symbol_transparency_superplot,
                                method = "swarm",
                                size = prev_input_values$symbol_size_superplot
                  )
                } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == paste0(z_var, " ")){
                  geom_beeswarm(data = Replicate_means_or_medians,
                                aes(colour = .data[[z_var]],
                                    fill = .data[[z_var]]),
                                cex=prev_input_values$symbol_jitter_superplot,
                                shape = as.numeric(prev_input_values$symbol_shape),
                                stroke = prev_input_values$symbol_edge_thickness_superplot,
                                alpha = prev_input_values$symbol_transparency_superplot,
                                method = "swarm",
                                size = prev_input_values$symbol_size_superplot
                  )
                } else if (prev_input_values$geom_jitter_colour_fill_by_group == paste0(x_var, " ")) {
                  geom_beeswarm(data = Replicate_means_or_medians,
                                aes(colour = .data[[x_var]],
                                    fill = .data[[x_var]]),
                                cex=prev_input_values$symbol_jitter_superplot,
                                shape = as.numeric(prev_input_values$symbol_shape),
                                stroke = prev_input_values$symbol_edge_thickness_superplot,
                                alpha = prev_input_values$symbol_transparency_superplot,
                                method = "swarm",
                                size = prev_input_values$symbol_size_superplot
                  )
                })+
              
                # adds colour scales
                scale_fill_geom_jitter_jitter +
                scale_border_geom_jitter +
                new_scale_color() + new_scale_fill()+
                
                # Show mean or median for each group
                stat_summary(data = Total_replicates, fun = function(x) {
                  if (input$mean_or_median_superplot == "mean") {
                    return(mean(x, na.rm = TRUE))
                  } else if (input$mean_or_median_superplot == "median"){
                    return(median(x, na.rm = TRUE))
                  }
                }, 
                geom = "crossbar", 
                linewidth = prev_input_values$box_line_thickness,
                width = as.numeric(prev_input_values$box_width * 0.5),
                colour = "black") +

                # Adds the SD or interquartile range (IQR) for each group
                (if (input$mean_or_median_superplot == "mean") {
                  stat_summary(data = Replicate_means_or_medians, 
                               fun.data = "mean_sdl", 
                               fun.args = list(mult = 1), 
                               geom = "errorbar", 
                               width = as.numeric(prev_input_values$box_width * 0.5), 
                               colour = "black")
                } else if (input$mean_or_median_superplot == "median"){
                  stat_summary(data = Replicate_means_or_medians, 
                               fun.data = median_IQR, 
                               fun.args = NULL, 
                               geom = "errorbar", 
                               width = as.numeric(prev_input_values$box_width * 0.5), 
                               colour = "black")
                })
               
            # for Superplot Scatter
          } else if (input$navpage == "Superplot" && input[[paste0("choose_", tab_id)]] == "Raincloud"){
            
            neuro_plot <-
            # if group_var is a factor
            (if (!is.numeric(filtered_data[[x_var]])){
              ggplot(filtered_data, aes(x = factor(.data[[x_var]], levels = group_levels), y = .data[[y_var]]))
              # if group_var is numeric
            } else if (is.numeric(filtered_data[[x_var]])) {
              ggplot(filtered_data, aes(x = .data[[x_var]], y = .data[[y_var]]))
            }) +
              
              #adjust colour, shape, fill, size, edge thickness, transparency for datapoints
              # for additional grouping variable
              (if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == z_var) {
                geom_beeswarm(aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                                  fill = factor(.data[[z_var]], levels = additional_group_levels)),
                              size = prev_input_values$symbol_size,
                              cex = prev_input_values$symbol_jitter,
                              shape = as.numeric(prev_input_values$symbol_shape),
                              stroke = prev_input_values$symbol_edge_thickness,
                              alpha = prev_input_values$symbol_transparency,
                              method = "swarm",
                              show.legend = "Dots" %in% prev_input_values$legends
                )
                # for initial grouping variable
              } else if (prev_input_values$geom_jitter_colour_fill_by_group == x_var || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour"){
                geom_beeswarm(aes(colour = factor(.data[[x_var]], levels = group_levels),
                                  fill = factor(.data[[x_var]], levels = group_levels)),
                              size = prev_input_values$symbol_size,
                              cex = prev_input_values$symbol_jitter,
                              shape = as.numeric(as.numeric(prev_input_values$symbol_shape)),
                              stroke = prev_input_values$symbol_edge_thickness,
                              alpha = prev_input_values$symbol_transparency,
                              method = "swarm",
                              show.legend = "Dots" %in% prev_input_values$legends
                              )
                
              } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == paste0(z_var, " ")){
                geom_beeswarm(aes(colour = .data[[z_var]],
                                  fill = .data[[z_var]]),
                              size = prev_input_values$symbol_size,
                              cex= prev_input_values$symbol_jitter,
                              shape = as.numeric(prev_input_values$symbol_shape),
                              stroke = prev_input_values$symbol_edge_thickness,
                              alpha = prev_input_values$symbol_transparency,
                              method = "swarm",
                              show.legend = "Dots" %in% prev_input_values$legends
                              )
              } else if (prev_input_values$geom_jitter_colour_fill_by_group == paste0(x_var, " ")) {
                geom_beeswarm(aes(colour = .data[[x_var]],
                                  fill = .data[[x_var]]),             
                              size = prev_input_values$symbol_size,
                              cex = prev_input_values$symbol_jitter,
                              shape = as.numeric(as.numeric(prev_input_values$symbol_shape)),
                              stroke = prev_input_values$symbol_edge_thickness,
                              alpha = prev_input_values$symbol_transparency,
                              method = "swarm",
                              show.legend = "Dots" %in% prev_input_values$legends
                              )
              })+
              
              #adjust colour, shape, fill, size, edge thickness, transparency for averaged datapoints
              # for additional grouping variable
              (if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == z_var) {
                geom_beeswarm(data = Replicate_means_or_medians,
                              aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                                  fill = factor(.data[[z_var]], levels = additional_group_levels)),
                              cex = prev_input_values$symbol_jitter_superplot,
                              shape = as.numeric(prev_input_values$symbol_shape),
                              stroke = prev_input_values$symbol_edge_thickness_superplot,
                              alpha = prev_input_values$symbol_transparency_superplot,
                              method = "swarm",
                              size = 5 * prev_input_values$symbol_size_superplot
                )
                # for initial grouping variable
              } else if (prev_input_values$geom_jitter_colour_fill_by_group == x_var || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour"){
                geom_beeswarm(data = Replicate_means_or_medians,
                              aes(colour = factor(.data[[x_var]], levels = group_levels),
                                  fill = factor(.data[[x_var]], levels = group_levels)),
                              cex = prev_input_values$symbol_jitter_superplot,
                              shape = as.numeric(prev_input_values$symbol_shape),
                              stroke = prev_input_values$symbol_edge_thickness_superplot,
                              alpha = prev_input_values$symbol_transparency_superplot,
                              method = "swarm",
                              size = 5 * prev_input_values$symbol_size_superplot
                )
              } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == paste0(z_var, " ")){
                geom_beeswarm(data = Replicate_means_or_medians,
                              aes(colour = .data[[z_var]],
                                  fill = .data[[z_var]]),
                              cex = prev_input_values$symbol_jitter_superplot,
                              shape = as.numeric(prev_input_values$symbol_shape),
                              stroke = prev_input_values$symbol_edge_thickness_superplot,
                              alpha = prev_input_values$symbol_transparency_superplot,
                              method = "swarm",
                              size = 5 * prev_input_values$symbol_size_superplot
                )
              } else if (prev_input_values$geom_jitter_colour_fill_by_group == paste0(x_var, " ")) {
                geom_beeswarm(data = Replicate_means_or_medians,
                              aes(colour = .data[[x_var]],
                                  fill = .data[[x_var]]),
                              cex = prev_input_values$symbol_jitter_superplot,
                              shape = as.numeric(prev_input_values$symbol_shape),
                              stroke = prev_input_values$symbol_edge_thickness_superplot,
                              alpha = prev_input_values$symbol_transparency_superplot,
                              method = "swarm",
                              size = 5 * prev_input_values$symbol_size_superplot
                )
              })+
              
              # adds colour scales
              scale_fill_geom_jitter_jitter +
              scale_border_geom_jitter +
              new_scale_color() + new_scale_fill()+
              
              # Show mean or median for each group
              stat_summary(data = Total_replicates, fun = function(x) {
                if (input$mean_or_median_superplot == "mean") {
                  return(mean(x, na.rm = TRUE))
                } else if (input$mean_or_median_superplot == "median"){
                  return(median(x, na.rm = TRUE))
                }
              }, 
              geom = "crossbar", 
              linewidth = prev_input_values$box_line_thickness,
              width = as.numeric(prev_input_values$box_width * 0.5),
              colour = "black") +

              # Adds the SD or interquartile range (IQR) for each group
              (if (input$mean_or_median_superplot == "mean") {
                stat_summary(data = Replicate_means_or_medians, 
                             fun.data = "mean_sdl", 
                             fun.args = list(mult = 1), 
                             geom = "errorbar", 
                             width = as.numeric(prev_input_values$box_width * 0.5), 
                             colour = "black")
                #aes(colour = factor(.data[[z_var]], levels = additional_group_levels)))
              } else if (input$mean_or_median_superplot == "median"){
                stat_summary(data = Replicate_means_or_medians, 
                             fun.data = median_IQR, 
                             fun.args = NULL, 
                             geom = "errorbar", 
                             width = as.numeric(prev_input_values$box_width * 0.5), 
                             colour = "black")
              }) +
              
              # Create the raincloud plot
              # for additional grouping variable
              (if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == z_var) {
                # for halfeye distribution
                stat_slab(width = prev_input_values$raincloud_halfeye_width,
                             adjust =  prev_input_values$raincloud_halfeye_line_thickness,
                             aes(colour = factor(.data[[z_var]], levels = additional_group_levels),
                                   fill = factor(.data[[z_var]], levels = additional_group_levels)),
                            alpha = prev_input_values$raincloud_halfeye_transparency,
                            justification = prev_input_values$raincloud_halfeye_justification,
                            show.legend = "Cloud" %in% prev_input_values$legends
                  ) 
              } else if (prev_input_values$geom_jitter_colour_fill_by_group == x_var || prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour"){
                # for initial grouping variable
                # for halfeye distribution
                stat_slab(width = prev_input_values$raincloud_halfeye_width,
                             adjust =  prev_input_values$raincloud_halfeye_line_thickness,
                             aes(colour = factor(.data[[x_var]], levels = group_levels),
                                 fill = factor(.data[[x_var]], levels = group_levels)),
                            alpha = prev_input_values$raincloud_halfeye_transparency,
                            justification = prev_input_values$raincloud_halfeye_justification,
                            show.legend = "Cloud" %in% prev_input_values$legends
                  ) 
              } else if (input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE &&  prev_input_values$geom_jitter_colour_fill_by_group == paste0(z_var, " ")){
                stat_slab(width = prev_input_values$raincloud_halfeye_width,
                             adjust =  prev_input_values$raincloud_halfeye_line_thickness,
                             aes(colour = .data[[z_var]],
                                 fill = .data[[z_var]]),
                            alpha = prev_input_values$raincloud_halfeye_transparency,
                            justification = prev_input_values$raincloud_halfeye_justification,
                            show.legend = "Cloud" %in% prev_input_values$legends
                  ) 
              } else if (prev_input_values$geom_jitter_colour_fill_by_group == paste0(x_var, " ")) {
                stat_halfeye(width = prev_input_values$raincloud_halfeye_width,
                             adjust =  prev_input_values$raincloud_halfeye_line_thickness,
                             aes(colour = .data[[x_var]],
                                 fill =.data[[x_var]]),
                               alpha = prev_input_values$raincloud_halfeye_transparency,
                               justification = prev_input_values$raincloud_halfeye_justification,
                               show.legend = "Cloud" %in% prev_input_values$legends
                  ) 
              })
          }
          ######################################################################
          # adds common elements to the main plot
          
          neuro_plot <- neuro_plot +
            
            # x and y labels
            labs(x = prev_input_values$x_axis_text_title, y = prev_input_values$y_axis_text_title) +
            
            # adjusts x axis scale based on selection
            (if (prev_input_values$x_scale_type == "linear") {
              #adjust minor and major X scale ticks
              #requires x range selection and values for major and minor ticks
              if (!is.na(prev_input_values$x_start_range) && !is.na(prev_input_values$x_end_range) && !is.na(prev_input_values$x_minor_ticks_interval) && !is.na(prev_input_values$x_minor_ticks_interval) && is.numeric(filtered_data[[x_var]])) {
                scale_x_continuous(guide = "prism_minor", limits = c(prev_input_values$x_start_range, prev_input_values$x_end_range), minor_breaks = seq(prev_input_values$x_start_range, prev_input_values$x_end_range, prev_input_values$x_minor_ticks_interval), breaks = seq(prev_input_values$x_start_range, prev_input_values$x_end_range, prev_input_values$x_major_ticks_interval))
              } else if (!is.na(prev_input_values$x_start_range) &&  (!is.na(prev_input_values$x_end_range)) && (is.na(prev_input_values$x_minor_ticks_interval)) && (!is.na(prev_input_values$x_major_ticks_interval)) && is.numeric(filtered_data[[x_var]])){
                scale_x_continuous(limits = c(prev_input_values$x_start_range, prev_input_values$x_end_range), breaks = seq(prev_input_values$x_start_range, prev_input_values$x_end_range, prev_input_values$x_major_ticks_interval))
                # without initial x range selection and no values for major and minor ticks
              } else {
                coord_cartesian(xlim = c(prev_input_values$x_start_range, prev_input_values$x_end_range))
              }
              # for log10 scale
            } else if (prev_input_values$x_scale_type == "log10") {
              #error message in case interval range includes value smaller than zero,
              if (!is.na(prev_input_values$x_start_range) && !is.na(prev_input_values$x_end_range)) {
                if (is.numeric(prev_input_values$x_start_range) && is.numeric(prev_input_values$x_end_range)) {
                  if (prev_input_values$x_start_range >= prev_input_values$x_end_range) {
                    output[[paste0("error_message_x_scale_", tab_id)]] <- renderText({"Default log10 scale applied - Start range must be less than end range"})
                  } else if (prev_input_values$x_start_range <= 0 || prev_input_values$x_end_range <= 0) {
                    output[[paste0("error_message_x_scale_", tab_id)]] <- renderText({"Default log10 scale applied - Please select a range including positive integers"})
                  } else {
                    output[[paste0("error_message_x_scale_", tab_id)]] <- renderText({NULL})
                  }
                } else {
                  output[[paste0("error_message_x_scale_", tab_id)]] <- renderText({"Default log10 scale applied - Values must be numeric"})
                }
              } else {
                output[[paste0("error_message_x_scale_", tab_id)]] <- renderText({"Default log10 scale applied - insert values for user-defined log10 scale"})
              }
              
              #requires x range selection
              if (!is.na(prev_input_values$x_start_range) && prev_input_values$x_start_range > 0 &&  (!is.na(prev_input_values$x_end_range)) && prev_input_values$x_end_range > 0 && is.numeric(filtered_data[[x_var]]) && isTRUE(prev_input_values$x_start_range < prev_input_values$x_end_range)) {
                # Calculate the range of the log10 scale
                log10_range <- log10(c(prev_input_values$x_start_range, prev_input_values$x_end_range))
                
                # Custom function to calculate minor breaks for log10 scale
                custom_minor_breaks <- function(range, n_minor = 9) {
                  log_range <- log10(range)
                  major_breaks <- seq(ceiling(log_range[1]), floor(log_range[2]), by = 1)
                  minor_breaks <- lapply(seq_along(major_breaks), function(i) {
                    seq(10^(major_breaks[i]), 10^(major_breaks[i] + 1), length.out = n_minor + 1)[-1]
                  })
                  unlist(minor_breaks)
                }
                
                # Calculate the minor breaks
                minor_breaks <- custom_minor_breaks(c(prev_input_values$x_start_range, prev_input_values$x_end_range))
                # Sets scale with major and minor breaks based on input$numeric_display_type_y_axis
                if (prev_input_values$numeric_display_type_y_axis == "Decimal") {
                  scale_x_continuous(guide = "prism_minor", trans = 'log10', limits = c(prev_input_values$x_start_range, prev_input_values$x_end_range), breaks = trans_breaks('log10', function(x) 10^x, n = log10_range[2] - log10_range[1] + 1), minor_breaks = minor_breaks)
                } else if (prev_input_values$numeric_display_type_y_axis == "Scientific") {
                  scale_x_continuous(guide = "prism_minor", trans = 'log10', limits = c(prev_input_values$x_start_range, prev_input_values$x_end_range), breaks = trans_breaks('log10', function(x) 10^x, n = log10_range[2] - log10_range[1] + 1), labels = scales::trans_format("log10", scales::math_format(10^.x)), minor_breaks = minor_breaks)
                }
              } else if ((is.na(prev_input_values$x_start_range) || (is.na(prev_input_values$x_end_range))) && is.numeric(filtered_data[[x_var]])) {
                scale_x_log10()
              } else if (!is.numeric(filtered_data[[x_var]])) {
                coord_cartesian(xlim = c(prev_input_values$x_start_range, prev_input_values$x_end_range))
              }
              #For log2 scale
            } else if (prev_input_values$x_scale_type == "log2") {
              #error message in case interval range includes value smaller than zero
              if (!is.na(prev_input_values$x_start_range) && !is.na(prev_input_values$x_end_range)) {
                if (is.numeric(prev_input_values$x_start_range) && is.numeric(prev_input_values$x_end_range)) {
                  if (prev_input_values$x_start_range >= prev_input_values$x_end_range) {
                    output[[paste0("error_message_x_scale_", tab_id)]] <- renderText({"Default log2 scale applied - Start range must be less than end range"})
                  } else if (prev_input_values$x_start_range <= 0 || prev_input_values$x_end_range <= 0) {
                    output[[paste0("error_message_x_scale_", tab_id)]] <- renderText({"Default log2 scale applied - Please select a range including positive integers"})
                  } else {
                    output[[paste0("error_message_x_scale_", tab_id)]] <- renderText({NULL})
                  }
                } else {
                  output[[paste0("error_message_x_scale_", tab_id)]] <- renderText({"Default log2 scale applied - Values must be numeric"})
                }
              } else {
                output[[paste0("error_message_x_scale_", tab_id)]] <- renderText({"Default log2 scale applied - insert values for user-defined log2 scale"})
              }
              #requires x range selection and values for major and minor ticks
              if (!is.na(prev_input_values$x_start_range) && prev_input_values$x_start_range > 0 &&  (!is.na(prev_input_values$x_end_range)) && prev_input_values$x_end_range > 0 && is.numeric(filtered_data[[x_var]]) && isTRUE(prev_input_values$x_start_range < prev_input_values$x_end_range)) {
                # Calculate the range of the log2 scale
                log2_range <- log2(c(prev_input_values$x_start_range, prev_input_values$x_end_range))
                # Sets scale with major and minor breaks based on input$numeric_display_type_y_axis
                if (prev_input_values$numeric_display_type_y_axis == "Decimal") {
                  scale_x_continuous(trans = 'log2', limits = c(prev_input_values$x_start_range, prev_input_values$x_end_range), breaks = trans_breaks('log2', function(x) 2^x, n = log2_range[2] - log2_range[1] + 1))
                } else if (prev_input_values$numeric_display_type_y_axis == "Scientific") {
                  scale_x_continuous(trans='log2', limits = c(prev_input_values$x_start_range, prev_input_values$x_end_range), breaks=trans_breaks('log2', function(x) 2^x, n = log2_range[2] - log2_range[1] + 1), labels = scales::trans_format("log2", scales::math_format(2^.x)))
                }
              } else if (is.na(prev_input_values$x_start_range) && (is.na(prev_input_values$x_end_range)) && is.numeric(filtered_data[[x_var]])) {
                scale_x_continuous(trans='log2')
              } else if (!is.numeric(filtered_data[[x_var]])) {
                coord_cartesian(xlim = c(prev_input_values$x_start_range, prev_input_values$x_end_range))
              }
            }) +
            
            # adjusts y axis scale based on selection
            #For linear scale
            (if (prev_input_values$y_scale_type == "linear") {
              # adjust minor and major Y scale ticks
              # requires y range selection and values for major and minor ticks
              if (!is.na(prev_input_values$y_start_range) &&  !is.na(prev_input_values$y_end_range) && !is.na(prev_input_values$y_minor_ticks_interval) && !is.na(prev_input_values$y_major_ticks_interval) && is.numeric(filtered_data[[y_var]])) {
                scale_y_continuous(guide = "prism_minor", limits = c(prev_input_values$y_start_range, prev_input_values$y_end_range), minor_breaks = seq(prev_input_values$y_start_range, prev_input_values$y_end_range, prev_input_values$y_minor_ticks_interval), breaks = seq(prev_input_values$y_start_range, prev_input_values$y_end_range, prev_input_values$y_major_ticks_interval))
                # without initial y range selection and no values for major and minor ticks
              } else {
                scale_y_continuous(limits = c(prev_input_values$y_start_range, prev_input_values$y_end_range))
              }
              #For log10 scale
            } else if (prev_input_values$y_scale_type == "log10") {
              #error message in case interval range includes value smaller than zero
              if (!is.na(prev_input_values$y_start_range) && !is.na(prev_input_values$y_end_range)) {
                if (is.numeric(prev_input_values$y_start_range) && is.numeric(prev_input_values$y_end_range)) {
                  if (prev_input_values$y_start_range >= prev_input_values$y_end_range) {
                    output[[paste0("error_message_y_scale_", tab_id)]] <- renderText({"Default log10 scale applied - Start range must be less than end range"})
                  } else if (prev_input_values$y_start_range <= 0 || prev_input_values$y_end_range <= 0) {
                    output[[paste0("error_message_y_scale_", tab_id)]] <- renderText({"Default log10 scale applied - Please select a range including positive integers"})
                  } else {
                    output[[paste0("error_message_y_scale_", tab_id)]] <- renderText({NULL})
                  }
                } else {
                  output[[paste0("error_message_y_scale_", tab_id)]] <- renderText({"Default log10 scale applied - Values must be numeric"})
                }
              } else {
                output[[paste0("error_message_y_scale_", tab_id)]] <- renderText({"Default log10 scale applied - insert values for user-defined log10 scale"})
              }
              # requires y range selection
              if (!is.na(prev_input_values$y_start_range) && prev_input_values$y_start_range > 0 &&  (!is.na(prev_input_values$y_end_range)) && prev_input_values$y_end_range > 0 && is.numeric(filtered_data[[y_var]]) && isTRUE(prev_input_values$y_start_range < prev_input_values$y_end_range)) {
                # Calculate the range of the log10 scale
                log10_range <- log10(c(prev_input_values$y_start_range, prev_input_values$y_end_range))
                
                # Custom function to calculate minor breaks for log10 scale
                custom_minor_breaks <- function(range, n_minor = 9) {
                  log_range <- log10(range)
                  major_breaks <- seq(ceiling(log_range[1]), floor(log_range[2]), by = 1)
                  minor_breaks <- lapply(seq_along(major_breaks), function(i) {
                    seq(10^(major_breaks[i]), 10^(major_breaks[i] + 1), length.out = n_minor + 1)[-1]
                  })
                  unlist(minor_breaks)
                }
                
                # Calculate the minor breaks
                minor_breaks <- custom_minor_breaks(c(prev_input_values$y_start_range, prev_input_values$y_end_range))
                
                # Sets scale with major and minor breaks based on input$numeric_display_type_y_axis
                if (prev_input_values$numeric_display_type_y_axis == "Decimal") {
                  scale_y_continuous(guide = "prism_minor", trans = 'log10', limits = c(prev_input_values$y_start_range, prev_input_values$y_end_range), breaks = trans_breaks('log10', function(x) 10^x, n = log10_range[2] - log10_range[1] + 1), minor_breaks = minor_breaks)
                } else if (prev_input_values$numeric_display_type_y_axis == "Scientific") {
                  scale_y_continuous(guide = "prism_minor", trans = 'log10', limits = c(prev_input_values$y_start_range, prev_input_values$y_end_range), breaks = trans_breaks('log10', function(x) 10^x, n = log10_range[2] - log10_range[1] + 1), labels = scales::trans_format("log10", scales::math_format(10^.x)), minor_breaks = minor_breaks)
                }
              } else {
                # without initial y range selection and no values for major and minor ticks
                scale_y_log10()
              }
              #For log2 scale
            } else if (prev_input_values$y_scale_type == "log2") {
              # requires y range selection
              if (!is.na(prev_input_values$y_start_range) && prev_input_values$y_start_range > 0 &&  !is.na(prev_input_values$y_end_range) && prev_input_values$y_end_range > 0 && is.numeric(filtered_data[[y_var]]) && isTRUE(prev_input_values$y_start_range < prev_input_values$y_end_range)) {
                # Calculate the range of the log2 scale
                log2_range <- log2(c(prev_input_values$y_start_range, prev_input_values$y_end_range))
                # Sets scale with major and minor breaks based on input$numeric_display_type_y_axis
                if (prev_input_values$numeric_display_type_y_axis == "Decimal") {
                  scale_y_continuous(trans = 'log2', limits = c(prev_input_values$y_start_range, prev_input_values$y_end_range), breaks = trans_breaks('log2', function(x) 2^x, n = log2_range[2] - log2_range[1] + 1))
                } else if (prev_input_values$numeric_display_type_y_axis == "Scientific") {
                  scale_y_continuous(trans='log2', limits = c(prev_input_values$y_start_range, prev_input_values$y_end_range), breaks=trans_breaks('log2', function(x) 2^x, n = log2_range[2] - log2_range[1] + 1), labels = scales::trans_format("log2", scales::math_format(2^.x)))
                }
              } else {
                # without initial y range selection and no values for major and minor ticks
                scale_y_continuous(trans='log2')
              }
            }) +

            # adds facet if applicable for ICC plot
            (if (input$num_levels == 3 && input$tabselected2 == 'Linear Mixed Model' && input$navpage == "Intraclass correlation & Linear Mixed Models") {
              facet_wrap(as.formula(paste0("~", w_var)), labeller = label_value)
            } else if (input$num_levels == 4 && input$tabselected2 == 'Linear Mixed Model' && input$navpage == "Intraclass correlation & Linear Mixed Models"){
              facet_grid(as.formula(paste0("~", extra_var, "~", w_var)), labeller = label_value)
            }) +
            
            theme(
              # Change plot background
              panel.background = element_rect(fill = prev_input_values$colour_background, color = prev_input_values$colour_background_border, size = prev_input_values$colour_background_border_thickness),
              plot.background = element_rect(fill = prev_input_values$colour_panel),
              # Change X axis elements
              axis.line.x = element_line(color =  prev_input_values$x_line_colour, size =  prev_input_values$x_line_thickness), #line
              axis.ticks.x = element_line(color = prev_input_values$x_tick_colour, size = prev_input_values$x_tick_thickness), #ticks
              axis.ticks.length.x = unit(prev_input_values$x_tick_length, "pt"), # Change the length of minor ticks
              prism.ticks.length.x = unit(prev_input_values$x_tick_minor_length, "pt"),
              axis.text.x = element_text(size = prev_input_values$x_text_size, color = prev_input_values$x_text_colour, family = prev_input_values$x_text_font),
              # Change Y axis elements
              axis.line.y = element_line(color =  prev_input_values$y_line_colour, size =  prev_input_values$y_line_thickness), #line
              axis.ticks.y = element_line(color = prev_input_values$y_tick_colour, size = prev_input_values$y_tick_thickness), #ticks
              axis.ticks.length.y = unit(prev_input_values$y_tick_length, "pt"), # Change the length of minor ticks
              prism.ticks.length.y = unit(prev_input_values$y_tick_minor_length, "pt"),
              axis.text.y = element_text(size = prev_input_values$y_text_size, color = prev_input_values$y_text_colour, family = prev_input_values$y_text_font),
              # Change grid lines
              panel.grid.major.x = element_line(color = prev_input_values$colour_major_x_grid, size =  prev_input_values$thickness_major_x_grid),
              panel.grid.minor.x = element_line(color = prev_input_values$colour_minor_x_grid, size = prev_input_values$thickness_minor_x_grid),
              panel.grid.major.y = element_line(color = prev_input_values$colour_major_y_grid, size = prev_input_values$thickness_major_y_grid),
              panel.grid.minor.y = element_line(color = prev_input_values$colour_minor_y_grid, size = prev_input_values$thickness_minor_y_grid),
              # Change x and y axes text
              axis.title.x = element_text(size = prev_input_values$x_axis_text_size, color = prev_input_values$x_axis_text_colour, face = prev_input_values$x_axis_text_face, family = prev_input_values$x_axis_text_font, hjust = prev_input_values$x_axis_text_justification, margin = margin(t = prev_input_values$x_axis_text_margin)),
              axis.title.y.left = element_text(size = prev_input_values$y_axis_text_size, color = prev_input_values$y_axis_text_colour, face = prev_input_values$y_axis_text_face, family = prev_input_values$y_axis_text_font, hjust = prev_input_values$y_axis_text_justification, margin = margin(r = prev_input_values$y_axis_text_margin)),
              #removes or adjusts legend position
              legend.position = legend_position_selection,
              legend.title = element_text(size = prev_input_values$marginal_plot_legend_text_size, family = prev_input_values$marginal_plot_legend_text_font, face = prev_input_values$marginal_plot_legend_text_face),
              legend.text = element_text(size = prev_input_values$marginal_plot_legend_text_size, family = prev_input_values$marginal_plot_legend_text_font, face = prev_input_values$marginal_plot_legend_text_face)
            ) +

            if (prev_input_values$marginal_plot_legend_title == FALSE) {
              theme (legend.title = element_blank())
            }
          
          # adds additional info for scatter plot
          if (input$navpage != "Superplot" && input[[paste0("choose_", tab_id)]] == "Scatter") {
            
            # adds trendline if selected (NEEDS FIXING)
            if (input[[paste0("geom_smooth_scatter_", tab_id)]] == TRUE && identical(prev_input_values$geom_jitter_colour_fill_by_group, z_var) && (is.numeric(filtered_data[[x_var]]) && is.numeric(filtered_data[[y_var]]))){
               
             # tryCatch({
              neuro_plot <- neuro_plot + geom_smooth(data = filtered_data, 
                                                     mapping = aes(x = .data[[x_var]],
                                                                   y = .data[[y_var]],
                                                                   group = factor(.data[[z_var]]),
                                                                   colour = factor(.data[[z_var]]),
                                                                   fill = factor(.data[[z_var]])),
                                                     alpha = prev_input_values$transparency_geom_smooth,
                                                     linewidth = prev_input_values$line_thickness_geom_smooth,
                                                     linetype = prev_input_values$line_type_geom_smooth,
                                                     method =  prev_input_values$method_geom_smooth_scatter,
                                                     span =  prev_input_values$span_geom_smooth,
                                                     se = TRUE,
                                                     level =  prev_input_values$confidence_interval_geom_smooth) +
                
                # adds correlation coefficient legend
                (if (prev_input_values$correlation_coefficient_show == TRUE){
                  stat_cor(mapping = aes(x = .data[[x_var]], y = .data[[y_var]], group = .data[[z_var]], colour = .data[[z_var]]),
                           method =  prev_input_values$correlation_coefficient_method,
                           cor.coef.name = prev_input_values$correlation_coefficient_name,
                           label.x.npc = prev_input_values$correlation_coefficient_legend_position_x,
                           label.y.npc = prev_input_values$correlation_coefficient_legend_position_y,
                           size = prev_input_values$correlation_coefficient_text_size, 
                           family = prev_input_values$correlation_coefficient_text_font)
                })

            } else if (input[[paste0("geom_smooth_scatter_", tab_id)]] == TRUE && !identical(prev_input_values$geom_jitter_colour_fill_by_group, z_var) && (is.numeric(filtered_data[[x_var]]) && is.numeric(filtered_data[[y_var]]))){
              neuro_plot <- neuro_plot + geom_smooth(fill = prev_input_values$fill_colour_geom_smooth,
                                                     colour = prev_input_values$line_colour_geom_smooth ,
                                                     alpha = prev_input_values$transparency_geom_smooth,
                                                     linewidth = prev_input_values$line_thickness_geom_smooth,
                                                     linetype = prev_input_values$line_type_geom_smooth,
                                                     method =  prev_input_values$method_geom_smooth_scatter,
                                                     span =  prev_input_values$span_geom_smooth,
                                                     se = TRUE,
                                                     level =  prev_input_values$confidence_interval_geom_smooth) +
                
                # adds correlation coefficient legend
                (if (prev_input_values$correlation_coefficient_show == TRUE){
                  stat_cor(method =  prev_input_values$correlation_coefficient_method,
                           cor.coef.name = prev_input_values$correlation_coefficient_name,
                           label.x.npc = prev_input_values$correlation_coefficient_legend_position_x,
                           label.y.npc = prev_input_values$correlation_coefficient_legend_position_y,
                           size = prev_input_values$correlation_coefficient_text_size,
                           family = prev_input_values$correlation_coefficient_text_font)
                })
              # adds correlation coefficient legend
                
            } else if (input[[paste0("geom_smooth_scatter_", tab_id)]] == TRUE && (!is.numeric(filtered_data[[x_var]]) || !is.numeric(filtered_data[[y_var]]))) {
              # shows error message
              showToast(
                session = shiny::getDefaultReactiveDomain(), input = input,
                text = "Selection for trendline needs to be numeric",
                type = "error",
                position = "bottom-left"
              )
              # updates checkbox input
              updateCheckboxInput(session, paste0("geom_smooth_scatter_", tab_id), value = FALSE)
              
              return(neuro_plot)
            }
            
              
            # adds marginal plots if selected
            if (input[[paste0("maginal_plot_", tab_id)]] == FALSE && input[[paste0("show_additional_group_legend_", tab_id)]] == FALSE) {
              return(neuro_plot)
            } else if (input[[paste0("maginal_plot_", tab_id)]] == TRUE && prev_input_values$geom_jitter_colour_fill_by_group == "Single Colour") {
              # without additional group selection
              neuro_plot_ggmarginal <- ggMarginal(neuro_plot, type = prev_input_values$marginal_plot_type,
                                                  size = prev_input_values$marginal_plot_size,
                                                  margins = prev_input_values$marginal_plot_margins,
                                                  xparams = list(binwidth = prev_input_values$marginal_plot_bindwidth_x, fill = prev_input_values$marginal_plot_fill_colour_x, color = prev_input_values$marginal_plot_border_colour_x, size = prev_input_values$marginal_plot_line_width_x, alpha = prev_input_values$marginal_plot_transparency_x),
                                                  yparams = list(binwidth = prev_input_values$marginal_plot_bindwidth_y, fill = prev_input_values$marginal_plot_fill_colour_y, color = prev_input_values$marginal_plot_border_colour_y, size = prev_input_values$marginal_plot_line_width_y, alpha = prev_input_values$marginal_plot_transparency_y)
              )
              # with additional group selection
            } else if (input[[paste0("maginal_plot_", tab_id)]] == TRUE && (prev_input_values$geom_jitter_colour_fill_by_group == x_var || prev_input_values$geom_jitter_colour_fill_by_group == z_var)) { #input[[paste0("show_additional_group_legend_", tab_id)]] == TRUE) {
              # uses tryCatch to deal with error when gradient is selected instead of discrete
              
              neuro_plot_ggmarginal <- ggMarginal(neuro_plot, type = prev_input_values$marginal_plot_type,
                                                  size = prev_input_values$marginal_plot_size,
                                                  groupFill = TRUE,
                                                  groupColour = TRUE,
                                                  margins = prev_input_values$marginal_plot_margins,
                                                  xparams = list(binwidth = prev_input_values$marginal_plot_bindwidth_x, fill = prev_input_values$marginal_plot_fill_colour_x, color = prev_input_values$marginal_plot_border_colour_x, size = prev_input_values$marginal_plot_line_width_x, alpha = prev_input_values$marginal_plot_transparency_x),
                                                  yparams = list(binwidth = prev_input_values$marginal_plot_bindwidth_y, fill = prev_input_values$marginal_plot_fill_colour_y, color = prev_input_values$marginal_plot_border_colour_y, size = prev_input_values$marginal_plot_line_width_y, alpha = prev_input_values$marginal_plot_transparency_y)
              )
            } else if (input[[paste0("maginal_plot_", tab_id)]] == TRUE && (prev_input_values$geom_jitter_colour_fill_by_group != x_var || prev_input_values$geom_jitter_colour_fill_by_group != z_var || prev_input_values$geom_jitter_colour_fill_by_group != "Single Colour")){
              output[[paste0("symbol_gradient_error_", tab_id)]] <- renderText({"Use discrete colour selection for marginal plots"})
              showToast(
                session = shiny::getDefaultReactiveDomain(), input = input,
                text = "Please select discrete scale for marginal plots",
                type = "error",
                position = "bottom-left"
              )
              return(neuro_plot)
            }
          }
          
          # because of ggMarginal, for some reactive operations, both plots need to be return
          if (input$navpage != "Superplot" && input[[paste0("choose_", tab_id)]] == "Scatter" && input[[paste0("maginal_plot_", tab_id)]] == TRUE){
            return(list(neuro_plot, neuro_plot_ggmarginal))
          } else {
            return(neuro_plot)
            
          }
          
          eval(parse(text = paste0("click_count_", tab_id, "(0)")))
          #click_count_table(0)  
        }, error = function(e) {
          output$table_error_message <- renderText({"Plot could not be generated. Please check data and format selections"})
          
          # Print or log the error message
          cat("Error Message:", conditionMessage(e), "\n")
          # Print the call stack trace
          traceback()
         })
        }), envir = .GlobalEnv) 
      })
   }

#Run the app
shinyApp(ui = ui, server = server)
