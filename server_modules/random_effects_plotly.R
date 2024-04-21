################################################################################
############Server module for LMM plot for ICC for 2 or more levels#############
################################################################################

#calls packages required
library(lme4)
library(plotly)
library(ggplot2)
library(rainbow)
library(cowplot)

# Define server logic
random_effects_plotly <- function(input, output, session) {

  
  #Plot data with plots subdivided on random effects
    # For ICC calculation
    # Populate grouping variable dropdown menu
    observe({
      req(data())
      updateSelectInput(session, "group_var", "Select the grouping variable", choices = colnames(data()))
    })
    
    # Populate level 1 column dropdown menu
    observe({
      req(data(), input$group_var)
      updateSelectInput(session, "x_var", "Select the column for level 1", choices = colnames(data())[colnames(data()) != input$group_var])
    })
    
    # Populate level 2 column dropdown menu
    observe({
      req(data(), input$group_var, input$x_var)
      updateSelectInput(session, "y_var", "Select the column for level 2", choices = colnames(data())[colnames(data()) != input$group_var & colnames(data()) != input$x_var])
    })
    
    # Populate level 3 column dropdown menu
    observe({
      req(data(), input$group_var, input$x_var, input$y_var)
      updateSelectInput(session, "z_var", "Select the column for level 3", choices = colnames(data())[colnames(data()) != input$group_var & colnames(data()) != input$x_var & colnames(data()) != input$y_var])
    })
    
    # Populate level 4 column dropdown menu
    observe({
      req(data(), input$group_var, input$x_var, input$y_var, input$z_var)
      updateSelectInput(session, "w_var", "Select the column for level 4", choices = colnames(data())[colnames(data()) != input$group_var & colnames(data()) != input$x_var & colnames(data()) != input$y_var & colnames(data()) != input$z_var])
    })
    
    # Render ICC plot
    output$icc_plot <- renderPlot({
      req(input$submit, data(), input$group_var, input$num_levels)
      calculate_and_plot_icc(data(), input$group_var, input$x_var, input$y_var)
    })
    
    # Render ICC plot for 2 or more variables using icc_lme
    #output$icc_plot_multiple <- renderPlot({
     # req(input$submit, data(), input$group_var, input$num_levels)
     # if (input$num_levels == 2) {
     #   icc_lme(data(), input$group_var, input$x_var, input$y_var)
     # } else if (input$num_levels == 3){
     #   icc_lme(data(), input$group_var, input$x_var, input$y_var, input$z_var)
     # } else if (input$num_levels == 4){
     #   icc_lme(data(), input$group_var, input$x_var, input$y_var, input$z_var, input$w_var)
     # } else {
     #   print("Less than 2 levels, ICC cannot be calculated")
     # }
    #})
    
    # Call random_effects_plotly module
    # Call random_effects_plotly module
    output$icc_plot_multiple <- renderPlotly({
      req(input$submit, data(), input$group_var, input$num_levels)
      if (input$num_levels == 2) {
        ggplot(data = data(), aes(x = !!sym(input$group_var), y = !!sym(input$y_var), colour = !!sym(input$x_var))) +
          geom_point(alpha = 0.5) +
          scale_color_gradientn(colors = rainbow(50)) +
          theme_classic()
      } else if (input$num_levels == 3){
        ggplot(data = data(), aes(x = !!sym(input$group_var), y = !!sym(input$z_var), colour = !!sym(input$y_var))) +
          facet_wrap(as.formula(paste0("~", input$x_var))) +
          geom_point(alpha = 0.5) +
          scale_color_gradientn(colors = rainbow(50)) +
          theme_classic()
      } else if (input$num_levels == 4){
        ggplot(data = data(), aes(x = !!sym(input$group_var), y = !!sym(input$w_var), colour = !!sym(input$z_var))) +
          facet_grid(as.formula(paste0("~", input$y_var, "~", input$x_var))) +
          geom_point(alpha = 0.5) +
          scale_color_gradientn(colors = rainbow(50)) +
          theme_classic()
      } else {
        print("Less than 2 levels, ICC cannot be calculated")
      }
    })
    
}






