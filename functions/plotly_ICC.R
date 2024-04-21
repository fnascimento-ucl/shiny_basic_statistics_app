library(ICC)
library(plotly)

plotly_ICC <- function(data, group_var, x_var, y_var) {
  
  # Extract grouping variable names from data
  group_levels <- unique(data[[group_var]])
  
  # Calculate ICC for each group
  icc_list <- list()
  for (group_level in group_levels) {
    # Subset data for current group
    group_data <- subset(data, data[[group_var]] == group_level)
    
    # Check that there are at least two observations per group
    if (nrow(group_data) < 2) {
      stop(paste("Group", group_var, "has less than 2 observations. ICC cannot be calculated."))
    }
    
    # Calculate ICC for current group
    icc <- ICCest(x = factor(group_data[[x_var]]), alpha = 0.05, y = group_data[[y_var]], data = group_data)
    
    # Store ICC result in list
    icc_list[[group_level]] <- icc
  }
  
  # Convert ICC results to data frame
  icc_df <- data.frame(Group = group_levels, ICC = sapply(icc_list, function(x) x$ICC), Lower = sapply(icc_list, function(x) x$LowerCI), Upper = sapply(icc_list, function(x) x$UpperCI), N = sapply(icc_list, function(x) x$N))
  
  # Plot ICC values
  plot1 <- plot_ly(icc_df, x = ~Group, y = ~ICC, type = "scatter", mode = "markers+lines", error_y = list(array = ~Upper - ~ICC, arrayminus = ~ICC - ~Lower)) %>%
    add_hline(yintercept = 0.5, line = list(color = "black")) %>%
    layout(yaxis = list(title = "ICC"), xaxis = list(title = group_var), title = "ICC by Group", showlegend = FALSE)
  
  # Plot data
  plot2 <- plot_ly(data, x = ~get(x_var), y = ~get(y_var), color = ~get(group_var), type = "scatter", mode = "markers") %>%
    layout(title = paste0(y_var, " per ", x_var, " by ", group_var), xaxis = list(title = x_var), yaxis = list(title = y_var), showlegend = FALSE)
  
  plot_grid(plot2, plot1, align = "h", axis = "l", rel_widths = c(4, 1))
}
