library(ICC)
library(ggplot2)
library(reshape2)
library(readxl)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(ggpp)


calculate_and_plot_icc <- function(data, group_var, x_var, y_var) {
  
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
  plot1 <- ggplot(icc_df, aes(x = Group, y = ICC, ymin = Lower, ymax = Upper)) +
    geom_point(size = 3) +
    geom_errorbar(width = 0.2) +
    geom_hline(yintercept = 0.5, color = "black")+
    scale_y_continuous(position = "right", labels = waiver()) +
    geom_text(aes(label = paste0(x_var, " = ", N)), position = position_dodge(width = 0.9), vjust = -0.5) +
    xlab(group_var) +
    ylab("ICC") +
    ggtitle("ICC by Group") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          axis.line.y.left = element_blank(),
          axis.text.y.left = element_blank(),
          axis.title.y.left = element_blank(),
          axis.ticks.y.left = element_blank(),
          axis.line = element_line(size = 1),
          axis.ticks = element_line(size = 1),
          panel.grid.major.x = element_blank(),
          axis.line.x = element_line(size = 1),
          axis.ticks.x = element_line(size = 1),
          axis.text.x.bottom = element_text(color = "black", size = 15),
          axis.text.y = element_text(color = "black", size = 15, margin = margin(r = 10)),
          axis.title.y = element_text(color = "black", size = 20, margin = margin(r = 10)),
          axis.title.x = element_text(color = "black", size = 20, margin = margin(r = 10)))
  
  #Plot data
  plot2 <- ggplot(data, aes_string(x = x_var, y = y_var, color = group_var)) +
    geom_point(size = 3, alpha = NA) +
    scale_color_manual(values = c("blue", "red", "green","yellow","orange")) +
    labs(title = paste0(y_var, " per ", x_var, " by ", group_var),
         x = x_var,
         y = y_var) +
    theme(panel.grid.major.x = element_blank(),
          axis.line.x = element_line(size = 1),
          axis.ticks.x = element_line(size = 1),
          axis.text.x.bottom = element_text(color = "black", size = 15),
          axis.line.y.left = element_line(color = "black", size = 1),
          axis.ticks.y = element_line(size = 1),
          axis.text.y = element_text(color = "black", size = 15, margin = margin(r = 10)),
          axis.title.y = element_text(color = "black", size = 20, margin = margin(r = 10)),
          axis.title.x = element_text(color = "black", size = 20, margin = margin(r = 10)))
  #plot2
  
  plot_grid(plot2, plot1, align = "h", axis = "l", rel_widths = c(4, 1))
}