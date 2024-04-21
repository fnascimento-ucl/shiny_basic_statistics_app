##############Superplots####################################################
############################################################################

superplot_server_module <- function(id){
  moduleServer(id,
               function(input, output, session) {
  

                 #Plots the superplot
                 output$superplot <- renderPlot({
                   req(input$submit_superplot,data_superplot(), input$group_var_superplot)
                   library(viridis)
                   library(ggbeeswarm)
                   
                   # Get the levels of the group variable in the order chosen by the user
                   #for numeric variables
                   if (is.numeric(data_superplot()[[input$group_var_superplot]])){
                     group_levels <- unique(data_superplot()[[input$group_var_superplot]])
                     #for non-numeric variables
                   } else {
                     group_levels <- levels(data_superplot()[[input$group_var_superplot]])
                   }
                   #for group order selection
                   if (!is.null(input$group_order)) {
                     group_levels <- input$group_order
                   }
                   
                   # Group and summarize the data by group and x-variable
                   if (input$mean_or_median == "mean"){
                     Replicate_means_or_medians <- data_superplot() %>% dplyr::group_by(!!as.name(input$group_var_superplot), !!as.name(input$x_var_superplot)) %>% summarise_each(list(mean))
                     # Calculates total averages
                     Total_replicates <- Replicate_means_or_medians %>% summarise_each(list(mean))
                   } else if (input$mean_or_median == "median"){
                     Replicate_means_or_medians <- data_superplot() %>% dplyr::group_by(!!as.name(input$group_var_superplot), !!as.name(input$x_var_superplot)) %>% summarise_each(list(median))
                     Total_replicates <- Replicate_means_or_medians %>% summarise_each(list(median))
                   }
                   
                   median_IQR <- function(x) {
                     data.frame(y = median(x), # Median
                                ymin = quantile(x)[2], # 1st quartile
                                ymax = quantile(x)[4])  # 3rd quartile
                   }
                   
                   #Plot superplot
                   ggplot(data_superplot(), aes(x = factor(!!as.name(input$group_var_superplot), levels = group_levels), y = !!as.name(input$y_var_superplot), colour = !!as.name(input$x_var_superplot))) +
                     # Adds individual data points as a scatter
                     geom_beeswarm(cex=3) +
                     
                     # Show mean or median for each group
                     stat_summary(data = Total_replicates, fun = function(x) {
                       if (input$mean_or_median == "mean") {
                         return(mean(x, na.rm = TRUE))
                       } else if (input$mean_or_median == "median"){
                         return(median(x, na.rm = TRUE))
                       }
                     }, geom = "crossbar", size = 0.1, color = "black") +
                     
                     # Adds the SD or interquartile range (IQR) for each group
                     (if (input$mean_or_median == "mean") {
                       stat_summary(data = Replicate_means_or_medians, fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "errorbar", width = 0.2, color = "black")
                     } else if (input$mean_or_median == "median"){
                       stat_summary(data = Replicate_means_or_medians, fun.data = median_IQR, fun.args = NULL, geom = "errorbar", width = 0.2, color = "black")
                     }) +
                     
                     # Adds color palette
                     #scale_colour_manual(values=rainbow(50)) +
                     scale_colour_viridis() +  
                     # Adds Replicative averages as points (argument "cex" can be used to spread the data points if the averages are close together)
                     geom_beeswarm(data=ReplicateAverages, size=5) +
                     #Cosmetics and labeling
                     theme_bw() + theme(axis.line = element_line(size = 1, colour = "black"),
                                        legend.position = "none",
                                        axis.title.y = element_text(family="Arial", size=28, color = "black", vjust = 2),
                                        axis.text = element_text(family="Arial", size = 28, color = "black"),
                                        axis.ticks = element_line(size = 1, color = "black"), 
                                        axis.ticks.length = unit(2, "mm"),
                                        panel.grid.major = element_blank(), 
                                        panel.grid.minor = element_blank(),
                                        panel.background = element_blank(), 
                                        panel.border = element_blank()) +
                     xlab("") + ylab("Total counts")
                   
                   
                 })
               }
  )
}
  
  










  