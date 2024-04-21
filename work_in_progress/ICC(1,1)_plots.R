#################################################################
###########ICC calculation for 2 level dataset###################
#################################################################


library(ICC)
library(readxl)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(ggpp)

# Load data from Excel file
data = read_excel("C:/Users/filip/Documents/R/SOD_paper/In_vivo.xlsx")

#For two levels performs ICC(1,1) using ICCbare
#Select your grouping and numerical variables
grouping_variable = data$Genotype
numerical_variable = data$Inhibition

#Calculate ICC using ICCest
ICCest(x = factor(Animal), alpha = 0.05, y = Inhibition, data = data)


#data = read_excel("C:/Users/filip/Documents/MATLAB/FUS_RI.xlsx")
#source("C:/Users/filip/Documents/calculate_and_plot_icc.R")

calculate_and_plot_icc(data = data, group_var = "Genotype", x_var = "Animal", y_var = "Inhibition")



library(ICC)
library(ggplot2)
library(reshape2)
library(readxl)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(ggpp)

# Load data from Excel file
data <- read_excel("C:/Users/filip/Documents/R/SOD_paper/In_vivo.xlsx")

# Extract grouping variable names from Excel file
group_vars <- unique(data$Genotype)

# Calculate ICC for each group
icc_list <- list()
for (group_var in group_vars) {
  # Subset data for current group
  group_data <- subset(data, Genotype == group_var)
  
  # Check that there are at least two observations per group
  if (nrow(group_data) < 2) {
    stop(paste("Group", group_var, "has less than 2 observations. ICC cannot be calculated."))
  }
  
  # Calculate ICC for current group
  icc <- ICCest(x = factor(Animal), alpha = 0.05, y = Inhibition, data = group_data)
  
  # Store ICC result in list
  icc_list[[group_var]] <- icc
}

# Print ICC results
print(icc_list)

# Convert ICC results to data frame
icc_df <- data.frame(Group = group_vars, ICC = sapply(icc_list, function(x) x$ICC), Lower = sapply(icc_list, function(x) x$LowerCI), Upper = sapply(icc_list, function(x) x$UpperCI), N = sapply(icc_list, function(x) x$N))

# Print data frame
print(icc_df)

# Plot ICC values
plot1 <- ggplot(icc_df, aes(x = Group, y = ICC, ymin = Lower, ymax = Upper)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.2) +
  geom_hline(yintercept = 0.5, color = "black")+
  scale_y_continuous(position = "right", labels = waiver()) +
  geom_text(aes(label = paste0("Animal = ", N)), position = position_dodge(width = 0.9), vjust = -0.5) +
  xlab("Group") +
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

plot1



plot2 <- ggplot(data, aes(x = Animal, y = Inhibition, color = Genotype)) +
  geom_point(size = 3, alpha = NA) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Inhibition per Animal by Genotype",
       x = "Animal",
       y = "Inhibition") +
  theme (panel.grid.major.x = element_blank(),
         axis.line.x = element_line(size = 1),
         axis.ticks.x = element_line(size = 1),
         axis.text.x.bottom = element_text(color = "black", size = 15),
         axis.line.y.left = element_line(color = "black", size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.text.y = element_text(color = "black", size = 15, margin = margin(r = 10)),
        axis.title.y = element_text(color = "black", size = 20, margin = margin(r = 10)),
        axis.title.x = element_text(color = "black", size = 20, margin = margin(r = 10)))
plot2

plot_grid(plot2, plot1, align = "h", axis = "l", rel_widths = c(4, 1))


