library(lme4)
library(plotly)
library(ggplot2)
library(rainbow)
library(cowplot)

icc_lme <- function(data, num_levels, group_var, x_var, y_var, z_var = NULL, w_var = NULL) {
  
  # Runs linear mixed-model for 2, 3, or 4 random variables without interaction
 # if (is.null(z_var) && is.null(w_var)) {
  #  mixed <- lmer(data[[y_var]] ~ data[[group_var]] + (1 | data[[x_var]]), data = data)
  #} else if (!is.null(z_var) && is.null(w_var)) {
   # mixed <- lmer(data[[z_var]] ~ data[[group_var]] + (1 | data[[y_var]]) + (1 | data[[x_var]]), data = data)
  #} else if (!is.null(z_var) && !is.null(w_var)) {
   # mixed <- lmer(data[[w_var]] ~ data[[group_var]] + (1 | data[[z_var]]) + (1 | data[[y_var]]) + (1 | data[[x_var]]), data = data)
  #} else {
   # mixed <- lmer(data[[y_var]] ~ data[[group_var]] + (1 | data[[x_var]]), data = data)
  #}
  
  # Convert num_levels to numeric if necessary
  # Change line 22
#if (length(unique(data[[group_var]])) <= 1) {
 # stop("grouping factors must have > 1 sampled level")
#}
  
  names(data)[names(data) == x_var] <- "x_var"
  names(data)[names(data) == y_var] <- "y_var"
  names(data)[names(data) == z_var] <- "z_var"
  names(data)[names(data) == w_var] <- "w_var"
  names(data)[names(data) == group_var] <- "group_var"
  
  #colnames(data)[colnames(data) == substitute(w_var)] <- "w_var"
  #colnames(data)[colnames(data) == substitute(group_var)] <- "group_var"
  

  # Runs linear mixed-model for 2, 3, or 4 random variables without interaction
  if (num_levels == 2) {
    mixed <- lmer(y_var ~ 1 + group_var + (1 | x_var), data)
  } else if (num_levels == 3) {
    mixed <- lmer(z_var ~ 1 + group_var + (1 | y_var) + (1 | x_var), data)
  } else if (num_levels == 4) {
    mixed <- lmer(w_var ~ 1 + group_var + (1 | z_var) + (1 | y_var) + (1 | x_var), data)
  } else {
    print("Less than 2 levels, LMM cannot calculate variance component")
  }
  
  
 

# Extract variance components from LMM
variance <- as.data.frame(VarCorr(mixed))

# Extract confidence intervals for random effects and residuals
conf_int <- confint(mixed)

# Get the names of the random effects and residuals
var_names <- variance$grp

# Get the variances and their confidence intervals
  if (".sig03" %in% rownames(conf_int)) {
    var_df <- data.frame(var_name = var_names,
                         variance = variance$vcov,
                         lower = c(conf_int[".sig03", "2.5 %"]^2, conf_int[".sig02", "2.5 %"]^2, conf_int[".sig01", "2.5 %"]^2, conf_int[".sigma", "2.5 %"]^2),
                         upper = c(conf_int[".sig03", "97.5 %"]^2, conf_int[".sig02", "97.5 %"]^2, conf_int[".sig01", "97.5 %"]^2, conf_int[".sigma", "97.5 %"]^2)
    )
  } else if (".sig02" %in% rownames(conf_int)) {
    var_df <- data.frame(var_name = var_names,
                         variance = variance$vcov,
                         lower = c(conf_int[".sig02", "2.5 %"]^2, conf_int[".sig01", "2.5 %"]^2, conf_int[".sigma", "2.5 %"]^2),
                         upper = c(conf_int[".sig02", "97.5 %"]^2, conf_int[".sig01", "97.5 %"]^2, conf_int[".sigma", "97.5 %"]^2)
    )
  } else if (".sig01" %in% rownames(conf_int)) {
    var_df <- data.frame(var_name = var_names,
                         variance = variance$vcov,
                         lower = c(conf_int[".sig01", "2.5 %"]^2, conf_int[".sigma", "2.5 %"]^2),
                         upper = c(conf_int[".sig01", "97.5 %"]^2, conf_int[".sigma", "97.5 %"]^2)
    )
  }
  
  #var_df

#


  #gets variance from random effects
  variance_random <- variance[variance$grp != "Residual", ]
  #gets variance from residuals
  variance_residuals <- variance[variance$grp == "Residual", ]
  
#calculates variance components
  #total variance
  var_total <- sum(variance_random$vcov) + variance_residuals$vcov
  
  # Compute variance components for each random effect
  var_components <- data.frame(var_name = character(), variance = numeric(), lower = numeric(), upper = numeric(), stringsAsFactors = FALSE)
  for (i in 1:nrow(var_df)) {
    var_comp <- var_df[i, "variance"] / var_total
    lower_comp <- var_df[i, "lower"] / var_total
    upper_comp <- var_df[i, "upper"] / var_total
    var_components <- rbind(var_components, data.frame(var_name = var_df[i, "var_name"], variance = var_comp, lower = lower_comp, upper = upper_comp))
  }
  #var_components
  
  
  # plots variance component of random effects
  plot_var <- ggplot(var_components, aes(x = var_name, y = variance, ymin = lower, ymax = upper)) +
    geom_point(size = 3) +
    #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    geom_hline(yintercept = 0.5, color = "black") +
    scale_y_continuous(position = "right", labels = waiver()) +
    xlab(group_var) +
    ylab("Variance components") +
    ggtitle("Variance component of random effects and residuals") +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
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
      axis.title.x = element_text(color = "black", size = 20, margin = margin(r = 10))
    )
  plot_var
}


  








