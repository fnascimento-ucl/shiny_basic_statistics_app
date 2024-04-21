################################################################################
#################Glass' delta##################################################

glass_delta <- function(control_group_glass, test_group_glass, mean_sd_length_list) {
  
  control_group <- control_group_glass
  test_group <- test_group_glass
  
  # Retrieve mean, sd, and number of observations for control group
  mean_control <- mean_sd_length_list$mean[, control_group]
  sd_control <- mean_sd_length_list$sd[, control_group]

  # Retrieve mean, sd, and number of observations for test group
  mean_test <- mean_sd_length_list$mean[, test_group]
  sd_test <- mean_sd_length_list$sd[, test_group]

  # Calculate pooled sd using test group's standard deviation
  pooled_sd <- sd_test
  
  # Calculate Glass's Delta
  glass_delta <- (mean_test - mean_control) / pooled_sd
  return(glass_delta)
}

