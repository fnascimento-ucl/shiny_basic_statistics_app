################################################################################
#################Cohen's d######################################################

cohens_d <- function(control_group_cohen, test_group_cohen, mean_sd_length_list) {
  
  control_group <- control_group_cohen
  test_group <- test_group_cohen
  
  # Retrieve mean, sd, and number of observations for control group
  mean_control <- mean_sd_length_list$mean[, control_group]
  sd_control <- mean_sd_length_list$sd[, control_group]
  n_control <- mean_sd_length_list$length[, control_group]
  
  # Retrieve mean, sd, and number of observations for test group
  mean_test <- mean_sd_length_list$mean[, test_group]
  sd_test <- mean_sd_length_list$sd[, test_group]
  n_test <- mean_sd_length_list$length[, test_group]
  
  # Calculate pooled sd using sample standard deviation
  pooled_sd <- sqrt((sd_test^2 + sd_control^2) / 2)
  
  # Calculate Cohen's d
  cohens_d <- (mean_test - mean_control) / pooled_sd
  
  return(cohens_d)
}
