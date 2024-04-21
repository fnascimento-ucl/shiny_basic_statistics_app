################################################################################
#########################Hedges g###############################################

#calculates Hedges' g by comparing a control and test groups. Requires list with mean, sd and length of observations
hedges_g <- function(control_group_hedges, test_group_hedges, mean_sd_length_list){
  
  control_group <- control_group_hedges
  test_group <- test_group_hedges
  
  # Retrieve mean, sd, and number of observations for control group
  mean_control <- mean_sd_length_list$mean[, control_group]
  sd_control <- mean_sd_length_list$sd[, control_group]
  n_control <- mean_sd_length_list$length[, control_group]
  
  # Retrieve mean, sd, and number of observations for test group
  mean_test <- mean_sd_length_list$mean[, test_group]
  sd_test <- mean_sd_length_list$sd[, test_group]
  n_test <- mean_sd_length_list$length[, test_group]
  
  # Calculate pooled weighted sd
  pooled_sd <- sqrt(((n_test - 1) * sd_test^2 + (n_control - 1) * sd_control^2) / (n_test + n_control - 2))
  
  # Calculate Hedges' g with correction!
  hedges_g <- (mean_test - mean_control) / pooled_sd * (1-3/(4*(n_test + n_control)-9))
  
  # Calculates Hedges' g without correction
  hedges_g <- (mean_test - mean_control)/ pooled_sd
  return(hedges_g)
}