boot_one_level_repeated <- function(data, variable_name, group_var, pair_id, n_replicas_hierarchical_boot) {
  
  # Define the grouping variable
  grouping_var <- unique(data[[group_var]])
  
  # Define the number of resampling replicas
  n_replicas <- n_replicas_hierarchical_boot
  
  # Creates matrix to stores bootstrap variables
  bootstrap_replicas_mean <- matrix(NA, nrow = n_replicas_hierarchical_boot, ncol = length(unique(data[[group_var]])))
  bootstrap_replicas_median <- matrix(NA, nrow = n_replicas_hierarchical_boot, ncol = length(unique(data[[group_var]])))
  bootstrap_replicas_sd <- matrix(NA, nrow = n_replicas_hierarchical_boot, ncol = length(unique(data[[group_var]])))
  bootstrap_replicas_length <- matrix(NA, nrow = n_replicas_hierarchical_boot, ncol = length(unique(data[[group_var]])))
  
  # Coerces column names to original names
  dimnames(bootstrap_replicas_mean) <- list(1:n_replicas_hierarchical_boot, unique(data[[group_var]]))
  dimnames(bootstrap_replicas_median) <- list(1:n_replicas_hierarchical_boot, unique(data[[group_var]]))
  dimnames(bootstrap_replicas_sd) <- list(1:n_replicas_hierarchical_boot, unique(data[[group_var]]))
  dimnames(bootstrap_replicas_length) <- list(1:n_replicas_hierarchical_boot, unique(data[[group_var]]))
  
  # Perform the bootstrap
  for (i in 1:n_replicas) {
    
    # Sample with replacement at the id pairing level
    pairing_sample <- sample(na.omit(data[[pair_id]]), size = length(unique(data[[pair_id]])), replace = TRUE)
    
    # for the sampled ID, subset the values for each grouping var
    for (j in seq_along(grouping_var)) {
      
      # subsets data for grouping_var
      data_subset <- data[data[[group_var]] == grouping_var[j], ]
      
      # Subsets the data for the pairing sample
      input_sample <- data_subset[match(pairing_sample, data_subset[[pair_id]]), variable_name]
      
      # Stores the mean, median, sd, length for each bootstrap replica
      bootstrap_replicas_mean[i, which(colnames(bootstrap_replicas_mean) == as.character(grouping_var[j]))] <- mean(input_sample)
      bootstrap_replicas_median[i, which(colnames(bootstrap_replicas_median) == as.character(grouping_var[j]))] <- median(input_sample)
      bootstrap_replicas_sd[i, which(colnames(bootstrap_replicas_sd) == as.character(grouping_var[j]))] <- sd(input_sample)
      bootstrap_replicas_length[i, which(colnames(bootstrap_replicas_length) == as.character(grouping_var[j]))] <- length(input_sample)
    }
  }
  
  # Return the bootstrap results as a list of arrays
  result <- list(
    mean = bootstrap_replicas_mean,
    median = bootstrap_replicas_median,
    sd = bootstrap_replicas_sd,
    length = bootstrap_replicas_length
  )
  
  return(result)
}