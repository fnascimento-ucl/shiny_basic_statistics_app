#################################################################################
#################################################################################
#Function for bootstrap to perform resampling

boot_one_level <- function(data, variable_name, group_var, pair_id = NULL, n_replicas_hierarchical_boot) {
  
  
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
    
    # for unpaired bootstrap
    if (is.null(pair_id)){
      
      # Perform the bootstrap for each grouping variable
      for (j in seq_along(grouping_var)) {
        
        # Subset the data for the current grouping variable
        data_subset <- data[data[[group_var]] == grouping_var[j], ]
        
        # Initialize lists for the current group
        mean_list <- numeric(n_replicas)
        median_list <- numeric(n_replicas)
        sd_list <- numeric(n_replicas)
        length_list <- numeric(n_replicas)
        
        # Create a progress bar for the current grouping variable
        withProgress(message = 'Simple botstrapping in progress',
                     detail = paste("Group", j, 'of', length(grouping_var)),
                     value = 0, {
                       
                       for (i in 1:n_replicas) {
                         
                         # Sample with replacement
                         input_sample <- sample(na.omit(data_subset[[variable_name]]), replace = TRUE)
                         
                         # Store the mean, median, sd, length for each bootstrap replica
                         mean_list[i] <- mean(input_sample)
                         median_list[i] <- median(input_sample)
                         sd_list[i] <- sd(input_sample)
                         length_list[i] <- length(input_sample)
                       }
                       # Store the lists for the current group
                       bootstrap_replicas_mean[, which(colnames(bootstrap_replicas_mean) == as.character(grouping_var[j]))] <- mean_list
                       bootstrap_replicas_median[, which(colnames(bootstrap_replicas_median) == as.character(grouping_var[j]))] <- median_list
                       bootstrap_replicas_sd[, which(colnames(bootstrap_replicas_sd) == as.character(grouping_var[j]))] <- sd_list
                       bootstrap_replicas_length[, which(colnames(bootstrap_replicas_length) == as.character(grouping_var[j]))] <- length_list
                       
                       
                       # Update the progress bar
                       setProgress(j / length(grouping_var))
                     })
      }
      
      # Return the bootstrap results as a list of lists
      result <- list(
        mean = bootstrap_replicas_mean,
        median = bootstrap_replicas_median,
        sd = bootstrap_replicas_sd,
        length = bootstrap_replicas_length
      )
      
    # for paired bootstrap
    } else {
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
    }
  
  return(result)
}