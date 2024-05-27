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
        withProgress(message = 'Simple bootstrapping in progress',
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
                         
                         # Update the progress bar incrementally every 10 iterations
                         if (i %% 10 == 0 || i == n_replicas) {
                           setProgress((j - 1 + (i / n_replicas)) / length(grouping_var))
                         }
                       }
                       # Store the lists for the current group
                       bootstrap_replicas_mean[, which(colnames(bootstrap_replicas_mean) == as.character(grouping_var[j]))] <- mean_list
                       bootstrap_replicas_median[, which(colnames(bootstrap_replicas_median) == as.character(grouping_var[j]))] <- median_list
                       bootstrap_replicas_sd[, which(colnames(bootstrap_replicas_sd) == as.character(grouping_var[j]))] <- sd_list
                       bootstrap_replicas_length[, which(colnames(bootstrap_replicas_length) == as.character(grouping_var[j]))] <- length_list
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
      
        
      # Create a progress bar outside the loop
      withProgress(message = 'Paired bootstrapping in progress',
                   detail = paste("Sample", 0, 'of', n_replicas),
                   value = 0, {
                     
                     # Perform the bootstrap
                     for (i in 1:n_replicas) {
                       # Update the progress message for the current iteration
                       setProgress(message = 'Paired bootstrapping in progress',
                                   detail = paste("Sample", i, 'of', n_replicas))
                       
                       # Sample with replacement at the id pairing level
                       pairing_sample <- sample(na.omit(data[[pair_id]]), size = length(unique(data[[pair_id]])), replace = TRUE)
                       
                       # Initialize a counter to update progress less frequently
                       update_counter <- 0
                       
                       # for the sampled ID, subset the values for each grouping var
                       for (j in seq_along(grouping_var)) {
                         
                         # subsets data for grouping_var
                         data_subset <- data[data[[group_var]] == grouping_var[j], ]
                         
                         # Subsets the data for the pairing sample
                         input_sample <- data_subset[match(pairing_sample, data_subset[[pair_id]]), variable_name]
                         
                         # Stores the mean, median, sd, length for each bootstrap replica
                         bootstrap_replicas_mean[i, which(colnames(bootstrap_replicas_mean) == as.character(grouping_var[j]))] <- mean(input_sample[[variable_name]])
                         bootstrap_replicas_median[i, which(colnames(bootstrap_replicas_median) == as.character(grouping_var[j]))] <- median(input_sample[[variable_name]])
                         bootstrap_replicas_sd[i, which(colnames(bootstrap_replicas_sd) == as.character(grouping_var[j]))] <- sd(input_sample[[variable_name]])
                         bootstrap_replicas_length[i, which(colnames(bootstrap_replicas_length) == as.character(grouping_var[j]))] <- length(input_sample[[variable_name]])
                         
                         # Increment the update counter
                         update_counter <- update_counter + 1
                         
                         # Update the progress bar value every 10 iterations or when the loop is about to finish
                         if (update_counter %% 10 == 0 || i == n_replicas) {
                           setProgress((i / n_replicas))
                         }
                       }
                     }
                   })
      

                         
                     
    
                   
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