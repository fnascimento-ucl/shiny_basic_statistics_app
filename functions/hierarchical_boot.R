#################################################################################
#################################################################################
#General function for hierarchical bootstrap that can perform resampling up to 4 levels
hierarchical_boot <- function(data, level_1_var, level_2_var, level_3_var, level_4_var, group_var, n_replicas_hierarchical_boot) {

# Define the grouping variable
grouping_var <- unique(data[[group_var]])

# Define the number of resampling replicas
n_replicas <- n_replicas_hierarchical_boot

# Creates matrix to stores bootstrap variables
bootstrap_replicas_mean <- matrix(NA, nrow = n_replicas_hierarchical_boot, ncol = length(unique(data[[group_var]])))
bootstrap_replicas_median <- matrix(NA, nrow = n_replicas_hierarchical_boot, ncol = length(unique(data[[group_var]])))
bootstrap_replicas_sd <- matrix(NA, nrow = n_replicas_hierarchical_boot, ncol = length(unique(data[[group_var]])))
bootstrap_replicas_length <- matrix(NA, nrow = n_replicas_hierarchical_boot, ncol = length(unique(data[[group_var]])))

#colnames(bootstrap_replicas_mean) <- unique(data[[group_var]])
dimnames(bootstrap_replicas_mean) <- list(1:n_replicas_hierarchical_boot, unique(data[[group_var]]))
dimnames(bootstrap_replicas_median) <- list(1:n_replicas_hierarchical_boot, unique(data[[group_var]]))
dimnames(bootstrap_replicas_sd) <- list(1:n_replicas_hierarchical_boot, unique(data[[group_var]]))
dimnames(bootstrap_replicas_length) <- list(1:n_replicas_hierarchical_boot, unique(data[[group_var]]))


#for 2 level data
if (is.na(level_3_var) && is.na(level_4_var)){ 
  
  # Create an empty matrix to store the bootstrap replicas
  bootstrap_replicas <- matrix(NA, nrow = n_replicas, ncol = length(grouping_var))
  
  # Perform the hierarchical bootstrap for each grouping variable
  for (j in seq_along(grouping_var)) {
      
    # Subset the data for the current grouping variable
    data_subset <- subset(data, data[[group_var]] == grouping_var[j])
    
    # Define the maximum length from level 1
    level_1_variable_length <- length(unique(na.omit(data_subset[[level_1_var]])))
    
    # Define the maximum number of level 2 observations obtained per each member of level 1
    level_2_variable_length <- max(na.omit(tapply(data_subset[[level_2_var]], data_subset[[level_1_var]], length)))
    
    mean_list <- numeric(n_replicas)
    median_list <- numeric(n_replicas)
    sd_list <- numeric(n_replicas)
    length_list <- numeric(n_replicas)
    
    # Create a progress bar for the current grouping variable
    withProgress(message = 'Hierarchical bootstrapping in progress',
                 detail = paste("Group", j, 'of', length(grouping_var)),
                 value = 0, {
    
    for (i in 1:n_replicas) {
      # Sample n members from level 1 with replacement
      level_1_sample <- sample(unique(na.omit(data_subset[[level_1_var]])), level_1_variable_length, replace = TRUE)
          
      # for each of the elements in level 1 it samples n maximum number of level 2 observations obtained per each member of level 1
      resampled_data <- lapply(level_1_sample, function(id) {
        subset_data <- data_subset[data_subset[[level_1_var]] == id, ]
        resampled_values <- sample(na.omit(subset_data[[level_2_var]]), size = level_2_variable_length, replace = TRUE)
        data.frame(level_1_var = rep(id, level_2_variable_length), level_2_var = resampled_values)
      })
      
      # merges sample data  
      input_values <- do.call(rbind, resampled_data)
        
      # Store the mean, median, sd, length for each bootstrap replica
      mean_list[i] <- mean(input_values$level_2_var)
      median_list[i] <- median(input_values$level_2_var)
      sd_list[i] <- sd(input_values$level_2_var)
      length_list[i] <- length(input_values$level_2_var)
      
      # Update the progress bar
      setProgress(i / n_replicas)
    }
                   
    # Store the lists for the current group
    bootstrap_replicas_mean[, which(colnames(bootstrap_replicas_mean) == as.character(grouping_var[j]))] <- mean_list
    bootstrap_replicas_median[, which(colnames(bootstrap_replicas_median) == as.character(grouping_var[j]))] <- median_list
    bootstrap_replicas_sd[, which(colnames(bootstrap_replicas_sd) == as.character(grouping_var[j]))] <- sd_list
    bootstrap_replicas_length[, which(colnames(bootstrap_replicas_length) == as.character(grouping_var[j]))] <- length_list 
    })
  }
  
  
#for 3 level data
} else if (!is.na(level_3_var) && is.na(level_4_var)){ 
  
  # Create an empty matrix to store the bootstrap replicas
  bootstrap_replicas <- matrix(NA, nrow = n_replicas, ncol = length(grouping_var))
  
  # Perform the hierarchical bootstrap for each grouping variable
  for (j in seq_along(grouping_var)) {
    
    # Subset the data for the current grouping variable
    data_subset <- subset(data, data[[group_var]] == grouping_var[j])
    
    # Define the maximum length from levels 1 and 2
    level_1_variable_length <- length(unique(na.omit(data_subset[[level_1_var]])))
    level_2_variable_length <- length(unique(na.omit(data_subset[[level_2_var]])))
    
    # Define the maximum number of level 3 observations obtained per each member of level 2 sampled from level 1
    level_3_variable_length <- max((na.omit(tapply(data_subset[[level_3_var]], data_subset[[level_2_var]], length))))
    
    # Create a progress bar for the current grouping variable
    withProgress(message = 'Hierarchical bootstrapping in progress',
                 detail = paste("Group", j, 'of', length(grouping_var)),
                 value = 0, {
    
    for (i in 1:n_replicas) {
      # Sample n members from level 1 with replacement
      level_1_sample <- sample(unique(na.omit(data_subset[[level_1_var]])), level_1_variable_length, replace = TRUE)
      
      # Sample n members from level 2 with replacement, previously taken from level 1 
      level_2_sample <- sample(unique(na.omit((data_subset[data_subset[[level_1_var]] %in%  level_1_sample,])[[level_2_var]])), level_2_variable_length, replace = TRUE)
      
      # for each of the elements in level 1 it samples n maximum number of level 2 observations obtained per each member of level 1
      resampled_data <- lapply(level_2_sample, function(id) {
        subset_data <- data_subset[data_subset[[level_2_var]] == id, ]
        resampled_values <- sample(na.omit(subset_data[[level_3_var]]), size = level_3_variable_length, replace = TRUE)
        data.frame(level_2_var = rep(id, level_2_variable_length), level_3_var = resampled_values)
      })
      
      # merges sample data  
      input_values <- do.call(rbind, resampled_data)
      
      # Store the mean, median, sd, length for each bootstrap replica
      mean_list[i] <- mean(input_values$level_2_var)
      median_list[i] <- median(input_values$level_2_var)
      sd_list[i] <- sd(input_values$level_2_var)
      length_list[i] <- length(input_values$level_2_var)
      
      # Update the progress bar
      setProgress(i / n_replicas)
    }
      
      # Store the lists for the current group
      bootstrap_replicas_mean[, which(colnames(bootstrap_replicas_mean) == as.character(grouping_var[j]))] <- mean_list
      bootstrap_replicas_median[, which(colnames(bootstrap_replicas_median) == as.character(grouping_var[j]))] <- median_list
      bootstrap_replicas_sd[, which(colnames(bootstrap_replicas_sd) == as.character(grouping_var[j]))] <- sd_list
      bootstrap_replicas_length[, which(colnames(bootstrap_replicas_length) == as.character(grouping_var[j]))] <- length_list 
    })
  }
  
# for 4 level data
  } else if (!is.na(level_3_var) && !is.na(level_4_var)){
  
  # Create an empty matrix to store the bootstrap replicas
  bootstrap_replicas <- matrix(NA, nrow = n_replicas, ncol = length(grouping_var))
  
  # Perform the hierarchical bootstrap for each grouping variable
  for (j in seq_along(grouping_var)) {
    
    # Subset the data for the current grouping variable
    data_subset <- subset(data, data[[group_var]] == grouping_var[j])
    
    # Define Define the maximum length from levels 1, 2 and 3
    level_1_variable_length <- length(unique(na.omit(data_subset[[level_1_var]])))
    level_2_variable_length <- length(unique(na.omit(data_subset[[level_2_var]])))
    level_3_variable_length <- length(unique(na.omit(data_subset[[level_3_var]])))
    
    # Define the maximum number of level 4 observations obtained per each member of level 3, sampled from level 2, taken from level 1
    level_4_variable_length <- max(na.omit(tapply(data_subset[[level_4_var]], data_subset[[level_3_var]], length)))
    
    # Create a progress bar for the current grouping variable
    withProgress(message = 'Hierarchical bootstrapping in progress',
                 detail = paste("Group", j, 'of', length(grouping_var)),
                 value = 0, {
    
    for (i in 1:n_replicas) {
      # Sample n members from level 1 with replacement
      level_1_sample <- sample(unique(na.omit(data_subset[[level_1_var]]), level_1_variable_length, replace = TRUE))
      
      # Sample n members from level 2 with replacement, previously taken from level 1 
      level_2_sample <- sample(unique(na.omit((data_subset[data_subset[[level_1_var]] %in%  level_1_sample,])[[level_2_var]])), level_2_variable_length, replace = TRUE)
      
      # Sample n members from level 3 with replacement, previously taken from level 2 
      level_3_sample <- sample(unique(na.omit((data_subset[data_subset[[level_2_var]] %in%  level_2_sample,])[[level_3_var]])), level_3_variable_length, replace = TRUE)
      
      # for each of the elements in level 1 it samples n maximum number of level 2 observations obtained per each member of level 1
      resampled_data <- lapply(level_3_sample, function(id) {
        subset_data <- data_subset[data_subset[[level_3_var]] == id, ]
        resampled_values <- sample(na.omit(subset_data[[level_4_var]]), size = level_4_variable_length, replace = TRUE)
        data.frame(level_3_var = rep(id, level_3_variable_length), level_4_var = resampled_values)
      })
      
      # merges sample data  
      input_values <- do.call(rbind, resampled_data)
      
      # Store the mean, median, sd, length for each bootstrap replica
      mean_list[i] <- mean(input_values$level_3_var)
      median_list[i] <- median(input_values$level_3_var)
      sd_list[i] <- sd(input_values$level_3_var)
      length_list[i] <- length(input_values$level_3_var)
      
      # Update the progress bar
      setProgress(i / n_replicas)
    }
                   
      # Store the lists for the current group
      bootstrap_replicas_mean[, which(colnames(bootstrap_replicas_mean) == as.character(grouping_var[j]))] <- mean_list
      bootstrap_replicas_median[, which(colnames(bootstrap_replicas_median) == as.character(grouping_var[j]))] <- median_list
      bootstrap_replicas_sd[, which(colnames(bootstrap_replicas_sd) == as.character(grouping_var[j]))] <- sd_list
      bootstrap_replicas_length[, which(colnames(bootstrap_replicas_length) == as.character(grouping_var[j]))] <- length_list
    })
  }
  }
  
#returns output
return(list(mean = bootstrap_replicas_mean, median = bootstrap_replicas_median, sd = bootstrap_replicas_sd, length = bootstrap_replicas_length))
  }