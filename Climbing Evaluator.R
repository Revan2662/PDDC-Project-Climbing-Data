# Load the required library
library(readr)

# Load data from CSV
trajectories <- read_csv("trajectories.csv")

# Gather the names of all columns with the letter "x"
Xcolumn <- grep("x", names(trajectories), value = TRUE)
cleaned <- list()  # Initialize an empty list

# Iterate through the trajectories vector to filter out NaN values
for (i in Xcolumn) {
  Ycolumn <- sub("x", "y", i)
  
  if (Ycolumn %in% names(trajectories)) {
    filtered <- trajectories[!is.nan(trajectories[[i]]), c(i, Ycolumn, "time")]
    cleaned[[i]] <- filtered
  }
}

# Initialize change_list
change_list <- list()

# Iterate through cleaned data
for (i in Xcolumn) {
  Ycolumn <- sub("x", "y", i)
  flyName <- paste0("Fly ", sub("x", "", i))
  
  if (!is.null(cleaned[[i]])) {
    current_data <- cleaned[[i]]
    
    # Initialize necessary lists in change_list
    change_list[[paste0("X Change ", flyName)]] <- numeric()
    change_list[[paste0("Y Change ", flyName)]] <- numeric()
    change_list[[paste0("Time ", flyName)]] <- numeric()
    change_list[[paste0("X Speed ", flyName)]] <- numeric()
    change_list[[paste0("Y Speed ", flyName)]] <- numeric()
    change_list[[paste0("X Speed Average ", flyName)]] <- numeric()
    change_list[[paste0("Y Speed Average ", flyName)]] <- numeric()
    change_list[[paste0("Passed ", flyName)]] <- numeric()
    
    # Calculate differences between the current and next rows
    for (j in 1:(nrow(current_data) - 1)) {
      current_x <- current_data[[i]][j + 1]  # Next x value
      previous_x <- current_data[[i]][j]      # Current x value
      
      # Update change_list directly
      change_list[[paste0("X Change ", flyName)]] <- c(change_list[[paste0("X Change ", flyName)]], abs(current_x - previous_x))
      
      # Calculate change in Y and update change_list directly
      current_y <- current_data[[Ycolumn]][j + 1]
      previous_y <- current_data[[Ycolumn]][j]
      change_list[[paste0("Y Change ", flyName)]] <- c(change_list[[paste0("Y Change ", flyName)]], abs(current_y - previous_y))
      
      # Calculate change in time and update change_list directly
      current_t <- current_data[["time"]][j + 1]
      previous_t <- current_data[["time"]][j]
      change_list[[paste0("Time ", flyName)]] <- c(change_list[[paste0("Time ", flyName)]], abs(current_t - previous_t))
      
      # Calculate change in speed
      speed_x <- change_list[[paste0("X Change ", flyName)]][j] / change_list[[paste0("Time ", flyName)]][j]
      speed_y <- change_list[[paste0("Y Change ", flyName)]][j] / change_list[[paste0("Time ", flyName)]][j]
      change_list[[paste0("X Speed ", flyName)]] <- c(change_list[[paste0("X Speed ", flyName)]], speed_x)
      change_list[[paste0("Y Speed ", flyName)]] <- c(change_list[[paste0("Y Speed ", flyName)]], speed_y)
    }
    
    # Calculate average speed and update change_list
    if (length(change_list[[paste0("X Speed ", flyName)]]) > 0) {
      change_list[[paste0("X Speed Average ", flyName)]] <- c(change_list[[paste0("X Speed Average ", flyName)]], mean(change_list[[paste0("X Speed ", flyName)]]))
    }
    if (length(change_list[[paste0("Y Speed ", flyName)]]) > 0) {
      change_list[[paste0("Y Speed Average ", flyName)]] <- c(change_list[[paste0("Y Speed Average ", flyName)]], mean(change_list[[paste0("Y Speed ", flyName)]]))
    }
    
    # Determine whether individual passed threshold distance
    if (Ycolumn %in% names(current_data) && !is.null(current_data[[Ycolumn]]) && length(current_data[[Ycolumn]]) > 0) {
      if (any(current_data[[Ycolumn]] >= 350)) {
        change_list[[paste0("Passed ", flyName)]] <- c(change_list[[paste0("Passed ", flyName)]], 1)
      } else {
        change_list[[paste0("Passed ", flyName)]] <- c(change_list[[paste0("Passed ", flyName)]], 0)
      }
    } else {
      change_list[[paste0("Passed ", flyName)]] <- c(change_list[[paste0("Passed ", flyName)]], NA)  # Handle case when Ycolumn has no data
    }
    
  }
}

