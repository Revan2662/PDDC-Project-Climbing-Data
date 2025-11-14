library(shiny)
library(readr)
library(dplyr)

# Create the Shiny application
shinyApp(
  ui = fluidPage(
    titlePanel("Trajectories Analysis"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload trajectories.csv", 
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        downloadButton("downloadRawData", "Download Raw Data"),
        downloadButton("downloadSummary", "Download Summary")
      ),
      mainPanel(
        verbatimTextOutput("summary")  # Display the summary output
      )
    )
  ),
  
  server = function(input, output) {
    change_list <- reactiveVal(data.frame())  # Store summary metrics in a data frame
    raw_data_for_download <- reactiveVal(data.frame())  # Raw data storage
    
    observe({
      req(input$file)  # Ensure a file is uploaded
      
      # Load data while replacing "nan" values
      trajectories <- read_csv(input$file$datapath, na = c("", "nan"))
      print("Loaded Trajectories:")
      print(head(trajectories))
      
      # Get the names of all columns with 'x'
      Xcolumns <- grep("x", names(trajectories), value = TRUE)
      print("X Columns Detected:")
      print(Xcolumns)
      
      change_list_temp <- data.frame()  # Temporary data frame for summary metrics
      raw_data_temp <- data.frame()      # Temporary data frame for raw data
      
      # Calculate metrics for each fly
      for (x_col in Xcolumns) {
        flyName <- sub("x", "", x_col)  # Extract fly number
        y_col <- sub("x", "y", x_col)
        
        # Filter to prepare data for calculations
        current_data <- trajectories %>%
          select(time, all_of(x_col), all_of(y_col)) %>%
          filter(!is.na(.[[x_col]]))  # Filter out NA values for x column
        
        print(paste("Filtered Data for Fly:", flyName))
        print(head(current_data))
        
        # Proceed only if there's enough data
        if (nrow(current_data) > 1) {  
          # Initialize lists to hold calculated values
          XChange <- numeric()
          YChange <- numeric()
          TimeChange <- numeric()
          
          # Calculate changes over rows
          for (j in 1:(nrow(current_data) - 1)) {
            current_x <- current_data[[x_col]][j + 1]
            previous_x <- current_data[[x_col]][j]
            current_y <- current_data[[y_col]][j + 1]
            previous_y <- current_data[[y_col]][j]
            
            if (!is.na(current_x) && !is.na(previous_x)) {
              XChange <- c(XChange, abs(current_x - previous_x))
            }
            if (!is.na(current_y) && !is.na(previous_y)) {
              YChange <- c(YChange, abs(current_y - previous_y))
            }
            
            # Always capture time differences
            current_time <- current_data$time[j + 1]
            previous_time <- current_data$time[j]
            TimeChange <- c(TimeChange, abs(current_time - previous_time))
          }
          
          # Calculate whether the fly passed the threshold
          passed_value <- ifelse(any(current_data[[y_col]] >= 350, na.rm = TRUE), 1, 0)
          
          # Aggregate raw data into a full detailed table
          for (j in 1:length(XChange)) {
            raw_data_temp <- rbind(raw_data_temp, data.frame(
              Fly = flyName,
              `X Change` = if(length(XChange) > 0) XChange[j] else NA,
              `Y Change` = if(length(YChange) > 0) YChange[j] else NA,
              Time = if(length(TimeChange) > 0) TimeChange[j] else NA,
              `X Speed` = if(TimeChange[j] > 0) (XChange[j] / TimeChange[j]) else NA,
              `Y Speed` = if(TimeChange[j] > 0) (YChange[j] / TimeChange[j]) else NA,
              Passed = passed_value,
              stringsAsFactors = FALSE
            ))
          }
          
          # Append summary metrics to change_list_temp
          change_list_temp <- rbind(change_list_temp, data.frame(
            Fly = flyName,
            `X Change Average` = if(length(XChange) > 0) mean(XChange, na.rm = TRUE) else NA,
            `Y Change Average` = if(length(YChange) > 0) mean(YChange, na.rm = TRUE) else NA,
            Time = max(current_data$time, na.rm = TRUE),
            `X Speed Average` = if(length(XChange) > 0) mean(XChange / TimeChange, na.rm = TRUE) else NA,
            `Y Speed Average` = if(length(YChange) > 0) mean(YChange / TimeChange, na.rm = TRUE) else NA,
            Passed = passed_value,
            stringsAsFactors = FALSE
          ))
        } else {
          print(paste("Not enough valid data for Fly", flyName))
        }
      }
      
      # Update the reactive values
      change_list(change_list_temp)  # Update summary metrics
      raw_data_for_download(raw_data_temp)  # Store raw data for download
    })
    
    # Allow downloading of raw data results
    output$downloadRawData <- downloadHandler(
      filename = function() {
        paste("raw_data_results", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(raw_data_for_download(), file, row.names = FALSE)  # Download raw data
      }
    )
    
    # Allow downloading of summary results
    output$downloadSummary <- downloadHandler(
      filename = function() {
        paste("summary_results", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        summary_df <- change_list() 
        # Add "Total Passed" row
        total_passed <- sum(summary_df$Passed, na.rm=TRUE)
        summary_df <- rbind(summary_df, data.frame(Fly = "Total Passed", 
                                                   `X Change Average` = NA, 
                                                   `Y Change Average` = NA, 
                                                   Time = NA,
                                                   `X Speed Average` = NA, 
                                                   `Y Speed Average` = NA, 
                                                   Passed = total_passed))
        
        write.csv(summary_df, file, row.names = FALSE)  # Download summary data
      }
    )
    
    # Show a custom summary in the UI
    output$summary <- renderPrint({
      req(change_list())
      summary_df <- change_list()
      
      print("Summary Data Frame:")
      print(summary_df)  # Display the summary
    })
  }
)