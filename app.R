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
        numericInput("distanceThreshold", "Distance Threshold (Y)", value = 350, min = 0), 
        numericInput("timeThreshold", "Time Threshold", value = 18, min = 0, max = 90), # Time threshold input
        downloadButton("downloadRawData", "Download Raw Data"),
        downloadButton("downloadSummary", "Download Summary")
      ),
      mainPanel(
        verbatimTextOutput("summary")  # To display the summary output
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
      
      change_list_temp <- data.frame()  # Initialize temporary data frame for summary metrics
      raw_data_temp <- data.frame()      # Initialize temporary data frame for raw data
      
      # Calculate metrics for each fly 
      for (x_col in Xcolumns) {
        # Inside your for (x_col in Xcolumns) loop:
        flyName <- paste("Fly", sub("x", "", x_col))  # Extract fly number
        y_col <- sub("x", "y", x_col)
        
        current_data <- trajectories %>%
          select(time, all_of(x_col), all_of(y_col)) %>%
          filter(floor(time) <= input$timeThreshold, !is.na(.[[x_col]]))
        
        print(paste("Filtered Data for Fly:", flyName))
        print(head(current_data))  # Print filtered data for debugging
        
        if (nrow(current_data) > 1) {
          # Calculate changes as vectors
          x_vals <- current_data[[x_col]]
          y_vals <- current_data[[y_col]]
          t_vals <- current_data[["time"]]
          
          XChange <- abs(x_vals[-1] - x_vals[-length(x_vals)])
          YChange <- abs(y_vals[-1] - y_vals[-length(y_vals)])
          TimeChange <- t_vals[-1] - t_vals[-length(t_vals)]
          XSpeed <- ifelse(TimeChange > 0, XChange / TimeChange, NA)
          YSpeed <- ifelse(TimeChange > 0, YChange / TimeChange, NA)
          
          # First exceedance logic
          passed_indices <- which(y_vals >= input$distanceThreshold)
          if (length(passed_indices) > 0) {
            passed_time <- t_vals[passed_indices[1]]
            passed_value <- 1
          } else {
            passed_time <- NA
            passed_value <- 0
          }
          
          # Append to raw_data_temp
          if(length(XChange) > 0) {
            raw_data_temp <- rbind(raw_data_temp,
                                   data.frame(
                                     Fly = flyName,
                                     `X Change` = XChange,
                                     `Y Change` = YChange,
                                     Time = TimeChange,
                                     `X Speed` = XSpeed,
                                     `Y Speed` = YSpeed,
                                     Passed = c(passed_value, rep(NA, length(XChange)-1)),
                                     stringsAsFactors = FALSE
                                   )
            )
          }
          
          # Append to change_list_temp (summary)
          change_list_temp <- rbind(change_list_temp,
                                    data.frame(
                                      Fly = flyName,
                                      `X Change` = mean(XChange, na.rm = TRUE),
                                      `Y Change` = mean(YChange, na.rm = TRUE),
                                      Time = passed_time,
                                      `X Speed Average` = mean(XSpeed, na.rm = TRUE),
                                      `Y Speed Average` = mean(YSpeed, na.rm = TRUE),
                                      Passed = passed_value,
                                      stringsAsFactors = FALSE
                                    )
          )
        } else {
          print(paste("Not enough valid data for Fly", flyName))
        }
      }
      
      # Update the reactive values
      change_list(change_list_temp)  # Update summary metrics
      raw_data_for_download(raw_data_temp)  # Update raw data for download
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
                                                   `X Change` = NA, 
                                                   `Y Change` = NA, 
                                                   Time = NA,
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