library(shiny)
library(readr)
library(dplyr)
library(jsonlite)
library(stringr)
library(tibble)
library(purrr)

options(shiny.maxRequestSize = 100 * 1024^2)

# Function to parse out section of the ZIP file for naming of the flies
parse_zipname <- function(zip_name, conditions, cohorts) {
  core <- zip_name %>%
    stringr::str_remove("_\\d*\\.avi-.zip$") %>%
    stringr::str_remove("\\.avi-.zip$") %>%
    stringr::str_remove("\\.zip$") %>%
    stringr::str_remove("\\.mov$")
  cond_regex <- paste0("(", paste(conditions, collapse = "|"), ")")
  first_cond_match <- regexpr(cond_regex, core)
  if (first_cond_match[1] == -1) return(tibble(condition=character(), cohort=character(), fly_line=character()))
  core <- substr(core, first_cond_match[1], nchar(core))
  tokens <- str_split(core, "_")[[1]]
  result <- tibble(condition=character(), cohort=character(), fly_line=character())
  current_cond <- NA_character_
  current_cohort <- NA_character_
  i <- 1
  while (i <= length(tokens)) {
    token <- tokens[i]
    if (token %in% conditions) {
      current_cond <- token
      i <- i + 1
    } else if (token %in% cohorts) {
      current_cohort <- token
      i <- i + 1
    } else {
      result <- bind_rows(result, tibble(
        condition = current_cond,
        cohort = current_cohort,
        fly_line = token
      ))
      i <- i + 1
    }
  }
  result
}

# Function for parsing out the ROI list from the session.json file
parse_ROI <- function(jsonfile) {
  roi_raw <- jsonfile$roi_list
  roi_coords <- roi_raw %>%
    str_remove("^\\+ Polygon\\s*") %>%
    str_trim()
  
  roi_xy_coords <- map(roi_coords, function(rc) {
    # Remove outer [ ] if present
    rc_clean <- str_replace_all(rc, "^\\[|\\]$", "")
    # Extract all [x, y] pairs
    pairs <- str_match_all(rc_clean, "\\[([^\\]]+)\\]")[[1]][,2]
    if (is.null(pairs)) {
      # Fallback: split by "], [" if no brackets remain
      pairs <- str_split(rc_clean, "\\],\\s*\\[")[[1]]
      pairs <- str_remove_all(pairs, "\\[|\\]")
    }
    # For each pair, split and coerce to numeric
    coords <- map(pairs, ~ as.numeric(str_split(.x, ",\\s*")[[1]]))
    xs <- map_dbl(coords, 1)
    ys <- map_dbl(coords, 2)
    list(x=xs, y=ys)
  })
  
  roi_x_coords <- map(roi_xy_coords, "x")
  roi_y_coords <- map(roi_xy_coords, "y")
  roi_x_bounds <- map(roi_x_coords, ~ c(min(.x, na.rm=TRUE), max(.x, na.rm=TRUE)))
  roi_y_bounds <- map(roi_y_coords, ~ c(min(.x, na.rm=TRUE), max(.x, na.rm=TRUE)))
  tibble(
    roi_index = seq_along(roi_raw),
    roi_raw = roi_raw,
    roi_coords = roi_coords,
    roi_x_coords = map_chr(roi_x_coords, ~ paste(round(.x), collapse=",")),
    roi_y_coords = map_chr(roi_y_coords, ~ paste(round(.x), collapse=",")),
    roi_x_bounds = map_chr(roi_x_bounds, ~ paste(round(.x), collapse=",")),
    roi_y_bounds = map_chr(roi_y_bounds, ~ paste(round(.x), collapse=","))
  )
}

# Function for scoring for placement in ROIs
ROI_score <- function() {
  
  
  
  # Isolate X column values per fly
  
  
}

shinyApp(
  ui = fluidPage(
    titlePanel("Trajectories Analysis"),
    sidebarLayout(
      sidebarPanel(
        fileInput("files", "Upload ZIP file containing trajectories.csv and session.json:", multiple = TRUE, accept = ".zip"),
        numericInput("distanceThreshold", "Distance Threshold (Y)", value = 350, min = 0),
        numericInput("timeThreshold", "Time Threshold", value = 18, min = 0),
        downloadButton("downloadAll", "Download All Results (.zip)")
      ),
      mainPanel(
        verbatimTextOutput("summary")
      )
    )
  ),
  server = function(input, output) {
    change_list <- reactiveVal(data.frame())
    raw_data_for_download <- reactiveVal(data.frame())
    results_metadata <- reactiveVal(tibble(condition=character(), cohort=character(), fly_line=character()))
    roi_tbl_data <- reactiveVal(tibble())  # Store ROI tbl
    
    observe({
      req(input$files)
      temp_dir <- tempdir()
      unzip(input$files$datapath, exdir = temp_dir)
      all_files <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
      trajectories_file <- all_files[basename(all_files) == "trajectories.csv"]
      trajectories_file <- trajectories_file[which.min(nchar(trajectories_file))]
      session_file <- all_files[basename(all_files) == "session.json"]
      session_file <- session_file[which.min(nchar(session_file))] 
      # Ensures upload of ZIP file was performed and prepares relevant files inside
      
      # Finds all relevant names from the ZIP file name
      zip_name <- input$files$name
      conditions <- c("PDDC", "GW4869", "Vehicle")
      cohorts <- c("Males", "Females")
      results_metadata(parse_zipname(zip_name, conditions, cohorts))
      print("Extracted sample metadata:")
      print(results_metadata())
      
      # Finds all ROIs from the session.json file
      jsonfile <- jsonlite::fromJSON(session_file)
      roi_tbl <- parse_ROI(jsonfile)
      roi_tbl_data(roi_tbl)
      print("ROI Coordinates tibble:")
      print(roi_tbl)
      
      # Finds all flies based on X Columns found in trajectories.csv
      trajectories <- read_csv(trajectories_file, na = c("", "nan"))
      print("Loaded Trajectories:")
      print(head(trajectories))
      Xcolumns <- grep("x", names(trajectories), value = TRUE)
      print("X Columns Detected:")
      print(Xcolumns)
      
      # Preloads needed lists
      change_list_temp <- data.frame()
      raw_data_temp <- data.frame()
      
      # Creates labels for each fly and performs needed calculations to determine positions, change in positions, total speed, and passing values
      for (x_col in Xcolumns) {
        flyName <- paste("Fly", sub("x", "", x_col))
        
        y_col <- sub("x", "y", x_col)
        
        current_data <- trajectories %>%
          select(time, all_of(x_col), all_of(y_col)) %>%
          filter(round(time) <= input$timeThreshold, !is.na(.[[x_col]]))
        
        print(paste("Filtered Data for Fly:", flyName))
        print(head(current_data))
        
        # Determine ROI position
        
        if (nrow(current_data) > 1) {
          x_vals <- current_data[[x_col]]
          # Creates an ROI Score based on the detected ROIs. This is used to assign the fly to a specific ROI.
          ROI_Scores <- c()
          print("Generating ROI locations...")
          for (i in 1:nrow(roi_tbl)) {
            cat("ROI", i, "bounds:", toString(roi_tbl[[i, 6]]), "\n")
          }
          for (x_val in x_vals) {
            for (i in 1:nrow(roi_tbl)) {
              b <- as.numeric(strsplit(roi_tbl[[i, 6]], ",")[[1]])
              if (!any(is.na(c(x_val, b[1], b[2]))) &&
                  x_val >= b[1] & x_val <= b[2]) {
                ROI_Scores <- c(ROI_Scores, i)
              }
            }
          }
          ROI_1_Score <- (sum(ROI_Scores == 1)/length(ROI_Scores))*100
          ROI_2_Score <- (sum(ROI_Scores == 2)/length(ROI_Scores))*100
          ROI_3_Score <- (sum(ROI_Scores == 3)/length(ROI_Scores))*100
          print(paste(flyName, " has appeared in ROI 1 ", ROI_1_Score, "% of the time."))
          print(paste(flyName, " has appeared in ROI 2 ", ROI_2_Score, "% of the time."))
          print(paste(flyName, " has appeared in ROI 3 ", ROI_3_Score, "% of the time."))
          
          # Bounding fly to highest occurance ROI
          majority_roi <- which.max(c(ROI_1_Score, ROI_2_Score, ROI_3_Score))
          print(paste(flyName, "is bound to ROI", majority_roi, "."))
          majority_filter <- as.numeric(strsplit(roi_tbl[[majority_roi, 6]], ",")[[1]])
          filtered_data <- current_data[
            !is.na(current_data[[x_col]]) &
              current_data[[x_col]] >= majority_filter[1] &
              current_data[[x_col]] <= majority_filter[2], 
          ]
          # Subsequent data analysis for the fly will only occur in rows in which the x_vals are within the bounded ROI
          x_vals <- filtered_data[[x_col]]
          y_vals <- filtered_data[[y_col]]
          t_vals <- filtered_data[["time"]]
          XChange <- abs(x_vals[-1] - x_vals[-length(x_vals)])
          YChange <- abs(y_vals[-1] - y_vals[-length(y_vals)])
          TimeChange <- t_vals[-1] - t_vals[-length(t_vals)]
          XSpeed <- ifelse(TimeChange > 0, XChange / TimeChange, NA)
          YSpeed <- ifelse(TimeChange > 0, YChange / TimeChange, NA)
          passed_indices <- which(y_vals >= input$distanceThreshold)
          
          if (length(passed_indices) > 0) {
            passed_time <- t_vals[passed_indices[1]]
            passed_value <- 1
            
          } else {
            passed_time <- NA
            passed_value <- 0
          }
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
          change_list_temp <- rbind(change_list_temp,
                                    data.frame(
                                      Fly = flyName,
                                      `X Change Average` = mean(XChange, na.rm = TRUE),
                                      `Y Change Average` = mean(YChange, na.rm = TRUE),
                                      `Passing Time` = passed_time,
                                      `X Speed Average` = mean(XSpeed, na.rm = TRUE),
                                      `Y Speed Average` = mean(YSpeed, na.rm = TRUE),
                                      `Bound ROI` = majority_roi,
                                      Passed = passed_value,
                                      stringsAsFactors = FALSE
                                    )
          )
        } else {
          print(paste("Not enough valid data for Fly", flyName))
        }
      }
      change_list(change_list_temp)
      raw_data_for_download(raw_data_temp)
    })
    
    # Download handler for the ZIP output
    output$downloadAll <- downloadHandler(
      filename = function() {
        paste0("Climbing_Results_", Sys.Date(), ".zip")
      },
      content = function(zipfile) {
        tmpdir <- tempdir()
        # Save summary
        summary_file <- file.path(tmpdir, "Summary.csv")
        summary_df <- change_list()
        total_passed <- sum(summary_df$Passed, na.rm=TRUE)
        if(nrow(summary_df) > 0){
          totals_row <- summary_df[1,]
          totals_row[1,] <- NA
          totals_row$Fly <- "Total Passed"
          totals_row$Passed <- total_passed
          summary_df <- rbind(summary_df, totals_row)
        }
        write.csv(summary_df, summary_file, row.names = FALSE)
        # Save raw data
        raw_file <- file.path(tmpdir, "Raw_Data.csv")
        write.csv(raw_data_for_download(), raw_file, row.names = FALSE)
        # Save metadata
        meta_file <- file.path(tmpdir, "ZIP_Metadata.csv")
        write.csv(results_metadata(), meta_file, row.names = FALSE)
        # Save ROI
        roi_file <- file.path(tmpdir, "ROI_Coordinates.csv")
        roi_tbl_flat <- roi_tbl_data() %>%
          mutate(
            roi_x_coords = sapply(roi_x_coords, function(x) paste(x, collapse = ",")),
            roi_y_coords = sapply(roi_y_coords, function(y) paste(y, collapse = ",")),
            roi_x_bounds = sapply(roi_x_bounds, function(xb) paste(xb, collapse = ",")),
            roi_y_bounds = sapply(roi_y_bounds, function(yb) paste(yb, collapse = ","))
          )
        zip_meta <- results_metadata()
        roi_combined <- bind_cols(roi_tbl_flat, zip_meta)
        write.csv(roi_combined, roi_file, row.names = FALSE)
        
        zip::zip(zipfile, files = c(summary_file, raw_file, meta_file, roi_file), mode = "cherry-pick")
      }
    )
    
    output$summary <- renderPrint({
      req(change_list())
      summary_df <- change_list()
      print("Summary Data:")
      print(summary_df)
    })
  }
)