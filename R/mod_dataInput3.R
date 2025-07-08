#' dataInput UI Function
#'
#' @description A shiny Module for loading and processing data directories.
#'
#' @param id Module ID
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tabPanel actionButton icon
#' @importFrom shinyFiles shinyDirButton
#' @importFrom shinyscreenshot screenshotButton

mod_dataInput_ui3 <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Delta",
    fluidRow(
      column(
        width = 12,
        h4("Data Directory Selection"),
        br(),
        fluidRow(
          column(
            width = 4,
            shinyFiles::shinyDirButton(
              ns("directory2"),
              "Load Directory 1",
              icon = icon("folder-open"),
              style = "width: 100%;"
            )
          ),
          column(
            width = 4,
            shinyFiles::shinyDirButton(
              ns("directory3"),
              "Load Directory 2",
              icon = icon("folder-open"),
              style = "width: 100%;"
            )
          ),
          column(
            width = 4,
            actionButton(
              ns("goButtonpd"),
              "Process Data",
              icon = icon("play"),
              class = "btn-primary",
              style = "width: 100%;"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 12,
            shinyscreenshot::screenshotButton(
              label = "Capture Screenshot",
              class = "btn-secondary"
            )
          )
        )
      )
    )
  )
}

#' dataInput Server Function
#'
#' @description Server logic for the dataInput module
#'
#' @param input,output,session Internal parameters for {shiny}
#'
#' @noRd
#' @export
#' @keywords internal

mod_dataInput_server3 <- function(input, output, session) {
  ns <- session$ns

  # Initialize volumes
  volumes <- shinyFiles::getVolumes()()

  # Directory choosers
  shinyFiles::shinyDirChoose(input, "directory2", roots = volumes, session = session)
  shinyFiles::shinyDirChoose(input, "directory3", roots = volumes, session = session)

  # Reactive values for storing processed data
  values <- reactiveValues(
    processed_data = NULL,
    file_list = NULL,
    summary_info = NULL
  )

  # Helper function to validate directory path
  validate_directory <- function(dir_path) {
    if (is.null(dir_path) || length(dir_path) == 0 || !dir.exists(dir_path)) {
      return(FALSE)
    }
    return(TRUE)
  }

  # Helper function to find metric files
  find_metric_files <- function(base_dir) {
    metric_patterns <- c(
      "/*/outs/per_sample_outs/*/metrics_summary.csv",
      "/*/outs/summary.csv"
    )

    all_files <- c()
    for (pattern in metric_patterns) {
      files <- Sys.glob(paste0(base_dir, pattern))
      all_files <- c(all_files, files)
    }

    return(all_files)
  }

  # Helper function to process individual file
  process_metric_file <- function(file_path, selected_library = "ALL") {
    tryCatch({
      # Determine file type
      file_type <- if (grepl("summary\\.csv$", file_path)) "summary.csv" else "metrics_summary.csv"

      if (file_type == "summary.csv") {
        # Process summary.csv files
        data <- readr::read_csv(file_path, show_col_types = FALSE) %>%
          janitor::clean_names()

        # Extract path and ID information
        path <- gsub("(.*)/outs/summary.csv", "\\1", file_path, perl = TRUE)
        id <- gsub(".*/([^/]+)/([^/]+)/outs/summary.csv", "\\1_\\2", file_path, perl = TRUE)

        # Transpose and format data
        data_transposed <- data %>%
          t() %>%
          as.data.frame() %>%
          tibble::rownames_to_column("metric_name") %>%
          setNames(c("metric_name", paste0("sample_", id)))

        # Add metadata columns
        processed_data <- data_transposed %>%
          dplyr::mutate(
            category = "Library",
            library_type = "ATAC",
            group_name = NA_character_,
            group_by = NA_character_
          ) %>%
          dplyr::select(category, library_type, group_by, group_name, metric_name, dplyr::everything()) %>%
          dplyr::slice(-c(1:3)) %>%  # Remove first 3 rows
          dplyr::filter(!grepl("q30", metric_name))

        return(processed_data)

      } else {
        # Process metrics_summary.csv files
        data <- readr::read_csv(file_path, show_col_types = FALSE) %>%
          janitor::clean_names()

        # Extract path and ID information
        path <- gsub("(.*)/outs.*", "\\1", file_path, perl = TRUE)
        id <- gsub(".*/([^/]+)/([^/]+)/outs.*", "\\1_\\2", file_path, perl = TRUE)

        # Filter by library type if specified
        if (selected_library != "ALL") {
          data <- data %>%
            dplyr::filter(library_type %in% selected_library)
        }

        # Add sample column
        data <- data %>%
          dplyr::mutate(!!paste0("sample_", id) := readr::parse_number(as.character(.data[[ncol(data)]])))

        return(data)
      }
    }, error = function(e) {
      warning(paste("Error processing file:", file_path, "-", e$message))
      return(NULL)
    })
  }

  # Reactive for directory path 2
  path2 <- reactive({
    req(input$directory2)
    path <- shinyFiles::parseDirPath(volumes, input$directory2)
    validate(need(validate_directory(path), "Please select a valid directory"))
    return(path)
  })

  # Reactive for directory path 3
  path3 <- reactive({
    req(input$directory3)
    path <- shinyFiles::parseDirPath(volumes, input$directory3)
    validate(need(validate_directory(path), "Please select a valid directory"))
    return(path)
  })

  # Main data processing reactive
  processed_data <- eventReactive(input$goButtonpd, {
    req(path2())

    withProgress(message = "Processing data...", detail = "", value = 0, max = 100, {

      # Find all metric files
      incProgress(20, detail = "Finding metric files...")
      metric_files <- find_metric_files(path2())

      if (length(metric_files) == 0) {
        stop("No metric files found in the selected directory")
      }

      # Sort files by configuration number
      incProgress(10, detail = "Sorting files...")
      file_info <- tibble::tibble(value = metric_files) %>%
        dplyr::mutate(
          config_num = stringr::str_extract(value, "multi_config_([0-9]+)") %>%
            stringr::str_extract("[0-9]+") %>%
            as.numeric()
        ) %>%
        dplyr::arrange(config_num) %>%
        dplyr::pull(value)

      # Process each file
      incProgress(20, detail = "Processing files...")
      processed_files <- list()
      file_metadata <- list()

      for (i in seq_along(file_info)) {
        incProgress(40 / length(file_info), detail = paste("Processing file", i, "of", length(file_info)))

        file_path <- file_info[i]
        processed_file <- process_metric_file(file_path, selected_library = "ALL")

        if (!is.null(processed_file)) {
          processed_files[[i]] <- processed_file

          # Extract metadata
          path_parts <- strsplit(file_path, "/")[[1]]
          file_metadata[[i]] <- list(
            path = file_path,
            run_directory = path_parts[length(path_parts) - 5],
            sample_directory = path_parts[length(path_parts) - 4],
            library = "processed"
          )
        }
      }

      # Combine processed files
      incProgress(10, detail = "Combining results...")

      # Remove empty results
      processed_files <- purrr::discard(processed_files, is.null)

      if (length(processed_files) == 0) {
        stop("No files could be processed successfully")
      }

      # Create metadata dataframe
      metadata_df <- dplyr::bind_rows(file_metadata) %>%
        tibble::as_tibble()

      # Store results
      values$processed_data <- processed_files
      values$file_list <- metadata_df
      values$summary_info <- list(
        total_files = length(file_info),
        processed_files = length(processed_files),
        directory = path2()
      )

      return(list(
        data = processed_files,
        metadata = metadata_df,
        summary = values$summary_info
      ))
    })
  })

  # Return reactive values for use in other modules
  return(list(
    processed_data = reactive({ values$processed_data }),
    file_metadata = reactive({ values$file_list }),
    summary_info = reactive({ values$summary_info }),
    path2 = path2,
    path3 = path3
  ))
}
