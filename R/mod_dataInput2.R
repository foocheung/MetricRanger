#' Data Input UI Function
#'
#' @description A shiny Module for data input and visualization
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tabPanel tabsetPanel selectInput uiOutput actionButton
#' @importFrom DT dataTableOutput
#' @importFrom plotly plotlyOutput
#' @importFrom slickR slickROutput

# Constants
RESOURCE_PATH <- "d"
RESOURCE_DIR <- "inst/app/www/"

# Add resource path once
addResourcePath(RESOURCE_PATH, RESOURCE_DIR)

#' Data Input UI Module
mod_dataInput_ui2 <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Data And Plotting",
    tabsetPanel(
      create_tables_tab(ns),
      create_web_report_tab(ns),
      create_heatmap_tab(ns),
      create_correlation_tab(ns),
      create_pca_tab(ns),
      create_bar_chart_tab(ns)
    )
  )
}

#' Helper function to create tables tab
create_tables_tab <- function(ns) {
  tabPanel(
    "Tables",
    selectInput(ns('column'), "Columns", choices = c("hide", "show"), "hide"),
    uiOutput(ns("tab2")),
    uiOutput(ns("tab2B"))
  )
}

#' Helper function to create web report tab
create_web_report_tab <- function(ns) {
  tabPanel(
    "10X Cell Ranger Web Report",
    uiOutput(ns("web")),
    actionButton(ns("go4"), "Go!"),
    slickR::slickROutput(ns("web_if"))
  )
}

#' Helper function to create heatmap tab
create_heatmap_tab <- function(ns) {
  tabPanel(
    "Heatmap Plot",
    uiOutput(ns('moreControls2d')),
    uiOutput(ns("dist2plot"))
  )
}

#' Helper function to create correlation tab
create_correlation_tab <- function(ns) {
  tabPanel(
    "Correlation Plot",
    uiOutput(ns("more_corr")),
    uiOutput(ns('plcor'))
  )
}

#' Helper function to create PCA tab
create_pca_tab <- function(ns) {
  tabPanel(
    "PCA Plot",
    plotOutput(ns("pca"), width = "100%")
  )
}

#' Helper function to create bar chart tab
create_bar_chart_tab <- function(ns) {
  tabPanel(
    "Bar Chart",
    uiOutput(ns("select_met")),
    uiOutput(ns("gpl"))
  )
}

#' Data Input Server Module
#'
#' @param input,output,session Standard shiny parameters
#' @param file Reactive containing file information
#'
#' @export
mod_dataInput_server2 <- function(input, output, session, file) {
  ns <- session$ns

  # Main data processing reactive
  dat2 <- reactive({
    req(file$goButtonp())
    process_data(file$df(), file$sel(), file$traf())
  })

  # Secondary data processing reactive (currently disabled)
  dat2B <- reactive({
    # TODO: Implement secondary data processing
    NULL
  })

  # UI Renderers
  render_heatmap_controls(input, output, ns)
  render_correlation_controls(input, output, ns)
  render_metric_selection(input, output, ns, dat2)
  render_web_interface(input, output, ns, dat2)
  render_data_tables(input, output, ns, dat2)

  # Plot Renderers
  render_heatmap_plot(input, output, ns, dat2)
  render_correlation_plot(input, output, ns, dat2)
  render_pca_plot(input, output, ns, dat2)
  render_bar_chart(input, output, ns, dat2)

  return(list(df = dat2))
}

#' Process data based on directory and selection parameters
#'
#' @param dir Directory path
#' @param sel Selection criteria
#' @param traf Traffic/traversal depth
#'
#' @return List containing processed data
process_data <- function(dir, sel, traf) {
  withProgress(message = "Processing data...", detail = "", value = 0, max = 100, {

    # Get file patterns based on traversal depth
    file_patterns <- get_file_patterns(traf)
    file_paths <- find_metric_files(dir, file_patterns)

    if (length(file_paths) == 0) {
      stop("No metric files found in the specified directory")
    }

    # Process each file
    processed_data <- process_metric_files(file_paths, sel)

    # Combine and transform data
    combined_data <- combine_and_transform_data(processed_data)

    return(combined_data)
  })
}

#' Get file patterns based on traversal depth
get_file_patterns <- function(traf) {
  base_patterns <- list(
    per_sample = '/*/outs/per_sample_outs/*/metrics_summary.csv',
    summary = '/*/outs/summary.csv',
    metrics = '/*/outs/metrics_summary.csv'
  )

  patterns <- base_patterns

  # Add deeper patterns based on traf parameter
  if (traf >= 2) {
    patterns <- c(patterns, list(
      per_sample_2 = '/*/*/outs/per_sample_outs/*/metrics_summary.csv',
      summary_2 = '/*/*/outs/summary.csv'
    ))
  }

  if (traf >= 3) {
    patterns <- c(patterns, list(
      per_sample_3 = '/*/*/*/outs/per_sample_outs/*/metrics_summary.csv',
      summary_3 = '/*/*/*/outs/summary.csv'
    ))
  }

  if (traf >= 4) {
    patterns <- c(patterns, list(
      per_sample_4 = '/*/*/*/*/outs/per_sample_outs/*/metrics_summary.csv'
    ))
  }

  return(patterns)
}

#' Find metric files using glob patterns
find_metric_files <- function(dir, patterns) {
  all_files <- c()

  for (pattern in patterns) {
    files <- Sys.glob(paste0(dir, pattern))
    all_files <- c(all_files, files)
  }

  return(unique(all_files))
}

#' Process individual metric files
process_metric_files <- function(file_paths, sel) {
  processed_files <- list()

  for (i in seq_along(file_paths)) {
    file_path <- file_paths[i]

    tryCatch({
      # Read and process the file
      df <- read_and_process_csv(file_path, sel)

      if (!is.null(df)) {
        processed_files[[i]] <- df
      }
    }, error = function(e) {
      warning(paste("Error processing file", file_path, ":", e$message))
    })
  }

  return(processed_files[!sapply(processed_files, is.null)])
}

#' Read and process individual CSV file
read_and_process_csv <- function(file_path, sel) {
  if (!file.exists(file_path)) {
    return(NULL)
  }

  df <- readr::read_csv(file_path, show_col_types = FALSE) %>%
    janitor::clean_names()

  # Filter by selection if not "ALL"
  if (sel != "ALL" && "library_type" %in% colnames(df)) {
    df <- df %>% dplyr::filter(library_type %in% sel)
  }

  # Add file information
  df <- add_file_metadata(df, file_path)

  return(df)
}

#' Add file metadata to dataframe
add_file_metadata <- function(df, file_path) {
  # Extract file information from path
  file_name <- basename(dirname(dirname(file_path)))
  path_parts <- strsplit(file_path, "/")[[1]]

  df$file <- file_name
  df$source_path <- file_path

  # Determine library type if not present
  if (!"library_type" %in% colnames(df)) {
    df$library_type <- determine_library_type(df)
  }

  return(df)
}

#' Determine library type based on column names
determine_library_type <- function(df) {
  col_names <- colnames(df)

  if ("Antibody: Fraction Antibody Reads" %in% col_names) {
    return("Antibody Capture")
  } else if ("Reads Mapped Confidently to Genome" %in% col_names) {
    return("Gene Expression")
  } else if ("Cells With IGH Contig" %in% col_names) {
    return("VDJ B")
  } else if ("Cells With TRA Contig" %in% col_names) {
    return("VDJ T")
  } else {
    return("Unknown")
  }
}

#' Combine and transform processed data
combine_and_transform_data <- function(processed_files) {
  if (length(processed_files) == 0) {
    return(list(s = NULL, s1 = NULL, ff = NULL, f = NULL, flst = NULL))
  }

  # Combine all dataframes
  combined_df <- dplyr::bind_rows(processed_files)

  # Transform data for visualization
  transformed_data <- transform_for_visualization(combined_df)

  return(transformed_data)
}

#' Transform data for visualization
transform_for_visualization <- function(df) {
  # Remove duplicates and prepare for pivoting
  df_clean <- df %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(metric_name)) %>%
    dplyr::mutate(unique_id = dplyr::row_number())

  # Create wide format for plotting
  df_wide <- df_clean %>%
    tidyr::pivot_wider(
      names_from = file,
      values_from = value,
      names_prefix = "sample_"
    ) %>%
    dplyr::select(-unique_id)

  return(list(
    s = df_wide,
    s1 = df_clean,
    ff = unique(df_clean$source_path),
    f = tibble::tibble(value = unique(df_clean$source_path)),
    flst = create_file_list(df_clean)
  ))
}

#' Create file list summary
create_file_list <- function(df) {
  df %>%
    dplyr::distinct(source_path, file, library_type) %>%
    dplyr::mutate(
      run_directory = dirname(dirname(source_path)),
      sample_directory = basename(dirname(source_path))
    ) %>%
    dplyr::select(source_path, run_directory, sample_directory, library_type) %>%
    dplyr::rename(path = source_path, library = library_type)
}

# UI Rendering Functions
render_heatmap_controls <- function(input, output, ns) {
  output$moreControls2d <- renderUI({
    tagList(
      div(
        style = "display: inline-block; width: 25%;",
        sliderInput(ns("width"), "Plot Width:", min = 400, max = 2000, value = 1200)
      ),
      div(
        style = "display: inline-block; width: 25%;",
        sliderInput(ns("height"), "Plot Height:", min = 400, max = 2000, value = 600)
      ),
      div(
        style = "display: inline-block; width: 25%;",
        selectInput(ns("norm"), "Normalization",
                    choices = c("normalize", "percentize", "scale"),
                    selected = "normalize")
      ),
      actionButton(ns("goplot"), "HeatMap", style = 'margin-top:25px')
    )
  })
}

render_correlation_controls <- function(input, output, ns) {
  output$more_corr <- renderUI({
    tagList(
      div(
        style = "display: inline-block; width: 25%;",
        sliderInput(ns("widthcor"), "Plot Width:", min = 400, max = 2000, value = 600)
      ),
      div(
        style = "display: inline-block; width: 25%;",
        sliderInput(ns("heightcor"), "Plot Height:", min = 400, max = 2000, value = 600)
      ),
      actionButton(ns("goplotcor"), "Plot", style = 'margin-top:25px')
    )
  })
}

render_metric_selection <- function(input, output, ns, dat2) {
  output$select_met <- renderUI({
    data <- dat2()
    if (is.null(data$s)) return(NULL)

    tagList(
      div(
        style = "display: inline-block; width: 20%;",
        selectInput(ns("sel"), "Select Metrics:",
                    choices = unique(data$s$metric_name),
                    multiple = TRUE)
      ),
      div(
        style = "display: inline-block; width: 20%;",
        sliderInput(ns("bar_width"), "Plot Width:", min = 400, max = 2000, value = 900)
      ),
      div(
        style = "display: inline-block; width: 20%;",
        sliderInput(ns("bar_height"), "Plot Height:", min = 400, max = 2000, value = 600)
      ),
      div(
        style = "display: inline-block; width: 20%;",
        sliderInput(ns("font"), "Font Size:", min = 8, max = 24, value = 12)
      ),
      div(
        style = "display: inline-block; width: 20%;",
        actionButton(ns("gosel"), "Plot")
      )
    )
  })
}

render_web_interface <- function(input, output, ns, dat2) {
  output$web <- renderUI({
    data <- dat2()
    if (is.null(data$ff)) return(NULL)

    web_urls <- data$ff %>%
      stringr::str_replace("(summary|metrics_summary)\\.csv$", "web_summary.html")

    tagList(
      fluidPage(
        column(12,
               selectInput(ns('websumm'), "Web Summary",
                           choices = web_urls,
                           width = '700px')
        )
      )
    )
  })
}

render_data_tables <- function(input, output, ns, dat2) {
  output$tab2 <- renderUI({
    data <- dat2()
    if (is.null(data$s)) return(NULL)

    create_data_table(data$s, input$column == "show")
  })
}

# Plot Rendering Functions
render_heatmap_plot <- function(input, output, ns, dat2) {
  output$dist2plot <- renderUI({
    req(input$goplot)
    plotly::plotlyOutput(ns("distPlot"))
  })

  output$distPlot <- plotly::renderPlotly({
    req(input$goplot)
    data <- dat2()
    if (is.null(data$s)) return(NULL)

    create_heatmap(data$s, input$norm, input$width, input$height)
  })
}

render_correlation_plot <- function(input, output, ns, dat2) {
  output$plcor <- renderUI({
    req(input$goplotcor)
    plotOutput(ns("heatmap"), width = input$widthcor, height = input$heightcor)
  })

  output$heatmap <- renderPlot({
    data <- dat2()
    if (is.null(data$s)) return(NULL)

    create_correlation_plot(data$s)
  })
}

render_pca_plot <- function(input, output, ns, dat2) {
  output$pca <- renderPlot({
    data <- dat2()
    if (is.null(data$s)) return(NULL)

    create_pca_plot(data$s)
  })
}

render_bar_chart <- function(input, output, ns, dat2) {
  output$gpl <- renderUI({
    req(input$gosel, input$sel)

    renderPlot({
      data <- dat2()
      if (is.null(data$s)) return(NULL)

      create_bar_chart(data$s, input$sel, input$font)
    }, width = input$bar_width, height = input$bar_height)
  })
}

# Helper plotting functions
create_heatmap <- function(data, norm_method, width, height) {
  # Prepare data for heatmap
  numeric_data <- data %>%
    dplyr::select(where(is.numeric)) %>%
    dplyr::select(-any_of(c("unique_id"))) %>%
    as.matrix()

  if (nrow(numeric_data) == 0 || ncol(numeric_data) == 0) {
    return(NULL)
  }

  # Apply normalization
  if (norm_method == "normalize") {
    normalized_data <- heatmaply::normalize(numeric_data)
  } else if (norm_method == "percentize") {
    normalized_data <- heatmaply::percentize(numeric_data)
  } else {
    normalized_data <- scale(numeric_data)
  }

  heatmaply::heatmaply(
    normalized_data,
    Rowv = FALSE,
    Colv = FALSE,
    plot_method = "plotly",
    width = width,
    height = height
  )
}

create_correlation_plot <- function(data) {
  numeric_data <- data %>%
    dplyr::select(where(is.numeric)) %>%
    dplyr::select(-any_of(c("unique_id")))

  if (ncol(numeric_data) < 2) {
    return(ggplot2::ggplot() + ggplot2::geom_text(ggplot2::aes(0, 0, label = "Insufficient data")))
  }

  corr_matrix <- cor(numeric_data, use = "complete.obs")
  ggcorrplot::ggcorrplot(corr_matrix)
}

create_pca_plot <- function(data) {
  numeric_data <- data %>%
    dplyr::select(where(is.numeric)) %>%
    dplyr::select(-any_of(c("unique_id"))) %>%
    na.omit()

  if (nrow(numeric_data) < 2 || ncol(numeric_data) < 2) {
    return(ggplot2::ggplot() + ggplot2::geom_text(ggplot2::aes(0, 0, label = "Insufficient data")))
  }

  pca_result <- stats::prcomp(numeric_data, scale. = TRUE)
  factoextra::fviz_pca_ind(pca_result)
}

create_bar_chart <- function(data, selected_metrics, font_size) {
  if (is.null(selected_metrics) || length(selected_metrics) == 0) {
    return(ggplot2::ggplot() + ggplot2::geom_text(ggplot2::aes(0, 0, label = "No metrics selected")))
  }

  plot_data <- data %>%
    dplyr::filter(metric_name %in% selected_metrics) %>%
    tidyr::pivot_longer(cols = starts_with("sample_"), names_to = "sample", values_to = "value") %>%
    dplyr::filter(!is.na(value))

  if (nrow(plot_data) == 0) {
    return(ggplot2::ggplot() + ggplot2::geom_text(ggplot2::aes(0, 0, label = "No data available")))
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(sample, -value), y = value)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = round(value, 2)), vjust = -0.5, size = font_size/4) +
    ggplot2::facet_wrap(library_type ~ metric_name, ncol = 5, scales = "free") +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 90)) +
    ggplot2::labs(x = "", y = "Value") +
    ggplot2::theme_minimal(base_size = font_size)
}

create_data_table <- function(data, show_columns) {
  if (is.null(data) || nrow(data) == 0) {
    return(DT::datatable(data.frame(Message = "No data available")))
  }

  # Format numeric columns
  formatted_data <- data %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ sprintf("%.2f", .)))

  dt_options <- list(
    dom = 'Bfrtip',
    pageLength = 25,
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    scrollX = TRUE,
    scrollY = "400px"
  )

  if (!show_columns) {
    dt_options$colnames <- rep("", ncol(formatted_data))
  }

  DT::datatable(
    formatted_data,
    extensions = 'Buttons',
    options = dt_options
  ) %>%
    DT::formatStyle(
      columns = names(dplyr::select(data, where(is.numeric))),
      fontWeight = "bold"
    )
}
