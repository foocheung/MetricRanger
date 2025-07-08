#' dataInput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
addResourcePath("d", "inst/app/www/")

mod_dataInput_ui2 <- function(id) {
  ns <- NS(id)
  tabPanel("Data And Plotting",
           tabsetPanel(
             tabPanel("Tables",
                      selectInput(ns('column'), "Columns", choices = c("hide", "show"), "hide"),
                      uiOutput(ns("tab2")),
                      uiOutput(ns("tab2B"))
             ),
             tabPanel("10X Cell Ranger Web Report",
                      uiOutput(ns("web")),
                      actionButton(ns("go4"), "Go!"),
                      slickR::slickROutput(ns("web_if"))
             ),
             tabPanel("Heatmap Plot",
                      uiOutput(ns('moreControls2d')),
                      uiOutput(ns("dist2plot"))
             ),
             tabPanel("Correlation Plot",
                      uiOutput(ns("more_corr")),
                      uiOutput(ns('plcor'))
             ),
             tabPanel("PCA Plot",
                      plotOutput(ns("pca"), width = "100%")
             ),
             tabPanel("Bar Chart",
                      uiOutput(ns("select_met")),
                      uiOutput(ns("gpl"))
             )
           )
  )
}

# Module Server
#' @noRd
#' @export
#' @keywords internal
mod_dataInput_server2 <- function(input, output, session, file) {
  ns <- session$ns

  # Helper function to read CSV and add file column
  read_csv_and_add_file_column <- function(file_path) {
    df <- readr::read_csv(file_path)
    file_name <- basename(dirname(dirname(file_path)))
    df$file <- file_name

    # Determine the Library based on column names
    if ("Antibody: Fraction Antibody Reads" %in% colnames(df)) {
      df$library_type <- "Antibody Capture"
    } else if ("Reads Mapped Confidently to Genome" %in% colnames(df)) {
      df$library_type <- "Gene Expression"
    } else if ("Cells With IGH Contig" %in% colnames(df)) {
      df$library_type <- "VDJ B"
    } else if ("Cells With TRA Contig" %in% colnames(df)) {
      df$library_type <- "VDJ T"
    } else {
      df$library_type <- NA
    }

    return(df)
  }

  # Helper function to add identifier
  add_identifier <- function(df) {
    df %>% dplyr::mutate(unique_id = dplyr::row_number())
  }

  # Main data processing function
  processData <- function(dir, sel, traf) {
    req(file$goButtonp())

    isolate({
      withProgress(message = "Please wait...", detail = "", value = 0, max = 100, {

        # Define file patterns based on traf parameter
        if (traf == 1) {
          metric_patterns <- c(
            '/*/outs/per_sample_outs/*/metrics_summary.csv',
            '/*/outs/summary.csv',
            '/*/outs/metrics_summary.csv'
          )
        } else if (traf == 2) {
          metric_patterns <- c(
            '/*/*/outs/per_sample_outs/*/metrics_summary.csv',
            '/*/outs/per_sample_outs/*/metrics_summary.csv',
            '/*/outs/summary.csv',
            '/*/*/outs/summary.csv'
          )
        } else if (traf == 3) {
          metric_patterns <- c(
            '/*/*/outs/per_sample_outs/*/metrics_summary.csv',
            '/*/outs/per_sample_outs/*/metrics_summary.csv',
            '/*/*/*/outs/per_sample_outs/*/metrics_summary.csv',
            '/*/outs/summary.csv',
            '/*/*/outs/summary.csv',
            '/*/*/*/outs/summary.csv'
          )
        } else if (traf == 4) {
          metric_patterns <- c(
            '/*/*/outs/per_sample_outs/*/metrics_summary.csv',
            '/*/outs/per_sample_outs/*/metrics_summary.csv',
            '/*/*/*/outs/per_sample_outs/*/metrics_summary.csv',
            '/*/*/*/*/outs/per_sample_outs/*/metrics_summary.csv'
          )
        }

        # Get all matching files
        ff <- unlist(lapply(metric_patterns, function(pattern) {
          Sys.glob(paste(dir, pattern, sep = ""))
        }))

        # Sort files by numeric order
        ff2 <- gsub(".*per_sample_outs\\/", "", ff)
        ff3 <- gsub("\\/.*", "", ff2)

        f <- ff %>%
          tibble::as_tibble() %>%
          dplyr::mutate("n" = gsub(".*multi_config_", "", value)) %>%
          dplyr::mutate("n" = gsub("\\/.*", "", n)) %>%
          dplyr::arrange(as.numeric(n))

        ff <- f$value

        # Read and combine data
        data_list <- purrr::map(ff, ~ readr::read_csv(.x))
        combined_data <- dplyr::bind_rows(data_list, .id = "file_id")
        tidy_data <- dplyr::as_tibble(combined_data)

        df.list <- list()
        f.list <- list()

        # Process each file
        for (i in 1:length(ff)) {
          file_o <- gsub(".*\\/(.*)\\/(.*)\\/outs\\/(['summary.csv'\\|'metrics_summary.csv'])", "\\3", perl = TRUE, ff[i])
          lines <- readr::read_csv(ff[i]) %>% nrow()

          if (lines == 1) {
            # Single line processing
            path <- gsub(".*\\/.*\\/.*\\/(.*)\\/outs", "\\1", perl = TRUE, ff[i])
            id <- gsub(".*\\/.*\\/(.*)\\/(.*)\\/outs", "\\1_\\2", perl = TRUE, ff[i])

            list_of_dataframes <- lapply(ff, read_csv_and_add_file_column)
            combined_df <- dplyr::bind_rows(list_of_dataframes)

            transposed_df <- t(combined_df)
            file_row_index <- which(rownames(transposed_df) == "file")
            colnames(transposed_df) <- transposed_df[file_row_index, ]
            transposed_df <- transposed_df[-nrow(transposed_df), ]

            s <- as.data.frame(transposed_df)
            s1 <- transposed_df[file_row_index, ]
            libs <- "Gene Expression"

          } else {
            # Multi-line processing
            path <- gsub("(.*)\\/outs.*", "\\1", perl = TRUE, ff[i])
            id <- gsub(".*\\/(.*)\\/(.*)\\/outs.*", "\\1_\\2", perl = TRUE, ff[i])

            if (sel == "ALL") {
              df <- readr::read_csv(ff[i]) %>% janitor::clean_names()
            } else {
              df <- readr::read_csv(ff[i]) %>%
                janitor::clean_names() %>%
                dplyr::filter(library_type %in% sel)
            }

            df_all <- readr::read_csv(ff[i]) %>% janitor::clean_names()
            libs <- dplyr::pull(df_all, library_type) %>% unique()

            colnames(df)[6] <- paste("sample", id, sep = "_")

            if (file_o == 'summary.csv') {
              df[6] <- df[[6]] %>% tibble::as_tibble()
              df.list[[i]] <- df
            } else {
              df[6] <- readr::parse_number(df[[6]]) %>% tibble::as_tibble()
              df.list[[i]] <- df
            }

            Split <- strsplit(ff[i], "\\/")
            s1 <- Split[[1]][length(Split[[1]]) - 5]
            s2 <- Split[[1]][length(Split[[1]]) - 4]
            s3 <- dput(libs)

            f.list[[i]] <- c(path, Split[[1]][length(Split[[1]]) - 5],
                             Split[[1]][length(Split[[1]]) - 4], deparse(dput(libs)))
          }

          # Create file list dataframe
          flst <- do.call(rbind.data.frame, f.list) %>%
            tibble::as_tibble()
          colnames(flst) <- c("path", "run_directory", "sample_directory", "library")

          # Add identifiers and filter
          library(tidyverse)

          df.list_with_id <- lapply(df.list, add_identifier)
          df.list_with_id <- lapply(df.list_with_id, function(x) {
            dplyr::filter(x, grouped_by != 'Fastq ID')
          })

          # Process and combine data
          s <- df.list_with_id %>%
            purrr::map(subset, select = -c(unique_id)) %>%
            purrr::reduce(dplyr::bind_rows) %>%
            tidyr::pivot_longer(
              cols = dplyr::starts_with("sample"),
              values_to = "values"
            ) %>%
            dplyr::distinct() %>%
            na.omit(values) %>%
            dplyr::mutate(group_name = dplyr::if_else(grouped_by == "Fastq ID", "Fastq ID", group_name)) %>%
            tidyr::pivot_wider(
              names_from = name,
              values_from = values
            )
        }
      })
    })

    return(list(s = s, s1 = s1, ff = ff, f = f, flst = flst))
  }

  # Reactive data
  dat2 <- reactive({
    req(file$goButtonp())
    processData(file$df(), file$sel(), file$traf())
  })

  # UI Controls
  output$moreControls2d <- renderUI({
    tagList(
      div(style = "display: inline-block; horizontal-align: top; width: 25%;",
          sliderInput(ns("width"), "Plot Width:", min = 400, max = 2000, value = 1200)),
      div(style = "display: inline-block; horizontal-align: top; width: 25%;",
          sliderInput(ns("height"), "Plot Height:", min = 400, max = 2000, value = 600)),
      div(style = "display: inline-block; vertical-align: top; width: 15%;",
          selectInput(ns("norm"), "Normalization",
                      c("normalize", "percentize", "scale"), selected = "normalize")),
      actionButton(ns("goplot"), label = "HeatMap", style = 'margin-top: 25px')
    )
  })

  output$more_corr <- renderUI({
    tagList(
      div(style = "display: inline-block; horizontal-align: top; width: 25%;",
          sliderInput(ns("widthcor"), "Plot Width:", min = 400, max = 2000, value = 600)),
      div(style = "display: inline-block; horizontal-align: top; width: 25%;",
          sliderInput(ns("heightcor"), "Plot Height:", min = 400, max = 2000, value = 600)),
      actionButton(ns("goplotcor"), "Plot", style = 'margin-top: 25px')
    )
  })

  output$select_met <- renderUI({
    e <- dat2()$s
    tagList(
      div(style = "display: inline-block; width: 15%; vertical-align: bottom;",
          selectInput(ns("sel"), "Select:", choices = e$metric_name, multiple = TRUE)),
      div(style = "display: inline-block; width: 15%; vertical-align: top;",
          sliderInput(ns("bar_width"), "Plot Width:", min = 400, max = 2000, value = 900)),
      div(style = "display: inline-block; width: 15%; vertical-align: top;",
          sliderInput(ns("bar_height"), "Plot Height:", min = 400, max = 2000, value = 600)),
      div(style = "display: inline-block; width: 15%; vertical-align: top;",
          sliderInput(ns("font"), "Font Size:", min = 1, max = 80, value = 25)),
      div(style = "display: inline-block; width: 10%; vertical-align: top;",
          actionButton(ns("gosel"), "Plot"))
    )
  })

  # Reactive selection
  rsel <- reactive({
    req(input$gosel)
    sss_l <- dat2()$s %>%
      gather(key = "key", value = "value", starts_with("sample_")) %>%
      dplyr::filter(metric_name %in% input$sel)

    return(sss_l)
  })

  # Plot outputs
  output$gpl <- renderUI({
    req(input$gosel)
    renderPlot({
      sss_l <- rsel()
      ggplot2::ggplot(sss_l, ggplot2::aes(x = reorder(key, -value), y = value)) +
        ggplot2::geom_col() +
        ggplot2::geom_text(ggplot2::aes(label = value), vjust = -0.5) +
        ggplot2::facet_wrap(library_type ~ metric_name, ncol = 5, scales = "free") +
        ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 90)) +
        ggplot2::xlab("") +
        ggplot2::theme_grey(base_size = input$font)
    }, width = input$bar_width, height = input$bar_height)
  })

  output$dist2plot <- renderUI({
    req(input$goplot)
    isolate({
      plotly::plotlyOutput(ns("distPlot"))
    })
  })

  output$distPlot <- plotly::renderPlotly({
    req(input$goplot)
    isolate({
      e <- dat2()$s
      e <- e %>% dplyr::distinct(metric_name, .keep_all = TRUE)
      e <- e[5:length(e)]
      e <- e[rowSums(e[-1]) > 0, ]
      d <- e[, -1]
      rownames(d) <- make.names(t(e[1]), unique = TRUE)
      d <- t(janitor::remove_constant(t(d)))

      if (input$norm == "normalize") {
        heatmaply::heatmaply(
          t(heatmaply::normalize(t(as.matrix(as.data.frame(d))))),
          Rowv = "FALSE", Colv = "FALSE", plot_method = "plotly",
          labRow = rownames(d), row_side_colors = rownames(d),
          width = input$width, height = input$height
        )
      } else if (input$norm == "percentize") {
        heatmaply::heatmaply(
          t(heatmaply::percentize(t(as.matrix(as.data.frame(d))))),
          Rowv = "FALSE", Colv = "FALSE", plot_method = "plotly",
          labRow = rownames(d), row_side_colors = rownames(d),
          width = input$width, height = input$height
        )
      } else if (input$norm == "scale") {
        heatmaply::heatmaply(
          scale = "row", as.matrix(as.data.frame(t(janitor::remove_constant(t(d))))),
          Rowv = "FALSE", Colv = "FALSE", plot_method = "plotly",
          labRow = rownames(d), row_side_colors = rownames(d),
          width = input$width, height = input$height
        )
      }
    })
  })

  output$pca <- renderPlot({
    e <- dat2()$s
    e <- e[5:length(e)]
    rownames(e) <- make.names(e$metric_name, unique = TRUE)
    factoextra::fviz_pca_ind(stats::prcomp(na.omit(t(e[-1]))))
  })

  output$plcor <- renderUI({
    req(input$goplotcor)
    output$heatmap <- renderPlot({
      e <- dat2()$s
      e <- e %>% dplyr::distinct(metric_name, .keep_all = TRUE)
      e <- e[5:length(e)]
      corr_matrix <- cor(t(e[, -1]))
      rownames(corr_matrix) <- make.names(t(e[1]), unique = TRUE)
      colnames(corr_matrix) <- make.names(t(e[1]), unique = TRUE)
      ggcorrplot::ggcorrplot(corr_matrix)
    }, width = input$widthcor, height = input$heightcor)
  })

  # Data table output
  output$tab2 <- renderUI({
    e <- dat2()$s
    output$tab <- DT::renderDataTable({
      if (input$column == "hide") {
        DT::datatable(
          cbind(
            e %>% dplyr::mutate(dplyr::across(where(is.numeric), sprintf, fmt = '%.2f')),
            prop.table(data.matrix(dat2()$s[4:length(dat2()$s)] %>%
                                     dplyr::mutate_if(is.character, as.numeric)), margin = 1) %>%
              tibble::as_tibble() %>%
              dplyr::rename_with(~ paste0("prop_", .x))
          ),
          colnames = rep("", ncol(e)),
          extensions = 'Buttons',
          options = list(
            columnDefs = list(list(
              targets = c((length(dat2()$s) + 1):(length(dat2()$s) * 2 - 3)),
              visible = FALSE
            )),
            dom = 'Bfrtip',
            pageLength = 500,
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
          )
        ) %>%
          DT::formatStyle(
            names(dat2()$s[4:length(dat2()$s)]),
            background = DT::styleColorBar(range(0, 1), 'lightblue'),
            backgroundSize = '98% 88%',
            valueColumns = c((length(dat2()$s) + 1):(length(dat2()$s) * 2 - 3)),
            fontWeight = "bold",
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          ) %>%
          DT::formatStyle(names(dat2()$s[1:3]), fontWeight = "bold")
      } else {
        DT::datatable(
          cbind(
            e %>% dplyr::mutate(dplyr::across(where(is.numeric), sprintf, fmt = '%.2f')),
            prop.table(data.matrix(dat2()$s[4:length(dat2()$s)] %>%
                                     dplyr::mutate_if(is.character, as.numeric)), margin = 1) %>%
              tibble::as_tibble() %>%
              dplyr::rename_with(~ paste0("prop_", .x))
          ),
          extensions = 'Buttons',
          options = list(
            columnDefs = list(list(
              targets = c((length(dat2()$s) + 1):(length(dat2()$s) * 2 - 3)),
              visible = FALSE
            )),
            dom = 'Bfrtip',
            pageLength = 500,
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
          )
        ) %>%
          DT::formatStyle(
            names(dat2()$s[4:length(dat2()$s)]),
            background = DT::styleColorBar(range(0, 1), 'lightblue'),
            backgroundSize = '98% 88%',
            valueColumns = c((length(dat2()$s) + 1):(length(dat2()$s) * 2 - 3)),
            fontWeight = "bold",
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          ) %>%
          DT::formatStyle(names(dat2()$s[1:3]), fontWeight = "bold")
      }
    })
  })

  # Web summary interface
  output$web <- renderUI({
    loc <- dat2()$ff
    names <- gsub(".*\\/(.*)\\/(.*)\\/outs.*", "\\2", perl = TRUE, loc)
    ur <- loc %>%
      tibble::as_tibble() %>%
      dplyr::mutate(url = gsub("summary.csv|metrics_summary.csv", "web_summary.html", value)) %>%
      dplyr::select(url) %>%
      dplyr::pull()

    tagList(
      fluidPage(
        column(12,
               selectInput(ns('websumm'), "Web Summary", choices = dput(ur), width = '700px')
        )
      )
    )
  })

  # Load picture reactive
  loadpic <- reactive({
    validate(need(input$websumm, "Member input is null!!"))
    uu <- input$websumm
    m <- paste("/", paste(gtools::split_path(dirname(uu), depth_first = FALSE)[1:(length(unlist(strsplit(uu, '/'))) - 6)], collapse = '/'), sep = "")
    m <- gsub("//", "/", m)
    addResourcePath("library", m)

    m2 <- paste(gtools::split_path(dirname(uu), depth_first = FALSE)[(length(unlist(strsplit(uu, '/'))) - 5):(length(unlist(strsplit(uu, '/'))) - 2)], collapse = '/')
    m2 <- paste(m2, "/web_summary.html", sep = "")
    paste("library/", m2, sep = "")
  })

  output$web_if <- slickR::renderSlickR({
    req(input$go4)
    url1 <- input$websumm

    slickR::slickR(slickR::slick_list(
      tags$iframe(
        height = "600px",
        width = "900px",
        scrolling = TRUE,
        src = loadpic(),
        height = 500,
        id = "theframe"
      )
    ))
  })

  return(list(df = dat2))
}

## To be copied in the UI
# mod_dataInput_ui("dataInput_1")

## To be copied in the server
# mod_dataInput_server("dataInput_1")
