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
mod_dataInput_ui2 <- function(id){
  ns <- NS(id)

  tabPanel("Data And Plotting",
           tabsetPanel(

             tabPanel("Tables",
                      #DT::dataTableOutput(ns('tab2'))
                      selectInput(ns('column'), "Columns", choices = c("hide", "show"),"hide"),
                      uiOutput(ns("tab2")),
                      uiOutput(ns("tab2B"))
             ),
             tabPanel("10X Cell Ranger Web Report",

                      uiOutput(ns("web")),
                      actionButton(ns("go4"),"Go!"),
                      # htmlOutput(ns("web_if")) #,
                      slickR::slickROutput(ns("web_if"))
                      # htmlOutput(ns("web_if2"))#,
                      #  textOutput(uu)
                      # includeHTML("/Volumes/CHI/TEMP/TEST3/CHI018_atac/outs/web_summary.html")
             ),
             tabPanel("Heatmap Plot",
                      uiOutput(ns('moreControls2d')),

                      #plotlyOutput(ns("distPlot"),width = input$size, height = "400px"),
                      #actionButton(ns("goplot"),"Plot"),
                      uiOutput(ns("dist2plot"))
                      #plotly::plotlyOutput(ns("distPlot"),width = input$width, height = input$height)
             ),
             tabPanel("Correlation Plot",
                      uiOutput(ns("more_corr")),
                      #  actionButton(ns("goplot2"),"Plot"),
                      # plotOutput(ns("heatmap")),
                      uiOutput(ns('plcor'))
             ),
             tabPanel("PCA Plot",
                      plotOutput(ns("pca"), width = "100%"),
             ),
             tabPanel("Bar Chart",
                      uiOutput(ns("select_met")),

                      #      plotOutput(ns("gpl"), width = "100%")
                      uiOutput(ns("gpl") )

             )

           )
  )
}


# Module Server

#' @noRd
#' @export
#' @keywords internal

mod_dataInput_server2 <- function(input, output, session, file){
  ns <- session$ns





  #########################################################
  #########################################################
  #########################################################
  #########################################################
  #########################################################
  read_csv_and_add_file_column <- function(file_path) {
    # Read CSV file
    df <- readr::read_csv(file_path)

    # Extract the third folder from the file path
    file_name <- basename(dirname(dirname(file_path)))

    # Add the file column
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
      df$library_type <- NA  # or any other default value
    }



    return(df)
  }

  add_identifier <- function(df, identifier) {
    df %>%
      dplyr::mutate(unique_id = dplyr::row_number())


  }


  processData <- function(dir,sel,traf) {
    req(file$goButtonp())
    isolate({
      withProgress(message = "Please wait...", detail = "", value = 0, max = 100, {
        # dir <- file$df()
        ddd <<- dir
        #  fsel <<- file$sel()

        if (traf == 1) {
          metric_f <- '/*/outs/per_sample_outs/*/metrics_summary.csv'
          ff <- Sys.glob(paste(dir, metric_f, sep = ""))

          metric_g <- '/*/outs/summary.csv'
          ffg <- Sys.glob(paste(dir, metric_g, sep = ""))
          fffgg2 <<- ffg

          metric_h <- '/*/outs/metrics_summary.csv'
          ffgg <- Sys.glob(paste(dir, metric_h, sep = ""))
          fffggg2 <<- ffgg



          ff <- c(ff, ffg, ffgg)
          ff2A <<- ff
        } else if (traf == 2) {
          metric_f2 <- '/*/*/outs/per_sample_outs/*/metrics_summary.csv'
          fff2 <- Sys.glob(paste(dir, metric_f2, sep = ""))
          metric_f <- '/*/outs/per_sample_outs/*/metrics_summary.csv'
          ff <- Sys.glob(paste(dir, metric_f, sep = ""))

          metric_g <- '/*/outs/summary.csv'
          ffg <- Sys.glob(paste(dir, metric_g, sep = ""))

          metric_g2 <- '/*/*/outs/summary.csv'
          ffg2 <- Sys.glob(paste(dir, metric_g2, sep = ""))

          ff <- c(ff, fff2, ffg, ffg2)
          ff2A2 <<- ff
        } else if (traf == 3) {
          metric_f2 <- '/*/*/outs/per_sample_outs/*/metrics_summary.csv'
          fff2 <- Sys.glob(paste(dir, metric_f2, sep = ""))
          metric_f <- '/*/outs/per_sample_outs/*/metrics_summary.csv'
          ff <- Sys.glob(paste(dir, metric_f, sep = ""))
          metric_f3 <- '/*/*/*/outs/per_sample_outs/*/metrics_summary.csv'
          fff3 <- Sys.glob(paste(dir, metric_f3, sep = ""))

          metric_g <- '/*/outs/summary.csv'
          ffg <- Sys.glob(paste(dir, metric_g, sep = ""))

          metric_g2 <- '/*/*/outs/summary.csv'
          ffg2 <- Sys.glob(paste(dir, metric_g2, sep = ""))

          metric_g3 <- '/*/*/*/outs/summary.csv'
          ffg3 <- Sys.glob(paste(dir, metric_g3, sep = ""))

          ff <- c(ff, fff2, fff3, ffg3, ffg, ffg2)
          ff3A3 <<- ff
        } else if (traf == 4) {
          metric_f2 <- '/*/*/outs/per_sample_outs/*/metrics_summary.csv'
          fff2 <- Sys.glob(paste(dir, metric_f2, sep = ""))
          metric_f <- '/*/outs/per_sample_outs/*/metrics_summary.csv'
          ff <- Sys.glob(paste(dir, metric_f, sep = ""))
          metric_f3 <- '/*/*/*/outs/per_sample_outs/*/metrics_summary.csv'
          fff3 <- Sys.glob(paste(dir, metric_f3, sep = ""))
          metric_f4 <- '/*/*/*/*/outs/per_sample_outs/*/metrics_summary.csv'
          fff4 <- Sys.glob(paste(dir, metric_f4, sep = ""))
          ff <- c(ff, fff2, fff3, fff4)
        }

        ff2 <- gsub(".*per_sample_outs\\/", "", ff)
        ff3 <- gsub("\\/.*", "", ff2)

        f <- ff %>%
          tibble::as.tibble() %>%
          dplyr::mutate("n" = gsub(".*multi_config_", "", value)) %>%
          dplyr::mutate("n" = gsub("\\/.*", "", n)) %>%
          dplyr::arrange(as.numeric(n))
        fffF <<- f
        ff <- f$value
        fff <<- ff
        df.list = list()
        f.list = list()

        data_list <- purrr::map(ff, ~ readr::read_csv(.x))

        # Combine data frames
        combined_data <- dplyr::bind_rows(data_list, .id = "file_id")

        # Create a tidyverse tibble
        tidy_data <- dplyr::as_tibble(combined_data)

        # Print the structure of the resulting tibble
        print(str(tidy_data))


        for (i in 1:length(t(ff))) {
          file_o <- gsub(".*\\/(.*)\\/(.*)\\/outs\\/(['summary.csv'\\|'metrics_summary.csv'])", "\\3", perl = TRUE, ff[i])
          fffo <<- file_o


          lines <- readr::read_csv(ff[i]) %>% nrow()
          if (lines ==1){
            ##  if (file_o == 'summary.csv') {
            path <- gsub(".*\\/.*\\/.*\\/(.*)\\/outs", "\\1", perl = TRUE, ff[i])
            id <- gsub(".*\\/.*\\/(.*)\\/(.*)\\/outs", "\\1_\\2", perl = TRUE, ff[i])



            # Apply the function to all files in file_list$value
            list_of_dataframes <- lapply(ff, read_csv_and_add_file_column)

            # Combine all data frames into one
            combined_df <- dplyr::bind_rows(list_of_dataframes)
            cd<<-combined_df
            # Transpose the combined data frame
            transposed_df <- t(combined_df)
            ttff<<-transposed_df

            file_row_index <- which(rownames(transposed_df) == "file")


            # Set the last row as column names
            colnames(transposed_df) <- transposed_df[file_row_index, ]


            # Remove the last row
            transposed_df <- transposed_df[-nrow(transposed_df), ]

            # Convert transposed tibble to data frame
            s <- as.data.frame(transposed_df)
            sssss<<-s
            s1<-transposed_df[file_row_index, ]
            ss11<<-s1
            libs <- "Gene Expression"
            # libs <- dplyr::pull(df_all, library_type) %>% unique()
          }
          else {
            path <- gsub("(.*)\\/outs.*", "\\1", perl = TRUE, ff[i])
            id <- gsub(".*\\/(.*)\\/(.*)\\/outs.*", "\\1_\\2", perl = TRUE, ff[i])
            ddd<<-id
            if (sel == "ALL") {
              df <- readr::read_csv(ff[i]) %>% janitor::clean_names()
            } else {
              df <- readr::read_csv(ff[i]) %>% janitor::clean_names() %>%
                dplyr::filter(library_type %in% sel)
            }
            dfdf <<- df
            df_all <- readr::read_csv(ff[i]) %>% janitor::clean_names()
            ff_all2 <<- df_all
            libs <- dplyr::pull(df_all, library_type) %>% unique()





            lll <<- libs
            colnames(df)[6] <- paste("sample", id, sep = "_")
            dffff <<- df[6]
            if (file_o == 'summary.csv') {
              df[6] <- df[[6]] %>% tibble::as.tibble()
              df.list[[i]] = df
            } else {
              df[6] <- readr::parse_number(df[[6]]) %>% tibble::as.tibble()
              df.list[[i]] = df
            }

            Split <- strsplit(ff[i], "\\/")
            spsp <<- Split
            flist1 <<- f.list
            iii <<- i
            s1 <- Split[[1]][length(Split[[1]]) - 5]
            s2 <<- Split[[1]][length(Split[[1]]) - 4]
            s3 <<- dput(libs)
            ppp<<-path
            f.list[[i]] <- c(path, Split[[1]][length(Split[[1]]) - 5], Split[[1]][length(Split[[1]]) - 4], deparse(dput(libs)))
          }

          flst <- do.call(rbind.data.frame, f.list) %>%
            tibble::as.tibble()
          colnames(flst) <- c("path", "run_directory", "sample_directory", "library")
          fflst <<- flst
          dfl<<- df.list

          # Create a function to add a unique identifier to each tibble in the list
          # add_identifier <- function(df, identifier) {
          #   df %>%
          #     dplyr::mutate(unique_id = dplyr::row_number())
          # }

          # Apply the function to each tibble in the list
          #  df.list_with_id <- purrr::imap(df.list, add_identifier) %>%   purrr::map(~ filter(.x, grouped_by != 'Fastq ID'))

          # df.list_with_id <- purrr::imap(df.list, function(df, identifier) {
          #    df %>%
          #      dplyr::mutate(unique_id = dplyr::row_number())
          #  }) %>%
          #    purrr::map(~ filter(.x, grouped_by != 'Fastq ID'))


          df.list_with_id <- list()
          for (i in seq_along(df.list)) {
            df <- df.list[[i]]
            df_with_id <- df %>%
              dplyr::mutate(unique_id = dplyr::row_number())
            df.list_with_id[[i]] <- df_with_id
          }
          ddff<<-df.list_with_id
          # Step 2: Filter rows in each data frame
          df.list_filtered <- list()
          for (i in seq_along(df.list_with_id)) {
            df_with_id <- df.list_with_id[[i]]
            df_filtered <- filter(df_with_id, "grouped_by" != 'Fastq ID')
            df.list_filtered[[i]] <- df_filtered
          }
          ddff2<<-df.list_filtered

          sdd <<- df.list_with_id
          s <- df.list_with_id  %>%
            #  purrr::map(subset, select = -c(group_name, grouped_by)) %>%
            purrr::map(subset,select = -c(unique_id)) %>%
            #     purrr::map(subset) %>%
            purrr::reduce(dplyr::bind_rows) %>%
            tidyr::pivot_longer(
              cols = dplyr::starts_with("sample"),
              values_to = "values"
            ) %>%
            dplyr::distinct() %>%
            na.omit(values) %>% dplyr::mutate(group_name = dplyr::if_else(grouped_by == "Fastq ID", "Fastq ID", group_name)) %>%
            tidyr::pivot_wider(
              names_from = name,
              values_from = values
            ) %>%
            #%>% dplyr::select(-c(group_name, grouped_by, unique_id))
            dplyr::select(-c(group_name, grouped_by))
          sss <<- s


        }
      })


    })

    return(list(s = s, s1 = s1, ff = ff, f = f, flst = flst))
  }


  dat2<-reactive({

    req(file$goButtonp())
    # output$col <- renderPrint({
    ##sliderInput(inputId="dat_st", label = "Data Starts Here"),

    processData(file$df(), file$sel(),file$traf())


  })



  #########################################################
  #########################################################
  #########################################################
  #########################################################
  #########################################################
  #########################################################

  dat2B<-reactive({

    ##FIX THIS
    # req(file$goButtonp2())
    #  req(file$goButtonp())
    #  processData(file$df2(), file$sel(),file$traf())


  })



  #########################################################
  #########################################################
  #########################################################
  #########################################################
  #########################################################












  output$moreControls2d <- renderUI({

    tagList(

      div(style="display: inline-block;horizontal-align:top; width: 25%;",sliderInput(ns("width"), "Plot Width:",
                                                                                      min = 400, max = 2000, value = 1200)) ,
      div(style="display: inline-block;horizontal-align:top; width: 25%;",sliderInput(ns("height"), "Plot Height:",
                                                                                      min = 400, max = 2000, value = 600)),
      #div(style="display: inline-block;vertical-align:top; width: 15%;",selectInput(ns("norm"), "Normalization", c("normalize", "percentize", "scale"), selected = "normalize")),
      # div(style="display: inline-block;margin-top:75px, width: 25%;",actionButton(ns("goplot"),label = "HeatMap"), "HeatMap\n<BR>Plot")
      div(style="display: inline-block;vertical-align:top; width: 15%;",selectInput(ns("norm"), "Normalization", c("normalize", "percentize", "scale"), selected = "normalize")),

      actionButton(ns("goplot"),label = "HeatMap", style = 'margin-top:25px')

    )
  })



  output$more_corr <- renderUI({

    tagList(

      div(style="display: inline-block;horizontal-align:top; width: 25%;",sliderInput(ns("widthcor"), "Plot Width:",
                                                                                      min = 400, max = 2000, value = 600)) ,
      div(style="display: inline-block;horizontal-align:top; width: 25%;",sliderInput(ns("heightcor"), "Plot Height:",
                                                                                      min = 400, max = 2000, value = 600)),
      actionButton(ns("goplotcor"), "Plot", style = 'margin-top:25px')

    )
  })



  output$select_met <- renderUI({
    e<-dat2()$s

    tagList(
      div(style = "display: inline-block; width: 15%; vertical-align: bottom;", selectInput(ns("sel"), "Select:", choices = e$metric_name, multiple = TRUE)),
      div(style = "display: inline-block; width: 15%; vertical-align: top;", sliderInput(ns("bar_width"), "Plot Width:", min = 400, max = 2000, value = 900)),
      div(style = "display: inline-block; width: 15%; vertical-align: top;", sliderInput(ns("bar_height"), "Plot Height:", min = 400, max = 2000, value = 600)),
      div(style = "display: inline-block; width: 15%; vertical-align: top;", sliderInput(ns("font"), "Font Size:", min = 1, max = 80, value = 25)),
      div(style = "display: inline-block; width: 10%; vertical-align: top;", actionButton(ns("gosel"), "Plot"))
    )


  })






  rsel<-reactive({
    req(input$gosel)

    sss1 <-input$sel



    # sss<-file$df()$s1 %>% dplyr::select(Metric.Name,file2$sampleid()) %>% dplyr::filter(Metric.Name %in% file2$rowid())
    # sss_l<-sss %>% tibble::as.tibble() %>%
    #   tidyr::gather(key = key, value = value, starts_with('sample')) %>% dplyr::rename_all(~ sub("sample_", "", .x))
    ##  sss_l<-   file$df()$s1 %>% tibble::as.tibble() %>% tidyr::gather(key = key, value = value, starts_with('sample'))


    sss_l<-    e<-dat2()$s %>% tibble::as.tibble() %>% tidyr::gather(key = "key", value = "value",4:length( e<-dat2()$s))

    ssss_ll<<-sss_l
    sss_l$key <- gsub("sample_", "", sss_l$key)
    #dplyr::rename_all(~ sub("sample_", "", .x)) %>%
    sss2_2l <<- sss_l
    # sss_l <- sss_l %>%  dplyr::filter(key %in% file2$sampleid())  %>% dplyr::filter(metric_name %in% file2$rowid())
    sss_l <- sss_l  %>% dplyr::filter(metric_name %in% input$sel)
    sss_l
  })

  output$gpl<-renderUI({
    req(input$gosel)

    #output$gpl <- renderPlot({
    renderPlot({

      sss_l<-rsel()
      ssslll<<-sss_l

      ggplot2::ggplot(sss_l, ggplot2::aes(x=reorder(key, -value), y=value)) +
        ggplot2::geom_col() +  ggplot2::geom_text(ggplot2:aes(label = value), vjust = -0.5)  +
        #   ggplot2::facet_wrap(.~ metric_name,ncol = 5, scales = "free")+
        ggplot2::facet_wrap(library_type ~ metric_name,ncol = 5, scales = "free")+
        ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 90))+ ggplot2::xlab("") +
        ggplot2::theme_grey(base_size = input$font)



    } , width=input$bar_width,height=input$bar_height)

  })



  output$dist2plot<-renderUI({

    req(input$goplot)
    #  we<<-input$width
    isolate({
      #  #  isolate(
      plotly::plotlyOutput(ns("distPlot"))
      #,width = input$width, height = input$height)
      ##)
    })
  })


  output$distPlot <- plotly::renderPlotly({
    req(input$goplot)
    #heatmaply(percentize(as.matrix(as.data.frame(dat2()$s[6:length(dat2()$s)]))), plot_method="plotly" ,row_side_colors =dat2()$s[5])
    isolate({
      e<-dat2()$s

      e <- e %>% dplyr::distinct(metric_name, .keep_all = TRUE)
      ee<<-e



      e<-e[3:length(e)]
      eee<<-e

      e <- e[rowSums(e[-1])>0,]

      eeee<<-e


      d<-e[, -1]
      ddd<<-d


      rownames(d) <- make.names(t(e[1]), unique = TRUE)

      d<-t(janitor::remove_constant(t(d)))
      #  rownames(d) <- make.names(t( subset(e[1], select=which(!duplicated(names(.))))) , unique = TRUE)

      # heatmaply(percentize(t(as.matrix(as.data.frame(d)))), plot_method="plotly",labCol=rownames(d) ,col_side_colors =rownames(d))
      #  scale = "column",
      #  heatmaply::heatmaply(t(scale=row, t(as.matrix(as.data.frame(d)))), plot_method="plotly",labRow=rownames(d) ,row_side_colors =rownames(d), width = input$width, height = input$height )
      #"normalize", "percentize", "scale"
      if (input$norm == "normalize") {
        heatmaply::heatmaply(t(heatmaply::normalize(t(as.matrix(as.data.frame(d))))),Rowv="FALSE", Colv="FALSE", plot_method="plotly",labRow=rownames(d) ,row_side_colors =rownames(d), width = input$width, height = input$height )
      }
      else if(input$norm == "percentize"){
        heatmaply::heatmaply(t(heatmaply::percentize(t(as.matrix(as.data.frame(d))))),Rowv="FALSE", Colv="FALSE", plot_method="plotly",labRow=rownames(d) ,row_side_colors =rownames(d), width = input$width, height = input$height )
      }
      else if(input$norm == "scale"){
        #  heatmaply::heatmaply(scale="column",(t(as.matrix(as.data.frame(d)))), plot_method="plotly",labRow=rownames(d) ,row_side_colors =rownames(d), width = input$width, height = input$height )
        heatmaply::heatmaply(scale="row",as.matrix(as.data.frame(t(janitor::remove_constant(t(d))))), Rowv="FALSE", Colv="FALSE", plot_method="plotly", labRow=rownames(d) ,row_side_colors =rownames(d), width = input$width, height = input$height )
      }
      #  heatmaply::heatmaply(t(heatmaply::normalize(t(as.matrix(as.data.frame(d))))), plot_method="plotly",labRow=rownames(d) ,row_side_colors =rownames(d), width = input$width, height = input$height )
      #  heatmaply(t(percentize(t(as.matrix(dat2()$s[6:length(dat2()$s)])))), plot_method="plotly" ,labRow= dat2()$s[5],row_side_colors =dat2()$s[5])
    })

  })



  output$pca <- renderPlot({

    e<-dat2()$s
    e<-e[3:length(e)]
    rownames(e)<-make.names(e$metric_name, unique = TRUE)

    factoextra::fviz_pca_ind(stats::prcomp(
      na.omit(t(e[-1]))))


  })


  output$plcor <- renderUI({

    req(input$goplotcor)


    output$heatmap <- renderPlot( {

      e<-dat2()$s
      e <- e%>%
        dplyr::distinct(metric_name, .keep_all = TRUE)

      e<-e[3:length(e)]
      # rownames(e)<-make.names(e$metric_name, unique = TRUE)
      #  colnames(e)<-make.names(e$metric_name, unique = TRUE)
      corr_matrix<-cor(t(e[, -1]))
      rownames(corr_matrix)<-make.names(t(e[1]), unique = TRUE)
      colnames(corr_matrix)<-make.names(t(e[1]), unique = TRUE)
      ggc<<-corr_matrix
      ggcorrplot::ggcorrplot(corr_matrix)

    }, width=input$widthcor,height=input$heightcor)

  })




  # output$tab2B <- renderUI({
  #
  #   ##FIX THI
  #    e<-dat2B()$s
  #
  #   sss<<-e
  #   ##colnames(e)<-gsub('^.*\\_','',colnames(e))
  #   output$tab3 <- DT::renderDataTable(
  #     DT::datatable(
  #       cbind(e,prop.table(data.matrix(dat2B()$s[4:length(dat2()$s)] %>% dplyr::mutate_if(is.character, as.numeric) ),margin = 1)  %>%
  #               tibble::as.tibble()  %>%   dplyr::rename_with( ~ paste0("prop_", .x)) )    ,
  #       extensions = 'Buttons', options = list(columnDefs = list(list(targets=c((length(dat2B()$s)+1):(length(dat2B()$s) * 2 - 3)),visible=FALSE)),
  #                                              dom = 'Bfrtip', pageLength = 500,
  #                                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print')#, buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
  #
  #       )) %>%
  #       DT::formatStyle(names(dat2B()$s[4:length(dat2B()$s)]),
  #                       #  DT::formatStyle(names(e[6:length(e)]),
  #                       background = DT::styleColorBar(range(0, 1), 'lightblue'),
  #                       backgroundSize = '98% 88%',
  #                       valueColumns = c((length(dat2B()$s)+1):(length(dat2B()$s) * 2 - 3)),fontWeight = "bold",
  #                       backgroundRepeat = 'no-repeat',
  #                       backgroundPosition = 'center') %>%
  #    #   DT::formatStyle(names(dat2B()$s[1:3]), fontWeight = "bold")
  #      DT::formatStyle(names(dat2B()$s[1:3]), fontWeight = "bold")
  #   )
  #
  # })


  output$tab2 <- renderUI({
    e<-dat2()$s
    eee<<-e


    output$tab <- DT::renderDataTable(



      if(input$column == "hide"){

        DT::datatable(
          cbind(e  %>% dplyr::mutate(dplyr::across(where(is.numeric), sprintf, fmt = '%.2f')),prop.table(data.matrix(dat2()$s[4:length(dat2()$s)] %>% dplyr::mutate_if(is.character, as.numeric) ),margin = 1)  %>%
                  tibble::as.tibble()  %>%   dplyr::rename_with( ~ paste0("prop_", .x)) )    ,
          colnames = rep("", ncol(e)),
          extensions = 'Buttons', options = list(columnDefs = list(list(targets=c((length(dat2()$s)+1):(length(dat2()$s) * 2 - 3)),visible=FALSE)),
                                                 dom = 'Bfrtip', pageLength = 500,
                                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')#, buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))

          )) %>%
          DT::formatStyle(names(dat2()$s[4:length(dat2()$s)]),
                          background = DT::styleColorBar(range(0, 1), 'lightblue'),
                          backgroundSize = '98% 88%',
                          valueColumns = c((length(dat2()$s)+1):(length(dat2()$s) * 2 - 3)),fontWeight = "bold",
                          backgroundRepeat = 'no-repeat',
                          backgroundPosition = 'center') %>%
          DT::formatStyle(names(dat2()$s[1:3]), fontWeight = "bold")

      }
      else{

        DT::datatable(cbind(e %>% dplyr::mutate(dplyr::across(where(is.numeric), sprintf, fmt = '%.2f')),prop.table(data.matrix(dat2()$s[4:length(dat2()$s)] %>% dplyr::mutate_if(is.character, as.numeric) ),margin = 1)  %>%
                              tibble::as.tibble()  %>%   dplyr::rename_with( ~ paste0("prop_", .x)) )    ,
                      extensions = 'Buttons', options = list(columnDefs = list(list(targets=c((length(dat2()$s)+1):(length(dat2()$s) * 2 - 3)),visible=FALSE)),
                                                             dom = 'Bfrtip', pageLength = 500,
                                                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')#, buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))

                      )) %>%
          DT::formatStyle(names(dat2()$s[4:length(dat2()$s)]),
                          background = DT::styleColorBar(range(0, 1), 'lightblue'),
                          backgroundSize = '98% 88%',
                          valueColumns = c((length(dat2()$s)+1):(length(dat2()$s) * 2 - 3)),fontWeight = "bold",
                          backgroundRepeat = 'no-repeat',
                          backgroundPosition = 'center') %>%
          DT::formatStyle(names(dat2()$s[1:3]), fontWeight = "bold")

      }





    )

  })


  output$web<-renderUI({

    loc<-dat2()$ff

    names<-gsub(".*\\/(.*)\\/(.*)\\/outs.*","\\2",perl=TRUE,loc)
    ur<-loc %>% tibble::as.tibble() %>%
      dplyr:: mutate(url=gsub("summary.csv|metrics_summary.csv","web_summary.html",value)) %>%
      dplyr::select(url) %>% dplyr::pull()
    uuu<<-ur

    tagList(
      fluidPage(

        column(12,
               selectInput(ns('websumm'), "Web Summary", choices = dput(ur),width = '700px')#,

               #       selectInput(ns('websumm2'), "Web Summary", choices = dput(ur),width ='700px' )
               # div(style="display: inline-block;vertical-align:top; width: 200;", selectInput(ns('websumm'), "Web Summary", choices = dput(ur))), #,selected=ur[1])),
               #div(style="display: inline-block;vertical-align:top; width: 200%;",selectInput(ns('websumm2'), "Web Summary", choices = dput(ur))) #, selected = ur[2]))
        )
      )
    )
  })


  loadpic <- reactive({
    validate(
      need(input$websumm, "Member input is null!!")
    )

    uu<- input$websumm

    m <- paste("/",paste(gtools::split_path(dirname(uu),depth_first = FALSE)[1:(length(unlist(strsplit(uu, '/')))-6)], collapse = '/'), sep="")
    # m<-paste(gtools::split_path(dirname(uu))[1:(length(unlist(strsplit(uu, '/')))-6)], collapse = '/')
    #  m<-paste("/",m,sep="")
    addResourcePath("library", m)
    #addResourcePath("library", "/Volumes/CHI/TEMP/RUN")
    mmm<<-m
    m2<- paste(gtools::split_path(dirname(uu),depth_first = FALSE)[(length(unlist(strsplit(uu, '/')))-5):(length(unlist(strsplit(uu, '/')))-2)], collapse = '/')
    m2<-paste(m2,"/web_summary.html",sep="")

    mmmm<<-m2
    paste("library/",m2, sep="")

  })

  output$web_if<-slickR::renderSlickR({
    #output$web_if<-renderUI({
    ##find a way of killing the CACHING OLD WEBPAGE !!!1
    # req(input$websumm)
    req(input$go4)
    ## req(file$goButtonp())
    url1<-input$websumm
    uu<<-url1
    uuu<<-loadpic()
    # HTML('<iframe width="350" height="250" src=input$websumm frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    # HTML('<iframe width="350" height="250" src="https://www.yahoo.com" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    #test <<- paste0("https://google.com") #sample url
    # test <<- paste0("/Volumes/CHI/TEMP/TEST3/CHI018_atac/outs/web_summary.html") #sample url
    #uu
    #uuu<<-tags$iframe(src = paste("library/",basename(uu), sep="") ,style='width:100vw;height:100vh;')

    #uuu<<-dirname(input$websumm)
    #tags$iframe(src = "" ,style='width:100vw;height:100vh;')
    #
    ####addResourcePath("library", dirname(input$websumm))

    # tagList(
    #   fluidPage(

    #      column(6,
    #
    # tags$iframe(src = paste("library/",basename(input$websumm), sep="") ,style='width:100vw;height:100vh;')
    ######tags$iframe(src = paste("library/",basename(input$websumm), sep="") ,style='width:100vw;height:100vh;')
    slickR::slickR(slickR::slick_list(
      tags$iframe(height="600px",
                  width="900px",
                  scrolling=T,
                  src = loadpic() ,
                  height = 500,id="theframe"
      )
    ))
    #)
    #)
    #    removeResourcePath("library")
    #)
    #  print(my_test)
    #  my_test
  })


  output$web_if2<-renderUI({
    req(input$go4)
    url2<-input$websumm2
    uu2<<-url2

    # HTML('<iframe width="350" height="250" src=input$websumm frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    # HTML('<iframe width="350" height="250" src="https://www.yahoo.com" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    #test <<- paste0("https://google.com") #sample url
    # test <<- paste0("/Volumes/CHI/TEMP/TEST3/CHI018_atac/outs/web_summary.html") #sample url

    addResourcePath("library2", dirname(input$websumm2) )
    tagList(
      fluidPage(

        column(6,


               tags$iframe(height="800px",
                           width="100%",
                           scrolling=T,src = paste("library2/",basename(input$websumm2), sep="") )

        )
      )
    )
    #  print(my_test)
    #  my_test
  })


  return(list(df =dat2))




}





## To be copied in the UI
# mod_dataInput_ui("dataInput_1")

## To be copied in the server
# mod_dataInput_server("dataInput_1")
