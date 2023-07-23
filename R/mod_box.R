#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

`%>%` <- dplyr::`%>%`

addResourcePath("d", "inst/app/www/")

mod_box_ui <- function(id, name){
    ns <- NS(id)


#tabsetPanel(

       tabPanel( "BoxPlot " ,
                 fluidRow(
                   column(5,
                          div(

                            uiOutput(ns('box1'))

                          )

                   )

                 )
       )




#)

}




# Module Server

#' @rdname mod_box
#' @export
#' @keywords internal

mod_box_server <- function(input, output, session, file, file2){  #,batches,sim){
  ns <- session$ns



  output$box1 <- renderUI({


    req(file2$goButtonp())

    tagList(

      plotOutput(ns("box"),height=file2$size(),width=file2$wsize())
    )

  })


  output$box <- renderPlot({

    req(file2$goButtonp())



    d<-file$df() #%>%  dplyr::filter(drop_na(!! rlang::sym(input$rn)))

    d<- d %>%  dplyr::filter(!is.na(!! rlang::sym(file2$rn())))


    # ttt<<-"hello"
    counts<-d[, c(which(colnames(d) %in% file2$data_st()):length(d))]

    # counts <- a[41:length(a)]
    # rownames(counts) <- d$reposid

    rownames(counts)<-t(d[file2$rn()])

    count2<-counts %>%
      tibble::rownames_to_column() %>%
      tidyr::gather(var, value, -rowname) %>%
      tidyr::spread(rowname, value)


    rownames(count2)<-count2$var
    count3<-count2[2:length(count2)]
    rownames(count3)<-count2$var
    count3<-count3 %>% dplyr::mutate_if(is.character, as.numeric)

    count3<-log(count3)

    #rownames(counts)


    ggplot2::ggplot(data = stack(count3), ggplot2::aes(x = ind, y = values)) +
      ggplot2::stat_boxplot(geom = "errorbar", # Boxplot with error bars
                   width = 0.2) +
      ggplot2::geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors
                   alpha = 0.9, outlier.colour = "red") +
      ggplot2::scale_y_continuous(name = "log(freq. main)") +  # Continuous variable label
      ggplot2::scale_x_discrete(name = "Samples") +      # Group label
      #ggtitle("Boxplot from data frame SomaLogic") + # Plot title
      ggplot2::theme(axis.line = ggplot2::element_line(colour = "black", # Theme customization
                                     size = 0.25)) + ggplot2::theme_grey(base_size = 18) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))




  })


#  return(list(rn = reactive({input$rn}),


}
## To be copied in the UI
# mod_table_ui("table_1")

## To be copied in the server
# mod_table_server("table_1")
