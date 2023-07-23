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

mod_lof_ui <- function(id, name){
    ns <- NS(id)


#tabsetPanel(

       tabPanel( "PCA" ,
                 fluidRow(
                   column(5,
                          div(

                            uiOutput(ns('lout1'))

                          )

                   )

                 )
       )




#)

}




# Module Server

#' @rdname mod_lof
#' @export
#' @keywords internal

mod_lof_server <- function(input, output, session, file, file2){  #,batches,sim){
  ns <- session$ns


    output$lout1 <- renderUI({


      req(file2$goButtonp())


      tagList(

        plotOutput(ns("plot"),height=file2$size(),width=file2$wsize())
      )

    })




    output$plot <- renderPlot({

      req(file2$goButtonp())




      d<-file$df() #%>%  dplyr::filter(drop_na(!! rlang::sym(input$rn)))

      d<- d %>%  dplyr::filter(!is.na(!! rlang::sym(file2$rn())))



      counts<-d[, c(which(colnames(d) %in% file2$data_st()):length(d))]




      a<-(file$df())
      a <- a %>%  dplyr::filter(!is.na(!! rlang::sym(file2$rn())) )



      rownames(a)<-make.names(t(a[which(colnames(a) %in% file2$rn())]),unique = TRUE)
        b<-a
      rownames(b)<-make.names(t(a[which(colnames(a) %in% file2$rn())]),unique = TRUE)




      d<-b


      data<-cbind(d[,1: c(which(colnames(d) %in% file2$data_st()))],
                  DMwR2::lofactor(d[, c(which(colnames(d) %in% file2$data_st()):length(d))], file2$ksize()))

      data<-tibble::as.tibble(data)


      colnames(data)[length(data)] <- "lof"
      data$lof<-as.numeric(data$lof)
      rownames(data)<-make.names(t(data[which(colnames(data) %in% file2$rn())]),unique = TRUE)


      stat<-file2$data_st()

      d<-as.data.frame(d)



      dfdat<-d[, c(which(colnames(d) %in% file2$data_st()):length(d))]


      rownames(dfdat)<-t(d[file2$rn()])


      if ((file2$addgroup() == TRUE) && (file2$remove_size() == FALSE)){

        factoextra::fviz_pca_ind( prcomp(dfdat, scale. = TRUE), pointsize = as.numeric(data$lof),
                      repel = TRUE ,
                      habillage  = as.factor(t(data[,file2$gp()])),addEllipses = TRUE #,title = #md()$infilename$name
        )  +  ggplot2::theme_grey(base_size = 12)
      }



      else  if ((file2$addgroup() == TRUE && file2$remove_size() == TRUE)){
#else  if (file2$remove_size() == TRUE){
        factoextra::fviz_pca_ind(prcomp(dfdat, scale. = TRUE),
                     repel = TRUE ,
                     habillage  = as.factor(t(data[,file2$gp])),addEllipses = TRUE #,title = #md()$infilename$name
        )  +  ggplot2::theme_grey(base_size = 12)
      }

      else{
        factoextra::fviz_pca_ind(
          prcomp(dfdat, scale. = TRUE),
          #res.pca,
          pointsize = as.numeric(data$lof),
          #repel = TRUE,
          # col.ind = as.factor(t(data[,input$rn])), # Color by the quality of representation
          repel = TRUE#,title = #md()$infilename$name
          #, habillage  =as.factor(data$Group),addEllipses = TRUE
        ) +  ggplot2::theme_grey(base_size = 12)
      }






    })

}

