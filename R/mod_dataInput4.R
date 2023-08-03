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
mod_dataInput_ui4 <- function(id){
  ns <- NS(id)




    tabPanel("Plot",
             plotOutput(ns("trend"),width = 1200))


}


# Module Server

#' @noRd
#' @export
#' @keywords internal

mod_dataInput_server4 <- function(input, output, session, file, file2){  #,batches,sim){
  ns <- session$ns



  s<-reactive({
    req(file2$goButtonp2())

   sss1 <-file$df()$s1
    sss2<<-sss1

 # sss<-file$df()$s1 %>% dplyr::select(Metric.Name,file2$sampleid()) %>% dplyr::filter(Metric.Name %in% file2$rowid())
 # sss_l<-sss %>% tibble::as.tibble() %>%
 #   tidyr::gather(key = key, value = value, starts_with('sample')) %>% dplyr::rename_all(~ sub("sample_", "", .x))
    sss_l<-   file$df()$s1 %>% tibble::as.tibble() %>% tidyr::gather(key = key, value = value, starts_with('sample'))
    sss_l$key <- gsub("sample_", "", sss_l$key)
      #dplyr::rename_all(~ sub("sample_", "", .x)) %>%
    sss_l <- sss_l %>%  dplyr::filter(key %in% file2$sampleid())  %>% dplyr::filter(metric_name %in% file2$rowid())

  sss_l
  })

  output$trend <- renderPlot({
    req(file2$goButtonp2())
sss_l<-s()
ssslll<<-sss_l

     ggplot2::ggplot(sss_l, ggplot2::aes(x=key, y=value)) +
      ggplot2::geom_col() +
      ggplot2::facet_wrap(.~ metric_name,ncol = 5, scales = "free")+
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 90))


  })



}
