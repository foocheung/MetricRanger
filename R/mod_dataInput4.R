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
             plotOutput(ns("trend"),width = 1200,height=1200))


}


# Module Server

#' @noRd
#' @export
#' @keywords internal

mod_dataInput_server4 <- function(input, output, session, file, file2) {
  ns <- session$ns

  s <- reactive({
    req(file2$goButtonp2())

    df_s <- file$df()$s
    sample_ids <- file2$sampleid()
    row_ids <- file2$rowid()

    df_long <- df_s %>%
      tibble::as_tibble() %>%
      tidyr::gather(key = "key", value = "value", 6:ncol(df_s)) %>%
      dplyr::mutate(key = gsub("sample_", "", key)) %>%
      dplyr::filter(key %in% sample_ids, metric_name %in% row_ids)

    df_long
  })

  output$trend <- renderPlot({
    req(file2$goButtonp2())
    df_long <- s()

    ggplot2::ggplot(df_long, ggplot2::aes(x = key, y = value)) +
      ggplot2::geom_col() +
      ggplot2::facet_wrap(. ~ metric_name, ncol = 5, scales = "free") +
      ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 90))
  })
}
