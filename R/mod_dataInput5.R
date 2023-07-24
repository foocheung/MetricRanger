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
mod_dataInput_ui5 <- function(id){
  ns <- NS(id)


    tabPanel("Files",
             DT::dataTableOutput(ns('tabf'))
    )

}


# Module Server

#' @noRd
#' @export
#' @keywords internal

mod_dataInput_server5 <- function(input, output, session, file){  #,batches,sim){
  ns <- session$ns


  output$tabf <- DT::renderDataTable(
    DT::datatable(
      file$df()$flst,
      extensions = 'Buttons', options = list(
        dom = 'Bfrtip', pageLength = 500,
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')

      )
    )
  )




#  )
 # )




}

## To be copied in the UI
# mod_dataInput_ui("dataInput_1")

## To be copied in the server
# mod_dataInput_server("dataInput_1")
