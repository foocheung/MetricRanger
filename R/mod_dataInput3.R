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
mod_dataInput_ui3 <- function(id){
  ns <- NS(id)

      tabPanel("Filter",

               # shinydashboard::dashboardSidebar(
               #  shinydashboard::sidebarMenu(id = "Options",
                uiOutput(ns("cont1")),
              # DT::dataTableOutput(ns('tab'))
              uiOutput(ns('cont6'))

             # DT::dataTableOutput(ns('tab2'))
              )

}


# Module Server

#' @noRd
#' @export
#' @keywords internal

mod_dataInput_server3 <- function(input, output, session, file){  #,batches,sim){
  ns <- session$ns

  output$cont1 <- renderUI({

    ffee<- file$df()$s$Metric.Name
    fffdd3<<-ffee

tagList(
  column(6,selectInput(ns("sampleid"), "Filter Samples" ,colnames(file$df()$s),   multiple = TRUE)),
  column(6,selectInput(ns("rowid"), "Filter Rows" ,ffee, "Metric.Name",multiple = TRUE)),
    actionButton(ns("goButtonp2"), "Go!",icon("paper-plane"))
)
    })



  ##DAT3 NOT WORKING NEED ANOTHER MOD WORKS ONE WAY!!!!!
  #dat3<-reactive({
  # req(input$goButtonp2)
  # sssid<- input$sampleid
  #ss
  #})
  output$cont6 <- renderUI({
    req(input$goButtonp2)

   ddffs<- file$df()$s
   ddffs22<-ddffs
  output$tab2 <- DT::renderDataTable(
     DT::datatable(
      file$df()$s %>% dplyr::select(input$sampleid) %>% dplyr::filter(Metric.Name %in% input$rowid),
      extensions = 'Buttons', options = list(
        dom = 'Bfrtip', pageLength = 500,
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')

      )

    )
  )
  } )

  return(list(
  goButtonp2= reactive({input$goButtonp2}),
  sampleid= reactive({input$sampleid}),
  rowid= reactive({input$rowid})
))

}
