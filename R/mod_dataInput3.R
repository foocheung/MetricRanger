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

    ffee<- file$df()$s$metric_name
    fffdd3<<-ffee



  # if (NCOL(ffee) == 1){
  #   HTML("<H3> There is no data</H3>")
  #
  # }else{
    tagList(
  column(6,selectInput(ns("sampleid"), "Filter Samples" ,colnames(file$df()$s),   multiple = TRUE)),
  column(6,selectInput(ns("rowid"), "Filter Rows" ,ffee, "metric_name",multiple = TRUE)),
  actionButton(ns("goButtonp2"), "Go!",icon("paper-plane"))
    )
#  }

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

 #  if (NCOL(ffee) == 1){
#     HTML("<H3> There is no data</H3>")

  # }else{

     output$tab2 <- DT::renderDataTable(
     DT::datatable(
      file$df()$s %>% dplyr::select(input$sampleid) %>% dplyr::filter(metric_name %in% input$rowid),
      extensions = 'Buttons', options = list(
        dom = 'Bfrtip', pageLength = 500,
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
))
     )
  #   }
   #   )

  #  )
  #)
  } )

  return(list(
  goButtonp2= reactive({input$goButtonp2}),
  sampleid= reactive({input$sampleid}),
  rowid= reactive({input$rowid})
))

}
