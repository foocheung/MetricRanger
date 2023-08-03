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
mod_dataInput_ui <- function(id){
  ns <- NS(id)

  tagList(
   # tabsetPanel(
    #  tabPanel(
  #  shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(id = "Options",
                                  shinyFiles::shinyDirButton(ns("directory"), "Load",
                                                 icon=icon("file-arrow-up"),
                                                 ""),

selectInput(ns("sel"), "Select Library", c("VDJ T","Antibody Capture","Gene Expression","VDJ B", "ATAC"), "Gene Expression"),
selectInput(ns("traf"), "Select How Many Directories to Transverse", c(1,2,3,4), "1"),
    actionButton(ns("goButtonp"), "Go!",icon("paper-plane"))

)
  )

}

#' dataInput Server Functions
#'
#' @noRd
mod_dataInput_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    volumes <- shinyFiles::getVolumes()()
    shinyFiles::shinyDirChoose(input, 'directory', roots=volumes, session=session)


      path1 <- reactive({
      req(input$goButtonp)
      return(print(shinyFiles::parseDirPath(volumes, input$directory)))
    })



    return(list(df =path1,
                goButtonp= reactive({input$goButtonp}),
                sel= reactive({input$sel}),
                traf= reactive({input$traf})
                )
           )

    #bat <- reactive({
    #  input$integer
    #})
    #return(
    #list(
    #    c(
    #    "dat"=datafile
        #,
        #"batches" = reactive({ input$integer }),
        #"sim" = reactive({ input$sim })
    #  ))
    #)



   # return(list(c("datafile" = datafile, "userFile" = userFile)))
  })

}

## To be copied in the UI
# mod_dataInput_ui("dataInput_1")

## To be copied in the server
# mod_dataInput_server("dataInput_1")
