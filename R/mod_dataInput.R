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
                                #  shinyFiles::shinyDirButton(ns("directory2"), "Load2",
                                 #                            icon=icon("file-arrow-up"),
                                #                             ""),
                                  shinyscreenshot::screenshotButton(label="Captureentirepage"),

selectInput(multiple = FALSE, ns("sel"), "Select Library", c("VDJ T","Antibody Capture","Gene Expression","VDJ B", "ATAC", "Custom Feature", "Antigen Capture","ALL"), "Gene Expression"),
selectInput(ns("traf"), "Select How Many Directories to Transverse", c(1,2,3,4), "1"),
    actionButton(ns("goButtonp"), "Go!",icon("paper-plane"))
#,

#actionButton(ns("goButtonp2"), "Go2!",icon("paper-plane"))
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
  #  shinyFiles::shinyDirChoose(input, 'directory', roots=c(CHI = '/Volumes/CHI/PROJECTS_Archive/2022_CHI_PROPOSALS/'), session=session)
      shinyFiles::shinyDirChoose(input, 'directory2', roots=volumes, session=session)

      path1 <- reactive({
      req(input$goButtonp)
      return(print(shinyFiles::parseDirPath(volumes, input$directory)))
    })
      path2 <- reactive({
        req(input$goButtonp)
        return(print(shinyFiles::parseDirPath(volumes, input$directory2)))
      })



    return(list(df =path1,
                df2 =path2,
                goButtonp= reactive({input$goButtonp}),
                sel= reactive({input$sel}),
                traf= reactive({input$traf})
                )
           )

  })

}

## To be copied in the UI
# mod_dataInput_ui("dataInput_1")

## To be copied in the server
# mod_dataInput_server("dataInput_1")
