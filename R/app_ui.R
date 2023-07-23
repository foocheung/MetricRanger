#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
## usethis::use_pipe()
app_ui <- function() {
#  tagList(
    # Leave this function for adding external resources
    # golem_add_external_resources(),
    # List the first level UI elements here
    shinyUI( pageWithSidebar(
      HTML("<CENTER><H2>10X Directory Parser"),
        # "",
      #theme="paper",
      tabPanel(
  "Test Load Data",
        sidebarPanel(width=2,
          mod_dataInput_ui(
            "dataInput_ui_meta"
          ),
          )
        ),
      mainPanel(

        tabsetPanel(
          mod_dataInput_ui2(
               "dataInput_ui_meta2"
            ),
          mod_dataInput_ui3(
            "dataInput_ui_meta3"
          ),
          mod_dataInput_ui4(
            "dataInput_ui_meta4"
          )
        )
        )
 #     )
   # )
  )
)
}


golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = ''
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
