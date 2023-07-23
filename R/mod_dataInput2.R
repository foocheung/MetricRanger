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
mod_dataInput_ui2 <- function(id){
  ns <- NS(id)

 # tagList(

      tabPanel("Table",
   # shinydashboard::dashboardSidebar(
    #  shinydashboard::sidebarMenu(id = "Options",
                       # uiOutput(ns("col"))
   DT::dataTableOutput(ns('tab'))

    )

 # )

}


# Module Server

#' @noRd
#' @export
#' @keywords internal

mod_dataInput_server2 <- function(input, output, session, file){  #,batches,sim){
  ns <- session$ns

dat2<-reactive({

  req(file$goButtonp())
 # output$col <- renderPrint({
    ##sliderInput(inputId="dat_st", label = "Data Starts Here"),
  isolate({
    withProgress(message="Please wait...", detail="", value=0, max=100, {

    dir<-file$df()

   ddd<<-dir

   if (file$traf() ==1){
    metric_f<-'/*/outs/per_sample_outs/*/metrics_summary.csv'
    ff<-Sys.glob(paste(dir,metric_f,sep=""))
    ff<-c(ff)
    }
else if (file$traf() ==2){
    metric_f2<-'/*/*/outs/per_sample_outs/*/metrics_summary.csv'
    fff2<-Sys.glob(paste(dir,metric_f2,sep=""))
    metric_f<-'/*/outs/per_sample_outs/*/metrics_summary.csv'
    ff<-Sys.glob(paste(dir,metric_f,sep=""))
    ff<-c(ff,fff2)
    }
   else if (file$traf() ==3){
     metric_f2<-'/*/*/outs/per_sample_outs/*/metrics_summary.csv'
     fff2<-Sys.glob(paste(dir,metric_f2,sep=""))
     metric_f<-'/*/outs/per_sample_outs/*/metrics_summary.csv'
     ff<-Sys.glob(paste(dir,metric_f,sep=""))
     metric_f3<-'/*/*/*/outs/per_sample_outs/*/metrics_summary.csv'
     fff3<-Sys.glob(paste(dir,metric_f3,sep=""))
     ff<-c(ff,fff2,fff3)
        }
   else if (file$traf() ==4){
     metric_f2<-'/*/*/outs/per_sample_outs/*/metrics_summary.csv'
     fff2<-Sys.glob(paste(dir,metric_f2,sep=""))
     metric_f<-'/*/outs/per_sample_outs/*/metrics_summary.csv'
     ff<-Sys.glob(paste(dir,metric_f,sep=""))
     metric_f3<-'/*/*/*/outs/per_sample_outs/*/metrics_summary.csv'
     fff3<-Sys.glob(paste(dir,metric_f3,sep=""))
     metric_f4<-'/*/*/*/*/outs/per_sample_outs/*/metrics_summary.csv'
     fff4<-Sys.glob(paste(dir,metric_f4,sep=""))
     ff<-c(ff,fff2,fff3,fff4)
   }






    ##ff2<-gsub(".*multi_config_","",ff)
    ff2<-gsub(".*per_sample_outs\\/","",ff)
    ff3<-gsub("\\/.*","",ff2)


   f<- ff %>% tibble::as.tibble() %>% dplyr::mutate("n"=gsub(".*multi_config_","", value)) %>% dplyr::mutate("n"=gsub("\\/.*","", n)) %>% dplyr::arrange(as.numeric(n))
  fffF<<-f
    ff<-f$value


    fff<<-ff
    df.list = list()
   # foreach (i=1:length(t(ff)),.combine="rbind") %do% {
    for(i in 1:length(t(ff))) {

      id<-gsub(".*\\/(.*)\\/(.*)\\/outs.*","\\1_\\2",perl=TRUE,ff[i])

       dddd<<-id

      dddd2<<-id
    #  id<-gsub("multi_config_","",perl=TRUE,id )
      #id<-gsub("","",perl=TRUE,id )
      df <-readr::read_csv(ff[i])  %>% dplyr::filter(!is.na(`Group Name`) ) %>% dplyr::filter(`Library Type` == file$sel())
        ##dplyr::filter(`Library Type` == "Gene Expression") #%>% filter(`Library Type` %in% "VDJ T") #c("Antibody Capture","Gene Expression"))

       colnames(df)[6] <- paste("sample",id, sep="_")
       dffff<<-df[6]
       df[6] <- readr::parse_number(df[[6]]) %>% tibble::as.tibble()
      df.list[[i]] = df
    }


    ss<<-df.list
    df.list<-purrr::discard(df.list, function(z) nrow(z) == 0)
    sss<<-as.data.frame(df.list)
    s1<-as.data.frame(df.list)
    s<-as.data.frame(df.list) %>% tibble::as.tibble() %>% dplyr::select(c(1:5,starts_with("sample_"))) %>% dplyr::rename_all(~ sub("sample_", "", .x)) #%>% tibble::as_tibble(.name_repair = "unique")

##data.table::rbindlist(ss, fill=TRUE) %>% view()




    })
})
  return(list(s =s,s1=s1))
  })




output$tab <- DT::renderDataTable(
  DT::datatable(
    dat2()$s,
    extensions = 'Buttons', options = list(
      dom = 'Bfrtip', pageLength = 500,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')

    )
  )
)



return(list(df =dat2

)
)




}

## To be copied in the UI
# mod_dataInput_ui("dataInput_1")

## To be copied in the server
# mod_dataInput_server("dataInput_1")
