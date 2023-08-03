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



      tabPanel("Tables",

   DT::dataTableOutput(ns('tab'))

)
}


# Module Server

#' @noRd
#' @export
#' @keywords internal

mod_dataInput_server2 <- function(input, output, session, file){
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


    metric_g<-'/*/outs/summary.csv'
    ffg<-Sys.glob(paste(dir,metric_g,sep=""))
    fffgg2<<-ffg

    ff<-c(ff,ffg)
    ff2A<<-ff
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
    f.list=list()
   # foreach (i=1:length(t(ff)),.combine="rbind") %do% {
    for(i in 1:length(t(ff))) {




     # dddd<<-id

     # dddd2<<-id


      file_o<-gsub(".*\\/(.*)\\/(.*)\\/outs\\/(['summary.csv'\\|'metrics_summary.csv'])","\\3",perl=TRUE,ff[i])
       fffo <<-file_o
      if (file_o == 'summary.csv'){

        path<-gsub(".*\\/.*\\/.*\\/(.*)\\/outs","\\1",perl=TRUE,ff[i])

        id<-gsub(".*\\/.*\\/(.*)\\/(.*)\\/outs","\\1_\\2",perl=TRUE,ff[i])
        #id<-gsub(".*\\/.*\\/(.*)\\/(.*)\\/outs","\\2",perl=TRUE,ff[i])

        a<-readr::read_csv(ff[i])  %>% janitor::clean_names()
        aa2<<-a
        a<-t(a) %>% as.data.frame() %>% tibble::rownames_to_column()
        aa3<<-a
        colnames(a)<-c( "metric_name","filename")
        aa4<<-a

        df<-a %>% dplyr::mutate("category" = "Library")%>%
          dplyr::mutate("library_type" = "ATAC") %>% dplyr::mutate("group_name" = 'NA')%>% dplyr::mutate("group_by" = 'NA') %>%
          dplyr::select(category,"library_type", "group_by", "group_name", "metric_name", "filename" )

        df<- df[-c(1:3),]
        df<-df %>% dplyr::filter(!grepl("q30",metric_name))

        ff_all<<-df
        df_all<-df
        df<-df %>% dplyr::filter(library_type == file$sel())
        libs<-dplyr::pull(df_all, "library_type") %>% unique()
      } else{

        path<-gsub("(.*)\\/outs.*","\\1",perl=TRUE,ff[i])

        id<-gsub(".*\\/(.*)\\/(.*)\\/outs.*","\\1_\\2",perl=TRUE,ff[i])
      #  id<-gsub(".*\\/(.*)\\/(.*)\\/outs.*","2",perl=TRUE,ff[i])

      df <-readr::read_csv(ff[i])  %>% janitor::clean_names() %>% dplyr::filter(!is.na(group_name) ) %>% dplyr::filter(library_type == file$sel())
      df_all<-readr::read_csv(ff[i])  %>% janitor::clean_names()  %>% dplyr::filter(!is.na(group_name))

      ff_all2<<-df_all
      libs<-dplyr::pull(df_all, library_type) %>% unique()
      }




       lll<<-libs

       colnames(df)[6] <- paste("sample",id, sep="_")
       dffff<<-df[6]
       if (file_o == 'summary.csv'){
         df[6] <- df[[6]] %>% tibble::as.tibble()
         df.list[[i]] = df

         }else{

       df[6] <- readr::parse_number(df[[6]]) %>% tibble::as.tibble()
       df.list[[i]] = df
       }

      Split <- strsplit(ff[i], "\\/")
      spsp<<-Split
      flist1<<-f.list
      iii<<-i
      s1<<- Split[[1]][length(Split[[1]])-5]
      s2<<- Split[[1]][length(Split[[1]])-4]
      s3<<- dput(libs)

      f.list[[i]]<-c(path,Split[[1]][length(Split[[1]])-5],Split[[1]][length(Split[[1]])-4],deparse(dput(libs)))

      flist2<<-f.list

     # f.list[[i]]<-c(ff[[i]][length(ff[[3]])-5],ff[[i]][length(ff[[3]])-4])
      }

    flst<-do.call(rbind.data.frame, f.list) %>% tibble::as.tibble()
    colnames(flst)<-c("path", "run_directory", "sample_directory", "library")

    fflst<<-flst
##NEED TO FIX HERE !!mkji9kl
    ss<<-df.list
    df.list<-purrr::discard(df.list, function(z) nrow(z) == 0)
    sss<<-as.data.frame(df.list)
    s1<-as.data.frame(df.list)
    s<-as.data.frame(df.list) %>% tibble::as.tibble() %>% dplyr::select(c(1:5,starts_with("sample_"))) %>% dplyr::rename_all(~ sub("sample_", "", .x)) %>%
      dplyr::mutate_if(is.numeric, round, digits=3) %>%
      dplyr::rename_all(~stringr::str_replace(.x,"TEST3_",""))

##data.table::rbindlist(ss, fill=TRUE) %>% view()




    })
})
  return(list(s =s,s1=s1,ff=ff, f=f,flst=flst ))
  })


# DT::datatable(
#   cbind(t,prop.table(data.matrix(t[6:length(t)]) %>% mutate_if(is.character, as.numeric)),margin = 1) %>% as.tibble()  %>%  rename_with( ~ paste0("prop_", .x))) ,
# extensions = 'Buttons', options = list(
#   dom = 'Bfrtip', pageLength = 500,
#   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
#
# )) %>%  DT::formatStyle(paste("prop_",names(t[6:length(t)]), sep=""),
#                         background = DT::styleColorBar(c(0,1), 'lightblue'),
#                         backgroundSize = '98% 88%',
#                         backgroundRepeat = 'no-repeat',
#                         backgroundPosition = 'center')
#%>%
#  formatRound(which(sapply(iris,is.numeric)), digits = 2)



output$tab <- DT::renderDataTable(
  DT::datatable(
    cbind(dat2()$s,prop.table(data.matrix(dat2()$s[6:length(dat2()$s)] %>% dplyr::mutate_if(is.character, as.numeric) ),margin = 1)  %>%
            tibble::as.tibble()  %>%   dplyr::rename_with( ~ paste0("prop_", .x)) )    ,
    extensions = 'Buttons', options = list(columnDefs = list(list(targets=c(1:4,(length(dat2()$s)+1):(length(dat2()$s) * 2 - 5)),visible=FALSE)),
      dom = 'Bfrtip', pageLength = 500,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')

    )) %>%  DT::formatStyle(names(dat2()$s[6:length(dat2()$s)]),
                            background = DT::styleColorBar(range(0, 1), 'lightblue'),
                            backgroundSize = '98% 88%',
                            valueColumns = c((length(dat2()$s)+1):(length(dat2()$s) * 2 - 5)),fontWeight = "bold",
                           # valueColumns = c(11:15),
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center') %>%
    DT::formatStyle(names(dat2()$s[1:5]), fontWeight = "bold")

)



return(list(df =dat2))




}

## To be copied in the UI
# mod_dataInput_ui("dataInput_1")

## To be copied in the server
# mod_dataInput_server("dataInput_1")
