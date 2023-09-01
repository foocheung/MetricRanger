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

  tabPanel("Tables + Web Summary",
tabsetPanel(

  tabPanel("Tables",
DT::dataTableOutput(ns('tab'))
),
  tabPanel("Web",
        uiOutput(ns("web")),
        actionButton(ns("go4"),"Go!"),
       # htmlOutput(ns("web_if")) #,
       slickR::slickROutput(ns("web_if"))
       # htmlOutput(ns("web_if2"))#,
      #  textOutput(uu)
       # includeHTML("/Volumes/CHI/TEMP/TEST3/CHI018_atac/outs/web_summary.html")
)
)
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

    metric_g<-'/*/outs/summary.csv'
    ffg<-Sys.glob(paste(dir,metric_g,sep=""))

    metric_g2<-'/*/*/outs/summary.csv'
    ffg2<-Sys.glob(paste(dir,metric_g2,sep=""))

    ff<-c(ff,fff2, ffg, ffg2)
    ff2A2<<-ff
    }
   else if (file$traf() ==3){
     metric_f2<-'/*/*/outs/per_sample_outs/*/metrics_summary.csv'
     fff2<-Sys.glob(paste(dir,metric_f2,sep=""))
     metric_f<-'/*/outs/per_sample_outs/*/metrics_summary.csv'
     ff<-Sys.glob(paste(dir,metric_f,sep=""))
     metric_f3<-'/*/*/*/outs/per_sample_outs/*/metrics_summary.csv'
     fff3<-Sys.glob(paste(dir,metric_f3,sep=""))

     metric_g<-'/*/outs/summary.csv'
     ffg<-Sys.glob(paste(dir,metric_g,sep=""))

     metric_g2<-'/*/*/outs/summary.csv'
     ffg2<-Sys.glob(paste(dir,metric_g2,sep=""))

     metric_g3<-'/*/*/*/outs/summary.csv'
     ffg3<-Sys.glob(paste(dir,metric_g3,sep=""))

     ff<-c(ff,fff2,fff3,ffg3, ffg, ffg2)
     ff3A3<<-ff
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

      df <-readr::read_csv(ff[i])  %>% janitor::clean_names()  %>% dplyr::filter(library_type == file$sel()) ## %>% dplyr::filter(!is.na(group_name) ) %>% dplyr::filter(library_type == file$sel())
      df_all<-readr::read_csv(ff[i])  %>% janitor::clean_names()  ## %>% dplyr::filter(!is.na(group_name))

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
   #   f.list[[i]]<-c(path,Split[[1]][length(Split[[1]])-5],Split[[1]][length(Split[[1]])-4],dput(libs))
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

    #  if (NCOL(df.list) == 1){
    #
    #    showModal(
    #      modalDialog(
    #        div("There is no processes data associated with this Analysis"),easyClose = TRUE)
    #    )
    #
    #  return()
    #
    # #
    #  }else{

      s<-as.data.frame(df.list) %>% tibble::as.tibble() %>% dplyr::select(c(1:5,starts_with("sample_"))) %>% dplyr::rename_all(~ sub("sample_", "", .x)) %>%
      dplyr::mutate_if(is.numeric, round, digits=3) %>%
      dplyr::rename_all(~stringr::str_replace(.x,"TEST3_",""))
 #}
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

#$lib<-renderUI({



 # selectInput(ns("sel"), "Select Library", c("VDJ T","Antibody Capture","Gene Expression","VDJ B", "ATAC"), "Gene Expression"),

  #})


output$tab <- DT::renderDataTable(
  DT::datatable(
    cbind(dat2()$s,prop.table(data.matrix(dat2()$s[6:length(dat2()$s)] %>% dplyr::mutate_if(is.character, as.numeric) ),margin = 1)  %>%
  #  cbind(dat2()$s,prop.table(data.matrix(dat2()$s[1:length(dat2()$s)] %>% dplyr::mutate_if(is.character, as.numeric) ),margin = 1)  %>%
            tibble::as.tibble()  %>%   dplyr::rename_with( ~ paste0("prop_", .x)) )    ,
  #  extensions = 'Buttons', options = list(columnDefs = list(list(targets=c(1:4,(length(dat2()$s)+1):(length(dat2()$s) * 2 - 5)),visible=FALSE)),
  extensions = 'Buttons', options = list(columnDefs = list(list(targets=c((length(dat2()$s)+1):(length(dat2()$s) * 2 - 5)),visible=FALSE)),
      dom = 'Bfrtip', pageLength = 500,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')

 #   )) %>%  DT::formatStyle(names(dat2()$s[6:length(dat2()$s)]),
    )) %>%  DT::formatStyle(names(dat2()$s[6:length(dat2()$s)]),
                            background = DT::styleColorBar(range(0, 1), 'lightblue'),
                            backgroundSize = '98% 88%',
                            valueColumns = c((length(dat2()$s)+1):(length(dat2()$s) * 2 - 5)),fontWeight = "bold",
                           # valueColumns = c(11:15),
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center') %>%
    DT::formatStyle(names(dat2()$s[1:5]), fontWeight = "bold")

)

output$web<-renderUI({

  loc<-dat2()$ff

  names<-gsub(".*\\/(.*)\\/(.*)\\/outs.*","\\2",perl=TRUE,loc)
    ur<-loc %>% tibble::as.tibble() %>%
    dplyr:: mutate(url=gsub("summary.csv|metrics_summary.csv","web_summary.html",value)) %>%
    dplyr::select(url) %>% dplyr::pull()
uuu<<-ur

  tagList(
    fluidPage(

      column(12,
             selectInput(ns('websumm'), "Web Summary", choices = dput(ur),width = '700px')#,

      #       selectInput(ns('websumm2'), "Web Summary", choices = dput(ur),width ='700px' )
   # div(style="display: inline-block;vertical-align:top; width: 200;", selectInput(ns('websumm'), "Web Summary", choices = dput(ur))), #,selected=ur[1])),
  #div(style="display: inline-block;vertical-align:top; width: 200%;",selectInput(ns('websumm2'), "Web Summary", choices = dput(ur))) #, selected = ur[2]))
)
)
  )
    })


loadpic <- reactive({
  validate(
    need(input$websumm, "Member input is null!!")
  )

 uu<- input$websumm

 m <- paste("/",paste(gtools::split_path(dirname(uu),depth_first = FALSE)[1:(length(unlist(strsplit(uu, '/')))-6)], collapse = '/'), sep="")
 # m<-paste(gtools::split_path(dirname(uu))[1:(length(unlist(strsplit(uu, '/')))-6)], collapse = '/')
#  m<-paste("/",m,sep="")
   addResourcePath("library", m)
 #addResourcePath("library", "/Volumes/CHI/TEMP/RUN")
    mmm<<-m
     m2<- paste(split_path(dirname(uu),depth_first = FALSE)[(length(unlist(strsplit(uu, '/')))-5):(length(unlist(strsplit(uu, '/')))-2)], collapse = '/')
   m2<-paste(m2,"/web_summary.html",sep="")

   mmmm<<-m2
   paste("library/",m2, sep="")

   })

output$web_if<-slickR::renderSlickR({
#output$web_if<-renderUI({
##find a way of killing the CACHING OLD WEBPAGE !!!1
 # req(input$websumm)
  req(input$go4)
 ## req(file$goButtonp())
url1<-input$websumm
uu<<-url1
uuu<<-loadpic()
 # HTML('<iframe width="350" height="250" src=input$websumm frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
 # HTML('<iframe width="350" height="250" src="https://www.yahoo.com" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  #test <<- paste0("https://google.com") #sample url
 # test <<- paste0("/Volumes/CHI/TEMP/TEST3/CHI018_atac/outs/web_summary.html") #sample url
#uu
#uuu<<-tags$iframe(src = paste("library/",basename(uu), sep="") ,style='width:100vw;height:100vh;')

#uuu<<-dirname(input$websumm)
#tags$iframe(src = "" ,style='width:100vw;height:100vh;')
#
####addResourcePath("library", dirname(input$websumm))

 # tagList(
 #   fluidPage(

#      column(6,
#
 # tags$iframe(src = paste("library/",basename(input$websumm), sep="") ,style='width:100vw;height:100vh;')
######tags$iframe(src = paste("library/",basename(input$websumm), sep="") ,style='width:100vw;height:100vh;')
slickR::slickR(slickR::slick_list(
  tags$iframe(height="800px",
              width="100%",
              scrolling=T,
    src = loadpic() ,
    height = 500,id="theframe"
  )
))
#)
#)
#    removeResourcePath("library")
#)
#  print(my_test)
#  my_test
})


output$web_if2<-renderUI({
  req(input$go4)
  url2<-input$websumm2
  uu2<<-url2

  # HTML('<iframe width="350" height="250" src=input$websumm frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  # HTML('<iframe width="350" height="250" src="https://www.yahoo.com" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  #test <<- paste0("https://google.com") #sample url
  # test <<- paste0("/Volumes/CHI/TEMP/TEST3/CHI018_atac/outs/web_summary.html") #sample url

  addResourcePath("library2", dirname(input$websumm2) )
  tagList(
    fluidPage(

         column(6,


    tags$iframe(height="800px",
                width="100%",
                scrolling=T,src = paste("library2/",basename(input$websumm2), sep="") )

    )
    )
  )
  #  print(my_test)
  #  my_test
})


return(list(df =dat2))




}

## To be copied in the UI
# mod_dataInput_ui("dataInput_1")

## To be copied in the server
# mod_dataInput_server("dataInput_1")
