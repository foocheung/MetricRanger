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
  tabPanel("Delta",
                                shinyFiles::shinyDirButton(ns("directory2"), "Load 1",
                                                           icon=icon("file-arrow-up"),
                                                           ""),
                                shinyFiles::shinyDirButton(ns("directory3"), "Load 2",
                                                           icon=icon("file-arrow-up"),
                                                           ""),

                                shinyscreenshot::screenshotButton(label="Captureentirepage"),
                                actionButton(ns("goButtonpd"), "Go!",icon("paper-plane"))
  )

}

# Module Server

#' @noRd
#' @export
#' @keywords internal

mod_dataInput_server3 <- function(input, output, session){
  ns <- session$ns


#mod_dataInput_server3 <- function(input, output, session){  #,batches,sim){
 # ns <- session$ns



  volumes <- shinyFiles::getVolumes()()
  shinyFiles::shinyDirChoose(input, 'directory2', roots=volumes, session=session)
  shinyFiles::shinyDirChoose(input, 'directory3', roots=volumes, session=session)

  path2 <- reactive({
    req(input$goButtonpd)
    return(print(shinyFiles::parseDirPath(volumes, input$directory2)) )
  })


  path3 <- reactive({
    req(input$goButtonpd)
    return(print(shinyFiles::parseDirPath(volumes, input$directory3)) )
  })





  dat_data2<-reactive({

    req(input$goButtonpd)
    # output$col <- renderPrint({
    ##sliderInput(inputId="dat_st", label = "Data Starts Here"),
  #  isolate({
      withProgress(message="Please wait...", detail="", value=0, max=100, {

        dir<-path2()

        ddd<<-dir

        fsel<<- "ALL"

          metric_f<-'/*/outs/per_sample_outs/*/metrics_summary.csv'
          ff<-Sys.glob(paste(dir,metric_f,sep=""))


          metric_g<-'/*/outs/summary.csv'
          ffg<-Sys.glob(paste(dir,metric_g,sep=""))
          fffgg2<<-ffg


          ff<-c(ff,ffg)
          ff2A<<-ff

        ff2<-gsub(".*per_sample_outs\\/","",ff)
        ff3<-gsub("\\/.*","",ff2)


        f<- ff %>% tibble::as.tibble() %>% dplyr::mutate("n"=gsub(".*multi_config_","", value)) %>% dplyr::mutate("n"=gsub("\\/.*","", n)) %>% dplyr::arrange(as.numeric(n))
        fffF<<-f
        ff<-f$value


        fff<<-ff
        df.list = list()
        f.list=list()

           for(i in 1:length(t(ff))) {

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

            if (file$sel() == "ALL"){
              df <-readr::read_csv(ff[i])  %>% janitor::clean_names() ## %>% dplyr::filter(library_type == file$sel()) ## %>% dplyr::filter(!is.na(group_name) ) %>% dplyr::filter(library_type == file$sel()
            }
            else{
              df <-readr::read_csv(ff[i])  %>% janitor::clean_names()  %>% dplyr::filter(library_type %in% file$sel())
            }

            dfdf<<-df
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

   #     s <- df.list %>%  purrr::map(subset,select=-c(group_name,grouped_by))  %>% purrr::reduce(dplyr::bind_rows)  %>% tidyr::pivot_longer(
    #      cols = dplyr::starts_with("sample"), values_to = "values") %>% dplyr::distinct()  %>% na.omit(values)  %>% tidyr::pivot_wider(names_from = name, values_from = values)%>%
    #      dplyr::rename_all(~ sub("sample_", "", .x))
s<-1
        sss<<-s


      })
      return(list(s =s,s1=s1,ff=ff, f=f,flst=flst ))

        })
  #


   # })


}
