# RatiocalcR
# Author: Giancarlo Tamburello
# Email: giancarlo.tamburello@ingv.it


#--------------------------------------------------------
# RATIOCALC SCRIPT START
#--------------------------------------------------------

library("fields")
library("sp")
library("geosphere")
library("shiny")
library("png")
library("signal")
library("MBA")
library("spatial")
library("splines")
library("aTSA")
library("boot")
library("simpleboot")
library("DT")
library("leaflet")

source("Ratiocalc_files.R")
source("Ratiocalc_calc.R")


#--------------------------------------------------------
# Ratiocalc Graphical Interface
#--------------------------------------------------------




#----------------------------
# Define UI for data upload app


ui <- function(){tagList(
  
  fluidPage(
    
    titlePanel(
      windowTitle = "Ratiocalc",
      title = tags$head(tags$link(rel="icon", 
                                  href="data:image/x-icon;base64,AAABAAEAEBAAAAAAAABoBQAAFgAAACgAAAAQAAAAIAAAAAEACAAAAAAAAAEAAAAAAAAAAAAAAAEAAAAAAAAAAAAAioqKAP+EAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAAAQAAAgAAAAAAAAAAAAAAAAABAQIAAAAAAAAAAAAAAAAAAAABAQAAAgAAAAIAAAAAAAAAAgABAQAAAAAAAAAAAAAAAAAAAAABAQAAAAAAAAAAAAACAAAAAAABAQIAAAAAAAAAAAAAAAIAAAABAQAAAAAAAAAAAAAAAAAAAAABAQAAAAAAAAAAAgAAAAIAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB//wAAf/8AAHf/AAB/fwAAN/8AAEf/AABzdwAAdP8AAH8/AAB3xwAAf3MAAH/8AAB/dwAAf/cAAH//AAA=", 
                                  type="image/x-icon")
      )),
    tags$head(HTML("<title>Ratiocalc</title>")),
    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    
    #----------------------------
    # Sidebar layout with input and output definitions
    sidebarLayout(
      
      #----------------------------
      # Sidebar panel for inputs
      
      sidebarPanel(
        
        #----------------------------
        # Input: Select a file and separator
        
        fluidRow(
          column(6,selectInput(inputId = "exm",label = "Examples:",choices = c("_","Lastarria","La Soufriere"),selected = "_") )),
        
        fluidRow(
          column(7,fileInput("file", "Choose File",multiple = TRUE)),
          fluidRow(column(3,radioButtons("sep", "Separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ";")),
                   column(1,checkboxInput(inputId =   "firstcol",label ="Time",value = T)))
        ),
        
        #----------------------------
        # Input: Previous file, Filename and Next file
        
        fluidRow(
          column(2,actionButton("prevcsv", "Prev",width = "100%")),
          column(2,numericInput(inputId ="ncsv",value = 1,label = NULL)),
          column(6,verbatimTextOutput("filezip")),
          column(2,actionButton("nextcsv", "Next",width = "100%"))),
        
        
        tags$hr(),
        
        
        
        #----------------------------
        # Input: Select numerator and denominator
        
        fluidRow(
          column(6,selectInput(inputId = "yy",label = "Numerator:",choices = c("_"),selected = "A") ),
          column(6,selectInput(inputId = "xx",label = "Denominator:",choices = c("_"),selected = "_"))),
        
        #----------------------------
        # Input: Select treshold and filter of numerator and denominator 
        
        fluidRow(
          column(3,numericInput(inputId ="tyy",label =  "Treshold:", 0)),                 
          column(3,numericInput(inputId ="fyy",label = "Filter:", 0)),
          column(3,numericInput(inputId ="txx",label = "Treshold:", 0)),
          column(3,numericInput(inputId ="fxx",label = "Filter:", 0))),
        
        
        fluidRow(
          column(3,numericInput(inputId ="oyy",label = "Offset:", 0)),
          column(3),
          column(3,numericInput(inputId ="oxx",label = "Offset:", 0)),
          column(3)),
        
        
        tags$hr(),
        
        #----------------------------
        # Input: Select shift, shift on/off and plume marker, add current ratio
        
        fluidRow(
          column(2,numericInput("sh", "Shift:",min = -20, max = 20,value = 0,step = 1)),
          column(1,checkboxInput(inputId =   "fsh",label = NULL ,value = F)),
          column(3,selectInput(inputId = "mm",label = "Marker:",choices = c("_"),selected = "_")),
          column(1,checkboxInput(inputId =   "ptp",label = NULL ,value = F)),
          column(5,fluidRow(
            column(6,actionButton(  "ratio", "Add",width = "100%")),
            column(6,actionButton(  "boot", "Boot",width = "100%"))
            
          )
          
          )),
        
        tags$hr(),
        
        #----------------------------
        # Input: Latitude and longitude, Google map zoom level
        fluidRow(
          column(3,selectInput(inputId = "lat",label = "Latitude:",choices = c("_"),selected = "_")),
          column(3,selectInput(inputId = "long",label = "Longitude:",choices = c("_"),selected = "_")),
          column(1,checkboxInput(inputId =   "domap",label =   "",value = F))),
        
        tags$hr()
        
      ),
      
      #----------------------------
      # Main panel for displaying outputs
      
      mainPanel(
        
        #----------------------------
        # Output: Data file
        # TableOutput("contents")
        
        tabsetPanel(id = "tab_being_displayed",type = "tabs",
                    
                    tabPanel("Plot",
                             fluidRow(
                               plotOutput(outputId =  "plot1", click ="plot1click",dblclick ="plot1dblclick"),
                               fluidRow(
                                 column(1,checkboxInput(inputId =   "dozm",label =   "Zoom",value = F)),
                                 column(width =  10,sliderInput("range", "Range:",value =c(1,10),min = 1,max = 10,width = "100%"))),
                               htmlOutput("text1"),
                               plotOutput("plot2"),
                               plotOutput("plot3"))),
                    
                    tabPanel("Map",
                             leafletOutput("mymap"),
                             fluidRow(
                               #plotOutput("plot4",width = "100%"),
                               column(3,numericInput(inputId ="mzoom",label = "Zoom out:", 0)),
                               selectInput(inputId = "maptype",label = "Map type:",choices = c("Open Street Map" = "osm", "ESRI" = "esri"),selected = "osm")
                             )),
                    
                    tabPanel("Summary",
                             fluidRow(
                               tags$hr(),
                               actionButton(  "refresh1", "Refresh",width = "150px"),
                               tags$hr(),
                               downloadButton("downloadSummary", "Download results"),
                               actionButton(  "clear", "Clear summary",width = "150px"),
                               actionButton(  "rmratio", "Remove ratio",width = "150px"),
                               tags$hr(),
                               DTOutput("resultstable")

                             )),
                    tabPanel("Data",
                             fluidRow(
                               # Input: Select number of rows to display ----
                               tags$hr(),
                               DTOutput("contents")
                             ),
                             downloadButton("merge", "Download merged files")
                    ),
                    tabPanel("Batch",
                             fluidRow(
                               fluidRow(
                                 column(numericInput(inputId ="winsize",label = "Window size:", 10),width = 4),
                                 column(numericInput(inputId ="movstep",label = "Moving step:", 1),width = 4),
                                 column(numericInput(inputId ="maxshift",label = "Max shift:", 5),width = 4)),
                               fluidRow(
                                 column(numericInput(inputId ="rlimit",label = "R limit:", 0.6),width = 4),
                                 column(numericInput(inputId ="gaslim",label = "Min gas:", 1),width = 4),
                                 column(numericInput(inputId ="peak",label = "Min samples half peak:", 0),width = 4)
                               ),
                               verbatimTextOutput("maxratios"),
                               htmlOutput("text2"),
                               radioButtons("batchtype", "Processing files:",choiceNames = c("Single","Multiple"),choiceValues = c("single","multiple"),selected = "single",inline = T),
                               fluidRow(
                                 column(actionButton("batch", "Run batch"),width = 4),
                                 column(actionButton("abortbatch", "Abort batch"),width = 4),
                                 column(downloadButton("downloadBatch", "Download batch results"),width = 4)),
                               plotOutput("plot7",width = "100%")
                             )),
                    tabPanel("Flux",
                             fluidRow(
                               plotOutput("plot5"),
                               
                               
                               fluidRow(
                                 column(12,radioButtons(inputId = "coordICA",label = "Select coordinates type: ",
                                                        choiceNames = c("Decimal long/Lat","UTM","Distance"),
                                                        choiceValues =c("lonlat","UTM","dist"),selected = "lonlat",inline = T))),
                               
                               #----------------------------
                               # Input: Height, Upper zero, Lower zero, ICA on/off, ICA coefficients
                               fluidRow(
                                 column(2,selectInput(inputId = "height",label = "Height:",choices = c("_"),selected = "_")),
                                 column(2,numericInput(inputId ="upzero",label = "Upper:", 1)),
                                 column(2,numericInput(inputId ="lowzero",label = "Lower:", 0)),
                                 column(1,radioButtons(inputId = "doICA",label = "",choices = c("spline","gauss"))),
                                 column(3,textInput(inputId ="nn",label = "Coefs:",value = ""))),
                               
                               plotOutput("plot6")
                             )),
                    tabPanel("Water calc",fluidRow(plotOutput("plot8"),
                                                   fluidRow(
                                                     column(3,selectInput(inputId = "waterRh",label = "Select Rh (%):",choices = c("_"),selected = "A") ),
                                                     column(3,selectInput(inputId = "waterT",label = "Select Temp (°C):",choices = c("_"),selected = "_")),
                                                     column(2,numericInput(inputId ="waterP",label = "Pressure (mbar):", 1013)),
                                                     column(3,actionButton("watercalc", "Calculate water from Rh, T, P"))
                                                   ))
                    ),
                    tabPanel("Detrend",fluidRow(plotOutput("plot9",click ="plot9click",dblclick ="plot9dblclick"),
                                                fluidRow(
                                                  column(1,radioButtons(inputId = "detrnd",label = "",choices = c("Numerator"="y","Denominator"="x"),selected = "y")),
                                                  column(3,actionButton("detrcanc", "Cancel trend")),
                                                  column(2,radioButtons(inputId = "fauto",label = "Type:",choices =c("Auto"=1,"Manual"=0),selected = 0))
                                                ))
                    ),
                    tabPanel("Response",fluidRow(plotOutput("plot10",click ="plot10click"),
                                                 fluidRow(
                                                   column(1,radioButtons(inputId = "respnd",label = "",choices = c("Numerator"="y","Denominator"="x"),selected = "y")),
                                                   column(2,numericInput(inputId ="respdt",label = "Sampling period (s):", 1)),
                                                   column(6,verbatimTextOutput("respcoeff"))
                                                   
                                                 ))
                    )
                    
                    
                    
        )
        
      )
      
    )
  ) )}

#--------------------------------------------------------
# Ratiocalc processes
#--------------------------------------------------------

server <- function(input, output,session) {
  
  
  
  autoInvalidate <- reactiveTimer(2000)
  
  #-----------
  # Allocate variables
  values <- reactiveValues(mainboot=0)
  values <- reactiveValues(doboot=0)
  values <- reactiveValues(respdf = data.frame(x=NA,y=NA))
  values <- reactiveValues(respok = 0)
  values <- reactiveValues(nextbutton = 0)
  values <- reactiveValues(zipon = 1)
  values <- reactiveValues(batch_summary=NULL)
  values <- reactiveValues(batch_row=NULL)
  values <- reactiveValues(zm=c(0,0,0))
  values <- reactiveValues(df_trend=NULL)
  values <- reactiveValues(df_trendpp=NULL)
  values <- reactiveValues(ii = 1)
  values <- reactiveValues(df_data = NULL)
  values <- reactiveValues(results_batch = data.frame(ID=NULL,
                                                      time=NULL,
                                                      ratio=NULL,
                                                      r=NULL,
                                                      shift=NULL,
                                                      max.conc=NULL,
                                                      nsamples=NULL))
  
  values <- reactiveValues(results_data = 
                             data.frame(
                               ID=NULL,
                               ratio=NULL,
                               start=NULL,
                               end=NULL,
                               area=NULL,
                               slope=NULL,
                               intercept=NULL,
                               cor=NULL,
                               sd=NULL,
                               marker=NULL,
                               gasmax=NULL,
                               shift=NULL,
                               filter=NULL,
                               tres=NULL,
                               nsamples=NULL,
                               latitude=NULL,
                               longitude=NULL))
  
  values <- reactiveValues(results_row = 
                             data.frame(
                               ID=NULL,
                               ratio=NULL,
                               start=NULL,
                               end=NULL,
                               area=NULL,
                               slope=NULL,
                               intercept=NULL,
                               cor=NULL,
                               sd=NULL,
                               marker=NULL,
                               gasmax=NULL,
                               shift=NULL,
                               filter=NULL,
                               tres=NULL,
                               nsamples=NULL,
                               latitude=NULL,
                               longitude=NULL))
  
  #----------
  # EXAMPLE
  
  observeEvent(input$exm,{
    
    if (input$exm == "_"){
      return(NULL)
    }
    else{
      
      if (input$exm == "Lastarria"){
        filename="Ratiocalc_example_2.csv"
      }else if (input$exm == "La Soufriere"){
        filename="Ratiocalc_flux_example.csv"
      }
      
      df <- read.csv(filename,
                     header = T,
                     sep = ";",na.strings = c("NaN","NAN","NA","Na"))
      
      if(!input$firstcol){df=cbind(1:nrow(df),df)}
      
      values$df_data <- read.csv(filename,
                                 header = T,
                                 sep = input$sep,na.strings = c("NaN","NAN","NA","Na"))
      
      
      updateSliderInput(session = session,inputId = "range",value = 1,min = 1,max = nrow(df))
      
      values$zm=c(0,0,0)
      updateSelectInput(session= session,inputId ="yy",label = "Numerator:",choices = names(df)[-1],selected =names(df)[2] )
      updateSelectInput(session= session,inputId ="xx",label = "Denominator:",choices = names(df)[-1],selected =names(df)[3] )
      updateSelectInput(session= session,inputId ="lat",label = "Latitude:",choices = names(df)[-1],selected =names(df)[3] )
      updateSelectInput(session= session,inputId ="long",label = "Longitude:",choices = names(df)[-1],selected =names(df)[3] )
      updateSelectInput(session= session,inputId ="mm",label = "Plume marker:",choices = names(df)[-1],selected =names(df)[3])
      updateSelectInput(session = session,inputId = "height",label = "Height:",choices = names(df)[-1],selected = names(df)[3])
      updateSelectInput(session = session,inputId = "waterRh",label = "Select Rh:",choices = names(df)[-1],selected = names(df)[3])
      updateSelectInput(session = session,inputId = "waterT",label = "Select Temp:",choices =names(df)[-1],selected =names(df)[3])
      
    } 
    
    
    
  })
  
  #----------
  # NEXT BUTTON
  observeEvent(input$nextcsv,{
    
    if (is.null(values$df_data))
      return(NULL)
    
    if (values$zipon == 0)
      return(NULL)
    
    xx=input$xx
    yy=input$yy
    mm=input$mm
    lat=input$lat
    long=input$long
    
    values$ii <- values$ii +1
    
    if (values$ii > length(list.files(paste("unzipped/",sep="")))) {
      values$ii <-length(list.files(paste("unzipped/",sep="")))
    }
    
    df <- read.csv(list.files(paste("unzipped/",sep=""),full.names = T)[values$ii],
                   header = T,
                   sep = input$sep,na.strings = c("NaN","NAN","NA","Na"))
    
    if(!input$firstcol){df=cbind(1:nrow(df),df)}
    
    
    values$df_data <- read.csv(file = list.files(paste("unzipped/",sep=""),full.names = T)[values$ii],header = T,sep = input$sep,na.strings = c("NaN","NAN","NA","Na"))
    
    output$filezip <- renderText({ list.files(paste("unzipped/",sep=""))[values$ii] })
    
    updateSliderInput(session = session,inputId = "range",min = 1,max = nrow(df))
    
    values$nextbutton=1
    values$zm=c(0,0,0)
    values$df_trend=NULL
    values$df_trendpp=NULL
    values$results_batch = data.frame(ID=NULL,time=NULL,ratio=NULL,r=NULL,shift=NULL,max.conc=NULL,nsamples=NULL)
    
    write.table(x = paste(Sys.time(),"load",nrow(df),"values"),
                file =  paste("log",".txt",sep = ""),
                append = T,col.names = F,row.names = F,quote = F)
    
    updateSelectInput(session= session,inputId ="yy",label = "Numerator:",choices = names(df)[-1],selected =yy )
    updateSelectInput(session= session,inputId ="xx",label = "Denominator:",choices = names(df)[-1],selected =xx )
    updateSelectInput(session= session,inputId ="lat",label = "Latitude:",choices = names(df)[-1],selected =lat )
    updateSelectInput(session= session,inputId ="long",label = "Longitude:",choices = names(df)[-1],selected =long )
    updateSelectInput(session= session,inputId ="mm",label = "Plume marker:",choices = names(df)[-1],selected =mm )
    updateSelectInput(session = session,inputId = "height",label = "Height:",choices = names(df)[-1],selected = mm)
    updateSelectInput(session = session,inputId = "waterRh",label = "Select Rh:",choices = names(df)[-1],selected = mm)
    updateSelectInput(session = session,inputId = "waterT",label = "Select Temp:",choices =names(df)[-1],selected = mm)
    updateNumericInput(session = session,inputId ="ncsv",value =  values$ii,label = NULL)
    
  })
  
  #----------
  # PREV BUTTON
  observeEvent(input$prevcsv,{
    
    if (is.null(values$df_data))
      return(NULL)
    
    if (values$zipon == 0)
      return(NULL)
    
    xx=input$xx
    yy=input$yy
    mm=input$mm
    lat=input$lat
    long=input$long
    
    values$ii <- values$ii -1
    
    if (values$ii < 1) {
      values$ii <- 1
    }
    
    df <- read.csv(list.files(paste("unzipped/",sep=""),full.names = T)[values$ii],
                   header = T,
                   sep = input$sep,na.strings = c("NaN","NAN","NA","Na"))
    
    if(!input$firstcol){df=cbind(1:nrow(df),df)}
    
    
    values$df_data <- read.csv(file = list.files(paste("unzipped/",sep=""),full.names = T)[values$ii],header = T,sep = input$sep,na.strings = c("NaN","NAN","NA","Na"))
    
    output$filezip <- renderText({ list.files(paste("unzipped/",sep=""))[values$ii] })
    
    updateSliderInput(session = session,inputId = "range",min = 1,max = nrow(df))
    
    values$nextbutton=1
    values$zm=c(0,0,0)
    values$df_trend=NULL
    values$df_trendpp=NULL
    values$results_batch = data.frame(ID=NULL,time=NULL,ratio=NULL,r=NULL,shift=NULL,max.conc=NULL,nsamples=NULL)
    
    write.table(x = paste(Sys.time(),"load",nrow(df),"values"),
                file =  paste("log",".txt",sep = ""),
                append = T,col.names = F,row.names = F,quote = F)
    
    updateSelectInput(session= session,inputId ="yy",label = "Numerator:",choices = names(df)[-1],selected =yy )
    updateSelectInput(session= session,inputId ="xx",label = "Denominator:",choices = names(df)[-1],selected =xx )
    updateSelectInput(session= session,inputId ="lat",label = "Latitude:",choices = names(df)[-1],selected =lat )
    updateSelectInput(session= session,inputId ="long",label = "Longitude:",choices = names(df)[-1],selected =long )
    updateSelectInput(session= session,inputId ="mm",label = "Plume marker:",choices = names(df)[-1],selected =mm )
    updateSelectInput(session = session,inputId = "height",label = "Height:",choices = names(df)[-1],selected = mm)
    updateSelectInput(session = session,inputId = "waterRh",label = "Select Rh:",choices = names(df)[-1],selected = mm)
    updateSelectInput(session = session,inputId = "waterT",label = "Select Temp:",choices =names(df)[-1],selected = mm)
    updateNumericInput(session = session,inputId ="ncsv",value =  values$ii,label = NULL)
    
  })
  
  #----------
  # CHANGE NUM FILE
  observeEvent(input$ncsv,{
    
    if (is.null(values$df_data))
      return(NULL)
    
    if (is.null(values$nextbutton) || values$nextbutton == 1) {
      values$nextbutton=0
      return(NULL)
    }
    
    
    if (values$zipon == 0)
      return(NULL)
    
    xx=input$xx
    yy=input$yy
    mm=input$mm
    lat=input$lat
    long=input$long
    
    values$ii <- input$ncsv
    
    if (values$ii > length(list.files(paste("unzipped/",sep="")))) {
      values$ii <-length(list.files(paste("unzipped/",sep="")))
      updateNumericInput(session = session,inputId ="ncsv",value =  values$ii,label = NULL)
      
    }
    
    df <- read.csv(list.files(paste("unzipped/",sep=""),full.names = T)[values$ii],
                   header = T,
                   sep = input$sep,na.strings = c("NaN","NAN","NA","Na"))
    
    if(!input$firstcol){df=cbind(1:nrow(df),df)}
    
    
    values$df_data <- read.csv(file = list.files(paste("unzipped/",sep=""),full.names = T)[values$ii],header = T,sep = input$sep,na.strings = c("NaN","NAN","NA","Na"))
    
    output$filezip <- renderText({ list.files(paste("unzipped/",sep=""))[values$ii] })
    
    updateSliderInput(session = session,inputId = "range",min = 1,max = nrow(df))
    
    values$zm=c(0,0,0)
    values$df_trend=NULL
    values$df_trendpp=NULL
    values$results_batch = data.frame(ID=NULL,time=NULL,ratio=NULL,r=NULL,shift=NULL,max.conc=NULL,nsamples=NULL)
    
    write.table(x = paste(Sys.time(),"load",nrow(df),"values"),
                file =  paste("log",".txt",sep = ""),
                append = T,col.names = F,row.names = F,quote = F)
    
    updateSelectInput(session= session,inputId ="yy",label = "Numerator:",choices = names(df)[-1],selected =yy )
    updateSelectInput(session= session,inputId ="xx",label = "Denominator:",choices = names(df)[-1],selected =xx )
    updateSelectInput(session= session,inputId ="lat",label = "Latitude:",choices = names(df)[-1],selected =lat )
    updateSelectInput(session= session,inputId ="long",label = "Longitude:",choices = names(df)[-1],selected =long )
    updateSelectInput(session= session,inputId ="mm",label = "Plume marker:",choices = names(df)[-1],selected =mm )
    updateSelectInput(session = session,inputId = "height",label = "Height:",choices = names(df)[-1],selected = mm)
    updateSelectInput(session = session,inputId = "waterRh",label = "Select Rh:",choices = names(df)[-1],selected = mm)
    updateSelectInput(session = session,inputId = "waterT",label = "Select Temp:",choices =names(df)[-1],selected = mm)
    
    
  })
  
  
  
  #----------
  # MERGE FILES
  
  
  output$merge <- downloadHandler(
    
    filename = function() {
      "merge.csv"
    },
    content = function(file) {
      
      if (is.null(values$df_data))
        return(NULL)
      
      if (values$zipon == 0)
        return(NULL)
      
      xx=input$xx
      yy=input$yy
      mm=input$mm
      long=input$long
      
      values$ii <- values$ii +1
      
      if (values$ii > length(list.files(paste("unzipped/",sep="")))) {
        values$ii <-length(list.files(paste("unzipped/",sep="")))
      }
      
      
      for (ii in 1:length(list.files(paste("unzipped/",sep="")))) {
        df <- read.csv(list.files(paste("unzipped/",sep=""),full.names = T)[ii],
                       header = T,
                       sep = input$sep,na.strings = c("NaN","NAN","NA","Na"))  
        
        if(!input$firstcol){df=cbind(1:nrow(df),df)}
        
        
        if(ii == 1){
          dfall=df
        }else{
          dfall=rbind(dfall,df)
        }
        
      }
      
      
      write.csv(x =  dfall, file, row.names = FALSE,quote = F)
    }
  )
  
  #----------
  # ADD RATIO BUTTON
  
  observeEvent(input$ratio,{
    
    values$results_data=rbind(values$results_data,values$results_row)
    
    results=values$results_data
    
    save(results,file = paste("results_data",".RData",sep = ""))
    
    
    
  })
  
  #----------
  # REMOVE ALL MANUAL RATIOS
  
  observeEvent(input$clear,{
    
    values$results_data=data.frame(
      start=NULL,
      end=NULL,
      slope=NULL,
      intercept=NULL,
      cor=NULL,
      gasmax=NULL,
      latitude=NULL,
      longitude=NULL)
    
    results=values$results_data
    save(results,file = paste("results_data",".RData",sep = ""))
    
    
    
  })
  
  #----------
  # REMOVE LAST MANUAL RATIO
  
  observeEvent(input$rmratio,{
    
    
    values$results_data=values$results_data[1:(nrow(values$results_data)-1),]
    
    results=values$results_data
    
    save(results,file = paste("results_data",".RData",sep = ""))
    
    
  })
  

  #----------
  # FILE SELECTION
  observeEvent(input$file,{
    
    req(input$file)
    
    
    
    if (regexpr(".zip",input$file$datapath)[1]>-1) {
      
      file.remove(list.files(paste("unzipped/",sep=""),full.names = T))
      
      values$zipon = 1
      
      values$ii <- 1
      
      
      unzip(zipfile =  input$file$datapath,exdir = paste("unzipped/",sep="")   )
      
      
      df <- read.csv(list.files(paste("unzipped/",sep=""),full.names = T)[values$ii],
                     header = T,
                     sep = input$sep,na.strings = c("NaN","NAN","NA","Na"))
      
      if(!input$firstcol){df=cbind(1:nrow(df),df)}
      
      
      values$df_data <- read.csv(file = list.files(paste("unzipped/",sep=""),full.names = T)[values$ii],header = T,sep = input$sep,na.strings = c("NaN","NAN","NA","Na"))
      output$filezip <- renderText({ list.files(paste("unzipped/",sep=""))[values$ii] })
      
      
    }else{
      
      values$zipon = 0
      
      df <- read.csv(input$file$datapath,
                     header = T,
                     sep = input$sep,na.strings = c("NaN","NAN","NA","Na"))
      
      
      if(!input$firstcol){df=cbind(1:nrow(df),df)}
      
      
      values$df_data <- read.csv(input$file$datapath,
                                 header = T,
                                 sep = input$sep,na.strings = c("NaN","NAN","NA","Na"))
      
    }
    
    updateSliderInput(session = session,inputId = "range",value = 1,min = 1,max = nrow(df))
    
    write.table(x = paste(Sys.time(),"load",nrow(df),"values"),
                file =  paste("log",".txt",sep = ""),
                append = T,col.names = F,row.names = F,quote = F)
    
    
    values$zm=c(0,0,0)
    updateSelectInput(session= session,inputId ="yy",label = "Numerator:",choices = names(df)[-1],selected =names(df)[2] )
    updateSelectInput(session= session,inputId ="xx",label = "Denominator:",choices = names(df)[-1],selected =names(df)[3] )
    updateSelectInput(session= session,inputId ="lat",label = "Latitude:",choices = names(df)[-1],selected =names(df)[3] )
    updateSelectInput(session= session,inputId ="long",label = "Longitude:",choices = names(df)[-1],selected =names(df)[3] )
    updateSelectInput(session= session,inputId ="mm",label = "Plume marker:",choices = names(df)[-1],selected =names(df)[3])
    updateSelectInput(session = session,inputId = "height",label = "Height:",choices = names(df)[-1],selected = names(df)[3])
    updateSelectInput(session = session,inputId = "waterRh",label = "Select Rh:",choices = names(df)[-1],selected = names(df)[3])
    updateSelectInput(session = session,inputId = "waterT",label = "Select Temp:",choices =names(df)[-1],selected =names(df)[3])
    
    
  })
  
  
  #----------
  # DISPLAY DATA
  
  output$contents = renderDT({
    
    dtout=datatable(values$df_data, rownames = F) %>%
      formatRound(columns = 2:(ncol(values$df_data)), digits=2)
    
    dtout}
  )
  
  
  
  #----------
  # DISPLAY RESULTS
  
  output$resultstable <- renderDT( {
    
    input$refresh1
    input$ratio
    input$rmratio
    input$clear
    
    results_data=load(file =  paste("results_data",".RData",sep = ""))
    results_data=get(results_data)
    
    row.names(results_data)=NULL
    results_data
    
    dtout=datatable(results_data[,-1], rownames = F) %>%
      formatRound(columns = c(4:8,10:16), digits=3)
    
  }, rownames = F)
  
  #----------
  # PLOT1 TIME SERIES
  
  output$plot1 <- renderPlot({
    
    if (is.null(values$df_data))
      return(NULL)
    
    if (input$xx == "_" || input $yy == "_")
      return(NULL)
    
    df=values$df_data
    
    
    
    if (is.null(values$df_trend) == FALSE) {
      df[,-1]=df[,-1]-values$df_trend[,-1]
    }
    
    
    if (is.null(input$plot1click)==F) {
      updateSliderInput(session = session,inputId = "range",value = c(input$plot1click$x,input$range[2]))
    }
    
    if (is.null(input$plot1dblclick)==F) {
      updateSliderInput(session = session,inputId = "range",value = c(input$range[1],input$plot1dblclick$x))
    }
    
    time=df[,1]
    
    header=names(df)
    xx=which(header == input$xx)
    yy=which(header == input$yy)
    
    sh=input$sh
    if ((input$range[1]-input$sh)<1) {
      sh=0
    } 
    if ((input$range[2]-input$sh)>nrow(df)) {
      sh=0
    } 
    
    
    
    
    
    if (input$dozm == F && values$zm[1] == 0) {
      
      df=ts(data =  df[,c(yy,xx)])  
    }else if(input$dozm == T && values$zm[1] == 0){
      df=ts(data =  df[seq(as.integer(input$range[1]),as.integer(input$range[2])),c(yy,xx)],start = as.integer(input$range[1]),end = as.integer(input$range[2]))  
      updateSliderInput(session = session,inputId = "range",value = 1,min = as.integer(input$range[1]),max = as.integer(input$range[2]))
      values$zm = c(1,as.integer(input$range[1]),as.integer(input$range[2]))
    }else if(input$dozm == T && values$zm[1] == 1){
      df=ts(data =  df[seq(values$zm[2],values$zm[3]),c(yy,xx)],start = values$zm[2],end = values$zm[3])  
    }else if(input$dozm == F && values$zm[1] == 1){
      df=ts(data =  df[,c(yy,xx)])
      values$zm = c(1,as.integer(input$range[1]),as.integer(input$range[2]))
      updateSliderInput(session = session,inputId = "range",value = c(values$zm[2],values$zm[3]),min = 1,max = nrow(df))
      values$zm[1] = 0
    }
    
    
    plot(df,main=paste(time[as.integer(input$range[1])],time[as.integer(input$range[2])],sep=" - "))
    
    abline(v = as.integer(input$range[1]),col="green",lwd=3)
    abline(v = as.integer(input$range[2]),col="red",lwd=3)
    
  })
  
  #----------
  # PLOT2 SCATTER PLOT WITH BESTFIT
  
  output$plot2 <- renderPlot({
    
    if (is.null(values$df_data))
      return(NULL)
    
    if (input$xx == "_" || input $yy == "_")
      return(NULL)
    
    
    
    if (input$fsh == T) {
      
      
      df=values$df_data
      
      if (is.null(values$df_trend) == FALSE) {
        df[,-1]=df[,-1]-values$df_trend[,-1]
      }
      
      time=df[,1]
      
      header=names(df)
      xx=which(header == input$xx)
      yy=which(header == input$yy)
      mm=which(header == input$mm)
      lat=which(header == input$lat)
      long=which(header == input$long)
      
      df[,xx]=df[,xx]-input$oxx
      df[,yy]=df[,yy]-input$oyy      
      
      
      mmax=max(df[as.integer(input$range[1]):as.integer(input$range[2]),mm])
      
      df=df[as.integer(input$range[1]):as.integer(input$range[2]),]
      
      if (as.numeric(input$fxx)>6) {
        fxx=round(as.numeric(input$fxx)/2)*2-1 
        df[,xx]=sgolayfilt(x = df[,xx],n = fxx)
      }
      if (as.numeric(input$fyy)>6) {
        fyy=round(as.numeric(input$fyy)/2)*2-1
        df[,yy]=sgolayfilt(x = df[,yy],n = fyy)
      }
      if (as.numeric(input$fxx)>0 && as.numeric(input$fxx)<1) {
        fxx=abs(as.numeric(input$fxx)) 
        df[,xx]=expsmooth(x=df[,xx],alpha = fxx)$estimate
      }
      if (as.numeric(input$fyy)>0 && as.numeric(input$fyy)<1) {
        fyy=abs(as.numeric(input$fyy)) 
        df[,yy]=expsmooth(x=df[,yy],alpha = fyy)$estimate
      }
      
      
      if ((as.numeric(input$txx) + as.numeric(input$tyy)) == 0) {
        df.ff=rep(TRUE,length(df[,xx]))
      }else if (as.numeric(input$txx) == 0) {
        df.ff=df[,yy]<as.numeric(input$tyy)
      }else if (as.numeric(input$tyy) == 0) {
        df.ff=df[,xx]<as.numeric(input$txx)
      }else{
        df.ff=df[,xx]<as.numeric(input$txx) & df[,yy]<as.numeric(input$tyy)
      }
      
      # Cross correlation calc between x and y
      sh=ccf(df[df.ff,xx],df[df.ff,yy],lag.max = 20,plot = F)
      
      # Calc optimal shift
      sh=-sh$lag[which.max(sh$acf)]     
      
      
      updateSliderInput(session = session,inputId =  "sh",label =  "Shift:",
                        min = -20, max = 20,
                        value = sh,step = 1)
      
      
      
    }else{
      
      df=values$df_data
      
      if (is.null(values$df_trend) == FALSE) {
        df[,-1]=df[,-1]-values$df_trend[,-1]
      }
      
      sh=input$sh
      
      if ((input$range[1]-input$sh)<1) {
        sh=0
      } 
      if ((input$range[2]-input$sh)>nrow(df)) {
        sh=0
      } 
      
    }
    
    
    
    df=values$df_data
    
    if (is.null(values$df_trend) == FALSE) {
      df[,-1]=df[,-1]-values$df_trend[,-1]
    }
    
    time=df[,1]
    
    header=names(df)
    xx=which(header == input$xx)
    yy=which(header == input$yy)
    mm=which(header == input$mm)
    lat=which(header == input$lat)
    long=which(header == input$long)
    
    df[,xx]=df[,xx]-input$oxx
    df[,yy]=df[,yy]-input$oyy    
    
    
    df[as.integer(input$range[1]):as.integer(input$range[2]),xx]=df[(as.integer(input$range[1]):as.integer(input$range[2]))-sh,xx]
    
    mmax=max(df[as.integer(input$range[1]):as.integer(input$range[2]),mm])
    
    df=df[as.integer(input$range[1]):as.integer(input$range[2]),]
    
    df.ts=ts(df[,c(yy,xx)],as.integer(input$range[1]),as.integer(input$range[2]))
    
    par(mfrow=c(1,2))
    
    if (as.numeric(input$fxx)>0 && as.numeric(input$fxx)<=1) {
      fxx=abs(as.numeric(input$fxx)) 
      df[,xx]=expsmooth(x=df[,xx],alpha = fxx)$estimate
      if (as.numeric(input$fyy) == 0) {
        df[,yy]=expsmooth(x=df[,yy],alpha = 1)$estimate
      }
    }
    if (as.numeric(input$fyy)>0 && as.numeric(input$fyy)<=1) {
      fyy=abs(as.numeric(input$fyy)) 
      df[,yy]=expsmooth(x=df[,yy],alpha = fyy)$estimate
      if (as.numeric(input$fxx) == 0) {
        df[,xx]=expsmooth(x=df[,xx],alpha = 1)$estimate
      }
    }
    
    if (as.numeric(input$fxx)>6) {
      fxx=round(as.numeric(input$fxx)/2)*2-1 
      df[,xx]=sgolayfilt(x = df[,xx],n = fxx)
    }
    if (as.numeric(input$fyy)>6) {
      fyy=round(as.numeric(input$fyy)/2)*2-1
      df[,yy]=sgolayfilt(x = df[,yy],n = fyy)
    }

    par(mai=c(1.02, 0.82, 0.82, 1.2))
    
    
    plot(as.integer(input$range[1]):as.integer(input$range[2]),df[,xx],type="l",ylab =names(df)[xx],xlab="Time")
    lines(as.integer(input$range[1]):as.integer(input$range[2]),scale(df[,yy],min(df[,yy]),
                                                                      diff(range(df[,yy])))*diff(range(df[,xx]))+min(df[,xx]),col="blue")
    
    axis(side = 4,at = seq(min(df[,xx]),max(df[,xx]),length.out = 5),
         labels =format(seq(min(df[,yy]),max(df[,yy]),length.out =5),digits = 2),col.axis = "blue")
    mtext(text = names(df)[yy],side = 4,col="blue",line=3)
    
    if ((as.numeric(input$txx) + as.numeric(input$tyy)) == 0) {
      df.ff=rep(TRUE,length(df[,xx]))
    }else if (as.numeric(input$txx) == 0) {
      df.ff=df[,yy]<as.numeric(input$tyy)
    }else if (as.numeric(input$tyy) == 0) {
      df.ff=df[,xx]<as.numeric(input$txx)
    }else{
      df.ff=df[,xx]<as.numeric(input$txx) & df[,yy]<as.numeric(input$tyy)
    }
    
    
    par(mai=c(1.02, 0.82, 0.82, 0.42))

    if(input$boot[1] == 0 || values$doboot == input$boot[1]){
      values$doboot=input$boot[1]
    }
    if (values$doboot != input$boot[1]) {
      r.boot=boot(data=df[,c(yy,xx)],statistic =function(x,i){mean(x[i,1])/mean(x[i,2])},R = 1000 )
      r.boot=c(r.boot$t0,sd(r.boot$t))
      df.lm=data.frame(x=df[df.ff,xx],y=df[df.ff,yy])
      df.lm=lm(y~x,data=df.lm)
      lm.boot=summary(lm.boot(lm.object =df.lm ,R = 1000,rows = F))
      lm.boot=c(lm.boot$orig.lm$coefficients[2],lm.boot$stdev.params[2],lm.boot$orig.lm$coefficients[1],lm.boot$stdev.params[1])
      values$doboot=input$boot[1]
      mainres=paste("BOOTSTRAP ",header[yy]," = ",format(lm.boot[1],digits = 4),"(",format(lm.boot[2],digits = 4),") * ",
                    header[xx]," ",sprintf("%+.3g",lm.boot[3]),"(",format(lm.boot[4],digits = 4),")",
                    "    Area = ",format(r.boot[1],digits = 4),"(",format(r.boot[2],digits = 4),")",
                    sep = "")
      values$mainboot=mainres
      df.lm$coefficients=c(lm.boot[1],lm.boot[3])
      
      if (input$domap == F) {
        reslat = NA
        reslong = NA
      }else{
        reslat=mean(df[,lat],na.rm = T)
        reslong=mean(df[,long],na.rm=T)
        
      }
      
      
      values$results_row =data.frame(
        ID=as.integer(input$range[1]),
        ratio=paste(header[yy],header[xx],sep = "/"),
        start=paste(time[as.integer(input$range[1])]),
        end=paste(time[as.integer(input$range[2])]),
        area=r.boot[1],
        slope=lm.boot[1],
        intercept=lm.boot[3],
        cor=cor(df[df.ff,xx],df[df.ff,yy]),
        sd=paste(format(r.boot[2],digits = 3),format(lm.boot[2],digits = 3),format(lm.boot[4],digits = 3),sep = "|"),
        marker=header[mm],
        gasmax=mmax,
        shift=input$sh,
        filter=paste(input$fyy,input$fxx,sep="|"),
        tres=paste(input$tyy,input$txx,sep="|"),
        nsamples=diff(input$range),
        latitude=reslat,
        longitude=reslong
      )
      
      
    }
    
    
    df.cor=cor(df[df.ff,xx],df[df.ff,yy])
    
    if (!exists("mainres")) {
      
      
      df.lm=lm(df[df.ff,yy]~df[df.ff,xx] )
      
      if (isolate(is.null(values$mainboot))) {
        mainres=paste("   ",header[yy]," = ",format(df.lm$coefficients[2],digits = 4),"*",header[xx]," ",
                      sprintf("%+.3g",df.lm$coefficients[1]),
                      "   R =",format(df.cor,digits = 4),
                      "   Area =",format(mean(df[,yy],na.rm = T)/mean(df[,xx],na.rm = T),digits = 4))
        
        if (input$domap == F) {
          reslat = NA
          reslong = NA
        }else{
          reslat=mean(df[,lat],na.rm = T)
          reslong=mean(df[,long],na.rm=T)
          
        }
        
        
        values$results_row =data.frame(
          ID=as.integer(input$range[1]),
          ratio=paste(header[yy],header[xx],sep = "/"),
          start=paste(time[as.integer(input$range[1])]),
          end=paste(time[as.integer(input$range[2])]),
          area=mean(df[,yy],na.rm = T)/mean(df[,xx],na.rm = T),
          slope=df.lm$coefficients[2],
          intercept=df.lm$coefficients[1],
          cor=df.cor,
          sd=NA,
          marker=header[mm],
          gasmax=mmax,
          shift=input$sh,
          filter=paste(input$fyy,input$fxx,sep="|"),
          tres=paste(input$tyy,input$txx,sep="|"),
          nsamples=diff(input$range),
          latitude=reslat,
          longitude=reslong
        )
        
      }else{
        mainres=isolate(values$mainboot)
        isolate(values$mainboot <- NULL)
      }
      
      
      
    }
    
    if (input$ptp == F) {
      output$text1 = renderText({
        
        paste('<pre><font face = "Arial" size = "3" color = "blue"><b>',"# sample = ",length(df[,xx]),
              "     max ",header[mm]," = ",format(mmax,digits = 1),"   ",mainres,"</b></font></pre>",sep="")
      })
      
      plot(df[df.ff,c(xx,yy)])  
      abline(a = df.lm$coefficients[1],b = df.lm$coefficients[2],col="red",lwd=3,lty=2)
    }else{
      plot(df[df.ff,mm],df[df.ff,yy]/df[df.ff,xx],ylab =paste0(names(df)[yy],"/",names(df)[xx]),xlab =names(df)[mm])
      pp.ratio=lm(formula = (df[df.ff,yy]/df[df.ff,xx])~1/df[df.ff,mm],weights = df[df.ff,mm])$coefficients
      lines(sort(df[df.ff,mm]),1/sort(df[df.ff,mm])+pp.ratio,lty=2,col="red",lwd=3)
      output$text1 = renderText({
        
        paste('<pre><font face = "Arial" size = "3" color = "blue"><b>',"# sample = ",length(df[,xx]),
              "     max ",header[mm]," = ",format(mmax,digits = 1),"   ",mainres,"    Asym = ",format(pp.ratio,digits = 1),"</b></font></pre>",sep="")
      })
    }
    
    
    
  
  })
  
  
  #----------
  # PLOT3 LONGITUDE AND LATITUDE
  
  output$plot3 <- renderPlot({
    
    if (is.null(values$df_data))
      return(NULL)
    
    if (input$domap == FALSE)
      return(NULL)
    
    df=values$df_data
    
    if (is.null(values$df_trend) == FALSE) {
      df[,-1]=df[,-1]-values$df_trend[,-1]
    }
    
    header=names(df)
    lat=which(header == input$lat)
    long=which(header == input$long)
    mm=which(header == input$mm)
    
    
    
    
    
    # 6
    par(mar = c(5.1, 4.1,4.1, 10))	
    plot(df[,c(long,lat)],xlab = "Longitude",ylab="Latitude")
    df=df[as.integer(input$range[1]):as.integer(input$range[2]),]
    
    points(df[,c(long,lat)],col=color.scale(df[,mm]),
           cex=scale(df[,mm],min(df[,mm]),diff(range(df[,mm])))*3+0.2
           ,pch=16)
    image.plot(matrix(df[,mm]),legend.only = T,legend.line = 3,legend.lab = "Plume marker",legend.mar = 10)
    par(mar = c(5.1, 4.1,4.1, 2.1))	
    
  })
  
  output$mymap <- renderLeaflet(
    
    if (input$maptype == "osm") {
      leaflet() %>% addTiles() %>% 
        addCircleMarkers( lng=values$df_data[,which(names(values$df_data) == input$long)]*input$domap,
                          lat=values$df_data[,which(names(values$df_data)  == input$lat)]*input$domap,
                          color="red",radius=4) %>%
        addCircleMarkers( lng=values$df_data[as.integer(input$range[1]):as.integer(input$range[2]),which(names(values$df_data) == input$long)]*input$domap,
                          lat=values$df_data[as.integer(input$range[1]):as.integer(input$range[2]),which(names(values$df_data)  == input$lat)]*input$domap,
                          color="blue",radius=4)
    }else if(input$maptype == "esri"){
      leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%   
        addCircleMarkers( lng=values$df_data[,which(names(values$df_data) == input$long)]*input$domap,
                          lat=values$df_data[,which(names(values$df_data)  == input$lat)]*input$domap,
                          color="red",radius=4) %>%
        addCircleMarkers( lng=values$df_data[as.integer(input$range[1]):as.integer(input$range[2]),which(names(values$df_data) == input$long)]*input$domap,
                          lat=values$df_data[as.integer(input$range[1]):as.integer(input$range[2]),which(names(values$df_data)  == input$lat)]*input$domap,
                          color="blue",radius=4)
    }
    
  )
  
  
  observe({
    
    req(input$tab_being_displayed == "Map" & input$domap == T) # Only display if tab is 'Map Tab'
    leafletProxy("mymap") %>%  leaflet() 
    
    
  })
  
  
  #----------
  # PLOT4 GOOGLE MAPS
  
  output$plot4 <- renderPlot({
    
    if (is.null(values$df_data))
      return(NULL)
    
    if (input$domap == FALSE)
      return(NULL)
    
    df=values$df_data
    
    if (is.null(values$df_trend) == FALSE) {
      df[,-1]=df[,-1]-values$df_trend[,-1]
    }
    
    
    
    
    header=names(df)
    lat=which(header == input$lat)
    long=which(header == input$long)
    hh=which(header == input$height)
    mm=which(header == input$mm)
    
    cols=rep(0,length(df[,long]))
    
    cols[as.integer(input$range[1]):as.integer(input$range[2])]=2
    
    
    # par(pty="s")
    
    
    zmm=as.numeric(input$mzoom)/1000
    
    df.merc=projectMercator(lat=df[,lat],long = df[,long])
    points(df.merc[,1],df.merc[,2])
    
    df=df[as.integer(input$range[1]):as.integer(input$range[2]),]
    df.merc=projectMercator(lat=df[,lat],long = df[,long])
    points(df.merc,col=color.scale(df[,mm]),
           cex=scale(df[,mm],min(df[,mm]),diff(range(df[,mm])))*3+0.2
           ,pch=16)
    image.plot(matrix(df[,mm]),legend.only = T,legend.line = 3,legend.lab = "Plume marker",legend.mar = 10)

  })
  
  #----------
  # PLOT5 DISTANCE VS HEIGHT
  
  output$plot5 <- renderPlot({
    
    if (is.null(values$df_data))
      return(NULL)
    
    df=values$df_data
    
    if (is.null(values$df_trend) == FALSE) {
      df[,-1]=df[,-1]-values$df_trend[,-1]
    }
    
    header=names(df)
    lat=which(header == input$lat)
    long=which(header == input$long)
    hh=which(header == input$height)
    zz=which(header == input$mm)
    
    cols=rep(0,length(df[,long]))
    
    cols[as.integer(input$range[1]):as.integer(input$range[2])]=2
    
    if (input$coordICA == "lonlat"){
      
      dfdist=df[as.integer(input$range[1]):as.integer(input$range[2]),c(long,lat)]
      dfdist=distVincentyEllipsoid(dfdist[1,],dfdist)
    }else if (input$coordICA == "UTM") {
      dfdist=df[as.integer(input$range[1]):as.integer(input$range[2]),c(long,lat)]
      dfdist=as.vector(as.matrix(dist(rbind(dfdist[1,],dfdist)))[-1,1])
    }else if (input$coordICA == "dist") {
      dfdist=df[as.integer(input$range[1]):as.integer(input$range[2]),long]
      
    }
    
    
    dff=data.frame(x=dfdist,y=df[as.integer(input$range[1]):as.integer(input$range[2]),hh],
                   z=df[as.integer(input$range[1]):as.integer(input$range[2]),zz])
    dff=rbind(dff,data.frame(x=rep(dff$x+runif(dim(dff)[1],-.1,.1),2),y=c(rep(input$lowzero,dim(dff)[1]),rep(input$upzero,dim(dff)[1])),
                             z=rep(0,dim(dff)[1]*2)))
    
    par(mar = c(5.1, 4.1,4.1, 10))	
    plot(dff$x,dff$y,col=color.scale(dff[,3]),
         cex=scale(dff[,3],min(dff[,3]),diff(range(dff[,3])))*3+0.2
         ,pch=16,xlab = "Distance",ylab="Height")
    image.plot(matrix(dff[,3]),legend.only = T,legend.line = 4,legend.lab = "",legend.mar = 10)
    par(mar = c(5.1, 4.1,4.1, 2.1))	
    
  })
  
  #----------
  # PLOT6 ICA INTERPOLATION
  
  output$plot6 <- renderPlot({
    
    if (is.null(values$df_data))
      return(NULL)
    
    if (input$domap == FALSE)
      return(NULL)
    
    
    df=values$df_data
    
    if (is.null(values$df_trend) == FALSE) {
      df[,-1]=df[,-1]-values$df_trend[,-1]
    }
    
    header=names(df)
    lat=which(header == input$lat)
    long=which(header == input$long)
    hh=which(header == input$height)
    zz=which(header == input$mm)
    
    nn=as.numeric(input$nn)
    pp=as.numeric(input$pp)
    
    cols=rep(0,length(df[,long]))
    
    cols[as.integer(input$range[1]):as.integer(input$range[2])]=2
    

    if (input$coordICA == "lonlat"){
      
      dfdist=df[as.integer(input$range[1]):as.integer(input$range[2]),c(long,lat)]
      dfdist=distVincentyEllipsoid(dfdist[1,],dfdist)
    }else if (input$coordICA == "UTM") {
      dfdist=df[as.integer(input$range[1]):as.integer(input$range[2]),c(long,lat)]
      dfdist=as.vector(as.matrix(dist(rbind(dfdist[1,],dfdist)))[-1,1])
    }else if (input$coordICA == "dist") {
      dfdist=df[as.integer(input$range[1]):as.integer(input$range[2]),long]
      
    }
    
    
    
    
    dff=data.frame(x=dfdist,y=df[as.integer(input$range[1]):as.integer(input$range[2]),hh],
                   z=df[as.integer(input$range[1]):as.integer(input$range[2]),zz])
    dff=rbind(dff,data.frame(x=rep(dff$x+runif(dim(dff)[1],-.1,.1),2),y=c(rep(input$lowzero,dim(dff)[1]),rep(input$upzero,dim(dff)[1])),
                             z=rep(0,dim(dff)[1]*2)))      
    
    if (input$doICA == "spline") {
      
      
      df.mba=mba.surf(xyz =  dff,no.X =  nn,no.Y =  nn)
      image.plot(df.mba$xyz.est$x,df.mba$xyz.est$y,df.mba$xyz.est$z,xlab = "Distance",ylab="Height",legend.line = 3,legend.lab = "Plume marker",legend.mar = 10)
      ica=sum(mean(diff(df.mba$xyz.est$x),na.rm=T)*mean(diff(df.mba$xyz.est$y),na.rm=T)*df.mba$xyz.est$z,na.rm = T)
      
      legend(x =  "topleft",legend = paste("ICA [ppm*m^2] =",format(ica,digits = 4)))
      
    }else if(input$doICA == "gauss"){
      
      
      coefs=as.numeric(read.table(text=input$nn,sep=","))
      coefs[4]=1/coefs[4]
      coefs[5]=1/coefs[5]
      AA=coefs[1]
      ica.nls=nls(z ~ coefs[1]*exp(-( (b*(x-x0)^2)+ (c*(y-y0)^2))),data=dff ,
                  start=list(x0=coefs[2],y0=coefs[3],b=coefs[4],c=coefs[5]),control = nls.control(maxiter = 1000,minFactor = 1E-4),algorithm = "port")
      coefs=summary(ica.nls)$coefficients[,1]
      coefs=c(AA,coefs)
      xx=rep(seq(min(dff$x),max(dff$x),length.out = 100 ),100)
      yy=rep(seq(min(dff$y),max(dff$y),length.out = 100 ),each=100)
      zz=coefs[1]*exp(-( (coefs[4]*(xx-coefs[2])^2)+ (coefs[5]*(yy-coefs[3])^2)))
      quilt.plot(xx,yy,zz,xlab = "Distance",ylab="Height")
      
      
      ica=sum(mean(diff(xx),na.rm=T)*mean((yy),na.rm=T)*zz,na.rm = T)
      legend(x =  "topleft",legend = paste("ICA [ppm*m^2] =",format(ica,digits = 4),
                                           "  x0 =",format(coefs[2],digits = 4),"  x0 =",format(coefs[3],digits = 4),
                                           "  b =",format(coefs[4],digits = 4),"  c =",format(coefs[5],digits = 4)
      ))
    }
  })
  
  
  #---------------------------------
  # DOWNLOAD SUMMARY
  
  output$downloadSummary <- downloadHandler(
    
    filename = function() {
      "results.csv"
    },
    content = function(file) {
      results_data=load(file =  paste("results_data",".RData",sep = ""))
      results_data=get(results_data)
      write.csv(x =  results_data, file, row.names = FALSE)
    }
  )
  
  #---------------------------------
  # DOWNLOAD SUMMARY BATCH
  
  output$downloadSummBatch <- downloadHandler(
    
    filename = function() {
      "results.csv"
    },
    content = function(file) {
      batch_data=load(file =  paste("batch_data",".RData",sep = ""))
      batch_data=get(batch_data)
      write.csv(x =  batch_data, file, row.names = FALSE)
    }
  )
  
  #---------------------------------
  # DISPLAY MAXIMUM CALCULABLE RATIOS
  
  output$text2 <-  renderText({
    
    if (is.null(values$df_data))
      return(NULL)
    
    autoInvalidate()
    
    df=values$df_data
    
    if (is.null(values$df_trend) == FALSE) {
      df[,-1]=df[,-1]-values$df_trend[,-1]
    }
    
    maxratios=nrow(df)/input$movstep
    
    if(file.exists(paste("batch_data",".RData",sep = ""))){
      results_batch=load(paste("batch_data",".RData",sep = ""))
      results_batch=get(results_batch)
      
      if(is.null(results_batch)){results_batch=data.frame(x="")}
      
      if (names(results_batch)[1]== "folder" ) {

        paste('<pre><font face = "Arial" size = "3" color = "blue"><b>'
              , paste0("Batch analysis started at ",results_batch$start.time),"</b></font></pre>",sep="")
        
        
      }else{
        
        if (maxratios > 10000) {

          paste('<pre><font face = "Arial" size = "3" color = "blue"><b>'
                , maxratios,"  (the number of ratios exceed 10,000!)","</b></font></pre>",sep="")
        }else{
          paste('<pre><font face = "Arial" size = "3" color = "blue"><b>'
                , maxratios,"  ratios expected","</b></font></pre>",sep="")
        }
        
      }
    }else{
      if (maxratios > 10000) {
        #try(print(paste(maxratios,"(the number of ratios exceed 10,000!)"),quote=F),T)
        paste('<pre><font face = "Arial" size = "3" color = "blue"><b>'
              , maxratios,"  (the number of ratios exceed 10,000!)","</b></font></pre>",sep="")
      }else{
        paste('<pre><font face = "Arial" size = "3" color = "blue"><b>'
              , maxratios,"  ratios expected","</b></font></pre>",sep="") 
      }
    }
    
    
    
    
  })
  
  #----------
  # ABORT BATCH ANALYSIS

  observeEvent(input$abortbatch,{
    file.remove(paste0("batch_data",".RData"))
  })
  
  #----------
  # DO BATCH ANALYSIS

  observeEvent(input$batch,{
    
    if (is.null(values$df_data))
      return(NULL)
    
    df=values$df_data
    
    if (is.null(values$df_trend) == FALSE) {
      df[,-1]=df[,-1]-values$df_trend[,-1]
    }
    
    
    maxratios=nrow(df)/input$movstep
    
    if (maxratios > 10000) {
      return(NULL)
    }
    
    time=df[,1]
    
    header=names(df)
    xx=which(header == input$xx)
    yy=which(header == input$yy)
    mm=which(header == input$mm)
    
    if (input$domap == T) {
      lat=which(header == input$lat)
      long=which(header == input$long)      
    } else{
      lat=NA
      long=NA
    }
    
    
    if ((as.numeric(input$txx) + as.numeric(input$tyy)) == 0) {
      df.ff=rep(TRUE,length(df[,xx]))
    }else if (as.numeric(input$txx) == 0) {
      df.ff=df[,yy]<as.numeric(input$tyy)
    }else if (as.numeric(input$tyy) == 0) {
      df.ff=df[,xx]<as.numeric(input$txx)
    }else{
      df.ff=df[,xx]<as.numeric(input$txx) & df[,yy]<as.numeric(input$tyy)
    }
    
    df=df[df.ff,]
    
    cat("Start batch analysis")
    
    
    batch.gaslim=input$gaslim
    batch.winstep=input$winstep
    batch.movstep=input$movstep
    batch.rlimit=input$rlimit
    batch.maxshift=input$maxshift
    batch.peak=input$peak
    
    if (input$batchtype == "multiple") {
      batch.folder = paste("unzipped/",sep="")
    }else{
      batch.folder = df
    }
    
    batch.batchtype=input$batchtype
    
    batch.results=NULL
    
    batch.script=paste("Rscript --vanilla Ratiocalc_batch.R batch_data",".RData",sep="")
    
    batch.data=list(folder = batch.folder,
                    sep = input$sep ,
                    num.col = yy,den.col = xx,marker.col = mm,
                    coord.col=c(long,lat),
                    marker.lim = input$gaslim,
                    max.shift=input$maxshift,
                    cor.lim =input$rlimit,
                    win.width =input$winsize,
                    mov.step=input$movstep,
                    peak.dist=input$peak,
                    start.time=Sys.time())
    
    save(batch.data,file = paste("batch_data",".RData",sep = ""))
    
    
    system(batch.script, wait=FALSE)
    
    
  })
  
  #---------------------------------
  # DOWNLOAD BATCH
  
  output$downloadBatch <- downloadHandler(
    
    filename = function() {
      paste0(format(Sys.time(),"%Y%m%d%H%M"),"_batch_results.csv")
    },
    content = function(file) {
      results_batch=load(paste("batch_data",".RData",sep = ""))
      results_batch=get(results_batch)
      write.csv(x =  results_batch, file, row.names = FALSE)
    }
  )
  

  #----------
  # PLOT7 BATCH RESULT
  
  output$plot7 <- renderPlot({
    
    input$nextcsv;input$prevcsv;input$ncsv;input$ratio;input$clear;input$rmratio
    input$batch
    
    autoInvalidate()
    

    if(file.exists(paste("batch_data",".RData",sep = ""))){
      results_batch=load(paste("batch_data",".RData",sep = ""))
      results_batch=get(results_batch)
      
      if (is.null(results_batch)) {
        return(NULL)
      }
      
      if(names(results_batch)[4] == "cor" ){
        
        
        par(mfrow=c(1,2))
        
        par(mai=c(1.02, 0.82, 0.82, 1.2))
        
        if (min(diff(results_batch[,2])) <0 ) {
          IDdiff=diff(results_batch[,2])
          IDdiff[IDdiff<0]=0
          IDdiff=c(1,cumsum(IDdiff ))
        }else{
          IDdiff=results_batch[,2]
        }
        
        plot(IDdiff,results_batch[,3],xlab="Sample",ylab="Ratio")  
        
        par(mai=c(1.02, 0.82, 0.82, 0.42))
        
        hist(results_batch[,3],breaks = "fd",xlab="Ratio",main="")
        abline(v=median(results_batch[,3],na.rm = T),lty=2,col="red")
        
      }else{
        return(NULL)
      }
    }else{
      return(NULL)
    }
    
    
  })
  
  
  #---------------------------------
  # SHOW BATCH SUMMARY
  
  
  output$batchtable <- renderDT( {
    
    input$refresh
    input$ratio
    input$rmratio
    input$clear
    input$nextcsv;input$prevcsv;input$ncsv;input$ratio;input$clear;input$rmratio;input$clearSummBatch;input$rmratioBatch
    
    #autoInvalidate()
    
    if(file.exists(paste("batch_data",".RData",sep = ""))){
      results_batch=load(paste("batch_data",".RData",sep = ""))
      results_batch=get(results_batch)
      if(names(results_batch)[5] == "median" ){
        return(results_batch)
      }else{
        return(NULL)
      }
    }else{
      return(NULL)
    }
    
    
    
    
  }, rownames = F)    
  
  #---------------------------------
  # WATER CALCULATION
  
  
  observeEvent(input$watercalc,{
    
    if (is.null(values$df_data))
      return(NULL)
    
    df=values$df_data
    
    
    time=df[,1]
    
    header=names(df)
    
    xx=which(header == input$xx)
    yy=which(header == input$yy)
    mm=which(header == input$mm)
    lat=which(header == input$lat)
    long=which(header == input$long)
    
    Rh=which(header == input$waterRh)
    tt=which(header == input$waterT)
    
    tt=df[,tt]
    Rh=df[,Rh]
    
    cat("Water calculation")
    
    df$H2Ocalc=(6.1121*(1.0007+3.46*(input$waterP^(-6)))*exp( (17.502*tt)/(240.97+tt)) *Rh/100*1E6 )/input$waterP
    
    
    values$df_data <- df
    
    
    updateSelectInput(session= session,inputId ="yy",label = "Numerator:",choices = names(df)[-1],selected =names(df)[yy] )
    updateSelectInput(session= session,inputId ="xx",label = "Denominator:",choices = names(df)[-1],selected =names(df)[xx] )
    updateSelectInput(session= session,inputId ="lat",label = "Latitude:",choices = names(df)[-1],selected =names(df)[lat])
    updateSelectInput(session= session,inputId ="long",label = "Longitude:",choices = names(df)[-1],selected =names(df)[long] )
    updateSelectInput(session= session,inputId ="mm",label = "Plume marker:",choices = names(df)[-1],selected =names(df)[mm] )
    updateSelectInput(session = session,inputId = "height",label = "Height:",choices = names(df)[-1],selected = names(df)[mm])
    updateSelectInput(session = session,inputId = "waterRh",label = "Select Rh:",choices = names(df)[-1],selected =input$waterRh)
    updateSelectInput(session = session,inputId = "waterT",label = "Select Temp:",choices =names(df)[-1],selected =input$waterT)
    
    if(is.null(values$df_trend) == F){
      
      df=values$df_trend
      df$H2Ocalc=rep(0,nrow(df))
      values$df_trend=df
      
      df=values$df_trendpp
      df$H2Ocalc=rep(NA,nrow(df))
      values$df_trendpp=df

    }

  })
  
  #----------
  # PLOT8 WATER CALC RESULT
  
  output$plot8 <- renderPlot({
    
    if (is.null(values$df_data))
      return(NULL)
    
    df=values$df_data
    
    if (is.null(values$df_trend) == FALSE) {
      df[,-1]=df[,-1]-values$df_trend[,-1]
    }
    
    time=df[,1]
    
    header=names(df)
    
    if (sum(header == "H2Ocalc") != 1)
      return(NULL)
    
    
    plot(df$H2Ocalc,type="l",xlab="Time",ylab="Calculated water")  
    
    
  })
  
  #----------
  # PLOT9 DETREND
  
  output$plot9 <- renderPlot({
    
    if (is.null(values$df_data))
      return(NULL)
    
    if (input$xx == "_" || input $yy == "_")
      return(NULL)
    
    df=values$df_data
    
    
    time=df[,1]
    
    header=names(df)
    xx=which(header == input$xx)
    yy=which(header == input$yy)
    
    if (input$detrnd == "x") {
      tr=xx
    } else{
      tr=yy
    }
    
    if (input$fauto == 2) {
      
      if(is.null(values$df_trendpp) == TRUE) {
        dftrpp=cbind(1:nrow(df),df[,-1]*NA)
        values$df_trendpp <- dftrpp
        values$df_trend <- cbind(1:nrow(df),df[,-1]*0)
        
      }
      

      plot(df[,tr],type="l",xlab="Time",ylab=header[tr])
      
      
      
    }
    
    
    if (input$fauto == 1) {
      
      if(is.null(values$df_trendpp) == TRUE) {
        dftrpp=cbind(1:nrow(df),df[,-1]*NA)
        values$df_trendpp=dftrpp
        values$df_trend=cbind(1:nrow(df),df[,-1]*0)
        
      }
      
      plot(df[,tr],type="l",xlab="Time",ylab=header[tr])
      ff=sgolayfilt(x =df[,tr] ,n =floor((nrow(df)-10)/2)*2+1  )
      
      autotr=df[,tr]-((df[,tr]-ff)-min(df[,tr]-ff,na.rm = T))
      
      lines(autotr,col="red")
      
      dftr=values$df_trend
      dftr[,tr]=autotr
      values$df_trend=dftr
      
    }
    
    
    
    if(is.null(values$df_trendpp) == TRUE && is.null(input$plot9click$x) == TRUE && input$fauto == 0){
      plot(df[,tr],type="l",main=paste(time[as.integer(input$range[1])],time[as.integer(input$range[2])],sep=" - "),xlab="Time",ylab=header[tr])
      
    }else if(is.null(values$df_trendpp) == TRUE && is.null(input$plot9click$x) == FALSE  && input$fauto == 0) {
      dftrpp=cbind(1:nrow(df),df[,-1]*NA)
      values$df_trendpp=dftrpp
      values$df_trend=cbind(1:nrow(df),df[,-1]*0)
      
    }else if(is.null(values$df_trendpp) == FALSE && is.null(input$plot9click$x) == FALSE  && input$fauto == 0){
      dftrpp=values$df_trendpp
      dftrpp[round(input$plot9click$x),tr]=input$plot9click$y
      values$df_trendpp=dftrpp
      plot(df[,tr],type="l",main=paste(time[as.integer(input$range[1])],time[as.integer(input$range[2])],sep=" - "),xlab="Time",ylab=header[tr])
      points(dftrpp[,1],dftrpp[,tr],col="red")
      
    }else if(is.null(values$df_trendpp) == FALSE && is.null(input$plot9click$x) == TRUE && input$fauto == 0){
      dftrpp=values$df_trendpp
      plot(df[,tr],type="l",main=paste(time[as.integer(input$range[1])],time[as.integer(input$range[2])],sep=" - "),xlab="Time",ylab=header[tr])
      points(dftrpp[,1],dftrpp[,tr],col="red")
      
      if (sum(!is.na(dftrpp[,tr])) >= 4) {
        spl=polySpline(interpSpline(dftrpp[,1],dftrpp[,tr], bSpline = TRUE,na.action =na.omit))
        lines(predict(spl,dftrpp[,1])$x,predict(spl,dftrpp[,1])$y,col="green")
        dftr=values$df_trend
        dftr[,tr]=predict(spl,dftrpp[,1])$y
        values$df_trend <- dftr
      }else if(sum(!is.na(dftrpp[,tr])) == 1){
        lines(dftrpp[,1],rep(sum(dftrpp[,tr],na.rm = T),length(dftrpp[,1])),col="green")
        dftr=values$df_trend
        dftr[,tr]=rep(sum(dftrpp[,tr],na.rm = T),length(dftrpp[,1]))
        values$df_trend <- dftr
        
      }
      
    }
    
    
  })
  
  #----------
  # REMOVE DETREND
  
  observeEvent(input$detrcanc,{
    
    if (is.null(values$df_data))
      return(NULL)
    
    if (input$xx == "_" || input $yy == "_")
      return(NULL)
    
    df=values$df_data
    
    
    time=df[,1]
    
    header=names(df)
    xx=which(header == input$xx)
    yy=which(header == input$yy)
    
    if (input$detrnd == "x") {
      tr=xx
    } else{
      tr=yy
    }
    
    
    dftr=values$df_trend
    dftr[,tr]=rep(0,nrow(dftr))
    values$df_trend=dftr
    
    dftrpp=values$df_trendpp
    dftrpp[,tr]=rep(NA,nrow(dftrpp))
    values$df_trendpp=dftrpp
    
    
  })
  
  #----------
  # PLOT RESPONSE
  
  output$plot10 <- renderPlot({
    
    if (is.null(values$df_data))
      return(NULL)
    
    if (input$xx == "_" || input $yy == "_")
      return(NULL)
    
    
    
    df=values$df_data
    
    
    header=names(df)
    xx=which(header == input$xx)
    yy=which(header == input$yy)
    
    if (input$respnd == "x") {
      tr=xx
    } else{
      tr=yy
    }
    
    
    df=df[as.integer(input$range[1]):as.integer(input$range[2]),]
    
    df[,tr]=scale(df[,tr],min(df[,tr],na.rm = T),diff(range(df[,tr],na.rm = T)))
    df[,tr]=log(df[,tr])
    
    tt=seq(from=1,by=input$respdt,length.out = nrow(df))
    plot(tt,df[,tr])
    
    try(    points(values$respdf,col="red",pch=16),silent = T)
    
    if (!is.null(values$respdf) && nrow(values$respdf)==4) {
      
      rr=values$respdf
      
      rr[1,1]=which(tt == rr[1,1])
      rr[2,1]=which(tt == rr[2,1])
      rr[3,1]=which(tt == rr[3,1])
      rr[4,1]=which(tt == rr[4,1])
      
      df1=data.frame(x=tt[rr[1,1]:rr[2,1]],y=df[rr[1,1]:rr[2,1],tr])
      df2=data.frame(x=tt[rr[3,1]:rr[4,1]],y=df[rr[3,1]:rr[4,1],tr])
      
      
      df1.lm=(lm(y ~ x, data = df1))
      df2.lm=(lm(y ~ x, data = df2))
      try(abline(a =  df1.lm$coefficients[1],b = df1.lm$coefficients[2],col="red"),silent = T)
      try(abline(a =  df2.lm$coefficients[1],b = df2.lm$coefficients[2],col="green"),silent = T)
      
      output$respcoeff <- renderPrint({
        print(paste("Fit1 = ",df1.lm$coefficients[2]),quote=F);
        print(paste("Fit2 = ",df2.lm$coefficients[2]),quote=F);
        print(paste("Tau1 =",-1/df1.lm$coefficients[2]),quote=F);
        print(paste("Tau2 =",-1/df2.lm$coefficients[2]),quote=F);
        print(paste("a1 =",1-exp(df1.lm$coefficients[2]),"  b1 =",exp(df1.lm$coefficients[2])),quote=F);
        print(paste("a2 =",1-exp(df2.lm$coefficients[2]),"  b2 =",exp(df2.lm$coefficients[2])),quote=F)
      })
    }
    
    
    if (is.null(input$plot10click$x)){
      values$respok=NULL
      return(NULL)
    }else{
      if (is.null(values$respok)){
        values$respdf=rbind(values$respdf,data.frame(x=tt[which.min(abs(tt - as.integer(input$plot10click$x)))],y=df[which.min(abs(tt - as.integer(input$plot10click$x))),tr]))
        values$respok=1
      }
    }
    
    if (!is.null(values$respdf) && nrow(values$respdf)>4)
      values$respdf=NULL
    
    
    
  })
}

#--------------------------------------------------------
# Create Shiny app



runApp(
  list(ui = ui, server = server),
  host = getOption("shiny.host", "0.0.0.0"),
  quiet = F,
  launch.browser =T
)
