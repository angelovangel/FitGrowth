 ## app.R ##
 # shiny dashboard version
 # v02-drc, uses memoise, plotting is now the slowest part
library(shiny)
library(shinydashboard)
library(tidyverse)
#library(ggpubr)
library(data.table)
#library(modelr)
#library(broom)
library(DT)
library(drc)
library(memoise)
library(lattice)

source("R/do_drm.R")


#
 #### ui ####

  header <- dashboardHeader(title = "FitGrowth-v02-drc")
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Load data", tabName = "data", icon = icon("database")),
      menuItem("Model", tabName = "model", icon = icon("rocket")),
      
      menuItem("Summary table", tabName = "summaryTable", icon = icon("table")),
      fluidRow(valueBoxOutput("numSamplesBox", width = 12), (valueBoxOutput("successSamplesBox", width = 12))),
      hr(),
      
      menuItem("Help", tabName = "help", icon = icon("user-o"))
        )
      ) #end of sidebar
    
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "data",
        fluidRow(
          box(width = 12, h5("Load the data as a text file, the first column must be named time, all other columns are treated as samples.")),
          box(width = 3,
            title = "Read file", status = "primary",
            fileInput('file1', 'Choose file'),
            tags$hr(),
            
            radioButtons("timeUnits", "Select time unit", 
                         c("Hours" = "h", "Minutes" = "min", "Seconds" = "sec"), selected = "h")
            ),
          box(width = 9, status = "primary", DT::dataTableOutput("data"))
          )
        ),
      tabItem(tabName = "model",
              #h2("Model plot"),
              fluidRow(
                box(width = 12, 
                  h5("Plots of original data, the points used in the model are in blue, model fit is a red line. Change time slider to re-calculate."),
                    column(6, selectizeInput("selectedSamples", 
                                             "Select which samples to analyse", choices = "", multiple = TRUE))
                    ),
                
                box(width = 12, title = "Model plot", status = "primary",
                    
                    plotOutput("model", height = "500px")),
                column(4, sliderInput("trim", label = h5("Trim time"), min=0, max=150, value=c(0,40))),
                column(4, sliderInput("pointsalpha", label = h5("Adjust point opacity"), min = 0, max = 1, value = 0.3, step = 0.1)),
                #column(1, checkboxInput("confidence", label = "Show confidence interval", value = FALSE)),
                column(1, checkboxInput("doublingtime", label = "Show doubling time", value = FALSE)),
                column(1, downloadButton("downloadPlot", label = "Download plot (pdf)"))
              )
            ),
      
      tabItem(tabName = "summaryTable",
              fluidRow(
                
                box(width = 12, title= "Growth rate constant for the selected samples",status = "primary", 
                    DT::dataTableOutput("summaryTable"))
              )),
      tabItem(tabName = "help", 
              box(width = 12, title = "Usage", status = "primary", 
                  htmlOutput("usage")),
              box(width = 12, title = "About", status = "warning", 
                  htmlOutput("about"))
              
          ))
              
        ) #end of dashboard body
ui <- dashboardPage(header, sidebar, body)

 #### server ####
server <- function(input, output, session) {
  
  session$onSessionEnded(function() {stopApp()})
    
  df <- reactive({
  
    inFile <- input$file1
     # if(is.null(inFile))
     # {return(NULL)}
    
    validate(
      need(expr = !is.null(input$file1), "Please select file first")
    )
    fread(inFile$datapath, header = TRUE)
  })

 # selectize initialisation and updates
  observe({  
  
    sampleslist <- colnames(df()) 
    updateSelectizeInput(session, "selectedSamples", 
                         choices = sampleslist[2:length(sampleslist)],       #excluding time, which is the first element
                         selected = sampleslist[2:3],                         
                         server = TRUE) 
    updateSliderInput(session, "trim", 
                      max = max(df()$time, na.rm = TRUE), 
                      min = min(df()$time, na.rm = TRUE),
                      value = c(min(df()$time), max(df()$time)))
    
  })
    
 #******************************************    
 ## 
    
    dflong <- function() {
      validate(need
               (input$selectedSamples, "Select samples first.. ")   
      )
      df() %>% 
        dplyr::select(time, one_of(input$selectedSamples)) %>% # here actual filtering on selectedSamples , notice one_of!       
        rename(t = time) %>%
        gather(sample, n, -t) %>%
        arrange(sample) # make sure the samples are in alphabeticasl order
      
    }
    
    
    # try memoise? to avoid fitting repeatedly
    # does not work
    # then try dflong as argument? works!
    df1 <- memoise(function(x, trim1 = input$trim[1], trim2 = input$trim[2]){
     x %>%
        filter(between(t, trim1, trim2)) %>%
        do_drm(t, n, sample)
          # get statistics with lapply on the drm output, e.g lapply(drmout$models, summary)
    })

    
    
  # observe({print(df1(dflong()))})
    
  # test lattice for faster plotting
    mplotlattice <- function() {
      # prepare a suitable dataframe, used the instructions from:
      # http://lattice.r-forge.r-project.org/Vignettes/src/lattice-tricks/regression-lines.pdf
      # on page 14
      predicted <- df1(dflong()) %>% unnest(pred) %>% rename(t = dose, n = pred) %>% dplyr::select(sample, t, n)
      datatrimmed <- df1(dflong()) %>% unnest(data) %>% dplyr::select(sample, t, n)
      datafull <- dflong()
      latticedf <- make.groups(full = datafull, trimmed = datatrimmed, predicted = predicted)
      # order elements of the vector alphabetically by name, because as.table = TRUE and only so can the
      # correct dt be printed on each plot
      dtvector <- purrr::as_vector(round(dtt()["dt"], digits = 3))
      print(dtvector)
      # 
      xmax <- max(datafull$t, na.rm = TRUE); print(xmax)
      ymin <- min(datafull$n, na.rm = TRUE); print(ymin)
      
      latticeplot <- xyplot(n ~ t | sample, 
             data = latticedf, 
             groups = which,
             distribute.type = TRUE, # how does this work, see  help(panel.superpose)
             type = c("p", "p", "l", "g"), # grid, point, point, line
             col = c("grey", "steelblue", "red"),
             par.settings = list(strip.background=list(col="lightgrey")),
             as.table = TRUE, 
             pch = 16, 
             alpha = c(input$pointsalpha, input$pointsalpha, 0.7), 
             aspect = 1, 
             xlab = paste0("time", " [",input$timeUnits, "]"), 
             ylab = "OD",
             main = "Growth curve(s), model fit and doubling times",
             sub = paste0("Data from ", 
                          input$trim[1], " to ", 
                          input$trim[2], " ",input$timeUnits," was used in fitting the model")
             
             )
      # trellis are S3 objects and can be updated:
      if(input$doublingtime) {
        return(update(latticeplot, panel = function(x, y, ...) { # here comes the panel.text to put doubling times
                                            panel.xyplot(x, y, ...)
                                            panel.text(xmax, ymin,
                                                       #labels = panel.number(),
                                                       labels = paste(dtvector[panel.number()], input$timeUnits, sep = " "),
                                                      adj = c(1, -1),
                                                      cex = 0.7,
                                                      alpha = 0.8) # use dtvector[panel.number()] to put the respective dt to panel
          
                                            }
                      )
               )
      
        }
      return(latticeplot)
    
    }
    
#### up to here drm ok

#** outputs
  
      
    sampledf <- reactive({
      df() %>% 
      gather(sample, od, -1) %>% 
      group_by(sample) %>% 
      summarise(NAs = sum(is.na(od)), measurements = n() - NAs)
  })
    
    output$data <- DT::renderDataTable({
      datatable(sampledf(), 
        rownames = FALSE, 
        options = list(scrollX = TRUE, 
        dom = "rltip", 
        columnDefs = list(list(className = 'dt-left', targets = 0))
                      )
                ) %>%
      formatRound(columns = c(2,3), digits = 0) %>%
      formatStyle(1, fontWeight = "bold") %>%
      formatStyle(1, backgroundColor = "steelblue", color = "white")
                                       })
    
   
     output$model <- renderPlot({
      mplotlattice()
    }, res = 120)
     
     output$downloadPlot <- downloadHandler(
       filename = function() {"plot.pdf"},
       content = function(file) {
         pdf(file)
         print(mplotlattice())
         dev.off()
       })
    
    # use the confints column from do_drm, contains slope parameter with CIs (2.5 % and 97.5 %)
     
     dtt <- reactive({
       df1(dflong()) %>% 
       unnest(confints) %>% 
       dplyr::mutate(Slope = abs(slope),
                     lower = abs(lowerCI),
                     upper = abs(upperCI),
                     dt = log(2)/Slope,
                     dtlower = log(2)/lower,
                     dtupper = log(2)/upper)
     })
    
    output$summaryTable <- DT::renderDataTable({
      dtt() %>%
             
              dplyr::select("Sample" = sample, 
                            "Growth rate constant" = Slope, 
                            "Growth rate 2.5 % CI" = lower,
                            "Growth rate 97.2 % CI" = upper,
                            "Doubling time" = dt,
                            "Doubling time 2.5 % CI" = dtlower,
                            "Doubling time 97.5 % CI" = dtupper) %>%
              datatable( 
                caption = paste0("L4 parameters, the time range used in the model is between ", input$trim[1], " and ", input$trim[2], " ", input$timeUnits),
                rownames = FALSE, 
                extensions = 'Buttons', 
                options = list(dom = 'Brltip', 
                               buttons = c("copy", "csv", "print"))
                ) %>%
    formatRound(2:7, 3) %>%
    formatStyle(1, fontWeight = "bold") %>%
    formatStyle(1, backgroundColor = "steelblue", color = "white")
        })
   
     output$numSamplesBox <- renderValueBox({
      color <- "green"
      if (ncol(df()) < 2) color <- "red"
      valueBox(value = ncol(df())-1, "samples read", color = color)
    })
    
    
     output$successSamplesBox <- renderValueBox({
      
      ifelse (ncol(df()) >= 2, successSample <- nrow(df1(dflong())), successSample <- 0)
      ifelse(successSample == 0, color2 <- "red", #yes
                                  ifelse(successSample < ncol(df())-1, color2 <- "yellow", color2 <- "green")
        )
      
      valueBox(value = successSample, "samples fitted", color=color2) 
    })

     
     
     
     
     
    
  
  
  
  output$usage <- renderUI({
    HTML(paste("<p>This app fits a logistic model to the growth data. 
    The best parameters are found using the <code>drc</code> library in <code>R</code>. More specifically, 
    the four-parameter logistic function is used (<code>L.4</code> in <code>drc</code>). 
    The app handles one or many samples (tested with 96), as well as
    <code>NA</code> values. You can get an example file <a href=https://www.dropbox.com/sh/zzf7y3ijwkat55e/AABUvp7BAARIdYBqZWgk1E37a?dl=0>here</a>.</p> Instructions: 
    load the data as a text file, the first column <u>must</u> be named <b>time</b>, all other columns are treated as
    samples. After uploading the file, go to the <b>Model</b> tab to select which samples to analyse.
    Note that the parameters of the model are re-calculated when the time interval is changed with the slider."))
        })
  output$about <- renderUI({
    HTML(paste("2018 Angel Angelov <p>aangeloo@gmail.com</p>
               <p> Built in <code>R</code> using the libraries <code>shiny</code>, <code>drc</code> and <code>tidyverse</code>. 
               The source code is available from GitHub <a href = https://github.com/angelovangel/FitGrowth>here</a>.</p>"))
        })
}

shinyApp(ui, server)