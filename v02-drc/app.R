## app.R ##
# shiny dashboard version
# shiny dashboard version
# v02-drc, uses memoise, plotting is now the slowest part
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggpubr)
library(data.table)
#library(modelr)
#library(broom)
library(DT)
library(drc)
library(memoise)

source("R/do_drm.R")


#
# ui ******

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
                                             "Select which samples to analyse", choices = c("A"), multiple = TRUE)),
                    column(4, selectizeInput("theme", "Plot theme", 
                                           choices = c("blackwhite", 
                                                       "minimal",
                                                       "pubclean",
                                                       "pubr"),
                                           selected = "blackwhite",
                                           multiple = F)),
                    
                    column(2, selectizeInput("facetCols", "Number of plot columns", choices = c(1:24), selected =4))
                    
                    ),
                
                box(width = 12, title = "Model plot", status = "primary",
                    
                    plotOutput("model", height = "400px")),
                column(4, sliderInput("trim", label = h5("Trim time"), min=0, max=150, value=c(0,40))),
                column(4, sliderInput("pointsalpha", label = h5("Adjust point opacity"), min = 0, max = 1, value = 0.3)),
                column(1, checkboxInput("confidence", label = "Show confidence interval", value = FALSE)),
                column(1, checkboxInput("doublingtime", label = "Show doubling time", value = FALSE)),
                column(2, downloadButton('downloadModelPlot', 'Download Plot (pdf)'))
                    
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

server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
    
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
    updateSliderInput(session, "trim", max = max(df()$time), value = c(0, max(df()$time)))
    
  })
    
#******************************************    
## model df, fits model after filtering by time
    
    dflong <- function() {
      validate(need
               (input$selectedSamples, "Select samples first.. ")   
      )
      df() %>% 
        dplyr::select(time, one_of(input$selectedSamples)) %>% # here actual filtering on selectedSamples , notice one_of!       
        rename(t = time) %>%
        gather(sample, n, -t)
      
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

    
    
    observe({print(df1(dflong()))})
    
    
    modelplot <- function() {
      predictions <- df1(dflong()) %>% unnest(pred)
      data <- df1(dflong()) %>% unnest(data)
      
     p <- dflong() %>%
      ggplot() + 
      geom_point(aes(t, n), alpha = input$pointsalpha, size = 1) +
       # main pred line
      geom_line(aes(dose, pred), color = "red", linetype = 4, 
                data = predictions) +
      
      geom_point(aes(t, n), alpha = input$pointsalpha, color = "steelblue", size = 1,
                 data = data) +
      # geom_vline(aes(xintercept = input$trim[1]), linetype = 5, size = 0.2) +
      # geom_vline(aes(xintercept = input$trim[2]), linetype = 5, size = 0.2) +
      
       
      facet_wrap(~ sample, ncol = as.integer(input$facetCols)) +
      xlab(paste0("Time [", input$timeUnits, "]")) +
      ylab("OD")
      
        if(input$confidence) p <- p + geom_ribbon(aes(dose, ymin = predmin, ymax = predmax), alpha = 0.3, data = predictions)
        if(input$doublingtime) p <- p + 
                                      geom_text(aes(x = Inf, y = -Inf, 
                                                    label = paste0(round(dt, 3)," ", input$timeUnits)
        ), 
        hjust = 1.1,
        vjust = -0.5,
        size = 3,
        color = "steelblue",
        data = dtt())
     
        if(input$theme == "blackwhite") p <- p + theme_bw()
        if(input$theme == "minimal")    p <- p + theme_minimal()
        if(input$theme == "pubclean") p <- p + theme_pubclean()
        if(input$theme == "pubr") p <- p + theme_pubr()
        
      p <- p + theme(aspect.ratio = 1)
     
      print(p)
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
      modelplot()
    }, res = 120)
     
   
    
    #if (ncol(df) <= 4) plotHeight2 = 200 else (plotHeight2 = ncol(df) * 25) #vary plot height according to number of samples
     
     dtt <- reactive({
       df1(dflong()) %>% 
       unnest(coefs) %>% 
       dplyr::filter(param == "Slope:(Intercept)") %>%
       dplyr::mutate(Estimate = abs(Estimate),
                     dt = log(2)/Estimate,
                     dtsterr = exp(Estimate + `Std. Error`) - exp(Estimate))
     })
    
    output$summaryTable <- DT::renderDataTable({
      dtt() %>%
             
              dplyr::select(Sample = sample, 
                            "Growth rate constant" = Estimate, 
                            "Growth rate std. error" = `Std. Error`,
                            "Doubling time" = dt,
                            "Doubling time std. error" = dtsterr) %>%
      
      
     
      
        
      datatable( 
                caption = paste0("L4 parameters, the time range used in the model is between ", input$trim[1], " and ", input$trim[2], " ", input$timeUnits),
                rownames = FALSE, 
                extensions = 'Buttons', 
                options = list(dom = 'Brltip', 
                               buttons = c("copy", "csv", "print"))
                ) %>%
    formatRound(2:5, 3) %>%
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

     #*** plot download handlers
     output$downloadModelPlot <- downloadHandler(
       filename = "modelPlot.pdf",
       content = function(file) {
          ggsave(file, plot = modelplot(), device = "pdf", width = 11, height = 8, units = "in")
             })    
     
     
     
     #***   
     
    
  
  
  
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