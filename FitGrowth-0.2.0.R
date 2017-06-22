## app.R ##
# shiny dashboard version
library(shiny)
library(shinydashboard)
library(tidyverse)
library(modelr)
library(broom)
library(DT)
#
# ui ******

  header <- dashboardHeader(title = "FitGrowth")
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Load data", tabName = "data", icon = icon("database")),
      menuItem("Model", tabName = "model", icon = icon("rocket")),
      menuItem("Summary plots", tabName = "summaryPlots", icon= icon("bar-chart")),
      menuItem("Summary table", tabName = "summaryTable", icon = icon("table")),
      fluidRow(valueBoxOutput("numSamplesBox", width = 12), (valueBoxOutput("successSamplesBox", width = 12))),
      hr(),
      menuItem(sliderInput("trim", label = h5("Trim time"), min=0, max=100, value=c(0,40))),
      hr(),
      menuItem("About", tabName = "about", icon = icon("user-o"))
        )
      ) #end of sidebar
    
  body <- dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "data",
        #fluidRow(valueBoxOutput("numSamplesBox", width = 3),
         #        valueBoxOutput("successSamplesBox", width = 4)
          #       ),
        
        fluidRow(
          box(width = 12, h5("Load the data as a text file, the first column must be named time, all other columns are treated as samples. Adjust the file input settings until the data is read into the app")),
          box(
            title = "Read file", status = "primary",
            fileInput('file1', 'Choose file'),
            tags$hr(),
            checkboxInput('header', 'Header', TRUE),
            radioButtons('sep', 'Separator',
                         c(Comma=',',
                           Semicolon=';',
                           Tab='\t'),
                         ','),
            radioButtons('decmark', 'Decimal mark',
                         c('comma'=',',
                           'point'="."),
                         '.'),
            width = 3),
          box(width = 9, status = "primary", DT::dataTableOutput("data"))
          )
        ),
      tabItem(tabName = "model",
              #h2("Model plot"),
              fluidRow(
                box(width = 12, 
                  h5("Plots of original data, the points used in the model are in blue, model fit is a red line. Change time slider to re-calculate."),
                  downloadLink('downloadModelPlot', 'Download Plot (pdf)')),
                box(width = 12, title = "Model plot", status = "primary", 
                    plotOutput("model"))
              )
            ),
      tabItem(tabName = "summaryPlots",
              fluidRow(
                box(width = 12, 
                    h5("Plots of the carrying capacities and growth rate constants. The red line is the mean value for all analysed samples.")),
                box(width = 6, title = "Carrying capacity", status = "primary",
                    downloadLink('downloadSummaryPlotK', 'Download Plot Carrying capacity (pdf)'),
                    plotOutput("summaryK")),
                box(width = 6, title = "Growth rate constant", status = "primary",
                    downloadLink('downloadSummaryPlotR', 'Download Plot Growth rate constant (pdf)'),
                    plotOutput("summaryR"))
                
                )
              ),
      tabItem(tabName = "summaryTable",
              fluidRow(
                box(width = 5, title= "Carrying capacity for all samples",status = "primary", DT::dataTableOutput("summaryTableK")),
                box(width = 7, title= "Growth rate constant for all samples",status = "primary", DT::dataTableOutput("summaryTableR"))
              )),
      tabItem(tabName = "about", 
              box(width = 12, title = "Usage", status = "primary", htmlOutput("usage")),
              box(width = 12, title = "About", status = "warning", htmlOutput("about"))
              
          ))
              
        ) #end of dashboard body
ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  observe({
    inFile <- input$file1
    if(is.null(inFile))
    {return(NULL)}
    growth.formula <- formula(n ~ k/(1 + ((k - n0)/n0) * exp(-r * t)))
    growth.function <- function(x) {try(nls(growth.formula, data = x, start = list(k = 10, n0 = 0.01, r = 0.5)))} #this is in try(), otherwise modelling stops at first error
#*** function to draw summary barplots    
    summaryplot <- function(x) {df2() %>%  
        unnest(params) %>% 
        select(-statistic, -p.value) %>% 
        filter(term == x) %>% 
        arrange(-estimate) %>% 
        mutate(sample = factor(sample, sample)) %>% 
                 ggplot() +
                 geom_bar(aes(sample, estimate), fill = "deepskyblue3", alpha = 0.8, stat = "identity", width = 0.6) +
                 geom_errorbar(aes(sample, ymin=estimate - std.error, ymax= estimate + std.error), width = 0.3) +
                 geom_hline(aes(yintercept= mean(estimate)), linetype = 4, color="red") +
                 coord_flip() +
                 theme_minimal()}
#***
    
    
    df <- read_delim(inFile$datapath, col_names = input$header, delim = input$sep, 
                     locale = locale(decimal_mark = input$decmark), trim_ws = TRUE, escape_double = FALSE, na = "NA")
# ** reactive things
    
    df1 <- reactive({         #this is df1(), attempt to model all samples, used to give number of failed  in errorSample 
      df %>%       
        rename(t = time) %>% 
        gather(sample, n, -t) %>% filter(between(t, input$trim[1], input$trim[2])) %>%
        group_by(sample) %>% nest %>%
        mutate(fits = map(data, growth.function)
               )
    })
    
    df2 <- reactive({       #this is the df used further, contains only passed models - filtered from df1()  
      df1() %>% filter(!grepl("*Error*", fits)) %>%   # filter passed models here    
        mutate(
               resids = map2(data, fits, add_residuals),
               preds = map2(data, fits, add_predictions),
               params = map(fits, tidy)
               )
    })
    
    modelplot <- function() { 
      df %>%  rename(t = time) %>% gather(sample, n, -t) %>%
        ggplot() + 
        geom_point(aes(t,n), alpha = 0.3) +
        geom_line(aes(t,pred), color = "red", linetype = 4, data = unnest(df2(), preds)) +  
        geom_point(aes(t, n), color="deepskyblue3", alpha = 0.8, data = unnest(df2(), preds)) +
        facet_wrap(~ sample, ncol = 8) +
        xlab("Time") +
        ylab("OD") +
        theme_minimal()
    }
    


#** outputs
    output$data <- DT::renderDataTable(datatable(df, 
                                                 rownames = FALSE, 
                                                 options = list(scrollX = TRUE, 
                                                                dom = "rltip", 
                                                                columnDefs = list(list(className = 'dt-left', targets = 0))
                                                                )
                                                 ) %>%
                                        formatRound(columns = colnames(df), 3)
                                       )
    
    if (ncol(df) <= 8) plotHeight1 = 300 else (plotHeight1 = ncol(df) * 20)
     output$model <- renderPlot({
      modelplot()
    }, res = 100, height = plotHeight1)
    
    if (ncol(df) <= 4) plotHeight2 = 200 else (plotHeight2 = ncol(df) * 25) #vary plot height according to number of samples
    output$summaryK <- renderPlot({
      summaryplot("k")
    }, res = 100, height = plotHeight2)
    
    output$summaryR <- renderPlot({
      summaryplot("r")
    }, res = 100, height = plotHeight2)
    
    output$summaryTableK <- DT::renderDataTable({
      dtt <- df2() %>% unnest(params) %>% filter(term == "k") %>% 
        rename("carrying capacity" = estimate) %>%
        select(-statistic, -p.value, -term) #formatRound does not work on df()
      datatable(dtt, rownames = FALSE, 
                filter = list(position = 'top', clear = FALSE), 
                extensions = 'Buttons', options = list(dom = 'Brltip', buttons = c("copy", "csv", "print"))
                ) %>%
     formatRound(colnames(dtt),3) %>%
     formatStyle(c(1,2), fontWeight = "bold") %>%
     formatStyle(1, backgroundColor = "steelblue", color = "white")
        })
    
    output$summaryTableR <- DT::renderDataTable({
      dtt <- df2() %>% unnest(params) %>% filter(term == "r") %>%
        mutate("doubling time" =log(2)/estimate) %>%
        mutate("DT std.error" = exp(estimate+std.error) - exp(estimate)) %>%
        rename("growth rate constant" = estimate) %>%
        select(-statistic, -p.value, -term) # formatRound does not work on df()
      datatable(dtt, rownames = FALSE, 
                filter = list(position = 'top', clear = FALSE),
                extensions = 'Buttons', options = list(dom = 'Brltip', buttons = c("copy", "csv", "print"))
                ) %>% 
        formatRound(colnames(dtt),3) %>%
        formatStyle(c(1,2,4), fontWeight = "bold") %>%
        formatStyle(1, backgroundColor = "steelblue", color = "white")
        })
   
     output$numSamplesBox <- renderValueBox({
      color <- "green"
      if (ncol(df) < 2) color <- "red"
      valueBox(value = ncol(df)-1, "samples read", color = color)
    })
    
    
     output$successSamplesBox <- renderValueBox({
      
      ifelse (ncol(df) >= 2, successSample <- nrow(df2()), successSample <- 0)
      ifelse(successSample == 0, color2 <- "red", #yes
                                  ifelse(successSample < ncol(df)-1, color2 <- "yellow", color2 <- "green")
        )
      
      valueBox(value = successSample, "samples fitted", color=color2) 
    })

     #*** plot download handlers
     output$downloadModelPlot <- downloadHandler(
       filename = "modelPlot.pdf",
       content = function(file) {
          ggsave(file, plot = modelplot(), device = "pdf")
             })    
     
     
     output$downloadSummaryPlotK <- downloadHandler(
       filename = "summaryPlot.pdf",
       content = function(file) {
         ggsave(file, plot = summaryplot("k"), device = "pdf")
       })
     
     
     output$downloadSummaryPlotR <- downloadHandler(
       filename = "summaryPlot.pdf",
       content = function(file) {
         ggsave(file, plot = summaryplot("r"), device = "pdf")
       })
     #***   
     
    }) # end observe
  
  
  
  output$usage <- renderUI({
    HTML(paste("<p>This app fits growth data to the continuous logistic equation (https://en.wikipedia.org/wiki/Generalised_logistic_function). 
    The best parameters (<code>n0</code>, <code>k</code> and <code>r</code>) are found using the nonlinear least-squares method (<code>nls</code> in <code>R</code>). 
    The app handles one or many samples (tested with 96), as well as
     NA values. You can get an example file <a href=https://www.dropbox.com/sh/zzf7y3ijwkat55e/AABUvp7BAARIdYBqZWgk1E37a?dl=0>here</a>.</p> Instructions: 
    load the data as a text file, the first column <u>must</u> be named <b>time</b>, all other columns are treated as
    samples. Adjust the file input settings until the data is read into the app. After that, take a look at the other tabs. 
    Note that the parameters of the logistic model are re-calculated when the time interval is changed with the slider."))
        })
  output$about <- renderUI({
    HTML(paste("2017 Angel Angelov <p>aangeloo@gmail.com</p>
               <p> Built in <code>R</code> using the libraries <code>shiny</code>, <code>broom</code>, <code>modelr</code> and <code>tidyverse</code>. 
               The source code is available from GitHub here.</p>"))
        })
}

shinyApp(ui, server)