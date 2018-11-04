# Load packages
y <- c("shiny", "plyr", "dplyr", "reshape", "shinythemes", "ggplot2", "readr", "shinyFiles",
       "shinyjs", "taRifx", "shinydashboard", "here", "plotrix", "data.table", "gridExtra",
       "rmarkdown", "tm", "rhandsontable", "taRifx", "DT", "pdftools", "tcltk", "grid",
       "gridExtra", "gsubfn", "rlist", "shinycssloaders")
for(i in 1:length(y)){is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
if(!is_installed(y[i])){install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN")}
library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)}

# Define UI 
ui <- dashboardPage(title="PFT Data Extractor",
                    dashboardHeader(title = "PFT Extractor"),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      fluidRow(
                        column(6, actionButton("chooseDir", "Select PFT Directory")),
                        column(2, uiOutput('fileChoose')),
                        column(4, uiOutput('pftID'))
                      ),
                      uiOutput('heading'),
                      fluidRow(
                        column(6,
                               withSpinner(tableOutput("pftMean"),
                                           type = getOption("spinner.type", default = 6))
                        ),
                        column(6,
                               tableOutput("pftInd")
                        )
                      ),
            
                   uiOutput('buttons')
                    )
)
   
# Define server logic 
server <- function(input, output) {

  source("./www/pftParseFun.R",local = TRUE)
  source("./www/meanPFT.R",local = TRUE)
  vals <- reactiveValues()
  observeEvent(input$chooseDir, { # choose directory on button click
   vals$dir <- tk_choose.dir() # establish reactive values
    })
  pftData <- reactive({ 
    validate(need(!is.null(vals$dir), ''))
    pftParse(vals$dir) # parse PFT using function 
   })
  output$heading <- renderUI({
    validate(need(!is.null(vals$dir), ''))
    fluidRow(
    column(6, h3("Mean Data")),
    column(6, h3("Individual Data")))
  })
  output$pftID <- renderUI({
    validate(need(!is.null(vals$dir), ''))
    data <- pftData()
    mainPanel(
      HTML(
        paste(
          h4(paste("ID:", data[[input$fileChoose]][['id']])),
          h4(paste("NAME:", data[[input$fileChoose]][['name']])),
          h4(paste("AGE:", data[[input$fileChoose]][['age']])),
          h4(paste("DATE:", data[[input$fileChoose]][['date']]))
        )
      )
    )  })
  
  meanPft <- reactive({
    meanPFT(pftData()) # mean PFT using function
    # print(meanPFT(pftData()) )
  })
  output$pftMean <- renderTable({
    meanPft() # render mean table 1
  }) 
  output$fileChoose <- renderUI({
    validate(need(!is.null(vals$dir), ''))
    selectInput('fileChoose', label = 'Choose individual file', choices = names(pftData()))
  })

  output$pftInd <- renderTable({
    validate(need(!is.null(input$fileChoose), ''))
    data <- pftData()
    data[[input$fileChoose]][['data']]
  })
  
  output$buttons <- renderUI({
    validate(need(!is.null(vals$dir), ''))
    fluidRow(
    column(6, downloadButton("meanDl", "Mean Data File")),
    column(2, downloadButton("indDl", "Current Individual File")),
    column(2, downloadButton("indDlall", "All Individual Files"))
  )
  })
  indData <- reactive({
     data <- pftData()
     data[[input$fileChoose]]
  })        

  pftDf <- reactive({
    dl <- lapply(pftData(), `[[`, 6)
  })
  
  output$meanDl <- downloadHandler(
      filename = function() {
        paste('mean_PFT_', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(meanPft(), con, fileEncoding = "UTF-8")
      }
    )
  output$indDl <- downloadHandler(
      filename = function() {
        paste0(substr(input$fileChoose,1, nchar(input$fileChoose)-4),'_', Sys.Date(), '.csv')
      },
      content = function(con) {
        write.csv(addHead(indData()), con,
                  row.names = FALSE)
      }
    )
  output$indDlall <- downloadHandler(
    filename = function() {
      paste0('all_pft_files_', Sys.Date(), '.zip')
    },
    content = function(fname) {
      allData <- pftData()
      fs <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      for (i in 1:length(dl)) {
        path <- paste0(substr(names(dl)[i],1, nchar(names(dl)[i])-4) , ".csv")
        fs <- c(fs, path)
        d <- allData[[i]]
        write.csv(addHead(d), path, 
                  row.names = FALSE)
      }
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )

}

# Run the application 
shinyApp(ui = ui, server = server)

