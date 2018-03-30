
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
                        column(6, actionButton("chooseDir", "Select PFT Directory"))
                      ),
                      fluidRow(
                        column(12,
                           tableOutput("pftMean")
                        )
                      ),
                      fluidRow(
                        column(12,
                          tableOutput("pftMeanRef")
                        )
                      ),
                      
                      fluidRow(
                        column(12,
                               DT::dataTableOutput("pft1Table"))
                      ),
                      fluidRow(
                        column(12,
                               DT::dataTableOutput("pft2Table"))
                      ),
                      fluidRow(
                        column(6,
                               downloadButton("indDl", "Individual Data")),
                        column(6, 
                               downloadButton("meanDl", "Mean Data"))
                      )
                    )
)
   
# Define server logic 
server <- function(input, output) {
  # Load packages
  y <- c("shiny", "plyr", "dplyr", "reshape", "shinythemes", "ggplot2", "readr", "shinyFiles",
         "shinyjs", "taRifx", "shinydashboard", "here", "plotrix", "data.table", "gridExtra",
         "rmarkdown", "tm", "rhandsontable", "taRifx", "DT", "pdftools", "tcltk", "grid",
         "gridExtra")
  for(i in 1:length(y)){is_installed <- function(mypkg){is.element(mypkg, installed.packages()[,1])}
  if(!is_installed(y[i])){install.packages(y[i], repos="http://lib.stat.cmu.edu/R/CRAN")}
  library(y[i], character.only=TRUE, quietly=TRUE,verbose=FALSE)}
  source("./www/pftParseFun.R")
  source("./www/meanPFT.R")
  vals <- reactiveValues()
  observeEvent(input$chooseDir, { # choose directory on button click
   vals$dir <- tk_choose.dir() # establish reactive values
    })
  pftData <- reactive({ 
    pftParse(vals$dir) # parse PFT using function 
   })
  meanPft <- reactive({
    meanPFT(pftData()) # mean PFT using function
  })
  output$pftMean <- renderTable({
    meanPFTData <- meanPft() # render mean table 1
    meanPFTData[, 1:9]
  }) 
  output$pftMeanRef <- renderTable({
    meanPFTData <- meanPft() # render mean table 2
    meanPFTData[, 10:ncol(meanPFTData)]
  }) 
  output$pft1Table <- DT::renderDataTable({
    pft1 <- pftData() # render individual table 1
    pft1[,c(1:12)] 
  })
  output$pft2Table <- DT::renderDataTable({
    pft2 <- pftData()  # render individual table 2
    pft2[,c(1,2,13:20)]
  })
  output$indDl <- downloadHandler(
      filename = function() {
        paste('ind_PFT_', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(pftData(), con)
      }
    )
  output$meanDl <- downloadHandler(
      filename = function() {
        paste('mean_PFT_', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(meanPft(), con)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

