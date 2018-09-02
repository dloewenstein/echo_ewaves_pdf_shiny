library(shiny)

ui <- fluidPage(
  titlePanel("Echo E-Waves parameterized diastolic filling method"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "AT", label = "AT", value = 0),
      numericInput(inputId = "DT", label = "DT", value = 0),
      numericInput(inputId = "Epeak", label = "Epeak", value = 0),
      actionButton(inputId = "add_data", "Add data")
    ),
    mainPanel(
      DT::dataTableOutput(outputId = "dataset"),
      downloadButton(outputId = "download_data", label = "Download Data")
    )
  )
)

server <- function(input, output){
  
  newData <- reactiveValues()
  newData$df <- data.frame(at = numeric(0),
                          dt = numeric(0),
                          epeak = numeric(0))
  showData <- eventReactive(input$add_data, {
      req(input$AT)
      req(input$DT)
      req(input$Epeak)
      
      newData$df <- rbind(newData$df, data.frame(at = input$AT,
                                               dt = input$DT,
                                               epeak = input$Epeak))
      return(newData$df)
    }, ignoreNULL = TRUE)
  
 output$dataset <- DT::renderDataTable({
   DT::datatable(showData())
 })
 output$download_data <- downloadHandler(filename = paste0("Echo_EWaves_PDF_", Sys.Date(), ".csv"),
                                         content = newData)
}

shinyApp(ui = ui, server = server)