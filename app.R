library(shiny)

lapply(list("R/fzero.R",
            "R/generate_initial_pdf_parameters.R",
            "R/generate_pdf_parameters.R"), source)


cot <- function(x){
  return(1/tan(x))
}

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
      fluidRow(
        column(3,
               downloadButton(outputId = "download_data", label = "Download Data")
               ),
        column(3,
               actionButton(inputId = "delete_rows", label = "Delete selected")
               )
        )
      )
    )
  )

server <- function(input, output){
  
  newData <- reactiveValues()
  newData$df <- data.frame(AT = numeric(0),
                           DT = numeric(0),
                           Epeak = numeric(0),
                           K = numeric(0),
                           C = numeric(0),
                           x0 = numeric(0),
                           Tau = numeric(0),
                           KFEI = numeric(0),
                           VTI = numeric(0),
                           peak_driving_force = numeric(0),
                           peak_resistive_force = numeric(0),
                           damping_index = numeric(0),
                           filling_energy = numeric(0),
                           stringsAsFactors = FALSE)
  
  showData <- observeEvent(input$add_data, {
      req(input$AT)
      req(input$DT)
      req(input$Epeak)
      
      
      initial_parameters <- generate_c_k_x0(AT = input$AT, DT = input$DT, Epeak = input$Epeak)
      secundary_parameters <- generate_pdf_parameters(C = initial_parameters$C,
                                                      K = initial_parameters$K,
                                                      x0 = initial_parameters$x0,
                                                      Epeak = input$Epeak)
      
      newData$df <- rbind(newData$df, data.frame(AT = input$AT,
                                                 DT = input$DT,
                                                 Epeak = input$Epeak,
                                                 K = initial_parameters$K,
                                                 C = initial_parameters$C,
                                                 x0 = initial_parameters$x0,
                                                 Tau = secundary_parameters$Tau,
                                                 KFEI = secundary_parameters$KFEI,
                                                 VTI = secundary_parameters$VTI,
                                                 peak_driving_force = secundary_parameters$peak_driving_force,
                                                 peak_resistive_force = secundary_parameters$peak_resistive_force,
                                                 damping_index = secundary_parameters$damping_index,
                                                 filling_energy = secundary_parameters$filling_energy,
                                                 stringsAsFactors = FALSE))
    }, ignoreNULL = TRUE)
  
  delete_rows <- observeEvent(input$delete_rows, {
    s <- input$dataset_rows_selected
    if (!is.null(s)) {
      newData$df <- newData$df[-s, ]
    }
  })
  
 output$dataset <- DT::renderDataTable({
   DT::datatable(newData$df, selection = "multiple")
 })
 output$download_data <- downloadHandler(filename = function() {
   paste0("Echo_EWaves_PDF_", Sys.Date(), ".csv")
   },
                                         content = function(file) {
                                           write.csv(newData$df, file)
                                           }
   )
}

shinyApp(ui = ui, server = server)