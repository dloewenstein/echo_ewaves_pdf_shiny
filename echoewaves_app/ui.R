library(shiny)
library(DT)

lapply(list("R/fzero.R",
            "R/generate_initial_pdf_parameters.R",
            "R/generate_pdf_parameters.R",
            "R/rclipBoardSetup.R",
            "R/rclipButton.R"), source)


cot <- function(x){
  return(1/tan(x))
}

ui <- fluidPage(
  
  #rclipboardSetup(),
  
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
        #column(3,
               #uiOutput("clip")),
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