library(shiny)
library(rhandsontable)
library(shinydashboard)
library(ggplot2)
library(plotly)

lapply(
  list(
    "R/fzero.R",
    "R/generate_initial_pdf_parameters.R",
    "R/generate_pdf_parameters.R",
    "R/rclipBoardSetup.R",
    "R/rclipButton.R"
  ),
  source
)

ui <- dashboardPage(
    skin="blue",
    title="Echo E-waves",
    sidebar = dashboardSidebar(disable=TRUE),
    header=dashboardHeader(title="Echo E-Waves parameterized diastolic filling method"),
    body=dashboardBody(
        fluidRow(
            box(width=7,
                solidHeader=TRUE,
                rHandsontableOutput("hot_table")
                )
            ),
        # fluidRow(
        #     column(
        #         3,
        #         downloadButton(outputId = "download_data", label = "Download Data")
        #         )
        #     ),
        fluidRow(
            box(width=7,
                plotlyOutput("scatterplot"))
            )
    )
)

server <- function(input, output, session) {
    
    original_table_length <- 10
    
    app_dataframe <- data.frame(
        AT = c(rep(NA_character_,original_table_length)),
        DT = rep(NA_character_,original_table_length),
        Epeak = rep(NA_character_,original_table_length),
        K = rep(NA_character_,original_table_length),
        C = rep(NA_character_,original_table_length),
        x0 = rep(NA_character_,original_table_length),
        Tau = rep(NA_character_,original_table_length),
        KFEI = rep(NA_character_,original_table_length),
        VTI = rep(NA_character_,original_table_length),
        peak_driving_force = rep(NA_character_,original_table_length),
        peak_resistive_force = rep(NA_character_,original_table_length),
        damping_index = rep(NA_character_,original_table_length),
        filling_energy = rep(NA_character_,original_table_length),
        stringsAsFactors = FALSE
    )
    
    read_only_columns <- names(
        subset(
            app_dataframe, 
            select=-c(AT, DT, Epeak)
        )
    )
    
    input_columns <- c("AT", "DT", "Epeak")
    
    col_names <- list(
        "E Acceleration Time [ms]",
        "E Decceleration Time [ms]",
        "E Vmax [m/s]",
        "Stiffness (K), [g/s2]",
        "Viscoelasticity (C), [cm]",
        "Load (x0), [cm]",
        "Tau [ms]",
        "KFEI [%]",
        "VTI [cm]",
        "Peak Driving Force [mN]",
        "Peak Resistive Force [mN]",
        "Damping Index [g2/s2]",
        "Filling Energy [mJ"
    )
    
    names(col_names) <- names(app_dataframe)
    
    datavalues <- reactiveValues(data=app_dataframe)
  
  output$hot_table <- renderRHandsontable({
      rhandsontable(datavalues$data) %>%
          hot_context_menu(allowColEdit=FALSE) %>% 
          hot_cols(columnSorting=TRUE,
                   type="numeric",
                   copyable=TRUE,
                   sorting=TRUE) %>% 
          hot_col(col=read_only_columns,
                  readOnly=TRUE) %>%
          hot_col(col=c("AT", "DT"), format="0") %>%
          hot_col(col="Epeak",       format="0.0") %>% 
          hot_col(col="KFEI",        format="0.0%") %>%
          hot_col(col="K",           format="0") %>% 
          hot_col(col="C",           format="0.0") %>% 
          hot_col(col="x0",          format="0.0") %>% 
          hot_col(col="Tau",         format="0") %>% 
          hot_col(col="peak_driving_force",   format="0.0") %>% 
          hot_col(col="peak_resistive_force", format="0.0") %>% 
          hot_col(col="damping_index",        format="0") %>% 
          hot_col(col="filling_energy",       format="0.00")
      })
  
  observeEvent(
      input$hot_table$changes$changes,
      {
          row_index <- input$hot_table$changes$changes[[1]][[1]]+1
          req(input$hot_table$data[[row_index]][[1]])
          req(input$hot_table$data[[row_index]][[2]])
          req(input$hot_table$data[[row_index]][[3]])
          
          datavalues$data <- hot_to_r(input$hot_table)
          datavalues$data <- as.data.frame(sapply(datavalues$data, as.numeric))
          
          initial_pdf_parameters <- generate_c_k_x0(AT=datavalues$data[row_index, "AT"],
                                                    DT=datavalues$data[row_index, "DT"],
                                                    Epeak=datavalues$data[row_index, "Epeak"])
          
          secondary_pdf_parameters <- generate_pdf_parameters(C=initial_pdf_parameters$C,
                                                              K=initial_pdf_parameters$K,
                                                              x0=initial_pdf_parameters$x0,
                                                              AT=datavalues$data[row_index, "AT"],
                                                              DT=datavalues$data[row_index, "DT"],
                                                              Epeak=datavalues$data[row_index, "Epeak"])
              datavalues$data[row_index, "K"] <- initial_pdf_parameters$K
              datavalues$data[row_index, "C"] <- initial_pdf_parameters$C
              datavalues$data[row_index, "x0"] <- abs(initial_pdf_parameters$x0)*100   #  Convert from meter to cm, present as absolut value
              datavalues$data[row_index, "Tau"] <- (secondary_pdf_parameters$Tau)*1000 #  Convert from seconds to milliseconds
              datavalues$data[row_index, "KFEI"] <- secondary_pdf_parameters$KFEI
              datavalues$data[row_index, "VTI"] <- secondary_pdf_parameters$VTI
              datavalues$data[row_index, "peak_driving_force"] <- abs(secondary_pdf_parameters$peak_driving_force) #  Present as absolute value
              datavalues$data[row_index, "peak_resistive_force"] <- secondary_pdf_parameters$peak_resistive_force
              datavalues$data[row_index, "damping_index"] <- secondary_pdf_parameters$damping_index
              datavalues$data[row_index, "filling_energy"] <- secondary_pdf_parameters$filling_energy
      })
  
  output$scatterplot <- renderPlotly({
      p_scatterplot <- ggplot(datavalues$data, aes(x=peak_resistive_force,
                                                   y=peak_driving_force)) +
          geom_point() +
          geom_smooth(method="lm", se=FALSE, show.legend=TRUE) +
          labs(x="Peak Driving Force [mN]",
               y="Peak Resistive Force [mN]",
               parse=TRUE) +
          theme_light()
      
      ggplotly(p_scatterplot)
      })
  
  session$allowReconnect(TRUE)
}
shinyApp(ui = ui, server = server)