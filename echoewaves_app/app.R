library(shiny)
library(rhandsontable)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

lapply(
  list(
    "R/fzero.R",
    "R/curve_fit.R",
    "R/color_renderer.R",
    "R/ewave_velocity_fx_time.R",
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
            box(width=2,
                title="Inputs",
                status="primary",
                solidHeader=TRUE,
                rHandsontableOutput("hot_table")
                ),
            box(width=10,
                title="Results",
                status="primary",
                solidHeader=TRUE,
                DT::dataTableOutput("dataview")
                )
        ),
        fluidRow(
            box(width=6,
                title="Lineplot",
                status="warning",
                solidHeader=TRUE,
                collapsible=TRUE,
                plotlyOutput("velocityplot")
                ),
            box(width=6,
                title="Scatterplot",
                status="warning",
                solidHeader=TRUE,
                collapsible=TRUE,
                plotlyOutput("scatterplot")
                )
            )
        )
    )

server <- function(input, output, session) {
    
    original_table_length <- 10
    
    input_dataframe <- data.frame(
        AT = rep(NA_character_,original_table_length),
        DT = rep(NA_character_,original_table_length),
        Epeak = rep(NA_character_,original_table_length),
        stringsAsFactors = FALSE
    )
    
    dataview_dataframe <- data.frame(
        AT = rep(NA_real_,original_table_length),
        DT = rep(NA_real_,original_table_length),
        Epeak = rep(NA_real_,original_table_length),
        K = rep(NA_real_,original_table_length),
        C = rep(NA_real_,original_table_length),
        x0 = rep(NA_real_,original_table_length),
        Tau = rep(NA_real_,original_table_length),
        KFEI = rep(NA_real_,original_table_length),
        VTI = rep(NA_real_,original_table_length),
        peak_driving_force = rep(NA_real_,original_table_length),
        peak_resistive_force = rep(NA_real_,original_table_length),
        damping_index = rep(NA_real_,original_table_length),
        filling_energy = rep(NA_real_,original_table_length),
        stringsAsFactors = FALSE
    )
    
    col_names <- c(
        "AT"="E\nAcceleration\nTime\n[ms]",
        "DT"="E\nDecceleration\nTime\n[ms]",
        "Epeak"="E\nVmax\n[m/s]",
        "K"="Stiffness\n(K)\n[g/s2]",
        "C"="Viscoelasticity\n(C)\n[cm]",
        "x0"="Load\n(x0)\n[cm]",
        "Tau"="Tau\n[ms]",
        "KFEI"="KFEI\n[%]",
        "VTI"="VTI\n[cm]",
        "peak_driving_force"="Peak\nDriving\nForce\n[mN]",
        "peak_resistive_force"="Peak\nResistive\nForce\n[mN]",
        "damping_index"="Damping\nIndex\n[g2/s2]",
        "filling_energy"="Filling\nEnergy\n[mJ]"
    )
    
    velocity_dataframe <- dplyr::tibble(id=rep(NA_real_, 100),
                                        K=rep(NA_real_, 100),
                                        C=rep(NA_real_, 100),
                                        x0=rep(NA_real_, 100),
                                        velocity_curve=vector("list", 100))
    
    datavalues <- reactiveValues(data=input_dataframe)
    dataview_values <- reactiveValues(data=dataview_dataframe)
    velocityvalues <- reactiveValues(data=velocity_dataframe)
  
  output$hot_table <- renderRHandsontable({
      rhandsontable(datavalues$data) %>%
          hot_context_menu(allowColEdit=FALSE) %>%
          hot_cols(columnSorting=TRUE,
                   type="numeric",
                   copyable=TRUE,
                   sorting=TRUE) %>%
          hot_col(col=c("AT", "DT"), format="0") %>%
          hot_col(col="Epeak",       format="0.0")
      })
  
  observeEvent(
      input$hot_table$changes$changes,
      {

# Requisites -------------------------------------------------------------


          row_index <- input$hot_table$changes$changes[[1]][[1]]+1
          req(input$hot_table$data[[row_index]][[1]])
          req(input$hot_table$data[[row_index]][[2]])
          req(input$hot_table$data[[row_index]][[3]])

# Make R objects ---------------------------------------------------------
          datavalues$data <- hot_to_r(input$hot_table)
          datavalues$data <- as.data.frame(sapply(datavalues$data, as.numeric))

# Generate PDF variables -------------------------------------------------

          input_AT <- datavalues$data[row_index, "AT"]
          input_DT <- datavalues$data[row_index, "DT"]
          input_Epeak <- datavalues$data[row_index, "Epeak"]
          
          initial_pdf_parameters <- generate_c_k_x0(AT=input_AT,
                                                    DT=input_DT,
                                                    Epeak=input_Epeak)
          
          secondary_pdf_parameters <- generate_pdf_parameters(C=initial_pdf_parameters$C,
                                                              K=initial_pdf_parameters$K,
                                                              x0=initial_pdf_parameters$x0,
                                                              AT=input_AT,
                                                              DT=input_DT,
                                                              Epeak=input_Epeak)
              
              dataview_values$data[row_index, "AT"] <- input_AT
              dataview_values$data[row_index, "DT"] <- input_DT
              dataview_values$data[row_index, "Epeak"] <- input_Epeak
              dataview_values$data[row_index, "K"] <- initial_pdf_parameters$K
              dataview_values$data[row_index, "C"] <- initial_pdf_parameters$C
              dataview_values$data[row_index, "x0"] <- abs(initial_pdf_parameters$x0)*100   #  Convert from meter to cm, present as absolut value
              dataview_values$data[row_index, "Tau"] <- (secondary_pdf_parameters$Tau)*1000 #  Convert from seconds to milliseconds
              dataview_values$data[row_index, "KFEI"] <- secondary_pdf_parameters$KFEI
              dataview_values$data[row_index, "VTI"] <- secondary_pdf_parameters$VTI*100    # Convert from meter to cm
              dataview_values$data[row_index, "peak_driving_force"] <- abs(secondary_pdf_parameters$peak_driving_force) #  Present as absolute value
              dataview_values$data[row_index, "peak_resistive_force"] <- secondary_pdf_parameters$peak_resistive_force
              dataview_values$data[row_index, "damping_index"] <- secondary_pdf_parameters$damping_index
              dataview_values$data[row_index, "filling_energy"] <- secondary_pdf_parameters$filling_energy
              
              velocityvalues$data[row_index, c("C", "K", "x0")] <- dataview_values$data[row_index, c("C", "K", "x0")]
              velocityvalues$data[row_index, "x0"] <- ((-1)*velocityvalues$data[row_index, "x0"])/100
              velocityvalues$data[row_index, "id"] <- row_index
              
              velocityvalues$data[row_index, "velocity_curve"] <- list(purrr::pmap(
                  velocityvalues$data[row_index,] %>%
                      select(K, C, x0),
                  ewave_velocity_fx_time_data
              ))
      })
  
  output$dataview <- DT::renderDataTable({
      DT::datatable(dataview_values$data,
                    colnames=c(
                        "E\nAcceleration\nTime\n[ms]"="AT",
                        "E\nDecceleration\nTime\n[ms]"="DT",
                        "E\nVmax\n[m/s]"="Epeak",
                        "Stiffness\n(K)\n[g/s2]"="K",
                        "Viscoelasticity\n(C)\n[cm]"="C",
                        "Load\n(x0)\n[cm]"="x0",
                        "Tau\n[ms]"="Tau",
                        "KFEI\n[%]"="KFEI",
                        "VTI\n[cm]"="VTI",
                        "Peak\nDriving\nForce\n[mN]"="peak_driving_force",
                        "Peak\nResistive\nForce\n[mN]"="peak_resistive_force",
                        "Damping\nIndex\n[g2/s2]"="damping_index",
                        "Filling\nEnergy\n[mJ]"="filling_energy")
                    ) %>% 
          formatRound(col_names[c("AT", "DT", "K", "Tau", "damping_index")], digits=0) %>% 
          formatPercentage(col_names["KFEI"], digits=1) %>% 
          formatRound(col_names[c("C", "x0", "VTI", "peak_driving_force", "peak_resistive_force")],
                      digits=1) %>% 
          formatRound(col_names["filling_energy"], digits=2)
  })
  
  output$scatterplot <- renderPlotly({
      p_scatterplot <- ggplot(dataview_values$data, aes(x=peak_resistive_force,
                                                   y=peak_driving_force)) +
          geom_point() +
          geom_smooth(method="lm", se=FALSE, show.legend=TRUE) +
          labs(x="Peak Driving Force [mN]",
               y="Peak Resistive Force [mN]",
               parse=TRUE) +
          theme_light()
      
      ggplotly(p_scatterplot)
      })
  
  output$velocityplot <- renderPlotly({
      if(is.na(velocityvalues$data$id[1])){
          example_data <- ewave_velocity_fx_time_data(41.58, 384.36, -0.1586)
          p_velocityplot <- ggplot(example_data, aes(x=x, y=y)) +
              geom_line() +
              labs(x="Time (s)", y="Velocity (m/s)") +
              annotate("text", x=0.3, y=0.9, label="Example curve", size=10) +
              theme_light()
      } else{
          p_velocityplot <- ggplot(velocityvalues$data %>%
                                   na.omit(id) %>%
                                   mutate(id=factor(id)) %>% 
                                   select(id, velocity_curve) %>% 
                                   unnest(velocity_curve), aes(x=x, y=y, colour=id)) +
              geom_line() +
              labs(x="Time (s)", y="Velocity (m/s)") +
              theme_light() +
              guides(colour=guide_legend(title="Curve ID"))
      }
      ggplotly(p_velocityplot)
  })
  
  session$allowReconnect(TRUE)
}
shinyApp(ui = ui, server = server)