library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(broom)

lapply(
  list(
    "R/fzero.R",
    "R/curve_fit.R",
    "R/color_renderer.R",
    "R/ewave_velocity_fx_time.R",
    "R/generate_initial_pdf_parameters.R",
    "R/generate_pdf_parameters.R"
  ),
  source
)

input_box_width <- '100'

ui <- dashboardPage(
    skin = "blue",
    title = "Echo E-waves",
    sidebar = dashboardSidebar(disable = TRUE),
    header = dashboardHeader(title = "Echo E-Waves parameterized diastolic filling method"),
    body = dashboardBody(
        tags$script('
    $(document).on("keydown", function (e) {
       Shiny.onInputChange("enter", e.which);
    });
                '),
        fluidRow(
            box(
                width = 3,
                title = "Inputs",
                status = "primary",
                solidHeader = TRUE,
                splitLayout(
                    numericInput(
                        inputId = "at_input",
                        label   = "AT [ms]",
                        value   = 0,
                        min     = 0,
                        width   = input_box_width
                    ),
                    numericInput(
                        inputId = "dt_input",
                        label   = "DT [ms]",
                        value   = 0,
                        min     = 0,
                        width   = input_box_width
                    ),
                    numericInput(
                        inputId = "epeak_input",
                        label   = "Epeak [m/s]",
                        value   = 0,
                        min     = 0,
                        width   = input_box_width
                    )
                )
            ),
            box(
                width = 4,
                title = "Messages",
                status = "danger",
                solidHeader = TRUE,
                textOutput("messages")
            )
        ), 
        fluidRow(
            box(width  = 8,
                title  = "Results",
                status = "primary",
                solidHeader = TRUE,
                DT::dataTableOutput("dataview")
                )
        ),
        fluidRow(
            box(width  = 8,
                title  = "Summary",
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput("summary")
            ),
            box(width = 3,
                title = "Regression",
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput("regression"))
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
    
    dataview_dataframe <- tibble(
        AT = numeric(0),
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
        velocity_curve = vector("list", 0)
    )
    
    lm_dataframe <- data.frame(
        term = character(0),
        estimate = numeric(0),
        R2 = numeric(0)
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
    
    lm_values <- reactiveValues(data = lm_dataframe)
    dataview_values <- reactiveValues(data = dataview_dataframe)
    summary_values <- reactiveValues(data = dataview_dataframe %>% select(-velocity_curve))
    
    .startup_message <- "Everything is correct"
    message_values <- reactiveValues(text = .startup_message)
  

    observeEvent(input$enter, {
# Requisites -------------------------------------------------------------
          if (input$enter == 13) {
              TRUE
          } else {
              return()
          }
          req(input$at_input)
          req(input$dt_input)
          req(input$epeak_input)

# Generate PDF variables -------------------------------------------------

          input_AT <- input$at_input
          input_DT <- input$dt_input
          input_Epeak <- input$epeak_input
          
          initial_pdf_parameters <- generate_c_k_x0(AT = input_AT,
                                                    DT = input_DT,
                                                    Epeak = input_Epeak)
          
          secondary_pdf_parameters <- generate_pdf_parameters(C     = initial_pdf_parameters$C,
                                                              K     = initial_pdf_parameters$K,
                                                              x0    = initial_pdf_parameters$x0,
                                                              AT    = input_AT,
                                                              DT    = input_DT,
                                                              Epeak = input_Epeak)
          
          if (initial_pdf_parameters$C < 0) {
                  .text <- "Error: Assigned inputs give unphysiological results"
                  message_values$text <- .text
              return()
          } else {
              message_values$text <- .startup_message
          }
          
          curve_parameters <- list(K = initial_pdf_parameters$K,
                                   C = initial_pdf_parameters$C,
                                   x0 = initial_pdf_parameters$x0)
          velocity_curve <- purrr::pmap(
              curve_parameters,
              ewave_velocity_fx_time_data
              )
          
          pdf_data <- tibble(
              AT = input_AT,
              DT = input_DT,
              Epeak = input_Epeak,
              K = initial_pdf_parameters$K,
              C = initial_pdf_parameters$C,
              x0 = abs(initial_pdf_parameters$x0) * 100,             #  Convert from meter to cm, present as absolut value
              Tau = (secondary_pdf_parameters$Tau) * 1000,            #  Convert from seconds to milliseconds
              KFEI = secondary_pdf_parameters$KFEI,
              VTI = secondary_pdf_parameters$VTI * 100,               # Convert from meter to cm
              peak_driving_force = abs(secondary_pdf_parameters$peak_driving_force), #  Present as absolute value
              peak_resistive_force = secondary_pdf_parameters$peak_resistive_force,
              damping_index = secondary_pdf_parameters$damping_index,
              filling_energy = secondary_pdf_parameters$filling_energy,
              velocity_curve = velocity_curve
          )
          
          dataview_values$data <- rbind(dataview_values$data, pdf_data)
          
          lm_fit <- lm(peak_driving_force ~ peak_resistive_force,
                       data = dataview_values$data)
          
          lm_tidy <- tidy(lm_fit) %>% 
              select(term, estimate)
          
          R2 <- summary(lm_fit)$r.squared
          
          lm_values$data <- cbind(lm_tidy, R2)

          mean_values  <- dataview_values$data %>% 
              select(-velocity_curve) %>% 
              summarize_all(mean)
          
          sd_values    <- dataview_values$data %>% 
              select(-velocity_curve) %>% 
              summarize_all(sd)
          
          summary_values$data <- rbind(mean_values, sd_values)
          row.names(summary_values$data) <- c("mean", "sd")
    })
    
  output$messages <- renderText({message_values$text})
  
  output$dataview <- DT::renderDataTable({
      DT::datatable(dataview_values$data %>% select(-velocity_curve),
                    colnames=c(
                        "E\nAcceleration\nTime\n[ms]"="AT",
                        "E\nDecceleration\nTime\n[ms]"="DT",
                        "E\nVmax\n[m/s]"="Epeak",
                        "Stiffness\n(k)\n[g/s2]"="K",
                        "Viscoelasticity\n(c)\n[g/s]"="C",
                        "Load\n(x0)\n[cm]"="x0",
                        "Tau\n[ms]"="Tau",
                        "KFEI\n[%]"="KFEI",
                        "VTI\n[cm]"="VTI",
                        "Peak\nDriving\nForce\n[mN]"="peak_driving_force",
                        "Peak\nResistive\nForce\n[mN]"="peak_resistive_force",
                        "Damping\nIndex\n[g2/s2]"="damping_index",
                        "Filling\nEnergy\n[mJ]"="filling_energy"),
                    extensions = c('Buttons', 'Responsive'),
                    options = list(
                        dom = 'Brtip',
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                        ),
                    autoHideNavigation = TRUE,
                    class = "compact"
                    ) %>% 
          formatRound(col_names[c("AT", "DT", "K", "Tau", "damping_index")], digits=0) %>% 
          formatPercentage(col_names["KFEI"], digits=1) %>% 
          formatRound(col_names[c("C", "x0", "VTI", "peak_driving_force", "peak_resistive_force")],
                      digits=1) %>% 
          formatRound(col_names["filling_energy"], digits=2)
  })
  
  output$summary <- DT::renderDataTable({
      DT::datatable(summary_values$data,
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
                        "Filling\nEnergy\n[mJ]"="filling_energy"),
                    extensions = c('Buttons', 'Responsive'),
                    options = list(
                        dom = 'B',
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                    ),
                    class = "compact"
      ) %>% 
          formatRound(col_names[c("AT", "DT", "K", "Tau", "damping_index")], digits=0) %>% 
          formatPercentage(col_names["KFEI"], digits=1) %>% 
          formatRound(col_names[c("C", "Epeak", "x0", "VTI", "peak_driving_force", "peak_resistive_force")],
                      digits=1) %>% 
          formatRound(col_names["filling_energy"], digits=2)
  })
  
  output$regression <- DT::renderDataTable({
      DT::datatable(lm_values$data,
                    class = "compact",
                    extensions = 'Responsive',
                    options = list(
                        dom = ""
                    )) %>%
          formatString(1) %>% 
          formatRound(c(2, 3), digits = 1)
          })
  
  output$scatterplot <- renderPlotly({
      p_scatterplot <- ggplot(dataview_values$data, aes(x=peak_resistive_force,
                                                   y=peak_driving_force)) +
          geom_point() +
          geom_smooth(method="lm", se=FALSE, show.legend=TRUE) +
          labs(y="Peak Driving Force [mN]",
               x="Peak Resistive Force [mN]",
               parse=TRUE) +
          theme_light()
      
      ggplotly(p_scatterplot)
      })
  
  output$velocityplot <- renderPlotly({
      if(!nrow(dataview_values$data)){
          example_data <- ewave_velocity_fx_time_data(41.58, 384.36, -0.1586)
          p_velocityplot <- ggplot(example_data, aes(x=x, y=y)) +
              geom_line() +
              labs(x="Time (s)", y="Velocity (m/s)") +
              annotate("text", x=0.3, y=0.9, label="Example curve", size=10) +
              scale_y_continuous(limit=c(0, 1.2), expand=c(0,0)) +
              scale_x_continuous(limit=c(0, 0.405), expand=c(0,0)) +
              theme_light()
      } else{
          velocity_data <- dataview_values$data %>% 
              select(velocity_curve) %>%
              mutate(id = row.names(.)) %>% 
              unnest(velocity_curve)

          y_max <- velocity_data %>%
              pull(y) %>% 
              max(., na.rm = TRUE)
          
          p_velocityplot <- ggplot(velocity_data, aes(x=x, y=y, colour=id)) +
              geom_line() +
              labs(x="Time (s)", y="Velocity (m/s)") +
              theme_light() +
              scale_y_continuous(limits = c(0, y_max+0.1), expand = c(0,0)) +
              scale_x_continuous(limits = c(0, 0.405), expand = c(0,0)) +
              guides(colour=guide_legend(title = "Curve ID"))
      }
      ggplotly(p_velocityplot)
  })
  
  session$allowReconnect(TRUE)
}
shinyApp(ui = ui, server = server)
