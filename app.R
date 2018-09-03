library(shiny)

source("R/fzero.R")

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
      downloadButton(outputId = "download_data", label = "Download Data")
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
                          "Peak driving force" = numeric(0),
                          "Peak resistive force" = numeric(0),
                          "Damping index" = numeric(0),
                          "Filling energy" = numeric(0)
                          )
  
  showData <- observeEvent(input$add_data, {
      req(input$AT)
      req(input$DT)
      req(input$Epeak)
      
      generate_c_k_x0 <- function(AT, DT, Epeak){
      if(DT/AT < exp(1)/(exp(1)-2)){
        K = 1/(AT + DT)^2 * pi^2/(1-(cos(pi*AT/(AT+DT)))^2)
        C = 2*pi/(AT+DT)*cot(pi*AT/(AT+DT))
        x0 = -Epeak*((AT+DT)/pi)*sin(pi*AT/(AT+DT))*exp((pi*AT/(AT+DT))*cot(pi*AT/(AT+DT)))
      } else if(DT/AT > exp(1)/(exp(1)-2)){
        f <- function(x){
          return(1-2*x*exp(log(((1+sqrt(1-x^(-2)))/(1-sqrt(1-x^(-2))))^(-1/(2*sqrt(1-x^(-2))))))-(AT/DT))
        }
        YY = fzero(f, 1.2)
        T = log(((1+sqrt(1-YY^(-2)))/(1-sqrt(1-YY^(-2))))^(-1/(2*sqrt(1-YY^(-2)))))
        K = ((1/AT)*(T/YY))*2
        C = YY*2*sqrt(K)
        x0 = Epeak*AT*(YY/(T*exp(T)))
      }
        return(list(K = K, C = C, x0 = x0))
      }
      
      initial_parameters <- generate_c_k_x0(AT = input$AT, DT = input$DT, Epeak = input$Epeak)
      
      generate_pdf_parameters <- function(C, K, x0, Epeak){
        a = C/2
        peak_driving_force = K*x0
        peak_resistive_force = C*Epeak
        damping_index = c^2-4*K
        filling_energy = 0.5*K*x0^2
        
        if(C^2-4*K < 0){ # underdamped
          w = sqrt(4*K-C^2)/2
          tmax = asin(sqrt((w*w)/(a*a+w*w)))/w
          DT = pi/w - (1/w) * (atan(2*w/C))   
          Edur = pi/w        
          Vmax = -(x0*K*exp(-a*(tmax))*sin(w*(tmax)))/w
          VTI = K*x0*(w*exp(-a*Edur)*cos(w*Edur) + a*exp(-a*Edur)*sin(w*Edur)-w)/w/(a*a+w*w)
          DTr = pi * (1/w - 1/sqrt(K))
          Tau = (DTr + 0.12)/2.88
          KFEI = 0.5 * (1 + exp((-C * pi) / sqrt(4*K-C^2)))
        } else if(C^2-4*K > 0){ # overdamped
          b = sqrt(C*C-4*K)/2
          tmax = log((a+b)/(a-b))/(2*b)
          Vmax = -x0*K*exp(-a*(tmax))*sinh(b*(tmax))/b
          Edur = AT + DT
          VTI = integrate(function(x){-K*x0/b*exp(-a*x)*sinh(b*x)}, 0, Edur)
          IdealVTI = integrate(function(x){-K*x0/sqrt(K)*sin(sqrt(K)*x)}, 0, pi/(0.5*sqrt(4*K))) #Integralen av funktion -k.*xo/sqrt(k).*sin(sqrt(k).*x)) mellan 0 och pi/(0.5*sqrt(4*k))
          KFEI = VTI/IdealVTI
        }
        
        return(list(Tau = Tau, 
                    KFEI = KFEI, 
                    VTI = VTI, 
                    peak_driving_force = peak_driving_force,
                    peak_resitive_force = peak_resistive_force,
                    damping_index = damping_index,
                    filling_energy = filling_energy))
      }
      
      
      
      newData$df <- rbind(newData$df, data.frame(at = input$AT,
                                               dt = input$DT,
                                               epeak = input$Epeak,
                                               C = initial_parameters$C,
                                               K = initial_parameters$K,
                                               x0 = initial_parameters$x0))
    }, ignoreNULL = TRUE)
  
 output$dataset <- DT::renderDataTable({
   DT::datatable(newData$df)
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