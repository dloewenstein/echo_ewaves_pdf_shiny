#' Generate viscoelastic energyloss, stiffness, and load
#'
#' @param AT E acceleration time
#' @param DT E decelleration time
#' @param Epeak E peak velocity
#'
#' @return
#' @export
#'
#' @examples
#' generate_c_k_x0(AT = 50, DT = 200, Epeak = 1.1)
generate_c_k_x0 <- function(AT, DT, Epeak){
    AT <- AT/1000
    DT <- DT/1000
  .cot <- function(x) 1/tan(x)
  if(DT/AT < exp(1)/(exp(1)-2)){
    K <- 1/(AT + DT)^2 * pi^2/(1-(cos(pi*AT/(AT+DT)))^2)
    C <- 2*pi/(AT+DT)*.cot(pi*AT/(AT+DT))
    x0 <- -Epeak*((AT+DT)/pi)*sin(pi*AT/(AT+DT))*exp((pi*AT/(AT+DT))*.cot(pi*AT/(AT+DT)))
  } else if(DT/AT > exp(1)/(exp(1)-2)){
    .f <- function(x){
        #  Had to add as.complex since sqrt throughs an error when trying to take sqrt out negative numbers
        #  Had to wrap in Re() since next function (fzero) needs real numbers
        Re(1-2*x*exp(log(((1+sqrt(as.complex(1-x^(-2))))/(1-sqrt(as.complex(1-x^(-2)))))^(-1/(2*sqrt(as.complex(1-x^(-2)))))))-(AT/DT))
    }
    YY <- fzero(.f, 1.2)$x
    Tk <- log(((1+sqrt(1-YY^(-2)))/(1-sqrt(1-YY^(-2))))^(-1/(2*sqrt(1-YY^(-2)))))
    K <- ((1/AT)*(Tk/YY))^2
    C <- YY*2*sqrt(K)
    x0 <- Epeak*AT*(YY/(Tk*exp(Tk)))
  }
  return(list(K = K, C = C, x0 = x0))
}
