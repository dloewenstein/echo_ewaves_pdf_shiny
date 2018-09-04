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