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