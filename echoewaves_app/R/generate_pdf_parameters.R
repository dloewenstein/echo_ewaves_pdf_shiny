generate_pdf_parameters <- function(C, K, x0, Epeak, AT, DT){
  a = C/2
  peak_driving_force = K*x0
  peak_resistive_force = C*Epeak
  damping_index = C^2-4*K
  filling_energy = 0.5*K*x0^2
  
  if(C^2-4*K < 0){ # underdamped
    w    = sqrt(4*K-C^2)/2
    tmax = asin(sqrt((w*w)/(a*a+w*w)))/w
    DT   = pi/w - (1/w) * (atan(2*w/C))   
    Edur = pi/w        
    Vmax = -(x0*K*exp(-a*(tmax))*sin(w*(tmax)))/w
    VTI  = K*x0*(w*exp(-a*Edur)*cos(w*Edur) + a*exp(-a*Edur)*sin(w*Edur)-w)/w/(a*a+w*w)
    DTr  = pi * (1/w - 1/sqrt(K))
    Tau  = (DTr + 0.12)/2.88
    KFEI = 0.5 * (1 + exp((-C * pi) / sqrt(4*K-C^2)))
  } else if(C^2-4*K > 0){ # overdamped
    b    = sqrt(C*C-4*K)/2
    tmax = log((a+b)/(a-b))/(2*b)
    Vmax = -x0*K*exp(-a*(tmax))*sinh(b*(tmax))/b
    Edur = AT + DT
    VTI  = integrate(function(x){-K*x0/b*exp(-a*x)*sinh(b*x)}, 0, Edur)$value
    IdealVTI = integrate(function(x){-K*x0/sqrt(K)*sin(sqrt(K)*x)}, 0, pi/(0.5*sqrt(4*K)))$value #Integralen av funktion -k.*xo/sqrt(k).*sin(sqrt(k).*x)) mellan 0 och pi/(0.5*sqrt(4*k))
    KFEI = VTI/IdealVTI
    
    # DT and Tau calculated with S's geometric method
    
    y   = C/(2*sqrt(K))
    t0  = log(((1+sqrt(1-y^(-2)))/(1-sqrt(1-y^(-2))))^(-1/(2*sqrt(1-y^(-2)))))
    DT  = (1/sqrt(K)) * (t0/(2*y^2*exp(t0)-y))
    DTr = tmax + DT - pi/sqrt(K)
    Tau = (DTr + 0.12)/2.88
  } else if(C^2-4*K == 0){#critically damped
    
    Vmax = -x0*K*(2/C)*exp(-1)
    tmax = 2/C
    DTr  = 2/C + DT - pi/sqrt(K)
    Tau  = (DTr + 0.12)/2.88
    ta   = DT + AT
    VTI  = -x0*K/(C*C/4)*(-C/2*ta*exp(-C/2*ta)-exp(-c/2*ta)) - x0*k/(C*C/4)
    IdealVTI = integrate(function(x){-K*x0/sqrt(K)*sin(sqrt(K)*x)}, 0, pi/(0.5*sqrt(4*K)))$value #Integralen av funktion -k.*xo/sqrt(k).*sin(sqrt(k).*x)) mellan 0 och pi/(0.5*sqrt(4*k))KFEI = VTI/IdealVTI 
    }
  
  return(list(Tau = Tau, 
              KFEI = KFEI, 
              VTI = VTI, 
              peak_driving_force = peak_driving_force,
              peak_resistive_force = peak_resistive_force,
              damping_index = damping_index,
              filling_energy = filling_energy))
}
