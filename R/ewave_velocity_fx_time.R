#' Velocity of motion as function of time for E-waves
#'
#' In the PDF framework the following expressions
#' describe the velocity of motion as function of time
#'
#' @param .t time as numeric
#' @param C Viscoelasticity (g/s)
#' @param K Stiffness (g/s2)
#' @param x0 Load (m)
#'
#' @return vt, velocity as a function of time
#' @export
#'
#' @examples
#'
ewave_velocity_fx_time <- function(.t, C, K, x0){
    if((C^2)-(4*K) < 0){ #  Underdamped cases
        omega <- 0.5*sqrt((4*K)-(C^2))
        vt <- (((-K)*x0)/omega)*exp((0.5*(-C))*.t)*sin(omega*.t)
    } else if((C^2)-(4*K) > 0){ #  Overdamped cases
        beta <- 0.5*sqrt((C^2)-(4*K))
        vt <- (((-K)*x0)/beta)*exp((0.5*(-C))*.t)*sinh(beta*.t)
    } else if((C^2)-(4*K) == 0){ #  Critically damped cases
        vt <- ((-K)*x0*.t)*exp((0.5*(-C))*.t)
    }
}

ewave_velocity_fx_time_vectorized <- Vectorize(ewave_velocity_fx_time, ".t")

ewave_velocity_fx_time_data <- function(C, K, x0){
    values <- data.frame(
        curve_fit(
            ewave_velocity_fx_time_vectorized(.t=x, C=C, K=K, x0=x0),
            from=0, to=0.4))
}
