#' Check duration
#'
#' @param duration (ms)
#'
#' @return TRUE if invalid
#' @export
#'
#' @examples
#' check_duration(501)
check_duration <- function(duration) {
    invalid_value <- FALSE
    above_max <- 500 < duration
    under_min <- duration < 10

    # Return true if break criteria
    if (above_max || under_min) {
        invalid_value <- TRUE
    }

    return(invalid_value)
}

#' Check velocity
#'
#' @param velocity (m/s)
#'
#' @return TRUE if invalid
#' @export
#'
#' @examples
#' check_velocity(5.1)
check_velocity <- function(velocity) {
    invalid_value <- FALSE
    above_max <- 5 < velocity
    under_low  <- velocity < 0.1

    # Return true if break criteria
    if (above_max || under_low) {
        invalid_value <- TRUE
    }

        return(invalid_value)
}

#' Convert to meter from cm
#'
#' @param value cm
#' @param convert logical if conversion needed
#'
#' @return value in m
#' @export
#'
convert_to_meter <- function(value, convert) {
    stopifnot(is.numeric(value))
    stopifnot(is.logical(convert))

    conversion_factor <- ifelse(convert, 100, 1)

    in_meter <- value / conversion_factor
}

#' Convert from acceleration to duration
#'
#' @param delta_velocity m/s
#' @param acceleration m/s2
#'
#' @return Duration in ms
#' @export
#'
#' @examples
#' convert_to_time(2.1, 30)
convert_to_time <- function(delta_velocity, acceleration) {

    stopifnot(is.numeric(delta_velocity))
    stopifnot(is.numeric(acceleration))

    # time = delta(velocity)/acceleration * 1000 to get milliseconds

    result_in_time <- (
        (delta_velocity / acceleration)
        * 1000 # To milliseconds
    )
}

#' Convert from duration to acceleration
#'
#' @param delta_velocity m/s
#' @param time ms
#'
#' @return cm/s2
#' @export
#'
#' @examples
#' converto_to_acceleration(2.1, 70)
convert_to_acceleration <- function(delta_velocity, time) {

    stopifnot(is.numeric(delta_velocity))
    stopifnot(is.numeric(time))

    # Need seconds
    time <- time/1000

    # acceleration = delta(velocity)/time * 100 to get cm/s2

    result_as_acceleration <- (
        (delta_velocity / time )
        * 100
    )
}
