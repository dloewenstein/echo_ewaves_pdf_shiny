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

convert_to_meter <- function(value, convert) {
    stopifnot(is.numeric(value))
    stopifnot(is.logical(convert))

    conversion_factor <- ifelse(convert, 100, 1)

    in_meter <- value / conversion_factor
}

convert_to_time <- function(delta_velocity, acceleration) {

    stopifnot(is.numeric(delta_velocity))
    stopifnot(is.numeric(acceleration))

    # time = delta(velocity)/acceleration * 1000 to get milliseconds

    result_in_time <- (
        (delta_velocity / acceleration)
        * 1000 # To milliseconds
    )
}

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
