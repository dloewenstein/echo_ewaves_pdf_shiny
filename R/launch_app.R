#' Launch shiny app
#'
#' @return
#' @export
#'
#' @examples
#'
#'
launch_app <- function() {
    app_dir <- system.file("shinyApp", package = "ewavesPDFshiny")
    if(app_dir == "") {
        stop("Could not find shinyApp directory. Try reinstalling `ewavesPDFshiny`.",
             call. = FALSE)
    }
    shiny::runApp(app_dir, display.mode = "normal")
}
