#' Launch shiny app
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
launch_app <- function() {
    required_pkgs <- c(
        "shiny",
        "shinydashboard",
        "DT",
        "ggplot2",
        "plotly",
        "dplyr",
        "tidyr",
        "broom"
    )
    
    shinyApp(ui = shiny_ui, server = shiny_server)
}