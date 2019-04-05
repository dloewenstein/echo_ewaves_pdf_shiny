library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(purrr)
library(ewavesPDFshiny)
library(DT)
library(tidyr)

shiny_ui <- dashboardPage(
    skin = "blue",
    title = "EchoEwaves(dev)",
    sidebar = dashboardSidebar(disable = TRUE),
    header = dashboardHeader(title = "Echo E-Waves (dev)"),
    body = dashboardBody(
        tags$head(
            tags$link(rel = "icon", type = "image/x-icon", href = readLines("bookmark_icon.txt")),
            includeScript(system.file("java", "googleanalytics.js", package = "ewavesPDFshiny"))
        ),
        tags$script('
                    $(document).on("keydown", function (e) {
                    Shiny.setInputValue("enter", e.which);
                    });
                    '),
        tags$script(
            '$(document).on("keydown", function () {
            var logical_var = $("input#epeak_input").is(":focus");
            Shiny.setInputValue("epeak_focus",logical_var);})'
        ),
        tags$script(
            'Shiny.addCustomMessageHandler("refocus",
            function(NULL) {
            document.getElementById("at_input").select();
            document.getElementById("at_input").focus();
            });'
        ),
        tags$style(HTML("
                        input[type=number] {
                        -moz-appearance:textfield;
                        }
                        input[type=number]::{
                        -moz-appearance:textfield;
                        }
                        input[type=number]::-webkit-outer-spin-button,
                        input[type=number]::-webkit-inner-spin-button {
                        -webkit-appearance: none;
                        margin: 0;
                        }
                        ")),
        fluidRow(
            box(width  = 9,
                title  = "Results",
                status = "primary",
                solidHeader = TRUE,
                uiOutput("input_controls"),
                splitLayout(
                    cellWidths = c(120, 120),
                    radioButtons("input_type", "Input:",
                                 choices = c(Duration = "duration",
                                             Acceleration = "acceleration"),
                                 selected = "duration"),
                    radioButtons("input_units", "Units:",
                                 choices = c(m = "m",
                                             cm =  "cm"),
                                 selected = "m")
                ),
                splitLayout(
                    cellWidths = c(80, 220),
                    downloadButton("download","CSV"),
                radioButtons("export_selection", NULL,
                             choices = c(All = "all",
                                         Summary = "summary",
                                         Selection = "selection"),
                             inline = TRUE)
                ),
                div(style = 'overflow-x: auto', # Auto adds scrollbar for table
                DT::dataTableOutput("dataview")
                ),
                actionButton(
                    inputId = "delete",
                    label = "Delete selected",
                    icon = icon("trash")
                ),
                actionButton(
                    inputId = "clear",
                    label = "Clear",
                    icon = icon("trash")
                )
            )
        ),
        fluidRow(
            box(width=6,
                title="Lineplot",
                status="warning",
                solidHeader=TRUE,
                collapsible=TRUE,
                plotlyOutput("velocityplot")
            ),
            box(width=6,
                title="Scatterplot",
                status="warning",
                solidHeader=TRUE,
                collapsible=TRUE,
                plotlyOutput("scatterplot")
            )
        )
        )
)
