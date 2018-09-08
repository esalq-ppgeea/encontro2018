#-------------------------------------------
# ui.R

library(shiny)

shinyUI(fluidPage(
    #-------------------------------------------
    # CSS Style
    tags$head(tags$link(
                  rel = "stylesheet",
                  href = "https://fonts.googleapis.com/css?family=Gentium+Basic|Merriweather|Ubuntu+Mono"),
              tags$link(
                  rel = "stylesheet",
                  type = "text/css",
                  href = "style.css")
              ),
    #-------------------------------------------
    # Inscricoes
    # div(class = "chart-title", h4(strong("Inscrição no evento"))),
    h3("Inscrição no evento"),
    uiOutput("UIinscricao"),
    br(),
    verbatimTextOutput("outputInscricao"),
    hr(style = "border: 2px solid #eee"),
    #-------------------------------------------
    # Inscricoes
    # div(class = "chart-title", h4(strong("Submissão de trabalho"))),
    h3("Submissão de trabalho"),
    uiOutput("UIsubmissao"),
    br(),
    verbatimTextOutput("outputSubmissao"),
    hr(style = "border: 2px solid #eee")
))
