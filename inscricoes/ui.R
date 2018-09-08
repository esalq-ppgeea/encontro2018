#-------------------------------------------
# ui.R

library(shiny)
library(rmarkdown)

infoInscricao <-
"A inscrição para o VIII Encontro dos Alunos em Estatística e
 Experimentação a ser realizado nos dias 21, 22 e 23 de novembro é
 realizada por meio desse
 <a href='https://eduardojr.shinyapps.io/encontro2018/'>formulário</a>), que ficará
 disponível até o dia 01/11. Todavia, sua inscrição só será confirmada
 após pagamento da taxa de inscrição no valor de R$20,00
 (<a href='https://esalq-ppgeea.github.io/encontro2018/datas-importantes'>formas de pagamento</a>)."

infoSubmissao <-
"Estão abertas as submissões para contribuição de artigos a serem
 apresentados em formato de comunicação oral (20min, sendo 17min para
 apresentação e 3 para perguntas). O objetivo geral do evento será a
 divulgação de trabalhos que explorem, promovam e estendam métodos
 aplicados da Estatística. São solicitados apenas o título e resumo do
 trabalho (português e inglês). Ao submeter seu trabalho, permite-se a
 publicação do título e resumo nos anis do evento."

infoConsulta <-
"Verifique se já está inscrito no evento."

shinyUI(fluidPage(
    title = "Inscrições-VIIIEncontro",
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
    # verbatimTextOutput("test"),

    #-------------------------------------------
    # Consulta
    div(HTML(infoConsulta), style = "margin-bottom: 10px"),
    uiOutput("UIconsulta"),

    #-------------------------------------------
    # Inscricoes
    # div(class = "chart-title", h4(strong("Inscrição no evento"))),
    h3("Inscrição no evento"),
    div(HTML(infoInscricao), style = "margin-bottom: 20px"),
    uiOutput("UIinscricao"),
    div("Ao clicar, aguarde a mensagem de confirmação.",
        class = "wait-message"),
    br(),
    htmlOutput("outputInscricao", class = "record-message"),
    hr(style = "border: 2px solid #eee"),
    #-------------------------------------------
    # Inscricoes
    # div(class = "chart-title", h4(strong("Submissão de trabalho"))),
    h3("Submissão de trabalho"),
    div(HTML(infoSubmissao), style = "margin-bottom: 20px"),
    uiOutput("UIsubmissao"),
    div("Ao clicar, aguarde a mensagem de confirmação.",
        class = "wait-message"),
    br(),
    htmlOutput("outputSubmissao", class = "record-message"),
    hr(style = "border: 2px solid #eee")
))
