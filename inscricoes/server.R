#-------------------------------------------
# server.R

library(shiny)

verifica_nwords <- function(txt, nwords) {
    words <- strsplit(txt, " ")[[1]]
    return(length(words) <= nwords)
}

verifica_campos <- function(...) {
    campos <- list(...)
    index <- vapply(campos, function(x) trimws(x) == "", TRUE)
    if (any(index)) {
        stop("Todos os campos são obrigatórios. Por favor, preenchê-los")
    }
    return(1L)
}

verifica_cpf <- function(cpf) {
    out <- gsub("\\D", "", cpf)
    out <- strsplit(out, "")[[1L]]
    out <- as.integer(out)
    if (length(out) != 11) {
        stop("CPF inválido")
    }
    #-------------------------------------------
    test <- rev(rev(out)[-(1:2)])
    digs <- rev(rev(out)[+(1:2)])
    #-------------------------------------------
    resto_d1 <- sum(test * (10:2)) %% 11
    d1 <- ifelse(resto_d1 %in% 0:1, 0, 11 - resto_d1)
    resto_d2 <- sum(c(test, d1) * (11:2)) %% 11
    d2 <- ifelse(resto_d2 %in% 0:1, 0, 11 - resto_d2)
    #-------------------------------------------
    if (!all(c(d1, d2) == digs)) {
        stop("CPF inválido")
    }
    return(1L)
}

organiza_cpf <- function(cpf) {
    out <- gsub("\\D", "", cpf)
    if (nchar(out) != 11) {
        stop("CPF inválido")
    }
    pos <- seq(1, nchar(out), 3)
    out <- substring(out, pos, pos + 2)
    out <- paste(out, collapse = ".")
    out <- sub("\\.(\\d{2})$", "-\\1", out)
    return(out)
}

verifica_dup <- function(cpf, conjunto) {
    ind <- cpf %in% conjunto
    if (ind) stop("Usuário já inscrito")
    return(1L)
}

verifica_rec <- function(cpf, conjunto) {
    ind <- !(cpf %in% conjunto)
    if (ind) stop("Usuário ainda não inscrito!")
    return(1L)
}

shinyServer(
    function(input, output, session) {
        #-------------------------------------------
        # Inscricoes (input infos)
        output$UIinscricao <- renderUI({
            tagList(
                textInput(
                    inputId = "name",
                    label = "Nome completo",
                    value = "",
                    width = "100%"),
                textInput(
                    inputId = "institute",
                    label = "Instituição/Empresa",
                    value = "",
                    width = "100%"),
                fluidRow(
                    column(width = 6,
                           textInput(
                               inputId = "cpf",
                               label = "CPF",
                               value = "",
                               width = "100%")
                           ),
                    column(width = 6,
                           selectInput(
                               inputId = "category",
                               label = "Categoria",
                               choices = c("Aluno de graduação",
                                           "Aluno de pós-graduação",
                                           "Docente/Pesquisador",
                                           "Profissional"),
                               width = "100%")
                           )),
                HTML(text = '<div class="shiny-input-container"
                          style="margin-bottom: -10px">
                       <label class="control-label" for="category">
                       Participará do minicurso?</label>
                     </div>'),
                checkboxInput(
                    inputId = "course",
                    label = "Modelos Não Lineares - Walmes Zeviani (LEG/UFPR)",
                    width = "100%"
                ),
                fluidRow(
                    column(width = 6, offset = 3,
                           actionButton(inputId = "inscricaoButton",
                                        label = strong("Inscrever"),
                                        class = "btn btn-primary",
                                        icon = icon("save"),
                                        width = "100%")
                           ))
            )
        })
        #-------------------------------------------
        # Submissao (input infos)
        output$UIsubmissao <- renderUI({
            tagList(
                fluidRow(
                    column(width = 6,
                           textInput(
                               inputId = "speaker",
                               label = "Apresentador",
                               value = "",
                               width = "100%")
                           ),
                    column(width = 6,
                           textInput(
                               inputId = "cpf2",
                               label = "CPF",
                               value = "",
                               width = "100%")
                           )
                ),
                textInput(
                    inputId = "authors",
                    label = "Autores",
                    value = "",
                    width = "100%"
                ),
                textAreaInput(
                    inputId = "title_pt",
                    label = "Título (português)",
                    value = "",
                    height = "34px"
                ),
                textAreaInput(
                    inputId = "abstract_pt",
                    label = "Resumo (português)",
                    value = "",
                    height = "136px"
                ),
                textAreaInput(
                    inputId = "title_en",
                    label = "Title (English)",
                    value = "",
                    height = "34px"
                ),
                textAreaInput(
                    inputId = "abstract_en",
                    label = "Abstract (English)",
                    value = "",
                    height = "136px"
                ),
                fluidRow(
                    column(width = 6, offset = 3,
                           actionButton(inputId = "submissaoButton",
                                        label = strong("Submeter"),
                                        class = "btn btn-primary",
                                        icon = icon("save"),
                                        width = "100%")
                           ))
            )
        })
        #-------------------------------------------
        # Grava as informações da inscrição
        dfInscricao <- eventReactive(input$inscricaoButton, {
            # Read the current data
            dados <- read.table(
                file = "./data/inscricoes.csv",
                comment.char = "",
                header = TRUE,
                sep = "\t")
            # Verify fields
            verifica_campos(input$name,
                            input$institute,
                            input$cpf)
            # Organize infos
            infos <- data.frame(
                "id"        = paste0("#", nrow(dados) + 1L),
                "name"      = trimws(input$name),
                "institute" = trimws(input$institute),
                "cpf"       = organiza_cpf(trimws(input$cpf)),
                "category"  = trimws(input$category),
                "course"    = trimws(input$course))
            # Verify infos
            verifica_cpf(infos$cpf)
            verifica_dup(infos$cpf, dados$cpf)
            # Append infos to data and re-write the file
            dados <- rbind(dados, infos)
            write.table(dados,
                        file = "./data/inscricoes.csv",
                        quote = FALSE,
                        row.names = FALSE,
                        sep = "\t")
            # Success message
            nam <- strsplit(input$name, " ")[[1L]]
            msg <- sprintf("Parabéns %s, inscrição realizada!",
                           paste(nam[c(1, length(nam))],
                                 collapse = " "))
            return(cat(msg, sep = "\n"))
        })
        #-------------------------------------------
        # Grava as informações da submissão
        dfSubmissao <- eventReactive(input$submissaoButton, {
            # Read the current data
            dados <- read.table(
                file = "./data/submissoes.csv",
                comment.char = "",
                header = TRUE,
                sep = "\t")
            inscr <- read.table(
                file = "./data/inscricoes.csv",
                comment.char = "",
                header = TRUE,
                sep = "\t")
            # Verify fields
            verifica_campos(input$speaker,
                            input$cpf2,
                            input$authors,
                            input$title_pt,
                            input$title_en,
                            input$abstract_pt,
                            input$abstract_en)
            # Organize infos
            infos <- data.frame(
                "speaker"     = trimws(input$speaker),
                "cpf"         = organiza_cpf(trimws(input$cpf2)),
                "authors"     = trimws(input$authors),
                "title_pt"    = gsub("\\t", "t", trimws(input$title_pt)),
                "title_en"    = gsub("\\t", "t", trimws(input$title_en)),
                "abstract_pt" = gsub("\\t", "t", trimws(input$abstract_pt)),
                "abstract_en" = gsub("\\t", "t", trimws(input$abstract_en)))
            # Verify infos
            verifica_cpf(infos$cpf)
            verifica_rec(infos$cpf, inscr$cpf)
            ind_title <- c(
                verifica_nwords(trimws(input$title_pt), 15),
                verifica_nwords(trimws(input$title_en), 15))
            if (!all(ind_title)) {
                stop("O título do trabalho (em português e inglês) não deve ultrapassar 15 palavras.")
            }
            ind_abstract <- c(
                verifica_nwords(trimws(input$abstract_pt), 300),
                verifica_nwords(trimws(input$abstract_en), 300))
            if (!all(ind_title)) {
                stop("O título do trabalho (em português e inglês) não deve ultrapassar 15 palavras.")
            }

            # Append infos to data and re-write the file
            dados <- rbind(dados, infos)
            write.table(dados,
                        file = "./data/submissoes.csv",
                        quote = FALSE,
                        row.names = FALSE,
                        sep = "\t")
            # Success message
            nam <- strsplit(input$name, " ")[[1L]]
            msg <- sprintf("Parabéns %s, sua proposta de comunicação oral foi submetida!",
                           paste(nam[c(1, length(nam))],
                                 collapse = " "))
            return(cat(msg, sep = "\n"))
        })
        #-------------------------------------------
        # Imprime mensagem após submeter inscrição
        output$outputInscricao <- renderPrint({
            dfInscricao()
        })
        #-------------------------------------------
        # Imprime mensagem após submeter trabalho
        output$outputSubmissao <- renderPrint({
            dfSubmissao()
        })
        #-------------------------------------------
        # Teste
        output$test <- renderPrint({
            # verifica_campos(input$name, input$institute, input$cpf)
        })
    }
)
