#-------------------------------------------
# server.R

library(shiny)
library(googledrive)

#-----------------------------------------------------------------------
# Useful functions

higienize <- function(txt) {
    out <- gsub("\\t", "@t", txt)
    out <- gsub("\\n", " ", out)
    out <- gsub("\"", "\\'", out)
    return(out)
}

verifica_nwords <- function(txt, nmax, nmin = 1L) {
    txt <- higienize(txt)
    words <- strsplit(txt, " ")[[1]]
    index <- length(words) <= nmax & length(words) >= nmin
    return(index)
}

verifica_campos <- function(...) {
    campos <- list(...)
    index <- vapply(campos, function(x) trimws(x) == "", TRUE)
    if (any(index)) {
        return("Todos os campos são obrigatórios. Por favor, preencha os campos em branco.")
    }
    return(1L)
}

verifica_cpf <- function(cpf) {
    out <- gsub("\\D", "", cpf)
    out <- strsplit(out, "")[[1L]]
    out <- as.integer(out)
    if (length(out) != 11) {
        return("CPF inválido. O CPF inserido não têm 11 digítos.")
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
        return("CPF inválido. Verifique seu CPF.")
    }
    return(1L)
}

organiza_cpf <- function(cpf) {
    out <- gsub("\\D", "", cpf)
    if (nchar(out) != 11) {
        return("CPF inválido. O CPF inserido não têm 11 digítos.")
    }
    pos <- seq(1, nchar(out), 3)
    out <- substring(out, pos, pos + 2)
    out <- paste(out, collapse = ".")
    out <- sub("\\.(\\d{2})$", "-\\1", out)
    return(out)
}

verifica_dup <- function(cpf, conjunto) {
    ind <- cpf %in% conjunto
    if (ind)
        return("<span class='warn'>Usuário já inscrito. O indivíduo com esse CPF já se inscreveu no evento.</span>")
    return(1L)
}

verifica_rec <- function(cpf, conjunto) {
    ind <- !(cpf %in% conjunto)
    if (ind)
        return("Usuário não inscrito. Por favor, realize sua inscrição antes de submeter seu trabalho.")
    return(1L)
}

shinyServer(
    function(input, output, session) {

        #-------------------------------------------
        # Consulta (input infos)
        output$UIconsulta <- renderUI({
            fluidRow(
                column(width = 4,
                       textInput(
                           inputId = "cpf3",
                           label = "CPF",
                           value = "",
                           width = "100%",
                           placeholder = "123.456.789-09")
                       ),
                column(width = 3,
                       HTML(text = '<div class="shiny-input-container"
                               style="margin-bottom: 4px">
                             <label class="control-label" for="category">
                               </label>
                             </div>'),
                           actionButton(inputId = "consultaButton",
                                        label = strong("Consultar"),
                                        class = "btn btn-primary",
                                        icon = icon("search"),
                                        width = "100%")
                           ),
                column(width = 5,
                       HTML(text = '<div class="shiny-input-container"
                               style="margin-bottom: 0px">
                             <label class="control-label" for="category">
                               Situação do usuário:</label>
                             </div>'),
                       htmlOutput("outputConsulta",
                                  class = "search-message")
                       )
            )
        })

        #-------------------------------------------
        # Inscricoes (input infos)
        output$UIinscricao <- renderUI({
            tagList(
                textInput(
                    inputId = "name",
                    label = "Nome completo",
                    value = "",
                    width = "100%",
                    placeholder = "Fulano Souza"),
                textInput(
                    inputId = "email",
                    label = "E-mail",
                    value = "",
                    width = "100%",
                    placeholder = "fsouza@example.com"),
                textInput(
                    inputId = "institute",
                    label = "Instituição/Empresa",
                    value = "",
                    width = "100%",
                    placeholder = "ESALQ/USP"),
                fluidRow(
                    column(width = 6,
                           textInput(
                               inputId = "cpf",
                               label = "CPF",
                               value = "",
                               width = "100%",
                               placeholder = "123.456.789-09")
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
                               Marque se participará do minicurso?</label>
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
        # output$UIsubmissao <- renderUI({
        #     tagList(
        #         fluidRow(
        #             column(width = 6,
        #                    textInput(
        #                        inputId = "speaker",
        #                        label = "Apresentador",
        #                        value = "",
        #                        width = "100%",
        #                        placeholder = "Fulano Souza")
        #                    ),
        #             column(width = 6,
        #                    textInput(
        #                        inputId = "cpf2",
        #                        label = "CPF",
        #                        value = "",
        #                        width = "100%",
        #                        placeholder = "123.456.789-09")
        #                    )
        #         ),
        #         textInput(
        #             inputId = "authors",
        #             label = "Autores",
        #             value = "",
        #             width = "100%",
        #             placeholder = paste("Fulano Souza",
        #                                 "Ciclano Braga",
        #                                 "Beltrano Neves",
        #                                 sep = "; ")
        #         ),
        #         textAreaInput(
        #             inputId = "title_pt",
        #             label = "Título (português)",
        #             value = "",
        #             height = "34px",
        #             placeholder = "Até 15 palavras",
        #             resize = "vertical"
        #         ),
        #         textAreaInput(
        #             inputId = "abstract_pt",
        #             label = "Resumo (português)",
        #             value = "",
        #             height = "204px",
        #             placeholder = "De 80 a 300 palavras",
        #             resize = "vertical"
        #         ),
        #         textAreaInput(
        #             inputId = "title_en",
        #             label = "Title (English)",
        #             value = "",
        #             height = "34px",
        #             placeholder = "Maximum 15 words",
        #             resize = "vertical"
        #         ),
        #         textAreaInput(
        #             inputId = "abstract_en",
        #             label = "Abstract (English)",
        #             value = "",
        #             height = "204px",
        #             placeholder = "Limited to 80 to 300 words",
        #             resize = "vertical"
        #         ),
        #         fluidRow(
        #             column(width = 6, offset = 3,
        #                    actionButton(inputId = "submissaoButton",
        #                                 label = strong("Submeter"),
        #                                 class = "btn btn-primary",
        #                                 icon = icon("save"),
        #                                 width = "100%")
        #                    ))
        #     )
        # })

        #-------------------------------------------
        # Consulta (search database)
        dfConsulta <- eventReactive(input$consultaButton, {
            # Read the current data
            download <- try(drive_download(
                file = "~/encontro2018/submissoes.csv",
                path = "./data/submissoes.csv",
                overwrite = TRUE,
                verbose = FALSE))
            if (any(class(download) %in% "try-error")) {
                return("Não foi possível conectar-se à base de dados. Tentar mais tarde ou contate Eduardo Jr <jreduardo@usp.br>.")
            }
            submi <- try(read.table(
                file = "./data/submissoes.csv",
                stringsAsFactors = FALSE,
                comment.char = "",
                header = TRUE,
                sep = "\t"))
            if (class(submi) == "try-error") {
                return("Não foi possível conectar-se à base de dados. Tentar mais tarde ou contate Eduardo Jr <jreduardo@usp.br>.")
            }
            download <- try(drive_download(
                file = "~/encontro2018/inscricoes.csv",
                path = "./data/inscricoes.csv",
                overwrite = TRUE,
                verbose = FALSE))
            if (any(class(download) %in% "try-error")) {
                return("Não foi possível conectar-se à base de dados. Tentar mais tarde ou contate Eduardo Jr <jreduardo@usp.br>.")
            }
            inscr <- try(read.table(
                file = "./data/inscricoes.csv",
                stringsAsFactors = FALSE,
                comment.char = "",
                header = TRUE,
                sep = "\t"))
            if (class(inscr) == "try-error") {
                return("Não foi possível conectar-se à base de dados. Tentar mais tarde ou contate Eduardo Jr <jreduardo@usp.br>.")
            }
            cpf <- organiza_cpf(trimws(input$cpf3))
            vcpf <- verifica_cpf(cpf)
            if (is.character(vcpf)) {
                return(vcpf)
            }
            if (cpf %in% inscr$cpf) {
                nam <- inscr$name[inscr$cpf == cpf]
                nam <- strsplit(nam, " ")[[1L]]
                msg <- sprintf("%s inscrito,",
                               paste(nam[c(1, length(nam))],
                                     collapse = " "))
                if (cpf %in% submi$cpf) {
                    return(paste(msg, "com trabalho submetido."))
                } else {
                    return(paste(msg, "sem trabalho submetido."))
                }
            } else {
                return("Usuário não inscrito.")
            }
        })
        # Output consulta
        output$outputConsulta <- renderPrint({
            cat(dfConsulta(), sep = "\n")
        })

        #-------------------------------------------
        # Grava as informações da inscrição
        dfInscricao <- eventReactive(input$inscricaoButton, {
            # Read the current data
            download <- try(drive_download(
                file = "~/encontro2018/inscricoes.csv",
                path = "./data/inscricoes.csv",
                overwrite = TRUE,
                verbose = FALSE))
            if (any(class(download) %in% "try-error")) {
                return("Não foi possível conectar-se à base de dados. Tentar mais tarde ou contate Eduardo Jr <jreduardo@usp.br>.")
            }
            dados <- try(read.table(
                file = "./data/inscricoes.csv",
                stringsAsFactors = FALSE,
                comment.char = "",
                header = TRUE,
                sep = "\t"))
            if (class(dados) == "try-error") {
                return("Não foi possível conectar-se à base de dados. Tentar mais tarde ou contate Eduardo Jr <jreduardo@usp.br>.")
            }
            # Verify fields
            vcam <- verifica_campos(input$name,
                                    input$email,
                                    input$institute,
                                    input$cpf)
            if (is.character(vcam)) return(vcam)
            # Organize infos
            infos <- data.frame(
                "id"        = paste0("#", nrow(dados) + 1L),
                "name"      = trimws(input$name),
                "email"     = trimws(input$email),
                "institute" = trimws(input$institute),
                "cpf"       = organiza_cpf(trimws(input$cpf)),
                "category"  = trimws(input$category),
                "course"    = trimws(input$course))
            # Verify infos
            vcpf <- verifica_cpf(infos$cpf)
            if (is.character(vcpf)) return(vcpf)
            vdup <- verifica_dup(infos$cpf, dados$cpf)
            if (is.character(vdup)) return(vdup)
            # Append infos to data and re-write the file
            dados <- rbind(dados, infos)
            writing <- try(write.table(
                dados,
                file = "./data/inscricoes.csv",
                quote = TRUE,
                row.names = FALSE,
                sep = "\t"))
            if (class(writing) == "try-error") {
                return("Não foi possível gravar os dados. Confira seus dados, se corretos contate Eduardo Jr <jreduardo@usp.br>.")
            }
            # Update the remote file
            update <- try(drive_update(
                file = "~/encontro2018/inscricoes.csv",
                media = "./data/inscricoes.csv",
                verbose = FALSE))
            if (any(class(update) %in% "try-error")) {
                return("Não foi possível conectar-se à base de dados. Tentar mais tarde ou contate Eduardo Jr <jreduardo@usp.br>.")
            }
            # Success message
            nam <- strsplit(input$name, " ")[[1L]]
            msg <- sprintf("Parabéns %s, inscrição realizada!",
                           paste(nam[c(1, length(nam))],
                                 collapse = " "))
            return(msg)
        })

        # #-------------------------------------------
        # # Grava as informações da submissão
        # dfSubmissao <- eventReactive(input$submissaoButton, {
        #     # Read the current data
        #     download <- try(drive_download(
        #         file = "~/encontro2018/submissoes.csv",
        #         path = "./data/submissoes.csv",
        #         overwrite = TRUE,
        #         verbose = FALSE))
        #     if (any(class(download) %in% "try-error")) {
        #         return("Não foi possível conectar-se à base de dados. Tentar mais tarde ou contate Eduardo Jr <jreduardo@usp.br>.")
        #     }
        #     dados <- try(read.table(
        #         file = "./data/submissoes.csv",
        #         stringsAsFactors = FALSE,
        #         comment.char = "",
        #         header = TRUE,
        #         sep = "\t"))
        #     if (class(dados) == "try-error") {
        #         return("Não foi possível conectar-se à base de dados. Tentar mais tarde ou contate Eduardo Jr <jreduardo@usp.br>.")
        #     }
        #     download <- try(drive_download(
        #         file = "~/encontro2018/inscricoes.csv",
        #         path = "./data/inscricoes.csv",
        #         overwrite = TRUE,
        #         verbose = FALSE))
        #     if (any(class(download) %in% "try-error")) {
        #         return("Não foi possível conectar-se à base de dados. Tentar mais tarde ou contate Eduardo Jr <jreduardo@usp.br>.")
        #     }
        #     inscr <- try(read.table(
        #         file = "./data/inscricoes.csv",
        #         stringsAsFactors = FALSE,
        #         comment.char = "",
        #         header = TRUE,
        #         sep = "\t"))
        #     if (class(inscr) == "try-error") {
        #         return("Não foi possível conectar-se à base de dados. Tentar mais tarde ou contate Eduardo Jr <jreduardo@usp.br>.")
        #     }
        #     # Verify fields
        #     vcam <- verifica_campos(input$speaker,
        #                             input$cpf2,
        #                             input$authors,
        #                             input$title_pt,
        #                             input$title_en,
        #                             input$abstract_pt,
        #                             input$abstract_en)
        #     if (is.character(vcam)) return(vcam)
        #     # Organize infos
        #     infos <- data.frame(
        #         "speaker"     = trimws(input$speaker),
        #         "cpf"         = organiza_cpf(trimws(input$cpf2)),
        #         "authors"     = trimws(input$authors),
        #         "title_pt"    = higienize(trimws(input$title_pt)),
        #         "title_en"    = higienize(trimws(input$title_en)),
        #         "abstract_pt" = higienize(trimws(input$abstract_pt)),
        #         "abstract_en" = higienize(trimws(input$abstract_en)))
        #     # Verify infos
        #     vcpf <- verifica_cpf(infos$cpf)
        #     if (is.character(vcpf)) return(vcpf)
        #     vrec <- verifica_rec(infos$cpf, inscr$cpf)
        #     if (is.character(vrec)) return(vrec)
        #     ind_title_pt <- verifica_nwords(infos$title_pt,
        #                                     nmax = 15, nmin = 1)
        #     if (!ind_title_pt) {
        #         return("O título do trabalho (português) não deve ultrapassar 15 palavras.")
        #     }
        #     ind_title_en <- verifica_nwords(infos$title_en,
        #                                     nmax = 15, nmin = 1)
        #     if (!ind_title_en) {
        #         return("O título do trabalho (inglês) não deve ultrapassar 15 palavras.")
        #     }
        #     ind_abstract_pt <- verifica_nwords(infos$abstract_pt,
        #                                        nmax = 300, nmin = 80)
        #     if (!ind_abstract_pt) {
        #         return("O resumo do trabalho (português) deve ser entre 80 e 300 palavras.")
        #     }
        #     ind_abstract_en <- verifica_nwords(infos$abstract_en,
        #                                        nmax = 300, nmin = 80)
        #     if (!ind_abstract_en) {
        #         return("O resumo do trabalho (inglês) deve ser entre 80 e 300 palavras.")
        #     }
        #     # Append infos to data and re-write the file
        #     dados <- rbind(dados, infos)
        #     writing <- try(write.table(
        #         dados,
        #         file = "./data/submissoes.csv",
        #         quote = TRUE,
        #         row.names = FALSE,
        #         sep = "\t"))
        #     if (class(writing) == "try-error") {
        #         return("Não foi possível gravar os dados. Confira seus dados, se corretos contate Eduardo Jr <jreduardo@usp.br>.")
        #     }
        #     # Update the remote file
        #     update <- try(drive_update(
        #         file = "~/encontro2018/submissoes.csv",
        #         media = "./data/submissoes.csv",
        #         verbose = FALSE))
        #     if (any(class(update) %in% "try-error")) {
        #         return("Não foi possível conectar-se à base de dados. Tentar mais tarde ou contate Eduardo Jr <jreduardo@usp.br>.")
        #     }
        #     # Success message
        #     nam <- strsplit(input$speaker, " ")[[1L]]
        #     msg <- sprintf("Parabéns %s, sua proposta de comunicação oral foi submetida!",
        #                    paste(nam[c(1, length(nam))],
        #                          collapse = " "))
        #     return(msg)
        # })

        #-------------------------------------------
        # Imprime mensagem após submeter inscrição
        output$outputInscricao <- renderPrint({
            imessage <- dfInscricao()
            if (grepl("Parabéns", imessage)) {
                cat(sprintf("<span class='sucess'> %s </span>",
                            imessage), sep = "\n")
            } else {
                cat(imessage, sep = "\n")
            }
        })

        # #-------------------------------------------
        # # Imprime mensagem após submeter trabalho
        # output$outputSubmissao <- renderPrint({
        #     imessage <- dfSubmissao()
        #     if (grepl("Parabéns", imessage)) {
        #         cat(sprintf("<span class='sucess'> %s </span>",
        #                     imessage), sep = "\n")
        #     } else {
        #         cat(imessage, sep = "\n")
        #     }
        # })

        #-------------------------------------------
        # Teste
        output$test <- renderPrint({
            input$name
        })
    }
)
