#-----------------------------------------------------------------------
# Pacotes
library(gmailr)
library(readODS)
library(dplyr)
library(stringr)
library(purrr)

#-----------------------------------------------------------------------
# Functions
enviar_email <- function(name, email, message, ...) {
    first <- str_split(name, " ")[[1]][1]
    body <- str_replace(message, "-NAME-", name)
    mail <- mime(
        To = email,
        From = "jreduardo@usp.br",
        Subject = "Pagamento VIII Encontro dos Alunos",
        body = body
    )
    send_message(mail)
    invisible()
}

#-----------------------------------------------------------------------
# Read data
pagamentos <-
    read_ods("../data/pagamentos.ods") %>%
    as_tibble() %>%
    select(name, email, pgto) %>%
    filter(pgto == "Não") %>%
    mutate(name = str_to_title(name))

# Correct invalid e-mails
pagamentos[1, "email"] <- "araaramirez@usp.br"
pagamentos[9, "email"] <- "igor.engler.lima@usp.br"

#-----------------------------------------------------------------------
# Message text
msg <- "
Prezado(a) -NAME-,

Não identificamos o seu pagamento para participação no VIII
Encontro dos Alunos (https://esalq-ppgeea.github.io/encontro2018/).
Por favor, realize o pagamento o quanto antes (agência 5930-7,
conta 1.876-7, titular Eduardo E Jr) e envie o comprovante como
resposta a esse e-mail.

Obrigado e te esperamos no evento!
"

#-----------------------------------------------------------------------
# Send emails
pagamentos %>%
    pwalk(enviar_email, message = msg)
