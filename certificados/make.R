#-----------------------------------------------------------------------
# Packages
library(readODS)
library(stringr)
library(dplyr)
library(purrr)
library(readr)

#-----------------------------------------------------------------------
# Function
names_to_authors <- function(string) {
    map_chr(string, function(char) {
        x <-
            char %>%
            str_split(";|,") %>%
            flatten_chr() %>%
            str_trim()
        n <- length(x)
        str_c(str_c(x[-n], collapse = ", "), x[n],
              sep = " e ")
    })
}

#-----------------------------------------------------------------------
# Text templates

# Text to the participation
text_participation <-
"
    Certificamos que \\textbf{-NAME-} participou de \\textbf{-FREQ-\\%}
    das atividades planejadas no VIII Encontro dos Alunos em Estatística
    e Experimentação Agronômica, realizado na Escola Superior de
    Agricultura Luiz de Queiroz, no período de 21 a 23 de novembro de
    2018.
"

# Text to the short course
text_course <-
"
    Certificamos que \\textbf{-NAME-} participou, com \\textbf{-FREQ-\\%}
    de frequência, do minicurso \\textbf{Modelos de Regressão Não
    Linear: teoria e aplicações} ministrado pelo Prof. Dr. Walmes
    Zeviani no VIII Encontro dos Alunos em Estatística e Experimentação
    Agronômica, realizado na Escola Superior de Agricultura Luiz de
    Queiroz, no período de 21 a 23 de novembro de 2018.
"

# Text to the oral communications
text_co <-
"
    Certificamos que o trabalho intitulado \\textbf{-TITLE-} de autoria
    de -AUTHORS- foi apresentado em formato de comunicação oral por
    \\textbf{-SPEAKER-} no VIII Encontro dos Alunos em Estatística e
    Experimentação Agronômica, realizado na Escola Superior de
    Agricultura Luiz de Queiroz, no período de 21 a 23 de novembro de
    2018.
"

# Function to replace key parts
make_tex <- function(name, freq, base_text, ...,
                     template = readLines("template.tex")) {
    bodytext <-
        base_text %>%
        str_replace("-NAME-", name) %>%
        str_replace("-FREQ-", freq) %>%
        str_replace_all("\\\\", "\\\\\\\\")
    template %>%
        str_replace("%-BODYTEXT-%", bodytext)
}

make_texco <- function(speaker, authors, title, base_text, ...,
                       template = readLines("template.tex")) {
    bodytext <-
        base_text %>%
        str_replace("-SPEAKER-", speaker) %>%
        str_replace("-AUTHORS-", authors) %>%
        str_replace("-TITLE-", title) %>%
        str_replace_all("\\\\", "\\\\\\\\")
    template %>%
        str_replace("%-BODYTEXT-%", bodytext)
}

# Function to compile .tex files
compile_tex <- function(text, prefix, sufix, remove = TRUE) {
    file_name <- str_c(prefix, sufix, ".tex")
    writeLines(text = text, con = file_name)
    tinytex::xelatex(file_name)
    if (remove) file.remove(file_name)
    invisible(str_replace(file_name, "(tex)$", "pdf"))
}

# Function to pmap
generate_pdf <- function(makefun, prefix, sufix, remove = TRUE, ...) {
    text <- makefun(...)
    file_name <- compile_tex(text, prefix, sufix, remove = remove)
    invisible(file_name)
}

#-----------------------------------------------------------------------
# Read data and compute frequency
presenca <-
    read_ods("../data/presenca.ods") %>%
    as_tibble() %>%
    select(name, course, starts_with("d")) %>%
    mutate(name = str_to_title(name)) %>%
    mutate_at(vars(starts_with("d")),
              funs(case_when(. == "Sim" ~ 1,
                             . == "Não" ~ 0))) %>%
    mutate(freq_course = (d21m + d22m) * 50,
           freq_evento = (d21m + d21t + d22t + d23m) * 25) %>%
    select(name, course, starts_with("freq"))

#-----------------------------------------------------------------------
# Read data from oral presentations
presentations <-
    read_delim("../data/submissoes.csv", delim = "\t") %>%
    select(speaker, authors, title_pt) %>%
    mutate(authors = names_to_authors(authors))

#=======================================================================
# Make the certificate of participation
#=======================================================================
files <-
    presenca %>%
    filter(freq_evento > 0) %>%
    mutate(freq = as.character(freq_evento),
           sufix = 1:n()) %>%
    pmap(generate_pdf,
         makefun = make_tex,
         prefix = "cert_part_",
         base_text = text_participation)


# Merge pdf's and remove individual ones
pdfs <- str_c(files, collapse = " ")
com0 <- str_glue("pdftk {pdfs} cat output certificate_participation.pdf")
com1 <- str_glue("rm {pdfs}")
system(com0)
system(com1)

#=======================================================================
# Make the certificate of short course
#=======================================================================
files <-
    presenca %>%
    filter(freq_course > 0 & course) %>%
    mutate(freq = as.character(freq_course),
           sufix = 1:n()) %>%
    pmap(generate_pdf,
         makefun = make_tex,
         prefix = "cert_course_",
         base_text = text_course)

# Merge pdf's and remove individual ones
pdfs <- str_c(files, collapse = " ")
com0 <- str_glue("pdftk {pdfs} cat output certificate_shortcourse.pdf")
com1 <- str_glue("rm {pdfs}")
system(com0)
system(com1)

#=======================================================================
# Make the certificate of oral communication
#=======================================================================
files <-
    presentations %>%
    arrange(speaker) %>%
    mutate(title = title_pt,
           sufix = 1:n()) %>%
    pmap(generate_pdf,
         makefun = make_texco,
         prefix = "cert_co_",
         base_text = text_co)

# Merge pdf's and remove individual ones
pdfs <- str_c(files, collapse = " ")
com0 <- str_glue("pdftk {pdfs} cat output certificate_oralcommunications.pdf")
com1 <- str_glue("rm {pdfs}")
system(com0)
system(com1)
