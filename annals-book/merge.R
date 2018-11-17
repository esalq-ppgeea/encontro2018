# =====================================================================
#
#                                                        Eduardo Junior
#                                                    edujrrib@gmail.com
#                                                            2018-11-07
# =====================================================================

#-----------------------------------------------------------------------
# Packages
library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(magrittr)

#-----------------------------------------------------------------------
# Functions
name_to_title <- function(x) {
    xvec <- x %>%
        str_to_lower() %>%
        str_split(" ") %>%
        flatten_chr()
    index <- str_detect(xvec, "^d?(a|e|i|o|u)s?$")
    xvec[!index] <- str_to_title(xvec[!index])
    str_c(xvec, collapse = " ")
}
name_to_cite <- function(x) {
    aux <- x %>%
        str_to_lower() %>%
        str_replace("( d?(a|e|i|o|u)s? )", " ") %>%
        str_replace("junior", "jr") %>%
        str_to_title() %>%
        str_split(" ") %>%
        flatten_chr()
    index <- str_detect(aux, "(Jr|Filho|Neto)")
    xvec <- aux[!index]
    k <- length(xvec)
    initials <- xvec[-k] %>%
        str_sub(1, 1) %>%
        str_c(collapse = ".") %>%
        str_c(".")
    family_name <- str_c(xvec[k], aux[index], sep = " ")
    str_c(family_name, initials, sep = ", ")
}
collapse_names <- function(x) {
    n <- length(x)
    str_c(x[-n], collapse = ", ") %>%
        str_c(x[n], sep = " \\\\& ")
}
build_template_tex <- function(authors,
                               title_pt,
                               title_en,
                               abstract_pt,
                               abstract_en,
                               template = read_lines("template_co.tex"),
                               ...) {
    parsed_names <- authors %>%
        str_split("(,|;)") %>%
        flatten_chr() %>%
        str_trim()
    author_name <- parsed_names  %>%
        map_chr(name_to_title) %>%
        collapse_names()
    author_cite <- parsed_names  %>%
        map_chr(name_to_cite) %>%
        collapse_names()
    template %>%
        str_replace("##PT-TITLE##", title_pt) %>%
        str_replace("##EN-TITLE##", title_en) %>%
        str_replace("##PT-ABSTRACT##", abstract_pt) %>%
        str_replace("##EN-ABSTRACT##", abstract_en) %>%
        str_replace("##SHORT-AUTHORS##", author_cite) %>%
        str_replace("##AUTHORS##", author_name)
}

#-----------------------------------------------------------------------
# Read data, build tex templates and replace into tex

# Read data
submissoes <- read_tsv("../data/submissoes.csv",
                       col_types = cols(.default = "c"))
inscricoes <- read_tsv("../data/inscricoes.csv",
                       col_types = cols(.default = "c"))

# Add referees
submissoes <- submissoes %>%
    mutate(referee = c("Thiago", "Renata", "Rafael", "Clarice",
                       "Renata", "Thiago", "Idemauro", "Clarice",
                       "Idemauro", "Rafael", "Thiago"))

# Build oral communications .tex
body_communication <- submissoes %>%
    # filter(referee == "Thiago") %>%
    pmap(build_template_tex,
         template = read_lines("template_co.tex")) %>%
    flatten_chr()

# Build participants list
body_participants <- inscricoes %>%
    select(name, institute) %>%
    arrange(name) %>%
    mutate(name = map_chr(name, name_to_title),
           institute = map_chr(institute, str_to_upper)) %$%
    str_glue("\\item {name} ({institute})")

# Read full document and replace
partial_latex <- read_lines("template.tex")
line_subm <- str_which(partial_latex, "%##BODY-COMMUNICATIONS##%")
line_insc <- str_which(partial_latex, "%##BODY-PARTICIPANTS##%")
line_last <- length(partial_latex)

full_latex <- c(partial_latex[1:line_subm],
                body_communication,
                partial_latex[line_subm:line_insc],
                body_participants,
                partial_latex[line_insc:line_last])

# Write the document
write_lines(full_latex, "annals.tex")

#-------------------------------------------
# End
