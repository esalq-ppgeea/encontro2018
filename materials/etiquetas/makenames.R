#-----------------------------------------------------------------------
# Packages
library(readr)
library(purrr)
library(dplyr)
library(stringr)

#-----------------------------------------------------------------------
# Functions

aux_template <- "
\\documentclass{article}
\\usepackage{geometry}
\\usepackage[utf8]{inputenc}
\\usepackage[brazil]{babel}
\\usepackage[T1]{fontenc}
\\usepackage{newcent}
\\geometry{
  paperwidth=8.0cm,
  paperheight=2.8cm,
  margin=0mm
}
\\begin{document}
\\topskip0pt
\\vspace*{\\fill}
\\centering
\\huge
\\textbf{-NAME-} \\\\
\\large -INSTITUTE-
\\vspace*{0.45cm}
\\end{document}
"

make_rnw <- function(name, institute, id,
                     template = readLines(textConnection(aux_template))
                     ) {
    filename <- str_c("etiqueta", id, ".Rnw")
    template %>%
        str_replace("-NAME-", name) %>%
        str_replace("-INSTITUTE-", institute) %>%
        write_lines(path = filename)
    knitr::knit2pdf(filename)
    file.remove(filename, str_replace(filename, ".Rnw", ".tex"))
    invisible()
}

name_to_short <- function(x) {
    aux <- x %>%
        str_to_upper() %>%
        str_replace("JUNIOR", "JR") %>%
        str_split(" ") %>%
        flatten_chr()
    index <- str_detect(aux, "(JR|FILHO|NETO)")
    xvec <- aux[!index]
    k <- length(xvec)
    family_name <- str_c(xvec[k], aux[index], sep = " ")
    str_c(xvec[1], family_name, sep = "\\\\\\\\[-0.2cm] ")
}

#-----------------------------------------------------------------------
# Read participants
inscricoes <- "../../data/inscricoes.csv" %>%
    read_tsv(col_types = cols(.default = "c")) %>%
    select(name, institute) %>%
    mutate(name = map_chr(name, name_to_short),
           institute = str_to_upper(institute))
inscricoes

#-----------------------------------------------------------------------
# Compile the tag names
inscricoes %>%
    mutate(id = 1:n()) %>%
    pwalk(.f = make_rnw)

#-----------------------------------------------------------------------
# Merge all pdf's
if(file.exists("all-etiquetas.pdf")) file.remove("all-etiquetas.pdf")
system("pdftk *.pdf cat output all-etiquetas.pdf")
