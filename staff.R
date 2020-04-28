
# __devtools workflow__

library(devtools)
library(available)
library(sinew)

# pesquisar por nome
available('getLattes')

# posso inicializar a criação de um pacote dentro de uma pasta que estou trabalhando
create_package("~/OneDrive/Rworkspace/getLattes")

load_all()          # para carregar todas as funções em `R/`

check()             # identificar se o pacote como um todo funciona, não apenas as funções do pacote

use_mit_license() 

makeOxyFile("R/getAreasAtuacao.R") # Skeleton

document()          # converter os comentários do `roxygen2` em documentação `.Rd`

# `DESCRIPTION` editar o arquivo de descrição do pacote 

install()           # instalar o pacote no R

use_github()        # push to github

use_readme_rmd()    # create a read me file, to render: `rmarkdown::render('README.Rmd')`

# The end: 
document()
check() 
install()

# _______________________________ 


data(lattesID)
data(lattesXML)

head(lattesID)
head(lattesXML)
length(lattesXML)

bind_cols
bind_rows
str_c

#' @importFrom dplyr group_by arrange mutate
#' @importFrom pipeR "%>>%"
#' @importFrom stringr str_c

#' @importFrom dplyr bind_cols bind_rows  mutate

#' @importFrom pipeR "%>>%"
#' @importFrom stringr str_c

#' @importFrom dplyr bind_rows

#' @importFrom tibble as_tibble
#' @importFrom rlang .data


rmarkdown::render('README.Rmd')

