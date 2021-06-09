
<!-- README.md is generated from README.Rmd. Please edit that file -->

# getLattes

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/258844181.svg)](https://zenodo.org/badge/latestdoi/258844181)
<!-- badges: end -->

The `getLattes` `R` package, written by [Roney Fraga
Souza](http://roneyfraga.com) and [Winicius
Sabino](https://stackoverflow.com/users/9278241/winicius-sabino), was
built to extract data from the [Lattes](http://lattes.cnpq.br/)
curriculum platform exported as `XML`.

![](http://roneyfraga.com/volume/getLattes_data/lattes_xml_download.gif)

The `XML` file needs to be extracted from `.zip`.

To automate the download process, please see [Captchas Negated by Python
reQuests - CNPQ](https://github.com/josefson/CNPQ).

## Installation

Stable version from [CRAN](https://cran.r-project.org/).

``` r
# install.packages('getLattes')
# library(getLattes)
```

Development version from
[GitHub](https://github.com/roneyfraga/getLattes).

``` r
# install and load devtools from CRAN
install.packages("devtools")
library(devtools)

# install and load getLattes
devtools::install_github("roneyfraga/getLattes")
library(getLattes)
```

## Import XML file

``` r
curriculo <- read_xml('data/4984859173592703.zip')
```

## Extract data

``` r
# to import from one curriculum 
getDadosGerais(curriculo)
getArtigosPublicados(curriculo)
getAreasAtuacao(curriculo)
getArtigosPublicados(curriculo)
getAtuacoesProfissionais(curriculo)
getBancasDoutorado(curriculo)
getBancasGraduacao(curriculo)
getBancasMestrado(curriculo)
getCapitulosLivros(curriculo)
getDadosGerais(curriculo)
getEnderecoProfissional(curriculo)
getEventosCongressos(curriculo)
getFormacaoDoutorado(curriculo)
getFormacaoMestrado(curriculo)
getFormacaoGraduacao(curriculo)
getIdiomas(curriculo)
getLinhaPesquisa(curriculo)
getLivrosPublicados(curriculo)
getOrganizacaoEventos(curriculo)
getOrientacoesDoutorado(curriculo)
getOrientacoesMestrado(curriculo)
getOrientacoesPosDoutorado(curriculo)
getOutrasProducoesTecnicas(curriculo)
getParticipacaoProjeto(curriculo)
getProducaoTecnica(curriculo)
getId(curriculo)
```
