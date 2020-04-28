
<!-- README.md is generated from README.Rmd. Please edit that file -->

# getLattes

<!-- badges: start -->

<!-- badges: end -->

The `getLattes` `R` package, written by [Roney Fraga
Souza](roneyfraga.com) and [Winicius
Sabino](https://stackoverflow.com/users/9278241/winicius-sabino), was
built to extract data from the [Lattes](http://lattes.cnpq.br/)
curriculum platform exported as `XML`.

![](docs/lattes_xml_download.gif)

The `XML` file needs to be extracted from `.zip`.

To automate the download process, please see
[CNPQ](https://github.com/josefson/CNPQ).

## Installation

You can install the released version of getLattes from
[github](https://CRAN.R-project.org) with:

``` r
# install and load devtools from CRAN
install.packages("devtools")
library(devtools)

# install and load getLattes
devtools::install_github("roneyfraga/getLattes")
library(getLattes)
```

## Import XML file as R list

``` r

# the file 6380212729787758.xml need to be in the R working directory 
# to be sure run getwd() and dir()
cl <- readLattes(filexml='6380212729787758.xml')

# to import several files
cls <- readLattes(filexml=list.files(pattern='.xml'))
```

## Loaded data

To load 500 random curricula data imported as an R list.

``` r
data(lattesXML)
length(lattesXML)
```

## Import general data

``` r
# to combine list of data frames in data frame
library(dplyr)

# to import from one curriculum 
getDadosGerais(lattesXML[[499]])

# to import from two or more curricula
lt <- lapply(lattesXML, getDadosGerais)
head(bind_rows(lt))
```

## Import Published Academic Papers

``` r
# to import from one curriculum 
getArtigosPublicados(lattesXML[[462]]) 

# to import from two or more curricula
lt <- lapply(lattesXML, getArtigosPublicados)
head(bind_rows(lt))
```

## Normalize informations

See `normalizeByDoi`, `normalizeByJournal` and `normalizeByYear` to
normalize publications data (journal title, ISSN and year).
