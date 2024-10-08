% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writePublicationsRis.R
\name{writePublicationsRis}
\alias{writePublicationsRis}
\title{writePublicationsRis}
\usage{
writePublicationsRis(
  x,
  filename = "papers.ris",
  append = F,
  citationName = F,
  tableLattes = "ArtigosPublicados"
)
}
\arguments{
\item{x}{data frame}

\item{filename}{external file where the data will be saved, Default: 'papers.ris'}

\item{append}{to an existing file, Default: F}

\item{citationName}{authors name field, Default: F}

\item{tableLattes}{bibliographic data to be exported, 'ArtigosPublicados', 'Livros' or 'CapitulosLivros', Default: 'ArtigosPublicados'}
}
\value{
data frame
}
\description{
Function to convert and export Published Articles and Books to RIS format.
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
