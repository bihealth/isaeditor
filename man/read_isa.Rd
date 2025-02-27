% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isatools.R
\name{read_isa}
\alias{read_isa}
\alias{write_isa}
\title{Read or write an isatab file}
\usage{
read_isa(file, type = "auto")

write_isa(x, file = NULL)
}
\arguments{
\item{file}{file name to read / write. For write_isa, if file is
missing, the function will return the isatab object as a character
vector. If file is not missing, the function will return the file name.}

\item{type}{Either 'auto', or 'investigation', 'study', 'assay' (can
be abbreviated)}

\item{x}{isatab object}
}
\value{
\code{read_isa()} returns either an object of class \code{isatab} (for study / assay files) or
an object of class \code{isa_i} (for investigation files).
}
\description{
Read or write an isatab file
}
\examples{
file <- system.file('extdata', 'i_Investigation.txt', package='isaeditor')
isa_i <- read_isa(file)
print(isa_i)

file <- system.file('extdata', 's_isatab.txt', package='isaeditor')
isa_s <- read_isa(file)
print(isa_s)

}
\seealso{
\code{\linkS4class{isatab}}
}
