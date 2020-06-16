\name{ncvar_get_CGSL.data}
\alias{ncvar_get_CGSL.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read single layers of Copernicus Global Land Service (CGLS) data
}
\description{
Opens single data layers Copernicus Global Land Service and adjusts coordinates for R.
}
\usage{
ncvar_get_CGSL.data(path, date, product, resolution, version, variable)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{path}{
Working directory, for example: D:/land.copernicus
}
\item{date}{
Date of interest, for example for 13 june 2019: 2019-06-13"
}
\item{product}{
 Product name: chose from fapar fapar, fcover, lai, ndvi,  ss, swi, lst, ...
}
\item{resolution}{
 "1km", "300m" or "100m"
}
\item{version}{
 Version number: "v1", "v2", "v3",...
}
\item{variable}{
 Product variable, for example: FAPAR, FAPAR_ERR, FAPAR_QFLAG, LMK, NMOD, ssm, ssm_noise: Check the product site (e.g. https://land.copernicus.eu/global/products/ssm) for available variable names under the tap 'technical'
}
}

\details{
Adjusting coordinates is a necessary step to use the data because Copernicus nc files have lat/long belonging to the centre of the pixel, and R uses upper/left corner.
}
\value{
large Matrix
}
\references{
https://land.copernicus.eu/global/products/
}
\author{
Willemijn Vroege, wvroege@ethz.ch. Many thanks to Tim Jacobs, VITO, Copernicus Global Help Desk and Xavier Rotllan Puig, Aster Projects for constructive feedback.
}
\note{
Also see https://github.com/AECP-ETHZ/Copernicus-Global-Land-Service-Data-Download-with-R
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PATH       <- "D:/land.copernicus" #Insert your target directory
DATE       <- "2019-06-13" #INSERT DATE OF INTEREST, for example June 13 2019
PRODUCT    <- "fapar" #Insert product variable -> CHOSE FROM fapar, fcover, lai, ndvi,  ssm, swi, lst, ...
RESOLUTION <- "1km" #Insert resolution (1km, 300m or 100m)
VERSION    <- "v1" #"Insert version: "v1", "v2", "v3",...
VARIABLE   <- "FAPAR" #Insert product variable, for example: FAPAR, FAPAR_ERR, FAPAR_QFLAG, LMK, NMOD, ssm, ssm_noise, ... . -->Go to the product site e.g. https://land.copernicus.eu/global/products/ssm) and check for available variable names under the tap 'techinal'

nc_data <- ncvar_get_CGSL.data (path=PATH,date=DATE, product=PRODUCT, resolution=RESOLUTION, version=VERSION, variable=VARIABLE)
}

