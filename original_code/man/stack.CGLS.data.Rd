\name{stack.CGLS.data}
\alias{stack.CGLS.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read Copernicus Global Land Service (CGLS) data as Raster Stack
}
\description{
Read all downloaded files from Copernicus Global Land Service within a Timeframe as Raster Stack  and adjusts coordinates for R.
}
\usage{
stack.CGLS.data(path, timeframe, product, resolution, version, variable)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
Working directory, for example: D:/land.copernicus
}
\item{timeframe}{
Timeframe of interest (as daily date vector), for example june 2019: seq(as.Date("2019-06-01"), as.Date("2019-06-15"), by="days")
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
 Product variable, for example: FAPAR, FAPAR_ERR, FAPAR_QFLAG, LMK, NMOD, ssm, ssm_noise: Check the product site (e.g. https://land.copernicus.eu/global/products/ssm) for available variable names under the tap 'technical'.
}
}
\details{
Adjusting coordinates is a necessary step to use the data because Copernicus nc files have lat/long belonging to the centre of the pixel, and R uses upper/left corner.
}
\value{
Raster Stack
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
PATH       <- "D:/land.copernicus" #INSERT DIRECTORY
TIMEFRAME  <- seq(as.Date("2019-06-01"), as.Date("2019-06-15"), by="days") #INSERT TIMEFRAME OF INTEREST, for example June 2019
PRODUCT    <- "fapar" #INSERT PRODUCT VARIABLE;(for example fapar) -> CHOSE FROM fapar, fcover, lai, ndvi,  ss, swi, lst, ...
RESOLUTION <- "1km" #INSERT RESOLTION (1km, 300m or 100m)
VERSION    <- "v1" #"INSERT VERSION: "v1", "v2", "v3",...
VARIABLE   <- "FAPAR" #INSERT VARIABLE NAME, for example: FAPAR, FAPAR_ERR, FAPAR_QFLAG, LMK, NMOD, ssm, ssm_noise, ... . -->Go to the product site e.g. https://land.copernicus.eu/global/products/ssm) and check for available variable names under the tap 'techinal'

data   <- stack.CGLS.data(path=PATH,timeframe=TIMEFRAME, product=PRODUCT, resolution=RESOLUTION, version=VERSION, variable=VARIABLE)
}
