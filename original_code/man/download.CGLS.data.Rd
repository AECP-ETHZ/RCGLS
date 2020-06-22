\name{download.CGLS.data}
\alias{download.CGLS.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download data provided by the Copernicus Global Land Service (CGLS) with R.
}
\description{
Downloads manifest files of the Copernicus Global Land Service. Registration at https://land.copernicus.eu/global/ is required.
}
\usage{
download.CGLS.data(path, username, password, timeframe, product, resolution, version)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{username}{
Register at https://land.copernicus.eu/global/
}
  \item{password}{
Register at https://land.copernicus.eu/global/
}
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
}
\details{
Check https://land.copernicus.eu/global/products/ for a product overview and product details.
Check https://land.copernicus.vgt.vito.be/manifest/ for an overview for data availability in the manifest.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~https://land.copernicus.eu/global/products/  ~
}
\author{
Willemijn Vroege, wvroege@ethz.ch.
Many thanks to Tim Jacobs, VITO, Copernicus Global Help Desk and Xavier Rotllan Puig, Aster Projects for constructive feedback.
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
USERNAME   <- "wvroege" #Insert username
PASSWORD   <- "CGLS_password" #Insert password
TIMEFRAME  <- seq(as.Date("2019-06-01"), as.Date("2019-06-15"), by="days") #Insert timeframe of interest, for example June 2019
PRODUCT    <- "fapar" #Insert product variable -> CHOSE FROM fapar, fcover, lai, ndvi,  ssm, swi, lst, ...
RESOLUTION <- "1km" #Insert resolution (1km, 300m or 100m)
VERSION    <- "v1" #"Insert version: "v1", "v2", "v3",...

download.CGLS.data(path=PATH, username=USERNAME, password=PASSWORD, timeframe=TIMEFRAME, product=PRODUCT, resolution=RESOLUTION, version=VERSION)
}
