\name{nc_open.CGLS.data}
\alias{nc_open.CGLS.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Explore original nc files of Global Land Service (CGLS) data
}
\description{
Opens single orginal data files/layers of Copernicus Global Land Service without adjusting coordinates as netCDF files.
}
\usage{
nc_open.CGLS.data(path, date, product, resolution, version)
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
}
\details{
Adjusting coordinates is a necessary step to use the data because Copernicus nc files have lat/long belonging to the centre of the pixel, and R uses upper/left corner. This function opens the data without any corrections.
}
\value{
netCDF
}
\references{
https://land.copernicus.eu/global/products/
}
\author{
Willemijn Vroege, wvroege@ethz.ch.
Many thanks to Tim Jacobs, VITO, Copernicus Global Help Desk and Xavier Rotllan Puig, Aster Projects for constructive feedback.
}
\note{
Coordinates are shifted and need to be adjusted, for example by:
  if(resolution == "300m"){
    lon <- lon - (1/336)/2
    lat <- lat + (1/336)/2
  }
  if(resolution == "1km"){
    lon <- lon - (1/112)/2
    lat <- lat + (1/112)/2
  }

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

nc      <- nc_open.CGLS.data   (path=PATH,date=DATE, product=PRODUCT, resolution=RESOLUTION, version=VERSION)

}
