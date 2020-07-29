############################################################################################################################
#
#COPERNICUS GLOBAL LAND SERVICE (CGLS) DATA DOWNLOAD
#
#These functions allow to download data provided by the Copernicus Global Land Service data <https://land.copernicus.eu/global>. 
#The functions rely on the manifest data <https://land.copernicus.eu/global/access>. Before you can download the data, you will first need to register 
#to create a username and password.
#Set your path, username, password, timeframe, product, resolution and if more than 1 version exists, version number. New products are created regularly.
#For the most recent product availabilities at the Copernicus data manifest check: https://land.copernicus.vgt.vito.be/manifest/
#
#
#These functions are distributed in the hope that they will be useful,
#but without any warranty.
#
#Author: Willemijn Vroege, ETH Zurich.
#E-mail: wvroege@ethz.ch
#
#
#First version: 28.10.2019
#Last update  : 25.06.2020
#
###########################################################################################################################

#' @import RCurl
#' @import ncdf4
#' @importFrom sp CRS proj4string<-
#' @importFrom raster extent extent<-
#' @importFrom utils download.file stack


#' @title Download CGLS data
#'
#' @description Downloads manifest files of the Copernicus Global Land Service. Registration at https://land.copernicus.eu/global/ is required.
#' @usage download_CGLS_data(username, password, timeframe, product, resolution, version)
#' @param username Register at https://land.copernicus.eu/global/
#' @param password Register at https://land.copernicus.eu/global/
#' @param timeframe Timeframe of interest (as daily date vector), for example june 2019: seq(as.Date("2019-06-01"), as.Date("2019-06-15"), by="days")
#' @param product Product name: chose from fapar, fcover, lai, ndvi,  ss, swi, lst, ...
#' @param resolution 1km, 300m or 100m
#' @param version Version number: v1, v2, v3,...
#' @return CGLS data Data saved locally in chosen folder.
#' @details Check https://land.copernicus.eu/global/products/ for a product overview and product details. Check https://land.copernicus.vgt.vito.be/manifest/ for an overview for data availability in the manifest.
#' @examples 
#' USERNAME   <- "usernames" #Insert username
#' PASSWORD   <- "password" #Insert password
#' TIMEFRAME  <- seq(as.Date("2019-06-01"), as.Date("2019-06-15"), by="days") #Insert timeframe of interest, for example June 2019
#' PRODUCT    <- "fapar" #Insert product variable -> CHOSE FROM fapar, fcover, lai, ndvi,  ssm, swi, lst, ...
#' RESOLUTION <- "1km" #Insert resolution (1km, 300m or 100m)
#' VERSION    <- "v1" #"Insert version: "v1", "v2", "v3",...
#' 
#'download.CGLS.data(username=USERNAME, password=PASSWORD, timeframe=TIMEFRAME, product=PRODUCT, resolution=RESOLUTION, version=VERSION) 

#' @export


download_CGLS_data <- function(username, password, timeframe, product,
                               resolution, version){

  if(resolution == "300m"){
    resolution1 <- "333m"
    product <- paste0(product, "300")
  }else if(resolution == "1km"){
    resolution1 <- resolution
  }

  collection <- paste(product, version, resolution1, sep="_")

  product.link<- paste0("@land.copernicus.vgt.vito.be/manifest/", collection, "/manifest_cgls_", collection, "_latest.txt" )

  url <- paste0("https://", paste(username, password, sep=":"), product.link)

  file.url <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE, crlf = TRUE)
  file.url <- unlist(strsplit(file.url, "\n"))
  file.url <- paste0("https://", paste(username, password, sep=":"), "@", sub(".*//", "",file.url))
  if(grepl("does not exist", file.url[10])) stop("This product is not available or the product name is misspecified")

  wd <- getwd()           
  if(!dir.exists(collection)) dir.create(collection)
  setwd(paste(wd, collection, sep="/"))

    for (i in 1:length(timeframe)){
     temp <- grep(gsub("-", "", timeframe[[i]]),file.url, fixed=T, value=T) #select a file for each day
      if (length(temp) > 0 ){ #if there is data for this day
        if (i>1){Sys.sleep(3)}
        download.file(temp, paste(collection, sub(".*/", "", temp), sep="_"), mode = 'wb')   #download function
        print(paste0(collection, "_", sub(".*/", "", temp), " is saved in ", getwd()))
      }
    }
}

#' @title Open netcdf CGLS data
#' 
#' @description Opens single orginal data files/layers of Copernicus Global as netCDF filesLand Service as netCDF files without adjusting coordinates. Coordinate adjustment is necessary as R uses upper left corner as pixel reference and Copernicus uses pixel centre. Also see: https://land.copernicus.eu/global/products/.
#' @usage nc_open_CGLS_data(date, product, resolution, version)
#' @param date Date of interest, for example for 13 june 2019: 2019-06-13
#' @param product Product name: chose from fapar fapar, fcover, lai, ndvi,  ss, swi, lst, ...
#' @param resolution 1km, 300m or 100m
#' @param version Version number: v1, v2, v3,...
#' @return CGLS data Opens single netcdf file in environment
#' 
#' @details Adjusting coordinates is a necessary step to use the data because Copernicus nc files have lat/long belonging to the centre of the pixel, and R uses upper/left corner. This function opens the data without any corrections.
#' @note  
#' Coordinates are shifted and need to be adjusted, for example by:
#' if(resolution == "300m"){
#' lon <- lon - (1/336)/2
#' lat <- lat + (1/336)/2
#' }
#' if(resolution == "1km"){
#' lon <- lon - (1/112)/2
#' lat <- lat + (1/112)/2
#' }
#' 
#' @examples 
#' DATE       <- "2019-06-13" #INSERT DATE OF INTEREST, for example June 13 2019
#' PRODUCT    <- "fapar" #Insert product variable -> CHOSE FROM fapar, fcover, lai, ndvi,  ssm, swi, lst, ...
#' RESOLUTION <- "1km" #Insert resolution (1km, 300m or 100m)
#' VERSION    <- "v1" #"Insert version: "v1", "v2", "v3",...
#' 
#' nc      <- nc_open.CGLS.data   (date=DATE, product=PRODUCT, resolution=RESOLUTION, version=VERSION)

#' @export

nc_open_CGLS_data <- function(date, product, resolution, version){
  if(resolution == "300m"){
    resolution1 <- "333m"
    product <- paste0(product, "300")
  }else if(resolution == "1km"){
    resolution1 <- resolution
  }
  collection <- paste(product, version, resolution1, sep="_")
  all.filenames.product  <- list.files(pattern=(collection), recursive = TRUE)
  specific.filename<-grep(gsub("-","",date), all.filenames.product, value = T)
  nc  <- nc_open(specific.filename)
}

#' @title Read netcdf CGLS data
#' @description Read single layers of Copernicus Global Land Service (CGLS) data and adjusts coordinates for R.
#' @usage ncvar_get_CGSL_data(date, product, resolution, version, variable)
#' @param date Date of interest, for example for 13 june 2019: 2019-06-13
#' @param product Product name: chose from fapar fapar, fcover, lai, ndvi,  ss, swi, lst, ...
#' @param resolution 1km, 300m or 100m
#' @param version Version number: v1, v2, v3,...
#' @param variable Product variable, for example: FAPAR, FAPAR_ERR, FAPAR_QFLAG, LMK, NMOD, ssm, ssm_noise: Check the product site (e.g. https://land.copernicus.eu/global/products/ssm) for available variable names under the tap 'technical'.
#' @return CGLS data Large matrix of a specific variable in environment, coordinates adjusted.
#' 
#' @details Adjusting coordinates is a necessary step to use the data because Copernicus nc files have lat/long belonging to the centre of the pixel, and R uses upper/left corner. This function opens the data without any corrections.
#' @examples 
#' DATE       <- "2019-06-13" #INSERT DATE OF INTEREST, for example June 13 2019
#' PRODUCT    <- "fapar" #Insert product variable -> CHOSE FROM fapar, fcover, lai, ndvi,  ssm, swi, lst, ...
#' RESOLUTION <- "1km" #Insert resolution (1km, 300m or 100m)
#' VERSION    <- "v1" #"Insert version: "v1", "v2", "v3",...
#' VARIABLE   <- "FAPAR" #Insert product variable, for example: FAPAR, FAPAR_ERR, FAPAR_QFLAG, LMK, NMOD, ssm, ssm_noise, ... . -->Go to the product site e.g. https://land.copernicus.eu/global/products/ssm) and check for available variable names under the tap 'techinal'
#' 
#' nc_data <- ncvar_get_CGSL.data (date=DATE, product=PRODUCT, resolution=RESOLUTION, version=VERSION, variable=VARIABLE)
#' @export

ncvar_get_CGSL_data <- function(date, product, resolution, version, variable){
  if(resolution == "300m"){
    resolution1 <- "333m"
    product <- paste0(product, "300")
  }else if(resolution == "1km"){
    resolution1 <- resolution
  }
  collection <- paste(product, version, resolution1, sep="_")
  all.filenames.product  <- list.files(pattern=(collection), recursive = TRUE)
  specific.filename<-grep(gsub("-","",date), all.filenames.product, value = T)
  nc  <- nc_open(specific.filename)
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  time <- ncvar_get(nc, "time")

  #Copernicus nc files have lat/long belonging to the centre of the pixel, and R uses upper/left corner --> adjust coordinates!
  if(resolution == "300m"){
    lon <- lon - (1/336)/2
    lat <- lat + (1/336)/2
  }
  if(resolution == "1km"){
    lon <- lon - (1/112)/2
    lat <- lat + (1/112)/2
  }
  nc_data <- ncvar_get(nc, variable)
}


#' @title stack CGLS data
#'
#' @description Read all downloaded files from Copernicus Global Land Service within a timeframe as Raster Stack and adjusts coordinates for R.
#' @usage stack_CGLS_data(timeframe, product, resolution, version, variable)
#' @param timeframe Timeframe of interest (as daily date vector), for example june 2019: seq(as.Date("2019-06-01"), as.Date("2019-06-15"), by="days")
#' @param product Product name: chose from fapar fapar, fcover, lai, ndvi,  ss, swi, lst, ...
#' @param resolution 1km, 300m or 100m
#' @param version Version number: v1, v2, v3,...
#' @param variable Product variable, for example: FAPAR, FAPAR_ERR, FAPAR_QFLAG, LMK, NMOD, ssm, ssm_noise: Check the product site (e.g. https://land.copernicus.eu/global/products/ssm) for available variable names under the tap 'technical'.
#' @details Adjusting coordinates is a necessary step to use the data because Copernicus nc files have lat/long belonging to the centre of the pixel, and R uses upper/left corner.
#' @return CGLS data Raster Stack
#' @examples
#' TIMEFRAME  <- seq(as.Date("2019-06-01"), as.Date("2019-06-15"), by="days") #INSERT TIMEFRAME OF INTEREST, for example June 2019
#' PRODUCT    <- "fapar" #INSERT PRODUCT VARIABLE;(for example fapar) -> CHOSE FROM fapar, fcover, lai, ndvi,  ss, swi, lst, ...
#' RESOLUTION <- "1km" #INSERT RESOLTION (1km, 300m or 100m)
#' VERSION    <- "v1" #"INSERT VERSION: "v1", "v2", "v3",...
#' VARIABLE   <- "FAPAR" #INSERT VARIABLE NAME, for example: FAPAR, FAPAR_ERR, FAPAR_QFLAG, LMK, NMOD, ssm, ssm_noise, ... . -->Go to the product site e.g. https://land.copernicus.eu/global/products/ssm) and check for available variable names under the tap 'techinal'
#' data   <- stack.CGLS.data(timeframe=TIMEFRAME, product=PRODUCT, resolution=RESOLUTION, version=VERSION, variable=VARIABLE)
#' @export

stack_CGLS_data <- function(timeframe, product, resolution, version, variable){
  if(resolution == "300m"){
    resolution1 <- "333m"
    product <- paste0(product, "300")
  }else if(resolution == "1km"){
    resolution1 <- resolution
  }
  collection <- paste(product, version, resolution1, sep="_")
  all.filenames.product  <- list.files(pattern=(collection), recursive = TRUE)
  datepattern   <- gsub("-", "", timeframe)
  datepattern.in.timeframe <- names(unlist(sapply(datepattern, grep, all.filenames.product)))
  filenames.in.timeframe <- paste(all.filenames.product[unlist(sapply(datepattern, grep, all.filenames.product))], sep="/")

  # save current warning level
  current_warn_level <- getOption("warn");

  # make sure warn is reset when execution of function ends
  on.exit(options(warn=current_warn_level))

  options(warn=-1)
  data <- stack(filenames.in.timeframe, varname=variable, quick=T) #this produces a warning because the projection gets off as R reads the coordinates as left upper corner. This is corrected below.
  extent(data) <- extent(c(-180, 180, -60, 80))
  proj4string(data) <- CRS("+init=epsg:4326")
  data<-data
}
