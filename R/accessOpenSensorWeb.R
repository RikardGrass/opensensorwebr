#' availablesensors
#'
#' @description
#' This function lists all available sensors for a specific device on open sensor web
#'
#' @param url the url adress (given without "/" at the end) of a specivic device to create a GET-request for the OpensSensorweb REST API character
#' @return a json file with all available sensors for the given device
#' @examples opensensorwebr::availablesensors("https://api.sensoto.io/v1/organizations/open/networks/AMMS_WETTERDATEN", my.devices = "S034")
#' @import httr jsonlite
#' @export
availablesensors <- function(url, my.device){
  # Create a GET-request
  sensors.available <- httr::GET(url = paste0(url,"/devices/",my.device,"/sensors") )

  # print device information
  print("following sensors are available at the given url:")
  print("----------------------------------------------------")
  print(sensors.available)

  print("this is the content of the returned json file at the given url:")
  print("----------------------------------------------------")
  print(jsonlite::fromJSON(httr::content(sensors.available, "text")))
}


#' hourly
#'
#' @description
#' Downloads hourly data for a given sensor provided by OpenSensorWeb REST API (https://www.opensensorweb.de/).
#'
#' OpenSensorWeb provides data in UTC-Timeformat. If TimeExtent is providet in the Request in UTC format (2002-12-12T12:00Z) data is returned in UTC format. Requests must use ISO 8601 Format: https://de.wikipedia.org/wiki/ISO_8601 while the "Z" in 2002-12-12T12:00Z notates for UTC and is equivalent to 2002-12-12T12:00+00:00
#' requests might also be defined using intervals (e.g. 2002-09-12T12:00Z/2002-12-12T12:00Z/PT1H (time-based multi-interval, UTC)) but this is not implemented in this package.
#' @param url the url adress (given without "/" at the end) of a specivic device to create a GET-request for the OpensSensorweb REST API character ()
#' @param my.device the device ID in the form "S021", can be defined by searches on https://www.opensensorweb.de
#' @param my.sensor the sensor ID in the form "Niederschlag", can be chosen by searches on https://www.opensensorweb.de or an request using availablesensors()
#' @param my.interval time interval of data from startdate (hours)
#' @param my.startdate start date of the requested data must be given in ISO 8601 Format (https://de.wikipedia.org/wiki/ISO_8601) e.g. "2002-12-12T12:00Z"
#' @param aggregation how data shall be agregated ("MEAN","SUM")
#' @return a json file with all available sensors
#' @import httr jsonlite dplyr
#' @examples hourly(url = "https://api.sensoto.io/v1/organizations/open/networks/AMMS_WETTERDATEN", my.device = "S021", my.sensor = "Niederschlag", aggregation = "MEAN", my.interval = 1000, my.startdate = "2015-09-18T00:00:00Z")
#' @export
hourly <- function(url = "https://api.sensoto.io/v1/organizations/open/networks/AMMS_WETTERDATEN/devices",
                                my.device,
                                my.sensor,
                                my.interval = 1000,
                                my.startdate = "2015-09-18T00:00:00Z",
                                aggregation = "MEAN"){  # interval in Stunden

  # create GET request url
  request <- paste0(url,"/devices/",my.device,"/sensors/",my.sensor,
                   "/measurements?interval=R",my.interval,"/",
                   my.startdate, "/PT1H&timeFormat=interval&agg=",aggregation,  #--> TODO: Rxyz anpassen wenn noetig
                   "&interpolator=LINEAR&includeLatest=true")

  # template for a request using fixed time interval
  # request <- paste("https://api.sensoto.io/v1/organizations/open/networks/AMMS_WETTERDATEN/devices/",my.device,"/sensors/",my.sensor,
  #                  "/measurements/raw?start=2018-08-31T05:00:00.000Z&end=2018-09-01T06:00:00.000Z&timeFormat=interval", sep = "")

  res   <- httr::GET(url = request)
  #data  <- jsonlite::fromJSON(httr::content(res, "text"))
  data  <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))


  # set timezone and data format
  data$date <- strptime(data$end, format="%Y-%m-%dT%H:%MZ", tz="UTC")  # Reference for hourly data: "end"; for daily data possibly "begin" recomended
  data$date <- as.POSIXct(data$date)

  data <- data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(sensor = my.sensor)

  print("Information: returning data in UTC Time format")
  return(data)
}




#' etmodeldata
#'
#' @description
#' Provide all a dataset usefull for evapotranspiration modelling if all data are available at one station. Data required at the moment is: GlobalRadiation, Air Temperature, Relative Humidity, Windspeed, Rain
#' Todo: make Rain optional!
#' @param url the url of the device in form of https://api.sensoto.io/v1/organizations/open/networks/AMMS_WETTERDATEN/devices/S034
#' @return A json file with all available sensors
#' @import httr jsonlite dplyr reshape2
#' @examples opensensorwebr::etmodeldata("https://api.sensoto.io/v1/organizations/open/networks/AMMS_WETTERDATEN", my.device = "S021", my.startdate = "2024-01-01T00:00:00Z",my.interval = 100, ID.GlobRad = "Globalstrahlg_200cm",ID.AirTemp = "Lufttemp_200cm",ID.RH = "Luftfeuchtigkeit_200cm",ID.Rain = "Niederschlag",ID.Wind = "Windgeschw_250cm",file = "temp/Wetter_Coswig_",write.RData = FALSE,write.csv = FALSE)
#' @export
#'
#'
#'
#'
#' etmodeldata
#'
#' @description
#' Provide a dataset useful for evapotranspiration modelling if all data are available at one station.
#' Required at the moment: Global Radiation, Air Temperature, Relative Humidity, Windspeed, Rain.
#'
#' !! if one parameter is not available, please use another parameter as a dummy which is not in the list at the moment !!
#'
#' @param url The base URL of the device, e.g. "https://api.sensoto.io/v1/organizations/open/networks/AMMS_WETTERDATEN/devices/S034"
#' @param my.device Device ID (e.g. "S021")
#' @param my.startdate Start date-time in ISO format
#' @param my.interval Interval for data aggregation
#' @param ID.GlobRad Sensor ID for global radiation
#' @param ID.AirTemp Sensor ID for air temperature
#' @param ID.RH Sensor ID for relative humidity
#' @param ID.Rain Sensor ID for precipitation
#' @param ID.Wind Sensor ID for windspeed
#' @param file Path prefix for output files
#' @param write.RData Logical, whether to save .RData
#' @param write.csv Logical, whether to save .csv
#'
#' @return A data frame of all chosen sensor data.
#'
#' @examples
#' opensensorwebr::etmodeldata(
#'   url = "https://api.sensoto.io/v1/organizations/open/networks/AMMS_WETTERDATEN",
#'   my.device = "S021",
#'   my.startdate = "2024-01-01T00:00:00Z",
#'   my.interval = 100,
#'   ID.GlobRad = "Globalstrahlg_200cm",
#'   ID.AirTemp = "Lufttemp_200cm",
#'   ID.RH = "Luftfeuchtigkeit_200cm",
#'   ID.Rain = "Niederschlag",
#'   ID.Wind = "Windgeschw_250cm",
#'   file = "temp/Wetter_Coswig_",
#'   write.RData = FALSE,
#'   write.csv = FALSE
#' )
#'
#' @import httr jsonlite dplyr
#' @export
etmodeldata <- function(url = "https://api.sensoto.io/v1/organizations/open/networks/AMMS_WETTERDATEN",
                        my.device = "S021",
                        my.startdate = "2015-09-18T00:00:00Z",
                        my.interval = 1000,
                        ID.GlobRad = "Globalstrahlg_200cm",
                        ID.AirTemp = "Lufttemp_200cm",
                        ID.RH = "Luftfeuchtigkeit_200cm",
                        ID.Rain = "Niederschlag",
                        ID.Wind = "Windgeschw_250cm",
                        file = "temp/Wetter_Coswig_",
                        write.RData = FALSE,
                        write.csv = FALSE){

  # create a list to store data
  my.elements    <- c("GlobRad_Wm", "AirTemp", "RH", "Rain", "Wind")
  weather        <- vector("list", length(my.elements))
  names(weather) <- my.elements

  weather <- weather
  weather[["GlobRad_Wm"]] <- hourly(my.sensor = ID.GlobRad, url, my.device = my.device, my.startdate = my.startdate, my.interval = my.interval, aggregation = "MEAN")
  weather[["AirTemp"]]    <- hourly(my.sensor = ID.AirTemp, url, my.device = my.device, my.startdate = my.startdate, my.interval = my.interval, aggregation = "MEAN")
  weather[["RH"]]         <- hourly(my.sensor = ID.RH, url, my.device = my.device, my.startdate = my.startdate, my.interval = my.interval, aggregation = "MEAN")
  weather[["Rain"]]       <- hourly(my.sensor = ID.Rain, url, my.device = my.device, my.startdate = my.startdate, my.interval = my.interval, aggregation = "SUM")
  weather[["Wind"]]       <- hourly(my.sensor = ID.Wind, url, my.device = my.device, my.startdate = my.startdate, my.interval = my.interval, aggregation = "MEAN")

  weather <- dplyr::bind_rows(weather)

  # rename variables
  weather <- weather %>%
    #separate(sensor, c(NA, "variable", NA), remove = FALSE) %>% # extract variable from sensor-identifier
    dplyr::mutate(sensor = ifelse(sensor == ID.GlobRad, "glob", sensor),
                  sensor = ifelse(sensor == ID.AirTemp, "Tmit", sensor),
                  sensor = ifelse(sensor == ID.RH, "rh", sensor),
                  sensor = ifelse(sensor == ID.Rain, "nied", sensor),
                  sensor = ifelse(sensor == ID.Wind, "wind2m", sensor)
    )

  # # transform long table to short form (flatfile-format to table)
  # # weather <- reshape2::dcast(weather %>% dplyr::select(date,sensor,v), formula = date ~ sensor) #vs. melt
  # weather_wide <- reshape2::dcast(
  #   weather %>% select(date, sensor, v),
  #   date ~ sensor,
  #   value.var = "v",
  #   fun.aggregate = mean   # or sum, max, identity, etc.
  # )

  # since 28.8.2025 using tidyr::pivot_wider()
  # if (date, sensor) is unique → you’ll just get the value, no aggregation needed.
  # if it’s not unique → you’ll get a warning and can then decide how to handle duplicates.
  weather <- weather %>%
    select(date, sensor, v) %>%
    pivot_wider(
      names_from = sensor,
      values_from = v
    )
  weather <- as.data.frame(weather)

  print(head(weather))
  print("Information: returning data in UTC Time format")

  # export data
  if (write.RData == TRUE){
    save(weather, file = paste0(file,as.character(as.Date(Sys.time())),".RData"))
    save(weather, file = paste0(file,"akt",".RData"))
  }

  if (write.csv == TRUE){
    write.csv(weather, paste0(file,as.character(as.Date(Sys.time())),".csv"))
    write.csv(weather, paste0(file,"akt",".csv"))
  }
  return(weather)
}






##' etmodeldata
##'
##' #@description
##' #This function shows all available sensors for a specific device on open sensor web
##' #' @param url the url of the device in form of https://api.sensoto.io/v1/organizations/open/networks/AMMS_WETTERDATEN/devices/S034
##' #' @return a json file with all available sensors
##' #' @examples availablesensors(https://api.sensoto.io/v1/organizations/open/networks/AMMS_WETTERDATEN/devices/S034)
##' #' @import httr jsonlite dplyr
##' #' @example etmodeldata(file = "temp/Wetter_Coswig_", write.RData = TRUE)
##' #' @export
# etmodeldata <- function(url = "https://api.sensoto.io/v1/organizations/open/networks/AMMS_WETTERDATEN",
#                         my.device = "S021",
#                         my.startdate = "2015-09-18T00:00:00Z",
#                         my.interval = 1000,
#                         file = "temp/Wetter_Coswig_",
#                         write.RData = FALSE,
#                         write.csv = FALSE){
#   # Opensensorweb stores data in UTC-Timeformat.
#   #    - At least if TimeExtent is given in UTC (2002-12-12T12:00Z) --> Z stands for UTC, same as 2002-12-12T12:00+00:00
#   #    - request for TimeInterval (TimeExtent) must be given in ISO 8601 Format: https://de.wikipedia.org/wiki/ISO_8601
#
#   # create a list to store data
#   my.elements    <- c("GlobRad_Wm", "AirTemp", "RH", "Rain", "Wind")
#   weather        <- vector("list", length(my.elements))
#   names(weather) <- my.elements
#
#   # neue Abfrage ab 2021 (API geaendert, va. die sensornamen haben andere Bezeichnungen bekommen) --> gut rauszufinden ueber https://opensensorweb.de/#/search?c=13.572997181410642%2C51.13120038875931&sid=AMMS_WETTERDATEN%24S021%24Bodentemp_-5cm%2CAMMS_WETTERDATEN%24S021%24Windgeschw_250cm%2CAMMS_WETTERDATEN%24S021%24Niederschlag%2CAMMS_WETTERDATEN%24S021%24Luftfeuchtigkeit_200cm%2C&te=2020-09-01T15%3A20%3A11.281Z%2C2021-09-01T15%3A20%3A11.281Z&tz=B&v=sidebar&z=13.757533237717272  --> dann Entwicklertools von Firefox: str-shift-E
#   weather <- weather
#   weather[["GlobRad_Wm"]] <- hourly(my.sensor = "Globalstrahlg_200cm", url, my.device = my.device, my.startdate = my.startdate, my.interval = my.interval, aggregation = "MEAN")
#   weather[["AirTemp"]] <- hourly(my.sensor = "Lufttemp_200cm", url, my.device = my.device, my.startdate = my.startdate, my.interval = my.interval, aggregation = "MEAN")
#   weather[["RH"]] <-  hourly(my.sensor = "Luftfeuchtigkeit_200cm", url, my.device = my.device, my.startdate = my.startdate, my.interval = my.interval, aggregation = "MEAN")
#   weather[["Rain"]] <- hourly(my.sensor = "Niederschlag", url, my.device = my.device, my.startdate = my.startdate, my.interval = my.interval, aggregation = "SUM")
#   weather[["Wind"]] <- hourly(my.sensor = "Windgeschw_250cm", url, my.device = my.device, my.startdate = my.startdate, my.interval = my.interval, aggregation = "MEAN")
#
#   weather <- dplyr::bind_rows(weather)
#
#   # rename variables
#   weather <- weather %>%
#     #separate(sensor, c(NA, "variable", NA), remove = FALSE) %>% # extract variable from sensor-identifier
#     dplyr::mutate(sensor = ifelse(sensor == "Globalstrahlg_200cm", "glob", sensor),
#                   sensor = ifelse(sensor == "Lufttemp_200cm", "Tmit", sensor),
#                   sensor = ifelse(sensor == "Luftfeuchtigkeit_200cm", "rh", sensor),
#                   sensor = ifelse(sensor == "Niederschlag", "nied", sensor),
#                   sensor = ifelse(sensor == "Windgeschw_250cm", "wind2m", sensor)
#     )
#
#   # transform long table to short form (flatfile-format to table)
#   weather <- reshape2::dcast(weather %>% dplyr::select(date,sensor,v), formula = date ~ sensor) #vs. melt
#   print(head(weather))
#   print("returning data in UTC Time format")
#
#   # export data
#   if (write.RData == TRUE){
#     save(weather, file = paste0(file,as.character(as.Date(Sys.time())),".RData"))
#     save(weather, file = paste0(file,"akt",".RData"))
#   }
#
#   if (write.csv == TRUE){
#     write.csv(weather, paste0(file,as.character(as.Date(Sys.time())),".csv"))
#     write.csv(weather, paste0(file,"akt",".csv"))
#   }
#   return(weather)
# }
