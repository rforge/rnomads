ReadGrib <- function (file.names, levels, variables, forecasts = NULL, domain = NULL,
    domain.type = "latlon", file.type = "grib2", missing.data = NULL)
{
    if (sum(sapply(file.names, file.exists)) == 0) {
        stop("The specified grib file(s) were not found.")
    }
    if (!(domain.type %in% c("latlon", "index"))) {
        stop(paste("domain.type must be either \"latlon\" or \"index\""))
    }
    if (file.type == "grib2") {
        op <- options("warn")
        options(warn = -1)
        test <- tryCatch(system("wgrib2", intern = TRUE))
        options(op)
        if (attr(test, "status") != 8) {
            stop("wgrib2 does not appear to be installed, or it is not on the PATH variable.\n                You can find wgrib2 here: http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/.\n If the binaries don't work, try compiling from source.")
        }
        variables <- stringr::str_replace_all(variables, "[{\\[()|?$^*+.\\\\]",
            "\\$0")
        levels <- stringr::str_replace_all(levels, "[{\\[()|?$^*+.\\\\]",
            "\\$0")
        match.str <- " -match \"("
        for (var in variables) {
            match.str <- paste(match.str, var, "|", sep = "")
        }
        match.str.lst <- strsplit(match.str, split = "")[[1]]
        match.str <- paste(match.str.lst[1:(length(match.str.lst) -
            1)], collapse = "")
        if (length(levels) > 0 & !is.null(levels)) {
            match.str <- paste(match.str, "):(", sep = "")
            for (lvl in levels) {
                match.str <- paste(match.str, lvl, "|", sep = "")
            }
        }
        else {
            match.str <- paste0(match.str, ")")
        }
        match.str.lst <- strsplit(match.str, split = "")[[1]]
        match.str <- paste(match.str.lst[1:(length(match.str.lst) -
            1)], collapse = "")
        if (length(forecasts) > 0 & !is.null(forecasts)) {
            match.str <- paste(match.str, "):(", sep = "")
            for (fcst in forecasts) {
                match.str <- paste(match.str, fcst, "|", sep = "")
            }
        }
        else {
            match.str <- paste0(match.str, ")")
        }
        match.str.lst <- strsplit(match.str, split = "")[[1]]
        match.str <- paste(match.str, "\"", sep = "")
        match.str <- paste(match.str.lst[1:(length(match.str.lst) -
            1)], collapse = "")
        match.str <- paste(match.str, ")\"", sep = "")
        if (!is.null(missing.data) & !is.numeric(missing.data)) {
            warning(paste("Your value", missing.data, " for missing data does not appear to be a number!"))
        }
        if (!(is.null(missing.data))) {
            missing.data.str <- paste0(" -rpn \"sto_1:", missing.data,
                ":rcl_1:merge\"")
        }
        else {
            missing.data.str <- ""
        }
        model.run.date <- c()
        forecast.date <- c()
        variables.tmp <- c()
        levels.tmp <- c()
        lon <- c()
        lat <- c()
        value <- c()
        for (file.name in file.names) {
            if (!file.exists(file.name)) {
                warning(paste("Grib file", file.name, "was not found."))
                next
            }
            if (!is.null(domain)) {
                if (!length(domain) == 4 | any(!is.numeric(domain))) {
                  stop("Input \"domain\" is the wrong length and/or consists of something other than numbers.\n                      It should be a 4 element vector: c(LEFT LON, RIGHT LON, TOP LAT, BOTTOM LAT)")
                }
                else {
                  if (domain.type == "latlon") {
                    wg2.pre <- paste0("wgrib2 ", file.name, " -inv my.inv ",
                      " -small_grib ", domain[1], ":", domain[2],
                      " ", domain[4], ":", domain[3], " tmp.grb && wgrib2 tmp.grb")
                  }
                  else {
                    wg2.pre <- paste0("wgrib2 ", file.name, " -ijundefine out-box ",
                      domain[1], ":", domain[2], " ", domain[4],
                      ":", domain[3])
                  }
                }
            }
            else {
                wg2.pre <- paste0("wgrib2 ", file.name)
            }
            wg2.str <- paste(wg2.pre, " -inv my.inv", missing.data.str,
                " -csv - -no_header", match.str, sep = "")
            if (Sys.info()[["sysname"]] == "Windows") {
                csv.str <- shell(wg2.str, intern = TRUE)
                if (csv.str[1] == "cygwin warning:") {
                  csv.str <- csv.str[7:length(csv.str)]
                }
            }
            else {
                csv.str <- system(wg2.str, intern = TRUE)
            }
            model.data.vector <- strsplit(paste(gsub("\"", "",
                csv.str), collapse = ","), split = ",")[[1]]
            if (length(model.data.vector) == 0) {
                warning(paste0("No combinations of variables ",
                  paste(variables, collapse = " "), " and levels ",
                  paste(levels, collapse = " "), " yielded any data for the specified model and model domain in grib file ",
                  file.name))
            }
            else {
                chunk.inds <- seq(1, length(model.data.vector) -
                  6, by = 7)
                model.run.date <- c(model.run.date, model.data.vector[chunk.inds])
                forecast.date <- c(forecast.date, model.data.vector[chunk.inds +
                  1])
                variables.tmp <- c(variables.tmp, model.data.vector[chunk.inds +
                  2])
                levels.tmp <- c(levels.tmp, model.data.vector[chunk.inds +
                  3])
                lon <- c(lon, as.numeric(model.data.vector[chunk.inds +
                  4]))
                lat <- c(lat, as.numeric(model.data.vector[chunk.inds +
                  5]))
                value <- c(value, model.data.vector[chunk.inds +
                  6])
            }
        }
    }
    else if (file.type == "grib1") {
        op <- options("warn")
        options(warn = -1)
        test <- tryCatch(system("wgrib", intern = TRUE, ignore.stderr = TRUE))
        options(op)
        if (attr(test, "status") != 8) {
            stop("wgrib does not appear to be installed, or it is not on the PATH variable.\n                  You can find wgrib here: http://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html.\n It is also available as an Ubuntu package.")
        }
        value          <- c()
        variables.tmp  <- c()
        levels.tmp     <- c()
        model.run.date <- c()
        forecast.date  <- c()
        lat            <- c()
        lon            <- c()
        for (file.name in file.names) {
            if (!file.exists(file.name)) {
                warning(paste("Grib file", file.name, "was not found."))
                next
            }
            for (var in variables) {
                for (lvl in levels) {
                  wg.str <- paste0("wgrib -s ", file.name, " | grep ",
                    "\":", var, ":", lvl, ":\" | wgrib -V -i -text ",
                    file.name, " -o tmp.txt")
                    print(wg.str)
                  if (Sys.info()[["sysname"]] == "Windows") {
                      out <- shell(wg.str, intern = TRUE, ignore.stderr = TRUE)
                  } else {
                      out <- system(wg.str, intern = TRUE, ignore.stderr = TRUE)
                  }
                 
                  #Figure out dates
                  model.run.tmp <- as.POSIXct(
                      stringr::str_extract(stringr::str_extract(
                      out[1], "date \\d+"), "\\d+"), 
                      format = "%Y%m%d%H", 
                      tz = "UTC")

                  if(grepl("anl:$", out[1])) {
                      hr <- 0
                  } else if (grepl("\\d+hr fcst$")) {
                      hr <- as.numeric(
                          stringr::str_extract(stringr::str_extract(
                          out[1], "\\d+hr fcst:$"), "\\d+"))
                  } else {
                      stop("Did not recognise forcast format.  Please contact the rNOMADS developer.")
                  }
                  fcst.tmp <- model.run.tmp + hr * 3600

                  #Figure out grid
                  lat.i <- which(grepl(" lat ", out))
                  lon.i <- which(grepl(" long ", out))
                  lat.def <- as.numeric(stringr::str_extract_all(out[lat.i], "-?\\d+\\.?\\d+")[[1]])
                  lon.def <- as.numeric(stringr::str_extract_all(out[lon.i], "-?\\d+\\.?\\d+")[[1]])

                  if((lat.def[2] < lat.def[1]) & (lat.def[3] > 0)) {
                       lats <- seq(lat.def[1], lat.def[2], -1 * lat.def[3])
                  } else {
                       lats <- seq(lat.def[1], lat.def[2], by = lat.def[3])
                  }
                  
                  if(lon.def[2] < 0) {
                      lons <- seq(lon.def[1], lon.def[2] + lon.def[4], by = lon.def[3])
                  } else {
                      lons <- seq(lon.def[1], lon.def[2], by = lon.def[3])
                  }

                  if((length(lats) != lon.def[5]) | (length(lons) != lon.def[4])) {
                      stop(paste0("Grid definition error: Expected ", 
                          lon.def[4], 
                          " longitudes but found ", 
                          length(lons), 
                          "; expected ", 
                          lon.def[5], 
                          " latitudes but found ", 
                          length(lats), "."))
                  }

                  value          <- c(value, scan("tmp.txt", skip = 1,
                                    quiet = TRUE))
                  variables.tmp <- c(variables.tmp, rep(var, length(value)))
                  levels.tmp     <- c(levels.tmp, rep(lvl, length(value)))
                  model.run.date <- c(model.run.date, rep(model.run.tmp, length(value)))
                  forecast.date  <- c(forecast.date, rep(fcst.tmp, length(value))) 
                  lon            <- c(lon, rep(lons, length(lats)))
                  lat            <- c(lat, 
                                     as.vector(t(array(rep(lats, length(lons)), 
                                     dim = c(length(lats), length(lons))))))
                }
            }
            
        }
    }

    #Remove unwanted variable/level combinations extracted by partial grep matches
    v.i <- rep(0, length(variables.tmp))
    l.i <- v.i
    for (k in 1:length(variables)) {
        v.i <- v.i + (variables.tmp == variables[k])
    }
    for (k in 1:length(levels)) {
       l.i <- l.i + (levels.tmp == stringr::str_replace_all(levels[k],
           "\\\\", ""))
    }
    k.i <- which(v.i & l.i)
    model.data <- list(model.run.date = model.run.date[k.i],
        forecast.date = forecast.date[k.i], variables = variables.tmp[k.i],
        levels = levels.tmp[k.i], lon = lon[k.i], lat = lat[k.i],
        value = value[k.i],
        grib.type = file.type)

    model.data$value <- as.numeric(unlist(model.data$value))
    return(model.data)
}
