
#Delta Breeze script 
library("rNOMADS") 
library("stringr")
library("dplyr")
library("ggplot2")
library("GEOmap")

HRRR.list <- GetDODSDates(abbrev = "hrrr", archive = F, request.sleep = 1)

current.HRRR.url <- tail(HRRR.list$url, 1)

HRRR.model.runs <- GetDODSModelRuns(current.HRRR.url)


Current.HRRR.run <- tail(HRRR.model.runs$model.run,1)

HRRR.info <- GetDODSModelRunInfo(current.HRRR.url, Current.HRRR.run) #run when you need to find different variables and model info 

lon <- c(344, 482) # northern California Longitude HRRR Delta Breeze
lat <- c(555, 704) # northern California Latitude HRRR Delta Breeze 

variables <- c("tmp2m")

HRRR.model.data <- DODSGrab(model.url = current.HRRR.url, model.run = Current.HRRR.run,variables = variables, time = c(0,0), lon, lat)

HRRR.modelgrid <- ModelGrid(HRRR.model.data, resolution = c(0.029, 0.027))
  
####################################################################################
Stations <- c("KSFO- San Francisco", "KSAC- Sacramento", "KLHM- Roseville")
Latitude <- c(37.619,38.909, 38.737)
Longitude <- c(-122.365, -121.495, -121.309)

Lat <- c(37.619, 38.909, 38.737)
Lon <- c(-122.365, -121.495, -121.309)


prefix  <-  "S:/Weather Forecasts/R programming/GFS data/HRRR" # each file begins with this text 
suffix  <-  ".RData" # each file ends with this text 

hrrr.test <- array(-1, dim = c(3, 3)) #Fill this with test values, all rows should have the same values
#Let's run a little test
for (k in 1:length(Lat)){

   #First, calculate nearest model node from DODSgrab
   deg.dist <- sqrt((HRRR.model.data$lon - Longitude[k])^2 + (HRRR.model.data$lat - Latitude[k])^2)
   t.i <- which(deg.dist == min(deg.dist))
   hrrr.test[1,k] <- HRRR.model.data$value[t.i]

   #Now, calculate nearest model node from ModelGrid
   lon.i <- which(abs(HRRR.modelgrid$x - Longitude[k]) == min(abs(HRRR.modelgrid$x - Longitude[k])))    
   lat.i <- which(abs(HRRR.modelgrid$y - Latitude[k]) == min(abs(HRRR.modelgrid$y - Latitude[k])))
   hrrr.test[2,k] <- HRRR.modelgrid$z[1,1,lon.i, lat.i]

   #Now use BuildProfile
   hrrr.test[3,k] <- BuildProfile(HRRR.modelgrid, Longitude[k], Latitude[k], spatial.average = FALSE)
}


#Okay, that's messed up...let's try GFS hires

GFS.list <- GetDODSDates(abbrev = "gfs_0p25", archive = F, request.sleep = 1)

current.GFS.url <- tail(GFS.list$url, 1)

GFS.model.runs <- GetDODSModelRuns(current.GFS.url)


Current.GFS.run <- tail(GFS.model.runs$model.run,1)

GFS.info <- GetDODSModelRunInfo(current.GFS.url, Current.GFS.run) #run when you need to find different variables and model info 

lon <- c(0, 1439) # northern California Longitude GFS Delta Breeze
lat <- c(0, 720) # northern California Latitude GFS Delta Breeze 

variables <- c("tmp80m") #Apparently this GFS doesn't have tmp2m...??

GFS.model.data <- DODSGrab(model.url = current.GFS.url, model.run = Current.GFS.run,variables = variables, time = c(0,0), lon, lat)

GFS.modelgrid <- ModelGrid(GFS.model.data, resolution = c(0.25, 0.25))

###################################################################################
Stations <- c("KSFO- San Francisco", "KSAC- Sacramento", "KLHM- Roseville")
Latitude <- c(37.619,38.909, 38.737)
Longitude <- c(-122.365, -121.495, -121.309)

Lat <- c(37.619, 38.909, 38.737)
Lon <- c(-122.365, -121.495, -121.309)

gfs.test <- array(-1, dim = c(3, 3)) #Fill this with test values, all rows should have the same values
#Let's run a little test
for (k in 1:length(Lat)){

   #First, calculate nearest model node from DODSgrab
   deg.dist <- sqrt((GFS.model.data$lon - Longitude[k])^2 + (GFS.model.data$lat - Latitude[k])^2)
   t.i <- which(deg.dist == min(deg.dist))
   gfs.test[1,k] <- GFS.model.data$value[t.i]

   #Now, calculate nearest model node from ModelGrid
   lon.i <- which(abs(GFS.modelgrid$x - Longitude[k]) == min(abs(GFS.modelgrid$x - Longitude[k])))
   lat.i <- which(abs(GFS.modelgrid$y - Latitude[k]) == min(abs(GFS.modelgrid$y - Latitude[k])))
   gfs.test[2,k] <- GFS.modelgrid$z[1,1,lon.i, lat.i]

   #Now use BuildProfile
   gfs.test[3,k] <- BuildProfile(GFS.modelgrid, Longitude[k], Latitude[k], spatial.average = FALSE)
}

#Now GFS lower res

#Okay, that's messed up...let's try GFS hires

GFS.list <- GetDODSDates(abbrev = "gfs_0p50", archive = F, request.sleep = 1)

current.GFS.url <- tail(GFS.list$url, 1)

GFS.model.runs <- GetDODSModelRuns(current.GFS.url)


Current.GFS.run <- tail(GFS.model.runs$model.run,1)

GFS.info <- GetDODSModelRunInfo(current.GFS.url, Current.GFS.run) #run when you need to find different variables and model info 

lon <- c(0, 719) # northern California Longitude GFS Delta Breeze
lat <- c(0, 360) # northern California Latitude GFS Delta Breeze 

variables <- c("tmp80m") #Apparently this GFS doesn't have tmp2m...??

GFS.model.data <- DODSGrab(model.url = current.GFS.url, model.run = Current.GFS.run,variables = variables, time = c(0,0), lon, lat)

GFS.modelgrid <- ModelGrid(GFS.model.data, resolution = c(0.5, 0.5))

###################################################################################
Stations <- c("KSFO- San Francisco", "KSAC- Sacramento", "KLHM- Roseville")
Latitude <- c(37.619,38.909, 38.737)
Longitude <- c(-122.365, -121.495, -121.309)

Lat <- c(37.619, 38.909, 38.737)
Lon <- c(-122.365, -121.495, -121.309)

gfs.test <- array(-1, dim = c(3, 3)) #Fill this with test values, all rows should have the same values
#Let's run a little test
for (k in 1:length(Lat)){

   #First, calculate nearest model node from DODSgrab
   deg.dist <- sqrt((GFS.model.data$lon - Longitude[k])^2 + (GFS.model.data$lat - Latitude[k])^2)
   t.i <- which(deg.dist == min(deg.dist))
   gfs.test[1,k] <- GFS.model.data$value[t.i]

   #Now, calculate nearest model node from ModelGrid
   lon.i <- which(abs(GFS.modelgrid$x - Longitude[k]) == min(abs(GFS.modelgrid$x - Longitude[k])))
   lat.i <- which(abs(GFS.modelgrid$y - Latitude[k]) == min(abs(GFS.modelgrid$y - Latitude[k])))
   gfs.test[2,k] <- GFS.modelgrid$z[1,1,lon.i, lat.i]

   #Now use BuildProfile
   gfs.test[3,k] <- BuildProfile(GFS.modelgrid, Longitude[k], Latitude[k], spatial.average = FALSE)
}


