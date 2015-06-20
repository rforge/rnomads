#Test behavior of ModelGrid and BuildProfile

#First get an actual data set, and replace values with made up ones

library(rNOMADS)

set.seed(628) 

models <- c(
    "gfs_0p50",         #First test GFS 0.5 x 0.5 (rectangular grid, square node spacing)
    "gfs_0p50",         #Second test, GFS 0.5 x 0.5, lat lon subset
    "gfs_0p25",         #GFS 0.5 x 0.5 (rectangular grid, dense node spacing
    "hrrr")             #High res rapid refresh, nonsquare grid

lats <- list(
    c(0, 360),
    c(40, 100), #Subset of GFS 0.50
    c(0, 720),
    c(555, 704)) #Adam's subset of HRRR

lons <- list(
    c(0, 719),
    c(10, 700), #Subset of GFS 0.50
    c(0, 1439), 
    c(344, 482)) #Adam's subset of HRRR

variables <- list(
   c("tmpprs", "rhprs"),
   c("tmpprs", "rhprs"),
   c("tmpprs", "rhprs"),
   c("vgrd10m","ugrd10m", "tmp2m","pressfc","gustsfc"))

levels <- list(
   c(1, 2),
   c(1, 2),
   c(1, 2),
   c(NA))

resolutions <- list(
    c(0.5, 0.5),
    c(0.5, 0.5),
    c(0.25, 0.25),
    c(0.25, 0.25),
    c(0.029, 0.027))

#First test GFS 0.5 x 0.5 (rectangular grid, square node spacing)

for(k in 1:length(models)) {
    
        model.urls <- GetDODSDates(models[k])
        latest.model <- tail(model.urls$url, 1)
        model.runs <- GetDODSModelRuns(latest.model)
        latest.model.run <- tail(model.runs$model.run, 1)
        
        time <- c(0,0) #Analysis model
        lon <- lons[[k]] #All 720 longitude points
        lat <- lats[[k]] #All 361 latitude points
        level <- levels[[k]] 
        variable <- variables[[k]]
        
        if(!is.na(level)) {
            dods.data <- DODSGrab(latest.model, latest.model.run,
                variable, time, lon, lat, level = level)
        } else {
            dods.data <- DODSGrab(latest.model, latest.model.run,
                variable, time, lon, lat)
        }
        
        
        
    for(i in 1:2) { 
        out.file <- paste0(sprintf("%02i", k), models[k], "_", sprintf("%03i", i), ".txt")
        write(paste("MODEL", models[k], "TRIAL", i), file = out.file)
        write(paste("LATS", lat), file = out.file, append = TRUE)
        write(paste("LONS", lon), file = out.file, append = TRUE)
        write(paste("LEVELS", level), file = out.file, append = TRUE)
        write(paste("VARIABLES", variable), file = out.file, append = TRUE)
         
        #Now replace values with my specified ones, this will help determine
        #if ModelGrid and BuildProfile are working right
        test.data <- dods.data
        
        test.lat <- sample(dods.data$lat, 1)
        test.lon <- sample(dods.data$lon, 1)
        test.data$value <- runif(length(dods.data$value), 0, 1)
       
        write(paste("\nTEST LAT", test.lat), file = out.file, append = TRUE) 
        write(paste("TEST LON", test.lon), file = out.file, append = TRUE) 

        #Replace with random numbers
        for(lev in unique(dods.data$levels)) {
            for(var in unique(dods.data$variables)) {
                vl.i <- dods.data$variables == var & dods.data$levels == lev & dods.data$lon == test.lon & dods.data$lat == test.lat
                test.data$value[which(vl.i)] <- runif(1, 10, 20)
            }
        }
           
        
        #Test ModelGrid
         
        atmos <- ModelGrid(test.data, c(0.5, 0.5))
        
        for(l in 1:dim(atmos$z)[1]) {
           for(m in 1:dim(atmos$z)[2]) {
               write(paste(variable[m], level[l], atmos$z[l, m, which(atmos$x == test.lon), which(atmos$y == test.lat)] >= 10), file = out.file, append = TRUE) #Should evaluate to TRUE if ModelGrid worked right
           }
        }
        
        #Test BuildProfile, no spatial average
        
        profile <- BuildProfile(atmos, test.lon, test.lat, spatial.average = FALSE)
        write(paste("Profile", all(profile > 10)), file = out.file, append = TRUE)
        save(dods.data, test.data, atmos, profile, file = paste0(sprintf("%02i", k), "_", models[k], "_", sprintf("%03i", i), ".RData"))
        print(paste("Model", models[k], "Trial", i, "complete"))
    } 
}
