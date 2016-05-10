#Test DODS ensemble downloading and subset capability

#Get library and source updated files (change path to reflect your computer's setup)

library(rNOMADS)
#Change path!

source("~/research/furiousblue/CODE/rnomads/trunk/rNOMADS/rNOMADS/R/GetDODS.R")
source("~/research/furiousblue/CODE/rnomads/trunk/rNOMADS/rNOMADS/R/RNomadsTools.R")

#Get the latest ensemble model run
model.urls <- GetDODSDates("gens")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
model.run <- tail(model.runs$model.run[grepl("all", model.runs$model.run)], 1)

#Define region of interest: Chapel Hill, NC
lon <- -79.052104
lat <- 35.907553

lons <- seq(0, 359, by = 1)
lats <- seq(-90, 90, by = 1)

lon.diff <- abs(lon + 360 - lons)
lat.diff <- abs(lat - lats)

model.lon.ind <- which(lon.diff == min(lon.diff)) - 1
model.lat.ind <- which(lat.diff == min(lat.diff)) - 1

#Set up call to NOMADS
time <- c(0, 0)      #Analysis(?) model only
node.lon  <- c(model.lon.ind - 2, model.lon.ind + 2)  #Longitude grid
node.lat  <- c(model.lat.ind - 2, model.lat.ind + 2)   #Latitude grid
variables <- c("tmpprs", "ugrdprs", "vgrdprs", "hgtprs") #Temperature, wind speeds, and geopotential height
levels    <- c(0, 25) #All available levels
ensemble <- c(0, 20)  #All available ensembles

model.data <- DODSGrab(latest.model, model.run, variables, time, node.lon, node.lat, levels = levels, ensemble = ensemble)

#Plot temperature 
plot(c(-65, 25), c(0, 35), xlab = "Temperature (C)", ylab = "Geopotential Height (km)", type = 'n')

for(k in ((ensemble[1]:ensemble[2] + 1))) {
    model.data.sub <- SubsetNOMADS(model.data, ensemble = c(k), variables = c("hgtprs", "tmpprs"))
    profile <- BuildProfile(model.data.sub, lon, lat)
    points(profile[[1]]$profile.data[, which(profile[[1]]$variables == "tmpprs"), 1] - 272.15, profile[[1]]$profile.data[, which(profile[[1]]$variables == "hgtprs"), 1] / 1000)
}

#Plot winds
zonal.wind <- NULL
merid.wind <- NULL
height     <- NULL

for(k in ((ensemble[1]:ensemble[2] + 1))) {
    model.data.sub <- SubsetNOMADS(model.data, ensemble = c(k), variables = c("hgtprs", "ugrdprs", "vgrdprs"))
    profile <- BuildProfile(model.data.sub, lon, lat)
    hgt     <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "hgtprs"),]    
    ugrd    <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "ugrdprs"),]
    vgrd    <- profile[[1]]$profile.data[, which(profile[[1]]$variables == "vgrdprs"),]

   synth.hgt <- seq(min(hgt),
       max(hgt), length.out = 1000)
   ugrd.spline <- splinefun(hgt, ugrd, method = "natural")
   vgrd.spline <- splinefun(hgt, vgrd, method = "natural")
   zonal.wind[[k]] <- ugrd.spline(synth.hgt)
   merid.wind[[k]] <- vgrd.spline(synth.hgt)
   height[[k]] <- synth.hgt
}

dev.new()
PlotWindProfile(zonal.wind, merid.wind, height, lines = TRUE,
    points = FALSE, elev.circles = c(0, 15000, 30000), elev.labels = c(0, 15, 30),
    radial.lines = seq(45, 360, by = 45), colorbar = TRUE, invert = FALSE,
    point.cex = 2, pch = 19, lty = 1, lwd = 1,
    height.range = c(0, 30000), colorbar.label = "Wind Speed (m/s)")
