#Test behavior of ModelGrid and BuildProfile

library(rNOMADS)

model.urls <- GetDODSDates("gfs_0p50")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)

time <- c(0,0) #Analysis model
lon <- c(0, 719) #All 720 longitude points
lat <- c(0, 360) #All 361 latitude points


