# Recreating models for temporally adaptive sampling paper 

# Call in necessary packages
library(AMModels)
library(data.table)
library(dplyr)
library(ggplot2)
library(here)
library(RSQLite)

# Source necessary functions into the global environment
source('functions.R')

# Set working directory path to access files throughout this script
# (note: the here function attempts to finds your working directory; 
# set your working directory manually if the path appears incorrect)
path <- here::here()
path <- paste0(path, '/')

# Create model equations
equations <- list(
  btgn_vocals = list(
    names = c('intercept', 
              'dayOfYear', 'daySin',
              'hourCos12', 
              'distRise.2',
              'temperature','temperature.3', 
              'windSpeed'),
    coefficients = c(-0.3, 
                     -0.002, 1, 
                     -0.5, 
                     -0.000007, 
                     0.009, -0.000001, 
                     -0.35)
  ),
  copo_vocals = list(
    names = c('intercept', 
              'dayOfYear', 
              'dayCos', 'daySin', 
              'hourCos24', 'hourCos12',
              'distRise', 'distSet', 
              'windSpeed',
              'moonCos'),
    coefficients = c(-1.5, 
                     -0.003, 
                     0.5, 0.6, 
                     1, -0.5,             
                     -0.0005, -0.0005, 
                     -0.1, 
                     -0.2)
  ),
  toad_vocals = list(
    names = c('intercept', 
              'dayCos', 'daySin',
              'hourCos24', 
              'rainAccum24'),
    coefficients = c(-8, 
                     -1, -2,
                     3, 
                     5)
  ),
  coyote_vocals = list(
    names = c('intercept', 
              'dayCosEquinox', 'daySinEquinox', 
              'hourCos24', 'hourCos12', 
              'distRise', 'distSet', 
              'moonCos'),
    coefficients = c(-3, 
                     -0.5, 0.2, 
                     1, -0.5, 
                     -0.001, -0.001, 
                     0.2)
  ),
  ecdo_vocals = list(
    names = c('intercept', 
              'daySin', 
              'hourCos12', 
              'distRise.2', 
              'temperature', 'temperature.3', 
              'windSpeed'), 
    coefficients = c(-1.4, 
                     1, 
                     -2, 
                     -0.000005, 
                     0.009, -0.000001, 
                     -0.25)
  ),
  gaqu_vocals = list(
    names = c('intercept', 
              'dayOfYear', 'daySin',
              'hourCos12', 
              'distRise.2', 
              'temperature', 'temperature.3', 
              'windSpeed'), 
    coefficients = c(-1.2, 
                     -0.002, 1.3, 
                     -2, 
                     -0.000005, 
                     0.009, -0.000001, 
                     -0.25)
  ),
  leni_vocals = list(
    names = c('intercept', 
              'dayOfYear', 
              'dayCos', 'daySin', 
              'hourCos24', 'hourCos12',
              'distRise', 'distSet', 
              'windSpeed',
              'moonCos'),
    coefficients = c(-2, 
                     -0.006, 
                     0.4, 0.7, 
                     1, -0.5, 
                     -0.0005, -0.0005, 
                     -0.25, 
                     -0.3)
  ),
  phai_vocals = list(
    names = c('intercept', 
              'dayOfYear.2', 'dayCos', 'daySin', 
              'hourCos12', 
              'distRise.2', 
              'temperature','temperature.3', 
              'windSpeed'), 
    coefficients = c(-2.2, 
                     -0.00001, 0.7, 2.2, 
                     -2.5, 
                     -0.000004, 
                     0.009, -0.000001, 
                     -0.25)
  ),
  verd_vocals = list(
    names = c('intercept', 
              'dayOfYear', 'daySin',
              'hourCos12', 
              'distRise.2',
              'temperature', 'temperature.3', 
              'windSpeed'),
    coefficients = c(-0.5, 
                     -0.004, 1, 
                     -1.5, 
                     -0.000007, 
                     0.009, -0.000001, 
                     -0.25)
  )
)

# Read in the sampled 10% of the temporal data
# (no lat longs are contained in these data)
sampled.temporals <- readRDS(paste0(path, 'sampled-temporals.RDS'))

# Add variables that accommodate circular (angular)
# variables within the temporal data
sampled.temporals <- scheduleAddVars(sampled.temporals)

# Reorder based on the index from the original data
setkey(sampled.temporals, index)

# Create vocalization models
mods <- simGlm(data = sampled.temporals,
               equation.list = equations,
               model.family = 'binomial',
               model.type = 'activity', 
               seed = 10)

# Turn these into AMModels 
# Create a new model library
amml <- amModelLib(description = "Vocalization models built on 10% of the 2016 temporal data",
                   info = list(owner = "Cathleen Balantic"))

# Add glm models to the model library
# need to create metadata that has speciesID in it
btgn <- amModel(model = mods$btgn_vocals)
copo <- amModel(model = mods$copo_vocals)
toad <- amModel(model = mods$toad_vocals)
coyote <- amModel(model = mods$coyote_vocals)
ecdo <- amModel(model = mods$ecdo_vocals)
gaqu <- amModel(model = mods$gaqu_vocals)
leni <- amModel(model = mods$leni_vocals)
phai <- amModel(model = mods$phai_vocals)
verd <- amModel(model = mods$verd_vocals)

amml <- insertAMModelLib(models = list(btgn = btgn, copo = copo, toad = toad, 
                                       coyote = coyote, ecdo = ecdo, gaqu = gaqu, 
                                       leni = leni, phai = phai, verd = verd),
                         amml = amml)

# create a named list of of metadata that can be added to all objects
model.meta <- list(type = 'vocalization model', 
                   class = 'glm', 
                   model.family = 'binomial',
                   speciesID = 'NA',
                   objectiveID = 'NA',
                   package = 'AMMonitor', 
                   functionName = 'simGlm')

# Add model metadata with speciesID to each amModel
speciesIDS <- c("btgn", "copo", "coyote", "ecdo", "gaqu", 
                "leni", "phai", "toad",  "verd")

for (i in 1:length(speciesIDs)) {
  model.meta$speciesID <- speciesIDs[i]
  AMModels::modelMeta(amml = amml, x = speciesIDs[i]) <- model.meta
}

# Save models
saveRDS(amml, paste0(path, 'ammls/vocalization-models.RDS'))

# Read in models if starting here: 
amml <- readRDS(paste0(path,'ammls/vocalization-models.RDS'))

