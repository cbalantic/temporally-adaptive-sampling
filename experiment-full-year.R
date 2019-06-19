# Run the "Full Year" experiment from temporal adaptive sampling paper

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

# Connect to the Full Year SQLite database
db.path <- paste0(path, 'experiment-full-year.sqlite')
conx <- dbConnect(drv = dbDriver('SQLite'), dbname = db.path)

# OPTIM TREATMENT -- FULL YEAR ==================================================

# Read in models if starting here: 
amml <- readRDS(paste0(path,'ammls/vocalization-models.RDS'))

# Set the experiment folder
exp.folder <- paste0(path, 'Results/Full_Year/')

# Set the names of the experiments
exp.names.optim <- c('optim-2-samples-all-placed-in-top-hour.RDS',
                     'optim-5-samples-all-placed-in-top-hour.RDS',
                     'optim-10-samples-all-placed-in-top-hour.RDS',
                     'optim-20-samples-all-placed-in-top-hour.RDS',
                     'optim-30-samples-all-placed-in-top-hour.RDS',
                     'optim-40-samples-all-placed-in-top-hour.RDS')

# Set the daily N sampling constraints (2 minutes ... 40 minutes)
constraints <- c(2, 5, 10, 20, 30, 40)

# Gather the speciesIDs for the full year experiment
speciesIDs <- c('btgn', 'copo', 'coyote', 
                'ecdo', 'gaqu', 'leni', 
                'phai', 'toad', 'verd') 

for (ex in 1:length(constraints)) {
  
  # Reset tables:
  dbClearTables(db.path, c('priorities', 'prioritization', 'schedule'))
  
  # Set equal weight priorities for all 9 species at all 133 locations
  # in the full year study
  prioritySet(db.path = db.path,
              speciesID = speciesIDs, 
              p.max = 0.95, 
              db.insert = TRUE)
  
  # Initialize monitoring to begin on 2016-01-01
  priorityInit(db.path = db.path, 
               init.date = '2016-01-01')
  
  cat('Experiment #', ex,'\n')
  start <- Sys.time()
  for (i in 1:365) {
    cat(i,'\n')
    
    # Run schedule optimization for this day
    output <- scheduleOptim(db.path = db.path, 
                            amml = amml, 
                            choose.models = speciesIDs, # the models are named after the speciesIDS
                            optimization.approach = 'simple', 
                            daily.site.constraint = constraints[ex], 
                            time.units = 'hourly',
                            db.insert = TRUE,
                            google.push = FALSE) 
    
  } # end looping through dates
  
  total.time <- Sys.time() - start
  
  # Collect and save the results
  results <- list(pc.table = data.table(dbReadTable(conx, 'prioritization')), 
                  schedule = data.table(dbReadTable(conx, 'schedule')), 
                  run.time = total.time)
  saveRDS(results, paste0(exp.folder, exp.names.optim[ex]))             
} # end looping thru sampling constraints



# FIXED TREATMENT -- FULL YEAR ================================================

# Read in models if starting here: 
amml <- readRDS(paste0(path,'ammls/vocalization-models.RDS'))

# Set experiment folder
exp.folder <- paste0(path, 'Results/Full_Year/')

# Set experiment names: 
exp.names <- c('fixed-2-samples.RDS', 'fixed-5-samples.RDS', 'fixed-10-samples.RDS',
               'fixed-20-samples.RDS', 'fixed-30-samples.RDS', 'fixed-40-samples.RDS')

# Set up a list of the fixed sampling times for all sampling efforts:
fixed.sampling.times.list <- list(two.samples = c('08:00:00', '23:00:00'),
                                  five.samples = c('02:00:00', '05:00:00', '06:00:00',
                                                   '08:00:00', '23:00:00'),
                                  ten.samples = c('00:00:00', '01:00:00', '02:00:00', '06:00:00',
                                                  '06:30:00', '07:00:00', '07:30:00', '08:00:00',
                                                  '22:00:00', '23:00:00'),
                                  twenty.samples = c('00:00:00', '01:00:00', '02:00:00', '03:00:00',
                                                     '04:00:00', '05:00:00', '05:30:00', '06:00:00',
                                                     '06:30:00', '07:00:00', '07:30:00', '08:00:00',
                                                     '18:00:00', '18:30:00', '19:00:00', '19:30:00',
                                                     '22:00:00', '22:30:00', '23:00:00', '23:30:00'),
                                  thirty.samples = c('00:00:00', '01:00:00', '01:30:00', '02:00:00',
                                                     '02:30:00', '03:00:00', '03:30:00', '04:00:00',
                                                     '04:30:00', '05:00:00', '05:30:00', '06:00:00',
                                                     '06:30:00', '07:00:00', '07:30:00', '08:00:00',
                                                     '08:30:00', '09:00:00', '09:30:00', '10:00:00',
                                                     '17:00:00', '17:30:00',
                                                     '18:00:00', '18:30:00', '19:00:00', '19:30:00',
                                                     '22:00:00', '22:30:00', '23:00:00', '23:30:00'),
                                  forty.samples = c('00:00:00', '00:30:00', '01:00:00', '01:30:00',
                                                    '02:00:00', '02:30:00', '03:00:00', '03:30:00',
                                                    '04:00:00', '04:30:00', '05:00:00', '05:30:00',
                                                    '05:45:00', '06:00:00', '06:15:00', '06:30:00',
                                                    '06:45:00', '07:00:00', '07:15:00', '07:30:00',
                                                    '07:45:00', '08:00:00', '08:15:00', '08:30:00',
                                                    '08:45:00', '09:00:00', '09:30:00', '10:00:00',
                                                    '17:00:00', '17:30:00', '18:00:00', '18:15:00',
                                                    '18:30:00', '18:45:00', '19:00:00', '19:30:00',
                                                    '22:00:00', '22:30:00', '23:00:00', '23:30:00'))

# Read in the temporals table, add any necessary additional variables manually
temporals <- data.table(dbReadTable(conx, 'temporals'))
temporals <- scheduleAddVars(temporals)
temporals[,'distRise.2' := distRise^2]
temporals[,'dayOfYear.2' := dayOfYear^2]
temporals[,'temperature.3' := temperature^3]

# Create a table to hold the probability of calling for each hour
# of the monitoring duration for each species:
pr.call <- data.table(cbind(temporals[,c('locationID', 'time')],
                            matrix(data = 0,
                                   nrow = nrow(temporals),
                                   ncol = length(names(amml@models)))))

# Order data tables by locationID & time:
setkeyv(pr.call, cols = c('locationID', 'time'))
setkeyv(temporals, cols = c('locationID', 'time'))

# Set pr.call for entire study season
for (i in 1:length(names(amml@models))) {
  this.species <- names(amml@models)[i]
  this.model <- amml@models[[i]]@model
  pr.call[,i + 2] <- predict(object = this.model, 
                             newdata = temporals, 
                             type = 'response')
  colnames(pr.call)[i + 2] <- this.species
}

# Loop through all 6 experiments and save results
for (ex in 1:length(exp.names)) {
  exp.name <- exp.names[ex]
  fixed.sampling.times <- fixed.sampling.times.list[[ex]]
  predictFixed(exp.name = exp.name, 
               exp.folder = exp.folder, 
               fixed.sampling.times = fixed.sampling.times, 
               pr.call = pr.call, 
               temporals = temporals, 
               tz = 'America/Los_angeles',
               voc.time.units = 'hourly')
}



# COMPARE FIXED & OPTIM =======================================================

# Set experiment folder
exp.folder <- paste0(path, 'Results/Full_Year/')

exps <- sort(list.files(path = exp.folder,pattern = 'samples'))
nam <- c('fixed10', 'fixed2', 'fixed20', 'fixed30', 'fixed40', 'fixed5',
         'optim10', 'optim2', 'optim20', 'optim30', 'optim40', 'optim5')
for (i in 1:length(exps)) assign(x = nam[i], value = readRDS(paste0(exp.folder, exps[i])))
all.results <- list(two.samples = list(fixed = fixed2,
                                       optim = optim2,
                                       n = 2),
                    five.samples = list(fixed = fixed5,
                                        optim = optim5,
                                        n = 5),
                    ten.samples = list(fixed = fixed10,
                                       optim = optim10,
                                       n = 10),
                    twenty.samples = list(fixed = fixed20,
                                          optim = optim20,
                                          n = 20),
                    thirty.samples = list(fixed = fixed30,
                                          optim = optim30,
                                          n = 30),
                    forty.samples = list(fixed = fixed40,
                                         optim = optim40,
                                         n = 40))
all.pc <- list()
for (k in 1:length(all.results)) {
  results <- all.results[[k]]
  fixed <- results$fixed$pc.table
  opt <- results$optim$pc.table
  N <- results$n
  
  # Fix column names
  opt[,c('weight', 'init') := NULL]
  colnames(fixed)[colnames(fixed) %in% c('p.current', 'p.max')] <- c('pCurrent', 'pMax')
  opt$type <- 'opt'
  fixed$type <- 'fixed'
  setcolorder(x = fixed, neworder = colnames(opt)) #reorder column names if needed?
  pc <- rbindlist(list(opt, fixed))
  all.pc[[k]] <- pc
  
  # Calculate the average date of meeting pMax, across locations
  setkeyv(opt, c('locationID', 'date', 'speciesID'))
  setkeyv(fixed, c('locationID', 'date', 'speciesID'))
  locs <- unique(pc$locationID)
  focals <- unique(pc$speciesID)
  met_pmax_opt <- met_pmax_fixed <- opt[0,]
  for (L in 1:length(locs)) {
    print(L)
    loc <- locs[L]
    opt_loc <- opt[locationID == loc]
    fixed_loc <- fixed[locationID == loc]
    for (S in 1:length(focals)) {
      met_pmax_opt <- rbind(met_pmax_opt,
                            opt_loc[which(speciesID == focals[S]
                                          & pCurrent > pMax)][1])
      met_pmax_fixed <- rbind(met_pmax_fixed,
                              fixed_loc[which(speciesID == focals[S]
                                              & pCurrent > pMax)][1])
    }
  }
  met_pmax_opt <- na.omit(met_pmax_opt)
  met_pmax_fixed <- na.omit(met_pmax_fixed)
  pmax_opt <- met_pmax_opt[, mean(as.Date(date)), by = 'speciesID']
  pmax_fixed <- met_pmax_fixed[, mean(as.Date(date)), by = 'speciesID']
  comparison <- merge(pmax_opt, pmax_fixed, by = 'speciesID', all.x = TRUE)
  colnames(comparison) <- c('speciesID', 'optim', 'fixed')
  comparison$diff <- difftime(comparison[,optim], comparison[,fixed], units = 'days')
  comparison$experiment <- N
  ifelse(k == 1,
         all.comparisons <- comparison,
         all.comparisons <- rbind(all.comparisons, comparison))
}
names(all.pc) <- names(all.results)

compare.dates <- melt(all.comparisons,
                      measure.vars = c('optim', 'fixed'),
                      variable.name
                      = 'Treatment')[,diff := NULL]
compare.dates[,speciesID := toupper(speciesID)]

# See pmax dates side by side:
all.comparisons
howmuchearlier <- sort(all.comparisons$diff)
mean(howmuchearlier) 
sd(howmuchearlier) 
summary(as.numeric(howmuchearlier))

# SAVE FOR APPENDIX
# (take this info into the "Tables" xlsx to create pretty tables)
full.year <- list(all.comparisons = all.comparisons,
                  average.earlier = mean(howmuchearlier),
                  sd.earlier = sd(howmuchearlier),
                  summary = summary(as.numeric(howmuchearlier)))

full.year$all.comparisons$speciesID <- toupper(full.year$all.comparisons$speciesID) 

full.year # ==> create Appendix table in excel 


# Not sure why the dates are suddenly misbehaving, but they are not always "equal" 
# so I'm coercing them to character and back to date
# https://stackoverflow.com/questions/21571744/compare-two-dates-in-r
compare.dates[,value := as.Date(as.character(value))]

# SAVE FOR EASY PLOTTING
pc.and.compare.dates <- list(compare.dates = compare.dates,
                             all.pc = all.pc)
saveRDS(pc.and.compare.dates, paste0(exp.folder, 'pc-and-compare-dates.RDS'))


