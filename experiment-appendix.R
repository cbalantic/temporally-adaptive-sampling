# Run the maxperhour experiment from temporally adaptive sampling paper

# Call in necessary packages
library(AMModels)
library(data.table)
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

# Connect to the March-specific SQLite database
db.path <- paste0(path, 'experiment-march.sqlite')
conx <- dbConnect(drv = dbDriver('SQLite'), dbname = db.path)


# OPTIMIZATION WITH MAX.PER.HOUR OPTION -- MARCH ==============================

# Read in models if starting here
amml <- readRDS(paste0(path,'ammls/vocalization-models.RDS'))

# Set the experiment folder
exp.folder <- paste0(path, 'Results/March_Only/')

# Set the names of the experiments
exp.names.optim <- c('optim-20-max-per-hour-10.RDS',
                     'optim-30-max-per-hour-10.RDS',
                     'optim-40-max-per-hour-10.RDS')

# Set the daily N sampling constraints 
constraints <- c(20, 30, 40)

# Gather the speciesIDs for the March experiment (no toad!)
speciesIDs <- c('btgn', 'copo', 'coyote', 
                'ecdo', 'gaqu', 'leni', 
                'phai', 'verd') 

# Run through each set of constraints
for (ex in 1:length(constraints)) {
  
  # Reset tables
  dbClearTables(db.path, c('priorities', 'prioritization', 'schedule'))
  
  # Set equal weight priorities for all 8 species at all 133 locations
  # in the full year study
  prioritySet(db.path = db.path,
              speciesID = speciesIDs, 
              p.max = 0.95, 
              db.insert = TRUE)
  
  # Initialize monitoring to begin on 2016-03-01
  priorityInit(db.path = db.path, 
               init.date = '2016-03-01')
  
  cat('Experiment #', ex,'\n')
  start <- Sys.time()
  scheds <- list()
  for (i in 1:30) {
    cat(i,'\n')
    
    # Run schedule optimization for this day 
    # using 'max.per.hour' as the optimization approach
    output <- scheduleOptim(db.path = db.path, 
                            amml = amml, 
                            choose.models = speciesIDs, 
                            optimization.approach = 'max.per.hour', 
                            max.per.hour = 10,
                            daily.site.constraint = constraints[ex], 
                            time.units = 'hourly',
                            db.insert = TRUE,
                            google.push = FALSE) 
    scheds[[i]] <- output$schedule
  } # end looping through dates
  
  total.time <- Sys.time() - start
  
  # Collect and save the results
  results <- list(pc.table = data.table(dbReadTable(conx, 'prioritization')), 
                  schedule = rbindlist(l = scheds), 
                  run.time = total.time)
  saveRDS(results, paste0(exp.folder, exp.names.optim[ex]))             
} # end looping thru sampling constraints


# COMPARE FIXED & OPTIM SIMPLE & OPTIM MAX.PER.HOUR ===========================

# Set experiment folder
exp.folder <- paste0(path, 'Results/March_Only/')

exps <- sort(list.files(path = exp.folder, pattern = '-20|-30|-40'))
nam <- c('fixed20', 'fixed30', 'fixed40', 
         'maxhr20', 'optim20', 'maxhr30', 
         'optim30', 'maxhr40', 'optim40')
for (i in 1:length(exps)) assign(x = nam[i], value = readRDS(paste0(exp.folder, exps[i])))
all.results <- list(twenty.samples = list(maxhr = maxhr20,
                                          optim = optim20,
                                          fixed = fixed20,
                                          n = 20),
                    thirty.samples = list(maxhr = maxhr30,
                                          optim = optim30,
                                          fixed = fixed30,
                                          n = 30),
                    forty.samples = list(maxhr = maxhr40,
                                         optim = optim40,
                                         fixed = fixed40,
                                         n = 40))
all.pc <- list()
for (k in 1:length(all.results)) {
  results <- all.results[[k]]
  fixed <- results$fixed$pc.table
  opt <- results$optim$pc.table
  maxhr <- results$maxhr$pc.table
  N <- results$n
  
  # Fix column names
  opt[,c('weight', 'init') := NULL]
  maxhr[,c('weight', 'init') := NULL]
  opt$type <- 'opt'
  maxhr$type <- 'maxhr'
  fixed$type <- 'fixed'
  setcolorder(x = fixed, neworder = colnames(opt)) #reorder column names if needed
  pc <- rbindlist(list(opt, maxhr, fixed))
  all.pc[[k]] <- pc
  
  # Calculate the average date of meeting pMax across locations
  setkeyv(opt, c('locationID', 'date', 'speciesID'))
  setkeyv(maxhr, c('locationID', 'date', 'speciesID'))
  setkeyv(fixed, c('locationID', 'date', 'speciesID'))
  locs <- unique(pc$locationID)
  focals <- unique(pc$speciesID)
  met_pmax_opt <- met_pmax_maxhr <- met_pmax_fixed <- opt[0,]
  for (L in 1:length(locs)) {
    print(L)
    loc <- locs[L]
    opt_loc <- opt[locationID == loc]
    maxhr_loc <- maxhr[locationID == loc]
    fixed_loc <- fixed[locationID == loc]
    for (S in 1:length(focals)) {
      met_pmax_opt <- rbind(met_pmax_opt,
                            opt_loc[which(speciesID == focals[S]
                                          & pCurrent > pMax)][1])
      met_pmax_maxhr <- rbind(met_pmax_maxhr,
                              maxhr_loc[which(speciesID == focals[S]
                                              & pCurrent > pMax)][1])
      met_pmax_fixed <- rbind(met_pmax_fixed,
                              fixed_loc[which(speciesID == focals[S]
                                              & pCurrent > pMax)][1])
    }
  }
  met_pmax_opt <- na.omit(met_pmax_opt)
  met_pmax_maxhr <- na.omit(met_pmax_maxhr)
  met_pmax_fixed <- na.omit(met_pmax_fixed)
  pmax_opt <- met_pmax_opt[, mean(as.Date(date)), by = 'speciesID']
  pmax_maxhr <- met_pmax_maxhr[, mean(as.Date(date)), by = 'speciesID']
  pmax_fixed <- met_pmax_fixed[, mean(as.Date(date)), by = 'speciesID']
  comparison <- merge(pmax_opt, pmax_maxhr, by = 'speciesID', all.x = TRUE)
  comparison <- merge(comparison, pmax_fixed, by = 'speciesID', all.x = TRUE)
  colnames(comparison) <- c('speciesID', 'optim', 'maxhr', 'fixed')
  comparison$diff <- difftime(comparison[,optim], comparison[,maxhr], units = 'days')
  comparison$experiment <- N
  ifelse(k == 1,
         all.comparisons <- comparison,
         all.comparisons <- rbind(all.comparisons, comparison))
}
names(all.pc) <- names(all.results)

compare.dates <- melt(all.comparisons,
                      measure.vars = c('optim', 'maxhr', 'fixed'),
                      variable.name = 'Treatment')[,diff := NULL]
compare.dates[,speciesID := toupper(speciesID)]

# See pmax dates side by side
all.comparisons
howmuchearlier <- sort(all.comparisons$diff)
mean(howmuchearlier) 
sd(howmuchearlier) 
summary(as.numeric(howmuchearlier))

# Fix misbehaving dates
compare.dates[,value := as.Date(as.character(value))]

# SAVE FOR EASY PLOTTING
pc.and.compare.dates <- list(compare.dates = compare.dates,
                             all.pc = all.pc)
saveRDS(pc.and.compare.dates, paste0(exp.folder, 'pc-and-compare-dates-simple-vs-maxhr.RDS'))
