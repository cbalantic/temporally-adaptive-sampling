# Recreating the Experiment for EcolEvol with latest version of AMMonitor

# cbalantic
# 2019-05-08

# Call in necessary packages
library(AMModels)
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(here)
library(RSQLite)

# Source necessary functions into the global environment
source('functions.R')

# Set working directory path to access files throughout this script
# (note: the here function attempts to finds your working directory; 
# set your working directory manually if the path appears incorrect)
path <- here::here()
path <- paste0(path, '/')


# Example ECDO vocalization model for Figure 1: ===============================

# Read in full year database
db.path <- paste0(path, 'experiment-full-year.sqlite')

# Connect to database
conx <- dbConnect(drv = dbDriver('SQLite'), dbname = db.path)

# Read in models 
amml <- readRDS(paste0(path,'ammls/vocalization-models.RDS'))

# Read in temporal data & subset the pattern for one location
temporal.dat <- data.table(dbReadTable(conx, 'temporals'))
temporal.dat <- temporal.dat[locationID == 'location@1']

# Add temporal data variables
temporal.dat <- scheduleAddVars(temporal.dat) 
temporal.dat[,'dayOfYear.2' := dayOfYear^2]
temporal.dat[,'distRise.2' := distRise^2]
temporal.dat[,'temperature.3' := temperature^3]

# Turn date into date data type
temporal.dat[,date := as.Date(date)]

focals <- sort(names(amml@models))

plot.types <- list(yearly = c('dayOfYear', 'dayOfYear.2', 'dayCos', 'daySin',
                              'dayCosEquinox', 'daySinEquinox'),
                   hourly = c('hour', 'hourSin24', 'hourCos24', 'hourSin12', 'hourCos12',
                              'distRise', 'distSet', 'distRise.2'),
                   temp = c('temperature', 'temperature.3'),
                   wind = 'windSpeed',
                   rain = 'rainAccum24',
                   moon = c('moonPhase', 'moonCos', 'moonSin'))
xaxes <- c('date', 'hour', 'temperature', 'windSpeed', 'rainAccum24', 'date')
plot.fodder.list <- list(btgn = list(),
                         copo = list(),
                         coyote = list(),
                         ecdo = list(),
                         gaqu = list(),
                         leni = list(),
                         phai = list(),
                         toad = list(),
                         verd = list())

# Grab categorical divergent plot colors: http://colorbrewer2.org/#type=qualitative&scheme=Paired&n=9
species_plot_colors <- c('#a6cee3', '#1f78b4', '#b2df8a',
                         '#33a02c', '#fb9a99', '#e31a1c',
                         '#fdbf6f', '#ff7f00', '#cab2d6')

# Loop through to shape up data.tables for plotting each 
# focal species and variable type
for (i in 1:length(focals)) {
  sp <- focals[i]
  this.model <- amml@models[[sp]]@model
  for (j in 1:6) {
    mod.DT <- temporal.dat
    mod.DT$intercept <- 1
    allvarnames <- plot.types[[j]]
    allcoefs <- this.model$coefficients
    varnames <- names(allcoefs)[names(allcoefs) %in% allvarnames]
    if (length(varnames) > 0) {
      coefs <- allcoefs[varnames]
      xaxis <- mod.DT[ , xaxes[j], with = FALSE]
      mod.DT <- mod.DT[ , c('intercept', varnames), with = FALSE]
      mod.DT$logit <- data.matrix(mod.DT) %*% c(0, coefs) # intercept 0
      mod.DT$pr <-  1/(1 + exp(-mod.DT$logit))
      plot.fodder <- cbind(xaxis, mod.DT$pr, rep(sp, length(xaxis)))
      colnames(plot.fodder) <- c(xaxes[j], 'pr', 'speciesID')
      if ('hour' %in% allvarnames) {
        plot.fodder <- plot.fodder[1:24,] # only need 24 examples for hourly
      }
      plot.fodder$sp.plot.col <- species_plot_colors[i]
      plot.fodder.list[[i]][[j]] <- plot.fodder
    } else {
      ifelse(xaxes[j] == 'date', type <- as.Date(0, origin = '1900-01-01'), type <- as.numeric(1))
      df <- data.table(x = type,
                       pr = as.numeric(1),
                       speciesID = as.character('a'),
                       sp.plot.col = as.character('a'))
      df <- df[0,]
      colnames(df)[1] <- xaxes[j]
      plot.fodder.list[[i]][[j]] <- df
    } # end if plot
  } # end for j
  names(plot.fodder.list[[i]]) <- names(plot.types)
} # end i

xaxes <- c('date', 'hour', 'temperature', 'windSpeed', 'rainAccum24', 'date')
xlabs <- c('Day of Year', 'Hour of Day', expression("Temperature "( degree~C)), 
           'Wind Speed (km/hr)', 'Rain Accumulation (mm)', 'Day of Year')
titles <- c('Day of Year', 'Hour of Day', expression("Temperature "( degree~C)), 
            'Wind Speed (km/hr)','Rain Accumulation (mm)', 'Moon Phase')
names(xlabs) <- names(xaxes) <- names(titles) <- c('yearly', 'hourly', 'temp', 'wind', 'rain', 'moon')

sp.col <- rep('black',9)
sp.lt <- rep('solid', 9)
names(sp.col) <- names(sp.lt) <- toupper(names(plot.fodder.list))
all_vars <- list()
just.ecdo <- plot.fodder.list$ecdo
for (i in 1:length(names(xaxes))) {
  x_name <- names(xaxes)[i]
  all_vars[[i]] <- ggVocModel(var = x_name,
                              x_axes = xaxes,
                              x_labs = xlabs,
                              titles = titles,
                              plot_fodder = just.ecdo,
                              species_colors = sp.col,
                              species_linetypes = sp.lt)
}
names(all_vars) <- names(xaxes)
all_vars <- all_vars[1:4]
x11(height = 3, width = 4) # may not work on mac
g <- do.call('grid.arrange', all_vars)

print(g)

# FIGURE 2: Visual demo. of sp. logistic regression vocalization models =======

# Read in full year database
db.path <- paste0(path, 'experiment-full-year.sqlite')

# Connect to database
conx <- dbConnect(drv = dbDriver('SQLite'), dbname = db.path)

# Read in models 
amml <- readRDS(paste0(path,'ammls/vocalization-models.RDS'))

# Read in temporal data & subset the pattern for one location
temporal.dat <- data.table(dbReadTable(conx, 'temporals'))
temporal.dat <- temporal.dat[locationID == 'location@1']

# Add temporal data variables
temporal.dat <- scheduleAddVars(temporal.dat)
temporal.dat[,'dayOfYear.2' := dayOfYear^2]
temporal.dat[,'distRise.2' := distRise^2]
temporal.dat[,'temperature.3' := temperature^3]

# Turn date into date data type
temporal.dat[,date := as.Date(date)]

focals <- sort(names(amml@models))

plot.types <- list(yearly = c('dayOfYear', 'dayOfYear.2', 'dayCos', 'daySin',
                              'dayCosEquinox', 'daySinEquinox'),
                   hourly = c('hour', 'hourSin24', 'hourCos24', 'hourSin12', 'hourCos12',
                              'distRise', 'distSet', 'distRise.2'),
                   temp = c('temperature', 'temperature.3'),
                   wind = 'windSpeed',
                   rain = 'rainAccum24',
                   moon = c('moonPhase', 'moonCos', 'moonSin'))
xaxes <- c('date', 'hour', 'temperature', 'windSpeed', 'rainAccum24', 'date')
plot.fodder.list <- list(btgn = list(),
                         copo = list(),
                         coyote = list(),
                         ecdo = list(),
                         gaqu = list(),
                         leni = list(),
                         phai = list(),
                         toad = list(),
                         verd = list())

# Grab categorical divergent plot colors: http://colorbrewer2.org/#type=qualitative&scheme=Paired&n=9
species_plot_colors <- c('#a6cee3', '#1f78b4', '#b2df8a',
                         '#33a02c', '#fb9a99', '#e31a1c',
                         '#fdbf6f', '#ff7f00', '#cab2d6')

# Loop through to shape up data.tables for plotting each 
# focal species and variable type
for (i in 1:length(focals)) {
  sp <- focals[i]
  this.model <- amml@models[[sp]]@model
  for (j in 1:6) {
    mod.DT <- temporal.dat
    mod.DT$intercept <- 1
    allvarnames <- plot.types[[j]]
    allcoefs <- this.model$coefficients
    varnames <- names(allcoefs)[names(allcoefs) %in% allvarnames]
    if (length(varnames) > 0) {
      coefs <- allcoefs[varnames]
      xaxis <- mod.DT[ , xaxes[j], with = FALSE]
      mod.DT <- mod.DT[ , c('intercept', varnames), with = FALSE]
      mod.DT$logit <- data.matrix(mod.DT) %*% c(0, coefs) # intercept 0
      mod.DT$pr <-  1/(1 + exp(-mod.DT$logit))
      plot.fodder <- cbind(xaxis, mod.DT$pr, rep(sp, length(xaxis)))
      colnames(plot.fodder) <- c(xaxes[j], 'pr', 'speciesID')
      if ('hour' %in% allvarnames) {
        plot.fodder <- plot.fodder[1:24,] # only need 24 examples for hourly
      }
      plot.fodder$sp.plot.col <- species_plot_colors[i]
      plot.fodder.list[[i]][[j]] <- plot.fodder
    } else {
      ifelse(xaxes[j] == 'date', type <- as.Date(0, origin = '1900-01-01'), type <- as.numeric(1))
      df <- data.table(x = type,
                       pr = as.numeric(1),
                       speciesID = as.character('a'),
                       sp.plot.col = as.character('a'))
      df <- df[0,]
      colnames(df)[1] <- xaxes[j]
      plot.fodder.list[[i]][[j]] <- df
    } # end if plot
  } # end for j
  names(plot.fodder.list[[i]]) <- names(plot.types)
} # end i

xaxes <- c('date', 'hour', 'temperature', 'windSpeed', 'rainAccum24', 'date')
xlabs <- c('Day of Year', 'Hour of Day', expression("Temperature "( degree~C)), 
           'Wind Speed (km/hr)', 'Rain Accumulation (mm)', 'Day of Year')
titles <- c('Day of Year', 'Hour of Day', expression("Temperature "( degree~C)), 
            'Wind Speed (km/hr)','Rain Accumulation (mm)', 'Moon Phase')
names(xlabs) <- names(xaxes) <- names(titles) <- c('yearly', 'hourly', 'temp', 'wind', 'rain', 'moon')

# Function for picking a color sequence
gg_color_hue <- function(n, seq1, seq2, L, C) {
  hues = seq(seq1, seq2, length = n + 1)
  hcl(h = hues, l = L, c = C)[1:n]
}
n <- 9; seq1 <- 15; seq2 <- 350; L <- 68; C <- 150 
sp.col <- gg_color_hue(n, seq1, seq2, L, C)
sp.lt <- c(rep('solid', 9))
names(sp.col) <- names(sp.lt) <- toupper(names(plot.fodder.list))

# Run the ggplot models for each covariate type
all_vars <- list()
for (i in 1:length(names(xaxes))) {
  x_name <- names(xaxes)[i]
  all_vars[[i]] <- ggVocModel(var = x_name,
                              x_axes = xaxes,
                              x_labs = xlabs,
                              titles = titles,
                              plot_fodder = plot.fodder.list,
                              species_colors = sp.col,
                              species_linetypes = sp.lt)
}
names(all_vars) <- names(xaxes)

x11(height = 6, width = 5)
g.full <- do.call('grid.arrange', all_vars)

# Finalize outside of R so that the legend can be combined with the figure


# FIGURE 3: Vocalization simulation results ===================================

# Connect to full year database
db.path <- paste0(path, 'experiment-full-year.sqlite')
conx <- dbConnect(drv = dbDriver('SQLite'), dbname = db.path)

# Read in models 
amml <- readRDS(paste0(path,'ammls/vocalization-models.RDS'))

# Read in the temporals table, add any necessary additional variables manually
temporals <- data.table(dbReadTable(conx, 'temporals'))
temporals <- scheduleAddVars(temporals) # may take a little while
temporals[,'distRise.2' := distRise^2]
temporals[,'dayOfYear.2' := dayOfYear^2]
temporals[,'temperature.3' := temperature^3]

focals <- sort(names(amml@models))

pr.call <- data.table(cbind(temporals[,c('locationID', 'time')],
                            matrix(data = 0,
                                   nrow = nrow(temporals),
                                   ncol = length(focals))))

# Order data tables by locationID & time:
setkeyv(pr.call, cols = c('locationID', 'time'))
setkeyv(temporals, cols = c('locationID', 'time'))

for (i in 1:length(focals)) {
  cat(i,'\n')
  this.species <- names(amml@models)[i]
  this.model <- amml@models[[i]]@model
  pr.call[,i + 2] <- predict(object = this.model, 
                             newdata = temporals, 
                             type = 'response')
  colnames(pr.call)[i + 2] <- this.species
}

pr.call[, month := lubridate::month(pr.call$time, label = TRUE)]
march.pr <- pr.call[month == 'Mar']

year.avgs <- data.table(sp = rep('phai',9),
                        mean = 0,
                        sd = 0,
                        se = 0,
                        lower.ci = 0,
                        upper.ci = 0,
                        highest = 0, 
                        lowest = 0,
                        type = 'Full Year')
march.avgs <- data.table(sp = rep('phai',9),
                         mean = 0,
                         sd = 0,
                         se = 0,
                         lower.ci = 0,
                         upper.ci = 0,
                         highest = 0,
                         lowest = 0,
                         type = 'March Only')

for (i in 1:9) {
  
  # Full year
  sp.yr <- colnames(pr.call)[i + 2]
  year.avgs[i, 'sp'] <- sp.yr
  year.avgs[i, 'mean'] <- mean(pr.call[,get(sp.yr)], na.rm = TRUE)
  year.avgs[i, 'se'] <- sd(pr.call[,get(sp.yr)], na.rm = TRUE)/sqrt(nrow(pr.call))
  year.avgs[i, 'lower.ci'] <- year.avgs[i,'mean'] - 2*year.avgs[i,'se']
  year.avgs[i, 'upper.ci'] <- year.avgs[i,'mean'] + 2*year.avgs[i,'se']
  year.avgs[i, 'sd'] <- sd(pr.call[,get(sp.yr)], na.rm = TRUE)
  year.avgs[i, 'highest'] <- range(na.omit(pr.call[,get(sp.yr)]))[2]
  year.avgs[i, 'lowest'] <- range(na.omit(pr.call[,get(sp.yr)]))[1]
  
  # March only
  sp.m <- colnames(march.pr)[i + 2]
  march.avgs[i, 'sp'] <- sp.m
  march.avgs[i, 'mean'] <- mean(march.pr[,get(sp.m)], na.rm = TRUE)
  march.avgs[i, 'se'] <- sd(march.pr[,get(sp.m)], na.rm = TRUE)/sqrt(nrow(march.pr))
  march.avgs[i, 'lower.ci'] <- march.avgs[i,'mean'] - 2*march.avgs[i,'se']
  march.avgs[i, 'upper.ci'] <- march.avgs[i,'mean'] + 2*march.avgs[i,'se']
  march.avgs[i, 'sd'] <- sd(march.pr[,get(sp.m)], na.rm = TRUE)
  march.avgs[i, 'highest'] <- range(na.omit(pr.call[,get(sp.m)]))[2]  
  march.avgs[i, 'lowest'] <- range(na.omit(pr.call[,get(sp.m)]))[1] 
}

av <- rbind(year.avgs, march.avgs)
av$sp <- toupper(av$sp)
colnames(av)[which(colnames(av) == 'type')] <- 'Study Duration'

windowsFonts(Times = windowsFont('Times New Roman'))
g <- ggplot(data = av, aes(x = sp, y = mean,
                           fill = `Study Duration`)) +
  geom_bar(stat = 'identity', position = position_dodge(0.9)) +
  
  # Plot standard devs with no below zero:
  geom_errorbar(aes(ymin = 0,
                    ymax = av$mean + av$sd),
                position = position_dodge(0.9),
                width = 0.1) +
  theme_bw() +
  scale_fill_manual(values = c('gray66', 'gray33')) +
  scale_y_continuous(limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1.0), expand = c(0,0)) + 
  ggtitle('Average Probability of Vocalization') +
  xlab('Species') +
  ylab('Average Probability of Vocalization') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        text = element_text(family = 'Times'),
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 8),
        axis.text = element_text(color = 'black', size = 8),
        axis.title = element_text(size = 10),
        panel.border = element_blank(),
        legend.title = element_text(size = 10)
  ) + 
  labs(fill = "Study Duration")
x11(height = 3, width = 6)
g


# FIGURE 4a: Full year p*c accumulation curves ================================

# Set experiment folder
exp.folder <- paste0(path, 'Results/Full_Year/')

results <- readRDS(paste0(exp.folder, 'pc-and-compare-dates.RDS'))
compare.dates <- results$compare.dates
colnames(compare.dates) <- c('speciesID', 'effort', 'Treatment', 'pmax_date')
compare.dates[Treatment == 'fixed', Treatment := 'Fixed']
compare.dates[Treatment == 'optim', Treatment := 'Optimized']
all.pc <- results$all.pc
windowsFonts(Times = windowsFont('Times New Roman'))

# P*C ACCUMULATION CURVES
all.pc.means <- list()
n.samples <- c(2,5,10,20,30,40)
for (pl in 1:length(all.pc)) {
  pc <- all.pc[[pl]]
  
  # For a single sampling regime:
  pcmeans <- pc[, mean(pCurrent), by = c('date', 'speciesID', 'type')]
  colnames(pcmeans) <- c('date', 'speciesID', 'Treatment', 'meanPC')
  pcmeans[,speciesID := toupper(speciesID)]
  pcmeans[Treatment == 'opt', Treatment := 'Optimized']
  pcmeans[Treatment == 'fixed', Treatment := 'Fixed']
  pcmeans$day.of.year <- yday(pcmeans$date)
  pcmeans$effort <- n.samples[pl]
  sp <- unique(pcmeans[,speciesID])
  trt <- unique(pcmeans[,Treatment])
  
  # Calculate AUCs
  aucs <- data.table(speciesID = rep('tbd', length(sp)*length(trt)),
                     Treatment = rep('tbd', length(sp)*length(trt)),
                     AUC = rep(0, length(sp)*length(trt)),
                     stringsAsFactors = FALSE)
  counter <- 0
  
  for (tr in 1:length(trt)) {
    for (s in 1:length(sp)) {
      counter <- counter + 1
      aucs[counter,'speciesID'] <- sp[s]
      aucs[counter,'Treatment'] <- trt[tr]
      spec.sub <- pcmeans[speciesID == sp[s] & Treatment == trt[tr], ]
      setkey(spec.sub, date)
      y.probs <- spec.sub[ , meanPC]
      x.day <- spec.sub[ , day.of.year]
      x.day <- x.day/length(x.day) # convert xday to probabilities
      aucs[counter,'AUC'] <- trapezoidRule(x = x.day, y = y.probs)
    }
  }
  aucs$AUC <- round(aucs$AUC,2)
  
  pcmeans$AUC <- 'stuff'
  for (s in 1:length(sp)) {
    pcmeans[speciesID == sp[s] & Treatment == 'Fixed', 'AUC'] <-
      paste('Fixed:', sprintf('%.2f', aucs[aucs$speciesID == sp[s] & aucs$Treatment == 'Fixed', 'AUC']))
    pcmeans[speciesID == sp[s] & Treatment == 'Optimized', 'AUC'] <-
      paste('Opt:', sprintf('%.2f', aucs[aucs$speciesID == sp[s] & aucs$Treatment == 'Optimized', 'AUC']))
  }
  all.pc.means[[pl]] <- pcmeans
}
pcmeans <- rbindlist(l = all.pc.means)

# Merge the pc & pmax data.tables
keycols <- c('speciesID', 'Treatment', 'effort')
setkeyv(pcmeans, keycols)
setkeyv(compare.dates, keycols)
pcmeans <- merge(x = pcmeans, y = compare.dates, by = keycols, all = TRUE)
pcmeans[ , date := as.Date(date)] # make sure date/character is all dates now!
pcmeans[,pmax_val := as.numeric(NA)]
pcmeans[date == pmax_date, pmax_val := meanPC]

# Only keep labels once so that they don't get reprinted over one another
pcmeans[!(date == as.Date('2016-12-31')), 'AUC' := '']

# Assign geom_text colors for AUC
pcmeans$label_color <- 'gray66'
pcmeans[Treatment == 'Fixed', label_color := 'gray33']
pcmeans[!(date == as.Date('2016-12-31')), label_color := '']

# Customize label positions for low AUC curves (e.g. toad, leni, coyote)
pcmeans$label_y <- 0.3
pcmeans$label_hjust <- 1
pcmeans$label_x <- as.Date('2016-12-25')
pcmeans$label_vjust <- 1
pcmeans[Treatment == 'Fixed', label_vjust := 2.2]
pcmeans[speciesID == 'TOAD', 'label_y' := 1]
pcmeans[speciesID == 'TOAD', 'label_x' := as.Date('2015-12-30')]
pcmeans[speciesID == 'TOAD', 'label_hjust' := 0]
pcmeans[speciesID == 'LENI' & effort == 2, 'label_y' := 1]
pcmeans[speciesID == 'LENI' & effort == 2, 'label_x' := as.Date('2015-12-30')]
pcmeans[speciesID == 'LENI' & effort == 2, 'label_hjust' := 0]
pcmeans[speciesID == 'COYOTE' & effort == 2, 'label_y' := 1]
pcmeans[speciesID == 'COYOTE' & effort == 2, 'label_x' := as.Date('2015-12-30')]
pcmeans[speciesID == 'COYOTE' & effort == 2, 'label_hjust' := 0]


g <- ggplot(data = pcmeans, aes(x = date, y = meanPC)) +
  geom_line(aes(col = Treatment), size = 1) +
  facet_grid(speciesID ~ effort) +
  geom_text(data = pcmeans, aes(x = label_x, y = label_y, label = AUC,
                                vjust = label_vjust, hjust = label_hjust),
            size = 3, family = 'Times') +
  geom_point(aes(x = pmax_date, y = pmax_val, col = Treatment),
             shape = 19, size = 2) + # shape = 8, 11, 18, 17
  labs(x = 'Date',
       y = expression(
         paste('Average ', italic('p*'), ' Across Locations')
         ),
       title = expression(
         paste(italic('p*'), ' Accumulation Curves at Six Sampling Efforts: Full Year'))
       ) + 
  scale_color_manual(values = c('gray66', 'gray33')) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        text = element_text(family = 'Times'),
        plot.title = element_text(hjust = 0.5, size = 13),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        axis.text = element_text(color = 'black'),
        legend.position = 'top',
        axis.text.x = element_text(angle = 90, vjust = 0.5)) 
g

# FIGURE 4b: March p*c accumulation curves ================================

# Set experiment folder
exp.folder <- paste0(path, 'Results/March_Only/')

results <- readRDS(paste0(exp.folder, 'pc-and-compare-dates.RDS'))
compare.dates <- results$compare.dates
colnames(compare.dates) <- c('speciesID', 'effort', 'Treatment', 'pmax_date')
compare.dates[Treatment == 'fixed', Treatment := 'Fixed']
compare.dates[Treatment == 'optim', Treatment := 'Optimized']
all.pc <- results$all.pc
windowsFonts(Times = windowsFont('Times New Roman'))

# P*C ACCUMULATION CURVES
all.pc.means <- list()
n.samples <- c(2,5,10,20,30,40)
for (pl in 1:length(all.pc)) {
  pc <- all.pc[[pl]]
  
  # For a single sampling regime:
  pcmeans <- pc[, mean(pCurrent), by = c('date', 'speciesID', 'type')]
  colnames(pcmeans) <- c('date', 'speciesID', 'Treatment', 'meanPC')
  pcmeans[,speciesID := toupper(speciesID)]
  pcmeans[Treatment == 'opt', Treatment := 'Optimized']
  pcmeans[Treatment == 'fixed', Treatment := 'Fixed']
  pcmeans$day.of.year <- yday(pcmeans$date)
  pcmeans$effort <- n.samples[pl]
  sp <- unique(pcmeans[,speciesID])
  trt <- unique(pcmeans[,Treatment])
  
  # Calculate AUCs
  aucs <- data.table(speciesID = rep('tbd', length(sp)*length(trt)),
                     Treatment = rep('tbd', length(sp)*length(trt)),
                     AUC = rep(0, length(sp)*length(trt)),
                     stringsAsFactors = FALSE)
  counter <- 0
  
  for (tr in 1:length(trt)) {
    for (s in 1:length(sp)) {
      counter <- counter + 1
      aucs[counter,'speciesID'] <- sp[s]
      aucs[counter,'Treatment'] <- trt[tr]
      spec.sub <- pcmeans[speciesID == sp[s] & Treatment == trt[tr], ]
      setkey(spec.sub, date)
      y.probs <- spec.sub[ , meanPC]
      x.day <- spec.sub[ , day.of.year]
      x.day <- x.day/length(x.day) # convert xday to probabilities
      aucs[counter,'AUC'] <- trapezoidRule(x = x.day, y = y.probs)
    }
  }
  aucs$AUC <- round(aucs$AUC,2)
  
  pcmeans$AUC <- 'stuff'
  for (s in 1:length(sp)) {
    pcmeans[speciesID == sp[s] & Treatment == 'Fixed', 'AUC'] <-
      paste('Fixed:', sprintf('%.2f', aucs[aucs$speciesID == sp[s] & aucs$Treatment == 'Fixed', 'AUC']))
    pcmeans[speciesID == sp[s] & Treatment == 'Optimized', 'AUC'] <-
      paste('Opt:', sprintf('%.2f', aucs[aucs$speciesID == sp[s] & aucs$Treatment == 'Optimized', 'AUC']))
  }
  all.pc.means[[pl]] <- pcmeans
}
pcmeans <- rbindlist(l = all.pc.means)

# Merge the pc & pmax data.tables
keycols <- c('speciesID', 'Treatment', 'effort')
setkeyv(pcmeans, keycols)
setkeyv(compare.dates, keycols)
pcmeans <- merge(x = pcmeans, y = compare.dates, by = keycols, all = TRUE)
pcmeans[ , date := as.Date(date)] # make sure date/character is all dates now!
pcmeans[,pmax_val := as.numeric(NA)]
pcmeans[date == pmax_date, pmax_val := meanPC]

# Only keep labels once so that they don't get reprinted over one another
pcmeans[!(date == as.Date('2016-03-31')), 'AUC' := '']

# Assign geom_text colors for AUC
pcmeans$label_color <- 'gray66'
pcmeans[Treatment == 'Fixed', label_color := 'gray33']
pcmeans[!(date == as.Date('2016-03-31')), label_color := '']

# Customize label positions for low AUC curves (e.g. leni, coyote)
pcmeans$label_y <- 0.3
pcmeans$label_hjust <- 1
pcmeans$label_x <- as.Date('2016-03-30')
pcmeans$label_vjust <- 1
pcmeans[Treatment == 'Fixed', label_vjust := 2.2]
pcmeans[effort == 2 | effort == 5, 'label_y' :=  1]
pcmeans[effort == 2 | effort == 5, 'label_x' := as.Date('2016-03-01')]
pcmeans[effort == 2 | effort == 5, 'label_hjust' := 0]
pcmeans[speciesID == 'COYOTE' | speciesID == 'LENI', 'label_y' :=  1]
pcmeans[speciesID == 'COYOTE' | speciesID == 'LENI', 'label_x' := as.Date('2016-03-01')]
pcmeans[speciesID == 'COYOTE' | speciesID == 'LENI', 'label_hjust' := 0]

# Create the plot
g <- ggplot(data = pcmeans, aes(x = date, y = meanPC)) +
  geom_line(aes(col = Treatment), size = 1) +
  facet_grid(speciesID ~ effort) +
  geom_text(data = pcmeans, aes(x = label_x, y = label_y, label = AUC,
                                vjust = label_vjust, hjust = label_hjust),
            size = 3, family = 'Times') +
  geom_point(aes(x = pmax_date, y = pmax_val, col = Treatment),
             shape = 19, size = 2) + # shape = 8, 11, 18, 17
  labs(x = 'Date',
       y = expression(
         paste('Average ', italic('p*'), ' Across Locations')
       ),
       title = expression(
         paste(italic('p*'), ' Accumulation Curves at Six Sampling Efforts: March Only'))
  ) + 
  scale_color_manual(values = c('gray66', 'gray33')) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        text = element_text(family = 'Times'),
        plot.title = element_text(hjust = 0.5, size = 13),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        axis.text = element_text(color = 'black'),
        legend.position = 'top',
        axis.text.x = element_text(angle = 90, vjust = 0.5))
g


# APPENDIX 2 FIGURE: Max. Per Hour ============================================

# Set experiment folder
exp.folder <- paste0(path, 'Results/March_Only/')

results <- readRDS(paste0(exp.folder, 'pc-and-compare-dates-simple-vs-maxhr.RDS'))
compare.dates <- results$compare.dates
colnames(compare.dates) <- c('speciesID', 'effort', 'Treatment', 'pmax_date')
compare.dates[Treatment == 'fixed', Treatment := 'Fixed']
compare.dates[Treatment == 'optim', Treatment := 'Optimized']
compare.dates[Treatment == 'maxhr', Treatment := 'Max. Per Hour']
all.pc <- results$all.pc
windowsFonts(Times = windowsFont('Times New Roman'))

# P*C ACCUMULATION CURVES
all.pc.means <- list()
n.samples <- c(20,30,40)
for (pl in 1:length(all.pc)) {
  pc <- all.pc[[pl]]
  
  # For a single sampling regime:
  pcmeans <- pc[, mean(pCurrent), by = c('date', 'speciesID', 'type')]
  colnames(pcmeans) <- c('date', 'speciesID', 'Treatment', 'meanPC')
  pcmeans[,speciesID := toupper(speciesID)]
  pcmeans[Treatment == 'opt', Treatment := 'Optimized']
  pcmeans[Treatment == 'fixed', Treatment := 'Fixed']
  pcmeans[Treatment == 'maxhr', Treatment := 'Max. Per Hour']
  pcmeans$day.of.year <- yday(pcmeans$date)
  pcmeans$effort <- n.samples[pl]
  sp <- unique(pcmeans[,speciesID])
  trt <- unique(pcmeans[,Treatment])
  
  # Calculate AUCs
  aucs <- data.table(speciesID = rep('tbd', length(sp)*length(trt)),
                     Treatment = rep('tbd', length(sp)*length(trt)),
                     AUC = rep(0, length(sp)*length(trt)),
                     stringsAsFactors = FALSE)
  counter <- 0
  
  for (tr in 1:length(trt)) {
    for (s in 1:length(sp)) {
      counter <- counter + 1
      aucs[counter,'speciesID'] <- sp[s]
      aucs[counter,'Treatment'] <- trt[tr]
      spec.sub <- pcmeans[speciesID == sp[s] & Treatment == trt[tr], ]
      setkey(spec.sub, date)
      y.probs <- spec.sub[ , meanPC]
      x.day <- spec.sub[ , day.of.year]
      x.day <- x.day/length(x.day) # convert xday to probabilities
      aucs[counter,'AUC'] <- trapezoidRule(x = x.day, y = y.probs)
    }
  }
  aucs$AUC <- round(aucs$AUC,2)
  
  pcmeans$AUC <- 'stuff'
  for (s in 1:length(sp)) {
    pcmeans[speciesID == sp[s] & Treatment == 'Fixed', 'AUC'] <-
      paste('Fixed:', sprintf('%.2f', aucs[aucs$speciesID == sp[s] & aucs$Treatment == 'Fixed', 'AUC']))
    pcmeans[speciesID == sp[s] & Treatment == 'Optimized', 'AUC'] <-
      paste('Opt:', sprintf('%.2f', aucs[aucs$speciesID == sp[s] & aucs$Treatment == 'Optimized', 'AUC']))
    pcmeans[speciesID == sp[s] & Treatment == 'Max. Per Hour', 'AUC'] <-
      paste('MaxHr:', sprintf('%.2f', aucs[aucs$speciesID == sp[s] & aucs$Treatment == 'Max. Per Hour', 'AUC']))
  }
  all.pc.means[[pl]] <- pcmeans
}
pcmeans <- rbindlist(l = all.pc.means)

# Merge the pc & pmax data.tables
keycols <- c('speciesID', 'Treatment', 'effort')
setkeyv(pcmeans, keycols)
setkeyv(compare.dates, keycols)
pcmeans <- merge(x = pcmeans, y = compare.dates, by = keycols, all = TRUE)
pcmeans[ , date := as.Date(date)] # make sure date/character is all dates now!
pcmeans[,pmax_val := as.numeric(NA)]
pcmeans[date == pmax_date, pmax_val := meanPC]

# Only keep labels once so that they don't get reprinted over one another
pcmeans[!(date == as.Date('2016-03-31')), 'AUC' := '']

# Assign geom_text colors for AUC
opt.col <- 'gray70'
max.col <- 'gray45'
fixed.col <- 'gray20'
pcmeans$label_color <- opt.col
pcmeans[Treatment == 'Fixed', label_color := fixed.col]
pcmeans[Treatment == 'Max. Per Hour', label_color := max.col]
pcmeans[!(date == as.Date('2016-03-31')), label_color := '']

# Customize label positions for low AUC curves 
pcmeans$label_y <- 0.4 
pcmeans$label_hjust <- 1
pcmeans$label_x <- as.Date('2016-03-30')
pcmeans$label_vjust <- 0
pcmeans[Treatment == 'Max. Per Hour', label_vjust := 1.2]
pcmeans[Treatment == 'Fixed', label_vjust := 2.4]
pcmeans[speciesID == 'COYOTE' | speciesID == 'LENI', 'label_y' :=  0.86] #1
pcmeans[speciesID == 'COYOTE' | speciesID == 'LENI', 'label_x' := as.Date('2016-03-01')]
pcmeans[speciesID == 'COYOTE' | speciesID == 'LENI', 'label_hjust' := 0]

# Create the plot
g <- ggplot(data = pcmeans, aes(x = date, y = meanPC)) +
  geom_line(aes(col = Treatment), size = 1) +
  facet_grid(speciesID ~ effort) +
  geom_text(data = pcmeans, aes(x = label_x, y = label_y, label = AUC,
                                vjust = label_vjust, hjust = label_hjust),
            size = 3, family = 'Times') +
  geom_point(aes(x = pmax_date, y = pmax_val, col = Treatment),
             shape = 19, size = 2) + # shape = 8, 11, 18, 17
  labs(x = 'Date',
       y = expression(
         paste('Average ', italic('p*'), ' Across Locations')
       ),
       title = expression(
         paste(italic('p*'), ' Accumulation Curves at Three Sampling Efforts: March Only'))
  ) + 
  scale_color_manual(values = c(opt.col, max.col, fixed.col)) +
  scale_x_date(date_breaks = '1 week', date_labels =  '%b %d') +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = 'black'),
        text = element_text(family = 'Times'),
        plot.title = element_text(hjust = 0.5, size = 13),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        axis.text = element_text(color = 'black'),
        legend.position = 'top',
        axis.text.x = element_text(angle = 90, vjust = 0.5))
g

# TABLE 5: Summary statistics for weather and temporal covariates =============

# Connect to full year database
db.path <- paste0(path, 'experiment-full-year.sqlite')
conx <- dbConnect(drv = dbDriver('SQLite'), dbname = db.path)

# Read in temporals table
temporals <- data.table(dbReadTable(conx, 'temporals'))
temporals[,c('sunriseTime', 'sunsetTime') := list(as.POSIXct(sunriseTime, tz = tz),
                                                  as.POSIXct(sunsetTime, tz = tz))]

# Set timezone
tz <- 'America/Los_angeles'

# Add a month column
temporals[,month := month(date)]

# Aggregate columns
agg <- temporals[,c('locationID', 'month', 'hour', 
                    'rainAccum24','temperature','windSpeed', 
                    'sunriseTime', 'sunsetTime')]
temp <- with(agg, aggregate(temperature ~ month, FUN =  function(z)
  c(avg = mean(z), min = min(z), max = max(z), sd = sd(z))))
rain <- with(agg, aggregate(rainAccum24 ~ month, FUN =  function(z)
  c(avg = mean(z), min = min(z), max = max(z), sd = sd(z))))
wind <- with(agg, aggregate(windSpeed ~ month, FUN =  function(z)
  c(avg = mean(z), min = min(z), max = max(z), sd = sd(z))))
# then took these tables into excel for table formatting


# Taking a naive average of sunrises and sunsets doesnt work as expected
#  To get the average, max, min, need to convert all of them to be on the same day
#  and then can compare them, e.g.,
# https://stackoverflow.com/questions/19021663/r-calculate-mean-of-time-variable-meandatetime
rises <- agg$sunriseTime
rises <- update(rises, year = 2000, month = 1, mday = 1, tz = tz)
summary(rises)

sets <- agg$sunsetTime
sets <- update(sets, year = 2000, month = 1, mday = 1, tz = tz)
summary(sets)

agg$rises <- rises
agg$sets <- sets

with(agg, aggregate(rises ~ month, FUN = function(z)
  c(avg = format(mean(z), format = "%H:%M:%S", tz = tz), 
    min = format(min(z), format = "%H:%M:%S", tz = tz),
    max = format(max(z), format = "%H:%M:%S", tz = tz), 
    sd = format(sd(z), format = "%H:%M:%S", tz = tz))))
with(agg, aggregate(sets ~ month, FUN = function(z)
  c(avg = format(mean(z, na.rm = TRUE), format = "%H:%M:%S", tz = tz),
    min = format(min(z, na.rm = TRUE), format = "%H:%M:%S", tz = tz),
    max = format(max(z, na.rm = TRUE), format = "%H:%M:%S", tz = tz),
    sd = format(sd(z, na.rm = TRUE), format = "%H:%M:%S", tz = tz))))

# take into excel for prettier formatting; convert sd from seconds to minutes in excel