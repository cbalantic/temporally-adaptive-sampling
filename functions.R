###############################################################################
#                                                                             #
# All functions necessary to reproduce the experiments from:                  #
#                                                                             #
# Balantic, C. M., & Donovan, T. M. (2019). Temporally adaptive acoustic      #
# sampling to maximize detection across a suite of focal wildlife species.    #
# Accepted at Ecology and Evolution.                                          #
#                                                                             #
###############################################################################

# Note: AMMonitor is now available at https://code.usgs.gov/vtcfwru/ammonitor

# The functions contained herein reflect pre-release AMMonitor functions as of
# 2019-05-10, bundled for Ecology and Evolution submission. Only the AMMonitor 
# functions necessary to run the manuscript experiments are contained here; 
# code and at https://code.usgs.gov/vtcfwru/ammonitor may differ slightly.  

## List of functions contained in this code: ##

## Functions from AMMonitor (as of 2019-05-10):
# - qryDeployment()
# - qryPrioritization()
# - scheduleAddVars()
# - scheduleOptim()
# - scheduleOptimCreate()
# - schedulePushHelper()
# - simGlm()

## Functions specific to reproducing the results in this paper:
# - ggVocModel(): A function to help with plotting
# - predictFixed(): a function to run the "fixed" experiments
# - trapezoidRule(): a function to calculate the trapezoid rule for AUC 

# qryDeployment() ============
#' @name qryDeployment
#' @title Query the database to return deployed devices at actively monitored locations
#' @description \code{qryDeployment} invokes SQL syntax to efficiently query an RSQLite-based AMMonitor database and return deployed equipment at actively monitored locations, as well as their associated Google accounts. Written primarily as an internal function to efficiently return equipment and account information required for automated routines, but may also be convenient for end users. See AMMonitor Documentation \href{placeholderurl}{Chapter 8: Equipment and Deployment} for details and context.
#' @param conn Class \code{SQLiteConnection} object, created by a call to \code{\link[DBI]{dbConnect}}. 
#' @param locationID Character vector of locationIDs for which to query the database for deployment information.
#' @return \code{qryDeployment} returns a data.frame of information about equipment deployed at active monitoring locations, composed of the following nine columns: equipmentID (character), locationID (character), accountID (character), dateDeployed (character), dateRetrieved (character), email (character), lat (numeric), long (numeric), tz (character).
#' @family qry
#' @export
#' @examples
#'\dontrun{
#'
#' # ------------------------------------------------------------
#' # Set up a demo AMMonitor database
#' # ------------------------------------------------------------
#' 
#' #' # Create the database (this will be deleted):
#' db.name <- 'demo.sqlite'
#'                 
#' dbCreateSample(db.name = db.name, 
#'                file.path = paste0(getwd(),'/'), 
#'                tables = c('locations', 'equipment', 'deployment',
#'                           'accounts', 'people'))
#'                 
#' # Verify that the database exists in your current working directory:
#' file.exists(db.name)
#' 
#' # Connect to the db: 
#' db.path <- paste0(getwd(), '/', db.name)
#' conx <- dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
#'
#'
#' # ------------------------------------------------------------
#' # Query database for deployment information about all actively
#' # monitored locations
#' # ------------------------------------------------------------
#'
#' all.deployment.info <- qryDeployment(conn = conx,
#'                                      locationID = 'all')
#' all.deployment.info
#'
#' # ------------------------------------------------------------
#' # Query database for deployment information about selected 
#' # locations
#' # ------------------------------------------------------------
#'
#' some.deployment.info <- qryDeployment(conn = conx,
#'                                       locationID = c('location@2', 
#'                                                     'location@10'))
#' some.deployment.info
#'
#'
#' # ------------------------------------------------------------
#' # Disconnect from demo database and delete from working directory
#' # ------------------------------------------------------------
#'
#' dbDisconnect(conx)
#' unlink(db.path)
#'
#'}

qryDeployment <- function(conn, locationID) {
  
  # Turn the SQLite foreign constraints on
  rs <- dbSendQuery(conn = conn, statement = "PRAGMA foreign_keys = ON;")
  dbClearResult(rs) 
  
  # Query to retrieve information only for actively deployed equipment 
  base.query <- "SELECT deployment.*, accounts.accountID, accounts.email, locations.lat, locations.long, locations.tz, deployment.dateRetrieved
FROM (deployment INNER JOIN (accounts INNER JOIN equipment ON accounts.accountID = equipment.accountID) ON deployment.equipmentID = equipment.equipmentID) INNER JOIN locations ON deployment.locationID = locations.locationID"
  
  # Columns to return: 
  return.cols <- c('equipmentID', 'locationID', 'accountID', 'dateDeployed', 
                   'dateRetrieved', 'email', 'lat', 'long', 'tz')
  
  # Create and run queries depending on locationID argument
  if (length(locationID) == 1 && locationID == "all") {
    query.all <- paste0(base.query, " WHERE (((deployment.dateRetrieved) Is Null));")
    results <- dbGetQuery(conn, query.all)
  } else {
    query.loc <- paste0(base.query, " WHERE (((deployment.dateRetrieved) Is Null) 
                                      AND (deployment.locationID)= ?);")
    results <- dbGetQuery(conn, query.loc, param = list(locationID))
  }
  
  # Return desired columns as a df
  return(results[,return.cols])
  
}


# qryPrioritization() ==============
#' @name qryPrioritization
#' @title Query the database for current prioritization status
#' @description \code{qryPrioritization} invokes SQL syntax to efficiently query an RSQLite-based AMMonitor database and return the latest records in the prioritization table. This is a simple convenience function that returns all data from the most recent date available in the prioritization table. \code{qryPrioritization} is used internally within other AMMonitor functions, but may also be convenient for users who have already initialized a database connection object (e.g., \strong{conx} in the below examples), and want to avoid constructing a SQLite query to view prioritization data. (See AMMonitor Documentation \href{placeholderurl}{Chapter 10: Prioritization} for further context.)
#' @param conn Class \code{SQLiteConnection} object, created by a call to \code{\link[DBI]{dbConnect}}. 
#' @return  \code{qryPrioritization} returns a data.frame of the most recent records in the prioritization table, composed of the following seven columns: the locationID (character), speciesID (character), date (character), pMax (numeric), pCurrent (numeric), weight (numeric) and init (numeric).
#' @family qry
#' @seealso  \code{\link{prioritySet}}, \code{\link{priorityInit}}, \code{\link{scheduleOptim}}, \code{\link{simGlm}}
#' @export
#' @examples
#'\dontrun{
#' 
#' # ------------------------------------------------------------
#' # Set up a demo AMMonitor database
#' # ------------------------------------------------------------
#' 
#' # Create the database (this will be deleted):
#' db.name <- 'demo.sqlite'
#'                 
#' dbCreateSample(db.name = db.name, 
#'                file.path = paste0(getwd(),'/'), 
#'                tables = c('locations', 'equipment', 'deployment',
#'                           'accounts', 'species', 'prioritization', 
#'                           'people'))
#'                 
#' # Verify that the database exists in your current working directory:
#' file.exists(db.name)
#' 
#' # Connect to the db: 
#' db.path <- paste0(getwd(), '/', db.name)
#' conx <- dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
#'
#' # ------------------------------------------------------------
#' # Query database for most recent prioritization table
#' # ------------------------------------------------------------
#' 
#' qryPrioritization(conn = conx) 
#'                                                             
#' # ------------------------------------------------------------
#' # Disconnect from demo database and delete from working directory
#' # ------------------------------------------------------------
#' 
#' dbDisconnect(conn = conx)
#' unlink(db.path)
#' 
#'}

qryPrioritization <- function(conn) {
  
  # Turn the SQLite foreign constraints on
  rs <- dbSendQuery(conn = conn, statement = "PRAGMA foreign_keys = ON;" )
  dbClearResult(rs) 
  
  priority <- dbGetQuery(conn, 'SELECT locationID, speciesID, weight, pCurrent, pMax, max(date), init
                                FROM prioritization
                                GROUP BY speciesID, locationID')
  
  colnames(priority)[colnames(priority) == 'max(date)'] <- 'date'
  
  return(priority)
}


# scheduleAddVars() =======
#' @name scheduleAddVars
#' @title Add covariates to accommodate circular (angular) variables within temporal data
#' @description \code{scheduleAddVars} adds covariates to temporal data to accommodate circular variables, as well as a few other potentially useful temporal covariates. These include the sine and cosine of the day of the year, the sine and cosine around equinoces, the numeric distance of a given sampling hour from sunrise and sunset, the sine and cosine of the distance from a sunrise or sunset event, sine and cosine of the hour of the day, and sine and cosine of the moonphase. Many of these covariates may provide finer control when attempting to model target species activity patterns that vary according to time of day and time of year.  
#' 
#' \code{scheduleAddVars} is a function invoked internally within \code{\link{simGlm}}. It is generally intended as an internal function and will be used automatically when inputting 'db.path' to \code{\link{simGlm}}. However, if users are not using the 'db.path' argument in \code{\link{simGlm}}, they may instead use the 'data' argument to input a freestanding data.frame, in which case they may wish to run \code{scheduleAddVars} if the freestanding data.frame happens to contain all of the temporal covariate data. 
#' @details The 18 additional temporal covariates added to the temporal data are defined as follows:
#'  \enumerate{
#'  \item{\strong{dayOfYear}: The integer day of the year, ranging from 1 to 365 (or 366 if leap year).}
#'  \item{\strong{dayCos}: Cosine of the integer day of the year: \eqn{cos(2*pi*dayOfYear/max(dayOfYear))}.}
#'  \item{\strong{daySin}: Sine of the integer day of the year: \eqn{sin(2*pi*dayOfYear/max(dayOfYear))}.}
#'  \item{\strong{dayCosEquinox}: Cosine of the integer day of the year divided by two: \eqn{cos(2*pi*dayOfYear/max(dayOfYear/2))}.}
#'  \item{\strong{daySinEquinox}: Sine of the integer day of the year divided by two: \eqn{sin(2*pi*dayOfYear/max(dayOfYear/2))}.}
#'  \item{\strong{distRise}: Distance from sunrise. The absolute value of the number of minutes to the nearest sunrise.}
#'  \item{\strong{distSet}: Distance from sunset. The absolute value of the number of minutes to the nearest sunset.}
#'  \item{\strong{distRiseCos}: Cosine of the distance from sunrise: \eqn{cos(2*pi*distRise/1440)}.}
#'  \item{\strong{distSetCos}: Cosine of the distance from sunset: \eqn{cos(2*pi*distSet/1440)}.}
#'  \item{\strong{distRiseSin}: Sine of the distance from sunrise: \eqn{sin(2*pi*distRise/1440)}.}
#'  \item{\strong{distSetSin}: Sine of the distance from sunset: \eqn{sin(2*pi*distSet/1440)}.}
#'  \item{\strong{hour}: The integer of the hour of the day, ranging from 1 to 24.}
#'  \item{\strong{hourCos24}: Cosine of the integer hour of the day on a 24-hour scale: \eqn{cos(2*pi*hour/24)}.}
#'  \item{\strong{hourSin24}: Sine of the integer hour of the day on a 24-hour scale: \eqn{sin(2*pi*hour/24)}.}
#'  \item{\strong{hourCos12}: Cosine of the integer hour of the day on a 12-hour scale: \eqn{cos(2*pi*hour/12)}.}
#'  \item{\strong{hourSin12}: Sine of the integer hour of the day on a 12-hour scale: \eqn{sin(2*pi*hour/12)}.}
#'  \item{\strong{moonCos}: Cosine of the lunar phase: \eqn{cos(2*pi*moonPhase)}.}
#'  \item{\strong{moonSin}: Sine of the lunar phase: \eqn{sin(2*pi*moonPhase)}.}
#'  }
#' @param temporal.data A data.frame or data.table of temporal covariates, with column names as described in \href{placeholderurl}{Chapter 8: Temporals}.
#' @return \code{scheduleAddVars} returns a data.table of temporal data, now with the 18 additional columns defined in \strong{Details}. 
#' @family schedule
#' @seealso \code{\link{simGlm}}
#' @export
#' @examples 
#'
#' # ------------------------------------------------------------
#' # Set up a demo AMMonitor database
#' # ------------------------------------------------------------
#'
#' # Create the database (this will be deleted):
#' db.name <- 'demo.sqlite'
#'                 
#' dbCreateSample(db.name = db.name, 
#'                file.path = paste0(getwd(),'/'), 
#'                tables = c('locations', 'equipment', 'deployment',
#'                           'accounts', 'temporals', 'people'))
#'                 
#' # Verify that the database exists in your current working directory:
#' file.exists(db.name)
#' 
#' # Connect to the db: 
#' db.path <- paste0(getwd(), '/', db.name)
#' conx <- dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
#'
#' # Set deployments so that only 3 locations are actively monitored
#' # (to concisify the demonstration of this function)
#' dbExecute(conn = conx, statement =  "UPDATE deployment 
#'                                      SET dateRetrieved = '2016-01-20'
#'                                      WHERE locationID NOT IN 
#'                                      ('location@1', 'location@2', 'location@3')")
#'
#' # ------------------------------------------------------------
#' # Add circular variables to the temporal data
#' # ------------------------------------------------------------
#' 
#' # Gather some temporal data from active monitoring locations
#' temporal.data <- qryTemporals(conn = conx, date = '2016-03-10')
#' 
#' # Add circular variables to the temporal data
#' temporal.data.plus <- scheduleAddVars(temporal.data = temporal.data)
#' 
#' head(temporal.data.plus)
#' 
#'  # ------------------------------------------------------------
#' # Disconnect from demo database and delete from working directory
#' # ------------------------------------------------------------
#' 
#' dbDisconnect(conn = conx)
#' unlink(db.path)
#' 
scheduleAddVars <- function(temporal.data){
  
  # Turn into data.table, set PKs:
  temporal.data <- setDT(temporal.data,
                         key = c('locationID', 'type', 'date', 'time'))
  temporal.data <- data.table(temporal.data)
  
  
  # Add variables
  temporal.data[, dayOfYear := lubridate::yday(time)]
  
  # Figure out whether there are leaps years in the data,
  # assign the correct number of days in the year to accurately 
  # assign the sin and cos of day of year
  temporal.data[,leap := lubridate::leap_year(year(date))]
  temporal.data[,nDaysYear := ifelse(leap == TRUE, 366, 365)]
  
  # Add variables:
  temporal.data[, dayCos := cos(2*pi*dayOfYear/nDaysYear)]
  temporal.data[, daySin := sin(2*pi*dayOfYear/nDaysYear)]
  temporal.data[, dayCosEquinox := cos(2*pi*dayOfYear/(nDaysYear/2))]
  temporal.data[, daySinEquinox := sin(2*pi*dayOfYear/(nDaysYear/2))]
  temporal.data[, distRise := as.numeric(abs(
    difftime(time1 = as.POSIXct(time,tz = ""), 
             time2 = as.POSIXct(sunriseTime, tz = ""),
             units = 'mins')))]
  temporal.data[, distSet := as.numeric(abs(
    difftime(time1 = as.POSIXct(time,tz = ""), 
             time2 = as.POSIXct(sunsetTime, tz = ""),
             units = 'mins')))]
  temporal.data[, distRiseCos := cos(2*pi*distRise/1440)]
  temporal.data[, distSetCos := cos(2*pi*distSet/1440)]
  temporal.data[, distRiseSin := sin(2*pi*distRise/1440)]
  temporal.data[, distSetSin := sin(2*pi*distSet/1440)]
  temporal.data[, hour := lubridate::hour(time)]
  temporal.data[, hourSin24 := sin(2*pi*hour/24)]
  temporal.data[, hourCos24 := cos(2*pi*hour/24)]
  temporal.data[, hourSin12 := sin(2*pi*hour/12)]
  temporal.data[, hourCos12 := cos(2*pi*hour/12)]
  temporal.data[, moonCos := cos(2*pi*moonPhase)]
  temporal.data[, moonSin := sin(2*pi*moonPhase)]
  
  # Remove columns that we only used for calculation purposes
  temporal.data[,c('leap', 'nDaysYear') := NULL]
  
  # DO NOT re-order columns to a more familiar order here. 
  # Doing so may eliminate any additional temporal columns users 
  # may have added to the database
  
}


# scheduleOptim() ===== 
#' @name scheduleOptim
#' @title Create optimized sampling schedule based on monitoring priorities
#' @description The temporally-adaptive sampling algorithm in \code{scheduleOptim} is designed to maximize detection probabilities for a suite of focal species amid sampling constraints. The algorithm combines user-supplied species activity models (stored in the "ammls" directory) with site-specific weather forecasts (stored in the temporals table) to set an optimized sampling schedule for the following day. Simulations show that over the course of a study season, the probability of acoustically capturing a focal species at least once via automated monitoring is higher (and capture occurs earlier in the season) when using the temporally-adaptive optimized schedule as compared to a fixed schedule, reducing the risk of false negatives \href{placeholderurl}{(Balantic and Donovan, in review)}. See \strong{Details} and AMMonitor Documentation \href{placeholderurl}{Chapter 10: Prioritization} for more information and context. NOTE: THIS FUNCTION CURRENTLY FOCUSES ON AUDIO RECORDINGS ONLY -- not photos, motion capture, or playback.
#' 
#' @details 
#' Prior to running \code{scheduleOptim}, users should ensure that the priorities table has been populated. Once the priorities table is in place, users should run \code{\link{priorityInit}}, which takes existing information from the priorities table to initialize an optimized monitoring period in the prioritization table. If the priorities table is not populated, users must do so manually or use \code{\link{prioritySet}}.
#' 
#' \code{scheduleOptim} assumes that all models in the optimization follow either a binomial (probability) or a poisson (count) process. Binomial and poisson models should not be mixed into the same optimization scheme; please ensure that each species has the same model 'type' or undesired outcomes may result. For binomial activity probability models, pCurrent updates are constrained between 0 and 1, and pMax values should not exceed 1. For poisson activity count models, pCurrent and pMax are treated as counts and are not constrained between 0 and 1. If all species target pMax values have been achieved, and all species optimization weights have dropped to zero prior to the end of the sampling period, the algorithm will internally reset the weights each day so that each species has equal weight, ensuring that sampling will continue at meaningful times of day even if targets have been achieved.
#' 
#' To use \code{scheduleOptim} for pushing an optimized sampling schedule to a Google Calendar within the context of an AMMonitor database, users must first set up a service account and share their Google Calendar(s)' primary calendar with that service account.
#' 
#' As detailed in AMMonitor Documentation \href{placeholderurl}{Chapter 7: Equipment}, users should have one primary Google account which is not connected to a piece of monitoring equipment, through which users will manage the Google Calendar API described below. The primary Google account will also be used to manage the Dropbox API covered in \href{placeholderurl}{Chapter 11: Recordings}. Meanwhile, each piece of smartphone-based monitoring equipment should have its own Google account independent of the primary account.
#' 
#' To send sampling schedules directly to a Google Calendar using AMMonitor, users must first complete two steps: 1) set up a Google API service account associated with the primary management account, and 2) share the main Google Calendar of each smartphone device with this service account.
#' 
#' Begin by navigating to the Google API development dashboard at: \url{https://console.developers.google.com/apis/api/calendar/overview}. This link will take you to any Google account you are currently logged into, so make sure you are logged into the primary Google account connected to a smartphone used in your monitoring program.
#' 
#' See AMMonitor Documentation \href{placeholderurl}{Chapter 9: Schedule} for additional details, context, and pictures to help you complete the below process (current as of 4 December 2018):
#' 
#' \strong{Step 1: Setting up a Google API Service Account}
#' \enumerate{
#'  \item{Once you have logged into your principal smartphone monitoring account and opened \url{https://console.developers.google.com/apis/api/calendar/overview}, you should accept any welcome pop-ups and Terms of Service notifications. Then, you will see a Google API Developer's screen. Click "Enable".}
#'  \item{On the left-hand tab, select "Credentials", and then "Create".}
#'  \item{You will arrive at a new project landing page. Choose a project name, then click "Create".}
#'  \item{Click the blue "Create credentials" button, which will bring up a drop-down menu of options. Select the "Service Account Key" option.}
#'  \item{Finally, you will arrive at the page that allows you to create a service account key. Under "Service account", select the "New service account" option. Create a name of your choosing in the "Service account name" field (this will also auto-populate the "Service account ID" field). In the "Role" dropdown menu, choose Project > Owner. Lastly, for "Key type", make sure that the JSON option is selected. When finished, click "Create".}
#'  \item{A new dialogue box will prompt an automatic download of the JSON key. You will experience a download of the JSON file and a pop-up prompting you to store the file in a folder of your choosing. You should store the JSON file in the \strong{settings} folder of your monitoring project directory (which was generated by \code{\link{ammCreateDirectories}}).}
#'  \item{Finally, you will recieve a message notifying you that the private key has been saved to your computer.}
#'  }
#' 
#' \strong{Step 2: Sharing the Service Token with Monitoring Equipment}:
#' The service account key generated in \strong{Step 1} above should be associated with your primary Google management account. We now need to share each monitoring smartphone's main Google Calendar with this service account by logging into the Google account for each piece of monitoring equipment. For example, if we have a piece of equipment named "equip@@3", whose Google account is "midEarth3" in the accounts table, then we need to log into that account and navigate to the following web address: \url{https://calendar.google.com/calendar/render#main_7}
#' \enumerate{
#'  \item{Once you have logged into the Google account for an individual piece of monitoring equipment (e.g., "midEarth3") and navigated to \url{https://calendar.google.com/calendar/render#main_7}, accept any Google welcome pop-ups and notifications. Then, in the left-hand pane, select the main calendar account for this device. Select the 3 vertical dots that say "Options for [Name] Account", and then select "Settings and sharing".}
#'  \item{In the left-hand panel, select "Share with specific people". Then, click "Add people", and paste in the name of the Service Account ID you generated in Step 1.5. Next, under "Permissions", select "Make changes to events". Finally, click "SEND" to save these settings.}
#'  \item{The settings page should contain the device's Google account name (for example, "midEarth3@@gmail.com (Owner)", followed by the Service Account ID address generated in step 1.5. This action allows us to interact with the monitoring device's calendar directly through R functions that use the Google Calendar API.}
#' }
#' You will have to repeat steps 2.1, 2.2, and 2.3 for each Google account associated with a piece of monitoring equipment to which you would like to remotely send schedules.
#' 
#' \strong{IMPORTANT NOTE}: While you are logged in to each monitoring account, you should make sure your monitoring equipment's Google Calendar is set to the timezone in which it is monitoring. For example, if you (and your computer) are located in Vermont, USA, but your study site is located in California, USA, you have a three hour time difference between the monitoring device and your computer (which is communicating the schedule to the device). In this example, if your Google Calendar's schedule is not set to the timezone in which it is monitoring, your schedules will unintentionally contain a three hour time difference.
#'
#' @param db.path  The file path that houses the SQLite database.
#' @param calendar.key Character string file path to a Service Account JSON key. If provided, \code{scheduleOptim} will push schedules to the Google Calendars associated with actively monitoring locationIDs. See \strong{Details} and/or \href{placeholderurl}{Chapter 9: Schedule}.
#' @param temporals.key File path to a Dark Sky API token stored as an RDS file. See \strong{Details} of \code{\link{temporalsGet}}. Note that if historical data for "tomorrow" are already contained in the temporals database, a temporals.key is not required because no forecast is needed (typical if running \code{scheduleOptim} as a simulation). 
#' @param amml Class \code{\link{AMModels}} \code{amModelLib} object containing activity models for all priority monitoring species. The object should contain class \code{glm} models for each species.
#' @param choose.models  Character vector of model names to use in the optimization scheme. Model names must match names of models present in the \code{amModelLib} object input to 'amml'. Each chosen model must contain a speciesID metadata element containing valid speciesIDs found in the prioritization table of the database.
#' @param optimization.approach Default = 'simple'. Character string indicating the priority optimization approach to use, with options 'simple' or 'max.per.hour'. All optimization approaches currently assume that the same number of samples are taken each day at each site. Under the 'simple' approach, 'daily.site.constraint' is the number of one-minute recording samples available to be taken each day at each site. If 'daily.site.constraint' <= 30, all samples are allotted into the highest scoring hour at evenly-spaced intervals throughout the hour. If 'daily.site.constraint' > 30, remaining samples are allotted in the same way in the second highest scoring hour, then the third highest scoring hour, and so on. If using the 'max.per.hour' approach, users must additionally provide a value for the 'max.per.hour' argument to indicate the maximum number of samples to be distributed within any given hour. This option exists for end users with lower daily site constraints who do not want all their sampling power allotted into a single hour. We generally recommend the 'simple' option. When activity models are well-informed and weather predictions are reliable, simulations indicate the 'max.per.hour' option is slightly inferior to the 'simple' option. See Appendix, \href{placeholderurl}{Temporally-adaptive acoustic sampling to maximize detection across a suite of focal wildlife species} (Balantic and Donovan in review).
#' @param time.units Character string indicating the time units upon which activity probabilities or rates were modeled. Use 'by.minute' if probabilities or rates should be applied to each minute. Use 'by.hour' if probabilities or rates should be applied to each hour.
#' @param daily.site.constraint Integer indicating the total number of one-minute samples available each day at each site.
#' @param max.per.hour Integer indicating the maximum number of samples to allocate into a given hour. Argument only necessary if using optimization.approach = 'max.per.hour'.
#' @param db.insert Default = FALSE. Logical flag for whether to insert records generated by this function into the database. If TRUE, events are added to the database. If FALSE, data is returned for examination by the user, but not added to the database.
#' @param google.push Default = FALSE. Logical flag for whether to push schedule events to Google Calendar. When FALSE, no events are pushed. When TRUE, schedule events are pushed to Google Calendar. google.push may not be TRUE if db.insert = FALSE. Finally, setting db.insert = TRUE and google.push = FALSE allows users to run \code{scheduleOptim} in a simulation setting, where database results are saved without having to actually push events to a Google Calendar (which is generally unnecessary in a simulation context).
#' @return \code{scheduleOptim} returns a list of two data.tables: both the prioritization table, and the schedule table for this day. The prioritization table contains seven columns: the locationID (character), speciesID (character), date (character), pMax (numeric), pCurrent (numeric), weight (numeric) and init (numeric) [see AMMonitor Documentation \href{placeholderurl}{Chapter 10: Prioritization} for details and context]. The schedule table has nine columns, all character data type, and is formatted according to the inputs required by the Google Calendar API. These columns are: Subject, Start Date, Start Time, End Date, End Time, All Day Event, Description, Location, and Private [see AMMonitor Documentation \href{placeholderurl}{Chapter 9: Schedule} for further context].
#' 
#'  If 'google.push' = TRUE, the \code{scheduleOptim} also returns messaging about attempts to push monitoring schedules to a Google Calendar. 
#' 
#' @seealso \code{\link{prioritySet}}, \code{\link{priorityInit}}, \code{\link{simGlm}}, \code{\link{scheduleAddVars}}, \code{\link{scheduleFixed}}, \code{\link{scheduleDelete}}
#' @export
#' @examples
#'
#' \dontrun{
#'        
#' # ===============================================================
#' #           EXAMPLE USING BINOMIAL-BASED ACTIVITY MODELS
#' #                   AND "PRESENT DAY" DATA
#' # ===============================================================  
#'      
#' # ------------------------------------------------------------
#' # Set up a demo AMMonitor database
#' # ------------------------------------------------------------
#' 
#' # Create the database (this will be deleted):
#' db.name <- 'demo.sqlite'
#'                 
#' dbCreateSample(db.name = db.name, 
#'                file.path = paste0(getwd(),'/'), 
#'                tables = c('locations', 'deployment', 'equipment',
#'                           'accounts', 'species', 'priorities',
#'                            'people'))
#'                 
#' # Verify that the database exists in your current working directory:
#' file.exists(db.name)
#' 
#' # Connect to the db: 
#' db.path <- paste0(getwd(), '/', db.name)
#' conx <- dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
#'
#' # Set deployments so that only 3 locations are actively monitored
#' # (to concisify the demonstration of this function)
#' dbExecute(conn = conx, statement =  "UPDATE deployment 
#'                                      SET dateRetrieved = '2016-01-20'
#'                                      WHERE locationID NOT IN 
#'                                      ('location@1', 'location@2', 'location@3')")
#'
#' # --------------------------------------------------------------
#' # Read in example AMModel library
#' # --------------------------------------------------------------
#'
#' # Read in an AMModel activity library example: 
#' data(activity_amml)
#' 
#' # ---------------------------------------------------------------
#' # Run the priorityInit() function to initialize a priority-driven
#' # monitoring period based on the priorities table:
#' # ---------------------------------------------------------------
#' 
#' # Set init.date = today by using Sys.Date()
#' priorityInit(db.path = db.path, init.date = Sys.Date())
#' 
#' # Check the priorities table to confirm it has been initialized:
#' dbGetQuery(conx, 'SELECT * FROM prioritization')
#' 
#' #----------------------------------------------------------------
#' # Optimize the schedule for tomorrow  and return the results as a 
#' # list of two data.tables: prioritization and schedule. 
#' # Set db.insert and google.push to FALSE to ensure results are not 
#' # added to the database, and no schedule events are pushed to Google 
#' # calendar. No calendar.key is required, but a temporals.key is 
#' # required to obtain a new forecast for tomorrow. 
#' #----------------------------------------------------------------
#' 
#' test_optim <- scheduleOptim(db.path = db.path,
#'                             amml = activity_amml,
#'                             choose.models = c('copo_binomial', 
#'                                               'leni_binomial', 
#'                                               'btgn_binomial', 
#'                                               'verd_binomial'), 
#'                             calendar.key = NULL,
#'                             temporals.key = 'settings/dark-sky-key.RDS',
#'                             optimization.approach = 'simple',
#'                             daily.site.constraint = 5, 
#'                             time.units = 'by.minute',
#'                             max.per.hour = NULL,
#'                             db.insert = FALSE,
#'                             google.push = FALSE)
#'
#' 
#' #----------------------------------------------------------------
#' # Optimize the schedule for tomorrow  and return the results as a 
#' # list of two data.tables: prioritization and schedule. 
#' # Set db.insert and google.push to TRUE to add results to the 
#' # database and to push scheduled events to Google Calendars. 
#' # A calendar.key and temporals.key are required. 
#' #----------------------------------------------------------------
#' 
#' # Run optimization
#' scheduleOptim(db.path = db.path,
#'               amml = activity_amml,
#'               choose.models = c('copo_binomial', 
#'                                 'leni_binomial', 
#'                                 'btgn_binomial', 
#'                                 'verd_binomial'), 
#'               temporals.key = 'settings/dark-sky-key.RDS',
#'               calendar.key = 'settings/calendar-10b0fdaac306.json',
#'               optimization.approach = 'simple',
#'               daily.site.constraint = 5, 
#'               time.units = 'by.minute',
#'               max.per.hour = NULL,
#'               db.insert = TRUE,
#'               google.push = TRUE)
#'               
#' # Check results:
#' dbGetQuery(conx, 'SELECT * from schedule')
#' dbGetQuery(conx, 'SELECT * from prioritization')
#' 
#' # Delete events from the Google Calendar itself:
#' scheduleDelete(db.path = db.path,
#'               # There are only three locations in this example:
#'               locationID = c('location@1', 'location@2', 'location@3'),
#'               calendar.key = 'settings/calendar-10b0fdaac306.json',
#'               start.date = as.Date(Sys.time()) + 1,
#'               end.date = as.Date(Sys.time()) + 1)
#'                
#' # ------------------------------------------------------------
#' # Disconnect from demo database and delete from working directory
#' # ------------------------------------------------------------
#' 
#' dbDisconnect(conx)
#' unlink(db.path)                 
#'          
#'                          
#'                                          
#' # ===============================================================
#' #            EXAMPLE USING POISSON-BASED ACTIVITY MODELS
#' #                      AND HISTORICAL DATA
#' # 
#' #    (i.e., running scheduleOptim() in a simulation context)
#' # 
#' # ===============================================================
#' 
#' # ------------------------------------------------------------
#' # Set up a demo AMMonitor database
#' # ------------------------------------------------------------
#' 
#' # Create the database (this will be deleted):
#' db.name <- 'demo.sqlite'
#'                 
#' dbCreateSample(db.name = db.name, 
#'                file.path = paste0(getwd(),'/'), 
#'                tables = c('locations', 'deployment', 'equipment',
#'                           'accounts', 'species', 
#'                           'temporals', 'people'))
#'                 
#'                 
#' # Verify that the database exists in your current working directory:
#' file.exists(db.name)
#' 
#' # Connect to the db: 
#' db.path <- paste0(getwd(), '/', db.name)
#' conx <- dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
#'
#' # Set deployments so that only 3 locations are actively monitored
#' # (to concisify the demonstration of this function)
#' dbExecute(conn = conx, statement =  "UPDATE deployment 
#'                                      SET dateRetrieved = '2016-01-20'
#'                                      WHERE locationID NOT IN 
#'                                      ('location@1', 'location@2', 'location@3')")
#' 
#' 
#' # ------------------------------------------------------------
#' # Read in an AMModel activity library example: 
#' # ------------------------------------------------------------
#' 
#' data(activity_amml)
#' 
#' # ---------------------------------------------------------------
#' # Run the prioritySet() function to populate the priorities
#' # table with equal monitoring weights for all species at all
#' # monitoring locations
#' # ---------------------------------------------------------------
#' 
#' # In this example, we will use the convenience function
#' # prioritySet() to set equal monitoring weights at all sites,
#' # with a pMax of 4, indicating that monitoring should continue
#' # until at least four signals are captured from each species
#' 
#' prioritySet(db.path = db.path, 
#'              speciesID = c('verd', 'leni', 'btgn', 'copo'),
#'              p.max = 4, 
#'              db.insert = TRUE)
#'              
#' # ---------------------------------------------------------------
#' # Now that the priorities table has been populated,
#' # Run the priorityInit() function to initialize a priority-driven
#' # monitoring period based on the priorities table:
#' # ---------------------------------------------------------------
#' 
#' # Set init.date to '2016-03-01' because we are using existing
#' # data in the temporals table
#' priorityInit(db.path = db.path, init.date = '2016-03-01')
#' 
#' # Check the priorities table to confirm it has been initialized:
#' dbGetQuery(conx, 'SELECT * FROM prioritization')
#'  
#' #----------------------------------------------------------------
#' # Optimize the schedule for tomorrow  and return the results as a 
#' # list of two data.tables: prioritization, and schedule. 
#' # Set db.insert and google.push to FALSE to ensure results are not 
#' # added to the database. No calendar.key is required. Set temporals.key 
#' # to NULL since we already added historical temporal data to the database.
#' #----------------------------------------------------------------
#' 
#' # Run optimization
#' test_optim <- scheduleOptim(db.path = db.path,
#'                             amml = activity_amml,
#'                             choose.models = c('verd_poisson',
#'                                               'btgn_poisson',
#'                                               'leni_poisson',
#'                                               'copo_poisson'), 
#'                             temporals.key = NULL,
#'                             calendar.key = NULL,
#'                             optimization.approach = 'simple',
#'                             daily.site.constraint = 5, 
#'                             time.units = 'by.minute',
#'                             max.per.hour = NULL,
#'                             db.insert = FALSE,
#'                             google.push = FALSE)
#'
#' 
#' #----------------------------------------------------------------
#' # Optimize the schedule for tomorrow  and return the results as a 
#' # list of two data.tables: prioritization, and schedule. 
#' # Set db.insert and google.push to TRUE to add results to the database
#' # and push scheduled events to Google Calendars. A calendar.key is required. 
#' # Set temporals.key to because the database already contains temporal 
#' # data.
#' #----------------------------------------------------------------
#' 
#' # Run optimization
#' optim <- scheduleOptim(db.path = db.path,
#'                        amml = activity_amml,
#'                        choose.models = c('verd_poisson',
#'                                          'btgn_poisson',
#'                                          'leni_poisson',
#'                                          'copo_poisson'), 
#'                        calendar.key = 'settings/calendar-10b0fdaac306.json',
#'                        temporals.key = NULL, 
#'                        optimization.approach = 'simple',
#'                        daily.site.constraint = 5, 
#'                        time.units = 'by.minute',
#'                        max.per.hour = NULL,
#'                        db.insert = TRUE,
#'                        google.push = TRUE)
#'               
#' # Check results:
#' dbGetQuery(conx, 'SELECT * from schedule')
#' dbGetQuery(conx, 'SELECT * from prioritization')
#' 
#' # Delete events from the Google Calendar itself:
#' scheduleDelete(db.path = db.path,
#'               # There are only three locations in this example:
#'               locationID = c('location@1', 'location@2', 'location@3'),
#'               calendar.key = 'settings/calendar-10b0fdaac306.json',
#'               start.date = as.Date(Sys.time()) + 1, 
#'               end.date = as.Date(Sys.time()) + 1)
#'                       
#'                        
#' # ------------------------------------------------------------
#' # Disconnect from demo database and delete from working directory
#' # ------------------------------------------------------------
#' 
#' dbDisconnect(conx)
#' unlink(db.path)
#' 
#' }
#'

scheduleOptim <- function(db.path,
                          calendar.key = NULL,
                          amml,
                          choose.models, 
                          temporals.key = NULL,
                          optimization.approach = 'simple',
                          daily.site.constraint,
                          time.units,
                          max.per.hour = NULL, 
                          db.insert = FALSE,
                          google.push = FALSE)
{
  
  # Perform argument checks: 
  if (missing(db.path))
    stop("Missing argument 'db.path'. A path to an active AMMonitor RSQLite database is required in order to use this function.")
  
  if (missing(amml))
    stop("An AMModels model library is required in order to use this function. See details.")
  
  if (daily.site.constraint > 690) 
    warning("You have more sampling power than this system can accommodate -- lucky you! This algorithm will run, but detection probabilities for your research circumstances will likely not be improved by this optimization scheme.")
  
  if (missing(time.units)) 
    stop("Please use the 'time.units' argument to clarify whether your modeled activity probabilities or rates should be applied on a by.hour or by.minute basis.")
  
  if (optimization.approach == 'max.per.hour' & is.null(max.per.hour)) 
    stop("Please input a value for argument max.per.hour if you have selected the 'max.per.hour' optimization approach.")
  
  if (optimization.approach == 'max.per.hour') {
    if (max.per.hour > 30) {
      message("You have supplied a 'max.per.hour' value above 30, but a maximum of only 30 minutes may be sampled out of each hour. Switching to method 'simple'.")
      optimization.approach <- 'simple'
    }
  }
  
  if (db.insert == TRUE & google.push == TRUE & is.null(calendar.key)) 
    stop("If db.insert and google.push are both set to TRUE, a calendar.key is required to use the Google Calendar.")
  
  # Connect to the database:
  conx <- dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
  
  # Turn the SQLite foreign constraints on
  rs <- dbSendQuery(conn = conx, statement = "PRAGMA foreign_keys = ON;" )
  dbClearResult(rs)
  
  # Gather deployment info for active monitoring locations:
  cal.info <- qryDeployment(conn = conx, locationID = 'all')
  length.locs <- nrow(cal.info)
  
  # Read in prioritization table from the most recent date 
  # for all species at all active monitoring locations
  optim.today <- qryPrioritization(conx)
  colnames(optim.today)[colnames(optim.today) == 'max(date)'] <- 'date'
  today <- unique(optim.today[,'date'])
  tomorrow <- as.character(as.Date(today) + 1)
  
  if (any(optim.today[, 'pMax'] < 0)) 
    stop('Your prioritization table contains a negative value(s) for pMax. Please ensure all pMax values are positive.')
  
  # Gather temporal data either via forecast or via history data stored in db:
  if (!is.null(temporals.key)) {
    temporal.dat <- temporalsGet(db.path = db.path, 
                                 db.insert = db.insert, 
                                 temporals.key = temporals.key) 
    temporal.dat <- temporal.dat$temporal.data # only keep data.table portion
  } 
  
  if (is.null(temporals.key)) {
    # Only grab historical temporal data for active monitoring locations:
    active.loc.ids <- cal.info$locationID
    in.qry <- paste0(sprintf("'%s'", active.loc.ids), collapse = ", ")
    temporal.dat <- dbGetQuery(conx, 
                               paste0("select * from temporals 
                                        where date = '", 
                                      as.character(tomorrow), "' AND
                                        locationID IN (", in.qry,")"))
    if (nrow(temporal.dat) == 0)
      stop("You did not enter a temporals.key, but your temporals table is empty for this day:", tomorrow, "Either enter a temporals.key to get the next day's forecast, or run temporalsGet() to acquire historical weather data to be added to the temporals table")
  }
  
  if (is.null(temporals.key)) {
    if (tomorrow != unique(temporal.dat$date)) 
      stop(paste0('There is a mismatch between the most recent dates available in the prioritization table (', tomorrow,') and the temporals table (', unique(temporal.dat$date),'). You may need to include a temporals.key, or you may need to first use temporalsGet() to collect any desired historical data for your temporals table before using this function.'))
  }
  
  # Make sure temporal dat is a data table
  setDT(temporal.dat, key = c('locationID', 'date', 'time'))
  tomorrow.dat <- temporal.dat[date == tomorrow]
  
  # Add circular variables 
  tomorrow.dat <- scheduleAddVars(tomorrow.dat)  
  
  #############################################################################
  #   GENERATE PREDICTIONS FOR SPECIES ACTIVITY BEHAVIOR AT ACTIVE LOCATIONS  #
  #############################################################################
  
  # Sort the focal species present in the optimization table, and check them 
  # against the focal speciesID metadata choose.model names selected from the amml
  
  # Get the names of the focal species in the prioritization table (sort for alpha order)
  focals <- sort(unique(optim.today$speciesID))
  
  # Read in metadata for the chosen models
  mods.meta <- modelMeta(amml = amml, x = choose.models)
  
  # Find speciesID metadata for each model (and sort for alpha order)
  sp.ids <- sort(unlist(lapply(mods.meta, '[[', 'speciesID')))
  
  # Perform cross-checks of sp.ids vs. focals
  if (any(!(focals %in% sp.ids))) {
    not.in <- paste(focals[!(focals %in% sp.ids)], collapse = ', ')
    stop(paste0('The following species present in the prioritization table do not have a matching speciesID indicated in the speciesID metadata of the models listed in choose.models: ', not.in))
  }
  
  if (any(!(sp.ids %in% focals))) {
    not.in <- paste(sp.ids[!(sp.ids %in% focals)], collapse = ', ')
    stop(paste0('The following speciesIDs in the metadata of the models selected in choose.models do not correspond with a speciesID in the prioritization table: ', not.in))
  }
  
  # Set up a data.table to collect activity probabilities or rates, 
  # for each species, at each location, over each hour of the day: 
  pr.call <- data.table(cbind(tomorrow.dat[,c('locationID', 'time')],
                              matrix(data = 0,
                                     nrow = nrow(tomorrow.dat),
                                     ncol = length(focals))))
  colnames(pr.call) <- c('locationID', 'time', focals)
  
  # Order data tables by locationID & time:
  setkeyv(pr.call, cols = c('locationID', 'time'))
  setkeyv(tomorrow.dat, cols = c('locationID', 'time'))
  
  # Loop through all models in the amml to generate predictions:
  model.families <- vector(mode = 'character', length(focals))
  for (i in 1:length(focals)) {
    message('Predicting activity for species ', i, ': ', focals[i])
    
    # Read in the model
    this.model <- getAMModel(amml = amml, 
                             # Use sorted sp.ids vector to ensure alpha order match:
                             x = names(sp.ids)[i],  
                             as.list = FALSE)
    
    # Grab and store the model families for checking
    fam <- this.model$family$family
    if (is.null(fam)) { fam <- this.model[[i]]$family$family }
    model.families[i] <- fam 
    
    # Apply polynomial variables (if any)
    tomorrow.dat.i <- tomorrow.dat
    varnames.spl <- strsplit(x = names(this.model$coefficients), 
                             split = '.', fixed = TRUE)
    with.poly <- varnames.spl[unlist(lapply(varnames.spl, length)) == 2]
    if (length(with.poly)) {
      poly.name <- sapply(with.poly, '[[' , 1)
      poly.coef <- sapply(with.poly,'[[', 2)
      
      # Eliminate any tic marks that pollute the data
      poly.name <- gsub(pattern = '`', replacement = '', x = poly.name)
      poly.coef <- gsub(pattern = '`', replacement = '', x = poly.coef)
      
      for (p in 1:length(with.poly)) {
        # Loop through to create each polynomial variable
        tomorrow.dat.i <- tomorrow.dat.i[,paste0(poly.name[p], '.', poly.coef[p]) := 
                                           get(poly.name[p])^as.numeric(poly.coef[p])]
      }
    } # end 'if polynomials' 
    
    # Generate species activity rates/probabilities and store them: 
    pr.call[,i + 2] <- predict(object = this.model, 
                               newdata = tomorrow.dat.i, 
                               type = 'response')
  }
  
  # Ensure that all models are of one type; if not, stop:
  family <- unique(model.families)
  if (length(family) != 1) 
    stop('Inconsistent species models: must either be all poisson or all binomial.')
  
  # Re-order the pr.call columns to ensure speciesIDs are in alpha order:
  setcolorder(pr.call, c('locationID', 'time', 
                         sort(colnames(pr.call)[3:ncol(pr.call)])))
  
  # Order optim.today table by locationID & speciesID:
  setDT(optim.today, key = c('locationID', 'speciesID'))
  
  
  #############################################################################
  #         CREATE OPTIMIZED SCHEDULES FOR EACH LOCATION FOR TOMORROW         #
  #############################################################################
  
  # Compute sumproduct of predicted activity probs * species weights by site
  # Example scores for 1 location:
  # score <- as.matrix(pr.call[locationID == 'location@1', 3:ncol(pr.call)]) %*%
  #     as.matrix(optim.tomorrow[locationID == 'location@1', 'weight'])
  
  # Set up to store outputs while looping:
  scores <- samples <- schedules <- problem.locs.list <- list()
  locations <- sort(cal.info$locationID)
  
  # Loop through each location to generate an optimized sampling schedule 
  # at each location based on the highest scoring time intervals (hours) 
  for (z in 1:length(locations)) {
    
    this.location <- locations[z]
    
    # Figure out if daylight saving transition will occur tomorrow:
    # (will either have a missing value or duplicated hour if so)
    date.range.covered <- seq(from = as.POSIXct(paste(tomorrow, '00:00:00'), 
                                                tz = cal.info[z, 'tz']),
                              to = as.POSIXct(paste(tomorrow, '23:00:00'), 
                                              tz = cal.info[z,'tz']), 
                              by = 60*60)
    rep.len <- length(date.range.covered)
    
    # In the SQLite system, we are not allowed to have 
    # rows with a duplicate for locationID/date/time/type (foreign key constraint). 
    # Thus, we cannot have the two "01:00:00" entries that occur during fall-back of DST
    # in the database at all, and one of those entries must be excluded. 
    if (rep.len == 25) rep.len <- 24
    
    # Initialize a vector of 0s for each hour available tomorrow:
    these.samples <- rep(0, rep.len)
    
    # Find current weights at this location today:
    #  (recall that optim.today has already been sorted so that
    #   speciesIDs are in alpha order; so weights are in correct order)
    weights <- as.matrix(optim.today[locationID == this.location, weight])
    
    # Grab activity probs/rates for all of tomorrow's hours for focal species:
    call.probs <- as.matrix(pr.call[locationID == this.location, 
                                    3:ncol(pr.call)])
    
    # Normalize the weights to ensure they sum to 1: 
    nweights <- weights/sum(weights)
    
    if (all(is.na(nweights))) {
      # If the pMax target has been met, and all weights are 0,
      # reset the weights so that each species is weighted equally.
      # This way sampling will continue at useful times of day 
      # (otherwise, sampling will be shunted to midnight due to NaNs)
      nweights <- rep(1/(length(nweights)), length(nweights))
    }
    
    # Compute scores at this site via matrix multiplication:
    scores[[z]] <- call.probs %*% nweights
    
    # Rank each hour of the day based on its species-aggregate score,
    # with random tiebreaker if ties are encountered:
    hour.ranks <- rank(-scores[[z]], ties.method = 'random')
    
    # Compute which times to sample under 'simple' method:
    #  If daily.site.constraint <= 30, all samples will be allocated into the same hour
    #  (if sampling 1 minute at a time, we need to leave 1 minute between samples
    #  for the recording scheduling protocol to work properly in the field)
    if (optimization.approach == 'simple') {
      n.samp.hour <- ifelse(test = daily.site.constraint <= 30, 
                            yes = daily.site.constraint, 
                            no = 30)
      
      if (daily.site.constraint <= 30) {
        # If daily.site.constraint <= 30, allocate all samples into the top scoring hour
        these.samples[which(hour.ranks == 1)] <- n.samp.hour
        # (note that 'these.samples' now gives a vector of how many samples to take
        #  during each hour of the day)
      }
      
      if (daily.site.constraint > 30) {
        # If have more than 30 minutes of sampling, allot extra to next top scoring hours:
        # Total of 690 minutes allowed to sample each day (to allow 1 minute buffer
        # between each scheduled event, and allowing a 30 minute buffer for data 
        # transmission of new schedule AND any recordings).
        if (daily.site.constraint > 690) {
          daily.site.constraint <- 690
          message('Constraining daily.site.constraint to 690.')
        }
        
        # Find the total number of hours during which we can sample:
        n.hours.to.sample <- ceiling(daily.site.constraint/30)
        
        # Assign 30 samples each to all hours during which we sample:
        these.samples[which(hour.ranks %in% 1:n.hours.to.sample)] <- 30
        
        # Correct the last-ranked sampled hour if daily.site.constraint
        # is not evenly divisible by 30: 
        if (daily.site.constraint %% 30 != 0) {
          these.samples[which(hour.ranks == n.hours.to.sample)] <-  
            daily.site.constraint %% 30
        }
      } # end if daily.site.constraint > 30
    } # end if 'simple'
    
    # Compute which times to sample under 'max.per.hour' method:
    if (optimization.approach == 'max.per.hour') {
      
      # Find the number of hours during which we sample: 
      n.hours.to.sample <- ceiling(daily.site.constraint/max.per.hour)
      
      # Assign max.per.hour samples each to all hours we sample:
      these.samples[which(hour.ranks %in% 1:n.hours.to.sample)] <- max.per.hour
      
      # Correct the number of samples in the last-ranked sampled hour  
      # if the daily.site.constraint is not evenly divisible by max.per.hour: 
      if (daily.site.constraint %% max.per.hour != 0) {
        these.samples[which(hour.ranks == n.hours.to.sample)] <-  
          daily.site.constraint %% max.per.hour
      }
      
    } # end if 'max.per.hour'
    
    # Store the chosen sampling times for this location: 
    samples[[z]] <- list(these.samples)
    tomorrow.times <- unique(tomorrow.dat[,time])
    
    # Create the Google Calendar sampling schedule for this location:
    google.cal <- scheduleOptimCreate(tomorrow.date = tomorrow,
                                      sampling.hours = these.samples,
                                      tomorrow.times = tomorrow.times,
                                      timezone = cal.info[z, 'tz'],
                                      locationID = this.location)
    
    # If delivering events to Google Calendar:
    if (google.push == TRUE) {
      
      # Invoke tryCatch to skip problem locations:
      catch.error <- tryCatch(
        
        # Add schedule events to Google Calendar and the database:
        google.cal <- schedulePushHelper(conn = conx,
                                         cal.info.i = cal.info[z, ],
                                         calendar.key = calendar.key,
                                         google.cal = google.cal,
                                         timezone = cal.info[z, 'tz'], 
                                         google.push = google.push),
        error = function(e) e
      ) # End tryCatch
      
      if (inherits(catch.error, "error")) {
        
        # If we encounter an error for this location, provide error message
        # and skip to the next location: 
        message('There was a problem pushing Google Calendar events for ', 
                cal.info[z,'locationID'], '\n Skipping to next location.')
        
        # Perhaps we append this to an object of problem locations for
        # which push did not successfully occur, and then return those as a message
        # at the end to make the error summary more clear
        problem.locs.list[[z]] <- cal.info[z, 'locationID']
        
        next
      }
      
    }  # end of google.push == TRUE
    
    # If inserting the schedule to the database but not pushing the calendars 
    # to google (i.e., if running a simulation setting), we still need to run
    # schedulePushHelper to get the column names & inputs all properly formatted
    # for the sqlite database
    if (google.push == FALSE & db.insert == TRUE) {
      # Add schedule events to Google Calendar and the database:
      google.cal <- schedulePushHelper(conn = conx,
                                       cal.info.i = cal.info[z, ],
                                       calendar.key = calendar.key,
                                       google.cal = google.cal,
                                       timezone = cal.info[z, 'tz'],
                                       google.push = google.push)
    }
    
    # Store a copy of this location's schedule: 
    schedules[[z]] <- google.cal
  } # end looping through locations to set schedules
  
  # Bind schedules at all locations into one data.table to return to user:  
  sched <- rbindlist(schedules)
  
  
  
  #############################################################################
  #        UPDATE PRIORITIZATION TABLE FOR EACH SPECIES AND LOCATION          #
  #############################################################################
  
  # Setup data.table summarizing activity probs/rates, scores, & how we sampled
  samp <- pr.call
  samp[, scores := unlist(scores)]
  samp[, n.samples.this.hour := unlist(samples)]
  
  # Grab the initial priority weights 
  init.weights <- dbGetQuery(conx, 'select 
                                    locationID, speciesID, weight, max(date), init
                                    from prioritization where init = 1
                                    group by locationID, speciesID')
  init.weights <- setDT(init.weights, key = c('locationID', 'speciesID'))
  
  # Loop through each location to compute P*d, the probability that a species 
  # has been acoustically captured today for each species at each site. 
  # Note that on day 1, P*d = P*c
  optim.tomorrow <- list()
  for (j in 1:length(locations)) {
    this.location <- locations[j]
    optim.today.loc <- setkey(optim.today[locationID == this.location], speciesID)
    optim.tomorrow.loc <- optim.today.loc
    
    # Find the matrix of probs/rates for sampled hours at this location:
    keep <- c(focals, 'n.samples.this.hour')
    sampled.loc <- samp[n.samples.this.hour > 0 & 
                          locationID == this.location, ..keep] 
    
    # Explode the matrix of probs/rates out for each minute that was sampled: 
    compute.pd.loc <- sampled.loc[rep(seq(1, .N), n.samples.this.hour)]
    compute.pd.loc[, n.samples.this.hour := NULL] # remove unneeded sample column
    
    # If rates/probs were modeled on an hourly basis, then divide by 60
    # to spread the values out across each minute of the hour
    if (time.units == 'hourly') compute.pd.loc <- compute.pd.loc/60
    
    # Grab the pMaxes for this location & ensure they are ordered by speciesID:
    pmax.loc <- setkey(optim.today[locationID == this.location], speciesID)[,pMax] 
    
    # For binomial activity probability:
    if (family == 'binomial') {
      
      # Compute P*d, the prob. of acoustic capture today: 
      pd.loc <- apply(compute.pd.loc, 2, function(x) 1 - prod(1 - x, na.rm = TRUE))
      
      # Compute P*c, the cumulative prob. of acoustic capture across all days:
      #    On day 1, pc.loc should == pd.loc
      pc.loc <- 1 - (1 - optim.today.loc[,pCurrent])*(1 - pd.loc)
      
      # If pc.loc is greater than one, constrain to 1: 
      pc.loc[pc.loc > 1] <- 1
      
    }
    
    # For poisson activity rates: 
    if (family == 'poisson') {
      
      # Compute P*d, the rate of acoustic capture today:
      pd.loc <- apply(compute.pd.loc, 2, sum, na.rm = TRUE)
      
      # Compute P*c, the cumulative rate of acoustic capture across all days:
      #    On day 1, pc.loc should == pd.loc
      pc.loc <- optim.today.loc[,pCurrent] + pd.loc
      
    }
    
    # Update tomorrow's optim table with p.current value
    optim.tomorrow.loc[ , pCurrent := pc.loc]
    
    # Compute the new weights:
    weights.loc <- init.weights[locationID == this.location, weight]
    wts.raw <- (pmax.loc - pc.loc)*weights.loc
    wts.raw[wts.raw <= 0] <- 0 # negative values where target has been met become 0
    wts <- unlist(wts.raw/sum(wts.raw))
    wts[is.na(wts)] <- 0 # Convert to 0 and nans produced as a result of hitting the target
    optim.tomorrow.loc[ , weight := wts]
    optim.tomorrow[[j]] <- optim.tomorrow.loc
  }
  
  # Save the optim.tomorrow results for return to user:
  optim.tomorrow.dt <- rbindlist(optim.tomorrow)
  optim.tomorrow.dt[, date := as.character(tomorrow)] 
  optim.tomorrow.dt[, init := 0]
  
  # Make sure columns are in the right order
  setcolorder(optim.tomorrow.dt, 
              neworder = dbListFields(conn = conx, name = 'prioritization'))
  
  if (db.insert == TRUE) {
    
    # Update prioritization table in the database:
    dbWriteTable(conn = conx, name = 'prioritization', value = optim.tomorrow.dt,
                 row.names = FALSE, overwrite = FALSE,
                 append = TRUE, header = FALSE)
    
  } # end prioritization calculations
  
  # Gather the final output
  output <- list(prioritization = optim.tomorrow.dt, schedule = sched)
  
  # Summarize any problematic pushes: 
  if (length(unlist(problem.locs.list))) {
    message(paste0('\n Google Calendar events failed to push for the following locations: \n',
                   paste(unlist(problem.locs.list), sep = '', collapse = ', ')))
  }
  
  return(output)
} # end function


# scheduleOptimCreate() =======================================================
# A helper function to create the schedule object from scheduleOptim

scheduleOptimCreate <- function(tomorrow.date,
                                sampling.hours,
                                tomorrow.times,
                                timezone,
                                locationID){
  # Find hours being sampled:
  s.hours <- which(sampling.hours > 0)
  
  # Create the time interval between samples:
  intervals <- 60/sampling.hours[s.hours] # rounds down if uneven interval
  
  # Generate the actual sampling times: 
  s.times <- as.POSIXct(tomorrow.times[s.hours], tz = timezone)
  
  # Create the end of the sequence:
  seq.to <- s.times + 59*60
  
  # Loop through each sampled hour to generate samples:
  start.minutes <- end.minutes <- list()
  for (i in 1:length(s.hours)) {
    start.minutes.posix <- seq(from = s.times[i],
                               to = seq.to[i],
                               by = paste0(intervals[i], ' min'))
    end.minutes.posix <- start.minutes.posix + 60 # we assume 1 minute recordings only
    
    # If uneven division produces more samples than required, delete extras:
    cut.samples <- length(start.minutes.posix) - sampling.hours[s.hours[i]]
    if (cut.samples > 0) {
      start.minutes.posix <- start.minutes.posix[-(1:cut.samples)]
      end.minutes.posix <- end.minutes.posix[-(1:cut.samples)]
    } # (note that there should never be an occasion where cut.samples < 0, 
    # because we are rounding down if the interval is uneven)
    
    # Save the sampling minutes in correct format:
    start.minutes[[i]] <- strftime(start.minutes.posix,
                                   format = '%H:%M:%S', tz = timezone)
    end.minutes[[i]] <- strftime(end.minutes.posix,
                                 format = '%H:%M:%S', tz = timezone)
  }
  
  # Create Google Calendar-formatted data.table:
  google.cal <- data.table(Subject = "Recording",
                           "Start Date" = as.character(tomorrow.date), 
                           "Start Time" = sort(unlist(start.minutes)), 
                           "End Date" = as.character(tomorrow.date), 
                           "End Time" = sort(unlist(end.minutes)),
                           "All Day Event" = "False",
                           Description = "Optim Calendar",
                           Location = locationID, 
                           Private = "False")
  return(google.cal)
}


# schedulePushHelper ================
# helper function for pushing schedule events in scheduleOptim(), scheduleFixed(), and scheduleSun()
# and recording events to Google Calendar 
schedulePushHelper <- function(conn, cal.info.i, calendar.key, 
                               google.cal, timezone, google.push){
  
  if (google.push == TRUE) {
    # Push schedule to Google account:
    schedulePush(calendar.key = calendar.key,
                 account = cal.info.i$email,
                 schedule = google.cal,
                 timezone = timezone)
  }
  
  # Rename & order columns so that they can be added to the database:
  sched <- google.cal
  sched[ , equipmentID := cal.info.i$equipmentID]
  sched[, tz := timezone]
  colnames(sched) <- c('subject', 'startDate', 'startTime', 'endDate',
                       'endTime', 'allDayEvent', 'description', 'locationID',
                       'private', 'equipmentID', 'tz')
  
  # Reorder columns based on what is anticipated by the database:
  setcolorder(x = sched, neworder = dbListFields(conn, 'schedule'))
  
  # Add schedule to database:
  dbWriteTable(conn = conn, name = 'schedule', value = sched,
               row.names = FALSE, overwrite = FALSE,
               append = TRUE, header = FALSE)
  return(sched)
}


# simGlm() ==========
#' @name simGlm
#' @title Simulate class 'glm' generalized linear models for activity or occurrence dynamics
#' @description Simulate generalized linear models (GLM) for species activity, the probability of occurrence, colonization, extinction, or other uses. Models may be generated using temporal data contained in the database, or with an independent data.frame or data.table. See AMMonitor Documentation \href{placeholderurl}{Chapter 10: Prioritization} for use context.
#' @param db.path The file path that houses the SQLite database. 
#' @param equation.list A list of lists, each named with a model name of the user's choice, and each containing a list with a character vector of covariate names and a numeric vector of coefficients. See \strong{Details} and \strong{Examples}.
#' @param model.type Character string indicating the model type to simulate. Options are 'activity', 'colonization', 'extinction', or 'other'.
#' @param model.family Character string indicating the model family to simulate. Options are 'binomial' or 'poisson'. Occurrence, colonization, and extinction models must be specified as binomial. Poisson is only an option for activity models, which may be modeled either as a binomial or count process. 
#' @param data If not using db.path, data.frame or data.table of covariates of the user's choice. If creating models using temporal data, note that \code{\link{scheduleAddVars}} is not automatically applied to a freestanding data.frame; users may wish to invoke \code{\link{scheduleAddVars}} to the 'data' object manually before using \code{simGlm}.
#' @param seed Optional integer to use within \code{\link{set.seed}} for reproducibility of calls to \code{\link{rpois}} or \code{\link{rbinom}}
#' @param ... Additional arguments to \code{\link[stats]{glm}}.
#' @details \code{simGlm} can be used to simulate target activity patterns or the probability of occurrence, colonization, extinction. The equation.list should contain a number of named elements equal to the number of models. Each named list element should contain a unique model name of the user's choice. Within each named list element, a nested list should contain the elements 'names' and 'coefficients'. The 'names' list element contains a character vector of the covariates in the model; each covariate must match the name of a column available in the temporals table if using the 'db.path' argument, or the name of a column available in the data.frame or data.table the user is inputting via the 'data' argument. If desired, polynomial terms may be added to covariate names by including '.N' next to the covariate (e.g. 'distRise.2' or 'temperature.3' for distRise^2 and temperature^3). Polynomial terms are automatically parsed and applied within \code{simGlm} via the '.' character. Lastly, the 'coefficients' list element contains a numeric vector of the coefficient values to be used in the model. Take care to ensure that coefficient values are ordered correctly according to covariate names. If using categorical covariates, ensure these are factor data type. 
#' 
#' If using 'db.path' argument, \code{simGlm} will automatically gather any data in the temporals table and apply the function \code{\link{scheduleAddVars}} to add covariates that accommodate circular variables, as well as a few other potentially useful temporal covariates. These include the sine and cosine of the day of the year, the sine and cosine around equinoces, the numeric distance of a given sampling hour from sunrise and sunset, the sine and cosine of the distance from a sunrise or sunset event, sine and cosine of the hour of the day, and sine and cosine of the moonphase. Many of these covariates may provide finer control when attempting to model target species activity patterns that vary according to time of day and time of year. 
#' 
#'  The 18 additional temporal covariates added to the temporal data by \code{\link{scheduleAddVars}}, and thus available as covariates within your equation.list object, are defined as follows:
#'  \enumerate{
#'  \item{\strong{dayOfYear}: The integer day of the year, ranging from 1 to 365 (or 366 if leap year).}
#'  \item{\strong{dayCos}: Cosine of the integer day of the year: \eqn{cos(2*pi*dayOfYear/max(dayOfYear))}.}
#'  \item{\strong{daySin}: Sine of the integer day of the year: \eqn{sin(2*pi*dayOfYear/max(dayOfYear))}.}
#'  \item{\strong{dayCosEquinox}: Cosine of the integer day of the year divided by two: \eqn{cos(2*pi*dayOfYear/max(dayOfYear/2))}.}
#'  \item{\strong{daySinEquinox}: Sine of the integer day of the year divided by two: \eqn{sin(2*pi*dayOfYear/max(dayOfYear/2))}.}
#'  \item{\strong{distRise}: Distance from sunrise. The absolute value of the number of minutes to the nearest sunrise.}
#'  \item{\strong{distSet}: Distance from sunset. The absolute value of the number of minutes to the nearest sunset.}
#'  \item{\strong{distRiseCos}: Cosine of the distance from sunrise: \eqn{cos(2*pi*distRise/1440)}.}
#'  \item{\strong{distSetCos}: Cosine of the distance from sunset: \eqn{cos(2*pi*distSet/1440)}.}
#'  \item{\strong{distRiseSin}: Sine of the distance from sunrise: \eqn{sin(2*pi*distRise/1440)}.}
#'  \item{\strong{distSetSin}: Sine of the distance from sunset: \eqn{sin(2*pi*distSet/1440)}.}
#'  \item{\strong{hour}: The integer of the hour of the day, ranging from 1 to 24.}
#'  \item{\strong{hourCos24}: Cosine of the integer hour of the day on a 24-hour scale: \eqn{cos(2*pi*hour/24)}.}
#'  \item{\strong{hourSin24}: Sine of the integer hour of the day on a 24-hour scale: \eqn{sin(2*pi*hour/24)}.}
#'  \item{\strong{hourCos12}: Cosine of the integer hour of the day on a 12-hour scale: \eqn{cos(2*pi*hour/12)}.}
#'  \item{\strong{hourSin12}: Sine of the integer hour of the day on a 12-hour scale: \eqn{sin(2*pi*hour/12)}.}
#'  \item{\strong{moonCos}: Cosine of the lunar phase: \eqn{cos(2*pi*moonPhase)}.}
#'  \item{\strong{moonSin}: Sine of the lunar phase: \eqn{sin(2*pi*moonPhase)}.}
#'  } 
#' @return List of models of class 'glm' 'lm' that can be stored in an AMModel library.
#' @family sim
#' @seealso \code{\link{scheduleAddVars}}
#' @export
#' @examples 
#'
#'\dontrun{
#'
#' # ------------------------------------------------------------
#' # Set up a demo AMMonitor database
#' # ------------------------------------------------------------
#'
#' # Create the database (this will be deleted):
#' db.name <- 'demo.sqlite'
#'                 
#' dbCreateSample(db.name = db.name, 
#'                file.path = paste0(getwd(),'/'), 
#'                tables = c('locations', 'equipment', 'deployment',
#'                           'accounts', 'species', 'priorities',
#'                           'people', 'temporals'))
#'                 
#' # Verify that the database exists in your current working directory:
#' file.exists(db.name)
#' 
#' # Connect to the db: 
#' db.path <- paste0(getwd(), '/', db.name)
#' conx <- dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
#' 
#' # ------------------------------------------------------------
#' # Set up an example equation object for species activity
#' # ------------------------------------------------------------ 
#' 
#' equations <- list(btgn_vocalization = 
#'                     list(names = c('intercept', 'dayOfYear', 
#'                                    'daySin', 'hourCos12',
#'                                    'distRise.2', 'temperature', 
#'                                    'temperature.3', 'windSpeed'),
#'                          coefficients = c(-0.3, -0.002, 
#'                                           1, -0.5, 
#'                                           -0.000007, 0.009, 
#'                                           -0.000001, -0.35)),
#'                   copo_vocalization = 
#'                     list(names = c('intercept', 'dayOfYear', 
#'                                    'dayCos', 'daySin', 
#'                                    'hourCos24', 'hourCos12', 
#'                                    'distRise', 'distSet', 
#'                                    'windSpeed','moonCos'),
#'                          coefficients = c(-1.5, -0.003, 
#'                                           0.5, 0.6, 
#'                                           1, -0.5, 
#'                                           -0.0005, -0.0005, 
#'                                           -0.1, -0.2)),
#'                   leni_vocalization = 
#'                     list(names = c('intercept', 'dayOfYear', 
#'                                    'dayCos', 'daySin', 
#'                                    'hourCos24', 'hourCos12', 
#'                                    'distRise', 'distSet',
#'                                    'windSpeed', 'moonCos'),
#'                          coefficients = c(-2, -0.006, 
#'                                           0.4, 0.7, 
#'                                           1, -0.5, 
#'                                           -0.0005, -0.0005, 
#'                                           -0.25, -0.3)),
#'                   verd_vocalization = 
#'                     list(names = c('intercept', 'dayOfYear', 
#'                                    'daySin', 'hourCos12', 
#'                                    'distRise.2', 'temperature', 
#'                                    'temperature.3', 'windSpeed'),
#'                          coefficients = c(-0.5, -0.004, 
#'                                           1, -1.5, 
#'                                           -0.000007, 0.009, 
#'                                           -0.000001, -0.25)))
#'
#' # ------------------------------------------------------------
#' # Run models using the 'db.path' argument. Here, temporal 
#' # data will be automatically extracted from the database and
#' # used to create models. The function scheduleAddVars() will
#' # run internally to calculate additional temporal covariates 
#' # that accommodate circulate data types.
#' # ------------------------------------------------------------  
#'   
#' models <- simGlm(db.path = db.path,
#'                  equation = equations,
#'                  model.type = 'activity',
#'                  model.family = 'binomial')
#'
#' # ------------------------------------------------------------
#' # Run models using the 'data' argument instead of 'db.path'. 
#' # If using temporal covariates, you may wish to add circular
#' # temporal covariates first with scheduleAddVars(). 
#' # You can also join in additional temporal or spatial covariates 
#' # according to your own needs.
#' # ------------------------------------------------------------                      
#' 
#' # Create a new equations object. Note that the below example
#' # contains covariates both from the typical AMMonitor temporal
#' # data, as well as three new variables named 'randomTemporal', 
#' # 'randomSpatial', and 'categoricalSpatial'. Note that we have 
#' # added a polynomial term to 'randomSpatial' in the bear_activity 
#' # list element below:
#' 
#' equations <- list(bear_activity = 
#'                    list(names = c('intercept', 'daySin', 
#'                                   'hourCos12', 'randomSpatial.3'),
#'                         coefficients = c(-1.3, 1, 
#'                                          -0.5, -0.000001)),
#'                  moose_activity = 
#'                    list(names = c('intercept', 'dayOfYear', 
#'                                   'randomTemporal', 'categoricalSpatial'),
#'                         coefficients = c(-2.5, -0.003, 
#'                                           0.003, -0.08)))
#' 
#' # Read in some temporal data:
#' data(temporals)
#' 
#' # Add circular temporal covariates to the data.table: 
#' more.data <- scheduleAddVars(temporal.data = temporal.data)
#' 
#' # Use R to add in any other necessary covariates you may wish to add,
#' # according to what you have modeled in the "equations" object:
#' 
#' #   Add a fake temporal covariate: 
#' more.data[, randomTemporal := runif(n = .N)] 
#' 
#' #   Add fake spatial covariates:
#' more.data[, randomSpatial := rnorm(n = length(unique(locationID))), 
#'           by = locationID]
#' categorical.spatial <- sample(x = c('forest', 'desert', 'mountain'), 
#'                               size = 50, replace = TRUE)
#' more.data[, categoricalSpatial := factor(
#'   unlist(lapply(categorical.spatial, rep, times = 24)))]
#'
#' # Create models using your custom data.table (or data.frame) 
#' # rather than db.path:
#' models <- simGlm(equation = equations,
#'                  model.type = 'activity',
#'                  model.family = 'poisson',
#'                  data = more.data)
#'
#'
#' # ------------------------------------------------------------
#' # Disconnect from demo database and delete from working directory
#' # ------------------------------------------------------------
#' 
#' dbDisconnect(conn = conx)
#' unlink(db.path)
#' 
#'}
#'

simGlm <- function(db.path, 
                   equation.list,
                   model.type,
                   model.family,
                   data,
                   seed = NULL,
                   ...
)
{
  
  # Perform argument checks: 
  if (missing(model.type)) {
    stop('Please input model.type: "activity", "occurrence", "colonization", "extinction", or "other"')
  }
  
  # If using data stored in the database, then grab it: 
  if (!missing(db.path)) {
    
    # Connect to the database if using it: 
    database <- TRUE
    conx <- dbConnect(drv = dbDriver('SQLite'), dbname = db.path)
    
    # Turn the SQLite foreign constraints on
    rs <- dbSendQuery(conn = conx, statement = "PRAGMA foreign_keys = ON;" )
    dbClearResult(rs)
    
    # Grab temporal.data
    temporal.data <- data.table(dbGetQuery(conx, "SELECT * FROM temporals"))
    
    # Add circular variables
    temporal.data <- scheduleAddVars(temporal.data)
    
    DT <- data.table(temporal.data)
    
    # Clean up/remove large objects just in case
    rm(temporal.data)
    
    # Disconnect from db when done grabbing data
    dbDisconnect(conn = conx)
    
  } else {
    # If not using db.path, use the free-standing data.frame provided by the user
    DT <- as.data.table(data)
  }
  
  # Loop through equation list to create models
  sim_models <- list()
  for (i in 1:length(equation.list)) {
    
    sp <- names(equation.list)[i]
    message('Working on model ', i, ' (', sp, ')...')
    coefs <- equation.list[[i]]$coefficients
    varnames <- equation.list[[i]]$names
    
    # Check for intercept in equation names
    check.intercept <- varnames[which(varnames == 'intercept')]
    if (length(check.intercept) == 0) {
      stop('Please include the intercept term in your equation.list names element.')
    }
    
    # Split the covariate names by . in case of polynomials
    varnames.spl <- strsplit(x = varnames, split = '.', fixed = TRUE)
    
    # Check that covariate column names are contained within DT 
    #  (leaving off any polynomial terms)
    check.names <- unique(sapply(X = varnames.spl, '[[', 1))
    check.names <- check.names[-which(check.names == 'intercept')]
    if (length(check.names) > 0 ) {
      bad.names <- paste(check.names[!(any(check.names %in% colnames(DT)))],
                         sep = '', collapse = ', ')
      if (length(check.names[!(any(check.names %in% colnames(DT)))])) {
        stop(sprintf('The following names used in the equation.list object do not exist in the columns of the dataset you provided: \n %s', bad.names))
      }
    }
    
    # Add an intercept column of 1
    mod.DT <- DT
    mod.DT$intercept <- 1
    
    # Apply polynomial variables to dataset (if any):
    with.poly <- varnames.spl[unlist(lapply(varnames.spl, length)) == 2]
    if (length(with.poly)) {
      poly.name <- sapply(with.poly, '[[' , 1)
      poly.coef <- sapply(with.poly,'[[', 2)
      for (p in 1:length(with.poly)) {
        mod.DT <- mod.DT[,paste0(poly.name[p], '.', poly.coef[p]) := 
                           get(poly.name[p])^as.numeric(poly.coef[p])]
      }
    }
    
    # Subset the data table to keep only the correctly-ordered equation names
    mod.DT <- mod.DT[ ,varnames, with = FALSE]
    
    # Remove rows containing NAs:
    problem.cols <- colnames(mod.DT)[colSums(is.na(mod.DT)) > 0]
    orig.nrow <- nrow(mod.DT)
    mod.DT <- na.omit(mod.DT)
    diff.row <- orig.nrow - nrow(mod.DT)
    
    # Check for NA values and remove problem columns
    if (diff.row > 0) {
      message('There are NA values in ', diff.row, 
              ' rows, which will prevent probability estimation. Removing ', 
              diff.row, ' rows out of ', orig.nrow,'. ',
              'Columns containing NAs: ', problem.cols)
    }
    
    # For binomial models:
    if (model.family == 'binomial') {
      
      # Calculate and assign logit values
      mod.DT$logit <- data.matrix(mod.DT) %*% coefs
      mod.DT$pr <-  1/(1 + exp(-mod.DT$logit))
      if (!is.null(seed)) set.seed(seed)
      mod.DT$y <-  rbinom(n = nrow(mod.DT), size = 1, prob = mod.DT$pr)
      theta <- mod.DT$y
    }
    
    # For poisson models:
    if (model.family == 'poisson') {

      # Calculate and assign log values
      mod.DT$log <- data.matrix(mod.DT) %*% coefs
      
      # calculate lambda from log link
      mod.DT$lambda <- exp(mod.DT$log)
      
      # Draw from rpois to model:
      if (!is.null(seed)) set.seed(seed)
      mod.DT$count <-  rpois(n = nrow(mod.DT), lambda = mod.DT$lambda)
      theta <- mod.DT$count
      
    }
    
    # Fit the glm models:
    vars.no.int <- varnames[varnames != 'intercept']
    covariates <- mod.DT[, vars.no.int, with = FALSE]
    if (nrow(covariates) == 0) {
      # If no covariates, fit intercept model
      sim_models[[i]] <- glm(formula = theta ~ 1,
                             family = model.family,
                             ...)
    }else{
      # If covariates, use them: 
      sim_models[[i]] <- glm(formula = theta ~ .,
                             data = covariates,
                             family = model.family,
                             ...)
    }
    
    # Provide summary information messages about models:
    ifelse(model.type == 'activity', first <- ' is active ',
           ifelse(model.type == 'occurrence', first <- ' occurs ',
                  ifelse(model.type == 'colonization', first <- ' colonizes ',
                         ifelse(model.type == 'extinction', first <- 'goes extinct',
                                first <- ' ... '))))
    if (model.family == 'binomial') {
      message('Finished model ', i, '. Model ',
              sp, first,
              as.numeric(round(table(theta)/sum(table(theta)),3)['1']*100),
              ' % of the time.\n')
    }
    if (model.family == 'poisson') {
      message('Finished model ', i, '. Model ',
              sp, first,
              'with average lambda of ', round(mean(theta), 2), '.\n')
    }
    
  } # end equation list loop for each species
  
  names(sim_models) <- names(equations)
  
  sim_models
}




# ggVocModel(): A function to help with plotting ==============================

ggVocModel <- function(var, plot_fodder, x_axes, x_labs, titles,
                       species_colors, species_linetypes, use_legend = FALSE) {
  x_var <- x_axes[var]
  
  # For journal:
  mytheme <- theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(color = 'black'),
                   text = element_text(family = 'Times'),
                   plot.title = element_text(hjust = 0.5, size = 13),
                   legend.text = element_text(size = 12),
                   strip.text.x = element_text(size = 12),
                   axis.text = element_text(color = 'black'))
  
  if (use_legend == FALSE) {
    mytheme <- mytheme + theme(legend.position = 'none')
  }
  
  windowsFonts(Times = windowsFont('Times New Roman'))
  
  # Check whether plot_fodder is a nested list (we are plotting figure 3) 
  # or not (we are plotting the example model portion of figure 1)
  if (any(sapply(plot_fodder, function(x) class(x) == "list"))) {
    var.df <- rbindlist(lapply(plot_fodder, '[[', var))
  } else {
    var.df <- plot_fodder[[var]]
  }
  
  var.df$speciesID <- toupper(var.df$speciesID)
  sp_colors_var <- species_colors[unique(var.df$speciesID)]
  sp_lt_var <- species_linetypes[unique(var.df$speciesID)]
  if (names(x_var) %in% c('yearly', 'moon')) { # if date
    var.g <- ggplot(data = var.df, aes_string(x = x_var, y = 'pr', color = 'speciesID')) +
      geom_line(aes(linetype = speciesID)) +
      ylim(c(0,1)) +
      scale_x_date(date_breaks = '3 month', date_labels =  '%b') +
      scale_color_manual(values = sp_colors_var) +
      scale_linetype_manual(values = sp_lt_var) +
      xlab(x_labs[names(x_var)]) +
      ylab(expression(italic(p['v']))) +
      ggtitle(titles[names(titles) == names(x_var)]) +
      theme_classic() +
      mytheme
    return(var.g)
  }
  
  if (names(x_var) == 'temp') {
    # Convert to metric
    var.df[,temperature := 5/9*(temperature - 32)]
    var.g <- ggplot(data = var.df, aes_string(x = x_var, y = 'pr', color = 'speciesID')) +
      geom_line(aes(linetype = speciesID)) +
      ylim(c(0,1)) +
      xlab(x_labs[names(x_var)]) +
      ylab(expression(italic(p['v']))) +
      scale_color_manual(values = sp_colors_var) +
      scale_linetype_manual(values = sp_lt_var) +
      ggtitle(titles[names(titles) == names(x_var)]) +
      theme_classic() +
      mytheme
    return(var.g)
  } 
  
  if (names(x_var) == 'wind') {
    # Convert to metric
    var.df[,windSpeed := windSpeed*1.60934]
    var.g <- ggplot(data = var.df, aes_string(x = x_var, y = 'pr', color = 'speciesID')) +
      geom_line(aes(linetype = speciesID)) +
      ylim(c(0,1)) +
      xlab(x_labs[names(x_var)]) +
      ylab(expression(italic(p['v']))) +
      scale_color_manual(values = sp_colors_var) +
      scale_linetype_manual(values = sp_lt_var) +
      ggtitle(titles[names(titles) == names(x_var)]) +
      theme_classic() +
      mytheme
    return(var.g)
  } 
  
  if (names(x_var) == 'rain') {
    # Convert to metric
    var.df[,rainAccum24 := rainAccum24*25.4]
    var.g <- ggplot(data = var.df, aes_string(x = x_var, y = 'pr', color = 'speciesID')) +
      geom_line(aes(linetype = speciesID)) +
      ylim(c(0,1)) +
      xlab(x_labs[names(x_var)]) +
      ylab(expression(italic(p['v']))) +
      scale_color_manual(values = sp_colors_var) +
      scale_linetype_manual(values = sp_lt_var) +
      ggtitle(titles[names(titles) == names(x_var)]) +
      theme_classic() +
      mytheme
    return(var.g)
  }
  if (names(x_var) == 'hourly') {
    var.g <- ggplot(data = var.df, aes_string(x = x_var, y = 'pr', color = 'speciesID')) +
      geom_line(aes(linetype = speciesID)) +
      ylim(c(0,1)) +
      xlab(x_labs[names(x_var)]) +
      ylab(expression(italic(p['v']))) +
      scale_color_manual(values = sp_colors_var) +
      scale_linetype_manual(values = sp_lt_var) +
      ggtitle(titles[names(titles) == names(x_var)]) +
      theme_classic() +
      mytheme
    return(var.g)
  }
}

# predictFixed(): a function to run the "fixed" experiments ===================================
predictFixed <- function(exp.name,
                         exp.folder,
                         fixed.sampling.times,
                         pr.call,
                         temporals,
                         tz,
                         voc.time.units)
{
  
  # re-order the pr.call columns to ensure speciesIDs are in alpha order:
  setcolorder(pr.call, c('locationID', 'time', sort(colnames(pr.call)[3:ncol(pr.call)])))
  
  # Gather the speciesIDs
  speciesIDs <- colnames(pr.call)[3:ncol(pr.call)]
  
  locations <- sort(unique(temporals$locationID))
  dates <- unique(temporals$date)
  
  all.hours <- as.character(strftime(x = strptime(x = pr.call$time,tz = tz,
                                                  format = "%Y-%m-%d %H:%M:%S"),
                                     tz = tz, format = '%H:%M:%S'))
  
  
  
  # Subset based only on the hours that were actually sampled under this experiment:
  sampled.hours <- pr.call[which(all.hours %in% fixed.sampling.times),]
  
  # Compute the number of samples taken in each hour
  num.samples <- table(hour(x = strptime(x = fixed.sampling.times, 
                                         format = '%H:%M:%S', tz = tz)))
  
  # Set up the fixed schedule data.table
  fixed.schedule.pc <- data.table(locationID = rep(locations, length(speciesIDs)*length(dates)),
                                  speciesID = 'tbd',
                                  pCurrent = 0,
                                  pMax = 0.95,
                                  date = rep(dates, length(speciesIDs)*length(locations)))
  setkeyv(fixed.schedule.pc, c('locationID', 'date'))
  fixed.schedule.pc[,speciesID := sort(speciesIDs)]
  
  start <- Sys.time()
  for (j in 1:length(locations)) {
    cat('location', j, '\n')
    cat('date \n')
    loc <- locations[j]
    sampled.loc <- sampled.hours[locationID == loc]
    
    for (k in 1:length(dates)) {
      
      today <- dates[k]
      tomorrow <- as.character(as.Date(dates[k]) + 1)
      fixed.loc <- fixed.schedule.pc[locationID == loc & date == today]
      sampled.today.loc <- sampled.loc[as.Date(time) == today]
      
      # Add a numeric column of the sampling hour and control for dst differences:
      sampled.today.loc[ , hr.num := hour(time)]
      num.samples.dst <- num.samples[which(as.numeric(names(num.samples)) %in%
                                             sampled.today.loc[,hr.num])] # to deal with missing March hr
      setkey(sampled.today.loc, locationID, time)
      unique.hrs <- unique(sampled.today.loc, by = key(sampled.today.loc)) # to get rid of extra November hr
      
      # Explode the matrix of probs/rates out for each minute that was sampled
      # to reflect if certain hours were sampled multiple times: 
      compute.pd.loc.explode <- unique.hrs[rep(seq(1, .N), num.samples.dst)]
      
      # In the fixed schedule, we directly pull each minute we sampled: 
      compute.pd.loc <- compute.pd.loc.explode[, ..speciesIDs] 
      
      if (voc.time.units == 'hourly') compute.pd.loc <- compute.pd.loc/60
      pd.loc <- apply(compute.pd.loc, 2, function(x) 1 - prod(1 - x, na.rm = TRUE))
      pc.loc <- 1 - (1 - fixed.loc[,pCurrent])*(1 - pd.loc)
      
      # Compute pCurrent for tomorrow at this location:
      fixed.schedule.pc[locationID == loc & date == tomorrow, pCurrent := pc.loc]
      cat(k, '\n')
    } #end looping through dates
  } # end looping through locations
  total.time <- Sys.time() - start
  results <- list(pc.table = fixed.schedule.pc, schedule = fixed.sampling.times, run.time = total.time)
  saveRDS(results, paste0(exp.folder, exp.name))    
  
}



# trapezoidRule(): a function to calculate the trapezoid rule for AUC =========
# Trapezoid rule calculation and code comes from R package pracma::trapz
# https://www.rdocumentation.org/packages/pracma/versions/1.9.9/topics/trapz
trapezoidRule <- function(x,y){
  m <- length(x)
  xp <- c(x, x[m:1])
  yp <- c(numeric(m), y[m:1])
  n <- 2 * m
  p1 <- sum(xp[1:(n - 1)] * yp[2:n]) + xp[n] * yp[1]
  p2 <- sum(xp[2:n] * yp[1:(n - 1)]) + xp[1] * yp[n]
  return(0.5 * (p1 - p2))
}


