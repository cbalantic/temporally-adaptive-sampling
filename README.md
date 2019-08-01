This Github repository exists to support reproducibility of analyses from the following paper:

Balantic, C. M., & Donovan, T. M. (2019). Temporally adaptive acoustic sampling to maximize detection across a suite of focal wildlife species" (accepted at Ecology and Evolution). 

Github is not designed for file storage and some of the necessary files accompanying this paper exceed Github's upload limits. Instead, you can acquire accompanying files in the temporally-adaptive-sampling.zip available on Zenodo at https://zenodo.org/record/3354623 (DOI: 10.5281/zenodo.3354623). The SQLite databases are required to run the code. Note that you can reproduce the .RDS files in the "ammls" and "Results" folders by running the scripts, but the .RDS files are provided in case you wish to skip this step and prefer to focus on digging into the code itself. 

Below is a guide to the files contained in the temporally-adaptive-sampling.zip folder on Zenodo: 

**Content List:**

*ammls* [Folder]

   * vocalization-models.RDS
   
*Results*  [Folder]

   * Full_Year [Folder]
   
       * fixed-2-samples.RDS
       * fixed-5-samples.RDS
       * fixed-10-samples.RDS
       * fixed-20-samples.RDS
       * fixed-30-samples.RDS
       * fixed-40-samples.RDS
       * optim-2-samples-all-placed-in-top-hour.RDS 
       * optim-5-samples-all-placed-in-top-hour.RDS
       * optim-10-samples-all-placed-in-top-hour.RDS
       * optim-20-samples-all-placed-in-top-hour.RDS
       * optim-30-samples-all-placed-in-top-hour.RDS
       * optim-40-samples-all-placed-in-top-hour.RDS
       * pc-and-compare-dates.RDS
       
   * March_Only [Folder]
   
       * fixed-2-samples.RDS
       * fixed-5-samples.RDS
       * fixed-10-samples.RDS
       * fixed-20-samples.RDS
       * fixed-30-samples.RDS
       * fixed-40-samples.RDS
       * optim-2-samples-all-placed-in-top-hour.RDS 
       * optim-5-samples-all-placed-in-top-hour.RDS
       * optim-10-samples-all-placed-in-top-hour.RDS
       * optim-20-max-per-hour-10.RDS
       * optim-20-samples-all-placed-in-top-hour.RDS
       * optim-30-max-per-hour-10.RDS
       * optim-30-samples-all-placed-in-top-hour.RDS
       * optim-40-max-per-hour-10.RDS
       * optim-40-samples-all-placed-in-top-hour.RDS
       * pc-and-compare-dates.RDS
       * pc-and-compare-dates-simple-vs-maxhr.RDS

   * experiment-appendix.R
   * experiment-full-year.R
   * experiment-full-year.sqlite
   * experiment-march.sqlite
   * experiment-march-only.R
   * functions.R
   * models.R
   * plots.R
   * sampled-temporals.RDS


**Content Description:** 

*ammls* [Folder] - Folder containing an AMModels library RDS file

   * vocalization-models.RDS - AMModels library RDS file containing vocalization models for all 9 focal species in the paper
   
*Results*  [Folder] 

   * Full_Year [Folder] - Folder containing RDS files of all factors in the Full Year experiment
   
       * fixed-2-samples.RDS - Results from the Fixed Schedule, 2 samples
       * fixed-5-samples.RDS - Results from the Fixed Schedule, 5 samples
       * fixed-10-samples.RDS - Results from the Fixed Schedule, 10 samples
       * fixed-20-samples.RDS - Results from the Fixed Schedule, 20 samples
       * fixed-30-samples.RDS - Results from the Fixed Schedule, 30 samples
       * fixed-40-samples.RDS - Results from the Fixed Schedule, 40 samples
       * optim-2-samples-all-placed-in-top-hour.RDS - Results from the Optimized Schedule, 2 samples
       * optim-5-samples-all-placed-in-top-hour.RDS - Results from the Optimized Schedule, 5 samples
       * optim-10-samples-all-placed-in-top-hour.RDS - Results from the Optimized Schedule, 10 samples
       * optim-20-samples-all-placed-in-top-hour.RDS - Results from the Optimized Schedule, 20 samples
       * optim-30-samples-all-placed-in-top-hour.RDS - Results from the Optimized Schedule, 30 samples
       * optim-40-samples-all-placed-in-top-hour.RDS - Results from the Optimized Schedule, 40 samples
       * pc-and-compare-dates.RDS - An RDS file comparing the cumulative probability of capture  (pc) and date of achievement of pmax for the fixed schedule vs. the optimized schedule
       
   * March_Only [Folder]
   
       * fixed-2-samples.RDS - Results from the Fixed Schedule, 2 samples
       * fixed-5-samples.RDS - Results from the Fixed Schedule, 5 samples
       * fixed-10-samples.RDS - Results from the Fixed Schedule, 10 samples
       * fixed-20-samples.RDS - Results from the Fixed Schedule, 20 samples
       * fixed-30-samples.RDS - Results from the Fixed Schedule, 30 samples
       * fixed-40-samples.RDS - Results from the Fixed Schedule, 40 samples
       * optim-2-samples-all-placed-in-top-hour.RDS - Results from the Optimized Schedule, 2 samples
       * optim-5-samples-all-placed-in-top-hour.RDS - Results from the Optimized Schedule, 5 samples
       * optim-10-samples-all-placed-in-top-hour.RDS - Results from the Optimized Schedule, 10 samples
       * optim-20-max-per-hour-10.RDS - Results from the 'max per hour' appendix experiment, 20 samples
       * optim-20-samples-all-placed-in-top-hour.RDS - Results from the Optimized Schedule, 20 samples
       * optim-30-max-per-hour-10.RDS - Results from the 'max per hour' appendix experiment, 30 samples
       * optim-30-samples-all-placed-in-top-hour.RDS - Results from the Optimized Schedule, 30 samples
       * optim-40-max-per-hour-10.RDS - Results from the 'max per hour' appendix experiment, 40 samples
       * optim-40-samples-all-placed-in-top-hour.RDS - Results from the Optimized Schedule, 40 samples
       * pc-and-compare-dates.RDS - An RDS file comparing the cumulative probability of capture  (pc) and date of achievement of pmax for the fixed schedule vs. the optimized schedule
       * pc-and-compare-dates-simple-vs-maxhr.RDS - An RDS file comparing the cumulative probability of capture  (p*c) and date of achievement of p*max for the fixed schedule vs. optimized schedule vs. the schedule with a cap on the maximum number of samples that can be taken in any single hour (appendix experiment). 
   * experiment-appendix.R - code for reproducing the experiment detailed in the appendix
   * experiment-full-year.R - code for reproducing the full year experiment
   * experiment-full-year.sqlite - SQLite database for reproducing the full year experiment
   * experiment-march.sqlite - code for reproducing the March experiment
   * experiment-march-only.R - SQLite database for reproducing the March experiment
   * functions.R - all AMMonitor functions and additional functions necessary to reproduce the results 
   * models.R - code for reproducing the vocalization models found in ammls/vocalization-models.RDS
   * plots.R - code for reproducing plots and tables from the paper
   * sampled-temporals.RDS - an RDS file containing the temporal and weather data necessary to use models.R to reproduce the models in ammls/vocalization-models.RDS
