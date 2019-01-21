# Script to import list of chances and minutes .csv files

library(data.table)

# Get list of file names
chances_csv_list <- list.files(path = 'C:/Users/Ferdia/Desktop/Football Data/Strata Data/Chances/', pattern = '.csv')
mins_csv_list <- list.files(path = 'C:/Users/Ferdia/Desktop/Football Data/Strata Data/Minutes/', pattern = '.csv')

# Import list of .csv files from directory
chances_list <- lapply(paste0('C:/Users/Ferdia/Desktop/Football Data/Strata Data/Chances/',chances_csv_list), function(x) fread(file = x, drop = 1, stringsAsFactors = FALSE, data.table = FALSE))
mins_list <- lapply(paste0('C:/Users/Ferdia/Desktop/Football Data/Strata Data/Minutes/',mins_csv_list), function(x) fread(file = x, drop = 1, stringsAsFactors = FALSE, data.table = FALSE))

# Bind files into single data frame
chances <- do.call(rbind, chances_list)
mins <- do.call(rbind, mins_list)

# Rename ID column in mins to match chances data frame
colnames(mins)[5] <- "gsm_id"
