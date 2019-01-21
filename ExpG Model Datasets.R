# Script to import list of chances and minutes .csv files

# Get list of file names
chances_csv_list <- list.files(path = 'C:/Users/Ferdia/Desktop/Football Data/Strata Data/Chances/', pattern = '.csv')
mins_csv_list <- list.files(path = 'C:/Users/Ferdia/Desktop/Football Data/Strata Data/Minutes/', pattern = '.csv')

# Import list of .csv files from directory
chances_list <- lapply(paste0('C:/Users/Ferdia/Desktop/Football Data/Strata Data/Chances/',chances_csv_list), function(x) read.csv(file = x, stringsAsFactors = FALSE))
mins_list <- lapply(paste0('C:/Users/Ferdia/Desktop/Football Data/Strata Data/Minutes/',mins_csv_list), function(x) read.csv(file = x, stringsAsFactors = FALSE))

# Bind files into single data frame
chances <- do.call(rbind, chances_list)
mins <- do.call(rbind, mins_list)

# Rename ID column in mins to match chances data frame
colnames(mins)[5] <- "gsm_id"
