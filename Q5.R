# Q5.1
require('reshape2')
require('lubridate')

temps <- read.table('temps.txt', header = TRUE)

datetime <- function(day) {
    strtok <- strsplit(as.character(day), '-')
    
    date_num <- strtok[[1]][1]
    mon <- strtok[[1]][2]
    mon_num <- match(mon, month.abb)
    #year_num <- as.numeric(substr(year, 2, 5))
    
    #date <- as_date(paste(date_num, mon_num, year_num), '%d %m %Y')
    
    date <- as_date(paste(date_num, mon_num), '%d %m')

    return(date)
}

temps_data <- temps
temps_data[,1] = as_date(sapply(temps$DAY, FUN = datetime))

#temps_long <- melt(temps)

#datums <- mapply(FUN = datetime, temps_long$DAY, temps_long$variable)

#temps_long <- data.frame(datum = as_date(datums), T = temps_long$value)