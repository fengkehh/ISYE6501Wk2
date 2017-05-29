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

# manually choose starting point of decrease
starts <- data.frame(X1996 = as.Date('2017-08-30'), 
                     X1997 = as.Date('2017-08-24'), 
                     X1998 = as.Date('2017-07-03'),
                     X1999 = as.Date('2017-08-19'),
                     X2000 = as.Date('2017-08-09'),
                     X2001 = as.Date('2017-08-13'),
                     X2002 = as.Date('2017-08-13'),
                     X2003 = as.Date('2017-08-12'),
                     X2004 = as.Date('2017-08-14'),
                     X2005 = as.Date('2017-09-07'),
                     X2006 = as.Date('2017-08-11'),
                     X2007 = as.Date('2017-08-18'),
                     X2008 = as.Date('2017-08-14'),
                     X2009 = as.Date('2017-08-11'),
                     X2010 = as.Date('2017-08-12'),
                     X2011 = as.Date('2017-08-11'),
                     X2012 = as.Date('2017-07-03'),
                     X2013 = as.Date('2017-08-21'),
                     X2014 = as.Date('2017-08-21'),
                     X2015 = as.Date('2017-08-11'))


temps_data <- temps
temps_data[,1] = as_date(sapply(temps$DAY, FUN = datetime))

pred <- cusum_searchL(temps_data, starts, c(0,5), c(0.1,20), 100)

indices <- rep(0, ncol(temps_data) - 1)
for (i in 2:ncol(temps_data)) {
    indices[i-1] <- cusum_L(temps_data[,i], 0, 8.54)
}

pred <- list(indices = indices)

cols = rainbow(5, s= 1, start = 0)

line <- lowess(temps_data[,1], temps_data[,2])
plot(x = as_date(line[[1]]), y = line[[2]], type = 'l', col = cols[1], 
     xlab = 'Time of Year', ylab = 'Temperature (F)')
pt_ind <- temps_data$DAY == starts[1,1]
points(temps_data[pt_ind,1], line[[2]][pt_ind], pch = 'O', col = cols[1])
#
points(temps_data[pred$indices[1], 1], line[[2]][pred$indices[1]], pch = 'X', col = cols[1])
for (i in 2:5) {
    line <- lowess(temps_data[,1], temps_data[,i+1])
    lines(line, col = cols[i])
    pt_ind <- temps_data$DAY == starts[1,i]
    points(temps_data[pt_ind,1], line[[2]][pt_ind], pch = 'O', col = cols[i])
    #
    points(temps_data[pred$indices[i], 1], line[[2]][pred$indices[i]], pch = 'X', col = cols[i])
}

legend('topright', lty = 1, col = cols, 
       legend = substr(names(temps_data)[2:(2+4)], 2, 5))






# Use the following to get plotly graphs
# li <- lowess(temps_data[,1], temps_data[,c('X2005')])
# plot_ly(x = as_date(li[[1]]), y = li[[2]], type = 'scatter', mode = 'markers')
