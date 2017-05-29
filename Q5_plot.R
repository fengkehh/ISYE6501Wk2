#Q5.1 plot

#par(mfrow = c(2,2))

cols = rainbow(5, s= 1, start = 0)

#plot(temps_data[,1], temps_data[,2], col = cols[1], cex = 0, pch = '.')
line <- lowess(temps_data[,1], temps_data[,2])
plot(x = as_date(line[[1]]), y = line[[2]], type = 'l', col = cols[1], xlab = 'Time of Year', ylab = 'Temperature (F)')
for (i in 2:5) {

lines(lowess(temps_data[,1], temps_data[,i+1]), col = cols[i])
}

legend('topright', lty = 1, col = cols, legend = substr(names(temps_data)[2:(2+4)], 2, 5))


# Use the following to get plotly graphs
li <- lowess(temps_data[,1], temps_data[,c('X2015')])
plot_ly(x = as_date(li[[1]]), y = li[[2]], type = 'scatter', mode = 'markers')

# manually choose starting point of decrease
starts <- data.frame(X1996 = as.Date('2017-08-30'), 
                     X1997 = as.Date('2017-08-24'), 
                     X1998 = as.Date('2017-09-05'),
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
