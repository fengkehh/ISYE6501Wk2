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
li <- lowess(temps_data[,1], temps_data[,c('X1997')])
plot_ly(x = as_date(li[[1]]), y = li[[2]], type = 'scatter', mode = 'markers')

# manually choose starting point of decrease
starts <- data.frame(X1996 = as.Date('2017-07-01'), X1997 = as.Date('2017-08-24'))
