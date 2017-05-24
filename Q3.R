# Q3
crime_data <- read.table('uscrime.txt', header = TRUE)

grubbs.test(crime_data$Crime)

lowest <- crime_data[which.min(crime_data$Crime),]
