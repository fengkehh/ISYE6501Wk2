# Q3
crime_data <- read.table('uscrime.txt', header = TRUE)

test <- grubbs.test(crime_data$Crime, opposite = TRUE)

n <- nrow(crime_data)
alpha <- 0.05

t <- qt(p = alpha/(2*n), df = n-2)

lowest <- crime_data[which.min(crime_data$Crime),]
