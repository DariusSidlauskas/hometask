# cleaning memory for any leftover variables
rm(list = ls())

library(data.table)
library(ggplot2)
library(forecast)

# reading raw data
augaliniai <- fread("C:/Users/ezys0/Desktop/LG/data/Total-Augaliniai.csv", header = TRUE) 
kkuras     <- fread("C:/Users/ezys0/Desktop/LG/data/Total-Kietasiskuras.csv", header = TRUE) 
maistas    <- fread("C:/Users/ezys0/Desktop/LG/data/Total-Maistoproduktai.csv", header = TRUE) 
mediena    <- fread("C:/Users/ezys0/Desktop/LG/data/Total-Mediena.csv", header = TRUE) 
nafta      <- fread("C:/Users/ezys0/Desktop/LG/data/Total-Nafta.csv", header = TRUE) 


# join data into one data set
total <- cbind(augaliniai[,2:3],kkuras[,3],maistas[,3],mediena[,3],nafta[,3])
setnames(total,c("date", "augaliniai","kkuras","maistas","mediena","nafta")  )
total <- total[order(date)]
total$date <- as.Date(total$date, format="%Y-%m-%d")


# split data into test and train
train <- total[date<"2018-01-01"]
test  <- total[date>="2018-01-01"]

# check data
acf(train[,2],lag.max=60)
pacf(train[,2],lag.max=60)


# training models
taugaliniai <- auto.arima(train[,2])
tkkuras     <- auto.arima(train[,3])
tmaistas    <- auto.arima(train[,4])
tmediena   <- auto.arima(train[,5])
tnafta      <- auto.arima(train[,6])


# predicting
paugaliniai  <- forecast(taugaliniai, h = 12)
pkkuras      <- forecast(tkkuras,  h = 12)
pmaistas     <- forecast(tmaistas, h = 12)
pmediena     <- forecast(tmediena, h = 12)
pnafta       <- forecast(tnafta,   h = 12)


# compare
ttest <- test
ttest$paugaliniai <- paugaliniai$mean
ttest$pkkuras     <- pkkuras$mean
ttest$pmaistas    <- pmaistas$mean
ttest$pmediena    <- pmediena$mean
ttest$pnafta      <- pnafta$mean

error_check <- ttest[,list(augalai = (augaliniai-paugaliniai)/augaliniai , kuras= (kkuras - pkkuras)/kkuras, maistas = (maistas - pmaistas)/maistas, mediena = (mediena-pmediena)/mediena , nafta = (nafta-pnafta)/nafta  )  ]
error_check$date <- ttest$date




# preict for 2019 training on full data

taugaliniaif <- auto.arima(total[,2])
tkkurasf     <- auto.arima(total[,3])
tmaistasf    <- auto.arima(total[,4])
tmedienaf   <- auto.arima(total[,5])
tnaftaf      <- auto.arima(total[,6])

paugaliniaif  <- forecast(taugaliniaif, h = 12)
pkkurasf      <- forecast(tkkurasf,  h = 12)
pmaistasf     <- forecast(tmaistasf, h = 12)
pmedienaf     <- forecast(tmedienaf, h = 12)
pnaftaf       <- forecast(tnaftaf,   h = 12)

# create a final data set and import upper,mean and lower values for each category
final <- c()
final$date <- ttest$date +365


final$augaliniai_mean  <- paugaliniaif$mean
final$augaliniai_lower <- paugaliniaif$lower
final$augaliniai_upper <- paugaliniaif$upper


final$kietaskuras_mean  <- pkkurasf$mean
final$kietaskuras_lower <- pkkurasf$lower
final$kietaskuras_upper <- pkkurasf$upper


final$maistas_mean  <- pmaistasf$mean
final$maistas_lower <- pmaistasf$lower
final$maistas_upper <- pmaistasf$upper

final$mediena_mean  <- pmedienaf$mean
final$mediena_lower <- pmedienaf$lower
final$mediena_upper <- pmedienaf$upper

final$nafta_mean  <- pnaftaf$mean
final$nafta_lower <- pnaftaf$lower
final$nafta_upper <- pnaftaf$upper




write.csv(ttest,"C:/Users/ezys0/Desktop/LG/data/trainingtest.csv",row.names = FALSE)

write.csv(error_check,"C:/Users/ezys0/Desktop/LG/data/error_check.csv",row.names = FALSE)

write.csv(final,"C:/Users/ezys0/Desktop/LG/data/final.csv",row.names = FALSE)





