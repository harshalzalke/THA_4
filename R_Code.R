library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)

#Importing Data
economic_data = read.csv("economic_freedom_2017.csv")
terrorism_data = read.csv("terrorist-incidents.csv")

#Data Preprocessing
View(economic_data)
View(terrorism_data)
#Pre-processing terrorism 2017
terrorism_2017 = subset(terrorism_data, Year == 2017)
names(terrorism_2017)[1] = "Country"
names(terrorism_2017)[4] = "Incident"
View(terrorism_2017)
#Pre-Processing economic_data
economic_f = economic_data[c(2,7)]
View(economic_f)
colnames(economic_f) = c("Country", "Score_2017")

#Merging Dataset
data = merge(economic_f, terrorism_2017, by = "Country")
data = data[c(1,2,5)]
View(data)
str(data)

#Change into numerical
data_t = data[,-1]
View(data_t)
indx = sapply(data_t, is.factor)
data_t[indx] = lapply(data_t[indx], function(x) as.numeric(as.character(x)))
View(data_t)

#Merging Fina Dataset
dataf = cbind(data[,1],data_t)
View(dataf)
names(dataf)[1] = "Country"
View(dataf)
write.csv(dataf, "dataf.csv")

plot(dataf$Incident)
plot(dataf$Score_2017)

#Plot
ggplot(dataf, aes(y = Incident, x = Score_2017)) + geom_point()

#Regression
mod = lm(dataf$Incident~dataf$Score_2017)
summary(mod)
