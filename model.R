install.packages("e1071")
install.packages("caTools")
adult.data <- read.csv('adult-data.csv', stringsAsFactors = FALSE)
plot(adult.data$age)
library(e1071)
library(caTools)

first = unlist(adult.data[1,])
length(first) 
is.vector(first) 
first

########################
str(adult.data)
round(prop.table(table(adult.data$a1.score))*100, digits = 1)
adult.data$a1.score = factor(adult.data$a1.score)

length(adult.data)

for (i in 1:10) {
  round(prop.table(table(adult.data[i,]))*100, digits = 1)
}

####


for(i in 1:11){
  adult.data[,i] <- as.integer(adult.data[,i])
}

adult.data$gender <- as.factor(adult.data$gender)
levels(adult.data$gender)

adult.data$gender <- as.factor(adult.data$gender)
adult.data$age.category <- NULL

names.vec <- names(adult.data)
names.vec
names.vec[c(12,13,14,15,16,17,19,20)]

for(i in c(12,13,14,15,16,17,19,20)){
  adult.data[,i] <- as.factor(adult.data[,i])
}
levels(adult.data[,13])

str(adult.data)
table(adult.data$ethnicity)

for(i in which(adult.data$ethnicity == "?")){
  adult.data$ethnicity[i]  <- 'Others'
}
adult.data$ethnicity[658]
levels(adult.data[,13])

adult.data$ethnicity
adult.data <- droplevels(adult.data)
levels(adult.data[,13])
adult.data <- droplevels(adult.data)
levels(adult.data[,13])
which(adult.data$ethnicity == "?")

View(adult.data)

levels(adult.data$who.completing.test)
table(adult.data$who.completing.test)

for(i in which(adult.data$who.completing.test == "?")) {
  adult.data$who.completing.test[i] <- NA
}

levels(adult.data$who.completing.test)
adult.data <- droplevels(adult.data)
levels(adult.data$who.completing.test)

for(i in which(adult.data$who.completing.test == "?")) {
  adult.data$who.completing.test[i] <- NA
}

levels(adult.data$who.completing.test)
adult.data <- droplevels(adult.data)
levels(adult.data$who.completing.test)

levels(adult.data$born.with.jaundice)
levels(adult.data$pdd.family.history)

## DATA  IS CLEANED



