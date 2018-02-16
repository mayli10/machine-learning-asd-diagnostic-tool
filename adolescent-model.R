install.packages("e1071")
install.packages("caTools")
install.packages("gmodels")

#gmodels package for fitting models & displaying results
library(gmodels)
#e1071 package for naive Bayes theorem & other functions 
library(e1071)
#caTools package for splitting train / test data
library(caTools)

########## Exploring the Raw Data ##########

#reads raw data from adoles.data.csv into a data frame and prevents strings from becoming factors
adoles.data <- read.csv('adolescent-data.csv', stringsAsFactors = FALSE)

#displays internal structure of adoles.data
str(adoles.data)
#further investigating data with a summary to see if anything else needs to be noted 
summary(adoles.data)
#view data in data frame
#View(adoles.data)

### Features ###

colnames(adoles.data) <- c("a1.score", "a2.score", "a3.score", "a4.score", "a5.score", "a6.score", 
                          "a7.score", "a8.score", "a9.score", "a10.score", "age", "gender", "ethnicity", 
                          "born.with.jaundice", "pdd.family.history", "country.of.residence", 
                          "screened.before", "score.of.aq10.adoles", "age.category", "who.completing.test", 
                          "has.autism.correct.response")

# a1.score: "I often notice small sounds when others do not"; 1 for yes, 0 for no
# a2.score: "I usually concentrate more on the whole picture, rather than the small details"; 1 for yes, 0 for no
# a3.score: "I find it easy to do more than one thing at once"; 1 for yes, 0 for no
# a4.score: "If there is an interruption, I can switch back to what I was doing very quickly"; 1 for yes, 0 for no
# a5.score: "I find it easy to ‘read between the lines’ when someone is talking to me"; 1 for yes, 0 for no
# a6.score: "I know how to tell if someone listening to me is getting bored"; 1 for yes, 0 for no
# a7.score: "When I’m reading a story I find it difficult to work out the characters’ intentions"; 1 for yes, 0 for no
# a8.score: "I like to collect information about categories of things (e.g. types of car, types of bird, types of train, 
# types of plant etc)"; 1 for yes, 0 for no
# a9.score: "I find it easy to work out what someone is thinking or feeling just by looking at their face"; 
# 1 for yes, 0 for no
# a10.score: "I find it difficult to work out people’s intentions"; 1 for yes, 0 for no
# age: an integer value for number of years
# gender: a string of male or female
# ethnicity: a string from list of common ethnicities
# born.with.jaundice: a string of yes or no
# pdd.family.history: checks if user's family has history of PDD; a string of yes or no
# country.of.residence: a string from list of countries
# screened.before: checks if user has been screened for Autism before; a string of yes or no
# score.of.aq10.adoles: total score of aq10 test (a1.score to a10.score) to diagnose autism
# age.category: string of "age 18 and more" to show that the user is an adult
# who.completing.test: a string from list of roles such as self, relative, parent, health care professional, or others
# has.autism.correct.response: NO or YES; the actual diagnosis of autism for the adult

#displays updated internal structure of adoles.data
str(adoles.data)
# updated summary
summary(adoles.data)
#view data in data frame
#View(adoles.data)

### Proportion Tables of Features ###
round(prop.table(table(adoles.data$gender))*100, digits = 1)                
round(prop.table(table(adoles.data$born.with.jaundice))*100, digits = 1)  
round(prop.table(table(adoles.data$pdd.family.history))*100, digits = 1)  
round(prop.table(table(adoles.data$screened.before))*100, digits = 1) 
round(prop.table(table(adoles.data$score.of.aq10.adoles))*100, digits = 1)  
round(prop.table(table(adoles.data$who.completing.test))*100, digits = 1)  
round(prop.table(table(adoles.data$has.autism.correct.response))*100, digits = 1)  

# current length of raw data is 21 variables (columns)
length(adoles.data)

########## Cleaning the Raw Data ##########

# Since all variables in the age.category is "18 and more", this data is not useful for model
adoles.data$age.category <- NULL

# updated length of data is now 20 variables (columns)
length(adoles.data)

### Check for missing values ("?") ###

# iterate through all the rows in age category, if there is missing data ("?") then set to NA
for (i in 1:nrow(adoles.data)) {
  if (adoles.data[i,11] == "?") {
    adoles.data[i,11] = NA
  }
}

#if there is a "?" in ethnicity, put the value into Others category
for(i in which(adoles.data$ethnicity == "?")){
  adoles.data$ethnicity[i]  <- 'Others'
}
which(adoles.data$ethnicity == "?")
str(adoles.data)

#if there is a "?" in who.completing.test, set the value to be NA
for(i in which(adoles.data$who.completing.test == "?")){
  adoles.data$who.completing.test[i]  <- NA
}
which(adoles.data$ethnicity == "?")
str(adoles.data)

#check if there are any ? left in the data
which(adoles.data == "?")

### Setting factors and integers ### 

#set the age value to be an integer
adoles.data$age <- as.integer(adoles.data$age)

levels(adoles.data$gender)

#get the non-numeric features
names.vec <- names(adoles.data)

#gender, ethnicity, born.with.jaundice, pdd.family.history, country.of.residence, screened.before, who.completing.test, has.autism.correct.response
names <- names.vec[c(12,13,14,15,16,17,19,20)]

#set the non-numeric features as factors
for(i in c(12,13,14,15,16,17,19,20)){
  adoles.data[,i] <- as.factor(adoles.data[,i])
}
#check the data to see that this has updated
levels(adoles.data)
str(adoles.data)

########## Splitting Data into Testing & Training Sets ##########

#reorders and randomizes the rows so that I take different training and testing data sets
adoles.data <- adoles.data[sample(nrow(adoles.data)),]
adoles.data

#train the model on 75% of data and test the model on 25% of the data
threeFourths <- round(103*.75)
threeFourths
adoles.data.train = adoles.data[1:threeFourths, ] # about 75%
adoles.data.test  = adoles.data[(threeFourths+1):nrow(adoles.data), ] # the rest

#check and see if the number of test and train data are similar for both
round(prop.table(table(adoles.data.train$has.autism.correct.response))*100)
round(prop.table(table(adoles.data.test$has.autism.correct.response))*100) #yes, they are similar


########## Using Bayes theorem ##########

# Storing model in adoles.classifier
# train data using Bayes theorem which is p(A|B) = (p(B|A)*p(A)) / p(B)
#View(adoles.data)
adoles.classifier = naiveBayes(adoles.data.train[, 1:19], adoles.data.train$has.autism.correct.response)

library(caret)

actual.outcome = adoles.data.test$has.autism.correct.response
predicted.outcome = predict(adoles.classifier, adoles.data.test)

actual.outcome
predicted.outcome

confusionMatrix(actual.outcome,predicted.outcome)

#test data with the predict function using the trained data
adoles.test.predicted = predict(adoles.classifier,
                               adoles.data.test[, 1:19])

########## Analyzing Results ##########

#CrossTable() is from gmodels, displays results
CrossTable(adoles.test.predicted,
           adoles.data.test[,20],
           prop.chisq = FALSE, # as before
           prop.t     = FALSE, # eliminate cell proprtions
           dnn        = c("predicted", "actual")) # relabels rows+cols

##### More information on individual features #####
adoles.classifier$apriori
adoles.classifier$tables 
