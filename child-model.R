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

#reads raw data from child.data.csv into a data frame and prevents strings from becoming factors
child.data <- read.csv('child-data.csv', stringsAsFactors = FALSE)

#displays internal structure of child.data
str(child.data)
#further investigating data with a summary to see if anything else needs to be noted 
summary(child.data)
#view data in data frame
#View(child.data)

### Features ###

colnames(child.data) <- c("a1.score", "a2.score", "a3.score", "a4.score", "a5.score", "a6.score", 
                           "a7.score", "a8.score", "a9.score", "a10.score", "age", "gender", "ethnicity", 
                           "born.with.jaundice", "pdd.family.history", "country.of.residence", 
                           "screened.before", "score.of.aq10.child", "age.category", "who.completing.test", 
                           "has.autism.correct.response")

# a1.score: "S/he often notices small sounds when others do not"; 1 for yes, 0 for no
# a2.score: "S/he usually concentrates more on the whole picture, rather than the small details"; 1 for yes, 0 for no
# a3.score: "In a social group, s/he can easily keep track of several different people’s conversations"; 1 for yes, 0 for no
# a4.score: "S/he finds it easy to go back and forth between different activities"; 1 for yes, 0 for no
# a5.score: "S/he doesn’t know how to keep a conversation going with his/her peers"; 1 for yes, 0 for no
# a6.score: "S/he is good at social chit-chat"; 1 for yes, 0 for no
# a7.score: "When s/he is read a story, s/he finds it difficult to work out the character’s intentions or feelings"; 1 for yes, 0 for no
# a8.score: "When s/he was in preschool, s/he used to enjoy playing games involving pretending with other children"; 1 for yes, 0 for no
# a9.score: "S/he finds it easy to work out what someone is thinking or feeling just by looking at their face"; 
# 1 for yes, 0 for no
# a10.score: "S/he finds it hard to make new friends"; 1 for yes, 0 for no
# age: an integer value for number of years
# gender: a string of male or female
# ethnicity: a string from list of common ethnicities
# born.with.jaundice: a string of yes or no
# pdd.family.history: checks if user's family has history of PDD; a string of yes or no
# country.of.residence: a string from list of countries
# screened.before: checks if user has been screened for Autism before; a string of yes or no
# score.of.aq10.child: total score of aq10 test (a1.score to a10.score) to diagnose autism
# age.category: string of "age 18 and more" to show that the user is an adult
# who.completing.test: a string from list of roles such as self, relative, parent, health care professional, or others
# has.autism.correct.response: NO or YES; the actual diagnosis of autism for the adult

#displays updated internal structure of child.data
str(child.data)
# updated summary
summary(child.data)
#view data in data frame
#View(child.data)

### Proportion Tables of Features ###
round(prop.table(table(child.data$gender))*100, digits = 1)                
round(prop.table(table(child.data$born.with.jaundice))*100, digits = 1)  
round(prop.table(table(child.data$pdd.family.history))*100, digits = 1)  
round(prop.table(table(child.data$screened.before))*100, digits = 1) 
round(prop.table(table(child.data$score.of.aq10.child))*100, digits = 1)  
round(prop.table(table(child.data$who.completing.test))*100, digits = 1)  
round(prop.table(table(child.data$has.autism.correct.response))*100, digits = 1)  

# current length of raw data is 21 variables (columns)
length(child.data)

########## Cleaning the Raw Data ##########

# Since all variables in the age.category is "18 and more", this data is not useful for model
child.data$age.category <- NULL

# updated length of data is now 20 variables (columns)
length(child.data)

### Check for missing values ("?") ###

# iterate through all the rows in age category, if there is missing data ("?") then set to NA
for (i in 1:nrow(child.data)) {
  if (child.data[i,11] == "?") {
    child.data[i,11] = NA
  }
}

#if there is a "?" in ethnicity, put the value into Others category
for(i in which(child.data$ethnicity == "?")){
  child.data$ethnicity[i]  <- 'Others'
}
which(child.data$ethnicity == "?")
str(child.data)

#if there is a "?" in who.completing.test, set the value to be NA
for(i in which(child.data$who.completing.test == "?")){
  child.data$who.completing.test[i]  <- NA
}
which(child.data$ethnicity == "?")
str(child.data)

#check if there are any ? left in the data
which(child.data == "?")

### Setting factors and integers ### 

#set the age value to be an integer
child.data$age <- as.integer(child.data$age)

levels(child.data$gender)

#get the non-numeric features
names.vec <- names(child.data)

#gender, ethnicity, born.with.jaundice, pdd.family.history, country.of.residence, screened.before, who.completing.test, has.autism.correct.response
names <- names.vec[c(12,13,14,15,16,17,19,20)]

#set the non-numeric features as factors
for(i in c(12,13,14,15,16,17,19,20)){
  child.data[,i] <- as.factor(child.data[,i])
}
#check the data to see that this has updated
levels(child.data)
str(child.data)

########## Splitting Data into Testing & Training Sets ##########

#reorders and randomizes the rows so that I take different training and testing data sets
child.data <- child.data[sample(nrow(child.data)),]
child.data

#train the model on 75% of data and test the model on 25% of the data
threeFourths <- round(291*.75)
threeFourths
child.data.train = child.data[1:threeFourths, ] # about 75%
child.data.test  = child.data[(threeFourths+1):nrow(child.data), ] # the rest

#check and see if the number of test and train data are similar for both
round(prop.table(table(child.data.train$has.autism.correct.response))*100)
round(prop.table(table(child.data.test$has.autism.correct.response))*100) #yes, they are similar


########## Using Bayes theorem ##########

# Storing model in child.classifier
# train data using Bayes theorem which is p(A|B) = (p(B|A)*p(A)) / p(B)
#View(child.data)
child.classifier = naiveBayes(child.data.train[, 1:19], child.data.train$has.autism.correct.response)

library(caret)

actual.outcome = child.data.test$has.autism.correct.response
predicted.outcome = predict(child.classifier, child.data.test)

actual.outcome
predicted.outcome

confusionMatrix(actual.outcome,predicted.outcome)

#test data with the predict function using the trained data
child.test.predicted = predict(child.classifier,
                                child.data.test[, 1:19])

########## Analyzing Results ##########

#CrossTable() is from gmodels, displays results
CrossTable(child.test.predicted,
           child.data.test[,20],
           prop.chisq = FALSE, # as before
           prop.t     = FALSE, # eliminate cell proprtions
           dnn        = c("predicted", "actual")) # relabels rows+cols

##### More information on individual features #####
child.classifier$apriori
child.classifier$tables 
