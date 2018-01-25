#gmodels package for fitting models & displaying results
library(gmodels)
#e1071 package for naive Bayes theorem & other functions 
library(e1071)
#caTools package for splitting train / test data
library(caTools)

########## Exploring the Raw Data ##########

#reads raw data from adult.data.csv into a data frame and prevents strings from becoming factors
adult.data <- read.csv('adult-data.csv', stringsAsFactors = FALSE)

#displays internal structure of adult.data
str(adult.data)
#further investigating data with a summary to see if anything else needs to be noted 
summary(adult.data)
#view data in data frame
#View(adult.data)

### Features ###

colnames(adult.data) <- c("a1.score", "a2.score", "a3.score", "a4.score", "a5.score", "a6.score", 
                          "a7.score", "a8.score", "a9.score", "a10.score", "age", "gender", "ethnicity", 
                          "born.with.jaundice", "pdd.family.history", "country.of.residence", 
                          "screened.before", "score.of.aq10.adult", "age.category", "who.completing.test", 
                          "has.autism.correct.response")



adult.data$age <- NULL
adult.data$born.with.jaundice <- NULL
adult.data$pdd.family.history <- NULL
adult.data$country.of.residence <- NULL
adult.data$screened.before <- NULL
adult.data$who.completing.test <- NULL
adult.data$age.category <- NULL

str(adult.data)


# REMOVED A1.SCORE AND ADJUSTED TOTAL SCORE
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
# score.of.aq10.adult: total score of aq10 test (a1.score to a10.score) to diagnose autism
# age.category: string of "age 18 and more" to show that the user is an adult
# who.completing.test: a string from list of roles such as self, relative, parent, health care professional, or others
# has.autism.correct.response: NO or YES; the actual diagnosis of autism for the adult

#displays updated internal structure of adult.data
str(adult.data)
# updated summary
summary(adult.data)
#view data in data frame
#View(adult.data)

### Proportion Tables of Features ###

# current length of raw data is 21 variables (columns)
length(adult.data)

########## Cleaning the Raw Data ##########

# Since all variables in the age.category is "18 and more", this data is not useful for model
adult.data$age.category <- NULL

# updated length of data is now 20 variables (columns)
length(adult.data)

### Check for missing values ("?") ###

# iterate through all the rows in age category, if there is missing data ("?") then set to NA
for (i in 1:nrow(adult.data)) {
  if (adult.data[i,1] == "?") {
    adult.data[i,1] = NA
  }
}

adult.data$has.autism.correct.response <- as.factor(adult.data$has.autism.correct.response)



str(adult.data)

########## Splitting Data into Testing & Training Sets ##########

#reorders and randomizes the rows so that I take different training and testing data sets
adult.data <- adult.data[sample(nrow(adult.data)),]
adult.data

threeFourths <- round(703*.75)
threeFourths
adult.data.train = adult.data[1:threeFourths, ] # about 75%
adult.data.test  = adult.data[(threeFourths+1):nrow(adult.data), ] # the rest

#typeof(adult.data$a1.score)
#adult.data$a1.score

round(prop.table(table(adult.data.train$has.autism.correct.response))*100)
round(prop.table(table(adult.data.test$has.autism.correct.response))*100) #they are similar


########## Using Bayes theorem ##########

# Storing model in adult.classifier
# train data
adult.classifier = naiveBayes(adult.data.train[, 1:11], adult.data.train$has.autism.correct.response)

#test data
adult.test.predicted = predict(adult.classifier,
                               adult.data.test[, 1:11])

########## Analyzing Results ##########
library(caret)

actual.outcome = adult.data.test$has.autism.correct.response
predicted.outcome = predict(adult.classifier, adult.data.test)

actual.outcome
predicted.outcome

confusionMatrix(actual.outcome,predicted.outcome)
#initial kappa value = 1
# accuracy rate: 1
# error rate: 0


#CrossTable() is from gmodels
CrossTable(adult.test.predicted,
           adult.data.test[,12],
           prop.chisq = FALSE, # as before
           prop.t     = FALSE, # eliminate cell proprtions
           dnn        = c("predicted", "actual")) # relabels rows+cols


#predicted to actual:
#(Y/Y + N/N) / (Y/Y + N/N + Y/N + N/Y)
a = (44+127)/(44+127+2+3) #0.9715909
b = (56+119)/(56+119+0+1) #0.9943181
c = (36+135)/(36+135+3+2) #0.97159
d = (48+122)/(48+122+2+4) #0.965909
e = (30+143)/(30+143+1+2) #0.98295
f = (51+117)/(51+117+4+4) #0.954545
g = (39+133)/(39+133+3+1) #0.9727272
h = (43+131)/(43+131+2+0) #0.988636
i = (43+129)/(43+129+1+3) #0.9722722
j = (50+124)/(50+124+0+2) #0.988636
avg = (a+b+c+d+e+f+g+h+i+j)/10
avg #0.9772727
