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



adult.data$a1.score <- NULL
adult.data$a2.score <- NULL
adult.data$a3.score <- NULL
adult.data$a4.score <- NULL
adult.data$a5.score <- NULL
adult.data$a6.score <- NULL
adult.data$a7.score <- NULL
adult.data$a8.score <- NULL
adult.data$a9.score <- NULL
adult.data$a10.score <- NULL
adult.data$score.of.aq10.adult <- NULL

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
round(prop.table(table(adult.data$gender))*100, digits = 1)  # female:47.8%  male:52.2%               
round(prop.table(table(adult.data$born.with.jaundice))*100, digits = 1)  # no: 90.2%  yes: 9.8%
round(prop.table(table(adult.data$pdd.family.history))*100, digits = 1)  # no: 87.1%  yes: 12.9%
round(prop.table(table(adult.data$screened.before))*100, digits = 1)  # no: 98.3%  yes: 1.7%
round(prop.table(table(adult.data$who.completing.test))*100, digits = 1)  # Self: 74.1%
round(prop.table(table(adult.data$has.autism.correct.response))*100, digits = 1)  # NO: 73.2%  YES: 26.8%

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

adult.data$age <- as.integer(adult.data$age)

adult.data$gender <- as.factor(adult.data$gender)
levels(adult.data$gender)

names.vec <- names(adult.data)
names.vec
#gender, ethnicity, born.with.jaundice, pdd.family.history, country.of.residence, screened.before, who.completing.test, has.autism.correct.response
names.vec[c(1,2,3,4,5,6,7,8,9)]

for(i in c(1,2,3,4,5,6,7,8,9)){
  adult.data[,i] <- as.factor(adult.data[,i])
}
levels(adult.data[,3])

str(adult.data)
table(adult.data$ethnicity)

for(i in which(adult.data$ethnicity == "?")){
  adult.data$ethnicity[i]  <- 'Others'
}
adult.data$ethnicity[658]
levels(adult.data[,3])

adult.data$ethnicity
adult.data <- droplevels(adult.data)
levels(adult.data[,3])
adult.data <- droplevels(adult.data)
levels(adult.data[,3])
which(adult.data$ethnicity == "?")

levels(adult.data$who.completing.test)
#View(adult.data)

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

str(adult.data)
#View(adult.data)
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
adult.classifier = naiveBayes(adult.data.train[, 1:8], adult.data.train$has.autism.correct.response)

#test data
adult.test.predicted = predict(adult.classifier,
                               adult.data.test[, 1:8])

########## Analyzing Results ##########

#CrossTable() is from gmodels
CrossTable(adult.test.predicted,
           adult.data.test[,9],
           prop.chisq = FALSE, # as before
           prop.t     = FALSE, # eliminate cell proprtions
           dnn        = c("predicted", "actual")) # relabels rows+cols

#predicted to actual:
#(Y/Y + N/N) / (Y/Y + N/N + Y/N + N/Y)
a = (28+98)/(28+98+28+22) #0.71590
b = (21+102)/(21+102+32+21) #0.69886
c = (27+97)/(27+97+32+20) #0.704545
d = (28+98)/(28+98+25+25) #0.715909
e = (22+105)/(22+105+26+23) #0.72159
f = (18+105)/(18+105+34+19) #0.698863
g = (24+104)/(25+104+22+25) #0.7272727
h = (23+102)/(23+102+30+21) #0.710227
i = (30+102)/(30+102+23+21) #0.75
j = (29+101)/(29+101+27+19) #0.738636
avg = (a+b+c+d+e+f+g+h+i+j)/10
avg #0.7181818

# 100 - (5 / 703) = %99.9929  ---- this is the accuracy of the first train/test run


adult.classifier$apriori
adult.classifier$tables # learn how to interpret these tables

# 100 - (5 / 703) = %99.9929  ---- this is the accuracy without the a2.score