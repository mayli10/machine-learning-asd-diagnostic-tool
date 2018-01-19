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
for (j in 1:8) {
  for (i in 1:nrow(adult.data)) {
    if (adult.data[i,j] == 1) {
      adult.data[i,j] = 'done'
      adult.data[i,18] = adult.data[i,18] - 1
    }
  }
}
str(adult.data)


adult.data$a1.score <- NULL
adult.data$a2.score <- NULL
adult.data$a3.score <- NULL
adult.data$a4.score <- NULL
adult.data$a5.score <- NULL
adult.data$a6.score <- NULL
adult.data$a7.score <- NULL
adult.data$a8.score <- NULL
adult.data$pdd.family.history <- NULL
adult.data$born.with.jaundice <- NULL
adult.data$ethnicity <- NULL
adult.data$age <- NULL
adult.data$gender <- NULL
adult.data$country.of.residence <- NULL
adult.data$screened.before <- NULL
adult.data$age.category <- NULL
adult.data$who.completing.test <- NULL

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
round(prop.table(table(adult.data$has.autism.correct.response))*100, digits = 1)  # NO: 73.2%  YES: 26.8%

# current length of raw data is 21 variables (columns)
length(adult.data)

########## Cleaning the Raw Data ##########

# Since all variables in the age.category is "18 and more", this data is not useful for model

# updated length of data is now 20 variables (columns)
length(adult.data)
#View(adult.data)
### Check for missing values ("?") ###

# iterate through all the rows in age category, if there is missing data ("?") then set to NA
for (i in 1:nrow(adult.data)) {
  if (adult.data[i,3] == "?") {
    adult.data[i,3] = NA
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
adult.classifier = naiveBayes(adult.data.train[, 1:3], adult.data.train$has.autism.correct.response)

#test data
adult.test.predicted = predict(adult.classifier,
                               adult.data.test[, 1:3])

########## Analyzing Results ##########

#CrossTable() is from gmodels
CrossTable(adult.test.predicted,
           adult.data.test[,4],
           prop.chisq = FALSE, # as before
           prop.t     = FALSE, # eliminate cell proprtions
           dnn        = c("predicted", "actual")) # relabels rows+cols

#predicted to actual:
#(Y/Y + N/N) / (Y/Y + N/N + Y/N + N/Y)
a = (39+117)/(39+117+9+11) 
b = (47+114)/(47+114+9+6) 
c = (33+118)/(33+118+12+13) 
d = (33+116)/(33+116+15+12) 
e = (33+114)/(33+114+16+13) 
f = (30+117)/(30+117+12+17) 
g = (37+119)/(37+119+6+14) 
h = (27+121)/(27+121+11+17) 
i = (40+108)/(40+108+12+16) 
j = (31+116)/(31+116+12+17) 
c(a, b, c, d, e, f, g, h, i, j)
#0.8863636 0.9147727 0.8579545 0.8465909 0.8352273 0.8352273 0.8863636 0.8409091 0.8409091 0.8352273
avg = (a+b+c+d+e+f+g+h+i+j)/10
avg #0.8579545