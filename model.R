install.packages("e1071")
install.packages("caTools")
adult.data <- read.csv('adult-data.csv', stringsAsFactors = FALSE)
plot(adult.data$gender)
library(e1071)
library(caTools)

first = c(unname(adult.data[1,])) #output is what you i sent you, and the list of 21
length(first) #output is 21 (which is correct)
is.vector(first) #output is true
first


c(1,1,1,1,1,0,0,1,1,0,0,26,"f","White-European","no","no","United States","no",6,18,"and","more","Self","NO")


c(adult.data[1,])
unname(c(adult.data[1,]))
mouse
orange <- c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,'aefa',2,3,1,'faa',3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3)
length(orange)
length(mouse)

########################
str(adult.data)
round(prop.table(table(adult.data$a1.score))*100, digits = 1)
adult.data$a1.score = factor(adult.data$a1.score)


