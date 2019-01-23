library("readr")
# Import Data Set ; Titanic
TitanicData <- read_csv("F:/ACADGILD - Online Course/1. DATA SETS/titanic3.csv")
View(TitanicData)
str(TitanicData)

psych::describe(TitanicData)

colnames(TitanicData) <- c("Pclass","Survived","Name","Sex","Age","SibSp","Parch","Ticket","Fare",
                           "Cabin","Embarked")

TitanicData <- TitanicData[,-13]

#-----------------------------------------------------------------------------------------------------------------

#a.  Preprocess the passenger names to come up with a list of titles 
# that represent families and 
# represent using appropriate visualization graph

# Convert Name as character
TitanicData$Name <- as.character(TitanicData$Name)

# Extract the title from passenger names
TitanicData$SubTitle <- gsub("\\..*", "", TitanicData$Name)
TitanicData$Title <- gsub(".*\\ ", "", TitanicData$SubTitle)

table(TitanicData$Title)  # Count of Titles

# Plot a bar-graph showing Number of Passengers by Title

Title <- barplot(table(TitanicData$Title),
                 main = "No. of Passangers by Title", xlab = "Title", 
                 ylab = "No. of Passangers", col = "Blue")
text(Title, 0,table(TitanicData$Title), pos = 3, srt = 90)



#-----------------------------------------------------------------------------------------------------------------
# b. Represent the proportion of people survived from the family size using a graph

x <- table(TitanicData$Survived, TitanicData$Title) 
# table for survived and died

x             # 0 for survived and 1 for died
p <- x[1,]    # number of passengers survived
p

prop <- round(p*100/sum(p),1)  # proportion of passangers survived 
prop
# in barchart format

barplot(p,                        # for number of Passangers
        main = "No. of Passangers Survived by Title", 
        xlab = "Title", 
        ylab = "No. of Passangers", col = rainbow(length(p)), las =3)
text(p, pos = 3, srt = 90)

barplot(prop,                     # for percentage of passangers
        main = "No. of Passangers by Title", xlab = "Title", 
        ylab = "Proportion  of Passangers", col = c("Blue","Red"), 
        legend = rownames(prop), ylim=c(0, 100), las = 3)
text(prop, pos = 3, srt = 90)

# in Pie Chart format

pie_chart <- pie(p, labels = p, main = " No.of passengers of Survival by Family",
                 col = rainbow(length(p)), cex = 1)
legend("right", names(p), cex= 0.5, fill = rainbow(length(p)))


pie(prop, labels = prop, main = " Proportion of Survival by Family",
    col = rainbow(length(prop)), cex = 1)
legend("right", names(prop), cex= 0.5, fill = rainbow(length(prop)))



#---------------------------------------------------------------------------------------------------------------
# c. Impute the missing values in Age variable using Mice Library, create two different 
#graphs showing Age distribution before and after imputation.
library(mice)
sum(is.na(TitanicData$age))
str(TitanicData)
#Removing columns 1,2,3,4,5,7,12,13,14,16,17,18
mini_data <- TitanicData[-c(1,2,3,4,5,7,12,13,14,16,17,18)]
View(mini_data)
md.pattern(mini_data)
library(dplyr) 
mini_data <- mini_data %>%
  mutate(
    survived = as.factor(survived),
    sex = as.factor(sex),
    age = as.numeric(age),
    sibsp = as.factor(sibsp),
    parch = as.factor(parch),
    embarked = as.factor(embarked)
  )
str(mini_data)
mice_data <- mice(mini_data, m=5, maxit=10,seed=500)
summary(mini_data)
Imputed=complete(mice_data,5)
hist(TitanicData$age,  main='Actual Data',col="green")
hist(Imputed$age, main='Imputed Data',col="black")

