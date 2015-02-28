# Udemy Class - Data Science
# https://www.udemy.com/introduction-to-data-science/#/
# http://winvector.github.io/IntroductionToDataScience/

library(dplyr)
library(ggplot2)
library(randomForest)

setwd("/Users/Stuart/R_Files/Udemy_Data_Science")

### SECTION 1
### Lecture 2 Walk throught of a data science project
hdata <- readRDS("homedata_lesson1_dirty.rds")
hdata <- tbl_df(hdata)
glimpse(hdata)
summary(hdata)

# Note: although Lot is an integer, each value represents a size of lot and thus should be modeledd as a category variable, else the algorithms will be "fooled" with an integer value
sort(unique(hdata$Lot))

# Note also that Year has at least one 0 value. Likely representing an unknown year house was built. Also, are there any NA values
sum(is.na(hdata$Year))
# Let's change any 0 Year values to NA
hdata$Year <- ifelse(hdata$Year <= 0, NA, hdata$Year)
summary(hdata$Year)

# Let's pretend we now have a clean data set (we read in a separate clean file)
hdata <- readRDS("homedata_lesson1_clean.rds")

# Let's turn Lot variable into a factor variable
hdata$Lot <- as.factor(hdata$Lot)

# Create an age variable:
thisyear <- 2015
hdata$Age <- thisyear - hdata$Year

# Let's see how age relates to price
ggplot(hdata,aes(x=Age, y=Price)) + geom_point() + geom_smooth(se=FALSE)

### Lecture 3 Starting with R and Data
### SECTION 2
### Lecture 5 Validating Models
salaryData <- readRDS("salaryData.RDS")
str(salaryData)
dim(salaryData)
outcome <- "logSalary"
vars <- setdiff(colnames(salaryData), c("Salary","Player","logSalary")) # Interesting way to select variables
set.seed(45433622)
nr <- nrow(salaryData)
is.test <- runif(nr) <= 0.25  # Nice way to ensure random selection of test and training sets
summary(is.test)
test <- salaryData[is.test,]
train <- salaryData[!is.test,]
salaryData$is.test <- is.test #put test marker in data for reproducibility
fmla <- paste(outcome, "~", paste(vars, collapse="+")) # setting up the formula
model <- lm(fmla,data=train)
summary(model)

# Now make predictions using our model
salPred <- predict(model, newdata=salaryData)
# Setup dataframe with outcomes
perf <- data.frame(logSalary = salaryData[[outcome]], pred=salPred, is.test=salaryData$is.test)
# Compute and create raw error variable for entire data set (test and training)
sqerr <- (perf$logSalary - perf$pred)^2
# Lets look at RMS training error
sqrt(mean(sqerr[!is.test]))
# Let's take a look at RMS of test error
sqrt(mean(sqerr[is.test]))
# We see that the test error is not too much bigger than training error. Good. No indication of overfitting

# Let's plot
ggplot(perf, aes(x=pred, y=logSalary, color=is.test)) + 
  geom_point(aes(shape=is.test)) +  
  geom_abline(slope=1) + 
  scale_color_manual(values = c("FALSE" = "darkgray", "TRUE" = "darkblue")) +
  coord_fixed()

# Let's do Random Forest model
mod2 <- randomForest(train[,vars], train[,outcome])

# Now make predictions using our model
salPred2 <- predict(mod2, newdata=salaryData)

# Setup dataframe with outcomes
perf2 <- data.frame(logSalary = salaryData[[outcome]], pred=salPred2, is.test=salaryData$is.test)
sqerr2 <- (perf2$logSalary - perf2$pred)^2

# Lets look at RMS training error
sqrt(mean(sqerr2[!is.test]))
# Let's take a look at RMS of test error
sqrt(mean(sqerr2[is.test]))
# Note that test error is much larger than training error and larger than regression test error
# Random Forest looks better in training, but may be slightly worse in test.

#### Logistic Regression

