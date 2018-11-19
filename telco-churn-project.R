####Install Packages
install.packages("ggplot2")
install.packages("kernlab")
install.packages("arules")
install.packages("arulesViz")
install.packages("e1071")
install.packages("knitr")
install.packages("moments")
install.packages("reshape")
install.packages("rpart")
install.packages("readxl")
install.packages("httr")

####Libraries
library(ggplot2)
library(kernlab)
library(arules)
library(arulesViz)
library(e1071)
library(knitr) 
library(moments)
library(reshape2)
library(rpart)
library(readxl)
library(httr)

### Reading/cleaning data
######################################################################
teledf <- read_excel("C:/Users/olgag/Desktop/SU/Summer2018/IntroDataScience/project.xlsx")

#Summary of data
summary(teledf)

#Dataset structure
str(teledf)

#Check for missing data
(colSums(is.na(teledf)))

#Missing data for total charges.
teledf[is.na(teledf$TotalCharges), ]
#Only ones missing totall charges had a tenure of zero
teledf[teledf$tenure == 0, ]

#Keep customers with tenure greater than or equal to 1
teledf <- subset(teledf, teledf$tenure > 0)

#Subsetting Customers with internet
internet <- subset(teledf, teledf$InternetService != "No")
summary(internet)

#Dropping the customer id
internet <- internet[,-1]

#Subsetting customers without internet
nointernet <- subset(teledf, teledf$InternetService == "No")

#Dropping unused levels from nointernet
summary(nointernet)

#deleting all unused columns
nointernet <- nointernet[,-6:-14]
######################################################################


###Descriptive statistics
######################################################################
#Function to calculate churn with tapply
churn <- function(col){
  per <- sum(col == "Yes") / length(col) * 100
  return(per)
}

#Overall churn rate
churn.all <- sum(teledf$Churn  == "Yes") / nrow(teledf) * 100

#Churn rate by not having any internet service 
#True means they have internet
churn.internet <- tapply(teledf$Churn, teledf$InternetService != "No", churn)

#Table containing all churn rates
table.churn <- data.frame("Overall Churn" = churn.all, "Internet Churn" = 
                            churn.internet[2], "No Internet Churn" = churn.internet[1])

#Putting together a nice table of churn rates
knitr::kable(table.churn)

#Looking at churn between the two internet types
tapply(internet$Churn, internet$InternetService, churn)

#Percent of Monthly revenue by internet
tapply(teledf$MonthlyCharges, teledf$InternetService, sum) / sum(teledf$MonthlyCharges) * 100

#Average cost by internet type
tapply(teledf$MonthlyCharges, teledf$InternetService, mean)

# Analyzing subsets of customers without internet and with internet
# Churn by gender for the subset of customers without internet
female <- length(nointernet$gender[which(nointernet$gender=='Female' & nointernet$Churn=='Yes')])
male <- length(nointernet$gender[which(nointernet$gender=='Male' & nointernet$Churn=='Yes')])
female
male

# Churn by age for the subset of customers without internet 
nonsenior <- length(nointernet$gender[which(nointernet$SeniorCitizen==0 & nointernet$Churn=='Yes')])
senior <- length(nointernet$gender[which(nointernet$SeniorCitizen==1 & nointernet$Churn=='Yes')])
nonsenior
senior

# Churn by gender for the subset of customers with internet
femaleint <- length(internet$gender[which(internet$gender=='Female' & internet$Churn=='Yes')])
maleint <- length(internet$gender[which(internet$gender=='Male' & internet$Churn=='Yes')])
femaleint
maleint

# Churn by age for the subset of customers with internet
nonseniorint <- length(internet$gender[which(internet$SeniorCitizen==0 & internet$Churn=='Yes')])
seniorint <- length(internet$gender[which(internet$SeniorCitizen==1 & internet$Churn=='Yes')])
nonseniorint
seniorint

#Function for print descriptive stats
printVecInfo <- function (myVector)
{
  cat("Mean: ", mean(myVector),"\n")
  cat("Median: ", median(myVector),"\n")
  cat("Min: ", min(myVector),"  ")
  cat("Max: ", max(myVector),"\n")
  cat("Sd: ", sd(myVector), "\n")
  cat("Quantiles (at  0.05  and  0.95): ", quantile(myVector, probs = c(0.05, 0.95)), "\n")
  cat("Skewness: ",skewness(myVector))
}
# NO INTERNET subset
# Descriptive stats for numerical variables
printVecInfo(nointernet$MonthlyCharges)
printVecInfo(nointernet$TotalCharges)

# find percentage of churned customers("churn="yes")
churnYes <- nointernet$Churn[which(nointernet$Churn=='Yes')]
(length(churnYes)/length(nointernet$Churn))*100


#INTERNET subset
# Descriptive stats for numerical variables
printVecInfo(internet$tenure)
printVecInfo(internet$MonthlyCharges)
printVecInfo(internet$TotalCharges)


#entire datatset
printVecInfo(teledf$tenure)
printVecInfo(teledf$MonthlyCharges)
printVecInfo(teledf$TotalCharges)

# find percentage of churned customers("churn="yes")
churnYes1 <- internet$Churn[which(internet$Churn=='Yes')]
(length(churnYes1)/length(internet$Churn))*100

######################################################################
# Demographics: dependents(y/n)
# dataframe: customers with dependents
dependents <- subset(teledf, teledf$Dependents == "Yes", drop = FALSE)

# dataframe: customers without dependents
no_dependents <- subset(teledf, teledf$Dependents == "No", drop = FALSE)

# churn rate: dependents(y/n)
dep_churn_rate <- churn(dependents$Churn)
nodep_churn_rate <- churn(no_dependents$Churn)

# dataframe: average churn, dependents(y) churn, dependents(n) churn
table.churn <- data.frame("Overall Churn" = churn.all, "Churn: Customers with Dependents" = 
                            dep_churn_rate, "Churn: Customers without Dependents" = nodep_churn_rate)

# table: print table.churn
knitr::kable(table.churn)

# Demographics: partner(y/n)

# dataframe: customers with partners
yes_partner <- subset(teledf, teledf$Partner == "Yes", drop = FALSE)

# dataframe: customers without partners
no_partner <- subset(teledf, teledf$Partner == "No", drop = FALSE)

# churn rates partner(y/n)
partner_churn <- churn(yes_partner$Churn)
no_partner_churn <- churn(no_partner$Churn)

# dataframe: churn rates with/without partner
table.partner.churn <- data.frame("Overall Churn" = churn.all, "Churn: Customers with Partners" =partner_churn, "Churn: Customers without Partners" = no_partner_churn)

# table: print table.partner.churn
knitr::kable(table.partner.churn)


# Demographics: customers with dependents and partners vs single customers

partner.dep <- subset(teledf,teledf$Dependents=='Yes' & teledf$Partner=='Yes',drop=FALSE)
nopartner.nodep <- subset(teledf,teledf$Dependents=='No' & teledf$Partner=='No', drop=FALSE)

# churn rates: multi-person households vs single customers
churn.family <- churn(partner.dep$Churn)
churn.single <- churn(nopartner.nodep$Churn)

# dataframe: family churn vs single churn
table.churn.household <- data.frame("Overall churn"=churn.all,"Dependents and Partners"=churn.family,"Single"=churn.single)

# print table: churn rates
knitr::kable(table.churn.household)


######################################################################
# Analyzing what contracts are better

# Attach the dataset
attach(teledf)

# Seeing Churn rates for male and female customers
tapply(Churn, gender, churn)


# Seeing what contracts have the most churn 
churn.contracts <- data.frame(tapply(Churn, Contract, churn))
colnames(churn.contracts) <- 'Churn Rate (%)'

# Seeing if paperless billing has any affect on Churn
churn.paperless <- data.frame(tapply(Churn, PaperlessBilling, churn))
colnames(churn.paperless) <- 'Churn Rate (%)'

# Seeing if Payment method has any affect on Churn
churn.payment <- data.frame(tapply(Churn, PaymentMethod, churn))
colnames(churn.payment) <- 'Churn Rate (%)'

######################################################################


###Inferential Statistics
##hypothesis test that seniors have statistically significant
#difference in churn rate

#Churn rates among senior citizens
seniors <- length(teledf$customerID[(teledf$SeniorCitizen == 1 & teledf$Churn == "Yes")]) / length(teledf$customerID[teledf$SeniorCitizen == 1]) * 100
#Rounding the rate
seniors = round(seniors, digits = 2)
#Print out churn rate for senior citizens
cat("Churn rate among senior citizens: ", seniors, "%", sep = "")

#Churn rate for non-senior citizen customers
allOtherCustomers <- length(teledf$customerID[teledf$Churn == "Yes" & teledf$SeniorCitizen == 0]) / length(teledf$customerID[teledf$SeniorCitizen == 0]) * 100
allOtherCustomers <- round(allOtherCustomers, 2)
cat("Churn rate of non-Seniors: ", allOtherCustomers, "%", sep = "")

#Creating booleans for churn in t.test application
teledf[teledf$Churn == "Yes", "LogicalChurn"] <- T
teledf[teledf$Churn == "No", "LogicalChurn"] <- F
#Are these groups statistically different
t <- t.test(LogicalChurn ~ SeniorCitizen, data = teledf)
print(t)


##Chi-square test on whether contract type and churn have a relationship
#Chi-square test for independence
chit <- chisq.test(teledf$Contract, teledf$Churn)
#What was actually observed
chit$observed
#What was expected
chit$expected

######################################################################

# chi-square test: dependents and churn

# chi-square test for independence
dep_chisq <- chisq.test(teledf$Dependents, teledf$Churn)

# observed
dep_chisq$observed

# expected
dep_chisq$expected

# t test: dependents and churn
t <- t.test(LogicalChurn ~ Dependents, data = teledf)
print(t)

# chi-square: partner and churn
partner_chisq <- chisq.test(teledf$Partner, teledf$Churn)
partner_chisq$observed
partner_chisq$expected

t2 <- t.test(LogicalChurn ~ Partner, data = teledf)
print(t2)


######################################################################

###Visualizations

######################################################################
#Churn by contract type bar plot
p <- ggplot(teledf, aes(Contract)) + geom_bar(aes(fill = Churn))
p <- p + ylab("Count") + ggtitle("Churn by Contract type")
p

#Histogram of tenure for month-to-month contracts with churn fill
p <- ggplot(teledf[teledf$Contract == "Month-to-month", ], aes(tenure, fill = Churn))
p <- p + geom_histogram(binwidth = 1) + xlab("Tenure (months)")
p <- p + ggtitle("Histogram on Churn and tenure\nfor Month-to-Month Customers")
p

#Plotting tenure vs charge to see if pattern in where churn is
g <- ggplot(teledf, aes(tenure, MonthlyCharges, color = Churn, shape = Contract))
g <- g + geom_point()
g <- g + ggtitle("Tenure vs. Average Monthly Charges")
g <- g + xlab("Tenure") + ylab("Average Monthly Charges")
g
######################################################################

# Plotting churn rates by contract type for subset of customers without internet
ChurnYesMTM <- length(nointernet$Contract[which(nointernet$Churn=='Yes'& nointernet$Contract=='Month-to-month')])
ChurnNoMTM <- length(nointernet$Contract[which(nointernet$Churn=='No'& nointernet$Contract=='Month-to-month')])
ChurnYes2y <- length(nointernet$Contract[which(nointernet$Churn=='Yes'& nointernet$Contract=='Two year')])
ChurnNo2y <- length(nointernet$Contract[which(nointernet$Churn=='No'& nointernet$Contract=='Two year')])
ChurnYes1y <- length(nointernet$Contract[which(nointernet$Churn=='Yes'& nointernet$Contract=='One year')])
ChurnNo1y <- length(nointernet$Contract[which(nointernet$Churn=='No'& nointernet$Contract=='One year')])

# Plot Churn by contract type for customers without internet
g <- ggplot(nointernet, aes(Contract)) + geom_bar(aes(fill = Churn)) + ggtitle("Churn by Contract Type for Customers without Internet")
g

# Plot Churn by contract type for customers with internet
g1 <- ggplot(internet, aes(Contract)) + geom_bar(aes(fill = Churn)) + ggtitle("Churn by Contract Type for Customers with Internet")
g1

#Tenure for different types of contract histograms
#Tenure for month-to month option
mtm <- subset(teledf, teledf$Contract=="Month-to-month")
mtmplot1 <- ggplot(mtm, aes(x=tenure))  + geom_histogram(binwidth=5, color="red", fill="#DD5868") +
  ggtitle("Month to Month Contract") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Tenure, months") + ylab("# of customers")
mtmplot1

#Tenure for one year contract option
oneyear <- subset(teledf, teledf$Contract=="One year")
mtmplot2 <- ggplot(oneyear, aes(x=tenure))  + geom_histogram(binwidth=5, color="blue", fill="#1E4594") +
  ggtitle("One Year Contract") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Tenure, months") + ylab("# of customers")
mtmplot2

#Tenure for two year contract option
twoyear <- subset(teledf, teledf$Contract=="Two year")
mtmplot3 <- ggplot(twoyear, aes(x=tenure))  + geom_histogram(binwidth=5, color="green", fill="#125528") +
  ggtitle("Two Year Contract") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Tenure, months") + ylab("# of customers")
mtmplot3


# plot: churn(y/n) and dependents (y/n)
a <- ggplot(teledf, aes(Dependents)) + geom_bar(aes(fill = Churn))
a <- a + ylab("Count") + ggtitle("Churn for Customers with/without Dependents")
a

# plot: churn(y/n), dependents(y/n) and other tested variables
# tenure
b <- ggplot(teledf, aes(Dependents, tenure, color = Churn))
b <- b + geom_boxplot()
b <- b + ggtitle("Tenure, Dependents and Churn")
b <- b + xlab("Dependents") + ylab("Tenure")
b

# monthly charges
c <- ggplot(teledf, aes(Dependents, MonthlyCharges, color = Churn))
c <- c + geom_boxplot()
c <- c + ggtitle("Monthly Charges, Dependents and Churn")
c <- c + xlab("Dependents") + ylab("Monthly Charges")
c

# total charges
d <- ggplot(teledf, aes(Dependents, TotalCharges, color = Churn))
d <- d + geom_boxplot()
d <- d + ggtitle("Total Charges, Dependents and Churn")
d <- d + xlab("Dependents") + ylab("Total Charges")
d

# VISUALIZATIONS -- churn, dependents and partner
e <- ggplot(teledf) +
  geom_bin2d(aes(x=Dependents, y=Churn,))
e

f <- ggplot(teledf) +
  geom_bin2d(aes(x=Partner, y=Churn))
f

# Histogram describing churn vs Tenure for Customers with Dependents
g <- ggplot(dependents) +
  geom_histogram(aes(x=tenure,fill=Churn)) + ggtitle('Churn and Tenure for Customers with Dependents')
g

# Histogram describing churn vs Tenure for Customers without Dependents
h <- ggplot(no_dependents) +
  geom_histogram(aes(x=tenure, fill=Churn)) + ggtitle('Churn and Tenure for Customers without Dependents')
h


# Histogram describing churn vs Tenure for Customers with Partners
j <- ggplot(yes_partner) +
  geom_histogram(aes(x=tenure, fill=Churn)) + ggtitle('Churn and Tenure for Customers with Partners')
j

# Histogram describing churn vs Tenure for Customers without Partners
k <- ggplot(no_partner) +
  geom_histogram(aes(x=tenure, fill=Churn)) + ggtitle('Churn and Tenure for Customers without Parteners')
k

# Overall Histogram of Churn vs Tenure
l <- ggplot(teledf) +
  geom_histogram(aes(x=tenure, fill=Churn)) + ggtitle('Histogram of Churn and Tenure')
l

# Plotting the churn vs gender
Gender_Plot <- ggplot(teledf) + geom_bar(aes(x = gender, fill=Churn)) + 
  ggtitle("Gender vs Churn")
Gender_Plot

# Subset of customers that have phone service
telephonedf <- teledf[teledf$PhoneService == 'Yes',]

# Boxplots showing if having multiple lines changes either the money made or tenure
ggplot(telephonedf, aes(MultipleLines, TotalCharges)) + geom_boxplot() + 
  ggtitle('Multiple Line Phone Service vs Total Charges') +xlab('Multiple Lines') + ylab('Total Charges') + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(telephonedf, aes(MultipleLines, tenure)) + geom_boxplot() + 
  ggtitle('Multiple Line Phone Service vs Tenure') +xlab('Multiple Lines') + ylab('Tenure (Mo.)') + 
  theme(plot.title = element_text(hjust = 0.5))

# Subset those customers with internet service that were churned
Churned.internet <- internet[internet$Churn == 'Yes',]

# Get the total numbre of churned customers that had internet
internet.churn.rows <- nrow(Churned.internet)

# Percentage of churned customers that had online security
churn.security <- 100 * nrow(Churned.internet[Churned.internet$OnlineSecurity =="Yes",])/internet.churn.rows

# Percentage of churned customers that had Online Backup
churn.backup <- 100 * nrow(Churned.internet[Churned.internet$OnlineBackup =="Yes",])/internet.churn.rows

# Percentage of churned customers that had Device Protection
churn.protect <- 100 * nrow(Churned.internet[Churned.internet$DeviceProtection =="Yes",])/internet.churn.rows

# Percentage of churned customers that had Tech Support
churn.support <- 100 * nrow(Churned.internet[Churned.internet$TechSupport =="Yes",])/internet.churn.rows

# Percentage of churned customers that had Streaming TV
churn.TV <- 100 * nrow(Churned.internet[Churned.internet$StreamingTV =="Yes",])/internet.churn.rows

# Percentage of churned customers that had Streaming Movies
churn.movies <- 100 * nrow(Churned.internet[Churned.internet$StreamingMovies =="Yes",])/internet.churn.rows

# Make a dataframe from above percentages
churned.options <- data.frame(c(churn.security,churn.backup,churn.protect,churn.support, churn.TV, churn.movies))
churned.options$Service <- c('Online Security','Online Backup','Device Protection','Tech Support','Spreaming TV','Streaming Movies')
colnames(churned.options) <- c('Churned_Percent','Service')

# Create Bar plot to show the churn percentages of each service
ggplot(churned.options, aes(x = Service, y=Churned_Percent)) + 
  geom_bar(stat = 'identity') + ggtitle('Churn Rates of for additional internet services') +
  theme(plot.title = element_text(hjust = 0.5))
######################################################################

###Linear Modeling

######################################################################
#Only two numerical columns, is there a linear relationship?
#Must use the churned customers because we know exactly how long they lasted
#Looking at correlation between monthly charges and tenure
cor(teledf[teledf$Churn == "Yes", "tenure"], teledf[teledf$Churn == "Yes", "MonthlyCharges"])
#Not that great
#Plot to take a look

#Subsetting the churned customers from the teledf
teledf.churn <- subset(teledf, teledf$Churn == "Yes")
#Getting rid of customer ID, total charges and churn
drop <- c("TotalCharges", "Churn", "customerID")
teledf.churn <- teledf.churn[, !names(teledf.churn) %in% drop]
#Creating a linear model directly from monthly charges
lm1 <- lm(tenure ~ MonthlyCharges, data = teledf.churn)

#Look at adjusted R2
summary(lm1)
#0.16 Adjusted R2 is terrible

#plotting Monthly charges vs tenure
plot(teledf.churn$MonthlyCharges, teledf.churn$tenure,
     xlab = "Monthly Charges $", ylab = "Tenure (Months)", main = "Monthly Charges vs. Tenure")
#Drawing the model in the plot
abline(lm1, col = "red", lwd = 3)

#Linear model using all values
lm.all <- lm(tenure ~ ., data = teledf.churn)
#Looking at model
summary(lm.all)
#Predicting tenure for all
lm.all.predict <- predict(lm.all, teledf.churn)
#Creating data frame to compare predicted and actual
df.predict.all <- data.frame(actual = teledf.churn$tenure, predicted = lm.all.predict)
#Calculating Residual
df.predict.all$residual <- df.predict.all$actual - df.predict.all$predicted
#Plotting residuals
plot(df.predict.all$actual, df.predict.all$residual, main = "Residuals for Tenure",
     xlab = "Actual Tenure", ylab = "Residual")


#What about for only internet customers
#Subsetting churned customers
internet.churn <- subset(internet, internet$Churn == "Yes")
#Getting rid of churn variable
internet.churn <- internet.churn[,!names(internet.churn) %in% c("TotalCharges", "Churn")]

#Is there a correlation for internet users?
cor(internet.churn$tenure, internet.churn$MonthlyCharges)
#Still not great
lm2 <- lm(tenure ~ MonthlyCharges, data = internet.churn)
summary(lm2)


#Linear model for internet customers using all variables
lm3 <- lm(tenure ~ ., data = internet.churn)
summary(lm3)

#Plot to take a look
plot(internet.churn$MonthlyCharges, internet.churn$tenure, main = "Monthly Charges vs Tenure",
     xlab = "Monthly Charges $", ylab = "Tenure (Months)")
#Similar shape
abline(lm2, lwd = 3, col = "red")


######################################################################

###Data Mining

######################################################################

##Subsetting train and test for all customers
#Dropping id and Total charges (it is the same as monthly * tenure)
#teledf.svm <- teledf[,!names(teledf) %in% c("customerID", "TotalCharges")]

#Setting seed to keep our random customers the same
set.seed(1016)
#Creating random indices to index from 1 to nrows in teledf.svm
randindex <- sample(1:nrow(teledf))
#Creating a cutoff point that is 2/3 through the data
cutOff <-  floor(2 * nrow(teledf) / 3)


#Indexing from our full dataset the random indexes from 1 to cutoff
teledf.train <- teledf[randindex[1:cutOff], ]

#Putting the rest in out test set
teledf.test <- teledf[randindex[(cutOff + 1):nrow(teledf)], ]

teledf.train <- teledf.train[,!names(teledf.train) %in% c("customerID", "TotalCharges")]
teledf.test <- teledf.test[,!names(teledf.test) %in% c("customerID", "TotalCharges")]


#Training and test data for internet subset
randindex <- sample(1:nrow(internet))
#Cutoff for train and test
cutoff <- floor(2 * length(randindex) / 3)
#Getting rid of internet total charges
internet <- internet[, !names(internet) %in% "TotalCharges"]
#Indexing training set
internet.train <- internet[randindex[1:cutoff], ]

#Indexing test set
internet.test <- internet[randindex[(cutoff+1):length(randindex)], ]

####################################
##SVM - All
#Want to minimize Type 2 Error (Predicted Not to Leave, actually leaves)
svm.1 <- ksvm(Churn ~ ., data = teledf.train, kernel = "rbfdot", 
              kpar = "automatic", C = 1, prob.model = T, cross = 3, probability = T)
#Predicting svm1 on the test set
svm.pred <- predict(svm.1, teledf.test, type = "p")
svm.pred.all <- svm.pred[,2]
svm.original <- table(data.frame(actual = teledf.test$Churn, predicted = predict(svm.1, teledf.test, type = "response")))
cat("Accuracy: ", (svm.original[1,1] + svm.original[2,2]) / sum(svm.original) * 100, "%", sep = "")
sum(teledf.test$Churn == "No") / length(teledf.test$Churn)

test.threshold <- seq(0.1, 0.85, by = 0.01)
#Creating vectors to hold model statistics
svm.accuracy.all <- numeric(75)
svm.recall.all <- numeric(75)
svm.precision.all <- numeric(75)
svm.f1.score.all <- numeric(75)

for (i in 1:length(test.threshold)){
  #If the prediction is greater than the threshold, make it a 1, else 0
  temp.pred <- ifelse(svm.pred.all > test.threshold[i], 1, 0)
  #Create a data frame with the predictions and the actual
  df.temp <- data.frame(actual = teledf.test$Churn, predicted = temp.pred)
  #Make it a table
  comptable.temp <- table(df.temp)
  #put Overall accuracy for this model in the accuracy vector
  svm.accuracy.all[i] <- (comptable.temp[1,1] + comptable.temp[2,2]) / sum(comptable.temp)
  #put Overall recall for this model in the recall vector
  svm.recall.all[i] <- comptable.temp[2,2] / (comptable.temp[2,1] + comptable.temp[2,2])
  #put precision accuracy for this model in the precision vector
  svm.precision.all[i] <- comptable.temp[2,2] / sum(comptable.temp[,2])
  #put f1 score accuracy for this model in the f1 score vector
  svm.f1.score.all[i] <- 2 * ((svm.precision.all[i] * svm.recall.all[i]) / (svm.precision.all[i] + svm.recall.all[i]))
}

#Creating  data frame for model statistics
results.svm.all <- data.frame(threshold = test.threshold, svm.accuracy.all, svm.recall.all , svm.precision.all , svm.f1.score.all)
#Finding maximum f1 score
max.f1.all <- results.svm.all[which.max(results.svm.all$svm.f1.score.all), "threshold"]
#Melting data frame while maintaining the threshold
results.svm.all <- melt(results.svm.all, id.vars = "threshold")

#Plotting the model statistics versus the threshold for probability
g <- ggplot(results.svm.all, aes(x = threshold, y = value, color = variable))
#Drawing the a line for each value and a vertical line at the probability of the maximum f1 score
g <- g + geom_line() + ggtitle("SVM Model Quality Statistics (All Customers)") + geom_vline(xintercept = max.f1.all, col = "red", size = 1)
g

svm.pred.final <- ifelse(svm.pred.all > max.f1.all, 1, 0)

#Table for confusion Matrix
compTable.svm <- data.frame(Actual = teledf.test$Churn, Predicted = svm.pred.final)

#Looking at the confusion Matrix
compTable.svm <- table(compTable.svm)

svm.accuracy.final <- (compTable.svm[1,1] + compTable.svm[2,2]) / sum(compTable.svm)
#put Overall recall for this model in the recall vector
svm.recall.final <- compTable.svm[2,2] / (compTable.svm[2,1] + compTable.svm[2,2])
#put precision accuracy for this model in the precision vector
svm.precision.final <- compTable.svm[2,2] / sum(compTable.svm[,2])
#put f1 score accuracy for this model in the f1 score vector
svm.f1.score.final <- 2 * ((svm.precision.final * svm.recall.final) / (svm.precision.final + svm.recall.final))

cat("SVM Model quality (All Customers):\n\taccuracy: ", svm.accuracy.final, "\n\trecall: ", svm.recall.final, 
    "\n\tprecision: ", svm.precision.final, "\n\tF1 score: ", svm.f1.score.final)

##SVM - Internet
#Want to minimize Type 2 Error (Predicted Not to Leave, actually leaves)
svm.2 <- ksvm(Churn ~ ., data = internet.train, kernel = "rbfdot", 
              kpar = "automatic", C = 1, prob.model = T, cross = 3, probability = T)
#Predicting svm1 on the test set
svm.pred.internet <- predict(svm.2, internet.test, type = "p")
svm.pred.internet <- svm.pred.internet[,2]
svm.original.internet <- table(data.frame(actual = internet.test$Churn, predicted = predict(svm.2, internet.test, type = "response")))
cat("Accuracy: ", (svm.original.internet[1,1] + svm.original.internet[2,2]) / sum(svm.original.internet) * 100, "%", sep = "")
sum(internet.test$Churn == "No") / length(internet.test$Churn)

test.threshold <- seq(0.1, 0.85, by = 0.01)
#Creating vectors to hold model statistics
svm.accuracy.internet <- numeric(75)
svm.recall.internet <- numeric(75)
svm.precision.internet <- numeric(75)
svm.f1.score.internet <- numeric(75)

for (i in 1:length(test.threshold)){
  #If the prediction is greater than the threshold, make it a 1, else 0
  temp.pred <- ifelse(svm.pred.internet > test.threshold[i], 1, 0)
  #Create a data frame with the predictions and the actual
  df.temp <- data.frame(actual = internet.test$Churn, predicted = temp.pred)
  #Make it a table
  comptable.temp <- table(df.temp)
  #put Overall accuracy for this model in the accuracy vector
  svm.accuracy.internet[i] <- (comptable.temp[1,1] + comptable.temp[2,2]) / sum(comptable.temp)
  #put Overall recall for this model in the recall vector
  svm.recall.internet[i] <- comptable.temp[2,2] / (comptable.temp[2,1] + comptable.temp[2,2])
  #put precision accuracy for this model in the precision vector
  svm.precision.internet[i] <- comptable.temp[2,2] / sum(comptable.temp[,2])
  #put f1 score accuracy for this model in the f1 score vector
  svm.f1.score.internet[i] <- 2 * ((svm.precision.internet[i] * svm.recall.internet[i]) / (svm.precision.internet[i] + svm.recall.internet[i]))
}

#Creating  data frame for model statistics
results.svm.internet <- data.frame(threshold = test.threshold, svm.accuracy.internet, svm.recall.internet , svm.precision.internet , svm.f1.score.internet)
#Finding maximum f1 score
max.f1 <- results.svm.internet[which.max(results.svm.internet$svm.f1.score.internet), "threshold"]
#Melting data frame while maintaining the threshold
results.svm.internet <- melt(results.svm.internet, id.vars = "threshold")

#Plotting the model statistics versus the threshold for probability
g <- ggplot(results.svm.internet, aes(x = threshold, y = value, color = variable))
#Drawing the a line for each value and a vertical line at the probability of the maximum f1 score
g <- g + geom_line() + ggtitle("SVM Model Quality Statistics (Internet Customers)") + geom_vline(xintercept = max.f1, col = "red", size = 1)
g

svm.pred.final.internet <- ifelse(svm.pred.internet > max.f1, 1, 0)

#Table for confusion Matrix
compTable.svm.internet <- data.frame(Actual = internet.test$Churn, Predicted = svm.pred.final.internet)

#Looking at the confusion Matrix
compTable.svm.internet <- table(compTable.svm.internet)

svm.accuracy.final.internet <- (compTable.svm.internet[1,1] + compTable.svm.internet[2,2]) / sum(compTable.svm.internet)
#put Overall recall for this model in the recall vector
svm.recall.final.internet <- compTable.svm.internet[2,2] / (compTable.svm.internet[2,1] + compTable.svm.internet[2,2])
#put precision accuracy for this model in the precision vector
svm.precision.final.internet <- compTable.svm.internet[2,2] / sum(compTable.svm.internet[,2])
#put f1 score accuracy for this model in the f1 score vector
svm.f1.score.final.internet <- 2 * ((svm.precision.final.internet * svm.recall.final.internet) / (svm.precision.final.internet + svm.recall.final.internet))

cat("SVM Model quality (Internet Customers):\n\taccuracy: ", svm.accuracy.final.internet, "\n\trecall: ", svm.recall.final.internet, 
    "\n\tprecision: ", svm.precision.final.internet, "\n\tF1 score: ", svm.f1.score.final.internet)



##################
## Naive Bayes
#Training a naive bayes classification model predicting churn from all variables
nb <- naiveBayes(Churn ~ ., data = teledf.train, probability = T) 

#Predicting based on nb model
nb.pred <- predict(nb, teledf.test, type = "raw")
nb.original <- table(data.frame(actual = teledf.test$Churn, predicted = predict(nb, teledf.test, type = "class")))
cat("Accuracy: ", (nb.original[1,1] + nb.original[2,2]) / sum(nb.original) * 100, "%", sep = "")
sum(teledf.test$Churn == "No") / length(teledf.test$Churn)

nb.pred.all <- nb.pred[,2]

test.threshold <- seq(0.1, 0.85, by = 0.01)
#Creating vectors to hold model statistics
nb.accuracy.all <- numeric(75)
nb.recall.all <- numeric(75)
nb.precision.all <- numeric(75)
nb.f1.score.all <- numeric(75)

for (i in 1:length(test.threshold)){
  #If the prediction is greater than the threshold, make it a 1, else 0
  temp.pred <- ifelse(nb.pred.all > test.threshold[i], 1, 0)
  #Create a data frame with the predictions and the actual
  df.temp <- data.frame(actual = teledf.test$Churn, predicted = temp.pred)
  #Make it a table
  comptable.temp <- table(df.temp)
  #put Overall accuracy for this model in the accuracy vector
  nb.accuracy.all[i] <- (comptable.temp[1,1] + comptable.temp[2,2]) / sum(comptable.temp)
  #put Overall recall for this model in the recall vector
  nb.recall.all[i] <- comptable.temp[2,2] / (comptable.temp[2,1] + comptable.temp[2,2])
  #put precision accuracy for this model in the precision vector
  nb.precision.all[i] <- comptable.temp[2,2] / sum(comptable.temp[,2])
  #put f1 score accuracy for this model in the f1 score vector
  nb.f1.score.all[i] <- 2 * ((nb.precision.all[i] * nb.recall.all[i]) / (nb.precision.all[i] + nb.recall.all[i]))
}

#Creating  data frame for model statistics
results.nb.all <- data.frame(threshold = test.threshold, nb.accuracy.all, nb.recall.all , nb.precision.all , nb.f1.score.all)
#Finding maximum f1 score
max.f1 <- results.nb.all[which.max(results.nb.all$nb.f1.score.all), "threshold"]
#Melting data frame while maintaining the threshold
results.nb.all <- melt(results.nb.all, id.vars = "threshold")

#Plotting the model statistics versus the threshold for probability
g <- ggplot(results.nb.all, aes(x = threshold, y = value, color = variable))
#Drawing the a line for each value and a vertical line at the probability of the maximum f1 score
g <- g + geom_line() + ggtitle("Naive Bayes Model Quality Statistics (All Customers)") + geom_vline(xintercept = max.f1, col = "red", size = 1)
g

nb.pred.final <- ifelse(nb.pred.all > max.f1, 1, 0)

#Table for confusion Matrix
compTable.nb <- data.frame(Actual = teledf.test$Churn, Predicted = nb.pred.final)

#Looking at the confusion Matrix
compTable.nb <- table(compTable.nb)

nb.accuracy.final <- (compTable.nb[1,1] + compTable.nb[2,2]) / sum(compTable.nb)
#put Overall recall for this model in the recall vector
nb.recall.final <- compTable.nb[2,2] / (compTable.nb[2,1] + compTable.nb[2,2])
#put precision accuracy for this model in the precision vector
nb.precision.final <- compTable.nb[2,2] / sum(compTable.nb[,2])
#put f1 score accuracy for this model in the f1 score vector
nb.f1.score.final <- 2 * ((nb.precision.final * nb.recall.final) / (nb.precision.final + nb.recall.final))

cat("Naive Bayes Model quality (All Customers):\n\taccuracy: ", nb.accuracy.final, "\n\trecall: ", nb.recall.final, 
    "\n\tprecision: ", nb.precision.final, "\n\tF1 score: ", nb.f1.score.final)

#Table for confusion Matrix for naive bayes model
compTable.nb <- data.frame(Actual = teledf.test$Churn, Predicted = nb.pred.final)

#Looking at the naive bayes confusion Matrix
table(compTable.nb)



###Naive Bayes - internet
#Training a naive bayes classification model predicting churn from all variables
nb.internet <- naiveBayes(Churn ~ ., data = internet.train, probability = T) 

#Predicting based on nb model
nb.pred.internet <- predict(nb.internet, internet.test, type = "raw")


nb.original.internet <- table(data.frame(actual = internet.test$Churn, predicted = predict(nb.internet, internet.test, type = "class")))
cat("Accuracy: ", (nb.original[1,1] + nb.original[2,2]) / sum(nb.original) * 100, "%", sep = "")

nb.pred.internet <- nb.pred.internet[,2]
test.threshold <- seq(0.1, 0.85, by = 0.01)
#Creating vectors to hold model statistics
nb.accuracy.internet <- numeric(75)
nb.recall.internet <- numeric(75)
nb.precision.internet <- numeric(75)
nb.f1.score.internet <- numeric(75)

for (i in 1:length(test.threshold)){
  #If the prediction is greater than the threshold, make it a 1, else 0
  temp.pred <- ifelse(nb.pred.internet > test.threshold[i], 1, 0)
  #Create a data frame with the predictions and the actual
  df.temp <- data.frame(actual = internet.test$Churn, predicted = temp.pred)
  #Make it a table
  comptable.temp <- table(df.temp)
  #put Overall accuracy for this model in the accuracy vector
  nb.accuracy.internet[i] <- (comptable.temp[1,1] + comptable.temp[2,2]) / sum(comptable.temp)
  #put Overall recall for this model in the recall vector
  nb.recall.internet[i] <- comptable.temp[2,2] / (comptable.temp[2,1] + comptable.temp[2,2])
  #put precision accuracy for this model in the precision vector
  nb.precision.internet[i] <- comptable.temp[2,2] / sum(comptable.temp[,2])
  #put f1 score accuracy for this model in the f1 score vector
  nb.f1.score.internet[i] <- 2 * ((nb.precision.internet[i] * nb.recall.internet[i]) / (nb.precision.internet[i] + nb.recall.internet[i]))
}

#Creating  data frame for model statistics
results.nb.internet <- data.frame(threshold = test.threshold, nb.accuracy.internet, nb.recall.internet , nb.precision.internet , nb.f1.score.internet)
#Finding maximum f1 score
max.f1 <- results.nb.internet[which.max(results.nb.internet$nb.f1.score.internet), "threshold"]
#Melting data frame while maintaining the threshold
results.nb.internet <- melt(results.nb.internet, id.vars = "threshold")

#Plotting the model statistics versus the threshold for probability
g <- ggplot(results.nb.internet, aes(x = threshold, y = value, color = variable))
#Drawing the a line for each value and a vertical line at the probability of the maximum f1 score
g <- g + geom_line() + ggtitle("Naive Bayes Model Quality Statistics (Internet Customers)") + geom_vline(xintercept = max.f1, col = "red", size = 1)
g

nb.pred.final.internet <- ifelse(nb.pred.internet > max.f1, 1, 0)

#Table for confusion Matrix
compTable.nb.internet <- data.frame(Actual = internet.test$Churn, Predicted = nb.pred.final.internet)

#Looking at the confusion Matrix
compTable.nb.internet <- table(compTable.nb.internet)

nb.accuracy.final.internet <- (compTable.nb.internet[1,1] + compTable.nb.internet[2,2]) / sum(compTable.nb.internet)
#put Overall recall for this model in the recall vector
nb.recall.final.internet <- compTable.nb.internet[2,2] / (compTable.nb.internet[2,1] + compTable.nb.internet[2,2])
#put precision accuracy for this model in the precision vector
nb.precision.final.internet <- compTable.nb.internet[2,2] / sum(compTable.nb.internet[,2])
#put f1 score accuracy for this model in the f1 score vector
nb.f1.score.final.internet <- 2 * ((nb.precision.final.internet * nb.recall.final.internet) / (nb.precision.final.internet + nb.recall.final.internet))

cat("Naive Bayes Model quality (Internet Customers):\n\taccuracy: ", nb.accuracy.final.internet, "\n\trecall: ", nb.recall.final.internet, 
    "\n\tprecision: ", nb.precision.final.internet, "\n\tF1 score: ", nb.f1.score.final.internet)

#Table for confusion Matrix for naive bayes model
compTable.nb.internet <- data.frame(Actual = internet.test$Churn, Predicted = nb.pred.final.internet)

#Looking at the naive bayes confusion Matrix
table(compTable.nb.internet)



###Decision Trees - All
##################
###Decision tree - all
dt <- rpart(Churn ~ ., teledf.train,  method = "class")
#Predict the probablility on the test data frame
dt.predict <- predict(dt, teledf.test, "class")
#Get the probability of churned customer

#Table for confusion Matrix
compTable.dt <- data.frame(Actual = teledf.test$Churn, Predicted = dt.predict)

#Looking at the confusion Matrix
compTable.dt <- table(compTable.dt)

rpart.plot(dt, main = "Decision Tree for All Customers")

dt.accuracy.final <- (compTable.dt[1,1] + compTable.dt[2,2]) / sum(compTable.dt)
#put Overall recall for this model in the recall vector
dt.recall.final <- compTable.dt[2,2] / (compTable.dt[2,1] + compTable.dt[2,2])
#put precision accuracy for this model in the precision vector
dt.precision.final <- compTable.dt[2,2] / sum(compTable.dt[,2])
#put f1 score accuracy for this model in the f1 score vector
dt.f1.score.final <- 2 * ((dt.precision.final * dt.recall.final) / (dt.precision.final + dt.recall.final))

cat("Decision Tree Model quality (All Customers):\n\taccuracy: ", dt.accuracy.final, "\n\trecall: ", dt.recall.final, 
    "\n\tprecision: ", dt.precision.final, "\n\tF1 score: ", dt.f1.score.final)

#Looking at the naive bayes confusion Matrix
compTable.dt




###Decision tree - internet
dt.internet <- rpart(Churn ~ ., internet.train, method = "class")
#Predict the probablility on the test data frame
dt.predict.internet <- predict(dt.internet, internet.test, "class")

rpart.plot(dt.internet, main = "Decision Tree for Internet Customers")

#Table for confusion Matrix
compTable.dt.internet <- data.frame(Actual = internet.test$Churn, Predicted = dt.predict.internet)

#Looking at the confusion Matrix
compTable.dt.internet <- table(compTable.dt.internet)
compTable.dt.internet

dt.accuracy.final.internet <- (compTable.dt.internet[1,1] + compTable.dt.internet[2,2]) / sum(compTable.dt.internet)
#put Overall recall for this model in the recall vector
dt.recall.final.internet <- compTable.dt.internet[2,2] / (compTable.dt.internet[2,1] + compTable.dt.internet[2,2])
#put precision accuracy for this model in the precision vector
dt.precision.final.internet <- compTable.dt.internet[2,2] / sum(compTable.dt.internet[,2])
#put f1 score accuracy for this model in the f1 score vector
dt.f1.score.final.internet <- 2 * ((dt.precision.final.internet * dt.recall.final.internet) / (dt.precision.final.internet + dt.recall.final.internet))

cat("Decision Tree Model quality (Internet Customers):\n\taccuracy: ", dt.accuracy.final.internet, "\n\trecall: ", dt.recall.final.internet, 
    "\n\tprecision: ", dt.precision.final.internet, "\n\tF1 score: ", dt.f1.score.final.internet)

#Table for confusion Matrix for decision model
compTable.dt.internet <- data.frame(Actual = internet.test$Churn, Predicted = dt.predict.internet)
table(compTable.dt.internet)

######################################################################

