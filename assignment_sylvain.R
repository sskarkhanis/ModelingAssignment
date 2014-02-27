#install.packages("aCRM")
library(aCRM)

# Functions
mvProp <- function(v){
  sum(is.na(v))/length(v)
}

setwd('/home/xclyde/Courses/UGENT/Modeling/Assignment')

# Complaints import
complaints <- read.table("complaints.txt", header=TRUE, sep=";", colClasses=c("character", "character", "character", "character", "character", "character", "character"))
dim(complaints)
str(complaints)
head(complaints)
tail(complaints)
colnames(complaints)

# Credit import
credit <- read.table("credit.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "character", "factor", "numeric", "integer"))
dim(credit)
str(credit)
head(credit)
tail(credit)
colnames(credit)

# Customers import
customers <- read.table("customers.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "character", "character", "character", "character"))
dim(customers)
str(customers)
head(customers)
tail(customers)
colnames(customers)

# Delivery import
delivery <- read.table("delivery.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "factor", "factor", "character", "character"))
dim(delivery)
str(delivery)
head(delivery)
tail(delivery)
colnames(delivery)

# Formula import
formula <- read.table("formula.txt", header=TRUE, sep=";", colClasses=c("character", "character", "factor", "integer"))
dim(formula)
str(formula)
head(formula)
tail(formula)
colnames(formula)

# Subscriptions import
subscriptions <- read.table("subscriptions.txt", header=TRUE, sep=";", colClasses=c("character","character","character","character","character","character","integer","integer","character","factor","factor","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
dim(subscriptions)
str(subscriptions)
head(subscriptions)
tail(subscriptions)
colnames(subscriptions)

tables = list(complaints, credit, customers, delivery, formula, subscriptions)

# Co
criptions[sapply(subscriptions, is.numeric)], use="complete.obs")

########
# Base table generation: customers
########
customersBaseTable = customers
predictors = list("Gender", "DOB", "District", "ZIP", "StreeID")
for (j in predictors){
  customersBaseTable[paste("MV", j, sep="")] <- as.factor(is.na(customers[j]))
}
customersBaseTable <- imputeMissings(customersBaseTable)

# Base table generation: subscriptions
predictors = list("Gender", "DOB", "District", "ZIP", "StreeID")
for (j in predictors){
  customersBaseTable[paste("MV", j, sep="")] <- as.factor(is.na(customers[j]))
}
#a = aggregate(subscriptions$GrossFormulaPrice, by=list(CustID=subscriptions$CustomerID), function(x) sum(is.na(x))/length(x))
#bt <- data.frame(A=c(NA,2,3), B=c(1,NA,3), C=c(1,2,NA))
#bt[,names(bt) %in% c("A", "B")] <- imputeMissings(bt[,names(bt) %in% c("A", "B")])

subscriptions$StartDate <- as.Date(subscriptions$StartDate,"%d/%m/%Y")
subscriptions$EndDate <- as.Date(subscriptions$EndDate,"%d/%m/%Y")
subscriptions$RenewalDate <- as.Date(subscriptions$RenewalDate,"%d/%m/%Y")
subscriptions$PaymentDate <- as.Date(subscriptions$PaymentDate,"%d/%m/%Y")

# Mean subscription duration
subscriptions["Duration"] <- subscriptions["EndDate"] - subscriptions["StartDate"]
subscriptionsBT= aggregate(list(meanSubDuration=subscriptions$Duration), subscriptions["CustomerID"], mean)

# Mean number of newspapers
meanNbrNP = aggregate(list(meanNbrNP=subscriptions$NbrNewspapers), subscriptions["CustomerID"], mean)
subscriptionsBT = merge(subscriptionsBT, meanNbrNP, by="CustomerID")

# Mean number of start newspapers
meanNbrStartNP = aggregate(list(meanNbrStartNP=subscriptions$NbrStart), subscriptions["CustomerID"], mean)
subscriptionsBT = merge(subscriptionsBT, meanNbrStartNP, by="CustomerID")

# RenewalDate
?

# Number of payment type
nbrPaymentType = aggregate(subscriptions$PaymentType, by=list(CustomerID=subscriptions$CustomerID), table)
nbrPaymentType = sapply(nbrPaymentType, unlist)
subscriptionsBT = merge(subscriptionsBT, nbrPaymentType, by="CustomerID")

# Number of payment status
nbrPaymentStatus = aggregate(subscriptions$PaymentStatus, by=list(CustomerID=subscriptions$CustomerID), table)
nbrPaymentStatus = t(apply(nbrPaymentStatus, 1, function(x) x))
subscriptionsBT = merge(subscriptionsBT, nbrPaymentStatus, by="CustomerID")

# Mean time before payment
subscriptions["PaymentTime"] <- subscriptions["PaymentDate"] - subscriptions["StartDate"]
meanPayTime= aggregate(list(meanPayTime=subscriptions$PaymentTime), subscriptions["CustomerID"], mean)
subscriptionsBT = merge(subscriptionsBT, meanPayTime, by="CustomerID")

# Mean net formula price
meanNetForPrice = aggregate(list(meanNetForPrice=subscriptions$NetFormulaPrice), subscriptions["CustomerID"], mean)
subscriptionsBT = merge(subscriptionsBT, meanNetForPrice, by="CustomerID")

# Mean total credit
meanTotCredit = aggregate(list(meanTotCredit=subscriptions$TotalCredit), subscriptions["CustomerID"], mean)
subscriptionsBT = merge(subscriptionsBT, meanTotCredit, by="CustomerID")

########
# Base table generation: credit
########

########
# Base table generation: complaints
########

########
# Base table generation: delivery
########

########
# Base table generation: formula
########

