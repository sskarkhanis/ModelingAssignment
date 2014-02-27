#install.packages("aCRM")
install.packages("dummies", repos="http://cran.rstudio.com/") 
library("dummies")
library("aCRM")
library("randomForest")
#packages = c("aCRM", "sdsd")
#library(l[1], character.only=TRUE)

# Functions
mvProp <- function(v){
  sum(is.na(v))/length(v)
}
get("credit")setwd('/home/xclyde/Courses/UGENT/Modeling/Assignment')

# Complaints import
complaints <- read.table("complaints.txt", header=TRUE, sep=";", colClasses=c("character", "character", "character", "character", "character", "character", "character"))
#dim(complaints)
#str(complaints)
#head(complaints)
#tail(complaints)
#colnames(complaints)

# Credit import
credit <- read.table("credit.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "character", "factor", "numeric", "integer"))
#dim(credit)
#str(credit)
#head(credit)
#tail(credit)
#colnames(credit)

# Customers import
customers <- read.table("customers.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "character", "character", "character", "character"))
#dim(customers)
#str(customers)
#head(customers)
#tail(customers)
#colnames(customers)

# Delivery import
delivery <- read.table("delivery.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "factor", "factor", "character", "character"))
#dim(delivery)
#str(delivery)
#head(delivery)
#tail(delivery)
#colnames(delivery)

# Formula import
formula <- read.table("formula.txt", header=TRUE, sep=";", colClasses=c("character", "character", "factor", "integer"))
#dim(formula)
#str(formula)
#head(formula)
#tail(formula)
#colnames(formula)

# Subscriptions import
subscriptions <- read.table("subscriptions.txt", header=TRUE, sep=";", colClasses=c("character","character","character","character","character","character","integer","integer","character","factor","factor","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
#dim(subscriptions)
#str(subscriptions)
#head(subscriptions)
#tail(subscriptions)
#colnames(subscriptions)

tables = list(complaints, credit, customers, delivery, formula, subscriptions)

StartTime = Sys.time()
########
# Base table generation: customers
########

customersBaseTable = customers
predictors = list("Gender", "DOB", "District", "ZIP", "StreeID")
for (j in predictors){
  customersBaseTable[paste("MV", j, sep="")] <- as.factor(is.na(customers[j]))
}
#Only gender values are missing
customers <- imputeMissings(customers)
#customers[, names(customers) == 'Gender'] <- imputeMissings(customers[, names(customers) == 'Gender'])
########
# Base table generation: subscriptions
########

# Missing values

subscriptions[subscriptions==""]  <- NA

subscriptions$StartDate <- as.Date(subscriptions$StartDate,"%d/%m/%Y")
subscriptions$EndDate <- as.Date(subscriptions$EndDate,"%d/%m/%Y")
subscriptions$RenewalDate <- as.Date(subscriptions$RenewalDate,"%d/%m/%Y")
subscriptions$PaymentDate <- as.Date(subscriptions$PaymentDate,"%d/%m/%Y")

# Columns for which the missing values should be replaced by mean/mode
missToMean = c("StartDate", "EndDate", "PaymentType", "PaymentStatus")

# Columns for which the missing values should be replaced by 0
missToZero = c("NbrNewspapers", "NbrStart", "NetFormulaPrice", "TotalCredit")

# Columns for which the missing values should be replaced by something else
missOther = c("RenewalDate", "PaymentDate")

predictors = c(missToMean, missToZero, missOther)
predTable = subset(subscriptions, select=(predictors))
mvVars = sapply(predictors, function(x) paste("MV",x,sep=""))
mvTable = aggregate(predTable, subscriptions["CustomerID"], function(x) round(sum(is.na(x))/length(x),2))
subscriptionsBT = setNames(mvTable, c("CustomerID", mvVars))

# Impute the 0
sub = subset(subscriptions, select=missToZero)
sub[is.na(sub) == "TRUE"] <- 0 
subscriptions[, names(subscriptions) %in% missToZero] <- sub 

# Impute the mean and mode
subscriptions[, names(subscriptions) %in% missToMean] <- imputeMissings(subscriptions[, names(subscriptions) %in% missToMean])

# Impute missing renewal dates with end dates
subscriptions$RenewalDate[is.na(subscriptions$RenewalDate)] <- subscriptions$EndDate[is.na(subscriptions$RenewalDate)]

# Impute missing payment dates with end dates
subscriptions$PaymentDate[is.na(subscriptions$PaymentDate)] <- subscriptions$EndDate[is.na(subscriptions$PaymentDate)]

# Remove missing values indicators when there are no missing values
for(p in predictors){
  if(sum(subscriptionsBT[paste("MV",p,sep="")])==0) subscriptionsBT[paste("MV",p,sep="")] <- NULL
}


# Number of subscriptions per customers
subscriptionsNb = aggregate(list(subscriptionsNb=subscriptions$StartDate), subscriptions["CustomerID"], length)
subscriptionsBT = merge(subscriptionsBT, subscriptionsNb, by="CustomerID")

# Mean subscription duration
subscriptions["Duration"] <- subscriptions["EndDate"] - subscriptions["StartDate"]
meanSubDuration = aggregate(list(meanSubDuration=subscriptions$Duration), subscriptions["CustomerID"], mean)
meanSubDuration[,"meanSubDuration"] = round(meanSubDuration[,"meanSubDuration"], 2)
subscriptionsBT = merge(subscriptionsBT, meanSubDuration, by="CustomerID")

# Mean number of newspapers
meanNbrNP = aggregate(list(meanNbrNP=subscriptions$NbrNewspapers), subscriptions["CustomerID"], mean)
meanNbrNP[,"meanNbrNP"] = round(meanNbrNP[,"meanNbrNP"], 2)
subscriptionsBT = merge(subscriptionsBT, meanNbrNP, by="CustomerID")

# Mean number of start newspapers
meanNbrStartNP = aggregate(list(meanNbrStartNP=subscriptions$NbrStart), subscriptions["CustomerID"], mean)
meanNbrStartNP[,"meanNbrStartNP"] = round(meanNbrStartNP[,"meanNbrStartNP"], 2)
subscriptionsBT = merge(subscriptionsBT, meanNbrStartNP, by="CustomerID")

# RenewalDate
#?

# Number of payment type
nbrPaymentType = aggregate(subscriptions$PaymentType, subscriptions["CustomerID"], table)
nbrPaymentType = sapply(nbrPaymentType, unlist)
subscriptionsBT = merge(subscriptionsBT, nbrPaymentType, by="CustomerID")

# Number of payment status
nbrPaymentStatus = aggregate(subscriptions$PaymentStatus, subscriptions["CustomerID"], table)
nbrPaymentStatus = sapply(nbrPaymentStatus, unlist)
subscriptionsBT = merge(subscriptionsBT, nbrPaymentStatus, by="CustomerID")

# Mean time before payment
subscriptions["PaymentTime"] <- subscriptions["PaymentDate"] - subscriptions["StartDate"]
meanPayTime= aggregate(list(meanPayTime=subscriptions$PaymentTime), subscriptions["CustomerID"], mean)
meanPayTime[,"meanPayTime"] = round(meanPayTime[,"meanPayTime"], 2)
subscriptionsBT = merge(subscriptionsBT, meanPayTime, by="CustomerID")

# Mean net formula price
meanNetForPrice = aggregate(list(meanNetForPrice=subscriptions$NetFormulaPrice), subscriptions["CustomerID"], mean)
meanNetForPrice[,"meanNetForPrice"] = round(meanNetForPrice[,"meanNetForPrice"], 2)
subscriptionsBT = merge(subscriptionsBT, meanNetForPrice, by="CustomerID")

# Mean total credit
meanTotCredit = aggregate(list(meanTotCredit=subscriptions$TotalCredit), subscriptions["CustomerID"], mean)
meanTotCredit[,"meanTotCredit"] = round(meanTotCredit[,"meanTotCredit"], 2)
subscriptionsBT = merge(subscriptionsBT, meanTotCredit, by="CustomerID")

########
# Base table generation: credit
########

credit$ProcessingDate <- as.Date(credit$ProcessingDate,"%d/%m/%Y")

# Missing values
credit[credit==""]  <- NA

# Columns for which the missing values should be replaced by mean/mode
missToMean = c("ActionType", "CreditSource")

# Columns for which the missing values should be replaced by 0
missToZero = c("Amount")

# Per subscriptions: Missing values
predictors = c(missToMean, missToZero)
predTable = subset(credit, select=(predictors))
mvVars = sapply(predictors, function(x) paste("MV",x,sep=""))
mvTable = aggregate(predTable, credit["SubscriptionID"], function(x) round(sum(is.na(x))/length(x),2))
creditBT = setNames(mvTable, c("SubscriptionID", mvVars))

#credit <- imputeMissings(credit)

# Impute the 0
sub = subset(credit, select=missToZero)
sub[is.na(sub) == "TRUE"] <- 0 
credit[, names(credit) %in% missToZero] <- sub 

# Impute the mean and mode
credit[, names(credit) %in% missToMean] <- imputeMissings(credit[, names(credit) %in% missToMean])

# Remove missing values indicators when there are no missing values
for(p in predictors){
  if(sum(creditBT[paste("MV",p,sep="")])==0) creditBT[paste("MV",p,sep="")] <- NULL
}

# Per subscriptions: Number of credits
creditNb = aggregate(list(creditNb=credit$ActionType), credit["SubscriptionID"], length)
creditBT = merge(creditBT, creditNb, by="SubscriptionID")

# Per subscriptions: Number of action type
nbrCreActionType = aggregate(credit$ActionType, credit["SubscriptionID"], table)
nbrCreActionType = sapply(nbrCreActionType, unlist)
creditBT = merge(creditBT, nbrCreActionType, by="SubscriptionID")

# Per subscriptions: Number of credit source
nbrCreSource = aggregate(credit$CreditSource, credit["SubscriptionID"], table)
nbrCreSource = sapply(nbrCreSource, unlist)
creditBT = merge(creditBT, nbrCreSource, by="SubscriptionID")

# Per subscriptiosn: Total amount
TotCreAmount = aggregate(list(TotCreAmount=credit$Amount), credit["SubscriptionID"], sum)
TotCreAmount[,"TotCreAmount"] = round(TotCreAmount[,"TotCreAmount"], 2)
creditBT = merge(creditBT, TotCreAmount, by="SubscriptionID")

# Per customers: Total credits number, amount, action types and sources
subSub = subset(subscriptions, select=c("SubscriptionID", "CustomerID"))
creditBT = merge(creditBT, subSub, by="SubscriptionID")
subCre = creditBT[!(names(creditBT) %in% c("CustomerID", "SubscriptionID"))]
creditBT = aggregate(subCre, creditBT["CustomerID"], sum)

########
# Base table generation: delivery
########

# Missing values
delivery[delivery==""]  <- NA

# Per subscriptions: Missing values
predictors = c("DeliveryType", "DeliveryClass", "DeliveryContext")
predTable = subset(delivery, select=(predictors))
mvVars = sapply(predictors, function(x) paste("MV",x,sep=""))
mvTable = aggregate(predTable, delivery["SubscriptionID"], function(x) round(sum(is.na(x))/length(x),2))
deliveryBT = setNames(mvTable, c("SubscriptionID", mvVars))

# Impute the mean and mode
delivery[, names(delivery) %in% predictors] <- imputeMissings(delivery[, names(delivery) %in% predictors])

# Remove missing values indicators when there are no missing values
for(p in predictors){
  if(sum(deliveryBT[paste("MV",p,sep="")])==0) deliveryBT[paste("MV",p,sep="")] <- NULL
}

# Per subscriptions: Number of deliveries
deliveryNb = aggregate(list(deliveryNb=delivery$DeliveryType), delivery["SubscriptionID"], length)

# Per subscriptions: Number of delivery type
#nbrDelType = aggregate(delivery$DeliveryType, delivery["SubscriptionID"], table)
#nbrDelType = sapply(nbrDelType, unlist)
#deliveryBT = merge(deliveryBT, nbrDelType, by="SubscriptionID")

# Per subscriptions: Number of delivery class
#nbrDelClass = aggregate(delivery$DeliveryClass, delivery["SubscriptionID"], table)
#nbrDelClass = sapply(nbrDelClass, unlist)
#deliveryBT = merge(deliveryBT, nbrDelClass, by="SubscriptionID")

# Per subscriptions: Number of delivery context
#nbrDelContext = aggregate(delivery$DeliveryContext, delivery["SubscriptionID"], table)
#nbrDelContext = sapply(nbrDelContext, unlist)
#deliveryBT = merge(deliveryBT, nbrDelContext, by="SubscriptionID")

# Per subscriptions: Number of delivery type, class and context
subDel = delivery[(names(delivery) %in% c("DeliveryType", "DeliveryClass", "DeliveryContext"))]
deliveryBT = aggregate(subDel, delivery["SubscriptionID"], table)
deliveryBT = sapply(deliveryBT, unlist)
deliveryBT = Reduce(function(x, y) merge(x, y, by="SubscriptionID"), list(deliveryNb, deliveryBT))

# Per customers: Total delivery number, types, class and context
subSub = subset(subscriptions, select=c("SubscriptionID", "CustomerID"))
deliveryBT = merge(deliveryBT, subSub, by="SubscriptionID")
subDel = deliveryBT[!(names(deliveryBT) %in% c("CustomerID", "SubscriptionID"))]
deliveryBT = aggregate(subDel, deliveryBT["CustomerID"], sum)

########
# Base table generation: complaints
########

########
# Base table generation: formula
########

########
# Base table generation: global merging
########

subBaseTable = list(subscriptionsBT, creditBT, deliveryBT)
baseTable = Reduce(function(x, y) merge(x, y, by='CustomerID', all=TRUE), subBaseTable)
baseTable <- imputeMissings(baseTable)
print(Sys.time()-StartTime)
