#install.packages("")
packages = c("aCRM", "dummies", "randomForest", "rpart")
for (p in packages){
  require(p, character.only=TRUE)
}

# Functions
mvProp <- function(v){
  sum(is.na(v))/length(v)
}

# Complaints import
complaints <- read.table("complaints.txt", header=TRUE, sep=";", colClasses=c("character", "character", "character", "character", "factor", "factor", "factor"))
# Credit import
credit <- read.table("credit.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "character", "factor", "numeric", "integer"))
# Customers import
customers <- read.table("customers.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "character", "character", "character", "character"))
# Delivery import
delivery <- read.table("delivery.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "factor", "factor", "character", "character"))
#delivery <- delivery[1:(nrow(delivery)*0.2),]
# Formula import
formula <- read.table("formula.txt", header=TRUE, sep=";", colClasses=c("character", "character", "factor", "factor"))
# Subscriptions import
subscriptions <- read.table("subscriptions.txt", header=TRUE, sep=";", colClasses=c("character","character","character","character","character","character","integer","integer","character","factor","factor","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

tables = list(complaints, credit, customers, delivery, formula, subscriptions)

StartTime = Sys.time()

########
# Time windows
########

# setting the format for date
DateFormat <- "%d/%m/%Y"

#start of indep period
StartIP <- as.Date("02/01/2006", DateFormat)

#end of indep period
EndIP <- as.Date("28/12/2010", DateFormat)

#keeping a gap of 1 month between the Indep & Dep Periods

#dependent time period of 1 year
#start of dep period
StartDP <- as.Date("29/01/2011", DateFormat)

#end of dep period
EndDP <- as.Date("28/01/2012", DateFormat)

########
# Base table generation: customers
########

#Step 01 the customers have to be active during the end of the independent period

#active_Customers <- data.frame(subset(subscriptions, (StartDate <= end_IP & (EndDate > start_DP | EndDate==NA)),select = CustomerID))

subscriptions[subscriptions==""]  <- NA
subscriptions$StartDate <- as.Date(subscriptions$StartDate,DateFormat)
subscriptions$EndDate <- as.Date(subscriptions$EndDate,DateFormat)
subscriptions$RenewalDate <- as.Date(subscriptions$RenewalDate,DateFormat)
subscriptions$PaymentDate <- as.Date(subscriptions$PaymentDate,DateFormat)

firstStartDate = aggregate(subscriptions["StartDate"], subscriptions['CustomerID'], min)
lastEndDate = aggregate(subscriptions["EndDate"], subscriptions['CustomerID'], max)
allCustomers = merge(firstStartDate, lastEndDate, by='CustomerID')
activeCustomers = subset(allCustomers,(StartDate <= EndIP & EndDate >= StartDP))
churnedCustomers = subset(activeCustomers, EndDate<= EndDP, select="CustomerID")
churnedCustomers["Churn"] = 1
activeCustomers = merge(activeCustomers["CustomerID"], churnedCustomers[c("CustomerID", "Churn")], by="CustomerID", all.x=TRUE)
activeCustomers["Churn"][is.na(activeCustomers["Churn"])] <- 0 
activeCustomers = merge(activeCustomers, customers, by="CustomerID")

activeCustomers$DOB <- as.Date(activeCustomers$DOB,DateFormat)

predictors = list("Gender", "DOB", "District")
customersBT = subset(activeCustomers, select=(c("CustomerID", "Churn", unlist(predictors))))
for (j in predictors){
  customersBT[paste("MV", j, sep="")] <- as.factor(is.na(activeCustomers[j]))
}
customersBT = imputeMissings(customersBT)
customersBT$Age = Sys.Date() - customersBT$DOB
customersBT$DOB <- NULL

customersBT = dummy.data.frame(customersBT, c("Gender", "District"), sep='_')


########
# Base table generation: subscriptions
########

# Keep active subscriptions
subscriptions = merge(subscriptions, customersBT["CustomerID"], by="CustomerID")
# Keep subscriptions starting within the independent period
subscriptions = subset(subscriptions, StartDate <= EndIP)
activeSubscriptions = subscriptions["SubscriptionID"]

# Missing values

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
subscriptionsNb = aggregate(list(SubscriptionsNb=subscriptions$StartDate), subscriptions["CustomerID"], length)
subscriptionsBT = merge(subscriptionsBT, subscriptionsNb, by="CustomerID")

# Mean subscription duration
subscriptions["Duration"] <- subscriptions["EndDate"] - subscriptions["StartDate"]
totSubDuration = aggregate(list(TotSubDuration=subscriptions$Duration), subscriptions["CustomerID"], sum)
totSubDuration[,"TotSubDuration"] = round(totSubDuration[,"TotSubDuration"], 2)
subscriptionsBT = merge(subscriptionsBT, totSubDuration, by="CustomerID")

# Mean number of newspapers
meanNbrNP = aggregate(list(MeanNbrNP=subscriptions$NbrNewspapers), subscriptions["CustomerID"], mean)
meanNbrNP[,"MeanNbrNP"] = round(meanNbrNP[,"MeanNbrNP"], 2)
subscriptionsBT = merge(subscriptionsBT, meanNbrNP, by="CustomerID")

# Mean number of start newspapers
meanNbrStartNP = aggregate(list(MeanNbrStartNP=subscriptions$NbrStart), subscriptions["CustomerID"], mean)
meanNbrStartNP[,"MeanNbrStartNP"] = round(meanNbrStartNP[,"MeanNbrStartNP"], 2)
subscriptionsBT = merge(subscriptionsBT, meanNbrStartNP, by="CustomerID")

# Mean renewal duration
subscriptions["RenewDuration"] = subscriptions["EndDate"] - subscriptions["RenewalDate"]
meanRenewDuration = aggregate(list(MeanRenewDuration=subscriptions$RenewDuration), subscriptions["CustomerID"], mean)
meanRenewDuration[,"MeanRenewDuration"] = round(meanRenewDuration[,"MeanRenewDuration"], 2)
subscriptionsBT = merge(subscriptionsBT, meanRenewDuration, by="CustomerID")
subscriptionsBT["MeanRenewDuration"] = as.numeric(subscriptionsBT$MeanRenewDuration) / as.numeric(subscriptionsBT$TotSubDuration) 

# Number of payment type
nbrPaymentType = aggregate(subscriptions["PaymentType"], subscriptions["CustomerID"], table)
nbrPaymentType = sapply(nbrPaymentType, unlist)
subscriptionsBT = merge(subscriptionsBT, nbrPaymentType, by="CustomerID")

# Number of payment status
nbrPaymentStatus = aggregate(subscriptions["PaymentStatus"], subscriptions["CustomerID"], table)
nbrPaymentStatus = sapply(nbrPaymentStatus, unlist)
subscriptionsBT = merge(subscriptionsBT, nbrPaymentStatus, by="CustomerID")

# Mean time before payment
subscriptions["PaymentTime"] <- subscriptions["PaymentDate"] - subscriptions["StartDate"]
meanPayTime= aggregate(list(MeanPayTime=subscriptions$PaymentTime), subscriptions["CustomerID"], mean)
meanPayTime[,"MeanPayTime"] = round(meanPayTime[,"MeanPayTime"], 2)
subscriptionsBT = merge(subscriptionsBT, meanPayTime, by="CustomerID")

# Mean and total net formula price
meanNetForPrice = aggregate(list(MeanNetForPrice=subscriptions$NetFormulaPrice), subscriptions["CustomerID"], mean)
meanNetForPrice[,"MeanNetForPrice"] = round(meanNetForPrice[,"MeanNetForPrice"], 2)
totNetForPrice = aggregate(list(totNetForPrice=subscriptions$NetFormulaPrice), subscriptions["CustomerID"], sum)
subscriptionsBT = Reduce(function(x, y) merge(x, y, by="CustomerID"), list(subscriptionsBT, meanNetForPrice, totNetForPrice))

# Mean total credit
meanTotCredit = aggregate(list(MeanTotCredit=subscriptions$TotalCredit), subscriptions["CustomerID"], mean)
meanTotCredit[,"MeanTotCredit"] = round(meanTotCredit[,"MeanTotCredit"], 2)
subscriptionsBT = merge(subscriptionsBT, meanTotCredit, by="CustomerID")

########
# Base table generation: credit
########

credit[credit==""]  <- NA

credit$ProcessingDate <- as.Date(credit$ProcessingDate, DateFormat)

# Keep active credits
credit = merge(credit, activeSubscriptions, by="SubscriptionID")
# Keep credit starting within the independent period
credit = subset(credit, ProcessingDate <= EndIP)

# Missing values

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
creditNb = aggregate(list(CreditNb=credit$ActionType), credit["SubscriptionID"], length)
creditBT = merge(creditBT, creditNb, by="SubscriptionID")

# Per subscriptions: Number of action type
nbrCreActionType = aggregate(credit["ActionType"], credit["SubscriptionID"], table)
nbrCreActionType = sapply(nbrCreActionType, unlist)
creditBT = merge(creditBT, nbrCreActionType, by="SubscriptionID")

# Per subscriptions: Number of credit source
nbrCreSource = aggregate(credit["CreditSource"], credit["SubscriptionID"], table)
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

delivery[delivery==""]  <- NA

delivery$StartDate <- as.Date(delivery$StartDate, DateFormat)

# Keep active deliveries
delivery = merge(delivery, activeSubscriptions, by="SubscriptionID")
# Keep deliveries starting within the independent period
delivery = subset(delivery, StartDate <= EndIP)

# Missing values

# Delete the empty levels in the factor variables 
for(n in names(delivery)){
  if(is.factor(delivery[,n])) delivery[,n] <- factor(delivery[,n])
}

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
deliveryNb = aggregate(list(DeliveryNb=delivery$DeliveryType), delivery["SubscriptionID"], length)

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

complaints[complaints==""]  <- NA

complaints$ComplaintDate <- as.Date(complaints$ComplaintDate, DateFormat)

# Keep active complaints
complaints = merge(complaints, activeCustomers["CustomerID"], by="CustomerID")
# Keep complaints starting within the independent period
complaints = subset(complaints, ComplaintDate <= EndIP)

# Missing values

# Per subscriptions: Missing values
missToMean = c("ComplaintType", "SolutionType")
predTable = subset(complaints, select=(missToMean))
mvVars = sapply(missToMean, function(x) paste("MV",x,sep=""))
mvTable = aggregate(predTable, complaints["CustomerID"], function(x) round(sum(is.na(x))/length(x),2))
complaintsBT = setNames(mvTable, c("CustomerID", mvVars))

# Impute the mean and mode
complaints[, names(complaints) == "MVComplaintType"] <- imputeMissings(complaints[, names(complaints) == "MVComplaintType"])

# Finding Nr of Complaints per customer
complaintsNb <- data.frame(table(complaints$CustomerID))
colnames(complaintsNb)[1:2] <- c("CustomerID", "ComplaintsNb")
complaintsBT = merge(complaintsBT, complaintsNb, by="CustomerID")

#Finding #days since last complaint
lastComDates <- aggregate(list(LastComDate=complaints$ComplaintDate), complaints['CustomerID'], max)
lastComDates$DaysSinceLastCom <- EndIP - lastComDates$LastComDate
lastComDates$LastComDate = NULL
complaintsBT = merge(complaintsBT, lastComDates, by="CustomerID")

# % of complaints solved = nb of complaints solved / nb of complaints
comSolvedNb = aggregate(list(ComSolvedNb=complaints$SolutionType), complaints['CustomerID'], function(x) sum(!is.na(x)))
complaintsBT = merge(complaintsBT, comSolvedNb, by="CustomerID")
complaintsBT$ComSolvingPercent = round(complaintsBT$ComSolvedNb / complaintsBT$ComplaintsNb, 2)
complaintsBT$ComSolvedNb = NULL

# Remove missing values indicators when there are no missing values
for(p in missToMean){
  if(sum(complaintsBT[paste("MV",p,sep="")])==0) complaintsBT[paste("MV",p,sep="")] <- NULL
}

# Impute the mean and mode
complaints[, names(complaints) == "MVSolutionType"] <- imputeMissings(complaints[, names(complaints) == "MVSolutionType"])

# Number of complaint type
nbrComplaintType = aggregate(complaints["ComplaintType"], complaints["CustomerID"], table)
nbrComplaintType = sapply(nbrComplaintType, unlist)
complaintsBT = merge(complaintsBT, nbrComplaintType, by="CustomerID")

# Number of solution type
nbrSolutionType = aggregate(complaints["SolutionType"], complaints["CustomerID"], table)
nbrSolutionType = sapply(nbrSolutionType, unlist)
complaintsBT = merge(complaintsBT, nbrSolutionType, by="CustomerID")

########
# Base table generation: formula
########

# Missing values
tmp_subs_formula = subset(subscriptions, select = c(SubscriptionID, StartDate, EndDate,CustomerID ,FormulaID))

formula[formula==""]  <- NA

#merging the tmp_subs_formula and formula data frames
merged_subs_formula <- merge(tmp_subs_formula, formula, by = "FormulaID")

# Missing values
predictors = c("FormulaType", "Duration")
predTable = subset(merged_subs_formula, select=(predictors))
mvVars = sapply(predictors, function(x) paste("MV",x,sep=""))
mvTable = aggregate(predTable, merged_subs_formula["CustomerID"], function(x) round(sum(is.na(x))/length(x),2))
formulaBT = setNames(mvTable, c("CustomerID", mvVars))

# Impute the mean and mode
merged_subs_formula[, names(merged_subs_formula) %in% predictors] <- imputeMissings(merged_subs_formula[, names(merged_subs_formula) %in% predictors])

# Remove missing values indicators when there are no missing values
for(p in predictors){
  if(sum(formulaBT[paste("MV",p,sep="")])==0) formulaBT[paste("MV",p,sep="")] <- NULL
}

#calculating aggegrate values for columns Duration and FormulaType
aggr_ct_reg_cam <- aggregate(merged_subs_formula["FormulaType"], merged_subs_formula["CustomerID"], table)
aggr_ct_reg_cam <- sapply(aggr_ct_reg_cam,unlist)

aggr_ct_Duration <- aggregate(merged_subs_formula["Duration"], merged_subs_formula["CustomerID"], table)
aggr_ct_Duration <- as.data.frame(sapply(aggr_ct_Duration,unlist))

#summary table containing one row per customer from formula table
tables = list(formulaBT, aggr_ct_reg_cam, aggr_ct_Duration)
formulaBT = Reduce(function(x, y) merge(x, y, by='CustomerID'), tables)

########
# Base table generation: global merging
########

subBaseTable = list(customersBT, subscriptionsBT, creditBT, deliveryBT, complaintsBT, formulaBT)
baseTable = Reduce(function(x, y) merge(x, y, by='CustomerID', all=TRUE), subBaseTable)
baseTable[is.na(baseTable)]  <- 0
print(Sys.time()-StartTime)

########
# Modeling
########

trainind <- sample(1:nrow(baseTable),round(0.5*nrow(baseTable)))

trainTable <- baseTable[trainind,]
testTable <- baseTable[-trainind,]
trainTable$CustomerID <- NULL
testTable$CustomerID <- NULL

BDT <- rpart(Churn ~ ., control=rpart.control(cp = 0.001), trainTable)
#table(predTree <- predict(BDT, testTable)[,2])
