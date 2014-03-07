#install.packages("rattle")
packages = c("aCRM", "dummies", "randomForest", "rpart", "ROCR", "ada", "randomForest", "pROC", "AUC", "FNN", "glmnet", "e1071", "nnet", "kernelFactory", "rattle")
for (p in packages){
  require(p, character.only=TRUE)
}
source("tuneMember.R")

# Compute the proportions of missing values
mvProp <- function(x){
  round(sum(is.na(x))/length(x), 2)
}

aggr <- function(sourceTable, destTable, col, newName, byCol, func){
  funcExpr = parse(text=func)
  aggrTable = unique(sourceTable[byCol])
  if(func=="table"){
    values = unique(sourceTable[, col])
    sourceTable = dummy.data.frame(sourceTable, col, sep='_')
    for (v in values){
      colName = paste(col, v, sep="_")
      resTable = tapply(sourceTable[, colName], sourceTable[, byCol], sum)
      aggrTable[colName] = resTable
      aggrTable[byCol] = row.names(resTable)
    }
    row.names(aggrTable) = NULL
  }
  else {
    resTable = tapply(sourceTable[, col], sourceTable[, byCol], eval(funcExpr))
    aggrTable[newName] = resTable
    aggrTable[byCol] = row.names(resTable)
    if(is.double(aggrTable[, newName])) aggrTable[, newName] = round(aggrTable[, newName], 2)
  }
  merge(destTable, aggrTable, all.x=TRUE)
}

# values = unique(delivery$DeliveryType)
# bb = dummy.data.frame(delivery, "DeliveryType", sep='_')
# del = unique(delivery["SubscriptionID"])
# for(v in values){
#   del[paste(v)] = tapply(bb[,paste("DeliveryType",v,sep="_")], bb$SubscriptionID, funcExpr)
# }
# row.names(del) = NULL

# Complaints import
complaints <- read.table("complaints.txt", header=TRUE, sep=";", colClasses=c("character", "character", "character", "character", "factor", "factor", "factor"), na.string=c("",".","NA"))
# Credit import
credit <- read.table("credit.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "character", "factor", "numeric", "integer"), na.string=c("",".","NA"))
# Customers import
customers <- read.table("customers.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "character", "character", "character", "character"), na.string=c("",".","NA"))
# Delivery import
delivery <- read.table("delivery.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "factor", "factor", "character", "character"), na.string=c("",".","NA"))
# Formula import
formula <- read.table("formula.txt", header=TRUE, sep=";", colClasses=c("character", "character", "factor", "factor"), na.string=c("",".","NA"))
# Subscriptions import
subscriptions <- read.table("subscriptions.txt", header=TRUE, sep=";", colClasses=c("character","character","character","character","character","character","integer","integer","character","factor","factor","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), na.string=c("",".","NA"))

tables = list(complaints, credit, customers, delivery, formula, subscriptions)

StartTime = Sys.time()

########
# Time windows
########

# setting the format for date
dateFormat <- "%d/%m/%Y"

#start of indep period
startIP <- as.Date("02/01/2006", dateFormat)
#end of indep period
endIP <- as.Date("01/02/2010", dateFormat)

#dependent time period of 1 year
#start of dep period
startDP <- as.Date("03/02/2010", dateFormat)
#end of dep period
endDP <- as.Date("03/02/2011", dateFormat)

########
# Base table generation: customers
########

#Step 01 the customers have to be active during the end of the independent period

dates = c("StartDate","EndDate","PaymentDate","RenewalDate")
subscriptions[,dates] <- sapply(dates,function(vars1) as.Date(subscriptions[,vars1], format=dateFormat), simplify=FALSE)

activeCustomers = unique(subscriptions[subscriptions$StartDate<=endIP & subscriptions$EndDate>startDP, "CustomerID"])
activeSubs <- subscriptions[subscriptions$CustomerID %in% activeCustomers,]
activeSubs$NonChurn <- ifelse((activeSubs$StartDate >= startDP & activeSubs$StartDate <= endDP), 1, 0)
churners = aggregate(list(NonChurn=activeSubs$NonChurn), activeSubs["CustomerID"], sum)
churners$Churn <- as.factor(ifelse(churners$NonChurn==0,1,0))
customers = merge(customers, churners[c("CustomerID", "Churn")], by="CustomerID")

customers$Age = endIP - as.Date(customers$DOB, dateFormat)

predictors = c("Gender", "Age", "District")
customersMV = sapply(customers[predictors], function(x) (!complete.cases(x))+0)
mvVars = sapply(predictors, function(x) paste("MV",x,sep=""))
colnames(customersMV) = mvVars

customersBT = subset(customers, select=c("CustomerID", "Churn", predictors))
customersBT = cbind(customersBT, customersMV)

# Remove missing values indicators when there are no missing values
for (p in predictors){
  if(sum(customersBT[paste("MV", p, sep="")])==0) customersBT[paste("MV",p,sep="")] <- NULL
}

customersBT[predictors] = imputeMissings(customersBT[predictors])
customersBT = dummy.data.frame(customersBT, c("Gender", "District"), sep='_')

########
# Base table generation: subscriptions
########

# Keep active subscriptions
subscriptions = merge(subscriptions, customersBT["CustomerID"], by="CustomerID")
# Keep subscriptions starting within the independent period
subscriptions = subset(subscriptions, StartDate <= endIP)
activeSubscriptions = subscriptions["SubscriptionID"]

# Missing values

# Columns for which the missing values should be replaced by mean/mode
missToMean = c("StartDate", "EndDate", "PaymentType", "PaymentStatus")

# Columns for which the missing values should be replaced by 0
missToZero = c("NbrNewspapers", "NbrStart", "NetFormulaPrice", "TotalCredit")

# Columns for which the missing values should be replaced by something else
missOther = c("RenewalDate", "PaymentDate")

predictors = c(missToMean, missToZero, missOther)
subscriptionsBT = unique(subscriptions["CustomerID"])
for(p in predictors){
  subscriptionsBT = aggr(subscriptions, subscriptionsBT, p, paste("MV",p,sep=""), "CustomerID", "mvProp")
}

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
  if(sum(subscriptionsBT[paste("MV", p, sep="")])==0) subscriptionsBT[paste("MV",p,sep="")] <- NULL
}

# Number of subscriptions per customers
subscriptionsNb <- data.frame(table(subscriptions$CustomerID))
colnames(subscriptionsNb)[1:2] <- c("CustomerID", "SubscriptionsNb")
subscriptionsBT = merge(subscriptionsBT, subscriptionsNb, by="CustomerID")

# Total subscription duration
subscriptions["Duration"] <- subscriptions["EndDate"] - subscriptions["StartDate"]
subscriptionsBT = aggr(subscriptions, subscriptionsBT, "Duration", "TotSubDuration", "CustomerID", "sum")

# Mean number of newspapers
subscriptionsBT = aggr(subscriptions, subscriptionsBT, "NbrNewspapers", "MeanNbrNP", "CustomerID", "mean")

# Mean number of start newspapers
subscriptionsBT = aggr(subscriptions, subscriptionsBT, "NbrStart", "MeanNbrStartNP", "CustomerID", "mean")

subscriptionsRenew = subset(subscriptions, RenewalDate <= endIP, select=c("CustomerID", "EndDate", "RenewalDate"))

# Mean renewal duration
subscriptionsRenew["RenewDuration"] = subscriptionsRenew["EndDate"] - subscriptionsRenew["RenewalDate"]
subscriptionsBT = aggr(subscriptionsRenew, subscriptionsBT, "RenewDuration", "MeanRenewDuration", "CustomerID", "mean")
subscriptionsBT$MeanRenewDuration = round(as.numeric(subscriptionsBT$MeanRenewDuration) / as.numeric(subscriptionsBT$TotSubDuration), 2)

# Time since last renewal
subscriptionsBT = aggr(subscriptionsRenew, subscriptionsBT, "RenewalDate", "TimeSinceLastRen", "CustomerID", "function(x) endIP-max(x)")

# Total Number of Renewals per Customer
subscriptionsBT = aggr(subscriptionsRenew, subscriptionsBT, "RenewalDate", "TotNbrRenew", "CustomerID", "length")

# Number of payment type
subscriptionsBT = aggr(subscriptions, subscriptionsBT, "PaymentType", "PaymentType", "CustomerID", "table")

# Number of payment status
subscriptionsBT = aggr(subscriptions, subscriptionsBT, "PaymentStatus", "PaymentStatus", "CustomerID", "table")

# Mean time before payment
subscriptions["PaymentTime"] <- subscriptions["PaymentDate"] - subscriptions["StartDate"]
subscriptionsBT = aggr(subscriptions, subscriptionsBT, "PaymentTime", "MeanPayTime", "CustomerID", "mean")

# Mean and total net formula price
subscriptionsBT = aggr(subscriptions, subscriptionsBT, "NetFormulaPrice", "MeanNetForPrice", "CustomerID", "mean")
subscriptionsBT = aggr(subscriptions, subscriptionsBT, "NetFormulaPrice", "TotNetForPrice", "CustomerID", "sum")

# Mean total credit
subscriptionsBT = aggr(subscriptions, subscriptionsBT, "TotalCredit", "MeanTotCredit", "CustomerID", "mean")

########
# Base table generation: credit
########

credit$ProcessingDate <- as.Date(credit$ProcessingDate, dateFormat)

# Keep active credits
credit = merge(credit, activeSubscriptions, by="SubscriptionID")
# Keep credit starting within the independent period
credit = subset(credit, ProcessingDate <= endIP)

# Missing values

# Columns for which the missing values should be replaced by mean/mode
missToMean = c("ActionType", "CreditSource")

# Columns for which the missing values should be replaced by 0
missToZero = c("Amount")

# Per subscriptions: Missing values - TEMP
predictors = c(missToMean, missToZero)
creditBT = unique(credit["SubscriptionID"])
for(p in predictors){
  creditBT = aggr(credit, creditBT, p, paste("MV",p,sep=""), "SubscriptionID", "mvProp")
}

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
creditNb <- data.frame(table(credit$SubscriptionID))
colnames(creditNb)[1:2] <- c("SubscriptionID", "CreditNb")
creditBT = merge(creditBT, creditNb, by="SubscriptionID")

# Per subscriptions: Number of action type
creditBT = aggr(credit, creditBT, "ActionType", "ActionType", "SubscriptionID", "table")

# Per subscriptions: Number of credit source
creditBT = aggr(credit, creditBT, "CreditSource", "CreditSource", "SubscriptionID", "table")

# Per subscriptiosn: Total amount
creditBT = aggr(credit, creditBT, "Amount", "TotCreAmount", "SubscriptionID", "sum")

# Per customers: Total credits number, amount, action types and sources
subSub = subset(subscriptions, select=c("SubscriptionID", "CustomerID"))
creditBT = merge(creditBT, subSub, by="SubscriptionID")
creditBT = aggregate(creditBT[!names(creditBT) %in% c("CustomerID", "SubscriptionID")], creditBT["CustomerID"], sum)

########
# Base table generation: delivery
########

delivery$StartDate <- as.Date(delivery$StartDate, dateFormat)

# Keep active deliveries
delivery = merge(delivery, activeSubscriptions, by="SubscriptionID")
# Keep deliveries starting within the independent period
delivery = subset(delivery, StartDate <= endIP)

# Missing values

# Delete the empty levels in the factor variables 
#for(n in names(delivery)){
#  if(is.factor(delivery[,n])) delivery[,n] <- factor(delivery[,n])
#}

# Per subscriptions: Missing values - TEMP
predictors = c("DeliveryType", "DeliveryClass", "DeliveryContext")
deliveryBT = unique(delivery["SubscriptionID"])
for(p in predictors){
  deliveryBT = aggr(delivery, deliveryBT, p, paste("MV",p,sep=""), "SubscriptionID", "mvProp")
}

# Impute the mean and mode
delivery[, names(delivery) %in% predictors] <- imputeMissings(delivery[, names(delivery) %in% predictors])

# Remove missing values indicators when there are no missing values
for(p in predictors){
  if(sum(deliveryBT[paste("MV",p,sep="")])==0) deliveryBT[paste("MV",p,sep="")] <- NULL
}

# Per subscriptions: Number of deliveries
deliveryNb <- data.frame(table(delivery$SubscriptionID))
colnames(deliveryNb)[1:2] <- c("SubscriptionID", "deliveryNb")
deliveryBT = merge(deliveryBT, deliveryNb, by="SubscriptionID")

# Per subscriptions: Number of delivery type, class and context
deliveryBT = aggr(delivery, deliveryBT, "DeliveryType", "DeliveryType", "SubscriptionID", "table")
deliveryBT = aggr(delivery, deliveryBT, "DeliveryClass", "DeliveryClass", "SubscriptionID", "table")
deliveryBT = aggr(delivery, deliveryBT, "DeliveryContext", "DeliveryContext", "SubscriptionID", "table")

# Per customers: Total delivery number, types, class and context
subSub = subset(subscriptions, select=c("SubscriptionID", "CustomerID"))
deliveryBT = merge(deliveryBT, subSub, by="SubscriptionID")
deliveryBT = aggregate(deliveryBT[!names(deliveryBT) %in% c("CustomerID", "SubscriptionID")], deliveryBT["CustomerID"], sum)

########
# Base table generation: complaints
########

complaints$ComplaintDate <- as.Date(complaints$ComplaintDate, dateFormat)

# Keep active complaints
complaints = complaints[complaints$CustomerID %in% activeCustomers,]
# Keep complaints starting within the independent period
complaints = subset(complaints, ComplaintDate <= endIP)

# Missing values

# Per subscriptions: Missing values - TEMP
missToMean = c("ComplaintType", "SolutionType")
complaintsBT = unique(complaints["CustomerID"])
for(p in missToMean){
  complaintsBT = aggr(complaints, complaintsBT, p, paste("MV",p,sep=""), "CustomerID", "mvProp")
}

# Remove missing values indicators when there are no missing values
for(p in missToMean){
  if(sum(complaintsBT[paste("MV",p,sep="")])==0) complaintsBT[paste("MV",p,sep="")] <- NULL
}

# Impute the mode
complaints["ComplaintType"] <- imputeMissings(complaints["ComplaintType"])

# Finding Nr of Complaints per customer
complaintsNb <- data.frame(table(complaints$CustomerID))
colnames(complaintsNb)[1:2] <- c("CustomerID", "ComplaintsNb")
complaintsBT = merge(complaintsBT, complaintsNb, by="CustomerID")

#Finding #days since last complaint
complaintsBT = aggr(complaints, complaintsBT, "ComplaintDate", "DaysSinceLastCom", "CustomerID", "function(x) endIP-max(x)")

# % of complaints solved = nb of complaints solved / nb of complaints
complaintsBT = aggr(complaints, complaintsBT, "SolutionType", "ComSolvingPercent", "CustomerID", "function(x) sum(!is.na(x))")
complaintsBT$ComSolvingPercent = round(complaintsBT$ComSolvingPercent / complaintsBT$ComplaintsNb, 2)

# Impute the mode of solution type because we needed the number of NA above
complaints["SolutionType"] <- imputeMissings(complaints["SolutionType"])

# Number of complaint type
complaintsBT = aggr(complaints, complaintsBT, "ComplaintType", "ComplaintType", "CustomerID", "table")

# Number of solution type
complaintsBT = aggr(complaints, complaintsBT, "SolutionType", "SolutionType", "CustomerID", "table")

########
# Base table generation: formula
########

subsFormula = subset(subscriptions, select = c(SubscriptionID, StartDate, EndDate, CustomerID, FormulaID))

#merging the subsFormula and formula table
mergedSubsformula <- merge(subsFormula, formula, by = "FormulaID")

# Missing values - TMP
predictors = c("FormulaType", "Duration")
formulaBT = unique(mergedSubsformula["CustomerID"])
for(p in predictors){
  formulaBT = aggr(mergedSubsformula, formulaBT, p, paste("MV",p,sep=""), "CustomerID", "mvProp")
}

# Impute the mean and mode
mergedSubsformula[, names(mergedSubsformula) %in% predictors] <- imputeMissings(mergedSubsformula[, names(mergedSubsformula) %in% predictors])

# Remove missing values indicators when there are no missing values
for(p in predictors){
  if(sum(formulaBT[paste("MV",p,sep="")])==0) formulaBT[paste("MV",p,sep="")] <- NULL
}

#calculating aggegrate values for columns Duration and FormulaType
formulaBT = aggr(mergedSubsformula, formulaBT, "FormulaType", "FormulaType", "CustomerID", "table")
formulaBT = aggr(mergedSubsformula, formulaBT, "Duration", "Duration", "CustomerID", "table")

########
# Base table generation: global merging
########

subBaseTable = list(customersBT, subscriptionsBT, creditBT, deliveryBT, complaintsBT, formulaBT)
baseTable = Reduce(function(x, y) merge(x, y, by='CustomerID', all=TRUE), subBaseTable)
baseTable[is.na(baseTable)]  <- 0

print(Sys.time()-StartTime)
save(baseTable, file='baseTable73.Rdata')
# load('baseTable73.Rdata')

########
# Modeling
########

AllModels = function(baseTable){
allind <- sample(1:nrow(baseTable), nrow(baseTable))
trainind <- allind[1:round(length(allind)/3)]
valind <- allind[(round(length(allind)/3)+1):round(length(allind)*(2/3))]
testind <- allind[round(length(allind)*(2/3)+1):length(allind)]

trainTable <- baseTable[trainind, names(baseTable) != "CustomerID"]
valTable <- baseTable[valind, names(baseTable) != "CustomerID"]
testTable <- baseTable[testind, names(baseTable) != "CustomerID"]

# Delete constant columns
for(n in names(trainTable)){
  trainNb = dim(table(trainTable[n]))
  valNb = dim(table(trainTable[n]))
  if (trainNb==1 | valNb==1){
    trainTable[n] = NULL
    valTable[n] = NULL
    testTable[n] = NULL
  }
}

churnTrain <- trainTable$Churn
trainTable$Churn <- NULL
churnVal <- valTable$Churn
valTable$Churn <- NULL
churnTest <- testTable$Churn
testTable$Churn <- NULL

bigTrainTable <- rbind(trainTable, valTable)
churnBigTrain <- factor(c(as.integer(as.character(churnTrain)),as.integer(as.character(churnVal))))

trainTable <- data.frame(sapply(trainTable, function(x) as.numeric(as.character(x))))
bigTrainTable = data.frame(sapply(bigTrainTable, function(x) as.numeric(as.character(x))))
valTable <- data.frame(sapply(valTable, function(x) as.numeric(as.character(x))))
testTable <- data.frame(sapply(testTable, function(x) as.numeric(as.character(x))))

scores = data.frame(matrix(ncol=0, nrow=1))

#####
# Binary decision tree
#####

BDT <- rpart(churnBigTrain ~ ., control=rpart.control(cp = 0.001), bigTrainTable)

# Prediction
table(predictionTree <- predict(BDT, testTable)[,2])

# Evaluation
scores["BinaryDecisionTree"] <- AUC::auc(roc(predictionTree, churnTest))

#####
# Bagged decision tree
#####

bigTrainTable["Churn"] = churnBigTrain
treeBag <- list()
for (i in 1:10) {
  trainTableInd <- sample(1:nrow(bigTrainTable), nrow(bigTrainTable), replace=TRUE)
  treeBag[[i]] <- rpart(Churn ~ ., bigTrainTable[trainTableInd,])
}
bigTrainTable["Churn"] = NULL

# Prediction
predTreebag <- data.frame(matrix(NA, nrow=nrow(testTable), ncol=10))
for (i in 1:10) {
  predTreebag[,i] <- predict(treeBag[[i]], testTable)[,2] 
}
predTreebagged <- rowMeans(predTreebag)

# Evaluation
scores["BaggedTrees"] = AUC::auc(roc(predTreebagged, churnTest))

#####
# Boosting
#####

ABmodel <- ada(churnBigTrain ~ . , bigTrainTable, iter=50)

# Prediction
predAB <- as.numeric(predict(ABmodel, testTable, type="probs")[,2])

# Evaluation
scores["Boosting"] = AUC::auc(roc(predAB, churnTest))

# Variable importances in a character string
varplot(ABmodel,type="scores")

#####
# Random forest
#####

rFmodel <- randomForest(x=bigTrainTable, y=churnBigTrain, ntree=1000, importance=TRUE)

# Prediction
predrF <- predict(rFmodel, testTable, type="prob")[,2]

# Evaluation
scores["RandomForest"] = AUC::auc(roc(predrF, churnTest))

#####
# K-Nearest Neighbors
#####

# Tuning k
#auc <- numeric()
#for (k in 1:nrow(trainTable)) { 
#  indicatorsKNN <- as.integer(knnx.index(data=trainTable, query=valTable, k=k))
#  predKNN <- as.integer(as.character(churnTrain[indicatorsKNN]))
#  predKNN <- rowMeans(data.frame(matrix(data=predKNN, ncol=k, nrow=nrow(valTable)))) 
#  auc[k] <- AUC::auc(roc(predKNN, churnVal))
#}
#print(which.max(auc))

# Prediction
k=30
indicatorsKNN <- as.integer(knnx.index(data=bigTrainTable, query=testTable, k=k))
predKNN <- as.integer(as.character(churnBigTrain[indicatorsKNN]))
predKNN <- rowMeans(data.frame(matrix(data=predKNN, ncol=k, nrow=nrow(testTable))))

# Estimation
scores["KNN"] = AUC::auc(roc(predKNN, churnTest))

#####
# GLM - Stepwise logistic regression
#####

LR = glm(churnBigTrain ~ ., data=bigTrainTable, family=binomial("logit"), control=glm.control(maxit=200))
# Take long time
#LR <- step(LR, direction="both", trace = FALSE)

# Prediction
predLRstep = predict(LR, newdata=testTable, type="response")

# Evaluation
scores["GLM-Step"] = AUC::auc(roc(predLRstep, churnTest))

#####
# GLM - Regularized logistic regression
#####

LR = glmnet(x=data.matrix(trainTable), y=churnTrain, family="binomial")

# Cross-validate lambda
aucstore <- numeric()
for (i in 1:length(LR$lambda) ) {
  predglmnet <- predict(LR,newx=data.matrix(valTable),type="response",s=LR$lambda[i])
  aucstore[i] <- AUC::auc(roc(as.numeric(predglmnet),churnVal))
}   
LR.lambda <- LR$lambda[which.max(aucstore)]

# Create final model
LR <- glmnet(x=data.matrix(bigTrainTable), y=churnBigTrain, family="binomial")
predLRlas <- as.numeric(predict(LR, newx=data.matrix(testTable), type="response",s =LR.lambda))

# Evaluation
scores["GLM-RLR"] = AUC::auc(roc(predLRlas, churnTest))

#####
# Support vector machines
#####

# Tuning
#bigTrainTable["Churn"] = churnBigTrain
#params <- tune.svm(Churn ~ ., data=bigTrainTable, gamma=10^(-6:-1), cost=10^(1:4))
#bigTrainTable["Churn"] = NULL

modelSVM = svm(churnBigTrain ~ ., data=bigTrainTable, cost=1000, gamma=0.001, degree=1)

# Prediction
predSVM = predict(modelSVM, testTable)

# Evaluation
scores["SVM"] = AUC::auc(roc(predSVM, churnTest))

#####
# Neural networks
#####

minima <- sapply(trainTable, min)
scaling <- sapply(trainTable, max)-minima
trainTableScaled <- data.frame(base::scale(trainTable, center=minima, scale=scaling))
bigTrainTableScaled <- data.frame(base::scale(bigTrainTable, center=minima, scale=scaling))
valTableScaled <- data.frame(base::scale(valTable,center=minima,scale=scaling))
testTableScaled <- data.frame(base::scale(testTable, center=minima, scale=scaling))

NN.rang <- 0.5 #the range of the initial random weights parameter
NN.maxit <- 10000 #set high in order not to run into early stopping
NN.size <- c(5,10,20) #number of units in the hidden layer
NN.decay <- c(0,0.001,0.01,0.1) #weight decay. Same as lambda in regularized LR. Controls overfitting

call <- call("nnet", formula = churnTrain ~ ., data=trainTableScaled, rang=NN.rang, maxit=NN.maxit, trace=FALSE, MaxNWts= Inf)
tuning <- list(size=NN.size, decay=NN.decay)

# Tuning           
#result <- tuneMember(call=call, tuning=tuning, xtest=valTableScaled, ytest=churnVal, predicttype="raw")

# Modeling
NN <- nnet(churnBigTrain ~ ., bigTrainTableScaled, size = 10, rang = NN.rang, decay = 0.1, maxit = NN.maxit, trace=FALSE, MaxNWts= Inf)

# Prediction
predNN <- as.numeric(predict(NN, testTableScaled, type="raw"))

# Evaluation
scores["NN"] = AUC::auc(roc(predNN, churnTest))

#####
# Kernel factory
#####

KF <- kernelFactory(bigTrainTable, as.factor(churnBigTrain))

# Prediction
predKF <- predict(KF, newdata=testTable)

# Evaluation
scores["KF"] = AUC::auc(roc(predKF, churnTest))

scores
}

#bt = baseTable[round(1:(nrow(baseTable)*0.1)),]

n = 50
for (i in 1:n){
  auc = AllModels(baseTable)
  if (i==1){
    scores = rbind((auc/n), auc, auc)
  } 
  else{
    scores[1,] = scores[1,] + (auc / n)
    scores[2,] = pmin(scores[2,], auc)
    scores[3,] = pmax(scores[3,], auc)
  } 
  print(paste("Iteration",i))
  print(scores)
}
finalScores = t(scores)
colnames(finalScores) = c("Mean", "Min", "Max")
save(scores, file='scores53.Rdata')

#####
# All roc curves
#####

plot(roc(predictionTree, churnTest))
plot(roc(predTreebagged, churnTest), add=TRUE, col="blue")
plot(roc(predAB, churnTest), add=TRUE, col="red")
plot(roc(predrF, churnTest), add=TRUE, col="green")
plot(roc(predKNN, churnTest), add=TRUE, col="yellow")
plot(roc(predLRstep, churnTest), add=TRUE, col="pink")
plot(roc(predLRlas, churnTest), add=TRUE, col="purple")
plot(roc(predSVM, churnTest), add=TRUE, col="cyan")
plot(roc(predNN, churnTest), add=TRUE, col="grey")
plot(roc(predKF, churnTest), add=TRUE, col="orange")
legend("bottomright", legend=c("Binary Decision Tree","Bagged Trees", "Ada Boost", "Random Forest", "KNN", "GLM-Step", "GLM-RLR", "SVM", "NN", "KF"), col=c("black","blue","red","green","pink","purple","cyan","grey","orange"), lwd=1)
