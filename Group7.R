# Sandeep Karkhanis & Sylvain Duquenne

ModelBuilding <- function(start.ind, end.ind, start.dep, end.dep){
   initialization <<- function(){
    packages = c("aCRM", "dummies", "ada", "AUC")
    for (p in packages){
      require(p, character.only=TRUE)
    }
    
    # Compute the proportions of missing values
    mvProp <<- function(x){
      round(sum(is.na(x))/length(x), 2)
    }
    
    # Aggregate col column of sourceTable by byCol with func function and merge it to destTable
    aggr <<- function(sourceTable, destTable, col, newName, byCol, func){
      funcExpr = parse(text=func)
      aggrTable = unique(sourceTable[byCol])
      if(func=="table"){
        values = unique(sourceTable[, col])
        sourceTable = dummy.data.frame(sourceTable, col, sep='_')
        if(length(values) > 1){
          for (v in values){
            colName = paste(col, v, sep="_")
            resTable = tapply(sourceTable[, colName], sourceTable[, byCol], sum)
            aggrTable[colName] = resTable
            aggrTable[byCol] = row.names(resTable)
          }
          row.names(aggrTable) = NULL
        }
      }
      else {
        resTable = tapply(sourceTable[, col], sourceTable[, byCol], eval(funcExpr))
        aggrTable[newName] = resTable
        aggrTable[byCol] = row.names(resTable)
        if(is.double(aggrTable[, newName])) aggrTable[, newName] = round(aggrTable[, newName], 2)
      }
      merge(destTable, aggrTable, all.x=TRUE)
    }
    
    # Remove constant missing values indicator columns
    rmMissValInd <<- function(table, predictors){
      for (p in predictors) if(sum(table[paste("MV", p, sep="")])==0) table[paste("MV",p,sep="")] <- NULL
      table
    }
    
    # Create missg values indicator columns
    createMissValInd <<- function (table, predictors, byVar){
      bT = unique(table[byVar])
      for(p in predictors) bT = aggr(table, bT, p, paste("MV",p,sep=""), byVar, "mvProp")
      bT
    }
  }
  dataCleaning <<- function(startIP, endIP, startDP, endDP, dateFormat, step){
    # Tables import
    complaints <- read.table("complaints.txt", header=TRUE, sep=";", colClasses=c("character", "character", "character", "character", "factor", "factor", "factor"), na.string=c("",".","NA"))
    credit <- read.table("credit.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "character", "factor", "numeric", "integer"), na.string=c("",".","NA"))
    customers <- read.table("customers.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "character", "character", "character", "character"), na.string=c("",".","NA"))
    delivery <- read.table("delivery.txt", header=TRUE, sep=";", colClasses=c("character", "factor", "factor", "factor", "character", "character"), na.string=c("",".","NA"))
    formula <- read.table("formula.txt", header=TRUE, sep=";", colClasses=c("character", "character", "factor", "factor"), na.string=c("",".","NA"))
    subscriptions <- read.table("subscriptions.txt", header=TRUE, sep=";", colClasses=c("character","character","character","character","character","character","integer","integer","character","factor","factor","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), na.string=c("",".","NA"))
          
    ########
    # Base table generation: customers
    ########
    
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
    
    customersBT = cbind(customersMV, subset(customers, select=c("CustomerID", "Churn", predictors)))
    
    # Remove missing values indicators when there are no missing values
    if(step=="train") customersBT = rmMissValInd(customersBT, predictors)
    
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
    subscriptionsBT = createMissValInd(subscriptions, predictors, "CustomerID")
    
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
    if(step=="train") subscriptionsBT = rmMissValInd(subscriptionsBT, predictors)
    
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
    
    subSubscriptions = subset(subscriptions, select = c(SubscriptionID, CustomerID))
    credit <- merge(subSubscriptions, credit, by = "SubscriptionID")
    
    # Missing values
    
    # Columns for which the missing values should be replaced by mean/mode
    missToMean = c("ActionType", "CreditSource")
    
    # Columns for which the missing values should be replaced by 0
    missToZero = c("Amount")
    
    # Per subscriptions: Missing values
    predictors = c(missToMean, missToZero)
    creditBT = createMissValInd(credit, predictors, "CustomerID")
    
    #credit <- imputeMissings(credit)
    
    # Impute the 0
    sub = subset(credit, select=missToZero)
    sub[is.na(sub) == "TRUE"] <- 0 
    credit[, names(credit) %in% missToZero] <- sub 
    
    # Impute the mean and mode
    credit[, names(credit) %in% missToMean] <- imputeMissings(credit[, names(credit) %in% missToMean])
    
    # Remove missing values indicators when there are no missing values
    if(step=="train") creditBT = rmMissValInd(creditBT, predictors)
    
    # Number of credits
    creditNb <- data.frame(table(credit$CustomerID))
    colnames(creditNb)[1:2] <- c("CustomerID", "CreditNb")
    creditBT = merge(creditBT, creditNb, by="CustomerID")
    
    # Number of action type
    creditBT = aggr(credit, creditBT, "ActionType", "ActionType", "CustomerID", "table")
    
    # Number of credit source
    creditBT = aggr(credit, creditBT, "CreditSource", "CreditSource", "CustomerID", "table")
    
    # Total amount
    creditBT = aggr(credit, creditBT, "Amount", "TotCreAmount", "CustomerID", "sum")
    
    ########
    # Base table generation: delivery
    ########
    
    delivery$StartDate <- as.Date(delivery$StartDate, dateFormat)
    
    # Keep active deliveries
    delivery = merge(delivery, activeSubscriptions, by="SubscriptionID")
    # Keep deliveries starting within the independent period
    delivery = subset(delivery, StartDate <= endIP)
    
    subSubscriptions = subset(subscriptions, select = c(SubscriptionID, CustomerID))
    delivery <- merge(subSubscriptions, delivery, by = "SubscriptionID")
    
    # Missing values
    
    # Per subscriptions: Missing values
    predictors = c("DeliveryType", "DeliveryClass", "DeliveryContext")
    deliveryBT = createMissValInd(delivery, predictors, "CustomerID")
    
    # Impute the mean and mode
    delivery[, names(delivery) %in% predictors] <- imputeMissings(delivery[, names(delivery) %in% predictors])
    
    # Remove missing values indicators when there are no missing values
    if(step=="train") deliveryBT = rmMissValInd(deliveryBT, predictors)
    
    # Number of deliveries
    deliveryNb <- data.frame(table(delivery$CustomerID))
    colnames(deliveryNb)[1:2] <- c("CustomerID", "deliveryNb")
    deliveryBT = merge(deliveryBT, deliveryNb, by="CustomerID")
    
    # Number of delivery type, class and context
    deliveryBT = aggr(delivery, deliveryBT, "DeliveryType", "DeliveryType", "CustomerID", "table")
    deliveryBT = aggr(delivery, deliveryBT, "DeliveryClass", "DeliveryClass", "CustomerID", "table")
    deliveryBT = aggr(delivery, deliveryBT, "DeliveryContext", "DeliveryContext", "CustomerID", "table")
    
    ########
    # Base table generation: complaints
    ########
    
    complaints$ComplaintDate <- as.Date(complaints$ComplaintDate, dateFormat)
    
    # Keep active complaints
    complaints = complaints[complaints$CustomerID %in% activeCustomers,]
    # Keep complaints starting within the independent period
    complaints = subset(complaints, ComplaintDate <= endIP)
    
    # Missing values
    missToMean = c("ComplaintType", "SolutionType")
    complaintsBT = createMissValInd(complaints, missToMean, "CustomerID")
    
    # Remove missing values indicators when there are no missing values
    if(step=="train") complaintsBT = rmMissValInd(complaintsBT, missToMean)
    
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
    
    subSubscriptions = subset(subscriptions, select = c(SubscriptionID, StartDate, EndDate, CustomerID, FormulaID))
    formula <- merge(subSubscriptions, formula, by = "FormulaID")
    
    # Missing values
    predictors = c("FormulaType", "Duration")
    formulaBT = createMissValInd(formula, predictors, "CustomerID")
    
    # Impute the mean and mode
    formula[, names(formula) %in% predictors] <- imputeMissings(formula[, names(formula) %in% predictors])
    
    # Remove missing values indicators when there are no missing values
    if(step=="train") formulaBT = rmMissValInd(formulaBT, predictors)
    
    #calculating aggegrate values for columns Duration and FormulaType
    formulaBT = aggr(formula, formulaBT, "FormulaType", "FormulaType", "CustomerID", "table")
    formulaBT = aggr(formula, formulaBT, "Duration", "Duration", "CustomerID", "table")
    
    ########
    # Base table generation: global merging
    ########
    
    subBaseTable = list(customersBT, subscriptionsBT, creditBT, deliveryBT, complaintsBT, formulaBT)
    baseTable = Reduce(function(x, y) merge(x, y, by='CustomerID', all=TRUE), subBaseTable)
    baseTable[is.na(baseTable)]  <- 0
    baseTable
  }
  ########
  # Time windows
  ########
  
  # Setting the format for date
  dateFormat <- "%d/%m/%Y"
  
  # Start of indep period
  startIP <- as.Date(start.ind, dateFormat)
  # End of indep period
  endIP <- as.Date(end.ind, dateFormat)
  #Start of dep period
  startDP <- as.Date(start.dep, dateFormat)
  # End of dep period
  endDP <- as.Date(end.dep, dateFormat)
  initialization()
  baseTable = dataCleaning(startIP, endIP, startDP, endDP, dateFormat, "train")
  baseTable <- baseTable[names(baseTable) != "CustomerID"]
  # Delete constant columns
  for(n in names(baseTable)){
    if (dim(table(baseTable[n]))==1 ) baseTable[n] = NULL
  }
  churn <- baseTable$Churn
  baseTable$Churn <- NULL  
  #print(dim(baseTable))
  #View(baseTable)
  # Columns to keep
  colToKeep = names(baseTable)
  baseTable <- data.frame(sapply(baseTable, function(x) as.numeric(as.character(x))))
  # Create the model
  ABmodel <- ada(churn ~ . , baseTable, iter=50)
  ret = list(ABmodel, colToKeep, endIP-startIP)
  ret
}

ModelingDeployment <- function(object, end.ind){
  dateFormat <- "%d/%m/%Y"
  endIP <- as.Date(end.ind, dateFormat)
  startIP = endIP - object[[3]]
  startDP = endIP
  endDP  = seq(startDP, length=2, by="years")[2]
  
  initialization()
  baseTable2 = dataCleaning(startIP, endIP, startDP, endDP, dateFormat, "test")
  
  customerID = baseTable2$CustomerID
  baseTable2 <- baseTable2[names(baseTable2) != "CustomerID"]
  churn <- baseTable2$Churn
  baseTable2$Churn <- NULL
  for(c in names(baseTable2)) if(!c %in% object[[2]]) baseTable2[c] = NULL 
  for(c in object[[2]]) if(!c %in% names(baseTable2)) baseTable2[c] = 0
  #print(dim(baseTable2))
  #View(baseTable2)
  baseTable2 <- data.frame(sapply(baseTable2, function(x) as.numeric(as.character(x))))
  predAB <- as.numeric(predict(object[[1]], baseTable2, type="probs")[,2])
  print(AUC::auc(roc(predAB, churn)))
  pred = ifelse(predAB<= 0.5,0,1)
  ret = cbind(customerID, pred, churn)
  ret
}

setwd("/home/xclyde/Courses/UGENT/Modeling/Assignment/trainTables")
model = ModelBuilding("02/01/2006", "01/02/2010", "03/02/2010", "03/02/2011")
setwd("/home/xclyde/Courses/UGENT/Modeling/Assignment/testTables")
predictions = ModelingDeployment(model, "01/02/2010")
