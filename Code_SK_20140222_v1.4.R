################################################
#1 Understanding the Data
################################################

#set the working directory
setwd("/Users/SKarkhanis/Desktop/MMA_UGent/2013_2014/06_Marketing_Models_&_Engineering/ModelingAssignment/data/")


#Importing the data
################################################

#subscriptions
subscriptions <- read.table("subscriptions.txt",header=TRUE, sep=";", colClasses=c("character","character","character","character","character","character","integer","integer","character","factor","factor","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
str(subscriptions);

#customers
customers <-read.table("customers.txt", header=TRUE, sep=";",colClasses=c("character","factor","character","character","character","character"))

#Delivery
delivery <-read.table("delivery.txt", header=TRUE, sep=";",colClasses=c("character","character","character","character","character","character"),na.strings = "NA" )

#Formula
formula <-read.table("formula.txt", header=TRUE, sep=";",colClasses=c("character","character","character","character" ),na.strings = "NA" )

#Credit
credit <-read.table("credit.txt", header=TRUE, sep=";",colClasses=c("character","character","character","character","numeric","numeric"),na.strings = "NA" )

#Complaints
complaints <-read.table("complaints.txt", header=TRUE, sep=";",colClasses=c("character","character","character","character","numeric","numeric","numeric" ))#,na.strings = "NA" )

#################################################
# DEFINING THE TIME WINDOW
#################################################

#date fields imported as character values, so we need to convert them to dates first
#find a better way to convert char dates to date format 'dd-mm-yyyy'
subscriptions$StartDate    <- as.Date(subscriptions$StartDate,"%d/%m/%Y")
subscriptions$EndDate      <- as.Date(subscriptions$EndDate,"%d/%m/%Y")
subscriptions$RenewalDate  <- as.Date(subscriptions$RenewalDate,"%d/%m/%Y")
subscriptions$PaymentDate  <- as.Date(subscriptions$PaymentDate,"%d/%m/%Y" )
customers$DOB              <- as.Date(customers$DOB,"%d/%m/%Y" )
complaints$ComplaintDate   <- as.Date(complaints$ComplaintDate, "%d/%m/%Y")

#Checking summary stats for subscription start date
#used to get a feel of the time period for which data is available
#might be useful in time-window 
summary(subscriptions$StartDate)

#Min.          1st Qu.       Median       Mean       3rd Qu.         Max. 
#"2006-01-02" "2008-01-01" "2009-03-04" "2009-01-24" "2010-03-18" "2012-01-28" 

# setting the format for date
Date_format <- "%d/%m/%Y"

#start of indep period
start_IP <- as.Date("02/01/2006", Date_format)

#end of indep period
end_IP <- as.Date("28/12/2010", Date_format)

#keeping a gap of 1 month between the Indep & Dep Periods

#dependent time period of 1 year
#start of dep period
start_DP <- as.Date("29/01/2011", Date_format)

#end of dep period
end_DP<- as.Date("28/01/2012", Date_format)

################################################
#basic investigations across diff variables
################################################

table(subscriptions$ProductID)
# 1    2    3    4    5    6    7    8 
#789 1459  793  971  494 1547  977 8429 

table(customers$Gender)

#F    M 
#508 1749 

table(subscriptions$Pattern)

#10     100     110   10000   10010  100010 1000000 1000010 1010000 1010010 
#273      83      15      18       3       6      20      15       9       9 

#1100010 1111110 
#2   15006 

table(subscriptions$PaymentType)

#BT    DD 
#13200  2259 

################################################
#Missings per variable in Subscriptions Table
################################################

colSums(is.na(subscriptions)) #result below
#~2% of the data in the following columns is missing
#GrossFormulaPrice | NetFormulaPrice | NetNewspaperPrice | 
#ProductDiscount | FormulaDiscount | TotalDiscount
#TotalPrice | TotalCredit

colSums(is.na(customers))#result below
#~4% of the data in the Gender column is missing

colSums(is.na(delivery))#result below - NOT WORKING CORRECTLY FOR DELIVERY TABLE
#~4% of the data in the  column is missing

colSums(is.na(formula))#result below
#no missing data

colSums(is.na(credit))#result below
#~27% of missing data in column NbrNewspapers

colSums(is.na(complaints))#result below
#~26% of missing data in column SolutionType
#~60% of missing data in column FeedbackType

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#Steps for creating Base Table
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#For this we need to,
# 01 select cust active @ end of indep period
# 02 then only focus on ALL ASPECTS(R,F,M) of these customers
# 03 I created some other Indep var based on Payment Type, Gender, Complaints, Delivery, Credit tables
# 04 NOT TO FORGET, CREATING DEPENDENT var (we wait for instruction from professor)
# 05 once, 01 ,02, 03, 04 are complete, we can merge all to create BASETABLE

####################################################################################
#Step 01 the customers have to be active during the end of the independent period
####################################################################################
#subscription$StartDate <= end_IP
#subscription$EndDate > start_DP or subscription$EndDate is missing

active_Customers <- data.frame(subset(subscriptions, (StartDate <= end_IP & (EndDate > start_DP | EndDate==NA)),select = CustomerID))

################################################
#Creating Independent variables
################################################
Amountlines_timewindow <- data.frame(subset(subscriptions,(PaymentDate <= end_IP)))

#sorting Amountlines_timewindow
Amountlines_timewindow <- Amountlines_timewindow[order(Amountlines_timewindow[,2],Amountlines_timewindow[,12]),]

# Renewal Frequency 
renewal_frequency <- data.frame(table(Amountlines_timewindow$CustomerID))

#renaming the variables in renewal_frequency
colnames(renewal_frequency)[1] <- 'CustomerID'
colnames(renewal_frequency)[2] <- 'Frequency'

# Recency

#sorting the data frame to get latest transaction per customer as first row
Amountlines_timewindow2 <- Amountlines_timewindow[order(Amountlines_timewindow[,2],as.Date(Amountlines_timewindow[,12], format="%d/%m/%Y"),decreasing = TRUE),]

Amountlines_timewindow3 <- Amountlines_timewindow2[ !duplicated(Amountlines_timewindow2$CustomerID,fromLast=FALSE),]

Amountlines_timewindow3 <- data.frame(Amountlines_timewindow3$CustomerID, Amountlines_timewindow3$PaymentDate)
colnames(Amountlines_timewindow3)[1] <- 'CustomerID'
colnames(Amountlines_timewindow3)[2] <- 'PaymentDate'

Amountlines_timewindow3$Recency <- end_IP - Amountlines_timewindow3$PaymentDate

################Age

cust_age <- subset(customers,select=c(CustomerID, DOB))
cust_age$age <- (Sys.Date() - cust_age$DOB)
cust_age$DOB <- NULL

################LOR
#sorted by customer id and start date
Amountlines_SD <- Amountlines_timewindow[order(Amountlines_timewindow[,2],as.Date(Amountlines_timewindow[,5], format="%d/%m/%Y"),decreasing = FALSE),]
Amountlines_SD <- data.frame(Amountlines_SD$CustomerID, Amountlines_SD$StartDate)

#sorted by customer id and end date
Amountlines_ED <- Amountlines_timewindow[order(Amountlines_timewindow[,2],as.Date(Amountlines_timewindow[,6], format="%d/%m/%Y"),decreasing = TRUE),]
Amountlines_ED <- data.frame(Amountlines_ED$CustomerID, Amountlines_ED$EndDate)

#renaming variables

colnames(Amountlines_SD)[1:2] <- c('CustomerID','StartDate')
colnames(Amountlines_SD)[1:2] <- c('CustomerID','EndDate')
#removing dups
Amountlines_SD <- Amountlines_SD[ !duplicated(Amountlines_SD$CustomerID,fromLast=FALSE),]
Amountlines_ED <- Amountlines_ED[ !duplicated(Amountlines_ED$CustomerID,fromLast=FALSE),]


Cust_LOR <- merge(Amountlines_SD,Amountlines_ED,all=TRUE)
Cust_LOR$LOR <- (Cust_LOR$EndDate - Cust_LOR$StartDate)

Cust_LOR$EndDate <-NULL
Cust_LOR$StartDate <- NULL


################Imputing Gender
#customers[,names(customers) == "Gender"] <- imputeMissings(customers[,names(customers) == "Gender"])
#customers[,names(customers) %in% c("Gender")] <- imputeMissings(customers[,names(customers) %in% c("Gender")])
customers <- dummy.data.frame(data=customers,names=c("Gender"),sep="_")
customers$Impu_Gender <- customers$Gender_M + customers$Gender_NA


############Tranforming the Subscriptions table############

#We can create RFM variables based on Subscriptions table & dummy variables for other catergorical variables
#install.packages('dummies')

############Tranforming the Complaints table############

#Finding Nr of Complaints per customer
(a <- Sys.time())
nr_complaints_per_cust <- data.frame(table(complaints$CustomerID))
Sys.time() - a
colnames(nr_complaints_per_cust)[1] <- "CustomerID"
colnames(nr_complaints_per_cust)[2] <- "Nr_Complaints"
 
#Finding #days since last complaint

last_complaints <- aggregate(complaints$ComplaintDate, complaints['CustomerID'], FUN=max)
colnames(last_complaints)[2] <- "ComplaintDate"
last_complaints$nr_days_since_last_complaint <- Sys.Date() - last_complaints$ComplaintDate

# % of complaints solved = nr(solution type) / nr (complaint type)
complaints_type <- aggregate(complaints$ComplaintType, complaints['CustomerID'], FUN=length)
solution_type <- aggregate(complaints$SolutionType, complaints['CustomerID'], FUN=length)
colnames(complaints_type)[2] <- "c_t"
colnames(solution_type)[2] <- "s_t"
complaints_solved <- merge(complaints_type,solution_type,all=TRUE)
complaints_solved$c_s <- (solution_type$s_t/complaints_type$c_t)
############Tranforming the Credit table############

#Finding Total credit amount per subscription, Total # news papers credited,

tmp_credit <- data.frame(credit$SubscriptionID, credit$Amount,credit$NbrNewspapers)
#renaming columns in tmp_credit data frame
colnames(tmp_credit)[1] <- "SubscriptionID"
colnames(tmp_credit)[2] <- "Amount"
colnames(tmp_credit)[3] <- "NbrNewspapers"

#Summing the amount & # news papers
total_amt_newspapers_per_subscriptionn <- data.frame (aggregate(tmp_credit[,names(tmp_credit) != 'SubscriptionID'], by=list(ID=tmp_credit$SubscriptionID),sum))
#removing tmp_credit to save space
rm(tmp_credit)

############Tranforming the Formula table############
tmp_subs_formula <- as.data.frame (subset(subscriptions, select = c(SubscriptionID, StartDate, EndDate,CustomerID ,FormulaID))) 

#merging the tmp_subs_formula and formula data frames

#left join
merged_subs_formula <- as.data.frame(merge(x=tmp_subs_formula,y=formula, by = "FormulaID", all.x = TRUE,incomparables=0))

#sorting the resulting data frame
merged_subs_formula <- merged_subs_formula[order(merged_subs_formula[,5],decreasing = FALSE),]

#looping through the data frame and assigning dummy var reg_ct and cam_ct

#calculating aggegrate values for columns Duration and FormulaType
aggr_ct_reg_cam <- aggregate(merged_subs_formula$FormulaType, merged_subs_formula['CustomerID'], table)
aggr_ct_reg_cam <- as.data.frame(sapply(aggr_ct_reg_cam,unlist))

aggr_ct_Duration <- aggregate(merged_subs_formula$Duration, by=list(CustomerID=merged_subs_formula$CustomerID), table)
aggr_ct_Duration <- as.data.frame(sapply(aggr_ct_Duration,unlist))

#auto-renaming the columns in aggr_ct_Duration
vars <- colnames(aggr_ct_Duration[, names(aggr_ct_Duration) != 'CustomerID'])
new  <- sapply(vars, function(x) paste('Duration_', x, sep=''))
colnames(aggr_ct_Duration) <- c(names(aggr_ct_Duration)[1], new) #understand

#sorting
aggr_ct_reg_cam <- aggr_ct_reg_cam[order(aggr_ct_reg_cam[,1]),]
aggr_ct_Duration <- aggr_ct_Duration[order(aggr_ct_Duration[,1]),]

#summary table containing one row per customer from formula table
aggr_Formula <- merge(x=aggr_ct_reg_cam,y=aggr_ct_Duration, by="CustomerID",all=TRUE, incomparables = NA)
colnames(aggr_Formula)[2]<-"cam_ct"
colnames(aggr_Formula)[3]<-"reg_ct"
