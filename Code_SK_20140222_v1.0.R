################################################
#1 Understanding the Data
################################################

#set the working directory
setwd("/Users/SKarkhanis/Desktop/MMA_UGent/2013_2014/06_Marketing_Models_&_Engineering/ModelingAssignment/data/")

################################################
#Importing the data
################################################

#subscriptions
subscriptions <- read.table("subscriptions.txt",header=TRUE, sep=";", colClasses=c("character","character","character","character","character","character","integer","integer","character","factor","factor","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
str(subscriptions);

#customers
customers <-read.table("customers.txt", header=TRUE, sep=";",colClasses=c("character","character","character","character","character","character"))

#Delivery
delivery <-read.table("delivery.txt", header=TRUE, sep=";",colClasses=c("character","character","character","character","character","character"),na.strings = "NA" )

#Formula
formula <-read.table("formula.txt", header=TRUE, sep=";",colClasses=c("character","character","character","character" ),na.strings = "NA" )

#Credit
credit <-read.table("credit.txt", header=TRUE, sep=";",colClasses=c("character","character","character","character","numeric","numeric"),na.strings = "NA" )

#Complaints
complaints <-read.table("complaints.txt", header=TRUE, sep=";",colClasses=c("character","character","character","character","numeric","numeric","numeric" ),na.strings = "NA" )

#################################################
# DEFINING THE TIME WINDOW
#################################################

#Checking summary stats for subscription start date
#used to get a feel of the time period for which data is available
#might be useful in time-window 

#date fields imported as character values, so we need to convert them to dates first
#find a better way to convert char dates to date format 'dd-mm-yyyy'

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
# 04 NOT TO FORGET, CREATING DEPENDENT var (we wait for instruction fom professor)
# 05 once, 01 ,02, 03, 04 are complete, we can merge all to create BASETABLE

####################################################################################
#Step 01 the customers have to be active during the end of the independent period
####################################################################################
#subscription$StartDate <= end_IP
#subscription$EndDate > start_DP or subscription$EndDate

active_Customers <- data.frame(subset(subscriptions, (StartDate <= end_IP & (EndDate > start_DP | EndDate==NA)),select = CustomerID))

################################################
#Creating Independent variables
################################################
Donationlines_timewindow



############Tranforming the Subscriptions table############

#We can create RFM variables based on Subscriptions table & dummy variables for other catergorical variables


############Tranforming the Complaints table############

#Finding Nr of Complaints per customer
nr_complaints_per_cust <- data.frame(table(complaints$CustomerID))
colnames(nr_complaints_per_cust)[1] <- "CustomerID"
colnames(nr_complaints_per_cust)[2] <-  "Nr_Complaints"

#Finding type of Complaints Type per customer (needed???)



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