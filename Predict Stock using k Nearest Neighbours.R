#############################################################
# ********************************************************* #
# *                                                       * #
# *   HarvardX Professional Certificate in Data Science    * #
# *                                                       * #
# *              PH125.9X: Capstone Project               * #
# *                                                       * #
# ********************************************************* #
#############################################################


#############################################################
# SECTION 1 - LOAD PACKAGES AND LOAD AND CLEAN DATA         #
#############################################################


# Note: this process could take a couple of minutes

# Install and Load required packages
if(!require(quantmod)) install.packages("quantmod", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
library(quantmod)
library(data.table)
library(tidyverse)
library(caret)
library(e1071)
library(ggcorrplot)
library(knitr)


# Import data from yahoo finance
getSymbols(c("^DJI","AMZN"), src = "yahoo")
getSymbols(c("AAPL","CSCO","GOOG","HPQ","IBM"), src = "yahoo")
getSymbols(c("INTC","MSFT","ORCL","QCOM","TXN"), src = "yahoo")


# Convert imported data to data frames
DJI <-as.data.frame(as.data.table(DJI)) %>% select(Date = index, DJI = DJI.Adjusted)
AMZN <-as.data.frame(as.data.table(AMZN)) %>% select(Date = index, AMZN = AMZN.Adjusted)
AAPL <-as.data.frame(as.data.table(AAPL)) %>% select(Date = index, AAPL = AAPL.Adjusted)
CSCO <-as.data.frame(as.data.table(CSCO)) %>% select(Date = index, CSCO = CSCO.Adjusted)
GOOG <-as.data.frame(as.data.table(GOOG)) %>% select(Date = index, GOOG = GOOG.Adjusted)
HPQ <-as.data.frame(as.data.table(HPQ)) %>% select(Date = index, HPQ = HPQ.Adjusted)
IBM <-as.data.frame(as.data.table(IBM)) %>% select(Date = index, IBM = IBM.Adjusted)
INTC <-as.data.frame(as.data.table(INTC)) %>% select(Date = index, INTC = INTC.Adjusted)
MSFT <-as.data.frame(as.data.table(MSFT)) %>% select(Date = index, MSFT = MSFT.Adjusted)
ORCL <-as.data.frame(as.data.table(ORCL)) %>% select(Date = index, ORCL =ORCL.Adjusted)
QCOM <-as.data.frame(as.data.table(QCOM)) %>% select(Date = index, QCOM = QCOM.Adjusted)
TXN <-as.data.frame(as.data.table(TXN)) %>% select(Date = index, TXN = TXN.Adjusted)


# dowloaded stocks list
stock_table <- data_frame(Symbol = "DJI", Name = "Dow Jones")
stock_table <- bind_rows(stock_table,data_frame(Symbol = "AMZN",Name = "Amazon"))
stock_table <- bind_rows(stock_table,data_frame(Symbol = "AAPL",Name = "Apple"))
stock_table <- bind_rows(stock_table,data_frame(Symbol = "CSCO",Name = "Cisco"))
stock_table <- bind_rows(stock_table,data_frame(Symbol = "GOOG",Name = "Google"))
stock_table <- bind_rows(stock_table,data_frame(Symbol = "HPQ",Name = "Hewlett Packard"))
stock_table <- bind_rows(stock_table,data_frame(Symbol = "IBM",Name = "IBM"))
stock_table <- bind_rows(stock_table,data_frame(Symbol = "INTC",Name = "Intel"))
stock_table <- bind_rows(stock_table,data_frame(Symbol = "MSFT",Name = "Microsoft"))
stock_table <- bind_rows(stock_table,data_frame(Symbol = "ORCL",Name = "Oracle"))
stock_table <- bind_rows(stock_table,data_frame(Symbol = "QCOM",Name = "Qualcom"))
stock_table <- bind_rows(stock_table,data_frame(Symbol = "TXN",Name = "Texas Intruments"))
stock_table


# Confirm imported datasets has same number of observations
nrow(DJI)
nrow(AMZN)
nrow(AAPL)
nrow(CSCO)
nrow(GOOG)
nrow(HPQ)
nrow(IBM)
nrow(INTC)
nrow(MSFT)
nrow(ORCL)
nrow(QCOM)
nrow(TXN)


# Confirm imported data has no missing values
sum(is.na(DJI$DJI))
sum(is.na(AMZN$AMZN))
sum(is.na(AAPL$AAPL))
sum(is.na(CSCO$CSCO))
sum(is.na(GOOG$GOOG))
sum(is.na(HPQ$HPQ))
sum(is.na(IBM$IBM))
sum(is.na(INTC$INTC))
sum(is.na(MSFT$MSFT))
sum(is.na(ORCL$ORCL))
sum(is.na(QCOM$QCOM))
sum(is.na(TXN$TXN))


# Combine all imported stocks into one temporary data frame and remove redundant date columns
MasterDataTemp <- cbind.data.frame(AMZN, AAPL, CSCO, GOOG, HPQ, IBM, INTC, MSFT, ORCL, QCOM, TXN)
MasterDataTemp <- MasterDataTemp[,c(1,2,4,6,8,10,12,14,16,18,20,22)]


# Create Performance Dataset for AMZN and DJI
Performance <- cbind.data.frame(AMZN, DJI)
Performance <- Performance[,c(1,2,4)]
Performance <- Performance %>% filter(Date > "2009-04-30" & Date <= "2019-04-30")


#############################################################
#                    END OF SECTION 1                       #
#############################################################

#***********************************************************#


#############################################################
# SECTION 2 - EDA                                           #
#############################################################


# Dow Jones performance return calculation
round(Performance$DJI[Performance$Date=="2009-05-01"],2)
Performance$DJI[Performance$Date=="2019-04-30"]
round((Performance$DJI[Performance$Date=="2019-04-30"]/Performance$DJI[Performance$Date=="2009-05-01"]-1),2)*100
round((Performance$DJI[Performance$Date=="2019-04-30"]/Performance$DJI[Performance$Date=="2009-05-01"]-1),2)*100/10

# Dow Jones performance chart
ggplot(Performance, aes(Date)) + 
  geom_line(aes(y = DJI), colour = "red") +
  ylab("Value") +
  xlab("Years") +
  ggtitle("Historic Performance of DJI May 2009 to April 2019")+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=14))


# Amazon performance return calculation
round((Performance$AMZN[Performance$Date=="2019-04-30"]/Performance$AMZN[Performance$Date=="2009-05-01"]-1),2)*100
round((Performance$AMZN[Performance$Date=="2019-04-30"]/Performance$AMZN[Performance$Date=="2009-05-01"]-1),2)*100/10

# Amazon performance chart
ggplot(Performance, aes(Date)) + 
  geom_line(aes(y = AMZN), colour = "blue") +
  ylab("Value") +
  xlab("Years") +
  ggtitle("Historic Performance of Amazon Stock May 2009 to April 2019")+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=14))


# DJI and Amazon combined performance chart with log transform
ggplot(Performance, aes(Date)) + 
  geom_line(aes(y = AMZN, colour = "red" )) +
  geom_line(aes(y = DJI, colour = "blue")) +
  scale_color_discrete(name = "",labels = c("DJI", "Amazon")) +
  scale_y_continuous(trans='log10') +
  ylab("Value - Log10") +
  xlab("Years") +
  ggtitle("Historic Performance of DJI vs Amazon")+
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=14))
  

# Create temporary dataframe for Amazon's prior day's data - using using 10 years from 30 April 2009 to 29 April 2019
AmazonPrior <- MasterDataTemp %>% filter(Date >= "2009-04-30" & Date <= "2019-04-29") %>% 
              select(Date.Prior = Date, AMZN.Prior = AMZN)

# Create temporary dataframe for Amazon's current day's data - using 10 years from 1 May 2009 to 30 April 2019
AmazonCurrent <- MasterDataTemp %>% filter(Date >= "2009-05-01" & Date <= "2019-04-30") %>% 
                select(Date.Current = Date, AMZN.Current = AMZN)

# Create temporary data set with columns indicating whether Amazon stock increased from the prior day to the current day
AmazonIncrease <- cbind.data.frame(AmazonPrior, AmazonCurrent) %>% 
      mutate(AMZN.Increase = ifelse(AMZN.Current - AMZN.Prior <= 0, FALSE, TRUE)) %>%
      select(AMZN.Increase)

# Filter Master data set for to include 10 years from 1 May 2009 to 30 April 2019
MasterDataTemp <- MasterDataTemp %>% filter(Date >= "2009-05-01" & Date <= "2019-04-30")

# Add Amazon increase indicator to temporary Masterdata data set
MasterData <- cbind.data.frame(MasterDataTemp, AmazonIncrease)


# Determine 5 most highly correlted stocks to AMZN
ggcorrplot(cor(MasterData[,2:12]), lab = TRUE,
           show.legend = FALSE) +
  ggtitle("Correlation Matrix") +
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"))


# stocks for analysis
stock_table_analysis <- data_frame(Symbol = "CSCO",Name = "Cisco")
stock_table_analysis <- bind_rows(stock_table_analysis,data_frame(Symbol = "GOOG",Name = "Google"))
stock_table_analysis <- bind_rows(stock_table_analysis,data_frame(Symbol = "INTC",Name = "Intel"))
stock_table_analysis <- bind_rows(stock_table_analysis,data_frame(Symbol = "MSFT",Name = "Microsoft"))
stock_table_analysis <- bind_rows(stock_table_analysis,data_frame(Symbol = "TXN",Name = "Texas Intruments"))
stock_table_analysis


# Create Final Master Data Set With Stock to be used in model
MasterDataAnalysis <- MasterData[,c(1,2,4,5,8,9,12,13)]


# Use first 9 years or data set for training and the most recent year for testing
Training <- MasterDataAnalysis %>% filter(Date <= "2018-04-30")
Testing <- MasterDataAnalysis %>% filter(Date >= "2018-05-01")


# Caret package used for analysis requires dependent variable top be factor for knn algorithm
Training[["AMZN.Increase"]] <- factor(Training[["AMZN.Increase"]])
Testing[["AMZN.Increase"]] <- factor(Testing[["AMZN.Increase"]])


# Remove redundant datasets
rm(MasterData, MasterDataTemp, AmazonPrior, AmazonCurrent, AmazonIncrease)
rm(DJI, AMZN,AAPL, CSCO, GOOG, HPQ, IBM, INTC, MSFT, ORCL, QCOM, TXN)


#############################################################
#                    END OF SECTION 2                       #
#############################################################

#***********************************************************#



#############################################################
# SECTION 3 - Modelling and Prediction                      #
#############################################################


# Train using caret package
trctrl <- trainControl(method = "repeatedcv", number = 33, repeats = 3)
set.seed(3)
knn_model <- train(AMZN.Increase ~., data = Training, method = "knn",
                    trControl=trctrl,
                    tuneLength = 10)

# Optimal k plot in training set
knn_model %>% ggplot(aes(k,accuracy)) + geom_line()+
  ggtitle("Optimal number of Neighbours") +
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=14))

# Optimal k dataframe
tr<-knn_model$results[c("k","Accuracy")]


# Prediction and testing set accuracy
prediction <- predict(knn_model, newdata = Testing)
cm <- confusionMatrix(prediction, Testing[,8])
cm
cm$overall["Accuracy"]


#############################################################
#                    END OF SECTION 3                       #
#############################################################

#***********************************************************#
