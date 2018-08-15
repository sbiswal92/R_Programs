library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)
library(dplyr)
library(magrittr)


#################################
# Data Retrieval and Formatting #
#################################

retail <- read_excel("https://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx")
View(retail)
#Only retain non-null rows : Return a logical vector indicating which cases are complete, i.e., have no missing values. 
retail <- retail[complete.cases(retail), ]
retail <- retail %>% mutate(Description = as.factor(Description))
retail <- retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo <- as.character(retail$InvoiceNo)
nrow(retail)


##########################################################################
# Who are the top 10 best sellers?                                       #
##########################################################################


tmp <- retail %>% 
       group_by(StockCode,Description) %>% 
       summarize(stock_ctr = length(Quantity)) %>%
       arrange(desc(stock_ctr))
tmp<-head(tmp,n=10) 
  
  tmp %>%
  ggplot(aes(x=reorder(Description,stock_ctr),y=stock_ctr)) + # x-axis:Description, y-axis:stock_ctr, descr on x-axis ordered by the stock_ctr
  geom_histogram(stat="identity",fill="indianred") + 
  coord_flip()  #flip the coordinates for readability of the description labels
 

  
  
##########################################################################
# Find most bought items and associations                                #
##########################################################################
  
#First organize the data such that items bought under same date are in a single row 
#(Assuming all items bought by a shopper in one day is one basket)
#The function ddply() accepts a data frame, splits it into pieces based on one or more factors,
#computes on the pieces, and then returns the results as a data frame. 
#We use “,” to separate different items.
  
itemList <- ddply(retail,c("CustomerID","Date"), function(split_df)paste(split_df$Description, collapse = ","))  
itemListCopy <-itemList

#Remove Cutsomer and invoice details from this dataframe because only associations betweenn items need to be done  
itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")
str(itemList)

#Now split the items column in itemList into different columns 
# Write the above dataset into csv and automatically we obtain a data file as per desired format
write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

# read.transactions is a special function to read data of above kind where each row represents a basket and has no other information
library("arules")
txn <- read.transactions('market_basket.csv', format = 'basket', sep=',')
inspect(txn[1:10,])
summary(txn)
#total number of items over all baskets = rows*columns*density
#most frequent items check summary
#element (itemset/transaction) length distribution: Shows how many transactions had a given basket size
itemFrequencyPlot(txn, topN=20, type='absolute')

#Let rule be X =>Y 
#Support is an indication of how frequently the itemset appears in the dataset. X appears in the dataset with frequency denoted by "support"
#Confidence is an indication of how often the rule has been found to be true. Out of the data where X is LHS and Y is RHS with frequency "confidence"
#Lift=1: Presence of both X and Y in basket is independent of each other
#Lift>1: Presence of both X and Y in basket are positively dependent on each other. Higher the value, higher the dependency. Presence of item X, causes presence of item Y in basket
#Lift<1: Presence of both X and Y in basket are negatively dependent on each other. Lower the value, lower the dependency. Presence of item X, prevents the presence of item Y in basket

#Here, atleast 0.1% support and atleast 100% confidence
#apriori automatically finds association rules with the above conditions

rules <- apriori(txn, parameter = list(supp=0.01, conf=1))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

#Summary gives number of rules generated
#How many rules for different length of rules
#For each rule: support and confidence is described

#Lets see the rules, that will hellp decide the what are the most commonly bought together items
inspect(rules[1:10]) # See top 10 rules if there lot many rules
inspect(rules)
