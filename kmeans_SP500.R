(# some references
# https://uc-r.github.io/kmeans_clustering
# https://scholarship.claremont.edu/cgi/viewcontent.cgi?article=3517&context=cmc_theses

# CLEAR WORKSPACE
rm(list=ls())
cat("\014")


library(ggplot2)
library(treemapify)
library(usmap)
library(wordcloud)
library(DataCombine)
library(RColorBrewer)
library(purrr)
library(factoextra)
library(hrbrthemes)
require(mosaic)
library(tidyverse)
library(corrplot)
library(Hmisc)


#loading data

options(warn=-1)
companies = read.csv('sp500_companies.csv')
index = read.csv('sp500_index.csv')
stocks = read.csv('sp500_stocks.csv')

str(companies)
str(index)
str(stocks)

companies$Sector = as.factor(companies$Sector)
companies$Exchange = as.factor(companies$Exchange)
companies$Industry = as.factor(companies$Industry)
companies$Country <- NULL

na.omit(companies)
summary(companies)

# Calculating daily change in S&P500 index
index = change(index, Var = 'S.P500', NewVar = 'daily_change', slideBy = -1, type = "proportion")

# pulling historical data for constructing the portfolio
getstockdata<-function(symbol){
  print(paste("Calculaitng metrics and organizing data for:", symbol))
  data <- subset(stocks , Symbol == symbol)
  rows <- as.integer(nrow(data) * 1)
  data <- data[1: rows,]
  data$daily_Change = (data$Open-data$Close)/data$Close
  data$daily_variation = (data$High-data$Low)/data$Low
  change(data, Var = 'Adj.Close', NewVar = 'daily_returns', slideBy = -1, type = "proportion")
}


# List of data frames are contained here for all 500 stocks
stocks_daily_data = list()

# calculating features for all 500 stocks
sp500_ann_returns = c()
sp500_ann_volatility = c()
sp500_ann_sharpe_ratio = c()
sp500_ann_daily_change = c()
sp500_ann_daily_variation = c()
sp500_beta = c()
sp500_cor = c()

# Extracting historical data for s&p500 stocks
symbols = companies$Symbol
for(symbol in symbols){
  
  # data preparation
  data = getstockdata(symbol)
  stocks_daily_data[[symbol]] = data
  df = merge(data,index,by="Date")
  na.omit(df)
  
  # calculating financial ratios from historical data
  annualized_returns = (tail(cumprod(na.omit(data$daily_returns) + 1),n=1) ** (252/2160)) - 1
  if(length(annualized_returns) == 0){annualized_returns = NA}
  annualized_volatilty = 252**(1/2) * sd(na.omit(data$daily_returns))
  annualized_sharpe_ratio = annualized_returns / annualized_volatilty
  annualized_daily_change = 252**(1/2) * mean(na.omit(data$daily_Change))
  annualized_daily_variation = 252**(1/2) * mean(na.omit(data$daily_variation))
  beta = cor(df$Adj.Close,df$S.P500) * ( sd(na.omit(df$daily_returns)) / sd(na.omit(df$daily_change)) )
  corr = cor(df$Adj.Close,df$S.P500)
  
  print(paste("Annualized Returns         :",annualized_returns))
  print(paste("Annualized Volatility      :",annualized_volatilty))
  print(paste("Annualized Sharpe Ratio    :",annualized_sharpe_ratio))
  print(paste("Annualized Daily Change    :",annualized_daily_change))
  print(paste("Annualized Daily Variation :",annualized_daily_variation))
  print(paste("Beta                       :",beta))
  print(paste("Corr                       :",corr))
  print("-----------------------------------------------------------------------")
  
  # appending data
  sp500_ann_returns = append(sp500_ann_returns,annualized_returns)
  sp500_ann_volatility = append(sp500_ann_volatility,annualized_volatilty)
  sp500_ann_sharpe_ratio = append(sp500_ann_sharpe_ratio,annualized_sharpe_ratio)
  sp500_ann_daily_change = append(sp500_ann_daily_change,annualized_daily_change)
  sp500_ann_daily_variation = append(sp500_ann_daily_variation,annualized_daily_variation)
  sp500_cor = append(sp500_cor,corr)
  sp500_beta = append(sp500_beta,beta)
  
}

# ORGANIZING DATA
drops <- c("Country","Exchange", "Shortname","Longname","Currentprice","Ebitda","Revenuegrowth","Longbusinesssummary", "Fulltimeemployees", "Marketcap")

cluster_features <- companies[ , !(names(companies) %in% drops)]

cluster_features$ann_return = sp500_ann_returns
cluster_features$ann_vol = sp500_ann_volatility
cluster_features$ann_sharpe_ratio = sp500_ann_sharpe_ratio
cluster_features$ann_daily_change = sp500_ann_daily_change
cluster_features$ann_daily_variation = sp500_ann_daily_variation
cluster_features$beta = sp500_beta
cluster_features$cor = sp500_cor

cluster_features <- na.omit(cluster_features)

Symbols = cluster_features$Symbol
City = cluster_features$City
State = cluster_features$State
Industry = cluster_features$Industry
Sector = cluster_features$Sector


drops <- c("Sector", "Industry", "City", "State", "Weight")
cluster_features <- cluster_features[ , !(names(cluster_features) %in% drops)]
row.names(cluster_features) <- cluster_features[,1]

drops <- c("Symbol")
cluster_features <- cluster_features[ , !(names(cluster_features) %in% drops)]
cluster_features <- na.omit(cluster_features)

head(cluster_features)


####################################################################
########################## CLUSTERING ##############################
####################################################################

# prep date frame
set.seed(123)
df.norm <- data.frame(sapply(cluster_features, scale))
row.names(df.norm) <- row.names(cluster_features) 

# DETERMINING OPTIMAL NUMBER OF CLUSTERS USING total within-cluster sum of square 
set.seed(123)

#wss <- function(k) {
  #kmeans(df.norm, k=k, nstart = 10 )$tot.withinss
#}
#wss_values <- map_dbl(1:15, wss)
#plot(1:15, wss_values,
     #type="b", pch = 19, frame = FALSE, 
     #xlab="Number of clusters K",
     #ylab="Total within-clusters sum of squares")


# running clustering
kmeans <- kmeans(df.norm, centers = 2, nstart = 20, iter.max = 50)
fviz_cluster(kmeans, data = df.norm,pointsize = 0.5, labelsize = 6,ellipse.alpha=0.1)
#table(kmeans$cluster)

# Run summary statistics
survey = cbind(cluster_features, cluster = kmeans$cluster)
cluster.ann_returns <- favstats(ann_return ~ cluster, data=survey); cluster.ann_returns
boxplot(ann_return~cluster,data=survey,
        main="Cluster wise Annualized Returns distribution",
        xlab="Cluster",
        ylab="Annualized Return",
        col="steelblue",
        border="black")


favstats.diet <- favstats(ann_vol ~ cluster, data=survey); favstats.diet
boxplot(ann_vol~cluster,data=survey,
        main="Cluster wise Annualized Volatility distribution",
        xlab="Cluster",
        ylab="Annualized Volatility",
        col="steelblue",
        border="black")

favstats.diet <- favstats(ann_sharpe_ratio ~ cluster, data=survey); favstats.diet
boxplot(ann_sharpe_ratio~cluster,data=survey,
        main="Cluster wise Annualized Sharpe Ratio distribution",
        xlab="Cluster",
        ylab="Sharpe Ratio",
        col="steelblue",
        border="black")

favstats.diet <- favstats(beta ~ cluster, data=survey); favstats.diet
boxplot(beta~cluster,data=survey,
        main="Cluster wise beta value distribution",
        xlab="Cluster",
        ylab="Beta",
        col="steelblue",
        border="black")


aggdf <- aggregate(cbind(cor,ann_return,ann_vol,ann_sharpe_ratio,beta) ~ cluster, data=survey, mean )
aggdf



## Plot few feature by clusters
# Plot of Ann Returns vs. Volatility by cluster
cluster_features$Cluster = kmeans$cluster
cluster_features$Cluster = as.factor(cluster_features$Cluster)
ggplot(cluster_features, aes(x=ann_return, y=ann_vol, color=Cluster)) + geom_point(size=4) + theme_ipsum() + xlab("Annualized Returns")   + ylab("Annualized Volatility") + ggtitle("Cluster wise Annualized Returns VS Annualized Volatility ")


cluster_features$Symbols = Symbols
cluster_features$City = City
cluster_features$State = State
cluster_features$Industry = Industry
cluster_features$Sector = Sector

nrow(cluster_features)

SR_sorted_cluster <- cluster_features %>% arrange(desc(ann_sharpe_ratio))
SR_sorted_cluster1 = subset(cluster_features, Cluster == 1) %>% arrange(desc(ann_sharpe_ratio))
SR_sorted_cluster2 = subset(cluster_features, Cluster == 2) %>% arrange(desc(ann_sharpe_ratio))
#SR_sorted_cluster3 = subset(cluster_features, Cluster == 3) %>% arrange(desc(ann_sharpe_ratio))
#SR_sorted_cluster4 = subset(cluster_features, Cluster == 4) %>% arrange(desc(ann_sharpe_ratio))
#SR_sorted_cluster5 = subset(cluster_features, Cluster == 5) %>% arrange(desc(ann_sharpe_ratio))
#SR_sorted_cluster6 = subset(cluster_features, Cluster == 6) %>% arrange(desc(ann_sharpe_ratio))

# CONSTRUCTING PORFOLIO USING TOP 4 STOCK BASED ON SHARPE RATIO FROM EACH CLUSTER
Portfolio = rbind(SR_sorted_cluster1[1:4,],SR_sorted_cluster2[1:4,])
Portfolio_Stocks = Portfolio$Symbols

# CONSTRUCTING PORFOLIO USING TOP 8 STOCK BASED ON SHARPE RATIO
Portfolio_ClusterX = SR_sorted_cluster[1:8,]
ClusterX_Stocks = Portfolio_ClusterX$Symbols

get_stock_validation_data<-function(symbol){
  print(paste("Calculaitng metrics and organizing data for:", symbol))
  data <- subset(stocks , Symbol == symbol)
  rows <- as.integer(nrow(data) * 1)
  data <- data[2160: nrow(data),]
  change(data, Var = 'Adj.Close', NewVar = 'daily_returns', slideBy = -1, type = "proportion")
}

portfolio_stocks_daily_data = c()
clusterx_stocks_daily_data = c()
index_validation_data = data_frame()

# for constructed portfolio
for(symbol in Portfolio_Stocks){
  # data preparation
  data = get_stock_validation_data(symbol)
  drops <- c("Open", "High", "Low", "Close", "Adj.Close","Volume","Symbol")
  data <- data[ , !(names(data) %in% drops)]
  colnames(data) <- c("Date",paste(symbol,"_dailty_change",sep=""))
  portfolio_stocks_daily_data[[symbol]] = data
  df = merge(data,index,by="Date")
  na.omit(df)
  index_validation_data = df
}


#for cluster X - top stocks in 7 year historical data
for(symbol in ClusterX_Stocks){
  # data preparation
  data = get_stock_validation_data(symbol)
  drops <- c("Open", "High", "Low", "Close", "Adj.Close","Volume","Symbol")
  data <- data[ , !(names(data) %in% drops)]
  colnames(data) <- c("Date",paste(symbol,"_dailty_change",sep=""))
  clusterx_stocks_daily_data[[symbol]] = data
}

# calculating index daily change
index_validation_data <- change(index_validation_data, Var = 'S.P500', NewVar = 'sp500_daily_returns', slideBy = -1, type = "proportion")
drops <- c("S.P500","Symbol","Volume","daily_change","VTR_dailty_change")
index_validation_data <- index_validation_data[ , !(names(index_validation_data) %in% drops)];index_validation_data
index_validation_data

# reorganize
portfolio_returns = portfolio_stocks_daily_data %>% reduce(full_join, by='Date')
portfolio_returnsx = portfolio_stocks_daily_data %>% reduce(full_join, by='Date')
portfolio_returnsx$Date = NULL
portfolio_returns$weighted_portfolio_returns <- rowMeans(portfolio_returnsx)
head(portfolio_returns)

# organizing data and performing calculations
clusterx_returns = clusterx_stocks_daily_data %>% reduce(full_join, by='Date')
clusterx_returnsx = clusterx_stocks_daily_data %>% reduce(full_join, by='Date')
clusterx_returnsx$Date = NULL
clusterx_returns$weighted_clusterx_returns <- rowMeans(clusterx_returnsx)
head(clusterx_returns)
merged_data = merge(portfolio_returns,index_validation_data)
merged_clusterx_data = merge(clusterx_returns,index_validation_data)
keep = c("weighted_portfolio_returns","sp500_daily_returns","Date")
merged_data <- merged_data[ , (names(merged_data) %in% keep)];
keep = c("weighted_clusterx_returns","sp500_daily_returns","Date")
merged_clusterx_data <- merged_clusterx_data[ , (names(merged_clusterx_data) %in% keep)];
merged_data <- na.omit(merged_data)
merged_clusterx_data <- na.omit(merged_clusterx_data)
merged_data$portfolio_cumulative_returns <- cumsum(merged_data$weighted_portfolio_returns)
merged_data$sp500_cumulative_return <- cumsum(merged_data$sp500_daily_returns)
merged_clusterx_data$clusterx_cumulative_returns <- cumsum(merged_clusterx_data$weighted_clusterx_returns)
final_cluster_analysis = data_frame()
final_cluster_analysis = merged_data
final_cluster_analysis$clusterx_cumulative_returns = merged_clusterx_data$clusterx_cumulative_returns
tail(final_cluster_analysis,n=50)
str(final_cluster_analysis)
final_cluster_analysis$Date <- as.Date(final_cluster_analysis$Date)

# plot performance of portfolio and index in the last 3 years
df <- final_cluster_analysis %>%
  select(Date, sp500_cumulative_return,portfolio_cumulative_returns) %>%
  gather(key = "Legend", value = "value", -Date)
head(df)

ggplot(df, aes(x = Date, y = value)) + 
  geom_line(aes(color = Legend)) + 
  scale_color_manual(values = c("orange", "red")  + xlab("Years")   + ylab("Cumulative Returns")  + ggtitle("K Means Portfolio vs S&P500 performance")  + theme_ipsum()

# Stocks in portfolio
Portfolio$Symbols
