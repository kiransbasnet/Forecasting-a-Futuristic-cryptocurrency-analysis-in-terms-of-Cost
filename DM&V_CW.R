#Data preprocessing
#importing libraries
install.packages("dplyr") #dplyr-offers various functions to perform var functions
install.packages("xts") #to manipulate time series data
install.packages("quantmod") #assists with development, testing
library("quantmod")
library("xts")
library("quantmod")

#importing csv files to the variables
bitcoin<-read.csv("/Users/joshnarakinda/Desktop/D-M&V Lab/CW/Crypto/cryptos data set/coin_Bitcoin.csv")
ethereum<-read.csv("/Users/joshnarakinda/Desktop/D-M&V Lab/CW/Crypto/cryptos data set/coin_Ethereum.csv")
litecoin<-read.csv("/Users/joshnarakinda/Desktop/D-M&V Lab/CW/Crypto/cryptos data set/coin_Litecoin.csv")
dogecoin<-read.csv("/Users/joshnarakinda/Desktop/D-M&V Lab/CW/Crypto/cryptos data set/coin_Dogecoin.csv")
polkadot<-read.csv("/Users/joshnarakinda/Desktop/D-M&V Lab/CW/Crypto/cryptos data set/coin_Dogecoin.csv")
cardano<-read.csv("/Users/joshnarakinda/Desktop/D-M&V Lab/CW/Crypto/cryptos data set/coin_Cardano.csv")
stellar<-read.csv("/Users/joshnarakinda/Desktop/D-M&V Lab/CW/Crypto/cryptos data set/coin_Stellar.csv")
aave<-read.csv("/Users/joshnarakinda/Desktop/D-M&V Lab/CW/Crypto/cryptos data set/coin_Aave.csv")
tether<-read.csv("/Users/joshnarakinda/Desktop/D-M&V Lab/CW/Crypto/cryptos data set/coin_Tether.csv")
wrappedbitcoin<-read.csv("/Users/joshnarakinda/Desktop/D-M&V Lab/CW/Crypto/cryptos data set/coin_WrappedBitcoin.csv")
marketprice<-read.csv("/Users/joshnarakinda/Desktop/D-M&V Lab/CW/Crypto/market-price.csv")

#Data cleaning
crypto_coins_initial <- rbind(ethereum, dogecoin, stellar, aave, tether,wrappedbitcoin) #binding ethereum, dogecoin
crypto_coins_merged <- rbind(crypto_coins_initial,litecoin, polkadot, cardano, bitcoin) #binding merged etherum, dogecoin with bitcoin
colnames(crypto_coins_merged) #to check column names
sum(is.na(crypto_coins_merged)) #to know the no.of missing values
crypto_coins_merged<-subset(crypto_coins_merged, select = -Symbol) #dropping unwanted columns
crypto_coins_merged<-subset(crypto_coins_merged, select = -SNo) #dropping unwanted columns
head(crypto_coins_merged) #viewing data
tail(crypto_coins_merged)
max(crypto_coins_merged$Close)


#Cleaning bitcoin data for analysis
bitcoin<-subset(bitcoin, select = -Symbol) #dropping unwanted columns
bitcoin<-subset(bitcoin, select = -SNo) #dropping unwanted columns
head(bitcoin) #viewing data
tail(bitcoin)


#performing the descriptive statistics 
install.packages("psych") #installing psych package for statistical methods
library("psych") #loading the library
#displaying the mean, sd, median, max, min, skew, kurtosis
describe(bitcoin, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,
         type=3,check=TRUE,fast=NULL,quant=NULL,IQR=FALSE,omit=FALSE,data=NULL)
describeData(bitcoin,head=4,tail=4) #shows 1st few and last few columns


#Data Visualizing
install.packages("ggplot2")
library("ggplot2")
#visualization of bitcoin close values
bithist<-ggplot(bitcoin, aes(x=Close))+geom_histogram('binwidth'=500)
print(btchistplot+ggtitle("Bitcoin Close Values")) #printing the title

#visualization of cryptocurrency data over time
ggplot(data= crypto_coins_merged)+
  geom_point(aes(x=as.Date(Date), y=Close, color=Name))+
  labs(title = "Crypto Prices over time", x="Time",y="Price in USD")+
  facet_wrap(~Name) 
#visualizing bitcoin data over time
ggplot(data= bitcoin)+
  geom_point(aes(x=as.Date(Date), y=Close, color=Name))+
  labs(title = "Bitcoin Prices over time", x="Time",y="Price in USD")+
  facet_wrap(~Name) 
#applying generalised additive mode(gam) smoothing line to a Ethereum scatter plot
ggplot(data = ethereum, aes(x=as.Date(Date),y=Close)) +
  geom_point() +
  geom_smooth(aes(group=10), method='gam', formula = y~s(x))+
  labs(title="Ethereum", y="Price in USD", x="Time")
ggplot(data = crypto_coins_merged,aes(x=Volume,y=Close, color=Name)) +
  geom_point() +
  geom_smooth(aes(group= -1), method ='gam', formula =y ~ s(x) ) +
  labs(title="Price vs Volume", y="Price in USD", x="Volume traded per Day") +
  facet_wrap(~ Name)
ggplot(data = bitcoin,aes(x=Volume,y=Close)) +
  geom_point(color="green") +
  geom_smooth(aes(group= -1), method ='gam', color="red", formula = y~s(x))+
  labs(title="Bitcoin Price vs Volume", y="Price in USD", x="Volume traded per Day", ) +
  facet_wrap(~ Name)
#smoothing using lm
ggplot(data = bitcoin,aes(x=Volume,y=Close)) +
  geom_point(color="green") +
  geom_smooth(aes(group= -1), method ='lm', color="red")+
  labs(title="Bitcoin Price vs Volume", y="Price in USD", x="Volume traded per Day", ) +
  facet_wrap(~ Name)
#line plot showing all the cryptocurrency prices over time
ggplot(bitcoin, aes(x = Date, y = Close)
       +geom_line(aes (color = Name, linetype = Name)) + 
         scale_color_manual(values = c("darkred", "steelblue","red","blue","green","purple","pink","yellow","orange")))
#to observe variables and preidentify the linear relationship
library(lattice)
splom(~bitcoin[c(2:5)], groups = NULL, data = bitcoin,
      axis.line.tck=0,
      axis.text.alpha=0)

#correlation
crypto<-cor.test(bitcoin$Marketcap, bitcoin$Close,
                 method = "pearson")
crypto

#plot(qplot-basic plot of ggplot2)
qplot(x=as.Date(Date), Close, data=bitcoin,
      main="Bitcoin closing prices 2014-2021")
#Log transformation
ds<-bitcoin$Date #assigning date as datastamp
y<-log(bitcoin$Close)
df<-data.frame(ds,y)

qplot(ds, y, data = bitcoin,
      main="Bitcoin prices in log scale")


#Predicting the output using linear regression
#performing linear regression
results<-lm(bitcoin$Close ~ bitcoin$High + bitcoin$Volume)
results
summary(results)
#forecasting with prophet's package
install.packages("prophet") #to perform forecasting methods
library(prophet) #loading package
btc<-prophet(df) #assigning df as btc
future<-make_future_dataframe(btc, periods=365) #makefuturedf function takes the model object and a number of periods to forecast and produces a suitable
tail(future) #view last few rows
forecast<-predict(btc, future) #provides completely automated forecasts

#plot forecast
plot(btc, forecast,
     main="bitcoin prediction")
dyplot.prophet(btc, forecast)
prophet_plot_components(btc, forecast)

#model performance
pred<-forecast$yhat[1:1544] #assigning to prediction model
actual<-btc$history$y #assigning btc data as actual
length(actual) #length of actual model
length(pred) #length of prediction model
plot(actual[1:length(pred)],pred) #visualizing the actual and prediction values
abline(lm(pred~actual), col='green') #to add a st.line to a graph
summary(lm(pred-actual))#summary of results

x<-cross_validation(m,365, units = 'days')
performance_metrics(x, rolling_window = 0.1)
plot_cross_validation_metric(x,
                             metric = 'rmse',
                             rolling_window = 0.2)


#applying polynomial regression to check the changes
best = lm(bitcoin$High ~ poly(bitcoin$Close,2, raw=T), data=bitcoin)
summary(best)
ggplot(bitcoin, aes(x=Close, y=High))+
  geom_point()+
  stat_smooth(method = 'lm', formula = y~poly(x,2), size=1)+
  xlab('Bitcoin Price')+
  ylab('High')
dev.off()







