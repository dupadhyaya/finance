# algo trading

#https://rstudio-pubs-static.s3.amazonaws.com/363773_2382648d32f748b2a7549816de5ea377.html
pacman::p_load(rvest, pbapply, TTR, dygraphs, lubridate, tidyquant, timetk,DT, purrr)
#https://bigcharts.marketwatch.com/industry/bigcharts-com/default.asp

website <- read_html("https://www.marketwatch.com/tools/industry/stocklist.asp?bcind_ind=9535&bcind_period=3mo")
website
table <- html_table(html_nodes(website, "table")[[1]], fill = TRUE)

stocks.symbols<-table$X2
stocks.names<-table$X3
table1<-table[-1,-1]
colnames(table1)<-table[1,-1]
DT::datatable(table1)

stock.list<-"https://www.marketwatch.com/tools/industry/stocklist.asp?bcind_ind=9535&bcind_period=3mo"
stock.list
stocks<-read_html(stock.list)
stocks
stocks.names<-html_nodes(stocks,".lk01")
stocks.names<-html_text(stocks.names)
stocks.names

table1[table1==""] <- NA

table1<-table1[complete.cases(table1$Symbol),]
# to keep columns with no NA:
#table1 <- table1[, colSums(complete.cases(table1)) == 0]
#table1 %>%filter(complete.cases(.)) 
DT::datatable(table1)

start.date<-Sys.Date()
end.date<-Sys.Date()-years(3)

start.date<-gsub('-','', start.date)
end.date<-gsub('-','', end.date)

start.date

# The symbols vector holds our tickers. 
symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

# The prices object will hold our raw price data 
prices <-   getSymbols(symbols, src = 'yahoo', from = "2020-01-01",  auto.assign = TRUE, warnings = FALSE) %>%  map(~Ad(get(.))) %>%  Reduce(merge) %>%  `colnames <-` (symbols)
?Ad
#Extract (transformed) data from a suitable OHLC object. getSymbols('IBM',src='yahoo') Ad(IBM)  #reduce() combines from the left, reduce_right() combines from the right
getSymbols('IBM',src='yahoo')
Ad(IBM) 

tail(prices)
head(prices)


library(quantmod)

tickers <- c("AAPL", "MSFT","GOOGL","IBM")
getSymbols(tickers)

closePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
closePrices

#ParallelMap package-----
#We take advantage of paralleMap package to reduce the time the stock data is read into r.
library(parallelMap)
parallelStartSocket(2) 
parallelStartMulticore()
# start in socket mode and create 2 processes on localhost
f = function(x) Cl(get(x))     # define our job
y = parallelMap(f, tickers) # like R's Map but in parallel
mapdata<-do.call(cbind,y)
parallelStop()
mapdata %>% head()
tail(mapdata)

#BiocParallel
#BiocManager::install("BiocParallel")
library(BiocParallel)
f = function(x) Ad(get(x))
f
options(MulticoreParam=quote(MulticoreParam(workers=4)))
param <- SnowParam(workers = 2, type = "SOCK")
vec=c(tickers[1],tickers[2],tickers[3],tickers[4])
#vec=c(paste0(quote(tickers),"[",1:length(tickers),"]",collapse=","))
multicoreParam <- MulticoreParam(workers = 7)
bio=bplapply(tickers, f, BPPARAM = multicoreParam)
biodata<-do.call(cbind, bio)
biodata%>%head()
biodata%>% tail()

AdjustedPrices<-biodata

dateWindow <- c("2022-01-01", "2023-09-01")

dygraph(AdjustedPrices, main = "Value", group = "stock") %>%  dyRebase(value = 100) %>%  dyRangeSelector(dateWindow = dateWindow)

end<-Sys.Date()
start<-Sys.Date()-years(3)


prices  <- tq_get(symbols , get = "stock.prices", from = start,to=end)
prices%>%head()

#------------
pacman::p_load(dygraph)
library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

f<-function(x) Ad(get(x))

AdjustedPrices <- do.call(merge, bplapply(vec, f, BPPARAM = multicoreParam))

AdjustedPrices%>%head()
AdjustedPrices%>% tail()

dateWindow <- c("2022-01-01", "2023-09-01")

dygraph(AdjustedPrices, main = "Value", group = "stock") %>%
  dyRebase(value = 100) %>%
  dyRangeSelector(dateWindow = dateWindow)


library(plotly)
library(quantmod)

getSymbols("AAPL",src='yahoo')


# basic example of ohlc charts
df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)
df
# cutom colors
i <- list(line = list(color = '#FFD700'))
d <- list(line = list(color = '#0000ff'))

p <- df %>%
  plot_ly(x = ~Date, type="ohlc",
          open = ~AAPL.Open, close = ~AAPL.Close,
          high = ~AAPL.High, low = ~AAPL.Low,
          increasing = i, decreasing = d)

p


library(plotly)

biodatadf<-tk_tbl(biodata, timetk_idx = TRUE)%>%rename(Date=index)

data<-biodatadf%>%filter(Date>"2014-01-11")


p <- plot_ly(data, x = ~Date, y = ~AAPL.Adjusted, name = 'AAPL', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MSFT.Adjusted, name = 'MSFT', mode = 'lines')%>%
  add_trace(y = ~IBM.Adjusted, name = 'IBM', mode = 'lines')%>%
  add_trace(y = ~GOOGL.Adjusted, name = 'GOOGL', mode = 'lines')%>%
  layout(title = "Visualizing Adjusted Stock Prices",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Adjusted Prices"))
p


#A Simple Trading Strategy: Trend Following
tail(SMA(AdjustedPrices$AAPL.Adjusted, 200))
tail(SMA(AdjustedPrices$AAPL.Adjusted, 50)) 

data.frame(sma200=SMA(AdjustedPrices$AAPL.Adjusted, 200),sma50=SMA(AdjustedPrices$AAPL.Adjusted, 50))%>%head()


sdata<-biodatadf%>%select(-Date)

#sdata<-tk_xts(data,date_var=Date)

df_50=as.data.frame.matrix(apply(sdata, 2, SMA,50))

colnames(df_50)=paste0(colnames(df_50),"_sma50")


df_200=as.data.frame.matrix(apply(sdata, 2, SMA,200))

colnames(df_200)=paste0(colnames(df_200),"_sma200")
library(tidyverse)
df_all <- cbind.data.frame(Date=biodatadf$Date,  df_200,df_50)%>% drop_na()

# sma 50
f50<- function(x) SMA(x,50)

# sma 50
f200<- function(x) SMA(x,200)

#library(pryr)
#data %>% plyr::colwise() %>% f50

df_all<-tk_xts(df_all,date_var=Date)

df_all%>%head()
dim(df_all)

mov.avgs<-function(df){
  
  ifelse((nrow(df)<(2*260)),
         x<-data.frame(df, 'NA', 'NA'),
         x<-data.frame( SMA(df, 200), SMA(df, 50)))
  colnames(x)<-c( 'sma_200','sma_50')
  x<-x[complete.cases(x$sma_200),]
  return(x)
}

dplyr::pull(sdata, AAPL.Adjusted)%>%head()

var=names(df_all)[str_detect(names(df_all), "AAPL")]
df_all[,var]%>%head()

dateWindow=c("2020-01-01","2023-02-01")

dygraph(df_all[,var],main = 'Apple Moving Averages') %>%
  dySeries('AAPL.Adjusted_sma50', label = 'sma 50') %>%
  dySeries('AAPL.Adjusted_sma200', label = 'sma 200') %>%
  dyRangeSelector(height = 30) %>%
  dyShading(from = '2020-01-01', to = '2021-9-01', color = '#CCEBD6') %>%
  dyShading(from = '2021-9-01', to = '2023-01-01', color = '#FFE6E6')%>%
  dyRangeSelector(dateWindow = dateWindow)
