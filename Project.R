rm(list=ls())

install.packages("readr")
install.packages("ggplot2")   
##install.packages("nnet")
install.packages("neuralnet")
install.packages("qgraph")
install.packages("igraph")
install.packages("RColorBrewer")
install.packages("gganimate")
install.packages("gifski")
install.packages("pheatmap")

library(pheatmap)


library(readr)
library(ggplot2)
##library(nnet)
library(neuralnet)
library(qgraph)
library(igraph)
library(RColorBrewer)
library(gganimate)
library(gifski)



################# DATA PREPARATION #################

coin_Bitcoin <- read_csv("D:/Privato/UNI/Financial data science/archive/coin_Bitcoin.csv")
BTC <- coin_Bitcoin[, 4:10]
## Could be done more efficiently, but I wanted to maximize readability
BTC$Return <- (BTC$High + BTC$Low)/2   ##for now it's only the mean, will be used to compute the returns
BTC2 <- as.data.frame(sapply(BTC[, 8], function(x) diff(log(x), lag=1)))
BTC2 <- as.data.frame(cbind(BTC[2:nrow(BTC), c(1:3, 6:7)],BTC2))
BTC2 <- BTC2[BTC2$Date>'2013-12-27 23:59:59',]  ##missing data before this date
colnames(BTC2)[-1] <- paste0('BTC_', colnames(BTC2)[-1])

coin_Ethereum <- read_csv("D:/Privato/UNI/Financial data science/archive/coin_Ethereum.csv")
ETH <- coin_Ethereum[, 4:10]
ETH$Return <- (ETH$High + ETH$Low)/2   
ETH2 <- as.data.frame(sapply(ETH[, 8], function(x) diff(log(x), lag=1)))
ETH2 <- as.data.frame(cbind(ETH[2:nrow(ETH), c(1:3, 6:7)],ETH2))
colnames(ETH2)[-1] <- paste0('ETH_', colnames(ETH2)[-1])

coin_ChainLink <- read_csv("D:/Privato/UNI/Financial data science/archive/coin_ChainLink.csv")
LINK <- coin_ChainLink[, 4:10]
LINK$Return <- (LINK$High + LINK$Low)/2   
LINK2 <- as.data.frame(sapply(LINK[, 8], function(x) diff(log(x), lag=1)))
LINK2 <- as.data.frame(cbind(LINK[2:nrow(LINK), c(1:3, 6:7)],LINK2))
colnames(LINK2)[-1] <- paste0('LINK_', colnames(LINK2)[-1])

coin_Cardano <- read_csv("D:/Privato/UNI/Financial data science/archive/coin_Cardano.csv")
ADA <- coin_Cardano[, 4:10]
ADA$Return <- (ADA$High + ADA$Low)/2   
ADA2 <- as.data.frame(sapply(ADA[, 8], function(x) diff(log(x), lag=1)))
ADA2 <- as.data.frame(cbind(ADA[2:nrow(ADA), c(1:3, 6:7)],ADA2))
colnames(ADA2)[-1] <- paste0('ADA_', colnames(ADA2)[-1])

coin_Litecoin <- read_csv("D:/Privato/UNI/Financial data science/archive/coin_Litecoin.csv")
LTC <- coin_Litecoin[, 4:10]
LTC$Return <- (LTC$High + LTC$Low)/2   
LTC2 <- as.data.frame(sapply(LTC[, 8], function(x) diff(log(x), lag=1)))
LTC2 <- as.data.frame(cbind(LTC[2:nrow(LTC), c(1:3, 6:7)],LTC2))
colnames(LTC2)[-1] <- paste0('LTC_', colnames(LTC2)[-1])

coin_BinanceCoin <- read_csv("D:/Privato/UNI/Financial data science/archive/coin_BinanceCoin.csv")
BNB <- coin_BinanceCoin[, 4:10]
BNB$Return <- (BNB$High + BNB$Low)/2   
BNB2 <- as.data.frame(sapply(BNB[, 8], function(x) diff(log(x), lag=1)))
BNB2 <- as.data.frame(cbind(BNB[2:nrow(BNB), c(1:3, 6:7)],BNB2))
colnames(BNB2)[-1] <- paste0('BNB_', colnames(BNB2)[-1])

coin_all <- merge(x=BTC2, y=ETH2, by="Date")
coin_all <- merge(x=coin_all, y=BNB2, by="Date")
coin_all <- merge(x=coin_all, y=LINK2, by="Date")
coin_all <- merge(x=coin_all, y=LTC2, by="Date")
coin_all <- merge(x=coin_all, y=ADA2, by="Date")


################# CORRELATION #################

cor_network <- cor_auto(coin_all[, c(6, 11, 16, 21, 26, 31)])
cor_2 <- cor_network
rownames(cor_2) <- c("BTC", "ETH", "BNB", "LINK", "LTC", "ADA")
colnames(cor_2) <- c("BTC", "ETH", "BNB", "LINK", "LTC", "ADA")
heatmap(cor_2, main="Correlation among cryptocurrencies' returns", Colv = NA, Rowv = NA, col= colorRampPalette(brewer.pal(6, "YlOrRd"))(50))

names(cor_2) <- paste("X", 1:10)
pheatmap(cor_2, display_numbers = T, color = colorRampPalette(c('white','orange'))(100), cluster_rows = F, cluster_cols = F, fontsize_number = 15)

# Simple correlation network
Graph_1 <- qgraph(cor_network, graph="cor", layout="spring", edge.width=0.5)
summary(Graph_1) # provides a summary of the network (number of edges)
centrality(Graph_1)

# Partial correlation network
Graph_2 <- qgraph(cor_network, graph="pcor", layout="spring", edge.width=1)
summary(Graph_2)

# We can get more precise results if we eliminate links that are not statistically significant.
# The threshold argument can be used to do just that -- to remove edges that are not significant.
Graph_3 <- qgraph(cor_network, graph = "pcor", layout = "spring", edge.width=1, threshold = "sig",
                  sampleSize = nrow(coin_all), alpha = 0.05)
summary(Graph_3)

centralities_Graph1 <- centrality(Graph_1)
centralities_Graph2 <- centrality(Graph_2)
centralities_Graph3 <- centrality(Graph_3)

# Plotting the centrality measures
centralityPlot(Graph_2, include =c("Strength", "Closeness"),scale="z-scores")


# Compare the two networks
centralityPlot(GGM = list("Simple correlation" = Graph_1, "Partial correlation" = Graph_2),scale="z-scores")

################# NN RETURNS #################

# Create the train data set, importing only the useful columns from coin_all
train <- coin_all[coin_all$Date<='2020-06-30 23:59:59', c(6, 11, 16, 21, 26, 31)]

formula <- ETH_Return ~ .

model_mlr <- lm(ETH_Return ~ BTC_Return + BNB_Return + LINK_Return + LTC_Return + ADA_Return, data = coin_all)

summary(model_mlr)

RSS <- c(crossprod(model_mlr$residuals))
MSE <- RSS / length(model_mlr$residuals)
RMSE <- sqrt(MSE)

RMSE_2
model_mlr

train <- coin_all[coin_all$Date<='2020-06-30 23:59:59', ]

formula <- ETH_Return ~ .

set.seed(123)
nn <- neuralnet(formula,
                data = train,
                hidden = c(3), 
                linear.output = TRUE,
                lifesign = "minimal")


# Plot the neural network 
plot(nn)


test <- coin_all[coin_all$Date>'2020-06-30 23:59:59', c(6, 11, 16, 21, 26, 31)]

set.seed(123)
nn_pred <- neuralnet::compute(nn, test)

predicted<- nn_pred$net.result

original_values <- test$ETH_Return
rmse <- sqrt(mean((original_values-predicted)^2))
rmse

mat1.data <- c(5,0.02232,7,0.02238,10,0.02213,12,0.02231,15,0.02279,17,0.02204,20,0.02246,22,0.02237,25,0.02206,27,0.02277,30,0.02232,32,0.02232,35,0.02219,37,0.02232,40,0.02295,42,0.02258,45,0.02370,47,0.02268,50,0.02239)
mat1 <- matrix(mat1.data,nrow=19,ncol=2,byrow=TRUE)
colnames(mat1) <- c("neuron","rmse")
mat1

rmse_gif <- as.data.frame(mat1) %>%
  ggplot( aes(x=neuron, y=rmse)) +
  geom_line(size=2.5, color="#f78e0c") +
  geom_point() +
  ggtitle("RMSE with different number of neurons") +
  ylab("RMSE") +
  xlab("Neurons") +
  ylim(0.02,0.03) +
  theme(panel.background = element_rect(fill="white"), panel.grid.major = element_line(color="grey"), panel.grid.minor = element_line(color="white", linetype="dashed")) +
  transition_reveal(neuron)

rmse_gif
animate(rmse_gif, renderer = gifski_renderer())
anim_save("output.gif")

################# NN VOLUMES #################

