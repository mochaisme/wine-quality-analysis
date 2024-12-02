wine_df=read.csv(".../Source/winequality-red.csv")
head(wine_df)
str(wine_df)
summary(wine_df)
dim(wine_df)
head(is.na(wine_df))
sum(is.na(wine_df))

for(i in colnames(wine_df))
{
  print(i)
  print(var(wine_df[[i]]))}
library(dplyr)
wine_df %>% summarise_if(is.numeric,var)

wine_df %>% summarise_if(is.numeric,sd)
wine_df %>% summarise_if(is.numeric,range)

v = wine_df$fixed.acidity
hist(v,xlab = "Fixed Acidity",col = "Green",border = "blue")

boxplot(wine_df$volatile.acidity)

library("ggplot2")
gg=ggplot(wine_df,aes(x=quality,y=alcohol))+geom_point(aes(col= density))+geom_smooth(method="loess",se=F)
plot(gg)

g<-ggplot(wine_df,aes(quality,density,na.rm=TRUE)) 
g+ geom_boxplot(varwidth = T,fill="plum")+
  labs(subtitle="Quality vs Contents",y="density",x="Quality",title="boxplot",caption="Source:Wine_prediction")

plot(wine_df, col="navy", main="Matrix Scatterplot")
pairs(wine_df)
library(corrplot)
wine_df1= select(wine_df,-quality)
correlations = cor(wine_df1)
corrplot(correlations)
ggplot(wine_df, aes(x = density, y = quality)) + geom_point(aes(color = pH))
library("psych")
describe(wine_df)
cor(wine_df$quality,wine_df$fixed.acidity)
cor(wine_df$quality,wine_df$volatile.acidity)
cor(wine_df$quality,wine_df$citric.acid)
cor(wine_df$quality,wine_df$residual.sugar)
cor(wine_df$quality,wine_df$chlorides)
cor(wine_df$quality,wine_df$free.sulfur.dioxide)
cor(wine_df$quality,wine_df$total.sulfur.dioxide)
cor(wine_df$quality,wine_df$density)
cor(wine_df$quality,wine_df$pH)
cor(wine_df$quality,wine_df$sulphates)
cor(wine_df$quality,wine_df$alcohol)

library(corrplot)

corrplot(cor(wine_df),
         method = "number",
         type = "upper" )

library(party)
tree1=ctree(quality~fixed.acidity +volatile.acidity+citric.acid+residual.sugar+chlorides +free.sulfur.dioxide+total.sulfur.dioxide+density +pH+sulphates+alcohol,data=wine_df) #~target variable,predicting 
tree1

library(rpart)
mytree=rpart(quality~.,data=wine_df,method="class")
mytree

library(rattle)
library(RColorBrewer)
fancyRpartPlot(mytree,caption=NULL)

Model=lm(quality ~ density,total.sulfur.dioxide,pH, data =wine_df)
summary(Model)

Model=lm(quality ~ volatile.acidity+chlorides, data =wine_df)
summary(Model)

Model=lm(quality ~ sulphates+citric.acid, data =wine_df)
summary(Model)

Model=lm(quality ~ alcohol,sulphates,data =wine_df)
summary(Model)

library(cluster)
wine_df2 <- wine_df[, -5]
set.seed(240) # Setting seed
kmeans.re <- kmeans(wine_df2, centers = 3, nstart = 20)
kmeans.re
cm <- table(wine_df$alcohol, kmeans.re$cluster)
head(cm)
plot(wine_df2[c("total.sulfur.dioxide", "free.sulfur.dioxide")])
plot(wine_df2[c("total.sulfur.dioxide", "free.sulfur.dioxide")], 
     col = kmeans.re$cluster)
plot(wine_df2[c("total.sulfur.dioxide", "free.sulfur.dioxide")], 
     col = kmeans.re$cluster, 
     main = "K-means with 3 clusters")
kmeans.re$centers
kmeans.re$centers[, c("total.sulfur.dioxide", "free.sulfur.dioxide")]
library("ggplot2")
library("dplyr")
library("ggfortify")
y_kmeans <- kmeans.re$cluster
clusplot(wine_df2[, c("total.sulfur.dioxide", "free.sulfur.dioxide")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 5,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster of sulphur dioxide"),
         ylab =  "free.sulfur.dioxide",
         xlab = "total.sulfur.dioxide")
kmean <- kmeans(wine_df2, 3)
kmean$centers
autoplot(kmean,wine_df2, frame = TRUE)
heatmap(as.matrix(wine_df))
