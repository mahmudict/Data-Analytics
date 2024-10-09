library(ggplot2)
library(plotly)
library(GGally)
library(aplpack)
library(pheatmap)

data("iris")
iris

friends_data <- read.delim("friends")

maxtemp <- friends_data[,2]
weight <- friends_data[,3]
height <- friends_data[,4]
Gender <- friends_data[,5]
Company <- friends_data[,6]

df<-data.frame(weight,height,maxtemp,Company)
# scatter plot. Change the point size
ggplot(df,aes(x=weight,y=height,size=maxtemp))+geom_point()

# Create a scatter plot by group. Color by group
ggplot(df,aes(x=weight,y=height,color=Company))+geom_point()

# Create a scatter plot by group. Shape by group
ggplot(df, aes(x=weight, y=height, group=Company)) +
  geom_point(aes(shape=Company),size=5)

# Create a scatter plot by group. Shape and color by group
ggplot(df, aes(x=weight, y=height, group=Company)) +
  geom_point(aes(shape=Company, color=Company),size=5)


# 3D scatter plot
plot_ly(x=weight, y=height , z=maxtemp, type="scatter3d", mode="markers")

# parallel coordinates
ggparcoord(data = df,
           columns = 1:3,
           groupColumn = "Company") 


# star plot
stars(df)

friends_MWH=friends_data[,2:4]
# install.packages("aplpack")
faces(friends_MWH, face.type = 5, scale = TRUE, label= Friend,  plot.faces = TRUE) 

# box-plot
boxplot(friends_MWH)

# computer covariance matrix
MC<-cov(friends_MWH)
# computer pearson correlation matrix
MP<-cor(friends_MWH,method = "pearson")
# computer Spearman correlation matrix
MS<-cov(friends_MWH,method = "spearman")

# scatter plot matrix
plot(friends_MWH)
pairs(friends_MWH)
ggpairs(friends_MWH)

# heatmap
dataM <- as.matrix(friends_MWH)
heatmap(dataM,cexCol = 1, cexRow=1)

#install.packages("pheatmap")
pheatmap(dataM)
