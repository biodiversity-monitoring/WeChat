# SDMs：Environmental data
# author: Biodiversity Monitoring


## ----sdm22a-------------------------------------------------------------------
# 加载predicts包，用于生态学预测和分析
library(predicts) 
# 寻找并设置包内的bio.tif文件路径
f <- system.file("ex/bio.tif", package="predicts") 
# 使用rast函数读取tif文件作为预测变量
predictors <- rast(f) 
# 显示预测变量的信息
predictors 
# 显示raster图层的名称
names(predictors) 
# 绘制预测变量的raster图层
plot(predictors) 


## ----sdm23a-------------------------------------------------------------------
# 加载geodata包，用于获取和处理地理数据
library(geodata) 
# 读取当前目录下的世界地图数据
wrld <- world(path=".") 
# 设置bradypus.csv文件的路径
file <- paste(system.file(package="predicts"), "/ex/bradypus.csv", sep="") 
# 读取bradypus.csv文件，该文件包含物种分布数据
bradypus <- read.table(file,  header=TRUE,  sep=',') 
# 我们不需要第一列，删除数据的第一列
bradypus  <- bradypus[,-1] 


## ----sdm23b-------------------------------------------------------------------
# 绘制预测变量raster的第一图层
plot(predictors, 1) 
# 在同一图上添加世界地图的边界线
lines(wrld)
# 添加物种分布点，颜色设为蓝色
points(bradypus, col='blue') 


## ----sdm24a-------------------------------------------------------------------
# 从预测变量raster中提取物种存在的位置的值
presvals <- extract(predictors, bradypus)
# 删除ID变量
presvals <- presvals[,-1] 
# 设置随机种子，以保证每次生成的随机点集相同
set.seed(0)
# 从预测变量raster中随机采样500个背景点
backgr <- spatSample(predictors, 500, "random", as.points=TRUE, na.rm=TRUE) 
# 提取背景点的值
absvals <- values(backgr) 

# 创建一个二元变量，表示物种存在(1)或不存在(0)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals))) 
# 将存在值、不存在值和二元变量合并为一个数据框
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals))) 
# 显示数据框的前几行
head(sdmdata) 
# 显示数据框的最后几行
tail(sdmdata)
# 显示数据框的统计摘要
summary(sdmdata)


## ----sdm24b-------------------------------------------------------------------
# 绘制sdmdata数据框中第2至5列的散点图矩阵，设置点的大小为0.1
pairs(sdmdata[,2:5], cex=0.1) 


## ----sdm24b2------------------------------------------------------------------
# 将sdmdata数据框保存为R数据存储文件sdm.Rds
saveRDS(sdmdata, "sdm.Rds") 
# 将presvals数据保存为R数据存储文件pvals.Rds
saveRDS(presvals, "pvals.Rds") 




