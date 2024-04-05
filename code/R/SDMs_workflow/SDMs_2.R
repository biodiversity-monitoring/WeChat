# SDMs：Absence and background points
# author: Biodiversity Monitoring


# 加载predicts包
library(predicts)  
# 获取预测变量文件名
f1 <- system.file("ex/bio.tif", package="predicts")  # bio.tif文件路径
f2 <- system.file("ex/biome.tif", package="predicts")  # biome.tif文件路径
# 使用c(f1, f2)创建栅格堆栈
r <- rast(c(f1, f2))  
# 选择5000个随机点，设置种子以确保示例总是有相同的随机样本。
set.seed(1963)  # 设置随机数种子以确保结果可重复
bg <- spatSample(r, 5000, "random", na.rm=TRUE, as.points=TRUE)  # 从栅格中随机抽取5000个点，去除NA，返回点格式



# 绘制第一个栅格层
plot(r, 1)
# 在图上添加随机抽取的点，点的大小为0.5
points(bg, cex=0.5)



# 创建一个地理范围（西经80至西经53，南纬39至南纬22）
e <- ext(-80, -53, -39, -22)  
# 在指定地理范围内随机抽取500个点
bg2 <- spatSample(r, 500, "random", na.rm=TRUE, as.points=TRUE, ext=e)  
# 绘制第一个栅格层
plot(r, 1)  
# 以红色线框绘制地理范围
lines(e, col="red", lwd=2)
# 在图上添加随机抽取的点，点的大小为0.5
points(bg2, cex=0.5)



# 获取acaule.rds文件的路径
acfile <- file.path(system.file(package="predicts"), "ex/acaule.rds")  
# 读取acaule.rds文件
ac <- readRDS(acfile)



# 对ac对象进行缓冲区分析，半径为50000米（50公里）
x <- buffer(ac, 50000) 
# 聚合缓冲区，合并重叠或相邻的区域
pol <- aggregate(x)  



# 从所有圆圈中随机采样
set.seed(999)  # 设置随机数种子
samp1 <- spatSample(pol, 500, "random")  # 从聚合后的多边形中随机抽取500个点



# 获取samp1中点对应的栅格单元编号
pcells <- cells(r, samp1)  
# 从pcells中删除重复的栅格单元编号
pcells <- unique(pcells[,2])  
# 根据栅格单元编号获取对应的坐标
xy <- xyFromCell(r, pcells)  



# 绘制聚合后的多边形，显示坐标轴
plot(pol, axes=TRUE)  
# 添加samp1中的点，使用加号标记，大小为0.5
points(samp1, pch="+", cex=.5)  
# 添加xy中的点，大小为0.75，使用实心圆标记，颜色为蓝色
points(xy, cex=0.75, pch=20, col='blue')  



# 将xy坐标转换为矢量格式，设置坐标参考系统为WGS84
spxy <- vect(xy, crs="+proj=longlat +datum=WGS84")  
# 找出spxy和x之间的交集，即在多边形内的点
xyInside <- intersect(spxy, x)  



# 创建一个新的、空的、更小的栅格
m <- crop(rast(r[[1]]), ext(x)+1)  # 以x的范围加1为基础，裁剪第一个栅格层
# 提取圆圈对应的栅格单元编号
v <- cells(m, x)  # 获取x中的多边形在m栅格中对应的单元编号
# 获取可以采样的唯一单元编号
v <- unique(v[,"cell"])  # 从v中获取唯一的栅格单元编号
head(v)  # 显示v的前几个值
# 显示结果
m[v] <- 1  # 将v对应的栅格单元值设置为1
plot(m)  # 绘制m栅格
lines(x)  # 在m栅格上绘制x多边形边界




