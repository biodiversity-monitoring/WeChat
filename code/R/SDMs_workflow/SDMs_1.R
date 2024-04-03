# SDMs Data preparation
# author: Biodiversity Monitoring

# install.packages(c("terra", "remotes", "predicts", "geodata"))


##导入出现数据##################################################################
## ----sdm10A-------------------------------------------------------------------
# 引入必要的R包
library(terra) # 用于空间数据分析
library(predicts) # 提供预测数据集和相关功能
# 构造文件路径，指向包内的示例数据
filename <- file.path(system.file(package="predicts"), "ex/bradypus.csv")
# 打印出文件的基本名称，用于确认文件路径正确
basename(filename) # 返回文件的基本名称

## ----sdm11A-------------------------------------------------------------------
# 读取之前获取路径的CSV文件
bradypus <- read.csv(filename)
# 打印CSV文件的前几行，以检查数据格式
head(bradypus)
# 选择CSV文件中的第2列和第3列，其余列不需要
bradypus <- bradypus[,2:3] # 提取经度和纬度列
# 再次打印处理后的前几行数据，确认只选取了指定列
head(bradypus)

## ----sdm11B, eval=FALSE-------------------------------------------------------
## 使用geodata包检索Solanum属acaule种的地理位置数据
## 这一步不会在此执行，但展示了如何获取一种植物种的地理数据
## acaule <- geodata::sp_occurrence("Solanum", "acaule*", geo=FALSE)

## ----sdm2---------------------------------------------------------------------
# 加载保存的S. acaule数据
acfile <- file.path(system.file(package="predicts"), "ex/acaule.csv") # 构造文件路径
acaule <- read.csv(acfile) # 读取CSV文件
# 查看数据框的维度，即行数和列数
dim(acaule)
# 选择具有经纬度数据的记录
colnames(acaule) # 打印列名，以确定经纬度列
acgeo <- subset(acaule, !is.na(lon) & !is.na(lat)) # 根据经纬度非空筛选数据
dim(acgeo) # 打印筛选后的数据维度
# 展示一些筛选后的数据值
acgeo[1:5, c(1:5,7:10)] # 选择前5行，指定的列

## ----sdm3---------------------------------------------------------------------
# 引入geodata包，用于访问地理数据
library(geodata)
# 创建world对象，用于后续绘图
wrld <- world(path=".")
# 绘制世界地图作为背景
plot(wrld, xlim=c(-110,60), ylim=c(-80,40), col="light yellow", border="light gray")
# 在地图上添加点
points(acgeo$lon, acgeo$lat, col='red', pch=20) # 将S. acaule的位置以红点标出



##数据清理######################################################################
## ----sdm4a--------------------------------------------------------------------
# 查看指定行的数据，这里是第303行和885行的1至10列
acgeo[c(303,885),1:10]

## ----sdm4b--------------------------------------------------------------------
# 选取经度为0的记录
lonzero <- subset(acgeo, lon==0)
# 展示所有经度为0的记录，但只显示前13列
lonzero[, 1:13]



##重复记录######################################################################
## ----sdm5a--------------------------------------------------------------------
# 检查lonzero数据框前10列中的重复记录
dups <- duplicated(lonzero[, 1:10])
# 移除重复记录
lonzero  <-  lonzero[!dups, ] # 这里应使用!dups来保留非重复记录
# 展示处理后的数据，前13列
lonzero[,1:13]

## ----sdm5b--------------------------------------------------------------------
# 按(亚)种类区分重复值
# dups2 <- duplicated(acgeo[, c('species', 'lon', 'lat')])
# 忽略(亚)种类及其它命名差异，仅根据经纬度去重
dups2 <- duplicated(acgeo[, c('lon', 'lat')])
# 计算重复记录的数量
sum(dups2)
# 保留非重复的记录
acg <- acgeo[!dups2, ]

## ----sdm5c--------------------------------------------------------------------
# 筛选经度和纬度都大于0的记录，然后将其值取反
i <- acg$lon > 0 & acg$lat > 0
acg$lon[i] <- -1 * acg$lon[i]
acg$lat[i] <- -1 * acg$lat[i]
# 进一步筛选经度小于-60，纬度大于-50的记录
acg <- acg[acg$lon < -60 & acg$lat > -50, ]



##交叉检查######################################################################
## ----sdm6a--------------------------------------------------------------------
# 引入terra包
library(terra)
# 将acg数据框转换为SpatVector对象
acv <- vect(acg, geom=c("lon", "lat"), crs="+proj=longlat +datum=WGS84")
# 显示acv对象的类别
class(acv)

## ----sdm6b--------------------------------------------------------------------
# 使用extract函数，从世界地图wrld中提取acv中的点对应的地理位置信息
ovr <- extract(acv, wrld)

## ----sdm6c--------------------------------------------------------------------
# 显示提取结果的前几行
head(ovr)
# 从结果中提取国家名
cntr <- ovr$NAME_0

## ----sdm6d--------------------------------------------------------------------
# 找出缺失国家名的记录位置
i <- which(is.na(cntr))
i
# 找出国家名与acv数据中的国家名不匹配的记录位置
j <- which(cntr != acv$country)
# 对于不匹配的记录，绑定多边形和点的国家名
m <- cbind(cntr[j], acg$country[j])
# 设置列名
colnames(m) <- c("polygons", "acaule")
m

## ----sdm6e--------------------------------------------------------------------
# 绘制acv的SpatVector
plot(acv)
# 添加世界地图边界，用蓝色线条
lines(wrld, col='blue', lwd=2)
# 将不匹配的点以红色加粗点标出
points(acv[j, ], col='red', pch=20, cex=2)



##地理配准######################################################################
## ----sdm8---------------------------------------------------------------------
# 从acaule数据中筛选出缺失经纬度但具有地点信息的记录
georef <- subset(acaule, (is.na(lon) | is.na(lat)) & ! is.na(locality) )
# 查看筛选结果的维度
dim(georef)
# 展示前几行数据
georef[1:3,1:13]

## ----sdm9, eval=FALSE---------------------------------------------------------
# 以下代码不执行，展示如何使用geocode函数根据地名获取地理坐标
## georef$cloc[4]
## #b <- geocode(georef$cloc[4], geo_key="abcdef" )
## #b



##抽样偏差######################################################################
## ----sdm100A------------------------------------------------------------------
# 使用acv的空间范围创建一个SpatRaster对象
r <- rast(acv)
# 设置单元格分辨率为1度
res(r) <- 1
# 将SpatRaster的范围稍微扩大
r <- extend(r, ext(r)+1)
# 示例：随机选择一个点
set.seed(13)
acsel <- spatSample(acv, size=1, "random", strata=r) 
# 为了展示方法和结果，绘制r的多边形和选中的点
p <- as.polygons(r)
plot(p, border='gray')
points(acv)
# 用红色的x标记选中的点
points(acsel, cex=1, col='red', pch='x')

## ----sdm12--------------------------------------------------------------------
# 构建文件路径
file <- paste(system.file(package="predicts"), '/ex/acaule.rds', sep='')
# 读取RDS文件
acsel <- readRDS(file)




