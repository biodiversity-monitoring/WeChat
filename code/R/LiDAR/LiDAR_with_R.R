
## 加载必需的包
# terra包主要用于空间数据分析，特别是栅格数据
library(terra)
# neonUtilities包用于下载和处理NEON提供的生态数据
library(neonUtilities)


# 设置一个变量wd指向你的数据所在目录
# 如: wd="C:/Biodiversity_Monitoring/tutorials/LiDAR/"，需以/结尾
wd="C:/.../" 
# 使用setwd函数将R的工作目录设置为刚才定义的路径
setwd(wd)


## 下载反射率数据
# 通过byTileAOP函数从NEON数据库下载特定地点、年份的空间数据
byTileAOP(dpID='DP3.30024.001',
           site='SJER',
           year='2021',
           easting=257500,
           northing=4112500,
           check.size=TRUE, # 若不想进行y/n确认，可以设置为FALSE
           savepath = wd)


# 定义数字表面模型（DSM）和数字地形模型（DTM）文件的完整路径，这些路径包括前面设定的工作目录
dsm_file <- paste0(wd,"DP3.30024.001/neon-aop-products/2021/FullSite/D17/2021_SJER_5/L3/DiscreteLidar/DSMGtif/NEON_D17_SJER_DP3_257000_4112000_DSM.tif")
dtm_file <- paste0(wd,"DP3.30024.001/neon-aop-products/2021/FullSite/D17/2021_SJER_5/L3/DiscreteLidar/DTMGtif/NEON_D17_SJER_DP3_257000_4112000_DTM.tif")


## 导入DSM
# 使用rast函数加载DSM文件到dsm对象
dsm <- rast(dsm_file)
# 打印dsm对象的信息，以了解栅格数据的基本情况
dsm
# 绘制数字表面模型（DSM）的图像
plot(dsm, main="Lidar Digital Surface Model \n SJER, California")


## 绘制DTM
# 以同样的方法导入并绘制数字地形模型（DTM）
dtm <- rast(dtm_file)
plot(dtm, main="Lidar Digital Terrain Model \n SJER, California")


## 计算并绘制冠层高度模型（CHM）
# 通过对DSM和DTM进行数学运算（相减）来创建冠层高度模型（CHM）
chm <- dsm - dtm
# 打印CHM的属性，以便查看
chm
# 绘制冠层高度模型的图像
plot(chm, main="Lidar CHM - SJER, California")


## 使用writeRaster函数将计算出的CHM保存为GeoTIFF格式
writeRaster(chm, "CHM_SJER.tif", overwrite=TRUE)
