# author: Biodiversity Monitoring


#设置路径，提前将数据拷贝到路径下并解压(解压后的文件名为NEON-DS-Site-Layout-Files和NEON-DS-Airborne-Remote-Sensing)
setwd("C:/users/……")
getwd()

#安装R包
# install.packages("sf")
# install.packages("raster")

#加载R包
library(sf)
library(raster) 

# 导入一个面shapefile 
aoiBoundary_HARV <- st_read("NEON-DS-Site-Layout-Files/HARV/HarClip_UTMZ18.shp")
# 导入一个线shapefile
lines_HARV <- st_read( "NEON-DS-Site-Layout-Files/HARV/HARV_roads.shp")
# 导入一个点shapefile  
point_HARV <- st_read("NEON-DS-Site-Layout-Files/HARV/HARVtower_UTM18N.shp")

# 提前下载并解压NEON-DS-Airborne-Remote-Sensing数据集
# 导入栅格冠层高度模型 (CHM)
chm_HARV <- raster("NEON-DS-Airborne-Remote-Sensing/HARV/CHM/HARV_chmCrop.tif")



# 绘制完整 CHM
plot(chm_HARV,
     main="LiDAR CHM - Not Cropped\nNEON Harvard Forest Field Site")





# 裁剪 chm
chm_HARV_Crop <- crop(x = chm_HARV, y = aoiBoundary_HARV)

# 绘制完整 CHM
plot(extent(chm_HARV),
     lwd=4,col="springgreen",
     main="LiDAR CHM - Cropped\nNEON Harvard Forest Field Site",
     xlab="easting", ylab="northing")

plot(chm_HARV_Crop,
     add=TRUE)



# 查看数据在图中的情况
plot(st_geometry(aoiBoundary_HARV), lwd=8, border="blue",
     main = "Cropped LiDAR Canopy Height Model \n NEON Harvard Forest Field Site")

plot(chm_HARV_Crop, add = TRUE)



# 让我们来查看所有对象的范围
extent(chm_HARV)
extent(chm_HARV_Crop)
extent(aoiBoundary_HARV)






## 定义范围
# extent format (xmin,xmax,ymin,ymax)
new.extent <- extent(732161.2, 732238.7, 4713249, 4713333)
class(new.extent)


## 使用定义的范围裁剪
# 裁剪栅格
CHM_HARV_manualCrop <- crop(x = chm_HARV, y = new.extent)

# 绘制范围边界和新裁剪的栅格
plot(st_geometry(aoiBoundary_HARV), 
     main = "Manually Cropped Raster\n NEON Harvard Forest Field Site")
plot(new.extent, 
     col="brown", 
     lwd=4,
     add = TRUE)
plot(CHM_HARV_manualCrop, 
     add = TRUE)



# 提取AOI的树高
# 设置df=TRUE以返回一个数据框，而不是值的列表
tree_height <- raster::extract(x = chm_HARV, 
                       y = aoiBoundary_HARV, 
                       df = TRUE)

head(tree_height)

nrow(tree_height)






# 查看研究区域中树高的直方图
hist(tree_height$HARV_chmCrop, 
     main="Histogram of CHM Height Values (m) \nNEON Harvard Forest Field Site",
     col="springgreen",
     xlab="Tree Height", ylab="Frequency of Pixels")


summary(tree_height$HARV_chmCrop)






# 提取平均树高（使用栅格像素计算）
# 位于AOI面内
av_tree_height_AOI <- raster::extract(x = chm_HARV, 
                              y = aoiBoundary_HARV,
                              fun=mean, 
                              df=TRUE)

# 查看结果
av_tree_height_AOI




# 缓冲区的单位是什么
st_crs(point_HARV)

# 在塔的位置提取平均树高（树高由栅格像素值给出）
# 使用20米的缓冲区和均值函数（fun）
av_tree_height_tower <- raster::extract(x = chm_HARV, 
                                y = point_HARV, 
                                buffer=20,
                                fun=mean, 
                                df=TRUE)

# 查看数据
head(av_tree_height_tower)

# 多少像素点被提取
nrow(av_tree_height_tower)



