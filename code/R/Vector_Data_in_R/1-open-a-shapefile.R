# author: Biodiversity Monitoring

#设置路径，提前将数据拷贝到路径下并解压(解压后的文件名为NEON-DS-Site-Layout-Files)
setwd("C:/users/……")
getwd()

#安装R包
# install.packages("sf")
# install.packages("raster")

#加载R包
library(sf)
library(raster) 


# 指定shapefile文件的路径
path <- "NEON-DS-Site-Layout-Files/HARV/HarClip_UTMZ18.shp"

# 使用sf包读取shapefile文件
aoiBoundary_HARV <- st_read(dsn = path)



## ----view-metadata-----------------------------------------------------
# view just the class for the shapefile
class(aoiBoundary_HARV)

# view just the crs for the shapefile
crs(aoiBoundary_HARV)

# view just the extent for the shapefile
extent(aoiBoundary_HARV)

# view all metadata at same time
aoiBoundary_HARV


## ----Shapefile-attributes-2--------------------------------------------
# alternate way to view attributes 
# 使用 st_drop_geometry 来获取属性数据
attributes <- st_drop_geometry(aoiBoundary_HARV)
attributes
# 访问属性数据
attributes$id


# 绘制一个shapefile
# create a plot of the shapefile
# 'lwd' sets the line width
# 'col' sets internal color
# 'border' sets line color
plot(aoiBoundary_HARV, col="cyan1", border="black", lwd=3,
     main="AOI Boundary Plot")



# 绘制多个shapefile

# 导入line shapefile
lines_HARV <- st_read("NEON-DS-Site-Layout-Files/HARV/HARV_roads.shp")
# 导入point shapefile
point_HARV <- st_read("NEON-DS-Site-Layout-Files/HARV/HARVtower_UTM18N.shp")

# 绘制区域边界
plot(st_geometry(aoiBoundary_HARV), col = "lightgreen", main="NEON Harvard Forest\nField Site")
# 绘制道路
plot(st_geometry(lines_HARV), add = TRUE)
# 绘制点
plot(st_geometry(point_HARV), add = TRUE, pch = 19, col = "purple")








