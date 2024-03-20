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




#  导入.csv文件
plot.locations_HARV <- 
  read.csv("NEON-DS-Site-Layout-Files/HARV/HARV_PlotLocations.csv",
           stringsAsFactors = FALSE)

# 查看数据结构
str(plot.locations_HARV)


# 查看列名
names(plot.locations_HARV)



## 检查坐标
# 查看 X 列和 Y 列的前 6 行
head(plot.locations_HARV$easting)
head(plot.locations_HARV$northing)

# 注意，也可以使用 COLUMN 编号调用相同的两列
# 查看 X 列和 Y 列的前 6 行
head(plot.locations_HARV[,1])
head(plot.locations_HARV[,2])



## 查看 CRS 信息
# 查看 X 列和 Y 列的前 6 行
head(plot.locations_HARV$geodeticDa)
head(plot.locations_HARV$utmZone)



# 导入线shapefile
lines_HARV <- st_read("NEON-DS-Site-Layout-Files/HARV/HARV_roads.shp")

# 查看 CRS
st_crs(lines_HARV)

# 查看 extent
extent(lines_HARV)




# 创建sf对象
plot.locations_HARV <- st_as_sf(plot.locations_HARV, coords = c(x = "easting", y = "northing"))
plot.locations_HARV
# 查看 CRS
st_crs(plot.locations_HARV)


## 方案 1 和方案 2只需运行一个！

# 方案 1：借用CRS
# 将lines_HARV的crs指定给plot.locations_HARV
plot.locations_HARV <- st_set_crs(plot.locations_HARV, st_crs(lines_HARV))
# 查看 CRS
st_crs(plot.locationsSp_HARV)


# 方案 2：指定CRS
# 通过EPSG码指定plot.locations_HARV的crs
st_crs(plot.locations_HARV) <- st_crs(32618)
# 查看 CRS
st_crs(plot.locations_HARV)




# 绘制sf对象
plot(st_geometry(plot.locationsSp_HARV), 
     main="Map of Plot Locations")





# 创建边界对象
aoiBoundary_HARV <- st_read("NEON-DS-Site-Layout-Files/HARV/HarClip_UTMZ18.shp")

# 绘制Boundary
plot(st_geometry(aoiBoundary_HARV),
     main="AOI Boundary\nNEON Harvard Forest Field Site")

# 添加样地位置
plot(st_geometry(plot.locationsSp_HARV), 
     pch=8, add=TRUE)

# 没有样地被添加, why? CRS?
# view CRS of each
st_crs(aoiBoundary_HARV)
st_crs(plot.locationsSp_HARV)





#查看范围
extent(aoiBoundary_HARV)
extent(plot.locationsSp_HARV)


plot(extent(plot.locationsSp_HARV),
     col="purple", 
     xlab="easting",
     ylab="northing", lwd=8,
     main="Extent Boundary of Plot Locations \nCompared to the AOI Spatial Object",
     ylim=c(4712400,4714000)) # extent the y axis to make room for the legend

plot(extent(aoiBoundary_HARV), 
     add=TRUE,
     lwd=6,
     col="springgreen")

legend("bottomright",
       #inset=c(-0.5,0),
       legend=c("Layer One Extent", "Layer Two Extent"),
       bty="n", 
       col=c("purple","springgreen"),
       cex=.8,
       lty=c(1,1),
       lwd=6)




plotLoc.extent <- extent(plot.locationsSp_HARV)
plotLoc.extent
# 从空间绘图位置层抓取 x 和 y 的最小值和最大值
xmin <- plotLoc.extent@xmin
xmax <- plotLoc.extent@xmax
ymin <- plotLoc.extent@ymin
ymax <- plotLoc.extent@ymax

# 使用 xlim 和 ylim 调整绘图范围
plot(st_geometry(aoiBoundary_HARV),
     main="NEON Harvard Forest Field Site\nModified Extent",
     border="darkgreen",
     xlim=c(xmin,xmax),
     ylim=c(ymin,ymax))

plot(st_geometry(plot.locationsSp_HARV), 
     pch=8,
		 col="purple",
		 add=TRUE)

# 添加图例
legend("bottomright", 
       legend=c("Plots", "AOI Boundary"),
       pch=c(8,NA),
       lty=c(NA,1),
       bty="n", 
       col=c("purple","darkgreen"),
       cex=.8)


# 导出shapefile

st_write(plot.locationsSp_HARV, "样地位置.shp")




