

#安装R包
# install.packages("sf")
# install.packages("raster")

#加载R包
library(sf)
library(raster) 

# 导入一个面 shapefile
aoiBoundary_HARV <- st_read("NEON-DS-Site-Layout-Files/HARV/HarClip_UTMZ18.shp")
# 导入一个线 shapefile
lines_HARV <- st_read( "NEON-DS-Site-Layout-Files/HARV/HARV_roads.shp")
# 导入一个点 shapefile 
point_HARV <- st_read("NEON-DS-Site-Layout-Files/HARV/HARVtower_UTM18N.shp")





# 转换为因子
lines_HARV$TYPE <- as.factor(lines_HARV$TYPE)
# 查看因子水平
levels(lines_HARV$TYPE)
# 创建线宽值向量
lineWidth <- c(2,4,3,8)[lines_HARV$TYPE]
lineWidth

# 创建由 4 种颜色组成的调色板--每个因子级别一种颜色
roadPalette <- c("blue","green","grey","purple")
roadPalette
# 创建一个颜色矢量--矢量对象中的每个特征都有一个颜色矢量
# 根据其属性值
roadColors <- c("blue","green","grey","purple")[lines_HARV$TYPE]
roadColors

# 创建线宽值向量
lineWidth <- c(2,4,3,8)[lines_HARV$TYPE]
lineWidth

# 在这种情况下，boardwalk (the first level)是最宽的
plot(st_geometry(lines_HARV), 
     col=roadColors,
     main="NEON Harvard Forest Field Site\n Roads & Trails \nLine Width Varies by Type Attribute Value",
     lwd=lineWidth)



## ----添加图例
plot(st_geometry(lines_HARV), 
     col=roadColors,
     main="NEON Harvard Forest Field Site\n Roads & Trails\n Default Legend")

# 我们可以使用上面创建的颜色对象来为图例着色
roadPalette

# 为地图添加图例
legend("bottomright", 
       legend=levels(lines_HARV$TYPE), 
       fill=roadPalette, 
       bty="n", # turn off the legend border
       cex=.8) # decrease the font / legend size



# 绘制坐个shapefile
plot(st_geometry(aoiBoundary_HARV), 
     col = "grey93", 
     border="grey",
     main="NEON Harvard Forest Field Site")

plot(st_geometry(lines_HARV), 
     col=roadColors,
     add = TRUE)

plot(st_geometry(point_HARV), 
     add  = TRUE, 
     pch = 19, 
     col = "purple")

# 将绘图分配给一个对象，以方便修改！
plot_HARV<- recordPlot()



## 创建自定义标签

# 创建所有标签的列表
labels <- c("Tower", "AOI", levels(lines_HARV$TYPE))
labels

# 绘制地图
plot_HARV

# 为地图添加图例
legend("bottomright", 
       legend=labels, 
       bty="n", # turn off the legend border
       cex=.8) # decrease the font / legend size




## ----添加颜色

# 我们已经有了上面使用过的颜色列表 - 我们可以在图例中使用它
roadPalette

# 创建要使用的颜色列表
plotColors <- c("purple", "grey", roadPalette)
plotColors

# 绘制地图
plot_HARV

# 为地图添加图例
legend("bottomright", 
       legend=labels, 
       fill=plotColors,
       bty="n", # turn off the legend border
       cex=.8) # decrease the font / legend size



## ----自定义符号

# 创建 pch 值列表
# 这些符号将用于每个图例值
# ?pch 将提供有关值的更多信息
plotSym <- c(16,15,15,15,15,15)
plotSym

# 绘制多个 shapefiles
plot_HARV

# 要创建自定义图例，我们需要伪造它
legend("bottomright", 
       legend=labels,
       pch=plotSym, 
       bty="n", 
       col=plotColors,
       cex=.8)



## ----修改图例
# 创建 line object
lineLegend = c(NA,NA,1,1,1,1)
lineLegend
plotSym <- c(16,15,NA,NA,NA,NA)
plotSym

# 绘制多个 shapefiles
plot_HARV

# 创建自定义图例
legend("bottomright", 
       legend=labels, 
       lty = lineLegend,
       pch=plotSym, 
       bty="n", 
       col=plotColors,
       cex=.8)



