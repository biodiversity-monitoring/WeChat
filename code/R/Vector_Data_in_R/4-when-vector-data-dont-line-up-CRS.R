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

# 导入边界文件
State.Boundary.US <- st_read("NEON-DS-Site-Layout-Files/US-Boundary-Layers/US-State-Boundaries-Census-2014.shp")

# 查看数据结构
class(State.Boundary.US)

# 绘制美国各州的数据
plot(st_geometry(State.Boundary.US), 
     main="Map of Continental US State Boundaries\n US Census Bureau Data")



# 导入国家边界文件
Country.Boundary.US <- st_read("NEON-DS-Site-Layout-Files/US-Boundary-Layers/US-Boundary-Dissolved-States.shp")

# 查看数据结构
class(Country.Boundary.US)

# 绘制美国边界
plot(st_geometry(State.Boundary.US), 
     main="Map of Continental US State Boundaries\n US Census Bureau Data",
     border="gray40")

plot(st_geometry(Country.Boundary.US), 
     lwd=4, 
     border="gray18",
     add=TRUE)





# 导入一个点shapefile 
point_HARV <- st_read("NEON-DS-Site-Layout-Files/HARV/HARVtower_UTM18N.shp")
class(point_HARV)

# 绘制点 
plot(st_geometry(point_HARV), 
     pch = 19, 
     col = "purple",
     main="Harvard Fisher Tower Location")






# 绘制州边界  
plot(st_geometry(State.Boundary.US), 
     main="Map of Continental US State Boundaries \n with Tower Location",
     border="gray40")

# 添加美国边界轮廓 
plot(st_geometry(Country.Boundary.US), 
     lwd=4, 
     border="gray18",
     add=TRUE)

# 添加塔点位置
plot(st_geometry(point_HARV), 
     pch = 19, 
     col = "purple",
     add=TRUE)


??st_crs

# 查看塔点数据的 CRS
st_crs(point_HARV)

# 查看边界数据的 CRS
st_crs(State.Boundary.US)
st_crs(Country.Boundary.US)




# UTM中HARV的范围
extent(point_HARV)

# geographic中对象的范围
extent(State.Boundary.US)




??st_transform


# reproject data
point_HARV_WGS84 <- st_transform(point_HARV,
                                st_crs(State.Boundary.US))

# 新对象的 CRS 是多少
st_crs(point_HARV_WGS84)
# 范围看起来像十进制度数吗？
extent(point_HARV_WGS84)






# 绘制州边界  
plot(st_geometry(State.Boundary.US), 
     main="Map of Continental US State Boundaries\n With Fisher Tower Location",
     border="gray40")

# 添加美国边境轮廓
plot(st_geometry(Country.Boundary.US), 
     lwd=4, 
     border="gray18",
     add=TRUE)

# 添加塔点位置
plot(st_geometry(point_HARV_WGS84), 
     pch = 19, 
     col = "purple",
     add=TRUE)




