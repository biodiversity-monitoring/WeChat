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


# 导入一个面 shapefile
aoiBoundary_HARV <- st_read("NEON-DS-Site-Layout-Files/HARV/HarClip_UTMZ18.shp")

# 导入一个线 shapefile
lines_HARV <- st_read( "NEON-DS-Site-Layout-Files/HARV/HARV_roads.shp")

# 导入一个点 shapefile 
point_HARV <- st_read("NEON-DS-Site-Layout-Files/HARV/HARVtower_UTM18N.shp")


# 查看 shapefile 元数据
## ----view-shapefile-metadata-------------------------------------------
# view class
class(x = point_HARV)

# x= isn't actually needed; it just specifies which object
# view features count
length(point_HARV)

# view crs - note - this only works with the raster package loaded
crs(point_HARV)

# view extent- note - this only works with the raster package loaded
extent(point_HARV)

# view metadata summary
point_HARV



## ----shapefile-attributes----------------------------------------------
# 只需查看数据的属性和前 6 个属性值 
head(st_drop_geometry(lines_HARV))
# 我们的矢量数据对象中有多少个属性？
length(st_drop_geometry(lines_HARV))


## ----view-shapefile-attributes-----------------------------------------
# 仅查看 lines_HARV 空间对象的属性名称
names(st_drop_geometry(lines_HARV))




## ----explore-attribute-values------------------------------------------
# view all attributes in the lines shapefile within the TYPE fieldy
# 在 "TYPE"字段中查看线shapefile文件中的所有属性
lines_HARV$TYPE

# 查看 "TYPE"属性中的唯一值
unique(st_drop_geometry(lines_HARV)$TYPE)





## ----Subsetting-shapefiles---------------------------------------------
# select features that are of TYPE "footpath"
# could put this code into other function to only have that function work on
# "footpath" lines
lines_HARV[lines_HARV$TYPE == "footpath",]

# save an object with only footpath lines
footpath_HARV <- lines_HARV[lines_HARV$TYPE == "footpath",]
footpath_HARV


## ----plot-subset-shapefile, fig.cap="Foothpaths at NEON Harvard Forest Field Site."----
# plot just footpaths
plot(st_geometry(footpath_HARV),
     lwd=6,
     main="NEON Harvard Forest Field Site\n Footpaths")


## ----plot-subset-shapefile-unique-colors, fig.cap="Foothpaths at NEON Harvard Forest Field Site with color varied by feature type."----
# plot just footpaths
plot(st_geometry(footpath_HARV),
     col=c("green","blue"), # set color for each feature 
     lwd=6,
     main="NEON Harvard Forest Field Site\n Footpaths \n Feature one = blue, Feature two= green")








## ----convert-to-factor-------------------------------------------------
# view the original class of the TYPE column
class(lines_HARV$TYPE)
lines_HARV$TYPE <- as.factor(lines_HARV$TYPE)
class(lines_HARV$TYPE)

# view levels or categories - note that there are no categories yet in our data!
# the attributes are just read as a list of character elements.
levels(lines_HARV$TYPE)

# how many features are in each category or level?
summary(lines_HARV$TYPE)



# count the number of unique values or levels
length(levels(lines_HARV$TYPE))

# create a color palette of 4 colors - one for each factor level
roadPalette <- c("blue","green","grey","purple")
roadPalette
# create a vector of colors - one for each feature in our vector object
# according to its attribute value
roadColors <- c("blue","green","grey","purple")[lines_HARV$TYPE]
roadColors

# plot the lines data, apply a diff color to each factor level)
plot(st_geometry(lines_HARV), 
     col=roadColors,
     lwd=3,
     main="NEON Harvard Forest Field Site\n Roads & Trails")



## ----adjust-line-width, fig.cap="Roads and trails at NEON Harvard Forest Field Site with color varied by attribute factor value and uniformly thick line width."----
# make all lines thicker
plot(st_geometry(lines_HARV), 
     col=roadColors,
     main="NEON Harvard Forest Field Site\n Roads & Trails\n All Lines Thickness=6",
     lwd=6)



## ----line-width-unique, fig.cap="Roads and trails at NEON Harvard Forest Field Site with color and line width varied by attribute factor value."----
class(lines_HARV$TYPE)
levels(lines_HARV$TYPE)
# create vector of line widths
lineWidths <- (c(1,2,3,4))[lines_HARV$TYPE]
# adjust line width by level
# in this case, boardwalk (the first level) is the widest.
plot(st_geometry(lines_HARV), 
     col=roadColors,
     main="NEON Harvard Forest Field Site\n Roads & Trails \n Line width varies by TYPE Attribute Value",
     lwd=lineWidths)




#添加图例

## ----add-legend-to-plot, fig.cap="Roads and trails at NEON Harvard Forest Field Site with color varied by attribute factor value and with a default legend."----
plot(st_geometry(lines_HARV), 
     col=roadColors,
     main="NEON Harvard Forest Field Site\n Roads & Trails\n Default Legend")

# we can use the color object that we created above to color the legend objects
roadPalette

# add a legend to our map
legend("bottomright",   # location of legend
      legend=levels(lines_HARV$TYPE), # categories or elements to render in 
			 # the legend
      fill=roadPalette) # color palette to use to fill objects in legend.



## ----modify-legend-plot, fig.cap="Roads and trails at NEON Harvard Forest Field Site with color varied by attribute factor value and with a modified legend."----

plot(st_geometry(lines_HARV), 
     col=roadColors,
     main="NEON Harvard Forest Field Site\n Roads & Trails \n Modified Legend")
# add a legend to our map
legend("bottomright", 
       legend=levels(lines_HARV$TYPE), 
       fill=roadPalette, 
       bty="n", # turn off the legend border
       cex=.8) # decrease the font / legend size



## ----plot-different-colors, fig.cap="Roads and trails at NEON Harvard Forest Field Site with manually set colors and with a modified legend."----

# manually set the colors for the plot!
newColors <- c("springgreen", "blue", "magenta", "orange")
newColors

# plot using new colors
plot(st_geometry(lines_HARV), 
     col=(newColors)[lines_HARV$TYPE],
     main="NEON Harvard Forest Field Site\n Roads & Trails \n Pretty Colors")

# add a legend to our map
legend("bottomright", 
       levels(lines_HARV$TYPE), 
       fill=newColors, 
       bty="n", cex=.8)

