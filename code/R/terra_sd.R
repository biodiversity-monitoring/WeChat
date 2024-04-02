
# 1.导入和准备数据______________________________________________________________
## ----a1-----------------------------------------------------------------------
# 如果rspat包没有安装，则通过remotes包从GitHub安装它
if (!require("rspat")) remotes::install_github('rspatial/rspat')
library(rspat)
library(terra)

## ----a1b, error=TRUE----------------------------------------------------------
# 寻找包中的wildpot.csv文件，这可能因为文件不存在而导致错误
f <- system.file("wildpot.csv", package="rspat")
# 输出文件名，确认文件路径正确
basename(f)
# 读取CSV文件到DataFrame
v <- read.csv(f)

## ----a3-----------------------------------------------------------------------
# 将经度和纬度的度、分、秒转换为数字类型
for (i in c('LongD', 'LongM', 'LongS', 'LatD', 'LatM', 'LatS')) {
  v[, i] <- as.numeric(v[,i])
}
# 将经度转换为十进制格式
v$lon <- -1 * (v$LongD + v$LongM / 60 + v$LongS / 3600)
# 将纬度转换为十进制格式
v$lat <- v$LatD + v$LatM / 60 + v$LatS / 3600
# 为南半球的纬度值加上负号
v$lat[v$LatH == 'S'] <- -1 * v$lat[v$LatH == 'S']
# 展示转换后的前几行数据
head(v)

## ----a4-----------------------------------------------------------------------
# 获取美洲国家的SpatVector数据
cn <- spat_data("pt_countries")
# 检查cn的数据类型，应为SpatVector
class(cn)

## ----a5-----------------------------------------------------------------------
# 绘制美洲国家地图，并在地图上标出野生马铃薯的观测点
plot(cn, xlim=c(-120, -40), ylim=c(-40,40), axes=TRUE)
points(v$lon, v$lat, cex=.5, col='red')

## ----a6-----------------------------------------------------------------------
# 将野生马铃薯数据转换为SpatVector，为了后续的空间分析
sp <- vect(v, crs="+proj=longlat +datum=WGS84")



# 2.汇总统计____________________________________________________________________
## ----b1-----------------------------------------------------------------------
# 展示每个国家的观测数量
table(v$COUNTRY)
# 将国家名称统一为大写，避免由于大小写不一致导致的数据不一致问题
v$COUNTRY <- toupper(v$COUNTRY)
table(v$COUNTRY)
# 对SpatVector应用同样的国家名称大写转换
sp$COUNTRY <- toupper(sp$COUNTRY)

## ----b2-----------------------------------------------------------------------
# 使用空间查询确定每个观测点的国家
vv <- intersect(sp[, "COUNTRY"], cn)
# 将查询结果的第一列名称更改为ptCountry
names(vv)[1] <- "ptCountry"
# 展示查询后的头几行数据，以验证国家信息是否正确关联
head(vv)
# 统计每个国家的观测数量
table(vv$COUNTRY)

## ----b3-----------------------------------------------------------------------
# 修正国家名称的拼写差异和处理空值
vv$COUNTRY[is.na(vv$COUNTRY)] <- ""
vv$COUNTRY[vv$COUNTRY=="UNITED STATES, THE"] <- "UNITED STATES"
vv$COUNTRY[vv$COUNTRY=="BRASIL"] <- "BRAZIL"
# 找出原始数据和空间查询国家名称不一致的记录
i <- which(toupper(vv$ptCountry) != vv$COUNTRY)
# 展示不一致记录的详细信息
as.data.frame(vv[i,])
# 在地图上标出这些不一致的观测点，以红色标记
plot(cn, xlim=c(-120, -40), ylim=c(-40,40), axes=TRUE)
points(sp, cex=.25, pch='+', col='blue')
points(vv[i,], col='red', pch='x', cex=1.5)

## ----b4-----------------------------------------------------------------------
# 计算每个国家的物种丰富度，并转换为数据框形式
spc <- tapply(v$SPECIES, sp$COUNTRY, function(x)length(unique(x)))
spc <- data.frame(COUNTRY=names(spc), nspp = spc)
# 将国界数据中的国家名称统一修改，便于合并
cn$COUNTRY[cn$COUNTRY=="UNITED STATES, THE"] <- "UNITED STATES"
cn$COUNTRY[cn$COUNTRY=="BRASIL"] <- "BRAZIL"
# 将物种丰富度数据合并到国界SpatVector中，并绘制物种丰富度地图
cns <- merge(cn, spc, by="COUNTRY", all.x=TRUE)
plot(cns, "nspp", col=rev(terrain.colors(25)), breaks=c(1,5,10,20,30,40,90))

## ----b5-----------------------------------------------------------------------
# 为每个国家和每个物种创建一个交叉表格，用以分析物种在不同国家的分布
tb <- table(v[ c('COUNTRY', 'SPECIES')])
# 展示表格的维度，显示其是一个大表格
dim(tb)
# 展示表格中的两列，作为示例
tb[,2:3]



# 3.投影空间数据________________________________________________________________
## ----c1-----------------------------------------------------------------------
# 定义要使用的坐标参考系统（CRS）- Lambert等面积方位投影
laea <-"+proj=laea  +lat_0=0 +lon_0=-80"
# 将国界数据和马铃薯数据投影到新的CRS
clb <- project(cn, laea)
pts <- project(sp, laea)
# 绘制投影后的国界，并在其上绘制马铃薯观测点
plot(clb)
points(pts, col='red', cex=.5)


# 4.物种丰富度__________________________________________________________________
## ----d1-----------------------------------------------------------------------
# 创建一个新的空间栅格对象，分辨率设置为200km
r <- rast(clb)
res(r) <- 200000

## ----d2-----------------------------------------------------------------------
# 使用rasterize函数，计算每个栅格内的物种丰富度，并绘制结果
rich <- rasterize(pts, r, "SPECIES", function(x, ...) length(unique(na.omit(x))))
plot(rich)
# 在地图上绘制国界线
lines(clb)

## ----d3-----------------------------------------------------------------------
# 使用rasterize函数，计算每个栅格内的观测数量，并绘制结果
obs <- rasterize(pts, r, field="SPECIES", fun=function(x, ...)length((na.omit(x))) )
plot(obs)
# 在地图上绘制国界线
lines(clb)

## ----d3b----------------------------------------------------------------------
# 绘制观测数量与物种丰富度之间的关系图
plot(obs, rich, cex=1, xlab="Observations", ylab="Richness")

## ----d4-----------------------------------------------------------------------
# 准备数据，通过对每个纬度上的物种进行汇总，以探究物种丰富度的纬度梯度
d <- v[, c('lat', 'SPECIES')]
d$lat <- round(d$lat)
g <- tapply(d$SPECIES, d$lat, function(x) length(unique(na.omit(x))) )
# 绘制纬度梯度图，并应用移动平均线进行平滑
plot(names(g), g)
lines(names(g), raster::movingFun(g, 3))



# 5.范围大小____________________________________________________________________
## ----f2-----------------------------------------------------------------------
# 首先获取所有独特物种的列表
spp <- unique(pts$SPECIES)
# 初始化一个与物种数量相同长度的NA向量，用于存储每个物种的最大距离（maxD）
maxD <- rep(NA, length(spp))
# 遍历每个物种，计算其观测点之间的最大距离
for (s in 1:length(spp)) {
  # 获取当前物种s的所有观测点坐标
  p <- pts[pts$SPECIES == spp[s], ]
  # 如果观测点少于2个，则无法计算距离，继续下一个循环
  if (nrow(p) < 2) next
  # 计算距离矩阵
  d <- as.matrix(distance(p))
  # 将对角线设置为NA，因为它表示点到自身的距离
  diag(d) <- NA
  # 从距离矩阵中找到最大值，即物种s的最大距离
  maxD[s] <- max(d, na.rm=TRUE)
}
# 绘制物种最大距离的分布图，注意到典型的J形曲线
plot(rev(sort(maxD))/1000, ylab="maxD (km)")

## ----f3-----------------------------------------------------------------------
# 初始化一个NA向量，用于存储每个物种50km半径圆覆盖面积的计算结果
CA <- rep(NA, length(spp))
# 遍历每个物种
for (s in 1:length(spp)) {
  # 获取当前物种s的所有观测点坐标
  p <- pts[pts$SPECIES == spp[s], ]
  # 对每个观测点运行"circles"模型，即围绕每个点创建50km的缓冲区
  m <- aggregate(buffer(p, 50000))
  # 计算所有缓冲区的总面积
  CA[s] <- expanse(m)
}
# 将面积标准化为一个50km半径圆的面积
CA <- CA / (pi * 50000^2)
# 绘制物种CA50的分布图
plot(rev(sort(CA)), ylab='CA50')

## ----f4-----------------------------------------------------------------------
# 初始化一个列表，用于存储每个物种的凸包
hull <- list()
# 遍历每个物种
for (s in 1:length(spp)) {
  # 获取当前物种s的独特观测点坐标
  p <- unique(pts[pts$SPECIES == spp[s], ])
  # 至少需要三个独特点来构建凸包
  if (nrow(p) > 3) {
    # 计算凸包
    h <- convHull(p)
    # 检查生成的凸包是否为多边形类型
    if (geomtype(h) == "polygons") {
      # 将凸包存储在列表中
      hull[[s]] <- h
    }
  }
}

## ----f4b----------------------------------------------------------------------
# 判断哪些元素是NULL，即哪些物种没有足够的观测点来构建凸包
i <- which(!sapply(hull, is.null))
# 根据索引i，筛选出非NULL的凸包列表
h <- hull[i]
# 将筛选后的凸包列表合并成一个SpatVector
hh <- do.call(rbind, h)
# 绘制合并后的凸包，展示所有物种的地理分布范围
plot(hh)

## ----f4c----------------------------------------------------------------------
# 计算每个凸包的面积
ahull <- expanse(hh)
# 将面积从平方米转换为千平方米，并按递减顺序绘制面积图，展示物种范围大小的分布
plot(rev(sort(ahull))/1000, ylab="Area of convex hull (km^2)")

## -----------------------------------------------------------------------------
# 为每个物种初始化一个NA数组，用于存放凸包面积
cHull <- rep(NA, length(spp))
# 将有凸包面积的物种对应的面积填入cHull中
cHull[i] <- ahull

## ----f5-----------------------------------------------------------------------
# 将maxD（物种间最大距离）、CA（50km半径圆覆盖面积的标准化值）、cHull（凸包面积）合并为一个数据框
d <- cbind(maxD, CA, cHull)
# 使用pairs函数绘制这三个变量的散点图矩阵，探索它们之间的关系
pairs(d)


################################################################################


# 练习1.绘制不同分辨率下的物种丰富度图
# 定义分辨率列表，单位是千米
resolutions <- c(50, 100, 250, 500)
# 准备绘图设定，每个分辨率两张图（观测数和物种丰富度），总共4个分辨率
par(mfrow=c(4, 2))
# 循环处理每个分辨率
for (res in resolutions) {
  # 根据当前分辨率创建栅格模板，单位转换为米（*1000）
  r <- rast(ext(pts), res=res * 1000, crs=crs(pts))
  # 观测数量地图：计算每个栅格内的观测点数
  obs_map <- rasterize(pts, r, fun=length)
  # 物种丰富度地图：计算每个栅格内的独特物种数
  rich_map <- rasterize(pts, r, field="SPECIES", fun=function(x) length(unique(na.omit(x))))
  # 绘制观测数量地图
  plot(obs_map, main=paste(res, "km Observations"), col=terrain.colors(100))
  # 绘制物种丰富度地图
  plot(rich_map, main=paste(res, "km Species Richness"), col=terrain.colors(100))
}
# 重置绘图参数
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)

# 练习2.绘制多样性地图
# 定义计算Shannon多样性指数的函数
compute_shannon_diversity <- function(species_vector) {
  # 删除向量中的缺失值
  species_vector <- na.omit(species_vector)
  # 计算每个物种的出现频次
  species_freq <- table(species_vector)
  # 计算每个物种的比例
  p <- species_freq / sum(species_freq)
  # 计算Shannon多样性指数
  H <- -sum(p * log(p))
  return(H)
}

# 使用定义的函数在200km分辨率下计算Shannon多样性指数
# 根据pts的范围和指定的分辨率创建栅格模板
r <- rast(ext(pts), res=200 * 1000, crs=crs(pts))
# 使用rasterize函数，应用compute_shannon_diversity函数计算每个栅格的Shannon多样性指数
diversity_map <- rasterize(pts, r, field="SPECIES", fun=compute_shannon_diversity)
# 绘制Shannon多样性指数地图
plot(diversity_map, main="Shannon Diversity Index (H) at 200km Resolution", col=terrain.colors(100))




# 练习3：绘制性状地图
# 定义计算平均抗霜性的函数
compute_average_frost <- function(frost_values) {
  # 计算并返回平均抗霜性，自动忽略NA值
  mean_frost <- mean(frost_values, na.rm = TRUE)
  return(mean_frost)
}

# 创建栅格模板，分辨率为100km
r <- rast(ext(pts), res=100 * 1000, crs=crs(pts))
# 使用rasterize函数计算每个栅格内的平均抗霜性
frost_map <- rasterize(pts, r, field="FROST", fun=compute_average_frost)
# 绘制平均抗霜性地图
plot(frost_map, main="Average Frost Tolerance at 100 km Resolution", col=terrain.colors(100))
















