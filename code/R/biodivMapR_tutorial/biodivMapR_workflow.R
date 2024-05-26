# 使用biodivMapR进行遥感影像的多样性地图绘制（1）：基本工作流程
# author: Biodiversity Monitoring

# biodivMapR安装
install.packages("remotes")
remotes::install_github('cran/dissUtils')
remotes::install_github('jbferet/biodivMapR')


# 设置路径，更改为自己的本地路径
setwd("C:/Users/...")
getwd()

################################################################################
# 1.数据准备 ###################################################################

##### 下载Sentinel-2图像样本 #####

# 加载所需的库
library(biodivMapR) 
library(utils)  
library(stars)  
# 定义遥感图像数据的URL
url <- 'https://gitlab.com/jbferet/myshareddata/-/raw/master/biodivMapR_S2_Sample/RASTER/S2A_T33NUD_20180104_Subset'
# 创建一个目录来存储下载的数据
tmpdir <- getwd() 
# 为下载的二进制栅格文件命名，使用与在线文件相同的名称
NameRaster <- 'S2A_T33NUD_20180104_Subset'
# 定义二进制栅格文件的完整本地存储路径，使用file.path()函数生成完整路径
destfile <- file.path(tmpdir, NameRaster) 
# 使用download.file()函数下载二进制栅格文件
download.file(url = url, destfile = destfile, method = 'auto', quiet = FALSE, mode = "wb")

# 定义头文件（HDR）的URL，这个文件包含了二进制栅格数据的元数据信息
urlhdr <-  'https://gitlab.com/jbferet/myshareddata/-/raw/master/biodivMapR_S2_Sample/RASTER/S2A_T33NUD_20180104_Subset.hdr'
# 使用自定义函数get_HDR_name()获取与二进制栅格相对应的HDR文件的名称
destfile_HDR <- get_HDR_name(destfile, showWarnings = FALSE)
# 下载HDR文件
download.file(url = urlhdr, destfile = destfile_HDR, method = 'auto', quiet = FALSE, mode = "w")


##### 为Tiff图像添加头文件 #####

# 使用stars包的read_stars函数读取ENVi文件，按波段组织数据，并设置proxy为FALSE，直接读取数据而不是创建代理对象
Stars_S2 <- stars::read_stars(destfile, along = 'band', proxy = FALSE)
# 为转换后的TIFF影像文件创建一个特定的目录结构，即在tmpdir指定的临时目录下创建一个名为'TIFF'的新目录
desttiff <- file.path(tmpdir, 'TIFF', fsep = '\\')
dir.create(desttiff, showWarnings = FALSE) 
# 定义转换后的TIFF文件的存储路径，即'tmpdir/TIFF/S2_Subset.tif'
destfiletiff <- file.path(desttiff, 'S2_Subset.tif', fsep = '\\')  
# 使用write_stars函数将读取的ENVI格式的Stars_S2对象写入到TIFF格式的文件中
# 指定文件路径、驱动类型为'GTiff'表示生成的是GeoTIFF格式的文件，type为'Int16'表示数据类型为16位整型
r <- write_stars(Stars_S2, dsn=destfiletiff, driver = 'GTiff', type='Int16')


# 用stars读取 ENVI 文件
create_hdr(ImPath = destfiletiff, Sensor = 'SENTINEL_2A', 
           SpectralBands = NULL, BandName = NULL, WLunits = NULL)


# 用stars读取 ENVI 文件
# 定义光谱波段名称
BandName <- c('band_02', 'band 03', 'band_04', 'band_05', 'band_06', 
              'band_07', 'band_08', 'band_08A', 'band_11', 'band_12')
# 定义对应的光谱波段波长（单位为纳米）
SpectralBands <- c(496.6, 560.0, 664.5, 703.9, 740.2, 
                   782.5, 835.1, 864.8, 1613.7, 2202.4)
# 定义波长单位
WLunits <- 'Nanometers'
# 次调用create_hdr函数，这次提供完整的参数，包括波段名称、波段值和波长单位
create_hdr(ImPath = destfiletiff, Sensor = 'MyOwnSensor', 
           SpectralBands = SpectralBands, BandName = BandName, WLunits = WLunits)


##### 下载用于“验证”的矢量文件#####

# 加载zip库，这个库提供了压缩文件处理的功能，比如文件的压缩和解压
library(zip)
# 定义ZIP文件的存储路径，这里使用file.path来构建路径，并指定文件夹和ZIP文件的名称
# tmpdir是之前定义的临时目录变量，'S2A_T33NUD_Plots.zip'是ZIP文件名
destzip <- file.path(tmpdir,'S2A_T33NUD_Plots.zip', fsep = '\\')
# 定义ZIP文件的下载URL
url <- 'https://gitlab.com/jbferet/myshareddata/-/raw/master/biodivMapR_S2_Sample/VECTOR/S2A_T33NUD_Plots.zip'
# 使用download.file函数下载ZIP文件，指定下载的URL和目标文件路径
download.file(url = url, destfile = destzip)
# 定义解压后文件的存放路径，也是使用file.path构建，这里创建一个新的文件夹用于存放解压后的内容
destunz <- file.path(tmpdir,'S2A_T33NUD_Plots', fsep = '\\')
# 使用unzip函数解压ZIP文件到指定目录
unzip(zipfile = destzip, exdir = destunz)


################################################################################
# 2.设置biodivMapR的参数 #######################################################

##### 待处理图像 #####

Input_Image_File <- destfile


##### 与图像对应的掩膜 #####

# 如果没有掩膜，则设为 FALSE
Input_Mask_File <- FALSE


##### 定义输出目录 #####

Output_Dir <- file.path(tmpdir, "RESULTS")


##### 辐射过滤 #####

NDVI_Thresh <- 0.8
Blue_Thresh <- 500
NIR_Thresh <- 1500


##### 反射率数据的归一化 #####

Continuum_Removal <- TRUE


##### 反射率转换和降维 #####

TypePCA <- 'SPCA'


##### 过滤PCA中的异常值 #####

# PCA 过滤：如果要根据 PCA 异常值进行二次过滤，则设置为 "TRUE"。
# 处理速度较慢
# 如果 TypePCA = 'MNF' 则自动设为 FALSE
FilterPCA <- FALSE


##### 定义计算光谱多样性的窗口大小 #####

window_size <- 10


##### 优化计算资源 #####

nbCPU <- 4
MaxRAM <- 0.5


##### 光谱物种数量 #####

nbclusters <- 50


################################################################################
# 3.执行空间和光谱过滤 #########################################################

##### 生成植被/云层/阴影遮罩 ##### 

print("PERFORM RADIOMETRIC FILTERING")
Input_Mask_File <- perform_radiometric_filtering(Image_Path = Input_Image_File, Mask_Path = Input_Mask_File,
                                                 Output_Dir = Output_Dir, TypePCA = TypePCA,
                                                 NDVI_Thresh = NDVI_Thresh, Blue_Thresh = Blue_Thresh,
                                                 NIR_Thresh = NIR_Thresh)

##### 生成光谱滤波器 #####

Excluded_WL <- c(0, 400)
Excluded_WL <- rbind(Excluded_WL, c(895, 1005))
Excluded_WL <- rbind(Excluded_WL, c(1180, 1480))
Excluded_WL <- rbind(Excluded_WL, c(1780, 2040))


################################################################################
# 4.PCA和降维 ##################################################################

##### 数据转换 #####

print("PERFORM DIMENSIONALITY REDUCTION")
PCA_Output <- perform_PCA(Input_Image_File = Input_Image_File, 
                          Input_Mask_File = Input_Mask_File,
                          Output_Dir = Output_Dir, 
                          TypePCA = TypePCA, 
                          FilterPCA = FilterPCA,
                          nbCPU = nbCPU, 
                          MaxRAM = MaxRAM, 
                          Continuum_Removal = Continuum_Removal)
# 降维后的栅格路径
PCA_Files <- PCA_Output$PCA_Files
# 更新掩膜的路径
Input_Mask_File <- PCA_Output$MaskPath


##### 降维的成分选择 #####

# 从 PCA/SPCA/MNF 栅格中选择成分
# Sel_PC = 存储所选成分的文件路径
Sel_PC <- select_PCA_components(Input_Image_File = Input_Image_File,
                                Output_Dir = Output_Dir, 
                                PCA_Files = PCA_Output$PCA_Files,
                                TypePCA = PCA_Output$TypePCA, 
                                File_Open = TRUE)

# 1
# 2
# 5
# 6
# 8


################################################################################
# 5.光谱物种映射 ###############################################################

print("MAP SPECTRAL SPECIES")
Kmeans_info <- map_spectral_species(Input_Image_File = Input_Image_File, 
                                    Input_Mask_File = PCA_Output$MaskPath,
                                    Output_Dir = Output_Dir,
                                    SpectralSpace_Output = PCA_Output, 
                                    nbclusters = nbclusters, 
                                    nbCPU = nbCPU, MaxRAM = MaxRAM)


Kmeans_info$SpectralSpecies
'RESULTS/S2A_T33NUD_20180104_Subset/SPCA/SpectralSpecies'

################################################################################
# 6.α和β多样性地图 #############################################################

print("MAP ALPHA DIVERSITY")
# Index.Alpha   = c('Shannon','Simpson')
Index_Alpha <- c('Shannon')
map_alpha_div(Input_Image_File = Input_Image_File, 
              Output_Dir = Output_Dir, 
              TypePCA = TypePCA,
              window_size = window_size, 
              nbCPU = nbCPU, 
              MaxRAM = MaxRAM,
              Index_Alpha = Index_Alpha, 
              nbclusters = nbclusters)

print("MAP BETA DIVERSITY")
map_beta_div(Input_Image_File = Input_Image_File, 
             Output_Dir = Output_Dir, 
             TypePCA = TypePCA,
             window_size = window_size, 
             nbCPU = nbCPU, 
             MaxRAM = MaxRAM,
             nbclusters = nbclusters)


################################################################################
# 7.功能多样性 #################################################################

################################################################################
##              MAP FUNCTIONAL DIVERSITY METRICS FRic, FEve, FDiv             ##
##          (Villeger et al, 2008 https://doi.org/10.1890/07-1206.1)          ##
################################################################################
## 读取从降维过程中选取的特征
Selected_Features <- read.table(Sel_PC)[[1]]
## 所选成分的路径
map_functional_div(Original_Image_File = Input_Image_File, 
                   Functional_File = PCA_Output$PCA_Files,
                   Selected_Features = Selected_Features, 
                   Output_Dir = Output_Dir,
                   window_size = window_size, 
                   nbCPU = nbCPU, 
                   MaxRAM = MaxRAM,
                   TypePCA = TypePCA)


################################################################################
# 8.如何进行验证？ #################################################################


# 定义存放样地形状文件的目录位置
VectorDir <- destunz
# 使用list_shp()函数列出指定目录下的所有形状文件
Path_Vector <- list_shp(VectorDir)
# 获取形状文件的名称（去除文件扩展名）
Name_Vector <- tools::file_path_sans_ext(basename(Path_Vector))
# 定义用于验证的光谱物种栅格数据的位置
Path_SpectralSpecies <- Kmeans_info$SpectralSpecies
# 根据样地形状文件计算多样性指标，这一步没有基于样地进行光谱多样性的分割
Biodiv_Indicators <- diversity_from_plots(Raster_SpectralSpecies = Path_SpectralSpecies, 
                                          Plots = Path_Vector,
                                          nbclusters = nbclusters, 
                                          Raster_Functional = PCA_Output$PCA_Files, 
                                          Selected_Features = Selected_Features)
# 提取香农多样性指标
Shannon_RS <- c(Biodiv_Indicators$Shannon)[[1]]
# 提取功能丰富度指标（FRic）
FRic <- c(Biodiv_Indicators$FunctionalDiversity$FRic)
# 提取功能均匀度指标（FEve）
FEve <- c(Biodiv_Indicators$FunctionalDiversity$FEve)
# 提取功能分化度指标（FDiv）
FDiv <- c(Biodiv_Indicators$FunctionalDiversity$FDiv)
# 如果样地没有命名，给样地赋予序列名
Biodiv_Indicators$Name_Plot = seq(1,length(Biodiv_Indicators$Shannon[[1]]),by = 1)




# 设置结果文件的存储目录
Path_Results <- file.path(Output_Dir, NameRaster, TypePCA, 'VALIDATION')
# 创建该目录，如果目录层次不存在则递归创建，且不显示警告信息
dir.create(Path_Results, showWarnings = FALSE,recursive = TRUE)
# 将香农指数数据写入csv文件
write.table(Shannon_RS, file = file.path(Path_Results, "ShannonIndex.csv"),
            sep="\t", dec=".", na=" ", row.names = Biodiv_Indicators$Name_Plot, col.names= FALSE, quote=FALSE)

# 构建包含所有α多样性指标的数据框
Results <- data.frame(Name_Vector, Biodiv_Indicators$Richness, Biodiv_Indicators$Fisher,
                      Biodiv_Indicators$Shannon, Biodiv_Indicators$Simpson,
                      Biodiv_Indicators$FunctionalDiversity$FRic,
                      Biodiv_Indicators$FunctionalDiversity$FEve,
                      Biodiv_Indicators$FunctionalDiversity$FDiv)
# 设置数据框的列名
names(Results) = c("ID_Plot", "Species_Richness", "Fisher", "Shannon", "Simpson", "FRic", "FEve", "FDiv")
# 将α多样性指标数据写入csv文件
write.table(Results, file = file.path(Path_Results, "AlphaDiversity.csv"),
            sep="\t", dec=".", na=" ", row.names = FALSE, col.names= TRUE, quote=FALSE)

# 获取Bray-Curtis不相似度的平均值
BC_mean <- Biodiv_Indicators$BCdiss
# 设置行名和列名
colnames(BC_mean) <- rownames(BC_mean) <- Biodiv_Indicators$Name_Plot
# 将Bray-Curtis不相似度数据写入csv文件
write.table(BC_mean, file = file.path(Path_Results, "BrayCurtis.csv"),
            sep="\t", dec=".", na=" ", row.names = FALSE, col.names= TRUE, quote=FALSE)




# 使用 PCoA 进行排序（与 map_beta_div 相同）
library(labdsv)
MatBCdist <- as.dist(BC_mean, diag = FALSE, upper = FALSE)
BetaPCO <- pco(MatBCdist, k = 3)




library(terra)
library(tools)
library(ggplot2)
library(gridExtra)
# 初始化变量
nbSamples <- shpName <- c()
# 遍历形状文件，统计每个文件中的样地数，并记录形状文件的名称
for (i in 1:length(Path_Vector)){
  shp <- Path_Vector[i]
  nbSamples[i] <- nrow(vect(shp))
  shpName[i] <- file_path_sans_ext(basename(shp))
}

# 为每个样地分配植被类型
Type_Vegetation = c()
for (i in 1: length(nbSamples)){
  for (j in 1:nbSamples[i]){
    Type_Vegetation = c(Type_Vegetation,shpName[i])
  }
}

# 创建数据框，其中包括以PCoA空间坐标表示的α多样性指标和β多样性指标选项
Results <- data.frame('vgtype'=Type_Vegetation,
                      'pco1'= BetaPCO$points[,1],
                      'pco2'= BetaPCO$points[,2],
                      'pco3' = BetaPCO$points[,3],
                      'shannon' = Shannon_RS,
                      'FRic' = FRic, 
                      'FEve' = FEve, 
                      'FDiv' = FDiv)

# 在 PCoA 空间中绘制野外数据，大小与香农指数相对应
g1 <-ggplot (Results, aes (x=pco1, y=pco2, color=vgtype,size=shannon)) + 
  geom_point(alpha=0.6) +
  scale_color_manual(values=c("#e6140a", "#e6d214", "#e68214", "#145ae6"))

g2 <-ggplot (Results, aes (x=pco1, y=pco3, color=vgtype,size=shannon)) + 
  geom_point(alpha=0.6) +
  scale_color_manual(values=c("#e6140a", "#e6d214", "#e68214", "#145ae6"))

g3 <-ggplot (Results, aes (x=pco2, y=pco3, color=vgtype,size=shannon)) + 
  geom_point(alpha=0.6) +
  scale_color_manual(values=c("#e6140a", "#e6d214", "#e68214", "#145ae6"))

# 提取图例
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
get_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(g3)
# 将三个散点图和共享图例组合起来
gAll <- grid.arrange(arrangeGrob(g1 + theme(legend.position="none"),
                                 g2 + theme(legend.position="none"),
                                 g3 + theme(legend.position="none"),
                                 nrow=1),legend,nrow=2,heights=c(5, 4))
# 保存图像文件
filename <- file.path(Path_Results,'BetaDiversity_PcoA1_vs_PcoA2_vs_PcoA3.png')
ggsave(filename, plot = gAll, device = 'png', path = NULL,
       scale = 1, width = 12, height = 7, units = "in",
       dpi = 600, limitsize = TRUE)



