# 实例教程 | 植物性状数据的获取指南（TRY | BIEN | GIFT | sPlotOpen ...）
# author:Biodiversity Monitoring


# 设置工作路径
setwd("C:/Users/Administrator/Desktop")
# 检查工作文件夹
getwd()


################################################################################

##### 1.TRY数据库 #####

# install.packages('rtry')
# 加载rtry包
library(rtry)

# 1.1 使用rtry_import函数进行数据导入
# 这里为了方便演示导入了内置的TRY数据
# 导入我们自己的数据只要更换路径就可以，例如：TRYdata <- rtry_import("C:/Users/Administrator/Desktop/34975.txt")
TRYdata1 <- rtry_import(system.file("testdata", "data_TRY_15160.txt", package = "rtry"))
# 使用View函数查看数据，后面需要查看数据时可以经常使用，不再赘述
View(TRYdata1)
# 同样的方式导入第2个数据集
TRYdata2 <- rtry_import(system.file("testdata", "data_TRY_15161.txt", package = "rtry"))
View(TRYdata2)

# 1.2 使用rtry_explore函数进行数据探索
TRYdata_explore <- rtry_explore(TRYdata1,DataID,DataName,TraitID,TraitName,sortBy=TraitID,showOverview=FALSE)
View(TRYdata_explore)

# 1.3 使用rtry_bind_row函数按行绑定导入的两个数据集
TRYdata <- rtry_bind_row(TRYdata1, TRYdata2)
View(TRYdata)


# 1.4 现在数据集的信息可能有点冗余，要精简信息可以使用下面的方法
# 使用rtry_remove_col删除某一列
workdata <- rtry_remove_col(TRYdata, V28)
# 使用rtry_select_col函数选择需要的列
workdata <- rtry_select_col(workdata, ObsDataID, ObservationID, AccSpeciesID, AccSpeciesName, ValueKindName, TraitID, TraitName, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, UnitName, OrigObsDataID, ErrorRisk, Comment)
# 使用rtry_select_row函数选择所有性状记录和感兴趣的辅助数据
workdata <- rtry_select_row(workdata, TraitID > 0 | DataID %in% c(59, 60, 61, 6601, 327, 413, 1961, 113))


# 1.5 使用rtry_exclude函数排除数据，根据实际研究选择
# 排除对幼年植物或幼树的观测
workdata <- rtry_exclude(workdata, (DataID %in% 413) & (OrigValueStr %in% c("juvenile", "saplings")), baseOn = ObservationID)
# 排除没有地理参考信息和不相关区域的观测值，如排除小于 10 或大于 60 的经度（纬度的DataID是59）
workdata <- rtry_exclude(workdata, (DataID %in% 60) & (StdValue < 10 | StdValue > 60 | is.na(StdValue)), baseOn = ObservationID)
# 排除非代表性子性状（7222和7223包含了性状3115的最小值和最大值， 3117 中的 6598 也是如此，因此排除掉它们）
workdata <- rtry_exclude(workdata, DataID %in% c(7222, 7223, 6598), baseOn = ObsDataID)
# 根据标准值 （StdValue） 排除数据，例如排除6582、6583 和 6584 三个SLA相关性状的数据中标准值小于5的值
workdata <- rtry_exclude(workdata, (DataID %in% c(6582, 6583, 6584)) & (StdValue < 5), baseOn = ObsDataID)
# 根据错误风险排除离群值 （ErrorRisk），这里排除ErrorRisk大于等于3的值
workdata <- rtry_exclude(workdata, ErrorRisk >= 3, baseOn = ObsDataID)


# 1.6 使用rtry_remove_dup函数根据重复标识符 （OrigObsDataID） 删除重复项
workdata <- rtry_remove_dup(workdata)


# 1.7 使用rtry_trans_wider函数转换为宽表（长表格式是出于数据管理目的，宽表格式才是我们平时处理数据比较习惯的格式）
# 选择包含完整TraitID和StdValue的行
num_traits <- rtry_select_row(workdata, complete.cases(TraitID) & complete.cases(StdValue))
# 从筛选后的数据中选择指定的列
num_traits <- rtry_select_col(num_traits, ObservationID, AccSpeciesID, AccSpeciesName, TraitID, TraitName, StdValue, UnitName)
# 提取纬度数据（DataID 59）及对应的ObservationID
workdata_lat <- rtry_select_anc(workdata, 59)
# 提取经度数据（DataID 60）及对应的ObservationID
workdata_lon <- rtry_select_anc(workdata, 60)
# 合并提取的辅助数据与数值性状数据
# 使用rtry_join_left（左连接）基于ObservationID合并相关数据框
# 将纬度数据合并到num_traits中
num_traits_georef <- rtry_join_left(num_traits, workdata_lat, baseOn = ObservationID)
# 将经度数据合并到num_traits_georef中
num_traits_georef <- rtry_join_left(num_traits_georef, workdata_lon, baseOn = ObservationID)
# 对TraitID、TraitName和UnitName进行宽表转换，单元格值为StdValue的平均值
# 将数据转换为宽表格式，每个性状作为一列，值为StdValue的平均值
num_traits_georef_wider <- rtry_trans_wider(num_traits_georef, 
                                            names_from = c(TraitID, TraitName, UnitName), 
                                            values_from = c(StdValue), 
                                            values_fn = list(StdValue = mean))
View(num_traits_georef_wider)

# 1.8 使用rtry_export函数导出预处理后的 TRY 数据
rtry_export(num_traits_georef_wider,"traits.csv")

################################################################################

##### 2.BIEN数据库 #####

#install.packages('BIEN')
library(BIEN)

# 如果我们对某一性状感兴趣，第一步是检查该性状是否存在，并使用函数 BIEN_trait_list 验证拼写
BIEN_trait_list()

# 获取特定科的所有性状数据
family_traits <- BIEN_trait_family(family = "Poaceae")
head(family_traits)
# 获取多个科的所有性状数据，以下类似
family_traits <- BIEN_trait_family(family = c("Poaceae","Orchidaceae"))
View(family_traits)

# 获取特定属的性状数据
genus_traits <- BIEN_trait_genus(genus = "Acer")
head(genus_traits)

# 获取特定物种的性状数据
species_traits <- BIEN_trait_species(species = "Poa annua")
head(species_traits)

# 获取特定性状的所有记录
leaf_area_traits <- BIEN_trait_trait(trait = "leaf area")
head(leaf_area_traits)
# 获取多个性状的所有记录
BIEN_traits <- BIEN_trait_trait(trait = c("whole plant height", "leaf dry mass per leaf fresh mass"))
head(BIEN_traits)

# 估算物种平均性状值
# BIEN_trait_mean函数在没有物种水平数据的情况下，使用属或科水平数据估算给定性状的物种平均值
BIEN_trait_mean(species=c("Poa annua","Juncus trifidus"),trait="leaf dry mass per leaf fresh mass")

# 获取特定科和性状的数据
family_leaf_area <- BIEN_trait_traitbyfamily(trait = "whole plant height", family = "Poaceae")
head(family_leaf_area)

# 获取特定属和性状的数据
genus_wood_density <- BIEN_trait_traitbygenus(trait = "whole plant height", genus = "Carex")
head(genus_wood_density)

# 获取特定物种和性状的数据
species_hl <- BIEN_trait_traitbyspecies(trait = c("whole plant height", "leaf area"), species = c("Carex capitata","Betula nana"))
head(species_hl)

# BIEN_trait_species 提取物种国家的性状数据
BIEN_trait_country("South Africa")
BIEN_trait_country(country="South Africa",trait="whole plant growth form")


################################################################################

##### 3.GIFT数据库 #####

# 安装并加载GIFT包
# install.packages("GIFT")
library("GIFT")
# 设置超时:查询需要很长时间才能完成，增加超时功能可以更容易地完成较大的下载
options(timeout = max(1000, getOption("timeout")))

# 查看性状具体的ID，包括每个性状的类型和内容
trait_meta <- GIFT_traits_meta()
View(trait_meta)

# 检索植物最大植株高度（trait_IDs 1.6.2）的平均值（aggregated trait）
height <- GIFT_traits(trait_IDs = c("1.6.2"), agreement = 0.66, bias_ref = FALSE, bias_deriv = FALSE)
View(height)
# 检索植物最大植株高度（trait_IDs 1.6.2）的原始值（raw traits）
height_raw <- GIFT_traits_raw(trait_IDs = c("1.6.2"))
View(height_raw)

# 使用GIFT_traits_tax函数在更高的分类学水平上检索性状
trait_tax <- GIFT_traits_tax(trait_IDs = c("1.1.1", "1.2.1", "1.4.1"),bias_ref = FALSE, bias_deriv = FALSE)
# 在所要求的三个性状中，growth form（1.2.1）无法在科水平获得。因此，输出表包含了其他两个性状在科水平的性状值。
View(trait_tax)


################################################################################








