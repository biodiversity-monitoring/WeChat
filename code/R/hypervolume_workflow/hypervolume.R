# 实例教程 | 使用hypervolume包计算n-维生态位超体积大小与重叠
# author:Biodiversity Monitoring


################################################################################
################################################################################
##### 1. 加载相关R包及导入数据 #####

# 设置路径
setwd("...")
getwd()

# 加载必要的R包
# install.packages("alphahull")  # 安装alphahull包(如果尚未安装)
library(alphahull)              # 用于计算几何体的包
# install.packages("hypervolume")# 安装hypervolume包(如果尚未安装)
library(hypervolume)            # 用于计算生态位超体积的核心包
library(tidyverse)              # 数据处理和可视化工具集
library(magrittr)               # 提供管道操作符
library(ggplot2)                # 绘图包

# 导入数据
panama <- read.csv("panama.csv")  # 读取CSV格式的数据文件
head(panama)                      # 查看数据前六行

################################################################################
################################################################################

##### 2. 数据转换和标准化 #####
# 对生长率(rate1)进行对数转换
growth.adder = 1                  # 设置加数为1，避免取对数时出现负值
growth.min = min(panama$rate1)    # 获取最小生长率
panama$log_rate1 = log(panama$rate1 + abs(growth.min) + growth.adder)  # 对生长率进行对数转换

# 对死亡率(rate2)进行数据处理
panama = panama %>%
  filter(!rate2 %in% c(0,1)) %>%  # 过滤掉死亡率为0和1的数据
  mutate(log_rate2 = log(1-rate2))# 对死亡率进行对数转换

# 计算并保存标准化参数
rate1.mean <- mean(panama$log_rate1, na.rm = T)  # 计算log_rate1的均值
rate1.sd <- sd(panama$log_rate1, na.rm = T)      # 计算log_rate1的标准差
rate2.mean <- mean(panama$log_rate2, na.rm = T)  # 计算log_rate2的均值
rate2.sd <- sd(panama$log_rate2, na.rm = T)      # 计算log_rate2的标准差

# 对转换后的数据进行标准化处理
panama = panama %>%
  mutate(
    # 对log_rate1进行中心化和标准化
    log_rate1_scaled = (log_rate1 - mean(log_rate1, na.rm=T)) / sd(log_rate1, na.rm=T),
    # 对log_rate2进行中心化和标准化
    log_rate2_scaled = (log_rate2 - mean(log_rate2, na.rm=T)) / sd(log_rate2, na.rm=T)
  )
head(panama)  # 查看处理后的数据前六行

# 将数据框转换为tibble格式
rates.tmp <- as_tibble(panama)

################################################################################
################################################################################

##### 3. 计算带宽并创建超体积 #####

# 获取所有演替阶段
timeslices <- unique(panama$time_slice)

# 创建临时数据框来存储带宽估计结果
kde.tmp = tibble(bandwidth1 = numeric(), bandwidth2 = numeric())

# 对每个演替阶段进行带宽估计
for(j in 1:length(timeslices)) {
  # 筛选当前演替阶段的数据
  dat.tmp = rates.tmp %>%
    filter(time_slice == timeslices[j])
  
  # 使用交叉验证方法估计带宽
  kde.tmp %<>% rbind(estimate_bandwidth(dat.tmp %>% select(log_rate1_scaled, log_rate2_scaled), 
                                        method="cross-validation"))
  names(kde.tmp) = c("bandwidth_1", "bandwidth_2")
}
kde.tmp
# 计算平均带宽
kde.bandwidth = colMeans(kde.tmp)
kde.bandwidth

# 定义点排序函数
# 该函数用于对ashape函数的输出进行排序，以便后续使用geom_polygon
sort_points <- function(df, y = "latitude", x = "longitude", clockwise = TRUE) {
  # 检查并处理NA值
  if (any(is.na(c(df[, y], df[, x])))) {
    df <- df[!(is.na(df[, y]) & is.na(df[, x])), ]
    warning("Missing coordinates were detected and have been removed.", call. = FALSE)
    if (nrow(df) == 0) stop("There are no valid coordinates.", call. = FALSE)
  }
  
  # 计算中心点
  x_centre <- mean(df[, x])
  y_centre <- mean(df[, y])
  
  # 计算相对于中心点的偏移量
  df$x_delta <- df[, x] - x_centre
  df$y_delta <- df[, y] - y_centre
  
  # 计算角度（弧度）
  df$angle <- atan2(df$y_delta, df$x_delta)
  
  # 按角度排序
  if (clockwise) {
    df <- df[order(df$angle, decreasing = TRUE), ]
  } else {
    df <- df[order(df$angle, decreasing = FALSE), ]
  }
  
  # 删除中间变量
  df[, c("x_delta", "y_delta", "angle")] <- NULL
  
  return(df)
}

# 创建列表存储计算结果
all.hvols.tmp = list()        # 存储超体积对象
random.points = list()        # 存储随机点
a.shapes.edges = list()       # 存储alpha shape边界
timeslices = unique(rates.tmp$time_slice)

# 对每个演替阶段进行超体积计算
for(j in 1:length(timeslices)) {
  timeslice.tmp = as.character(timeslices[j])
  
  # 筛选当前演替阶段数据
  rates.tmp.ts = rates.tmp %>% 
    filter(time_slice == timeslice.tmp)
  
  # 计算高斯核超体积
  hvol.tmp = hypervolume_gaussian(
    rates.tmp.ts %>% select(log_rate1_scaled, log_rate2_scaled), 
    kde.bandwidth = estimate_bandwidth(
      data = rates.tmp.ts %>% select(log_rate1_scaled, log_rate2_scaled), 
      method = "fixed", 
      value = kde.bandwidth
    ),
    quantile.requested = 0.8,           # 设置概率阈值
    quantile.requested.type = "probability", 
    verbose=FALSE, 
    name = paste("Panama", timeslice.tmp, sep = ".")
  )
  all.hvols.tmp[[timeslice.tmp]] = hvol.tmp
  
  # 将随机点反转换回原始尺度
  random.points.tmp = data.frame(hvol.tmp@RandomPoints)
  random.points.tmp %<>% 
    mutate(
      log_rate1 = log_rate1_scaled * rate1.sd + rate1.mean, 
      log_rate2 = log_rate2_scaled * rate2.sd + rate2.mean
    )
  random.points[[timeslice.tmp]] = random.points.tmp
  
  # 创建alpha shape边界
  a.shape.tmp = ashape(
    x = random.points.tmp$log_rate1, 
    y = random.points.tmp$log_rate2,
    alpha = 1
  )
  a.shape.tmp = data.frame(a.shape.tmp$edges)
  a.shape.tmp = data.frame(
    x = c(rbind(a.shape.tmp$x1, a.shape.tmp$x2)), 
    y = c(rbind(a.shape.tmp$y1, a.shape.tmp$y2))
  )
  a.shape.tmp %<>% sort_points(x="x", y="y")
  a.shapes.edges[[timeslice.tmp]] = a.shape.tmp
}

# 合并所有结果
all.hvols = all.hvols.tmp
all.random.points = random.points %>% bind_rows(.id = "time_slice")
all.edges = a.shapes.edges %>% bind_rows(.id = "time_slice")

################################################################################
################################################################################

##### 4.超体积形状可视化 #####

# 定义颜色方案
col.palette = c("#fcd14d", "#88d01e","#008844")  # 黄色、绿色、深绿色

# 创建基础图形
ggplot() + 
  # 添加散点图层
  geom_point(data = panama,  # 原始数据点
             aes(x = log_rate1, y = log_rate2, 
                 fill = time_slice, size = rel_abundance), 
             col = "black", shape = 21, alpha = 0.5) + 
  
  # 添加多边形图层（超体积边界）
  geom_polygon(data = all.edges,  # 超体积边缘数据
               aes(x = x, y = y, col = time_slice, fill = time_slice), 
               linewidth = 2, alpha = 0.3) + 
  
  # 设置主题和图形属性
  theme_bw() +
  theme(panel.grid = element_blank(),  # 移除网格线
        aspect.ratio = 1,  # 设置纵横比
        text = element_text(size = 14)) +  # 设置文字大小
  
  # 设置颜色和图例
  scale_color_manual(name = "Successional stage", 
                     values = col.palette, 
                     labels = c("ESF", "LSF", "OGF")) +  # 颜色映射
  scale_fill_manual(name = "Successional stage", 
                    values = col.palette, 
                    labels = c("ESF", "LSF", "OGF")) +  # 填充色映射
  
  # 设置点大小映射
  scale_size_continuous(name = "Relative abundance", 
                        range = c(1,8), 
                        breaks = c(3,6,9), 
                        labels = c("3%", "6%", "9%")) +
  
  # 设置图例位置和样式
  guides(color = guide_legend(title.position = "top"), 
         fill = guide_legend(title.position = "top", 
                             override.aes = list(shape = NA)), 
         size = guide_legend(title.position = "top")) +
  
  # 设置x轴和y轴刻度
  scale_x_continuous(breaks = log(c(0,1,10,40) + abs(growth.min) + growth.adder),
                     labels = c(0,1,10,40), 
                     limits = c(-0.5, 3.8)) +
  scale_y_continuous(breaks = log(c(0,0.01,0.1,1)+0.001),
                     labels = c(0,0.01,0.1,1),
                     limits = log(c(0, 1)+0.001)) +
  
  # 添加轴标签
  xlab("Growth (mm dbh/year)") +
  ylab("Mortality (1/year)") +
  
  # 添加水平参考线
  geom_hline(yintercept = log(0.1+0.001), linetype = 3, linewidth = 0.5)

################################################################################
################################################################################

##### 5. 计算超体积重叠度 #####

# 计算两个演替阶段超体积的集合
hv_set <- hypervolume_set(all.hvols$`0-30`, all.hvols$`30-120`, check.memory=FALSE)

# 计算重叠统计量
hypervolume_overlap_statistics(hv_set)

# 对0-30演替阶段超体积进行自助抽样
path1 = hypervolume_resample("0-30", 
                             all.hvols$`0-30`, 
                             method = "bootstrap", 
                             n = 100, 
                             cores = 12)

# 对30-120演替阶段超体积进行自助抽样
path2 = hypervolume_resample("30-120", 
                             all.hvols$`30-120`, 
                             method = "bootstrap", 
                             n = 100, 
                             cores = 12)

# 进行重叠显著性检验
result <- hypervolume_overlap_test(all.hvols$`0-30`, all.hvols$`30-120`, c(path1,path2), cores = 12)
result$p_values

# 绘制Sorensen距离分布图
result$plots$sorensen + 
  xlab("Sorensen distance") +
  ylab("Density") +
  theme_bw()











