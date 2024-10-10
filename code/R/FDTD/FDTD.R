# 实例教程 | 功能多样性的三元图
# author:Biodiversity Monitoring


setwd("...")
getwd()

library(adiv)
library(ggtern)


# 导入数据
grazed_data <- read.csv("Species_abundances_and_functional_traits_in_grazed_plots.csv")
ungrazed_data <- read.csv("Species_abundances_and_functional_traits_in_ungrazed_plots.csv")

# 查看数据结构
grazed_data
ungrazed_data

# 去掉NA值
grazed_data <- na.omit(grazed_data)
ungrazed_data <- na.omit(ungrazed_data)

# 提取群落数据
com1 <- grazed_data[, c("G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8")]
rownames(com1) <- grazed_data$Species
com1 <- t(com1)
com1
com2 <- ungrazed_data[, c("N1", "N2", "N3", "N4", "N5", "N6", "N7")]
rownames(com2) <- ungrazed_data$Species
com2 <- t(com2)
com2

# 提取性状数据
traits1 <- grazed_data[, c("SLA", "LDMC", "N", "C")]
rownames(traits1) <- grazed_data$Species
traits1
traits2 <- ungrazed_data[, c("SLA", "LDMC", "N", "C")]
rownames(traits2) <- ungrazed_data$Species
traits2

################################################################################

FD_DRQ <- function(traits, com) {
  # 线性缩放性状数据到[0,1]范围
  traits_scaled <- apply(traits, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  # 计算物种间的功能欧几里得距离
  dist_func <- dist(traits_scaled, method = "euclidean")
  # 将距离除以最大值进行缩放
  dist_func_scaled <- dist_func / max(dist_func)
  # 使用adiv包计算Simpson优势度(D)、功能冗余(R)和Rao's Q
  S <- speciesdiv(com, "GiniSimpson")
  colnames(S) <- "S"
  Q <- QE(com, dist_func_scaled, "QE")
  colnames(Q) <- "Q"
  R <- S - Q 
  colnames(R) <- "R"
  D <- 1 - S
  colnames(D) <- "D"
  # 创建DRQ数据框
  DRQ <- data.frame(D = D, R = R, Q = Q)
  return(DRQ)
}


FD_DRQ(traits1, com1)
FD_DRQ(traits2, com2)

DRQ <- FD_DRQ(traits1, com1)
ggtern(data = DRQ, aes(x = D, y = R, z = Q)) +
  geom_point(shape=21,size=2,fill="red") +
  theme_custom() +
  labs(x = "D", y = "R", z = "Q")




# 合并两个数据集
data1 <- FD_DRQ(traits1, com1)
data1$group <- "Grazed"
data2 <- FD_DRQ(traits2, com2)
data2$group <- "Ungrazed"
data <- rbind(data1, data2)
data

# 创建三元图
ggtern(data = data, aes(x = D, y = R, z = Q, fill = group, shape = group)) +
  geom_point(size = 2) +
  theme_custom() +
  labs(x = "D", y = "R", z = "Q") +
  scale_fill_manual(values = c("Grazed" = "red", "Ungrazed" = "blue")) +
  scale_shape_manual(values = c("Grazed" = 21, "Ungrazed" = 22)) +
  theme(legend.background = element_blank(),
        legend.key = element_blank())





