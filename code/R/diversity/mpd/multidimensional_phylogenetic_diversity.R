# 实例教程 | 系统发育多样性指数的统一框架：丰富度，离散性，规律性
# author:Biodiversity Monitoring

# 设置路径
setwd("...")
getwd()

# 加载r包
library(PhyloMeasures) # https://cran.r-project.org/src/contrib/Archive/PhyloMeasures/；PhyloMeasures包需要从本地安装
library(picante)
library(vegan)
library(snowfall)

# 读取群落数据
plant <- read.csv("com.csv",row.names = 1,stringsAsFactors = FALSE, check.names = FALSE)
# 读取系统发育树
planttree <- read.tree("phy.tre")

# 处理物种名称,确保群落数据和系统发育树的物种名称匹配
a <- gsub(" ","_",colnames(plant))
colnames(plant) <- a
b <- match.phylo.comm(planttree,plant)
com <- b$comm
phy <- b$phy

##### alpha多样性计算 #####

# 物种丰富度维度 - 计算系统发育多样性(PD)
com_pd <- pd(com, phy, include.root=F)

# 物种离散度维度 - 计算平均谱系距离(MPD) 
com_mpd <- as.data.frame(mpd(com, cophenetic(phy)))
row.names(com_mpd) <- row.names(com)

# 物种规律性维度
# 计算每个物种的进化独特性
ed <- as.data.frame(evol.distinct(phy, "fair.proportion"))
ed2 <- as.data.frame(ed[,-1])
row.names(ed2) <- ed$Species
colnames(ed2) <- "ed"
pd <- sum(ed2)

fun_eedmarc <- function(i) {
  # 提取单个样方数据
  com.i <- plant[i,]
  total_names <- colnames(com.i)
  sp1 <- total_names[com.i > 0]
  # 计算进化熵
  ed.i <- subset(ed2, rownames(ed2) %in% sp1)
  hed <- ed.i/pd
  hed2 <- hed * log(hed)
  hed3 <- sum(hed2[,1])
  hed4 <- (-hed3)
  # 标准化
  ric.i <- sum(com.i != 0)
  eed.i <- hed4/log(ric.i)
  return(eed.i)
}

# 初始化并行计算
sfInit(parallel = TRUE, cpus = 40)
sfLibrary(tidyverse)

# 导出需要的对象到并行环境
sfExport("plant","ed2", "pd")
sfExport("fun_eedmarc")

# 并行计算结果
resultss <- sfLapply(1:nrow(plant), fun_eedmarc)
com_Eed = do.call(rbind,resultss)
row.names(com_Eed) <- row.names(plant)
sfStop()

com_alpha <- data.frame(PD = com_pd[,1], MPD = com_mpd[,1], Eed = com_Eed)
com_alpha 

##### beta多样性计算 #####

# 物种丰富度维度 
comm2 <- vegan::decostand(com, method="pa", MARGIN=1)
com_unifrac <- PhyloMeasures::unifrac.query(phy, comm2)
row.names(com_unifrac) <- row.names(comm2)
colnames(com_unifrac) <- row.names(comm2)
com_unifrac

# 物种离散度维度
com_betampd <- PhyloMeasures::cd.query(phy, comm2)
row.names(com_betampd) <- row.names(comm2)
colnames(com_betampd) <- row.names(comm2)
com_betampd

# 物种规律性维度
# 计算总系统发育多样性
plant_pd <- sum(ed[2])
# 计算每个物种的相对进化独特性
plant_proed <- ed[2]/plant_pd
row.names(plant_proed) <- ed[,1]
# 计算进化熵
plant_li <- as.data.frame(plant_proed * log(plant_proed))

# 初始化结果矩阵
z <- dim(comm2)[1]
betaeed_result <- array(0, dim=c(z,z))
row.names(betaeed_result) <- row.names(comm2)
colnames(betaeed_result) <- row.names(comm2)

# 计算每对样方之间的Beta进化熵
for(i in 1:z){
  for(j in 1:z){
    # 提取两个样方的数据
    sample.i <- comm2[i,,drop=F]
    sample.j <- comm2[j,,drop=F]
    # 合并两个样方数据
    sampleij <- rbind(sample.i,sample.j)
    sampleij2 <- colSums(sampleij[1:2,])
    sampleij3 <- rbind(sampleij,sampleij2)
    sampleij3 <- as.matrix(sampleij3)
    # 转换为出现-不出现数据
    sampleij3 <- vegan::decostand(sampleij3, method="pa", MARGIN=1)
    # 计算物种丰富度
    richness <- as.data.frame(rowSums(sampleij3 > 0))
    # 计算进化熵
    sampleij4 <- t(sampleij3)
    res <- sampleij4*plant_li[,1]
    dp <- exp(-colSums(res))
    ep <- as.data.frame(dp/richness)
    # 计算Beta进化熵
    betaeed <- ep[3,1]/mean(ep[1,1],ep[2,1])
    betaeed_result[i,j] <- betaeed
  }
}

com_betaeed <- betaeed_result
com_betaeed




