
# 安装
# install.packages("funspace")

# 加载软件包
library(funspace)

# 加载示例数据（性状已进行对数 10 转换和缩放）
data.aux <- funspace::GSPFF_missing



## 1.构建和探索功能性状空间 #####################################################

# 加载示例树（必须是 phylo 类对象）
phylo.tree <- funspace::phylo

# 插补缺失数据（因为有 >10k 个物种，所以运行时间很长！）
GSPFF_imputed <- impute(traits=data.aux, phylo=phylo.tree, addingSpecies=TRUE) # 输出是一个列表

# 将估算的性状数据保存在一个单独的对象中：
imputed.traits <- GSPFF_imputed$imputed

# 测试定义功能性状空间的维度
funspaceDim(imputed.traits) 

# 运行 PCA
pca.trait <- princomp(imputed.traits, cor=TRUE)

# 构建功能性状空间（使用前两个 PC）
trait_space_global <- funspace(x=pca.trait, PCs=c(1,2), n_divisions=300)



# 加载分类信息
tax_inf <- funspace::GSPFF_missing_tax

# 将一个组定义为包括四个科
selFam <- c("Pinaceae", "Poaceae", "Fabaceae", "Lauraceae")

selRows <- which(tax_inf$family %in% selFam)

tax_subset <- droplevels(tax_inf[selRows,])

# 创建 GSPFF 的子集以保留目标群体
GSPFF_subset <- imputed.traits[selRows, ]

# 在子集上运行 PCA
PCA_subset <- princomp(GSPFF_subset)



# 建立功能性状空间（使用前两个 PC），包括组别

trait_space_families <- funspace(x=PCA_subset, PCs=c(1,2), group.vec=tax_subset$family, n_divisions=300)

# 我们可以打印两个空间的输出结果

summary(trait_space_global)

summary(trait_space_families)



## 2.映射功能性状空间 ##########################################################

# 创建 GSPFF 功能空间：

pca.gspff <- princomp(GSPFF, cor=TRUE)

trait_space_gspff <- funspace(x=pca.gspff, PCs=c(1,2), n_divisions=300)

# 我们要映射的响应变量
y <- abs(pca.gspff$scores[, 1]*pca.gspff$scores[, 2])+rnorm(nrow(GSPFF), 0, 1)

# 对汇集的所有数据进行 GAM 拟合（运行只需几秒钟！）

fit.gam <- funspaceGAM(y=y, funspace=trait_space_gspff)

# 要拟合每一级分组变量的 GAM（未显示）
# 我们只需运行 funspaceGAM()，通过指定 group.vec 参数来导入一个 funspace()对象




## 3.绘制功能性状空间图 ########################################################

# 绘制包括全局（所有数据汇集）性状概率分布的功能空间图
# 在这种情况下，我们将绘制 trait_space_families 对象

plot(x=trait_space_families, # funspace对象
     type="global", # 绘制全局TPD
     quant.plot=TRUE, # 添加分位数线
     arrows=TRUE, # 为 PCA 载荷添加箭头
     arrows.length=0.9) # 使箭头比默认值短一些




# 绘制使用 funspaceGAM() 函数创建的对象：我们将使用步骤 2 中创建的 fit.gam 对象
plot(x=fit.gam,
     type="global",
     quant.plot=TRUE,
     quant.col="grey80") # a lighter tone for the quantiles





## 为了展示‘type’参数的使用，我们还会绘制功能性状空间，
## 并针对分组变量的每个水平（Fabaceae、Lauraceae、Pinceae、Poaceae）绘制一个单独的性状概率密度函数
## 每个群组的GAM输出未显示，但可以通过在下面的代码中将trait_space_families对象替换为fit.gam对象来获取

par(mfrow=c(2, 2), mar=c(2,2,1,1), mgp=c(1, 0.1, 0))

plot(x=trait_space_families,
     type="groups", # 指定绘制每个组(科)的TPD图
     quant.plot=TRUE,
     globalContour=T, # 添加全局TPD的等高线
     pnt=T, # 在每个组的TPD图上添加物种点
     pnt.cex=0.1, # 设置点的大小为0.1，使点看起来更小
     pnt.col=rgb(0.2, 0.8, 0.1, alpha=0.2), # 设置点的颜色
     axis.title.line=1)




