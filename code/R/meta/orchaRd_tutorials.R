
# title: 使用orchaRd包从meta分析和meta回归中绘制果园图和毛毛虫图
# original author: S. Nakagawa, M. Lagisz, R. E. O’Dea, P. Pottier, J. Rutkowska, Y. Yang, A. M. Senior & D.W.A. Noble


################################################################################
##### 安装和加载 ###############################################################

# 使用install.packages函数安装pacman包。pacman是一个R包管理器，用于简化包的安装和加载过程。
# install.packages('pacman')
# 清除当前R会话中的所有对象，确保环境干净，不受之前执行的代码的影响。
rm(list = ls())
# 使用devtools包的install_github函数从GitHub安装orchaRd包。
devtools::install_github("daniel1noble/orchaRd", ref = "main", force = TRUE)
# 使用pacman包的p_load函数一次性加载多个包。
# 如果指定的包尚未安装，p_load会自动尝试安装这些包，这使得包管理变得更加方便。
pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp, orchaRd, emmeans,
               ape, phytools, flextable)

################################################################################
##### 果园图和毛毛虫图实例 #####################################################

##### Example 1：饮食限制与寿命#####

# 加载内置的'english'数据集
data(english)
# 计算效应量，这里计算的是标准化平均差(SMD)
# - measure = "SMD"指定计算的效应量为标准化平均差
# - n1i, n2i分别代表对照组和实验组的样本大小
# - sd1i, sd2i分别代表对照组和实验组的标准差
# - m1i, m2i分别代表对照组和实验组的平均值
# - var.names定义了在结果数据框中生成的效应量和方差变量的名称
# - data指定了数据来源于'english'数据集
english <- escalc(measure = "SMD", n1i = NStartControl, sd1i = SD_C, m1i = MeanC,
                  n2i = NStartExpt, sd2i = SD_E, m2i = MeanE, var.names = c("SMD", "vSMD"), data = english)
# 使用rma.mv函数进行多层次meta分析（mixed-effects model meta-analysis）。
# - yi = SMD 指定了效应量
# - V = vSMD 指定了效应量标准误的方差
# - random = list(~1 | StudyNo, ~1 | EffectID) 定义了随机效应的结构，这里涉及两个层次：研究编号和效应ID
# - data参数指定数据来自'english'数据集
english_MA <- rma.mv(yi = SMD, V = vSMD, random = list(~1 | StudyNo, ~1 | EffectID),
                     data = english)
# 输出meta分析模型的概要统计信息
summary(english_MA)


# 使用orchaRd包的mod_results函数提取并处理meta分析模型结果
model_results <- orchaRd::mod_results(english_MA, mod = "1", at = NULL, group = "StudyNo")
model_results


# 使用orchaRd包的orchard_plot函数绘制元分析结果的图形表示
orchaRd::orchard_plot(english_MA, mod = "1", group = "StudyNo", xlab = "Standardised mean difference",
                      transfm = "none", twig.size = 0.5, trunk.size = 1)


# 使用i2_ml函数计算meta分析模型english_MA的I²值
I2 <- orchaRd::i2_ml(english_MA)
# 使用orchard_plot函数绘制元分析结果并添加I²统计量的注释
orchaRd::orchard_plot(model_results, mod = "1", xlab = "Standardised mean difference") +
  annotate(geom = "text", x = 0.8, y = 1, label = paste0("italic(I)^{2} == ", round(I2[1], 4), "*\"%\""), 
           color = "black", parse = TRUE, size = 5) + scale_fill_manual(values = "grey") +
  scale_colour_manual(values = "grey")


# 使用orchaRd包的caterpillars函数绘制毛毛虫图
orchaRd::caterpillars(model_results, mod = "1", xlab = "Standardised mean difference")


# 使用escalc函数计算对数变异系数比(lnCVR)
english <- escalc(measure = "CVR", n1i = NStartControl, sd1i = SD_C, m1i = MeanC,
                  n2i = NStartExpt, sd2i = SD_E, m2i = MeanE, var.names = c("lnCVR", "vlnCVR"),
                  data = english)
# 拟合meta回归模型
english_MR0 <- rma.mv(yi = SMD, V = vSMD, mods = ~ManipType, random = list(~1 | StudyNo,
                                                                           ~1 | EffectID), data = english)
summary(english_MR0)


# 从meta回归模型english_MR0中提取和整理结果，创建结果表
res2 <- orchaRd::mod_results(english_MR0, mod = "ManipType", group = "StudyNo")
res2


# 使用rma.mv函数拟合meta回归模型，考察ManipType对lnCVR的影响
senior_MR0 <- rma.mv(yi = lnCVR, V = vlnCVR, mods = ~ManipType, random = list(~1 |
                                                                                StudyNo, ~1 | EffectID), data = english)
summary(senior_MR0)


# 从meta回归模型senior_MR0中提取和整理结果，创建特定条件下的结果表
res3 <- orchaRd::mod_results(senior_MR0, mod = "ManipType", group = "StudyNo", 
                             at = list(ManipType = "Quality"), subset = TRUE)
res3


# 绘制标准化均值差异(SMD)的图形
p1 <- orchaRd::orchard_plot(res2, mod = "ManipType", group = "StudyNo", xlab = "Standardised mean difference")
# 绘制对数变异系数比(lnCVR)的图形
p2 <- orchaRd::orchard_plot(res3, mod = "ManipType", group = "StudyNo", xlab = "log(CV ratio) (lnCVR)")
# 将两张图并排显示
(p1 / p2)


# 为res2数据绘制毛毛虫图，展示标准化均值差异(SMD)
p1c <- orchaRd::caterpillars(res2, mod = "ManipType", group = "StudyNo", xlab = "Standardised mean difference")
# 为res3数据绘制毛毛虫图，展示对数变异系数比(lnCVR)
p2c <- orchaRd::caterpillars(res3, mod = "ManipType", group = "StudyNo", xlab = "log(CV ratio) (lnCVR)")
# 尝试并排放置两个图形进行比较
p1c/p2c



##### Example 2：捕食和无脊椎动物群落 #####

data(eklof)
# 计算效应量
eklof <- escalc(measure = "ROM", n1i = N_control, sd1i = SD_control, m1i = mean_control,
                n2i = N_treatment, sd2i = SD_treatment, m2i = mean_treatment, var.names = c("lnRR",
                                                                                            "vlnRR"), data = eklof)
# 添加观测水平因子
eklof$Datapoint <- as.factor(seq(1, dim(eklof)[1], 1))
# 计算样本量
eklof$N <- rowSums(eklof[, c("N_control", "N_treatment")])
# 拟合meta回归模型
eklof_MR0 <- rma.mv(yi = lnRR, V = vlnRR, mods = ~Grazer.type, random = list(~1 |
                                                                               ExptID, ~1 | Datapoint), data = eklof)
#输出模型摘要
summary(eklof_MR0)


# 提取模型结果
f <- orchaRd::mod_results(eklof_MR0, mod = "Grazer.type", group = "ExptID")
# 创建图形
p3 <- orchaRd::orchard_plot(eklof_MR0, mod = "Grazer.type", group = "ExptID", xlab = "log(Response ratio) (lnRR)",
    transfm = "none")
p4 <- orchaRd::orchard_plot(eklof_MR0, mod = "Grazer.type", group = "ExptID", xlab = "log(Response ratio) (lnRR)",
    transfm = "none", angle = 45, g = FALSE)
p3/p4


p5 <- orchaRd::orchard_plot(eklof_MR0, mod = "Grazer.type", group = "ExptID", xlab = "log(Response ratio) (lnRR)")
p6 <- orchaRd::orchard_plot(eklof_MR0, mod = "Grazer.type", group = "ExptID", xlab = "log(Response ratio) (lnRR)",
                            angle = 45, N = "N", g = FALSE)
p5/p6


# 从eklof_MR0模型中提取结果，基于“Grazer.type”和“ExptID”
eklof_MR_result <- orchaRd::mod_results(eklof_MR0, mod = "Grazer.type", group = "ExptID")
# 使用提取的结果绘制毛毛虫图，展示log(Response ratio) (lnRR)
orchaRd::caterpillars(eklof_MR_result, mod = "Grazer.type", group = "ExptID", xlab = "log(Response ratio) (lnRR)")


p6_per <- orchaRd::orchard_plot(eklof_MR0, mod = "Grazer.type", group = "ExptID",
                                xlab = "Percentage Change (%)", angle = 45, N = "N", g = FALSE, transfm = "percentr")
p6_per



##### Example 3：母体与后代形态相关性 #####

# 加载‘lim’数据集
data(lim)
# 添加采样方差
lim$vi <- (1/sqrt(lim$N - 3))^2
# 让我们进行一个meta回归——我将处理文章的非独立性。
# 系统发育模型发现了系统发育效应，但是，我们可以将门分类作为固定效应来拟合，并使用果园图来探索它们。
lim_MR <- metafor::rma.mv(yi = yi, V = vi, mods = ~Phylum - 1, random = list(~1 |                                                                               Article, ~1 | Datapoint), data = lim)
summary(lim_MR)


# 绘制meta回归模型
orchaRd::orchard_plot(lim_MR, mod = "Phylum", group = "Article", xlab = "Correlation coefficient",
                      alpha = 0.5, transfm = "tanh", angle = 45, N = "N", cb = FALSE)


# 让我们在图形上添加 R2，并稍作修改：
R2 <- orchaRd::r2_ml(lim_MR)
# 我们可以用 R2 绘制果园图
orchaRd::orchard_plot(lim_MR, mod = "Phylum", group = "Article", xlab = "Correlation coefficient (r)",
                      alpha = 0.5, transfm = "tanh", angle = 45, N = "N", cb = TRUE) + 
  theme(legend.position = c(0.05,0.99), legend.justification = c(0, 1), legend.key.size = unit(1, "mm")) + 
  theme(legend.direction = "horizontal",legend.title = element_text(size = 8), legend.text = element_text(size = 10)) +
  annotate(geom = "text", x = 0.8, y = 0.7, label = paste0("italic(R)^{2} == ",
                                                           round(R2[1], 4) * 100), color = "black", parse = TRUE, size = 4)


# 绘制meta回归模型
lim_MR_results <- orchaRd::mod_results(lim_MR, mod = "Phylum", group = "Article")
orchaRd::caterpillars(lim_MR_results, mod = "Phylum", group = "Article", xlab = "Correlation coefficient",
                      transfm = "tanh", g = FALSE)


# 绘制meta回归模型
lim_MR_results <- orchaRd::mod_results(lim_MR, mod = "Phylum", group = "Article",
                                       at = list(Phylum = c("Chordata", "Arthropoda", "Mollusca")), subset = TRUE)
orchaRd::orchard_plot(lim_MR_results, xlab = "Correlation coefficient", transfm = "tanh",
                      g = TRUE, angle = 45)


# 绘制meta回归模型
lim_MR_results <- orchaRd::mod_results(lim_MR, mod = "Phylum", group = "Article",
                                       at = list(Phylum = c("Chordata", "Arthropoda", "Mollusca")), subset = TRUE)
orchaRd::orchard_plot(lim_MR_results, xlab = "Correlation coefficient", transfm = "tanh",
                      g = TRUE, flip = FALSE)



################################################################################
##### 果园图与meta回归模型：带有异方差残差的边际均值和条件均值 #################

##### Example 4：meta回归和异方差模型 #####

# 加载 orchaRd 附带的数据集
data(fish)
# 用于演示的子集数据
warm_dat <- fish


model_het <- metafor::rma.mv(
  yi = lnrr,                       # 要分析的效应大小
  V = lnrr_vi,                     # 每项研究效应大小的方差
  mods = ~trait.type,              # 固定效应模型，这里是用于分析的主要变量
  method = "REML",                 # 使用的估计方法是限制性最大似然(REML)
  test = "t",                      # 对固定效应进行假设检验时使用t测试
  random = list(~1 | group_ID, ~1 + trait.type | es_ID), # 指定随机效应结构
  rho = 0,                         # 认为随机效应之间的相关性为0
  struc = "HCS",                   # 使用的协方差结构，这里是异方差复合对称结构(Heterogeneous Compound Symmetry)
  data = warm_dat,                 # 提供数据的数据框
  control = list(optimizer = "optim", optmethod = "Nelder-Mead") # 优化算法控制参数，这里使用Nelder-Mead方法
)
model_het


# 提取模型结果
het_model <- orchaRd::mod_results(model_het, group = "group_ID", mod = "trait.type")
# 绘制果园图
orchaRd::orchard_plot(het_model, xlab = "lnRR")


# 清空工作空间
rm(list = ls())
# 加载数据 "pottier"
data("pottier")
# 加载物种间的系统发育关联矩阵 "phylo_matrix"
data("phylo_matrix")
# 加载采样方差矩阵 "VCV_dARR"
data("VCV_dARR")


# 在效应量水平上建立的异方差模型
mod.habitat_het <- rma.mv(
  yi = dARR,                         # 效应大小变量
  V = VCV_dARR,                      # 每项研究效应大小的方差
  mods = ~habitat,                   # 固定效应模型，这里是探索生境变量
  method = "REML",                   # 使用的估计方法是限制性最大可能似然法(REML)
  test = "t",                        # 对固定效应进行假设检验时使用t测试
  dfs = "contain",                   # 自由度的计算方法
  random = list(
    ~1 | species_ID,               # 指定物种ID的随机效应
    ~1 | phylogeny,                # 指定系统发育的随机效应
    ~habitat | es_ID               # 指定生境在每个效应大小估计值ID上的随机效应
  ),
  struct = "HCS",                    # 使用异方差复合对称(heterogeneous compound symmetry)结构
  rho = 0,                           # 认为随机效应之间的相关性为0
  R = list(phylogeny = phylo_matrix),# 系统发育关联矩阵，用于考虑物种间的系统发育相关性
  data = pottier                      # 提供数据的数据框
)
mod.habitat_het


# 绘制果园图
orchard_plot(mod.habitat_het, group = "species_ID", mod = "habitat", xlab = "dARR",angle = 45)


# 根据mod.habitat_het模型的结果，分组并格式化表格
flextable::flextable(mod_results(mod.habitat_het, group = "species_ID", mod = "habitat")$mod_table)


# 在效应量水平上建立的异方差模型
mod.habitat_het2 <- rma.mv(
  yi = dARR,                               # 效应大小变量
  V = VCV_dARR,                            # 每项研究效应大小的方差矩阵
  mods = ~habitat,                         # 固定效应，这里是生境类型
  method = "REML",                         # 估计方法为限制性最大似然(REML)
  test = "t",                              # 对固定效应进行假设检验时采用的是 t 测试
  dfs = "contain",                         # 自由度的计算方法
  random = list(
    ~habitat | species_ID,               # 物种ID内部的生境随机效应
    ~1 | phylogeny,                     # 系统发育的随机效应
    ~habitat | es_ID                    # 每个效应大小估计ID内的生境随机效应
  ),
  struct = "HCS",                         # 使用异方差复合对称结构
  rho = 0,                                # 随机效应之间相关性为0
  phi = 0,                                # 放宽默认假设，即没有提前指定的效应大小级别的随机效应之间的相关性
  R = list(phylogeny = phylo_matrix),     # 提供系统发育相关性矩阵
  data = pottier                           # 数据集
)
mod.habitat_het2


# 绘制果园图
orchard_plot(mod.habitat_het2, group = "species_ID", mod = "habitat", xlab = "dARR",angle = 45)


# 查看模型结果表
flextable::flextable(mod_results(mod.habitat_het2, group = "species_ID", mod = "habitat")$mod_table)



##### Example 5：带有连续调节变量的气泡图 #####

data(lim)
# 将年份数据转换为数值型
lim[, "year"] <- as.numeric(lim$year)
# 计算每项研究的方差
lim$vi <- 1/(lim$N - 3)
# meta回归模型拟合
model <- metafor::rma.mv(yi = yi, V = vi, mods = ~Environment * year, random = list(~1 | Article, ~1 | Datapoint), data = na.omit(lim))
# 模型结果处理
lim_bubble <- orchaRd::mod_results(model, mod = "year", group = "Article", weights = "prop",by = "Environment")
# 绘制气泡图
orchaRd::bubble_plot(lim_bubble, group = "Article", mod = "year", xlab = "Year",legend.pos = "top.left")


# 提取complete cases
data(pottier)
pottier2 <- pottier[complete.cases(pottier$body_mass), ]
# 拟合关于体重影响的模型
mod.body_mass <- metafor::rma.mv(yi = dARR, V = Var_dARR, mods = ~body_mass, method = "REML",
                                 test = "t", dfs = "contain", random = list(~1 | species_ID, ~1 | es_ID), data = pottier2)
# 绘制气泡图
orchaRd::bubble_plot(mod.body_mass, mod = "body_mass", group = "species_ID", xlab = "Body mass (g)",
                     ylab = "Acclimation Response Ratio (ARR)", legend.pos = "top.left")


# 绘制气泡图
orchaRd::bubble_plot(mod.body_mass, mod = "body_mass", group = "species_ID", xlab = "Body mass (g)",
                     ylab = "Acclimation Response Ratio (ARR)", legend.pos = "top.left") + 
                     geom_point(data = pottier,aes(x = body_mass, y = dARR, color = habitat, size = 1/sqrt(Var_dARR)), alpha = 0.6) +
                     scale_color_discrete()

##### Example 6：meta回归和条件平均效应量 #####

# 加载 orchaRd 附带的数据集
data(fish)
# 用于演示的子集数据
warm_dat <- fish


# 拟合meta回归模型
model <- metafor::rma.mv(
  yi = lnrr,  # 指定了模型的因变量，这里是对数响应比
  V = lnrr_vi,  # 对数响应比的方差
  mods = ~experimental_design + trait.type + deg_dif + treat_end_days,  # 模型中的固定效应
  method = "REML",  # 使用限制性最大似然法进行估计
  test = "t",  # 使用t检验来检验固定效应的显著性
  random = list(~1 | group_ID, ~1 + trait.type | es_ID),  # 模型中的随机效应
  rho = 0,  # 假设随机效应之间的相关性为0
  struc = "HCS",  # 指定随机效应的协方差结构为异方差复合对称结构
  data = warm_dat,  # 使用的数据集
  control = list(optimizer = "optim", optmethod = "Nelder-Mead")  # 指定优化算法为Nelder-Mead
)
model


# 提取结果
HetModel <- orchaRd::mod_results(model, group = "group_ID", mod = "trait.type", at = list(deg_dif = c(5,10, 15)), by = "deg_dif", weights = "prop")
# 绘制果园图
orchaRd::orchard_plot(HetModel, xlab = "lnRR", angle = 45, g = FALSE, legend.pos = "top.left",
                      condition.lab = "Temperature Difference") + theme(legend.direction = "vertical")


# 提取结果
HetModel2 <- orchaRd::mod_results(model, group = "group_ID", mod = "trait.type",
                                  at = list(deg_dif = c(5, 10, 15), treat_end_days = c(10)), by = "deg_dif", weights = "prop")
# 绘制果园图
orchaRd::orchard_plot(HetModel2, xlab = "lnRR", angle = 45, g = FALSE, legend.pos = "top.left",
                      condition.lab = "Temperature Difference") + theme(legend.direction = "vertical")






