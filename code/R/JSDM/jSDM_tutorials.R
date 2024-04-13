
# 安装jSDM包
# install.packages("jSDM")

# 加载jSDM包
library(jSDM)

################################################################################

# 2. 出现数据集

# 加载 'frogs' 数据集
data(frogs, package="jSDM")
# 显示 'frogs' 数据集的前几行以进行检查
head(frogs)


# data.obs
# 从 'frogs' 数据集中选取第4列到第12列的数据作为物种存在/缺失的观测数据，这些列代表不同物种在各个样点的存在状态（1表示存在，0表示不存在）。
PA_frogs <- frogs[,4:12]
# 归一化连续变量
Env_frogs <- cbind(scale(frogs[,1]), frogs[,2], scale(frogs[,3]))
# 设置 'Env_frogs' 数据框的列名，使其与原数据集 'frogs' 中对应的环境变量列名一致。
colnames(Env_frogs) <- colnames(frogs[,1:3])

################################################################################

# 3. 参数推断 

# 使用 'jSDM' 包中的 'jSDM_binomial_probit' 函数拟合一个二项分布的 Probit 模型。
mod_frogs_jSDM_probit <- jSDM_binomial_probit(
  # Markov Chain Monte Carlo (MCMC) 设置：指定 burn-in 期间的迭代次数、MCMC 迭代次数和抽样间隔。
  burnin=1000, mcmc=1000, thin=1,
  
  # 响应变量：物种的存在/缺失数据，从之前创建的 'PA_frogs' 数据框中获取。
  presence_data = PA_frogs,
  
  # 解释变量：环境变量，指定模型中地点的公式和数据。
  site_formula = ~.,   # 使用 '.' 表示使用 'Env_frogs' 数据框中的所有变量作为解释变量
  site_data = Env_frogs,
  
  # 模型规范：设置潜在因子的数量为2，并指定地点效应为随机效应。
  n_latent=2, site_effect="random",
  
  # 初始值：为 alpha（截距）、beta（环境变量系数）、lambda（潜在变量系数）、W（潜在变量值）设置初始值。
  alpha_start=0, beta_start=0,
  lambda_start=0, W_start=0,
  V_alpha=1,   # 设置随机效应的方差组件初始值为1。
  
  # 先验分布：设置随机效应方差组件 V_alpha 的形状参数和速率参数，beta 和 lambda 的均值和方差。
  shape_Valpha=0.1,
  rate_Valpha=0.1,
  mu_beta=0, V_beta=1,
  mu_lambda=0, V_lambda=1,
  
  # 其他参数：设置随机数种子以确保结果可重复，verbose=1 用于打印出模型拟合过程中的信息。
  seed=1234, verbose=1)

################################################################################

# 4. 结果分析 
# 获取模型中 beta 系数的参数数量
# 这是通过检查模型规格中 beta_start 组件的行数来完成的，这个组件包含了系数的起始值
np <- nrow(mod_frogs_jSDM_probit$model_spec$beta_start)

## 绘制前两个物种的 beta_j 系数的迹线图和密度图
# 设置图形参数，分割绘图区域为2行2列，以便同时查看多个图形
par(mfrow=c(2,2))
# 对于前两个物种（j=1,2）...
for (j in 1:2) {
  # 对于所有的 beta 系数（p=1到np）...
  for (p in 1:np) {
    # 使用coda包的traceplot函数绘制MCMC迹线图，以评估链的收敛情况
    # as.mcmc函数将模型中的 MCMC 样本转换为 coda 包能处理的格式
    coda::traceplot(coda::as.mcmc(mod_frogs_jSDM_probit$mcmc.sp[[j]][,p]))
    
    # 使用coda包的densplot函数绘制MCMC样本的密度图，以查看后验分布
    # main参数设置图形的标题，paste函数用于拼接标题字符串，cex.main用于调整标题字体大小
    coda::densplot(coda::as.mcmc(mod_frogs_jSDM_probit$mcmc.sp[[j]][,p]), 
                   main = paste(colnames(mod_frogs_jSDM_probit$mcmc.sp[[j]])[p],
                                ", 物种：", j), cex.main=0.9)
  }
}



## 绘制前两个物种的 lambda_j 参数的迹线图和密度图
# 获取模型中潜在变量的数量，它存储在模型规格的 n_latent 中
n_latent <- mod_frogs_jSDM_probit$model_spec$n_latent

# 设置图形参数，准备在2行2列的布局中绘制图形
par(mfrow=c(2,2))

# 遍历前两个物种（j=1,2）
for (j in 1:2) {
  # 对于每个物种的潜在变量参数（l=1到n_latent）
  for (l in 1:n_latent) {
    # 使用 coda 包的 traceplot 函数绘制 MCMC 迹线图
    # 这有助于检查 MCMC 算法的收敛性，确保参数估计的稳定性
    # mod_frogs_jSDM_probit$mcmc.sp[[j]] 是第 j 个物种的 MCMC 样本
    # [,np+l] 选择的是与潜在变量 lambda_j 相关的列
    coda::traceplot(coda::as.mcmc(mod_frogs_jSDM_probit$mcmc.sp[[j]][,np+l]))
    
    # 使用 coda 包的 densplot 函数绘制参数的后验密度图
    # 这有助于直观了解参数的后验分布情况
    # main 参数用于指定图形的主标题，paste 函数用于拼接字符串，cex.main 用于调整主标题的字体大小
    coda::densplot(coda::as.mcmc(mod_frogs_jSDM_probit$mcmc.sp[[j]][,np+l]), 
                   main = paste(colnames(mod_frogs_jSDM_probit$mcmc.sp[[j]])
                                [np+l], ", 物种：", j), cex.main=0.9)
  }
}



## 绘制前两个地点的潜在变量 W_i 的迹线图和密度图
# 设置图形参数，准备在2行2列的布局中绘制图形
par(mfrow=c(2,2))

# 遍历潜在变量的数量（由之前设定的 n_latent 控制）
for (l in 1:n_latent) {
  # 对前两个地点进行迭代（i=1,2）
  for (i in 1:2) {
    # 使用 coda 包的 traceplot 函数绘制第 l 个潜在变量在第 i 个地点的 MCMC 迹线图
    # 这有助于检查 MCMC 算法的收敛性，确保参数估计的稳定性
    # mod_frogs_jSDM_probit$mcmc.latent[[paste0("lv_",l)]] 是第 l 个潜在变量的 MCMC 样本
    # [,i] 表示选取第 i 个地点的样本数据
    coda::traceplot(mod_frogs_jSDM_probit$mcmc.latent[[paste0("lv_",l)]][,i],
                    main = paste0("潜在变量 W_", l, ", 地点 ", i),
                    cex.main=0.9)
    
    # 使用 coda 包的 densplot 函数绘制第 l 个潜在变量在第 i 个地点的后验密度图
    # 这有助于直观了解参数的后验分布情况
    # main 参数用于指定图形的主标题，paste0 函数用于拼接字符串，cex.main 用于调整主标题的字体大小
    coda::densplot(mod_frogs_jSDM_probit$mcmc.latent[[paste0("lv_",l)]][,i],
                   main = paste0("潜在变量 W_", l, ", 地点 ", i),
                   cex.main=0.9)
  }
}


## 绘制前两个地点的随机效应 alpha_i 的迹线图
# 使用 coda 包的 as.mcmc 函数将模型中 alpha_i 参数的 MCMC 输出转换为 coda 可处理的 MCMC 对象
# mod_frogs_jSDM_probit$mcmc.alpha[,1:2] 选取了前两个地点的 alpha_i 参数样本
# 然后使用 plot 函数绘制这些样本的迹线图
# 这有助于评估这些随机效应参数的 MCMC 算法的收敛性，确保参数估计的稳定性
plot(coda::as.mcmc(mod_frogs_jSDM_probit$mcmc.alpha[,1:2]))


## 绘制 V_alpha 和模型偏差 (Deviance) 的迹线图和密度图
# 设置图形参数，准备在2行2列的布局中绘制四幅图形
par(mfrow=c(2,2))

# 绘制 V_alpha 参数的迹线图
# V_alpha 是随机效应 alpha_i 的方差组件，迹线图有助于检查 MCMC 算法的收敛性
coda::traceplot(mod_frogs_jSDM_probit$mcmc.V_alpha)

# 绘制 V_alpha 参数的后验密度图
# 后验密度图用于展示参数的后验分布特征
coda::densplot(mod_frogs_jSDM_probit$mcmc.V_alpha)

# 绘制模型偏差 (Deviance) 的迹线图
# 模型偏差用于评估模型拟合的好坏，迹线图有助于评估模型偏差的稳定性和收敛性
coda::traceplot(mod_frogs_jSDM_probit$mcmc.Deviance)

# 绘制模型偏差 (Deviance) 的后验密度图
# 后验密度图展示了模型偏差的分布情况，有助于进一步理解模型的统计性质
coda::densplot(mod_frogs_jSDM_probit$mcmc.Deviance)



## 绘制预测的 Probit(theta) 和 Theta 的直方图
# 设置图形参数，准备在1行2列的布局中绘制两幅图形
par(mfrow=c(1,2))

# 绘制预测的 Probit(theta) 的直方图
# 'mod_frogs_jSDM_probit$probit_theta_latent' 包含从模型中预测出的 Probit(theta) 值
# Probit(theta) 是经过 Probit 链接函数转换的潜在变量值
# main 参数设置图形的主标题为 "Predicted probit theta"
# xlab 参数设置横坐标的标签为 "predicted probit theta"
hist(mod_frogs_jSDM_probit$probit_theta_latent,
     main = "Predicted probit theta",
     xlab = "predicted probit theta")

# 绘制预测的 Theta 的直方图
# 'mod_frogs_jSDM_probit$theta_latent' 包含从模型中预测出的 Theta 值
# Theta 是表示物种存在概率的参数，未经 Probit 转换
# main 参数设置图形的主标题为 "Predicted theta"
# xlab 参数设置横坐标的标签为 "predicted theta"
hist(mod_frogs_jSDM_probit$theta_latent,
     main = "Predicted theta", 
     xlab = "predicted theta")


################################################################################

# 5. 相关矩阵
## 绘制模型残差的相关图
# 使用 'plot_residual_cor' 函数绘制模型 'mod_frogs_jSDM_probit' 的残差相关图
# 此函数通常用于检查模型拟合效果，通过展示残差的相关性帮助识别模型中可能存在的问题
# 如果残差显示出明显的模式或结构，这可能表明模型中存在未被解释的变异
plot_residual_cor(mod_frogs_jSDM_probit)

################################################################################

# 6. 预测 
# 为预测选择和生成数据
## 从104个地点中随机选择50个地点
Id_sites <- sample.int(nrow(PA_frogs), 50)

## 选择所有物种
Id_species <- colnames(PA_frogs)

# 在这些地点上模拟新的协变量观测数据
# 创建一个新的数据矩阵，其行数为50（选定的地点数），列数为模型中协变量的数量
simdata <- matrix(nrow=50, ncol=ncol(mod_frogs_jSDM_probit$model_spec$site_data))
# 将新数据矩阵的列名设置为模型协变量的列名
colnames(simdata) <- colnames(mod_frogs_jSDM_probit$model_spec$site_data)
# 将新数据矩阵的行名设置为选定的地点ID
rownames(simdata) <- Id_sites
# 将数据矩阵转换为数据框
simdata <- as.data.frame(simdata)

# 为指定的协变量生成随机数据
simdata$Covariate_1 <- rnorm(50)  # 生成50个正态分布的随机数
simdata$Covariate_3 <- rnorm(50)  # 生成50个正态分布的随机数
simdata$Covariate_2 <- rbinom(50, 1, 0.5)  # 生成50个二项分布的随机数（成功概率为0.5）

# 使用模拟数据进行预测
# 调用 predict 函数，传入模型对象、新的协变量数据、物种ID和地点ID
# type="mean" 指定返回的预测类型为平均值
theta_pred <- predict(mod_frogs_jSDM_probit, newdata=simdata, Id_species=Id_species,
                      Id_sites=Id_sites, type="mean")

# 绘制预测的 Theta 的直方图
# 直方图展示了模拟数据下预测的存在概率 Theta 的分布情况
hist(theta_pred, main="Predicted theta with simulated data", xlab="predicted theta")


