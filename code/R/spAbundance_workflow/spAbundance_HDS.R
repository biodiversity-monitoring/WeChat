# 实例教程 | 使用spAbundance包进行物种多度的单变量和多变量空间建模（1）：拟合HDS模型

# install.packages("spAbundance")
library(spAbundance)
library(coda)
library(stars)
library(ggplot2)
# install.packages("unmarked")
library(unmarked)
set.seed(123)


# 加载数据集
data(neonDWP)
# 获取数据内容概览
str(neonDWP)


sp.names <- dimnames(neonDWP$y)[[1]]
dat.EATO <- neonDWP
dat.EATO$y <- dat.EATO$y[sp.names == "EATO", , ]
# 在每个地点观察到的 EATO 个体数量
apply(dat.EATO$y, 1, sum)


################################################################################
##### 2 单物种HDS模型 ##########################################################

##### 2.1 用DS()拟合单物种HDS模型#####

## DS()函数的参数
## DS(abund.formula, det.formula, data, inits, priors, tuning,
##    n.batch, batch.length, accept.rate = 0.43, family = 'Poisson',
##    transect = 'line', det.func = 'halfnormal',
##    n.omp.threads = 1, verbose = TRUE,
##    n.report = 100, n.burn = round(.10 * n.batch * batch.length), n.thin = 1,
##    n.chains = 1, ...)


# 使用scale()函数对模型中的所有系数进行标准化处理，使其均值为0，标准偏差为1
abund.formula <- ~ scale(forest) + scale(grass)
det.formula <- ~ scale(wind)


# dat.EATO中的dist.breaks元素包含五个值
dat.EATO$dist.breaks


# 从平方公里转换为公顷的偏移量
radius.km <- .25
pi * radius.km^2 * 100


# 初始值
inits.list <- list(beta = 0, alpha = 0, kappa = 1, N = apply(dat.EATO$y, 1, sum))


prior.list <- list(beta.normal = list(mean = 0, var = 100),
                   alpha.normal = list(mean = 0, var = 100),
                   kappa.unif = c(0, 100))


n.batch <- 2000
batch.length <- 25
# 每条链的 MCMC 样本总数
batch.length * n.batch


tuning <- list(beta = 0.5, alpha = 0.5, kappa = 0.5)
# 默认情况下，accept.rate = 0.43，因此我们没有指定


n.burn <- 20000
n.thin <- 30
n.chains <- 3


out <- DS(abund.formula = abund.formula,
          det.formula = det.formula,
          data = dat.EATO,
          n.batch = n.batch,
          batch.length = batch.length,
          inits = inits.list,
          family = 'Poisson',
          det.func = 'halfnormal',
          transect = 'point',
          tuning = tuning,
          priors = prior.list,
          accept.rate = 0.43,
          n.omp.threads = 1,
          verbose = TRUE,
          n.report = 500,
          n.burn = n.burn,
          n.thin = n.thin,
          n.chains = n.chains)


summary(out)


plot(out, param = 'beta', density = FALSE)



det.int.samples <- out$alpha.samples[, 1] 
det.quants <- quantile(exp(out$alpha.samples[, 1]), c(0.025, 0.5, 0.975))

x.vals <- seq(0, .125, length.out = 200)
n.vals <- length(x.vals)
p.plot.df <- data.frame(med = gxhn(x.vals, det.quants[2]), 
                        low = gxhn(x.vals, det.quants[1]), 
                        high = gxhn(x.vals, det.quants[3]),
                        x.val = x.vals * 1000)

ggplot(data = p.plot.df) + 
  geom_ribbon(aes(x = x.val, ymin = low, ymax = high), fill = 'grey', 
              alpha = 0.5) +
  theme_bw(base_size = 14) +
  geom_line(aes(x = x.val, y = med), col = 'black', linewidth = 1.3) + 
  labs(x = 'Distance (m)', y = 'Detection Probability')


##### 2.2 后验预测 #####

ppc.out <- ppcAbund(out, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out)


##### 2.3 使用WAIC选择模型 #####

waicAbund(out)


out.nb <- DS(abund.formula = ~ scale(forest) + scale(grass),
             det.formula = ~ scale(wind),
             data = dat.EATO,
             n.batch = n.batch,
             batch.length = batch.length,
             inits = inits.list,
             family = 'NB',
             det.func = 'halfnormal',
             transect = 'point',
             tuning = tuning,
             priors = prior.list,
             accept.rate = 0.43,
             n.omp.threads = 1,
             verbose = FALSE,
             n.report = 400,
             n.burn = n.burn,
             n.thin = n.thin,
             n.chains = n.chains)

waicAbund(out)

waicAbund(out.nb)


##### 2.4 预测 #####

data(neonPredData)
str(neonPredData)


# 根据模型拟合所用的值，将协变量居中并按比例放大
forest.pred <- (neonPredData$forest - mean(dat.EATO$covs$forest)) / 
  sd(dat.EATO$covs$forest)
grass.pred <- (neonPredData$grass - mean(dat.EATO$covs$grass)) / 
  sd(dat.EATO$covs$grass)


X.0 <- cbind(1, forest.pred, grass.pred)
colnames(X.0) <- c('(Intercept)', 'forest', 'grass')
out.pred <- predict(out, X.0)
str(out.pred)



mu.0.quants <- apply(out.pred$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975))
plot.df <- data.frame(Easting = neonPredData$easting,
                      Northing = neonPredData$northing,
                      mu.0.med = mu.0.quants[2, ],
                      mu.0.ci.width = mu.0.quants[3, ] - mu.0.quants[1, ])
coords.stars <- st_as_stars(plot.df, crs = st_crs(32617))
coords.sf <- st_as_sf(as.data.frame(dat.EATO$coords), coords = c('easting', 'northing'), 
                      crs = st_crs(32617))
# Plot of median estimate
ggplot() +
  geom_stars(data = coords.stars, aes(x = Easting, y = Northing, fill = mu.0.med)) +
  geom_sf(data = coords.sf) +
  scale_fill_viridis_c(na.value = NA) +
  theme_bw(base_size = 12) +
  labs(fill = 'Individuals\nper ha', x = 'Longitude', y = 'Latitude', 
       title = 'Eastern Towhee Median Density')



# Plot of 95% CI width
ggplot() +
  geom_stars(data = coords.stars, aes(x = Easting, y = Northing, fill = mu.0.ci.width)) +
  geom_sf(data = coords.sf) +
  scale_fill_viridis_c(na.value = NA) +
  theme_bw(base_size = 12) +
  labs(fill = 'Individuals\nper ha', x = 'Longitude', y = 'Latitude', 
       title = 'Eastern Towhee Density 95% CI Width')



################################################################################
##### 3 单物种空间HDS模型 ##########################################################

##### 3.2 用spDS()拟合单物种空间HDS模型 #####

## spDS()函数的参数
## spDS(abund.formula, det.formula, data, inits, priors, tuning,
##      cov.model = 'exponential', NNGP = TRUE,
##      n.neighbors = 15, search.type = 'cb',
##      n.batch, batch.length, accept.rate = 0.43, family = 'Poisson',
##      transect = 'line', det.func = 'halfnormal',
##      n.report = 100, n.burn = round(.10 * n.batch * batch.length), n.thin = 1, 
##      n.chains = 1, ...)


abund.formula <- ~ scale(forest) + scale(grass)
det.formula <- ~ scale(wind)
str(dat.EATO) # spDS() 需要使用坐标


# 所有地点之间的配对距离
dist.mat <- dist(dat.EATO$coords)
# 指数协方差模型
cov.model <- 'exponential'
# 指定 inits 列表
inits.list <- list(beta = 0, alpha = 0, kappa = 1,
                   sigma.sq = 1, phi = 3 / mean(dist.mat),
                   w = rep(0, nrow(dat.EATO$y)),
                   N = apply(dat.EATO$y, 1, sum))


NNGP <- TRUE
n.neighbors <- 15
search.type <- 'cb'


min.dist <- min(dist.mat)
max.dist <- max(dist.mat)
priors <- list(alpha.normal = list(mean = 0, var = 100),
               beta.normal = list(mean = 0, var = 100),
               kappa.unif = c(0, 100),
               sigma.sq.ig = c(2, 1),
               phi.unif = c(3 / max.dist, 3 / min.dist))


tuning <- list(beta = 0.5, alpha = 0.5, kappa = 0.5, beta.star = 0.5,
               w = 0.5, phi = 0.5)



verbose <- TRUE
batch.length <- 25
n.batch <- 2000
# 每条链的 MCMC 样本总数
batch.length * n.batch


n.report <- 500
n.omp.threads <- 1


n.burn <- 20000
n.thin <- 30
n.chains <- 3
# 大约运行时间：2 分钟
out.sp <- spDS(abund.formula = abund.formula,
               det.formula = det.formula,
               data = dat.EATO,
               inits = inits.list,
               priors = priors,
               n.batch = n.batch,
               batch.length = batch.length,
               tuning = tuning,
               cov.model = cov.model,
               NNGP = NNGP,
               n.neighbors = n.neighbors,
               search.type = search.type,
               n.omp.threads = n.omp.threads,
               n.report = n.report,
               family = 'Poisson',
               det.func = 'halfnormal',
               transect = 'point',
               verbose = TRUE,
               n.burn = n.burn,
               n.thin = n.thin,
               n.chains = n.chains)


summary(out.sp)


w.means <- apply(out.sp$w.samples, 2, mean)
hist(w.means)


##### 3.3 后验预测检查 #####

ppc.out.sp <- ppcAbund(out.sp, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out.sp)


##### 3.4 使用WAIC选择模型 #####

# 非空间
waicAbund(out)

# 空间
waicAbund(out.sp)

##### 3.5 预测 #####

# 再次查看预测数据集
str(neonPredData)

# 利用空间 HDS 模型进行预测需要坐标
coords.0 <- neonPredData[, c('easting', 'northing')]
out.sp.pred <- predict(out.sp, X.0, coords = coords.0, n.report = 400)



mu.0.quants <- apply(out.sp.pred$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975))
plot.df <- data.frame(Easting = neonPredData$easting,
                      Northing = neonPredData$northing,
                      mu.0.med = mu.0.quants[2, ],
                      mu.0.ci.width = mu.0.quants[3, ] - mu.0.quants[1, ])
coords.stars <- st_as_stars(plot.df, crs = st_crs(32617))
coords.sf <- st_as_sf(as.data.frame(dat.EATO$coords), coords = c('easting', 'northing'),
                      crs = st_crs(32617))
# Plot of median estimate
ggplot() +
  geom_stars(data = coords.stars, aes(x = Easting, y = Northing, fill = mu.0.med)) +
  geom_sf(data = coords.sf) +
  scale_fill_viridis_c(na.value = NA) +
  theme_bw(base_size = 14) +
  labs(fill = 'Individuals\nper ha', x = 'Longitude', y = 'Latitude',
       title = 'Eastern Towhee Median Density')



# Plot of 95% CI width
ggplot() +
  geom_stars(data = coords.stars, aes(x = Easting, y = Northing, fill = mu.0.ci.width)) +
  geom_sf(data = coords.sf) +
  scale_fill_viridis_c(na.value = NA) +
  theme_bw(base_size = 14) +
  labs(fill = 'Individuals\nper ha', x = 'Longitude', y = 'Latitude',
       title = 'Eastern Towhee Density 95% CI Width')


################################################################################
##### 4 多物种HDS模型 ##########################################################

##### 4.2 用msDS()拟合多物种HDS模型 #####

## msDS(abund.formula, det.formula, data, inits, priors,
##      tuning, n.batch, batch.length, accept.rate = 0.43,
##      family = 'Poisson', transect = 'line', det.func = 'halfnormal',
##      n.omp.threads = 1, verbose = TRUE, n.report = 100,
##      n.burn = round(.10 * n.batch * batch.length), n.thin = 1,
##      n.chains = 1, ...)


# 回顾完整数据集的结构
str(neonDWP)


abund.formula <- ~ scale(forest) + scale(grass)
det.formula <- ~ scale(wind)


# 物种数量
n.sp <- dim(neonDWP$y)[1]
ms.inits <- list(alpha.comm = 0,
                 beta.comm = 0,
                 beta = 0,
                 alpha = 0,
                 tau.sq.beta = 1,
                 kappa = 1,
                 tau.sq.alpha = 1,
                 sigma.sq.mu = 0.5,
                 N = apply(neonDWP$y, c(1, 2), sum, na.rm = TRUE))


ms.priors <- list(beta.comm.normal = list(mean = 0, var = 100),
                  alpha.comm.normal = list(mean = 0, var = 100),
                  tau.sq.beta.ig = list(a = 0.1, b = 0.1),
                  tau.sq.alpha.ig = list(a = 0.1, b = 0.1), 
                  kappa.unif = list(a = 0, b = 100))



# 指定初始调整值
ms.tuning <- list(beta = 0.3, alpha = 0.3, beta.star = 0.5, kappa = 0.5)
# 大约运行时间：6 分钟
out.ms <- msDS(abund.formula = abund.formula,
               det.formula = det.formula,
               data = neonDWP,
               inits = ms.inits,
               n.batch = 2000,
               tuning = ms.tuning,
               batch.length = 25,
               priors = ms.priors,
               n.omp.threads = 1,
               family = 'Poisson',
               det.func = 'halfnormal',
               transect = 'point',
               verbose = TRUE,
               n.report = 500,
               n.burn = 20000,
               n.thin = 30,
               n.chains = 3)


summary(out.ms)



# 生成物种特定检测概率图 ---------------
det.int.samples <- out.ms$alpha.samples[, 1:n.sp]
det.means <- apply(exp(det.int.samples), 2, mean)
det.comm.means <- mean(exp(out.ms$alpha.comm.samples[, 1]))
det.comm.quants <- quantile(exp(out.ms$alpha.comm.samples[, 1]), c(0.025, 0.975))
x.vals <- seq(0, .250, length.out = 200)
n.vals <- length(x.vals)
p.plot.df <- data.frame(val = NA,
                        x.val = rep(x.vals, n.sp),
                        sp = rep(sp.names, each = n.vals))
for (i in 1:n.sp) {
  indx <- ((i - 1) * n.vals + 1):(i * n.vals)
  p.plot.df$val[indx] <- gxhn(x.vals, det.means[i])
}

comm.plot.df <- data.frame(mean = gxhn(x.vals, det.comm.means),
                           x.val = x.vals,
                           low = gxhn(x.vals, det.comm.quants[1]),
                           high = gxhn(x.vals, det.comm.quants[2]))
ggplot(data = comm.plot.df) +
  geom_ribbon(aes(x = x.val, ymin = low, ymax = high), fill = 'grey',
              alpha = 0.5) +
  geom_line(data = p.plot.df, aes(x = x.val, y = val, col = sp), lwd = 1, lty = 1) +
  theme_bw(base_size = 14) +
  geom_line(aes(x = x.val, y = mean), col = 'black', lwd = 1.3) +
  labs(x = 'Distance (m)', y = 'Detection Probability', col = 'Species')



##### 4.3后验预测检查 #####

ppc.ms.out <- ppcAbund(out.ms, fit.stat = 'chi-squared', group = 1)


summary(ppc.ms.out)


##### 4.4 使用WAIC选择模型 #####

# Multi-species HDS model 
waicAbund(out.ms, by.sp = TRUE)

# Single-species Poisson HDS model for EATO
waicAbund(out)


##### 4.5 预测 #####

# 回顾预测设计矩阵中的内容 X.0
str(X.0)
out.ms.pred <- predict(out.ms, X.0)
# 请注意，这需要相当大的内存来执行
str(out.ms.pred)
# 每个地点所有物种的预期总多度
mu.sum.samples <- apply(out.ms.pred$mu.0.samples, c(1, 3), sum)
# 每个地点的平均总多度
mu.sum.means <- apply(mu.sum.samples, 2, mean)
plot.df <- data.frame(Easting = neonPredData$easting,
                      Northing = neonPredData$northing,
                      mu.sum.means = mu.sum.means)
coords.stars <- st_as_stars(plot.df, crs = st_crs(32617))
coords.sf <- st_as_sf(as.data.frame(neonDWP$coords), coords = c('easting', 'northing'),
                      crs = st_crs(32617))
# Plot of median estimate
ggplot() +
  geom_stars(data = coords.stars, aes(x = Easting, y = Northing, fill = mu.sum.means)) +
  geom_sf(data = coords.sf) +
  scale_fill_viridis_c(na.value = NA) +
  theme_bw(base_size = 14) +
  labs(fill = 'Individuals\nper ha', x = 'Longitude', y = 'Latitude',
       title = 'Average total density of birds')


################################################################################
##### 5 潜在因子多物种HDS模型 ##################################################

##### 5.2 用lfMsDS()拟合潜因多物种HDS模型 #####

## lfMsDS(abund.formula, det.formula, data, inits, priors,
##        tuning, n.factors, n.batch, batch.length, accept.rate = 0.43,
##        family = 'Poisson', transect = 'line', det.func = 'halfnormal',
##        n.omp.threads = 1, verbose = TRUE, n.report = 100,
##        n.burn = round(.10 * n.batch * batch.length), n.thin = 1,
##        n.chains = 1, ...)


n.factors <- 1


abund.formula <- ~ scale(forest) + scale(grass)
det.formula <- ~ scale(wind)
str(neonDWP)


# 将所有 lambda 初始值设为 0
lambda.inits <- matrix(0, n.sp, n.factors)
# 设置对角线元素为 1
diag(lambda.inits) <- 1
# 将下三角元素设置为标准正态分布的随机值
lambda.inits[lower.tri(lambda.inits)] <- rnorm(sum(lower.tri(lambda.inits)))
# 确保它是一个矩阵
lambda.inits <- as.matrix(lambda.inits)
# 检查一下
lambda.inits


w.inits <- matrix(0, n.factors, ncol(neonDWP$y))
ms.inits <- list(alpha.comm = 0,
                 beta.comm = 0,
                 beta = 0,
                 alpha = 0,
                 tau.sq.beta = 1,
                 kappa = 1,
                 tau.sq.alpha = 1,
                 sigma.sq.mu = 0.5,
                 lambda = lambda.inits, 
                 w = w.inits,
                 N = apply(neonDWP$y, c(1, 2), sum, na.rm = TRUE))


ms.priors <- list(beta.comm.normal = list(mean = 0, var = 100),
                  alpha.comm.normal = list(mean = 0, var = 100),
                  tau.sq.beta.ig = list(a = 0.1, b = 0.1),
                  tau.sq.alpha.ig = list(a = 0.1, b = 0.1), 
                  kappa.unif = list(a = 0, b = 100))




# 指定初始调整值
ms.tuning <- list(beta = 0.3, alpha = 0.3, beta.star = 0.5, kappa = 0.5, 
                  w = 0.5, lambda = 0.5)
# 大约运行时间：2.5 分钟
out.lf.ms <- lfMsDS(abund.formula = abund.formula,
                    det.formula = det.formula,
                    data = neonDWP,
                    inits = ms.inits,
                    n.batch = 2000,
                    tuning = ms.tuning,
                    batch.length = 25,
                    priors = ms.priors,
                    n.omp.threads = 1,
                    n.factors = n.factors,
                    family = 'Poisson',
                    det.func = 'halfnormal',
                    transect = 'point',
                    verbose = TRUE,
                    n.report = 500,
                    n.burn = 20000,
                    n.thin = 30,
                    n.chains = 1)


summary(out.lf.ms)


# ESS for lambda
out.lf.ms$ESS$lambda


# 潜在因子载荷的后验定量
summary(out.lf.ms$lambda.samples)$quantiles


##### 5.3后验预测 #####

ppc.out.lf.ms <- ppcAbund(out.lf.ms, fit.stat = 'chi-squared', group = 1)


# 用贝叶斯 p 值进行总结
summary(ppc.out.lf.ms)



##### 5.4 使用WAIC选择模型 #####

# 有潜在因子
waicAbund(out.lf.ms, by.sp = TRUE)

# 没有潜在因子
waicAbund(out.ms, by.sp = TRUE)



##### 5.5 预测 #####

out.lf.ms.pred <- predict(out.lf.ms, X.0, coords.0)


################################################################################
##### 6 空间因子多物种HDS模型 ##################################################

##### 6.2 用sfMsDS()拟合空间因子多物种HDS模型 #####

## sfMsDS(abund.formula, det.formula, data, inits, priors,  
##        tuning, cov.model = 'exponential', NNGP = TRUE, 
##        n.neighbors = 15, search.type = 'cb', n.factors, 
##        n.batch, batch.length, accept.rate = 0.43, 
##        family = 'Poisson', transect = 'line', det.func = 'halfnormal', 
##        n.omp.threads = 1, verbose = TRUE, n.report = 100, 
##        n.burn = round(.10 * n.batch * batch.length), n.thin = 1, 
##        n.chains = 1, ...)


# 公式
abund.formula <- ~ scale(forest) + scale(grass)
det.formula <- ~ scale(wind)

# 将所有 lambda 初始值设为 0
lambda.inits <- matrix(0, n.sp, n.factors)
# 设置对角线元素为 1
diag(lambda.inits) <- 1
# 将下三角元素设置为标准正态分布的随机值
lambda.inits[lower.tri(lambda.inits)] <- rnorm(sum(lower.tri(lambda.inits)))
# 确保它是一个矩阵
lambda.inits <- as.matrix(lambda.inits)
# 检查一下
lambda.inits


# 对于 phi 的初始值和先验值，所有地点之间的配对距离
dist.mat <- dist(neonDWP$coords)
w.inits <- matrix(0, n.factors, ncol(neonDWP$y))
ms.inits <- list(alpha.comm = 0,
                 beta.comm = 0,
                 beta = 0,
                 alpha = 0,
                 tau.sq.beta = 1,
                 kappa = 1,
                 phi = 3 / mean(dist.mat),
                 tau.sq.alpha = 1,
                 sigma.sq.mu = 0.5,
                 lambda = lambda.inits,
                 w = w.inits,
                 N = apply(neonDWP$y, c(1, 2), sum, na.rm = TRUE))
# 指数协方差模型
cov.model <- 'exponential'

# Priors
ms.priors <- list(beta.comm.normal = list(mean = 0, var = 100),
                  alpha.comm.normal = list(mean = 0, var = 100),
                  tau.sq.beta.ig = list(a = 0.1, b = 0.1),
                  tau.sq.alpha.ig = list(a = 0.1, b = 0.1),
                  phi.unif = list(a = 3 / max(dist.mat), 3 / min(dist.mat)),
                  kappa.unif = list(a = 0, b = 100))

# 指定初始调整值
ms.tuning <- list(beta = 0.3, alpha = 0.3, beta.star = 0.5, kappa = 0.5, 
                  w = 0.5, lambda = 0.5, phi = 0.5)
# 约运行时间：2.88 分钟
out.sf.ms <- sfMsDS(abund.formula = abund.formula,
                    det.formula = det.formula,
                    data = neonDWP,
                    inits = ms.inits,
                    n.batch = 2000,
                    tuning = ms.tuning,
                    batch.length = 25,
                    priors = ms.priors,
                    n.omp.threads = 1,
                    n.factors = n.factors,
                    family = 'Poisson',
                    det.func = 'halfnormal',
                    cov.model = 'exponential',
                    NNGP = TRUE, 
                    n.neighbors = 15,
                    transect = 'point',
                    verbose = TRUE,
                    n.report = 500,
                    n.burn = 20000,
                    n.thin = 30,
                    n.chains = 1)


summary(out.sf.ms)


##### 6.3 后验预测检查 #####

ppc.out.sf.ms <- ppcAbund(out.sf.ms, fit.stat = 'chi-squared', group = 1)


# 用贝叶斯 p 值进行总结
summary(ppc.out.sf.ms)

##### 6.4 使用WAIC选择模型 #####

# 有潜在因子
waicAbund(out.lf.ms, by.sp = TRUE)

# 没有潜在因子
waicAbund(out.sf.ms, by.sp = TRUE)

##### 6.5 预测 #####

out.sf.ms.pred <- predict(out.sf.ms, X.0, coords.0)






