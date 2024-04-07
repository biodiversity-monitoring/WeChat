# SDMs：Model fitting, prediction, and evaluation
# author: Biodiversity Monitoring


##模型拟合######################################################################

## 替换为数据放置目录的路径
setwd("C:/……")
getwd()
## 读取存储在"sdm.Rds"文件中的物种分布模型数据到sdmdata变量中
sdmdata <- readRDS("sdm.Rds")
## 读取存储在"pvals.Rds"文件中的物种出现值数据到presvals变量中
presvals <- readRDS("pvals.Rds")


## 使用广义线性模型（Generalized Linear Model, GLM）分析物种分布数据
## 模型m1预测物种出现的概率(pb)与三个生态因子（bio1, bio5, bio12）之间的关系
m1 <- glm(pb ~ bio1 + bio5 + bio12, data=sdmdata)
## 输出模型m1的类型，以确认其为广义线性模型
class(m1)
## 显示模型m1的摘要信息，包括系数估计、统计显著性等
summary(m1)
## 创建第二个广义线性模型m2，使用sdmdata数据集中的所有变量作为解释变量来预测物种出现概率(pb)
m2 = glm(pb ~ ., data=sdmdata)
## 输出模型m2的详细信息，包括系数估计值、标准误差等
m2


## 载入predicts包，该包提供了预测生态模型和分析物种分布的函数
library(predicts)
## 使用envelope函数分析物种出现值与三个生态因子（bio1, bio5, bio12）之间的关系，
## 并计算出这些因子的边界条件。这个分析帮助理解物种出现与特定生态因子的依赖性
bc <- envelope(presvals[,c("bio1", "bio5", "bio12")])
## 输出边界条件分析结果
bc

##模型预测######################################################################

## 创建生态因子数据框
## bio1, bio5, bio12分别代表三组生态因子的数据
bio1 <- c(40, 150, 200)
bio5 <- c(60, 115, 290)
bio12 <- c(600, 1600, 1700)
## 将这三组生态因子合并为一个数据框pd
pd <- data.frame(cbind(bio1, bio5, bio12))
## 输出数据框pd以查看其内容
pd
## 使用m1模型对新的生态因子数据（pd）进行预测
predict(m1, pd)


## 对bc对象执行部分响应分析，分析bio1因子对物种出现概率的影响
pr <- partialResponse(bc, presvals, "bio1")
## 绘制部分响应分析的结果，使用线型图展示
plot(pr, type="l")


## 载入predicts包
library(predicts)
## 从predicts包中获取名为"bio.tif"的示例栅格数据文件路径
f <- system.file("ex/bio.tif", package="predicts")
## 读取栅格数据，将其存储在predictors变量中
predictors <- rast(f)
## 输出predictors对象的变量名
names(predictors)
## 使用m1模型对predictors数据进行空间预测
p <- predict(predictors, m1)
## 绘制空间预测结果
plot(p)

##模型评估######################################################################

## 生成第一组数据：50个服从正态分布的随机数，均值为0.7，标准差为0.3
p <- rnorm(50, mean=0.7, sd=0.3)
## 生成第二组数据：50个服从正态分布的随机数，均值为0.4，标准差为0.4
a <- rnorm(50, mean=0.4, sd=0.4)
## 设置绘图参数，准备在一个图形窗口中并排绘制两幅图
par(mfrow=c(1, 2))
## 绘制第一组数据（p），使用红色，标记类型为21（填充的圆圈）
plot(sort(p), col='red', pch=21)
## 在相同的图形上添加第二组数据（a），使用蓝色，标记类型为24（三角形）
points(sort(a), col='blue', pch=24)
## 添加图例，位置在图形的左上角，显示“presence”和“absence”两个类别，分别用红色和蓝色的对应标记类型表示
legend(1, 0.95 * max(a,p), c('presence', 'absence'),
       pch=c(21,24), col=c('red', 'blue'))
## 将两组数据合并为一个向量
comb <- c(p,a)
## 创建一个分组向量，长度与comb相同，标记前半部分为'presence'，后半部分为'absence'
group <- c(rep('presence', length(p)), rep('absence', length(a)))
## 使用boxplot函数绘制合并数据的箱线图，根据group变量分组，分别用蓝色和红色表示两组数据
boxplot(comb~group, col=c('blue', 'red'))


## 创建一个分组向量，前一半元素标记为1（对应于'p'，即存在的情况），后一半元素标记为0（对应于'a'，即不存在的情况）
group = c(rep(1, length(p)), rep(0, length(a))) 
## 使用Pearson相关性检验来评估合并后的数据向量`comb`与分组向量`group`之间的相关性
## `$estimate`提取了相关性检验结果中的估计值，通常是相关系数
cor.test(comb, group)$estimate
## 使用Wilcoxon秩和检验来比较两组数据（`p`和`a`）的中心位置是否有显著差异
## 这是一个非参数检验，适用于不假定数据分布的情况
mv <- wilcox.test(p,a)
## 计算曲线下面积（AUC）作为评估两组数据区分度的一个指标
## AUC是通过将Wilcoxon秩和统计量除以两组数据数量的乘积来计算得出的
auc <- as.numeric(mv$statistic) / (length(p) * length(a))
auc


## 载入predicts包
library(predicts)
## 使用pa_evaluate函数评估两组数据（存在的p和不存在的a）的性能指标
## 函数返回的结果赋值给变量e
e <- pa_evaluate(p=p, a=a)
## 获取结果对象e的类别
class(e)
## 输出e的详细信息，可能包括各种性能评估指标
e
## 设置绘图参数，准备在一个图形窗口中并排绘制两幅图
par(mfrow=c(1, 2))
## 绘制e对象的密度图，以可视化两组数据的分布情况
plot(e, "density")
## 绘制e对象的箱线图，使用蓝色和红色分别表示两组数据
## 这个图形用于比较存在和不存在数据的中心位置、分布范围等统计特性
plot(e, "boxplot", col=c('blue', 'red'))


## 从sdmdata数据集中随机抽样75%的行作为训练数据的索引
samp <- sample(nrow(sdmdata), round(0.75 * nrow(sdmdata)))
## 根据上面抽样得到的索引，创建训练数据集traindata
traindata <- sdmdata[samp,]
## 进一步筛选训练数据集，只保留第一列（假设为物种存在/不存在的标记）为1的行，且只保留第2到第9列的数据
traindata <- traindata[traindata[,1] == 1, 2:9]
## 创建测试数据集testdata，包含未被抽选为训练数据的剩余行
testdata <- sdmdata[-samp,]
## 使用训练数据traindata通过envelope函数构建生态学模型，函数返回的模型赋值给变量bc
bc <- envelope(traindata)
## 使用假定的pa_evaluate函数评估模型bc在测试数据上的性能，特别地，将测试数据分为正例（第一列为1的行）和负例（第一列为0的行）
e <- pa_evaluate(testdata[testdata[,1]==1,], testdata[testdata[,1]==0,], bc)
## 输出模型性能评估结果e
e
## 绘制ROC曲线来可视化模型性能，ROC曲线是通过比较真正例率和假正例率在不同阈值下的变化而绘制的
plot(e, "ROC")


## 从sdmdata数据集中筛选出存在（pres）和背景（back）的观察值，只保留第2到第9列的特征
pres <- sdmdata[sdmdata[,1] == 1, 2:9]
back <- sdmdata[sdmdata[,1] == 0, 2:9]


## 设定交叉验证的折数（k-fold）为5
k <- 5
## 使用folds函数（这可能是假设存在的特定函数，用于生成分组）对存在数据（pres）生成k个分组
group <- folds(pres, k)
## 输出前10个数据的分组情况，以便检查分组是否正确
group[1:10]
## 输出所有唯一分组的值，以确认分组正确生成
unique(group)
## 使用table函数查看每个分组中的数据量，以确保数据均匀分布到每个折中
table(group)


## 初始化一个列表e来存储每一次迭代的评估结果
e <- list()
## 对于k折中的每一折，将其作为测试集，其他作为训练集，然后进行模型训练和测试
for (i in 1:k) {
  # 从pres数据集中排除当前折（即作为测试集的折）来创建训练集
  train <- pres[group != i,]
  # 将当前折作为测试集
  test <- pres[group == i,]
  # 使用envelope函数（假设存在的特定函数，用于模型构建）和训练集数据构建模型
  bc <- envelope(train)
  # 对测试集数据进行预测
  p <- predict(bc, test)
  # 对背景数据（back）进行预测
  a <- predict(bc, back)
  # 使用pa_evaluate函数（同样假设存在的特定函数，用于性能评估）评估模型的性能，并将结果存储到e列表中
  e[[i]] <- pa_evaluate(p, a)
}


## 使用lapply函数遍历e列表（包含每次交叉验证迭代的模型评估对象），
## 并应用一个匿名函数来提取每个对象中的统计数据（假设评估结果对象具有stats属性）
stats <- lapply(e, function(x){x@stats})
## 使用do.call函数和rbind将所有迭代中的统计数据合并成一个矩阵，
## 其中每一行对应一个迭代的统计结果
stats <- do.call(rbind, stats)
## 计算合并后矩阵中每一列的均值，这将给出所有交叉验证迭代中每个统计量的平均值
colMeans(stats)


## 使用system.file函数尝试从一个名为"predicts"的包中寻找路径"/ex/bradypus.csv"，
## 但这里使用的是绝对路径，实际上system.file主要用于查找包内的文件，此用法可能不符合预期
fsp <- system.file("/ex/bradypus.csv", package="predicts")
## 使用read.csv函数读取bradypus数据文件
bradypus <- read.csv(fsp)
## 去除数据集的第一列，假设第一列是不需要的索引或ID列
bradypus <- bradypus[,-1]
## 使用extract函数（假设来源于某个地理空间数据处理包）提取与bradypus数据中的位置相对应的环境变量值
## 这需要一个名为predictors的先前定义的环境变量栅格数据集
presvals <- extract(predictors, bradypus)
## 设置随机种子以确保后续操作的可重复性
set.seed(0)
## 使用spatSample函数（假设来源于地理空间数据处理包）从predictors栅格数据集中随机抽取500个背景（非存在）点
## xy=TRUE表明返回这些点的空间坐标，values=FALSE意味着不提取它们的环境变量值
backgr <- spatSample(predictors, 500, na.rm=TRUE, xy=TRUE, values=FALSE)
## 获取bradypus数据的行数
nr <- nrow(bradypus)
## 再次设置随机种子以进行随机抽样
s <- sample(nr, 0.25 * nr)
## 根据抽样结果，将bradypus数据分割为训练集和测试集，测试集约占原数据的25%
pres_train <- bradypus[-s, ]
pres_test <- bradypus[s, ]
## 获取背景点数据的行数
nr <- nrow(backgr)
## 改变随机种子，以便于进行另一次随机抽样
set.seed(9)
## 从背景点数据中抽样约25%作为测试集
s <- sample(nr, 0.25 * nr)
back_train <- backgr[-s, ]
back_test <- backgr[s, ]


## install.packages("dismo")
## 载入dismo包
library(dismo)
## 使用ssb函数从dismo包中，根据测试集的存在数据和背景数据，以及训练集的存在数据，生成物种分布建模的样本平衡比
sb <- dismo::ssb(pres_test, back_test, pres_train)
## 计算样本平衡比的存在样本与背景样本的比率
sb[,1] / sb[,2]


## 使用pwd_sample函数进行基于概率加权距离(PWD)的样本选择，以优化测试集
i <- pwd_sample(pres_test, back_test, pres_train, n=1, tr=0.1)
## 从原始测试集中排除在PWD样本选择中被标记为NA的存在数据
pres_test_pwd <- pres_test[!is.na(i[,1]), ]
## 从背景测试集中选择不含NA值的数据
back_test_pwd <- back_test[na.omit(as.vector(i)), ]
## 重新计算使用PWD优化后的测试集的样本平衡比
sb2 <- dismo::ssb(pres_test_pwd, back_test_pwd, pres_train)
## 计算优化后的样本平衡比的存在样本与背景样本的比率
sb2[1]/ sb2[2]


## 使用envelope函数根据训练集创建生态适宜性模型
bc <- envelope(predictors, pres_train)
## 对测试集中的存在数据和背景数据进行预测
ptst <- predict(bc, extract(predictors, pres_test))
btst <- predict(bc, extract(predictors, back_test))
## 使用pa_evaluate函数评估模型的统计性能
pa_evaluate(ptst, btst)@stats
## 对经过PWD优化后的测试集进行预测
pwdptst <- predict(bc, extract(predictors, pres_test_pwd))
pwdbtst <- predict(bc, extract(predictors, back_test_pwd))
## 评估PWD优化后的测试集的模型性能
pa_evaluate(pwdptst, pwdbtst)@stats





