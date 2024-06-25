

# 安装comspat包
# devtools::install_github("jamestsakalos/comspat",build_vignettes = TRUE)


##### 1 探索模拟数据集和真实数据集 #############################################

# 让我们先看看示例数据集的前几行和结构
# 查看网格数据集
library(comspat)  # 加载comspat包
head(grid_patchy_associated)  # 查看grid_patchy_associated数据集的前几行
str(grid_patchy_associated)   # 查看grid_patchy_associated数据集的结构
# 查看样带数据集
head(tran_grass_s)  # 查看tran_grass_s数据集的前几行          
str(tran_grass_s)   # 查看tran_grass_s数据集的结构





# 查看物种的空间分布

# 存储用户默认的参数设置,以便在退出时重置
oldpar <- par(no.readonly = TRUE)
on.exit(par(oldpar))

# 设置绘图参数
par(mar = c(0.5, 0.5, 0.5, 0.5), pty = "s", ps = 12, mfrow = c(1, 3))  # 设置图形的边距、类型、字体大小和布局
colvec <- sample(c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
                   "#e6ab02"), 6)  # 随机选择6种颜色

# 绘制patchy associated数据框的散点图
plot(y = grid_patchy_associated$Y, x = grid_patchy_associated$X,
     xlab = "", ylab = "", xaxt = "none", yaxt = "none", cex = 0)  # 创建一个空白图,不显示坐标轴和标签
points(y = grid_patchy_associated$Y, x = grid_patchy_associated$X,
       pch = c(22, 17, 18, 2, 20, 18)[grid_patchy_associated$Species],  # 根据物种设置点的形状
       col = colvec[grid_patchy_associated$Species],  # 根据物种设置点的颜色
       bg = c("yellow2")[grid_patchy_associated[grid_patchy_associated[, 3] ==
                                                  "Sp1", "Species"]],  # 将Sp1物种的点背景设为黄色
       cex = c(0.8, 0.8, 0.8, 0.6, 0.8, 1)[grid_patchy_associated$Species],  # 根据物种设置点的大小
       lwd = 2)  # 设置点的边框宽度
box(which = "plot", lwd = 2)  # 为图形添加边框
legend("topleft", as.expression(bquote(bold("A"))),
       inset = c(-0.3, -0.12), bty = "n", cex = 2)  # 在图的左上角添加一个加粗的"A"标签

# 绘制patchy random数据框的散点图
plot(y = grid_patchy_n_isc$Y, x = grid_patchy_n_isc$X, xlab = "", ylab = "",
     xaxt = "none", yaxt = "none", cex = 0)
points(y = grid_patchy_n_isc$Y, x = grid_patchy_n_isc$X,
       pch = c(22, 17, 18, 2, 20, 18)[grid_patchy_n_isc$Species],
       col = colvec[grid_patchy_n_isc$Species],
       bg = c("yellow2")[grid_patchy_n_isc[grid_patchy_n_isc[, 3]
                                           == "Sp1", "Species"]],
       cex = c(0.8, 0.8, 0.8, 0.6, 0.8, 1)[grid_patchy_n_isc$Species],
       lwd = 2)
box(which = "plot", lwd = 2)
legend("topleft", as.expression(bquote(bold("C"))),
       inset = c(-0.3, -0.12), bty = "n", cex = 2)  # 添加"C"标签

# 绘制mixed random数据框的散点图
plot(y = grid_random$Y, x = grid_random$X, xlab = "", ylab = "",
     xaxt = "none", yaxt = "none", cex = 0)
points(y = grid_random$Y, x = grid_random$X,
       pch = c(22, 17, 18, 2, 20, 18)[grid_random$Species],
       col = colvec[grid_random$Species],
       bg = c("yellow2")[grid_random[grid_random[, 3] == "Sp1", "Species"]],
       cex = c(0.8, 0.8, 0.8, 0.6, 0.8, 1)[grid_random$Species],
       lwd = 2)
box(which = "plot", lwd = 2)
legend("topleft", as.expression(bquote(bold("B"))),
       inset = c(-0.3, -0.12), bty = "n", cex = 2)  # 添加"B"标签






# 查看物种出现频率

# 设置绘图参数
par(mar = c(4, 5, 2, 1), cex = 0.8, mfrow = c(1, 3))

# 定义颜色向量,用于绘制不同物种的柱状图
colvec <- c("lightgoldenrod", "sienna1", "mediumspringgreen", "forestgreen", "red4", "blue")

# 绘制斑块关联数据框的物种频率柱状图
barplot(table(grid_patchy_associated$Species), col = colvec, las = 2,
        ylim = c(0, 800))
# table(): 统计grid_patchy_associated数据框中物种出现的频率
# col: 设置柱状图的填充颜色
# las: 设置坐标轴标签的方向,2表示垂直
# ylim: 设置y轴的范围

mtext("Species frequency", side = 2, line = 3.5, cex = 0.8, font = 2)
# 在图形左侧添加y轴标签"Species frequency"
# side: 指定标签的位置,2表示在左侧
# line: 指定标签与图形边缘的距离
# cex: 设置标签字体大小
# font: 设置标签字体样式,2表示粗体

box(which = "plot", lwd = 2)
# 在图形周围添加边框
# which: 指定在绘图区域周围添加边框
# lwd: 设置边框线宽

# 绘制斑块随机数据框的物种频率柱状图,参数设置与上面类似
barplot(table(grid_patchy_n_isc$Species), col = colvec, las = 2,
        ylim = c(0, 800))
mtext("Species frequency", side = 2, line = 3.5, cex = 0.8, font = 2)
box(which = "plot", lwd = 2)

# 绘制混合随机数据框的物种频率柱状图,参数设置与上面类似
barplot(table(grid_random$Species), col = colvec, las = 2, ylim = c(0, 800))
mtext("Species frequency", side = 2, line = 3.5, cex = 0.8, font = 2)
box(which = "plot", lwd = 2)






# 绘制物种组合的分布情况

# 获取主采样单元尺度下的物种频率
a <- freq_comb(grid_patchy_associated, type = "Grid", dim_max = 64)
b <- freq_comb(grid_patchy_n_isc, type = "Grid", dim_max = 64)
c <- freq_comb(grid_random, type = "Grid", dim_max = 64)
# freq_comb(): 计算物种组合的频率
# type: 指定数据类型为网格数据
# dim_max: 指定最大维度为64

# 现在我们需要准备数据用于简单的柱状图
# 为数据集分配名称
a$dat <- "patchy associated"
b$dat <- "patchy no isc"
c$dat <- "random"

# 将数据合并在一起
a <- rbind(a, b, c)

# 重塑数据
data_base <- reshape(a, idvar = "dat", timevar = "combinations", direction = "wide")
# reshape(): 重塑数据结构
# idvar: 指定不需要重塑的变量
# timevar: 指定需要重塑的变量
# direction: 指定重塑的方向,wide表示宽格式

data_base[is.na(data_base)] <- 0
# 将缺失值替换为0

row.names(data_base) <- data_base$dat
data_base$dat <- NULL
data_base <- as.matrix(data_base)
# 将数据框转换为矩阵

colnames(data_base) <- gsub(pattern = "Freq.", replacement = "", colnames(data_base))
# 修改列名,去掉"Freq."前缀

# 创建柱状图(注意我们使用对数变换以更好地查看不同的组合)
par(mar = c(8, 4, 1, 1))
barplot(height = log1p(data_base), beside = TRUE, cex.names = 0.7,las = 2, col = c("red", "blue", "green"))
legend("topright", legend = row.names(data_base),
       col = c("red", "blue", "green"), pch = c(15, 15, 15), pt.cex = 1.5)
axis(side = 1, line = 4, at = 15, expression(italic("grain size = 0.04 sq.m*")),tick = FALSE, cex.axis = 0.75)
mtext("Species combinations", 1, 6.5, font = 2)
mtext("Freqency (log %)", 2, 2.5, font = 2)
box(lwd = 2)





# 查看物种的空间分布

# 设置绘图参数
par(mar = c(8, 3, 5, 1), mfrow = c(1, 2), cex = 0.8)

# 绘制样带植物功能型数据框的图
plot(as.numeric(tran_grass_s$Species) ~ as.numeric(tran_grass_s$X),
     xaxt = 'n', yaxt = 'n', ann = FALSE, xlim = c(0,500),
     pch = c(3, 4, 15:22)[tran_grass_s$Species],
     col=c("red","blue","green","pink","orange","black",
           "red","blue","green","pink")[tran_grass_s$Species])
# 设置x轴刻度和标签
axis(1, at = c(0, 500), labels = c(0, 500))
# 添加y轴标签
mtext("Species", side = 2, line = 1.5, font = 2, cex = 0.8)
# 添加x轴标签
mtext("Plant presences in 5x5cm units", side = 1, line = 1, font = 2, cex = 0.8)
# 添加图形边框
box(which = "plot", lwd = 2)

# 添加图例
legend(x = 20, y = -0.3, legend = c(levels(tran_grass_s$Species)),
       pch = c(3, 4, 15:22),
       col = c("red","blue","green","pink","orange","black",
               "red","blue","green","pink"),
       ncol = 2, cex = 0.8,
       xpd = TRUE)

# 绘制样带植物功能型数据框的图
plot(as.numeric(tran_grass_t$Species) ~ as.numeric(tran_grass_t$X),
     xaxt = 'n', yaxt = 'n', ann = FALSE, xlim = c(0,500),
     pch = c(3, 4, 15:18)[tran_grass_t$Species],
     col = c("red", "blue", "green", "pink", "orange", "black")[tran_grass_t$Species])
# 设置x轴刻度和标签  
axis(1, at = c(0, 500), labels = c(0, 500))
# 添加y轴标签
mtext("Plant functional types", side = 2, line = 1.5, font = 2, cex = 0.8)
# 添加x轴标签
mtext("PFT presences in 5x5cm units", side = 1, line = 1, font = 2, cex = 0.8)
# 添加图形边框
box(which = "plot", lwd = 2)

# 添加图例
legend(x = 50, y = 0.2, legend = c(levels(tran_grass_t$Species)),
       pch = c(3, 4, 15:18),
       col = c("red", "blue", "green", "pink", "orange", "black"),
       ncol = 2, cex = 0.8,
       xpd = TRUE)




# 查看物种出现频率

# 设置图形参数
par(mar = c(7.5, 5, 1, 1), mfrow = c(1, 2), cex = 0.8, cex.axis = 0.8)

# 这部分绘制样带物种数据框的图
barplot(table(tran_grass_s$Species), las = 2, ylim = c(0, 200)) 
mtext("Species frequency", side = 2, line = 3.5, font = 2)
box(which = "plot", lwd = 2)

# 这部分绘制斑块相关数据框的图
barplot(table(tran_grass_t$Species), las = 2, ylim = c(0, 400))
mtext("PFT frequency", side = 2, line = 3.5, font = 2) 
box(which = "plot", lwd = 2)





##### 2 准备参数数据集 #########################################################

# 加载并可视化参数数据集
data("param_grid")

# 我们模拟数据的采样单元大小为5 cm x 5 cm
su_area <- param_grid[,2] * param_grid[,3] * (0.05 * 0.05)

# 绘制面积图
par(mar = c(4.5, 4.5, 0.5, 0.5), ps = 12)  # 设置图形的边距和字体大小
plot(su_area ~ c(1:16), type = 'b', pch = 19, las = 2,  # 绘制采样单元面积与缩放步骤的关系图
     ylab = "Sample Unit Area",  # y轴标签
     xlab = "Steps of Scaling")  # x轴标签
mtext(text = expression('(cm'^2*')'),  # 在y轴标签旁添加单位(平方厘米)
      side = 2, line = 1.9, cex = 0.6)  # 设置单位的位置和大小




##### 5 不含零模型的comspat ####################################################

# 为网格数据集计算NRC和CD
g_r <- comspat(data = grid_random, params = param_grid,dim_max = 64, type = "Grid")
g_pr <- comspat(grid_patchy_n_isc, param_grid, 64, "Grid")
g_pa <- comspat(grid_patchy_associated, param_grid, 64, "Grid")

# comspat_plot的用法
# 让我们在一个图上可视化所有网格数据集的CD
data <- list(g_r, g_pr, g_pa)
p_col <- list("red", "blue", "green")
par(mar = c(3.5, 3, 0.5, 0.5), mfrow = c(1, 2))  # 设置图形边距和布局

# 首先添加组成多样性(即CD)
comspat_plot(data = data, params = param_grid, type = "Grid", measure = "CD",
             su_size = 0.01, ymin = 0, ymax = 5.5, xmax = 1, p_col = p_col)

# 尝试将measure参数更改为"NRC"
comspat_plot(data = data, params = param_grid, type = "Grid", measure = "NRC", 
             su_size = 0.01, ymin = 0, ymax = 65, xmax = 1, p_col = p_col)




##### 6 包含零模型的comspat ####################################################


##### 案例研究1:模拟网格数据集 #####

# 计算NRC和CD
# 注意,运行999次迭代可能需要相当长的时间

# 对随机网格数据进行CSR随机化999次迭代的共空间分析
rand_csr <- comspat(data = grid_random, params = param_grid, dim_max = 64,
                    type = "Grid", randomization_type = "CSR", iterations = 999)
# 对随机网格数据进行RS随机化999次迭代的共空间分析                    
rand_rs <- comspat(data = grid_random, params = param_grid, dim_max = 64,
                   type="Grid", randomization_type = "RS", iterations = 999)

# 对斑块状非独立同分布网格数据进行CSR随机化999次迭代的共空间分析
patch_csr <- comspat(data = grid_patchy_n_isc, params = param_grid,
                     dim_max = 64, type = "Grid", randomization_type = "CSR",
                     iterations = 999)
# 对斑块状非独立同分布网格数据进行RS随机化999次迭代的共空间分析                     
patch_rs <- comspat(data = grid_patchy_n_isc, params = param_grid,
                    dim_max = 64, type = "Grid", randomization_type = "RS",
                    iterations = 999)

# 对斑块状关联网格数据进行CSR随机化999次迭代的共空间分析
assoc_csr <- comspat(data = grid_patchy_associated, params = param_grid,
                     dim_max = 64, type = "Grid", randomization_type = "CSR",
                     iterations = 999)
# 对斑块状关联网格数据进行RS随机化999次迭代的共空间分析                     
assoc_rs <- comspat(data = grid_patchy_associated, params = param_grid,
                    dim_max = 64, type = "Grid", randomization_type = "RS",
                    iterations = 999)

# 设置绘图参数
# 随机选择6种颜色
colA <- sample(
  c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02"),6)
# 设置绘图布局为3行3列,边距等参数  
par(mfrow = c(3, 3), mar = c(0, 0, 0, 0), oma = c(5, 5, 0.5, 0.5),
    mgp = c(2, 1, 0), tcl = -0.5)





# Part 1:空间分布

# 绘制斑块状关联数据的空间分布
plot(y = grid_patchy_associated$Y, x = grid_patchy_associated$X,
     xlab = "", ylab = "", xaxt = "none", yaxt = "none", cex = 0)
points(y = grid_patchy_associated$Y, x = grid_patchy_associated$X,
       pch = c(22, 17, 18, 2, 20, 18)[grid_patchy_associated$Species],
       col = colA[grid_patchy_associated$Species],
       bg = c("yellow2")[grid_patchy_associated[grid_patchy_associated[, 3]
                                                == "Sp1", "Species"]],
       cex = c(0.8, 0.8, 0.8, 0.6, 0.8, 1)[grid_patchy_associated$Species],
       lwd = 2)
text(x = 3,y = 62, "A", font = 2, cex = 1)

# 绘制斑块状非独立同分布数据的空间分布
plot(y = grid_patchy_n_isc$Y, x = grid_patchy_n_isc$X,
     xlab = "", ylab = "", xaxt = "none", yaxt = "none", cex = 0)
points(y = grid_patchy_n_isc$Y, x = grid_patchy_n_isc$X,
       pch = c(22, 17, 18, 2, 20, 18)[grid_patchy_n_isc$Species],
       col = colA[grid_patchy_n_isc$Species],
       bg = c("yellow2")[grid_patchy_n_isc[grid_patchy_n_isc[, 3]
                                           == "Sp1", "Species"]],
       cex = c(0.8, 0.8, 0.8, 0.6, 0.8, 1)[grid_patchy_n_isc$Species],
       lwd = 2)
text(x = 3, y = 62,"B", font = 2, cex = 1)

# 绘制随机数据的空间分布
plot(y = grid_random$Y, x = grid_random$X,
     xlab = "", ylab = "", xaxt = "none", yaxt = "none", cex = 0)
points(y = grid_random$Y, x = grid_random$X,
       pch = c(22, 17, 18, 2, 20, 18)[grid_random$Species],
       col = colA[grid_random$Species],
       bg = c("yellow2")[grid_random[grid_random[, 3] == "Sp1", "Species"]],
       cex = c(0.8, 0.8, 0.8, 0.6, 0.8, 1)[grid_random$Species],
       lwd = 2)
text(x = 3, y = 62, "C", font = 2, cex = 1)





# Part 2: the NRC plots with 95% CI

p_col = list("blue","red")

# 绘制混合随机数据框
data <- list("CSR" = assoc_csr[[2]], "RS" = assoc_rs[[2]])

comspat_plot(data = data, params = param_grid, type = "Grid", measure = "NRC",
             su_size = 0.01, ymin = 0, ymax = 65, xmin = 0.01, xmax = 10,
             xaxt = FALSE, yaxt = TRUE, p_col = p_col, stats_output = TRUE)
mtext("NRC", side = 2, line = 3, font = 2)

axis(1, at = round(c(0.01, seq(0.02, 0.09, 0.01), 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = 0.015)
axis(1, at = round(c(0.01, 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = 0.025)
text(x = 0.0125, y = 63,"D", font = 2, cex = 1)

# 绘制斑块随机数据框
data <- list("CSR" = patch_csr[[2]],"Random Shift" = patch_rs[[2]])

comspat_plot(data = data, params = param_grid, type = "Grid", measure = "NRC",
             su_size = 0.01, ymin = 0, ymax = 65, xmin = 0.01, xmax = 10,
             xaxt = FALSE, yaxt = FALSE, p_col = p_col, stats_output = TRUE)

axis(1, at = round(c(0.01, seq(0.02, 0.09, 0.01), 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = 0.015)
axis(1, at = round(c(0.01, 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = 0.025)
axis(2, at = round(seq(0, 60, 10)),
     labels = FALSE, cex.axis = 0.75, tck = 0.015)
text(x = 0.0125, y = 63,"E", font = 2, cex = 1)

# 绘制斑块关联数据框
data <- list("CSR" = rand_csr[[2]], "Random Shift" = rand_rs[[2]])

comspat_plot(data = data, params = param_grid, type = "Grid", measure = "NRC",
             su_size = 0.01, ymin = 0, ymax = 65, xmin = 0.01, xmax = 10,
             xaxt = FALSE, yaxt = FALSE, p_col = p_col, stats_output = TRUE)

axis(1, at = round(c(0.01, seq(0.02, 0.09, 0.01), 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = 0.015)
axis(1, at = round(c(0.01, 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = 0.025)
axis(2, at = round(seq(0, 60, 10)),
     labels = FALSE, cex.axis = 0.75, tck = 0.015)
text(x = 0.0125,y = 63,"F", font = 2, cex = 1)





# Part 3: the CD plots with 95% CI

# 绘制混合随机数据框
data <- list("CSR" = assoc_csr[[2]], "Random Shift" = assoc_rs[[2]])

comspat_plot(data = data, params = param_grid, type = "Grid", measure = "CD",
             su_size = 0.01, ymin = 0, ymax = 5.5, xmin = 0.01, xmax = 10,
             xaxt = FALSE, yaxt = TRUE, p_col = p_col, stats_output = TRUE)
mtext("CD", side = 2, line = 3, font = 2)
axis(1, at = round(c(0.01, seq(0.02, 0.09, 0.01), 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = -0.015)
axis(1, at = round(c(0.01, 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = -0.025)
text(x = 0.0125, y = 5.3, "G", font = 2, cex = 1)

# 绘制斑块随机数据框
data<-list("CSR" = patch_csr[[2]], "Random Shift" = patch_rs[[2]])

comspat_plot(data = data, params = param_grid, type = "Grid", measure = "CD",
             su_size = 0.01, ymin = 0, ymax = 5.5, xmin = 0.01, xmax = 10,
             xaxt = FALSE, yaxt = FALSE, p_col = p_col, stats_output = TRUE)

mtext("Length of sampling units (m)", side = 1, line = 3, font =2)
axis(1, at = round(c(0.01, seq(0.02, 0.09, 0.01), 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = -0.015)
axis(1, at = round(c(0.01, 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = -0.025)
axis(2, at = round(seq(0, 5, 1)), labels = FALSE, cex.axis = 0.75, tck = 0.015)
text(x = 0.0125, y = 5.3,"H", font = 2, cex = 1)

# 绘制斑块关联数据框
data <- list("CSR" = rand_csr[[2]],"Random Shift" = rand_rs[[2]])
comspat_plot(data = data, params = param_grid, type = "Grid", measure = "CD",
             su_size = 0.01, ymin = 0, ymax = 5.5, xmin = 0.01, xmax = 10,
             xaxt = FALSE, yaxt = FALSE, p_col = p_col, stats_output = TRUE)
axis(1, at = round(c(0.01, seq(0.02, 0.09, 0.01), 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = -0.015)
axis(1, at = round(c(0.01, 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = -0.025)
axis(2, at = round(seq(0, 5, 1)), labels = FALSE, cex.axis = 0.75, tck = 0.015)
text(x = 0.0125, y = 5.3, "I", font = 2, cex = 1)






knitr::kable(assoc_csr$`Summary statistics`[["CD"]][,1:10],format = "pipe", table.attr = "style='width:100%;'")




##### 案例研究2：真实样带数据 #####

# 计算 NRC 和 CD
# 请注意，运行 999 次迭代会耗费大量时间

s_csr <- comspat(data = tran_grass_s, params = param_tran, dim_max = 500,
                 type = "Transect", randomization_type = "CSR",
                 iterations = 999)
s_rs <- comspat(data = tran_grass_s, params = param_tran, dim_max = 500,
                type = "Transect", randomization_type = "RS",
                iterations = 999)
t_csr <- comspat(data = tran_grass_t, params = param_tran, dim_max = 500,
                 type = "Transect", randomization_type = "CSR",
                 iterations = 999)
t_rs <- comspat(data = tran_grass_t, params = param_tran, dim_max = 500,
                type = "Transect", randomization_type = "RS",
                iterations = 999)

# 设置绘图参数
par(mfrow = c(2, 2), mar = c(0.5, 2.2, 0, 0), oma = c(7, 3, 0.5, 0.5),
    mgp = c(2, 1, 0), tcl = -0.5)

p_col = list("blue","red")






# Part 1: the NRC plots with 95% CI

# 绘制样带物种数据框
data <- list("CSR" = s_csr[[2]], "RS" = s_rs[[2]])
comspat_plot(data = data, params = param_grid, type = "Transect", 
             measure = "NRC", su_size = 0.01, p_col = p_col,
             ymin = 0, ymax = 200, xmin = 0.01, xmax = 10,
             xaxt = FALSE, yaxt = TRUE, stats_output = TRUE)

mtext("NRC", side = 2, line = 3, font = 2)
axis(1, at = round(c(0.01, seq(0.02, 0.09, 0.01), 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = 0.015)
axis(1, at = round(c(0.01, 0.1, 1, 10, 100), 2), labels = FALSE,
     cex.axis = 0.75, tck = 0.025)
text(x = 0.0125, y = 197,"A", font = 2, cex = 1)

# 绘制样带 PFT 数据框
data <- list("CSR" = t_csr[[2]], "RS" = t_rs[[2]])

comspat_plot(data = data, params = param_grid, type = "Transect", 
             measure = "NRC", su_size = 0.01, p_col = p_col,
             ymin = 0, ymax = 45, xmin = 0.01, xmax = 10,
             xaxt = FALSE, yaxt = TRUE, stats_output = TRUE)

axis(1, at = round(c(0.01, seq(0.02, 0.09, 0.01), 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = 0.015)
axis(1, at = round(c(0.01, 0.1, 1, 10, 100), 2), labels = FALSE,
     cex.axis = 0.75, tck = 0.025)
text(x=0.0125,y=44,"B", font = 2, cex = 1)






# Part 2: the CD plots with 95% CI

# 绘制样带物种数据框
data<-list("CSR"=s_csr[[2]],"Random Shift"=s_rs[[2]])

comspat_plot(data = data, params = param_grid, type = "Transect", 
             measure = "CD", su_size = 0.01, p_col = p_col,
             ymin = 0, ymax = 8, xmin = 0.01, xmax = 10,
             xaxt = TRUE, yaxt = TRUE, stats_output = TRUE)

mtext("CD (bits)", side = 2, line = 3, font = 2)
axis(1, at = round(c(0.01, seq(0.02, 0.09, 0.01), 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = -0.015)
axis(1, at = round(c(0.01, 0.1, 1, 10, 100), 2), labels = FALSE,
     cex.axis = 0.75, tck = -0.025)
mtext("Length of sampling units (m)", side =1, line = 4, font =2)
text(x = 0.0125, y = 7.8,"C", font = 2, cex = 1)

# 绘制样带 PFT 数据框
data <- list("CSR" = t_csr[[2]], "Random Shift" = t_rs[[2]])

comspat_plot(data = data, params = param_grid, type = "Transect", 
             measure = "CD", su_size = 0.01, p_col = p_col,
             ymin = 0, ymax = 5, xmin = 0.01, xmax = 10,
             xaxt = TRUE, yaxt = TRUE, stats_output = TRUE)

axis(1, at = round(c(0.01, seq(0.02, 0.09, 0.01), 0.1, 1, 10, 100), 2),
     labels = FALSE, cex.axis = 0.75, tck = -0.015)
axis(1, at = c(0.01, 0.1, 1, 10, 100), labels = TRUE,
     cex.axis = 0.75, tck = -0.025)

text(x = 0.0125, y = 5,"D", font = 2, cex = 1)

legend(x = 0.013, y = -1.5, legend = c("Observed", "CSR 95% CI", "RS 95% CI"),
       lty = c(1, 1, 1),
       col = c("black", "blue", "red"),
       ncol = 2, cex = 0.8,
       xpd = NA)






