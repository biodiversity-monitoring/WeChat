
# 1 安装

# install.packages("divraster")

# devtools::install_github("flaviomoc/divraster")


library(divraster)

################################################################################

# 2 alaph计算

# 2.1.1 alaph TD
# Loading data
# Presence-absence SpatRaster
bin1 <- terra::rast(system.file("extdata", 
                                "ref_frugivor.tif", 
                                package = "divraster"))
bin2 <- terra::rast(system.file("extdata",
                                "fut_frugivor.tif",
                                package = "divraster"))

# Change extension to process faster
terra::ext(bin1)
#> SpatExtent : -41.875, -38.75, -21.375, -13 (xmin, xmax, ymin, ymax)
e <- c(-41, -39, -15, -13)
bin1 <- terra::crop(bin1, e)
bin2 <- terra::crop(bin2, e)

# Species traits
traits <- read.csv(system.file("extdata", 
                               "traits_frugivor.csv", 
                               package = "divraster"), 
                   sep = ";", 
                   row.names = 1)

# Phylogenetic tree
tree <- ape::read.tree(system.file("extdata", 
                                   "tree_frugivor.tre", 
                                   package = "divraster"))

# Alpha TD calculation for scenario 1
alpha.td <- divraster::spat.alpha(bin1)
alpha.td
#> class       : SpatRaster 
#> dimensions  : 16, 16, 1  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : Alpha_TD 
#> min value   :        0 
#> max value   :       67
terra::plot(alpha.td, main = paste0(names(alpha.td), "_sce1"))





# Alpha TD calculation for scenario 2
alpha.td2 <- divraster::spat.alpha(bin2)
alpha.td2
#> class       : SpatRaster 
#> dimensions  : 16, 16, 1  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : Alpha_TD 
#> min value   :        0 
#> max value   :       65
terra::plot(alpha.td2, main = paste0(names(alpha.td2), "_sce2"))





# Difference in Alpha TD between scenarios
alpha.td2-alpha.td
#> class       : SpatRaster 
#> dimensions  : 16, 16, 1  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : Alpha_TD 
#> min value   :      -18 
#> max value   :        0
terra::plot(alpha.td2-alpha.td, main = "Delta Alpha TD")



# 2.1.2 Alpha FD

alpha.fd <- divraster::spat.alpha(bin1, traits)
alpha.fd
#> class       : SpatRaster 
#> dimensions  : 16, 16, 1  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : Alpha_FD 
#> min value   : 3.723151 
#> max value   : 4.378846
terra::plot(alpha.fd, main = names(alpha.fd))



# 2.1.3 Alpha PD

# Alpha PD calculation
alpha.pd <- divraster::spat.alpha(bin1, tree)
alpha.pd
#> class       : SpatRaster 
#> dimensions  : 16, 16, 1  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : Alpha_PD 
#> min value   : 1523.921 
#> max value   : 1859.668
terra::plot(alpha.pd, main = names(alpha.pd))



################################################################################

# 3 标准效应大小（SES）

# 3.1.1 SES FD

# SES FD calculation
ses.fd <- divraster::spat.rand(x = bin1, 
                               tree = traits, 
                               aleats = 3, 
                               random = "site")
ses.fd
#> class       : SpatRaster 
#> dimensions  : 16, 16, 4  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> names       :  Mean_FD,      SD_FD, Observed_FD,    SES_FD 
#> min values  : 3.516019, 0.00108655,    3.723151, -37.54013 
#> max values  : 4.423417, 0.19567515,    4.378846,  21.99738
terra::plot(ses.fd, main = names(ses.fd))


# 3.1.2 SES PD
# SES PD calculation
ses.pd <- divraster::spat.rand(x = bin1, 
                               tree = tree, 
                               aleats = 3, 
                               random = "site")
ses.pd
#> class       : SpatRaster 
#> dimensions  : 16, 16, 4  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> names       :  Mean_PD,      SD_PD, Observed_PD,    SES_PD 
#> min values  : 1631.433,  0.8527218,    1623.451, -24.01647 
#> max values  : 1877.305, 66.0914497,    1880.676,  17.16512
terra::plot(ses.pd, main = names(ses.pd))



################################################################################












# 4 beta多样性


# 4.1.1 beta空间TD

# Beta spatial TD calculation
beta.td <- divraster::spat.beta(bin1)
beta.td
#> class       : SpatRaster 
#> dimensions  : 16, 16, 4  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> names       :   Btotal_TD,   Brepl_TD,    Brich_TD, Bratio_TD 
#> min values  : 0.006060606, 0.00000000, 0.004264392, 0.0000000 
#> max values  : 0.109409606, 0.03638009, 0.103181683, 0.7259259
terra::plot(beta.td, main = names(beta.td))


# 4.1.2 beta空间FD

# Beta spatial FD calculation
beta.fd <- divraster::spat.beta(bin1, traits)
beta.fd
#> class       : SpatRaster 
#> dimensions  : 16, 16, 4  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> names       :    Btotal_FD,   Brepl_FD,     Brich_FD, Bratio_FD 
#> min values  : 0.0004245858, 0.00000000, 0.0004245858,  0.000000 
#> max values  : 0.0793045655, 0.01846139, 0.0793045655,  0.756675
terra::plot(beta.fd, main = names(beta.fd))


# 4.1.3 beta空间PD

# Beta spatial PD calculation
beta.pd <- divraster::spat.beta(bin1, tree)
beta.pd
#> class       : SpatRaster 
#> dimensions  : 16, 16, 4  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> names       :   Btotal_PD,    Brepl_PD,    Brich_PD, Bratio_PD 
#> min values  : 0.002851805, 0.000000000, 0.001618621, 0.0000000 
#> max values  : 0.059656685, 0.008026876, 0.059656685, 0.8250616
terra::plot(beta.pd, main = names(beta.pd))


# 4.1.4 beta时间TD

# Beta temporal TD calculation
betatemp.td <- divraster::temp.beta(bin1, bin2)
betatemp.td
#> class       : SpatRaster 
#> dimensions  : 16, 16, 4  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> names       :  Btotal_TD,   Brepl_TD,   Brich_TD, Bratio_TD 
#> min values  : 0.02985075, 0.00000000, 0.02985075, 0.0000000 
#> max values  : 0.31578947, 0.09836066, 0.31578947, 0.6666667
terra::plot(betatemp.td, main = names(betatemp.td))


# 4.1.5 beta时间FD

# Beta temporal FD calculation
betatemp.fd <- divraster::temp.beta(bin1, bin2, traits)
betatemp.fd
#> class       : SpatRaster 
#> dimensions  : 16, 16, 4  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> names       :  Btotal_FD,   Brepl_FD,   Brich_FD, Bratio_FD 
#> min values  : 0.01733908, 0.00000000, 0.01012696, 0.0000000 
#> max values  : 0.17031569, 0.03741327, 0.17031569, 0.6872465
terra::plot(betatemp.fd, main = names(betatemp.fd))


# 4.1.6 beta时间PD

# Beta temporal PD calculation
betatemp.pd <- divraster::temp.beta(bin1, bin2, tree)
betatemp.pd
#> class       : SpatRaster 
#> dimensions  : 16, 16, 4  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> names       : Btotal_PD,  Brepl_PD,    Brich_PD, Bratio_PD 
#> min values  : 0.0120217, 0.0000000, 0.009533166, 0.0000000 
#> max values  : 0.1792291, 0.0394096, 0.179229119, 0.7759122
terra::plot(betatemp.pd, main = names(betatemp.pd))


################################################################################

# 5 性状平均

# 5.1 性状平均示例

# Average traits calculation
# Scenario 1
avg.traits1 <- divraster::spat.trait(bin1, traits)
avg.traits1[[4]]
#> class       : SpatRaster 
#> dimensions  : 16, 16, 1  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : Beak.Depth 
#> min value   :   8.526923 
#> max value   :   9.601515
terra::plot(avg.traits1[[4]], main = paste0(names(avg.traits1[[4]]), "_sce1"))



# Scenario 2
avg.traits2 <- divraster::spat.trait(bin2, traits)
avg.traits2[[4]]
#> class       : SpatRaster 
#> dimensions  : 16, 16, 1  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : Beak.Depth 
#> min value   :   8.786792 
#> max value   :   9.942308
terra::plot(avg.traits2[[4]], main = paste0(names(avg.traits2[[4]]), "_sce2"))



# Percentage of change
change.traits <- (avg.traits2 - avg.traits1) / avg.traits1 * 100
change.traits[[4]]
#> class       : SpatRaster 
#> dimensions  : 16, 16, 1  (nrow, ncol, nlyr)
#> resolution  : 0.125, 0.125  (x, y)
#> extent      : -41, -39, -15, -13  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : Beak.Depth 
#> min value   :  -4.495893 
#> max value   :  13.541296
terra::plot(change.traits[[4]], main = paste0(names(change.traits[[4]]), "_%"))






