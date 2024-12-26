## Install the package from the github repo: ----------------------------------
devtools::install_github('HongfanChen/PertGD')
## Load the package: ----------------------------------------------------------
library(PertGD)
## Look for document: ---------------------------------------------------------
?train_gd()
?PertGD
## Objective function: --------------------------------------------------------
obj_f = function(x){
  y = 0.5 * x[1]^2 + 0.5 * sin(3 * x[1]) + x[2]^2
  return(y)
}

## Gradient function: ---------------------------------------------------------
gd = function(x){
  gd_vec = c(x[1] + 3 / 2 * cos(3 * x[1]), 2 * x[2])
  return(gd_vec)
}
## plot the initial trace: ----------------------------------------------------
getTrace = function(results, title = "Contour"){
  range_x = 2
  range_y = 2
  x = seq(-range_x, range_x, length.out = 100)
  y = seq(-range_y, range_y, length.out = 100)
  z = outer(0.5 * x^2 + 0.5 * sin(3*x), y^2, `+`)
  view = contour(x, y, z, col = 'black', nlevels = 40, drawlabel = FALSE,
                 main = title)
  points(results[,1], results[,2], pch = 20)
  lines(results[,1], results[,2])
}
## add trace to existing plot: ------------------------------------------------
addTrace = function(results, color = "red"){
  points(results[,1], results[,2], pch = 20, col = color)
  lines(results[,1], results[,2], col = color)
}
## start from c(0.6806,0): ----------------------------------------------------
params1 = list(x = c(0.6806,0), eta = 0.1, theta = 0.5, epsilon = 1e-2,
               radius = 1e-3, v = c(0,0), gamma = 1e-2, s = 0.1, count = 0,
               t = 10, zeta = 0, z = c(1,1), x_0 = c(1,1), iter = 0, t_sub = 5,
               eta_sub = 0.05)
res_gd = train_gd(vanilla_gd, params1, gd, obj_f, 20)
res_agd = train_gd(AGD, params1, gd, obj_f, 20)
res_pagd = train_gd(PAGD, params1, gd, obj_f, 20)
res_fpgd = train_gd(FPGD, params1, gd, obj_f, 20)
res_fpagd = train_gd(FPAGD, params1, gd, obj_f, 20)
## plot results: --------------------------------------------------------------
getTrace(res_gd)
addTrace(res_agd, color = "red")
addTrace(res_pagd, color = "blue")
addTrace(res_fpgd, color = "green")
addTrace(res_fpagd, color = "orange")
legend(1, 2, legend = c("GD", "AGD", "PAGD", "FPGD", "FPAGD"),
       col = c("black", "red", "blue", "green", "orange"),
       pch = 19)
