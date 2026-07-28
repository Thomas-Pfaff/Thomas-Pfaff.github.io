##################################################################
###  Code for Figure 20.2
###  By Thomas J. Pfaff
###  For The Text Applied Calculus with R
##################################################################


install.packages("rgl")
library(rgl)
open3d()
cylinder <- function(r, h) {pi*r^2*h}
x <- y <- seq(0, 4, length = 100)
z <- outer(x, y, cylinder)
persp3d(x, y, z, col = "blue", zlim = c(0, max(z)), xlab = "r-radius",
        ylab = "h-height", zlab = "V-volume", front = "lines", back = "lines")
  rgl.spheres(0, 0, 0, r = 4, color = "yellow") 
  rgl.spheres(2, 3, cylinder(2,3), r = 4, color = "red") 
  abclines3d(2, 3, 12*pi, a = 1, b = 0, c = 12*pi, color = "red", lwd = 3)
  abclines3d(2, 3, 12*pi, a = 0, b = 1, c = 4*pi, color = "red", lwd = 3)
filename <- paste("surface3", formatC(1, digits = 1, flag = "0"), ".png", sep = "")
snapshot3d(filename)

