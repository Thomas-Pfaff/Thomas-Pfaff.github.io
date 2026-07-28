##################################################################
###  Code in Chapter 1: A Brief Introduction to R
###  By Thomas J. Pfaff
###  For The Text Applied Calculus with R
##################################################################

## R Code 1.1
2 + 2

## R Code 1.2
3^2

## R Code 1.3

f<-function(x) { 4*x^5 + 15*x^4 - 140*x^3 - 430*x^2 + 1200*x + 1000}
f(-4.2)

## R Code 1.4

curve(f, -6, 5.5, lwd = 2) 
grid(NULL, NULL, col = "black")


## R Code 1.5 and 1.6

install.packages("rootSolve")


## R Code 1.7 and 1.8

library(rootSolve)

## R Code 1.9

uniroot.all(f, c(-10, 10))

## R Code 1.10

f(-0.7)

## R Code 1.11

f_roots<-uniroot.all(f, c(-10, 10))
f_roots[1]

## R Code 1.12

f(f_roots[1])

## R Code 1.13

points(f_roots[1], f(f_roots[1]), pch = 16, col = "red", cex = 1.25)

## R Code 1.14

f(c(1, 2, 3, 4))

## R Code 1.15

points(f_roots, f(f_roots), pch = 16, col = "red", cex = 1.25)

## R Code 1.16

f_2000 <- function(x) {f(x) - 2000}
uniroot.all(f_2000, c(2, 6))

## R Code 1.17

f_2000_roots <- uniroot.all(f_2000, c(2, 6))
points(f_2000_roots, f(f_2000_roots), pch = 16, col = "blue", cex = 1.25)

## R Code 1.18

g <- function(x) {500*x^2 - 1000}
curve(g, -6, 5.5, lwd = 2, add = TRUE)

## R Code 1.19

f_g_diff <- function(x) {f(x) - g(x)}
f_g_diff_roots <- uniroot.all(f_g_diff, c(-10, 10))
points(f_g_diff_roots, f(f_g_diff_roots), pch = 17, col = "purple", cex = 1.25)

## R Code 1.20

segments(f_g_diff_roots[1],f(f_g_diff_roots[1]),
         f_g_diff_roots[2],f(f_g_diff_roots[2]),
         lty = 2, lwd = 2, col = "purple")
abline(h = 0)

