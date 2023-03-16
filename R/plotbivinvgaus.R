


#' Density Contour Plot for Bivariate Inverse Gaussian Distribution
#'
#'
#' @param x vector defining range of non negative variable x
#' @param y vector defining range of non negative variable y
#' @param u1 mean value of variable x
#' @param u2 mean value of variable y
#' @param l1 shape parameter of variable x
#' @param l2 shape parameter of variable y
#' @param r correlation coefficient of variable X and Y
#' @param v correlation coefficient of bivariate normal distribution (Z1, Z2)
#'
#'
#' @return Density contour plot for bivariate inverse Gaussian distribution
#'
#' @import plotly
#'
#' @examples
#' x=seq(1,10,0.2)
#' y=seq(1,10,0.2)
#' v=0.3
#' r=0.5
#' l1=4
#' l2=4
#' u1=3
#' u2=3
#' PlotBivInvGaus(x,y,u1,u2,l1,l2,r,v)
#'
#' @references Continuous Bivariate Distributions Second Edition by N. Balakrishnan, Chin-Diew Lai
#'
#' @export PlotBivInvGaus

PlotBivInvGaus <- function(x,y,u1,u2,l1,l2,r,v)
{
  A= (1/4*3.1415)*sqrt((l1*l2)/((x^3)*(y^3)*(1-v^2)))
  B=(-1/(2*(1-r^2)))
  C=(l1/u1^2)*(((x-u1)^2)/x)
  D=((2*v)/(u1*u2))*(sqrt((l1*l2)/(x*y)))*(x-u1)*(y-u2)
  F=(l2/(u2^2))*(((y-u2)^2)/y)
  h=A*(exp(B*(C-D+F))+exp(B*(C+D+F)))
  fig= plot_ly(x=x, y=y, z=h , type = "contour", contours = list(showlabels = TRUE))
  fig= fig %>% colorbar(title = "Density")
  return(fig)
}
