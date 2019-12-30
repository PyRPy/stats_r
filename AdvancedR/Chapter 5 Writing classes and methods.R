# S3 system ---------------------------------------------------------------
library(checkpoint)
# checkpoint()

library(ggplot2)
options(width = 80)

class(mtcars)

table

x <- table(mtcars$cyl)
class(x) <- c("newclass", "table")
class(x)

# method
d <- data.frame(
  x = c(1, 3, 5),
  y = c(1, 2, 4),
  labels = c("first", "second", "third")
)

class(d) <- "textplot"
print(d)

class(d) <- c("textlot", "data.frame")
print(d) # default as df

# define another S3 class
textplot_data <- function(f, d) {
  stopifnot(inherits(d, "data.frame"))
  stopifnot(inherits(f, "formula"))
  
  newdata <- get_all_vars(formula = f, data = d)
  colnames(newdata) <- c("y", "x", "labels")
  class(newdata) <- c("textplot", "data.frame")
  return(newdata)
}

textplot_data(f = mpg ~ hp | cyl, d = mtcars[1:10, ])

# define a method to plot linear model
plot.textplot <- function(d){
  op <- par(mar = c(4, 4, 1, 1))
  on.exit(par(op))
  
  plot.new()
  plot.window(xlim = range(d$x, na.rm = TRUE),
              ylim = range(d$y, na.rm = TRUE))
  text(d$x, d$y, labels = d$labels)
  
  axis(side = 1, range(d$x, na.rm = TRUE))
  axis(side = 2, range(d$y, na.rm = TRUE))
  
  invisible(d)
}

dat <- textplot_data(f = mpg ~ hp | cyl, d = mtcars[1:10, ])
plot(dat)
plot
methods(plot) # many classes have this method


# build a new method on ggplot to plot lm  --------------------------------

m <- lm(mpg ~ hp*vs + factor(cyl), data = mtcars)
summary(m)

class(m)
methods(ggplot)

ggplot.lm <- function(data, mapping, vars, ...){
  newdat <- do.call(expand.grid, vars)
  yvar <- as.character(formula(data)[[2]])
  d <- as.data.frame(predict(data, newdata=newdat, se.fit=TRUE))
  d <- within(d, {
    LL <- fit + qnorm(0.025)*se.fit
    UL <- fit + qnorm(0.975)*se.fit
  })
  colnames(d)[1] <- yvar
  data <- cbind(newdat, d[, c(yvar, "LL", "UL")])
  ggplot(data = data, mapping = mapping, ...)
}

# use the 'new' method on lm class
ggplot(m, aes(hp, mpg), vars = list(
  hp = min(mtcars$hp):max(mtcars$hp),
  vs = mean(mtcars$vs),
  cyl = 8)) +
  geom_line(size=2) +
  theme_bw()


ggplot(m, aes(hp, mpg, linetype=factor(vs), group=factor(vs)), 
  vars = list(
  hp = min(mtcars$hp):max(mtcars$hp),
  vs = c(0, 1),
  cyl = 8)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha=0.25) +
  geom_line(size=1) +
  theme_bw()
