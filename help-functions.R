#R
.plotPlate <- function(x, group){
  m <- length(table(x[, group])) + 1
  #x <- x[x['plate'] == 1,]
  palette <- hcl.pals(type='qualitative')
  cols <- sapply(palette, hcl.colors, n = m)
  
  y <- as.integer(as.factor(x$y))
  ymax <- max(y)
  y0 <- (ymax * (x$plate - 1)) + y
  
  plot(x$x, y0,
       type='b', axes=FALSE, pch=16, col=cols[as.factor(x[, group])], cex=4,
       ylim = c(min(1)-0.5, max(y0)+0.5),
       xlab='x', 
       ylab='y',
       main='sample run order')
  
  axis(1, x$x, x$x)
  axis(2, 1:max(y0), rep(names(table(x$y)), length(table(x$plate))))
  
  text(x$x, y0, 1:nrow(x))
}