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


dump <- function(){
iris |>
head(48 + 48 - 3) |>
  assignPlatePosition(volume=1, plate = c(2,1)) |>
  blockRandom() |>
  na.omit() |>
  insertSamples(howoften=4, begin=FALSE, end=FALSE,
                stdPosX='6', stdPosY='F', plate=1, stdName = "clean",
                volume=2,
                method="C:\\Xcalibur\\methods\\__Standard_methods\\general_clean") |> 
  insertSamples(howoften=8, begin=FALSE, end=FALSE,
                stdPosX='8', stdPosY='F', plate=1, stdName = "autoQC01",
                volume=2,
                method="C:\\Xcalibur\\methods\\__autoQC\\trap\\autoQC01") |>
  insertSamples(howoften=0, begin=FALSE, end=TRUE,
                volume=2,
                stdPosX='7', stdPosY='F', plate=1, stdName = "autoQC4L",
                method="C:\\Xcalibur\\methods\\__autoQC\\trap\\autoQC4L")  |> 
  protViz:::formatXCalibur(path=sprintf("D:\\Data2San\\p%d\\Proteomics\\%s\\%s_%s",
                                        container,
                                        instrument,
                                        user,
                                        format(Sys.time(), "%Y%m%d")))
}