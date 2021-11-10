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

#```{r EH4547.raw, error=TRUE, message = FALSE, warning = FALSE, echo = FALSE}
## fetch raw file using ExperimentHub
## data is part of tartare package
## https://bioconductor.org/packages/3.14/data/experiment/html/tartare.html
library(ExperimentHub)
eh <- ExperimentHub::ExperimentHub()
EH4547 <- normalizePath(eh[["EH4547"]])

EH4547.raw <- paste0(EH4547, ".raw")

if (!file.exists(EH4547.raw)){
  file.copy(EH4547, EH4547.raw)
}

iRTmz <- c(487.2571, 547.2984, 622.8539, 636.8695, 644.8230, 669.8384, 683.8282,
           683.8541, 699.3388, 726.8361, 776.9301)
names(iRTmz) <- c("LGGNEQVTR", "YILAGVENSK", "GTFIIDPGGVIR", "GTFIIDPAAVIR", "GAGSSEPVTGLDAK",
                  "TPVISGGPYEYR", "VEATFGVDESNAK", "TPVITGAPYEYR", "DGLDAASYYAPVR", "ADVTPADFSEWSK", "LFLQFGAQGSPFLK")

#````
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