#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

source("Ratiocalc_files.R")

source("Ratiocalc_calc.R")

load(args[1])


results=ratiocalc(files=batch.data[[1]],
                  sep = batch.data[[2]],
                  num.col=batch.data[[3]],
                  den.col=batch.data[[4]],
                  marker.col=batch.data[[5]],
                  coord.col=batch.data[[6]],
                  marker.lim=batch.data[[7]],
                  max.shift=batch.data[[8]],
                  cor.lim=batch.data[[9]],
                  time.str=NULL,
                  win.width=batch.data[[10]],
                  mov.step=batch.data[[11]],
                  peak.dist=batch.data[[12]],
                  output=args[1]
)

save(results,file = args[1])