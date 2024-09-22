source("src/Functions.R")

arithmetic <- commandArgs(trailingOnly=TRUE)[1]
outfile <- commandArgs(trailingOnly=TRUE)[2]

plot_memtime_time(arithmetic, outfile)