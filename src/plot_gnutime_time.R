source("src/Functions.R")

arithmetic <- commandArgs(trailingOnly=TRUE)[1]
outfile <- commandArgs(trailingOnly=TRUE)[2]

plot_gnutime_time(arithmetic, outfile)