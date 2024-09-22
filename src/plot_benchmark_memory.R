source("src/Functions.R")

arithmetic <- commandArgs(trailingOnly=TRUE)[1]
outfile <- commandArgs(trailingOnly=TRUE)[2]

plot_benchmark_memory(arithmetic, outfile)