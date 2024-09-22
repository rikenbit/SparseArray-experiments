# Package Loading
library('matter') # for memtime
library('SparseArray')
library('spray')
library('ggplot2')

#######################################################
# All Functions for profiling
#######################################################
## 変換関数
arr2indval <- function(arr){
    index <- which(arr != 0, arr.ind=TRUE)
    value <- arr[index]
    list(index=index, value=value)
}

memtime_xxx <- function(arithmetic, method, size, outfile){
	cmd <- paste0("memtime_", arithmetic, "_", method,
		"(size, outfile)")
	eval(parse(text=cmd))
}

# unfold
size_all <- list(
    "1E7" = c(170,200,300),
    "5E7" = c(170,200,1500),
    "1E8" = c(400,500,500),
    "5E8" = c(400,500,2500),
    "1E9" = c(1000,1000,1000)
)

# vec
size_vec <- size_all

memtime_vec_array <- function(size, outfile){
    arr <- array(rbinom(prod(size_vec[[size]]),
        size=1, prob=1E-2), dim=size)
    out <- memtime(as.vector(arr))
    save(out, file=outfile)
}

memtime_vec_sparsearray <- function(size, outfile){
    arr <- array(rbinom(prod(size_vec[[size]]),
        size=1, prob=1E-2), dim=size)
    sparr <- as(arr, "SparseArray")
    out <- memtime(as.vector(sparr))
    save(out, file=outfile)
}

# sum
size_sum <- size_all

memtime_sum_array <- function(size, outfile){
    arr <- array(rbinom(prod(size_sum[[size]]),
        size=1, prob=1E-2), dim=size)
    out <- memtime(sum(arr))
    save(out, file=outfile)
}

memtime_sum_sparsearray <- function(size, outfile){
    arr <- array(rbinom(prod(size_sum[[size]]),
        size=1, prob=1E-2), dim=size)
    sparr <- as(arr, "SparseArray")
    out <- memtime(sum(sparr))
    save(out, file=outfile)
}

# innerprod
size_innerprod <- size_all

memtime_innerprod_array <- function(size, outfile){
    arr <- array(rbinom(prod(size_innerprod[[size]]),
        size=1, prob=1E-2), dim=size)
    out <- memtime(sum(arr*arr))
    save(out, file=outfile)
}

memtime_innerprod_sparsearray <- function(size, outfile){
    arr <- array(rbinom(prod(size_innerprod[[size]]),
        size=1, prob=1E-2), dim=size)
    sparr <- as(arr, "SparseArray")
    out <- memtime(sum(sparr*sparr))
    save(out, file=outfile)
}

# kronecker
size_kronecker <- list(
    "1E7" = list(c(100,100), c(100,10)),
    "5E7" = list(c(100,100), c(100,50)),
    "1E8" = list(c(100,100), c(100,100)),
    "5E8" = list(c(100,100), c(100,500)),
    "1E9" = list(c(1000,100), c(100,100))
)

memtime_kronecker_array <- function(size, outfile){
    arr1 <- array(rbinom(prod(size_kronecker[[size]][[1]]),
        size=1, prob=1E-2),
        dim=size_kronecker[[size]][[1]])
    arr2 <- array(rbinom(prod(size_kronecker[[size]][[2]]),
        size=1, prob=1E-2),
        dim=size_kronecker[[size]][[2]])
    out <- memtime(kronecker(arr1, arr2))
    save(out, file=outfile)
}

memtime_kronecker_sparsearray <- function(size, outfile){
    arr1 <- array(rbinom(prod(size_kronecker[[size]][[1]]),
        size=1, prob=1E-2),
        dim=size_kronecker[[size]][[1]])
    arr2 <- array(rbinom(prod(size_kronecker[[size]][[2]]),
        size=1, prob=1E-2),
        dim=size_kronecker[[size]][[2]])
    sparr1 <- as(arr1, "SparseArray")
    sparr2 <- as(arr2, "SparseArray")
    out <- memtime(kronecker(sparr1, sparr2))
    save(out, file=outfile)
}

#######################################################
# All Functions for Visualization (Calculation time)
#######################################################
# memtime
plot_memtime_time <- function(arithmetic, outfile){
	cmd <- paste0("plot_memtime_time_", arithmetic, "(outfile)")
	eval(parse(text=cmd))
}

aggregate_memtime_time_vec <- function(){
    files <- list.files("memtime", pattern="^vec")
    sec <- sapply(files, function(x){
        load(paste0("memtime/", x))
        out[[5]]})
    sec <- as.numeric(gsub(" sec", "", sec))
    elements <- gsub("vec_.*_", "", gsub(".RData", "", files))
    method <- gsub("vec_", "",
        gsub("_1E.*RData", "",
            gsub("_5E.*RData", "", files)))
    method <- factor(method, level=c("array", "sparsearray"))
    data <- data.frame(elements, sec, method)
    colnames(data) <- c("elements", "sec", "method")
    data
}

plot_memtime_time_vec <- function(outfile){
  data <- aggregate_memtime_time_vec()
  g <- ggplot(data, aes(x = elements, y = sec, fill = method))
  g <- g + geom_bar(stat = "identity", position = "dodge")
  g <- g + ggtitle("vec()")
  g <- g + xlab("# Elements")
  g <- g + ylab("Calculation Time (sec)")
  g <- g + theme(legend.title = element_blank())
  g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
  g <- g + theme(axis.text.x = element_text(size=12))
  g <- g + theme(axis.text.y = element_text(size=12))
  g <- g + theme(axis.title.x = element_text(size=18))
  g <- g + theme(axis.title.y = element_text(size=18))
  ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_memtime_time_sum <- function(){
    files <- list.files("memtime", pattern="^sum")
    sec <- sapply(files, function(x){
        load(paste0("memtime/", x))
        out[[5]]})
    sec <- as.numeric(gsub(" sec", "", sec))
    elements <- gsub("sum_.*_", "", gsub(".RData", "", files))
    method <- gsub("sum_", "",
        gsub("_1E.*RData", "",
            gsub("_5E.*RData", "", files)))
    method <- factor(method, level=c("array", "sparsearray"))
    data <- data.frame(elements, sec, method)
    colnames(data) <- c("elements", "sec", "method")
    data
}

plot_memtime_time_sum <- function(outfile){
  data <- aggregate_memtime_time_sum()
  g <- ggplot(data, aes(x = elements, y = sec, fill = method))
  g <- g + geom_bar(stat = "identity", position = "dodge")
  g <- g + ggtitle("sum()")
  g <- g + xlab("# Elements")
  g <- g + ylab("Calculation Time (sec)")
  g <- g + theme(legend.title = element_blank())
  g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
  g <- g + theme(axis.text.x = element_text(size=12))
  g <- g + theme(axis.text.y = element_text(size=12))
  g <- g + theme(axis.title.x = element_text(size=18))
  g <- g + theme(axis.title.y = element_text(size=18))
  ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_memtime_time_innerprod <- function(){
    files <- list.files("memtime", pattern="^innerprod")
    sec <- sapply(files, function(x){
        load(paste0("memtime/", x))
        out[[5]]})
    sec <- as.numeric(gsub(" sec", "", sec))
    elements <- gsub("innerprod_.*_", "", gsub(".RData", "", files))
    method <- gsub("innerprod_", "",
        gsub("_1E.*RData", "",
            gsub("_5E.*RData", "", files)))
    method <- factor(method, level=c("array", "sparsearray"))
    data <- data.frame(elements, sec, method)
    colnames(data) <- c("elements", "sec", "method")
    data
}

plot_memtime_time_innerprod <- function(outfile){
  data <- aggregate_memtime_time_innerprod()
  g <- ggplot(data, aes(x = elements, y = sec, fill = method))
  g <- g + geom_bar(stat = "identity", position = "dodge")
  g <- g + ggtitle("innerprod()")
  g <- g + xlab("# Elements")
  g <- g + ylab("Calculation Time (sec)")
  g <- g + theme(legend.title = element_blank())
  g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
  g <- g + theme(axis.text.x = element_text(size=12))
  g <- g + theme(axis.text.y = element_text(size=12))
  g <- g + theme(axis.title.x = element_text(size=18))
  g <- g + theme(axis.title.y = element_text(size=18))
  ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_memtime_time_kronecker <- function(){
    files <- list.files("memtime", pattern="^kronecker")
    sec <- sapply(files, function(x){
        load(paste0("memtime/", x))
        out[[5]]})
    sec <- as.numeric(gsub(" sec", "", sec))
    elements <- gsub("kronecker_.*_", "", gsub(".RData", "", files))
    method <- gsub("kronecker_", "",
        gsub("_1E.*RData", "",
            gsub("_5E.*RData", "", files)))
    method <- factor(method, level=c("array", "sparsearray"))
    data <- data.frame(elements, sec, method)
    colnames(data) <- c("elements", "sec", "method")
    data
}

plot_memtime_time_kronecker <- function(outfile){
  data <- aggregate_memtime_time_kronecker()
  g <- ggplot(data, aes(x = elements, y = sec, fill = method))
  g <- g + geom_bar(stat = "identity", position = "dodge")
  g <- g + ggtitle("kronecker()")
  g <- g + xlab("# Elements")
  g <- g + ylab("Calculation Time (sec)")
  g <- g + theme(legend.title = element_blank())
  g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
  g <- g + theme(axis.text.x = element_text(size=12))
  g <- g + theme(axis.text.y = element_text(size=12))
  g <- g + theme(axis.title.x = element_text(size=18))
  g <- g + theme(axis.title.y = element_text(size=18))
  ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

# benchmark
plot_benchmark_time <- function(arithmetic, outfile){
    cmd <- paste0("plot_benchmark_time_", arithmetic, "(outfile)")
    eval(parse(text=cmd))
}

aggregate_benchmark_time_vec <- function(){
    files <- list.files("benchmarks", pattern="^vec")
    sec <- sapply(files, function(x){
        read.table(paste0("benchmarks/", x), stringsAsFactors=FALSE)[2,1]})
    sec <- as.numeric(sec)
    elements <- gsub("vec_.*_", "", gsub(".txt", "", files))
    method <- gsub("vec_", "",
        gsub("_1E.*txt", "",
            gsub("_5E.*txt", "", files)))
    method <- factor(method, level=c("array", "sparsearray"))
    data <- data.frame(elements, sec, method)
    colnames(data) <- c("elements", "sec", "method")
    data
}

plot_benchmark_time_vec <- function(outfile){
  data <- aggregate_benchmark_time_vec()
  g <- ggplot(data, aes(x = elements, y = sec, fill = method))
  g <- g + geom_bar(stat = "identity", position = "dodge")
  g <- g + ggtitle("vec()")
  g <- g + xlab("# Elements")
  g <- g + ylab("Calculation Time (sec)")
  g <- g + theme(legend.title = element_blank())
  g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
  g <- g + theme(axis.text.x = element_text(size=12))
  g <- g + theme(axis.text.y = element_text(size=12))
  g <- g + theme(axis.title.x = element_text(size=18))
  g <- g + theme(axis.title.y = element_text(size=18))
  ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_benchmark_time_sum <- function(){
    files <- list.files("benchmarks", pattern="^sum")
    sec <- sapply(files, function(x){
        read.table(paste0("benchmarks/", x), stringsAsFactors=FALSE)[2,1]})
    sec <- as.numeric(sec)
    elements <- gsub("sum_.*_", "", gsub(".txt", "", files))
    method <- gsub("sum_", "",
        gsub("_1E.*txt", "",
            gsub("_5E.*txt", "", files)))
    method <- factor(method, level=c("array", "sparsearray"))
    data <- data.frame(elements, sec, method)
    colnames(data) <- c("elements", "sec", "method")
    data
}

plot_benchmark_time_sum <- function(outfile){
  data <- aggregate_benchmark_time_sum()
  g <- ggplot(data, aes(x = elements, y = sec, fill = method))
  g <- g + geom_bar(stat = "identity", position = "dodge")
  g <- g + ggtitle("sum()")
  g <- g + xlab("# Elements")
  g <- g + ylab("Calculation Time (sec)")
  g <- g + theme(legend.title = element_blank())
  g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
  g <- g + theme(axis.text.x = element_text(size=12))
  g <- g + theme(axis.text.y = element_text(size=12))
  g <- g + theme(axis.title.x = element_text(size=18))
  g <- g + theme(axis.title.y = element_text(size=18))
  ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_benchmark_time_innerprod <- function(){
    files <- list.files("benchmarks", pattern="^innerprod")
    sec <- sapply(files, function(x){
        read.table(paste0("benchmarks/", x), stringsAsFactors=FALSE)[2,1]})
    sec <- as.numeric(sec)
    elements <- gsub("innerprod_.*_", "", gsub(".txt", "", files))
    method <- gsub("innerprod_", "",
        gsub("_1E.*txt", "",
            gsub("_5E.*txt", "", files)))
    method <- factor(method, level=c("array", "sparsearray"))
    data <- data.frame(elements, sec, method)
    colnames(data) <- c("elements", "sec", "method")
    data
}

plot_benchmark_time_innerprod <- function(outfile){
  data <- aggregate_benchmark_time_innerprod()
  g <- ggplot(data, aes(x = elements, y = sec, fill = method))
  g <- g + geom_bar(stat = "identity", position = "dodge")
  g <- g + ggtitle("innerprod()")
  g <- g + xlab("# Elements")
  g <- g + ylab("Calculation Time (sec)")
  g <- g + theme(legend.title = element_blank())
  g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
  g <- g + theme(axis.text.x = element_text(size=12))
  g <- g + theme(axis.text.y = element_text(size=12))
  g <- g + theme(axis.title.x = element_text(size=18))
  g <- g + theme(axis.title.y = element_text(size=18))
  ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_benchmark_time_kronecker <- function(){
    files <- list.files("benchmarks", pattern="^kronecker")
    sec <- sapply(files, function(x){
        read.table(paste0("benchmarks/", x), stringsAsFactors=FALSE)[2,1]})
    sec <- as.numeric(sec)
    elements <- gsub("kronecker_.*_", "", gsub(".txt", "", files))
    method <- gsub("kronecker_", "",
        gsub("_1E.*txt", "",
            gsub("_5E.*txt", "", files)))
    method <- factor(method, level=c("array", "sparsearray"))
    data <- data.frame(elements, sec, method)
    colnames(data) <- c("elements", "sec", "method")
    data
}

plot_benchmark_time_kronecker <- function(outfile){
  data <- aggregate_benchmark_time_kronecker()
  g <- ggplot(data, aes(x = elements, y = sec, fill = method))
  g <- g + geom_bar(stat = "identity", position = "dodge")
  g <- g + ggtitle("kronecker()")
  g <- g + xlab("# Elements")
  g <- g + ylab("Calculation Time (sec)")
  g <- g + theme(legend.title = element_blank())
  g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
  g <- g + theme(axis.text.x = element_text(size=12))
  g <- g + theme(axis.text.y = element_text(size=12))
  g <- g + theme(axis.title.x = element_text(size=18))
  g <- g + theme(axis.title.y = element_text(size=18))
  ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

# gnutime
plot_gnutime_time <- function(arithmetic, outfile){
    cmd <- paste0("plot_gnutime_time_", arithmetic, "(outfile)")
    eval(parse(text=cmd))
}

aggregate_gnutime_time_vec <- function(){
    files <- list.files("logs", pattern="^vec")
    sec <- sapply(files, function(x){
        tmp <- read.delim(paste0("logs/", x), stringsAsFactors=FALSE)[,1]
        tmp <- tmp[grep("User time", tmp)]
        tmp <- gsub("User time \\(seconds\\): ", "", tmp)
        })
    sec <- as.numeric(sec)
    elements <- gsub("vec_.*_", "", gsub(".log", "", files))
    method <- gsub("vec_", "",
        gsub("_1E.*log", "",
            gsub("_5E.*log", "", files)))
    method <- factor(method, level=c("array", "sparsearray"))
    data <- data.frame(elements, sec, method)
    colnames(data) <- c("elements", "sec", "method")
    data
}

plot_gnutime_time_vec <- function(outfile){
    data <- aggregate_gnutime_time_vec()
    g <- ggplot(data, aes(x = elements, y = sec, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("vec()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Calculation Time (sec)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_gnutime_time_sum <- function(){
    files <- list.files("logs", pattern="^sum")
    sec <- sapply(files, function(x){
        tmp <- read.delim(paste0("logs/", x), stringsAsFactors=FALSE)[,1]
        tmp <- tmp[grep("User time", tmp)]
        tmp <- gsub("User time \\(seconds\\): ", "", tmp)
        })
    sec <- as.numeric(sec)
    elements <- gsub("sum_.*_", "", gsub(".log", "", files))
    method <- gsub("sum_", "",
        gsub("_1E.*log", "",
            gsub("_5E.*log", "", files)))
    method <- factor(method, level=c("array", "sparsearray"))
    data <- data.frame(elements, sec, method)
    colnames(data) <- c("elements", "sec", "method")
    data
}

plot_gnutime_time_sum <- function(outfile){
    data <- aggregate_gnutime_time_sum()
    g <- ggplot(data, aes(x = elements, y = sec, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("sum()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Calculation Time (sec)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_gnutime_time_innerprod <- function(){
    files <- list.files("logs", pattern="^innerprod")
    sec <- sapply(files, function(x){
        tmp <- read.delim(paste0("logs/", x), stringsAsFactors=FALSE)[,1]
        tmp <- tmp[grep("User time", tmp)]
        tmp <- gsub("User time \\(seconds\\): ", "", tmp)
        })
    sec <- as.numeric(sec)
    elements <- gsub("innerprod_.*_", "", gsub(".log", "", files))
    method <- gsub("innerprod_", "",
        gsub("_1E.*log", "",
            gsub("_5E.*log", "", files)))
    method <- factor(method, level=c("array", "sparsearray"))
    data <- data.frame(elements, sec, method)
    colnames(data) <- c("elements", "sec", "method")
    data
}

plot_gnutime_time_innerprod <- function(outfile){
    data <- aggregate_gnutime_time_innerprod()
    g <- ggplot(data, aes(x = elements, y = sec, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("innerprod()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Calculation Time (sec)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_gnutime_time_kronecker <- function(){
    files <- list.files("logs", pattern="^kronecker")
    sec <- sapply(files, function(x){
        tmp <- read.delim(paste0("logs/", x), stringsAsFactors=FALSE)[,1]
        tmp <- tmp[grep("User time", tmp)]
        tmp <- gsub("User time \\(seconds\\): ", "", tmp)
        })
    sec <- as.numeric(sec)
    elements <- gsub("kronecker_.*_", "", gsub(".log", "", files))
    method <- gsub("kronecker_", "",
        gsub("_1E.*log", "",
            gsub("_5E.*log", "", files)))
    method <- factor(method, level=c("array", "sparsearray"))
    data <- data.frame(elements, sec, method)
    colnames(data) <- c("elements", "sec", "method")
    data
}

plot_gnutime_time_kronecker <- function(outfile){
    data <- aggregate_gnutime_time_kronecker()
    g <- ggplot(data, aes(x = elements, y = sec, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("kronecker()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Calculation Time (sec)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}



#######################################################
# All Functions for Visualization (Memory usage)
#######################################################
# memtime
plot_memtime_memory <- function(arithmetic, outfile){
	cmd <- paste0("plot_memtime_memory_", arithmetic, "(outfile)")
	eval(parse(text=cmd))
}

aggregate_memtime_memory_vec <- function(){
    files <- list.files("memtime", pattern="^vec")
    gb <- sapply(files, function(x){
        load(paste0("memtime/", x))
        out[[1]]})
    for(i in seq_along(gb)){
        if(length(grep("MB", gb[i])) != 0){
            gb[i] <- as.numeric(gsub(" MB", "", gb[i])) / 10^3
        }else{
            gb[i] <- as.numeric(gsub(" GB", "", gb[i]))
        }
    }
    gb <- as.numeric(gb)
    elements <- gsub("vec_.*_", "", gsub(".RData", "", files))
    method <- gsub("vec_", "", gsub("_.E.*RData", "", files))
    method <- factor(method,
    	level=c("array", "sparsearray"))
    data <- data.frame(elements, gb, method)
    colnames(data) <- c("elements", "gb", "method")
    data
}

plot_memtime_memory_vec <- function(outfile){
	data <- aggregate_memtime_memory_vec()
	g <- ggplot(data, aes(x = elements, y = gb, fill = method))
	g <- g + geom_bar(stat = "identity", position = "dodge")
	g <- g + ggtitle("vec()")
	g <- g + xlab("# Elements")
	g <- g + ylab("Memory usage (GB)")
	g <- g + theme(legend.title = element_blank())
	g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
	g <- g + theme(axis.text.x = element_text(size=12))
	g <- g + theme(axis.text.y = element_text(size=12))
	g <- g + theme(axis.title.x = element_text(size=18))
	g <- g + theme(axis.title.y = element_text(size=18))
	ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_memtime_memory_sum <- function(){
    files <- list.files("memtime", pattern="^sum")
    gb <- sapply(files, function(x){
        load(paste0("memtime/", x))
        out[[1]]})
    for(i in seq_along(gb)){
        if(length(grep("MB", gb[i])) != 0){
            gb[i] <- as.numeric(gsub(" MB", "", gb[i])) / 10^3
        }else{
            gb[i] <- as.numeric(gsub(" GB", "", gb[i]))
        }
    }
    gb <- as.numeric(gb)
    elements <- gsub("sum_.*_", "", gsub(".RData", "", files))
    method <- gsub("sum_", "", gsub("_.E.*RData", "", files))
    method <- factor(method,
        level=c("array", "sparsearray"))
    data <- data.frame(elements, gb, method)
    colnames(data) <- c("elements", "gb", "method")
    data
}

plot_memtime_memory_sum <- function(outfile){
    data <- aggregate_memtime_memory_sum()
    g <- ggplot(data, aes(x = elements, y = gb, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("sum()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Memory usage (GB)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_memtime_memory_innerprod <- function(){
    files <- list.files("memtime", pattern="^innerprod")
    gb <- sapply(files, function(x){
        load(paste0("memtime/", x))
        out[[1]]})
    for(i in seq_along(gb)){
        if(length(grep("MB", gb[i])) != 0){
            gb[i] <- as.numeric(gsub(" MB", "", gb[i])) / 10^3
        }else{
            gb[i] <- as.numeric(gsub(" GB", "", gb[i]))
        }
    }
    gb <- as.numeric(gb)
    elements <- gsub("innerprod_.*_", "", gsub(".RData", "", files))
    method <- gsub("innerprod_", "", gsub("_.E.*RData", "", files))
    method <- factor(method,
        level=c("array", "sparsearray"))
    data <- data.frame(elements, gb, method)
    colnames(data) <- c("elements", "gb", "method")
    data
}

plot_memtime_memory_innerprod <- function(outfile){
    data <- aggregate_memtime_memory_innerprod()
    g <- ggplot(data, aes(x = elements, y = gb, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("innerprod()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Memory usage (GB)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_memtime_memory_kronecker <- function(){
    files <- list.files("memtime", pattern="^kronecker")
    gb <- sapply(files, function(x){
        load(paste0("memtime/", x))
        out[[1]]})
    for(i in seq_along(gb)){
        if(length(grep("MB", gb[i])) != 0){
            gb[i] <- as.numeric(gsub(" MB", "", gb[i])) / 10^3
        }else{
            gb[i] <- as.numeric(gsub(" GB", "", gb[i]))
        }
    }
    gb <- as.numeric(gb)
    elements <- gsub("kronecker_.*_", "", gsub(".RData", "", files))
    method <- gsub("kronecker_", "", gsub("_.E.*RData", "", files))
    method <- factor(method,
        level=c("array", "sparsearray"))
    data <- data.frame(elements, gb, method)
    colnames(data) <- c("elements", "gb", "method")
    data
}

plot_memtime_memory_kronecker <- function(outfile){
    data <- aggregate_memtime_memory_kronecker()
    g <- ggplot(data, aes(x = elements, y = gb, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("kronecker()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Memory usage (GB)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

# benchmark
plot_benchmark_memory <- function(arithmetic, outfile){
    cmd <- paste0("plot_benchmark_memory_", arithmetic, "(outfile)")
    eval(parse(text=cmd))
}

aggregate_benchmark_memory_vec <- function(){
    files <- list.files("benchmarks", pattern="^vec")
    gb <- sapply(files, function(x){
        read.table(paste0("benchmarks/", x),
            stringsAsFactors=FALSE)[2,3]})
    gb <- as.numeric(gb) / 10^3
    elements <- gsub("vec_.*_", "", gsub(".txt", "", files))
    method <- gsub("vec_", "", gsub("_.?E.*.txt", "", files))
    method <- factor(method,
        level=c("array", "sparsearray"))
    data <- data.frame(elements, gb, method)
    colnames(data) <- c("elements", "gb", "method")
    data
}

plot_benchmark_memory_vec <- function(outfile){
    data <- aggregate_benchmark_memory_vec()
    g <- ggplot(data, aes(x = elements, y = gb, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("vec()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Memory usage (GB)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_benchmark_memory_sum <- function(){
    files <- list.files("benchmarks", pattern="^sum")
    gb <- sapply(files, function(x){
        read.table(paste0("benchmarks/", x),
            stringsAsFactors=FALSE)[2,3]})
    gb <- as.numeric(gb) / 10^3
    elements <- gsub("sum_.*_", "", gsub(".txt", "", files))
    method <- gsub("sum_", "", gsub("_.?E.*.txt", "", files))
    method <- factor(method,
        level=c("array", "sparsearray"))
    data <- data.frame(elements, gb, method)
    colnames(data) <- c("elements", "gb", "method")
    data
}

plot_benchmark_memory_sum <- function(outfile){
    data <- aggregate_benchmark_memory_sum()
    g <- ggplot(data, aes(x = elements, y = gb, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("sum()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Memory usage (GB)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_benchmark_memory_innerprod <- function(){
    files <- list.files("benchmarks", pattern="^innerprod")
    gb <- sapply(files, function(x){
        read.table(paste0("benchmarks/", x),
            stringsAsFactors=FALSE)[2,3]})
    gb <- as.numeric(gb) / 10^3
    elements <- gsub("innerprod_.*_", "", gsub(".txt", "", files))
    method <- gsub("innerprod_", "", gsub("_.?E.*.txt", "", files))
    method <- factor(method,
        level=c("array", "sparsearray"))
    data <- data.frame(elements, gb, method)
    colnames(data) <- c("elements", "gb", "method")
    data
}

plot_benchmark_memory_innerprod <- function(outfile){
    data <- aggregate_benchmark_memory_innerprod()
    g <- ggplot(data, aes(x = elements, y = gb, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("innerprod()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Memory usage (GB)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_benchmark_memory_kronecker <- function(){
    files <- list.files("benchmarks", pattern="^kronecker")
    gb <- sapply(files, function(x){
        read.table(paste0("benchmarks/", x),
            stringsAsFactors=FALSE)[2,3]})
    gb <- as.numeric(gb) / 10^3
    elements <- gsub("kronecker_.*_", "", gsub(".txt", "", files))
    method <- gsub("kronecker_", "", gsub("_.?E.*.txt", "", files))
    method <- factor(method,
        level=c("array", "sparsearray"))
    data <- data.frame(elements, gb, method)
    colnames(data) <- c("elements", "gb", "method")
    data
}

plot_benchmark_memory_kronecker <- function(outfile){
    data <- aggregate_benchmark_memory_kronecker()
    g <- ggplot(data, aes(x = elements, y = gb, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("kronecker()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Memory usage (GB)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

# gnutime
plot_gnutime_memory <- function(arithmetic, outfile){
    cmd <- paste0("plot_gnutime_memory_", arithmetic, "(outfile)")
    eval(parse(text=cmd))
}

aggregate_gnutime_memory_vec <- function(){
    files <- list.files("logs", pattern="^vec")
    gb <- sapply(files, function(x){
        tmp <- read.delim(paste0("logs/", x), stringsAsFactors=FALSE)[,1]
        tmp <- tmp[grep("Maximum resident", tmp)]
        tmp <- gsub("Maximum resident set size \\(kbytes\\): ", "", tmp)
        })
    gb <- as.numeric(gb) / 10^6
    elements <- gsub("vec_.*_", "", gsub(".log", "", files))
    method <- gsub("vec_", "", gsub("_.?E.*log", "", files))
    method <- factor(method,
        level=c("array", "sparsearray"))
    data <- data.frame(elements, gb, method)
    colnames(data) <- c("elements", "gb", "method")
    data
}

plot_gnutime_memory_vec <- function(outfile){
    data <- aggregate_gnutime_memory_vec()
    g <- ggplot(data, aes(x = elements, y = gb, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("vec()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Memory usage (GB)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}


aggregate_gnutime_memory_sum <- function(){
    files <- list.files("logs", pattern="^sum")
    gb <- sapply(files, function(x){
        tmp <- read.delim(paste0("logs/", x), stringsAsFactors=FALSE)[,1]
        tmp <- tmp[grep("Maximum resident", tmp)]
        tmp <- gsub("Maximum resident set size \\(kbytes\\): ", "", tmp)
        })
    gb <- as.numeric(gb) / 10^6
    elements <- gsub("sum_.*_", "", gsub(".log", "", files))
    method <- gsub("sum_", "", gsub("_.?E.*log", "", files))
    method <- factor(method,
        level=c("array", "sparsearray"))
    data <- data.frame(elements, gb, method)
    colnames(data) <- c("elements", "gb", "method")
    data
}

plot_gnutime_memory_sum <- function(outfile){
    data <- aggregate_gnutime_memory_sum()
    g <- ggplot(data, aes(x = elements, y = gb, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("sum()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Memory usage (GB)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_gnutime_memory_innerprod <- function(){
    files <- list.files("logs", pattern="^innerprod")
    gb <- sapply(files, function(x){
        tmp <- read.delim(paste0("logs/", x), stringsAsFactors=FALSE)[,1]
        tmp <- tmp[grep("Maximum resident", tmp)]
        tmp <- gsub("Maximum resident set size \\(kbytes\\): ", "", tmp)
        })
    gb <- as.numeric(gb) / 10^6
    elements <- gsub("innerprod_.*_", "", gsub(".log", "", files))
    method <- gsub("innerprod_", "", gsub("_.?E.*log", "", files))
    method <- factor(method,
        level=c("array", "sparsearray"))
    data <- data.frame(elements, gb, method)
    colnames(data) <- c("elements", "gb", "method")
    data
}

plot_gnutime_memory_innerprod <- function(outfile){
    data <- aggregate_gnutime_memory_innerprod()
    g <- ggplot(data, aes(x = elements, y = gb, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("innerprod()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Memory usage (GB)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}

aggregate_gnutime_memory_kronecker <- function(){
    files <- list.files("logs", pattern="^kronecker")
    gb <- sapply(files, function(x){
        tmp <- read.delim(paste0("logs/", x), stringsAsFactors=FALSE)[,1]
        tmp <- tmp[grep("Maximum resident", tmp)]
        tmp <- gsub("Maximum resident set size \\(kbytes\\): ", "", tmp)
        })
    gb <- as.numeric(gb) / 10^6
    elements <- gsub("kronecker_.*_", "", gsub(".log", "", files))
    method <- gsub("kronecker_", "", gsub("_.?E.*log", "", files))
    method <- factor(method,
        level=c("array", "sparsearray"))
    data <- data.frame(elements, gb, method)
    colnames(data) <- c("elements", "gb", "method")
    data
}

plot_gnutime_memory_kronecker <- function(outfile){
    data <- aggregate_gnutime_memory_kronecker()
    g <- ggplot(data, aes(x = elements, y = gb, fill = method))
    g <- g + geom_bar(stat = "identity", position = "dodge")
    g <- g + ggtitle("kronecker()")
    g <- g + xlab("# Elements")
    g <- g + ylab("Memory usage (GB)")
    g <- g + theme(legend.title = element_blank())
    g <- g + theme(plot.title = element_text(size=22, hjust = 0.5))
    g <- g + theme(axis.text.x = element_text(size=12))
    g <- g + theme(axis.text.y = element_text(size=12))
    g <- g + theme(axis.title.x = element_text(size=18))
    g <- g + theme(axis.title.y = element_text(size=18))
    ggsave(file=outfile, plot=g, dpi=200, width=7.0, height=4.0)
}
