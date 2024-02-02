libs = c("dplyr", "sparklyr", "igraph", "MCL", "ggplot2", "Metrics", "stringr",
         "readr", "tibble","foreach","arsenal", "stringdist", "microbenchmark")


mainPath = "C:\\Users\\Mark\\Desktop\\test\\"
numBenchMarkRuns = 1

install.packages(libs)
source("clustering.R")

#execute preparation
source("preparation.R")

#execute ER
source("ER.R")

#execute evaluation
source("evaluation.R")
