libs = c("dplyr", "sparklyr", "igraph", "MCL", "ggplot2", "Metrics", "stringr",
         "readr", "tibble","foreach","arsenal", "stringdist", "microbenchmark")


mainPath = "C:\\Users\\Mark\\Desktop\\test\\"
codePath = "C:\\Users\\Mark\\Desktop\\DIA-Exercise_Mark_Paranskij_375558\\code\\"
numBenchMarkRuns = 1

install.packages(libs)
source(paste(codePath, "clustering.R", sep = ""))

#execute preparation
source(paste(codePath, "preparation.R", sep = ""))

#execute ER
source(paste(codePath, "ER.R", sep = ""))

#execute evaluation
source(paste(codePath, "evaluation.R", sep = ""))
