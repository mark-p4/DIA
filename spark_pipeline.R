library(sparklyr)
library(dplyr)

source("clustering.R")

laptop= "C:\\Users\\User\\Dropbox\\PC\\Documents\\Uni\\DIA\\files\\unpacked\\"
pc = "C:\\Users\\Mark\\Dropbox\\PC\\Documents\\Uni\\DIA\\files\\unpacked\\"
originPath = mainPath

dblpPAth = paste(originPath, "DBLP_1995_2004.csv", sep = "")
acmPath = paste(originPath, "ACM_1995_2004.csv", sep = "")

spark_installed_versions()

conf = spark_config()

conf$`sparklyr.shell.driver-memory` <- "16G"

sc = spark_connect(master = "local", version = "2.4.3", config = conf)

normText = function(x, whiteSpace = TRUE){
  x = str_replace_all(x, "[[:punct:]]", "")
  #x = str_replace_all(x, "[[:alnum:]]", "")
  if (whiteSpace) {
    x = str_replace_all(x, "[[:space:]]", "")
  }
  x = tolower(x)
  return(x)
}

acmData = spark_read_csv(sc = sc, name = "acm", path = acmPath)
dblpData = spark_read_csv(sc = sc, name = "dblp", path = dblpPAth)

#sdf_bind_rows(acmData, dblpData)
#adf =spark_dataframe(tbl(sc, "acm"))

acmData$id = spark_apply(acmData, function(x) paste("a_", x$id, sep = ""))

#acmData = tbl(sc, "acm") %>% mutate(across(c("id"), function(x) paste("a_", x, sep = "")))
#dblpData = dblpData %>% mutate(across(c("id"), function(x) paste("d_", x, sep = "")))

combDF = tbl(sc, "acm") %>% union(tbl(sc, "dblp"))
sdf_collect(combDF)

spark_disconnect(sc)
