library(stringr)
library(readr)
library(dplyr)
library(tibble)
library(foreach)
library(arsenal)
library(stringdist)
library(microbenchmark)

laptop= "C:\\Users\\User\\Dropbox\\PC\\Documents\\Uni\\DIA\\files\\unpacked\\"
pc = "C:\\Users\\Mark\\Dropbox\\PC\\Documents\\Uni\\DIA\\files\\unpacked\\"
originPath = pc

dblpPAth = paste(originPath, "DBLP_1995_2004.csv", sep = "")
acmPath = paste(originPath, "ACM_1995_2004.csv", sep = "")

dblpFiltered = read.csv(dblpPAth)
acmFiltered = read.csv(acmPath)

#plan:

#preprosses text: lowercase all strings, remove ids (arbitrary)

#implement a pipeline with the most basic blocking technique with the most basic matching on top
#-> this will be the baseline

#vary blocking techniques; 3
  #select different blocking attributes for example year and venue
#vary matching techniques; 3
  #different similarity or distance measures

#results in a total of 9 matching outputs and 8 comparisons

#clustering will follow as a last step after evaluation and tuning of prior methods

#after clustering add back single entries to create a complete dataset

#-------------------------------------------------------------------------------
#coding part
#-------------------------------------------------------------------------------

#make all columns containing text string into lower case and remove special characters and white spaces

normText = function(x, whiteSpace = TRUE){
  x = str_replace_all(x, "[[:punct:]]", "")
  x = str_replace_all(x, "[^[:alnum:]]", "")
  if (whiteSpace) {
    x = str_replace_all(x, " ", "")
  }
  x = tolower(x)
  return(x)
}
dblpFiltered[, c("title","authors","venue","abstract")] = apply(dblpFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x))
acmFiltered[, c("title","authors","venue","abstract")] = apply(acmFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x))

dblpFiltered$id = paste("d", 1:nrow(dblpFiltered),sep = "_")
colnames(dblpFiltered) = paste("d", colnames(dblpFiltered),sep = "_")

acmFiltered$id = paste("a",1:nrow(acmFiltered), sep = "_")
colnames(acmFiltered) = paste("a", colnames(acmFiltered), sep = "_")

fullData = merge(dblpFiltered, acmFiltered)
fullData = add_column(fullData, key = paste(fullData$d_id, fullData$a_id, sep = "-"))
length(fullData$key)
length(unique(fullData$key))

#fullData[,"yearDiff"] = abs(fullData$d_year - fullData$a_year)

baseLineLV = function(fullData1){
  fullData1[, "lvSim"] = stringsim(fullData1$d_title, fullData1$a_title, method = "lv")
  #fullData1[, "lvPerc"] = fullData1$lvDist / max(nchar(fullData1$d_title), nchar(fullData1$a_title))
  fullData1[, "matched"] = fullData1$lvSim >= 0.95
  return(fullData1)
}

results = microbenchmark(baseLineLV(fullData), times = 10)
write.csv(results,paste(originPath, "result_baseline_bench.csv", sep = ""), row.names = FALSE)

fullData1 = baseLineLV(fullData)

write.csv(fullData1[,c("key", "matched")], paste(originPath, "matched_Entities_Baseline.csv", sep = ""), row.names = FALSE)
  
blockedData[, "lvDist"] = stringdist(blockedData$d_title, blockedData$a_title, method = "lv")
blockedData[, "lvPerc"] = blockedData$lvDist / max(nchar(blockedData$d_title), nchar(blockedData$a_title))

similar = fullData1[fullData1$matched, c("key", "d_title", "a_title", "lvSim", "matched")]

hist(blockedData$lvDist)
hist(blockedData$lvPerc)

similar = blockedData[blockedData$lvPerc <= 0.05, c("d_id", "a_id", "d_title", "a_title", "lvDist", "lvPerc","d_year","a_year")]
