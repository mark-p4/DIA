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
originPath = laptop

dblpPAth = paste(originPath, "DBLP_1995_2004.csv", sep = "")
acmPath = paste(originPath, "ACM_1995_2004.csv", sep = "")

overviewCols = c("experiment", "tuplesComputed" ,"blocking", "SimMeasure", "Matches", "avgTime")
resultOverview = data.frame(matrix(nrow = 10, ncol = length(overviewCols)))
colnames(resultOverview) =  overviewCols


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

normText = function(x, whiteSpace = TRUE){
  x = str_replace_all(x, "[[:punct:]]", "")
  #x = str_replace_all(x, "[[:alnum:]]", "")
  if (whiteSpace) {
    x = str_replace_all(x, "[[:space:]]", "")
  }
  x = tolower(x)
  return(x)
}
#-------------------------------------------------------------------------------
#baseline
#-------------------------------------------------------------------------------

#make all columns containing text string into lower case and remove special characters and white spaces

dblpFiltered = read.csv(dblpPAth)
acmFiltered = read.csv(acmPath)


dblpFiltered[, c("title","authors","venue","abstract")] = apply(dblpFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x))
acmFiltered[, c("title","authors","venue","abstract")] = apply(acmFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x))

#dblpFiltered$id = paste("d", 1:nrow(dblpFiltered),sep = "_")
colnames(dblpFiltered) = paste("d", colnames(dblpFiltered),sep = "_")

#acmFiltered$id = paste("a",1:nrow(acmFiltered), sep = "_")
colnames(acmFiltered) = paste("a", colnames(acmFiltered), sep = "_")

fullData = merge(dblpFiltered, acmFiltered)
#fullData = add_column(fullData, key = paste(fullData$d_id, fullData$a_id, sep = "-"))
#length(fullData$key)
#length(unique(fullData$key))

#fullData[,"yearDiff"] = abs(fullData$d_year - fullData$a_year)

baseLineLV = function(fullData1){
  fullData1[, "lvSim"] = stringsim(fullData1$d_title, fullData1$a_title, method = "lv")
  #fullData1[, "lvPerc"] = fullData1$lvDist / max(nchar(fullData1$d_title), nchar(fullData1$a_title))
  fullData1[, "matched"] = fullData1$lvSim >= 0.8
  return(fullData1)
}

#microbenchmark results are in Nanoseconds
results = microbenchmark(baseLineLV(fullData), times = 10)
#print(results, unit = "ns")
write.csv(results,paste(originPath, "result_baseline_bench.csv", sep = ""), row.names = FALSE)

fullData1 = baseLineLV(fullData)
nrow(fullData1[fullData1$matched, ])

write.csv(fullData1[,c("d_id", "a_id", "lvSim","matched")], paste(originPath, "matched_Entities_Baseline.csv", sep = ""), row.names = FALSE)

resultSum = c("baseline", nrow(fullData), "without", "Levenshtein - untuned", nrow(fullData1[fullData1$matched, ]), mean(results$time))
resultOverview[1,] = resultSum

#-------------------------------------------------------------------------------
#baseline - tuned
#-------------------------------------------------------------------------------

#make all columns containing text string into lower case and remove special characters and white spaces


dblpFiltered = read.csv(dblpPAth)
acmFiltered = read.csv(acmPath)


dblpFiltered[, c("title","authors","venue","abstract")] = apply(dblpFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x))
acmFiltered[, c("title","authors","venue","abstract")] = apply(acmFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x))

#dblpFiltered$id = paste("d", 1:nrow(dblpFiltered),sep = "_")
colnames(dblpFiltered) = paste("d", colnames(dblpFiltered),sep = "_")

#acmFiltered$id = paste("a",1:nrow(acmFiltered), sep = "_")
colnames(acmFiltered) = paste("a", colnames(acmFiltered), sep = "_")

fullData = merge(dblpFiltered, acmFiltered)
#fullData = add_column(fullData, key = paste(fullData$d_id, fullData$a_id, sep = "-"))
#length(fullData$key)
#length(unique(fullData$key))

#fullData[,"yearDiff"] = abs(fullData$d_year - fullData$a_year)

baseLineLV = function(fullData1){
  fullData1[, "lvSim"] = stringsim(fullData1$d_title, fullData1$a_title, method = "lv")
  #fullData1[, "lvPerc"] = fullData1$lvDist / max(nchar(fullData1$d_title), nchar(fullData1$a_title))
  fullData1[, "matched"] = fullData1$lvSim >= 0.95
  return(fullData1)
}

#microbenchmark results are in Nanoseconds
results = microbenchmark(baseLineLV(fullData), times = 10)
#print(results, unit = "ns")
write.csv(results,paste(originPath, "result_baseline_tuned_bench.csv", sep = ""), row.names = FALSE)

fullData1 = baseLineLV(fullData)
nrow(fullData1[fullData1$matched, ])

write.csv(fullData1[,c("d_id", "a_id", "lvSim","matched")], paste(originPath, "matched_Entities_Baseline_tuned.csv", sep = ""), row.names = FALSE)

resultSum = c("baseline-tuned", nrow(fullData), "without", "Levenshtein", nrow(fullData1[fullData1$matched, ]), mean(results$time))
resultOverview[2,] = resultSum

#-------------------------------------------------------------------------------
# blocking by year and levenshtein distance tuned
#-------------------------------------------------------------------------------
dblpFiltered = read.csv(dblpPAth)
acmFiltered = read.csv(acmPath)

dblpFiltered[, c("title","authors","venue","abstract")] = apply(dblpFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x))
acmFiltered[, c("title","authors","venue","abstract")] = apply(acmFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x))

#dblpFiltered$id = paste("d", 1:nrow(dblpFiltered),sep = "_")
colnames(dblpFiltered) = paste("d", colnames(dblpFiltered),sep = "_")

#acmFiltered$id = paste("a",1:nrow(acmFiltered), sep = "_")
colnames(acmFiltered) = paste("a", colnames(acmFiltered), sep = "_")

fullData = merge(dblpFiltered, acmFiltered)

pipeline1 = function(){
  fullData[, "yearDiff"] = abs(fullData$d_year - fullData$a_year)
  blockedData = fullData[fullData$yearDiff <= 3,]
  blockedData[, "lvSim"] = stringsim(blockedData$d_title, blockedData$a_title, method = "lv")
  #untuned
  blockedData[, "matched"] = blockedData$lvSim >= 0.95
  #tuned
  #blockedData[, "matched"] = blockedData$lvSim >= 0.95 
  return(blockedData)
}

results = microbenchmark(pipeline1(), times = 10)
write.csv(results,paste(originPath, "result_pipeline1_bench.csv", sep = ""), row.names = FALSE)
#write.csv(results,paste(originPath, "result_pipeline1_bench.csv", sep = ""), row.names = FALSE)

fullData1 = pipeline1()

write.csv(fullData1[,c("d_id", "a_id", "lvSim","matched")], paste(originPath, "matched_Entities_pipeline1.csv", sep = ""), row.names = FALSE)
#write.csv(fullData1[,c("d_id", "a_id", "lvSim","matched")], paste(originPath, "matched_Entities_pipeline1.csv", sep = ""), row.names = FALSE)

resultSum = c("pipeline1", nrow(fullData1), "by yearDiff", "Levenshtein", nrow(fullData1[fullData1$matched, ]), mean(results$time))
resultOverview[3,] = resultSum

#-------------------------------------------------------------------------------
# blocking by year and jaccard - distance for trigrams
#-------------------------------------------------------------------------------
dblpFiltered = read.csv(dblpPAth)
acmFiltered = read.csv(acmPath)

dblpFiltered[, c("title","authors","venue","abstract")] = apply(dblpFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x, whiteSpace = FALSE))
acmFiltered[, c("title","authors","venue","abstract")] = apply(acmFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x, whiteSpace = FALSE))

#dblpFiltered$id = paste("d", 1:nrow(dblpFiltered),sep = "_")
colnames(dblpFiltered) = paste("d", colnames(dblpFiltered),sep = "_")

#acmFiltered$id = paste("a",1:nrow(acmFiltered), sep = "_")
colnames(acmFiltered) = paste("a", colnames(acmFiltered), sep = "_")

fullData = merge(dblpFiltered, acmFiltered)

pipeline2 = function(){
  fullData[, "yearDiff"] = abs(fullData$d_year - fullData$a_year)
  blockedData = fullData[fullData$yearDiff <= 3,]
  blockedData[, "jacSim"] = stringsim(blockedData$d_title, blockedData$a_title, method = "jaccard", q = 3)
  blockedData[, "matched"] = blockedData$jacSim >= 0.8
  return(blockedData)
}

results = microbenchmark(pipeline2(), times = 10)
write.csv(results, paste(originPath, "result_pipeline1_untuned_bench.csv", sep = ""), row.names = FALSE)
#write.csv(results,paste(originPath, "result_pipeline1_bench.csv", sep = ""), row.names = FALSE)

fullData1 = pipeline2()
hist(fullData1$jacSim)
nrow(fullData1[fullData1$matched, ])

similar = fullData1[fullData1$matched, c("d_title", "a_title", "jacSim", "matched")]

#stringsim(strsplit(x = fullData[1, "d_title"], split = " "), strsplit(x = fullData[1, "a_title"], split = " "), method = "jaccard")

write.csv(fullData1[,c("d_id", "a_id", "jacSim","matched")], paste(originPath, "matched_Entities_pipeline1_untuned.csv", sep = ""), row.names = FALSE)
#write.csv(fullData1[,c("d_id", "a_id", "lvSim","matched")], paste(originPath, "matched_Entities_pipeline1.csv", sep = ""), row.names = FALSE)

resultSum = c("pipeline2", nrow(fullData1), "by yearDiff", "jaccard trigram", nrow(fullData1[fullData1$matched, ]), mean(results$time))
resultOverview[3,] = resultSum
