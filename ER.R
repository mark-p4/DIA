library(stringr)
library(readr)
library(dplyr)
library(tibble)
library(foreach)
library(arsenal)
library(stringdist)
library(microbenchmark)
library(igraph)
library(MCL)
library(ggplot2)

source("clustering.R")

laptop= "C:\\Users\\User\\Dropbox\\PC\\Documents\\Uni\\DIA\\files\\unpacked\\"
pc = "C:\\Users\\Mark\\Dropbox\\PC\\Documents\\Uni\\DIA\\files\\unpacked\\"
originPath = pc

dblpPAth = paste(originPath, "DBLP_1995_2004.csv", sep = "")
acmPath = paste(originPath, "ACM_1995_2004.csv", sep = "")

overviewCols = c("experiment", "tuplesComputed" ,"blocking", "SimMeasure", "Matches", "avgMatchTime", "numDuplicates")
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

dblpFiltered$id = paste("d", 1:nrow(dblpFiltered),sep = "_")
colnames(dblpFiltered) = paste("d", colnames(dblpFiltered),sep = "_")

acmFiltered$id = paste("a",1:nrow(acmFiltered), sep = "_")
colnames(acmFiltered) = paste("a", colnames(acmFiltered), sep = "_")

fullData = merge(dblpFiltered, acmFiltered)
#fullData = add_column(fullData, key = paste(fullData$d_id, fullData$a_id, sep = "-"))
#length(fullData$key)
#length(unique(fullData$key))

#fullData[,"yearDiff"] = abs(fullData$d_year - fullData$a_year)

baseLineLV = function(fullData1){
  fullData1[, "Sim"] = stringsim(fullData1$d_title, fullData1$a_title, method = "lv")
  #fullData1[, "lvPerc"] = fullData1$lvDist / max(nchar(fullData1$d_title), nchar(fullData1$a_title))
  fullData1[, "matched"] = fullData1$Sim >= 0.8
  fullData = fullData1
  fullData[, c("clustNo", "refersTo")] = NA
  fullData1 = clusterMatched(fullData1[fullData1$matched, ])
  fullData1 = rbind(fullData1, fullData[!fullData$matched, ])
  return(fullData1)
}

#microbenchmark results are in Nanoseconds
resultsMatch = microbenchmark(baseLineLV(fullData), times = 10)
#print(resultsMatch, unit = "ns")
write.csv(resultsMatch,paste(originPath, "result_baseline_bench.csv", sep = ""), row.names = FALSE)

fullData1 = baseLineLV(fullData)
nrow(fullData1[fullData1$matched, ])

duplicateEntries = duplicateTupleIDs(fullData1)
nameVec = substr(colnames(acmFiltered), 3, nchar(acmFiltered))
colnames(acmFiltered) = nameVec
colnames(dblpFiltered) = nameVec
resolvedEntities = rbind(acmFiltered, dblpFiltered)
resolvedEntities = resolvedEntities[ !(resolvedEntities$id %in% duplicateEntries),]

write.csv(fullData1[,c("d_id", "a_id", "Sim","matched")], paste(originPath, "matched_Entities_Baseline.csv", sep = ""), row.names = FALSE)

resultSum = c("baseline", nrow(fullData), "without", "Levenshtein - untuned", nrow(fullData1[fullData1$matched, ]), mean(resultsMatch$time), length(duplicateEntries))
resultOverview[1,] = resultSum

#-------------------------------------------------------------------------------
#baseline - tuned
#-------------------------------------------------------------------------------

#make all columns containing text string into lower case and remove special characters and white spaces


dblpFiltered = read.csv(dblpPAth)
acmFiltered = read.csv(acmPath)


dblpFiltered[, c("title","authors","venue","abstract")] = apply(dblpFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x))
acmFiltered[, c("title","authors","venue","abstract")] = apply(acmFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x))

dblpFiltered$id = paste("d", 1:nrow(dblpFiltered),sep = "_")
colnames(dblpFiltered) = paste("d", colnames(dblpFiltered),sep = "_")

acmFiltered$id = paste("a",1:nrow(acmFiltered), sep = "_")
colnames(acmFiltered) = paste("a", colnames(acmFiltered), sep = "_")

fullData = merge(dblpFiltered, acmFiltered)

baseLineLV = function(fullData1){
  fullData1[, "Sim"] = stringsim(fullData1$d_title, fullData1$a_title, method = "lv")
  #fullData1[, "lvPerc"] = fullData1$lvDist / max(nchar(fullData1$d_title), nchar(fullData1$a_title))
  fullData1[, "matched"] = fullData1$Sim >= 0.95
  #fullData = fullData1
  #fullData[, c("clustNo", "refersTo")] = NA
  fullData1 = clusterMatched(fullData1[fullData1$matched, ])
  #fullData1 = rbind(fullData1, fullData[!fullData$matched, ])
  return(fullData1)
}

#microbenchmark results are in Nanoseconds
resultsMatch = microbenchmark(baseLineLV(fullData), times = 10)
#print(resultsMatch, unit = "ns")
write.csv(resultsMatch,paste(originPath, "result_baseline_tuned_bench.csv", sep = ""), row.names = FALSE)

fullData1 = baseLineLV(fullData)
nrow(fullData1[fullData1$matched, ])

duplicateEntries = duplicateTupleIDs(fullData1)
nameVec = substr(colnames(acmFiltered), 3, nchar(acmFiltered))
colnames(acmFiltered) = nameVec
colnames(dblpFiltered) = nameVec
resolvedEntities = rbind(acmFiltered, dblpFiltered)
resolvedEntities = resolvedEntities[ !(resolvedEntities$id %in% duplicateEntries),]

write.csv(fullData1[,c("d_id", "a_id", "Sim","matched")], paste(originPath, "matched_Entities_Baseline_tuned.csv", sep = ""), row.names = FALSE)

resultSum = c("baseline-tuned", nrow(fullData), "without", "Levenshtein", nrow(fullData1[fullData1$matched, ]), mean(resultsMatch$time), length(duplicateEntries))
resultOverview[2,] = resultSum

#-------------------------------------------------------------------------------
# blocking by year and levenshtein distance tuned
#-------------------------------------------------------------------------------
dblpFiltered = read.csv(dblpPAth)
acmFiltered = read.csv(acmPath)

dblpFiltered[, c("title","authors","venue","abstract")] = apply(dblpFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x))
acmFiltered[, c("title","authors","venue","abstract")] = apply(acmFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x))

dblpFiltered$id = paste("d", 1:nrow(dblpFiltered),sep = "_")
colnames(dblpFiltered) = paste("d", colnames(dblpFiltered),sep = "_")

acmFiltered$id = paste("a",1:nrow(acmFiltered), sep = "_")
colnames(acmFiltered) = paste("a", colnames(acmFiltered), sep = "_")

fullData = merge(dblpFiltered, acmFiltered)

pipeline1 = function(){
  fullData[, "yearDiff"] = abs(fullData$d_year - fullData$a_year)
  blockedData = fullData[fullData$yearDiff <= 3,]
  blockedData[, "Sim"] = stringsim(blockedData$d_title, blockedData$a_title, method = "lv")
  blockedData[, "matched"] = blockedData$Sim >= 0.95
  #fullData = blockedData
  #fullData[, c("clustNo", "refersTo")] = NA
  blockedData = clusterMatched(blockedData[blockedData$matched,])
  #blockedData = rbind(blockedData, fullData[!fullData$matched, ])
  return(blockedData)
}

resultsMatch = microbenchmark(pipeline1(), times = 10)
write.csv(resultsMatch,paste(originPath, "result_pipeline1_bench.csv", sep = ""), row.names = FALSE)
#write.csv(resultsMatch,paste(originPath, "result_pipeline1_bench.csv", sep = ""), row.names = FALSE)

fullData1 = pipeline1()

duplicateEntries = duplicateTupleIDs(fullData1)
nameVec = substr(colnames(acmFiltered), 3, nchar(acmFiltered))
colnames(acmFiltered) = nameVec
colnames(dblpFiltered) = nameVec
resolvedEntities = rbind(acmFiltered, dblpFiltered)
resolvedEntities = resolvedEntities[ !(resolvedEntities$id %in% duplicateEntries),]

write.csv(fullData1[,c("d_id", "a_id", "Sim","matched")], paste(originPath, "matched_Entities_pipeline1.csv", sep = ""), row.names = FALSE)
#write.csv(fullData1[,c("d_id", "a_id", "lvSim","matched")], paste(originPath, "matched_Entities_pipeline1.csv", sep = ""), row.names = FALSE)

resultSum = c("pipeline1", nrow(fullData1), "by yearDiff", "Levenshtein title", nrow(fullData1[fullData1$matched, ]), mean(resultsMatch$time), length(duplicateEntries))
resultOverview[3,] = resultSum

#-------------------------------------------------------------------------------
# blocking by year and jaccard - distance for trigrams
#-------------------------------------------------------------------------------
dblpFiltered = read.csv(dblpPAth)
acmFiltered = read.csv(acmPath)

dblpFiltered[, c("title","authors","venue","abstract")] = apply(dblpFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x, whiteSpace = FALSE))
acmFiltered[, c("title","authors","venue","abstract")] = apply(acmFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x, whiteSpace = FALSE))

dblpFiltered$id = paste("d", 1:nrow(dblpFiltered),sep = "_")
colnames(dblpFiltered) = paste("d", colnames(dblpFiltered),sep = "_")

acmFiltered$id = paste("a",1:nrow(acmFiltered), sep = "_")
colnames(acmFiltered) = paste("a", colnames(acmFiltered), sep = "_")

fullData = merge(dblpFiltered, acmFiltered)



pipeline2 = function(){
  fullData[, "yearDiff"] = abs(fullData$d_year - fullData$a_year)
  blockedData = fullData[fullData$yearDiff <= 1,]
  blockedData[, "Sim"] = stringsim(blockedData$d_title, blockedData$a_title, method = "jaccard", q = 3)
  blockedData[, "matched"] = blockedData$Sim >= 0.7
  #fullData = blockedData
  #fullData[, c("clustNo", "refersTo")] = NA
  blockedData = clusterMatched(blockedData[blockedData$matched,])
  #blockedData = rbind(blockedData, fullData[!fullData$matched, ])
  return(blockedData)
}

resultsMatch = microbenchmark(pipeline2(), times = 10)
write.csv(resultsMatch, paste(originPath, "result_pipeline2_bench.csv", sep = ""), row.names = FALSE)

fullData1 = pipeline2()

duplicateEntries = duplicateTupleIDs(fullData1)
nameVec = substr(colnames(acmFiltered), 3, nchar(acmFiltered))
colnames(acmFiltered) = nameVec
colnames(dblpFiltered) = nameVec
resolvedEntities = rbind(acmFiltered, dblpFiltered)
resolvedEntities = resolvedEntities[ !(resolvedEntities$id %in% duplicateEntries),]

#hist(fullData1$Sim)
#nrow(fullData1[fullData1$matched, ])

#similar = fullData1[fullData1$matched, c("d_id","d_title", "a_id","a_title", "Sim", "matched")]


write.csv(fullData1[,c("d_id", "a_id", "Sim","matched")], paste(originPath, "matched_Entities_pipeline2.csv", sep = ""), row.names = FALSE)

resultSum = c("pipeline2", nrow(fullData1), "by yearDiff", "jaccard trigram title", nrow(fullData1[fullData1$matched, ]), mean(resultsMatch$time), length(duplicateEntries))
resultOverview[4,] = resultSum


#-------------------------------------------------------------------------------
#blocking by year and venue, jaccard trigram similarity measure on title
#-------------------------------------------------------------------------------
dblpFiltered = read.csv(dblpPAth)
acmFiltered = read.csv(acmPath)

dblpFiltered[, c("title","authors","venue","abstract")] = apply(dblpFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x, whiteSpace = FALSE))
acmFiltered[, c("title","authors","venue","abstract")] = apply(acmFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x, whiteSpace = FALSE))

dblpFiltered$id = paste("d", 1:nrow(dblpFiltered),sep = "_")
colnames(dblpFiltered) = paste("d", colnames(dblpFiltered),sep = "_")

acmFiltered$id = paste("a",1:nrow(acmFiltered), sep = "_")
colnames(acmFiltered) = paste("a", colnames(acmFiltered), sep = "_")

fullData = merge(dblpFiltered, acmFiltered)

pipeline3 = function(){
  fullData[, "yearDiff"] = abs(fullData$d_year - fullData$a_year)
  blockedData = fullData[fullData$yearDiff <= 1,]
  blockedData = blockedData[
    (grepl("sigmod",x = blockedData$d_venue, ignore.case = TRUE) & grepl("sigmod",x = blockedData$a_venue, ignore.case = TRUE)) | 
    (grepl("vldb",x = blockedData$d_venue, ignore.case = TRUE) & grepl("vldb",x = blockedData$a_venue, ignore.case = TRUE))  ,]
  blockedData[, "Sim"] = stringsim(blockedData$d_title, blockedData$a_title, method = "jaccard", q = 3)
  blockedData[, "matched"] = blockedData$Sim >= 0.7
  #fullData = blockedData
  #fullData[, c("clustNo", "refersTo")] = NA
  blockedData = clusterMatched(blockedData[blockedData$matched,])
  #blockedData = rbind(blockedData, fullData[!fullData$matched, ])
  return(blockedData)
}



resultsMatch = microbenchmark(pipeline3(), times = 10)
write.csv(resultsMatch, paste(originPath, "result_pipeline2_bench.csv", sep = ""), row.names = FALSE)

fullData1 = pipeline3()

duplicateEntries = duplicateTupleIDs(fullData1)
nameVec = substr(colnames(acmFiltered), 3, nchar(acmFiltered))
colnames(acmFiltered) = nameVec
colnames(dblpFiltered) = nameVec
resolvedEntities = rbind(acmFiltered, dblpFiltered)
resolvedEntities = resolvedEntities[ !(resolvedEntities$id %in% duplicateEntries),]

#hist(fullData1$Sim)
#nrow(fullData1[fullData1$matched, ])

#similar = fullData1[fullData1$matched, c("d_id","d_title", "a_id","a_title", "Sim", "matched")]


write.csv(fullData1[,c("d_id", "a_id", "Sim","matched")], paste(originPath, "matched_Entities_pipeline3.csv", sep = ""), row.names = FALSE)

resultSum = c("pipeline3", nrow(fullData1), "yearDiff & venue", "jaccard trigram title", nrow(fullData1[fullData1$matched, ]), mean(resultsMatch$time), length(duplicateEntries))
resultOverview[5,] = resultSum

#-------------------------------------------------------------------------------
#blocking by year and venue, jaccard trigram similarity measure on title and author
#-------------------------------------------------------------------------------
dblpFiltered = read.csv(dblpPAth)
acmFiltered = read.csv(acmPath)

dblpFiltered[, c("title","authors","venue","abstract")] = apply(dblpFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x, whiteSpace = FALSE))
acmFiltered[, c("title","authors","venue","abstract")] = apply(acmFiltered[, c("title","authors","venue","abstract")], MARGIN = 2, FUN = function(x) normText(x, whiteSpace = FALSE))

dblpFiltered$id = paste("d", 1:nrow(dblpFiltered),sep = "_")
colnames(dblpFiltered) = paste("d", colnames(dblpFiltered),sep = "_")

acmFiltered$id = paste("a",1:nrow(acmFiltered), sep = "_")
colnames(acmFiltered) = paste("a", colnames(acmFiltered), sep = "_")

fullData = merge(dblpFiltered, acmFiltered)

pipeline4 = function(){
  fullData[, "yearDiff"] = abs(fullData$d_year - fullData$a_year)
  blockedData = fullData[fullData$yearDiff <= 1,]
  blockedData = blockedData[
    (grepl("sigmod",x = blockedData$d_venue, ignore.case = TRUE) & grepl("sigmod",x = blockedData$a_venue, ignore.case = TRUE)) | 
      (grepl("vldb",x = blockedData$d_venue, ignore.case = TRUE) & grepl("vldb",x = blockedData$a_venue, ignore.case = TRUE))  ,]
  blockedData[, "SimTitle"] = stringsim(blockedData$d_title, blockedData$a_title, method = "jaccard", q = 3)
  blockedData[, "SimAuthors"] = stringsim(blockedData$d_authors, blockedData$a_authors, method = "jaccard", q = 3)
  blockedData = add_column(blockedData, Sim = ifelse(is.na(blockedData$SimAuthors), blockedData$SimTitle, (blockedData$SimTitle + blockedData$SimAuthors)/2)) 
  blockedData[, "matched"] = blockedData$Sim >= 0.7
  #fullData = blockedData
  #fullData[, c("clustNo", "refersTo")] = NA
  blockedData = clusterMatched(blockedData[blockedData$matched,])
  #blockedData = rbind(blockedData, fullData[!fullData$matched, ])
  return(blockedData)
}

resultsMatch = microbenchmark(pipeline4(), times = 10)
write.csv(resultsMatch, paste(originPath, "result_pipeline4_bench.csv", sep = ""), row.names = FALSE)

fullData1 = pipeline4()

duplicateEntries = duplicateTupleIDs(fullData1)
nameVec = substr(colnames(acmFiltered), 3, nchar(acmFiltered))
colnames(acmFiltered) = nameVec
colnames(dblpFiltered) = nameVec
resolvedEntities = rbind(acmFiltered, dblpFiltered)
resolvedEntities = resolvedEntities[ !(resolvedEntities$id %in% duplicateEntries),]

#hist(fullData1$Sim)
#nrow(fullData1[fullData1$matched, ])

#similar = fullData1[fullData1$matched, c("d_id","d_title", "a_id","a_title", "Sim", "matched")]


write.csv(fullData1[,c("d_id", "a_id", "Sim","matched")], paste(originPath, "matched_Entities_pipeline4.csv", sep = ""), row.names = FALSE)

resultSum = c("pipeline4", nrow(fullData1), "yearDiff & venue", "jaccard trigram title & author", nrow(fullData1[fullData1$matched, ]), mean(resultsMatch$time), length(duplicateEntries))
resultOverview[6,] = resultSum

#-------------------------------------------------------------------------------
write.csv(resultOverview, paste(originPath, "resultOverview.csv",sep = ""), row.names = F)

