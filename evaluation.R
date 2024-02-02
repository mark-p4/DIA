library(Metrics)

laptop= "C:\\Users\\User\\Dropbox\\PC\\Documents\\Uni\\DIA\\files\\unpacked\\"
pc = "C:\\Users\\Mark\\Dropbox\\PC\\Documents\\Uni\\DIA\\files\\unpacked\\"
originPath = mainPath

files = list.files(pc)
baselineFile = "matched_Entities_Baseline.csv"
files = subset(files, startsWith(files, "matched_Entities") & !startsWith(files, baselineFile))

colScoreDF = c("baselineExp", "predictionExp", "precision", "recall", "f1-Score")
scoreDF = data.frame(matrix(ncol = length(colScoreDF), nrow = 5))
colnames(scoreDF) = colScoreDF

baseline = read.csv(paste(pc, baselineFile, sep = ""))
baseline = add_column(baseline, key = paste(baseline$d_id, baseline$a_id, sep = "-"))
for (i in 1:length(files)) {
  baseline1 = baseline
  fileName = substr(files[i], nchar("matched_Entities_")+1, nchar(files[i])-4)
  toCompare = read.csv(paste(pc, files[i], sep =""))
  toCompare = add_column(toCompare, key = paste(toCompare$d_id, toCompare$a_id ,sep = "-"))
  newDF = left_join(baseline, toCompare, "key")
  newDF[, "matched.y"] = if_else(is.na(newDF$matched.y), F, newDF$matched.y)
  
  prec = Metrics::precision(newDF$matched.x, predicted = newDF$matched.y)
  rec = Metrics::recall(newDF$matched.x, predicted = newDF$matched.y)
  f1 = Metrics::f1(newDF$matched.x, predicted = newDF$matched.y)
  
  scoreDF[i,] = c("baseline - untuned", fileName, prec, rec, f1)
}

write.csv(scoreDF, paste(originPath, "scoring.csv", sep = ""), row.names = F)

files = list.files(pc)
baselineFile = "matched_Entities_Baseline_tuned.csv"
files = subset(files, startsWith(files, "matched_Entities_p"))

colScoreDF = c("baselineExp", "predictionExp", "precision", "recall", "f1-Score")
scoreDF = data.frame(matrix(ncol = length(colScoreDF), nrow = 4))
colnames(scoreDF) = colScoreDF

baseline = read.csv(paste(pc, baselineFile, sep = ""))
baseline = add_column(baseline, key = paste(baseline$d_id, baseline$a_id, sep = "-"))
for (i in 1:length(files)) {
  baseline1 = baseline
  fileName = substr(files[i], nchar("matched_Entities_")+1, nchar(files[i])-4)
  toCompare = read.csv(paste(pc, files[i], sep =""))
  toCompare = add_column(toCompare, key = paste(toCompare$d_id, toCompare$a_id ,sep = "-"))
  newDF = left_join(baseline, toCompare, "key")
  newDF[, "matched.y"] = if_else(is.na(newDF$matched.y), F, newDF$matched.y)
  
  prec = Metrics::precision(newDF$matched.x, predicted = newDF$matched.y)
  rec = Metrics::recall(newDF$matched.x, predicted = newDF$matched.y)
  f1 = Metrics::f1(newDF$matched.x, predicted = newDF$matched.y)
  
  scoreDF[i,] = c("baseline", fileName, prec, rec, f1)
}

write.csv(scoreDF, paste(originPath, "scoring_baseline_tuned.csv", sep = ""), row.names = F)


