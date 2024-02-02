library(Metrics)

laptop= "C:\\Users\\User\\Dropbox\\PC\\Documents\\Uni\\DIA\\files\\unpacked\\"
pc = "C:\\Users\\Mark\\Dropbox\\PC\\Documents\\Uni\\DIA\\files\\unpacked\\"
originPath = pc

files = list.files(pc)
baselineFile = "matched_Entities_Baseline.csv"
files = subset(files, startsWith(files, "matched_Entities") & !startsWith(files, baselineFile))

baseline = read.csv(paste(pc, baselineFile, sep = ""))
baseline = add_column(baseline, key = paste(baseline$d_id, baseline$a_id, sep = "-"))
toCompare = read.csv(paste(pc, files[5], sep =""))
toCompare = add_column(toCompare, key = paste(toCompare$d_id, toCompare$a_id ,sep = "-"))

baseline[,"pipe4"] = F

for (i in 1:nrow(toCompare)) {
  baseline[toCompare[i, "key"] == baseline$key, "pipe4"] = toCompare[i, "matched"]
}

newDF = left_join(baseline, toCompare, "key")

Metrics::precision(newDF$matched.x, newDF$pipe4)
Metrics::recall(newDF$matched.x, newDF$pipe4)
Metrics::f1(newDF$matched.x, newDF$pipe4)
