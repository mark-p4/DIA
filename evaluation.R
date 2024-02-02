laptop= "C:\\Users\\User\\Dropbox\\PC\\Documents\\Uni\\DIA\\files\\unpacked\\"
pc = "C:\\Users\\Mark\\Dropbox\\PC\\Documents\\Uni\\DIA\\files\\unpacked\\"
originPath = pc

files = list.files(pc)
baselineFile = "matched_Entities_Baseline.csv"
files = subset(files, startsWith(files, "matched_Entities") & !startsWith(files, baselineFile))

baseline = read.csv(paste(pc, baselineFile, sep = ""))

