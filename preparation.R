library(stringr)
library(readr)
library(dplyr)
library(tibble)
library(foreach)

categorizeLine = function(line){
  case_when(
    startsWith(line,"#*") ~ "title",
    startsWith(line, "#@") ~ "authors",
    startsWith(line, "#t") ~ "year",
    startsWith(line, "#c") ~ "venue",
    startsWith(line, "#index") ~ "index",
    startsWith(line, "#%") ~ "references",
    startsWith(line, "#!") ~ "abstract",
    .default = NA
  )
}

cleanTags = function(line){
  case_when(
    startsWith(line,"#*") ~ substr(line,3,nchar(line)),
    startsWith(line, "#@") ~ substr(line,3,nchar(line)),
    startsWith(line, "#t") ~ substr(line,3,nchar(line)),
    startsWith(line, "#c") ~ substr(line,3,nchar(line)),
    startsWith(line, "#index") ~ substr(line,7,nchar(line)),
    startsWith(line, "#%") ~ substr(line,3,nchar(line)),
    startsWith(line, "#!") ~ substr(line,3,nchar(line)),
    .default = line
  )
}

laptop= "C:\\Users\\User\\Dropbox\\PC\\Documents\\Uni\\DIA\\files\\unpacked\\"
pc = "C:\\Users\\Mark\\Dropbox\\PC\\Documents\\Uni\\DIA\\files\\unpacked\\"
originPath = mainPath

#create Paths for both files for the assignment
dblpPath = paste(originPath, "dblp.txt", sep="")
acmPath = paste(originPath, "citation-acm-v8.txt", sep="")


dblpTXT = read_delim(dblpPath, delim = "\t", col_names = FALSE)
dblpFrame = data.frame(dblpTXT)

dblpFrame = add_column(dblpFrame, isTitle=startsWith(dblpFrame$X1, "#*"))

dblpFrame = add_column(dblpFrame, id = cumsum(dblpFrame$isTitle))

dblpFrame = data.table::as.data.table(dblpFrame)
dblpFrame = add_column(dblpFrame, value=categorizeLine(dblpFrame$X1))


dblpFrame[,c("X1")] = apply(dblpFrame[,c("X1")], MARGIN = 2, FUN = function(x) ifelse(startsWith(x,"#index"),substr(x,7,nchar(x)),substr(x, 3, nchar(x))))

df = data.table::dcast(dblpFrame, id ~ value, fun.aggregate = function(x) toString(x) ,value.var = "X1")

#there are 2 artifacts where the abstract text seems to not have been within a single line
#manually correcting the abstract for those instances
unique(df$`NA`)
ids = df[nchar(df$`NA`) > 2, "id"]
dblpFrame1 = data.frame(dblpFrame)
dblpFrameArtifact1 = dblpFrame1[dblpFrame1$id == 1588910 & (dblpFrame1$value == "abstract" | is.na(dblpFrame1$value)),]
dblpFrameArtifact2 = dblpFrame1[dblpFrame1$id == 2392422 & (dblpFrame1$value == "abstract" | is.na(dblpFrame1$value)),]
dblpFrameArtifact1Abstract = paste(dblpFrameArtifact1$X1, collapse = " ")
dblpFrameArtifact2Abstract = paste(dblpFrameArtifact2$X1, collapse = " ")
df[1588910,"abstract"] = dblpFrameArtifact1Abstract
df[2392422,"abstract"] = dblpFrameArtifact2Abstract

df = df[,c("id","title","authors","year","venue","index","references","abstract")]

#filter for entries published from 1995 to 2004
df19952004 = df[df$year >= 1995 & df$year <= 2004,]
#check unique values to confirm all values are within given range

dfFiltered = df19952004 %>% filter(grepl("sigmod|vldb",ignore.case = TRUE, df19952004$venue))

write.csv(dfFiltered, file = paste(originPath,"DBLP_1995_2004.csv", sep = ""), row.names = FALSE)

#---------------------------------------------
#acm import and preparation
#---------------------------------------------

#load the dataset
acmTXT = read_delim(acmPath, delim = "\t", col_names = FALSE)
#create a second dataframe to work on
acmFrame = data.frame(acmTXT)

#create a new column with a boolean flag incidating if the line contains a title
acmFrame = add_column(acmFrame, isTitle=startsWith(acmFrame$X1, "#*"))
#create a new index for every line based on the "isTitle"-flag so that every line references one publication
acmFrame = add_column(acmFrame, id = cumsum(acmFrame$isTitle))

#238675 titles found whereas on the aminer.org/citation website it states that 2381688 publication are
#contained in the dataset
sum(acmFrame$isTitle)

#make the dataframe a data table in order to use the dcast method from that library
acmFrame = data.table::as.data.table(acmFrame)
#to prepare the reshaping of the data table every line is categorized based on content
#NOTE: some line containing the abstract where found to not contain an annotation
acmFrame = add_column(acmFrame, value=categorizeLine(acmFrame$X1))

#remove the annotations denoting the category of each line
acmFrame[,c("X1")] = apply(acmFrame[,c("X1")], MARGIN = 2, FUN = function(x) cleanTags(x))

#reshape the table so the every publication has many attributes instead of single lines
dfACM = data.table::dcast(acmFrame, id ~ value, fun.aggregate = function(x) toString(x) ,value.var = "X1")

#there is one instance of an abstract which hast not been transformed correctly
#this issue will be addressed manually for the specitic case
unique(dfACM$`NA`)
acmEntry4 = head(acmFrame, 100)
acmEntry4Abstract = acmEntry4[acmEntry4$id == 4 & ( acmEntry4$value == "abstract" | is.na(acmEntry4$value) ),]
acmEntry4Abstract = paste(acmEntry4Abstract$X1, collapse = " ")

dfACM[4, "abstract"] = acmEntry4Abstract

#remove the NA column, since it does not contain anything relevant anymore
#and order the columns in accordance with the dblp dataset formatted previously
dfACM = dfACM[,c("id","title","authors","year","venue","index","references","abstract")]

#filter all years between 1995 and 2004
dfACM19952004 = dfACM[dfACM$year >= 1995 & dfACM$year <= 2004,]
#confirm all values are within range
unique(dfACM19952004$year)

#entries found with multiple year values "1996, 2013", "2003, 2003"
dfACM[dfACM$year == "1996, 2013"]
#manual confirmation that the first publication is from 1996 (id: 1746206)
#many entries around the entry with id 1746206 seem to be erroneous since the titles are e.g. "Editorial Board" or "Table of Contents"
artifacts = dfACM[dfACM$id >= 1746200 & dfACM$id <= 1746210,]

#redo and lookup original data to investigate issue for id 1746206
acmFrame1 = acmTXT
acmFrame1 = add_column(acmFrame1, isTitle=startsWith(acmFrame1$X1, "#*"))
acmFrame1 = add_column(acmFrame1, id = cumsum(acmFrame1$isTitle))
frameArtifacts = acmFrame1[acmFrame1$id >= 1746150 & acmFrame1$id <= 1746300,]
#entry with id 1746206 is two entries combined and should be either resolved or deleted
#it seems plausible that it could be resolved given the attributes available
#since it will not be considered as it was not published in sigmod or vldb
#it will be left in

#review second conflict
dfACM[dfACM$year == "2003, 2003"]

#lookup original data to investigate issue for id 2178650
frameArtifacts2 = acmFrame1[acmFrame1$id >= 2178600 & acmFrame1$id <= 2178700,]
#entry with id 2178650 is two entries combined and should be either resolved or deleted
#it seems plausible that it could be resolved given the attributes available
#since it will not be considered as it was not published in sigmod or vldb
#it will be left in

dfACMFiltered = dfACM19952004 %>% filter(grepl("sigmod|vldb",ignore.case = TRUE, dfACM19952004$venue))

write.csv(dfACMFiltered, file = paste(originPath,"ACM_1995_2004.csv", sep = ""), row.names = FALSE)
