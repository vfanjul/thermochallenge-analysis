## Set parameters
project = "Heart progerin" # mir29   Heart progerin   Heart lamin  ISO Challenge
technique = "Cold.challenge" # Cold.challenge Thermochallenge.ECG Cold.challenge.2  Tolerance.Test
channel = "CH2" # CH1 CH2 CH3
maxtime = 10 # for thermochallenge may be 10 min
excludevars = c("Duration (min)",	"MedianBeats",	"TotalBeats",	"RelBeats (%)", "Pabs")
intpolfreq = 15 # interpolation frequency
meth = "natural" # fmm  periodic  natural
factors = c("Id", "Date", "Time")

## Set directories
if (.Platform$OS.type == "unix") setwd("/Volumes/Victor/") else setwd("S:/LAB_VA/LAB/Victor/")
baseroute = paste0(project, " project/Raw data/")
route = paste0(baseroute, technique, "/")
dir.create(paste0(baseroute, technique, ".0/"), showWarnings = F)


## Import and merge files
if (technique == "Thermochallenge.ECG") {
  tempdata = read.delim(paste0(route, "tempdata.txt"), encoding = "latin1", row.names = NULL, stringsAsFactors = F, na.strings = c("N/A", "NA", "#N/A", "", "nan"))
  tempdata$Id = toupper(tempdata$Id)
  templabels = as.character(read.delim(paste0(route, "tempdata.txt"), encoding = "latin1", header = F, row.names = NULL, stringsAsFactors = F, dec = ".", na.strings = "N/A")[1,])
  
  ecgdata = read.delim(paste0(route, "analyzed ", channel, "/rawdata.txt"), encoding = "latin1", row.names = NULL, stringsAsFactors = F, na.strings = c("N/A", "NA", "#N/A", "", "nan"))
  # ecgdata$Min = as.numeric(as.factor(gsub("[[:graph:]]*\\.", "", ecgdata$Id)))-1
  # ecgdata$Time = ecgdata$Time + as.numeric(as.factor(gsub("[[:graph:]]*\\.", "", ecgdata$Id)))
  ecgdata$Time = as.numeric(gsub("[[:graph:]]*\\.", "", ecgdata$Id))
  ecgdata$Id = gsub("\\.[[:graph:]]*", "", ecgdata$Id)
  ecglabels = as.character(read.delim(paste0(route, "analyzed ", channel, "/rawdata.txt"), encoding = "latin1", header = F, row.names = NULL, stringsAsFactors = F, dec = ".", na.strings = "N/A")[1,])
  ecgdata = ecgdata[,-sapply(excludevars, function (x) which(ecglabels == x))]
  ecglabels = ecglabels[-sapply(excludevars, function (x) which(ecglabels == x))]

  rawdata = merge(tempdata, ecgdata, by = factors, all = T)
  rawdata = rawdata[rawdata$Time <= maxtime,]
  ylabels = setdiff(union(templabels, ecglabels), factors)
  
} else if (technique == "Cold.challenge") {
  rawdata = read.delim(paste0(route, technique, ".data.txt"), encoding = "latin1", row.names = NULL, stringsAsFactors = F, na.strings = c("N/A", "NA", "#N/A", "", "nan"))
  rawdata$Id = toupper(rawdata$Id)
  rawdata = rawdata[sapply(1:nrow(rawdata), function (x) !all(is.na(rawdata[x,setdiff(names(rawdata), factors)]))),]
  ylabels = as.character(read.delim(paste0(route, technique, ".data.txt"), encoding = "latin1", header = F, row.names = NULL, stringsAsFactors = F, dec = ".", na.strings = "N/A")[1,])
  ylabels = setdiff(ylabels, factors)
  ## Interpolate missing values
  interpol = c()
  for (Date in unique(rawdata$Date)) {
    # for (Id in unique(rawdata$Id[rawdata$Date == Date])) interpol = rbind.data.frame(interpol, cbind.data.frame(Id, Date, data.frame(spline(rawdata$Time[rawdata$Id == Id & rawdata$Date == Date], rawdata$BAT.temperature...C.[rawdata$Id == Id & rawdata$Date == Date], method = meth, xout = seq(0, max(rawdata$Time), by = intpolfreq)))))
    for (Id in unique(rawdata$Id[rawdata$Date == Date])) {
      interpol = rbind.data.frame(interpol, cbind.data.frame(Id, Date,
                                                             data.frame(spline(rawdata$Time[rawdata$Id == Id & rawdata$Date == Date], rawdata$Body.temperature...C.[rawdata$Id == Id & rawdata$Date == Date], method = meth, xout = seq(0, max(rawdata$Time[rawdata$Id == Id & rawdata$Date == Date]), by = intpolfreq))),
                                                             data.frame(spline(rawdata$Time[rawdata$Id == Id & rawdata$Date == Date], rawdata$BAT.temperature...C.[rawdata$Id == Id & rawdata$Date == Date], method = meth, xout = seq(0, max(rawdata$Time[rawdata$Id == Id & rawdata$Date == Date]), by = intpolfreq)))[,-1]))
    }
  }
  names(interpol) = names(rawdata)[1:length(names(interpol))]
  interpol$Relative.BAT.temperature...C. = interpol$BAT.temperature...C. - interpol$Body.temperature...C.
  rawdata = merge(rawdata, interpol[,c(1:3,6)], by = 1:3, all = T)
  ylabels = c(ylabels, "Relative BAT temperature ( C)")
    # # interpol = merge(interpol, rawdata[,1:5], by = 1:3, all = T)
    # plots_per_col = ifelse(length(unique(interpol$Id)) == 3, 1, round(sqrt(length(unique(interpol$Id))),0))
    # plots_per_row = ifelse(length(unique(interpol$Id)) == 3, 3, ceiling(length(unique(interpol$Id))/plots_per_col))
    # par(mfrow = c(plots_per_col,plots_per_row), bty = "l", pch = 19, cex = 0.5, mar = c(3,3,0,0))
    # for (i in unique(interpol$Id)) {
    #   # plot(BAT.temperature...C. ~ x, interpol[interpol$Id == i,], col = 1)
    #   # lines(y ~ x, interpol[interpol$Id == i,], col = 2)
    #   plot(Relative.BAT.temperature...C. ~ Time, interpol[interpol$Id == i,], xlim = c(0, max(interpol$Time)), ylim = summary(interpol$Relative.BAT.temperature...C.)[c(1,6)], col = 1)
    #   lines(Relative.BAT.temperature...C. ~ Time, interpol[interpol$Id == i,], col = 2)
    #   legend("top", legend = i, bty = "n")
    # }
    # dev.off()
} else if (technique == "Cold.challenge.2" | technique == "Tolerance.Test") {
  rawdata = read.delim(paste0(route, technique, ".data.txt"), encoding = "latin1", row.names = NULL, stringsAsFactors = F, na.strings = c("N/A", "NA", "#N/A", "", "nan"))
  rawdata$Id = toupper(rawdata$Id)
  rawdata = rawdata[sapply(1:nrow(rawdata), function (x) !all(is.na(rawdata[x,setdiff(names(rawdata), factors)]))),]
  ylabels = as.character(read.delim(paste0(route, technique, ".data.txt"), encoding = "latin1", header = F, row.names = NULL, stringsAsFactors = F, dec = ".", na.strings = "N/A")[1,])
  ylabels = setdiff(ylabels, factors)
}


## Calculate values at time 0 and variations
rawdata0 = rawdata[rawdata$Time == 0,-3]
rawdata = merge(rawdata, rawdata0, by = c("Id", "Date"), all = T, suffixes = c("", ".var"))
rawdata[,grep("\\.var", names(rawdata))] = rawdata[,gsub("\\.var", "", grep("\\.var", names(rawdata), value = T))] - rawdata[,grep("\\.var", names(rawdata))]
rawdata = merge(rawdata, rawdata0, by = c("Id", "Date"), all = T, suffixes = c("", ".rel"))
rawdata[,grep("\\.rel", names(rawdata))] = rawdata[,gsub("\\.rel", "", grep("\\.rel", names(rawdata), value = T))] / rawdata[,grep("\\.rel", names(rawdata))] * 100

names(rawdata0) = c(factors[-3], ylabels)
# ylabels = c(ylabels, gsub("\\(", "variation \\(", ylabels), gsub("\\(.*", "variation \\(%\\)", ylabels))
ylabels = c(ylabels, paste("Delta", ylabels), gsub("\\(.*", "\\(%\\)", ylabels))
names(rawdata) = c(factors, ylabels)

## Export files
write.table(rawdata, file = paste0(route, "rawdata.txt"), row.names = F, col.names = T, sep = "\t", append = F)
write.table(rawdata0, file = paste0(baseroute, technique, ".0/rawdata.txt"), row.names = F, col.names = T, sep = "\t", append = F)






