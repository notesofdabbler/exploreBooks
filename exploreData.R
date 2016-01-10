
# load libraries
library(dplyr)

# load data
load(file = "bookinfodf.Rda") # info on books
load(file = "recobookinfodf.Rda") # info on recommended books

head(data.frame(bookinfodf))

RinTitle = grepl("^R .*$|^.* R$| R ",bookinfodf$title)
statsSeries = series_srt$series[grep("Statistics",series_srt$series)]
seriesOfInterest = c("Use R!",statsSeries)

incbook = RinTitle | (bookinfodf$series %in% seriesOfInterest)

# keep only R or stats relevant books
bookinfodf_filt = bookinfodf[incbook,]

# check dropped books
data.frame(bookinfodf[!incbook,"title"])

downloads_srt = bookinfodf_filt %>% arrange(desc(downloads)) %>% select(title,downloads)
data.frame(downloads_srt[1:50,])

table(bookinfodf$pubyr)
data.frame(bookinfodf[bookinfodf$pubyr >= 2016,])

series_srt = bookinfodf %>% group_by(series) %>% summarize(seriescnt = n()) %>% arrange(desc(seriescnt))
data.frame(series_srt)

