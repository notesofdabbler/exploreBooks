
# load libraries
library(dplyr)
library(tidyr)

# load data
load(file = "bookinfodf.Rda") # info on books
load(file = "recobookinfodf.Rda") # info on recommended books

head(data.frame(bookinfodf))
head(data.frame(recobookinfodf))

# Remove titles not related to R
RinTitle = grepl("^R .*$|^.* R$| R ",bookinfodf$title)
statsSeries = series_srt$series[grep("Statistics",series_srt$series)]
statsSeries
seriesOfInterest = c("Use R!",statsSeries)

incbook = RinTitle | (bookinfodf$series %in% seriesOfInterest)

# keep only R or stats relevant books
bookinfodf_filt = bookinfodf[incbook,]
recobookinfodf_filt = inner_join(recobookinfodf,bookinfodf_filt[,"id"],by = "id")

# check dropped books
data.frame(bookinfodf[!incbook,"title"])

downloads_srt = bookinfodf_filt %>% arrange(desc(downloads)) %>% select(title,downloads)
data.frame(downloads_srt[1:50,])

table(bookinfodf$pubyr)
data.frame(bookinfodf[bookinfodf$pubyr >= 2016,])

series_srt = bookinfodf %>% group_by(series) %>% summarize(seriescnt = n()) %>% arrange(desc(seriescnt))
data.frame(series_srt)

# clustering of book recommendations

recobookinfodf_filt$wt = 9 - recobookinfodf_filt$rnk

# remove duplicates (they are not actually duplicates since they differ in publication year)
# But to keep it simple, we will remove them (there are only couple of them)
recobookinfodf_filt2 = recobookinfodf_filt[!duplicated(recobookinfodf_filt[,c("title","recobooks")]),]
bookinfodf_filt2 = bookinfodf_filt[!duplicated(bookinfodf_filt[,c("title")]),]

names(recobookinfodf_filt2)[names(recobookinfodf_filt2) == "id"] = "titleid"

recobookinfodf_filt2 = inner_join(recobookinfodf_filt2,bookinfodf_filt2[,c("id","title")],by = c("recobooks" = "title"))
names(recobookinfodf_filt2)[names(recobookinfodf_filt2) == "id"] = "recoid"

tmp = recobookinfodf_filt2[,c("titleid","recoid","rnk")]
tmpmaster = expand.grid(titleid = bookinfodf_filt2$id, recoid = bookinfodf_filt2$id)
tmpmaster = left_join(tmpmaster,tmp,by = c("titleid","recoid"))
tmpmaster$rnk[is.na(tmpmaster$rnk) & (tmpmaster$titleid != tmpmaster$recoid)] = 99
tmpmaster$rnk[tmpmaster$titleid == tmpmaster$recoid] = 0

distdf = spread(tmpmaster[,c("titleid","recoid","rnk")],recoid,rnk)

distmat = as.matrix(distdf[,2:ncol(distdf)])
rownames(distmat) = distdf$titleid

distmat2 = as.dist(distmat)
hc = hclust(distmat2,method = "ward.D2")

clusters = cutree(hc,k=20)

plot(hc)

table(clusters)

bookinfodf_filt2$title[bookinfodf_filt2$id %in% as.numeric(names(clusters)[clusters == 8])]



edgelist = recobookinfodf_filt2[,c("title","recobooks","wt")]
names(edgelist) = c("from","to","weights")

g = graph_from_data_frame(edgelist,vertices = bookinfodf_filt2$title,directed = FALSE)
x = clusters(g)
x$csize

V(g)[x$csize[x$membership] == 1]

V(g)[(x$csize[x$membership] > 1) & (x$csize[x$membership] <= 10)]


V(g)[x$membership == 9]

delv = V(g)[x$membership != 2]

g1 = delete.vertices(g,delv)
is.connected(g1)

table(degree(g1))
V(g1)[degree(g1) >= 10]

wc1 = cluster_walktrap(g1)

table(wc1$membership)
sum(table(wc1$membership))

plot_dendrogram(wc1)

V(g1)[wc1$membership == 4]

x = page.rank(g1)

tmpNodes = data.frame(name = bookinfodf_filt2$title, group = 1,size = 1)
tmpNodes$name = as.character(tmpNodes$name)
tmpNodes$group = x$membership[tmpNodes$name]
tmpNodes$id = seq(0,nrow(tmpNodes) - 1)

tmpLinks = recobookinfodf_filt2[,c("title","recobooks","wt")]
tmpLinks = inner_join(tmpLinks,tmpNodes[,c("id","name")],by = c("title" = "name"))
names(tmpLinks)[names(tmpLinks) == "id"] = "source"

tmpLinks = inner_join(tmpLinks,tmpNodes[,c("id","name")],by = c("recobooks" = "name"))
names(tmpLinks)[names(tmpLinks) == "id"] = "target"


forceNetwork(Links = tmpLinks, Nodes = tmpNodes,
             Source = "source", Target = "target",
             Value = "wt", NodeID = "name",
             Group = "group", opacity = 0.8,zoom = TRUE,fontSize = 20)
