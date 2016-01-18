
options(width = 400)

#
#
#  Explore the R related books scraped from Springer site
#  Specific questions considered:
#  - Find the most downloaded books
#  - Contruct a network of the R related books based on recommended books with each book and explore the network
#        +  Find any disconnected components of the graph
#        +  Find books that have largest degree (most recommended)
# 
#  Note: Some tables are wide and only part of them are showing. You can scroll 
#  to the right to see the remaining part of the table

# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(igraph)
library(networkD3)

# load data
load(file = "bookinfodf.Rda") # info on books
load(file = "recobookinfodf.Rda") # info on recommended books

head(data.frame(bookinfodf))
head(data.frame(recobookinfodf))

paste0("Number of books = ",nrow(bookinfodf))

# Remove titles not related to R
# Criteria (1) R appears in the beginning or end of title or in the middle with spaces around it
# Criteria (2) Series is related to statistics
RinTitle = grepl("^R .*$|^.* R$| R ",bookinfodf$title)

# Series in the book list
seriesList = unique(bookinfodf$series)
statsSeries = seriesList[grep("Statistics",seriesList)]
statsSeries
seriesOfInterest = c("Use R!",statsSeries) # statsSeries doesn't have Use R, so added manually

# keep only R or stats relevant books
incbook = RinTitle | (bookinfodf$series %in% seriesOfInterest)
bookinfodf_filt = bookinfodf[incbook,]
recobookinfodf_filt = inner_join(recobookinfodf,bookinfodf_filt[,"id"],by = "id")

paste0("Number of books after filtering non R books = ",nrow(bookinfodf_filt))

# check dropped books
data.frame(bookinfodf[!incbook,"title"])

# sort books based on number of downloads and show top 50 downloaded books
downloads_srt = bookinfodf_filt %>% arrange(desc(downloads)) %>% select(title,downloads)
#data.frame(downloads_srt[1:50,])
#+ fig.height = 10, fig.width = 10
p = ggplot(data = downloads_srt[1:50,]) + geom_point(aes(x = reorder(title,downloads),y = downloads),size = 5) + 
  geom_segment(aes(x = reorder(title,downloads), y = 0, xend = reorder(title,downloads),yend = downloads)) + 
  xlab("") + 
  theme_bw() + coord_flip()
p

# Number of books published by year
#table(bookinfodf_filt$pubyr)

booksbyyr = bookinfodf_filt %>% group_by(pubyr) %>% summarize(numbooks = n())
#+ fig.height = 5, fig.width = 8
p = ggplot(data = booksbyyr) + geom_bar(aes(x = factor(pubyr),y = numbooks),stat = "identity",fill = "grey") + 
   xlab("") + ylab("# books") + theme_bw()
p

# Upcoming books to be published in 2016 and beyond
data.frame(bookinfodf[bookinfodf$pubyr >= 2016,]) # looks like still non-R books are there in the list


##  exploring book recommendations

# books appearing multiple times due to different editions
dupbooks = unique(bookinfodf_filt$title[duplicated(bookinfodf_filt[,c("title")])])
data.frame(bookinfodf_filt[bookinfodf_filt$title %in% dupbooks,])
# remove the books with latest edition for simplicity
bookinfodf_filt2 = bookinfodf_filt[!(bookinfodf_filt$id %in% c(5,151)),]


# create a wt that is 9 - rank so that higher lower rank corresponds to higher wt
recobookinfodf_filt$wt = 9 - recobookinfodf_filt$rnk
#unique(recobookinfodf_filt[,c("wt","rnk")])

recobookinfodf_filt2 = recobookinfodf_filt[!duplicated(recobookinfodf_filt[,c("title","recobooks")]),]

# keep only recommended books that are part of the original R book list 
# here it is done by merging recobooks with original book list based on recobooks field
recobookinfodf_filt2 = inner_join(recobookinfodf_filt,bookinfodf_filt2[,c("id")],by = c("id"))
names(recobookinfodf_filt2)[names(recobookinfodf_filt2) == "id"] = "titleid"
names(recobookinfodf_filt2)[names(recobookinfodf_filt2) == "title"] = "titlefrom"
recobookinfodf_filt2 = inner_join(recobookinfodf_filt2,bookinfodf_filt2[,c("id","link","title")],by = c("recobooklinks" = "link"))
names(recobookinfodf_filt2)[names(recobookinfodf_filt2) == "id"] = "recoid"
names(recobookinfodf_filt2)[names(recobookinfodf_filt2) == "title"] = "titleto"


# Create an undirected graph based on books <-> recobooks links
edgelist = recobookinfodf_filt2[,c("titlefrom","titleto","wt")]
names(edgelist) = c("from","to","weights")
head(edgelist,10)

g = graph_from_data_frame(edgelist,vertices = bookinfodf_filt2$title,directed = FALSE)

# Find disconnected components
comps = clusters(g)
paste0("Number of disconnected components = ",length(comps$csize))
paste0("Number of titles in each component")
comps$csize

# Find the titles that appear as one off components
names(comps$membership)[comps$csize[comps$membership] == 1]

# Other connected components with few vertices
names(comps$membership)[comps$csize[comps$membership] == 3] # finance
names(comps$membership)[comps$csize[comps$membership] == 4] # econometrics
names(comps$membership)[comps$csize[comps$membership] == 9] # ecology, chemometrics etc.
names(comps$membership)[comps$csize[comps$membership] == 17]


# find degree distribition
table(degree(g))

# find titles with highest degree
# doesn't seem to correlate well with downloads
degree_df = data.frame(title = names(degree(g)),degree = degree(g))
degree_df = inner_join(degree_df,bookinfodf_filt2[,c("title","downloads")],by = "title")
degree_df %>% arrange(desc(degree)) %>% head(20)

#
# for example: R for Marketing Research and Analytics has degree 50 i.e being recommended
# along with several other books. Not sure how Springer reco system works
#
recobookinfodf_filt2[grepl("Marketing Research",recobookinfodf_filt2$titleto),c("titlefrom","titleto")]

# plot the network with networkD3
# code adapted from https://christophergandrud.github.io/networkD3/
pltNodes = data.frame(name = bookinfodf_filt2$title, group = 1,size = 1)
pltNodes$name = as.character(pltNodes$name)
pltNodes$group = comps$membership[pltNodes$name] # group is the connected component number
pltNodes$id = seq(0,nrow(pltNodes) - 1) # networkD3 mentioned something about starting index with 0

pltLinks = recobookinfodf_filt2[,c("titlefrom","titleto","wt")]
pltLinks = inner_join(pltLinks,pltNodes[,c("id","name")],by = c("titlefrom" = "name"))
names(pltLinks)[names(pltLinks) == "id"] = "source"

pltLinks = inner_join(pltLinks,pltNodes[,c("id","name")],by = c("titleto" = "name"))
names(pltLinks)[names(pltLinks) == "id"] = "target"


forceNetwork(Links = pltLinks, Nodes = pltNodes,
             Source = "source", Target = "target",
             Value = "wt", NodeID = "name",
             Group = "group", opacity = 0.8,zoom = TRUE,fontSize = 20)

# This analysis was done in RStudio 0.99.489
sessionInfo()
