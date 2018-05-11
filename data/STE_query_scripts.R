library(tidyr)
library(dplyr)
library(gplots)
library(RColorBrewer)
library(ggplot2)
library(viridis)
library(extrafont)
library(ggthemes)
library(d3Network)
library(treemap)
library(maptools)

# setwd("~/Documents/github/marine_ste/")
# map_data_final <- readRDS("data/map_data_final_4_24.rds")
# load("data/STE_Evidence_Map_2_10_2018.RData")

data <- extdata

out_type <- c("Economic well-being","Health","Political empowerment","Social capital","Education","Culture")
int_type <- c("MPA","PES","CBNRM","Certification")

io_counts = matrix(nrow=6, ncol=4)
rownames(io_counts) <- out_type
colnames(io_counts) <-int_type

for (i in int_type){
  for (j in out_type){
    subset <- filter(data, Out_type == j, Int_type == i)
    io_counts[j,i] <- n_distinct(subset$aid)
  }
}

rownames(io_counts) <- out_type
colnames(io_counts) <- int_type
io_counts <- as.data.frame(io_counts)
io_counts$Out_type <- rownames(io_counts)
io_counts <- gather(io_counts,"Int_type","n",1:4)

pdf(file="STE_Int_Outcome_Heatmap_articles_test.pdf", width=11, height=8.5)
ggplot(io_counts, aes(x=Out_type,y=Int_type,fill=n)) +
  geom_tile(color="gray90",size=0.1) +
  geom_text(aes(label=n, color="white")) +
  scale_fill_gradient(low="#f7fbff",high="#084594",name="# Cases",na.value="gray90") +
  coord_equal() +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_line(size=0.4)) +
  theme(axis.text=element_text(size=12)) +
  theme(legend.title=element_text(size=10)) +
  theme(legend.text=element_text(size=10)) +
  theme(legend.title.align=1) +
  theme(legend.position="bottom") +
  theme(legend.key.size=unit(1, "cm")) +
  theme(legend.key.width=unit(1, "cm")) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=12))
dev.off()

##BY STE TYPE AND OUTCOME DOMAIN - HEATMAP

ste_outcomes <- select(data,aid,oid,Int_type,Out_type,STE_domain,STE_econ,STE_social,STE_sex,STE_ethnic,STE_age,STE_occupation,STE_social_org,STE_spatial,STE_temporal_unit,STE_temporal_scale) %>% distinct() 

ste_outcomes2 <- ste_outcomes %>% gather("ste_type","ste_interaction",5:15) %>% arrange(Int_type) %>% filter(ste_interaction != "NA") %>% distinct() %>% filter(!is.na(Out_type)) %>% filter(!is.na(Int_type))

n_distinct(ste_outcomes2$aid)
n_distinct(ste_outcomes2$oid)

ste_outcomes3 <- ste_outcomes2 %>% select(-aid)

ste_types <- c("STE_domain","STE_econ","STE_social","STE_sex","STE_ethnic","STE_age","STE_occupation","STE_social_org","STE_spatial","STE_temporal_unit","STE_temporal_scale")

matrix <- count(ste_outcomes3,Out_type,Int_type,ste_type)
matrix2 <- matrix %>% ungroup() %>% mutate(Int_type=factor(Int_type, levels=c("MPA","CBNRM","Certification")))
matrix2$n <- as.numeric(matrix2$n)

all_poss <- expand.grid(Out_type=unique(matrix2$Out_type),ste_type=unique(matrix2$ste_type),Int_type=unique(matrix2$Int_type))
all_merge <- merge(matrix2,all_poss,by=c("Out_type","ste_type","Int_type"),all=TRUE) %>% arrange(Int_type)

gg <- ggplot(all_merge, aes(x=ste_type,y=Out_type,fill=n)) + 
  geom_tile(color="gray90",size=0.1) +
  geom_text(aes(label=n, color="white")) +
  scale_fill_gradient(low="#f7fbff",high="#084594",name="# Cases",na.value="gray90") +
  coord_equal() +
  facet_wrap(~Int_type, ncol=3) +
  labs(x="STE Type",y=NULL,title="STEs observed in domains of human well-being amongst marine conservation interventions (193 cases in 77 articles)") +
  theme_tufte(base_family="Helvetica") +
  theme(plot.title=element_text(hjust=0)) +
  theme(axis.ticks=element_line(size=0.4)) +
  theme(axis.text=element_text(size=7)) +
  theme(panel.border=element_rect(fill=NA,size=0.1)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(panel.spacing.x=unit(0.5, "cm")) +
  theme(panel.spacing.y=unit(0.5, "cm")) +
  theme(legend.title=element_text(size=6)) +
  theme(legend.text=element_text(size=6)) +
  theme(legend.title.align=1) +
  theme(legend.position="bottom") +
  theme(legend.key.size=unit(0.2, "cm")) +
  theme(legend.key.width=unit(1, "cm")) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size=7))

pdf(file="STE_type_Int_Outcome_Heatmap_5_11.pdf", width=11, height=8.5)
gg
dev.off()

#############################################################################

##Plot countries
#load in full country list
country <- read.csv("data/allcountries.csv", head=TRUE, sep=",")
names(country) <- c("Study_country", "Region", "Code", "Subregion")
regions <- country
regions <- arrange(regions,Region)

##Count number of studies for all countries and arrange by region
country_count <- matrix(nrow=nrow(regions), ncol=2)
rownames(country_count) <- regions$Study_country
colnames(country_count) <- c("Study_country", "counts")
#Calculate in for loop and write to blank matrix
for (c in regions$Study_country){
  subset <- filter(data, Study_country.x == c)
  country_count[c,1] <- c
  country_count[c,2] <- as.numeric(n_distinct(subset$aid))
}
#Remove rownames and reformat data types
rownames(country_count) = NULL
country_count <- as.data.frame(country_count, stringsAsFactors=FALSE)
countries_only <- inner_join(country_count,regions,by="Study_country")
countries_only <- filter(countries_only, Code != "")

countries_only$counts <- as.numeric(countries_only$counts)
countries_only <- as.data.frame(countries_only)
countries_zero <- filter(countries_only, counts == 0)

map <- readShapeSpatial("data/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
plot(map)
map <- fortify(map, region="ISO3")

pdf(file="STE_Country_Map_dark_5_11.pdf", width=16, height=8.5)
ggplot() + 
  geom_map(data=countries_only, aes(map_id=Code, fill=counts),map=map) + 
  geom_map(data=countries_zero, aes(map_id=Code),fill="#f0f0f0",map=map) + 
  expand_limits(x=map$long,y=map$lat) + 
  theme(panel.background = element_rect(fill = "darkgray", colour = "darkgray"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_gradient2(low="#f7fbff",mid="#6baed6",high="#08306b",midpoint=(max(countries_only$counts)/2),limits=c(0,max(countries_only$counts)))
dev.off()

########
## Trying with chord diagram instead
library(circlize)
library(reshape)

###########################
##FOR MPAS
###########################
syn <- filter(mpa_outcomes,STE_domain == "Synergy") %>% arrange(Out_type,aid)
tradeoff <- filter(mpa_outcomes,STE_domain == "Tradeoff") %>% arrange(Out_type,aid)
both <- filter(mpa_outcomes,STE_domain == "Both") %>% arrange(Out_type,aid)

#Create input for tradeoffs
aid <- as.list(unique(tradeoff$aid))

links <- matrix(nrow=1,ncol=3)
colnames(links) <- c("source","target","aid")
links <- as.data.frame(links)

for(i in aid){
  sub <- filter(tradeoff, aid == i)
  if(nrow(sub) > 1){
    sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
    colnames(sub2) <- c("source","target")
    sub2$aid <- c(i)
    links <- bind_rows(links,sub2)
  } else 
    links
}

links <- slice(links,-1)

##Count links 
counts <- count(links,source,target)
colnames(counts) <- c("source","target","value") 
counts <- as.data.frame(counts)

##For synergies

aid2 <- as.list(unique(syn$aid))

links2 <- matrix(nrow=1,ncol=3)
colnames(links2) <- c("source","target","aid")
links2 <- as.data.frame(links2)

for(i in aid2){
  sub <- filter(syn, aid == i)
  if(nrow(sub) > 1){
    sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
    colnames(sub2) <- c("source","target")
    sub2$aid <- c(i)
    links2 <- bind_rows(links2,sub2)
  } else 
    links2
}

links2 <- slice(links2,-1)

##Count links 
counts2 <- count(links2,source,target)
colnames(counts2) <- c("source","target","value") 
counts2 <- as.data.frame(counts2)

#Create input for both
aid3 <- as.list(unique(both$aid))

links3 <- matrix(nrow=1,ncol=3)
colnames(links3) <- c("source","target","aid")
links3 <- as.data.frame(links3)

for(i in aid3){
  sub <- filter(both, aid == i)
  if(nrow(sub) > 1){
    sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
    colnames(sub2) <- c("source","target")
    sub2$aid <- c(i)
    links3 <- bind_rows(links3,sub2)
  } else 
    links3
}

links3 <- slice(links3,-1)

##Count links 
counts3 <- count(links3,source,target)
colnames(counts3) <- c("source","target","value") 
counts3 <- as.data.frame(counts3)

grid.col <- c("Economic well-being"="#66c2a5","Social capital"="#fc8d62", "Culture"="#8da0cb", "Political empowerment"="#e78ac3","Education"="#a6d854","Health"="#ffd92f")
pdf(file="STE by Domain - MPA_4_24.pdf",width=11,height=8.5)
par(mfrow=c(1,2),oma=c(0,0,2,0))
chordDiagram(as.data.frame(counts), transparency = 0.3, grid.col=grid.col)
title("Tradeoffs (n=16 articles, x=46 cases)",line=-3.5)
chordDiagram(as.data.frame(counts3), transparency = 0.3, grid.col=grid.col)
title("Both (n=16 articles, x=46 cases)",line=-3.5)
chordDiagram(as.data.frame(counts2), transparency = 0.3, grid.col=grid.col)
title("Synergies (n=26 articles, x=70 cases)",line=-3.5)
mtext("STE by outcome domain type in MPAs",outer=TRUE,cex=1.5)
dev.off()

###########################
##for CBNRM
###########################
syn <- filter(cbnrm_outcomes,STE_domain == "Synergy") %>% arrange(Out_type,aid)
tradeoff <- filter(cbnrm_outcomes,STE_domain == "Tradeoff") %>% arrange(Out_type,aid)
both <- filter(cbnrm_outcomes,STE_domain == "Both")

#Create input for tradeoffs
aid <- as.list(unique(tradeoff$aid))

links <- matrix(nrow=1,ncol=3)
colnames(links) <- c("source","target","aid")
links <- as.data.frame(links)

for(i in aid){
  sub <- filter(tradeoff, aid == i)
  if(nrow(sub) > 1){
    sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
    colnames(sub2) <- c("source","target")
    sub2$aid <- c(i)
    links <- bind_rows(links,sub2)
  } else 
    links
}

links <- slice(links,-1)

##Count links 
counts <- count(links,source,target)
colnames(counts) <- c("source","target","value") 
counts <- as.data.frame(counts)

##For synergies

aid2 <- as.list(unique(syn$aid))

links2 <- matrix(nrow=1,ncol=3)
colnames(links2) <- c("source","target","aid")
links2 <- as.data.frame(links2)

for(i in aid2){
  sub <- filter(syn, aid == i)
  if(nrow(sub) > 1){
    sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
    colnames(sub2) <- c("source","target")
    sub2$aid <- c(i)
    links2 <- bind_rows(links2,sub2)
  } else 
    links2
}

links2 <- slice(links2,-1)

##Count links 
counts2 <- count(links2,source,target)
colnames(counts2) <- c("source","target","value") 
counts2 <- as.data.frame(counts2)

##For both

aid3 <- as.list(unique(syn$aid))

links3 <- matrix(nrow=1,ncol=3)
colnames(links3) <- c("source","target","aid")
links3 <- as.data.frame(links3)

for(i in aid3){
  sub <- filter(syn, aid == i)
  if(nrow(sub) > 1){
    sub3 <- as.data.frame(t(combn(sub$Out_type, 2)))
    colnames(sub3) <- c("source","target")
    sub3$aid <- c(i)
    links3 <- bind_rows(links3,sub3)
  } else 
    links3
}

links3 <- slice(links3,-1)

##Count links 
counts3 <- count(links3,source,target)
colnames(counts3) <- c("source","target","value") 
counts3 <- as.data.frame(counts3)

grid.col <- c("Economic well-being"="#66c2a5","Social capital"="#fc8d62", "Culture"="#8da0cb", "Political empowerment"="#e78ac3","Education"="#a6d854","Health"="#ffd92f")
pdf(file="STE by Domain - CBNRM_4_24.pdf",width=11,height=8.5)
par(mfrow=c(1,2),oma=c(0,0,2,0))
chordDiagram(as.data.frame(counts), transparency = 0.3, grid.col=grid.col)
title("Tradeoffs (n=8 articles, x=18 cases)",line=-3.5)
chordDiagram(as.data.frame(counts2), transparency = 0.3, grid.col=grid.col)
title("Synergies (n=8 articles, x=21 cases)",line=-3.5)
chordDiagram(as.data.frame(counts3), transparency = 0.3, grid.col=grid.col)
title("Both (n=8 articles, x=21 cases)",line=-3.5)
mtext("STE by outcome domain type in CBNRM",outer=TRUE,cex=1.5)
dev.off()

##########
#Treemapping to look at outcome subtypes within outcome domains
pdf(file="Outcome_attr_by_domain_MPA.pdf")
treemap(mpa_count,index=c("Out_type","Out_subtype"),
        vSize="n",
        type="index",
        fontsize.labels = c(15,10),
        fontcolor.labels=c("white","black"),
        fontface.labels=c(2,1),
        bg.labels=c("transparent"),
        align.labels=list(
          c("center","top"),
          c("left","bottom")
        ),
        overlap.labels=0.5,
        inflate.labels=F,
        title="MPAs")
dev.off()
pdf(file="Outcome_attr_by_domain_CBNRM.pdf")
treemap(cbnrm_count,index=c("Out_type","Out_subtype"),
        vSize="n",
        type="index",
        fontsize.labels = c(15,10),
        fontcolor.labels=c("white","black"),
        fontface.labels=c(2,1),
        bg.labels=c("transparent"),
        align.labels=list(
          c("center","top"),
          c("left","bottom")
        ),
        overlap.labels=0.5,
        inflate.labels=F,
        title="CBNRM")
dev.off()

# ###########################################################
# ##d3 Force network - node colors indicate outcome domain, each node is an individual outcome
# 
# #Filter dataset to those that only look at STE by domain
# subset <- filter(data, STE_domain != "NA") %>% distinct()
# mpa_outcomes <- filter(subset, Int_type == "MPA") %>% select(aid,oid,Int_type,Out_type,STE_domain) %>% arrange(aid)
# 
# cbnrm_outcomes <- filter(subset, Int_type == "CBNRM") %>% select(aid,oid,Int_type,Out_type,STE_domain) %>% arrange(aid)
# 
# cert_outcomes <- filter(subset, Int_type == "Certification") %>% select(aid,oid,Int_type,Out_type,STE_domain) %>% arrange(aid)
# 
# #Create input
# aid <- as.list(unique(mpa_outcomes$aid))
# 
# links <- matrix(nrow=1,ncol=3)
# colnames(links) <- c("source","target","aid")
# links <- as.data.frame(links)
# 
# for(i in aid){
#   sub <- filter(mpa_outcomes, aid == i)
#   if(nrow(sub) > 1){
#     sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
#     colnames(sub2) <- c("source","target")
#     sub2$aid <- c(i)
#     links <- bind_rows(links,sub2)
#   } else 
#     links
# }
# 
# links <- slice(links,-1)
# 
# ##Count links 
# counts <- count(links,source,target)
# colnames(counts) <- c("source","target","value") 
# counts <- as.data.frame(counts)
# counts$source=as.numeric(as.factor(counts$source))-1
# counts$target=as.numeric(as.factor(counts$target))-1
# ##Define node IDs
# # nodes <- data.frame(name=out_type,group=c(1:6))
# # nodes <- as.data.frame(nodes)
# 
# nodes <- select(mpa_outcomes,aid,Out_type)
# nodes <- as.data.frame(nodes)
# colnames(nodes) <- c("name","group")
# 
# #Simple network
# simple <- select(links,-aid)
# d3SimpleNetwork(simple, file="SimpleNetwork.html")
# 
# #Force network
# d3ForceNetwork(Links=counts, Nodes=nodes, Source="source",Target="target",Value="value",NodeID="name",Group="group",opacity=0.9,file="ForceNetwork.html")

###########################
##for Certification
###########################
# syn <- filter(cert_outcomes,STE_domain == "Synergy") %>% arrange(Out_type,aid)
# tradeoff <- filter(cert_outcomes,STE_domain == "Tradeoff") %>% arrange(Out_type,aid)
# pos_neu <- filter(cert_outcomes,STE_domain == "Pos/neutral")
# 
# #Create input for tradeoffs
# aid <- as.list(unique(tradeoff$aid))
# 
# links <- matrix(nrow=1,ncol=3)
# colnames(links) <- c("source","target","aid")
# links <- as.data.frame(links)
# 
# for(i in aid){
#   sub <- filter(tradeoff, aid == i)
#   if(nrow(sub) > 1){
#     sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
#     colnames(sub2) <- c("source","target")
#     sub2$aid <- c(i)
#     links <- bind_rows(links,sub2)
#   } else 
#     links
# }
# 
# links <- slice(links,-1)
# 
# ##Count links 
# counts <- count(links,source,target)
# colnames(counts) <- c("source","target","value") 
# counts <- as.data.frame(counts)
# 
# ##For synergies
# 
# aid2 <- as.list(unique(syn$aid))
# 
# links2 <- matrix(nrow=1,ncol=3)
# colnames(links2) <- c("source","target","aid")
# links2 <- as.data.frame(links2)
# 
# for(i in aid2){
#   sub <- filter(syn, aid == i)
#   if(nrow(sub) > 1){
#     sub2 <- as.data.frame(t(combn(sub$Out_type, 2)))
#     colnames(sub2) <- c("source","target")
#     sub2$aid <- c(i)
#     links2 <- bind_rows(links2,sub2)
#   } else 
#     links2
# }
# 
# links2 <- slice(links2,-1)
# 
# ##Count links 
# counts2 <- count(links2,source,target)
# colnames(counts2) <- c("source","target","value") 
# counts2 <- as.data.frame(counts2)
# 
# grid.col <- c("Economic well-being"="#66c2a5","Social capital"="#fc8d62", "Culture"="#8da0cb", "Political empowerment"="#e78ac3","Education"="#a6d854","Health"="#ffd92f")
# pdf(file="STE by Domain - Cert_4_24.pdf",width=11,height=8.5)
# par(mfrow=c(1,2),oma=c(0,0,2,0))
# chordDiagram(as.data.frame(counts), transparency = 0.3, grid.col=grid.col)
# title("Tradeoffs (n=1 articles, x=2 cases)",line=-3.5)
# chordDiagram(as.data.frame(counts2), transparency = 0.3, grid.col=grid.col)
# title("Synergies (n=2 articles, x=4 cases)",line=-3.5)
# mtext("STE by outcome domain type in Certification",outer=TRUE,cex=1.5)
# dev.off()