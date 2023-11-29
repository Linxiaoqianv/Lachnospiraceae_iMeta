library(ggplot2)

#Figure 1A
reference<-read.table("genus_16s.csv",header = T,sep=",")
names(reference) <- c('genus','group',"number")
reference$genus <- factor(reference$genus)
reference$group <- factor(reference$group,levels = c('potential-new-species','match_species','potential-new-genus'),ordered=TRUE)
ggplot(reference, aes(x=reorder(genus,number), y=number,fill=group)) +
  geom_col(position = 'stack', width = 0.7) +
  scale_fill_manual(values=c("#a0a0a0","#ffcc66","#54b3e8"),breaks = c('match_species','potential-new-species','potential-new-genus'))+
  theme_bw()+
  coord_flip()+
  theme(axis.title.y = element_text(face = 'bold',color = 'black',size = 14),
        axis.title.x = element_text(face = 'bold',color = 'black',size = 14,vjust = 0),
        axis.text.y = element_text(color = 'black',size = 11),
        axis.text.x = element_text(color = 'black',size = 11,hjust=1), #x轴标签偏???45°，并下降0.5
        #panel.grid = element_blank(),
        legend.position = 'right',
        legend.text = element_text(size = 10))+
  labs(x="Genus",y="The number of genomes")

#Figure 1C
library(rgdal)
library(sf)
country_map <- rgdal::readOGR("./World")
l9 <- rgdal::readOGR("./Line9")
plot(country_map)
plot(l9)
#write.table (country_map, file ="world.txt", sep =",", row.names =TRUE, col.names =TRUE, quote =TRUE)
x <- country_map@data
xs <-data.frame(x,id=seq(1:164)-1)
map1 <-fortify(country_map)
map_data <-plyr::join(map1, xs,by="id", type = "full")[,1:12]

map2 <-fortify(l9)
map2$group<-as.factor(as.numeric(map2$id)+164)
map2$NAME<-rep("L9",199)
map_data2 <-merge(map_data,map2,all = T,sort = FALSE)

data<- read.table("world_num.csv",header = T,sep=",")
names(data) <- c('country','number','ngroup','continent')
data$ngroup <- factor(data$ngroup,levels = c('10','100','226','449','800'),ordered=TRUE)

map_data2 %>%
  left_join(data,by=c("NAME"="country")) -> world_map2
world_map2<-world_map2[order(world_map2$group),]

ggplot(world_map2,aes(x=long,y=lat,group=group))+
geom_polygon(aes(fill=ngroup),color="grey20")+
  scale_fill_manual(values=c("#d9d9d9","#fdd0a2","#fdae6b","#fd8d3c","#e6550d","#a63603"),breaks = c('NA','10','100','226','449','800'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),panel.border = element_rect(fill=NA,color="black", size=1,))

library(scatterpie)
region = read.table("world_region.csv",head=T,sep=",",row.names = 1,as.is = T)
names(region) <- c('number','Animal rumen','Animal other region','Environment','Human GI','Human oral','Human other region','x','y','name')
ggplot()+
  geom_scatterpie(data=region,aes(x=x,y=y,r=log(number)),
                  cols=colnames(region)[2:7],color=NA)+
  scale_fill_manual(values=c("#762a83","#af8dc3","#e7d4e8","#d9f0d3","#7fbf7b","#1b7837"),breaks = c('Animal rumen','Animal other region','Environment','Human oral','Human other region','Human GI'))+
  geom_scatterpie_legend(log(region$number),x=-30,y=100)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),panel.border = element_rect(fill=NA,color="black", size=1,))

#Figure 1D
library(ggpubr)

data.inter2 <-read.csv("jaccard_index_N.csv",header=T,sep = ",")
names(data.inter2) <- c('genus','group','jaccard distance')
data.inter2$group <- factor(data.inter2$group,levels = c("intra","inter"),ordered=TRUE)
theme_set(theme_bw())

ggplot(data.inter2, aes(genus, `jaccard distance`, fill=factor(group))) + 
  geom_boxplot(outlier.size=0.5) + 
  scale_fill_manual(values = c("intra"="yellowgreen","inter"="violetred1"))+
  stat_compare_means(aes(group=group),method = "wilcox.test",label="p.signif",label.y = c(1.01))+
  theme(panel.background = element_rect(colour = "black",size = 0.5),
        axis.title.y = element_text(face = 'bold',color = 'black',size = 11),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = 'black',size = 11),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.position = 'top')