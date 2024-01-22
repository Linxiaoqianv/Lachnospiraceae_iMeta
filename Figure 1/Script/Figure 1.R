library(ggplot2)

#Figure 1A
reference<-read.table("genus_16s.csv",header = T,sep=",")
names(reference) <- c('genus','group',"number","textcolor")
reference$genus <- factor(reference$genus)
reference$group <- factor(reference$group,levels = c('Potential new species','Matched species','Potential new genera'),ordered=TRUE)
ggplot(reference, aes(x=reorder(genus,number), y=number,fill=group)) +
  geom_col(position = 'stack', width = 0.7) +
  scale_fill_manual(values=c("#a0a0a0","#ffcc66","#54b3e8"),breaks = c('Matched species','Potential new species','Potential new genera'))+
  theme_bw()+
  coord_flip()+
  theme(panel.grid=element_blank(),
        axis.title.y = element_text(face = 'bold',color = 'black',size = 14),
        axis.title.x = element_text(face = 'bold',color = 'black',size = 14,vjust = 0),
        axis.text.y = element_text(color = reference$textcolor,size = 11,face="italic"),
        axis.text.x = element_text(color = 'black',size = 11),
        legend.position = c(0.7, 0.2),
        legend.text = element_text(size = 10))+
  labs(x="Genus",y="The number of genomes")+
  guides(fill=guide_legend("Group"))

#Figure 1B
library(reshape2)
library(ggbreak)
data<- read.table("world_num.csv",header = T,sep=",",row.names = 1)
names(data) <- c('Continent','rank','Animal rumen','Animal other region','Environment','Human GI','Human oral','Human other region')
data$Country <- row.names(data)
td <- data[,-1:-2]
td2 <- melt(td,id="Country")
names(td2) <- c('Country','Niches','Number')
td3 <- merge(td2, data[,c(1:2,9)], by.x="Country")
td3$Continent <- factor(td3$Continent, levels = c("Africa","Asia","Europe","North America","Oceania","South America","N.A"),
                  labels = c("Africa","Asia","Europe","North\nAmerica","Oceania","South\nAmerica",""))

ggplot(td3, aes(x=reorder(Country,rank), y=Number,fill=Niches)) +
  geom_col(position = 'stack', width = 0.7) +
  scale_fill_manual(values=c("#762a83","#af8dc3","#e7d4e8","#d9f0d3","#7fbf7b","#1b7837"),breaks = c('Animal rumen','Animal other region','Environment','Human oral','Human other region','Human GI'))+
  theme_bw()+
  theme_classic()+
  facet_grid(~Continent, scales="free_x",space = "free",switch = "x")+
  theme(panel.grid=element_blank(),
        axis.title.y = element_text(angle = 90,color = 'black',size = 14),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = 'black',size = 11),
        axis.text.x = element_text(color = 'black',size = 11,angle = 90, hjust = 1,vjust=.3),
        legend.position="none",
        panel.spacing=unit(1, "mm"),
        strip.text.x = element_text(size=11, color = "black",
                                    vjust = 0.5,margin = margin(b = 3,t=3)),
        strip.placement = "outside",
        strip.background = element_blank()
        )+
  scale_y_break(c(70, 120), scales = .5)+
  labs(x="",y="The number of genomes")

#Figure 1D
library(ggpubr)

data.inter2 <-read.csv("jaccard_index_N.csv",header=T,sep = ",")
names(data.inter2) <- c('genus','group','jaccard distance')
data.inter2$group <- factor(data.inter2$group,levels = c("intra-branches","inter-branches"),ordered=TRUE)
theme_set(theme_bw())

ggplot(data.inter2, aes(genus, `jaccard distance`, fill=factor(group))) + 
  geom_boxplot(outlier.size=0.5) + 
  scale_fill_manual(values = c("intra-branches"="yellowgreen","inter-branches"="violetred1"))+
  stat_compare_means(aes(group=group),method = "wilcox.test",label="p.signif",label.y = c(1.01))+
  theme(panel.grid=element_blank(),
        panel.background = element_rect(colour = "black",size = 0.5),
        axis.title.y = element_text(color = 'black',size = 11),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = 'black',size = 11),
        axis.text.x = element_text(color = 'black',angle = 90,hjust = 1,vjust=.3),
        legend.position = 'top',
        legend.title=element_blank())+
  guides(fill=guide_legend("Group"))
