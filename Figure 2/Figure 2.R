library(ggplot2)

#Figure 2B
KO <- read.csv('UHGP_KO.csv',header=T)
names(KO) <- c('group','functions','number','ratio')
KO$functions <- factor(KO$functions,levels = c("Metabolism","Genetic Information Processing","Environmental Information Processing","Cellular Processes","Organismal Systems","Human Diseases"))
ggplot(KO,aes(functions,ratio,fill=group))+
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values =c("coverage"="#377eb8","uncoverage"="#4daf4a"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))

#Figure 2C
pathway_compleness <- read.csv("pathway_compleness-2.csv", row.names=NULL)
names(pathway_compleness) <- c('rank','Module','compleness','group','anno','func')
pathway_compleness$group <- factor(pathway_compleness$group,levels = c('UHGP_ad','Lach'),ordered=TRUE)
ggplot(pathway_compleness, aes(x=rank, y=compleness,fill=group)) +
  geom_bar(stat="identity",position = 'stack', width = 0.7) +
  scale_fill_manual(values =c("Lach"="#377eb8","UHGP_ad"="#4daf4a"))+
  theme_bw()+
  facet_grid(facets='.~func',scales='free_x',space = 'free_x',drop=TRUE)+
  theme(axis.title.y = element_text(face = 'bold',color = 'black',size = 14),
        axis.title.x = element_text(face = 'bold',color = 'black',size = 14,vjust = 0),
        axis.text.y = element_text(color = 'black',size = 11),
        axis.text.x = element_text(color = 'black',size = 11,hjust=1), 
        #panel.grid = element_blank(),
        legend.position = 'right',
        legend.text = element_text(size = 10))+
  labs(y="Compleness")
