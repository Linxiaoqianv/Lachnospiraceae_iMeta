library(ggplot2)

#Figure 2B
KO <- read.csv('UHGP_KO.csv',header=T)
names(KO) <- c('group','functions','number','ratio')
KO$functions <- factor(KO$functions,levels = c("Metabolism","Genetic Information Processing","Environmental Information Processing","Cellular Processes","Organismal Systems","Human Diseases"))
KO$group <- factor(KO$group,levels = c("coverage","uncoverage"),labels = c("Covered by Lachnospiraceae","Not covered by Lachnospiraceae"))
ggplot(KO,aes(functions,ratio*100,fill=group))+
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values =c("Covered by Lachnospiraceae"="#377eb8","Not covered by Lachnospiraceae"="#4daf4a"))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.title=element_blank())+
  labs(y="Coverage (%)")

#Figure 2C
pathway_compleness <- read.csv("pathway_compleness-2.csv", row.names=NULL)
names(pathway_compleness) <- c('rank','Module','compleness','group','anno','func')
pathway_compleness$group <- factor(pathway_compleness$group,levels = c('UHGP_ad','Lach'),ordered=TRUE)
ggplot(pathway_compleness, aes(x=rank, y=compleness,fill=group)) +
  geom_bar(stat="identity",position = 'stack', width = 0.7) +
  scale_fill_manual(values =c("Lach"="#377eb8","UHGP_ad"="#4daf4a"))+
  theme_bw()+
  facet_grid(facets='.~func',scales='free_x',space = 'free_x',drop=TRUE)+
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(face = 'bold',color = 'black',size = 14),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = 'black',size = 11),
        axis.text.x = element_blank(), 
        strip.background = element_rect(fill="white"))+
  guides(fill = "none")+
  labs(y="Compleness (%)")
