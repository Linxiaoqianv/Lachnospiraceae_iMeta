library(ggplot2)
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
