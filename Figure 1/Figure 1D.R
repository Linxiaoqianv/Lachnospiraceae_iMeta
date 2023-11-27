library(ggplot2)
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
