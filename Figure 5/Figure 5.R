library(ggplot2)
sig  <- read.csv("edgeR_diff_p0.01_total.csv",header = T,sep=",")
ggplot(data = sig, mapping = aes(x = name, y = logFC,color=type)) + 
  geom_point()+
  theme_bw()+
  facet_wrap(~ group,nrow=3)+
  scale_color_manual(values=c("#2b83ba","#d7191c"),breaks = c("know","novel"))+
  theme(axis.text.x = element_text(color = 'black',size = 8,angle = 45, hjust = 1),
        axis.title.y = element_text(face = 'bold',color = 'black',size = 10,vjust=2),
        axis.title.x = element_text(face = 'bold',color = 'black',size = 10,vjust = 0),
        axis.text.y = element_text(color = 'black',size = 8),
        panel.grid = element_blank(),
        legend.position = 'top')+
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed")
