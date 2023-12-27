library(ggplot2)
library(ggforce)
library(ggpubr)
library(RColorBrewer)

#Figure S1
DDH  <- read.csv("Fig S1.csv",header = T,sep=",")
ggplot(DDH, aes(x=cluster, y=DDH)) +
  theme_bw()+
  coord_flip()+
  stat_boxplot(mapping=aes(x=cluster,y=DDH),
               width=0.15,position=position_dodge(0.8))+
  geom_boxplot(aes(x=cluster,y=DDH),
               position=position_dodge(0.8),
               width=0.6,
               outlier.color = "black")+
  theme(panel.grid=element_blank(),
        axis.title.y = element_text(face = 'bold',color = 'black',size = 12),
        axis.title.x = element_text(face = 'bold',color = 'black',size = 12),
        axis.text.y = element_text(color = 'black',size = 5),
        axis.text.x = element_text(color = 'black',size = 8),
        legend.position = 'right',
        legend.text = element_text(size = 8))+
  geom_hline(aes(yintercept=70), colour="black", linetype="dashed")+
  labs(y="dDDH (%)",x="Cluster ID")

#Figure S2
library(gcookbook)
genome<- read.table("Fig S2.csv",header = T,sep=",",row.names = 1)
genome$group <- factor(genome$group,levels = c('N','A','16Sl','ANIl','Discrete'),
                       labels = c('ANI ≤ 95 and similarity ≤ 98.7','ANI ≥ 95 and similarity ≥ 98.7','ANI ≥ 95 and similarity ≤ 98.7','ANI ≤ 95 and similarity ≥ 98.7','Discrete'))
ggplot(genome, aes(x=ANI, y=X16S,color=group))+ 
  geom_point(alpha=0.5)+
  scale_color_manual(values=c("grey","#e7298a","#1b9e77","#d95f02","#7570b3"),
                     breaks = c('ANI ≤ 95 and similarity ≤ 98.7','ANI ≥ 95 and similarity ≥ 98.7','ANI ≥ 95 and similarity ≤ 98.7','ANI ≤ 95 and similarity ≥ 98.7','Discrete'))+
  theme_bw()+
  geom_hline(aes(yintercept=98.7), colour="black", linetype="dashed")+
  geom_vline(aes(xintercept=95), colour="black", linetype="dashed")+
  theme(axis.text.x = element_text(color = 'black',size = 10),
        axis.title.y = element_text(face = 'bold',color = 'black',size = 14,vjust=2),
        axis.title.x = element_text(face = 'bold',color = 'black',size = 14,vjust = 0),
        axis.text.y = element_text(color = 'black',size = 10),
        legend.text = element_text(face = 'bold',color = 'black',size = 10),
        panel.background = element_rect(colour = "grey"),
        legend.position="top")+
  facet_zoom(xy = group=='ANI ≥ 95 and similarity ≥ 98.7',split = TRUE,zoom.size=1)+
  labs(x="ANI with type strain (%)",y="16S rRNA gene sequence similarity with type strain (%)")

#Figure S3
library(vcd)
#A
data = read.table("Fig S3_cluster.csv",head=T,sep=",",row.names = 1,comment.char="$")
color<-data[,5]
ternaryplot(data[,1:3],cex =0.8,col = color,grid_color = "grey50",labels_color = "black",main ="Cluster source",labels = "outside")
#B
data2 = read.table("Fig S3_genus.csv",head=T,sep=",",row.names = 1,comment.char="$")
color2<-data2[,5]
ternaryplot(data2[,1:3],cex =0.8,col = color2,grid_color = "grey50",labels_color = "black",main ="Genus source",labels = "outside")

#Figure S4B
sig  <- read.csv("Fig S4B.csv",header = T,sep=",")
ggplot() +
  geom_bar(data=sig, aes(x = reorder(Name, LDA), y=LDA,fill=group), stat = "identity")+
  theme_bw()+
  coord_flip()+
  scale_fill_manual(
    values = c('new'="red",'know'="green"))+
  labs(y="LDA score (log 10)")+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title=element_text(hjust=0.5),
        legend.position = 'top',
        legend.title=element_blank())

#Fig S5A
genus<- read.table("Fig S5A.csv",header = T,sep=",",row.names = 1)
genus_pan<- data.frame(genus$pan)
row.names(genus_pan)=rownames(genus)
genus_core <- genus[,-1]
coul1 <- colorRampPalette(brewer.pal(8, "Oranges")[1:7])(25)
coul2 <- colorRampPalette(brewer.pal(8, "Purples")[1:7])(25)
library(ComplexHeatmap)
p1 <-Heatmap(genus_pan,col=coul1,cluster_rows = FALSE,width = unit(1, "cm"),height = unit(32, "cm"),name = "Pan",
             right_annotation = rowAnnotation("Genome number" = row_anno_barplot(genus_core$genome.number,axis = TRUE),"Cluster number"= anno_points(genus_core$cluster.number,ylim = c(0, 40)),width = unit(3, "cm"),gap = unit(1, "mm")),
             row_names_gp = gpar(fontsize = 10),
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf("%0.1f", genus_pan[i, j]/1000), x, y, gp = gpar(fontsize = 8))
             })
p2<-Heatmap(genus_core[,1],col=coul2,cluster_rows = FALSE,width = unit(1, "cm"),height = unit(32, "cm"),name = "Core",
            cell_fun = function(j, i, x, y, width, height, fill) {
              grid.text(sprintf("%0.1f", genus_core[i, j]/10), x, y, gp = gpar(fontsize = 8))
            })
p2+p1

#Fig S5B
data<- read.table("Fig S5B.csv",header = T,sep=",")
names(data) <- c('genus','group',"number")
ggplot(data,aes(x=reorder(genus,-number),y=number,fill=group))+
  geom_bar(position=position_dodge(0.95),stat="identity")+
  scale_fill_manual(values = c("#998ec3", "#f1a340"))+
  theme_bw()+
  theme(legend.position = 'top',
        panel.grid = element_blank(),
        axis.text.x = element_text(color = 'black',size = 8),
        axis.title.y = element_text(face = 'bold',color = 'black',size = 14,vjust=2),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = 'black',size = 8),
        legend.text = element_text(face = 'bold',color = 'black',size = 8),
        panel.background = element_rect(colour = "grey"))+
  scale_y_continuous(breaks = seq(0, 300, by = 50))+
  labs(y="Number of genomes")

#Fig S5C
library(readxl)
library(ggplot2)
library(micropan)
library(tidyr)
library(stringr)
library(reshape2)
library(vegan)
Blautia_A_k <- read.table('Fig S5C_Blautia_A_know.csv',header = T,row.names = 1,sep=",",quote = "")
Blautia_A_k[is.na(Blautia_A_k)]<-0
Blautia_A_k = data.frame(t(Blautia_A_k))
Blautia_A <- read.table('Fig S5C_Blautia_A_1.csv',header = T,row.names = 1,sep=",",quote = "")
Blautia_A[is.na(Blautia_A)] <- 0
Blautia_A = data.frame(t(Blautia_A))
Coprococcus_k <- read.table('Fig S5C_Coprococcus_know.csv',header = T,row.names = 1,sep=",")
Coprococcus_k = data.frame(t(Coprococcus_k))
Coprococcus <- read.table('Fig S5C_Coprococcus.csv',header = T,row.names = 1,sep=",")
Coprococcus = data.frame(t(Coprococcus))
Butyrivibrio_k <- read.table('Fig S5C_Butyrivibrio_know.csv',header = T,row.names = 1,sep=",",quote = "")
Butyrivibrio_k = data.frame(t(Butyrivibrio_k))
Butyrivibrio <- read.table('Fig S5C_Butyrivibrio.csv',header = T,row.names = 1,sep=",",quote = "")
Butyrivibrio = data.frame(t(Butyrivibrio))
Pseudobutyrivibrio_k <- read.table('Fig S5C_Pseudobutyrivibrio_know.csv',header = T,row.names = 1,sep=",",quote = "")
Pseudobutyrivibrio_k = data.frame(t(Pseudobutyrivibrio_k))
Pseudobutyrivibrio <- read.table('Fig S5C_Pseudobutyrivibrio.csv',header = T,row.names = 1,sep=",",quote = "")
Pseudobutyrivibrio = data.frame(t(Pseudobutyrivibrio))
Blautia_k <- read.table('Fig S5C_Blautia_know.csv',header = T,row.names = 1,sep=",",quote = "")
Blautia_k = data.frame(t(Blautia_k))
Blautia <- read.table('Fig S5C_Blautia.csv',header = T,row.names = 1,sep=",",quote = "")
Blautia = data.frame(t(Blautia))

BlAk <- specaccum(Blautia_A_k,method = 'random')
BlA <- specaccum(Blautia_A,method = 'random')
Blk <- specaccum(Blautia_k,method = 'random')
Bl <- specaccum(Blautia,method = 'random')
Cok <- specaccum(Coprococcus_k,method = 'random')
Co <- specaccum(Coprococcus,method = 'random')
Buk <- specaccum(Butyrivibrio_k,method = 'random')
Bu <- specaccum(Butyrivibrio,method = 'random')
Psk <- specaccum(Pseudobutyrivibrio_k,method = 'random')
Ps <- specaccum(Pseudobutyrivibrio,method = 'random')

p1<-plot(x=BlA$sites,y=BlA$richness,type='n')+
  lines(x=BlAk$sites,y=BlAk$richness,col ='#48D1CC',lwd =2,lty=2)+
  lines(x=BlA$sites,y=BlA$richness,col ='#48D1CC',lwd =2,lty=1)+
  lines(x=Blk$sites,y=Blk$richness,col ='#48D1CC',lwd =2,lty=2)+
  lines(x=Bl$sites,y=Bl$richness,col ='#48D1CC',lwd =2,lty=1)

p2<-plot(x=Co$sites,y=Co$richness,type='n')+
  lines(x=Cok$sites,y=Cok$richness,col ='#006400',lwd =2,lty=2)+
  lines(x=Co$sites,y=Co$richness,col ='#006400',lwd =2,lty=1)

p3<-plot(x=Bu$sites,y=Bu$richness,type='n')+
  lines(x=Buk$sites,y=Buk$richness,col ='#40E0D0',lwd =2,lty=2)+
  lines(x=Bu$sites,y=Bu$richness,col ='#40E0D0',lwd =2,lty=1)

p4<-plot(x=Ps$sites,y=Ps$richness,type='n')+
  lines(x=Psk$sites,y=Psk$richness,col ='#E9967A',lwd =2,lty=2)+
  lines(x=Ps$sites,y=Ps$richness,col ='#E9967A',lwd =2,lty=1)
ggarrange(p1,p2,p3,p4,ncol = 3, nrow = 2,align="hv")

#Fig S6A
top10 <- read.table("Fig S6A.csv",sep=",",header = T)
ggplot(top10,mapping = aes(reorder(gene,-num),num,fill=type))+
  geom_bar(stat='identity',position='stack') +
  theme_bw()+
  scale_fill_manual(values=c("#fb8072","#fdb462","#80b1d3"))+
  labs(x = 'Gene',y = 'The number of variations') +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Fig S6B
bceB<- read.table("Fig S6B.csv",sep=",",header = T)
c1 <- ggplot(bceB[1:1104,],aes(x=pos,fill=group,color=group))+
  annotate("rect", xmin = 2941421, 
           xmax = 2943464, 
           ymin = 0, 
           ymax = 1,
           alpha = .2) +
  geom_freqpoly(aes(pos,..count../21),binwidth=1,alpha = 1) + 
  scale_color_manual(values=c("#fc8d62"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_x_continuous(labels = scales::label_comma())+
  labs(x="Genomic position",y="Density")+
  geom_vline(aes(xintercept=2943302), colour="black", linetype="dashed")+
  guides(fill=guide_legend("Clade"),color=guide_legend("Variant type"))
c2 <- ggplot(bceB[1105:1406,],aes(x=pos,fill=group,color=group))+
  annotate("rect", xmin = 2941421, 
           xmax = 2943464, 
           ymin = 0, 
           ymax = 1,
           alpha = .2) +
  geom_freqpoly(aes(pos,..count../8),binwidth=1,alpha = 1) + 
  scale_color_manual(values=c("#66c2a5"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_x_continuous(labels = scales::label_comma())+
  labs(x="Genomic position",y="Density")+
  geom_vline(aes(xintercept=2943302), colour="black", linetype="dashed")+
  guides(fill=guide_legend("Clade"),color=guide_legend("Variant type"))
c3 <- ggplot(bceB[1407:3121,],aes(x=pos,fill=group,color=group))+
  annotate("rect", xmin = 2941421, 
           xmax = 2943464, 
           ymin = 0, 
           ymax = 1,
           alpha = .2) +
  geom_freqpoly(aes(pos,..count../28),binwidth=1,alpha = 1) + 
  scale_color_manual(values=c("#8da0cb"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_x_continuous(labels = scales::label_comma())+
  labs(x="Genomic position",y="Density")+
  geom_vline(aes(xintercept=2943302), colour="black", linetype="dashed")+
  guides(fill=guide_legend("Clade"),color=guide_legend("Variant type"))
c4 <- ggplot(bceB[3122:5257,],aes(x=pos,fill=group,color=group))+
  annotate("rect", xmin = 2941421, 
           xmax = 2943464, 
           ymin = 0, 
           ymax = 1,
           alpha = .2) +
  geom_freqpoly(aes(pos,..count../38),binwidth=1,alpha = 1) + 
  scale_color_manual(values=c("#e78ac3"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_x_continuous(labels = scales::label_comma())+
  labs(x="Genomic position",y="Density")+
  geom_vline(aes(xintercept=2943302), colour="black", linetype="dashed")+
  guides(fill=guide_legend("Clade"),color=guide_legend("Variant type"))
ggarrange(c1,c2,c3,c4,ncol = 1, nrow = 4,align="hv")

#Fig S7
spore <- read.delim('Fig S7.csv', row.names = 1, sep = ',', stringsAsFactors = FALSE, check.names = FALSE)
spore_group <- read.csv("Fig S7_anno.csv",header=T,sep = ",")
names(spore_group) <- c('genome','source')
library(vegan)
distance <- vegdist(spore, method = 'bray')
pcoa <- cmdscale(distance, k = (nrow(spore) - 1), eig = TRUE)
pcoa_eig <- (pcoa$eig)[1:2] / sum(pcoa$eig)
sample_site <- data.frame({pcoa$point})[1:2]
sample_site$genome <- rownames(sample_site)
names(sample_site)[1:2] <- c('PCoA1', 'PCoA2')
sample_site <- merge(sample_site, spore_group, by = 'genome', all.x = TRUE)
sample_site$source<- factor(sample_site$source, levels = c('Animal rumen','Animal','Environment','Human GI','Human oral','Human other region'))
library(plyr)      
group_border <- ddply(sample_site, 'source', function(df) df[chull(df[[2]], df[[3]]), ])
ggplot(sample_site, aes(PCoA1, PCoA2, color = source))+
  theme_classic()+
  geom_vline(xintercept = 0, color = 'gray', size = 0.4) + 
  geom_hline(yintercept = 0, color = 'gray', size = 0.4) +
  #geom_polygon(data = group_border, alpha=0.3, aes(fill = factor(phylum)), show.legend = F) +
  stat_ellipse(level = 0.95, show.legend = F)+
  geom_point(size = 1.5)+
  scale_color_manual(values=c("#762a83","#af8dc3","#e7d4e8","#d9f0d3","#7fbf7b","#1b7837"),breaks = c('Animal rumen','Animal','Environment','Human oral','Human other region','Human GI'))+
  theme(panel.grid = element_line(color = 'gray', linetype = 2, size = 0.1), 
        panel.background = element_rect(color = 'black', fill = 'transparent'), 
        legend.title=element_blank()
  )+
  labs(x = paste('PCoA1: ', round(100 * pcoa_eig[1], 2), '%'), y = paste('PCoA2: ', round(100 * pcoa_eig[2], 2), '%'))

#Fig S8A
ACVD_sig_frame<- read.csv("Fig S8A_abun.csv",sep=",",header = T)
ACVD_sig_frame$group <- factor(ACVD_sig_frame$group,levels = c("ACVD","control"),ordered=TRUE)
library(ggpubr)
logFC<- read.csv("Fig S8A_logfc.csv",sep=",",header = T)
p1<-ggdotchart(logFC, x = "species", y = "logFC",
               color = "enrich",  
               palette = c("#00AFBB", "#FC4E07"), 
               sorting = "ascending",  
               rotate = F,
               xlab = FALSE,
               dot.size = 2,
               add = "segments",                            
               ggtheme = theme_pubr())+
  theme(axis.text.x = element_blank())+
  ylab(expression(paste(Log[2]," FC", sep = "")))
p2<-ggplot(ACVD_sig_frame, aes(x=reorder(species, -rank), y = log10(abundance), fill = factor(group))) +
  theme_bw()+
  scale_fill_manual(values =c("#00AFBB", "#FC4E07" ))+
  geom_boxplot()+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5),
        axis.title.x = element_blank())+
  ylab(expression(paste(Log[10]," (abundance)", sep = "")))
ggarrange(p1, p2, ncol = 1, nrow = 2,align="v",heights = c(1, 3),common.legend =TRUE)

#Fig S9A
CD_sig_frame<- read.csv("Fig S9A_abun.csv",sep=",",header = T)
CD_sig_frame$group <- factor(CD_sig_frame$group,levels = c("CD","control"),ordered=TRUE)
library(ggpubr)
logFC<- read.csv("Fig S9A_logfc.csv",sep=",",header = T)
p1<-ggdotchart(logFC, x = "species", y = "logFC",
               color = "enrich",  
               palette = c("#00AFBB", "#FC4E07"), 
               sorting = "ascending",  
               rotate = F,
               xlab = FALSE,
               dot.size = 2,
               add = "segments",                            
               ggtheme = theme_pubr())+
  theme(axis.text.x = element_blank())+
  ylab(expression(paste(Log[2]," FC", sep = "")))
p2<-ggplot(CD_sig_frame, aes(x=reorder(species, -rank), y = log10(abundance), fill = factor(group))) +
  theme_bw()+
  scale_fill_manual(values =c("#00AFBB", "#FC4E07" ))+
  geom_boxplot()+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5),
        axis.title.x = element_blank())+
  ylab(expression(paste(Log[10]," (abundance)", sep = "")))
ggarrange(p1, p2, ncol = 1, nrow = 2,align="v",heights = c(1, 3),common.legend =TRUE)

#Fig S10A
UC_sig_frame<- read.csv("Fig S10A_abun.csv",sep=",",header = T)
UC_sig_frame$group <- factor(UC_sig_frame$group,levels = c("UC","control"),ordered=TRUE)
library(ggpubr)
logFC<- read.csv("Fig S10A_logfc.csv",sep=",",header = T)
p1<-ggdotchart(logFC, x = "species", y = "logFC",
               color = "enrich",  
               palette = c("#00AFBB", "#FC4E07"), 
               sorting = "ascending",  
               rotate = F,
               xlab = FALSE,
               dot.size = 2,
               add = "segments",                            
               ggtheme = theme_pubr())+
  theme(axis.text.x = element_blank())+
  ylab(expression(paste(Log[2]," FC", sep = "")))
p2<-ggplot(UC_sig_frame, aes(x=reorder(species, -rank), y = log10(abundance), fill = factor(group))) +
  theme_bw()+
  scale_fill_manual(values =c("#00AFBB", "#FC4E07" ))+
  geom_boxplot()+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5),
        axis.title.x = element_blank())+
  ylab(expression(paste(Log[10]," (abundance)", sep = "")))
ggarrange(p1, p2, ncol = 1, nrow = 2,align="v",heights = c(1, 3),common.legend =TRUE)

#Fig S11
sig  <- read.csv("Fig S11.csv",header = T,sep=",")
sig$type <- factor(sig$type,levels = c("know","novel"),labels = c("Know","Potentially novel"))
ggplot(data = sig, mapping = aes(x = name, y = logFC,color=type)) + 
  geom_point()+
  theme_bw()+
  facet_wrap(~ group,nrow=3)+
  scale_color_manual(values=c("#2b83ba","#d7191c"),breaks = c("Know","Potentially novel"))+
  theme(axis.text.x = element_text(color = 'black',size = 8,angle = 45, hjust = 1),
        axis.title.y = element_text(face = 'bold',color = 'black',size = 10,vjust=2),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = 'black',size = 8),
        panel.grid = element_blank(),
        legend.position = 'top')+
  ylab(expression(paste(Log[2]," FC", sep = "")))+
  geom_hline(aes(yintercept=0), colour="black", linetype="dashed")+
  guides(color=guide_legend("Cluster novelty"))
