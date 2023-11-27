library(ggplot2)
library(ComplexHeatmap)

#Figure 3A
data<- read.table("01.data/gene_genome_number.txt",header = T)
ggplot(data, aes(x=No/1868)) + 
  geom_histogram(binwidth=0.005) +
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
ggplot(data, aes(x=No/1868)) + 
  geom_histogram(binwidth=0.005) + 
  theme_bw() +ylim(0, 50) + 
  xlim(0.1,0.42)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

#Figure 3B
species_pan<- read.table("species_pan.csv",header = T,sep=",",row.names = 1)
species_core<- read.table("species_core.csv",header = T,sep=",",row.names = 1)
species_genenumber<- read.table("species_genenumber.csv",sep=",",header = T)
p3 <- Heatmap(species_pan,col=coul1,cluster_rows = FALSE,width = unit(1, "cm"),height = unit(32, "cm"),name = "Pan",
              right_annotation = rowAnnotation("Genome number" = row_anno_barplot(species_core$genome.number,axis = TRUE,width = unit(2, "cm"))),
              row_names_gp = gpar(fontsize = 10),
              cell_fun = function(j, i, x, y, width, height, fill) {
                grid.text(sprintf("%0.1f", species_pan[i, j]/1000), x, y, gp = gpar(fontsize = 8))
              })
p4<-Heatmap(species_core[,1],col=coul2,cluster_rows = FALSE,width = unit(1, "cm"),height = unit(32, "cm"),name = "Core",
            left_annotation = rowAnnotation("Gene number" = anno_density(species_genenumber, type = "violin"),width = unit(2, "cm")),
            cell_fun = function(j, i, x, y, width, height, fill) {
              grid.text(sprintf("%0.1f", species_core[i, j]/1000), x, y, gp = gpar(fontsize = 8))
            })
p4+p3

#Figure 3D
bceB<- read.table("bceB_SNP_frame.csv",sep=",",header = T)
p1<-ggplot(
  data=bceB,
  mapping=aes(
    x=pos,
    color=group,fill=group)
)+
  geom_density(alpha = 0.6) +
  theme_bw()+
  geom_vline(aes(xintercept=2943302), colour="black", linetype="dashed")+
  geom_vline(aes(xintercept=2941421), colour="black", linetype="dashed")+
  geom_vline(aes(xintercept=2943464), colour="black", linetype="dashed")
bceB_group<- read.table("bceB_SNP_group.csv",sep=",")
p2<-ggplot(bceB_group,aes(V1,fill=V2, color=V2)) + 
  xlab("Genomic position") + 
  geom_rug() + 
  theme_bw()+
  scale_color_manual(values=c("#a65628","#377eb8","#984ea3","#ff7f00","#e41a1c"),breaks = c('stop_gained','downstream_gene_variant','upstream_gene_variant','synonymous_variant','missense_variant'))+
  geom_vline(aes(xintercept=2943302), colour="black", linetype="dashed")+
  geom_vline(aes(xintercept=2941421), colour="black", linetype="dashed")+
  geom_vline(aes(xintercept=2943464), colour="black", linetype="dashed")
ggarrange(p1, p2, ncol = 1, nrow = 2,align="v",common.legend =TRUE)

#Figure 3E
gene <- read.delim('Anaerotignum_gene_presence_absence.csv', row.names = 1, sep = ',', stringsAsFactors = FALSE, check.names = FALSE)
anno <- read.delim('Anaerotignum_gene_presence_absence_group.csv', row.names = 1, sep = ',', stringsAsFactors = FALSE, check.names = FALSE)
ha <- HeatmapAnnotation(
  region=anno$group,
  col = list(
    region = c('1core' = "#993300",'2overlap' = "#FF6600", '3HumanGI'  = "#336633",'4Animal'="#9999CC",'5Environment' = "#FFCCFF")),
  simple_anno_size = unit(0.2, "cm"))
Heatmap(gene,col=c("white","#993300","#FF6600","#336633","#9999CC","#FFCCFF"),use_raster=FALSE,cluster_rows = FALSE,cluster_columns = FALSE,show_row_names = TRUE,show_column_names = FALSE,bottom_annotation = ha)
