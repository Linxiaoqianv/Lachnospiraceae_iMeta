library(ggplot2)
library(ComplexHeatmap)
library(ggpubr)
library(RColorBrewer)

#Figure 3A
data<- read.table("gene_genome_number.txt",header = T)
p1<-ggplot(data, aes(x=No/1868)) + 
  geom_histogram(binwidth=0.005) +
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  labs(x="Genome ratio",y="Gene count")+
  scale_y_continuous(labels = scales::label_comma())
p2<-ggplot(data, aes(x=No/1868)) + 
  geom_histogram(binwidth=0.005) + 
  theme_bw() +ylim(0, 50) + 
  xlim(0.1,0.42)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

library(cowplot)
ggdraw()+
  draw_plot(p1,x=0,y=0,width=1,height=1)+
  draw_plot(p2,x=0.1,y=0.1,width=1,height=1,scale=0.5)

#Figure 3B
species_pan<- read.table("species_pan.csv",header = T,sep=",",row.names = 1)
species_core<- read.table("species_core.csv",header = T,sep=",",row.names = 1)
species_genenumber<- read.table("species_genenumber.csv",sep=",",header = T)
coul1 <- colorRampPalette(brewer.pal(8, "Oranges")[1:7])(25)
coul2 <- colorRampPalette(brewer.pal(8, "Purples")[1:7])(25)
p3 <- Heatmap(species_pan[,1],col=coul1,cluster_rows = FALSE,width = unit(1, "cm"),height = unit(32, "cm"),name = "Pan",
              right_annotation = rowAnnotation("Genome number" = row_anno_barplot(species_pan$genome.number,axis = TRUE,width = unit(2, "cm"))),
              cell_fun = function(j, i, x, y, width, height, fill) {
                grid.text(sprintf("%0.1f", species_pan[i, j]/1000), x, y, gp = gpar(fontsize = 8))
              })
p4<-Heatmap(species_core,col=coul2,cluster_rows = FALSE,width = unit(1, "cm"),height = unit(32, "cm"),name = "Core",
            left_annotation = rowAnnotation("Gene number" = anno_density(species_genenumber, type = "violin"),width = unit(2, "cm")),
            row_names_gp = gpar(fontsize = 10),row_names_side = "left",
            cell_fun = function(j, i, x, y, width, height, fill) {
              grid.text(sprintf("%0.1f", species_core[i, j]/1000), x, y, gp = gpar(fontsize = 8))
            })
p4+p3

#Figure 3D
bceB<- read.table("bceB_SNP_frame.csv",sep=",",header = T)
ggplot(bceB,aes(x=pos,fill=group))+
  annotate("rect", xmin = 2941421, 
           xmax = 2943464, 
           ymin = 0, 
           ymax = 4E-4,
           alpha = .2) +
  geom_density(alpha = 0.6,color=NA) +
  geom_rug(aes(color = type))+
  scale_color_manual(values=c("#a65628","#377eb8","#984ea3","#ff7f00","#e41a1c"),breaks = c('stop_gained','downstream_gene_variant','upstream_gene_variant','synonymous_variant','missense_variant'))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  scale_x_continuous(labels = scales::label_comma())+
  scale_y_continuous(labels = c(expression(0),
                                expression(1%*%10^-4),
                                expression(2%*%10^-4),
                                expression(3%*%10^-4)),
                     breaks = c(0,0.0001,0.0002,0.0003))+
  labs(x="Genomic position",y="Density")+
  geom_vline(aes(xintercept=2943302), colour="black", linetype="dashed")+
  guides(fill=guide_legend("Clade"),color=guide_legend("Variant type"))
  
#Figure 3E
gene <- read.delim('Anaerotignum_gene_presence_absence.csv', row.names = 1, sep = ',', stringsAsFactors = FALSE, check.names = FALSE)
anno <- read.delim('Anaerotignum_gene_presence_absence_group.csv', row.names = 1, sep = ',', stringsAsFactors = FALSE, check.names = FALSE)
ha <- HeatmapAnnotation(
  region=anno$group,
  col = list(
    region = c('1core' = "#993300",'2overlap' = "#FF6600", '3HumanGI'  = "#336633",'4Animal'="#9999CC",'5Environment' = "#FFCCFF")),
  simple_anno_size = unit(0.2, "cm"))
Heatmap(gene,col=c("white","#993300","#FF6600","#336633","#9999CC","#FFCCFF"),use_raster=FALSE,cluster_rows = FALSE,cluster_columns = FALSE,show_row_names = TRUE,show_column_names = FALSE,bottom_annotation = ha)
