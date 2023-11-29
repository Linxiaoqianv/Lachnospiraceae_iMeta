library(ComplexHeatmap)

#Figure 4D
data.inter<-read.csv("spore.csv",header=T,sep = ",",row.names=1)
data.inter<-as.data.frame(data.inter[,1:ncol(data.inter)])
Type<-read.csv("spore.anno.csv",header=T,sep = ",",row.names=1)
ha <- rowAnnotation(
  region=Type$source,
  col = list(
    region = c('Animal' = "#af8dc3",'Animal rumen' = "#762a83", 'Environment'  = "#e7d4e8",'Human GI'="#1b7837",'Human oral' = "#d9f0d3",'Human other region'="#7fbf7b")),
  simple_anno_size = unit(0.5, "cm"))
Heatmap(data.inter,col=c("#E8E4E4","#3570D3"),cluster_rows = TRUE,cluster_columns = FALSE,show_row_names = FALSE,show_column_names = TRUE,name = "Gene number",row_split=2,column_split = rep(c("Stage 0", "Stage I","Stage II","Stage III","Stage IV","Stage V","Stgermination","Ucharacterized"), c(1,4,4,5,4,3,4,13)),left_annotation =ha)
