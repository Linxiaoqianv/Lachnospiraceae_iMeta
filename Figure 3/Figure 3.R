library(ggplot2)

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

