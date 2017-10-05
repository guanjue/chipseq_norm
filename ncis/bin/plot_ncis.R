library(LSD)
library(pheatmap)

#data = read.table('ERY_ad.h3k4me3rep.120.ncsi.matrix.txt',header = F)





name_vec = c('mep_vs_lsk.200.txt',
	'ERY_ad.h3k4me3rep.120.ncsi.matrix.txt.200.txt', 
	'ERY_ad.h3k4me3rep.120.ncsi.matrix.txt.200.txt',
	'ERY_ad.h3k4me3rep.120.ncsi.matrix.txt.600.txt',
	'ERY_ad.h3k4me3rep.120.ncsi.matrix.txt.800.txt',
	'ERY_ad.h3k4me3rep.120.ncsi.matrix.txt.1000.txt',
	'ERY_ad.h3k4me3rep.120.ncsi.matrix.txt.1200.txt',
	'ERY_ad.h3k4me3rep.120.ncsi.matrix.txt.1400.txt',
	'ERY_ad.h3k4me3rep.120.ncsi.matrix.txt.1600.txt',
	'ERY_ad.h3k4me3rep.120.ncsi.matrix.txt.1800.txt',
	'ERY_ad.h3k4me3rep.120.ncsi.matrix.txt.2000.txt'
	)



log2_fc = c() #c(1: 9999)
for (name in name_vec){
	data = read.table(name,header = F)
	head(data)
	data_m = data[-1,]
	log2_fc_tmp = log2((data_m[,2]+1) / (data_m[,3]+1))
	log2_fc_tmp = log2((data_m[,2]+1)/(mean(data_m[,2]+1)*var(data_m[,2]+1)) / (data_m[,3]+1) * (mean(data_m[,3]+1)*var(data_m[,3]+1)))
	log2_fc = rbind(log2_fc, log2_fc_tmp)
	png(paste(name, '.hs.png', sep =''))
	heatscatter(data_m[,1], log2(data_m[,2])-log2(data_m[,3]), log='x', pch = 20)#, ylim=c(-10,5), xlim=c(20,10000))
	dev.off()
}





log2_fc = log2_fc[,1:200]



log2_fc = t(log2_fc)


rownames(log2_fc) = c(1: dim(log2_fc)[1])

pheatmap(log2_fc, cluster_cols = FALSE, cluster_rows=FALSE)

,annotation_names_row=FALSE,annotation_names_col=TRUE,show_rownames=FALSE,show_colnames=TRUE )


heatscatter(data_m[,1], log2(data_m[,2])-log2(data_m[,3]), log='x', pch = 20)


heatscatter(data_m[,1], log2(data_m[,2])-log2(data_m[,3]), log='x', pch = 20)



