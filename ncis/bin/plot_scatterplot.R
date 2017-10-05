library(LSD)

### get parameters
args = commandArgs(trailingOnly=TRUE)
xais_variable_file = args[1]
yais_variable_file = args[2]
output_file = args[3]

log = args[4]


data_x = read.table(xais_variable_file,header = T)
data_x = as.matrix(data_x[,2])
print(head(data_x))
data_y = read.table(yais_variable_file,header = T)
data_y = as.matrix(data_y[,2])
print(head(data_y))


used_id = sample(dim(data_y)[1],1000000)

data_x = data_x[used_id]
data_y = data_y[used_id]

print('MA plot')
M = log(data_y) - log(data_x)
A = 0.5*(log(data_y) + log(data_x))

print('scatter plot')
png(paste(output_file, '.hs.png', sep =''))
if (log == 'T'){
	heatscatter(data_x, data_y, log='xy', pch = 20)#, ylim=c(-10,5), xlim=c(20,10000))
} else{
	heatscatter(data_x, data_y, pch = 20)#, ylim=c(-10,5), xlim=c(20,10000))
}
abline(0,1,col = 'blue')
dev.off()


###### scale all
zero_mean_x = mean(data_x)
zero_std_x = sd(data_x[data_x!=0])
print(xais_variable_file)
print('sum(data_x!=0)')
print(sum(data_x!=0))

zero_mean_y = mean(data_y)
zero_std_y = sd(data_y[data_y!=0])
print(yais_variable_file)
print('sum(data_y!=0)')
print(sum(data_y!=0))

sf = log(zero_mean_x / zero_mean_y)

M_mean = log(data_y) - log(data_x) + sf
A_mean = 0.5*(log(data_y) + log(data_x) + sf)


###### scale nonzero
zero_mean_x = mean(data_x[as.logical((data_x!=0) * (data_y!=0))])
zero_std_x = sd(data_x[as.logical((data_x!=0) * (data_y!=0))])

zero_mean_y = mean(data_y[as.logical((data_x!=0) * (data_y!=0))])
zero_std_y = sd(data_y[as.logical((data_x!=0) * (data_y!=0))])

sf = log(zero_mean_x / zero_mean_y)

M_no0_mean = log(data_y) - log(data_x) + sf
A_no0_mean = 0.5*(log(data_y) + log(data_x) + sf)


###### scale nonzero median
zero_mean_x = median(data_x[as.logical((data_x!=0) * (data_y!=0))])
zero_std_x = sd(data_x[as.logical((data_x!=0) * (data_y!=0))])

zero_mean_y = median(data_y[as.logical((data_x!=0) * (data_y!=0))])
zero_std_y = sd(data_y[as.logical((data_x!=0) * (data_y!=0))])

sf = log(zero_mean_x / zero_mean_y)

M_median = log(data_y) - log(data_x) + sf
A_median = 0.5*(log(data_y) + log(data_x) + sf)



png(paste(output_file, '.4.MA.png', sep =''))
par(mfrow=c(2,2))
###
heatscatter(A, M, pch = 20, ylim=c(-10,10), main='original TPM')#, xlim=c(20,10000))
abline(h=0,col = 'red')
###
heatscatter(A_mean, M_mean, pch = 20, ylim=c(-10,10), main='over all mean')#, xlim=c(20,10000))
abline(h=0,col = 'red')
###
heatscatter(A_no0_mean, M_no0_mean, pch = 20, ylim=c(-10,10), main='non-zero mean')#, xlim=c(20,10000))
abline(h=0,col = 'red')
###
heatscatter(A_median, M_median, pch = 20, ylim=c(-10,10), main='non-zero median')#, xlim=c(20,10000))
abline(h=0,col = 'red')
###
dev.off()



###### scale nonzero median
zero_mean_x = mean(data_x[as.logical((data_x!=0) * (data_y!=0))])
zero_std_x = sd(data_x[as.logical((data_x!=0) * (data_y!=0))])

zero_mean_y = mean(data_y[as.logical((data_x!=0) * (data_y!=0))])
zero_std_y = sd(data_y[as.logical((data_x!=0) * (data_y!=0))])

sf = log(zero_mean_x / zero_mean_y)

A_mean = 0.5*(log(data_y) + log(data_x) + sf)

M_mean_anorm = log(data_y) - log(data_x) + sf - A_mean
A_mean_anorm = 0.5*(log(data_y) + log(data_x) + sf - A_mean)



png(paste(output_file, '.1.MA.png', sep =''))

###
heatscatter(A_mean_anorm, M_mean_anorm, pch = 20, ylim=c(-10,10), main='non-zero median')#, xlim=c(20,10000))
abline(h=0,col = 'red')
###
dev.off()




