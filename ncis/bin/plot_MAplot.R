library(LSD)

### get parameters
args = commandArgs(trailingOnly=TRUE)
xais_variable_file = args[1]
yais_variable_file = args[2]
output_file = args[3]

log = args[4]

x_lower = as.numeric(args[5])
x_upper = as.numeric(args[6])
y_lower = as.numeric(args[7])
y_upper = as.numeric(args[8])


data_x = read.table(xais_variable_file,header = T)
data_x = as.numeric(data_x[,2])

data_y = read.table(yais_variable_file,header = T)
data_y = as.numeric(data_y[,2])


M = log2(data_y) - log2(data_x)
A = 0.5*(log2(data_y) + log2(data_x))

png(paste(output_file, '.MA.png', sep =''))
heatscatter(A, M, pch = 20)#, ylim=c(-10,5), xlim=c(20,10000))
dev.off()






