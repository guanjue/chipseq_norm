library(LSD)
library(changepoint)

### get parameters
args = commandArgs(trailingOnly=TRUE)
input_file = args[1]
output_file = args[2]

changepoint_method = args[3]
#output_file = 'cmp_vs_lsk.rm0.txt.200.cp.png'

data = read.table(input_file,header = F)
data_m = data[data[,1]>1,]


x_od = (data_m[,1])
y_od = log2(data_m[,2])-log2(data_m[,3])

y = y_od[!is.na(y_od)]
x = x_od[!is.na(y_od)]

### kernal fit
#ks_line = ksmooth(x, y, 'normal', bandwidth = 5)
#lo = loess(y ~ log(x), family='gaussian', span=0.75, degree=1)
lo = lm(y~poly(log(x),30, raw=TRUE))
lo_fit_value = predict(lo, data.frame(x=x))

### variance change-point
ansvar=cpt.var(y, class=FALSE, method = changepoint_method)

png(paste(output_file, sep =''))
heatscatter(x, y, pch = 20, ylim=c(-2,3.3), xlim=c(1,10000), log='x')
#heatscatter(x, y, pch = 20, ylim=c(-1,2.3), xlim=c(1,500))
#lines(ks_line, col = 'blue', lty=7)
lines(lo_fit_value, col = 'blue', lty=2)
abline(v = ansvar[1], col = 'red', lty=2)
dev.off()

### variance change-point
ansvar_smooth=cpt.var(y-lo_fit_value, class=FALSE, method = changepoint_method)

png(paste(output_file, '.ks.png', sep =''))
heatscatter(x, y-lo_fit_value, pch = 20, ylim=c(-2,3.3), xlim=c(1,10000), log='x')
abline(v = ansvar_smooth[1], col = 'red', lty=2)
dev.off()



