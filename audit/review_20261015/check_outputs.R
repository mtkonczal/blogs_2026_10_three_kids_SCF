suppressPackageStartupMessages(library(data.table))
setDTthreads(2)
out <- 'audit/review_20261015/results'
repro <- 'audit/review_20261015/reproduction'
checks <- list()
clean <- function(x) {
 attr(x,'built_at') <- NULL
 if(is.data.frame(x)) return(as.data.frame(x))
 if(is.list(x)) {x$built_at<-NULL;if(!is.null(x$sample_note))x$sample_note$built_at<-NULL; x<-lapply(x,clean)}
 x
}
for(f in c('data/scf_linked.rds','output/kids_main.rds','output/kids_income.rds','output/kids_blended.rds','output/kids_wealth_work.rds','output/kids_rep5.rds')) {
 comparison <- all.equal(clean(readRDS(f)),clean(readRDS(file.path(repro,f))))
 checks[[f]] <- data.table(file=f,result=paste(comparison,collapse='; '))
}
a <- fread('graphics/post_numbers.csv'); b <- fread(file.path(repro,'graphics/post_numbers.csv'))
checks[['numbers']] <- data.table(file='graphics/post_numbers.csv',result=paste(all.equal(a[,!'built_at'],b[,!'built_at']),collapse='; '))
fwrite(rbindlist(checks),file.path(out,'reproduction_comparison.csv'))
stopifnot(all(vapply(checks,function(x)x$result=='TRUE',logical(1))))

# Point estimates directly from inputs, including the quantile labels' actual coverage.
d <- readRDS(file.path(out,'independent_recent.rds'))
wm <- function(x,w) sum(x*w)/sum(w)
qw <- function(x,w,p) {o<-order(x);x[o][which(cumsum(w[o])/sum(w)>=p-1e-12)[1]]}
cuts <- c(0,seq(.1,.9,.1),.95,.99,1)
labels <- c(paste0('P',seq(0,80,10),'-',seq(10,90,10)),'P90-95','P95-99','Top1')
rows <- list()
for(rk in c('ir','nr')) {
 v <- if(rk=='ir')'inc' else 'NETWORTH'
 dollars <- vapply(cuts,function(p)qw(d[[v]],d$w,p),numeric(1)); dollars[c(1,length(dollars))]<-c(-Inf,Inf)
 s <- copy(d);s[,bin:=cut(get(rk),cuts,labels=labels,include.lowest=TRUE)]
 z <- s[,.(share3=100*wm(kids>=3,w),kids=wm(kids,w),
   dollar_coverage=100*wm(get(v)>=dollars[as.integer(bin)[1]]&get(v)<=dollars[as.integer(bin)[1]+1],w)),by=bin]
 z[,`:=`(rank=rk,low=dollars[as.integer(bin)],high=dollars[as.integer(bin)+1])]
 z[,literal_dollar_share3:=vapply(seq_len(.N),function(j){k<-d[[v]]>=low[j]&d[[v]]<high[j];100*wm(d$kids[k]>=3,d$w[k])},numeric(1))]
 rows[[rk]]<-z
}
fwrite(rbindlist(rows),file.path(out,'bins_and_dollars.csv'))
sizes<-rbindlist(lapply(c('ir','nr'),function(rk)d[get(rk)>.99,.(n=.N,weight=sum(WGT),kish_effective_n=sum(WGT)^2/sum(WGT^2)),by=.(year,implicate)][,rank:=rk]))
fwrite(sizes,file.path(out,'top1_sample_sizes.csv'))

# Current status prevalence is not the probability of belonging to the top 1%.
status<-d[,.(share_top1=100*wm(ir>.99,w),share_top1_given_exact3=100*wm((ir>.99)[kids==3],w[kids==3]),
 sample_families=uniqueN(paste(year,YY1)),top1_union_cases=uniqueN(paste(year,YY1)[ir>.99]))]
fwrite(status,file.path(out,'status_interpretation.csv'))

# Boundary handling for stock concentration: production whole-row CDF vs fractional
# assignment at percentile boundaries. The latter gives exactly 50/40/10 percent weight.
s <- fread('data_raw/xlsx/SCFP2025.csv',integer64='double')
stock <- function(z,fractional=FALSE) {
 z<-z[order(NETWORTH)];w<-z$WGT/sum(z$WGT);r<-cumsum(w)
 if(fractional) {
  shares<-vapply(c(.5,.9),function(p)sum(z$EQUITY*pmax(0,pmin(r,p)-(r-w)))/sum(z$EQUITY*w)*100,numeric(1))
 } else shares<-vapply(c(.5,.9),function(p)sum(z$EQUITY[r<=p]*w[r<=p])/sum(z$EQUITY*w)*100,numeric(1))
 c(bottom50=shares[1],top10=100-shares[2],owners=100*wm(z$HEQUITY==1,w))
}
st<-rbind(cbind(data.table(method='pooled_whole_row'),as.data.table(as.list(stock(s)))),
 cbind(data.table(method='pooled_fractional'),as.data.table(as.list(stock(s,TRUE)))),
 cbind(data.table(method='average_implicate_whole_row'),as.data.table(as.list(rowMeans(sapply(1:5,function(i)stock(s[Y1%%10==i])))))))
fwrite(st,file.path(out,'stock_checks.csv'))
cat('All rebuilt outputs match. Independent stock, dollar-label and sample-size checks saved.\n')
