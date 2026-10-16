# Independent verification of all 48 new bin estimates and standard errors.
# Uses the independently reconstructed raw-data sample from the original audit.
# Computes replicate influence totals, rather than sourcing R/28's estimator.
suppressPackageStartupMessages({library(data.table);library(haven)})
setDTthreads(2)
out <- 'audit/review_20261015/revision_7121ec0/results'
d <- readRDS('audit/review_20261015/results/independent_recent.rds')
setorder(d,year,Y1)
p <- readRDS('output/kids_main.rds')[year>=2013];setorder(p,year,Y1)
stopifnot(identical(d$Y1,p$Y1),all(d$year==p$year),all(d$kids==p$kids),
 all(d$WGT==p$WGT),all(d$inc==p$inc_measure),all(d$NETWORTH==p$NETWORTH))
labels <- c(paste0('P',seq(0,80,10),'-',seq(10,90,10)),'P90-95','P95-99','Top 1%')
cuts <- c(seq(.1,.9,.1),.95,.99)
bins <- function(s,w,v) {
 r<-numeric(nrow(s))
 for(ix in split(seq_len(nrow(s)),s$age5)) {
  o<-ix[order(s[[v]][ix])];r[o]<-(cumsum(w[o])-w[o]/2)/sum(w[o])
 }
 1L+rowSums(outer(r,cuts,`>`))
}
totals <- function(s,w,v) {
 b<-bins(s,w,v);w<-w/sum(w)
 t(vapply(1:12,function(j){ix<-which(b==j);c(sum(w[ix]),sum(w[ix]*100*(s$kids[ix]>=3)),sum(w[ix]*s$kids[ix]))},numeric(3)))
}
years<-sort(unique(d$year)); RW<-lapply(years,function(yr){
 s<-d[year==yr & implicate==1]
 z<-as.data.table(zap_labels(read_dta(sprintf('data_raw/rw/p%02d_rw1.dta',yr%%100))));setnames(z,tolower(names(z)))
 ix<-match(s$Y1,z$y1);stopifnot(!anyNA(ix))
 w<-sapply(1:999,function(b)z[[paste0('wt1b',b)]][ix]*z[[paste0('mm',b)]][ix]);w[is.na(w)]<-0
 stopifnot(all(w>=0),all(is.finite(w)),all(colSums(w)>0));w
})
results <- rbindlist(lapply(c('inc','NETWORTH'),function(v){
 M<-lapply(1:5,function(i)Reduce(`+`,lapply(years,function(yr){s<-d[year==yr & implicate==i];totals(s,s$WGT,v)})))
 Q<-simplify2array(lapply(M,function(m)m[,2:3]/m[,1]));theta<-Q[,,1];den<-M[[1]][,1]
 V<-matrix(0,12,2)
 for(k in seq_along(years)) {
  s<-d[year==years[k] & implicate==1];W<-RW[[k]]
  influence<-array(0,c(12,2,999))
  for(rep in 1:999) {
   w<-W[,rep];b<-bins(s,w,v);w<-w/sum(w)
   for(j in 1:12) {
    ix<-which(b==j)
    influence[j,1,rep]<-sum(w[ix]*(100*(s$kids[ix]>=3)-theta[j,1]))/den[j]
    influence[j,2,rep]<-sum(w[ix]*(s$kids[ix]-theta[j,2]))/den[j]
   }
  }
  V<-V+apply(influence,c(1,2),var)
 }
 est<-apply(Q,c(1,2),mean);se<-sqrt(V+1.2*apply(Q,c(1,2),var))
 message(v,' independent bin check complete')
 rbindlist(lapply(1:2,function(k)data.table(rank=if(v=='inc')'inc_rank' else 'nw_rank',outcome=c('share3','kids')[k],inc_bin=labels,est=est[,k],se=se[,k])))
}))
prod<-readRDS('output/kids_bin_ci.rds')[,.(rank,outcome,inc_bin=as.character(inc_bin),production_est=est,production_se=se)]
c<-merge(results,prod,by=c('rank','outcome','inc_bin'))
c[,`:=`(est_error=est-production_est,se_error=se-production_se)]
stopifnot(nrow(c)==48,max(abs(c$est_error))<1e-9,max(abs(c$se_error))<1e-9)
fwrite(c,file.path(out,'independent_bands.csv'))
print(c[,.(est_error=max(abs(est_error)),se_error=max(abs(se_error)))])
message('All 48 bin estimates and SEs pass.')
