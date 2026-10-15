# Compare fixed versus re-estimated ranks holding the original equal-wave-gap estimator fixed.
library(data.table)
labels<-as.vector(outer(c('all','no_remarried','first_marriage'),c('top','aff','middle'),paste,sep='_'))
statfun<-function(m){dimnames(m)<-list(c('den','num3','numk'),labels); val<-c();for(g in c('all','no_remarried','first_marriage')){q<-m[,paste0(g,c('_top','_aff','_middle')),drop=FALSE];a<-100*q[2,]/q[1,];k<-q[3,]/q[1,];v<-c(a[1]-a[2],k[1]-k[2]);names(v)<-paste(g,c('gap3','gapkids'),sep='_');val<-c(val,v)};val}
out<-list()
for(rk in c('inc_rank','nw_rank')){
 p<-readRDS(paste0('audit/results/moments_',rk,'.rds'))
 for(yrs in list(c(2019,2022,2025),seq(2013,2025,3)))for(mode in c('fixed','reranked')){
  a<-lapply(p[as.character(yrs)],function(z){q<-sapply(z$imp,statfun);draw<-apply(z$draws[[mode]],3,statfun);list(est=rowMeans(q),variance=apply(draw,1,var)+1.2*apply(q,1,var))})
  out[[length(out)+1]]<-data.table(rank=rk,years=paste(range(yrs),collapse='-'),method=mode,stat=names(a[[1]]$est),est=rowMeans(sapply(a,`[[`,'est')),se=sqrt(rowSums(sapply(a,`[[`,'variance')))/length(a))
 }
}
z<-rbindlist(out);z[,`:=`(lower=est-1.96*se,upper=est+1.96*se)];fwrite(z,'audit/results/matched_inference.csv')
