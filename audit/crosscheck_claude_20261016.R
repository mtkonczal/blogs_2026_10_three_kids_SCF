# Codex cross-check of Claude's October 16 review. No production files changed.
# Raw recent CSV/Stata inputs; equal wave weight, age 45-64, five implicates.
# Supplemental estimands (capped count, conditional shares) are labeled explicitly.
suppressPackageStartupMessages({library(data.table); library(haven)})
setDTthreads(2)
dir.create('audit/results', showWarnings=FALSE)
pr <- function(x,w) {o<-order(x); r<-numeric(length(x)); r[o]<-(cumsum(w[o])-w[o]/2)/sum(w); r}
waves <- seq(2013,2025,3)
raw <- lapply(waves,function(wv) {
 s<-fread(sprintf('data_raw/xlsx/SCFP%d.csv',wv))
 f<-sprintf('data_raw/full/p%02di6.dta',wv%%100); nm<-names(read_dta(f,n_max=1))
 want<-c('y1','x5910','x7377','x7392','x8023','x105')
 z<-as.data.table(zap_labels(read_dta(f,col_select=dplyr::all_of(nm[tolower(nm)%in%want]))))
 setnames(z,tolower(names(z)))
 stopifnot(!anyDuplicated(s$Y1),!anyDuplicated(z$y1),setequal(s$Y1,z$y1))
 z<-z[match(s$Y1,y1)]
 s[,`:=`(kids=KIDS+pmax(z$x5910,0),first_proxy=MARRIED==1 & z$x7377==1 & z$x7392==1,
   first_strict=z$x8023==1 & z$x105==1 & z$x7377==1 & z$x7392==1,year=wv)]
 s<-s[AGE>=45 & AGE<=64]
 s[,`:=`(implicate=Y1%%10,age5=5*floor(AGE/5))]
 s[,ir:=pr(NORMINC,WGT),by=.(implicate,age5)]
 s[,nr:=pr(NETWORTH,WGT),by=.(implicate,age5)]
 s[,w:=WGT/sum(WGT),by=implicate]
 s[,.(year,Y1,YY1,implicate,age5,NORMINC,NETWORTH,KIDS,kids,WGT,w,MARRIED,first_proxy,first_strict,ir,nr)]
})
d<-rbindlist(raw)
production<-readRDS('output/kids_main.rds')[year>=2013]
ix<-match(paste(d$year,d$Y1),paste(production$year,production$Y1))
stopifnot(!anyNA(ix),nrow(d)==nrow(production),all(d$kids==production$kids[ix]),
 max(abs(d$ir-production$inc_rank[ix]))<1e-12,max(abs(d$nr-production$nw_rank[ix]))<1e-12)
specs<-c('mean_count','capped_at_four','excess_above_four','three_plus','three_plus_among_two_plus')
moment<-function(s,w,r) {
 w<-w/sum(w); ans<-matrix(0,4,length(specs),dimnames=list(NULL,specs))
 for(j in seq_along(specs)) {
  keep<-if(j==5) s$kids>=2 else rep(TRUE,nrow(s))
  y<-switch(j,s$kids,pmin(s$kids,4),pmax(s$kids-4,0),100*(s$kids>=3),100*(s$kids>=3))
  t<-keep & r>.99; a<-keep & r>.9 & r<=.99
  ans[,j]<-c(sum(w[t]),sum(w[t]*y[t]),sum(w[a]),sum(w[a]*y[a]))
 };ans
}
getgap<-function(m) m[2,]/m[1,]-m[4,]/m[3,]
out<-list(); desc<-list(); tailcov<-list()
for(rk in c('ir','nr')) {
 variable<-if(rk=='ir') 'NORMINC' else 'NETWORTH'
 bases<-lapply(1:5,function(i) Reduce(`+`,lapply(raw,function(z) {s<-z[implicate==i];moment(s,s$WGT,s[[rk]])})))
 Q<-sapply(bases,getgap); base<-bases[[1]]
 G<-rbind(-base[2,]/base[1,]^2,1/base[1,],base[4,]/base[3,]^2,-1/base[3,])
 V<-F<-E<-rep(0,length(specs)); vc<-0
 for(wv in waves) {
  s<-d[year==wv & implicate==1]
  rw<-as.data.table(zap_labels(read_dta(sprintf('data_raw/rw/p%02d_rw1.dta',wv%%100))));setnames(rw,tolower(names(rw)))
  ix<-match(s$Y1,rw$y1);stopifnot(!anyNA(ix))
  point<-moment(s,s$WGT,s[[rk]])
  lin<-fixed<-exact<-matrix(0,999,length(specs))
  for(b in 1:999) {
   w<-rw[[paste0('wt1b',b)]][ix]*rw[[paste0('mm',b)]][ix];w[is.na(w)]<-0
   stopifnot(all(is.finite(w)),all(w>=0),sum(w)>0)
   r<-numeric(nrow(s));for(a in unique(s$age5)) {j<-which(s$age5==a);stopifnot(sum(w[j])>0);r[j]<-pr(s[[variable]][j],w[j])}
   m<-moment(s,w,r)
   lin[b,]<-colSums(G*m)
   fixed[b,]<-colSums(G*moment(s,w,s[[rk]]))
   # Exact substitution is a diagnostic approximation to independent-wave variance.
   exact[b,]<-getgap(base-point+m)
  }
  V<-V+apply(lin,2,var);F<-F+apply(fixed,2,var);E<-E+apply(exact,2,var)
  vc<-vc+cov(lin[,2],lin[,3])
  message(rk,' ',wv,' complete')
 }
 B<-apply(Q,1,var);se<-sqrt(V+1.2*B);est<-rowMeans(Q)
 out[[rk]]<-data.table(rank=rk,spec=specs,top=rowMeans(sapply(bases,function(m)m[2,]/m[1,])),
  affluent=rowMeans(sapply(bases,function(m)m[4,]/m[3,])),gap=est,se=se,
  se_exact=sqrt(E+1.2*B),se_fixed_rank=sqrt(F+1.2*B),lower=est-1.96*se,upper=est+1.96*se,p=2*pnorm(-abs(est/se)))
 tailcov[[rk]]<-data.table(rank=rk,cov_cap_excess=vc+1.2*cov(Q[2,],Q[3,]))
 desc[[rk]]<-d[get(rk)>.9,.(families=uniqueN(paste(year,YY1)),weight=sum(w),
  first_proxy=100*weighted.mean(first_proxy,w),first_strict=100*weighted.mean(first_strict,w),
  first_among_couples=100*weighted.mean(first_proxy[MARRIED==1],w[MARRIED==1]),
  childless=100*weighted.mean(kids==0,w),one_child=100*weighted.mean(kids==1,w),
  above_four=100*weighted.mean(kids>4,w),unweighted_above_four=uniqueN(paste(year[kids>4],YY1[kids>4]))),
  by=.(group=ifelse(get(rk)>.99,'top1','p90_99'))][,rank:=rk]
}
ans<-rbindlist(out)
baseline<-readRDS('output/kids_rep5.rds')[stat%in%c('all_gapkids','all_gap3')]
for(rk in c('ir','nr')) for(sp in c('mean_count','three_plus')) {
 a<-ans[rank==rk & spec==sp]; b<-baseline[rank==if(rk=='ir') 'inc_rank' else 'nw_rank'][stat==if(sp=='mean_count') 'all_gapkids' else 'all_gap3']
 stopifnot(abs(a$gap-b$est)<1e-8,abs(a$se-b$se)<1e-5)
}
fwrite(ans,'audit/results/crosscheck_claude_20261016_inference.csv')
fwrite(rbindlist(desc),'audit/results/crosscheck_claude_20261016_composition.csv')
fwrite(rbindlist(tailcov),'audit/results/crosscheck_claude_20261016_covariance.csv')
# Public X5910 extrema in all records, not only age-eligible records.
files<-c('1989'='p89i6','1992'='p92i4',setNames(sprintf('p%02di6',seq(1995,2025,3)%%100),seq(1995,2025,3)))
extrema<-rbindlist(lapply(names(files),function(wv) {
 f<-sprintf('data_raw/full/%s.dta',files[[wv]]);nm<-names(read_dta(f,n_max=1))
 x<-zap_labels(read_dta(f,col_select=dplyr::all_of(nm[tolower(nm)=='x5910'])))[[1]]
 data.table(year=as.integer(wv),min=min(x),max=max(x),missing=sum(is.na(x)))
}))
fwrite(extrema,'audit/results/crosscheck_claude_20261016_extrema.csv')
writeLines(capture.output(sessionInfo()),'audit/results/crosscheck_claude_20261016_session.txt')
print(ans,digits=6);print(rbindlist(desc),digits=6);print(extrema)
