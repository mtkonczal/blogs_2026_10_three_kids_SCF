# Independent check of new claims at 7b3f727. Does not change production outputs.
suppressPackageStartupMessages({library(data.table);library(haven)})
setDTthreads(2)
out <- 'audit/review_20261015/revision_7b3f727/results'
years <- c(1989,1992,1995,1998,2013,2016,2019,2022,2025)
dat <- lapply(years,function(yr) {
  s <- fread(sprintf('data_raw/xlsx/SCFP%d.csv',yr))
  if ('X1'%in%names(s)) setnames(s,c('X1','XX1'),c('Y1','YY1'))
  f <- sprintf('data_raw/full/p%02di%d.dta',yr%%100,if(yr==1992)4 else 6)
  nm <- names(read_dta(f,n_max=1)); need <- nm[tolower(nm)%in%c('x1','y1','x5910')]
  z <- as.data.table(zap_labels(read_dta(f,col_select=dplyr::all_of(need)))); setnames(z,tolower(names(z)))
  if('x1'%in%names(z))setnames(z,'x1','y1')
  stopifnot(!anyDuplicated(z$y1),setequal(s$Y1,z$y1))
  s[,`:=`(year=yr,imp=Y1%%10,kids=KIDS+pmax(z$x5910[match(Y1,z$y1)],0),normal=as.numeric(NORMINC))]
  s[,inc:=fifelse(is.na(normal),INCOME,normal)]
  s[,eligible:=AGE>=45 & AGE<=64]
  setorder(s,Y1)
  s[,.(year,Y1,imp,WGT,AGE,INCOME,NETWORTH,inc,kids,eligible)]
})
names(dat) <- years
rank_mid <- function(x,w) {o<-order(x); ans<-numeric(length(x)); ans[o]<-(cumsum(w[o])-w[o]/2)/sum(w);ans}
rw <- lapply(dat,function(s) {
  z<-as.data.table(zap_labels(read_dta(sprintf('data_raw/rw/p%02d_rw1.dta',s$year[1]%%100))));setnames(z,tolower(names(z)))
  if('x1'%in%names(z))setnames(z,'x1','y1')
  ix<-match(s[imp==1,Y1],z$y1);stopifnot(!anyNA(ix))
  W<-sapply(1:999,function(b)z[[paste0('wt1b',b)]][ix]*z[[paste0('mm',b)]][ix]); W[is.na(W)]<-0
  stopifnot(all(is.finite(W)),all(W>=0),all(colSums(W)>0));W
})
# Generic independent estimator: full CSV population supports the national-rank check.
moment <- function(s,w,v,definition='base',cut=.99) {
  pop <- if(definition=='allpop')rep(TRUE,nrow(s)) else s$eligible
  g <- if(definition=='age10')floor((s$AGE-45)/10) else if(definition%in%c('age4564','allpop')) rep(1,nrow(s)) else floor(s$AGE/5)
  r<-rep(NA_real_,nrow(s))
  for(ix in split(which(pop),g[pop]))r[ix]<-rank_mid(s[[v]][ix],w[ix])
  w<-w/sum(w[s$eligible]); t<-which(s$eligible & r>cut); a<-which(s$eligible & r>.9 & r<=cut)
  c(sum(w[t]),sum(w[t]*(s$kids[t]>=3)),sum(w[a]),sum(w[a]*(s$kids[a]>=3)))
}
values <- function(m)c(top=100*m[2]/m[1],aff=100*m[4]/m[3],gap=100*(m[2]/m[1]-m[4]/m[3]))
jacobian <- function(m) {t<-100*c(-m[2]/m[1]^2,1/m[1],0,0);a<-100*c(0,0,-m[4]/m[3]^2,1/m[3]);rbind(top=t,aff=a,gap=t-a)}
estimate <- function(ys,v,definition='base',cut=.99) {
  M<-lapply(1:5,function(i)Reduce(`+`,lapply(dat[as.character(ys)],function(s){s<-s[imp==i];moment(s,s$WGT,v,definition,cut)})))
  Q<-sapply(M,values); J<-jacobian(M[[1]])
  V<-Reduce(`+`,lapply(as.character(ys),function(yr){s<-dat[[yr]][imp==1];W<-rw[[yr]];D<-sapply(1:999,function(b)moment(s,W[,b],v,definition,cut));apply(J%*%D,1,var)}))
  list(Q=Q,V=V)
}
table_for <- function(z) {est<-rowMeans(z$Q);se<-sqrt(z$V+1.2*apply(z$Q,1,var));data.table(stat=rownames(z$Q),est,se,lower=est-1.96*se,upper=est+1.96*se)}
recent<-seq(2013,2025,3); early<-c(1989,1992,1995,1998)
defs<-rbindlist(lapply(c('base','age10','age4564','allpop','actual','top2','top05'),function(def) {
  rbindlist(lapply(if(def=='actual')'income' else c('income','wealth'),function(rk){
    z<-estimate(recent,if(rk=='wealth')'NETWORTH' else if(def=='actual')'INCOME' else 'inc',def,if(def=='top2').98 else if(def=='top05').995 else .99)
    message(def,' ',rk,' complete'); table_for(z)[stat=='gap'][,`:=`(id=def,rank=rk)]
  }))
}))
old<-readRDS('output/kids_top1_defs.rds')[,.(id,rank,production_est=gap,production_se=se)]
chk<-merge(defs,old,by=c('id','rank'));chk[,`:=`(est_error=est-production_est,se_error=se-production_se)]
stopifnot(max(abs(chk$est_error))<1e-9,max(abs(chk$se_error))<1e-7)
fwrite(chk,file.path(out,'independent_definitions.csv'))

# Pooled ranks are a joint statistic. Perturb ONE independent wave at a time,
# re-estimate the shared cutoff everywhere, then sum wave variance contributions.
# This avoids relying on arbitrary matching of replicate indices across waves.
pool<-rbindlist(dat[as.character(recent)])[eligible==TRUE]
pooled_m <- function(s,w,v) {
  for(ix in split(seq_len(nrow(s)),s$year))w[ix]<-w[ix]/sum(w[ix])
  r<-rank_mid(s[[v]],w);t<-r>.99;a<-r>.9&r<=.99
  c(sum(w[t]),sum(w[t]*(s$kids[t]>=3)),sum(w[a]),sum(w[a]*(s$kids[a]>=3)))
}
pooled <- rbindlist(lapply(c('inc','NETWORTH'),function(v) {
  M<-lapply(1:5,function(i){s<-pool[imp==i];pooled_m(s,s$WGT,v)});Q<-sapply(M,values);J<-jacobian(M[[1]])
  s<-pool[imp==1];V<-rep(0,3)
  for(yr in as.character(recent)) {
    ix<-which(s$year==as.integer(yr));W<-rw[[yr]][dat[[yr]][imp==1,eligible],,drop=FALSE]
    D<-sapply(1:999,function(b){w<-s$WGT;w[ix]<-W[,b];pooled_m(s,w,v)})
    V<-V+apply(J%*%D,1,var)
  }
  message('Independent-wave pooled rank ',v,' complete')
  table_for(list(Q=Q,V=V))[stat=='gap'][,`:=`(id='pooled',rank=if(v=='inc')'income' else 'wealth')]
}))
fwrite(merge(pooled,old,by=c('id','rank')),file.path(out,'pooled_rank_independent_wave_check.csv'))

period <- rbindlist(lapply(c('inc','INCOME','NETWORTH'),function(v) {
  e<-estimate(early,v);r<-estimate(recent,v)
  # Same MI combination as production, but independent raw reconstruction and analytic Jacobian.
  d<-list(Q=r$Q-e$Q,V=r$V+e$V)
  message('Period ',v,' complete')
  rbindlist(list(table_for(e)[,period:='early'],table_for(r)[,period:='recent'],table_for(d)[,period:='change']))[,rank:=switch(v,inc='income (production)',INCOME='actual income',NETWORTH='net worth')]
}))
oldp<-readRDS('output/kids_period_change.rds')[,.(period,stat,rank,production_est=est,production_se=se)]
pc<-merge(period,oldp,by=c('period','stat','rank'));pc[,`:=`(est_error=est-production_est,se_error=se-production_se)]
stopifnot(max(abs(pc$est_error))<1e-9,max(abs(pc$se_error))<1e-7)
fwrite(pc,file.path(out,'independent_period_change.csv'))

# Compare isolated production reruns without counting build-time metadata.
repro<-'audit/review_20261015/reproduction'
cmp<-rbindlist(lapply(c('kids_top1_defs.rds','kids_period_change.rds'),function(f){
  a<-readRDS(file.path('output',f));b<-readRDS(file.path(repro,'output',f))
  attr(a,'built_at')<-attr(b,'built_at')<-NULL
  data.table(file=f,equal=isTRUE(all.equal(a,b,check.attributes=FALSE)))
}))
stopifnot(all(cmp$equal));fwrite(cmp,file.path(out,'rerun_comparison.csv'))
message('Independent revision checks passed.')
