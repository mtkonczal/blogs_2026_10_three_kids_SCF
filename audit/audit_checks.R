# Independent referee diagnostics. Run from project root: Rscript audit/audit_checks.R
# No production sample definitions or scripts are changed. Sensitivities are explicitly labeled.
suppressPackageStartupMessages({library(data.table); library(haven)})
source('R/scf_functions.R')
set.seed(20261009)
dir.create('audit/results', showWarnings=FALSE)
main <- readRDS('output/kids_main.rds')
scf <- readRDS('data/scf_linked.rds')
prank <- function(x,w) {o<-order(x); r<-numeric(length(x)); r[o]<-(cumsum(w[o])-w[o]/2)/sum(w); r}
# Same tie-breaking as production, to isolate rank re-estimation from tie-policy changes.
files <- c('1989'='p89i6','1992'='p92i4','1995'='p95i6','1998'='p98i6','2001'='p01i6','2004'='p04i6','2007'='p07i6','2010'='p10i6','2013'='p13i6','2016'='p16i6','2019'='p19i6','2022'='p22i6','2025'='p25i6')
rels<-c('x108','x114','x120','x126','x132','x202','x208','x214','x220','x226')
checks<-list(); full<-list()
for (yr in names(files)) {
 f<-paste0('data_raw/full/',files[[yr]],'.dta'); nm<-names(read_dta(f,n_max=1))
 need<-c('y1','x1','x42001','x104','x8023','x105',rels)
 z<-as.data.table(zap_labels(read_dta(f,col_select=dplyr::all_of(nm[tolower(nm)%in%need]))));setnames(z,tolower(names(z)))
 if('x1'%in%names(z))setnames(z,'x1','y1')
 z[,year:=as.numeric(yr)]; z[,roster_kids:=rowSums(as.matrix(.SD)==4|as.matrix(.SD)==13|as.matrix(.SD)==36,na.rm=TRUE),.SDcols=intersect(rels,names(z))]
 a<-merge(scf[year==as.numeric(yr),.(year,Y1,WGT,KIDS,YY1,implicate)],z,by.x=c('year','Y1'),by.y=c('year','y1'))
 checks[[yr]]<-a[,.(rows=.N,duplicate_ids=anyDuplicated(Y1),max_weight_ratio_error=max(abs(5*WGT/x42001-1)),kids_roster_disagreements=sum(KIDS!=roster_kids),families=uniqueN(YY1))]
 checks[[yr]][,year:=as.numeric(yr)]; full[[yr]]<-z[,.(year,Y1=y1,spouse_age=x104,raw_marital=x8023,spouse_marital=x105)]
}
fwrite(rbindlist(checks),'audit/results/raw_integrity.csv')
main<-merge(main,rbindlist(full),by=c('year','Y1'))
stopifnot(!anyDuplicated(main[,.(year,Y1)]),all(is.finite(main$kids)),all(main$WGT>0),all(main$x5910 >= -1))
main[,remarried:=MARRIED==1&(x7377==2|x7392==2)]
fwrite(main[,.(n_cases=uniqueN(case),n_rows=.N,missing_kids=sum(is.na(kids)),topcoded_pct=100*wmean(x5910==10,w)),by=year],'audit/results/sample.csv')
rec<-main[year>=2013]
# Reproduce headline levels and report composition, precision-relevant weight concentration.
comp<-rbindlist(lapply(c('inc_rank','nw_rank'),function(rk){
 s<-copy(rec);s[,band:=fifelse(get(rk)>.99,'Top 1%',fifelse(get(rk)>.9,'P90-99',fifelse(get(rk)>.4 & get(rk)<=.6,'P40-60','Other')))]
 s[,.(share3=100*wmean(kids>=3,w),mean_kids=wmean(kids,w),partnered=100*wmean(MARRIED==1,w),remarried_pct=100*wmean(remarried,w),n_union=uniqueN(case),head_age=wmean(AGE,w),spouse_under45=100*wmean(spouse_age[MARRIED==1]<45,w[MARRIED==1])),by=band][,rank:=rk]
}))
fwrite(comp,'audit/results/composition.csv')
# Re-rank only for named sensitivity checks. Do not adopt any as the primary specification.
gaps<-function(s,rk='inc_rank') {
 s<-copy(s); s[,band:=fifelse(get(rk)>.99,'Top 1%',fifelse(get(rk)>.9,'P90-99','Other'))]
 a<-s[band!='Other',.(share3=100*wmean(kids>=3,w),mean_kids=wmean(kids,w)),by=.(implicate,band)]
 b<-dcast(a,implicate~band,value.var=c('share3','mean_kids'))
 data.table(top3=mean(b[['share3_Top 1%']]),aff3=mean(b[['share3_P90-99']]),gap3=mean(b[['share3_Top 1%']]-b[['share3_P90-99']]),gapkids=mean(b[['mean_kids_Top 1%']]-b[['mean_kids_P90-99']]))
}
sens<-list();add_sens<-function(s,label,rk='inc_rank') {sens[[length(sens)+1]]<<-gaps(s,rk)[,spec:=label]}
add_sens(rec,'Baseline')
add_sens(rec[first_marriage_couple==TRUE],'Both report first marriage; original ranks')
add_sens(rec[remarried==FALSE],'Exclude previously married couples; original ranks')
add_sens(rec[MARRIED==1],'Partnered; original ranks')
add_sens(rec[MARRIED==2],'Unpartnered; original ranks')
add_sens(rec[AGE>=50 & (MARRIED==2|spouse_age>=45)],'Head 50-64, spouse 45+ if partnered; original ranks')
for (age in c(45,50,55,60))add_sens(rec[age5==age],paste('Head age band',age))
s<-copy(rec);s[,inc_rank:=prank(INCOME,WGT),by=.(year,implicate,age5)];add_sens(s,'Actual income; age-band ranks')
s<-copy(rec);s[,inc_rank:=prank(inc_measure,WGT),by=.(year,implicate)];add_sens(s,'Usual income; all 45-64 ranked together')
s<-copy(rec);s[,inc_rank:=rev(prank(rev(inc_measure),rev(WGT))),by=.(year,implicate,age5)];add_sens(s,'Reverse within-tie order; usual income')
for(yr in sort(unique(rec$year)))add_sens(rec[year!=yr],paste('Omit survey',yr))
fwrite(rbindlist(sens),'audit/results/sensitivities.csv')
# Historical rank-measure check: actual income throughout vs production mix.
hist<-list()
for(mode in c('Production mix','Actual income throughout')) {
 s<-copy(main);if(mode!='Production mix')s[,inc_rank:=prank(INCOME,WGT),by=.(year,implicate,age5)]
 for(pp in levels(s$period))hist[[length(hist)+1]]<-gaps(s[period==pp])[,`:=`(spec=mode,period=pp)]
}
fwrite(rbindlist(hist),'audit/results/history.csv')
# Dollar-label agreement: rank is within BOTH year and age, labels are pooled.
for(rk in c('inc','nw')) {
 v<-if(rk=='inc')'inc_measure' else 'NETWORTH';bn<-paste0(rk,'_bin')
 cuts<-vapply(c(0,seq(.1,.9,.1),.95,.99,1),function(p)wq(rec[[v]],rec$w,p),numeric(1));cuts[1]<- -Inf;cuts[length(cuts)]<-Inf
 z<-rec[,.(coverage=100*wmean(get(v)>=cuts[as.integer(get(bn))[1]]&get(v)<=cuts[as.integer(get(bn))[1]+1],w)),by=bn]
 fwrite(z,paste0('audit/results/dollar_agreement_',rk,'.csv'))
}
# Re-estimate the complete ranking procedure inside each bootstrap replicate.
# Compute per-wave normalized subgroup moments, then combine independent wave variances
# by the delta method for the pooled ratio. This avoids artificial cross-wave bootstrap covariance.
# MI point estimates and B are computed for the actual pooled estimator, per implicate.
labels<-as.vector(outer(c('all','no_remarried','first_marriage'),c('top','aff','middle'),paste,sep='_'))
moments<-function(s,w,ranks) {
 w<-w/sum(w); out<-matrix(0,3,length(labels),dimnames=list(c('den','num3','numk'),labels))
 for(j in seq_along(labels)) {
 lab<-labels[j]; keep<-if(grepl('^no_remarried',lab)) !s$remarried else if(grepl('^first_marriage',lab)) s$first_marriage_couple%in%TRUE else rep(TRUE,nrow(s))
 band<-sub('.*_','',lab); keep<-keep & switch(band,top=ranks>.99,aff=ranks>.9&ranks<=.99,middle=ranks>.4&ranks<=.6)
 keep[is.na(keep)]<-FALSE
 out[,j]<-c(sum(w[keep]),sum(w[keep]*(s$kids[keep]>=3)),sum(w[keep]*s$kids[keep]))
 };out
}
statfun<-function(m) {
 val<-c();for(g in c('all','no_remarried','first_marriage')){
  q<-m[,paste0(g,c('_top','_aff','_middle')),drop=FALSE]
  a<-100*q[2,]/q[1,];k<-q[3,]/q[1,]
  v<-c(top3=a[1],aff3=a[2],gap3=a[1]-a[2],gapmid3=a[1]-a[3],topkids=k[1],gapkids=k[1]-k[2]); names(v)<-paste(g,c('top3','aff3','gap3','gapmid3','topkids','gapkids'),sep='_');val<-c(val,v)
 };val
}
years<-seq(2013,2025,3); results<-list()
for(rk in c('inc_rank','nw_rank')) {
 message('Bootstrap audit: ',rk)
 v<-if(rk=='inc_rank')'inc_measure' else 'NETWORTH'; per<-list()
 for(yr in years) {
  s<-rec[year==yr];s1<-s[implicate==1]
  f<-sprintf('data_raw/rw/p%02d_rw1.dta',yr%%100)
  rw<-as.data.table(zap_labels(read_dta(f)));setnames(rw,tolower(names(rw)));ix<-match(s1$Y1,rw$y1);stopifnot(!anyNA(ix))
  imp<-lapply(1:5,function(i){z<-s[implicate==i];moments(z,z$WGT,z[[rk]])})
  draws<-list(fixed=array(0,c(3,length(labels),999)),reranked=array(0,c(3,length(labels),999)))
  for(b in 1:999){
   w<-rw[[paste0('wt1b',b)]][ix]*rw[[paste0('mm',b)]][ix];w[is.na(w)]<-0
   r<-s1[[rk]];for(a in unique(s1$age5)){j<-which(s1$age5==a);r[j]<-prank(s1[[v]][j],w[j])}
   draws$fixed[,,b]<-moments(s1,w,s1[[rk]]);draws$reranked[,,b]<-moments(s1,w,r)
  }
  per[[as.character(yr)]]<-list(imp=imp,draws=draws)
  message('  completed ',yr)
 }
 for(yrs in list(years,c(2019,2022,2025))) {
  p<-per[as.character(yrs)];M<-lapply(1:5,function(i)Reduce('+',lapply(p,function(z)z$imp[[i]])))
  Q<-sapply(M,statfun);est<-rowMeans(Q);B<-apply(Q,1,var);base<-M[[1]]
  # Jacobian of pooled statistics wrt moments. Central differences on positive denominators.
  grad<-sapply(seq_along(base),function(j){h<-1e-6;up<-dn<-base;up[j]<-up[j]+h;dn[j]<-dn[j]-h;(statfun(up)-statfun(dn))/(2*h)})
  for(mode in c('fixed','reranked')) {
   V<-Reduce('+',lapply(p,function(z){D<-matrix(z$draws[[mode]],nrow=length(base));apply(grad%*%D,1,var)}))
   se<-sqrt(V+1.2*B)
   results[[length(results)+1]]<-data.table(rank=rk,years=paste(range(yrs),collapse='-'),method=mode,stat=names(est),est=est,se=se,lower=est-1.96*se,upper=est+1.96*se)
  }
 }
 saveRDS(per,paste0('audit/results/moments_',rk,'.rds'))
}
fwrite(rbindlist(results),'audit/results/replicate_audit.csv')
# Preserve environment and provenance for diagnostics.
writeLines(capture.output(sessionInfo()),'audit/results/sessionInfo.txt')
rawfiles<-c(list.files('data_raw',recursive=TRUE,full.names=TRUE),list.files('audit/rw',full.names=TRUE))
fwrite(data.table(file=rawfiles,md5=unname(tools::md5sum(rawfiles)),bytes=file.size(rawfiles)),'audit/results/input_hashes.csv')
message('Audit diagnostics complete')
