# Independent review. All changes of definition below are labeled sensitivities;
# no production data, estimands, code, charts, or text are overwritten.
suppressPackageStartupMessages({library(data.table); library(haven)})
setDTthreads(2)
outdir <- 'audit/review_20261015/results'
wm <- function(x, w) sum(x*w)/sum(w)
midrank <- function(x,w) {o <- order(x); ans <- numeric(length(x)); ans[o] <- (cumsum(w[o])-w[o]/2)/sum(w); ans}
qweighted <- function(x,w,p) {o <- order(x); x[o][which(cumsum(w[o])/sum(w)>=p-1e-12)[1]]}
files <- c('1989'='p89i6','1992'='p92i4','1995'='p95i6','1998'='p98i6','2001'='p01i6','2004'='p04i6','2007'='p07i6','2010'='p10i6','2013'='p13i6','2016'='p16i6','2019'='p19i6','2022'='p22i6','2025'='p25i6')
rel <- c('x108','x114','x120','x126','x132','x202','x208','x214','x220','x226')
liv <- c('x112','x118','x124','x130','x136','x206','x212','x218','x224','x230')
raw_checks <- list(); dat <- list()
for(yr in names(files)) {
 s <- fread(paste0('data_raw/xlsx/SCFP',yr,'.csv'), integer64='double')
 s[,NORMINC:=as.numeric(NORMINC)]
 if('X1'%in%names(s)) setnames(s,c('X1','XX1'),c('Y1','YY1'))
 f <- paste0('data_raw/full/',files[[yr]],'.dta'); nm <- names(read_dta(f,n_max=1))
 want <- c('y1','x1','x42001','x5910','x7377','x7392','x8023','x105','x19',rel,liv)
 z <- as.data.table(zap_labels(read_dta(f,col_select=dplyr::all_of(nm[tolower(nm)%in%want]))))
 setnames(z,tolower(names(z))); if('x1'%in%names(z))setnames(z,'x1','y1')
 for(v in setdiff(want,c('y1','x1'))) if(!v%in%names(z)) z[,(v):=NA_real_]
 stopifnot(!anyDuplicated(s$Y1),!anyDuplicated(z$y1),setequal(s$Y1,z$y1))
 z <- z[match(s$Y1,y1)]
 R <- as.matrix(z[,..rel]); L <- as.matrix(z[,..liv])
 count <- rowSums((R==4)|(R==13)|(R==36),na.rm=TRUE)
 away <- rowSums(((R==4)|(R==13)|(R==36)) & L==5,na.rm=TRUE)
 raw_checks[[yr]] <- data.table(year=as.integer(yr),rows=nrow(s),families=uniqueN(s$YY1),
   max_weight_error=max(abs(5*s$WGT/z$x42001-1)),roster_disagreements=sum(s$KIDS!=count),
   min_x5910=min(z$x5910),max_x5910=max(z$x5910),missing_x5910=sum(is.na(z$x5910)),
   max_implicate_population_difference=diff(range(tapply(s$WGT,s$Y1%%10,sum))))
 keep <- s$AGE>=45 & s$AGE<=64
 d <- s[keep,.(Y1,YY1,WGT,AGE,KIDS,MARRIED,INCOME,NORMINC,NETWORTH)]
 z <- z[keep]; d[,`:=`(year=as.integer(yr),implicate=Y1%%10,age5=5*floor(AGE/5),
   kids_away=pmax(z$x5910,0),away_roster=away[keep],
   previous_r=z$x7377,previous_s=z$x7392,marital_r=z$x8023,marital_s=z$x105,spouse_age=z$x19)]
 d[,`:=`(kids=KIDS+kids_away,inc=fifelse(is.na(NORMINC),INCOME,NORMINC))]
 d[,w:=WGT/sum(WGT),by=implicate]
 d[,ir:=midrank(inc,WGT),by=.(implicate,age5)]
 d[,nr:=midrank(NETWORTH,WGT),by=.(implicate,age5)]
 dat[[yr]] <- d
}
d <- rbindlist(dat); fwrite(rbindlist(raw_checks),file.path(outdir,'raw_checks.csv'))
production <- readRDS('output/kids_main.rds')
ix <- match(paste(d$year,d$Y1),paste(production$year,production$Y1))
stopifnot(!anyNA(ix),nrow(d)==nrow(production),all(d$kids==production$kids[ix]),
 max(abs(d$ir-production$inc_rank[ix]))<1e-12,max(abs(d$nr-production$nw_rank[ix]))<1e-12,
 max(abs(d$w-production$w[ix]))<1e-12)
writeLines('Independent raw-data reconstruction exactly matches production sample, counts, weights and ranks.',file.path(outdir,'reconstruction.txt'))
rec <- d[year>=2013]
rec[,`:=`(baseline_flag=MARRIED==1&(previous_r==2|previous_s==2),
 strict_first=MARRIED==1 & marital_r==1 & marital_s==1 & previous_r==1 & previous_s==1)]
# Cohabiters can report a first PAST marriage. A current cohabiting union plus any
# prior legal marriage is a broader potential blended-family exclusion.
rec[,broad_flag:=baseline_flag | (MARRIED==1 & (marital_r!=1|marital_s!=1) &
 (previous_r%in%c(1,2)|previous_s%in%c(1,2)))]
# Overlap cannot be observed. The maximum possible overlap is bounded by BOTH counts.
rec[,kids_min:=kids-pmin(away_roster,kids_away)]
rec[,kids_drop_all_away:=kids-away_roster]
saveRDS(rec,file.path(outdir,'independent_recent.rds'))
desc <- list()
for(rk in c('ir','nr')) {
 a <- copy(rec)[,band:=fifelse(get(rk)>.99,'top1',fifelse(get(rk)>.9,'p90_99','other'))]
 desc[[rk]] <- a[,.(share3=100*wm(kids>=3,w),mean_kids=wm(kids,w),
  possible_overlap_pct=100*wm(away_roster>0 & kids_away>0,w),
  share3_min=100*wm(kids_min>=3,w),share3_drop_all_away=100*wm(kids_drop_all_away>=3,w),
  strict_first_pct=100*wm(strict_first,w),additional_blended_pct=100*wm(broad_flag & !baseline_flag,w),
  partnered_pct=100*wm(MARRIED==1,w)),by=band][,rank:=rk]
}
fwrite(rbindlist(desc),file.path(outdir,'measurement.csv'))

# Replicate inference is implemented independently via analytic ratio influence
# functions. Wave samples are treated as independent, as in the production estimator.
# These are pointwise 95% intervals, not simultaneous tests across specifications.
specs <- c('baseline','max_overlap_all','max_overlap_top_only','drop_all_away',
 'exclude_baseline_flag','exclude_broader_blended','strict_current_first_marriage',
 'partnered_only','older_spouses','exactly3','fourplus','mean_kids')
select <- function(s,name,rank) {
 k <- s$kids; keep <- rep(TRUE,nrow(s))
 if(name=='max_overlap_all') k <- s$kids_min
 if(name=='max_overlap_top_only') k <- ifelse(rank>.99,s$kids_min,s$kids)
 if(name=='drop_all_away') k <- s$kids_drop_all_away
 if(name=='exclude_baseline_flag') keep <- !s$baseline_flag
 if(name=='exclude_broader_blended') keep <- !s$broad_flag
 if(name=='strict_current_first_marriage') keep <- s$strict_first
 if(name=='partnered_only') keep <- s$MARRIED==1
 if(name=='older_spouses') keep <- s$AGE>=50 & (s$MARRIED==2|s$spouse_age>=45)
 y <- if(name=='mean_kids') k else if(name=='exactly3') 100*(k==3) else if(name=='fourplus') 100*(k>=4) else 100*(k>=3)
 list(keep=keep%in%TRUE,y=y)
}
mom <- function(s,w,r) {
 w <- w/sum(w)
 ans <- matrix(0,4,length(specs),dimnames=list(c('dt','nt','da','na'),specs))
 for(j in seq_along(specs)) {
  a <- select(s,specs[j],r); t <- a$keep&r>.99; f <- a$keep&r>.9&r<=.99
  ans[,j] <- c(sum(w[t]),sum(w[t]*a$y[t]),sum(w[f]),sum(w[f]*a$y[f]))
 }
 ans
}
results <- list(); wave_results <- list()
for(rk in c('ir','nr')) {
 variable <- if(rk=='ir')'inc' else 'NETWORTH'
 base <- lapply(1:5,function(i)Reduce(`+`,lapply(split(rec[implicate==i],by='year'),function(s)mom(s,s$WGT,s[[rk]]))))
 estimates <- sapply(base,function(m)m[2,]/m[1,]-m[4,]/m[3,])
 top <- sapply(base,function(m)m[2,]/m[1,]); aff <- sapply(base,function(m)m[4,]/m[3,])
 b <- base[[1]]
 deriv <- rbind(-b[2,]/b[1,]^2,1/b[1,],b[4,]/b[3,]^2,-1/b[3,])
 vs <- numeric(length(specs))
 for(yr in sort(unique(rec$year))) {
  s <- rec[year==yr & implicate==1]
  rw <- as.data.table(zap_labels(read_dta(sprintf('data_raw/rw/p%02d_rw1.dta',yr%%100))))
  setnames(rw,tolower(names(rw))); ix <- match(s$Y1,rw$y1); stopifnot(!anyNA(ix))
  draws <- matrix(NA_real_,999,length(specs)); gaps <- draws
  for(j in 1:999) {
   w <- rw[[paste0('wt1b',j)]][ix]*rw[[paste0('mm',j)]][ix]; w[is.na(w)]<-0
   stopifnot(all(is.finite(w)),all(w>=0),sum(w)>0)
   r <- numeric(nrow(s))
   for(a in unique(s$age5)) {ids<-which(s$age5==a);stopifnot(sum(w[ids])>0); r[ids]<-midrank(s[[variable]][ids],w[ids])}
   m <- mom(s,w,r); draws[j,]<-colSums(deriv*m); gaps[j,]<-m[2,]/m[1,]-m[4,]/m[3,]
  }
  vs <- vs+apply(draws,2,var)
  q <- sapply(1:5,function(i){z<-rec[year==yr & implicate==i];m<-mom(z,z$WGT,z[[rk]]);m[2,]/m[1,]-m[4,]/m[3,]})
  wave_results[[paste(rk,yr)]] <- data.table(rank=rk,year=yr,spec=specs,gap=rowMeans(q),se=sqrt(apply(gaps,2,var)+1.2*apply(q,1,var)))
  message(rk,' ',yr,' independent replicates complete')
 }
 est <- rowMeans(estimates); B <- apply(estimates,1,var); se <- sqrt(vs+1.2*B)
 df <- ifelse(B>0,4*(1+vs/(1.2*B))^2,Inf)
 results[[rk]] <- data.table(rank=rk,spec=specs,top=rowMeans(top),affluent=rowMeans(aff),gap=est,
  se=se,lower=est-1.96*se,upper=est+1.96*se,p=2*pnorm(-abs(est/se)),
  mi_df=df,t_lower=est-qt(.975,df)*se,t_upper=est+qt(.975,df)*se)
}
fwrite(rbindlist(results),file.path(outdir,'independent_inference.csv'))
fwrite(rbindlist(wave_results),file.path(outdir,'wave_inference.csv'))
writeLines(capture.output(sessionInfo()),file.path(outdir,'sessionInfo.txt'))
message('Independent checks finished')
