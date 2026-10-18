suppressPackageStartupMessages({library(data.table); library(haven)})
s <- readRDS('audit/deepseek_20261018/deep_sample.rds')
years <- c(2013,2016,2019,2022,2025)
setorder(s,year,Y1)
# add adult roster ages and minor roster counts
rel <- c('x108','x114','x120','x126','x132','x202','x208','x214','x220','x226')
liv <- c('x112','x118','x124','x130','x136','x206','x212','x218','x224','x230')
age <- c('x110','x116','x122','x128','x134','x204','x210','x216','x222','x228')
fx <- rbindlist(lapply(years,function(yr){
 f <- sprintf('data_raw/full/p%02di6.dta',yr%%100); nm <- names(read_dta(f,n_max=1))
 want <- c('y1',rel,liv,age,'x8023','x105','x104','x7372','x7018','x7377','x7392')
 z <- as.data.table(zap_labels(read_dta(f,col_select=dplyr::all_of(nm[tolower(nm)%in%want])))); setnames(z,tolower(names(z))); for(v in setdiff(want,'y1')) if(!v%in%names(z)) z[,(v):=NA_real_]
 R <- as.matrix(z[,..rel]); L <- as.matrix(z[,..liv]); A <- as.matrix(z[,..age])
 z[,`:=`(year=yr,roster_count=rowSums(R==4),roster_away=rowSums(R==4&L==5),
         resident_minors=rowSums(R==4&A>0&A<=17&L!=5),roster_adults=rowSums(R==4&A>=18))]
 z[,.(year,Y1=y1,roster_count,roster_away,resident_minors,roster_adults,x8023,x105,x104,x7372,x7018,x7377,x7392)]
}))
d <- merge(s[,.(year,Y1,implicate,WGT,AGE,KIDS,MARRIED,kids,x5910,x19,inc_measure,NETWORTH,inc_rank,nw_rank,age5,kids_min,strict_first,older,remarried)],fx,by=c('year','Y1'),all.x=TRUE)
stopifnot(nrow(d)==nrow(s),all(d$KIDS==d$roster_count))
d[,count_minors_away:=pmax(x5910,0)+resident_minors]
d[,strict_couple:=x8023==1&x105==1&x7372==1&x7018==1&x7377==1&x7392==1]
d[,mature_x104:=AGE>=50&(MARRIED!=1|x104>=45)]
d[,mature_x19:=AGE>=50&(MARRIED!=1|x19>=45)]
pr <- function(x,w){o<-order(x);r<-numeric(length(x));r[o]<-(cumsum(w[o])-w[o]/2)/sum(w);r}
ra <- function(x,w,a){r<-numeric(length(x));for(k in unique(a)){j<-which(a==k);r[j]<-pr(x[j],w[j])};r}
# generic gap for an outcome vector and optional selection
gapmom <- function(dat,w,r,y,keep=rep(TRUE,nrow(dat))){
 w<-w/sum(w); t<-keep&r>.99; a<-keep&r>.9&r<=.99
 c(dt=sum(w[t]),nt=sum(w[t]*y[t]),da=sum(w[a]),na=sum(w[a]*y[a]))
}
statgap <- function(m) 100*(m['nt']/m['dt']-m['na']/m['da'])
run <- function(rankvar, yfun, keepfun=function(z)rep(TRUE,nrow(z))){
 # point
 M <- lapply(1:5,function(i){Reduce('+',lapply(years,function(yr){z<-d[year==yr&implicate==i]; y<-yfun(z); gapmom(z,z$WGT,z[[rankvar]],y,keepfun(z))}))})
 Q <- sapply(M,statgap); est<-mean(Q); B<-var(Q)
 base<-M[[1]]; g<-100*c(-base['nt']/base['dt']^2,1/base['dt'],base['na']/base['da']^2,-1/base['da'])
 V<-0
 for(yr in years){
  z<-d[year==yr&implicate==1]; RW<-as.data.table(zap_labels(read_dta(sprintf('data_raw/rw/p%02d_rw1.dta',yr%%100))));setnames(RW,tolower(names(RW)));ix<-match(z$Y1,RW$y1)
  D<-matrix(0,4,999)
  for(b in 1:999){w<-RW[[paste0('wt1b',b)]][ix]*RW[[paste0('mm',b)]][ix];w[is.na(w)]<-0
   r<-ra(z[[rankvar]],w,z$age5); D[,b]<-gapmom(z,w,r,yfun(z),keepfun(z))}
  V<-V+apply(g%*%D,1,var)
 }
 se<-sqrt(V+1.2*B); c(est=est,se=se,lower=est-1.96*se,upper=est+1.96*se,p=2*pnorm(-abs(est/se)))
}
cat('baseline income:'); print(run('inc_rank',function(z)as.numeric(z$kids>=3)))
cat('baseline wealth:'); print(run('nw_rank',function(z)as.numeric(z$kids>=3)))
cat('conditional 2+ income:'); print(run('inc_rank',function(z)as.numeric(z$kids>=3),function(z)z$kids>=2))
cat('conditional 2+ wealth:'); print(run('nw_rank',function(z)as.numeric(z$kids>=3),function(z)z$kids>=2))
cat('minors away income:'); print(run('inc_rank',function(z)as.numeric(z$count_minors_away>=3)))
cat('minors away wealth:'); print(run('nw_rank',function(z)as.numeric(z$count_minors_away>=3)))
cat('minors away TOP ONLY income:'); print(run('inc_rank',function(z)ifelse(z$inc_rank>.99,as.numeric(z$count_minors_away>=3),as.numeric(z$kids>=3))))
cat('minors away TOP ONLY wealth:'); print(run('nw_rank',function(z)ifelse(z$nw_rank>.99,as.numeric(z$count_minors_away>=3),as.numeric(z$kids>=3))))
cat('strict couple + dedup income:'); print(run('inc_rank',function(z)as.numeric(z$kids_min>=3),function(z)z$strict_couple))
cat('strict couple + dedup wealth:'); print(run('nw_rank',function(z)as.numeric(z$kids_min>=3),function(z)z$strict_couple))
cat('mature x104 income:'); print(run('inc_rank',function(z)as.numeric(z$kids>=3),function(z)z$mature_x104))
cat('mature x19 income:'); print(run('inc_rank',function(z)as.numeric(z$kids>=3),function(z)z$mature_x19))
cat('top1 vs 95-99 only income:'); 
# custom gap top vs 95-99 (r>.95 & <=.99)
gm <- function(dat,w,r,y){w<-w/sum(w);t<-r>.99;a<-r>.95&r<=.99;c(dt=sum(w[t]),nt=sum(w[t]*y[t]),da=sum(w[a]),na=sum(w[a]*y[a]))}
# just point estimate
for(rk in c('inc_rank','nw_rank')){vals<-sapply(1:5,function(i){z<-d[implicate==i];gm(z,z$WGT,z[[rk]],as.numeric(z$kids>=3))});m<-rowMeans(vals);cat(rk,'top',100*m['nt']/m['dt'],'p95-99',100*m['na']/m['da'],'gap',100*(m['nt']/m['dt']-m['na']/m['da']),'\n')}
