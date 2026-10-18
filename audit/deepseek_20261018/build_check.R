suppressPackageStartupMessages({library(data.table); library(haven)})
years <- c(2013,2016,2019,2022,2025)
rel <- c('x108','x114','x120','x126','x132','x202','x208','x214','x220','x226')
liv <- c('x112','x118','x124','x130','x136','x206','x212','x218','x224','x230')
files <- c(`2013`='p13i6',`2016`='p16i6',`2019`='p19i6',`2022`='p22i6',`2025`='p25i6')
read_wave <- function(y){
  s <- fread(sprintf('data_raw/xlsx/SCFP%d.csv',y), integer64='double')
  s <- s[,.(year=y,Y1,YY1,WGT,AGE,KIDS,MARRIED,INCOME,NORMINC,NETWORTH,EQUITY)]
  s[,implicate:=as.integer(Y1-10*YY1)]
  f <- sprintf('data_raw/full/%s.dta',files[[as.character(y)]])
  nm <- names(read_dta(f,n_max=1))
  want <- c('y1','x5910','x7377','x7392','x8023','x105','x19',rel,liv)
  z <- as.data.table(zap_labels(read_dta(f,col_select=dplyr::all_of(nm[tolower(nm)%in%want]))))
  setnames(z,tolower(names(z)))
  for(v in setdiff(want,'y1')) if(!v%in%names(z)) z[,(v):=NA_real_]
  m <- merge(s,z,by.x='Y1',by.y='y1',all.x=TRUE,sort=FALSE)
  stopifnot(nrow(m)==nrow(s),!anyNA(m$Y1))
  R <- as.matrix(m[,..rel]); L <- as.matrix(m[,..liv])
  m[,away_roster:=rowSums((R==4|R==13|R==36)&L==5,na.rm=TRUE)]
  m[,kids:=KIDS+pmax(x5910,0)]
  m[,inc_measure:=fifelse(is.na(NORMINC),INCOME,NORMINC)]
  m[,age5:=floor(AGE/5)*5]
  m[,case:=paste(year,YY1)]
  m[,remarried:=MARRIED==1 & (x7377==2|x7392==2)]
  m[,strict_first:=MARRIED==1 & x8023==1 & x105==1 & x7377==1 & x7392==1]
  m[,older:=AGE>=50 & (MARRIED==2 | x19>=45)]
  m[,kids_min:=kids-pmin(away_roster,pmax(x5910,0))]
  m
}
d <- rbindlist(lapply(years,read_wave),use.names=TRUE)
setorder(d,year,Y1)
cat('rows',nrow(d),'families',uniqueN(d$case),'\n')
cat('kids NA',sum(is.na(d$kids)),'x5910 range',range(d$x5910),'\n')
cat('age range',range(d$AGE),'\n')
# sample
s <- d[AGE>=45 & AGE<=64]
cat('sample rows',nrow(s),'families',uniqueN(s$case),'\n')
prank <- function(x,w){o<-order(x); r<-numeric(length(x)); r[o]<-(cumsum(w[o])-w[o]/2)/sum(w); r}
rank_age <- function(x,w,age5){r<-numeric(length(x)); for(a in unique(age5)){j<-which(age5==a); r[j]<-prank(x[j],w[j])}; r}
s[,inc_rank:=rank_age(inc_measure,WGT,age5),by=.(year,implicate)]
s[,nw_rank:=rank_age(NETWORTH,WGT,age5),by=.(year,implicate)]
bin_lv <- c('P0-10','P10-20','P20-30','P30-40','P40-50','P50-60','P60-70','P70-80','P80-90','P90-95','P95-99','Top 1%')
binit <- function(r) factor(as.character(cut(pmin(r,1),c(0,seq(.1,.9,.1),.95,.99,1),labels=bin_lv,include.lowest=TRUE)),bin_lv)
s[,inc_bin:=binit(inc_rank)]; s[,nw_bin:=binit(nw_rank)]
s[,w:=WGT/sum(WGT),by=.(year,implicate)]
# pooled curve (mean over implicates of ratio moments)
wmean <- function(x,w) sum(x*w,na.rm=TRUE)/sum(w[!is.na(x)])
curve <- function(binvar){s[,.(share3=100*wmean(kids>=3,w),exact3=100*wmean(kids==3,w),fourplus=100*wmean(kids>=4,w),meankids=wmean(kids,w),n=uniqueN(case)),by=binvar][order(get(binvar))]}
cat('\nINCOME CURVE\n'); print(curve('inc_bin'))
cat('\nWEALTH CURVE\n'); print(curve('nw_bin'))
# gap top1 vs 90-99, pooled moments per implicate
gapfun <- function(dat,rankvar,outcome){
  den_t<-sum(dat$WGT[dat[[rankvar]]>.99]); num_t<-sum(dat$WGT[dat[[rankvar]]>.99]*outcome[dat[[rankvar]]>.99])
  den_a<-sum(dat$WGT[dat[[rankvar]]>.9 & dat[[rankvar]]<=.99]); num_a<-sum(dat$WGT[dat[[rankvar]]>.9 & dat[[rankvar]]<=.99]*outcome[dat[[rankvar]]>.9 & dat[[rankvar]]<=.99])
  100*(num_t/den_t-num_a/den_a)
}
for(rk in c('inc_rank','nw_rank')){
  for(oc in c('kids>=3','kids==3','kids>=4','kids')){
    y <- switch(oc,`kids>=3`=as.numeric(s$kids>=3),`kids==3`=as.numeric(s$kids==3),`kids>=4`=as.numeric(s$kids>=4),`kids`=s$kids)
    vals <- sapply(1:5,function(i){z<-s[implicate==i]; yy<-switch(oc,`kids>=3`=as.numeric(z$kids>=3),`kids==3`=as.numeric(z$kids==3),`kids>=4`=as.numeric(z$kids>=4),`kids`=z$kids); gapfun(z,rk,yy)})
    cat(rk,oc,'mean',mean(vals),'vals',paste(round(vals,3),collapse=','),'\n')
  }
}
saveRDS(s,'audit/deepseek_20261018/deep_sample.rds')
