suppressPackageStartupMessages({library(data.table); library(haven)})
s <- readRDS('audit/deepseek_20261018/deep_sample.rds')
years <- c(2013,2016,2019,2022,2025)
setorder(s,year,Y1)
prank <- function(x,w){o<-order(x); r<-numeric(length(x)); r[o]<-(cumsum(w[o])-w[o]/2)/sum(w); r}
rank_age <- function(x,w,age5){r<-numeric(length(x)); for(a in unique(age5)){j<-which(age5==a); r[j]<-prank(x[j],w[j])}; r}
# moments: den top, num top (3+), den aff, num aff, and mean kids
mom <- function(dat,w,r){
  w <- w/sum(w)
  t <- r>.99; a <- r>.9 & r<=.99
  c(dt=sum(w[t]), nt=sum(w[t]*(dat$kids[t]>=3)), dta=sum(w[a]), nta=sum(w[a]*(dat$kids[a]>=3)),
    kt=sum(w[t]*dat$kids[t]), ka=sum(w[a]*dat$kids[a]),
    x3t=sum(w[t]*(dat$kids[t]==3)), x3a=sum(w[a]*(dat$kids[a]==3)),
    f4t=sum(w[t]*(dat$kids[t]>=4)), f4a=sum(w[a]*(dat$kids[a]>=4)))
}
stat <- function(m){
  c(top3=100*m['nt']/m['dt'], aff3=100*m['nta']/m['dta'], gap3=100*(m['nt']/m['dt']-m['nta']/m['dta']),
    topk=m['kt']/m['dt'], affk=m['ka']/m['dta'], gapk=m['kt']/m['dt']-m['ka']/m['dta'],
    topx3=100*m['x3t']/m['dt'], affx3=100*m['x3a']/m['dta'], gapx3=100*(m['x3t']/m['dt']-m['x3a']/m['dta']),
    top4=100*m['f4t']/m['dt'], aff4=100*m['f4a']/m['dta'], gap4=100*(m['f4t']/m['dt']-m['f4a']/m['dta']))
}
# read replicate weights into list
RW <- list()
for(y in years){
  rw <- as.data.table(zap_labels(read_dta(sprintf('data_raw/rw/p%02d_rw1.dta',y%%100))))
  setnames(rw,tolower(names(rw)))
  s1 <- s[year==y & implicate==1]
  ix <- match(s1$Y1,rw$y1); stopifnot(!anyNA(ix))
  M <- sapply(1:999,function(b) rw[[paste0('wt1b',b)]][ix]*rw[[paste0('mm',b)]][ix])
  M[is.na(M)] <- 0
  RW[[as.character(y)]] <- M
}
run <- function(rankvar, wmode=c('equal','pop')){
  wmode <- match.arg(wmode)
  # point moments per implicate, pooled
  M <- lapply(1:5,function(i){
    z <- lapply(years,function(y){d<-s[year==y & implicate==i]
      w <- if(wmode=='equal') d$WGT/sum(d$WGT) else d$WGT
      mom(d,w,d[[rankvar]])})
    Reduce('+',z)
  })
  Q <- sapply(M,stat); est <- rowMeans(Q); B <- apply(Q,1,var)
  base <- M[[1]]
  # numerical gradient
  grad <- sapply(seq_along(base),function(j){h<-1e-6;u<-base;d<-base;u[j]<-u[j]+h;d[j]<-d[j]-h;(stat(u)-stat(d))/(2*h)})
  V <- 0
  for(y in years){
    d <- s[year==y & implicate==1]; R <- RW[[as.character(y)]]
    D <- matrix(0,length(base),999)
    for(b in 1:999){
      w <- R[,b]
      if(wmode=='pop') w <- w/sum(w)
      r <- rank_age(d[[rankvar]],w,d$age5)
      D[,b] <- mom(d,w,r)
    }
    V <- V + apply(grad %*% D,1,var)
  }
  se <- sqrt(V+1.2*B)
  data.table(stat=names(est),est=est,se=se,lower=est-1.96*se,upper=est+1.96*se,p=2*pnorm(-abs(est/se)))
}
cat('=== income equal ===\n'); print(run('inc_rank','equal'),digits=3)
cat('=== wealth equal ===\n'); print(run('nw_rank','equal'),digits=3)
cat('=== income pop ===\n'); print(run('inc_rank','pop'),digits=3)
cat('=== wealth pop ===\n'); print(run('nw_rank','pop'),digits=3)
