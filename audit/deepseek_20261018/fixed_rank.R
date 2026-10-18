suppressPackageStartupMessages({library(data.table);library(haven)})
s<-readRDS('audit/deepseek_20261018/deep_sample.rds');years<-c(2013,2016,2019,2022,2025);setorder(s,year,Y1)
pr<-function(x,w){o<-order(x);r<-numeric(length(x));r[o]<-(cumsum(w[o])-w[o]/2)/sum(w);r};ra<-function(x,w,a){r<-numeric(length(x));for(k in unique(a)){j<-which(a==k);r[j]<-pr(x[j],w[j])};r}
moment<-function(dat,w,r){w<-w/sum(w);t<-r>.99;a<-r>.9&r<=.99;c(dt=sum(w[t]),nt=sum(w[t]*(dat$kids[t]>=3)),da=sum(w[a]),na=sum(w[a]*(dat$kids[a]>=3)))}
stat<-function(m)100*(m['nt']/m['dt']-m['na']/m['da'])
for(rk in c('inc_rank','nw_rank')){
 M<-lapply(1:5,function(i)Reduce('+',lapply(years,function(yr){z<-s[year==yr&implicate==i];moment(z,z$WGT,z[[rk]])})));Q<-sapply(M,stat);B<-var(Q);base<-M[[1]];g<-100*c(-base['nt']/base['dt']^2,1/base['dt'],base['na']/base['da']^2,-1/base['da'])
 for(fixed in c(FALSE,TRUE)){
  V<-0
  for(yr in years){z<-s[year==yr&implicate==1];rw<-as.data.table(zap_labels(read_dta(sprintf('data_raw/rw/p%02d_rw1.dta',yr%%100))));setnames(rw,tolower(names(rw)));ix<-match(z$Y1,rw$y1);D<-matrix(0,4,999);for(b in 1:999){w<-rw[[paste0('wt1b',b)]][ix]*rw[[paste0('mm',b)]][ix];w[is.na(w)]<-0;r<-if(fixed) z[[rk]] else ra(z[[rk]],w,z$age5);D[,b]<-moment(z,w,r)};V<-V+apply(g%*%D,1,var)}
  cat(rk,'fixed',fixed,'est',mean(Q),'se',sqrt(V+1.2*B),'\n')
 }
}
