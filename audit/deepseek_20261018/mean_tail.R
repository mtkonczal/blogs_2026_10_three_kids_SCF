suppressPackageStartupMessages({library(data.table);library(haven)})
s<-readRDS('audit/deepseek_20261018/deep_sample.rds');years<-c(2013,2016,2019,2022,2025);setorder(s,year,Y1)
pr<-function(x,w){o<-order(x);r<-numeric(length(x));r[o]<-(cumsum(w[o])-w[o]/2)/sum(w);r};ra<-function(x,w,a){r<-numeric(length(x));for(k in unique(a)){j<-which(a==k);r[j]<-pr(x[j],w[j])};r}
stat<-function(m)c(top=m['kt']/m['dt'],aff=m['ka']/m['da'],gap=m['kt']/m['dt']-m['ka']/m['da'])
run<-function(rk,cap){
 yfun<-function(z)if(cap) pmin(z$kids,cap) else z$kids
 mom<-function(dat,w,r){w<-w/sum(w);t<-r>.99;a<-r>.9&r<=.99;y<-yfun(dat);c(dt=sum(w[t]),kt=sum(w[t]*y[t]),da=sum(w[a]),ka=sum(w[a]*y[a]))}
 M<-lapply(1:5,function(i)Reduce('+',lapply(years,function(yr){z<-s[year==yr&implicate==i];mom(z,z$WGT,z[[rk]])})));Q<-sapply(M,stat);rownames(Q)<-c('top','aff','gap');est<-rowMeans(Q);B<-apply(Q,1,var);base<-M[[1]];g<-sapply(seq_along(base),function(j){h<-1e-6;u<-base;d<-base;u[j]<-u[j]+h;d[j]<-d[j]-h;(stat(u)-stat(d))/(2*h)});V<-0
 for(yr in years){z<-s[year==yr&implicate==1];rw<-as.data.table(zap_labels(read_dta(sprintf('data_raw/rw/p%02d_rw1.dta',yr%%100))));setnames(rw,tolower(names(rw)));ix<-match(z$Y1,rw$y1);D<-matrix(0,4,999);for(b in 1:999){w<-rw[[paste0('wt1b',b)]][ix]*rw[[paste0('mm',b)]][ix];w[is.na(w)]<-0;r<-ra(z[[rk]],w,z$age5);D[,b]<-mom(z,w,r)};V<-V+apply(g%*%D,1,var)}
 se<-sqrt(V+1.2*B);data.table(cap=if(cap)cap else 0,stat=rownames(Q),est=est,se=se,lower=est-1.96*se,upper=est+1.96*se,p=2*pnorm(-abs(est/se)))}
for(rk in c('inc_rank','nw_rank'))for(cap in c(0,4,5,6)){cat(rk,'cap',cap,'\n');print(run(rk,cap)[stat=='gap'],digits=4)}
