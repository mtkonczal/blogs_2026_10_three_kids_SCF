suppressPackageStartupMessages({library(data.table);library(haven)})
s<-readRDS('audit/deepseek_20261018/deep_sample.rds');years<-c(2013,2016,2019,2022,2025);setorder(s,year,Y1)
pr<-function(x,w){o<-order(x);r<-numeric(length(x));r[o]<-(cumsum(w[o])-w[o]/2)/sum(w);r};ra<-function(x,w,a){r<-numeric(length(x));for(k in unique(a)){j<-which(a==k);r[j]<-pr(x[j],w[j])};r}
mom<-function(dat,w,r){w<-w/sum(w);t<-r>.99;a<-r>.9&r<=.99;c(dt=sum(w[t]),nt=sum(w[t]*(dat$kids[t]>=3)),da=sum(w[a]),na=sum(w[a]*(dat$kids[a]>=3)))}
stat<-function(m)100*(m['nt']/m['dt']-m['na']/m['da'])
run<-function(rk,mode){
 pop<-s[implicate==1,.(P=sum(WGT)),by=year];setkey(pop,year)
 M<-lapply(1:5,function(i){Reduce('+',lapply(years,function(yr){z<-s[year==yr&implicate==i];w<-if(mode=='equal') z$WGT/sum(z$WGT) else z$WGT*pop[year==yr,P]/sum(z$WGT);mom(z,w,z[[rk]])}))})
 Q<-sapply(M,stat);est<-mean(Q);B<-var(Q);base<-M[[1]];g<-100*c(-base['nt']/base['dt']^2,1/base['dt'],base['na']/base['da']^2,-1/base['da']);V<-0
 for(yr in years){z<-s[year==yr&implicate==1];rw<-as.data.table(zap_labels(read_dta(sprintf('data_raw/rw/p%02d_rw1.dta',yr%%100))));setnames(rw,tolower(names(rw)));ix<-match(z$Y1,rw$y1);D<-matrix(0,4,999);for(b in 1:999){w<-rw[[paste0('wt1b',b)]][ix]*rw[[paste0('mm',b)]][ix];w[is.na(w)]<-0;if(mode!='equal') w<-w*pop[year==yr,P]/sum(w);r<-ra(z[[rk]],w,z$age5);D[,b]<-mom(z,w,r)};V<-V+apply(g%*%D,1,var)}
 se<-sqrt(V+1.2*B);c(est=est,se=se,lower=est-1.96*se,upper=est+1.96*se)
}
for(rk in c('inc_rank','nw_rank'))for(mode in c('equal','pop')){cat(rk,mode);print(run(rk,mode))}
