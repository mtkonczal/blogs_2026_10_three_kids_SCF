suppressPackageStartupMessages({library(data.table);library(haven)})
s<-readRDS('audit/deepseek_20261018/deep_sample.rds');years<-c(2013,2016,2019,2022,2025);setorder(s,year,Y1)
# strict first marriage using x8023 etc from deep_sample? deep_sample has x8023,x105, but not x7372/x7018; strict_first already computed
pr<-function(x,w){o<-order(x);r<-numeric(length(x));r[o]<-(cumsum(w[o])-w[o]/2)/sum(w);r};ra<-function(x,w,a){r<-numeric(length(x));for(k in unique(a)){j<-which(a==k);r[j]<-pr(x[j],w[j])};r}
gapmom<-function(dat,w,r,y,keep){w<-w/sum(w);t<-keep&r>.99;a<-keep&r>.9&r<=.99;c(dt=sum(w[t]),nt=sum(w[t]*y[t]),da=sum(w[a]),na=sum(w[a]*y[a]))}
stat<-function(m)100*(m['nt']/m['dt']-m['na']/m['da'])
run<-function(rk,sub,dedup){
 # point per implicate, ranks within subset/year/implicate/age5
 M<-lapply(1:5,function(i){Reduce('+',lapply(years,function(yr){z<-s[year==yr&implicate==i];if(sub=='strict')z<-z[strict_first==TRUE]; if(sub=='older')z<-z[older==TRUE];if(dedup)z[,kids2:=kids_min] else z[,kids2:=kids];z[,rr:=ra(z[[rk]],z$WGT,z$age5)];gapmom(z,z$WGT,z$rr,as.numeric(z$kids2>=3),rep(TRUE,nrow(z)))}))})
 Q<-sapply(M,stat);est<-mean(Q);B<-var(Q);base<-M[[1]];g<-100*c(-base['nt']/base['dt']^2,1/base['dt'],base['na']/base['da']^2,-1/base['da']);V<-0
 for(yr in years){z<-s[year==yr&implicate==1];if(sub=='strict')z<-z[strict_first==TRUE]; if(sub=='older')z<-z[older==TRUE];if(dedup)z[,kids2:=kids_min] else z[,kids2:=kids];rw<-as.data.table(zap_labels(read_dta(sprintf('data_raw/rw/p%02d_rw1.dta',yr%%100))));setnames(rw,tolower(names(rw)));ix<-match(z$Y1,rw$y1);D<-matrix(0,4,999);for(b in 1:999){w<-rw[[paste0('wt1b',b)]][ix]*rw[[paste0('mm',b)]][ix];w[is.na(w)]<-0;rr<-ra(z[[rk]],w,z$age5);D[,b]<-gapmom(z,w,rr,as.numeric(z$kids2>=3),rep(TRUE,nrow(z)))};V<-V+apply(g%*%D,1,var)}
 se<-sqrt(V+1.2*B);c(est=est,se=se,lower=est-1.96*se,upper=est+1.96*se,p=2*pnorm(-abs(est/se)))
}
for(rk in c('inc_rank','nw_rank'))for(sub in c('none','strict','older'))for(dedup in c(FALSE,TRUE)){cat(rk,'sub',sub,'dedup',dedup);print(run(rk,sub,dedup))}
