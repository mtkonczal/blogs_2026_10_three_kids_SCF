suppressPackageStartupMessages({library(data.table);library(haven)})
source('R/scf_functions.R')
d<-readRDS('output/kids_main.rds'); sx<-readRDS('data/scf_linked.rds')
a<-list()
for(yr in seq(2013,2025,3)) {
 f<-sprintf('data_raw/full/p%02di6.dta',yr%%100)
 z<-as.data.table(zap_labels(read_dta(f,col_select=c(y1,x5702,x5729,x8023,x105,x4100,x4700))))
 z<-merge(d[year==yr],z,by.x='Y1',by.y='y1',suffixes=c('','.raw'))
 z<-merge(z,sx[year==yr,.(Y1,WAGEINC)],by='Y1')
 f_bad<-z[x5729>0,sum(INCOME*WGT)/sum(x5729*WGT)]
 f_good<-z[x5702>0,sum(WAGEINC*WGT)/sum(x5702*WGT)]
 a[[as.character(yr)]]<-data.table(year=yr,inferred_deflator=f_bad,wage_deflator=f_good,percent_excess=100*(f_bad/f_good-1),workers96=sum(z$x4100==96|z$x4700==96),first_marriage_cohabiting=sum(z$first_marriage_couple%in%TRUE & z$x8023==2))
}
fwrite(rbindlist(a),'audit/results/secondary_checks.csv')
# Distinguish pooled ratios from mean of per-implicate ratios.
q<-d[year>=2013,.(share3=100*wmean(kids>=3,w)),by=.(implicate,inc_bin)]
p<-d[year>=2013,.(pooled=100*wmean(kids>=3,w)),by=inc_bin]
p<-merge(p,q[,.(mi_mean=mean(share3)),by=inc_bin],by='inc_bin');p[,difference:=pooled-mi_mean];fwrite(p,'audit/results/mi_pooling.csv')
# Historical tail cap diagnostic, no change adopted.
fwrite(d[,.(cases_over10=uniqueN(case[x5910>10]),mean_children=wmean(kids,w),mean_children_cap10=wmean(KIDS+pmin(pmax(x5910,0),10),w)),by=period],'audit/results/topcoding.csv')
# Replicate-free rough confidence interval for hours as actually implemented.
h<-readRDS('output/kids_wealth_work.rds')$top1w_hours_gap
h[,`:=`(lower=est-1.96*se,upper=est+1.96*se)];fwrite(h,'audit/results/hours.csv')
