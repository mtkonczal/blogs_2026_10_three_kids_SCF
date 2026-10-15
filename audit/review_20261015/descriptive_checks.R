suppressPackageStartupMessages(library(data.table))
setDTthreads(2)
dir <- 'audit/review_20261015/results'
d <- readRDS('output/kids_main.rds')
wm <- function(x,w)sum(x*w)/sum(w)
rankmid <- function(x,w){o<-order(x);r<-numeric(length(x));r[o]<-(cumsum(w[o])-w[o]/2)/sum(w);r}
gap <- function(s,rk='inc_rank',weight='w') {
 t<-s[get(rk)>.99,.(top=100*wm(kids>=3,get(weight))),by=implicate]
 a<-s[get(rk)>.9&get(rk)<=.99,.(aff=100*wm(kids>=3,get(weight))),by=implicate]
 z<-merge(t,a,by='implicate');data.table(top=mean(z$top),affluent=mean(z$aff),gap=mean(z$top-z$aff))
}
hist<-rbindlist(lapply(c('Production mix','Actual income throughout'),function(mode){
 s<-copy(d);if(mode!='Production mix')s[,inc_rank:=rankmid(INCOME,WGT),by=.(year,implicate,age5)]
 rbindlist(lapply(levels(s$period),function(p)gap(s[period==p])[,`:=`(period=p,mode=mode)]))
}))
fwrite(hist,file.path(dir,'historical_income_definition.csv'))
rec<-d[year>=2013]; sens<-list()
add<-function(s,label,rk='inc_rank',weight='w')sens[[length(sens)+1]]<<-gap(s,rk,weight)[,spec:=label]
add(rec,'baseline')
add(rec,'population weighted waves',weight='WGT')
for(drop_year in seq(2013,2025,3))add(rec[year!=drop_year],paste('omit',drop_year))
for(a in seq(45,60,5))add(rec[age5==a],paste('age',a,'to',a+4))
s<-copy(rec);s[,inc_rank:=rankmid(INCOME,WGT),by=.(year,implicate,age5)];add(s,'actual income')
s<-copy(rec);s[,inc_rank:=rankmid(inc_measure,WGT),by=.(year,implicate)];add(s,'rank all 45-64 together')
s<-copy(rec);s[,inc_rank:=rev(rankmid(rev(inc_measure),rev(WGT))),by=.(year,implicate,age5)];add(s,'reverse tie order')
fwrite(rbindlist(sens),file.path(dir,'point_sensitivities.csv'))

# Source-of-income distinction: business income is mixed labor/capital, so do not
# silently assign all of it to capital. NORMINC does not have a source decomposition.
sx<-readRDS('data/scf_linked.rds')
src<-merge(rec,sx[,.(year,Y1,WAGEINC,BUSSEFARMINC,INTDIVINC,KGINC,SSRETINC,TRANSFOTHINC)],by=c('year','Y1'))
src[,capital:=INTDIVINC+KGINC]
src[,band:=fifelse(inc_rank>.99,'top1',fifelse(inc_rank>.9,'p90_99','other'))]
cap<-src[,.(wage_share_of_total=100*sum(w*WAGEINC)/sum(w*INCOME),business_share_of_total=100*sum(w*BUSSEFARMINC)/sum(w*INCOME),
 interest_dividends_gains_share_of_total=100*sum(w*capital)/sum(w*INCOME),
 families_majority_interest_dividends_gains=100*wm(capital>.5*INCOME & INCOME>0,w)),by=band]
fwrite(cap,file.path(dir,'income_composition.csv'))
cat('Point sensitivities and income composition complete\n')
