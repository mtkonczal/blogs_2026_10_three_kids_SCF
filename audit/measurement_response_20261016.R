# Supplemental measurement diagnostics for attack_memo_codex.md.
# Run from the repository root: Rscript audit/measurement_response_20261016.R
# Added October 16, 2026. These checks do not change production files or specifications.
# Universe: heads 45-64, 2013-2025. Keep production age/year ranks; equal wave weights.
# Strict marriage requires both roster statuses and legal statuses to be married,
# plus both spouses reporting a first marriage. This still cannot exclude all
# premarital children or identify births shared by the current spouses.
# count_minors_away = X5910 (none recoded to zero) + code-4 roster children
# aged <=17 who are not flagged as living elsewhere (residence 0 is inapplicable
# for minors). It excludes ALL adult roster children, including genuine children.
# Interpreting this as a lower count assumes resident minors are genuine children,
# not children-in-law; the codebook does not logically guarantee that assumption.
# count_dedup assumes roster-away flags capture possible overlap with X5910;
# subtract at most min(roster-away count, X5910), so the total never falls below X5910.
# Adverse comparisons correct only the top group; both-sided checks recode both.
# These checks address specified overcounts, not all possible survey/reporting errors.
# Confidence intervals: production method, 999 re-ranked replicates on implicate 1,
# independent-wave delta-method sampling variance plus 1.2 x pooled MI variance.
# Pointwise normal intervals, not simultaneous bounds for a partially identified set.
# Reproduce baseline estimates and SEs from R/25 as a required sanity check.

suppressPackageStartupMessages({library(data.table);library(haven)})
options(width=190)
main <- readRDS('output/kids_main.rds')[year >= 2013]
rel <- c('x108','x114','x120','x126','x132','x202','x208','x214','x220','x226')
liv <- c('x112','x118','x124','x130','x136','x206','x212','x218','x224','x230')
age <- c('x110','x116','x122','x128','x134','x204','x210','x216','x222','x228')
extra <- rbindlist(lapply(seq(2013,2025,3),function(yr) {
 f <- sprintf('data_raw/full/p%02di6.dta',yr %% 100); nm <- names(read_dta(f,n_max=1))
 want <- c('y1',rel,liv,age,'x8023','x105','x104','x7372','x7018')
 stopifnot(all(want %in% tolower(nm)))
 s <- as.data.table(zap_labels(read_dta(f,col_select=dplyr::all_of(nm[tolower(nm) %in% want])))); setnames(s,tolower(names(s)))
 R <- as.matrix(s[,..rel]); L <- as.matrix(s[,..liv]); A <- as.matrix(s[,..age])
 stopifnot(!anyNA(R),!anyNA(L),!anyNA(A))
 s[,`:=`(roster_count=rowSums(R==4),roster_away=rowSums(R==4 & L==5),resident_minors=rowSums(R==4 & A>0 & A<=17 & L!=5),roster_adults=rowSums(R==4 & A>=18),year=yr)]
 s[,.(year,Y1=y1,roster_count,roster_away,resident_minors,roster_adults,x8023,x105,x104,x7372,x7018)]
}))
d <- merge(main,extra,by=c('year','Y1'))
stopifnot(nrow(d)==nrow(main),all(d$KIDS==d$roster_count))
d[,`:=`(count_base=kids,count_dedup=kids-pmin(roster_away,kids_away),count_minors_away=kids_away+resident_minors,count_away=kids_away)]
d[,strict_couple:=x8023==1 & x105==1 & x7372==1 & x7018==1 & x7377==1 & x7392==1]
d[,mature:=AGE>=50 & (MARRIED!=1 | x104>=45)]
for (v in c('count_dedup','count_minors_away','count_away')) stopifnot(all(d[[v]]<=d$count_base),all(d[[v]]>=0))
pr <- function(x,w) {o<-order(x); r<-numeric(length(x));r[o]<-(cumsum(w[o])-w[o]/2)/sum(w);r}
# Each row changes a diagnostic outcome or subgroup only; baseline ranks and wave normalization stay fixed.
scenarios <- data.table(id=c('baseline','dedup_both','dedup_top_only','exclude_all_adult_roster_both','exclude_all_adult_roster_top_only','away_both','away_top_only','strict_current_first_marriage','mature_sample','strict_mature','strict_dedup','strict_adult_removed'),
 outcome=c('count_base','count_dedup','count_dedup','count_minors_away','count_minors_away','count_away','count_away','count_base','count_base','count_base','count_dedup','count_minors_away'),
 filter=c('all','all','all','all','all','all','all','strict','mature','strict_mature','strict','strict'),
 adverse=c(FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE))
selection <- function(s,f) switch(f,all=rep(TRUE,nrow(s)),strict=s$strict_couple,mature=s$mature,strict_mature=s$strict_couple & s$mature)
# Pooled normalized-wave moments; average the five implicate-specific ratios, matching production inference.
mom <- function(s,w,r) {
 w<-w/sum(w);out<-matrix(0,nrow(scenarios),4)
 for(j in seq_len(nrow(scenarios))) {
 z<-scenarios[j]; sel<-selection(s,z$filter); top<-sel & r>.99; aff<-sel & r>.9 & r<=.99
 yt<-s[[z$outcome]]>=3; ya<-if(z$adverse) s$count_base>=3 else yt
 out[j,]<-c(sum(w[top]),sum(w[top]*yt[top]),sum(w[aff]),sum(w[aff]*ya[aff]))
 };out
}
getstat <- function(m) cbind(top=100*m[,2]/m[,1],aff=100*m[,4]/m[,3],gap=100*(m[,2]/m[,1]-m[,4]/m[,3]))
# Extra diagnostic: three-plus among families reporting at least two children.
scenarios <- rbind(scenarios,data.table(id='three_plus_among_two_plus',outcome='count_base',filter='parents2',adverse=FALSE))
selection <- function(s,f) switch(f,all=rep(TRUE,nrow(s)),strict=s$strict_couple,mature=s$mature,strict_mature=s$strict_couple & s$mature,parents2=s$count_base>=2)
res<-list()
for(rk in c('inc_rank','nw_rank')) {
 v<-if(rk=='inc_rank') 'inc_measure' else 'NETWORTH'
 M<-lapply(1:5,function(i) Reduce(`+`,lapply(seq(2013,2025,3),function(yr) {s<-d[year==yr & implicate==i];mom(s,s$WGT,s[[rk]])})))
 Q<-simplify2array(lapply(M,getstat)); est<-apply(Q,c(1,2),mean); B<-apply(Q[,3,],1,var);base<-M[[1]]
 G<-cbind(-100*base[,2]/base[,1]^2,100/base[,1],100*base[,4]/base[,3]^2,-100/base[,3])
 V<-numeric(nrow(scenarios))
 for(yr in seq(2013,2025,3)) {
  s<-d[year==yr & implicate==1];setorder(s,Y1)
  rw<-as.data.table(zap_labels(read_dta(sprintf('data_raw/rw/p%02d_rw1.dta',yr %% 100))));setnames(rw,tolower(names(rw)));ix<-match(s$Y1,rw$y1);stopifnot(!anyNA(ix))
  lin<-sapply(1:999,function(b) {
   w<-rw[[paste0('wt1b',b)]][ix]*rw[[paste0('mm',b)]][ix];w[is.na(w)]<-0
   r<-numeric(nrow(s));for(a in unique(s$age5)) {j<-which(s$age5==a);r[j]<-pr(s[[v]][j],w[j])}
   rowSums(G*mom(s,w,r))
  })
  V<-V+apply(lin,1,var);message(rk,': ',yr,' finished')
 }
 se<-sqrt(V+1.2*B)
 res[[rk]]<-cbind(scenarios,data.table(rank=rk,top=est[,1],aff=est[,2],gap=est[,3],se=se,lower=est[,3]-1.96*se,upper=est[,3]+1.96*se))
}
ans<-rbindlist(res)
z<-readRDS('output/kids_rep5.rds')[stat=='all_gap3',.(rank,base_gap=est,base_se=se)]
ck<-merge(ans[id=='baseline'],z,by='rank')
stopifnot(max(abs(ck$gap-ck$base_gap))<1e-6,max(abs(ck$se-ck$base_se))<1e-3)
print(ans[,.(rank,id,top,aff,gap,se,lower,upper)],digits=5)
dir.create('audit/results', recursive=TRUE, showWarnings=FALSE)
fwrite(ans,'audit/results/measurement_response_20261016.csv')
