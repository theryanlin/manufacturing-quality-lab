# From this project directory: Rscript tests.R
source("R/core.R")
d<-load_quality_data()
stopifnot(nrow(d)==18000,nrow(d[d$day<=20,])==12000,nrow(d[d$day>20,])==6000)
reference<-read.csv("data/reference_coefficients.csv",stringsAsFactors=FALSE)
metrics<-read.csv("data/reference_metrics.csv",stringsAsFactors=FALSE)
for(s in c("machine","material","interaction")) {
  x<-d[d$scenario==s,];model<-fit_quality_model(x)
  r<-reference[reference$scenario==s,];r<-r[match(names(model$coefficients),r$feature),]
  stopifnot(max(abs(model$coefficients-r$coefficient))<1e-4)
  expected<-metrics[metrics$scenario==s,]
  stopifnot(abs(model$brier-expected$brier)<1e-6,abs(model$auc-expected$auc)<1e-4,
            model$brier<model$baseline_brier)
  h<-heatmap_summary(x);z<-h[h$stage==3&h$machine=="B",]
  for(stage in 1:5)stopifnot(sum(h$n[h$stage==stage])==6000,sum(h$bad[h$stage==stage])==sum(x$defect))
  if(s=="material")stopifnot(z$rate-z$other_rate>.12,abs(z$std-z$std_other)<.03)
  for(batch in c("M01","M02","M03"))for(limit in c(.05,.1,.2,.3)) {
    a<-compare_plans(model,"B",batch,90,110,limit=limit)
    stopifnot(all(a$plans$risk>=0),all(a$plans$risk<=1),all(a$plans$cost>0))
    if(!is.null(a$best))stopifnot(a$best$risk<=limit,a$best$good>=160)
  }
  message("PASS: ",s)
}
message("All R data/model/decision checks passed.")
