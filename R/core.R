# Core calculations use base R only. Unit IDs, never row positions, join sources.
load_quality_data <- function(path="data") {
  mes <- read.csv(file.path(path,"mes.csv"), stringsAsFactors=FALSE)
  eq <- read.csv(file.path(path,"equipment.csv"), stringsAsFactors=FALSE)
  qms <- read.csv(file.path(path,"qms.csv"), stringsAsFactors=FALSE)
  stopifnot(!anyDuplicated(mes$unit_id), !anyDuplicated(qms$unit_id),
            !anyDuplicated(paste(eq$unit_id,eq$stage)), nrow(eq)==5*nrow(mes),
            setequal(mes$unit_id,qms$unit_id), setequal(mes$unit_id,eq$unit_id))
  d <- merge(mes,qms[c("unit_id","dimension_defect")],by="unit_id",all=TRUE)
  names(d)[names(d)=="dimension_defect"] <- "defect"
  for (stage in 1:5) {
    cols <- c("unit_id","machine",if(stage==3)c("temperature_c","speed_pct"))
    part <- eq[eq$stage==stage,cols,drop=FALSE]
    stopifnot(setequal(part$unit_id,mes$unit_id))
    names(part)[names(part)=="machine"] <- paste0("p",stage)
    d <- merge(d,part,by="unit_id",all=TRUE)
  }
  stopifnot(nrow(d)==nrow(mes),!anyNA(d))
  d$day <- as.integer(substr(d$produced_at,9,10))
  d
}
feature_matrix <- function(d) {
  b <- as.numeric(d$p3=="B"); c <- as.numeric(d$p3=="C")
  m2 <- as.numeric(d$material_batch=="M02"); m3 <- as.numeric(d$material_batch=="M03")
  t <- (d$temperature_c-80)/10
  cbind(intercept=1,machine_B=b,machine_C=c,material_M02=m2,material_M03=m3,
        B_M02=b*m2,B_M03=b*m3,C_M02=c*m2,C_M03=c*m3,
        temperature_z=t,temperature_z2=t*t,speed_z=(d$speed_pct-100)/20,
        product_B=as.numeric(d$product=="Model-B"),night=as.numeric(d$shift=="Night"))
}
fit_quality_model <- function(d) {
  train <- d[d$day<=20,]; test <- d[d$day>20,]
  fit <- glm.fit(feature_matrix(train),train$defect,family=binomial(),
                 control=glm.control(epsilon=1e-10,maxit=100))
  stopifnot(fit$converged,all(is.finite(fit$coefficients)))
  pred <- as.numeric(plogis(feature_matrix(test)%*%fit$coefficients));y <- test$defect
  np <- sum(y);nn <- length(y)-np
  auc <- (sum(rank(pred,ties.method="average")[y==1])-np*(np+1)/2)/(np*nn)
  groups <- split(order(pred),rep(1:5,length.out=length(pred),each=ceiling(length(pred)/5)))
  # Test size is 2,000, giving five equal-sized sorted probability bins.
  calibration <- do.call(rbind,lapply(groups,function(i) data.frame(n=length(i),predicted=mean(pred[i]),observed=mean(y[i]))))
  list(coefficients=fit$coefficients,train_n=nrow(train),test_n=nrow(test),auc=auc,
       brier=mean((pred-y)^2),baseline_brier=mean((mean(train$defect)-y)^2),
       calibration=calibration,train=train)
}
cohort_data <- function(d,scenario,product="all",period="all") {
  x <- d[d$scenario==scenario,]
  if(product!="all") x <- x[x$product==product,]
  if(period=="early") x <- x[x$day<=20,]
  if(period=="late") x <- x[x$day>20,]
  x
}
material_matrix <- function(d,stage=3L) {
  do.call(rbind,lapply(c("M01","M02","M03"),function(batch)
    do.call(rbind,lapply(c("A","B","C"),function(machine) {
      g <- d[d$material_batch==batch & d[[paste0("p",stage)]]==machine,]
      n <- nrow(g);bad <- sum(g$defect)
      data.frame(batch,machine,n,bad,rate=if(n)bad/n else NA_real_,stringsAsFactors=FALSE)
    }))
  ))
}
heatmap_summary <- function(d) {
  total_n <- nrow(d);total_bad <- sum(d$defect)
  out <- do.call(rbind,lapply(1:5,function(stage) {
    mm <- material_matrix(d,stage)
    weights <- prop.table(table(factor(d$material_batch,levels=c("M01","M02","M03"))))
    do.call(rbind,lapply(c("A","B","C"),function(machine) {
      g <- d[d[[paste0("p",stage)]]==machine,];n<-nrow(g);bad<-sum(g$defect)
      other_n<-total_n-n;other_bad<-total_bad-bad
      p<-if(n>0&&other_n>0)fisher.test(matrix(c(bad,n-bad,other_bad,other_n-other_bad),2,byrow=TRUE))$p.value else NA_real_
      std <- 0;std_other<-0
      for(batch in names(weights)) {
        a<-mm[mm$batch==batch & mm$machine==machine,];o<-mm[mm$batch==batch & mm$machine!=machine,]
        std<-std+as.numeric(weights[batch])*a$rate
        std_other<-std_other+as.numeric(weights[batch])*if(sum(o$n)>0)sum(o$bad)/sum(o$n) else NA_real_
      }
      data.frame(stage,machine,n,bad,rate=if(n)bad/n else NA_real_,other_n,other_bad,
        other_rate=if(other_n)other_bad/other_n else NA_real_,p,std,std_other,stringsAsFactors=FALSE)
    }))
  }))
  out$q <- p.adjust(out$p,method="BH")
  out
}
score_plan <- function(model,machine,batch,temp,speed,product="Model-A",shift="Day") {
  row<-data.frame(p3=machine,material_batch=batch,temperature_c=temp,speed_pct=speed,product,shift)
  risk<-as.numeric(plogis(feature_matrix(row)%*%model$coefficients))
  uph<-200*speed/100;good<-uph*(1-risk)
  cost<-(1200+45*uph+12*uph*risk)/good
  data.frame(machine,batch,temp,speed,risk,uph,good,cost,stringsAsFactors=FALSE)
}
compare_plans <- function(model,machine,batch,temp,speed,product="Model-A",shift="Day",limit=.1) {
  score<-function(m=machine,b=batch,t=temp,s=speed) score_plan(model,m,b,t,s,product,shift)
  plans<-rbind(score(),score(t=80),score(s=90),score(b="M01"),score(m="A"))
  plans$plan<-c("目前設定","溫度調到 80°C","速度調到 90%","換成 M01 材料","改用 A 機")
  grid<-expand.grid(temp=seq(60,100,5),speed=seq(80,120,5))
  candidates<-do.call(rbind,lapply(seq_len(nrow(grid)),function(i) score(t=grid$temp[i],s=grid$speed[i])))
  eligible<-candidates[candidates$risk<=limit & candidates$good>=160,]
  best<-NULL
  if(nrow(eligible)>0) {
    best<-eligible[which.min(eligible$cost),,drop=FALSE];best$plan<-"同機台／材料的可行低成本參數"
    plans<-rbind(plans,best)
  }
  plans$feasible<-plans$risk<=limit & plans$good>=160
  list(plans=plans,best=best,eligible=nrow(eligible),searched=nrow(candidates))
}
