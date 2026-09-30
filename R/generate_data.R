# Recreates the supplied CSVs with the same cross-language Park-Miller sequence.
# Explicit synthetic mechanism. No real machine recipe or HP data is used.
generate_quality_data <- function(path="data",seed=20260929) {
  dir.create(path,showWarnings=FALSE,recursive=TRUE)
  state<-as.double(seed)
  u<-function(){state<<-(state*16807)%%2147483647;state/2147483647}
  mes<-vector("list",18000);eq<-vector("list",90000);qms<-vector("list",18000)
  row_index<-0L;eq_index<-0L
  for(scenario in c("machine","material","interaction"))for(i in 1:6000) {
    row_index<-row_index+1L;day<-(i-1L)%/%200L+1L
    uid<-sprintf("%s-%05d",toupper(substr(scenario,1,3)),i)
    product<-if(u()<.5)"Model-A" else "Model-B";shift<-if(u()<.5)"Day" else "Night"
    machines<-vapply(1:5,function(j)c("A","B","C")[floor(u()*3)+1L],"")
    r<-u()
    if(scenario=="material") {
      pm3<-if(machines[3]=="B").75 else .12
      batch<-if(r<pm3)"M03" else if(r<pm3+(1-pm3)/2)"M01" else "M02"
    } else batch<-c("M01","M02","M03")[floor(r*3)+1L]
    temps<-vapply(1:5,function(j)round(60+40*u(),2),0)
    speeds<-vapply(1:5,function(j)round(80+40*u(),2),0)
    z<--3.6+.3*((temps[3]-80)/10)^2+.65*(speeds[3]-100)/20+.2*(product=="Model-B")+.15*(shift=="Night")
    if(scenario=="machine")z<-z+2.4*(machines[3]=="B")
    if(scenario=="material")z<-z+2.4*(batch=="M03")
    if(scenario=="interaction")z<-z+2.9*(machines[3]=="B"&&batch=="M03")
    defect<-as.integer(u()<plogis(z))
    mes[[row_index]]<-data.frame(scenario,unit_id=uid,
      produced_at=sprintf("2026-09-%02d %s:00:00",day,if(shift=="Day")"08" else "20"),product,shift,material_batch=batch)
    qms[[row_index]]<-data.frame(scenario,unit_id=uid,inspection_day=day,dimension_defect=defect)
    for(j in 1:5) {
      eq_index<-eq_index+1L
      eq[[eq_index]]<-data.frame(scenario,unit_id=uid,stage=j,machine=machines[j],temperature_c=temps[j],speed_pct=speeds[j])
    }
  }
  write.csv(do.call(rbind,mes),file.path(path,"mes.csv"),row.names=FALSE)
  write.csv(do.call(rbind,eq),file.path(path,"equipment.csv"),row.names=FALSE)
  write.csv(do.call(rbind,qms),file.path(path,"qms.csv"),row.names=FALSE)
  invisible(path)
}
