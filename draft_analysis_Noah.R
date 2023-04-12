## Load necessary packages
library(lubridate)
library(marked)
## Read in the detection data
detects=read.csv("C:/Users/george.maynard/Documents/GitHubRepos/RFID_GIGO/FVN_RFID_DETECTS.csv")
## Read in the haul data
hauls=read.csv("C:/Users/george.maynard/Documents/GitHubRepos/RFID_GIGO/FVN_HAULS.csv")
## Format columns 
detects$TIMESTAMP=mdy_hm(as.character(detects$Event.DateTime))
detects$TAG=as.factor(detects$Tag..)
hauls$START_TIME=mdy(hauls$Start.Haul.Date)+hms(hauls$Start.Haul.Time)
hauls$END_TIME=mdy(hauls$End.Haul.Date)+hms(hauls$End.Haul.Time)
## Remove data before install date
detects=subset(detects,floor(yday(detects$TIMESTAMP))>=280)
hauls=subset(hauls,floor(yday(hauls$START_TIME))>=280)
## Exploratory plot of detections and hauls
plot(detects$TIMESTAMP,type='n')
for(i in 1:max(as.numeric(detects$TAG))){
  points(
    subset(
      detects,
      as.numeric(detects$TAG)==i
    )$TIMESTAMP,
    pch=i,
    col=i
  )
}
abline(h=hauls$START_TIME,lty=2)
abline(h=hauls$END_TIME,lty=3)

## Create capture histories for each tag using the buffer
buffer=hours(12)
history=data.frame(
  TAG=as.character(),
  CH=as.character()
)
for(i in 1:max(as.numeric(detects$TAG))){
  tag=levels(detects$TAG)[i]
  ch="1"
  ## The tags should be detected at least once per haul either at the beginning 
  ## or the end (within buffer distance around the reported haul time)
  for(h in 1:nrow(hauls)){
    buffer_start=c(hauls$START_TIME[h]-buffer,hauls$START_TIME[h]+buffer)
    buffer_end=c(hauls$END_TIME[h]-buffer,hauls$END_TIME[h]+buffer)
    ## Check to see if there is at least one detection for the tag
    start_count=nrow(
      subset(
        detects,
        as.numeric(detects$TAG)==i & 
          detects$TIMESTAMP%within%interval(buffer_start[1],buffer_start[2])
      )
    )
    end_count=nrow(
      subset(
        detects,
        as.numeric(detects$TAG)==i & 
          detects$TIMESTAMP%within%interval(buffer_end)
      )
    )
    ch=ifelse(
      sum(start_count,end_count)>0,
      paste0(ch,1),
      paste0(ch,0)
    )
  }
  history=rbind(history,data.frame(TAG=tag,CH=ch))
}
history$ch=history$CH
detection_history=data.frame(
  ch=paste0(1,history$ch),
  tag=history$TAG
)
dh.proc=process.data(detection_history)
ddl=make.design.data(dh.proc)
cjs.hessian(ddl)

x=crm(
  detection_history,
  model.parameters=list(
    Phi=list(
      formula=~1,
      fixed=cbind(
        1:nrow(detection_history),
        rep(1,nrow(detection_history)),
        rep(1,nrow(detection_history)),
        rep(1,nrow(detection_history))
      )
    )
  )
)
cjs.hessian(x)

dp=process.data(detection_history)
ddl=make.design.data(dp)
ddl$Phi$fix=ifelse(ddl$Phi$time%in%seq(0,1000,1),1,NA)
cjs.hessian(ddl)
