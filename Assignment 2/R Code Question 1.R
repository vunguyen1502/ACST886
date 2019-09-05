#Question 1
## ------------------------------------------------------------------------
LifeA <- as.Date(c("1964-03-17", "1992-06-20", "1998-06-20"))
LifeB <- as.Date(c("1964-05-06", "1992-08-06", "1993-06-12"))
LifeC <- as.Date(c("1964-08-12", "1992-12-18", "1995-06-18"))
LifeD <- as.Date(c("1964-10-27", "1993-01-04", "1998-06-20"))
LifeE <- as.Date(c("1965-01-04", "1993-04-28", "1996-08-29"))
LifeF <- as.Date(c("1965-04-18", "1993-06-16", "1995-12-12"))
LifeG <- as.Date(c("1965-05-20", "1993-10-29", "1996-04-21"))
LifeH <- as.Date(c("1965-07-04", "1994-02-16", "1998-06-20"))
LifeI <- as.Date(c("1965-09-16", "1994-08-22", "1997-02-22"))
LifeJ <- as.Date(c("1965-12-11", "1995-03-06", "1997-02-17"))


## ------------------------------------------------------------------------
live=function(Life){
  insure=as.Date(paste0(substr(Life[1],1,4),substr(Life[2],5,10)))
  if (abs(as.numeric(Life[1]-insure))<183){
  insbirth=as.numeric(substr(Life[1],1,4))
  } else if (as.numeric(Life[1]-insure)>182){
  insbirth=as.numeric(substr(Life[1],1,4))+1
  } else if (as.numeric(Life[1]-insure)<-182){
  insbirth=as.numeric(substr(Life[1],1,4))-1
  }
  agexentry=as.Date(paste0(insbirth+30,substr(Life[2],5,10)))
  min(as.numeric(Life[3]-agexentry)/365.25,1)
}



## ------------------------------------------------------------------------
AgeLive=(live(LifeA)+live(LifeD)+live(LifeH))
AgeLive


## ------------------------------------------------------------------------
withdrawal=function(Life){
  insure=as.Date(paste0(substr(Life[1],1,4),substr(Life[2],5,10)))
  if (abs(as.numeric(Life[1]-insure))<183){
    insbirth=as.numeric(substr(Life[1],1,4))
  } else if (as.numeric(Life[1]-insure)>182){
    insbirth=as.numeric(substr(Life[1],1,4))+1
  } else if (as.numeric(Life[1]-insure)<-182){
    insbirth=as.numeric(substr(Life[1],1,4))-1
  }
  policyyear=as.numeric(substr(Life[2],1,4))
  issuringage=policyyear-insbirth
  withdrawage=as.numeric(substr(Life[3],1,4))-policyyear+issuringage
  agexentry=as.Date(paste0(insbirth+30,substr(Life[2],5,10)))
  agexexit=as.Date(paste0(insbirth+withdrawage,substr(Life[2],5,10)))
  min(max(as.numeric(agexexit-agexentry)/365.25,0),1)
}


## ------------------------------------------------------------------------
AgeWithdraw=(withdrawal(LifeC)+withdrawal(LifeF)+withdrawal(LifeI))
AgeWithdraw


## ------------------------------------------------------------------------
death=function(Life){
  insure=as.Date(paste0(substr(Life[1],1,4),substr(Life[2],5,10)))
  if (abs(as.numeric(Life[1]-insure))<183){
    insbirth=as.numeric(substr(Life[1],1,4))
  } else if (as.numeric(Life[1]-insure)>182){
    insbirth=as.numeric(substr(Life[1],1,4))+1
  } else if (as.numeric(Life[1]-insure)<(-182)){
    insbirth=as.numeric(substr(Life[1],1,4))-1
  }
  agexentry=as.Date(paste0(insbirth+30,substr(Life[2],5,10)))
  insuredeath=as.Date(paste0(substr(Life[3],1,4),substr(Life[2],5,10)))
  if (abs(as.numeric(Life[3]-insuredeath))>0){
    insdeath=as.numeric(substr(Life[3],1,4))+1
  } else {
    insdeath=as.numeric(substr(Life[3],1,4))
  }
  agexentry=as.Date(paste0(insbirth+30,substr(Life[2],5,10)))
  agexexit=as.Date(paste0(insuredeath,substr(Life[2],5,10)))
  min(max(as.numeric(agexexit-agexentry)/365.25,0),1)
}


## ------------------------------------------------------------------------
AgeDeath=death(LifeB)+death(LifeE)+death(LifeG)+death(LifeJ)
AgeDeath


## ------------------------------------------------------------------------
qx=2/(AgeDeath+AgeLive+AgeWithdraw)
qx


## ------------------------------------------------------------------------
maxlh=function(q){
  2*q*((1-q)^6)+(q^2)*6*((q-1)^5)}
uniroot(maxlh,c(0.01,0.99))$root


