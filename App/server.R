Test1 <- read.delim("E:/hobby/Test1.txt")
CurrentSeason = 55.1
is.between = function(x, a, b) {
  x>=a & x<b
}
SR=subset(Test1, 등급=="SR")
SRP=subset(Test1, 등급=="SR+")
SSR=subset(Test1, 등급=="SSR")
PremiumSSR=subset(Test1, 등급=="SSR" & 시즌!=CurrentSeason)
PremiumSRP=subset(Test1, 등급=="SR+" & 마일리지==FALSE & 시즌!=CurrentSeason)
PremiumSR=subset(Test1, 등급=="SR" & 시즌!=CurrentSeason)
SeasonCard=subset(Test1, 시즌==CurrentSeason)
SeasonSSR=subset(SeasonCard, 등급=="SSR")
SeasonSRP=subset(SeasonCard, 등급=="SR+")
SeasonSR=subset(SeasonCard, 등급=="SR")
MileagePool=rbind(SSR,SRP)
ATKSSR=subset(Test1, 등급=="SSR" & 속성=="공격")
ATKSRP=subset(Test1, 등급=="SR+" & 속성=="공격" & 마일리지==FALSE)
ATKSR=subset(Test1, 등급=="SR" & 속성=="공격")
DEFSSR=subset(Test1, 등급=="SSR" & 속성=="방어")
DEFSRP=subset(Test1, 등급=="SR+" & 속성=="방어" & 마일리지==FALSE)
DEFSR=subset(Test1, 등급=="SR" & 속성=="방어")
SPRSSR=subset(Test1, 등급=="SSR" & 속성=="회복")
SPRSRP=subset(Test1, 등급=="SR+" & 속성=="회복" & 마일리지==FALSE)
SPRSR=subset(Test1, 등급=="SR" & 속성=="회복")

Test2 <- read.delim("E:/hobby/Test2.txt")
Box7=subset(Test2, 단계==7)
Box6=subset(Test2, 단계==6)
Box5=subset(Test2, 단계==5)
Box4=subset(Test2, 단계==4)
Box3=subset(Test2, 단계==3)
Box2=subset(Test2, 단계==2)
Box1=subset(Test2, 단계==1)


draw = function(a) {
  f = vector('numeric')
  for(i in 1:a) {
    c = vector('numeric')
    for(j in 1:10){
      x = runif(1)
      b = c(x)
      b = replace(b, is.between(b, 0, 0.405), "R" )
      b = replace(b, is.between(b, 0.405, 0.805), "R+" )
      b = replace(b, is.between(b, 0.805, 0.905), toString(PremiumSR[sample(nrow(PremiumSR), 1, replace=TRUE), 1]))
      b = replace(b, is.between(b, 0.905, 0.955), toString(SeasonSR[sample(nrow(SeasonSR), 1, replace=TRUE), 1]))
      b = replace(b, is.between(b, 0.955, 0.97), toString(PremiumSRP[sample(nrow(PremiumSRP), 1, replace=TRUE), 1]))
      b = replace(b, is.between(b, 0.97, 0.98), toString(SeasonSRP[sample(nrow(SeasonSRP), 1, replace=TRUE), 1]))
      b = replace(b, is.between(b, 0.98, 0.992), toString(PremiumSSR[sample(nrow(PremiumSSR), 1, replace=TRUE), 1]))
      b = replace(b, is.between(b, 0.992, 1), toString(SeasonSSR[sample(nrow(SeasonSSR), 1, replace=TRUE), 1]))
      c = append(c,b)
    }
    y = runif(1)
    e = c(y)
    e = replace(e, is.between(e, 0, 0.55), toString(PremiumSRP[sample(nrow(PremiumSRP), 1, replace=TRUE), 1]))
    e = replace(e, is.between(e, 0.55, 0.95), toString(SeasonSRP[sample(nrow(SeasonSRP), 1, replace=TRUE), 1]))
    e = replace(e, is.between(e, 0.95, 0.966), toString(PremiumSSR[sample(nrow(PremiumSSR), 1, replace=TRUE), 1]))
    e = replace(e, is.between(e, 0.966, 1), toString(SeasonSSR[sample(nrow(SeasonSSR), 1, replace=TRUE), 1]))
    c = append(c,e)
    f = append(f,c)
  }
  f=noquote(f)
  f=table(f)
  f=as.data.frame(f)
  names(f)=c("카드", "장수")
  return(f)
}
drawbox = function(b) {
  c = vector('numeric')
  for(j in 1:b) {
    x = runif(1)
    b = c(x)
    if(is.between(b, 0, 0.0003)==TRUE)
    { d=Box7[sample(nrow(Box7), 1, replace=TRUE),]
    d=subset(d,select = -c(단계))
    c=rbind(c,d)}
    if(is.between(b, 0.0003, 0.001)==TRUE)
    { d=Box6[sample(nrow(Box6), 1, replace=TRUE),]
    d=subset(d,select = -c(단계))
    c=rbind(c,d)}
    if(is.between(b, 0.001, 0.01)==TRUE)
    { d=Box5[sample(nrow(Box5), 1, replace=TRUE),]
    d=subset(d,select = -c(단계))
    c=rbind(c,d)}
    if(is.between(b, 0.01, 0.08)==TRUE)
    { d=Box4[sample(nrow(Box4), 1, replace=TRUE),]
    d=subset(d,select = -c(단계))
    c=rbind(c,d)}
    if(is.between(b, 0.08, 0.2)==TRUE)
    { d=Box3[sample(nrow(Box3), 1, replace=TRUE),]
    d=subset(d,select = -c(단계))
    c=rbind(c,d)}
    if(is.between(b, 0.2, 0.6)==TRUE)
    { d=Box2[sample(nrow(Box2), 1, replace=TRUE),]
    d=subset(d,select = -c(단계))
    c=rbind(c,d)}
    if(is.between(b, 0.6, 1)==TRUE)
    { d=Box1[sample(nrow(Box1), 1, replace=TRUE),]
    d=subset(d,select = -c(단계))
    c=rbind(c,d)}
  }
  c=aggregate(개수~아이템, data=c, FUN=sum)
  return(c)
}
drawbox2 = function(a) {
  c = vector('numeric')
  for(i in 1:a) {
    x = runif(1)
    b = c(x)
    if(is.between(b, 0, 0.0015)==TRUE)
    { d=Box7[sample(nrow(Box7), 1, replace=TRUE),]
    d=subset(d,select = -c(단계))
    c=rbind(c,d)}
    if(is.between(b, 0.0015, 0.005)==TRUE)
    { d=Box6[sample(nrow(Box6), 1, replace=TRUE),]
    d=subset(d,select = -c(단계))
    c=rbind(c,d)}
    if(is.between(b, 0.005, 0.05)==TRUE)
    { d=Box5[sample(nrow(Box5), 1, replace=TRUE),]
    d=subset(d,select = -c(단계))
    c=rbind(c,d)}
    if(is.between(b, 0.05, 0.4)==TRUE)
    { d=Box4[sample(nrow(Box4), 1, replace=TRUE),]
    d=subset(d,select = -c(단계))
    c=rbind(c,d)}
    if(is.between(b, 0.4, 1)==TRUE)
    { d=Box3[sample(nrow(Box3), 1, replace=TRUE),]
    d=subset(d,select = -c(단계))
    c=rbind(c,d)}
  }
  c=aggregate(개수~아이템, data=c, FUN=sum)
  return(c)
}


shinyServer(
  function(input, output, session){
    
    my_draw = reactive({
      if(input$button == 0)
      {return()}
      isolate({
        if(input$type == "행운상자")
        {drawbox(input$trial2)}
        else if(input$type == "프리미엄 인쇄")
        {draw(input$trial)}
        else{
          if(input$type == "고급 행운상자")
          {drawbox2(input$trial3)}        
        }
      })
      
    })
    output$draw=renderTable({my_draw()})
    
  }
)