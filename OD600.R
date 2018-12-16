#'Bacterial Growth OD600 Predictor
#'
#'This function allows you to predict the OD reading of bacterial culture through the time in optimal environment (such as pH). The mathematic model of bacterial growth curve is based on Gompertz equation. The function has two options: accurate=T or F.If accurate=F, function will use default bacterial growth parameters. If accurate=T, bacterial growth parameters will be calculated based on input data.
#'
#' @usage OD600(ODwan,accurate=F,t0,OD0,t1,OD1) #accurate=F is defalut setting
#'
#' @param ODwan a numeric input of the OD600 reading you want to reach
#' @param accurate a logical indicating if you want to use default bacterial growth parameters or not. If the accurate is not specified, t0 cannot be ZERO and ONE.
#' @param t0 a numeric input of time. If accurate=F, it is the incubated time (in minute) of your culture. If accurate =T, it is incubated time (in minutes) when you take first OD reading.
#' @param OD0 a numeric input of the first OD reading you have taken which can be used only when accurate=T.
#' @param t1 a numeric input of incubated time (in minutes) when you take second OD reading which can be used only when accurate=T.
#' @param OD1 a numeric input of the second OD reading you have taken which can be used only when accurate=T.
#'
#' @details The default bacterial growth parameters are based on the E.coli BL21(DE3) in 37 degree. If your bacteria are not E.coli, the default setting may not match your situation. Please make sure choosing accurate=T. NOTICE: If the accurate is not specified, t0 cannot be ZERO and ONE.
#'
#' @return The return value will be the time you need to reach your wanted OD reading
#' @references Zwietering, M. H., Jongenburger, I., Rombouts, F. M., & Van't Riet, K. (1990). Modeling of the Bacterial Growth Curve. Applied and Environmental Microbiology, 56(6), 1875-1881.
#'
#' @examples
#' OD600(1.0,170)#use default setting which accurate=F
#' #[1] "Need 11 Hour 45 min to reach OD 1"
#'
#' OD600(1.0,accurate=T,170,0.5,400,0.9) #use accurate=T
#' #[1] "Need 6 Hour 45 min to reach OD 1"
#' @export




OD600<-function(ODwan,accurate,t0,OD0,t1,OD1){ #two options, accurate =T or F
  y<-NULL                                        #if accurate=F, only have to input ODwan,t0 are needed
  tw<-NULL                                       #if accurate=T,have to input all arguments
  tx<-NULL
  th<-NULL
  tmin<-NULL
  A<-2.5
  if(accurate==F){    #if accurate==F, the parameters will be set
  maxrate<-0.00144
  l<-180
  tw<-l-(A*log(-log(ODwan/A))-A)/(maxrate*exp(1)) #the equation convert OD to time
  tx<-tw-t0
  if (tx>60){
    th<-trunc(tx/60)
    tmin<-tx-th*60
    print(paste("Need",th,"Hour",round(tmin,0),"min to reach OD",ODwan))
  } else{
    print(paste("Need",round(tx,0),"min to reach OD",ODwan))
  }
  } else if (accurate==T){    #if accurate==T, the parameters will be calculated by inputted t0,OD0,t1,OD1
  maxrate<-(OD1-OD0)/(t1-t0)  #maxrate
  l<-OD0-maxrate*t0
  tw<-l-(A*log(-log(ODwan/A))-A)/(maxrate*exp(1)) #the equation convert OD to time
  tx<-tw-t0
  if (tx>60){
    th<-trunc(tx/60)
    tmin<-tx-th*60
    print(paste("Need",th,"Hour",round(tmin,0),"min to reach OD",ODwan))
  } else{
    print(paste("Need",round(tx,0),"min to reach OD",ODwan))
  }
  } else {   #if accurate is not specified,

     t0 <- accurate
    maxrate<-0.00144
    l<-180
    tw<-l-(A*log(-log(ODwan/A))-A)/(maxrate*exp(1)) #the equation convert OD to time
    tx<-tw-t0
    if (tx>60){
      th<-trunc(tx/60)
      tmin<-tx-th*60
      print(paste("Need",th,"Hour",round(tmin,0),"min to reach OD",ODwan))
    } else{
      print(paste("Need",round(tx,0),"min to reach OD",ODwan))
    }
    }


}
