
##### Diagnostic function to evaluate assumptions for linear models ####

diagnostic<-function(x){
  plot(x) # shows index and trait
  abline(mean(x),0) 
  hist(x) # shows histogram distribution
  qqnorm(x) # shows QQ plot
  qqline(x,lty=2, col="Red")
skew<-function(x){
    m3<-sum((x-mean(x))^3)/length(x)
    s3<-sqrt(var(x))^3
    m3/s3}
kurtosis<-function(x){
    m4<-sum((x-mean(x))^4)/length(x)
    s4<-var(x)^2
    m4/s4-3}
  print(paste("Kurtosis=", kurtosis(x), sep=""))
  print(paste("Skew=", skew(x), sep=""))
  }

