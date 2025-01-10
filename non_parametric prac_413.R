#=================================Binomial Distribution=========================================
set.seed(1234)
par(mfrow= c(2,2))
Xn_bar= array(dim=1)
func_binom= function(n){
  for(i in 1:1000)
  {
    b= rbinom(n,5,0.5)
    Xn_bar[i]=mean(b)
  }
  Xn_bar
  t = (Xn_bar-(n*0.5))/sqrt((n*(0.5^2)))
  hist(t,main=paste("n=",n),freq = F)
  lines(density(t))
}
func_binom(20)
func_binom(50)
func_binom(70)
func_binom(100)
#==============================Poisson Distribution=============================================
set.seed(1234)
par(mfrow= c(2,2))
Xn_bar= array(dim=1)
func_pois= function(n){
  for(i in 1:1000)
  {
    lamda=1
    b= rpois(n,lamda)
    Xn_bar[i]=mean(b)
  }
  Xn_bar
  t = (Xn_bar-lamda)/sqrt(lamda/n)
  hist(t,main=paste("n=",n),freq = F)
  lines(density(t))
}
func_pois(20)
func_pois(50)
func_pois(70)
func_pois(100)
