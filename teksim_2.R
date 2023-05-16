Additive_RNG<-function(a,z0,c,m,n){
  xi<-matrix(NA,n,3)
  colnames(xi)<-c("aZ(i-1)+c", "Xi","Ui")
  for (i in 1:n)
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
  }
  hist(xi[,3])
  View(xi)
}
Additive_RNG(35,11123,437,138,100)

Additive_RNG<-function(a,z0,c,m,n)
{
  z<-rep(0,100)
  for (i in 1:100)
  {
  z[i]<-((a*z0)+c)%%m
  z0<-z[i]
  }
print(z)  
}
Additive_RNG(35,11123,437,138,100)

Multiplicative_RNG<-function(a,z0,m,n){
  xj<-matrix(NA,n,3)
  colnames(xj)<-c("aZ","Xj","Uj")
  for (j in 1:n)
  {
    xj[j,1]<-(a*z0)
    xj[j,2]<-xj[j,1]%%m.
    xj[j,3]<-xj[j,2]/m
    z0<-xj[j,2]
  }
  hist(xj[,3])
  View(xj)
}

Bernouli_1<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-NULL
  for (z in 1:i) ifelse (X[z]<=p,Y[z]<-1,Y[z]<-0)
  (tabel<-table(Y)/length(Y))
}
barplot(tabel,main="Bernouli")

Bernouli_2<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-(x<=p)+0
  for (z in 1:i) ifelse (X[z]<=p,Y[z]<-1,Y[z]<-0)
  (tabel<-table(Y)/length(Y))
}
barplot(tabel,main="Bernouli")

Bernouli_2<-function(n,p) {
  i<-100
  p<- .65
  X<-runif(i)
  Y<-(x<=p)+0
  for (z in 1:i) ifelse (X[z]<=p,Y[z]<-1,Y[z]<-0)
  (tabel<-table(Y)/length(Y))
}
Bernouli_2(100,0.65)
barplot(tabel,main="Bernouli")