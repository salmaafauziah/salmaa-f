#Salmaa F_028
#Tugas_1

##Multiplicative##
Multiplicative_RNG<-function(a,z0,m,n){
  xj<-matrix(NA,n,3)
  colnames(xj)<-c("aZ(i-1)","Xj","Uj")
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
Multiplicative_RNG(35,11123,138,100)


##menampilkan z1,z2,...,zn##
Multiplicative_RNG<-function(a,z0,m,n)
{
  z<-rep(0,100)
  for (i in 1:100)
  {
    z[i]<-(a*z0)%%m
    z0<-z[i]
  }
  print(z) 
}
Multiplicative_RNG(35,11123,138,100)

##Bernouli_1##
Bernouli_1<-function(n,p) {
  i<-100
  p<-.65
  X<-runif(i)
  Y<-NULL
  for (z in 1:i) ifelse (X[z]<=p,Y[z]<-1,Y[z]<-0)
  (tabel<-table(Y)/length(Y))
}
Bernouli_1(100, 0.65)
barplot(tabel,main="Bernouli")

