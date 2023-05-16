Additive_RNG<-function(a,z0,c,m,n) {
  xi<-matrix(NA,n,3)
  colnames(xi)<-c("aZ(i-1)+c","Xi","Ui")
  for (i in 1:n)
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
  }
  hist(xi[,3])
  data<-xi
}

multiplicative_RNG<-function(a,z0,m,n) {
  xj<-matrix(NA,n,3)
  colnames(xj)<-c("aZ","Xj","Uj")
  for (j in 1:n)
  {
    xj[j,1]<-(a*z0)
    xj[j,2]<-xj[j,1]%%m
    xj[j,3]<-xj[j,2]/m
    z0<-xj[j,2]
  }
  hist(xj[,3])
  View(xj)
  X<-xj[,3]
    Binom<-as.numeric(cut(X,breaks=c(0,1/8,4/8,7/8,1),include.lowest = T))-1
  (tabel<-table(Binom)/length(Binom))
  print(Binom)
}

Bernouli_1<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-NULL
  for (z in 1:i) ifelse (X[z]<=p,Y[z]<-1,Y[z]<-0)
  (tabel<-table(Y)/length(Y))
}
#barplot(tabel,main="Bernoulli")


Bernouli_2<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-(X<=p)+0
  (tabel<-table(Y)/length(Y))
}
#barplot(tabel,main="Bernoulli")

Z0= 11123 a=35 m=138 n=100
p=0.65
#tugas_1

Gabungan_RNG_ber<-function(a,z0,c,m,n,p) {
  xi<-matrix(NA,n,3)
  di<-matrix(NA,n,3)
  colnames(xi)<-c("aZ(i-1)+c","Xi","Ui")
  colnames(di)<-c("Xi","0","1")
  for (i in 1:n)
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
    di[i,1]<-xi[i,2]
    di[i,2:3]<-Bernouli_1(xi[i,2],p)
  }
  #hist(xi[,3])
  View(xi)
  View(di)
}







#VR binomial
#binomial dari bernoulli
Binomial_sim<-function(i,n,p) {
    i<-i
    n<-n
    p<-p
    Binom<-NULL
    for (z in 1:i){
      m<-0
      for (k in 1:n){
        y<-(runif(1)<=p)+0
        m<-m+y
      }
      Binom[z]<-m
    }
    (tabel<-table(Binom)/length(Binom))
    print(Binom)
}


#uniform


X<-
Binom<-as.numeric(cut(X,breaks=c(0,1/8,4/8,7/8,1),include.lowest = T))-1
(tabel<-table(Binom)/length(Binom))
View(Binom)


#fungsi R

x <- rbinom(16, 4, 0.5)

#VR geometri
#transformation
i<-1000
p<-0.5
R<-runif(i)
X<-log(1-R)/log(1-p)
hist(X)

#input peluang sukses
i <- 100
sebaran_geom <- function(p){
  R <- runif(i)
  X <- log(1-R)/log(1-p)
  print(X)
}
sebaran_geom(0.5)

#Sebaran bernauli

K <- 1
p <- 0.5
while(runif(1)>p)
  K=K+1;
K


#fungsi di R

# x ~ geometrik(0.4) sebanyak 16 bilangan acak
x <- rgeom(16, 0.4)
x


#VR Binomial Negatif
#sebaran geometri
K <- 1
p <- 0.5
r <- 3
R <-runif(i)
s <-0
while(s < r){
  if (runif(1)>p)
  {K = K+1;
  print=0
  }
  else
  {s = s+1;
  print = 1}
}
K+r-1

#unifrom
n<-1000
U<- runif(n)
m<- 5
p<- 0.5
F<-pnbinom(1:20,size = m,p)
negative.binom <- NULL
for (i in 1:n){
  negative.binom[i]<- min(which(U[i]<F))-1
}
table(negative.binom)


#menggunakan fungsi di R
# x ~ negative binom(4,0.5) sebanyak 16 bil. acak
x <- rnbinom(16,4,0.5)
x


#VR Poisson
#sebaran uniform
i<-100
lambda<-1
K<-NULL
for (z in 1:i){
  k<-0
  sk<-1
  while(sk>=exp(-lambda)){
    u<-runif(1)
    sk<-sk*u
    k<-k+1
  }
  K[z]<-k
}
K
(tabel1<-table(K)/length(K))


#sebaran eksponensial
i<-100
lambda<-1
K<-NULL
for (z in 1:i){
  sk<-0
  k<-0
  while (sk<=1){
    u<-runif(1)
    y<-log(u)/lambda
    sk<-y+sk
    k<-k+1
  }
  K[z]<-k-1
}
K
(tabel2<-table(K)/length(K))


#fungsi di R
# x ~ poisson(4) sebanyak 16 bilangan acak
x<-rpois(16, 4)
x

#Eksponensial(λ=4)
i<-1000
lambda<-4
U<-runif(i)
X<--log(U)/lambda
hist(X)


# x ~ eksponensial (4) sebanyak 16 bilangan acak
x <- rexp(16, 4)
x


#Gamma(α=4,β=3)
i<-1000
lambda<-3
alpha<-4
U<-log(runif(i*alpha))
Um<-matrix(U,i)
Y<-apply(Um,1,sum)
Gamma<--Y/lambda
hist(Gamma)


# x ~ gamma(4,3) sebanyak 16 bilangan acak
x <- rgamma(16, 4, 3)
x


#chi-square(10)
i<-1000
lambda<-2
alpha<-5
U<-log(runif(i*alpha))
Um<-matrix(U,i)
Y<-apply(Um,1,sum)
chi<--Y/lambda
hist(chi)

#chi-square(11)
i<-1000
lambda<-2
alpha<-5
U<-log(runif(i*alpha))
Um<-matrix(U,i)
Y<-apply(Um,1,sum)
chi<--Y/lambda
chi<-chi+(rnorm(i))^2
hist(chi)


# x ~ chisquare(11) sebanyak 16 bilangan acak
x <- rchisq(16,11)
x



