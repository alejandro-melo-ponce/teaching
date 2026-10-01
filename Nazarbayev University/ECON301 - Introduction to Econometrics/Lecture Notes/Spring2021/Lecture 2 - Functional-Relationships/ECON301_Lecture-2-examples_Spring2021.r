#Example 2.1 Vector (X,Y) with pdf f(x,y)=x+y
#plot of: E(Y|X)
x<-seq(from=0, to=1,by=0.05)
EYDX<-(3*x+2)/(6*x+3)
plot(x,EYDX,type="l",col="red")

#Plot of E*(Y|X)
EsYDX<-(7/11)-(x/11)
points(x,EsYDX,type="b",col="black")


#Computation of BLP of Y as a function of X and X^2
Momen<-function(m,n){Num<-(m+1)*(n+2)+(m+2)*(n+1);
Denom<-(m+1)*(m+2)*(n+1)*(n+2);
Momen<-Num/Denom}
# End of function Momen

EX<-Momen(1,0)
EX
EX2<-Momen(2,0)
VarX<-EX2-(EX)^2
VarX
CovXyX2<-Momen(3,0)-EX*EX2
CovXyX2
VarX2<-Momen(4,0)-(EX2)^2
VdeX<-matrix(c(VarX,CovXyX2,CovXyX2,VarX2),2,2)
VdeX
EY<-EX
CovXyY<-Momen(1,1)-EX*EY
CovX2yY<-Momen(2,1)-EX2*EY
CovY<-c(CovXyY,CovX2yY)
CovY
VdeXinv<-solve(VdeX)
VdeXinv
Betap<-VdeXinv%*%CovY
Betap
Beta0<-EY-Betap[1]*EX-Betap[2]*EX2
Beta0
EsYDXyX2<-Beta0+Betap[1]*x+Betap[2]*x*x
points(x,EsYDXyX2,type="b",col="blue")
