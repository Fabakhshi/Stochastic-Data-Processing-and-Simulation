rm(list=ls())



#===================Question 3
fx <- function(x){
  1/6
}
fy <- function(y){
  1/6
}
s=0;
for (x in 1:6){
  for (y in 1:6){
    s=s + x*y*fx(x)*fy(y);
  }
}
print(s)


#===================Question q4
  integrate(dnorm, -1, 1)
  
  
 n=10000
 a=-1;
 b=1;
 
dx=(b-a)/n;
s=0;
  
  for(i in 1:n){
    s=s+ (1/sqrt(2*pi)) * exp((-.5*(-1+(i-1)*dx)^2))*dx;
  }
 print(s)
 
 
 #===================Question q5 - a

 n=30;
 r=20;
 t=seq(0,1,length.out=n)
 for(j in 1:r){
   x=rnorm(1,5,1.5)
   y=rnorm(1,0.5,0.3)
   u=c();
   for(i in 1:n){
     u[i]=exp(-t[i]*y)*x;
   }
   
   if(j==r) plot(t,u,"l",col="blue") else{
     
    plot(t,u,"l",yaxt="n",col="blue")}
    par(new=TRUE)
 }
 
 
 
 
 
 
 
 
 
 n=30;
 r=20;
 t=seq(0,1,length.out=n)
 for(j in 1:r){
   x=rnorm(1,5,1.5)
   
   y=rnorm(1,0.5,0.3)
   u=c();
   for(i in 1:n){
     u[i]=exp(-t[i]*y)*x;
     
   }
   cat(x, u[1],"\n \n")
   if(j==r) plot(t,u,type="l",col="blue",xlim =c(0 , 1),ylim=c(0 , 10)) else{
     
     plot(t,u,"l",yaxt="n",xaxt="n",col="blue",xlim =c(0 , 1),ylim=c(0 , 10))}
   par(new=TRUE)
 }
 
 

 #===================Question q5 - b
 
 
 
 n=500;
 a=0; b=10; c=-1; d=2;
 dx=(b-a)/n;
 dy=(d-c)/n;
 s=0;
 
 for(i in 1:n){
   for(j in 1:n){
     xs=a+(i-1)*dx;
     ys=c+(j-1)*dy;
     s=s+ exp(-ys)*xs*dnorm(xs,5,1.5)*dnorm(ys,0.5,0.3)*dx*dy;
   }
 }
 print(s)