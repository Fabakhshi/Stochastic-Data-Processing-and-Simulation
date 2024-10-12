function f=negloglik(par,x,delta)

 alpha=par(1);
 beta=par(2);
 sigma=par(3);
 n=length(x);
 
   gamma2=((sigma^2)/(2*beta))*(1-exp(-2*beta*delta));
   s=0;
   m=n-1;
   
  for i=2:n
  mu=alpha+(x(i-1)-alpha)*exp(-beta*delta);
  s=s+(x(i)-mu)^2;
  end
  f=(m/2)*log(2*pi*gamma2)+(1/(2*gamma2))*s;
end

