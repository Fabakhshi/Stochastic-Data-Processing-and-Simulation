function X=generateOU(param,n,x0,delta)
 alpha=param(1);
 beta=param(2);
 sigma=param(3);
N=randn(n-1,1);
X=x0;

gamma=sqrt(((sigma^2)/(2*beta))*(1-exp(-2*beta*delta)));
for i=2:n 
    mu=alpha + (X(i-1)-alpha)*exp(-beta*delta);
    
    X(i) = mu + gamma * N(i-1);  
end   
end