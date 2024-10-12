import numpy as np
import matplotlib.pyplot as plt




#Function to generate w(t_n)-w(t_{n-1})~N(0,h)
def d_w(T,N):
 dt=T/N
 dw=np.sqrt(dt) * np.random.normal(size=N)
 return dw

#Function to Compute X with recursion scheme X_h(t_n)
def Xh(T,N,dW,mu=2,sigma=1):
 h=T/N
 Xhsteps=  (1+mu*h) + sigma*dW
 Xhsteps[0]=1
 Xh=np.cumproduct(Xhsteps)

 return Xh


#Function to compute X(T)
def X(T,N,dw,mu=2,sigma=1):
 t=np.linspace(0,T,N)
 X=np.exp((mu-(sigma**2)/2)*t + sigma*np.cumsum(dw))
 return X

#Function tocompute weak error
def weak_error(mu,M,N):
 S=0
 for m in range(1,M): 
  dw=d_w(T,N)
  S+=Xh(T,N,dw)[N-1]
 EXh=S/M
 we=np.abs(np.exp(mu)-EXh)
 return we


#Parameters
T=1
#N=2**10
M=1000
mu=2
sigma=1

np.random.seed(0) #for reproducibility

#Compute and plot 
we=[]
for i in range(1,11):
 N=2**i
 we.append( weak_error(mu,M,N))
  

resolutions=[(2**-i) for i in range(1,11)]
plt.figure(figsize=(10,6))
plt.loglog(resolutions,we,'o',label='Weak error')
plt.loglog(resolutions,[h**0.5 for h in resolutions],'k-',label='Reference slope hˆ(1/2)')
plt.legend()
plt.title('StrongErrorvsResolution')
plt.xlabel('Resolution(h)')
plt.ylabel('StrongError')
plt.show()