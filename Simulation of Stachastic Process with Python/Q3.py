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

#Function to compute strong error
def strong_error(T,N,M):
 S=0
 for m in range(1,M):
   dw=d_w(T,N)
   X1=X(T,N,dw)[N-1]  #X_t(T)
   Xh1=Xh(T,N,dw)[N-1] #X_h(T)
   S+=np.power(X1-Xh1,2)
 return np.sqrt(S/M)

#Parameters
T=1
M=1000

#Compute and plot strong error for different resolutions
np.random.seed(0) #for reproducibility
errors=[]
for i in range(1,11):
 N=2**(i)
 errors.append(strong_error(T,N,M))


resolutions=[(2**-i) for i in range(1,11)]
plt.figure(figsize=(10,6))
plt.loglog(resolutions,errors,'o',label='Strong error')
plt.loglog(resolutions,[h**0.5 for h in resolutions],'k-',label='Reference slope hˆ(1/2)')
plt.legend()
plt.title('StrongErrorvsResolution')
plt.xlabel('Resolution(h)')
plt.ylabel('StrongError')
plt.show()