import numpy as np
import matplotlib.pyplot as plt


#Function to generate w(t_n)-w(t_{n-1})~N(0,h)
def d_w(T,N):
 dt=T/N
 dw=np.sqrt(dt) * np.random.normal(size=N)
 return dw


#Function to Compute X with recursion scheme X_h(t_n)
def X_h(T,N,dW,mu=2,sigma=1):
 dt=T/N
 Xhsteps=  (1+mu*dt) + sigma*dW
 Xhsteps[0]=1
 Xh=np.cumproduct(Xhsteps)
 return Xh

#Function to compute X(T)
def X(T,N,dW,mu=2,sigma=1):
 t=np.linspace(0,T,N)
 X=np.exp((mu-(sigma**2)/2)*t + sigma*np.cumsum(dw))
 return X

#Function to interpolate at a coarser resolution
def interpolate(X_fine,N_coarse):
 t_coarse=np.linspace(0,T,N_coarse)
 t_fine=np.linspace(0,T,len(X_fine))
 X_coarse=np.interp(t_coarse,t_fine,X_fine)
 return X_coarse


#Parameters
T=1
N=2**10
mu=2
sigma=1

#Generate noise
np.random.seed(0) #for reproducibility
dw=d_w(T,N)

#compute X(T)
x=X(T,N,dw)
#compute Xh(T)
Xht= X_h(T,N,dw)
print(x)
#Generate Brownian motion at all resolutions using the same noise
plt.figure(figsize=(10,6))
for i in range(10):
 N_coarse=2**(i)
 Xh_inter=interpolate(Xht,N_coarse)
 plt.plot(np.linspace(0,T,N_coarse),Xh_inter,label=f'h{i+1}=2ˆ(−{i+1})')
 plt.legend()
plt.plot(np.linspace(0,T,N),x,label='X(T)') 
plt.legend()
plt.title('Sample Paths of X, Xh')
plt.xlabel('Time')
plt.ylabel('Value')
plt.show() 