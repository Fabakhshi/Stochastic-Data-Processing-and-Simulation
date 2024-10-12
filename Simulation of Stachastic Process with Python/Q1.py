

import numpy as np
import matplotlib.pyplot as plt

#Function to generate w(t_n)-w(t_{n-1})~N(0,h)
def d_w(T,N,M):
 dt=T/N
 dw=np.sqrt(dt) * np.random.normal(size=N)
 return dw

#Function to generate Brownia nmotion W(T)
def W(T,N,M):
 dw=d_w(T,N,M)
 W=np.cumsum(dw)
 return W

#Function to interpolate at a coarser resolution
def interpolate(W_fine,N_coarse):
 t_coarse=np.linspace(0,T,N_coarse)
 t_fine=np.linspace(0,T,len(W_fine))
 W_coarse=np.interp(t_coarse,t_fine,W_fine)
 return W_coarse

#Parameters
T=1
N=2**10
M=10

#Generate noise at finest resolution
np.random.seed(0) #for reproducibility
W_fine=W(T,N,M)

#Generate Brownian motion at all resolutions using the same noise
plt.figure(figsize=(10,6))
for i in range(M):
 N_coarse=2**(i)
 W_coarse=interpolate(W_fine,N_coarse)
 plt.plot(np.linspace(0,T,N_coarse),W_coarse,label=f'h{i+1}=2ˆ(−{i+1})')
 plt.legend()
plt.title('Sample Paths of Brownian Motionat Different Resolutions')
plt.xlabel('Time')
plt.ylabel('Value')
plt.show() 