import numpy as np
import matplotlib.pyplot as plt
from copy import deepcopy
from matplotlib.animation import FuncAnimation
import time
import sys

# Assigning variables
N = 20                    # Cell number
L = 20                  # length of the domain
ds = L/float(N - 1)       # space cell`s length 
K = 97                    # alpha
dt = 1/500                # length of time cell
tempo = 100000               # Time steps


# Creating a temperature matrix
T = np.zeros(shape=(N, N))

# INITAL CONDITION
T[:,:] = 0
T[int(N/2),int(N/2)] = 200

# Creating simulation buffer
Tb = deepcopy(T)

# Initianting window with nitial condition
fig = plt.figure()
pcm = plt.pcolormesh(Tb)
pcm = plt.pcolormesh(Tb)
pcm.set_array(Tb.ravel())
plt.colorbar()
plt.title("Time: {0} s\n".format(0*dt))
plt.ion()
plt.show()


# Initiate simulation
for n in range(tempo):         #(from 0 to tempo - 1)
    
    # Thermal equation aplied to the domain
    for i in range(1 , N-1):
        for j in range(1 , N-1):
            T[i,j] = Tb[i,j] + K*(dt/ds**2)*(Tb[i+1,j]+Tb[i-1,j]+Tb[i,j+1]+Tb[i,j-1]-4*Tb[i,j])

    # Neumann Boundary Conditions
    for i in range(0, N):
        T[0,i]  = Tb[1,i]         # left   (x = 0)
        T[N-1, i] = Tb[N-2, i]    # right  (x = N)
        T[i,0]  = Tb[i,1]         # botton (y = 0)
        T[i,N - 1]  = Tb[i,N-2]   # UP (y = N)

    # Dirichlet Condition
    T[int(N/2),int(N/2)] = 200

    # Update simulation buffer
    Tb = deepcopy(T)
    print(Tb[N-1,int(N/2)] , Tb[3,3])
    if (n % 1000 == 0):
        # Print simulation buffer to HeatMap
        if plt.fignum_exists(fig.number):
            fig.clear()
            pcm.set_array(Tb.ravel())
            pcm = plt.pcolormesh(Tb)
            plt.title("Time: {0} s\n".format(n*dt))
            fig.canvas.draw()
            fig.canvas.flush_events()
        else:
            sys.exit()


