import math
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import axes3d
from matplotlib.pyplot import rc
from matplotlib import cm
import os
from os import path
import math


outpath = os.getcwd()    # get current directory



# explicito
N = np.array([ (25) ,            (50) ,         (100) ,     (200)  ,  (400) ])
Li = np.array([(5.13413e-7) , (1.24027e-7) , (3.0455e-8), (7.54172e-9) , (1.87627e-9)])
L2 = np.array([(1.23219e-06/math.sqrt(25)), (4.30173e-07/math.sqrt(50)), ( 1.5079e-07/math.sqrt(100)),(5.30646e-08/math.sqrt(200)), (1.87161e-08/math.sqrt(400))])


plt.figure(3)
plt.plot( N , Li ,  'k', linewidth = '1', color = 'black' , label = 'L infinito')
plt.plot( N , L2 ,  'k', linewidth = '1', color = 'blue' , label = 'L2')
plt.legend()
plt.title('Análise de erros do métodos explícito')
plt.xlabel('N')
plt.ylabel('L inf')
plt.show()

#implicito
N = np.array([ (25) ,            (50) ,         (100) ,     (200)  ])
Li = np.array([(5.99932e-06) , (1.38382e-06) , (3.36175e-07), (8.30313e-08) ])
L2 = np.array([( 2.87968e-06), ( 6.78769e-07), ( 1.66449e-07),(4.13106e-08)])


plt.figure(3)
plt.plot( N , Li ,  'k', linewidth = '1', color = 'black' , label = 'L infinito')
plt.plot( N , L2 ,  'k', linewidth = '1', color = 'blue' , label = 'L2')
plt.legend()
plt.title('Análise de erros do métodos implicito')
plt.xlabel('N')
plt.ylabel('L inf')
plt.show()
