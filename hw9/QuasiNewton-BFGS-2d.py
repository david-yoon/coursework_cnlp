
# coding: utf-8

# In[1]:

'''
Created on 2016. 5. 13

@author: dato
@desc: cnlp hw9 (quasi-newton method) - BFGS equation 2
'''
#-*- coding: utf-8 -*-

import io
import numpy as np
from scipy.misc import derivative
from numpy.linalg import inv
from numpy.linalg import pinv


# In[2]:

def f(x):
    return np.log(2*np.exp(-x[0][0]) + 2 * np.exp(-x[0][0] - x[0][1]) + 2) + x[0][0] * 32/36 + x[0][1] * 24/36


# In[3]:

# def g(x):
#     x1=x[0][0]
#     x2=x[0][1]
    
#     x_dx1= -( -8*(np.exp(x1+x2)) + 2*np.exp(x2) +1 ) / ( 9*(np.exp(x1+x2) + np.exp(x2) +1) )
#     x_dx2= ( 2*(np.exp(x1+x2)) + 2*np.exp(x2) -1 ) / ( 3*(np.exp(x1+x2) + np.exp(x2) +1) )
    
#     return np.asarray([[x_dx1, x_dx2]])


# In[4]:

def g(x):
    x1=x[0][0]
    x2=x[0][1]
    
    x_dx1= -( 0.111111*( -8*2.71828**(x1+x2) + 2.71828**(x2) +1 ) ) / ( 2.71828**(x1+x2) + 2.71828**(x2) +1 ) 
    x_dx2= ( 0.333333*( 2*2.71828**(x1+x2) + 2*2.71828**(x2) -1 ) ) / ( 2.71828**(x1+x2) + 2.71828**(x2) +1 ) 
    
    return np.asarray([[x_dx1, x_dx2]])


# In[5]:

def linesearch(x0, d, g):
    a = 100
    r = 0.5
    b = 0.5
    
    c_tmp = -b*d.dot(g.transpose())
    c = c_tmp[0][0]
    
    while True:

        if f(x0) - f(x0 + a*d) >= a*c:
            break
        else:
            a = r*a
    
    x1 = x0 + a*d
    
    return x1


# In[6]:

def quasi_update_BFGS(C0, s1, y1):

    I = np.identity(2)
    p1 = pinv(np.transpose(y1).dot(s1))
    c1 = (I - p1*s1*np.transpose(y1)) * C0 * (I - p1*y1*np.transpose(s1)) + p1*s1*np.transpose(s1)
    
    return c1   


# In[7]:

def quasi_newton_bfgs(x0, f, g, C0, tol, maxiter):
    
    cnt = 0
    
    for i in range(maxiter):
        
        cnt = cnt +1
    
        g0 = g(x0)
        d = -C0.dot(g0.transpose()).transpose()
        
        x1 = linesearch(x0, d, g0)
        
        if np.linalg.norm(x1-x0) < tol:
            break
        else:
            g1 = g(x1)
            s1 = x1 - x0
            y1 = g1 - g0
            C1 = quasi_update_BFGS(C0, s1, y1)

            x0 = x1
            C0 = C1
            
    return x0, cnt


# In[8]:

x0 = np.asarray([[100,100]], dtype=np.dtype('d'))
C0 = np.asarray([[2,3], [5,3]])
tol = 0.01
maxiter = 500

print quasi_newton_bfgs(x0, f, g, C0, tol, maxiter)


# In[ ]:



