
# coding: utf-8

# In[1]:

'''
Created on 2016. 5. 13

@author: dato
@desc: cnlp hw9 (quasi-newton method) - BFGS equation 1
    
'''
#-*- coding: utf-8 -*-

import io
import numpy as np
from scipy.misc import derivative


# In[2]:

def f(x):
    return np.log(4 * np.exp(-x) + 2) + x * 32/36


# In[3]:

def g(x):
    return (2*4*np.exp(x) -1) / (9*np.exp(x) +2)


# In[4]:

def linesearch(x0, d, g):
    a = 100
    r = 0.5
    b = 0.5
    cnt = 0
    
    c = -b*np.transpose(d)*g

    while True:

        if f(x0) - f(x0 + a*d) >= a*c:
            break
        else:
            a = r*a
        
        cnt = cnt + 1
        if cnt > 1000:
            print 'reach max loop'
            break
            
    
    x1 = x0 + a*d
        
    return x1


# In[5]:

def quasi_update_BFGS(C0, s1, y1):
    
    I = np.identity(1)
    p1 = y1 * s1
    c1 = (I - p1*s1*np.transpose(y1)) * C0 * (I - p1*y1*np.transpose(s1)) + p1*s1*np.transpose(s1)
    
    return c1    


# In[6]:

def quasi_newton(x0, f, g, C0, tol, maxiter):
    
    cnt = 0
    
    for i in range(maxiter):
        cnt = cnt +1
        
        g0 = g(x0)
        d = -C0 * g0
        x1 = linesearch(x0, d, g0)

        if abs(x1-x0) < tol:
            break
        else:
            g1 = g(x1)
            s1 = x1 - x0
            y1 = g1 - g0
            C1 = quasi_update_BFGS(C0, s1, y1)

            x0 = x1
            C0 = C1
            
    return x0, cnt


# In[7]:

x0 = 100
C0 = 2
tol = 0.01
maxiter = 100

quasi_newton(x0, f, g, C0, tol, maxiter)


# In[ ]:



