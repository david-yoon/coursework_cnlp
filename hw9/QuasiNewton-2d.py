
# coding: utf-8

# In[13]:

'''
Created on 2016. 5. 13

@author: dato
@desc: cnlp hw9 (quasi-newton method- 2d)
'''
#-*- coding: utf-8 -*-

import io
import numpy as np
from scipy.misc import derivative


# In[14]:

def f(x):
    return np.log(2*np.exp(-x[0]) + 2 * np.exp(-x[0] - x[1]) + 2) + x[0] * 32/36 + x[1] * 24/36


# In[15]:

def f_param(x1, x2):
    return np.log(2*np.exp(-x1) + 2 * np.exp(-x1 - x2) + 2) + x1 * 32/36 + x2 * 24/36


# In[16]:

def partial_derivative(func, var=0, point=[]):
    args = point[:]
    def wraps(x):
        args[var] = x
        return func(*args)
    return derivative(wraps, point[var], dx = 1e-6)


# In[17]:

def g(x):
    tmp=np.asarray([1,1], dtype=np.float32)
    tmp[0] = partial_derivative(f_param, 0, [x[0],x[1]])
    tmp[1] = partial_derivative(f_param, 1, [x[0],x[1]])
    
    return tmp


# In[18]:

def linesearch(x0, d, g):
    a = 100
    r = 0.5
    b = 0.5
    
    c = -b*np.transpose(d).dot(g)
    
    while True:

        if f(x0) - f(x0 + a*d) >= a*c:
            break
        else:
            a = r*a
    
    x1 = x0 + a*d
    
#     print 'avl' +  str(x0) + " " + str(x1) + str(a*d)
    
    return x1


# In[19]:

def quasi_update_BFGS(C0, s1, y1):

#     print 'enter'
    
    I = np.identity(2)
    p1 = y1 * s1
    c1 = (I - p1*s1*np.transpose(y1)) * C0 * (I - p1*y1*np.transpose(s1)) + p1*s1*np.transpose(s1)
    
    return c1   


# In[20]:

def quasi_newton(x0, f, g, C0, tol, maxiter):
    for i in range(maxiter):

        g0 = g(x0)
        d = -C0.dot(g0)
        
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
            
    return x0


# In[21]:

x0 = np.asarray([100,100], dtype=np.dtype('d'))
C0 = np.asarray([[2,3], [5,3]])
tol = 0.01
maxiter = 100

quasi_newton(x0, f, g, C0, tol, maxiter)


# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:



