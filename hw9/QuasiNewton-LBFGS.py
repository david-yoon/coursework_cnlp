
# coding: utf-8

# In[1]:

'''
Created on 2016. 5. 13

@author: dato
@desc: cnlp hw9 (quasi-newton method) - L-BFGS equation 1
'''
#-*- coding: utf-8 -*-

import io
import numpy as np
from scipy.misc import derivative
from numpy.linalg import inv
from numpy.linalg import pinv


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

def cal_p(y, s, i):
    if i < 0:
        print "i is below zero"
    return y[i]*s[i]


# In[6]:

def quasi_update_L_BFGS(s, y, g):
    
    a ={}
    m = 5
    q = g
    
    for i in range(len(y)-1, len(y)-1-m, -1):
        if i<0:
            break
        else:
            a[i] = (-1) * cal_p(y, s, i)*s[i]*q
            q = (-1) * (q - a[i]*y[i])
    
    q = cal_p(y, s, len(y)-1) * y[-1]*s[-1]*q
    
    for i in range(m-1, len(y)+1, 1):
        b = (-1) * cal_p(y, s, i)*y[i]*q
        q = (-1) * (q + (a[i] - b)*s[i])
    
    
    return q    


# In[7]:

def quasi_newton_l_bfgs(x0, f, g, tol, maxiter):
    
    cnt = 0
    
    s = []
    y = []
    
    first = 1
    
    for i in range(maxiter):
        cnt = cnt +1
        
        g0 = g(x0)
        
        if first == 1:
            first = 0
            d = -2*(g0.transpose()).transpose()
        else:
            d = -quasi_update_L_BFGS(s, y, g1)*(g0.transpose()).transpose()
        
        x1 = linesearch(x0, d, g0)

        if abs(x1-x0) < tol:
            break
        else:
            g1 = g(x1)
            s.append(x1 - x0)
            y.append(g1 - g0)
            x0 = x1
            
    return x0, cnt


# In[8]:

x0 = 100
tol = 0.01
maxiter = 100

quasi_newton_l_bfgs(x0, f, g, tol, maxiter)


# In[ ]:



