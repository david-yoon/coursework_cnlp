
# coding: utf-8

# In[1]:

'''
Created on 2016. 5. 13

@author: dato
@desc: cnlp hw9 (quasi-newton method) - L-BFGS equation 2
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

def g(x):
    x1=x[0][0]
    x2=x[0][1]
    
    x_dx1= -( -8*(np.exp(x1+x2)) + 2*np.exp(x2) +1 ) / ( 9*(np.exp(x1+x2) + np.exp(x2) +1) )
    x_dx2= ( 2*(np.exp(x1+x2)) + 2*np.exp(x2) -1 ) / ( 3*(np.exp(x1+x2) + np.exp(x2) +1) )
    
    return np.asarray([[x_dx1, x_dx2]])


# In[4]:

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


# In[5]:

def cal_p(y, s, i):
    if i < 0:
        print "i is below zero"
    return pinv(np.dot(np.transpose(y[i]),s[i]))


# In[6]:

def quasi_update_L_BFGS(s, y, g):

    if( len(y) < 1):
        return np.asarray([[2,3], [5,3]])*g
    
    a ={}
    m = 5
    q = g
    
#     C0 =  cal_p(y, s, len(y)-1) * np.dot(np.transpose(y[-1]),s[-1])
    
    for i in range(len(y)-1, len(y)-1-m, -1):
        if i<0:
            break
        else:
            a[i] = (-1) * cal_p(y, s, i)*np.transpose(s[i])*q
            q = (-1) * (q - a[i]*y[i])
    
    q = cal_p(y, s, len(y)-1) * np.dot(np.transpose(y[-1]),s[-1])*q
    
    for i in range(m-1, len(y)+1, 1):
        b = (-1) * cal_p(y, s, i)*np.transpose(y[i])*q
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
            d = -np.asarray([[2,3], [5,3]]).dot(g0.transpose()).transpose()
        else:
            d = -quasi_update_L_BFGS(s, y, g1).dot(g0.transpose()).transpose()
        
        x1 = linesearch(x0, d, g0)
        
        if np.linalg.norm(x1-x0) < tol:
            break
        else:
            g1 = g(x1)
            s.append(x1 - x0)
            y.append(g1 - g0)
            x0 = x1
            
    return x0, cnt


# In[8]:

x0 = np.asarray([[100,100]], dtype=np.dtype('d'))
tol = 0.01
maxiter = 500

print quasi_newton_l_bfgs(x0, f, g, tol, maxiter)


# In[ ]:



