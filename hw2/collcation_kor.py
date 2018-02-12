
# coding: utf-8

# In[1]:

'''
Created on 2016. 3. 17

@author: dato
@Des: cnlp hw2
'''
#-*- coding: utf-8 -*-


import io
import math
from __future__ import division
import sys
import nltk

from konlpy.tag import Kkma
from os import listdir
from os.path import isfile, join

srcDir = '../data/chosun/'



# # Data Read

# In[2]:

onlyfiles = [f for f in listdir(srcDir) if isfile(join(srcDir, f))]
data=''


# In[3]:

for x in range(0,len(onlyfiles)):
#    print (onlyfiles[x])
    path = srcDir + onlyfiles[x]
#    print ('data read : ' + srcDir + inputFileName)
    f = io.open(path, 'r', encoding='utf-8' )
    data+=(f.read())
    #f = open(path, 'r')
len(data)


# In[4]:

kkma = Kkma()
token=kkma.morphs(data)
len(token)


# In[ ]:




# In[11]:

wordDic={}
colDic={}

w2=token[0].encode('utf-8')
wordDic[w2] = 1


# In[12]:

for x in range (1, len(token)):
    w1 = w2
    w2 = token[x].encode('utf-8')
    if w2 not in wordDic:
        wordDic[w2] = 1
    else:
        wordDic[w2] += 1
        
    data = w1+w2
    
    if len(w1)<10:
        col='0'+len(w1).__str__()+data
    else:
        col=len(w1).__str__()+data
    
    if col not in colDic:
        colDic[col] = 1
    else:
        colDic[col] += 1


# In[17]:




# In[16]:

N = sum(wordDic.values())
print (N)


# In[15]:

wordDic[',']


# # Likelihood Calculation

# In[340]:

def ratio_func(k, n, x):
    result = math.pow(x, k)*math.pow(1-x, n-k)
    if result==0:
        return sys.float_info.min
    return float(result)


# In[341]:

def decode(data):
    
    num=int(data[:2])
    a=data[2:num+2]
    b=data[num+2:]
    
    return num, a, b


# In[342]:

math.log(ratio_func(712,26444,0.026))


# In[ ]:




# In[ ]:




# In[343]:

ratioDic={}

for key, value in colDic.iteritems():
#     print ('key=' + key)
#     print ('value=' + value.__str__())

    num, a, b = decode(key)
       
#     print (num)
#     print (a)
#     print (b)

#    w=key.split('_', 1)    
    
    c1=float(wordDic[a])
    c2=float(wordDic[b])
    c12=float(value)
    p=float(c2/N)
    p1=float(c12/c1)
    p2=float((c2-c12)/(N-c1))
    
    result=math.log(ratio_func(c12, c1, p)) + math.log(ratio_func(c2-c12, N-c1,p))                 - math.log(ratio_func(c12, c1, p1)) - math.log(ratio_func(c2-c12, N-c1, p2))
    result = float(result * (-2))   
  
    ratioDic[key]=result
    
#     break;



# In[344]:

sortRatioDic=sorted(ratioDic, key=ratioDic.get, reverse=True)


# In[345]:

sortRatioDic[2]


# In[346]:

from __future__ import print_function


# In[347]:

for x in range(0, 20):
    
    num, a, b = decode(sortRatioDic[x])
#     print (num)
#     print (a)
#     print (b)
       
    mergeStr = ratioDic[sortRatioDic[x]].__str__() + '\t'         + wordDic[a].__str__() + '\t'         + wordDic[b].__str__() + '\t'         + colDic[sortRatioDic[x]].__str__() + '\t'
    
    print(mergeStr,end="")
    print(a + ' ' + b)  
    


# In[ ]:




# In[ ]:



