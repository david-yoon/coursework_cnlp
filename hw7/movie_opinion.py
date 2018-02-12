
# coding: utf-8

# In[870]:

'''
Created on 2016. 4. 17

@author: dato
@desc: cnlp hw7 (movie rating classification)
'''
#-*- coding: utf-8 -*-


import io
from konlpy.tag import Kkma


# In[720]:

data_file = './movie.tsv'


# In[721]:

f = io.open(data_file, 'r', encoding='utf-8')
lines = f.readlines()

total_doc = len(lines)
print 'total # of doc : ' + str(total_doc)


# 
# 0: 제목
# 1: rating (1~4, 5~7, 8~10)
# 2: comment
# 
# word_2_index = {} : 입력 word 를 index 로 바꾼 값
# index_2_word = [] : index 가 의미하는 word
# 
# cnt_dic_all = {} : 전체 morph dictionary 
# cnt_dic_pos = {} : positive class dictionary 
# cnt_dic_neut = {} : neutral class dictionry
# cnt_dic_neg = {} : negative class dictionary
# 
# cnt_class_pos : postive class count
# cnt_class_neut : neutral class count
# cnt_class_neg : negative class count
# 
# 

# In[722]:

def bulid_dictionary (tokens, word_to_index, index_to_word):
    
    for token in tokens:

        if token not in word_to_index:
            word_to_index[token] = len(word_to_index)
            index_to_word.append(token)
            
    return


# In[723]:

parsed_lines = [line.split('\t') for line in lines]

# change rating into class
for line in parsed_lines:
    rating = float(line[1])
    
    if rating >= 8.0:
        line[1] = 'POS'
    elif rating >= 5.0:
        line[1] = 'NEUT'
    else:
        line[1] = 'NEG' 


# Create Dictionary

# In[724]:

kkma = Kkma()

word_to_index = {}
index_to_word = []


for line in parsed_lines:
    # 0: title
    # 1: rating
    # 2: comment
    tokens=kkma.morphs(line[2])
    bulid_dictionary(tokens, word_to_index, index_to_word)

print len(word_to_index)


# Count word within class

# In[725]:

# initialize word count dictionary of each class with bias ( =1 )

cnt_dic_pos = {}
cnt_dic_neut = {}
nt_dic_neg = {}

# Laplace smoothing (add one)
cnt_dic_pos = dict( (nkey, 1) for nkey in [key for key in word_to_index.keys()])
cnt_dic_neut = dict( (nkey, 1) for nkey in [key for key in word_to_index.keys()])
cnt_dic_neg = dict( (nkey, 1) for nkey in [key for key in word_to_index.keys()])


# In[726]:

def add_word_to_class_info(tokens, cnt_dic_class):
    
    for token in tokens:
        cnt_dic_class[token] = cnt_dic_class[token] + 1


# In[727]:

cnt_class_pos = 0
cnt_class_neut = 0
cnt_class_neg = 0

for line in parsed_lines:
    # 0: title
    # 1: rating
    # 2: comment
    tokens=kkma.morphs(line[2])

    if  line[1] == 'POS':
        add_word_to_class_info(tokens, cnt_dic_pos)
        cnt_class_pos = cnt_class_pos + 1
        
    elif line[1] == 'NEUT':
        add_word_to_class_info(tokens, cnt_dic_neut)
        cnt_class_neut  = cnt_class_neut + 1
    else:
        add_word_to_class_info(tokens, cnt_dic_neg)
        cnt_class_neg  = cnt_class_neg + 1    


# In[728]:

print 'pos_class_number : ' + str(cnt_class_pos)
print 'neut_class_number : ' + str(cnt_class_neut)
print 'neg_class_number : ' + str(cnt_class_neg)

print 'pos_class_word_cnt_sum : ' + str(sum(cnt_dic_pos.values())) 
print 'neut_class_word_cnt_sum : ' + str(sum(cnt_dic_neut.values()))
print 'neg_class_word_cnt_sum : ' + str(sum(cnt_dic_neg.values()))


# Calculate Prior, Likelihood table

# In[729]:

# Log Prior calculation

import math

log_prior_pos =  math.log( (float)(cnt_class_pos) / (float)(total_doc) )
log_prior_neut =  math.log( (float)(cnt_class_neut) / (float)(total_doc) )
log_prior_neg =  math.log( (float)(cnt_class_neg) / (float)(total_doc) )


# In[730]:

# Likelihood table calculation


# In[731]:

def cal_likelihood_table(cnt_dic_class):
    
    new_talbe = {}
    word_sum = sum(cnt_dic_class.values())    
    new_table = dict( (key, math.log((float)(value)/(float)(word_sum)) ) for (key, value) in cnt_dic_class.iteritems())    
    
    return new_table


# In[732]:

LL_dic_pos = {}
LL_dic_neut = {}
LL_dic_neg = {}

LL_dic_pos = cal_likelihood_table(cnt_dic_pos)
LL_dic_neut = cal_likelihood_table(cnt_dic_neut)
LL_dic_neg = cal_likelihood_table(cnt_dic_neg)

print sum(cnt_dic_pos.values())
print sum(cnt_dic_neut.values())
print sum(cnt_dic_neg.values())

print sum(LL_dic_pos.values())
print sum(LL_dic_neut.values())
print sum(LL_dic_neg.values())


# Evaluation with naive bayes classification
# 
# eval[][0] = Pos LL
# 
# eval[][1] = Neut LL
# 
# eval[][2] = Neg LL
# 
# eval[][3] = predicted class
# 
# eval[][4] = Pos posterior prob.
# 
# eval[][5] = Neut posterior prob.
# 
# eval[][6] = Neg posterior prob.
# 

# In[742]:

def eval_tokens(tokens, eval):
    
    for token in tokens:
        
        # dictionay 에 없는 token 은 무시
        if token not in word_to_index:
            continue
        
        eval[0] += LL_dic_pos[token]
        eval[1] += LL_dic_neut[token]
        eval[2] += LL_dic_neg[token]
        
    eval[0] += log_prior_pos
    eval[1] += log_prior_neut
    eval[2] += log_prior_neg
    
       
    if (eval[0] >= eval[1]) & (eval[0] >= eval[2]):
        eval[3] = 'POS'
        max = eval[0]
        
    elif eval[1] >= eval[2]:
        eval[3] = 'NEUT'
        max = eval[1]
        
    else:
        eval[3] = 'NEG'
        max = eval[2]
        
        
    nom = math.exp(eval[0] - max) + math.exp(eval[1] - max) + math.exp(eval[2] - max)
    
    eval[4] = math.exp(eval[0] - max) / nom
    eval[5] = math.exp(eval[1] - max) / nom
    eval[6] = math.exp(eval[2] - max) / nom
    


# In[797]:

eval = [ [0 for x in range(7)] for x in range(len(parsed_lines)) ]

cnt = 0
for line in parsed_lines:
    
    tokens = tokens=kkma.morphs(line[2])
    eval_tokens(tokens, eval[cnt])
    cnt += 1


# Calculate In-sample Error

# In[746]:

correct = 0
incorrect = 0

error = []

for x in range( len(parsed_lines) ):
    
    if parsed_lines[x][1] == eval[x][3]:
        correct += 1
    else:
        incorrect += 1
        error.append(x)
        
print 'correct: ' + str(correct)
print 'incorrect: ' + str(incorrect)
print 'accuracy: ' + str( (float)(correct) / (float)(correct+incorrect) )
print 'error: '
error


# In[747]:

print 'ERROR CHECK'

for index in error:
    print 'data: ' + parsed_lines[index][2]
    print 'label: ' + parsed_lines[index][1]
    print 'predict: '
    print eval[index][3:]
    print '\n'


# In[921]:

for index in error:
    tmp = parsed_lines[index][2]
    tmpToken = kkma.morphs(tmp)
    print '\n'
    
    for token in tmpToken:
        print token + '\t'+ str(LL_dic_pos[token]) + '\t'+ str(LL_dic_neut[token]) +'\t'+ str(LL_dic_neg[token])      


# print posterior prob.

# In[785]:

class prettyfloat(float):
    def __repr__(self):
        return "%0.10f" % self


# In[920]:

for index in range(len(parsed_lines)):
    eval[index][4:] = map(prettyfloat, eval[index][4:])    
    print eval[index][3:]
    


# [sample test] Single Sentence Evaluation

# In[968]:

def eval_single_sentence( tmpSen ):

    tmpSen = unicode(sample.decode('utf-8'))
    
    rst = [0 for x in range(7)]
    tmpToken = kkma.morphs(tmpSen)
    eval_tokens(tmpToken, rst)

    print 'INPUT: ' + tmpSen
    print '\n'
    print 'OUTPUT: \n'
    
    print 'log prior probabilities P(c):'
    print 'postive \t neutral \t negative'
    print str(log_prior_pos) + '\t' + str(log_prior_neut) + '\t' + str(log_prior_neg)
    print '\n'

    print 'log likelihood P(v|c):'
    print 'word_id \t postive \t neutral \t negative'
    for token in tmpToken:
        if token not in word_to_index:
            print token +  '\t' + str(0) + '\t' +  str(0) +  '\t' + str(0)
        else:
            print token +  '\t' + str(LL_dic_pos[token]) + '\t' +  str(LL_dic_neut[token]) +  '\t' + str(LL_dic_neg[token])

    print '\n'
    print 'log P(c) + sum log P(v|c) :'
    print 'postive \t neutral \t negative'
    print str(rst[0]) + '\t' + str(rst[1]) + '\t' + str(rst[2])

    print '\n'
    print 'posterior probabilities:'
    print 'positive \t neutral \t negative'
    print str(rst[4]) + '\t' + str(rst[5]) + '\t' + str(rst[6])


# In[969]:

sample = '정말 재밌게 본 영화 강추!!!'


# In[971]:

sample = '내 인생 최악 졸작 영화. 알바 댓글에 속았음'


# In[973]:

eval_single_sentence(sample)

