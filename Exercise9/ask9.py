#!/usr/bin/python
from __future__ import print_function
import sys
import requests
from lxml import html

def create_args (a,b,x,y):
        args = []
        args.append(a)
        args.append(b)
        args.append(x)
        args.append(y)
        return args

def gcdExtended(args):
    if args[0] == 0:
        args[2] = 0
        args[3] = 1
        return args
    ret = gcdExtended(create_args(args[1]%args[0], args[0],args[2], args[3]))
    args[2] = ret[3] - (args[1]/args[0]) * ret[2]
    args[3] = ret[2]
    return args


def inverse (x,p):
    ret = gcdExtended(create_args(x, p, 0, 0))
    if (ret[2] < 0):
        return ret[2] + ret[1]
    else:
        return ret[2]

def modMult(x,y,p):
    return x*y % p

def modFact(n,k,p):
    res = 1
    while (n > k):
        res = res*n % p
        n = n - 1
    return res

def binomial_coefficient(n,k,p):
    return modMult(modFact(n,k,p),inverse(modFact(n-k,1,p),p),p)

def get_numbers (tree):
    n = tree.get_element_by_id('N').text_content()
    k = tree.get_element_by_id('K').text_content()
    p = tree.get_element_by_id('P').text_content()
    return (n,k,p)

try:
    url = sys.argv[1];
except:
    sys.exit("Wrong arguments")

#again continue

s = requests.Session()
r = s.get(url)
tree = html.document_fromstring(r.text)

for i in range (0,10,1):
    print (tree.find_class('question')[0].text)
    (n,k,p) = get_numbers(tree)
    print ("N =",n,"K =",k,"P =",p)
    answer = binomial_coefficient(int(n),int(k),int(p))
    print ("The answer is",answer)
    myobj = { 'answer': answer,
              'submit': 'Submit!'
     }
    r = s.post(url,data = myobj)
    tree = html.document_fromstring(r.text)
    if (len(tree.find_class('wrong')) > 1):
        print ("Wrong :-(")
    elif (len(tree.find_class('right'))):
        print ("Right :-)")
    myobj = { 'again': 'Continue!' }
    r = s.post(url,data = myobj)
    tree = html.document_fromstring(r.text)
