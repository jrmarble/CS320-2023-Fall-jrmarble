# Assign2-6: 20 points
# Please translate the following code
# into Python and then implement all the
# functions called in the code to make the
# translation (written in Python) works correctly
#
# let
# string_merge
# (cs1: string)(cs2: string): string =
# let n1 = string_length(cs1)
# and n2 = string_length(cs2) in
# let rec
# foreach(i1: int)(i2:int)(work) =
# if
# i1 < n1
# then (
# if
# i2 < n2
# then
#   let c1 = string_get_at(cs1)(i1)
#   and c2 = string_get_at(cs2)(i2) in
#   if c1 <= c2
#   then (work(c1); foreach(i1+1)(i2+0)(work))
#   else (work(c2); foreach(i1+0)(i2+1)(work))
# else
#   int1_foreach(n1-i1)
#     (fun i -> work(string_get_at(cs1)(i1+i)))
# ) else (
#   int1_foreach(n2-i2)
#     (fun i -> work(string_get_at(cs2)(i2+i)))
# )
# in
#   string_make_fwork(foreach(0)(0))
# ;;
import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

def string_length(cs):
    return len(cs)

def string_get_at(cs,i):
    return cs[i]

def string_make_fwork(fwork):
    xs = []
    def char_col(ch):
        xs.append(ch)
    fwork(char_col)
    return ''.join(xs)

def string_merge(cs1,cs2):
    l1 = string_length(cs1)
    l2 = string_length(cs2)
    
    list = []
    
    def foreach(i1, i2, work):
        if (i1<l1):
            if (i2<l2):
                c1 = string_get_at(cs1,i1)
                c2 = string_get_at(cs2,i2)
                if(c1 <= c2):
                    work(c1)
                    foreach(i1+1, i2, work)
                else:
                    work(c2)
                    foreach(i1,i2+1, work)
            else:
                for i in range(i1, l1):
                    work(cs1[i])
        else:
            for i in range(i2, l2):
                work(cs2[i])
    foreach(0,0,list.append)
    return ''.join(list)
        