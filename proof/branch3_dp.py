from functools import lru_cache
from itertools import product
@lru_cache(None)
def F(st):  # st: sorted tuple of candidate-digit counts per position (only one position nonzero)
    st=tuple(sorted(x for x in st if x>0))
    if sum(st)==1: return 1
    best=99
    k=len(st)
    for opts in product('ZTN',repeat=4):
        # positions beyond k are empty (known not the nonzero position)
        A=[];B=0;C=[];nz=0
        for i,o in enumerate(opts):
            s=st[i] if i<k else 0
            if o=='Z': nz+=1; A.append(s)
            elif o=='T':
                if s==0: break  # testing in empty pos == N
                B+=1; C.append(s-1)
            else:
                if s==9: break
                C.append(s)
        else:
            classes=[]
            if sum(A): classes.append(('A',tuple(A)))
            if sum(C): classes.append(('C',tuple(C)))
            if B: classes.append(('B',B))
            if len(classes)<=1 and not (B==1 and nz==3): continue
            w=0
            for t,c in classes:
                if t=='B':
                    v=0 if (B==1 and nz==3) else (1 if B==1 else F(tuple([1]*B)))
                else: v=F(c)
                w=max(w,v)
            best=min(best,1+w)
    return best
for s in [(9,9,9,9),(9,9,9),(9,9),(9,),(8,),(5,5)]:
    print(s,F(s))
