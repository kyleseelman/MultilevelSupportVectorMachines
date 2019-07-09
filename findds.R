
findds<-function(A){
print("finddsss")
T = 0.6
dimA = dim(A);
l = c(1:dimA[1])

R = NULL
iter = 0
while(length(R) < (T * dimA[1])){
    l = c(1:dimA[1])
    l[R]=0
    while(nnz(l)>0){
        toPick = which(l!=0)
        i = toPick[sample(1:length(toPick), size=1)]
        
        R = cbind(R, i)
        l[i] = 0
        neigh = which(A[i,]!=0)
        l[neigh] = 0
    }
    iter = iter+1
}
return(R)
}
