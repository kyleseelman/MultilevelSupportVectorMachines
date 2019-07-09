ClustSubs <-function(traindata1,traindata2,resu){
# Cluster data in refinement phase

Class1_size = dim(traindata1)[1]      # of train data of the majority class
Class2_size = dim(traindata2)[1]   # of train data of the minority class

SS = floor(Class1_size/Param_bag)   # the number of clusters
T = floor(Class2_size/Param_bag)

if(SS == 0){
    SS = 1
}

if(T == 0){
    T = 1
}


NKm = kmeans(traindata1,SS)
Pkm = kmeans(traindata2,T)


Dist_center_cluster = dist(rbind(Nkm$centers,Pkm$centers),method="euclidean")

A_sorted = NULL
I = NULL
tt = t(apply(Dist_center_cluster, 1,index.return=TRUE,sort))
for (i in 1:dim(Dist_center_cluster)[1]){
    A_sorted[[i]] = tt[[i]]$x
    I[[i]] = tt[[i]]$ix
}
A_sorted = matrix(data=unlist(A_sorted),dim(Dist_center_cluster)[1], byrow=TRUE)
I = matrix(data=unlist(I),nrow = dim(Dist_center_cluster)[1], byrow=TRUE)


dimAS = dim(A_sorted)
NCL1 = round((dimAS[2]*per_num)+1);
if(NCL1>dimAS[2]){
    NCL1 = dimAS[2]
}

val = A_sorted[,1:NCL1]
idx = I[, 1:NCL1]


for(i in 1:dim(Dist_center_cluster)[1]){
    Dist_center_cluster[i,idx[i,]] = 1000000
}

B_sorted = NULL
J = NULL
tt = apply(Dist_center_cluster, 2,index.return=TRUE,sort)
for (i in 1:dim(Dist_center_cluster)[2]){
    B_sorted[[i]] = tt[[i]]$x
    J[[i]] = tt[[i]]$ix
}
B_sorted = t(matrix(data=unlist(B_sorted),dim(Dist_center_cluster)[2], byrow=TRUE))
J = t(matrix(data=unlist(J),ncol = dim(Dist_center_cluster)[2], byrow=TRUE))


dimB = dim(B_sorted)
NCL2 = round((dimB[1]*per_num)+1)
if(NCL2>dimB[1]){
    NCL2 = dimB[1]
}

val_Col = B_sorted[1:NCL2,]
idx_Col = J[1:NCL2,]

}