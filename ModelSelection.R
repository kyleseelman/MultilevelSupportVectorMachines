ModelSelection <- function(train_l,traindata,Nweight,Pweight,resu,id,C1,G1){

y = dim(traindata)

if(resu$Model_Select == 0){
    BestC = 100
    BestG = 0.5   
    Gmean = 0.9
} else if ((y[1] > resu$MoS_UB) && (resu$Multilevel==1)  && (resu$coarse == 0)){
        BestC = C1 # 100
        BestG = G1 #0.5
        Gmean = 0.9
} else{
    ParamsR <-list(t=0,r=1,v=10,p=0)

    ParamsR[['Design']] = rbind(9,5)

    if(id==1){
    C_Range = cbind(-6.64385619, 6.64385619)
    }
    else{
    C_Range = cbind(-6.64385619, 10.5507468)
    }
# determine the range of gamma
    G_Range = cbind(-7.6439, 1.5850)
#===============================
# Start our selection procedure
#===============================
    if(nargs()< 7){
        Center = cbind(sum(C_Range)/2,sum(G_Range)/2,0.25)
    }
    else{
        Center = cbind(log(C1,base=2),log(G1,base=2),0.25)
    }


UDPts1 = UDSampleRSVM(id, C_Range, G_Range, 1, ParamsR$Design[1], 'class', Center)   # decide training points in 1st stage

GMean1 = GridExploreRSVM(train_l, traindata, UDPts1, ParamsR, Nweight, Pweight, id)

ind1 = 0
for(i in 1:dim(GMean1)[2]){
    ind1[[i]] = which.max(GMean1[,i])
}

Center=UDPts1[ind1,]             # decide the center of searching box in 2nd stage
#-----------------------------------------------
UDPts2 = UDSampleRSVM(id, C_Range, G_Range, 2, ParamsR$Design[2], 'class', Center) # decide training points in 2nd stage
#------------------------------------------------
UDPts2 = UDPts2[2:dim(UDPts2)[1],]                               # remove the center to avoid duplicate computation
GMean2 = GridExploreRSVM(train_l, traindata, UDPts2, ParamsR, Nweight, Pweight, id) # Training in 2nd stage
GMeanAll = rbind(GMean1, GMean2)
Points = rbind(UDPts1, UDPts2)

Gmean = NULL
ind = NULL
for (i in 1:dim(GMeanAll)[2]){
    Gmean[i] = max(GMeanAll[,i])
    ind[i] = which.max(GMeanAll[,i])
}

BestC=2^Points[ind,1] 
BestG=2^Points[ind,2]

}
ModelReturn <-list(BestC = unname(BestC), BestG= unname(BestG), Gmean = Gmean)
return(ModelReturn)
}