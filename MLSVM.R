# setwd("Documents")
library(R.matlab)
library(Matrix)
source("MLD.R")
source("ClustSubs.R")
source("UpdateTrainData.R")
source("SVMandWSVM.R")
source("ModelSelection.R")
source("UDSampleRSVM.R")
source("findds.R")
source("GridExploreSVM.R")
library(FNN)
library(pracma)
library(e1071)
#use source() to add file



start = Sys.time()

# define all resu parameters into list
resu<-list(KNN=10,KNN_UNCOARSE=10,Param_bag=300,U_trainsize=3000,MoS_UB=500,Upperlim=500,
Imb_size=300,per_num=0.01,M_LOOP_clusters=1,Multilevel=0, Model_Select=0,Have_test=0, coarse=0,Level_size=1)

# file path 

filename="/Users/kyleseelman/Documents/R/twonorm.mat"
data = readMat(filename)


# convert to matrix
lbl <- Matrix(data = unlist(data$lbl), byrow=TRUE)
data <- Matrix(data = unlist(data$data), byrow=TRUE)


if (resu$Have_test == 0){
    # sparse data for 
    sparse_data <- Matrix(data, sparse=TRUE)
    sparse_lbl <- Matrix(lbl, sparse=TRUE)

    # normalize
    mu <- apply(sparse_data,2,mean)
    sdd <- apply(sparse_data,2,sd)
    normalized_data <- t((t(sparse_data)-mu)/sdd)
    print("----------loaded and normalized data----------------")
    temp = as.vector(lbl)
    min_data = normalized_data[temp==1,]  
    min_lbl =  sparse_lbl[lbl==1]        
    maj_data = normalized_data[temp==-1,] 
    maj_lbl = sparse_lbl[lbl==-1]        

    for(ite in 1:1){
        level = 0
        r = sample(length(maj_lbl))
        tot = floor(length(maj_lbl)*.9)
        Ntrainlbl = maj_lbl[r[1:tot]]       # train lables
        Ntraindata = maj_data[r[1:tot],]
        NVhat = r[1:tot]
        NVhat1 = 1:size(NVhat)[2]                # Comment this line for no multilevel
        
        s = sample(length(min_lbl))      #chooses rows randomly and put it in matrix r
        sot = floor(length(min_lbl)*.9)      # 90% data is chosen for train, 10% for test,s sot: minority class size
        Ptrainlbl = min_lbl[s[1:sot]]     #train lables
        Ptraindata = min_data[s[1:sot],]
        PVhat = s[1:sot]                       # Comment this line for no multilevel
        PVhat1 = 1:size(PVhat)[2]            # Comment this line for no multilevel
        
        Pweight = 1/dim(Ptraindata)[1]                       # The inverse size of minority class
        Nweight = 1/dim(Ntraindata)[1]                     # The inverse size of majority class
        train_l = c(Ntrainlbl, Ptrainlbl)
        traindata = rbind(Ntraindata, Ptraindata)
        
        test_l = c(maj_lbl[r[(tot+1):length(r)]], min_lbl[s[(sot+1):length(s)]])
        testdata = rbind(maj_data[r[(tot+1):length(r)],], min_data[s[(sot+1):length(s)],])
        

        if (resu$Multilevel == 1){
            # call KNN function
            Nknn = get.knnx(Ntraindata,Ntraindata,k=resu$KNN, algorithm='kd_tree')

            # get indices of knn solution
            NAD1 <- matrix(nrow= dim(Ntraindata),ncol=dim(Ntraindata))
            for (i in 1:dim(Ntraindata)[1]){
                NAD1[i,t(Nknn$nn.index)[,i]] = 1 
            }

            Pknn = get.knnx(Ptraindata,Ptraindata,k=resu$KNN, algorithm='kd_tree')
            PAD1 <- matrix(nrow = dim(Ptraindata),ncol= dim(Ptraindata))
            for (i in 1:dim(Ptraindata)[1]){
                PAD1[i,t(Pknn$nn.index)[,i]] = 1
            }

            # Get edges of the knn graph
            Edge_NAD = sum(colSums(NAD1)) - dim(NAD1)[1]
            Node_NAD = dim(NAD1)[1]
            Edge_PAD = sum(colSums(PAD1)) - dim(PAD1)[1]
            Node_PAD = dim(PAD1)[1]
            
            Node_NAD
            Edge_NAD 
            Node_PAD
            Edge_PAD
            
            nresult1 = Nknn$nn.index
            presult1 = Pknn$nn.index

        }

        if (resu$Multilevel == 1){
            # run MLD
            MLD(traindata,train_l,testdata,test_l,Ntraindata,Ntrainlbl,Ntraindata,Ntrainlbl,NAD1,Ptraindata,Ptrainlbl,Ptraindata,Ptrainlbl,PAD1,Pweight,Nweight,level,nresult1,presult1,Nknn$nn.index,Pknn$nn.index,resu)
            
        }
        else{
            # run SVMandWSVM
            SVMReturn = SVMandWSVM(traindata,train_l,testdata,test_l,Pweight,Nweight,resu)
            Sen1=SVMReturn$Sen1;Spe1=SVMReturn$Spe1;Acc1=SVMReturn$Acc1;GMean1=SVMReturn$GMean1;Sen2=SVMReturn$Sen2;Spe2=SVMReturn$Spe2;Acc2=SVMReturn$Acc2;GMean2=SVMReturn$GMean2;SV_indices=SVMReturn$SV_indices;SVData=SVMReturn$SVData;SVlbl=SVMReturn$SVlbl;w=SVMReturn$w;b=SVMReturn$b;BestC=SVMReturn$BestC;BestG=SVMReturn$BestG;BestWC=SVMReturn$BestWC;BestWG=SVMReturn$BestWG;predict_label_SVM=SVMReturn$predict_label_SVM;predict_label_WSVM=SVMReturn$predict_label_WSVM
        }
    }

    # Get average stats
  GMean1 = mean(GMean1)
  GMean2 = mean(GMean2)
  Sensitivity1 = mean(Sen1)
  Sensitivity2 = mean(Sen2)
  Specificity1 = mean(Spe1)
  Specificity2 = mean(Spe2)
  Accuracy1 = mean(Acc1)
  Accuracy2 = mean(Acc2)
  stdGMean1 = sd(GMean1)
  stdGMean2 = sd(GMean2)
  stdSensitivity1 = sd(Sen1)
  stdSensitivity2 = sd(Sen2)
  stdSpecificity1 = sd(Spe1)
  stdSpecificity2 = sd(Spe2)
  stdAccuracy1 = sd(Acc1)
  stdAccuracy2 = sd(Acc2)

  end = Sys.time()
  timecode = end-start
    

} 