SVMandWSVM <- function(traindata,train_l,testdata,test_l,Pweight,Nweight,resu,C1,G1,CW1,GW1){
library(e1071)
#source("ModelSelection.R")

if(nargs() < 8){
    # Run model selection
    ModelReturn = ModelSelection(train_l,traindata,Nweight,Pweight,resu,1)
    
    BestC = ModelReturn$BestC
    BestG = ModelReturn$BestG
    Gt = ModelReturn$Gmean
} else{
    # Run model selection
    ModelReturn = ModelSelection(train_l,traindata,Nweight,Pweight,resu,1,C1,G1)
    BestC = ModelReturn$BestC
    BestG = ModelReturn$BestG
    Gt = ModelReturn$Gmean
}
b=0
c=0
ROC=matrix(data=0,nrow=2,ncol=2)                 #contingency table

# Run SVM 
model = svm(x = as.matrix(traindata), y = train_l,type = "C-classification", kernal = "radial", cost = BestC, gamma = BestG, epsilon =0.1, probability = TRUE)
pred = predict(object = model, newdata = as.matrix(testdata), probability = TRUE)

# remove label from prediction values
predict_label_SVM = unname(pred[1:length(pred)])
# Get probabilities
prob = attr(pred, "probabilities")
    
SV_indices = model$index


SVData = traindata[SV_indices,]
SVlbl = train_l[SV_indices]    
    
w = t(model$SV) %*% model$coefs
b = -model$rho
    
if(model$label[1] == -1){
    w = -w
    b = -b
}
    
Acc1 = sum(predict_label_SVM==test_l)/length(test_l)


for(j in 1:length(test_l)){
    if(test_l[j]==-1){
        if(predict_label_SVM[j]==-1){
            ROC[1,1]=ROC[1,1]+1
        }
        if(predict_label_SVM[j]==1){
            ROC[2,1]=ROC[2,1]+1
        }
    }
        
    if(test_l[j]==1){
        if(predict_label_SVM[j]==-1){
            ROC[1,2]=ROC[1,2]+1
        }
        if(predict_label_SVM[j]==1){
            ROC[2,2]=ROC[2,2]+1
        }
    }
}   

b=ROC[1,1]+ROC[2,1]
Sen1 = ROC[1,1]/b
c = ROC[2,2]+ROC[1,2]
Spe1 = ROC[2,2]/c



if (nargs() < 8){
    # Run model selection
    ModelReturn = ModelSelection(train_l,traindata,Nweight,Pweight,resu,2)
    BestWC = ModelReturn$BestC
    BestWG = ModelReturn$BestG
    WGt = ModelReturn$Gmean
} else{
    # Run model selection
    ModelReturn = ModelSelection(train_l,traindata,Nweight,Pweight,resu,2,CW1,GW1)
    BestWC =  ModelReturn$BestC
    BestWG =  ModelReturn$BestG
    WGt =  ModelReturn$Gmean
}
b=0
c=0
ROC=matrix(data = 0,nrow=2,ncol=2)                 

model = svm(x = as.matrix(traindata), y = train_l,type = "C-classification", kernal = "radial", cost = BestWC, gamma = BestWG, epsilon =0.1, probability = TRUE)
pred = predict(object = model, newdata = as.matrix(testdata), probability = TRUE)

predict_label_WSVM = unname(pred[1:length(pred)])
    
    
Acc2 = sum(predict_label_WSVM==test_l)/length(test_l)

for(j in 1:length(test_l)){
    if(test_l[j]==-1){
        if(predict_label_WSVM[j]==-1){
            ROC[1,1]=ROC[1,1]+1
        }
        if(predict_label_WSVM[j]==1){
            ROC[2,1]=ROC[2,1]+1
        }
    }
        
    if(test_l[j]==1){
        if(predict_label_WSVM[j]==-1){
            ROC[1,2]=ROC[1,2]+1
        }
        if(predict_label_WSVM[j]==1){
            ROC[2,2]=ROC[2,2]+1
        }
    }
}   


b = ROC[1,1]+ROC[2,1]
Sen2 = ROC[1,1]/b
c = ROC[2,2]+ROC[1,2]
Spe2 = ROC[2,2]/c
GMean1 = sqrt(Sen1*Spe1)
GMean2 = sqrt(Sen2*Spe2)

SVMReturn <-list(Sen1=Sen1,Spe1=Spe1,Acc1=Acc1,GMean1=GMean1,Sen2=Sen2,Spe2=Spe2,Acc2=Acc2,GMean2=GMean2,SV_indices=SV_indices,SVData=SVData,SVlbl=SVlbl,w=w,b=b,BestC=BestC,BestG=BestG,BestWC=BestWC,BestWG=BestWG,predict_label_SVM=predict_label_SVM,predict_label_WSVM=predict_label_WSVM)


}