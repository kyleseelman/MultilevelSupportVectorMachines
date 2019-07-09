GridExploreRSVM<-function(Label, data, UDTable, Params,Nweight,Pweight,id){
library(SparseM)
#--------------------------------------------------------------------------
# under designated parameters values, calculate GMean
# Inputs:
# Label     [r x 1]: training data label
# data      [r x c]: training data inputs
# id        If id=1: SVM and WSVM are being used, if id=2: WRSVM is being used
# Outputs:
# GMean      [m x n]: GMean 
#--------------------------------------------------------------------------    
classes = Label
Attributes = data

m = dim(UDTable)[1]
GMean = matrix(data=0, nrow=m, ncol=1)
Sen = matrix(data=0, nrow=m, ncol=1)
Spe = matrix(data=0, nrow=m, ncol=1)
Ac = matrix(data=0, nrow=m, ncol=1)
   
   
r=sample(length(classes))    #chooses rows randomly and put it in matrix r
tot=floor(length(classes)*.9)  # 90% data is chosen for train, 10% for test
train_l=classes[r[1:tot]]      #train lables
traindata=Attributes[r[1:tot],]


test = r[tot+3:length(r)]
test_l = classes[r[(tot+3):length(r)]]

testdata = Attributes[r[(tot+3):length(r)],]
print("aqui")

idx = which(classes == 1)    #To have label = 1 in testdata
fg = sample(length(idx))
mm = idx[fg[1]] 
test = cbind(test, mm)      
test_l = c(test_l, 1)
testdata = rbind(testdata, Attributes[mm,])      
    
Nidx = which(classes == -1) # To have label = -1 in testdata
Nfg = sample(length(Nidx))
mm = Nidx[Nfg[1]]
test = cbind(test, mm)      
test_l = c(test_l, -1)
testdata = rbind(testdata, Attributes[mm,])

    
for(x in 1:m){
   Params$c=2^UDTable[x, 1]
   Params$g=2^UDTable[x, 2]
   b=0
   c=0
   ROC=matrix(data=0,nrow=2,ncol=2)
  
    if(id==1){
        model = svm(x = as.matrix(traindata), y = train_l,type = 'C-classification', kernal = "radial", cost = Params$c, gamma = Params$g, epsilon =0.1, probability = TRUE)
        pred= predict(object = model, newdata = as.matrix(testdata), probability = TRUE)
    }
    if(id==2){        

        # class.weights= c("-1" = Nweight,"1" = Pweight)
        model = svm(x = as.matrix(traindata), y = train_l,type = 'C-classification', kernal = "radial", cost = Params$c, class.weights= c("-1" = Nweight,"1" = Pweight), gamma = Params$g, epsilon =0.1, probability = TRUE)
        pred = predict(object = model, newdata = as.matrix(testdata), probability = TRUE)
        
    }
    
    predict_label=unname(pred[1:length(pred)])

    # Accuracy
    Ac[x]=sum(predict_label==test_l)/length(test_l)

    for(j in 1:length(test_l)){
        if(test_l[j]== -1){
            if(predict_label[j] == -1){
                ROC[1,1]=ROC[1,1]+1
            }
            if(predict_label[j]==1){
                ROC[2,1]=ROC[2,1]+1
            }
        }   
        if(test_l[j]==1){
            if(predict_label[j]== -1){
               ROC[1,2]=ROC[1,2]+1
            }
            if(predict_label[j]==1){
                ROC[2,2]=ROC[2,2]+1
            }
        }
    }
    b = ROC[1,1]+ROC[2,1]
    Sen[x] = ROC[1,1]/b
    c = ROC[2,2]+ROC[1,2]
    Spe[x] = ROC[2,2]/c
    GMean[x] = sqrt(Sen[x]*Spe[x])

    return(GMean)
}
}
