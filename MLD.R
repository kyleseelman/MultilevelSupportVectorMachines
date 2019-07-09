# NEED TO LOOK AT FIND FUNCTION TOWARDS THE END 

MLD<-function(traindata,train_l,testdata,test_l,Ndata,Nlbl,Ndata1,Nlbl1,NAD,Pdata,Plbl,Pdata1,Plbl1,PAD,Pweight,Nweight,level,nresult1,presult1,nresult,presult,resu){    
    library(pdist)
    data_size = dim(traindata)[1]


    if ((data_size < resu$Upperlim) || (resu$coarse == 1)){
        resu$Level_size = level
        resu$coarse = 1
        SVMReturn = SVMandWSVM(traindata,train_l,testdata,test_l,Pweight,Nweight,resu)
        Sen1=SVMReturn$Sen1;Spe1=SVMReturn$Spe1;Acc1=SVMReturn$Acc1;GMean1=SVMReturn$GMean1;Sen2=SVMReturn$Sen2;Spe2=SVMReturn$Spe2;Acc2=SVMReturn$Acc2;GMean2=SVMReturn$GMean2;SV_indices=SVMReturn$SV_indices;SVData=SVMReturn$SVData;SVlbl=SVMReturn$SVlbl;w=SVMReturn$w;b=SVMReturn$b;BestC=SVMReturn$BestC;BestG=SVMReturn$BestG;BestWC=SVMReturn$BestWC;BestWG=SVMReturn$BestWG;predict_label_SVM=SVMReturn$predict_label_SVM;predict_label_WSVM=SVMReturn$predict_label_WSVM
        resu$coarse = 0
    } else{
        Ndata = Ndata1  
        Nlbl = Nlbl1
        Pdata = Pdata1 
        Plbl = Plbl1           
        nresult1 = nresult
        presult1 = presult
             
        if ((dim(Ndata1)[1] > resu$Imb_size)){
            R = findds(NAD)
            l = dim(Ndata1)[1]
            Nnode = 1:l
            NVhat = sort(R)
            Comp_Vhat = sort(c(setdiff(Nnode,NVhat),setdiff(NVhat,Nnode)))
                
            Ndata1 = Ndata1[-Comp_Vhat,]
            Nlbl1 = Nlbl1[-Comp_Vhat]
            Nknn = get.knnx(data=Ndata1,query=Ndata1,k=resu$KNN, algorithm='kd_tree')
            NAD <- Matrix(data=0,nrow=dim(Ndata1)[1],ncol=dim(Ndata1)[1], sparse=TRUE)
            for (i in 1:dim(Ndata1)[1]){
                NAD[i,t(Nknn$nn.index)[,i]] = 1
            }
        }

        if (dim(Pdata1)[1] > resu$Imb_size){

            S = findds(NAD)

            p = dim(Pdata1)[1]
            Pnode = 1:p
            PVhat = sort(S)
            Comp_Vhat = sort(c(setdiff(Pnode,PVhat),setdiff(PVhat,Pnode)))
                
            Pdata1 = Pdata1[-Comp_Vhat,] 
            Plbl1 = Plbl1[-Comp_Vhat]

            Pknn = get.knnx(data=Pdata1,query=Pdata1,k=resu$KNN, algorithm='kd_tree')
        
            PAD <- Matrix(data= 0,nrow=dim(Pdata1)[1],ncol=dim(Pdata1)[1], sparse=TRUE)
            for (i in 1:dim(Pdata1)[1]){
                    PAD[i,t(Pknn$nn.index)[,i]] = 1
            }

        }

        Edge_NAD = sum(sum(NAD)) - size(NAD,1)  
        Node_NAD = dim(NAD)[1]
        Edge_PAD = sum(sum(PAD)) - size(PAD,1)
        Node_PAD = dim(PAD)[1]

        level = level + 1
        traindata = rbind(Ndata1, Pdata1)

        train_l = c(Nlbl1,Plbl1)
        Pweight = 1/dim(Plbl1)[1]
        Nweight = 1/dim(Nlbl1)[1]

        if((dim(Ndata1)[1] < resu$Imb_size) && (dim(Pdata1)[1] < resu$Imb_size)){
            resu$coarse = 1
        }
        # Run MLD
        MLDReturn = MLD(traindata,train_l,testdata,test_l,Ndata,Nlbl,Ndata1,Nlbl1,NAD,Pdata,Plbl,Pdata1,Plbl1,PAD,Pweight,Nweight,level,nresult1,presult1,nresult,presult,resu)
        GMean1=MLDReturn$GMean1;GMean2=MLDReturn$GMean2;Sen1=MLDReturn$Sen1;Spe1=MLDReturn$Spe1;Acc1=MLDReturn$Acc1;Sen2=MLDReturn$Sen2;Spe2=MLDReturn$Spe2;Acc2=MLDReturn$Acc2;SVData=MLDReturn$SVData;SVlbl=MLDReturn$SVlbl;BestC=MLDReturn$BestC;BestG=MLDReturn$BestG;BestWC=MLDReturn$BestWC;BestWG=MLDReturn$BestWG;resu=MLDReturn$resu
        # Updata train_data
        UpdateTrainReturn = Update_Train_Data(SVData,SVlbl,Ndata,Nlbl,Pdata,Plbl,resu,nresult1,presult1)
        traindata=UpdateTrainReturn$traindata;train_l=UpdateTrainReturn$train_l;Pweight=UpdateTrainReturn$Pweight;Nweight=UpdateTrainReturn$Nweight;traindata1=UpdateTrainReturn$traindata1;train_l1=UpdateTrainReturn$train1;traindata2=UpdateTrainReturn$traindata2;train_l2=UpdateTrainReturn$train_l2;w=UpdateTrainReturn$w

        resu$coarse = 0
        if(dim(traindata)[1] < resu$U_trainsize){
            # run SVMANDWSVM
            SVMReturn = SVMandWSVM(traindata,train_l,testdata,test_l,Pweight,Nweight,resu,BestC,BestG,BestWC,BestWG)
            Sen1=SVMReturn$Sen1;Spe1=SVMReturn$Spe1;Acc1=SVMReturn$Acc1;GMean1=SVMReturn$GMean1;Sen2=SVMReturn$Sen2;Spe2=SVMReturn$Spe2;Acc2=SVMReturn$Acc2;GMean2=SVMReturn$GMean2;SV_indices=SVMReturn$SV_indices;SVData=SVMReturn$SVData;SVlbl=SVMReturn$SVlbl;w=SVMReturn$w;b=SVMReturn$b;BestC=SVMReturn$BestC;BestG=SVMReturn$BestG;BestWC=SVMReturn$BestWC;BestWG=SVMReturn$BestWG;predict_label_SVM=SVMReturn$predict_label_SVM;predict_label_WSVM=SVMReturn$predict_label_WSVM

        }
        else{
            # all from one of the ones above
            CSVM = BestC
            GSVM = BestG
            CWSVM = BestWC
            GWSVM = BestWG

            for (iter in 1:resu$M_LOOP_clusters){
                UpdateTrainReturn= Update_Train_Data(SVData,SVlbl,Ndata,Nlbl,Pdata,Plbl,resu,nresult1,presult1)
                traindata=UpdateTrainReturn$traindata;train_l=UpdateTrainReturn$train_l;Pweight=UpdateTrainReturn$Pweight;Nweight=UpdateTrainReturn$Nweight;traindata1=UpdateTrainReturn$traindata1;train_l1=UpdateTrainReturn$train1;traindata2=UpdateTrainReturn$traindata2;train_l2=UpdateTrainReturn$train_l2;w=UpdateTrainReturn$w
                ClustSubs(traindata1,traindata2,resu)
            
                for (se in 1:SS){
                    eclass1 = train_l1[NIDX==se]                  # creat a subset of majority class
                    edata1 = traindata1[NIDX==se,]
                            
                    for(t in 1:NCL1){
                            
                        eclass2 = train_l2[PIDX==idx[se,t]]                  # creat a subset of majority class
                        edata2 = traindata2[PIDX==idx[se,t],]
                                
                        traindata = rbind(c(edata1), c(edata2))
                        train_l = rbind(c(eclass1), c(eclass2))
                        Pweight = 1/dim(eclass2)[1]                              # the weight of minority class is the inverse size of minority class
                        Nweight = 1/dim(eclass1)[1]                             # the weight of majority class is the inverse size of majority class
                        CenterMaj = CN[se,]
                        CenterMin = CP[idx[se,t],]

                        # run SVMANDWSVM
                        SVMandWSVM(traindata,train_l,testdata,test_l,Pweight,Nweight,resu,CSVM,GSVM,CWSVM,GWSVM)

                        A = c(A, predict_label_SVM)
                        B = c(B, predict_label_WSVM)
                                
                        dist1 = NULL
                        dist2 = NULL
                        # calculates Euclidean distance of points
                        if(dim(predict_label_SVM)[1] != 0){
                            temp = attr(pdist(testdata,CenterMin),"dist")
                            for (i in 1:length(temp)){
                                dist1[[i]] = temp[i]
                            }
                            temp = attr(pdist(testdata,CenterMaj),"dist")
                            for (i in 1:length(temp)){
                                dist2[[i]] = temp[i]
                            }
                            DistTest[,size(A,2)] = dist1+dist2
                        }

                        SVData1 = rbind(c(SVData1),c(SVData))
                        SVlbl1 = rbind(c(SVlbl1), c(SVlbl))
                        temp = c(SVData1, SVlbl1)
                        b = unique(temp)
                        SVData1 = b[,1:w]
                        SVlbl1 = b[,w+1]
                    }
                }
                for (se in 1:T){
                    for (t in 1:NCL2){
                        if (B_sorted[t,se] != 1000000){
                            eclass2 = train_l2[PIDX==se]                   # creat a subset of majority class
                            edata2 = traindata2[PIDX==se,]
                            # r = r + 1;
                            # Dividing the class2 data
                            eclass1 = train_l1[NIDX==idx_Col[t,se]]                # creat a subset of majority class
                            edata1 = traindata1[NIDX==idx_Col[t,se],]
                                    
                            traindata = rbind(c(edata1), c(edata2))
                            train_l = rbind(c(eclass1), c(eclass2))
                            Pweight = 1/dim(eclass2)[1]                           # the weight of minority class is the inverse size of minority class
                            Nweight = 1/dim(eclass1)[1]                              # the weight of majority class is the inverse size of majority class
                            CenterMaj = CN[idx_Col[t,se],]
                            CenterMin = CP[se,]
                                    
                            SVMandWSVM(traindata,train_l,testdata,test_l,Pweight,Nweight,Model_Selec,Multilevel,MoS_UB,coarse,CSVM,GSVM,CWSVM,GWSVM)
                            A = cbind(c(A), c(predict_label_SVM))
                            B = cbind(c(B), c(predict_label_WSVM))

                                    
                            A = cbind(c(A), c(predict_label_SVM))
                            B = cbind(c(B), c(predict_label_WSVM))

                            # Calculate Euclidean distance
                            if (dim(predict_label_SVM)[1] != 0){
                                temp = attr(pdist(testdata,CenterMin),"dist")
                                for (i in 1:length(temp)){
                                    dist1[[i]] = temp[i]
                                }
                                temp = attr(pdist(testdata,CenterMaj),"dist")
                                for (i in 1:length(temp)){
                                    dist2[[i]] = temp[i]
                                }
                                DistTest[,size(A,2)] = dist1+dist2
                            }
                                    
                                    
                           SVData1 = cbind(c(SVData1), c(SVData))
                           SVlbl1 = cbind(c(SVlbl1), c(SVlbl))
                           temp = c(SVData1,SVlbl1)
                           b = unique(temp, 'rows');
                           SVData1 = b[,1:w]
                           SVlbl1 = b[,w+1]
                        } 
                    }
                }

                num = length(test_l)
                b = 0
                c = 0
                ROC = matrix(data=0,nrow=2,ncol=2)
                predictedSVM = matrix(data=-1,ncol=num,nrow=1);
                for(qq in 1:num){
                    N1 = 0
                    N2 = 0


                    n1 = which((DisTest[qq,]==min(DisTest[qq,])) != FALSE)                            

                    if (dim(A)[2] != dim(DisTest)[2]){
                        sprintf('Unequal size of n1 and Distest %d',n1)
                    }
                            
                    if (dim(n1)[2] ==1){
                        predictedSVM[qq,1] = A[qq,n1]
                    }
                    else{     
                        # if there is two indexes which has the same minimum distance choose the one with majority voting
                        for(ss in 1:dim(n1)[2]){
                            if(A[qq,n1[ss]] == -1){
                                N1 = N1+1
                            }
                            if(A[qq,n1[ss]] == 1){
                                N2 = N2+1
                            }
                        }         
                        if(N1>=N2){
                            predictedSVM[qq,1] = -1            
                            #Combined SVM and label based on majority voting
                        }
                        if(N1<N2){
                            predictedSVM[qq,1] = 1
                        }
                    }
                }
                Acc1 = sum(test_l == predictedSVM)/length(test_l)

                #sensitivity & Specificity
                for(j in 1:length(test_l)){
                    if(test_l[j] == -1){
                        if(predictedSVM[j] == -1){
                            ROC[1,1]=ROC[1,1]+1
                        }
                        if(predictedSVM[j]==1){
                            ROC[1,2]=ROC[1,2]+1
                        }
                    }   
                    if(test_l[j]==1){
                        if(predictedSVM[j]==-1){
                            ROC[2,1]=ROC[2,1]+1
                        }
                        if(predictedSVM[j]==1){
                            ROC[2,2]=ROC[2,2]+1
                        }
                    }
                }
                b = ROC[1,1]+ROC[1,2]
                Sen1 = ROC[1,1]/b
                c = ROC[2,2]+ROC[2,1]
                Spe1 = ROC[2,2]/c
                GMean1 = sqrt(Sen1*Spe1)

                #----------------------BagWSVM------------------------
                b = 0
                c = 0
                ROC = matrix(data=0,nrow=2,ncol=2);                 #contingency table
                predicted = matrix(data=-1,nrow=num,ncol=1)
                for(qq in 1:num){
                    N1 = 0
                    N2 = 0
                    n2 = which((DisTest[qq,]==min(DisTest[qq,])) != FALSE)                    
                    if(dim(n2)[2] == 1){
                        predicted[qq,1] = B[qq,n2]
                    }
                    else{     
                        # if there is two indexes which has the same minimum distance choose the one with majority voting
                        for(ss in 1:dim(n2)[2]){
                            if(B[qq,n2[ss]] == -1){
                                N1 = N1+1
                            }
                            if( B[qq,n2[ss]] == 1){
                                N2 = N2+1
                            }
                        }
                                
                        if(N1>=N2){
                            predicted[qq,1] = -1            
                            # Combined SVM and label based on majority voting
                        }
                        if(N1<N2){
                            predicted[qq,1] = 1
                        }
                    }
                }
                Acc2 = sum(test_l == predicted)/length(test_l)

                #sensitivity & Specificity
                for(j in 1:length(test_l)){
                    if(test_l[j]==-1){
                        if(predicted[j]==-1){
                            ROC[1,1]=ROC[1,1]+1
                        }
                        if(predicted[j]==1){
                            ROC[1,2]=ROC[1,2]+1
                        }
                    }
                            
                    if(test_l[j]==1){
                        if(predicted[j]==-1){
                            ROC[2,1]=ROC[2,1]+1
                        }
                        if(predicted[j]==1){
                            ROC[2,2]=ROC[2,2]+1
                        }
                    }
                }
                b = ROC[1,1]+ROC[1,2]
                Sen2 = ROC[1,1]/b
                c = ROC[2,2]+ROC[2,1]
                Spe2 = ROC[2,2]/c
                GMean2 = sqrt(Sen2*Spe2)
                        
                SVData = SVData1
                SVlbl = SVlbl1
            }
        BestC = CSVM
        BestG = GSVM
        BestWC = CWSVM
        BestWG = GWSVM
        }
    }
        MLDReturn<-list(GMean1=GMean1,GMean2=GMean2,Sen1=Sen1,Spe1=Spe1,Acc1=Acc1,Sen2=Sen2,Spe2=Spe2,Acc2=Acc2,SVData=SVData,SVlbl=SVlbl,BestC=BestC,BestG=BestG,BestWC=BestWC,BestWG=BestWG,resu=resu)

}
