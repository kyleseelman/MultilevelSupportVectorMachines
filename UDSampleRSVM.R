UDSampleRSVM<-function(id, C_Range, G_Range, Stage, Pattern, Params, Center){
# use UD table to determine the parameter values for model selection
# Inputs:
# C_Range   [1 x 2]  : [min_C, max_C]
# G_Range   [1 x 2]  : [min_gamma, max_gamma]  or e_Range   [1 x 2]  : [min_e, max_e] if id==1
# In Regression, the searching range of episilon is equal to [0 , 0.5]
# Stage     [1 x ?]  : the stage of nested UDs
# Pattern   [1 x 1]  : the UD pattern, 5-5runs,9-9runs,and 13-13runs
# Center    [1 x 2]  : the UD center for current stage 
#                      (default= center of the searching box)

# id        id=1: WSVM and SVM,     id=2: WRSVM
# Outputs:
# UDPts     [? x 2 (or 3)]  : the UD sampling points for current stage
# by K.Y. Lee
#--------------------------------------------------------------------------
   if(nargs() < 7){
        Center = cbind(sum(C_Range)/2,sum(G_Range)/2,0.25)
   }
    CLower=C_Range[1] 
    CUpper=C_Range[2] 
    CLen=CUpper-CLower # lower bound, upper bound and length for C range
    GLower=G_Range[1] 
    GUpper=G_Range[2] 
    GLen=GUpper-GLower # lower bound, upper bound and length for gamma range
    
    
    Center = Center[1:2]
    # 2-D Uniform Design table from http://www.math.hkbu.edu.hk/UniformDesign/

    # FINSIH DOING THIS
    UDTable_30 = cbind(rbind(24,23,1,9,5,11,19,6,21,3,12,15,29,18,17,26,7,4,28,27,25,13,14,29,22,8,2,16,30,10),rbind(25, 6, 12, 18, 16, 7, 4, 9, 11, 3, 14, 20,30, 15,24,2, 29, 21,13, 28,17, 27, 1, 8, 19, 5, 26, 10, 22, 23))
    # UDTable_30 = [24, 25; 23, 6; 1, 12; 9, 18; 5, 16; 11, 7; 19, 4; 6, 9; 21, 11; 3, 3; 12, 14; 15, 20; 20, 30; 18, 15; 17, 24; 26, 2; 7, 29; 4, 21; 28, 13; 27, 28; 25, 17; 13, 27; 14, 1; 29, 8; 22, 19; 8, 5; 2, 26; 16, 10; 30, 22; 10, 23];
    UDTable_20 = cbind(rbind(16,18, 12, 19, 1, 10, 9, 4, 2, 14, 6, 15, 5, 20, 11, 13, 8, 7, 3, 17), rbind(15, 19, 1, 3, 9, 7, 20, 13, 18, 10, 16, 5, 6, 12, 14, 17, 4, 11, 2, 8))
    # UDTable_20 = [16, 15; 18, 19; 12, 1; 19, 3; 1, 9; 10, 7; 9, 20; 4, 13; 2, 18; 14, 10; 6, 16; 15, 5; 5, 6; 20, 12; 11, 14; 13, 17; 8, 4; 7, 11; 3, 2; 17 8 ];
    UDTable_13 = cbind(rbind(7, 5, 12, 2, 9, 6, 3, 11, 13, 19, 1, 4, 8), rbind(7, 4, 3, 11, 10, 13, 2, 12, 8, 5, 6, 9, 1))
    # UDTable_13 = [7, 7; 5, 4; 12, 3; 2, 11; 9, 10; 6, 13; 3, 2; 11, 12; 13, 8; 10, 5; 1, 6; 4, 9; 8, 1];
    UDTable_10 = cbind(rbind(2, 8, 10, 4, 1, 5, 9, 7, 6, 3), rbind(9, 10, 6, 7, 3, 1, 2, 4, 8, 5))
    # UDTable_10 = [2, 9; 8, 10; 10, 6; 4, 7; 1, 3; 5, 1; 9, 2; 7, 4; 6, 8; 3, 5];
    UDTable_9 = cbind(rbind(5, 1, 7, 2, 3, 9, 8, 6, 4), rbind(5, 4, 8, 7, 2, 6, 3, 1, 9))
    # UDTable_9 = [5, 5; 1, 4; 7, 8; 2, 7; 3, 2; 9, 6; 8, 3; 6, 1; 4, 9];
    UDTable_5 = cbind(rbind(3, 1, 2, 4, 5), rbind(3, 2, 5, 1, 4))
    # UDTable_5 = [3, 3; 1, 2; 2, 5; 4, 1; 5, 4];
    #Table = []; 
    
    if (Pattern == 30){
       Table = UDTable_30
    }
    else if (Pattern == 20){
       Table = UDTable_20
    }
    else if (Pattern == 13){
       Table = UDTable_13
    }
    else if (Pattern == 10){
       Table = UDTable_10
    }
    else if (Pattern == 9){
       Table = UDTable_9
    }
    else if (Pattern == 5){
       Table = UDTable_5
    }
    UDPts =(Table-1)/(Pattern-1)/2^(Stage-1)*(matrix(data=1,nrow=Pattern,ncol=1)%*%cbind(CLen,GLen))+matrix(data=1,nrow=Pattern,ncol=1)%*%cbind(Center[1]-CLen/2^Stage, Center[2]-GLen/2^Stage)
    UDPts[which((UDPts[,1]<CLower)),1] = CLower
    UDPts[which((UDPts[,1]>CUpper)),1] = CUpper
    UDPts[which((UDPts[,2]<GLower)),2] = GLower
    UDPts[which((UDPts[,2]>GLower)),2] = GUpper

    return(UDPts)
    
    
}