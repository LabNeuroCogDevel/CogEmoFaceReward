function [] = ConvertRTs(trnblk, pred, misc1, misc2, misc3, misc4, misc5) 
global TrlType_Colmn
global RTpredCEV RTpredCEVR RTpredDEV RTpredIEV
global CEV_misc1 CEVR_misc1 DEV_misc1 IEV_misc1
global CEV_misc2 CEVR_misc2 DEV_misc2 IEV_misc2
global CEV_misc3 CEVR_misc3 DEV_misc3 IEV_misc3
global CEV_misc4 CEVR_misc4 DEV_misc4 IEV_misc4
global CEV_misc5 CEVR_misc5 DEV_misc5 IEV_misc5
global PE

PE = [PE;misc1];

if(trnblk(1,TrlType_Colmn)==12)
        RTpredCEV = [RTpredCEV; pred];
CEV_misc1 = [CEV_misc1; misc1];
CEV_misc2 = [CEV_misc2; misc2];
CEV_misc3 = [CEV_misc3; misc3]; 
CEV_misc4 = [CEV_misc4; misc4];
CEV_misc5 = [CEV_misc5; misc5]; 

    elseif(trnblk(1,TrlType_Colmn)==34)
        RTpredCEVR = [RTpredCEVR; pred];  
CEVR_misc1 = [CEVR_misc1; misc1];
CEVR_misc2 = [CEVR_misc2; misc2];
CEVR_misc3 = [CEVR_misc3; misc3]; 
CEVR_misc4 = [CEVR_misc4; misc4];
CEVR_misc5 = [CEVR_misc5; misc5]; 
 
  
 elseif(trnblk(1,TrlType_Colmn)==56)
        RTpredDEV =  [RTpredDEV; pred];
DEV_misc1 = [DEV_misc1; misc1];
DEV_misc2 = [DEV_misc2; misc2];
DEV_misc3 = [DEV_misc3; misc3]; 
DEV_misc4 = [DEV_misc4; misc4];
DEV_misc5 = [DEV_misc5; misc5]; 


    elseif(trnblk(1,TrlType_Colmn)==78)
        RTpredIEV = [RTpredIEV; pred];  
IEV_misc1 = [IEV_misc1; misc1];
IEV_misc2 = [IEV_misc2; misc2];
IEV_misc3 = [IEV_misc3; misc3]; 
IEV_misc4 = [IEV_misc4; misc4];
IEV_misc5 = [IEV_misc5; misc5]; 

end 