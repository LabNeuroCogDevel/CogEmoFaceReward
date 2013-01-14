
%% saves RT preds and misc variables for different gene groups (or for all subjects if no gene data)


global RTpredCEV RTpredCEVR RTpredDEV RTpredIEV;
global CEV_misc1 CEVR_misc1 DEV_misc1 IEV_misc1;
global CEV_misc2 CEVR_misc2 DEV_misc2 IEV_misc2;
global CEV_misc3 CEVR_misc3 DEV_misc3 IEV_misc3
global CEV_misc4 CEVR_misc4 DEV_misc4 IEV_misc4
global CEV_misc5 CEVR_misc5 DEV_misc5 IEV_misc5

global rtcev_met rtcevr_met rtdev_met rtiev_met
global rtCEVpred_met rtCEVRpred_met rtDEVpred_met rtIEVpred_met
global cev_misc1_met cevr_misc1_met dev_misc1_met iev_misc1_met
global cev_misc2_met cevr_misc2_met dev_misc2_met iev_misc2_met
global sess_trn1
global FitTrls

if(sess_trn1(1,comt_colmn)>=1) % met carriers
    j=j+1;
    
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 12)),3*FitTrls))
        rtcev_met(j,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 12,1),Resp_Colmn);
        rtCEVpred_met(j,i) = RTpredCEV(i);
        cev_misc1_met(j,i) = CEV_misc1(i);
        cev_misc2_met(j,i) = CEV_misc2(i);
        cev_misc3_met(j,i) = CEV_misc3(i);
        cev_misc4_met(j,i) = CEV_misc4(i);
        cev_misc5_met(j,i) = CEV_misc5(i);
        
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 34)),3*FitTrls))
        
        rtcevr_met(j,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 34,1),Resp_Colmn);
        rtCEVRpred_met(j,i) = RTpredCEVR(i);
        cevr_misc1_met(j,i) = CEVR_misc1(i);
        cevr_misc2_met(j,i) = CEVR_misc2(i);
        cevr_misc3_met(j,i) = CEVR_misc3(i);
        cevr_misc4_met(j,i) = CEVR_misc4(i);
        cevr_misc5_met(j,i) = CEVR_misc5(i);
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 56)),3*FitTrls))
        
        rtdev_met(j,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 56,1),Resp_Colmn);
        rtDEVpred_met(j,i) = RTpredDEV(i);
        dev_misc1_met(j,i) = DEV_misc1(i);
        dev_misc2_met(j,i) = DEV_misc2(i);
        dev_misc3_met(j,i) = DEV_misc3(i);
        dev_misc4_met(j,i) = DEV_misc4(i);
        dev_misc5_met(j,i) = DEV_misc5(i);
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 78)),3*FitTrls))
        rtiev_met(j,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 78,1),Resp_Colmn);
        rtIEVpred_met(j,i) = RTpredIEV(i);
        iev_misc1_met(j,i) = IEV_misc1(i);
        iev_misc2_met(j,i) = IEV_misc2(i);
        iev_misc3_met(j,i) = IEV_misc3(i);
        iev_misc4_met(j,i) = IEV_misc4(i);
        iev_misc5_met(j,i) = IEV_misc5(i);
        
        
    end
    
elseif(sess_trn1(1,comt_colmn)==0)
    k=k+1;
    
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 12)),3*FitTrls))
        rtcev_val(k,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 12,1),Resp_Colmn);
        rtCEVpred_val(k,i) = RTpredCEV(i);
        
        cev_misc1_val(k,i) = CEV_misc1(i);
        cev_misc2_val(k,i) = CEV_misc2(i);
        cev_misc3_val(k,i) = CEV_misc3(i);
        cev_misc4_val(k,i) = CEV_misc4(i);
        cev_misc5_val(k,i) = CEV_misc5(i);
        
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 34)),3*FitTrls))
        
        rtcevr_val(k,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 34,1),Resp_Colmn);
        rtCEVRpred_val(k,i) = RTpredCEVR(i);
        
        cevr_misc1_val(k,i) = CEVR_misc1(i);
        cevr_misc2_val(k,i) = CEVR_misc2(i);
        cevr_misc3_val(k,i) = CEVR_misc3(i);
        cevr_misc4_val(k,i) = CEVR_misc4(i);
        cevr_misc5_val(k,i) = CEVR_misc5(i);
        
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 56)),3*FitTrls))
        
        rtdev_val(k,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 56,1),Resp_Colmn);
        rtDEVpred_val(k,i) = RTpredDEV(i);
        
        dev_misc1_val(k,i) = DEV_misc1(i);
        dev_misc2_val(k,i) = DEV_misc2(i);
        dev_misc3_val(k,i) = DEV_misc3(i);
        dev_misc4_val(k,i) = DEV_misc4(i);
        dev_misc5_val(k,i) = DEV_misc5(i);
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 78)),3*FitTrls))
        rtiev_val(k,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 78,1),Resp_Colmn);
        rtIEVpred_val(k,i) = RTpredIEV(i);
        
        iev_misc1_val(k,i) = IEV_misc1(i);
        iev_misc2_val(k,i) = IEV_misc2(i);
        iev_misc3_val(k,i) = IEV_misc3(i);
        iev_misc4_val(k,i) = IEV_misc4(i);
        iev_misc5_val(k,i) = IEV_misc5(i);
        
    end
    
end

if(sess_trn1(1,comt_colmn)==2)
    r=r+1;
    
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 12)),3*FitTrls))
        rtcev_mm(r,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 12,1),Resp_Colmn);
        rtCEVpred_mm(r,i) = RTpredCEV(i);
        cev_misc1_mm(r,i) = CEV_misc1(i);
        cev_misc2_mm(r,i) = CEV_misc2(i);
        
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 34)),3*FitTrls))
        
        rtcevr_mm(r,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 34,1),Resp_Colmn);
        rtCEVRpred_mm(r,i) = RTpredCEVR(i);
        cevr_misc1_mm(r,i) = CEVR_misc1(i);
        cevr_misc2_mm(r,i) = CEVR_misc2(i);
        
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 56)),3*FitTrls))
        
        rtdev_mm(r,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 56,1),Resp_Colmn);
        rtDEVpred_mm(r,i) = RTpredDEV(i);
        dev_misc1_mm(r,i) = DEV_misc1(i);
        dev_misc2_mm(r,i) = DEV_misc2(i);
        
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 78)),3*FitTrls))
        rtiev_mm(r,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 78,1),Resp_Colmn);
        rtIEVpred_mm(r,i) = RTpredIEV(i);
        iev_misc1_mm(r,i) = IEV_misc1(i);
        iev_misc2_mm(r,i) = IEV_misc2(i);
    end
end


if(sess_trn1(1,d2_colmn)==1)
    o=o+1;
    
    
elseif(sess_trn1(1,d2_colmn)==0)
    l=l+1;
    
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 12)),3*FitTrls))
        rtcev_C(l,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 12,1),Resp_Colmn);
        rtCEVpred_C(l,i) = RTpredCEV(i);
        cev_misc1_C(l,i) = CEV_misc1(i);
        cev_misc2_C(l,i) = CEV_misc2(i);
        cev_misc3_C(l,i) = CEV_misc3(i);
        cev_misc4_C(l,i) = CEV_misc4(i);
        cev_misc5_C(l,i) = CEV_misc5(i);
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 34)),3*FitTrls))
        
        rtcevr_C(l,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 34,1),Resp_Colmn);
        rtCEVRpred_C(l,i) = RTpredCEVR(i);
        cevr_misc1_C(l,i) = CEVR_misc1(i);
        cevr_misc2_C(l,i) = CEVR_misc2(i);
        cevr_misc3_C(l,i) = CEVR_misc3(i);
        cevr_misc4_C(l,i) = CEVR_misc4(i);
        cevr_misc5_C(l,i) = CEVR_misc5(i);
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 56)),3*FitTrls))
        
        rtdev_C(l,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 56,1),Resp_Colmn);
        rtDEVpred_C(l,i) = RTpredDEV(i);
        dev_misc1_C(l,i) = DEV_misc1(i);
        dev_misc2_C(l,i) = DEV_misc2(i);
        dev_misc3_C(l,i) = DEV_misc3(i);
        dev_misc4_C(l,i) = DEV_misc4(i);
        dev_misc5_C(l,i) = DEV_misc5(i);
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 78)),3*FitTrls))
        rtiev_C(l,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 78,1),Resp_Colmn);
        rtIEVpred_C(l,i) = RTpredIEV(i);
        iev_misc1_C(l,i) = IEV_misc1(i);
        iev_misc2_C(l,i) = IEV_misc2(i);
        iev_misc3_C(l,i) = IEV_misc3(i);
        iev_misc4_C(l,i) = IEV_misc4(i);
        iev_misc5_C(l,i) = IEV_misc5(i);
    end
    
end

if(sess_trn1(1,d32_colmn)==1)
    q=q+1;
    
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 12)),3*FitTrls))
        rtcev_A(q,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 12,1),Resp_Colmn);
        rtCEVpred_A(q,i) = RTpredCEV(i);
        cev_misc1_A(q,i) = CEV_misc1(i);
        cev_misc2_A(q,i) = CEV_misc2(i);
        cev_misc3_A(q,i) = CEV_misc3(i);
        cev_misc4_A(q,i) = CEV_misc4(i);
        cev_misc5_A(q,i) = CEV_misc5(i);
        
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 34)),3*FitTrls))
        
        rtcevr_A(q,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 34,1),Resp_Colmn);
        rtCEVRpred_A(q,i) = RTpredCEVR(i);
        cevr_misc1_A(q,i) = CEVR_misc1(i);
        cevr_misc2_A(q,i) = CEVR_misc2(i);
        cevr_misc3_A(q,i) = CEVR_misc3(i);
        cevr_misc4_A(q,i) = CEVR_misc4(i);
        cevr_misc5_A(q,i) = CEVR_misc5(i);
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 56)),3*FitTrls))
        
        rtdev_A(q,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 56,1),Resp_Colmn);
        rtDEVpred_A(q,i) = RTpredDEV(i);
        dev_misc1_A(q,i) = DEV_misc1(i);
        dev_misc2_A(q,i) = DEV_misc2(i);
        dev_misc3_A(q,i) = DEV_misc3(i);
        dev_misc4_A(q,i) = DEV_misc4(i);
        dev_misc5_A(q,i) = DEV_misc5(i);
        
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 78)),3*FitTrls))
        rtiev_A(q,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 78,1),Resp_Colmn);
        rtIEVpred_A(q,i) = RTpredIEV(i);
        iev_misc1_A(q,i) = IEV_misc1(i);
        iev_misc2_A(q,i) = IEV_misc2(i);
        iev_misc3_A(q,i) = IEV_misc3(i);
        iev_misc4_A(q,i) = IEV_misc4(i);
        iev_misc5_A(q,i) = IEV_misc5(i);
        
    end
    
elseif(sess_trn1(1,d32_colmn)==0)
    z=z+1;
    
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 12)),3*FitTrls))
        rtcev_G(z,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 12,1),Resp_Colmn);
        rtCEVpred_G(z,i) = RTpredCEV(i);
        cev_misc1_G(z,i) = CEV_misc1(i);
        cev_misc2_G(z,i) = CEV_misc2(i);
        
        cev_misc3_G(z,i) = CEV_misc3(i);
        cev_misc4_G(z,i) = CEV_misc4(i);
        cev_misc5_G(z,i) = CEV_misc5(i);
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 34)),3*FitTrls))
        
        rtcevr_G(z,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 34,1),Resp_Colmn);
        rtCEVRpred_G(z,i) = RTpredCEVR(i);
        cevr_misc1_G(z,i) = CEVR_misc1(i);
        cevr_misc2_G(z,i) = CEVR_misc2(i);
        cevr_misc3_G(z,i) = CEVR_misc3(i);
        cevr_misc4_G(z,i) = CEVR_misc4(i);
        cevr_misc5_G(z,i) = CEVR_misc5(i);
        
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 56)),3*FitTrls))
        
        rtdev_G(z,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 56,1),Resp_Colmn);
        rtDEVpred_G(z,i) = RTpredDEV(i);
        dev_misc1_G(z,i) = DEV_misc1(i);
        dev_misc2_G(z,i) = DEV_misc2(i);
        dev_misc3_G(z,i) = DEV_misc3(i);
        dev_misc4_G(z,i) = DEV_misc4(i);
        dev_misc5_G(z,i) = DEV_misc5(i);
        
    end
    for(i=1:min(length(find(sess_trn1(:,TrlType_Colmn) == 78)),3*FitTrls))
        rtiev_G(z,i) = sess_trn1(i-1 + find(sess_trn1(:,TrlType_Colmn) == 78,1),Resp_Colmn);
        rtIEVpred_G(z,i) = RTpredIEV(i);
        iev_misc1_G(z,i) = IEV_misc1(i);
        iev_misc2_G(z,i) = IEV_misc2(i);
        iev_misc3_G(z,i) = IEV_misc3(i);
        iev_misc4_G(z,i) = IEV_misc4(i);
        iev_misc5_G(z,i) = IEV_misc5(i);
        
    end
    
end


