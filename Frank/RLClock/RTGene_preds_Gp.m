 
global RTpredCEV RTpredCEVR RTpredDEV RTpredIEV;
global CEV_misc1 CEVR_misc1 DEV_misc1 IEV_misc1;
global CEV_misc2 CEVR_misc2 DEV_misc2 IEV_misc2;
global rtcev_met rtcevr_met rtdev_met rtiev_met
global rtCEVpred_met rtCEVRpred_met rtDEVpred_met rtIEVpred_met
global cev_misc1_met cevr_misc1_met dev_misc1_met iev_misc1_met
global cev_misc2_met cevr_misc2_met dev_misc2_met iev_misc2_met
global sess_trn1
global FitTrls

if(sess_trn1(1,comt_colmn)>=1)

    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 12)))
        rtcev_met(subsessnum,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 12,1),Resp_Colmn);
        rtCEVpred_met(subsessnum,i) = RTpredCEV(i);
        cev_misc1_met(subsessnum,i) = CEV_misc1(i);
        cev_misc2_met(subsessnum,i) = CEV_misc2(i);

    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 34)))

        rtcevr_met(subsessnum,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 34,1),Resp_Colmn);
        rtCEVRpred_met(subsessnum,i) = RTpredCEVR(i);
        cevr_misc1_met(subsessnum,i) = CEVR_misc1(i);
        cevr_misc2_met(subsessnum,i) = CEVR_misc2(i);

    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 56)))

        rtdev_met(subsessnum,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 56,1),Resp_Colmn);
        rtDEVpred_met(subsessnum,i) = RTpredDEV(i);
        dev_misc1_met(subsessnum,i) = DEV_misc1(i);
        dev_misc2_met(subsessnum,i) = DEV_misc2(i);

    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 78)))
        rtiev_met(subsessnum,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 78,1),Resp_Colmn);
        rtIEVpred_met(subsessnum,i) = RTpredIEV(i);
        iev_misc1_met(subsessnum,i) = IEV_misc1(i);
        iev_misc2_met(subsessnum,i) = IEV_misc2(i);
    end

elseif(sess_trn1(1,comt_colmn)==0)
    k=k+1;

    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 12)))
        rtcev_val(k,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 12,1),Resp_Colmn);
        rtCEVpred_val(k,i) = RTpredCEV(i);

        cev_misc1_val(k,i) = CEV_misc1(i);
        cev_misc2_val(k,i) = CEV_misc2(i);
    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 34)))

        rtcevr_val(k,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 34,1),Resp_Colmn);
        rtCEVRpred_val(k,i) = RTpredCEVR(i);

        cevr_misc1_val(k,i) = CEVR_misc1(i);
        cevr_misc2_val(k,i) = CEVR_misc2(i);

    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 56)))

        rtdev_val(k,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 56,1),Resp_Colmn);
        rtDEVpred_val(k,i) = RTpredDEV(i);

        dev_misc1_val(k,i) = DEV_misc1(i);
        dev_misc2_val(k,i) = DEV_misc2(i);

    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 78)))
        rtiev_val(k,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 78,1),Resp_Colmn);
        rtIEVpred_val(k,i) = RTpredIEV(i);

        iev_misc1_val(k,i) = IEV_misc1(i);
        iev_misc2_val(k,i) = IEV_misc2(i);

    end

end

if(sess_trn1(1,comt_colmn)==2)
    r=r+1;

    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 12)))
        rtcev_mm(r,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 12,1),Resp_Colmn);
        rtCEVpred_mm(r,i) = RTpredCEV(i);
        cev_misc1_mm(r,i) = CEV_misc1(i);
        cev_misc2_mm(r,i) = CEV_misc2(i);

    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 34)))

        rtcevr_mm(r,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 34,1),Resp_Colmn);
        rtCEVRpred_mm(r,i) = RTpredCEVR(i);
        cevr_misc1_mm(r,i) = CEVR_misc1(i);
        cevr_misc2_mm(r,i) = CEVR_misc2(i);

    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 56)))

        rtdev_mm(r,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 56,1),Resp_Colmn);
        rtDEVpred_mm(r,i) = RTpredDEV(i);
        dev_misc1_mm(r,i) = DEV_misc1(i);
        dev_misc2_mm(r,i) = DEV_misc2(i);

    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 78)))
        rtiev_mm(r,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 78,1),Resp_Colmn);
        rtIEVpred_mm(r,i) = RTpredIEV(i);
        iev_misc1_mm(r,i) = IEV_misc1(i);
        iev_misc2_mm(r,i) = IEV_misc2(i);
    end
end


if(sess_trn1(1,d2_colmn)==1)
    o=o+1;

    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 12)))
        rtcev_T(o,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 12,1),Resp_Colmn);
        rtCEVpred_T(o,i) = RTpredCEV(i);
    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 34)))

        rtcevr_T(o,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 34,1),Resp_Colmn);
        rtCEVRpred_T(o,i) = RTpredCEVR(i);
    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 56)))

        rtdev_T(o,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 56,1),Resp_Colmn);
        rtDEVpred_T(o,i) = RTpredDEV(i);
    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 78)))
        rtiev_T(o,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 78,1),Resp_Colmn);
        rtIEVpred_T(o,i) = RTpredIEV(i);
    end

elseif(sess_trn1(1,d2_colmn)==0)
    l=l+1;

    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 12)))
        rtcev_C(l,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 12,1),Resp_Colmn);
        rtCEVpred_C(l,i) = RTpredCEV(i);
    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 34)))

        rtcevr_C(l,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 34,1),Resp_Colmn);
        rtCEVRpred_C(l,i) = RTpredCEVR(i);
    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 56)))

        rtdev_C(l,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 56,1),Resp_Colmn);
        rtDEVpred_C(l,i) = RTpredDEV(i);
    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 78)))
        rtiev_C(l,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 78,1),Resp_Colmn);
        rtIEVpred_C(l,i) = RTpredIEV(i);
    end

end

if(sess_trn1(1,d32_colmn)==1)
    q=q+1;

    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 12)))
        rtcev_A(q,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 12,1),Resp_Colmn);
        rtCEVpred_A(q,i) = RTpredCEV(i);
    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 34)))

        rtcevr_A(q,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 34,1),Resp_Colmn);
        rtCEVRpred_A(q,i) = RTpredCEVR(i);
    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 56)))

        rtdev_A(q,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 56,1),Resp_Colmn);
        rtDEVpred_A(q,i) = RTpredDEV(i);
    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 78)))
        rtiev_A(q,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 78,1),Resp_Colmn);
        rtIEVpred_A(q,i) = RTpredIEV(i);
    end

elseif(sess_trn1(1,d32_colmn)==0)
    z=z+1;

    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 12)))
        rtcev_G(z,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 12,1),Resp_Colmn);
        rtCEVpred_G(z,i) = RTpredCEV(i);
    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 34)))

        rtcevr_G(z,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 34,1),Resp_Colmn);
        rtCEVRpred_G(z,i) = RTpredCEVR(i);
    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 56)))

        rtdev_G(z,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 56,1),Resp_Colmn);
        rtDEVpred_G(z,i) = RTpredDEV(i);
    end
    for(i=1:length(find(sess_trn1(1:FitTrls,TrlType_Colmn) == 78)))
        rtiev_G(z,i) = sess_trn1(i-1 + find(sess_trn1(1:FitTrls,TrlType_Colmn) == 78,1),Resp_Colmn);
        rtIEVpred_G(z,i) = RTpredIEV(i);
    end

end


