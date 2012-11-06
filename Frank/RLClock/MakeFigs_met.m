 
%% Plot single subject model and data

% this guy is met
%figure
set(figure(1), 'color', 'white'); 
hold off

plot(1:50,smooth(rtCEVpred_met(1,:),1),'--',1:50,smooth(rtcev_met(1,:),1),'-','LineWidth',2);
axis([0 48 0 5000]);
Title('Single Subject CEV Data','FontSize', 18)
legend('Model','Data')
ylabel('RT (ms)',  'FontSize', 16)
xlabel('Trial', 'FontSize', 16) 
set(gca, 'Box', 'off' ); 
saveas(gcf, 'S1_CEV.epsc')

plot(1:50,smooth(rtCEVRpred_met(1,:),1),'--',1:50,smooth(rtcevr_met(1,:),1),'-','LineWidth',2)
axis([0 48 0 5000])
Title('Single Subject CEVR Data','FontSize', 18)
legend('Model','Data')
ylabel('RT (ms)',  'FontSize', 16)
xlabel('Trial', 'FontSize', 16)
set(gca, 'Box', 'off' ); 
saveas(gcf, 'S1_CEVR.epsc')



plot(1:50,smooth(rtIEVpred_met(1,:),1),'--',1:50,smooth(rtiev_met(1,:),1),'-','LineWidth',2)
axis([0 48 0 5000])
Title('Single Subject IEV Data','FontSize', 18)
legend('Model','Data')
ylabel('RT (ms)',  'FontSize', 16)
xlabel('Trial', 'FontSize', 16)
set(gca, 'Box', 'off' ); 
saveas(gcf, 'S1_IEV.epsc')

plot(1:50,smooth(rtDEVpred_met(1,:),1),'--',1:50,smooth(rtdev_met(1,:),1),'-','LineWidth',2)
axis([0 48 0 5000])
Title('Single Subject DEV Data','FontSize', 18)
legend('Model','Data')
ylabel('RT (ms)',  'FontSize', 16)
xlabel('Trial', 'FontSize', 16)
set(gca, 'Box', 'off' ); 
saveas(gcf, 'S1_DEV.epsc')






plot(1:50,smooth(rtCEVpred_met(7,:),1),'--',1:50,smooth(rtcev_met(7,:),1),'-','LineWidth',2);
axis([0 48 0 5000]);
Title('Single Subject CEV Data','FontSize', 18)
legend('Model','Data')
ylabel('RT (ms)',  'FontSize', 16)
xlabel('Trial', 'FontSize', 16) 
set(gca, 'Box', 'off' ); 
saveas(gcf, 'S2_CEV.epsc')

plot(1:50,smooth(rtCEVRpred_met(7,:),1),'--',1:50,smooth(rtcevr_met(7,:),1),'-','LineWidth',2)
axis([0 48 0 5000])
Title('Single Subject CEVR Data','FontSize', 18)
legend('Model','Data')
ylabel('RT (ms)',  'FontSize', 16)
xlabel('Trial', 'FontSize', 16)
set(gca, 'Box', 'off' ); 
saveas(gcf, 'S2_CEVR.epsc')



plot(1:50,smooth(rtIEVpred_met(7,:),1),'--',1:50,smooth(rtiev_met(7,:),1),'-','LineWidth',2)
axis([0 48 0 5000])
Title('Single Subject IEV Data','FontSize', 18)
legend('Model','Data')
ylabel('RT (ms)',  'FontSize', 16)
xlabel('Trial', 'FontSize', 16)
set(gca, 'Box', 'off' ); 
saveas(gcf, 'S2_IEV.epsc')

plot(1:50,smooth(rtDEVpred_met(7,:),1),'--',1:50,smooth(rtdev_met(7,:),1),'-','LineWidth',2)
axis([0 48 0 5000])
Title('Single Subject DEV Data','FontSize', 18)
legend('Model','Data')
ylabel('RT (ms)',  'FontSize', 16)
xlabel('Trial', 'FontSize', 16)
set(gca, 'Box', 'off' ); 
saveas(gcf, 'S2_DEV.epsc')

 
  

 
for (i=1:size(rtDEVpred_met,1))
for (j=1:size(rtDEVpred_met,2))
if (rtdev_met(i,j) == 0) rtDEVpred_met(i,j)= nan; % temporarily set zeros to nan
rtdev_met(i,j)=nan;
end
end
end
notNaN = ~isnan(rtDEVpred_met); % get indices of non nan values
howmany = sum(notNaN); % count them
rtDEVpred_met(~notNaN) =0; % set nans back to zeros
rtdev_met(~notNaN)=0;
columnTot1 = sum(rtDEVpred_met); % sum columns
columnTot2 = sum(rtdev_met); % sum columns

devPred_met_mn = columnTot1 ./ howmany; % calc mean per column
dev_met_mn = columnTot2 ./ howmany; % calc mean per column

% 
% plot(1:50,devPred_met_mn,'--',1:50,dev_met_mn,'-','LineWidth',2)
% axis([0 50 1000 3000])
% Title('DEV Data COMT = met', 'FontSize', 18)
% legend('Model','Data')
% ylabel('RT (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'COMT_met_DEV.epsc')

 



for (i=1:size(rtIEVpred_met,1))
for (j=1:size(rtIEVpred_met,2))
if (rtiev_met(i,j) == 0) rtIEVpred_met(i,j)= nan; % temporarily set zeros to nan
rtiev_met(i,j)=nan;
end
end
end
notNaN = ~isnan(rtIEVpred_met); % get indices of non nan values
howmany = sum(notNaN); % count them
rtIEVpred_met(~notNaN) =0; % set nans back to zeros
rtiev_met(~notNaN)=0;
columnTot1 = sum(rtIEVpred_met); % sum columns
columnTot2 = sum(rtiev_met); % sum columns

ievPred_met_mn = columnTot1 ./ howmany; % calc mean per column
iev_met_mn = columnTot2 ./ howmany; % calc mean per column



% 
% plot(1:50,ievPred_met_mn,'--',1:50,iev_met_mn,'-','LineWidth',2)
% axis([0 50 1000 3000])
% Title('IEV Data COMT =met', 'FontSize', 18)
% legend('Model','Data')
% ylabel('RT (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'COMT_met_IEV.epsc')
  
% 
% plot(1:50,ievPred_val_mn,'--',1:50,iev_val_mn,'-','LineWidth',2)
% axis([0 50 1000 3000])
% Title('IEV Data COMT = val/val', 'FontSize', 18)
% legend('Model','Data')
% ylabel('RT (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'COMT_val_IEV.epsc')



for (i=1:size(rtCEVpred_met,1))
for (j=1:size(rtCEVpred_met,2))
if (rtcev_met(i,j) == 0) rtCEVpred_met(i,j)= nan; % temporarily set zeros to nan
rtcev_met(i,j)=nan;
end
end
end
notNaN = ~isnan(rtCEVpred_met); % get indices of non nan values
howmany = sum(notNaN); % count them
rtCEVpred_met(~notNaN) =0; % set nans back to zeros
rtcev_met(~notNaN)=0;
columnTot1 = sum(rtCEVpred_met); % sum columns
columnTot2 = sum(rtcev_met); % sum columns

cevPred_met_mn = columnTot1 ./ howmany; % calc mean per column
cev_met_mn = columnTot2 ./ howmany; % calc mean per column



% 
% plot(1:50,cevPred_met_mn,'--',1:50,cev_met_mn,'-','LineWidth',2)
% axis([0 50 1000 3000])
% Title('CEV Data COMT = met', 'FontSize', 18)
% legend('Model','Data')
% ylabel('RT (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'COMT_met_CEV.epsc')


for (i=1:size(rtCEVRpred_met,1))
for (j=1:size(rtCEVRpred_met,2))
if (rtcevr_met(i,j) == 0) rtCEVRpred_met(i,j)= nan; % temporarily set zeros to nan
rtcevr_met(i,j)=nan;
end
end
end
notNaN = ~isnan(rtCEVRpred_met); % get indices of non nan values
howmany = sum(notNaN); % count them
rtCEVRpred_met(~notNaN) =0; % set nans back to zeros
rtcevr_met(~notNaN)=0;
columnTot1 = sum(rtCEVRpred_met); % sum columns
columnTot2 = sum(rtcevr_met); % sum columns

cevrPred_met_mn = columnTot1 ./ howmany; % calc mean per column
cevr_met_mn = columnTot2 ./ howmany; % calc mean per
 

%% Plot CEV, DEV, IEV model & data on one graph for each genotyype

 



  

plot(1:50,smooth(cev_met_mn,10,'lowess'),'k-',1:50,smooth(dev_met_mn,10,'lowess'),'b-',1:50,smooth(iev_met_mn,10,'lowess'),'r-',1:50,smooth(cevr_met_mn,10,'lowess'),'m-','LineWidth',2)
 axis([0 50 1000 3000])
Title('COMT met carriers: Data', 'FontSize', 18)
legend('CEV','DEV','IEV','CEVR')
ylabel('RT (ms)',  'FontSize', 16)
xlabel('Trial', 'FontSize', 16)
set(gca, 'Box', 'off' ); 
saveas(gcf, 'COMT_met_All_data.epsc')


plot(1:50,smooth(cevPred_met_mn,10,'lowess'),'k--',1:50,smooth(devPred_met_mn,10,'lowess'),'b--',1:50,smooth(ievPred_met_mn,10,'lowess'),'r--',1:50,smooth(cevrPred_met_mn,10,'lowess'),'m--','LineWidth',2)
 axis([0 50 1000 3000])
Title('COMT met carriers: Model Fits', 'FontSize', 18)
legend('CEV','DEV','IEV','CEVR')
ylabel('RT (ms)',  'FontSize', 16)
xlabel('Trial', 'FontSize', 16)
set(gca, 'Box', 'off' ); 
saveas(gcf, 'COMT_met_All_model.epsc')

%      
  
%  

i=5;plot(1:48,10*smooth(cev_misc2_met(i,2:49),1),1:48,smooth(rtcev_met(i,2:49)-rtcev_met(i,1:48),1),'LineWidth',2)
axis([1 48 -4000 4000])
Title('Exploration, Single met/met Subject, CEV','FontSize', 20)
 xlabel('Trial',  'FontSize', 16)
 ylabel('RT Diff (ms)', 'FontSize', 16)
legend('Model Exp term', 'RT diff')
 set(gca, 'Box', 'off' ); 
 saveas(gcf, 'Explore_CEV_met22.epsc')
 
%plot(1:48,10*smooth(cev_misc2_met(25,2:49),1),1:48,smooth(rtcev_met(25,2:49)-rtcev_met(25,1:48),1),'LineWidth',2)
%% above plots rt change from one trial to next instead of raw rt -- this
%% is better to go along with exp, which is in addition to effects of
%% rt_last etc and cumulative go/nogo...

%% this caclulates correlation between exploration prediction and change in
%% rt's in cev
%corr2(cev_misc2_met(:,2:50),rtcev_met(:,2:50)-rtcev_met(:,1:49))
%corr2(cev_misc2_val(:,2:50),rtcev_val(:,2:50)-rtcev_val(:,1:49))
 
% 
% for i=1:size(cev_misc2_val,1) corr_exp_val(i) = corr2(cev_misc2_val(i,2:50),rtcev_val(i,2:50)-rtcev_val(i,1:49));
% if isnan(corr_exp_val(i)) corr_exp_val(i)=0;
% end
% end
% for i=1:size(cev_misc2_met,1) corr_exp_met(i) = corr2(cev_misc2_met(i,2:50),rtcev_met(i,2:50)-rtcev_met(i,1:49));
% if isnan(corr_exp_met(i)) corr_exp_met(i)=0;
% end
% end
% for i=1:size(cev_misc2_mm,1) corr_exp_mm(i) = corr2(cev_misc2_mm(i,2:50),rtcev_mm(i,2:50)-rtcev_mm(i,1:49));
% if isnan(corr_exp_mm(i)) corr_exp_mm(i)=0;
% end
% end
% mean(nonzeros(corr_exp_mm))
% mean(nonzeros(corr_exp_met))
% mean(nonzeros(corr_exp_val))

%% plot scatter between predicted and actual explores for mets with nonzero
%% explore param

 rtcev_met_diff(:,2:50)=rtcev_met(:,2:50)-rtcev_met(:,1:49);
 %rtcev_met_diff(:,2:50)= sign(rtcev_met_diff(:,2:50)).*log(abs(rtcev_met_diff(:,2:50)));
  
 
 rtdev_met_diff(:,2:50)=rtdev_met(:,2:50)-rtdev_met(:,1:49); 
 rtiev_met_diff(:,2:50)=rtiev_met(:,2:50)-rtiev_met(:,1:49); 
 
 rtcevr_met_diff(:,2:50)=rtcevr_met(:,2:50)-rtcevr_met(:,1:49);
%rtcevr_met_diff(:,2:50)= sign(rtcevr_met_diff(:,2:50)).*log(abs(rtcevr_met_diff(:,2:50)));
  
 misc_met = [cev_misc2_met(:,2:50) cevr_misc2_met(:,2:50) dev_misc2_met(:,2:50) iev_misc2_met(:,2:50)];
 rt_met_diff = [rtcev_met_diff(:,2:50) rtcevr_met_diff(:,2:50) rtdev_met_diff(:,2:50) rtiev_met_diff(:,2:50)];
 
for i=1:size(cev_misc2_met,1) corr_exp_met(i) = corr2(misc_met(i,2:50),rt_met_diff(i,2:50));
if isnan(corr_exp_met(i)) corr_exp_met(i)=0;
end
end
mean(nonzeros(corr_exp_met))
 
 


% this calculates correlation of entire vector of model explore vs actual for all subjects, only non-zero model predictions included. 
% misc_val1=[]; rt_val_diff1=[]; for i=1:19 for j=1:196 if misc_val(i,j)~=0 misc_val1 = [misc_val1 misc_val(i,j)]; rt_val_diff1=[rt_val_diff1 rt_val_diff(i,j)];
% end
% end
% end
% 
% misc_met1=[]; rt_met_diff1=[]; for i=1:50 for j=1:196 if misc_met(i,j)~=0 misc_met1 = [misc_met1 misc_met(i,j)]; rt_met_diff1=[rt_met_diff1 rt_met_diff(i,j)];
% end
% end
% end
% 
% misc1 = [misc_val1 misc_met1];
% rt_diff1 = [rt_val_diff1 rt_met_diff1];
% corr2(misc1,rt_diff1)



%  cev_misc2_val1= zscore(cev_misc2_val);
%  cev_misc2_met1= zscore(cev_misc2_met);
%  cevr_misc2_val1= zscore(cevr_misc2_val);
%  cevr_misc2_met1= zscore(cevr_misc2_met);
 

 for(i=1:size(cev_misc2_met,1))
for(j=1:50)
if cev_misc2_met(i,j)==0 cev_misc2_met1(i,j)= 0;
else cev_misc2_met1(i,j)= cev_misc2_met(i,j); 
end
if dev_misc2_met(i,j)==0 dev_misc2_met1(i,j)=  0; % convert 0's to large # and don't plot
else dev_misc2_met1(i,j)= dev_misc2_met(i,j);
end
if iev_misc2_met(i,j)==0 iev_misc2_met1(i,j)=  0; % convert 0's to large # and don't plot
else iev_misc2_met1(i,j)= iev_misc2_met(i,j);
end
if cevr_misc2_met(i,j)==0 cevr_misc2_met1(i,j)= 0;
else cevr_misc2_met1(i,j)= cevr_misc2_met(i,j);
end
end
 end
  
 figure(1)
hold off
 for(i=1:size(cev_misc2_met,1)) 
if(sum(cev_misc2_met(i,:))~=0)
scatter(cev_misc2_met1(i,:),rtcev_met_diff(i,:),'m')
end
hold on
if(sum(cevr_misc2_met(i,:))~=0)
scatter(cevr_misc2_met1(i,:),rtcevr_met_diff(i,:),'m')
end
if(sum(dev_misc2_met(i,:))~=0)
scatter(dev_misc2_met1(i,:),rtdev_met_diff(i,:),'m')
end
if(sum(iev_misc2_met(i,:))~=0)
scatter(iev_misc2_met1(i,:),rtiev_met_diff(i,:),'m')
end
end
axis([-800 900 -5000 5000])

% 
%  for(i=1:size(cev_misc2_mm,1)) 
% if(sum(cev_misc2_mm(i,:))~=0)
% scatter(cev_misc2_mm1(i,:),rtcev_mm_diff(i,:),'b')
% end
% hold on
% if(sum(cevr_misc2_mm(i,:))~=0)
% scatter(cevr_misc2_mm1(i,:),rtcevr_mm_diff(i,:),'b')
% end
% if(sum(dev_misc2_mm(i,:))~=0)
% scatter(dev_misc2_mm1(i,:),rtdev_mm_diff(i,:),'b')
% end
% if(sum(iev_misc2_mm(i,:))~=0)
% scatter(iev_misc2_mm1(i,:),rtiev_mm_diff(i,:),'b')
% end
%  end
% axis([-800 900 -5000 5000])


% and now for val 
 
Title('Trial to Trial Exploration','FontSize', 20)
 xlabel('Model Explore (ms)',  'FontSize', 16)
 ylabel('RT Diff (ms)', 'FontSize', 16)
 set(gca, 'Box', 'off' ); 
 axis([-800 900 -5000 5000])
 saveas(gcf, 'Explore_scatter.epsc')
hold off

%% Plot all diffs smoothed
% First across all subs (just concatenate D2T and D2C)
% ievPred_diff = [ievPred_diff_T; ievPred_diff_C];
% iev_diff = [iev_diff_T; iev_diff_C];
% plot(1:45,smooth(mean(ievPred_diff(:,1:45)),7),'r--',1:45,smooth(mean(iev_diff(:,1:45)),7),'r-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('IEVdiff All Subjects')
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'IEVdiff_All.epsc')
% 
% devPred_diff = [devPred_diff_T; devPred_diff_C];
% dev_diff = [dev_diff_T; dev_diff_C];
% plot(1:45,smooth(men(devPred_diff(:,1:45)),7),'b--',1:45,smooth(mean(dev_diff(:,1:45)),7),'b-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('DEVdiff All Subjects')
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'DEVdiff_All.epsc')
% 
% 
% cevrPred_diff = [cevrPred_diff_T; cevrPred_diff_C];
% cevr_diff = [cevr_diff_T; cevr_diff_C];
% plot(1:45,smooth(mean(cevrPred_diff(:,1:45)),7),'b--',1:45,smooth(mean(cevr_diff(:,1:45)),7),'b-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('CEVRdiff All Subjects')
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'CEVRdiff_All.epsc')
% 
% 
% % now for each genotype separately
% 
% 
% 
% 
% plot(1:45,smooth(mean(ievPred_diff_T(:,1:45)),7),'r--',1:45,smooth(mean(iev_diff_T(:,1:45)),7),'r-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('IEVdiff D2 = T/T', 'FontSize', 16)
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'D2T_IEVdiff.epsc')
% 
% plot(1:45,smooth(mean(ievPred_diff_C(:,1:45)),7),'k--',1:45,smooth(mean(iev_diff_C(:,1:45)),7),'k-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('IEVdiff D2 = C', 'FontSize', 16)
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'D2C_IEVdiff.epsc')
% 
% plot(1:45,smooth(mean(devPred_diff_T(:,1:45)),7),'r--',1:45,smooth(mean(dev_diff_T(:,1:45)),7),'r-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('DEVdiff D2 = T/T', 'FontSize', 16)
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'D2T_DEVdiff.epsc')
% 
% plot(1:45,smooth(mean(devPred_diff_C(:,1:45)),7),'k--',1:45,smooth(mean(dev_diff_C(:,1:45)),7),'k-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('DEVdiff D2 = C', 'FontSize', 16)
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'D2C_DEVdiff.epsc')
% 
% 
% plot(1:45,smooth(mean(cevrPred_diff_T(:,1:45)),7),'r--',1:45,smooth(mean(cevr_diff_T(:,1:45)),7),'r-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('CEVRdiff D2 = T/T', 'FontSize', 16)
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'D2T_CEVRdiff.epsc')
% 
% plot(1:45,smooth(mean(cevrPred_diff_C(:,1:45)),7),'k--',1:45,smooth(mean(cevr_diff_C(:,1:45)),7),'k-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('CEVRdiff D2 = C', 'FontSize', 16)
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'D2C_CEVRdiff.epsc')
% 
% 
% 
% 
% plot(1:45,smooth(mean(ievPred_diff_met(:,1:45)),7),'m--',1:45,smooth(mean(iev_diff_met(:,1:45)),7),'m-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('IEVdiff COMT = met')
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'met_IEVdiff.epsc')
% 
% plot(1:45,smooth(mean(ievPred_diff_val(:,1:45)),7),'k--',1:45,smooth(mean(iev_diff_val(:,1:45)),7),'k-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('IEVdiff COMT = val/val')
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'val_IEVdiff.epsc')
% 
% plot(1:45,smooth(mean(devPred_diff_met(:,1:45)),7),'m--',1:45,smooth(mean(dev_diff_met(:,1:45)),7),'m-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('DEVdiff COMT = met')
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'met_DEVdiff.epsc')
% 
% plot(1:45,smooth(mean(devPred_diff_val(:,1:45)),7),'k--',1:45,smooth(mean(dev_diff_val(:,1:45)),7),'k-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('DEVdiff COMT = val/val')
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'val_DEVdiff.epsc')
% 
% 
% plot(1:45,smooth(mean(cevrPred_diff_met(:,1:45)),7),'m--',1:45,smooth(mean(cevr_diff_met(:,1:45)),7),'m-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('CEVRdiff COMT = met')
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'met_CEVRdiff.epsc')
% 
% plot(1:45,smooth(mean(cevrPred_diff_val(:,1:45)),7),'k--',1:45,smooth(mean(cevr_diff_val(:,1:45)),7),'k-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('CEVRdiff COMT = val/val')
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'val_CEVRdiff.epsc')
% 
% 
% 
% 
% plot(1:45,smooth(mean(ievPred_diff_A(:,1:45)),7),'b--',1:45,smooth(mean(iev_diff_A(:,1:45)),7),'b-','LineWidth',2);
% axis([0 45 -600 1400])
% Title('IEVdiff DARPP32 = A/A', 'FontSize', 16)
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'D32A_IEVdiff.epsc');
% 
% plot(1:45,smooth(mean(ievPred_diff_G(:,1:45)),7),'k--',1:45,smooth(mean(iev_diff_G(:,1:45)),7),'k-','LineWidth',2);
% axis([0 45 -600 1400])
% Title('IEVdiff DARPP32 = G', 'FontSize', 16)
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'D32G_IEVdiff.epsc');
% 
% plot(1:45,smooth(mean(devPred_diff_A(:,1:45)),7),'b--',1:45,smooth(mean(dev_diff_A(:,1:45)),7),'b-','LineWidth',2);
% axis([0 45 -600 1400])
% Title('DEVdiff DARPP32 = A/A', 'FontSize', 16)
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'D32A_DEVdiff.epsc');
% 
% plot(1:45,smooth(mean(devPred_diff_G(:,1:45)),7),'k--',1:45,smooth(mean(dev_diff_G(:,1:45)),7),'k-','LineWidth',2);
% axis([0 45 -600 1400])
% Title('DEVdiff DARPP32 = G', 'FontSize', 16)
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'D32G_DEVdiff.epsc');
% 
% 
% plot(1:45,smooth(mean(cevrPred_diff_A(:,1:45)),7),'b--',1:45,smooth(mean(cevr_diff_A(:,1:45)),7),'b-','LineWidth',2);
% axis([0 45 -600 1400])
% Title('CEVRdiff DARPP32 = A/A', 'FontSize', 16)
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'D32A_CEVRdiff.epsc');
% 
% plot(1:45,smooth(mean(cevrPred_diff_G(:,1:45)),7),'k--',1:45,smooth(mean(cevr_diff_G(:,1:45)),7),'k-','LineWidth',2)
% axis([0 45 -600 1400])
% Title('CEVRdiff DARPP32 = G', 'FontSize', 16)
% legend('Model','Data')
% ylabel('RT Diff (ms)',  'FontSize', 16)
% xlabel('Trial', 'FontSize', 16)
% saveas(gcf, 'D32G_CEVRdiff.epsc')
% 
% 
%  
