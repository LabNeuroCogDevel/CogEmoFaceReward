
%% Plot single subject model and data
 
global FitTrls

set(figure(1), 'color', 'white'); 
% hold off
% 
% 
% plot(1:FitTrls,smooth(rtCEVpred_T(1,:),1),'--',1:FitTrls,smooth(rtcev_T(1,:),1),'-','LineWidth',2);
% axis([0 FitTrls 0 5000]);
% Title('Single Subject CEV','FontSize', 32)
% legend('Model','Data')
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24) 
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'S1_CEV.epsc')
% 
% plot(1:FitTrls,smooth(rtCEVRpred_T(1,:),1),'--',1:FitTrls,smooth(rtcevr_T(1,:),1),'-','LineWidth',2)
% axis([0 FitTrls 0 5000])
% Title('Single Subject CEVR','FontSize', 32)
% legend('Model','Data')
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'S1_CEVR.epsc')
% 
% 
% 
% plot(1:FitTrls,smooth(rtIEVpred_T(1,:),1),'--',1:FitTrls,smooth(rtiev_T(1,:),1),'-','LineWidth',2)
% axis([0 FitTrls 0 5000])
% Title('Single Subject IEV','FontSize', 32)
% legend('Model','Data')
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'S1_IEV.epsc')
% 
% plot(1:FitTrls,smooth(rtDEVpred_T(1,:),1),'--',1:FitTrls,smooth(rtdev_T(1,:),1),'-','LineWidth',2)
% axis([0 FitTrls 0 5000])
% Title('Single Subject DEV','FontSize', 32)
% legend('Model','Data')
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'S1_DEV.epsc')




cev_misc3= [ cev_misc3_val]; %go
cevr_misc3= [ cevr_misc3_val];
dev_misc3= [ dev_misc3_val];
iev_misc3= [ iev_misc3_val];


cev_misc4= [ cev_misc4_val]; %nogo
cevr_misc4= [ cevr_misc4_val];
dev_misc4= [ dev_misc4_val];
iev_misc4= [iev_misc4_val];

rtcev = [rtcev_val];
rtcevr = [ rtcevr_val];
rtdev = [ rtdev_val];
rtiev = [ rtiev_val];

rtcev_pred = [rtCEVpred_val];
rtcevr_pred = [rtCEVRpred_val]; 
rtdev_pred = [rtDEVpred_val]; 
rtiev_pred =  [rtIEVpred_val];  

plot(1:FitTrls,smooth(rtdev(sub,:),1,'lowess'),'k',1:FitTrls,x*dev_misc3(sub,:),'g-',1:FitTrls,x*dev_misc4(sub,:),'r-','LineWidth',2);

axis([0 49 0 5000])
Title('Single Subject DEV','FontSize', 32)
legend('Data','Go','NoGo') 
%legend('Data','Go','NoGo','\mu(fast)','\mu(slow)') 
ylabel('RT (ms)',  'FontSize', 24)
xlabel('Trial', 'FontSize', 24) 
set(gca, 'Box', 'off' ); 
saveas(gcf, 'S49_DEV.epsc')
saveas(gcf, 'S49_DEV.fig')

  
%plot(1:FitTrls,smooth(rtcev(sub,:),1,'lowess'),'k',1:FitTrls,x*cev_misc3(sub,:),'g-',1:FitTrls,x*cev_misc4(sub,:),'r-',1:FitTrls-2,6000*mean_f(2:FitTrls-1),1:FitTrls-2,6000*mean_s(2:FitTrls-1), 2:FitTrls,rtcev_pred(sub,2:FitTrls),'w:','LineWidth',2);
plot(1:FitTrls,smooth(rtcev(sub,:),1,'lowess'),'k',1:FitTrls,x*cev_misc3(sub,:),'g-',1:FitTrls,x*cev_misc4(sub,:),'r-','LineWidth',2);

 axis([0 48 0 5000])
Title('Single Subject CEV','FontSize', 32)
%legend('Data','Go','NoGo','Model')
%legend('Data','Go','NoGo','\mu(fast)','\mu(slow)') 
legend('Data','Go','NoGo') 
ylabel('RT (ms)',  'FontSize', 24)
xlabel('Trial', 'FontSize', 24) 
set(gca, 'Box', 'off' ); 
saveas(gcf, 'S49_CEV.epsc')
saveas(gcf, 'S49_CEV.fig')
 
 

% plot(1:FitTrls,smooth(rtcevr(sub,:),1,'lowess'),'k',1:FitTrls,x*cevr_misc3(sub,:),'g-',1:FitTrls,x*cevr_misc4(sub,:),'r-',1:FitTrls-2,6000*mean_f(2:FitTrls-1),1:FitTrls-2,6000*mean_s(2:FitTrls-1),2:FitTrls,rtcevr_pred(sub,2:FitTrls),'w:','LineWidth',2);
plot(1:FitTrls,smooth(rtcevr(sub,:),1,'lowess'),'k',1:FitTrls,x*cevr_misc3(sub,:),'g-',1:FitTrls,x*cevr_misc4(sub,:),'r-','LineWidth',2);
 axis([0 49 0 5000])
 Title('Single Subject CEVR','FontSize', 32)
legend('Data','Go','NoGo')
%legend('Data','Go','NoGo','\mu(fast)','\mu(slow)') 
ylabel('RT (ms)',  'FontSize', 24)
xlabel('Trial', 'FontSize', 24) 
set(gca, 'Box', 'off' ); 
saveas(gcf, 'S49_CEVR.epsc')
saveas(gcf, 'S49_CEVR.fig')

 
%plot(1:FitTrls,smooth(rtiev(sub,:),1,'lowess'),'k',1:FitTrls,x*iev_misc3(sub,:),'g-',1:FitTrls,x*iev_misc4(sub,:),'r-',1:FitTrls-2,6000*mean_f(2:FitTrls-1),1:FitTrls-2,6000*mean_s(2:FitTrls-1),2:FitTrls,rtiev_pred(sub,2:FitTrls),'w:','LineWidth',2);
plot(1:FitTrls,smooth(rtiev(sub,:),1,'lowess'),'k',1:FitTrls,x*iev_misc3(sub,:),'g-',1:FitTrls,x*iev_misc4(sub,:),'r-','LineWidth',2);

 axis([0 49 0 5000])
Title('Single Subject IEV','FontSize', 32)
%legend('Data','Go','NoGo','Model')
legend('Data','Go','NoGo') 
%legend('Data','Go','NoGo','\mu(fast)','\mu(slow)') 
ylabel('RT (ms)',  'FontSize', 24)
xlabel('Trial', 'FontSize', 24) 
set(gca, 'Box', 'off' ); 
saveas(gcf, 'S49_IEV.epsc')
saveas(gcf, 'S49_IEV.fig')

 
% eta_f(1) = 1; b_f(1) =1; % reconstruct eta and beta for fast and slow resps
% eta_s(1) = 1; b_s(1) =1;
% 
% mean_s(1) = 0.5; mean_f(1) = 0.5;
% sigma_s(1) = sqrt(eta_s(1)*b_s(1)/(((eta_s(1)+b_s(1))^2)*(eta_s(1)+b_s(1)+1)));
% sigma_f(1) = sqrt(eta_f(1)*b_f(1)/(((eta_f(1)+b_f(1))^2)*(eta_f(1)+b_f(1)+1)));
% 
% for(i=2:FitTrls-1)
%     eta_s(i) = eta_s(i-1); eta_f(i) = eta_f(i-1); b_s(i) = b_s(i-1); b_f(i) = b_f(i-1); 
%     delta_f(i) =0;delta_s(i) =0;
%     if dev_misc1_met(sub,i) > 0 % positive PE
%         if dev_misc5_met(sub,i) >0
%         eta_s(i) =eta_s(i) +1; 
%          delta_s(i) = dev_misc1_met(sub,i);
%     else
%          eta_f(i) =eta_f(i) +1;
%          delta_f(i) = dev_misc1_met(sub,i);
%         end
%     else
%         if dev_misc5_met(sub,i) >0
%         b_s(i) =b_s(i) +1; 
%         delta_s(i) = dev_misc1_met(sub,i);
%     else
%          b_f(i) =b_f(i) +1;
%          delta_f(i) = dev_misc1_met(sub,i);
%         end
%    end
%     mean_s(i) = eta_s(i) / (eta_s(i) + b_s(i));
%     mean_f(i) = eta_f(i) / (eta_f(i) + b_f(i));   
%     sigma_s(i) = sqrt(eta_s(i)*b_s(i)/(((eta_s(i)+b_s(i))^2)*(eta_s(i)+b_s(i)+1)));
%     sigma_f(i) = sqrt(eta_f(i)*b_f(i)/(((eta_f(i)+b_f(i))^2)*(eta_f(i)+b_f(i)+1)));
% 
% end

% figure(1)
% set(figure(1), 'color', 'white'); 
% subplot(2,1,1)
% 
% plot(1:FitTrls-1,delta_f(1:FitTrls-1)/10,1:FitTrls-1,eta_f(1:FitTrls-1),'k-',1:FitTrls-1,b_f(1:FitTrls-1),'k:','LineWidth',2);
% %1:FitTrls-1,eta_s(1:FitTrls-1),'r-',1:FitTrls-1,b_s(1:FitTrls-1),'r-.','LineWidth',2);
% axis([0 50 -5 20]);
% legend('\delta(fast)','\eta(fast)','\beta(fast)')
% legend('Location','BestOutside')
% legend('boxoff')
% L=legend; set(L,'FontSize',20);
% Title('Beta Params DEV','FontSize', 32)
% set(gca, 'Box', 'off' ); 
% 
% subplot(2,1,2)
% plot(1:FitTrls-1,mean_f(1:FitTrls-1),'g-', 1:FitTrls-1,mean_s(1:FitTrls-1),'r',1:FitTrls-1,sigma_f(1:FitTrls-1),'c-',1:FitTrls-1,sigma_s(1:FitTrls-1),'m-','LineWidth',2); 
% 
% legend('\mu(fast)', '\mu(slow)','\sigma(fast)','\sigma(slow)') 
% legend('Location','BestOutside')
% legend('boxoff'); L=legend; set(L,'FontSize',20);
% set(gca, 'Box', 'off' ); 
% xlabel('Trial', 'FontSize', 24)
% 
% 
% %ylabel('RT (ms)',  'FontSize', 24)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'Beta_DEV.epsc')
% saveas(gcf, 'Beta_DEV.fig')

 
% 
% for(i=2:FitTrls-1)
%     eta_s(i) = eta_s(i-1); eta_f(i) = eta_f(i-1); b_s(i) = b_s(i-1); b_f(i) = b_f(i-1); 
%     delta_f(i) =0;delta_s(i) =0;
%     if iev_misc1_met(sub,i) > 0 % positive PE
%         if iev_misc5_met(sub,i) >0
%         eta_s(i) =eta_s(i) +1; 
%          delta_s(i) = iev_misc1_met(sub,i);
%     else
%          eta_f(i) =eta_f(i) +1;
%          delta_f(i) = iev_misc1_met(sub,i);
%         end
%     else
%         if iev_misc5_met(sub,i) >0
%         b_s(i) =b_s(i) +1; 
%         delta_s(i) = iev_misc1_met(sub,i);
%     else
%          b_f(i) =b_f(i) +1;
%          delta_f(i) = iev_misc1_met(sub,i);
%         end
%     end
%     mean_s(i) = eta_s(i) / (eta_s(i) + b_s(i));
%     mean_f(i) = eta_f(i) / (eta_f(i) + b_f(i));   
%     sigma_s(i) = sqrt(eta_s(i)*b_s(i)/(((eta_s(i)+b_s(i))^2)*(eta_s(i)+b_s(i)+1)));
%     sigma_f(i) = sqrt(eta_f(i)*b_f(i)/(((eta_f(i)+b_f(i))^2)*(eta_f(i)+b_f(i)+1)));
% 
% end
% %  
% 
% figure(2)
% set(figure(2), 'color', 'white'); 
% subplot(2,1,1) 
% 
% plot(1:FitTrls-1,delta_s(1:FitTrls-1)/10,1:FitTrls-1,eta_s(1:FitTrls-1),'k-',1:FitTrls-1,b_s(1:FitTrls-1),'k:','LineWidth',2);
% %1:FitTrls-1,eta_s(1:FitTrls-1),'r-',1:FitTrls-1,b_s(1:FitTrls-1),'r-.','LineWidth',2);
% axis([0 50 -5 20]);
% legend('\delta(slow)','\eta(slow)','\beta(slow)')
% legend('Location','BestOutside')
% legend('boxoff');  L=legend; set(L,'FontSize',20);
% Title('Beta Params IEV','FontSize', 32)
% set(gca, 'Box', 'off' ); 
% 
% subplot(2,1,2)
% plot(1:FitTrls-1,mean_f(1:FitTrls-1),'g-', 1:FitTrls-1,mean_s(1:FitTrls-1),'r',1:FitTrls-1,sigma_f(1:FitTrls-1),'c-',1:FitTrls-1,sigma_s(1:FitTrls-1),'m-','LineWidth',2); 
% 
% legend('\mu(fast)', '\mu(slow)','\sigma(fast)','\sigma(slow)') 
% legend('Location','BestOutside')
% legend('boxoff');  L=legend; set(L,'FontSize',20);
% set(gca, 'Box', 'off' ); 
% xlabel('Trial', 'FontSize', 24)
% 
% 
% %ylabel('RT (ms)',  'FontSize', 24)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'Beta_IEV.epsc')
% saveas(gcf, 'Beta_IEV.fig')

% 
% cev_misc4= cev_misc4_T;
% cevr_misc4= cevr_misc4_T; 
% dev_misc4= dev_misc4_T; 
% iev_misc4= iev_misc4_T; 
% 
% cev_misc5= cev_misc5_T;  
% cevr_misc5= cevr_misc5_T;  
% dev_misc5= dev_misc5_T;  
% iev_misc5= iev_misc5_T; 

% figure(1)
% hold off;  
% 
% plot(1:FitTrls,smooth(mean(iev_misc4),10,'lowess'),'r-',1:FitTrls,smooth(mean(cevr_misc4),10,'lowess'),'m-',1:FitTrls,smooth(mean(cev_misc4),10,'lowess'),'k-',1:FitTrls,smooth(mean(dev_misc4),10,'lowess'),'LineWidth',2) 
% 
% %axis([0 FitTrls 1000 3000])
% Title('Model Go','FontSize', 32 )
% legend('IEV','CEVR','CEV','DEV'); legend BOXOFF
% legend BOXOFF
% %legend('Location','SouthWest')
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'AllConds_ModelGO.epsc')
% saveas(gcf, 'AllConds_ModelGO.jpg')
% saveas(gcf, 'AllConds_ModelGO.fig')
% 
% figure(2)
% plot(1:FitTrls,smooth(mean(iev_misc5),10,'lowess'),'r-',1:FitTrls,smooth(mean(cevr_misc5),10,'lowess'),'m-',1:FitTrls,smooth(mean(cev_misc5),10,'lowess'),'k-',1:FitTrls,smooth(mean(dev_misc5),10,'lowess'),'LineWidth',2) 
% 
% %axis([0 FitTrls 1000 3000])
% Title('Model NoGo','FontSize', 32 )
% legend('IEV','CEVR','CEV','DEV'); legend BOXOFF
% legend BOXOFF
% %legend('Location','SouthWest')
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'AllConds_ModelNOGO.epsc')
% saveas(gcf, 'AllConds_ModelNOGO.jpg')
% saveas(gcf, 'AllConds_ModelNOGO.fig')
%  
% plot(1:FitTrls,smooth(mean(iev_misc4-iev_misc5),10,'lowess'),'r-',1:FitTrls,smooth(mean(cevr_misc4-cevr_misc5),10,'lowess'),'m-',1:FitTrls,smooth(mean(cev_misc4-cev_misc5),10,'lowess'),'k-',1:FitTrls,smooth(mean(dev_misc4-dev_misc5),10,'lowess'),'LineWidth',2) 
%   
% %axis([0 FitTrls 1000 3000])
% Title('Model Go - NoGo','FontSize', 32 )
% legend('IEV','CEVR','CEV','DEV'); legend BOXOFF
% legend BOXOFF
% %legend('Location','SouthWest')
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'AllConds_ModelGN.epsc')
% saveas(gcf, 'AllConds_ModelGN.jpg')
% saveas(gcf, 'AllConds_ModelGN.fig')
% else
% 
% plot(1:FitTrls,smooth(rtCEVpred_met(17,:),1),'--',1:FitTrls,smooth(rtcev_met(17,:),1),'-','LineWidth',2);
% axis([0 FitTrls 0 5000]);
% Title('Single Subject CEV','FontSize', 32)
% legend('Model','Data')
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24) 
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'S2_CEV.epsc')
% 
% plot(1:FitTrls,smooth(rtCEVRpred_met(17,:),1),'--',1:FitTrls,smooth(rtcevr_met(17,:),1),'-','LineWidth',2)
% axis([0 FitTrls 0 5000])
% Title('Single Subject CEVR','FontSize', 32)
% legend('Model','Data')
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'S2_CEVR.epsc')
% 
% 
% 
% plot(1:FitTrls,smooth(rtIEVpred_met(17,:),1),'--',1:FitTrls,smooth(rtiev_met(17,:),1),'-','LineWidth',2)
% axis([0 FitTrls 0 5000])
% Title('Single Subject IEV','FontSize', 32)
% legend('Model','Data')
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'S2_IEV.epsc')
% 
% plot(1:FitTrls,smooth(rtDEVpred_met(17,:),1),'--',1:FitTrls,smooth(rtdev_met(17,:),1),'-','LineWidth',2)
% axis([0 FitTrls 0 5000])
% Title('Single Subject DEV','FontSize', 32)
% legend('Model','Data')
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'S2_DEV.epsc')

 

%% Plot genotypes for DEV and IEV

% get rid of missing data trials for mean calculation..
  
  
%% Plot CEV, DEV, IEV model & data on one graph for each genotyype


cev_Pred = [ rtCEVpred_C];
cev_RT = [ rtcev_C];
iev_Pred = [ rtIEVpred_C];
iev_RT = [ rtiev_C];

cevr_Pred = [ rtCEVRpred_C];
cevr_RT = [ rtcevr_C];

dev_Pred = [ rtDEVpred_C];
dev_RT = [ rtdev_C];
 
for (i=1:size(cev_RT,1))
for (j=1:size(cev_RT,2))
if (cev_RT(i,j) == 0) cev_RT(i,j)= nan; % temporarily set zeros to nan
cev_RT(i,j)=nan;
end
end
end
notNaN = ~isnan(cev_RT); % get indices of non nan values
howmany = sum(notNaN); % count them
cev_RT(~notNaN) =0; % set nans back to zeros
 
columnTot  = sum(cev_RT); % sum columns 
cev_mn = columnTot ./ howmany; % calc mean per column
 

for (i=1:size(dev_RT,1))
for (j=1:size(dev_RT,2))
if (dev_RT(i,j) == 0) dev_RT(i,j)= nan; % temporarily set zeros to nan
dev_RT(i,j)=nan;
end
end
end
notNaN = ~isnan(dev_RT); % get indices of non nan values
howmany = sum(notNaN); % count them
dev_RT(~notNaN) =0; % set nans back to zeros
 
columnTot  = sum(dev_RT); % sum columns 
dev_mn = columnTot ./ howmany; % calc mean per column


for (i=1:size(iev_RT,1))
for (j=1:size(iev_RT,2))
if (iev_RT(i,j) == 0) iev_RT(i,j)= nan; % temporarily set zeros to nan
iev_RT(i,j)=nan;
end
end
end
notNaN = ~isnan(iev_RT); % get indices of non nan values
howmany = sum(notNaN); % count them
iev_RT(~notNaN) =0; % set nans back to zeros
 
columnTot  = sum(iev_RT); % sum columns 
iev_mn = columnTot ./ howmany; % calc mean per column


for (i=1:size(cevr_RT,1))
for (j=1:size(cevr_RT,2))
if (cevr_RT(i,j) == 0) cevr_RT(i,j)= nan; % temporarily set zeros to nan
cevr_RT(i,j)=nan;
end
end
end
notNaN = ~isnan(cevr_RT); % get indices of non nan values
howmany = sum(notNaN); % count them
cevr_RT(~notNaN) =0; % set nans back to zeros
 
columnTot  = sum(cevr_RT); % sum columns 
cevr_mn = columnTot ./ howmany; % calc mean per column

hold off;
plot( 1:FitTrls,smooth(iev_mn(1:FitTrls),10,'lowess'),'r-'  ,1:FitTrls,smooth(cevr_mn(1:FitTrls),10,'lowess'),'m-', 1:FitTrls,smooth(cev_mn(1:FitTrls),10,'lowess'),'k-',1:FitTrls,smooth(dev_mn(1:FitTrls),10,'lowess'),'LineWidth',2) 
axis([0 FitTrls 1000 3000])
Title('All Subjects: Data','FontSize', 32 )
legend('IEV','CEVR','CEV','DEV'); legend BOXOFF
legend BOXOFF
%legend('Location','SouthWest')
ylabel('RT (ms)',  'FontSize', 24)
xlabel('Trial', 'FontSize', 24)
set(gca, 'Box', 'off' ); 
saveas(gcf, 'AllConds_Data.epsc')
saveas(gcf, 'AllConds_Data.jpg')
saveas(gcf, 'AllConds_Data.fig')





for (i=1:size(cev_Pred,1))
for (j=1:size(cev_Pred,2))
if (cev_Pred(i,j) == 0) cev_Pred(i,j)= nan; % temporarily set zeros to nan
cev_Pred(i,j)=nan;
end
end
end
notNaN = ~isnan(cev_Pred); % get indices of non nan values
howmany = sum(notNaN); % count them
cev_Pred(~notNaN) =0; % set nans back to zeros
 
columnTot  = sum(cev_Pred); % sum columns 
cevPred_mn = columnTot ./ howmany; % calc mean per column
 

for (i=1:size(dev_Pred,1))
for (j=1:size(dev_Pred,2))
if (dev_Pred(i,j) == 0) dev_Pred(i,j)= nan; % temporarily set zeros to nan
dev_Pred(i,j)=nan;
end
end
end
notNaN = ~isnan(dev_Pred); % get indices of non nan values
howmany = sum(notNaN); % count them
dev_Pred(~notNaN) =0; % set nans back to zeros
 
columnTot  = sum(dev_Pred); % sum columns 
devPred_mn = columnTot ./ howmany; % calc mean per column


for (i=1:size(iev_Pred,1))
for (j=1:size(iev_Pred,2))
if (iev_Pred(i,j) == 0) iev_Pred(i,j)= nan; % temporarily set zeros to nan
iev_Pred(i,j)=nan;
end
end
end
notNaN = ~isnan(iev_Pred); % get indices of non nan values
howmany = sum(notNaN); % count them
iev_Pred(~notNaN) =0; % set nans back to zeros
 
columnTot  = sum(iev_Pred); % sum columns 
ievPred_mn = columnTot ./ howmany; % calc mean per column


for (i=1:size(cevr_Pred,1))
for (j=1:size(cevr_Pred,2))
if (cevr_Pred(i,j) == 0) cevr_Pred(i,j)= nan; % temporarily set zeros to nan
cevr_Pred(i,j)=nan;
end
end
end
notNaN = ~isnan(cevr_Pred); % get indices of non nan values
howmany = sum(notNaN); % count them
cevr_Pred(~notNaN) =0; % set nans back to zeros
 
columnTot  = sum(cevr_Pred); % sum columns 
cevrPred_mn = columnTot ./ howmany; % calc mean per column




plot(1:FitTrls,smooth(ievPred_mn(1:FitTrls),10,'lowess'),'r--',  1:FitTrls,smooth(cevrPred_mn(1:FitTrls),10,'lowess'),'m--', 1:FitTrls,smooth(cevPred_mn(1:FitTrls),10,'lowess'),'k--',  1:FitTrls,smooth(devPred_mn(1:FitTrls),10,'lowess'),'b--','LineWidth',2) 
Title('All Subjects: Model Fits','FontSize', 32)
axis([0 FitTrls 1000 3000]) 
legend('IEV','CEVR','CEV','DEV'); legend BOXOFF
legend BOXOFF
 
ylabel('RT (ms)',  'FontSize', 24)
xlabel('Trial', 'FontSize', 24)
set(gca, 'Box', 'off' ); 
saveas(gcf, 'AllConds_model.epsc')
saveas(gcf, 'AllConds_model.jpg')
saveas(gcf, 'AllConds_model.fig')

%  

 
 
 

cev_PE =[cev_misc1_met; cev_misc1_val];
cevr_PE =[cevr_misc1_met; cevr_misc1_val];
dev_PE =[dev_misc1_met; dev_misc1_val];
iev_PE =[iev_misc1_met; iev_misc1_val];

rtdev = [rtdev_met; rtdev_val];
rtcev = [rtcev_met; rtcev_val];
rtiev = [rtiev_met; rtiev_val];
rtcevr = [rtcevr_met; rtcevr_val];

n_PEtrls = 15; % only look at first n trials when EV's aren't quite learned
 % (otherwise PE's end up getting smaller in place where rt's converge..)
rtdev1 = nonzeros(rtdev(:,1:n_PEtrls)); % filter out non-RTs
rtcev1 = nonzeros(rtcev(:,1:n_PEtrls)); 
rtiev1 = nonzeros(rtiev(:,1:n_PEtrls));  
rtcevr1 = nonzeros(rtcevr(:,1:n_PEtrls));

dev_PE1 = dev_PE(find(nonzeros(rtdev(:,1:n_PEtrls)))); % get corresponding PE's
cev_PE1 = cev_PE(find(nonzeros(rtcev(:,1:n_PEtrls))));
iev_PE1 = iev_PE(find(nonzeros(rtiev(:,1:n_PEtrls))));
cevr_PE1 = cevr_PE(find(nonzeros(rtcevr(:,1:n_PEtrls))));

y_dev = [rtdev1 dev_PE1];
y_dev=sortrows(y_dev,1); % sort by increasing rt's

y_cev = [rtcev1 cev_PE1];
y_cev=sortrows(y_cev,1); % sort by increasing rt's

y_iev = [rtiev1 iev_PE1];
y_iev=sortrows(y_iev,1); % sort by increasing rt's

y_cevr = [rtcevr1 cevr_PE1];
y_cevr=sortrows(y_cevr,1); % sort by increasing rt's
% 
figure(1)
plot(y_dev(:,1),smooth(y_dev(:,2),500,'lowess'),'Linewidth',2);
axis([500 4000 -15 25])
Title('DEV Prediction Errors','FontSize', 32);
xlabel('RT (ms)',  'FontSize', 24)
ylabel('Reward Prediction Error', 'FontSize', 24)
set(gca, 'Box', 'off' );
saveas(gcf, 'DEV_PE_15trl.epsc')

figure(2)
set(figure(2), 'color', 'white');
hold off
plot(y_cev(:,1),smooth(y_cev(:,2),500,'lowess'),'k','Linewidth',2);
axis([500 4000 -15 25])
Title('CEV Prediction Errors','FontSize', 32);
xlabel('RT (ms)',  'FontSize', 24)
ylabel('Reward Prediction Error', 'FontSize', 24)
set(gca, 'Box', 'off' );
saveas(gcf, 'CEV_PE_15trl.epsc')

figure(3)
set(figure(3), 'color', 'white');
hold off
plot(y_iev(:,1),smooth(y_iev(:,2),500,'lowess'),'r','Linewidth',2);
axis([500 4000 -15 25])
Title('IEV Prediction Errors','FontSize', 32);
xlabel('RT (ms)',  'FontSize', 24)
ylabel('Reward Prediction Error', 'FontSize', 24)
set(gca, 'Box', 'off' );
saveas(gcf, 'IEV_PE_15trl.epsc')

figure(4)
set(figure(4), 'color', 'white');
hold off
plot(y_cevr(:,1),smooth(y_cevr(:,2),500,'lowess'),'c','Linewidth',2);
axis([500 4000 -15 25])
Title('CEVR Prediction Errors','FontSize', 32);
xlabel('RT (ms)',  'FontSize', 24)
ylabel('Reward Prediction Error', 'FontSize', 24)
set(gca, 'Box', 'off' );
saveas(gcf, 'CEVR_PE_15trl.epsc')

%  
figure(1)
hold off;
i=32; %  
plot(1:FitTrls-2,10*smooth(cevr_misc2_val(i,2:FitTrls-1),1),1:FitTrls-2,smooth(rtcevr_val(i,2:FitTrls-1)-rtcevr_val(i,1:FitTrls-2),1),'LineWidth',2)
axis([1 FitTrls -4000 4000])
Title('Exploration','FontSize', 32)
  
xlabel('Trial',  'FontSize', 24)
 ylabel('RT Diff (ms)', 'FontSize', 24)
legend('Model Exp term', 'RT diff'); legend('Location','NorthEast'); %legend BOXOFF;
 set(gca, 'Box', 'off' ); 
 set(0,'defaulttextcolor','r');
 text(30,-3400,'Single Subject, CEV','HorizontalAlignment','center')
 set(0,'defaulttextcolor','k');
 set(0,'defaulttextfontsize',16)
  saveas(gcf, 'Explore_CEV_32.epsc')
  saveas(gcf, 'Explore_CEV_32.jpg')
  saveas(gcf, 'Explore_CEV_32.fig')
  
  
  
 
%plot(1:FitTrls,10*smooth(cev_misc2_met(25,2:49),1),1:FitTrls,smooth(rtcev_met(25,2:49)-rtcev_met(25,1:FitTrls),1),'LineWidth',2)
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

  
 rtcev_val_diff(:,2:FitTrls)=rtcev_val(:,2:FitTrls)-rtcev_val(:,1:FitTrls-1);
 rtdev_val_diff(:,2:FitTrls)=rtdev_val(:,2:FitTrls)-rtdev_val(:,1:FitTrls-1); 
 rtiev_val_diff(:,2:FitTrls)=rtiev_val(:,2:FitTrls)-rtiev_val(:,1:FitTrls-1); 
 rtcevr_val_diff(:,2:FitTrls)=rtcevr_val(:,2:FitTrls)-rtcevr_val(:,1:FitTrls-1);
   
 misc_val = [cev_misc2_val(:,2:FitTrls) cevr_misc2_val(:,2:FitTrls) dev_misc2_val(:,2:FitTrls) iev_misc2_val(:,2:FitTrls)];
 rt_val_diff = [rtcev_val_diff(:,2:FitTrls) rtcevr_val_diff(:,2:FitTrls) rtdev_val_diff(:,2:FitTrls) rtiev_val_diff(:,2:FitTrls)];
   

for i=1:size(cev_misc2_val,1) corr_exp_val(i) = corr2(misc_val(i,2:FitTrls),rt_val_diff(i,2:FitTrls));
if isnan(corr_exp_val(i)) corr_exp_val(i)=0;
end
end 
 
 
mean(nonzeros(corr_exp_val))


% this calculates correlation of entire vector of model explore vs actual for all subjects, only non-zero model predictions included. 
 misc_val1=[]; rt_val_diff1=[]; for i=1:19 for j=1:196 if misc_val(i,j)~=0 misc_val1 = [misc_val1 misc_val(i,j)]; rt_val_diff1=[rt_val_diff1 rt_val_diff(i,j)];
 end
 end
 end 
% 
 misc1 = [misc_val1 ];
 rt_diff1 = [rt_val_diff1];
 corr_exp = corr2(misc1,rt_diff1)
size(misc1)
 %    
%  for(i=1:size(cev_misc2_met,1))
% for(j=1:FitTrls)
% if cev_misc2_met(i,j)==0 cev_misc2_met1(i,j)= 0;
% else cev_misc2_met1(i,j)= cev_misc2_met(i,j); 
% end
% if dev_misc2_met(i,j)==0 dev_misc2_met1(i,j)=  0; % convert 0's to large # and don't plot
% else dev_misc2_met1(i,j)= dev_misc2_met(i,j);
% end
% if iev_misc2_met(i,j)==0 iev_misc2_met1(i,j)=  0; % convert 0's to large # and don't plot
% else iev_misc2_met1(i,j)= iev_misc2_met(i,j);
% end
% if cevr_misc2_met(i,j)==0 cevr_misc2_met1(i,j)= 0;
% else cevr_misc2_met1(i,j)= cevr_misc2_met(i,j);
% end
% end
%  end
%  
%  
%  
% 
%  for(i=1:size(cev_misc2_val,1))
% for(j=1:FitTrls)
% if cev_misc2_val(i,j)==0 cev_misc2_val1(i,j)= 0;
% else cev_misc2_val1(i,j)= cev_misc2_val(i,j); 
% end
% if dev_misc2_val(i,j)==0 dev_misc2_val1(i,j)=  0; % convert 0's to large # and don't plot
% else dev_misc2_val1(i,j)= dev_misc2_val(i,j);
% end
% if iev_misc2_val(i,j)==0 iev_misc2_val1(i,j)=  0; % convert 0's to large # and don't plot
% else iev_misc2_val1(i,j)= iev_misc2_val(i,j);
% end
% if cevr_misc2_val(i,j)==0 cevr_misc2_val1(i,j)= 0;
% else cevr_misc2_val1(i,j)= cevr_misc2_val(i,j);
% end
% end
% end
 
if generative ==0

 set(figure(5), 'color', 'white'); 
clf;
 figure(5)
hold off

exp_trls=1:FitTrls; % number of trials at beginning of each block to plot exploration 

 

for(i=1:size(cev_misc2_val,1)) 
if(sum(cev_misc2_val(i,:))~=0)
scatter(cev_misc2_val(i,exp_trls),rtcev_val_diff(i,exp_trls),'k')
end
hold on
if(sum(cevr_misc2_val(i,:))~=0)
scatter(cevr_misc2_val(i,exp_trls),rtcevr_val_diff(i,exp_trls),'k')
end
if(sum(dev_misc2_val(i,:))~=0)
scatter(dev_misc2_val(i,exp_trls),rtdev_val_diff(i,exp_trls),'k')
end
if(sum(iev_misc2_val(i,:))~=0)
scatter(iev_misc2_val(i,exp_trls),rtiev_val_diff(i,exp_trls),'k')
end
 end

  
Title('Exploration, All Subs','FontSize', 32)
 xlabel('Model Explore (ms)',  'FontSize', 24)
 ylabel('RT Diff (ms)', 'FontSize', 24)
 set(gca, 'Box', 'off' ); 
 %axis([-550 550 -5000 5000])
 axis([-600 600 -5000 5000])
 saveas(gcf, 'Explore_scatter.epsc')
 saveas(gcf, 'Explore_scatter.jpg')
 saveas(gcf, 'Explore_scatter.fig')
 
%  hold off
% 
%  set(figure(6), 'color', 'white');
%  clf;
%  figure(6)
%  hold off
% 
%  for(i=1:size(cev_misc2_met,1))
%      if(sum(cev_misc2_met(i,:))~=0)
%          scatter(cev_misc2_met(i,exp_trls),cev_misc1_met(i,exp_trls),'m')
%      end
%      hold on
%      if(sum(cevr_misc2_met(i,:))~=0)
%          scatter(cevr_misc2_met(i,exp_trls),cev_misc1_met(i,exp_trls),'m')
%      end
%      if(sum(dev_misc2_met(i,:))~=0)
%          scatter(dev_misc2_met(i,exp_trls),cev_misc1_met(i,exp_trls),'m')
%      end
%      if(sum(iev_misc2_met(i,:))~=0)
%          scatter(iev_misc2_met(i,exp_trls),cev_misc1_met(i,exp_trls),'m')
%      end
%  end
% 
% 
%  for(i=1:size(cev_misc2_val,1))
%      if(sum(cev_misc2_val(i,:))~=0)
%          scatter(cev_misc2_val(i,exp_trls),cev_misc1_val(i,exp_trls),'k')
%      end
%      hold on
%      if(sum(cevr_misc2_val(i,:))~=0)
%          scatter(cevr_misc2_val(i,exp_trls),cev_misc1_val(i,exp_trls),'k')
%      end
%      if(sum(dev_misc2_val(i,:))~=0)
%          scatter(dev_misc2_val(i,exp_trls),cev_misc1_val(i,exp_trls),'k')
%      end
%      if(sum(iev_misc2_val(i,:))~=0)
%          scatter(iev_misc2_val(i,exp_trls),cev_misc1_val(i,exp_trls),'k')
%      end
%  end
% 
% 
%  Title('Exploration, All Subs','FontSize', 32)
%  xlabel('Model Explore (ms)',  'FontSize', 24)
%  ylabel('Actual Explore (ms)', 'FontSize', 24)
%  set(gca, 'Box', 'off' );
%  axis([-530 530 -5000 5000]) 
%  hold off
% 
% 
 end
 

