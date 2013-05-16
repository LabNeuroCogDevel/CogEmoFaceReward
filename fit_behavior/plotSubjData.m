function plotSubjData(subjid, ret_all, model)

global trialLength;
global rewFuncNames;
global emoNames;
% Plot single subject model and data

blocks = [ret_all.block];

%set(figure(1), 'color', 'white');
%hold off

%% plot each block separately
for b = 1:length(blocks)

    ntrials=length(ret_all(b).rtobs);
    figure;
    plot(1:ntrials, smooth(ret_all(b).rtpred,1),'--', ... %% smooth with window of 1? thats no smoothing at all.
        1:ntrials, smooth(ret_all(b).rtobs,1),'-', ...
        'LineWidth', 2);
    
    axis([0 ntrials 0 trialLength]);
    title(['Subject: ' num2str(subjid) ', ' rewFuncNames{ret_all(b).rewFunc} ', block: ', num2str(blocks(b))], 'FontSize', 32);
    legend('Model','Data')
    ylabel('RT (ms)',  'FontSize', 24);
    xlabel('Trial', 'FontSize', 24);
    set(gca, 'Box', 'off' );
    saveas(gcf, ['../outputs/figures/S' num2str(subjid) '_' model '_' rewFuncNames{ret_all(b).rewFunc}  '_block' num2str(blocks(b)) '.jpg']);
end

close all;

%% plot each reward function separatly 

%scrsz = get(0,'ScreenSize');
%h=figure('Position',[1 scrsz(4)/2 800 600]);
set(figure(1), 'color', 'white');
%sort condition
Emo_block = [];
Rew_block = [];
for b = 1:length(blocks)
    Emo_block(b) = squeeze(ret_all(b).emo);
    Rew_block(b) = squeeze(ret_all(b).rewFunc);
end

%Happy_blocks = find(Emo_block==1);
%Fear_blocks = find(Emo_block==2);
%Neutral_blocks = find(Emo_block==3);
%'Position', [0.07, 0.56, 0.4,0.35]

%plot
for b=1:4
    hs=subplot(2,2,b);
    %Happy
    plot(1:ntrials, smooth(ret_all(Emo_block==1&Rew_block==b).rtpred,1),'-g','LineWidth', 2);
    hold on
    %Fear
    plot(1:ntrials, smooth(ret_all(Emo_block==2&Rew_block==b).rtpred,1),'-r','LineWidth', 2);
    %Neutral
    plot(1:ntrials, smooth(ret_all(Emo_block==3&Rew_block==b).rtpred,1),'-b','LineWidth', 2);
    
    axis([0 ntrials 0 trialLength]);
    title([   rewFuncNames{ret_all(b).rewFunc}  ], 'FontSize', 14);
    if b==4
        legend('Happy','Fear','Neutral','location','Best');
    end
    if b ==1 || b==3
        ylabel('RT (ms)',  'FontSize', 12);
    end
    if b ==3 || b==4
        xlabel('Trial', 'FontSize', 12);
    end
    set(gca, 'Box', 'off','FontSize',12 );
    hold off
    %set(gca, 'LooseInset', get(gca,'TightInset'));
end
mtit(strcat('Subject: ',num2str(subjid)),'xoff',0,'yoff',+0.02,'fontsize',16,'fontweight','bold');
print(gcf, ['../outputs/figures/S' num2str(subjid) '_' model '_' 'RewardByEmotion'], '-djpeg100  ', '-r300')
close all;

%% plot exploration parameter


for b = 1:length(blocks)

    ntrials=length(ret_all(b).rtobs);
    figure;
    plot(2:ntrials-1, 20*(ret_all(b).explore(2:ntrials-1)),'-k', ... % *20 is from MF
        2:ntrials-1, ret_all(b).rtobs(2:ntrials-1)-ret_all(b).rtobs(1:ntrials-2),'-b', ...
        'LineWidth', 2);
    
    axis([0 ntrials -trialLength trialLength]);
    title(['Subject: ' num2str(subjid) ', ' rewFuncNames{ret_all(b).rewFunc} ', ' emoNames{ret_all(b).emo} ], 'FontSize', 32);
    legend('Exploration Param','RT Swing')
    ylabel('RT (ms)',  'FontSize', 24);
    xlabel('Trial', 'FontSize', 24);
    set(gca, 'Box', 'off' );
    saveas(gcf, ['../outputs/figures/S' num2str(subjid) '_' model '_' rewFuncNames{ret_all(b).rewFunc}  '_' emoNames{ret_all(b).emo} '_RTSwing.jpg']);
end

close all;


end


% 
% %% Plot CEV, DEV, IEV model & data on one graph for each genotyype
% 
% cev_Pred = [rtCEVpred_met];  % everyone is met when no DNA avail!
% cev_RT = [rtcev_met];
% iev_Pred = [rtIEVpred_met];
% iev_RT = [rtiev_met];
% 
% cevr_Pred = [rtCEVRpred_met];
% cevr_RT = [rtcevr_met];
% 
% dev_Pred = [rtDEVpred_met];
% dev_RT = [rtdev_met];
% 
% for (i=1:size(cev_RT,1))
%     for (j=1:size(cev_RT,2))
%         if (cev_RT(i,j) == 0) cev_RT(i,j)= nan; % temporarily set zeros to nan
%             cev_RT(i,j)=nan;
%         end
%     end
% end
% notNaN = ~isnan(cev_RT); % get indices of non nan values
% howmany = sum(notNaN); % count them
% cev_RT(~notNaN) =0; % set nans back to zeros
% 
% columnTot  = sum(cev_RT); % sum columns
% cev_mn = columnTot ./ howmany; % calc mean per column
% 
% 
% for (i=1:size(dev_RT,1))
%     for (j=1:size(dev_RT,2))
%         if (dev_RT(i,j) == 0) dev_RT(i,j)= nan; % temporarily set zeros to nan
%             dev_RT(i,j)=nan;
%         end
%     end
% end
% notNaN = ~isnan(dev_RT); % get indices of non nan values
% howmany = sum(notNaN); % count them
% dev_RT(~notNaN) =0; % set nans back to zeros
% 
% columnTot  = sum(dev_RT); % sum columns
% dev_mn = columnTot ./ howmany; % calc mean per column
% 
% 
% for (i=1:size(iev_RT,1))
%     for (j=1:size(iev_RT,2))
%         if (iev_RT(i,j) == 0) iev_RT(i,j)= nan; % temporarily set zeros to nan
%             iev_RT(i,j)=nan;
%         end
%     end
% end
% notNaN = ~isnan(iev_RT); % get indices of non nan values
% howmany = sum(notNaN); % count them
% iev_RT(~notNaN) =0; % set nans back to zeros
% 
% columnTot  = sum(iev_RT); % sum columns
% iev_mn = columnTot ./ howmany; % calc mean per column
% 
% 
% for (i=1:size(cevr_RT,1))
%     for (j=1:size(cevr_RT,2))
%         if (cevr_RT(i,j) == 0) cevr_RT(i,j)= nan; % temporarily set zeros to nan
%             cevr_RT(i,j)=nan;
%         end
%     end
% end
% notNaN = ~isnan(cevr_RT); % get indices of non nan values
% howmany = sum(notNaN); % count them
% cevr_RT(~notNaN) =0; % set nans back to zeros
% 
% columnTot  = sum(cevr_RT); % sum columns
% cevr_mn = columnTot ./ howmany; % calc mean per column
% 
% hold off;
% plot( 1:3*FitTrls,smooth(iev_mn(1:3*FitTrls),10,'lowess'),'r-'  ,1:3*FitTrls,smooth(cevr_mn(1:3*FitTrls),10,'lowess'),'m-', 1:3*FitTrls,smooth(dev_mn(1:3*FitTrls),10,'lowess'),'LineWidth',2)
% axis([0 3*FitTrls 1000 3000])
% title('All Subjects: Data','FontSize', 32 )
% legend('IEV','CEVR','DEV'); legend BOXOFF
% legend BOXOFF
% %legend('Location','SouthWest')
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24)
% set(gca, 'Box', 'off' );
% saveas(gcf, 'AllConds_Data.epsc')
% saveas(gcf, 'AllConds_Data.jpg')
% saveas(gcf, 'AllConds_Data.fig')
% 
% figure(2)
% set(figure(2), 'color', 'white');
% 
% iev_mn1 = (iev_mn(1:FitTrls) + iev_mn(FitTrls+1:2*FitTrls) + iev_mn(2*FitTrls+1:end))/3 
% %cev_mn1 = (cev_mn(1:FitTrls) + cev_mn(FitTrls+1:2*FitTrls) + cev_mn(2*FitTrls+1:end))/3;
% dev_mn1 = (dev_mn(1:FitTrls) + dev_mn(FitTrls+1:2*FitTrls) + dev_mn(2*FitTrls+1:end))/3;
% cevr_mn1 = (cevr_mn(1:FitTrls) + cevr_mn(FitTrls+1:2*FitTrls) + cevr_mn(2*FitTrls+1:end))/3;
%  
% 
% 
% n = 10; % smooth by n
% 
% plot( 1:FitTrls,smooth(iev_mn1(1:FitTrls),n,'lowess'),'r-'  ,1:FitTrls,smooth(cevr_mn1(1:FitTrls),n,'lowess'),'m-',1:FitTrls,smooth(dev_mn1(1:FitTrls),n,'lowess'),'LineWidth',2) 
% axis([0 FitTrls 1000 3000])
% title('All Subjects: Data','FontSize', 32 )
% legend('IEV','CEVR','DEV'); legend BOXOFF
% legend BOXOFF
% %legend('Location','SouthWest')
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'AllConds_Data1.epsc')
% saveas(gcf, 'AllConds_Data1.jpg')
% saveas(gcf, 'AllConds_Data1.fig')
% 
% for (i=1:size(cev_Pred,1))
%     for (j=1:size(cev_Pred,2))
%         if (cev_Pred(i,j) == 0) cev_Pred(i,j)= nan; % temporarily set zeros to nan
%             cev_Pred(i,j)=nan;
%         end
%     end
% end
% notNaN = ~isnan(cev_Pred); % get indices of non nan values
% howmany = sum(notNaN); % count them
% cev_Pred(~notNaN) =0; % set nans back to zeros
% 
% columnTot  = sum(cev_Pred); % sum columns
% cevPred_mn = columnTot ./ howmany; % calc mean per column
% 
% 
% for (i=1:size(dev_Pred,1))
%     for (j=1:size(dev_Pred,2))
%         if (dev_Pred(i,j) == 0) dev_Pred(i,j)= nan; % temporarily set zeros to nan
%             dev_Pred(i,j)=nan;
%         end
%     end
% end
% notNaN = ~isnan(dev_Pred); % get indices of non nan values
% howmany = sum(notNaN); % count them
% dev_Pred(~notNaN) =0; % set nans back to zeros
% 
% columnTot  = sum(dev_Pred); % sum columns
% devPred_mn = columnTot ./ howmany; % calc mean per column
% 
% 
% for (i=1:size(iev_Pred,1))
%     for (j=1:size(iev_Pred,2))
%         if (iev_Pred(i,j) == 0) iev_Pred(i,j)= nan; % temporarily set zeros to nan
%             iev_Pred(i,j)=nan;
%         end
%     end
% end
% notNaN = ~isnan(iev_Pred); % get indices of non nan values
% howmany = sum(notNaN); % count them
% iev_Pred(~notNaN) =0; % set nans back to zeros
% 
% columnTot  = sum(iev_Pred); % sum columns
% ievPred_mn = columnTot ./ howmany; % calc mean per column
% 
% 
% for (i=1:size(cevr_Pred,1))
%     for (j=1:size(cevr_Pred,2))
%         if (cevr_Pred(i,j) == 0) cevr_Pred(i,j)= nan; % temporarily set zeros to nan
%             cevr_Pred(i,j)=nan;
%         end
%     end
% end
% notNaN = ~isnan(cevr_Pred); % get indices of non nan values
% howmany = sum(notNaN); % count them
% cevr_Pred(~notNaN) =0; % set nans back to zeros
% 
% columnTot  = sum(cevr_Pred); % sum columns
% cevrPred_mn = columnTot ./ howmany; % calc mean per column
% 
% 
% 
% 
% plot(1:3*FitTrls,smooth(ievPred_mn(1:3*FitTrls),10,'lowess'),'r--',  1:3*FitTrls,smooth(cevrPred_mn(1:3*FitTrls),10,'lowess'),'m--',  1:3*FitTrls,smooth(devPred_mn(1:3*FitTrls),10,'lowess'),'b--','LineWidth',2)
% title('All Subjects: Model Fits','FontSize', 32)
% axis([0 3*FitTrls 1000 3000])
% legend('IEV','CEVR','DEV'); legend BOXOFF
% legend BOXOFF
% 
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24)
% set(gca, 'Box', 'off' );
% saveas(gcf, 'AllConds_model.epsc')
% saveas(gcf, 'AllConds_model.jpg')
% saveas(gcf, 'AllConds_model.fig')
% 
% %
% 
% figure(2)
%  
% 
% 
% ievPred_mn1 = (ievPred_mn(1:FitTrls) + ievPred_mn(FitTrls+1:2*FitTrls) + ievPred_mn(2*FitTrls+1:end))/3 
% %cevPred_mn1 = (cevPred_mn(1:FitTrls) + cevPred_mn(FitTrls+1:2*FitTrls) + cevPred_mn(2*FitTrls+1:end))/3;
% devPred_mn1 = (devPred_mn(1:FitTrls) + devPred_mn(FitTrls+1:2*FitTrls) + devPred_mn(2*FitTrls+1:end))/3;
% cevrPred_mn1 = (cevrPred_mn(1:FitTrls) + cevrPred_mn(FitTrls+1:2*FitTrls) + cevrPred_mn(2*FitTrls+1:end))/3;
%  
% 
% n = 10; % smooth by n
%  
% plot( 1:FitTrls,smooth(ievPred_mn1(1:FitTrls),n,'lowess'),'r-'  ,1:FitTrls,smooth(cevrPred_mn1(1:FitTrls),n,'lowess'),'m-',1:FitTrls,smooth(devPred_mn1(1:FitTrls),n,'lowess'),'LineWidth',2) 
% axis([0 FitTrls 1000 3000])
% title('All Subjects: Model Fits','FontSize', 32 )
% legend('IEV','CEVR','DEV'); legend BOXOFF
% legend BOXOFF
% %legend('Location','SouthWest')
% ylabel('RT (ms)',  'FontSize', 24)
% xlabel('Trial', 'FontSize', 24)
% set(gca, 'Box', 'off' ); 
% saveas(gcf, 'AllConds_Model1.epsc')
% saveas(gcf, 'AllConds_Model1.jpg')
% saveas(gcf, 'AllConds_Model1.fig')
% 
% cev_PE =[cev_misc1_met];
% cevr_PE =[cevr_misc1_met];
% dev_PE =[dev_misc1_met];
% iev_PE =[iev_misc1_met];
% 
% rtdev = [rtdev_met];
% rtcev = [rtcev_met];
% rtiev = [rtiev_met];
% rtcevr = [rtcevr_met];
% 
% n_PEtrls = 25; % only look at first n trials when EV's aren't quite learned
% % (otherwise PE's end up getting smaller in place where rt's converge..)
% rtdev1 = nonzeros(rtdev(:,1:n_PEtrls))'; % filter out non-RTs
% %rtcev1 = nonzeros(rtcev(:,1:n_PEtrls))';
% rtiev1 = nonzeros(rtiev(:,1:n_PEtrls))';
% rtcevr1 = nonzeros(rtcevr(:,1:n_PEtrls))';
% 
% dev_PE1 = dev_PE(find(nonzeros(rtdev(:,1:n_PEtrls)))); % get corresponding PE's
% %cev_PE1 = cev_PE(find(nonzeros(rtcev(:,1:n_PEtrls))));
% iev_PE1 = iev_PE(find(nonzeros(rtiev(:,1:n_PEtrls))));
% cevr_PE1 = cevr_PE(find(nonzeros(rtcevr(:,1:n_PEtrls))));
% 
% y_dev = [rtdev1' dev_PE1];
% y_dev=sortrows(y_dev,1); % sort by increasing rt's
% 
% % y_cev = [rtcev1' cev_PE1];
% % y_cev=sortrows(y_cev,1); % sort by increasing rt's
% 
% y_iev = [rtiev1' iev_PE1];
% y_iev=sortrows(y_iev,1); % sort by increasing rt's
% 
% y_cevr = [rtcevr1' cevr_PE1];
% y_cevr=sortrows(y_cevr,1); % sort by increasing rt's
% 
% n=1000;
% 
% figure(1)
% plot(y_dev(:,1),smooth(y_dev(:,2),n,'lowess'),'g','Linewidth',2);
% axis([0 5000 -10 10])
% title('DEV Prediction Errors','FontSize', 32);
% xlabel('RT (ms)',  'FontSize', 24)
% ylabel('Reward Prediction Error', 'FontSize', 24)
% set(gca, 'Box', 'off' );
% saveas(gcf, 'DEV_PE_25trl.epsc')
% saveas(gcf, 'DEV_PE_25trl.jpg')
% % 
% % figure(2)
% % set(figure(2), 'color', 'white');
% % hold off
% % plot(y_cev(:,1),smooth(y_cev(:,2),n,'lowess'),'k','Linewidth',2);
% % axis([0 5000 -10 15])
% % title('CEV Prediction Errors','FontSize', 32);
% % xlabel('RT (ms)',  'FontSize', 24)
% % ylabel('Reward Prediction Error', 'FontSize', 24)
% % set(gca, 'Box', 'off' );
% % saveas(gcf, 'CEV_PE_25trl.epsc')
% % saveas(gcf, 'CEV_PE_25trl.jpg')
% 
% figure(3)
% set(figure(3), 'color', 'white');
% hold off
% plot(y_iev(:,1),smooth(y_iev(:,2),n,'lowess'),'r','Linewidth',2);
% axis([0 5000 -10 10])
% title('IEV Prediction Errors','FontSize', 32);
% xlabel('RT (ms)',  'FontSize', 24)
% ylabel('Reward Prediction Error', 'FontSize', 24)
% set(gca, 'Box', 'off' );
% saveas(gcf, 'IEV_PE_25trl.epsc')
% saveas(gcf, 'IEV_PE_25trl.jpg')
% 
% figure(4)
% set(figure(4), 'color', 'white');
% hold off
% plot(y_cevr(:,1),smooth(y_cevr(:,2),n,'lowess'),'c','Linewidth',2);
% axis([0 5000 -15 10])
% title('CEVR Prediction Errors','FontSize', 32);
% xlabel('RT (ms)',  'FontSize', 24)
% ylabel('Reward Prediction Error', 'FontSize', 24)
% set(gca, 'Box', 'off' );
% saveas(gcf, 'CEVR_PE_25trl.epsc')
% saveas(gcf, 'CEVR_PE_25trl.jpg')
% 
% i=1;
% 
% 
% figure(1)
% hold off;

%% here is where MF plot exploration
% 
% plot(2: FitTrls-1,20*smooth(iev_misc2_met(i,2:FitTrls-1),1),2:FitTrls-1,smooth(rtiev_met(i,2:FitTrls-1)-rtiev_met(i,1:FitTrls-2),1),'LineWidth',2)
% axis([1 FitTrls -4000 4000])
% title('Exploration','FontSize', 32)
% 
% xlabel('Trial',  'FontSize', 24)
% ylabel('RT Diff (ms)', 'FontSize', 24)
% legend('Model Exp term', 'RT diff'); legend('Location','NorthEast'); %legend BOXOFF;
% set(gca, 'Box', 'off' );
% set(0,'defaulttextcolor','r');
% text(30,-3400,'Single Subject, IEV','HorizontalAlignment','center')
% set(0,'defaulttextcolor','k');
% set(0,'defaulttextfontsize',16)
% saveas(gcf, 'Explore_IEV_S1.epsc')
% saveas(gcf, 'Explore_IEV_S1.jpg')
% saveas(gcf, 'Explore_IEV_S1.fig')
% 

% for i=1:size(cevr_misc2_met,1) corr_exp_met(i) = corr(cevr_misc2_met(i,2:50)',rtcevr_met(i,2:50)'-rtcevr_met(i,1:49)');
%     if isnan(corr_exp_met(i)) corr_exp_met(i)=0;
%     end
% end
% mean(nonzeros(corr_exp_met))
% 
% 
% %rtcev_met_diff(:,2:FitTrls)=rtcev_met(:,2:FitTrls)-rtcev_met(:,1:FitTrls-1);
% rtdev_met_diff(:,2:3*FitTrls)=rtdev_met(:,2:3*FitTrls)-rtdev_met(:,1:3*FitTrls-1);
% rtiev_met_diff(:,2:3*FitTrls)=rtiev_met(:,2:3*FitTrls)-rtiev_met(:,1:3*FitTrls-1);
% rtcevr_met_diff(:,2:3*FitTrls)=rtcevr_met(:,2:3*FitTrls)-rtcevr_met(:,1:3*FitTrls-1);
% 
% 
% misc_met = [ cevr_misc2_met(:,2:3*FitTrls) dev_misc2_met(:,2:3*FitTrls) iev_misc2_met(:,2:3*FitTrls)];
% 
% rt_met_diff = [ rtcevr_met_diff(:,2:3*FitTrls) rtdev_met_diff(:,2:3*FitTrls) rtiev_met_diff(:,2:3*FitTrls)];
% 
% 
% 
% for i=1:size(cevr_misc2_met,1) corr_exp_met(i) = corr(misc_met(i,2:3*FitTrls)',rt_met_diff(i,2:3*FitTrls)');
%     if isnan(corr_exp_met(i)) corr_exp_met(i)=0;
%     end
% end
% 
% 
% mean(nonzeros(corr_exp_met))
% 
%  
% 
% 
% %% plot scatter between predicted and actual explores for mets with nonzero
% %% explore param
% 
% %rtcev_met_diff(:,2:3*FitTrls)=rtcev_met(:,2:3*FitTrls)-rtcev_met(:,1:3*FitTrls-1);
% rtdev_met_diff(:,2:3*FitTrls)=rtdev_met(:,2:3*FitTrls)-rtdev_met(:,1:3*FitTrls-1);
% rtiev_met_diff(:,2:3*FitTrls)=rtiev_met(:,2:3*FitTrls)-rtiev_met(:,1:3*FitTrls-1);
% rtcevr_met_diff(:,2:3*FitTrls)=rtcevr_met(:,2:3*FitTrls)-rtcevr_met(:,1:3*FitTrls-1);
% uncdiff = [ (cevr_misc4_met(:,2:3*FitTrls) - cevr_misc3_met(:,2:3*FitTrls)) (dev_misc4_met(:,2:3*FitTrls) - dev_misc3_met(:,2:3*FitTrls)) (iev_misc4_met(:,2:3*FitTrls) - iev_misc3_met(:,2:3*FitTrls))];
% 
% misc_met = [ cevr_misc2_met(:,2:3*FitTrls) dev_misc2_met(:,2:3*FitTrls) iev_misc2_met(:,2:3*FitTrls)];
% rt_met_diff = [ rtcevr_met_diff(:,2:3*FitTrls) rtdev_met_diff(:,2:3*FitTrls) rtiev_met_diff(:,2:3*FitTrls)];
% 
% for i=1:size(cevr_misc2_met,1) corr_exp_met(i) = corr(misc_met(i,2:3*FitTrls)',rt_met_diff(i,2:3*FitTrls)');
%     if isnan(corr_exp_met(i)) corr_exp_met(i)=0;
%     end
% end
% 
% %cevr_explore_corr = mean(nonzeros(corr_exp_met))
% 
% 
% 
% hold off
% figure
% n=357; corr_exp_met=[]; reg=[];
% for i=1:size(cevr_misc2_met,1)
%     eps(i) =  Best_fit_params_Trn(i,3);
%     if eps(i)>0
%         corr_exp_met(i) = corr(misc_met(i,2:n)',rt_met_diff(i,2:n)');%','type','Spearman');
%         reg(i)= regress(zscore(misc_met(i,find(misc_met(i,:)~=0))'),zscore(rt_met_diff(i,find(misc_met(i,:)~=0))'));
%         
%         %get regression line not including trials in which exp was reset to 0
%         
%         scatter(zscore(misc_met(i,find(misc_met(i,:)~=0))),zscore(rt_met_diff(i,find(misc_met(i,:)~=0))));
%         
%         hold on;
%         if isnan(corr_exp_met(i)) corr_exp_met(i)=0;
%         end
%     end
% end
% x=-10:10;
% plot(x,x*mean(nonzeros(reg)),'k') % plot regression line
% 
% title('RT swings and relative uncertainty, Explorers')
% xlabel('Standardized relative uncertainty z(\sigma_{diff})')
% ylabel('Standardized RT swing z(RT_{diff})')
% set(gcf, 'color', 'white');
% axis([-3 3 -3.5 3.5])
% saveas(gcf, 'Explorer_scatter.epsc')
% saveas(gcf, 'Explorer_scatter.jpg')
% saveas(gcf, 'Explorer_scatter.fig')
% 
% %all_explore_corr =mean(nonzeros(corr_exp_met))
% %[h,p]=ttest(nonzeros(corr_exp_met))
