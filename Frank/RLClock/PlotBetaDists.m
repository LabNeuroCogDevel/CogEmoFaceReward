set(figure(10), 'color', 'white'); 
set(figure(11), 'color', 'white'); 
set(figure(12), 'color', 'white'); 
set(figure(13), 'color', 'white'); 

 %% this function plots  beta distributions for fast and slow responses
 %% (not run automatically)
 %% to use set misc1 to alph_short and misc2 to b_short in TC_Alg
 % run once then set them to long and uncomment long code below, add to
 % plots...
 %% after do this once can just "load cev_alph_short" etc.
 
 
 cev_alph_short=[cev_misc1_met; cev_misc1_val];
 cev_beta_short=[cev_misc2_met; cev_misc2_val];
 dev_alph_short=[dev_misc1_met; dev_misc1_val];
 dev_beta_short=[dev_misc2_met; dev_misc2_val];
 cevr_alph_short=[cevr_misc1_met; cevr_misc1_val];
 cevr_beta_short=[cevr_misc2_met; cevr_misc2_val];
 iev_alph_short=[iev_misc1_met; iev_misc1_val];
 iev_beta_short=[iev_misc2_met; iev_misc2_val];
 save cev_alph_short;
 save cev_beta_short;
 save dev_alph_short;
 save dev_beta_short;
 save cevr_alph_short;
 save cevr_beta_short;
 save iev_alph_short;
 save iev_beta_short;



 % %
 cev_alph_long=[cev_misc1_met; cev_misc1_val];
 cev_beta_long=[cev_misc2_met; cev_misc2_val];
 dev_alph_long=[dev_misc1_met; dev_misc1_val];
 dev_beta_long=[dev_misc2_met; dev_misc2_val];
 cevr_alph_long=[cevr_misc1_met; cevr_misc1_val];
 cevr_beta_long=[cevr_misc2_met; cevr_misc2_val];
 iev_alph_long=[iev_misc1_met; iev_misc1_val];
 iev_beta_long=[iev_misc2_met; iev_misc2_val];
 save cev_alph_long;
 save cev_beta_long;
 save dev_alph_long;
 save dev_beta_long;
 save cevr_alph_long;
 save cevr_beta_long;
 save iev_alph_long;
 save iev_beta_long;
 %

 load cev_alph_long;
 load cev_beta_long;
 load dev_alph_long;
 load dev_beta_long;
 load cevr_alph_long;
 load cevr_beta_long;
 load iev_alph_long;
 load iev_beta_long;
 %


 figure(10)
 set(figure(10), 'color', 'white');
 hold off;

 y =betadist(mean(dev_alph_short(:,47)), mean(dev_beta_short(:,47))); % after 47 trials
 plot(0.01:.01:1,y,'g','LineWidth',2.5);
 hold on;

 y =betadist(mean(dev_alph_long(:,47)), mean(dev_beta_long(:,47))); % after 47 trials
 plot(0.01:.01:1,y,'r','LineWidth',2.5);

 legend('Fast','Slow');
 legend('Location','Best');

 y =betadist(mean(dev_alph_short(:,25)), mean(dev_beta_short(:,25))); % after 25 trials
 plot(0.01:.01:1,y,'g-.','LineWidth',2.5);
 y =betadist(mean(dev_alph_long(:,25)), mean(dev_beta_long(:,25))); % after 25 trials
 plot(0.01:.01:1,y,'r-.','LineWidth',2.5);
 y =betadist(mean(dev_alph_short(:,1)), mean(dev_beta_short(:,1))); % first trial
 plot(0.01:.01:1,y,'g:','LineWidth',2.5);
 y =betadist(mean(dev_alph_long(:,1)), mean(dev_beta_long(:,1))); % first trial
 plot(0.01:.01:1,y,'r:','LineWidth',3.5);
 
 xlabel('p(\delta_{s,a} > 0)', 'FontSize', 22)
 ylabel('', 'FontSize', 24)

 Title('DEV Beta Distributions','FontSize', 32);

 set(0,'defaultaxesfontsize',16);

 set(gca, 'Box', 'off' );
 saveas(gcf, 'DEV_beta.epsc')
 saveas(gcf, 'DEV_beta.fig')


 figure(11)
 set(figure(11), 'color', 'white');
 hold off;

 y =betadist(mean(iev_alph_short(:,47)), mean(iev_beta_short(:,47))); % after 47 trials
 plot(0.01:.01:1,y,'g','LineWidth',2.5);
 hold on;

 y =betadist(mean(iev_alph_long(:,47)), mean(iev_beta_long(:,47))); % after 47 trials
 plot(0.01:.01:1,y,'r','LineWidth',2.5);
 legend('Fast','Slow');
 legend('Location','Best');
 y =betadist(mean(iev_alph_short(:,25)), mean(iev_beta_short(:,25))); % after 25 trials
 plot(0.01:.01:1,y,'g-.','LineWidth',2.5);
 y =betadist(mean(iev_alph_long(:,25)), mean(iev_beta_long(:,25))); % after 25 trials
 plot(0.01:.01:1,y,'r-.','LineWidth',2.5);
 y =betadist(mean(iev_alph_short(:,1)), mean(iev_beta_short(:,1))); % first trial
 plot(0.01:.01:1,y,'g:','LineWidth',2.5);

 y =betadist(mean(iev_alph_long(:,1)), mean(iev_beta_long(:,1))); % first trial
 plot(0.01:.01:1,y,'r:','LineWidth',3.5);

 Title('IEV Beta Distributions','FontSize', 32);

 set(0,'defaultaxesfontsize',16);
 xlabel('p(\delta_{s,a} > 0)', 'FontSize', 22)  
 ylabel('P(\delta_{s,a} = x)', 'FontSize', 24)

 set(gca, 'Box', 'off' );
 saveas(gcf, 'IEV_beta.epsc')
 saveas(gcf, 'IEV_beta.fig')


 figure(12)
 set(figure(12), 'color', 'white');
 hold off;

 y =betadist(mean(cev_alph_short(:,47)), mean(cev_beta_short(:,47))); % after 47 trials
 plot(0.01:.01:1,y,'g','LineWidth',2.5);
 hold on;

 y =betadist(mean(cev_alph_long(:,47)), mean(cev_beta_long(:,47))); % after 47 trials
 plot(0.01:.01:1,y,'r','LineWidth',2.5);
 legend('Fast','Slow');
 legend('Location','Best');
 y =betadist(mean(cev_alph_short(:,25)), mean(cev_beta_short(:,25))); % after 25 trials
 plot(0.01:.01:1,y,'g-.','LineWidth',2.5);
 y =betadist(mean(cev_alph_long(:,25)), mean(cev_beta_long(:,25))); % after 25 trials
 plot(0.01:.01:1,y,'r-.','LineWidth',2.5);
 y =betadist(mean(cev_alph_short(:,1)), mean(cev_beta_short(:,1))); % first trial
 plot(0.01:.01:1,y,'g:','LineWidth',2.5);

 y =betadist(mean(cev_alph_long(:,1)), mean(cev_beta_long(:,1))); % first trial
 plot(0.01:.01:1,y,'r:','LineWidth',3.5);

 Title('CEV Beta Distributions','FontSize', 32);

 set(0,'defaultaxesfontsize',16);
 xlabel('p(\delta_{s,a} > 0)', 'FontSize', 22)
 ylabel('', 'FontSize', 24)
 set(gca, 'Box', 'off' );
 saveas(gcf, 'CEV_beta.epsc')
 saveas(gcf, 'CEV_beta.fig')

 figure(13)
 set(figure(13), 'color', 'white');
 hold off;

 y =betadist(mean(cevr_alph_short(:,47)), mean(cevr_beta_short(:,47))); % after 47 trials
 plot(0.01:.01:1,y,'g','LineWidth',2.5);
 hold on;

 y =betadist(mean(cevr_alph_long(:,47)), mean(cevr_beta_long(:,47))); % after 47 trials
 plot(0.01:.01:1,y,'r','LineWidth',2.5);
 legend('Fast','Slow');
 legend('Location','Best');
 y =betadist(mean(cevr_alph_short(:,25)), mean(cevr_beta_short(:,25))); % after 25 trials
 plot(0.01:.01:1,y,'g-.','LineWidth',2.5);
 y =betadist(mean(cevr_alph_long(:,25)), mean(cevr_beta_long(:,25))); % after 25 trials
 plot(0.01:.01:1,y,'r-.','LineWidth',2.5);
 y =betadist(mean(cevr_alph_short(:,1)), mean(cevr_beta_short(:,1))); % first trial
 plot(0.01:.01:1,y,'g:','LineWidth',2.5);

 y =betadist(mean(cevr_alph_long(:,1)), mean(cevr_beta_long(:,1))); % first trial
 plot(0.01:.01:1,y,'r:','LineWidth',3.5);

 xlabel('p(\delta_{s,a} > 0)', 'FontSize', 22)
 ylabel('', 'FontSize', 24)

 Title('CEVR Beta Distributions','FontSize', 32);
 set(0,'defaultaxesfontsize',16);
 
   
 set(gca, 'Box', 'off' );
 saveas(gcf, 'CEVR_beta.epsc')
 saveas(gcf, 'CEVR_beta.fig')











 %
 % figure(11)
 % hold on;
 %  y =betadist(mean(cev_alph_long(:,1)), mean(cev_beta_long(:,1))); % first trial
 % plot(0.01:.01:1,y,'r:','LineWidth',2.5);
 % hold on;
 % y =betadist(mean(cev_alph_long(:,25)), mean(cev_beta_long(:,25))); % after 25 trials
 % plot(0.01:.01:1,y,'r-.','LineWidth',2.5);
 % y =betadist(mean(cev_alph_long(:,47)), mean(cev_beta_long(:,47))); % after 47 trials
 % plot(0.01:.01:1,y,'r','LineWidth',2.5);
 % set(0,'defaultaxesfontsize',20);
 % Title('CEV Beta Distributions','FontSize', 32);
 % legend('Fast t=1','Fast t=25', 'Fast t=50', 'Slow t=1','Slow t=25', 'Slow t=50', ) %,'CEVR Model','CEVR Data')
 % legend('Location','Best')
 % %legend BOXOFF
 % xlabel('p(\delta_{s,a} > 0)', 'FontSize', 22)  
 % ylabel('P(\delta_{s,a} = x)', 'FontSize', 24)
 % set(gca, 'Box', 'off' );
 % saveas(gcf, 'CEV_beta.epsc')
 % saveas(gcf, 'CEV_beta.fig')
 % %




 


 %% makes different fig for each trial, single subject - can then make
 %% movies in anim directory with jpeg_to_mpeg.sh

 % set(figure(24), 'color', 'white');
 % figure(24)
 % hold off
 % y =betadist(1,1);
 % plot(0.01:.01:1,y+.04,'g',0.01:.01:1,y,'r','LineWidth',2.5);
 % %plot(0.01:.01:1,y,'r','LineWidth',2.5);
 % Title('DEV Beta Distributions','FontSize', 28);
 % text(.5,4.5,'Single Subject','HorizontalAlignment','center')
 % xlabel('p(\delta_{s,a} > 0)', 'FontSize', 22)  
 % ylabel('', 'FontSize', 24)
 % axis([0 .98 0 4.5])
 % legend('Fast','Slow')
 % set(gca, 'Box', 'off' );
 % saveas(gcf, 'DEV_S2_beta24.jpg')
 %
 % %
 % for (j=25:70)
 %
 % set(figure(j), 'color', 'white');
 % figure(j)
 % hold off;
 %  y =betadist(dev_alph_short(2,j-24), dev_beta_short(2,j-24)); % first trial
 % plot(0.01:.01:1,y+.04,'g','LineWidth',2);
 % hold on;
 % y =betadist(dev_alph_long(2,j-24), dev_beta_long(2,j-24));
 % plot(0.01:.01:1,y,'r','LineWidth',2);
 %
 %
 % Title('DEV Beta Distributions','FontSize', 28);
 % text(.5,4.5,'Single Subject','HorizontalAlignment','center')
 %
 % xlabel('p(\delta_{s,a} > 0)', 'FontSize', 22)  
 % ylabel('', 'FontSize', 24)
 % axis([0 .98 0 4.5])
 % legend('Fast','Slow')
 % set(gca, 'Box', 'off' );
 % saveas(gcf, strcat('DEV_S2_beta',num2str(j),'.jpg'))
 %
 % end
 %
 %
 % for (j=24:70)
 % figure(j)
 % close
 % end
 %
 %
 % set(figure(24), 'color', 'white');
 % figure(24)
 % hold off
 % y =betadist(1,1);
 % plot(0.01:.01:1,y+.04,'g',0.01:.01:1,y,'r','LineWidth',2.5);
 % %plot(0.01:.01:1,y,'r','LineWidth',2.5);
 % Title('CEV Beta Distributions','FontSize', 28);
 % text(.5,4.5,'Single Subject','HorizontalAlignment','center')
 % xlabel('p(\delta_{s,a} > 0)', 'FontSize', 22)  
 % ylabel('', 'FontSize', 24)
 % axis([0 .98 0 4.5])
 % legend('Fast','Slow')
 % set(gca, 'Box', 'off' );
 % saveas(gcf, 'CEV_S22_beta24.jpg')
 % %
 % for (j=25:70)
 %
 % set(figure(j), 'color', 'white');
 % figure(j)
 % hold off;
 %
 % y =betadist(cev_alph_short(22,j-24), cev_beta_short(22,j-24)); % first trial
 % plot(0.01:.01:1,y+.04,'g','LineWidth',2);
 % hold on;
 % y =betadist(cev_alph_long(22,j-24), cev_beta_long(22,j-24));
 % plot(0.01:.01:1,y,'r','LineWidth',2);
 %
 % Title('CEV Beta Distributions','FontSize', 28);
 % text(.5,4.5,'Single Subject','HorizontalAlignment','center')
 % xlabel('p(\delta_{s,a} > 0)', 'FontSize', 22)  
 % ylabel('', 'FontSize', 24)
 % axis([0 .98 0 4.5])
 % legend('Fast','Slow')
 % set(gca, 'Box', 'off' );
 % saveas(gcf, strcat('CEV_S22_beta',num2str(j),'.jpg'))
 %
 % end


 %%  this is for single subject showing beta dists for multiple trials on
 %%  one fig

 % set(figure(20), 'color', 'white');
 % figure(20)
 % hold off;
 % y =betadist(1,1);
 % plot(0.01:.01:1,y,'r','LineWidth',2);
 % plot(0.01:.01:1,y,'g','LineWidth',2);
 % hold on;
 % for(i=[1,3,4,10,15,45])
 % y =betadist(cev_alph_long(2,i), cev_beta_long(2,i));
 % plot(0.01:.01:1,y,'r','LineWidth',2);
 %
 % y =betadist(cev_alph_short(2,i), cev_beta_short(2,i)); % first trial
 % plot(0.01:.01:1,y,'g','LineWidth',2);
 % end
 %
 % Title('CEV Beta Distributions, Single Subject','FontSize', 18);
 % xlabel('x',  'FontSize', 16)
 % ylabel('P(\delta_{s,a} = x)', 'FontSize', 16)
 % set(gca, 'Box', 'off' );
 %
 % set(figure(21), 'color', 'white');
 % figure(21)
 % hold off;
 % y =betadist(1,1);
 % plot(0.01:.01:1,y,'r','LineWidth',2);
 % plot(0.01:.01:1,y,'g','LineWidth',2);
 % hold on;
 % for(i=[1,3,4,10,15,45])
 % y =betadist(cevr_alph_long(2,i), cevr_beta_long(2,i));
 % plot(0.01:.01:1,y,'r','LineWidth',2);
 %
 % y =betadist(cevr_alph_short(2,i), cevr_beta_short(2,i)); % first trial
 % plot(0.01:.01:1,y,'g','LineWidth',2);
 % end
 %
 % Title('CEVR Beta Distributions, Single Subject','FontSize', 18);
 % xlabel('x',  'FontSize', 16)
 % ylabel('P(\delta_{s,a} = x)', 'FontSize', 16)
 % set(gca, 'Box', 'off' );
 %
 % set(figure(22), 'color', 'white');
 % figure(22)
 % hold off;
 % y =betadist(1,1);
 % plot(0.01:.01:1,y,'r','LineWidth',2);
 % plot(0.01:.01:1,y,'g','LineWidth',2);
 % hold on;
 % for(i=[1,3,4,10,15,45])
 % y =betadist(dev_alph_long(2,i), dev_beta_long(2,i));
 % plot(0.01:.01:1,y,'r','LineWidth',2);
 %
 % y =betadist(dev_alph_short(2,i), dev_beta_short(2,i)); % first trial
 % plot(0.01:.01:1,y,'g','LineWidth',2);
 % end
 %
 % Title('DEV Beta Distributions, Single Subject','FontSize', 18);
 % xlabel('x',  'FontSize', 16)
 % ylabel('P(\delta_{s,a} = x)', 'FontSize', 16)
 % set(gca, 'Box', 'off' );
 %
 % set(figure(23), 'color', 'white');
 % figure(23)
 % hold off;
 % y =betadist(1,1);
 % plot(0.01:.01:1,y,'r','LineWidth',2);
 % plot(0.01:.01:1,y,'g','LineWidth',2);
 % hold on;
 % for(i=[1,3,4,10,15,25,45])
 % y =betadist(iev_alph_long(2,i), iev_beta_long(2,i));
 % plot(0.01:.01:1,y,'r','LineWidth',2);
 %
 % y =betadist(iev_alph_short(2,i), iev_beta_short(2,i)); % first trial
 % plot(0.01:.01:1,y,'g','LineWidth',2);
 % end
 %
 % Title('IEV Beta Distributions, Single Subject','FontSize', 18);
 % xlabel('x',  'FontSize', 16)
 % ylabel('P(\delta_{s,a} = x)', 'FontSize', 16)
 % set(gca, 'Box', 'off' );
