% The actual scoring function. Taken from Frank et al
function [Mag Freq] = getScore(RT,scrfunc)
          % Values for Reward computation - constant for all phases
   k = 37;
   Shift = 700;
   rt_extended = 7000;
   DEV_factor = 10;
   DEV_factor2= 1;
   sin_factor = 0.25;

    % score response time based on one of 3 functions
    % given in master file (either inc,dec,or const)
    switch scrfunc
     case 'CEV'
         Mag = (k*rt_extended)/(rt_extended-(RT+Shift)); 
         Freq = 1-((RT+Shift)/rt_extended);
         
     case 'DEV'
        Mag = DEV_factor*log(DEV_factor2*(RT+Shift)); 
        CEV_x = 1-((RT+Shift)/rt_extended);
        IEV_x = CEV_x + (CEV_x*(sin_factor*sin((RT*pi)/5000)));
        Freq = (2*CEV_x)-IEV_x;
     case 'IEV'
        CEV_x = (k*rt_extended)/(rt_extended-(RT+Shift)); 
        DEV_x = DEV_factor*log(DEV_factor2*(RT+Shift));
        Mag = (2*CEV_x)-(DEV_x);
        CEV_x2 = 1-((RT+Shift)/rt_extended);
        Freq = CEV_x2 + (CEV_x2*(sin_factor*sin((RT*pi)/5000)));
     case 'CEVR'
        Mag = 1-((RT+Shift)/rt_extended);
        Mag = Mag*200;
        Freq = (k*rt_extended)/(rt_extended-(RT+Shift)) ;
        Freq = Freq/200;
     
     otherwise
      %score=score;
      Mag = 0;
      Freq = 0;
      warning(['!!!WHAT function did you mean by' scrfunc]);
    end

end

