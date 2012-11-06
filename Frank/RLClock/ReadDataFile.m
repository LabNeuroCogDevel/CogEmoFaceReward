% reading data from E-data file - this code is a mess!
global NumColum;
NumColum = 20;

FilePathComb = strcat(PathName,'/',File_or_Mat_Name);
[c(:,1) c(:,2) c(:,3) c(:,4) c(:,5) c(:,6) c(:,7) c(:,8) c(:,9) c(:,10) c(:,11) c(:,12) c(:,13) c(:,14) c(:,15) c(:,16) c(:,17) c(:,18) c(:,19) c(:,NumColum)] = textread(FilePathComb,'%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s ','delimiter',',','headerlines',0);  % later on, I will deal session and drug columns.
%c=char(c);
% Important cariables: v1 (subj number), v2(session), v3 (acc),v6(feedback, v11(trial type)
global c;

ColmnNum(1,1)= GetColmnNum('Subject');
ColmnNum(1,2)= GetColmnNum('Session');
ColmnNum(1,3)= GetColmnNum('RT');    % 1 = predict rew, 0 pun
ColmnNum(1,4)= GetColmnNum('TrialType');
ColmnNum(1,5)= GetColmnNum('score'); % this is for feedback in trn phase

if (Use_Drug_Data == 1)
    ColmnNum(1,6)= GetColmnNum('Drug'); % this for patient info: onM, OffM, etc.
end

if (Use_Drug_Data == 1)
    NumColmnUsed = 6;
else
    NumColmnUsed = 5;
end


% delete first fmin_bugrow; convert trialtype to number; combine all vectors; delete rows with 6666;


% delete first row
for i=1:NumColmnUsed  % it was 5
    Temp = c(:,ColmnNum(1,i));
    Temp = Temp(2:length(Temp));    % remove first row
    v_noTitle(:,ColmnNum(1,i)) = Temp;
    clear Temp;
end

 %========================================  Trn vector======================

Temp = v_noTitle(:,ColmnNum(1,3));
Temp = char(Temp);      % this is to get rid of the cell arrary and use char array instead.
Temp = str2num(Temp);
for i=1:length(Temp)
    if (Temp(i) < 1 || Temp(i) >5000)
        Temp(i)= -3;   
    end
end
Temp = num2str(Temp);
Temp = cellstr(Temp);
v_noTitle(:,ColmnNum(1,3)) = Temp;
clear Temp;


%====================== converting trial type to number. it should work for both trn and tst columns ====================================================
Temp = v_noTitle(:,ColmnNum(1,4));
Temp = char(Temp);      % this is 
v_trial_type = zeros (length(v_noTitle(:,ColmnNum(1,4))),1);
for i=1:length(Temp)
    if (Temp(i,1:3) == 'CEV')       
        v_trial_type(i) = 12;
      elseif (Temp(i,1:3) == 'CER')       
        v_trial_type(i) = 34;
        elseif (Temp(i,1:3) == 'DEV')       
        v_trial_type(i) = 56;
    elseif (Temp(i,1:3) == 'IEV')      
        v_trial_type(i) = 78;
        
    else
        v_trial_type(i) = -5;      % this means put other trial types and 6666 to -5  *see if this exist or not.
    end
end
clear Temp;
v_trial_type = num2str(v_trial_type);
v_trial_type = cellstr(v_trial_type);
v_noTitle(:,ColmnNum(1,4)) = v_trial_type;
% =======================================================================================================

%================================================drug data=================
if (Use_Drug_Data == 1)
    Temp = v_noTitle(:,ColmnNum(1,6));
    Temp = char(Temp);      % this is to get rid of the cell arrary and use char array instead.
    %Temp = str2num(Temp);
    for i=1:length(Temp)
        if(Temp(i,1) == 'P')
            v_drug(i,1) = 1;       
        elseif(Temp(i,1) == 'M')
            v_drug(i,1) = 2;
        elseif(Temp(i,1) == 'S')
            v_drug(i,1) = 3;
        end
    end
    clear Temp;
    v_drug = num2str(v_drug);
    v_drug = cellstr(v_drug);
    v_noTitle(:,ColmnNum(1,6)) = v_drug;
end
%========================================================================
%at this point, v_notitle has everything I need and it is all numbers..it is still cell
%v_noTitle = char(v_noTitle);   % not working.
for i=1:NumColmnUsed    % it was 5
    Temp = v_noTitle(:,ColmnNum(1,i));
    Temp = char(Temp);
    Temp = str2num(Temp);
    v_trn_or_tst (:,i) = Temp;
end

% I will convert v_trn_or_tst to either v_trn or v_tst

% make sure bad rows are deleted here.
% now I will combine v1, v3, and v8
% v = zeros(length(v3),5);    % v3 is used here because they all have the same length.
% 
% v(:,1) = v1;   %subj number
% v(:,2) = v2;   %session 
% v(:,3) = v3;   % acc
% v(:,4) = v6;   % feedback
% v(:,5) = v_trial_type;

%========================================================== v_trn==========================
v_trn = v_trn_or_tst;
i=1;
temp =length(v_trn);
while (i<= temp)  % why v and v3 have different lenghs. because some rows are deleted from v.
    if (v_trn(i,3) == -3)
        v_trn(i,:) = [];   % delete that row
        %i=i+1;
        temp = temp-1;
    else
        i=i+1;
    end
end



% deleting 6666 in the feedback column in both v_trn. v_tst does not have a feedback column
i=1;
temp =length(v_trn);
while (i<= temp)  % why v and v3 have different lenghs. because some rows are deleted from v.
    if (v_trn(i,5) == 6666)   % 5 is for the feedback column
        v_trn(i,:) = [];   % delete that row
        %i=i+1;
        temp = temp-1;
    else
        i=i+1;
    end
end
%=======================================================



d1 = ismember(6666,v_trn);

if(d1 ==1)
    'there could be some problem in the file you read because of 6666'
    'type return and press enter if you want to ingore that'

    keyboard
end
