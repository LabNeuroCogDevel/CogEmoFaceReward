function f=GetColmnNum(S)    % S is the title of the string that I want to get number in the input matrix
global c;
global NumColum;
global ColumnNum;
%NumColumns = NumColum;
% find the vectors that have subj num, session, acc, etc...
for i=1:NumColum
    clear Temp;
    Temp=char(c(1,i));
    if (isequal(Temp,S))
        ColumnNum = i;
        break;
    end
end
f=ColumnNum;