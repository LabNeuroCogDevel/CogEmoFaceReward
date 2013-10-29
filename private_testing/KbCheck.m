function [ keyIsDown, seconds, keyCode ] = KbCheck()
 %% KbCheck (private)
 % get time of first kbcheck and compare to KBResponse matrix
 %   [ time_after_first_kbcheck keycode_to_send ]
 % if first column > duration since first kbcheck
 global LastKBCheck KBcounter KBResponse;
 keyIsDown=0;
 keyCode=zeros(1,256);
 % initialize globals if needed
 if(isempty(LastKBCheck))
     fprintf('initializing KB autos\n');
     if(isempty(KBResponse))
       error('need global KBResponse as two column matrix [time keycode]')
     end
     KBcounter=1;
     LastKBCheck=GetSecs();

 end

  seconds=GetSecs();
  
 % if we are out of KBResponses
 % panic and throw escapes and spaces
 if( KBcounter > size(KBResponse,1))
      warning('not enough testing input (looking for %d, only have %d)!!', KBcounter,size(KBResponse,1))
      if(mod(KBcounter,2)==0)
          keyCode(KbName('ESCAPE'))=1;
      else
          keyCode(KbName('SPACE'))=1;
      end
      keyIsDown=1;
      KBcounter = KBcounter+1;
      return
 end
 

 
 if(seconds-LastKBCheck > KBResponse(KBcounter,1) )
   keyCode(KBResponse(KBcounter,2))=1;
   keyIsDown=1;
   KBcounter = KBcounter+1;
   LastKBCheck=seconds;
 end
 %fprintf('%.2f (%.2f) @ %d\n',LastKBCheck,seconds-LastKBCheck,KBcounter);
end

