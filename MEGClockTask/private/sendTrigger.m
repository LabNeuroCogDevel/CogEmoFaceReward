function sendTrigger(trigger)
  global opts;
  % only send a trigger if we are set up to do so
  if(opts.trigger~=1), return,   end
  
  %% Parallel Port - windows
  %outp(opts.port,trigger)
  
  %% Parallel Port - Legacy
  if(opts.USEDAQ==1)
    putvalue(opts.port,0);      % weird addition prevention heres 
    putvalue(opts.port,trigger);
  end
  
  %% Serial Port -- any
  %fprintf(opts.port,'%d',trigger);
  
  %% Serial Port psychtoolbox
  % [nwritten, when, errmsg, prewritetime, postwritetime, lastchecktime] = IOPort31('Write32', opts.port, trigger,blocking=1);
  
  %% Testing
  if(opts.DEBUG==1)
    fprintf('send\t%d\t%.4f\n', trigger,  GetSecs() )
  end
end
