function initTriggerSender()
  global opts;
  % only initialize if we need to send triggers
  if(opts.trigger~=1), return,   end
  
  %% Parallel Port -- windows only, preinstalled driver
  % see windows-lpt/README.txt
  %addpath(genpath('windows-lpt')); % add outp function to write to port
  %opts.port=hex2dec('378');
  
  %% Parallel port, Windows + DAQ Legacy Interface only
  if(opts.USEDAQ==1)
      handle = digitalio('parallel','lpt1');
      addline(handle,0:7,'out');
      opts.port = daqgetfield(handle,'uddobject'); % speed up write by caching handle
      % http://psychtoolbox.org/faqttltrigger
  else 
      fprintf('initTriggerSender: disabled\n')
  end
  %% Serial Port - via matlab
  % % see instrhwinfo('serial')
  % % windows
  % opts.port = fopen(serial('COM1') )
  %  % OSX
  %  % serial('/dev/tty/KeySerial1')
  %  % Linux
  %  % serial('/dev/ttys0')
  
  %% Serial Port via psychtoolbox
  % % http://docs.psychtoolbox.org/IOPort
  % [ opts.port , error ] = IOPort('OpenSerialPort','COM1')
  % % for linux use /dev/ttyS0; OSX use  /dev/something
  
  % override any other codes
  sendTrigger(0)
end
