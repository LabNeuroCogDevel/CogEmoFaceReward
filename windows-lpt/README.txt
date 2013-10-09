from:

http://sunburst.usd.edu/~schieber/psyc770/IO64.html

as ref'ed by 
http://psychtoolbox.org/faqttltrigger


INSTALL:

1. Log in as a user with Administrator privileges.
2. Disable UAC (User Account Control).  An easy way to do this in Windows Vista is to: Start-Run-MSCONFIG. Select the Tools tab, scroll down to the option for "Disable UAC" and select it. Next, press the "Launch" button. You must then RESTART the system for this change to take effect.
3. Download and copy the inpoutx64.dll10 file to the C:\WINDOWS\SYSTEM32 directory.
4. Download the io64.mexw6411, config_io.m12, inp.m13 and outp,m14 files to a working directory of your choice. This directory will be added to your MATLAB path in step-6 below.
5. Start MATLAB in "Run as Administrator" mode (Right-click icon and select "Run as Administrator").
6. Add the directory containing the downloaded m-files to your MATLAB path via the File|Set Path|Add with Subfiles... menu command.
7. Run "config_io" from the MATLAB command window.  If there's no error message at this point, you've successfully installed the software.
8. Optional: If you need to re-enable UAC (User Account Control), follow the instructions in step-2 but select "Enable UAC" instead of "Disable UAC".

MATLAB code:

% initialize access to the inpoutx64 low-level I/O driver
config_io;
% optional step: verify that the inpoutx64 driver was successfully initialized
global cogent;
if( cogent.io.status ~= 0 )
   error('inp/outp installation failed');
end
% write a value to the default LPT1 printer output port (at 0x378)
address = hex2dec('378');
byte = 99;
outp(address,byte);
% read back the value written to the printer port above
datum=inp(address);