% script to loop through subjects and plot data


%initialize global definitions
globdefs

%specify model that you are plotting
model = 'noemo'; %no parameters vary by emotion
%model = 'emoexplore'; %explore epsilon varies by emotion

%data directory
mat_dir='../outputs/parameter_mat/';
subjdata = dir(strcat(mat_dir,strcat('*',model,'.mat')));

for f = 1:length(subjdata)
    %load data
    data_file = fullfile(mat_dir,subjdata(f).name);
    load(data_file);
    
    plotSubjData(subject,ret_all,model);
    
end
