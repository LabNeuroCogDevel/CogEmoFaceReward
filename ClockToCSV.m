function ClockToExcel(filename, fieldnames)
if nargin < 2
    %if fieldnames not specified, these are the default
    fieldnames = { 'run', 'trial', 'rewFunc', 'emotion', 'magnitude', 'probability', 'score', 'ev', 'rt', 'clock_onset', ...
        'isi_onset', 'feedback_onset', 'iti_onset' 'iti_ideal' 'image' };
end

    %helper function
    function st = cell2str(cellStr)
        %add a num2str to each conversion so that numeric and char data are handled properly
        cellStr= cellfun(@(x){[num2str(x) ',']},cellStr); %# Add ',' after each string.
        st = cat(2,cellStr{:});  %# Convert to string
        st(end) = []; %# Remove last ','
    end


%verify file existence
if ~exist(filename,'file'), error('cannot find file: %s\n', filename); end

%load behavioral results into local structure
fdata=load(filename);

%derive new filename based on input
[name.path,name.name,name.ext]=fileparts(filename);
outfile=fullfile(name.path, sprintf('%s_tcExport.csv', name.name));

%open text file for writing
fid=fopen(outfile, 'w');

%write field header to file
fprintf(fid, '%s\n', cell2str(fieldnames));

for i = 1:size(fdata.order,1)
    %convert=cellfun(@num2str, order{i}, 'UniformOutput', false);
    %mat{i}=convert;
    fprintf(fid, '%s\n', cell2str(fdata.order{i}));
    %fprintf(fid ,'%i\t%i\t%s\t%s\t%i\t%f\t%f\t%f\t%i\t%f\t%f\t%f\t%f\t%i\t%s\n',...
    %    order{i}{:})
end

fclose(fid);
%xlswrite('fMRIEmoClock_6_tc.xls', mat)
end
