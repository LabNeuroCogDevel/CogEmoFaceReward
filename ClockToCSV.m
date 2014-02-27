function ClockToCSV(filename, fieldnames)
    %helper function
    function st = cell2str(cellStr)
        if isempty(cellStr)
            st='';
        else
            %add a num2str to each conversion so that numeric and char data are handled properly
            cellStr= cellfun(@(x){[num2str(x) ',']},cellStr); %# Add ',' after each string.
            st = cat(2,cellStr{:});  %# Convert to string
            st(end) = []; %# Remove last ','
        end
    end

%verify file existence
if ~exist(filename,'file'), error('cannot find file: %s\n', filename); end

%load behavioral results into local structure
fdata=load(filename);

if nargin < 2
    if ismember('orderfmt',fields(fdata))
        fieldnames=fdata.orderfmt; %order of fields saved in more recent versions of task
    else
        %if fieldnames not specified, these are the default
        fieldnames = { 'run', 'trial', 'rewFunc', 'emotion', 'magnitude', 'probability', 'score', 'ev', 'rt', 'clock_onset', ...
            'isi_onset', 'feedback_onset', 'iti_onset' 'iti_ideal' 'image' };
    end
end

%derive new filename based on input
[name.path,name.name,name.ext]=fileparts(filename);
outfile=fullfile(name.path, sprintf('%s_tcExport.csv', name.name));

%open text file for writing
fid=fopen(outfile, 'w');

%write field header to file
fprintf(fid, '%s\n', cell2str(fieldnames));

%write each row as a comma-separated string to file
for i = 1:size(fdata.order,1)
    fprintf(fid, '%s\n', cell2str(fdata.order{i}));
end

fclose(fid);

end
