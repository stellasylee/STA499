function importfile(filename)
%IMPORTFILE(FILETOREAD1)
%  Imports data from the specified file
%  FILETOREAD1:  file to read
% Import the file
data = load('-mat', filename);

% Delete field except 'elemDataI'
vars = fieldnames(data);
for i = 1:length(vars)
    if (strcmp(vars{i}, 'elemDataI') == 1)
        disp(vars{i})
        data = rmfield(data,vars{i});
    end
end

% Delete unneccesary variables in 'elemDataI'

save (filename, '-struct', 'data')






