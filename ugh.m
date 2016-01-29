photodir = dir('*.tif');
for i = 1:length(photodir)
    newname = photodir(i).name(1:10);
    if ~exist(newname)
        mkdir(newname);
    end
    
    copyfile(photodir(i).name, newname);
end

newdir = dir('*_01');

for i = 1:length(newdir)
    xdir = dir([newdir(i).name, '/*.csv.']);
    movefile(xdir(i).name, [xdir(i).name(1:end-4), 'csv']);
end


