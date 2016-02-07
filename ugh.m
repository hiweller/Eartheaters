photodir = dir('*.tif');
for i = 1:length(photodir)
    newname = photodir(i).name(1:10);
    if ~exist(newname)
        mkdir(newname);
    end
    
    copyfile(photodir(i).name, newname);
end

newdir = dir('*_01');


