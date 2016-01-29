%% script for loading AVIs and converting them into images
avidir = dir('*.avi');
% avi2tiff(avidir(1).name, 50, 67, 'SD-01-01');
% 
% avi2tiff(avidir(2).name, 8, 21, 'SD-01-02');
% 
% avi2tiff(avidir(3).name, 56, 65, 'SD-01-04A');
% avi2tiff(avidir(3).name, 89, 109, 'SD-01-04B');
% 
% avi2tiff(avidir(4).name, 8, 25, 'SD-02-02A');
% avi2tiff(avidir(4).name, 27, 44, 'SD-02-02B');
% avi2tiff(avidir(4).name, 49, 64, 'SD-02-02C');

avi2tiff(avidir(1).name, 20, 68, 'SD1_01');
avi2tiff(avidir(3).name, 4, 26, 'SD1_03');
avi2tiff(avidir(4).name, 1, 13, 'SD1_04');
avi2tiff(avidir(5).name, 10, 22, 'SD1_05A');
avi2tiff(avidir(5).name, 23, 53, 'SD1_05B');
avi2tiff(avidir(6).name, 3, 41, 'SD1_06');
avi2tiff(avidir(7).name, 2, 38, 'SD1_07');
avi2tiff(avidir(8).name, 5, 26, 'SD1_08');
avi2tiff(avidir(10).name, 0, 28, 'SD1_10');

%%
% making all the suction/winnowing/ejection folders
% gen. rule: for suction/ejection, take every 5 frames
% for winnowing: take every 15 frames
% [num, txt, raw] = xlsread('Video_specs.xls');

for i = 1:21
% for i = 22:length(raw(:,1))
    videoname = sprintf('%s%s%s', raw{i, 1}, '_', raw{i, 3});
    if strcmp(raw{i, 4}, 'S') == 1
        savename = sprintf('%s%s', 'S_', videoname);
        avi2tiff([videoname, '_1-18-2016_C001H001S0001.avi'], raw{i,5}, raw{i,6}, savename);
%         frames_fixed(videoname, 5, 'S');
    elseif strcmp(raw{i, 4}, 'W') == 1
        avi2tiff([videoname, '_1-18-2016_C001H001S0001.avi'], raw{i,5}, raw{i,6}, ['W_', videoname]);

%         frames_fixed(videoname, 15, 'W');
    elseif strcmp(raw{i,4}, 'E') == 1
        avi2tiff([videoname, '_1-18-2016_C001H001S0001.avi'], raw{i,5}, raw{i,6}, ['E_', videoname]);

%         frames_fixed(videoname, 5, 'E');
    end
end


%%
Wdir15 = dir('W_*');
for i = 1:length(Wdir15)
    frames_fixed(Wdir15(i).name, 15)
end

%%
Sdir5 = dir('S_*');
for i = 1:length(Sdir5)
    frames_fixed(Sdir5(i).name, 5)
end

%%
Edir5 = dir('E_*');
for i = 1:length(Edir5)
    frames_fixed(Edir5(i).name, 5)
end


%% Make shapes folders to correspond to Intervals folders

ohdir = dir('*Intervals');
for i = 1:length(ohdir)
    newname = sprintf('%s%s%s','../Shapes/', ohdir(i).name(1:end-9), 'Shapes');
    if ~exist(newname)
        mkdir(newname)
    end
end
%% pull out frames
tiffdir = dir('SD1*')

for i = 1:length(tiffdir)
    if length(tiffdir(i).name) <= 10
        frames_fixed(tiffdir(i).name, 15)
    end
end

%% whoops...frame number formatting
SDdir = dir('*SD1_*/')
for k = 1:length(SDdir)
jpgdir=dir([SDdir(k).name, '/*.jpg']);
for i = 1:length(jpgdir)
    if length(jpgdir(i).name) == 19
        newname = sprintf('%s%s%s%s%s',SDdir(k).name,'/', jpgdir(i).name(1:6), '000', jpgdir(i).name(7:end));
        oldname = sprintf('%s%s%s', SDdir(k).name, '/', jpgdir(i).name);
        movefile(oldname, newname)
    end
    if length(jpgdir(i).name) == 20
        newname = sprintf('%s%s%s%s%s',SDdir(k).name,'/', jpgdir(i).name(1:6), '00', jpgdir(i).name(7:end));
        oldname = sprintf('%s%s%s', SDdir(k).name, '/', jpgdir(i).name);
        movefile(oldname, newname)
    end
    if length(jpgdir(i).name) == 21
        newname = sprintf('%s%s%s%s%s',SDdir(k).name,'/', jpgdir(i).name(1:6), '0', jpgdir(i).name(7:end));
        oldname = sprintf('%s%s%s', SDdir(k).name, '/', jpgdir(i).name);
        movefile(oldname, newname)
    end
end
end

%%
newdir = dir('*_10FrameIntervals');
for i = 1:length(newdir)
    source = newdir(i).name;
    destination = sprintf('%s%s', '/Users/hannah/Dropbox/Westneat Lab/Eartheater Project/Data/Videos/SD1/01-15-16/', newdir(i).name);
    copyfile(source, destination);
end