function frames25(framestart, frameend, clipfoldername)
jpgdir = dir([clipfoldername,'/*.jpg']);
destination = sprintf('%s%s', clipfoldername, '_25frames');
stepsize = round((frameend-framestart)/25);

if ~exist(destination)
    mkdir(destination);
end

k = framestart;
for j = 1:25
    copyfile([clipfoldername, '/', jpgdir(k).name,], [destination, '/', jpgdir(k).name]);
    k = framestart+stepsize*j;
end

end