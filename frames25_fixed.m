function frames25_fixed(clipfoldername)
jpgdir = dir([clipfoldername,'/*.jpg']);
destination = sprintf('%s%s', clipfoldername, '_25frames');
stepsize = 20;

if ~exist(destination)
    mkdir(destination);
end

k = 1;
while k <= length (jpgdir)
    copyfile([clipfoldername, '/', jpgdir(k).name,], [destination, '/', jpgdir(k).name]);
    k = k+stepsize;
end

end