function avi2tiff(avifile, clipstart, clipend, savename) 
    v = VideoReader(avifile);
    if ~exist(savename)
        mkdir(savename);
    end
    v.CurrentTime = clipstart;
    k = 1;
    while v.CurrentTime <= clipend
        img = readFrame(v);
        filename = sprintf('%s%s%s%s%s', savename, '/Frame-', num2str(k), savename, '.jpg');
        imwrite(img, filename);
        k = k+1;
    end

end