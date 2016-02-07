function avi2tiff(avifile, clipstart, clipend, savename) 
    v = VideoReader(avifile);
    if ~exist(savename, 'dir')
        mkdir(savename);
    end
    v.CurrentTime = clipstart;
    k = 1;
    while v.CurrentTime <= clipend
        img = readFrame(v);
        if k < 10
            filename = sprintf('%s%s%s%s%s', savename, '/Frame-000', num2str(k), savename, '.jpg');
        elseif k < 100
            filename = sprintf('%s%s%s%s%s', savename, '/Frame-00', num2str(k), savename, '.jpg');
        elseif k < 1000
            filename = sprintf('%s%s%s%s%s', savename, '/Frame-0', num2str(k), savename, '.jpg');
        else
            filename = sprintf('%s%s%s%s%s', savename, '/Frame-', num2str(k), savename, '.jpg');
        end
        imwrite(img, filename);
        k = k+1;
    end

end