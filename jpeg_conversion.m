% in_name = 'xyz.tif';    %the name of your input image
% out_name = 'xyz_converted.jpg';   %the name of the desired output
% IM = imread(in_name);   %read in the image
% imwrite(IM, out_name);  %write it out

function jpeg_conversion(readname, savename)

    pic = imread(readname);
    savename2 = sprintf('%s%s', savename, '.jpg');
    imwrite(pic, savename2);
    
end