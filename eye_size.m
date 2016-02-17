function[pixels] = eye_size(imagename, threshold)
image = imread(imagename);
greyimage = rgb2gray(image);
imdim = size(greyimage);

% set threshold; if above then turn white

for i = 1:imdim(1)
    for j = 1:imdim(2)
        if greyimage(i, j) > threshold
            greyimage(i, j) = 255;
        else
            greyimage(i, j) = 0;
        end
    end
end
BW2 = bwmorph(greyimage,'remove');
alternate = bwmorph(greyimage, 'majority', Inf);
BW3 = bwmorph(alternate, 'remove');
figure; imshow(image);
figure; imshow(greyimage);
% figure; imshow(BW2);
figure; imshow(alternate);
figure; imshow(BW3);

pixels = bwconncomp(alternate);

end