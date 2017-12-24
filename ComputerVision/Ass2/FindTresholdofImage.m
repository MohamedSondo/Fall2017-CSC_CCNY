%Image Threshold 

clc;
im=imread('pepperAsIntensity.png');                             % Read Image
figure(41);
subplot(1,2,1);
imshow(im);
title('Original Image');                            % Display Image
[ht wd]=size(im);

opim=zeros(ht,wd,'uint8');                          % Take Output variable

for i=1:ht 
    for j=1:wd
        int=im(i,j);
        if int<=120                                 % Define Threshol level
            opim(i,j)=0;
        else                                        % Thresholding
            opim(i,j)=255;
        end;
    end;
end;

imwrite(opim,'pepperAsIntensityTreshold.png');  % Create Output Image & Store
subplot(1,2,2);
imshow(opim);
title('Image after Threshold');                     % Display Output Image