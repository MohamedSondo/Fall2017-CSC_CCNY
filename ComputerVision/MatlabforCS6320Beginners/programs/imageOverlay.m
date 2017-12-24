clear all; close all; clc;
%load an image and display it in first row, 1st column, figure 1
I = imread('capitol.jpg');
%display grayscaled image in 1st row, 2nd column, figure 1
%y-derivative filter convolution with grayscale image
mask = [0,1,0;
    0,0,0;
    0,-1,0];
Iy = signedfilterImage(I,3,mask);
%x-derivative filter convolution with grayscale image
mask = [0,0,0;
    -1,0,1;
    0,0,0];
Ix = signedfilterImage(I,3,mask);
%compute gradient = square root of Ix^2 + Iy^2
Ig = uint8(sqrt(double(Ix).^2 + double(Iy).^2));
%binarize gradient edge map
Ig(Ig>35) = 255;
Ig(Ig<=35) = 0;
subplot(1,2,1); colormap(gray(256)); image(Ig);
axis image; title('Edge Map');
imgOverlay(:,:,1) = I+Ig;
imgOverlay(:,:,2) = I;
imgOverlay(:,:,3) = I;
subplot(1,2,2); image(imgOverlay);
axis image; title('Edge Image Overlay Original');
