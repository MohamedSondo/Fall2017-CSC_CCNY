clear all; close all; clc;
%load an image and display it in first row, 1st column, figure 1
I = imread('capitol.jpg');
%display grayscaled image in 1st row, 2nd column, figure 1
subplot(2,2,1); colormap(gray(256)); image(I);
axis image; title('Original');
%y-derivative filter convolution with grayscale image
mask = [0,1,0;
    0,0,0;
    0,-1,0];
Iy = signedfilterImage(I,3,mask);
subplot(2,2,2); colormap(gray(256));  image(Iy);
axis image; title('Y Derivative');
%x-derivative filter convolution with grayscale image
mask = [0,0,0;
    -1,0,1;
    0,0,0];
Ix = signedfilterImage(I,3,mask);
subplot(2,2,3); colormap(gray(256));  image(Ix);
axis image; title('X Derivative');
%compute gradient = square root of Ix^2 + Iy^2
Ig = uint8(sqrt(double(Ix).^2 + double(Iy).^2));
subplot(2,2,4); colormap(gray(256));  image(Ig);
axis image; title('Gradient');
