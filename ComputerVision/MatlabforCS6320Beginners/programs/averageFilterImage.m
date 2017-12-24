clear all; close all; clc;
%load an image and display it in first row, 1st column, figure 1
I = imread('capitol.jpg');
%display grayscaled image in 1st row, 2nd column, figure 1
subplot(1,2,1); colormap(gray(256)); image(I);
axis image; title('Original');
%averaging filter convolution with grayscale image
oneNinth = 1.0/9.0;
mask = [oneNinth,oneNinth,oneNinth;
    oneNinth,oneNinth,oneNinth;
    oneNinth,oneNinth,oneNinth];
av_filter_img = signedfilterImage(I,3,mask);
subplot(1,2,2); image(av_filter_img);
axis image; title('Average Filtered');
