%Read and display an RGB image, and then convert it to grayscale.
%Read the sample file, peppers.png, and display the RGB image.

img = imread('pepperAsIntensityTreshold.png');
imshow(img)
% I = rgb2gray(RGB);
figure
imshow(img)
imhist(img);
% Ihisteq = histeq(I);
% imshow(Ihisteq);


clc; clear all; close all;
g=imread('pepperAsIntensity.png');
h=rgb2gray(g);
[M,N]=size(h);

t=1:256;
n=0:255;
count=0;

for z=1:256
    for i=1:M
        for j=1:N
            
            if h(i,j)==z-1
                count=count+1;
            end
        end
    end
            t(z)=count;
            count=0;
end
disp(t')

stem(n,t); 
grid on;
ylabel('no. of pixels with such intensity levels---->');
xlabel('intensity levels---->'); title('HISTOGRAM OF THE IMAGE')