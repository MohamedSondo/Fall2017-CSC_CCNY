clear all; close all; clc;
%load an image
rgb_img = imread('Photo_062011_002.jpg');
image(rgb_img);
%fit plot box tightly around the image data
axis image;
%Change image to grayscale 2D matrix; note elipsis (...)
I = .2989*rgb_img(:,:,1)...
 +.5870*rgb_img(:,:,2)...
 +.1140*rgb_img(:,:,3);
%convert to double range [0 1]
Idouble = double(I)/255;
%smooth I
mask = [0.0 0.1 0.0;
    0.1 0.6 0.1;
    0.0 0.1 0.0];
IdoubleS = signedFilterImage01(Idouble,3,mask);
%convert to grayscale smoothed image
IdoubleSImg = uint16(round(65535*IdoubleS));
%y-derivative filter convolution with image
mask = [0,1,0;
    0,0,0;
    0,-1,0];
Iy = signedfilterImage01(IdoubleS,3,mask);
%x-derivative filter convolution with image
mask = [0,0,0;
    -1,0,1;
    0,0,0];
Ix = signedfilterImage01(IdoubleS,3,mask);

%Harris Corner Detection Algorithm
%compute Ix.^2
Ixsqrd = Ix.^2;
IxsqrdImg = uint16(round(65535*Ixsqrd));
figure;
subplot(2,3,1); image(IxsqrdImg); colormap(gray);
axis image; title('Ix Squared');
%smooth Ix.^2
mask = [0.0 0.1 0.0;
    0.1 0.6 0.1;
    0.0 0.1 0.0];
Ixsqrdsmooth = signedFilterImage01(Ixsqrd,3,mask);
IxsqrdsmoothImg = uint16(round(Ixsqrdsmooth*65535));
subplot(2,3,4);  image(IxsqrdsmoothImg); colormap(gray);
axis image; title('Ix Squared Smoothed');

%compute Iy^2
Iysqrd = Iy.^2;
IysqrdImg = uint16(round(65535*Iysqrd));
subplot(2,3,2); image(IysqrdImg); colormap(gray);
axis image; title('Iy Squared');
%smooth Iy.^2
Iysqrdsmooth = signedFilterImage01(Iysqrd,3,mask);
IysqrdsmoothImg = uint16(round(Iysqrdsmooth*65535));
subplot(2,3,5);  image(IysqrdsmoothImg); colormap(gray);
axis image; title('Iy Squared Smoothed');

%compute Ix*Iy
IxIy = Ix.*Iy;
IxIyImg = uint16(round(65535*IxIy));
subplot(2,3,3); image(IxIyImg); colormap(gray);
axis image; title('IxIy');
%smooth IxIy
IxIysmooth = signedFilterImage01(IxIy,3,mask);
IxIysmoothImg = uint16(round(65535*IxIysmooth));
subplot(2,3,6);  image(IxIysmoothImg); colormap(gray);
axis image; title('IxIy Smoothed');

%M = [Ix.^2, Ix.*Iy; Ix.*Iy, Iy.^2] element-wise
a = Ixsqrd;
b = IxIy;
c = Iysqrd;
%Response = det[M] - alpha*(trace[M].^2) = (a.*c - b.^2)-0.05((a + c).^2)
Response1 = (a.*c - b.^2) - 0.04*((a + c).^2);
Response1(Response1>0)=1;
Response1(Response1<=0)=0;
Response1Img = uint16(round(65535*Response1));
figure;
subplot(1,2,1); image(Response1Img); colormap(gray);
axis image; title('Response, no Smoothing');
imgOverlay1(:,:,1) = IdoubleSImg+Response1Img;
imgOverlay1(:,:,2) = IdoubleSImg;
imgOverlay1(:,:,3) = IdoubleSImg;
subplot(1,2,2); image(imgOverlay1);
axis image; title('Response + Original');
%Response after smoothing Ix.^2, Iy.^2, and Ix.*Iy
asmth = Ixsqrdsmooth;
bsmth = IxIysmooth;
csmth = Iysqrdsmooth;
%Response2 = (asmth.*csmth - bsmth.^2) - 0.04*((asmth + csmth).^2);
Response2 = (asmth.*csmth - bsmth.^2) - 0.04*((asmth + csmth).^2);
Response2(Response2>0.000001)=1;
Response2(Response2<=0.000001)=0;
Response2Img = uint16(round(65535*Response2));
figure;
subplot(1,2,1); image(Response2Img); colormap(gray);
axis image; title('Smoothed a,b,c');
imgOverlay1(:,:,1) = IdoubleSImg+Response2Img;
imgOverlay1(:,:,2) = IdoubleSImg;
imgOverlay1(:,:,3) = IdoubleSImg;
subplot(1,2,2); image(imgOverlay1);
axis image; title('Smoothed a,b,c + Original');
%Response2s = smoothed Response2
Response2s = signedFilterImage01(Response2,3,mask);
Response2s(Response2s>0.000001)=1;
Response2s(Response2s<=0.000001)=0;
Response2sImg = uint16(round(65535*Response2s));
figure;
subplot(1,2,1); image(Response2sImg); colormap(gray);
axis image; title('Response, more smooth');
imgOverlay1(:,:,1) = IdoubleSImg+Response2sImg;
imgOverlay1(:,:,2) = IdoubleSImg;
imgOverlay1(:,:,3) = IdoubleSImg;
subplot(1,2,2); image(imgOverlay1);
axis image; title('Response, more smooth + Original');
