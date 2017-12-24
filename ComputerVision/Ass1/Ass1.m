InputImage = 'IDPicture.bmp';
%OutputImage1 = 'IDPicture_bw.bmp';

C1 = imread(InputImage);
[ROWS COLUMNS LEVELS] = size(C1);
CR1 =uint8(zeros(ROWS, COLUMNS, LEVELS));

for unit = 1 : LEVELS,
    CR1(:,:,unit) = (C1(:,:,1));
end

% repeat the same  for G
CG1 =uint8(zeros(ROWS, COLUMNS, LEVELS));
for unit = 1 : LEVELS,
    CG1(:,:,unit) = (C1(:,:,2));
end

% and for B
CB1 =uint8(zeros(ROWS, COLUMNS, LEVELS));
for unit = 1 : LEVELS,
    CB1(:,:,unit) = (C1(:,:,3));
end


figure();
image(CR1), title('red');

figure();
image(CG1),title('green');

figure();
image(CB1),title('blue');

 I = 0.299*CR1 + 0.587*CG1 + 0.114*CB1;

 figure();
 image(I);

 AVERAGE= (CR1+CG1+CB1)/2;
 figure();
 image(AVERAGE);
 scale= double(I)/255;
 newSize= uint8(scale*4);
 newSize = double (newSize)/4;

 figure();
 image(newSize),title('k=4');

 newSize1= uint8(scale*16);
 newSize1 = double (newSize1)/16;

 figure();
 image(newSize1),title('k=16');

 newSize2= uint8(scale*32);
 newSize2 = double (newSize2)/32;

 figure();
 image(newSize2),title('k=32');

 newSize3= uint8(scale*64);
 newSize3 = double (newSize3)/64;

 figure();
 image(newSize3),title('k=64');

  color_factor= double(C1)/255;

 newSize4= uint8(color_factor*2);
 newSize4 = double (newSize4)/2;

 figure();
 image(newSize4),title('k=2 color');

 newSize5= uint8(color_factor*4);
 newSize5 = double (newSize5)/4;

 figure();
 image(newSize5),title('k=4 color');

 scale_log= double (100)/log(100);
 log_pic= double(C1);
 log_pic= scale_log * log(log_pic +1);
 log_pic = uint8(log_pic);

 figure();
 image(log_pic), title("log");
