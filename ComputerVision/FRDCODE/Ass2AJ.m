
%Aijie Chen
%last four ID: 5774

I = imread('IDPicture.bmp');
I2 = imread('peppers.png');


[ROWS COLS CHANNELS] = size(I);
[R C CHANNELS] = size(I2);

MAP = double(zeros(256, CHANNELS));
for i = 1 : 256
    for band = 1:CHANNELS
        MAP(i,band) = (i-1)/255;
    end 
end



intensity= 0.299*I(:,:,1)+ 0.587*I(:,:,2) + 0.114*I(:,:,3);
intensity2=0.299*I2(:,:,1)+ 0.587*I2(:,:,2) + 0.114*I2(:,:,3);

figure();
image(intensity),title('intensity of original picture');
colormap(MAP);
figure();
image(intensity2),title('intensity of manmade picture');
colormap(MAP);

%Q1 histogram
counter=zeros(256,1);
for i=1:250
    for j=1:250
        counter(intensity(i,j)+1)=counter(intensity(i,j)+1)+1;
    end
end
figure();
bar(1:256,counter),title('histogram of oringinal picture');

%thresholding
%create a empty image
threshold = uint8(zeros(ROWS, COLS));
counter_t =zeros(256,1);

for i=1:250
    for j=1:250
        if intensity(i,j) <= 90
            threshold(i,j) = 0;
        else
            threshold(i,j) =255;

        end
         counter_t(threshold(i,j)+1) = counter_t(threshold(i,j)+1) +1;
    end
end
figure();
image(threshold),title('treshold image t=90');
colormap(MAP);
figure();
bar(1:256,counter_t);
title('histogram for threshold');

%Contrast Enhancement
contra_enhance = uint8(zeros(ROWS, COLS));
counter_c =zeros(256,1);
for i=1 :250
    for j=1:250
        contra_enhance(i,j) = (double(intensity(i,j))).^1.25;
        counter_c(contra_enhance(i,j)+1) = counter_c(contra_enhance(i,j)+1) +1;

    end
end
figure();
image(contra_enhance),title('Contrast Enhancement');
colormap(MAP);
figure();
bar(1:256,counter_c);
title('histogram contast enhancement');

%equzlization
%original_hist = zeros(256,1); It is counter

Np = ROWS.*COLS./256;
CH_j = zeros(256,1);
for i=1:256
    if i==1
        CH_j(i) = counter(i);
    else
        CH_j(i) = CH_j(i-1) + counter(i);
    end
end

equa_i = zeros(256,1);
for i =1:256
    equa_i(i)=max(0, round(CH_j(i)/Np)-1);
end

equal_image = zeros(250,250);
for i=1:250
    for j=1:250
      equal_image(i,j) = round((CH_j(intensity(i,j)+1)-1)/(250.*250-1).* 254)+1;
    end
end

        
figure();
image(equal_image),title('Equalization');
colormap(MAP);
equal_c = zeros(256,1);
for i=1:250
    for j = 1:250
    equal_c(equal_image(i,j)+1) = equal_c(equal_image(i,j)+1)+1;
    end
end
figure();
bar(1:256,equal_c);
title('Histogram of Equalization');










%question 2
%Apply the 1x2 operator and Sobel operator to your image and analyze
%  the results of the gradient magnitude images (including vertical gradients, horizontal
%  gradients, and the combined)

%for 1X2 opertator for ID picture
dx_one = zeros(250, 250);
dy_one = zeros(250, 250);
filter = [1, -1];
for i = 2:250
    for j = 2:250
        dx_one(i, j) = abs(intensity(i,j) - intensity(i-1, j));
        dy_one(i, j) = abs(intensity(i,j) - intensity(i, j-1));
    end
end

combine_one = uint8(zeros(250, 250));
for i = 1:250
    for j = 1:250
        combine_one(i, j) = sqrt((double(dx_one(i, j)).^2)+ (double(dy_one(i, j)).^2));
    end
end
figure();
comb_one= [dx_one, dy_one, combine_one];
image(comb_one),title('herizonital, verital, and combined of 1x2 operator of ID');
colormap(MAP);

%for 1X2 opertator for flower
dx_one2 = zeros(R, C);
dy_one2 = zeros(R, C);
filter = [1, -1];
for i = 2:R
    for j = 2:C
        dx_one2(i, j) = abs(intensity2(i,j) - intensity2(i-1, j));
        dy_one2(i, j) = abs(intensity2(i,j) - intensity2(i, j-1));
    end
end

combine_one2 = uint8(zeros(R, C));
for i = 1:R
    for j = 1:C
        combine_one2(i, j) = sqrt((double(dx_one2(i, j)).^2)+ (double(dy_one2(i, j)).^2));
    end
end
figure();
comb_one2= [dx_one2, dy_one2, combine_one2];
image(comb_one2),title('herizonital, verital, and combined of 1x2 operator of flower');
colormap(MAP);

%for sobel operation for ID
dx_sobel = (zeros(250, 250));
dy_sobel = (zeros(250, 250));
sobel_XY= (zeros(250,250));
Gx = [1, 0, -1; 2, 0, -2; 1, 0, -1];
Gy = [-1, -2, -1; 0, 0 ,0; 1, 2, 1];

for i=2:249
    for j=2:249
        sobel_valueX=double (0);
        sobel_valueY=double(0);
        for s_r=1:3
            for s_c=1:3
                if(s_r==1 & s_c==1)
                sobel_valueX =sobel_valueX+double(intensity(i-1,j-1)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i-1,j-1)).*Gy(s_r,s_c);
                elseif(s_r==1 & s_c==2)
                sobel_valueX =sobel_valueX+double(intensity(i-1,j)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i-1,j)).*Gy(s_r,s_c);
                elseif(s_r==1 & s_c==3)
                sobel_valueX =sobel_valueX+double(intensity(i-1,j+1)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i-1,j+1)).*Gy(s_r,s_c);
                elseif(s_r==2 & s_c==1)
                sobel_valueX =sobel_valueX+double(intensity(i,j-1)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i,j-1)).*Gy(s_r,s_c);
                elseif(s_r==2 & s_c==2)
                sobel_valueX =sobel_valueX+double(intensity(i,j)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i,j)).*Gy(s_r,s_c);
                elseif(s_r==2 & s_c==3)
                sobel_valueX =sobel_valueX+double(intensity(i,j+1)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i,j+1)).*Gy(s_r,s_c);
                elseif(s_r==3 & s_c==1)
                sobel_valueX =sobel_valueX+double(intensity(i+1,j-1)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i+1,j-1)).*Gy(s_r,s_c);
                elseif(s_r==3 & s_c==2)
                sobel_valueX =sobel_valueX+double(intensity(i+1,j)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i+1,j)).*Gy(s_r,s_c);
                elseif(s_r==3 & s_c==3)
                sobel_valueX =sobel_valueX+double(intensity(i+1,j+1)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i+1,j+1)).*Gy(s_r,s_c);
                end
 
            end
        end
        dx_sobel(i,j) =abs(sobel_valueX)/4;
        dy_sobel(i,j) =abs(sobel_valueY)/4;
        sobel_XY(i,j)= sqrt((double(dx_sobel(i, j)).^2)+ (double(dy_sobel(i, j)).^2));
    end
end


figure();
sobel_images=[dx_sobel,dy_sobel,sobel_XY];
image(sobel_images),title('sobel with x, sobel with y and combine sobel XY');
colormap(MAP);

%for sobel operation for flower
dx_sobel2 = (zeros(R, C));
dy_sobel2 = (zeros(R, C));
sobel_XY2= (zeros(R,C));

for i=2:R-1
    for j=2:C-1
        sobel_valueX=double (0);
        sobel_valueY=double(0);
        for s_r=1:3
            for s_c=1:3
                if(s_r==1 & s_c==1)
                sobel_valueX =sobel_valueX+double(intensity2(i-1,j-1)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity2(i-1,j-1)).*Gy(s_r,s_c);
                elseif(s_r==1 & s_c==2)
                sobel_valueX =sobel_valueX+double(intensity2(i-1,j)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity2(i-1,j)).*Gy(s_r,s_c);
                elseif(s_r==1 & s_c==3)
                sobel_valueX =sobel_valueX+double(intensity2(i-1,j+1)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity2(i-1,j+1)).*Gy(s_r,s_c);
                elseif(s_r==2 & s_c==1)
                sobel_valueX =sobel_valueX+double(intensity2(i,j-1)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity2(i,j-1)).*Gy(s_r,s_c);
                elseif(s_r==2 & s_c==2)
                sobel_valueX =sobel_valueX+double(intensity2(i,j)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity2(i,j)).*Gy(s_r,s_c);
                elseif(s_r==2 & s_c==3)
                sobel_valueX =sobel_valueX+double(intensity2(i,j+1)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity2(i,j+1)).*Gy(s_r,s_c);
                elseif(s_r==3 & s_c==1)
                sobel_valueX =sobel_valueX+double(intensity2(i+1,j-1)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity2(i+1,j-1)).*Gy(s_r,s_c);
                elseif(s_r==3 & s_c==2)
                sobel_valueX =sobel_valueX+double(intensity2(i+1,j)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity2(i+1,j)).*Gy(s_r,s_c);
                elseif(s_r==3 & s_c==3)
                sobel_valueX =sobel_valueX+double(intensity2(i+1,j+1)).*Gx(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity2(i+1,j+1)).*Gy(s_r,s_c);
                end
 
            end
        end
        dx_sobel2(i,j) =abs(sobel_valueX)/4;
        dy_sobel2(i,j) =abs(sobel_valueY)/4;
        sobel_XY2(i,j)= sqrt((double(dx_sobel2(i, j)).^2)+ (double(dy_sobel2(i, j)).^2));
    end
end


figure();
sobel_images=[dx_sobel2,dy_sobel2,sobel_XY2];
image(sobel_images),title(' Flower sobel with x, sobel with y and combine sobel XY');
colormap(MAP);


%If you subtract the 1x2 edge image from the Sobel are there any residuals?
residuals = zeros(250,250);
for i=1:250
    for j=1:250
        residuals(i,j) = abs(sobel_XY(i,j)- combine_one(i,j));
    end
end
figure();
image(residuals),title('residuals of 1X2 and sobel operator');
colormap(MAP);
%for flower
residuals2 = zeros(R,C);
for i=1:R
    for j=1:C
        residuals2(i,j) = abs(sobel_XY2(i,j)- combine_one2(i,j));
    end
end
figure();
image(residuals2),title(' Flower residuals of 1X2 and sobel operator');
colormap(MAP);


%Q3
map_one = uint8(zeros(250, 250));
map_sobel = uint8(zeros(250,250));
keep_one = ROWS *COLS * .85;
keep_sobel = ROWS *COLS * .75;
counter_one=zeros(256,1);
counter_sobel=zeros(256,1);


for i=1:250
    for j=1:250
    counter_one(combine_one(i,j)+1) = counter_one(combine_one(i,j)+1)+1;

    end
end

for i=1:250
    for j=1:250
    counter_sobel(round(sobel_XY(i,j))+1) = counter_sobel(round(sobel_XY(i,j))+1)+1;

    end
end

sum=0;
sum2=0;
for  i=1:256
    if sum >= keep_one 
        one_threhold = i-1;
        break;
    else
        sum = sum +counter_one(i);
    end
end

for  i=1:256
    if sum2 >= keep_sobel 
        sobel_threhold = i-1;
        break;
    else
        sum2 = sum2 +counter_sobel(i);
    end
end
        
    

for i = 1:250
    for j = 1:250
        if combine_one(i,j) <= one_threhold
            map_one(i,j) = 0;
        else
            map_one(i, j) = 255;
        end
        if sobel_XY(i,j) <= sobel_threhold
            map_sobel(i,j) = 0;
        else
            map_sobel(i, j) = 255;
        end
    end
end

edge_maps = [map_one, map_sobel];
No14 = figure;
image(edge_maps),title('edge maps for 1X2 and sobel operation');
colormap(MAP);

%local, adaptive thresholds
p = zeros(125,125,4);
p(:,:,1) = sobel_XY(1:125, 1:125);
p(:,:,2) = sobel_XY(126:250, 1:125);
p(:,:,3) = sobel_XY(1:125, 126:250);
p(:,:,4) = sobel_XY(126:250, 126:250);
p_threshold=zeros(4,1);
local = zeros(125,125,4);
combine4 = zeros(250,250);

for i = 1:4
    p_threshold(i) = keep_pixel(p(:,:,i), 0.20);
end
for i = 1:125
    for j = 1:125
        for k = 1:4
            if p(i,j,k) <= p_threshold(k)
                local(i,j, k) = 0;
            else
                local(i, j, k) = 255;
            end
        end
    end
end
combine4(1:125, 1:125) = local(:,:,1);
combine4(126:250, 1:125) = local(:,:,2);
combine4(1:125, 126:250) = local(:,:,3);
combine4(126:250, 126:250) = local(:,:,4);
figure();
image(combine4),title('local thresholding with four pieces');
colormap(MAP);




%Q4

Gx5 = [1,2,0,-2,-1;4,8,0,-8,-4;6,12,0,-12,-6;4,8,0,-8,-4;1,2,0,-2,-1];
Gy5 = [-1,-4,-6,-4,-1; -2, -8, -12, -8, -2;0,0,0,0,0;2,8,12,8,2;1,4,6,4,1];
sobel5_XY= (zeros(250,250));
dx5_sobel= (zeros(250,250));
dy5_sobel= (zeros(250,250));


for i=3:248
    for j=3:248
        sobel_valueX=double (0);
        sobel_valueY=double(0);
        for s_r=1:5
            for s_c=1:5
                if(s_r==1 && s_c==1)
                sobel_valueX =sobel_valueX+double(intensity(i-2,j-2)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i-2,j-2)).*Gy5(s_r,s_c);
                elseif(s_r==1 & s_c==2)
                sobel_valueX =sobel_valueX+double(intensity(i-2,j-1)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i-2,j-1)).*Gy5(s_r,s_c);
                elseif(s_r==1 & s_c==3)
                sobel_valueX =sobel_valueX+double(intensity(i-2,j)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i-2,j)).*Gy5(s_r,s_c);
                elseif(s_r==1 & s_c==4)
                sobel_valueX =sobel_valueX+double(intensity(i-2,j+1)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i-2,j+1)).*Gy5(s_r,s_c);
                elseif(s_r==1 & s_c==5)
                sobel_valueX =sobel_valueX+double(intensity(i-2,j+2)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i-2,j+2)).*Gy5(s_r,s_c);                
                
                elseif(s_r==2 & s_c==1)
                sobel_valueX =sobel_valueX+double(intensity(i-1,j-2)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i-1,j-2)).*Gy5(s_r,s_c);
                elseif(s_r==2 & s_c==2)
                sobel_valueX =sobel_valueX+double(intensity(i-1,j-1)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i-1,j-1)).*Gy5(s_r,s_c);
                elseif(s_r==2 & s_c==3)
                sobel_valueX =sobel_valueX+double(intensity(i-1,j)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i-1,j)).*Gy5(s_r,s_c);
                elseif(s_r==2 & s_c==4)
                sobel_valueX =sobel_valueX+double(intensity(i-1,j+1)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i-1,j+1)).*Gy5(s_r,s_c);                
                elseif(s_r==2 & s_c==5)
                sobel_valueX =sobel_valueX+double(intensity(i-1,j+2)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i-1,j+2)).*Gy5(s_r,s_c);                
                
                elseif(s_r==3 & s_c==1)
                sobel_valueX =sobel_valueX+double(intensity(i,j-2)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i,j-2)).*Gy5(s_r,s_c);
                elseif(s_r==3 & s_c==2)
                sobel_valueX =sobel_valueX+double(intensity(i,j-1)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i,j-1)).*Gy5(s_r,s_c);
                elseif(s_r==3 & s_c==3)
                sobel_valueX =sobel_valueX+double(intensity(i,j)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i,j)).*Gy5(s_r,s_c);
                elseif(s_r==3 & s_c==4)
                sobel_valueX =sobel_valueX+double(intensity(i,j+1)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i,j+1)).*Gy5(s_r,s_c);
                elseif(s_r==3 & s_c==5)
                sobel_valueX =sobel_valueX+double(intensity(i,j+2)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i,j+2)).*Gy5(s_r,s_c); 
                
                elseif(s_r==4 & s_c==1)
                sobel_valueX =sobel_valueX+double(intensity(i+1,j-2)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i+1,j-2)).*Gy5(s_r,s_c);
                elseif(s_r==4 & s_c==2)
                sobel_valueX =sobel_valueX+double(intensity(i+1,j-1)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i+1,j-1)).*Gy5(s_r,s_c);
                elseif(s_r==4 & s_c==3)
                sobel_valueX =sobel_valueX+double(intensity(i+1,j)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i+1,j)).*Gy5(s_r,s_c);
                elseif(s_r==4 & s_c==4)
                sobel_valueX =sobel_valueX+double(intensity(i+1,j+1)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i+1,j+1)).*Gy5(s_r,s_c);
                elseif(s_r==4 & s_c==5)
                sobel_valueX =sobel_valueX+double(intensity(i+1,j+2)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i+1,j+2)).*Gy5(s_r,s_c);                
                
                              
                elseif(s_r==5 & s_c==1)
                sobel_valueX =sobel_valueX+double(intensity(i+2,j-2)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i+2,j-2)).*Gy5(s_r,s_c);
                elseif(s_r==5 & s_c==2)
                sobel_valueX =sobel_valueX+double(intensity(i+2,j-1)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i+2,j-1)).*Gy5(s_r,s_c);
                elseif(s_r==5 & s_c==3)
                sobel_valueX =sobel_valueX+double(intensity(i+2,j)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i+2,j)).*Gy5(s_r,s_c);
                elseif(s_r==5 & s_c==4)
                sobel_valueX =sobel_valueX+double(intensity(i+2,j+1)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i+2,j+1)).*Gy5(s_r,s_c);
                elseif(s_r==5 & s_c==5)
                sobel_valueX =sobel_valueX+double(intensity(i+2,j+2)).*Gx5(s_r,s_c);
                sobel_valueY =sobel_valueY+double(intensity(i,j+2)).*Gy5(s_r,s_c);              
                end
 
            end
        end
        dx5_sobel(i,j) =abs(sobel_valueX)/6;
        dy5_sobel(i,j) =abs(sobel_valueY)/6;
        sobel5_XY(i,j)= sqrt((double(dx5_sobel(i, j)).^2)+ (double(dy5_sobel(i, j)).^2));
    end
end


figure();
sobel_images=[dx5_sobel,dy5_sobel,sobel5_XY];
image(sobel_images),title('sobel 5X5 with x, sobel with y and combine sobel XY');
colormap(MAP);

%5
%RGB BANDS
RGB_x =zeros(250,250,3);
RGB_y =zeros(250,250,3);
Gx = [1, 0, -1; 2, 0, -2; 1, 0, -1];
Gy = [-1, -2, -1; 0, 0 ,0; 1, 2, 1];
for i = 1 : 3
    RGB_x(:,:,i)= abs(double(conv2(I(:,:,i),Gx, 'same')/4));
    RGB_y(:,:,i)= abs(double(conv2(I(:,:,i),Gy, 'same')/4));
end
comb_RGB = comb_XY(RGB_x,RGB_y);
figure();
together = [RGB_x,RGB_y,comb_RGB];
image(together),title('Sobel operator to each of the RGB color bands of a color image');
%thresholding RGB
COLOR_THRESHOLDING = uint8(zeros(250,250,3));
T_holding = zeros(3,1);
for i = 1:3
    T_holding(i) = keep_pixel(comb_RGB(:,:,i), 0.20);
    COLOR_THRESHOLDING(:,:,i) = Thresholdingcolor(comb_RGB(:,:,i), T_holding(i), 250, 250, 1);
end
figure();
image(COLOR_THRESHOLDING), title('Thresholding of color image');




function  [output] = comb_XY(Gx,Gy)
G = uint8(sqrt(double(Gx.^2 + Gy.^2)));
output = G;
end


% Thresholding color function
function [output] = Thresholdingcolor(image, T, rows, cols, p)
output = uint8(zeros(rows, cols,p));
for i = 1:rows
    for j = 1:cols
        if image(i, j, p)<= T
            output(i, j, p) = 0;
        else
            output(i, j, p) = 255;
        end
    end
end
end




function output = keep_pixel(image,pixle)
    counter = zeros(256,1);
    r = length(image(:,1));
    c = length(image(1, :));
    for i=1 : r
        for j=1:c
            counter(round(image(i,j))+1)= counter(round(image(i,j))+1) + 1;
        end
    end% counter of image
    counter_so_far = 0;
    result_value =0;
    pixle_size = r*c*(1-pixle);
    
      for i=1:256
        if counter_so_far >= pixle_size 
            result_value = i-1;
            break;
        else
            counter_so_far = counter_so_far + counter(i);
        end
    end
    output = result_value;
end






    
    

    










            







            
            


    