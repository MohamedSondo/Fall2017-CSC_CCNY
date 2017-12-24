% Mohamed Sondo 
%ID 6423


imgl = imread('pic410.bmp');
imgr = imread('pic430.bmp');
[ROWS COLS CHANNELS] = size(imgl);
disimg = [imgl imgr];
image(disimg);
Nc = 12;
Nt = 4;

 %load pl.mat pl;
 %load pr.mat pr;

cnt = 1;
hold;

while(cnt <= Nc+Nt)

dR = 50;
dC = 50;

[X, Y] = ginput(1);
Cl = X(1); Rl = Y(1);
pl(cnt,:) = [Cl Rl 1];



Cl= pl(cnt,1);  Rl=pl(cnt,2); 
rectangle('Curvature', [0 0], 'Position', [Cl Rl dC dR]);


[X, Y] = ginput(1);
Cr = X(1); Rr = Y(1);
pr(cnt,:) = [Cr-COLS Rr 1];


Cr=pr(cnt,1)+COLS; Rr=pr(cnt,2);
rectangle('Curvature', [0 0], 'Position', [Cr Rr dC dR]);
plot(Cr+COLS,Rr,'r*');
drawnow;
display(pl);
display(pr);
cnt = cnt+1;
end
save pr.mat pr;
save pl.mat pl;


[a b] = size(pl);

for i=1:a
  
    x1 = pl(i,1);
    y1 = pl(i,2);
    x2 = pr(i,1);
    y2 = pr(i,2);
    A(i,:) = [x1*x2 y1*x2 x2 x1*y2 y1*y2 y2 x1 y1 1];
    
end
[U D V] = svd(A);

f = V(:,9);
F = [f(1) f(2) f(3); f(4) f(5) f(6); f(7) f(8) f(9)];

[FU FD FV]= svd (F);
FDnew = FD;
FDnew(3,3) = 0;
FM = FU*FDnew*FV';

F = FM;
display(F);





for cnt=1:1:Nc+Nt,
  an = F*pl(cnt,:)';
  x = 0:COLS; 
  y = -(an(1)*x+an(3))/an(2);

  x = x+COLS;
  plot(pl(cnt,1),pl(cnt,2),'r#');
  line(x,y,'Color', 'r');
  [X, Y] = ginput(1); 
  plot(pr(cnt,1),pr(cnt,2),'b#SS');
  line(x,y,'Color', 'b');

end 


save pr.mat pr;
save pl.mat pl;
save F.txt F -ASCII



figure(1);    
[left_x left_y] = ginput(1);
hold on;
plot(left_x,left_y,'r#SSSSS');

left_P = [left_x; left_y; 1];

right_P = FM*left_P;

right_epipolar_x=1:2*ROWS;
right_epipolar_y=(-right_P(3)-right_P(1)*right_epipolar_x)/right_P(2);
figure(2);
hold on;
plot(right_epipolar_x,right_epipolar_y,list(mod(i,8)+1));

left_epipole = FV(:,3);
left_epipole = left_epipole/left_epipole(3);

left_epipolar_x = 1:2*ROWS;
left_epipolar_y = left_y + (left_epipolar_x-left_x)*(left_epipole(2)-left_y)/(left_epipole(1)-left_x);
figure(1);
hold on;
plot(left_epipolar_x,left_epipolar_y,list(mod(i,8)+1));


save eR.txt eRv -ASCII; 
save eL.txt eRv -ASCII; 

