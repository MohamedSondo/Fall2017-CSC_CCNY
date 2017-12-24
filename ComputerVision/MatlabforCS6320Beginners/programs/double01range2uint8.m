clear all; close all; clc;
%create random matrix with values in range [0 1]
D01 = rand(5)
whos D01
%convert doubles in range [0 1] to uint16
u8 = uint8(round(255*D01))
whos u8
