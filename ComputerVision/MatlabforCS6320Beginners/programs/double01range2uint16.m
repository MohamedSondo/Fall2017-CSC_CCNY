clear all; close all; clc;
%create random matrix with values in range [0 1]
D01 = rand(5)
whos D01
%convert doubles in range [0 1] to uint16
u16 = uint16(round(65535*D01))
whos u16
