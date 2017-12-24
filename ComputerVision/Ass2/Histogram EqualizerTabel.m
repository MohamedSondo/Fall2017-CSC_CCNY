figure('Position',get(0,'screensize'));

dat=cell(256,6);


for i=1:256

dat(i,:)={i,freq(i),probf(i),cum(i),probc(i),output(i)};   

end


   columnname =   {'Bin', 'Histogram', 'Probability', 'Cumulative histogram','CDF','Output'};

columnformat = {'numeric', 'numeric', 'numeric', 'numeric', 'numeric','numeric'};

columneditable =  [false false false false false false];

t = uitable('Units','normalized','Position',...

            [0.1 0.1 0.4 0.9], 'Data', dat,...

            'ColumnName', columnname,...

            'ColumnFormat', columnformat,...

            'ColumnEditable', columneditable,...

            'RowName',); 

    subplot(2,2,2); bar(GIm);

    title('Before Histogram equalization');

    subplot(2,2,4); bar(HIm);

    title('After Histogram equalization');
