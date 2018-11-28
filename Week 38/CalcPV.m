function presentvalue = CalcPV(r,time,value)
presentvalue=0;
for i=1:size(time,1)
    presentvalue = presentvalue + value(i)/((1+r)^time(i));
end
end