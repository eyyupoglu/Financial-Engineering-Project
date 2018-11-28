%% Index Linked Bonds are removed for NS
Week38
x = yearfrac(datenum(2018,9,18), Maturity([1:2 3:9]));
y = Yield([1:2 3:9]);
[x, order] = sort(x);
y = y(order)*100;
par = nelsonfit(x,y);
p = nelsonfun(0:0.25:30,par);
figure
set(gcf,'Color','w')
plot(x,y,'-rs'); hold on
plot(0:0.25:30,p,'-g' )
title('Nelson-Siegel approximation: example')
xlabel('Maturity, years')
legend('DK yield curve (as of Sep 2018)', ...
      ['NS(\beta_{0} = '  sprintf('%3.2f',par.beta(1)) ',' ...
           '\beta_{1} = ' sprintf('%3.2f',par.beta(2)) ',' ...
           '\beta_{2} = ' sprintf('%3.2f',par.beta(3)) ',' ...
           '\tau = '      sprintf('%3.2f',par.tau)     ')'])
legend(gca,'boxoff')

%%


