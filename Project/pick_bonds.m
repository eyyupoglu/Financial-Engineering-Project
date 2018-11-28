%% Getting the NS model parameters
clear 
% Prices from Sep 27 (or earlier if no price for Sep 27)
Price = [171.14; 111.00; 106.85; 109.00; 111.62; 106.81; 142.40; 101.62; 101.92];
Coupon = [4.5; 1.75; 0.1; 1.5; 3; 4; 7; 0.25; 0.5];
MatYear = [2039; 2025; 2023; 2023; 2021; 2019; 2024; 2020; 2027];
for i = 1:length(Price)
    Maturity(i) = datenum(MatYear(i),11,15);
end
for j = 1:length(Price)
    clearvars TimeOfPayments TimeToPayments Payment
    for i = year(today):year(Maturity(j))
        TimeOfPayments(i-year(today)+1,1) = datenum(i,11,28);
        TimeToPayments(i-year(today)+1,1) = yearfrac(today,datenum(i,11,1),0);
        Payment(i-year(today)+1,1) = Coupon(j);
    end
    Payment(end,1) = 100+Payment(end,1);
    AccruedInterest = yearfrac(datenum(year(today)-1,11,15),datenum(2018,9,21),0)*Coupon(j);
    DirtyPrice(j) = AccruedInterest + Price(j);
    Yield(j) = cfyield(Payment',TimeOfPayments',DirtyPrice(j),today); % Check with programming yourself using fzero.
    CalcPV(Yield(j),TimeToPayments,Payment)
    fun = @(x) CalcPV(x,TimeToPayments,Payment)-DirtyPrice(j);
    YieldProgrammed(j) = fzero(fun,0);
end
x = yearfrac(datenum(2018,9,18), Maturity);
y = Yield;
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



FaceValue = Price;
couponsToMaturity = Coupon;
cashFlowsDirty = zeros(length(Price), length(couponsToMaturity));
for i = 1:length(FaceValue)
    AccruedInterest = yearfrac(datenum(year(today)-1,11,15),datenum(2018,9,21),0)*Coupon(i);
    dirtyPrice(i) = AccruedInterest + Price(i);
    cashFlowsDirty(i,1) = - dirtyPrice(i);
  for j = 1:couponsToMaturity(i)
    cashFlowsDirty(i,j+1) = Coupon(i);   
    if (j == couponsToMaturity(i))
      cashFlowsDirty(i,j+1) = cashFlowsDirty(i,j+1) + FaceValue(i)  ;
    end
  end
end
%%

rates = nelsonfun(x,par);
for i=1:length(rates)
   [duration, convexity] = duration_conv(Price(1), Payment', TimeToPayments', rates(i));
   display(duration)
end
