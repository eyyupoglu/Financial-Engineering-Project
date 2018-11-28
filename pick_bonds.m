%% Getting the Yield Curve
clear 
% Price = [108.485; 110.41; 171.25; 104.613; 142.340;110.980;101.702;111.680;101.720;106.998];
% Coupon = [1.5; 3; 4.5; 4; 7; 0.1; 0.25; 1.75;0.5;0.1];
% MatYear = [2023; 2021; 2039; 2019; 2024 ;2030; 2020;2025;2027;2023];

Price = [171.25; 111.65;  109.00; 111.62; 104.54; 142.40; 101.62; 101.92];
Coupon = [4.5; 1.75;  1.5; 3; 4; 7; 0.25; 0.5];
MatYear = [2039; 2025;  2023; 2021; 2019; 2024; 2020; 2027];
for i = 1:length(Price)
    Maturity(i) = datenum(MatYear(i),11,15);
end
clearvars TimeOfPayments TimeToPayments Payment
TimeOfPayments = zeros(length(Price), length((year(today) +1):year(Maturity(1))));
TimeToPayments = zeros(length(Price), length((year(today) +1):year(Maturity(1))));
CashFlowsDirty = zeros(length(Price), length((year(today) +1):year(Maturity(1))));
DirtyPrice = zeros(length(Price), 1);
PVs = zeros(length(Price), 1);
for j = 1:length(Price)
    for i = (year(today) +1):year(Maturity(j))
        TimeOfPayments(j, i-year(today)+1) = datenum(i,11,15);
        TimeToPayments(j, i-year(today)+1) = yearfrac(today,datenum(i,11,1),0);
        CashFlowsDirty(j, i-year(today)+1) = Coupon(j);
    end
    CashFlowsDirty(j, year(Maturity(j)) - year(today) +1 ) = 100+Coupon(j, end);
    AccruedInterest = yearfrac(datenum(year(today),11,15),today,0)*Coupon(j);
    DirtyPrice(j) = AccruedInterest + Price(j);
    CashFlowsDirty(j, 1) =  - DirtyPrice(j);
    
    end_ = year(Maturity(j)) - year(today) + 1;
    Yield(j) = cfyield(CashFlowsDirty(j,2:end_),TimeOfPayments(j, 2:end_),DirtyPrice(j),today); % Check with programming yourself using fzero.
%     PVs(j, 1) = CalcPV(Yield(j), TimeToPayments(j, 2:end), CashFlowsDirty(j,2:end));
%     fun = @(x) CalcPV(x,TimeToPayments,CashFlowsDirty)-DirtyPrice(j);
%     YieldProgrammed(j) = fzero(fun,0);
end


x = yearfrac(datenum(today), Maturity);
Yield = Yield;
[x, order] = sort(x);
Yield = Yield(order)*100;
par = nelsonfit(x,Yield);
NSRates = nelsonfun(x,par);
figure
set(gcf,'Color','w')
plot(x,Yield,'-rs'); hold on
plot(x,NSRates,'-g' )
title('Nelson-Siegel approximation: example')
xlabel('Maturity, years')
legend('DK yield curve (as of Sep 2018)', ...
      ['NS(\beta_{0} = '  sprintf('%3.2f',par.beta(1)) ',' ...
           '\beta_{1} = ' sprintf('%3.2f',par.beta(2)) ',' ...
           '\beta_{2} = ' sprintf('%3.2f',par.beta(3)) ',' ...
           '\tau = '      sprintf('%3.2f',par.tau)     ')'])
legend(gca,'boxoff')




%%

r_dur = NSRates/100;

duration = zeros(length(r_dur), 1);
convexity = zeros(length(r_dur), 1);
for i=1:length(Yield)
   [duration(i, 1), convexity(i, 1)] = duration_conv(DirtyPrice(i),...
       CashFlowsDirty(i,2:end), TimeToPayments(i, 2:end), r_dur(i));
end

%% Choosing 3 bonds with duration 10



A = [-1     0     0
     0     -1     0
     0     0     -1];
 
B = [-0.1
     -0.1
     -0.1];


Aeq = [duration(1:3)'
        1     1     1];


Beq = [10
        1];

f = [0.5 -4 1];

optimal_weights = linprog(f, A, B, Aeq, Beq);

portfolio_p = optimal_weights' * Price(1:3);
portfolio_c = optimal_weights' * convexity(1:3);
portfolio_d = optimal_weights' * duration(1:3);

delta_r = 0.01;
percent_change = - portfolio_d * delta_r + 0.5 * portfolio_c * (delta_r)^2;
price_change = portfolio_p * percent_change;

%% Plotting the curve change plot advanced method!!!

yields = 0:0.01:1;
prices = zeros(length(0:0.1:1), 1);
percent_change_list = zeros(length(0:0.1:1), 1);
for i = 2:length(yields)-1
    delta_r = yields(i) - yields(i-1);
    percent_change(i) = - portfolio_d * delta_r + 0.5 * portfolio_c * (delta_r)^2;
    prices(i) = portfolio_p * percent_change(i);
end

plot(prices)










