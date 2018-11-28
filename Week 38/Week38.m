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
        TimeToPayments(i-year(today)+1,1) = yearfrac(today,datenum(i,11,28),0);
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

scatter(MatYear,Yield*100)
close


