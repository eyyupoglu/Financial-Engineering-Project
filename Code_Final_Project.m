%% Data Download
close all
%clear
stock = hist_stock_data('01012010','01012018','KO','MSFT','MCD','EBAY','PKI','XOM','K','WMT','GE','F','JPM','VMC');
stock_list = ["KO",'MSFT','MCD','EBAY','PKI','XOM','K','WMT','GE','F','JPM','VMC'];
today_stock = hist_stock_data('01012018','27112018','KO','MSFT','MCD','EBAY','PKI');
today_list = ["KO",'MSFT','MCD','EBAY','PKI'];

%% Correlation
%%% Returns
DailyReturns=zeros(length(stock(1).AdjClose(2:end)),12);
AvDaRet=zeros(1,12); AvAnRet=zeros(1,12);
StDvDa=zeros(1,12); Sharpe=zeros(1,12);
for i=1:12
    DailyReturns(:,i)=(stock(i).AdjClose(2:end)-stock(i).AdjClose(1:end-1))./stock(i).AdjClose(1:end-1);
    AvDaRet(i)=geomean(1+DailyReturns(:,i))-1;
    AvAnRet(i)=((1+AvDaRet(i))^252)-1;
    StDvDa(i) = std(DailyReturns(:,i));
end
StDvAn=sqrt(252)*StDvDa;
for i=1:12
    Sharpe(i)=(AvAnRet(i)-0.02)/StDvAn(i);
end

%%% Annual Covariance Matrix and correlation
AnCov = 252*cov(DailyReturns);
Corr = corr(DailyReturns);

%% Portfolio Theory 1

%%% A matrix
mu(:,1)=AvAnRet;
mu(:,2)=ones(12,1);
A_mat=mu'/AnCov*mu;

%%% Annual Covariance Matrix and A Matrix
AnCov = 252*cov(DailyReturns);
mu(:,1)=AvAnRet;
mu(:,2)=ones(12,1);
A_mat=mu'/AnCov*mu;

%Only global minimum variance portfolio Short Selling allowed
I = ones(size(AnCov,1),1);
weight_GMV_SS = (AnCov \ I) / ((I' / AnCov) * I);
return_gmv_SS = AvAnRet * weight_GMV_SS;
StDv_gmv_SS = 1 / (I' / AnCov * I);

%%% Borrowing tangent portfolio
borrow_rate=0.1;
borr_exc_return=AvAnRet'-borrow_rate;
borr_tan_ret=(borr_exc_return'/(AnCov)*borr_exc_return)/(I'/(AnCov)*borr_exc_return);
borr_tan_stdv=sqrt(borr_exc_return'/(AnCov)*borr_exc_return)/(I'/(AnCov)*borr_exc_return);
Borr_tan_weight=AnCov\borr_exc_return*borr_tan_ret/(borr_exc_return'/(AnCov)*borr_exc_return);

%%% lending tangent Portoflio
lending_rate=0.01;
lend_exc_return=AvAnRet'-lending_rate;
lend_tan_ret=(lend_exc_return'/(AnCov)*lend_exc_return)/(I'/(AnCov)*lend_exc_return);
lend_tan_stdv=sqrt(lend_exc_return'/(AnCov)*lend_exc_return)/(I'/(AnCov)*lend_exc_return);
lend_tan_weight=AnCov\lend_exc_return*lend_tan_ret/(lend_exc_return'/(AnCov)*lend_exc_return);

%%% CML plotting + Efficient Frontier
for i=1:130
    mup(i) = 0.01*i;
    Varp(i)=[0.01*i 1]/A_mat*[0.01*i;1];
    CML_B(i)=(0.01*(i)-borrow_rate)/sqrt(borr_exc_return'/AnCov*borr_exc_return);
    CML_L(i)=(0.01*(i)-lending_rate)/sqrt(lend_exc_return'/AnCov*lend_exc_return);
end
plot(Varp.^(1/2),mup,CML_B,mup,CML_L,mup);
legend('Portfolio returns','Borrowing CML','Lending CML');

%%% Global minimum variance portfolio weights, risky only
fun = @(x)x'*AnCov*x;
x0=zeros(12,1);
X0(1)=1;
weight_GMV=fmincon(fun,x0,[],[],mu',[A_mat(1,2)/A_mat(2,2);1],zeros(12,1),ones(12,1));
Total_weight_GMV=sum(weight_GMV)

%%% Maximum return portfolio, risky only
Id_Max_Ret=find(AvAnRet==max(AvAnRet));
Weight_MaxRet_Port=zeros(12,1);
Weight_Maxret(Id_Max_Ret)=1;

% Equal weights portfolio
weight_equal=ones(12,1)/12;
Ret_equal=AvAnRet*weight_equal;
StDv_equal=weight_equal'*AnCov*weight_equal;

%% Portfolio Theory 2

% Finding dominated stocks
scatter(StDvAn,AvAnRet);
text(StDvAn, AvAnRet, stock_list, 'fontsize', 10);
xlabel('Risk');
ylabel('Return');
hold on %We remove the dominated stocks VMC, JPM, F, GE, XOM, WMT and K.
%We already put them in the end of the list of stocks so that it would be
%easier to use a increment to build the new stock data

for i=1:5
    new_stock(i)=stock(i);
end

new_DailyReturns=zeros(length(new_stock(1).AdjClose(2:end)),5);
new_AvDaRet=zeros(1,5); new_AvAnRet=zeros(1,5);
new_StDvDa=zeros(1,5);
for i=1:5
    new_DailyReturns(:,i)=(new_stock(i).AdjClose(2:end)-new_stock(i).AdjClose(1:end-1))./new_stock(i).AdjClose(1:end-1);
    new_AvDaRet(i)=geomean(1+new_DailyReturns(:,i))-1;
    new_AvAnRet(i)=((1+new_AvDaRet(i))^252)-1;
    new_StDvDa(i) = std(new_DailyReturns(:,i));
end
new_StDvAn=sqrt(252)*new_StDvDa;
new_AnCov = 252*cov(new_DailyReturns);

% GMV Portfolio, risky assets only, without dominated stocks
new_I = ones(5,1);
new_weight_GMV = (new_AnCov\new_I)/(new_I'/new_AnCov*new_I);
new_ret_GMV = new_AvAnRet*new_weight_GMV;
new_StDv_GMV = 1 / (new_I'/new_AnCov*new_I);

% Equal weights Portfolio, risky assets only, without dominated stocks
new_weight_equal=ones(5,1)/5;
new_ret_equal = new_AvAnRet * new_weight_equal;
new_StDv_equal = new_weight_equal' * new_AnCov * new_weight_equal;

hold on
scatter(new_StDvAn,new_AvAnRet,'r');

%% Portfolio performances




