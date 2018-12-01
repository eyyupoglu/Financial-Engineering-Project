%% Data Download
close all
clear

        %%% Choose one of the two lines, and put the other one in comment.
        %%% The first is the Portfolio Theory data, and the second is the
        %%% Portfolio Performance Data
        
        %%%All the plots are in paragraph comment, just remove it to plot
        %%%what you want to see

%stock = hist_stock_data('01012010','01012018','KO','MCD','PKI','MSFT','EBAY','XOM','K','WMT','GE','F','JPM','VMC');
stock = hist_stock_data('01012018','27112018','KO','MCD','PKI','MSFT','EBAY','XOM','K','WMT','GE','F','JPM','VMC');
stock_list = ["KO",'MCD','PKI','MSFT','EBAY','XOM','K','WMT','GE','F','JPM','VMC'];

%% Correlation
%%% Returns
DailyReturns=zeros(length(stock(1).AdjClose(2:end)),12);
AvDaRet=zeros(1,12); AvAnRet=zeros(1,12);
StDvDa=zeros(1,12); Sharpe=zeros(12,1);
Kurtosis=zeros(12,1);Skewness=zeros(12,1);
Min_Da_Ret=zeros(12,1); Max_Da_Ret=zeros(12,1);
for i=1:12
    DailyReturns(:,i)=(stock(i).AdjClose(2:end)-stock(i).AdjClose(1:end-1))./stock(i).AdjClose(1:end-1);
    Min_Da_Ret(i)=min(DailyReturns(:,i));
    Max_Da_Ret(i)=max(DailyReturns(:,i));
    AvDaRet(i)=geomean(1+DailyReturns(:,i))-1;
    AvAnRet(i)=((1+AvDaRet(i))^252)-1;
    StDvDa(i) = std(DailyReturns(:,i));
    Kurtosis(i)= mean(((DailyReturns(:,i) - mean(DailyReturns(:,i)))/std(DailyReturns(:,i))).^4);
    Skewness(i)= mean(((DailyReturns(:,i) - mean(DailyReturns(:,i)))/std(DailyReturns(:,i))).^3);
end

StDvAn=sqrt(252)*StDvDa;
for i=1:12
    Sharpe(i)=(AvAnRet(i)-0.02)/StDvAn(i);
end

%%% Annual Covariance Matrix and correlation
AnCov = 252*cov(DailyReturns);
Corr = corr(DailyReturns);

%% Portfolio Theory 1
Port_weights=zeros(12,8);
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
StDv_gmv_SS = sqrt(1 / (I' / AnCov * I));
Port_weights(:,1)=weight_GMV_SS;

%%% Borrowing tangent portfolio
borrow_rate=0.1;
borr_exc_return=AvAnRet'-borrow_rate;
borr_tan_ret=(borr_exc_return'/(AnCov)*borr_exc_return)/(I'/(AnCov)*borr_exc_return);
borr_tan_stdv=sqrt(borr_exc_return'/(AnCov)*borr_exc_return)/(I'/(AnCov)*borr_exc_return);
Borr_tan_weight=AnCov\borr_exc_return*borr_tan_ret/(borr_exc_return'/(AnCov)*borr_exc_return);
Port_weights(:,2)=Borr_tan_weight;

%%% lending tangent Portoflio
lending_rate=0.01;
lend_exc_return=AvAnRet'-lending_rate;
lend_tan_ret=(lend_exc_return'/(AnCov)*lend_exc_return)/(I'/(AnCov)*lend_exc_return);
lend_tan_stdv=sqrt(lend_exc_return'/(AnCov)*lend_exc_return)/(I'/(AnCov)*lend_exc_return);
lend_tan_weight=AnCov\lend_exc_return*lend_tan_ret/(lend_exc_return'/(AnCov)*lend_exc_return);
Port_weights(:,3)=lend_tan_weight;

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
%Total_weight_GMV=sum(weight_GMV)
gmv_Ret= AvAnRet * weight_GMV;
Port_weights(:,4)=weight_GMV;

%%% Maximum return portfolio, risky only
Id_Max_Ret=find(AvAnRet==max(AvAnRet));
Weight_MaxRet_Port=zeros(12,1);
Weight_MaxRet_Port(Id_Max_Ret)=1;
Port_weights(:,5)=Weight_MaxRet_Port;

% Equal weights portfolio
weight_equal=ones(12,1)/12;
Ret_equal=AvAnRet*weight_equal;
StDv_equal=weight_equal'*AnCov*weight_equal;
Port_weights(:,6)=weight_equal;

%% Portfolio Theory 2

% Finding dominated stocks

%{
scatter(StDvAn,AvAnRet);
text(StDvAn, AvAnRet, stock_list, 'fontsize', 10);
xlabel('Risk');
ylabel('Return');
hold on  
%}

%We remove the dominated stocks VMC, JPM, F, GE, XOM, WMT and K.
%We already put them in the end of the list of stocks so that it would be
%easier to use a increment to build the new stock data

for i=1:3
    new_stock(i)=stock(i);
end

new_DailyReturns=zeros(length(new_stock(1).AdjClose(2:end)),3);
new_AvDaRet=zeros(1,3); new_AvAnRet=zeros(1,3);
new_StDvDa=zeros(1,3);
for i=1:3
    new_DailyReturns(:,i)=(new_stock(i).AdjClose(2:end)-new_stock(i).AdjClose(1:end-1))./new_stock(i).AdjClose(1:end-1);
    new_AvDaRet(i)=geomean(1+new_DailyReturns(:,i))-1;
    new_AvAnRet(i)=((1+new_AvDaRet(i))^252)-1;
    new_StDvDa(i) = std(new_DailyReturns(:,i));
end
new_StDvAn=sqrt(252)*new_StDvDa;
new_AnCov = 252*cov(new_DailyReturns);

% GMV Portfolio, risky assets only, without dominated stocks
new_I = ones(3,1);
new_weight_GMV = (new_AnCov\new_I)/(new_I'/new_AnCov*new_I);
new_ret_GMV = new_AvAnRet*new_weight_GMV;
new_StDv_GMV = 1 / (new_I'/new_AnCov*new_I);
for i=1:3
    Port_weights(i,7)=new_weight_GMV(i);
end

% Equal weights Portfolio, risky assets only, without dominated stocks
new_weight_equal=ones(3,1)/3;
new_ret_equal = new_AvAnRet * new_weight_equal;
new_StDv_equal = new_weight_equal' * new_AnCov * new_weight_equal;
for i=1:3
Port_weights(i,8)=new_weight_equal(i);
end
%{
hold off
scatter(new_StDvAn,new_AvAnRet,'r');
%}
%% Plotting Portfolio Performance

%{
date_span=length(DailyReturns(:,1))+1;
monney=zeros(date_span,8);
monney(1,:)=100;
dates=datenum(stock(1).Date,'yyyy-mm-dd');
hold off
for j=1:8
    for i=2:date_span
        PortDaReturns(i,j)=DailyReturns(i-1,:)*Port_weights(:,j);
        monney(i,j)=monney(i-1,j)*(1+PortDaReturns(i,j));
    end  
    plot(dates,monney(:,j));
    hold on
end
legend('GMV SS','Borr Tan', 'Lend Tan','GMV no SS','Max Return','Equal weight','GMV SS 3 assets','Equal weights 3 assets');
%}


