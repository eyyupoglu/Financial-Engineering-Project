%% Getting the data etc.
stock_list = ["KO", 'MSFT', 'XOM', 'MCD', 'K', 'WMT', 'GE', 'F', 'EBAY', 'JPM', 'VMC', 'PKI']
stocks = hist_stock_data('01012010','01012018','KO', 'MSFT', 'XOM', 'MCD', 'K', 'WMT', 'GE', 'F', 'EBAY', 'JPM', 'VMC', 'PKI');
snp_index = hist_stock_data('01012010','01012018','SPY')
%% Calculate returns
rf = 0.02

daily_returns = zeros(2012, length(stock_list))
snp_returns = (snp_index(1).AdjClose(2:end) - snp_index(1).AdjClose(1:end-1))./snp_index(1).AdjClose(1:end-1)
for i=1:length(stock_list)
   daily_returns(:, i) = ((stocks(i).AdjClose(2:end) - stocks(i).AdjClose(1:end-1))./stocks(i).AdjClose(1:end-1))
end

%% Converting daily returns to yearly returns
SnP_mean_d = geomean(1+snp_returns)-1;
Snp_mean_a = ((1+SnP_mean_d)^252)-1;

returns = mat2dataset(daily_returns);
returns.Properties.VarNames = {'KO', 'MSFT', 'XOM', 'MCD', 'K', 'WMT', 'GE', 'F', 'EBAY', 'JPM', 'VMC', 'PKI'};

snp_returns = mat2dataset(snp_returns);
returns = [returns snp_returns];

DailyAvg = zeros(13, 1);
YearlyAvg = zeros(13, 1);
for i = 1:length(returns.Properties.VarNames)
    DailyAvg(i) = geomean(1+double(returns(:, i)))-1;
    YearlyAvg(i) = (1+DailyAvg(i))^252-1;
end
%% Linear model for betas
alpha = zeros(12, 1);
beta = zeros(12, 1);
mse = zeros(12, 1);

for i=1:12
    my_fit = LinearModel.fit(double(returns(:,i)), returns.snp_returns1)
    alpha(i) = my_fit.Coefficients.Estimate(1)
    beta(i) = my_fit.Coefficients.Estimate(2)
    mse(i) = my_fit.MSE
end

beta_dataset = array2table(transpose(beta));
beta_dataset.Properties.RowNames = {'Betas'}
beta_dataset.Properties.VariableNames = {'KO', 'MSFT', 'XOM', 'MCD', 'K', 'WMT', 'GE', 'F', 'EBAY', 'JPM', 'VMC', 'PKI'};
f = figure;
uit = uitable(f,beta_dataset);


