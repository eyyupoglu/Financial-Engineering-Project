
stock_list = ["KO", 'MSFT', 'XOM', 'MCD', 'K', 'WMT', 'GE', 'F', 'EBAY', 'JPM', 'VMC', 'PKI']

stocks = hist_stock_data('01012010','01012018','KO', 'MSFT', 'XOM', 'MCD', 'K', 'WMT', 'GE', 'F', 'EBAY', 'JPM', 'VMC', 'PKI');

%%
%Plot all stocks
for i=1:length(stock_list)
    
   plot(stocks(i).AdjClose);
   labels{i} = (stock_list(i))
   hold on
end
legend(labels)
%% Calculate returns
daily_returns = zeros(2012, length(stock_list))
for i=1:length(stock_list)
   daily_returns(:, i) = ((stocks(i).AdjClose(2:end) - stocks(i).AdjClose(1:end-1))./stocks(i).AdjClose(1:end-1))
    
end

%% plot all

for i=1:length(stock_list)
   plot(daily_returns(:, i));
   hold on
end

%% histograms
for i=1:length(stock_list)
    subplot(4,3,i);
    
    histogram(daily_returns(:, i));
    title(stock_list(i));
end
%%
Corr = corr([daily_returns]);
VarCov = cov([daily_returns]);
heatmap(Corr)

%% Calculating Kurtosis, Skewness, Autocorrelation

for i=1:length(stock_list)
   X = daily_returns(:, i);
   Variance(i) = mean((X - mean(X)).^2);
   Kurtosis(i) = mean(((X - mean(X))/std(X)).^4);
   Skewness(i) = mean(((X - mean(X))/std(X)).^3);
   Autocorrs(i) = mean(autocorr(X));
end




