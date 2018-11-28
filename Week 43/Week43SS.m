clear
% With no restictions to get a good starting point for the optimization
mu = [0.05 ;0.02; 0.03];
sd1 = 0.2;
sd2 = 0.1;
sd3=0.15;
Sigma =[ [sd1^2 sd1*sd2*0.5 sd1*sd3*0.7];[sd1*sd2*0.5 sd2^2 sd3*sd2*0.1];[sd1*sd3*0.7 sd3*sd2*0.1 sd3^2];];
std = diag(Sigma).^0.5;
a = mu'/Sigma*mu;
b = mu'/Sigma*ones(3,1);
c = ones(3,1)'/Sigma*ones(3,1);
SigmaInv = inv(Sigma);
ParabolaCoeff = [a/(a*c-b^2) -2*b/(a*c-b^2) c/(a*c-b^2)];
w = SigmaInv*[mu ones(3,1)]*[[c -b];[-b a]]/((a*c-b^2));

mu_gmv = b/c;
sigma_gmv = (1/c)^0.5;
w_gmv = (1/c)*SigmaInv*ones(3,1);

Aeq = ones(1,3); % weights summing to 1
beq = 1;

Aeq(2,:) = (mu'); % expected return

ub = ones(3,1); % short selling restrictions
lb = zeros(3,1); % short selling restrictions

options= optimset('Display','off');
for i=1:101
    ReqMean(i) = (max(mu)-min(mu))/100*(i-1)+min(mu);
    i;
    beq(2,1) = ReqMean(i); % setting the required return equal to the value defined above
    [w_nr(:,i),sigma2_P(i)] = fmincon(@CalcVariance,w_gmv, [],[],Aeq, beq, lb, ub, [], options, Sigma); % minimize the variance subject to constraints
    sigma_P(i) = sqrt(sigma2_P(i));
end

[ind1, ind2] = min(sigma2_P);
sigma_gmv_SS = sigma2_P(ind2).^0.5;
mu_gmv_SS = ReqMean(ind2);
w_gmv_SS = w_nr(:,ind2);

mu_vector = (min(mu) - range(mu)):0.001:(max(mu) + range(mu));
EF_sigma = ((c*mu_vector.^2-2*b*mu_vector + a)/(a*c-b^2)).^(1/2);
plot(EF_sigma,mu_vector) 
hold on
plot(sigma_P,ReqMean)


function ResVar = CalcVariance(w,Sigma)
    ResVar = w'*Sigma*w;
end







