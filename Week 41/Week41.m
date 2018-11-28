close all
clear
mu = [0.1;0.2];
rho = -0.5;
Sigma = [[0.1^2 0.1*0.2*rho];[rho*0.1*0.2 0.2^2]];
std = sqrt(diag(Sigma));
rf = 0.02;
a = mu'/Sigma*mu;
b = mu'/Sigma*ones(size(mu,1),1);
c = ones(size(mu,1),1)'/Sigma*ones(size(mu,1),1);
SigmaInv = inv(Sigma);
ParabolaCoeff = [a/(a*c-b^2) -2*b/(a*c-b^2) c/(a*c-b^2)];

mu_gmv = b/c;
sigma_gmv = (1/c)^0.5;
w_gmv = (1/c)*SigmaInv*ones(size(mu,1),1);

   
mu_e = mu-ones(size(mu,1),1)*rf;
mu_e_tan = (mu_e'/Sigma*mu_e)/(ones(size(mu,1),1)'/Sigma*mu_e);
mu_tan = mu_e_tan+rf;
sigma_tan = mu_e_tan/sqrt(mu_e'/Sigma*mu_e);
w_tan = mu_e_tan/(mu_e'/Sigma*mu_e)*SigmaInv*mu_e;
slope = 1/sqrt(mu_e'/Sigma*mu_e);
w_rf = SigmaInv*mu_e/(mu_e'/Sigma*mu_e);

mu_vector = (min(mu) - range(mu)):0.001:(max(mu) + range(mu));

EF_sigma = ((c*mu_vector.^2-2*b*mu_vector + a)/(a*c-b^2)).^(1/2);
EF_sigma2 = ((c*mu_vector.^2-2*b*mu_vector + a)/(a*c-b^2));

CML = mu_vector/sqrt(mu_e'/Sigma*mu_e)-rf/sqrt(mu_e'/Sigma*mu_e);


plot(EF_sigma, mu_vector, 'b','LineWidth', 2)
hold on
plot(CML, mu_vector, 'cyan','LineWidth', 2)
xlim([0 inf]);
ylim([mu_vector(1) mu_vector(end)])
plot( sigma_tan,mu_tan,'b--X','LineWidth',2, 'MarkerSize',30,...
'MarkerEdgeColor','cyan' )
plot( std(1),mu(1),'b--X','LineWidth',2, 'MarkerSize',30,...
'MarkerEdgeColor','black' )
plot( std(2),mu(2),'b--X','LineWidth',2, 'MarkerSize',30,...
'MarkerEdgeColor','black' )
TanPF = [sigma_tan,mu_tan]
GMV_PF = [sigma_gmv,mu_gmv]
plot( sigma_gmv,mu_gmv,'b--X','LineWidth',2, 'MarkerSize',30,...
'MarkerEdgeColor','green' )
xlabel('\sigma')
ylabel('\mu_P')
