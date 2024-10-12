clc;clear all;
format long
digits(6)
x0=5;
alpha=20; %stationary mean
beta=0.001;
sigma=0.1;
param=[alpha beta sigma];
delta=1;
n=10000;



%% Question 1
rng(300)
X=generateOU(param,n,x0,delta) ;
subplot(3, 1, 1)
plot(1:n , X)
A = importdata( 'OUdata.dat' );

%% Question 2
theta_hat=LOU(param,X,delta)

%% Question 3
psi_hat=theta_hat; %step 2. obtained in Question 2
rng(100)
B=1000;   %step 3

%% i=1
Xstar=generateOU(psi_hat,n,x0,delta);  %step 4
theta_star_hat(1,:)=LOU(psi_hat,Xstar,delta); %step 5

%% i=2 to B
for i=2:B
    display(i);
    Xstar=generateOU(psi_hat,n,x0,delta);   %step 4 repeat
    theta_star_hat(i,:)=LOU(theta_star_hat(i-1,:),Xstar,delta);  %step 5 repeat
end    
%% histogram
alpha_hat=theta_star_hat(:,1);
beta_hat=theta_star_hat(:,2);
sigma_hat=theta_star_hat(:,3);

subplot(3, 3, 4)
hist(alpha_hat)
title('alpha');
subplot(3, 3, 5)
hist(beta_hat)
title('beta');
subplot(3, 3, 6)
hist(sigma_hat)
title('sigma');


%% confidence interval Question 3

lower_alpha_hat = prctile(alpha_hat,2.5);
upper_alpha_hat = prctile(alpha_hat,97.5);
formatSpec = 'CI .95 of alpha:(%4.4f , %4.4f)\n';
fprintf(formatSpec,lower_alpha_hat,upper_alpha_hat)

lower_beta_hat = prctile(beta_hat,2.5);
upper_beta_hat = prctile(beta_hat,97.5);
formatSpec = 'CI .95 of beta:(%4.4f , %4.4f)\n';
fprintf(formatSpec,lower_beta_hat,upper_beta_hat)

lower_sigma_hat = prctile(sigma_hat,2.5);
upper_sigma_hat = prctile(sigma_hat,97.5);
formatSpec = 'CI .95 of sigma:(%4.4f , %4.4f)\n';
fprintf(formatSpec,lower_sigma_hat,upper_sigma_hat)

%% Qestion 4
rng(200)
B=1000;
POPULATION=1:10; %step 1
K=10;

%% step 2 we create the matrix Bi, any rows is a block
for i=1:10
   Bi(i,:)=((i-1)*B+1):(i*B);
end    
 %% step3 resampling
   sample=randsample(POPULATION,K,1);   
  
   X_s=X(Bi(sample(1),:));    
   for i=2:K
     X_s=[X_s X(Bi(sample(i),:))];
   end 
   %% step 4 estimate
  theta_hat_star_non(1,:)=LOU(theta_hat,X_s,delta);

%% step 5, repeat step 3, 4
for j=2:B
    display(j);
   sample=randsample(POPULATION,K,true);    
   X_s=X(Bi(sample(1),:));    
   for i=2:K
     X_s=[X_s X(Bi(sample(i),:))];
   end 
    % X_boots(j,:)=X_s;  %[X_boots(j,:) Bi(sample(i))];
   theta_hat_star_non(j,:)=LOU(theta_hat_star_non(j-1,:),X_s,delta);
end    
%% histogram
alpha_hat_non=theta_hat_star_non(:,1);
beta_hat_non=theta_hat_star_non(:,2);
sigma_hat_non=theta_hat_star_non(:,3);

subplot(3, 3, 7)
hist(alpha_hat_non)
title('alpha');
subplot(3, 3, 8)
hist(beta_hat_non)
title('beta');
subplot(3, 3, 9)
hist(sigma_hat_non)
title('sigma');

%% confidence interval question 4

lower_alpha_hat_non = prctile(alpha_hat_non,2.5);
upper_alpha_hat_non = prctile(alpha_hat_non,97.5);
formatSpec = 'CI .95 of alpha:(%4.4f , %4.4f)\n';
fprintf(formatSpec,lower_alpha_hat_non,upper_alpha_hat_non)

lower_beta_hat_non = prctile(beta_hat_non,2.5);
upper_beta_hat_non = prctile(beta_hat_non,97.5);
formatSpec = 'CI .95 of beta:(%4.4f , %4.4f)\n';
fprintf(formatSpec,lower_beta_hat_non,upper_beta_hat_non)

lower_sigma_hat_non = prctile(sigma_hat_non,2.5);
upper_sigma_hat_non = prctile(sigma_hat_non,97.5);
formatSpec = 'CI .95 of sigma:(%4.4f , %4.4f)\n';
fprintf(formatSpec,lower_sigma_hat,upper_sigma_hat)



%% ####################### Bias and Variance #################


%bias alpha 
formatSpec = 'Bias alpha, parametric=%4.4f\n';
fprintf(formatSpec,sum(alpha_hat)/B - alpha);

formatSpec = 'Bias alpha, non-parametric=%4.4f\n';
fprintf(formatSpec,sum(alpha_hat_non)/B - alpha);


%variance alpha
formatSpec = 'Variance alpha, parametric=%4.4f\n';
fprintf(formatSpec,sum((alpha_hat-alpha).^2));

formatSpec = 'Variance alpha, non-parametric=%4.4f\n';
fprintf(formatSpec,sum((alpha_hat_non-alpha).^2))




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%bias betha
formatSpec = 'Bias beta, parametric=%4.4f\n';
fprintf(formatSpec,sum(beta_hat)/B - beta);

formatSpec = 'Bias beta, non-parametric=%4.4f\n';
fprintf(formatSpec,sum(beta_hat_non)/B - beta)


%variance betha
formatSpec = 'Variance beta, parametric=%4.4f\n';
fprintf(formatSpec,sum((beta_hat-beta).^2));

formatSpec = 'Variance beta, non-parametric=%4.4f\n';
fprintf(formatSpec,sum((beta_hat_non-beta).^2));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%bias sigma
formatSpec = 'Bias sigma, parametric=%4.4f\n';
fprintf(formatSpec,sum(sigma_hat)/B - sigma);

formatSpec = 'Bias sigma, non-parametric=%4.4f\n';
fprintf(formatSpec,sum(sigma_hat_non)/B - sigma);


%variance sigma
formatSpec = 'Variance sigma, parametric=%4.4f\n';
fprintf(formatSpec,sum((sigma_hat-sigma).^2));

formatSpec = 'Variance sigma, non-parametric=%4.4f\n';
fprintf(formatSpec,sum((sigma_hat_non-sigma).^2));






