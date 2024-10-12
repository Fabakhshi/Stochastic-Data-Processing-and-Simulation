function out=LOU(param,X,delta)
par_start = param;
f = @(par)negloglik(par,X,delta);
out = fminsearch(f,par_start);
end