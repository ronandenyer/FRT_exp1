function model = fit_model(RT,hit)%,fixed_params)
% fits selection model to forced RT data by max. likelihood
%
% Inputs:
%       RT - vector of reaction times for each trial (in s)
%       hit - vector of responses (1 = hit, 0 = miss)
%       fixed_params - parameters to be fixed. Vector of parameters,
%       containing fixed values. Use NaN to keep a parameter open.
%       
% Outputs:
%       params - optimized parameters
%       presponse - time-varying response probabilities
%
%
% Model parametrization:
%       params = [ [mu_i sigma_i] - mean and variability of RT
%                                   distribution for each process
%                   qN            - asymptotic error for last process
%                   qInit         - lower asymptotic error
%                   rho ]         - habit strength
%

        % if some parameters have been specified to be fixed, set these
        if(nargin<4)
            fixed_params = NaN*[1 1 1 1]; % all parameters open unless specified
        end
        paramsInit = [.15 .05 .25 .95];
        
        % [mu (mean time), sigma (slope), qI (lower asymptote), qA (upper asymptote)]
        LB = [0 .01 .1 .5]; % lower bound on parameters
        UB = [.5 10 .4 1]; % upper bound on parameters
        
        A = []; B = []; % inequality constraints
Aeq = []; Beq = [];


% weed out bad trials
good_trials = ~isnan(RT);
good_trials = good_trials(good_trials>-1);
RT = RT(good_trials);
hit = hit(good_trials);

% create function handle to calculate the likelihood for each trial
logLikelihood = @(params) -sum(hit.*log(params(3) + (params(4)-params(3))*normcdf(RT,params(1),params(2))) + (1-hit).*log(1-params(3)+(params(3)-params(4))*normcdf(RT,params(1),params(2))));


%LLall = hit.*log((1/8+AE*normcdf(RT,mu,sigma)*7/8)) + (1-hit).*log(1-(1/8+AE*normcdf(RT,mu,sigma)*7/8));

%like_fun = @(params) habit_lik(RT,response,params);

% make 
%

% calculate initial log-likelihood
%model.nLL_init = like_fun(paramsInit);


%[model(m).paramsOpt(subject,:,c), model(m).LLopt(c,subject)] = fmincon(like_fun,paramsInit,A,B,Aeq,Beq,LB,UB);
[model.paramsOpt, model.LLopt] = fmincon(logLikelihood,paramsInit,A,B,Aeq,Beq,LB,UB);

model.t_plot = [0:.001:.5];
model.p_hit_plot = model.paramsOpt(3) + (model.paramsOpt(4)-model.paramsOpt(3))*normcdf(model.t_plot,model.paramsOpt(1),model.paramsOpt(2));

model.nLL_init = logLikelihood(paramsInit);
model.nLL = logLikelihood(model.paramsOpt); % negative log-likelihood for optimized model
                

%{
% plot fit
data_sw = sliding_window(RT,2-response,model.xplot,.05)
figure(1); clf; hold on
plot(model.xplot,data_sw)
plot(model.xplot,model.presponse(1,:))
%}
