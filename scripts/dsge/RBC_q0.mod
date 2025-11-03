
%----------------------------------------------------------------
% 0. Housekeeping (close all graphic windows)
%----------------------------------------------------------------

close all;
%----------------------------------------------------------------
% 1. Defining variables
%----------------------------------------------------------------

var c uc uA uN hu i y h v m u vh rer_obs tb vc
	y_N k_N i_N h_N w_N r q_N p_N d
	y_A k_A i_A h_A w_A r q_A p_A land x y_N_obs
	gdp NFA 
	c_R c_N de
	c_star m_star uc_star pc_A pc_N rer r_star n
	phi varrho ca_y dyA dy dc dH welf
	y_obs i_obs h_obs smdi_obs y_a_obs wy_obs c_obs
	e_z e_h e_g e_i e_n e_s e_e;
varexo eta_z eta_h eta_g eta_i eta_n eta_s eta_c eta_e;


parameters	beta delta_K alpha sigmaC sigmaH chi gy b chi_I iota varphi mu 
			gamma Tss
			omega tau kappa_A Lss delta_L psi theta1 theta2 theta3 theta4 alpha_N mu_N alpha_A mu_A chi_b rho_c sigmaC_star b_star
			sig_z sig_h sig_g sig_i sig_n sig_s sig_c sig_e
			% shocks
			rho_z rho_h rho_g rho_i rho_n rho_s rho_e ;

%----------------------------------------------------------------
% 2. Calibration
%----------------------------------------------------------------

alpha   = 0.33;		% share of capital in ouput
beta    = .9883;	% discount factor
delta_K = 0.025;	% depreciation of capital
sigmaC	= 1.5;		% risk aversion consumption
sigmaH	= 2;		% labor disutility
gy		= 0.22;		% share of public spendings in gdp
b		= 0.7;		% habits in consumption
chi_I	= 4;		% AC on investment
iota	= 1;		% substitution cost across labor types
Lss		= .4;		% ss land per capita
psi		= 1.5;		% shape of the land cost function
varphi	= 0.15;		% share of agricultural goods in consumption
mu		= 2.5;		% substitution non-agriculture vs agriculture
omega	= 0.15;		% share of land in agriculture
delta_L	= 0.10;		% land decay rate
theta1	= 0;		% damage current
theta2	= 0;		% damage lag 1
theta3	= 0;		% damage lag 2
theta4	= 0;		% damage lag 3
alpha_N	= .25;		% share of imported goods non agricultural
alpha_A	= .45;		% share of imported goods agricultural
mu_N	= .3;		% substitution non agricultural
mu_A	= .3;		% substitution agricultural
chi_b	= 0.0007;	% portfolio adjustment cost
rho_c	= .8;		% persistence foreign consumption
sigmaC_star = 1;	% risk aversion foreign
b_star = 0;			% foreign habits in consumption
gamma	= 0; 		% share of non ricardian

% shock persistence
rho_z   = 0.95; 
rho_h   = 0.95; 
rho_g   = 0.95; 
rho_i   = 0.95; 
rho_n   = 0.38; 
rho_s   = 0.38; 
rho_e   = 0.00; 
% shock std
sig_z	= 0.00; 
sig_h	= 0.00; 
sig_g	= 0.00; 
sig_i 	= 0.00;
sig_n 	= 0.00;
sig_s	= 0.00;
sig_c	= 0.00;
sig_e	= 0.00;


%----------------------------------------------------------------
% 3. Model
%----------------------------------------------------------------	
model; 
	%% Household
	[name='FOC home asset']
	m(+1)*r=1;
	[name='FOC foreign asset']
	beta*uc(+1)/uc*rer(+1)*r_star = rer*(1+p_N*chi_b*NFA);
	[name='labor disutility index']
	hu = (h_N^(1+iota)+h_A^(1+iota))^(1/(1+iota));
	[name='FOC h_N']
	w_N*uc = uN;
	[name='FOC h_A']
	w_A*uc = uA;
	[name='welfare']
	v = u + beta*v(+1);
	[name='utility']
	u = (c-b*c(-1))^(1-sigmaC)/(1-sigmaC)-chi/(1+sigmaH)*hu^(1+sigmaH);
	[name='Welfare loss auxiliary variable to convert welfare into unconditional consumption']
	vh = chi/(1+sigmaH)*hu^(1+sigmaH) + beta*vh(+1);
	[name='stochastic discount factor']
	m = beta*uc/uc(-1);
	[name='Marginal utility of consumption']
	uc = (c_R-b*c(-1))^-sigmaC;
	[name='Marginal disutility of labor h_N']
	uN = e_h*chi*hu^sigmaH*(h_N/hu)^iota;
	[name='Marginal disutility of labor h_A']
	uA = e_h*chi*hu^sigmaH*(h_A/hu)^iota;

	%% Capital supply
	[name='No arbitrage Bonds-Capital']
	1 = m(+1)*(alpha*p_N(+1)*y_N(+1)/k_N+(1-delta_K)*q_N(+1))/q_N;
	1 = m(+1)*(alpha*(1-omega)*p_A(+1)*y_A(+1)/k_A+(1-delta_K)*q_A(+1))/q_A;
	[name='Capital law of motion']
	i_N = k_N-(1-delta_K)*k_N(-1);
	i_A = k_A-(1-delta_K)*k_A(-1);
	[name='investment equation']
	q_N = p_N + chi_I*e_i*(e_i*i_N/i_N(-1)-1) - m(+1)*0.5*chi_I*((e_i(+1)*i_N(+1)/i_N)^2-1);
	q_A = p_N + chi_I*e_i*(e_i*i_A/i_A(-1)-1) - m(+1)*0.5*chi_I*((e_i(+1)*i_A(+1)/i_A)^2-1);

	% Firms
	[name='Production y_N']
	y_N = e_z*(k_N(-1)^alpha)*(h_N^(1-alpha));
	[name='Production y_A']
	y_A = (d*land(-1))^omega * (e_z*kappa_A*k_A(-1)^alpha*h_A^(1-alpha))^(1-omega);
	[name='labor cost minimization N']
	w_N = (1-alpha)*p_N*y_N/h_N;
	[name='labor cost minimization A']
	w_A = (1-omega)*(1-alpha)*p_A*y_A/h_A;
	
	[name='FOC land']
	varrho = m(+1)*(omega*p_A(+1)*y_A(+1)/land + ((1-delta_L)*d(+1)*varrho(+1)+phi(+1)/land));
	[name='FOC x']
	p_N	= tau*x^(psi-1)*varrho*d*land(-1);
	[name='land cost']
	phi = tau/(psi)*x^(psi)*d*land(-1);
	vc = x;


	[name='Land']
	land = (1-delta_L)*d*land(-1) + phi;
	[name='Damage function']
	d = e_s^-theta1 * e_s(-1)^-theta2*e_s(-2)^-theta2*e_s(-3)^-theta3*e_s(-4)^-theta4;

	%%% AGGREGATION
	[name='total prod']
	y = (1-n)*p_N*y_N + n*p_A*y_A;
	i = (1-n)*i_N + n*i_A;
	h = (1-n)*h_N + n*h_A;
	[name='Reallocation shock']
	n = steady_state(n)*e_n;
	[name='Ressources Constraint']
	(1-n)*y_N 	= (1-varphi)*( (1-alpha_N)*(p_N/pc_N)^-mu_N*pc_N^-mu*c + alpha_N*(p_N/rer)^-mu_N*c_star) + (1-n)*(i_N+i_N(-1)*(chi_I/2)*(e_i*i_N/i_N(-1)-1)^2) + n*(i_A+i_A(-1)*(chi_I/2)*(e_i*i_A/i_A(-1)-1)^2) + n*vc + gy*steady_state(y_N)*e_g + 0.5*chi_b*rer*NFA^2;
	n*y_A 		= varphi*    ( (1-alpha_A)*(p_A/pc_A)^-mu_A*pc_A^-mu*c + alpha_A*(p_A/rer)^-mu_A*c_star);
	[name='Ricardian definitions']
	c = gamma*c_N + (1-gamma)*c_R;
	c_N + Tss = n*w_A*h_A + (1-n)*w_N*h_N;
	[name='Gross Domestic Product']
	gdp = y - n*p_N*vc;
	
	%%% INTERNATIONAL DEFINITIONS
	[name='Price indexes']
	pc_N^(1-mu_N) = (1-alpha_N) * p_N^(1-mu_N) + alpha_N * rer^(1-mu_N);
	pc_A^(1-mu_A) = (1-alpha_A) * p_A^(1-mu_A) + alpha_A * rer^(1-mu_A);
	[name='net foreign assets']
	NFA = rer/rer(-1)*r_star(-1)*NFA(-1) + tb;
	[name='trade balance']
	tb = pc_N*(1-varphi)*( (1-alpha_N)*(p_A/pc_N)^-mu_N*pc_N^-mu*c + alpha_N*(p_N/rer)^-mu_N*c_star ) + pc_A*varphi*( (1-alpha_A)*(p_A/pc_A)^-mu_A*pc_A^-mu*c + alpha_A*(p_A/rer)^-mu_A*c_star) - c; 
	[name='relative price']
	1 = (1-varphi) * pc_N^(1-mu) + varphi * pc_A^(1-mu);
	
	%%% FOREIGN BLOCK
	[name='net foreign assets']
	m_star = beta*uc_star/uc_star(-1);
	uc_star = e_e*(c_star-b_star*c_star(-1))^-sigmaC_star;
	m_star(+1)*r_star=1;
	log(c_star) = (1-rho_c)*log(STEADY_STATE(c)) + rho_c*log(c_star(-1)) + sig_c*eta_c/100;


	[name='Observables']
	y_obs		= 100*log(gdp/STEADY_STATE(gdp));
	i_obs		= 100*log(p_N*i/STEADY_STATE(i));
	h_obs		= 100*log(h/STEADY_STATE(h));
	c_obs		= 100*log(c/STEADY_STATE(c));
	smdi_obs	= 100*log(e_s/STEADY_STATE(e_s));
	y_a_obs		= 100*log(p_A*n*y_A/STEADY_STATE(y_A)/steady_state(n));
	wy_obs		= 100*log(c_star/STEADY_STATE(c_star));
	rer_obs		= 100*log(1/de);
	y_N_obs		= 100*log(p_N*(1-n)*y_N/(STEADY_STATE(y_N)*(1-steady_state(n))));

	[name='current account']
	ca_y		= (NFA-NFA(-1))/(4*y);
	[name='Shock decomposition']
	dyA		= y_a_obs-y_a_obs(-4);
	dy		= y_obs-y_obs(-4);
	dc		= c_obs-c_obs(-4);
	dH		= h_obs - h_obs(-4);
	[name='welfare']
	welf	= u+beta*welf(+1);
	[name='Expected rer']
	de			= rer(+1)/rer;
	
	
	[name='shocks']
	log(e_z) = rho_z*log(e_z(-1))+sig_z*eta_z/100;
	log(e_h) = rho_h*log(e_h(-1))+sig_h*eta_h/100;
	log(e_g) = rho_g*log(e_g(-1))+sig_g*eta_g/100;
	log(e_i) = rho_i*log(e_i(-1))+sig_i*eta_i/100;
	log(e_n) = rho_n*log(e_n(-1))+sig_n*eta_n/100;
	log(e_s) = rho_s*log(e_s(-1))+sig_s*eta_s/100;
	log(e_e) = rho_e*log(e_e(-1))+sig_e*eta_e/100;
end;

%----------------------------------------------------------------
% 4. Computation
%----------------------------------------------------------------

steady_state_model;
	e_z		= 1; e_h = 1; e_g = 1; e_i = 1; e_n = 1; e_s = 1; e_e = 1;
	r		= 1/beta;
	m 		= beta;
	h_N		= 1/3; h_A		= h_N;
	hu 		= (h_N^(1+iota)+h_A^(1+iota))^(1/(1+iota));
	p_N		= 1;	p_A		= 1;
	q_N		= 1;	q_A		= 1;
	z		= (r-(1-delta_K));
	k_N		= h_N*(z/alpha)^(1/(alpha-1));
	y_N		= k_N^alpha*h_N^(1-alpha);
	i_N		= delta_K*k_N;
	w_N		= (1-alpha)*y_N/h_N;	
	w_A		= w_N;	
	y_A		= h_A*w_A/((1-omega)*(1-alpha));
	k_A		= alpha*(1-omega)/z*y_A;
	land	= Lss;
	phi		= delta_L*land;
	varrho	= (omega*y_A/land + phi/land)/(1/m-(1-delta_L));
	x		= psi*phi*varrho;
	tau		= 1/(x^(psi-1)*varrho*land);
	vc = x;
	kappa_A = (y_A/(land^omega*(k_A^alpha*h_A^(1-alpha))^(1-omega)))^(1/(1-omega));
	i_A		= delta_K*k_A;
	n		= ((1-gy)*y_N-i_N)/((1-varphi)/varphi*y_A+y_N-i_N+i_A+vc);
	c		= n*y_A/varphi;	
	c_star 	= c; 
	y 		= (1-n)*p_N*y_N + n*p_A*y_A;
	i 		= (1-n)*i_N + n*i_A;
	h 		= (1-n)*h_N + n*h_A;
	gdp 	= y - n*vc;
	pc_A = 1; pc_N = 1; rer = 1; NFA = 0; r_star = r;
	y_obs 	= 0; c_obs 	= 0; i_obs = 0; h_obs = 0; smdi_obs = 0; y_a_obs = 0; wy_obs = 0; y_N_obs = 0;
	m_star 	= beta;
	rer_obs	= 0; tb = 0; ca_y = 0;
	d = 1; dyA = 0;
	c_N		= c; c_R = c;	
	uc 		= ((c-b*c)^-sigmaC);
	uc_star = (c_star-b_star*c_star)^-sigmaC_star;
	chi 	= w_N*uc/(hu^sigmaH*(h_N/hu)^iota);
	uN 		= e_h*chi*hu^sigmaH*(h_N/hu)^iota;
	uA 		= e_h*chi*hu^sigmaH*(h_A/hu)^iota;
	vh 		= chi/(1+sigmaH)*hu^(1+sigmaH)/(1-beta);
	u		= (c-b*c)^(1-sigmaC)/(1-sigmaC)-chi/(1+sigmaH)*hu^(1+sigmaH);
	v 		= u/(1-beta);
	Tss		= n*w_A*h_A + (1-n)*w_N*h_N - c_R;
	de		= 1;
	welf	= u/(1-beta);
	dyA = 0; dy = 0; dc = 0; dH = 0;
end;

M_.Sigma_e = eye(M_.exo_nbr);

resid;
%steady;
check;

varobs    smdi_obs y_a_obs wy_obs y_obs h_obs c_obs i_obs rer_obs;

estimated_params;
//	PARAM NAME,		INITVAL,		LB,		UB,		PRIOR_SHAPE,		PRIOR_P1,		PRIOR_P2,		PRIOR_P3,		PRIOR_P4,		JSCALE
	rho_z,				0.33,		,		,		beta_pdf,			.5,           	0.2;
	rho_h,				0.97,		,		,		beta_pdf,			.5,           	0.2;
	rho_g,				0.90,		,		,		beta_pdf,			.5,           	0.2;
	rho_i,				0.55,		,		,		beta_pdf,			.5,           	0.2;
	rho_n,				0.85,		,		,		beta_pdf,			.5,           	0.2;
	rho_s,				0.41,		,		,		beta_pdf,			.5,           	0.2;
	rho_e,				0.31,		,		,		beta_pdf,			.5,           	0.2;
	rho_c,				0.92,		,		,		beta_pdf,			.5,           	0.2;

	sig_z,				2.00,		,		,		weibull_pdf,		1,				2;
	sig_h,				4.20,		,		,		weibull_pdf,		1,				2;
	sig_g,				4.4,		,		,		weibull_pdf,		1,				2;
	sig_i,				7.2,		,		,		weibull_pdf,		1,				2;
	sig_n,				7.4,		,		,		weibull_pdf,		1,				2;
	sig_s,				.75,		,		,		weibull_pdf,		1,				2;
	sig_e,				4.57,		,		,		weibull_pdf,		1,				2;
	sig_c,				.66,		,		,		weibull_pdf,		1,				2;

%	theta1,				16.1,		,		,		uniform_pdf,		0,				500;
%	theta2,				8.1,		,		,		uniform_pdf,		0,				500;

	sigmaC,				1.50,		,		,		normal_pdf,			1,          .35;
	sigmaH,				3.01,		,		,		normal_pdf,			2,          .75;
	psi,				1.41,		,		,		normal_pdf,			1,            1;
	omega,				0.14,		,		,		beta_pdf,			0.2,        0.08;
	delta_L,			0.03,		,		,		beta_pdf,			.25,         .1;
	b,					0.65,		,		,		beta_pdf,			.7,          .1;
	iota,				1.9,		,		,		normal_pdf,			1,            0.75;
	chi_I,				0.95,		,		,		normal_pdf,			4,            1.5;
	mu,					5.85,		,		,		gamma_pdf,			2,           1;
	mu_A,				1.30,		,		,		gamma_pdf,			2,           1;
	mu_N,				1.1,		,		,		gamma_pdf,			2,           1;
end;

estimation(
order=1,
datafile='dataNZ_cubic_trend.m',presample=29,first_obs=1
,prefilter=1,plot_priors=0,lik_init=2,mh_nblocks=4,mh_jscale=0.38
%,mode_compute=5,mh_replic=00000
,mh_replic=00000,mode_compute=0,mode_file=RBC_q0_mode%,load_mh_file,mh_drop=.1,load_results_after_load_mh,nodiagnostic,posterior_nograph
%,bayesian_irf,irf=20
,mh_conf_sig=0.90,conf_sig=0.90
);
