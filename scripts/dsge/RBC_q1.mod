%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WEATHER SHOCKS: ESTIMATED MODEL WITH PHYSICAL DAMAGES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%----------------------------------------------------------------
% 0. Housekeeping (close all graphic windows)
%----------------------------------------------------------------

close all;
%----------------------------------------------------------------
% 1. Defining variables
%----------------------------------------------------------------

var	y        $y_t$        (long_name = 'output'),
        c        $c_t$        (long_name = 'consumption'),
        uc       $uc_t$       (long_name = 'marginal utility of consumption'),
        uA       $u^A_t$      (long_name = 'Marginal utility of agri labor'),
        uN       $u^N_t$      (long_name = 'Marginal utility in nonagri labor'),
        hu       $hu_t$       (long_name = 'Labor preference bundle'),
        i        $i_t$        (long_name = 'investment'),
        h        $h_t$        (long_name = 'hours worked demand'),
        v        $v_t$        (long_name = 'value function/Welfare'),
        m        $m_t$        (long_name = 'Stochastic Discount Factor'),
        u        $u_t$        (long_name = 'Utility'),
        vh       $v^h_t$      (long_name = 'Welfare term - welfare cost'),
        rer_obs  $rer^{obs}_t$ (long_name = 'real exchange rate (observed)'),
        tb       $tb_t$       (long_name = 'trade balance'),
        vc       $vc_t$       (long_name = 'Investment land'),

        y_N      $y^N_t$      (long_name = 'output in non-agriculture'),
        k_N      $k^N_t$      (long_name = 'capital in non-agriculture'),
        i_N      $i^N_t$      (long_name = 'investment in non-agriculture'),
        h_N      $h^N_t$      (long_name = 'labor in non-agriculture'),
        w_N      $w^N_t$      (long_name = 'wage in non-agriculture'),
        r        $r_t$        (long_name = 'interest rate'),
        q_N      $q^N_t$      (long_name = 'relative price of non-agricultural inv. good'),
        p_N      $p^N_t$      (long_name = 'relative price in non-agriculture'),
        d        $d_t$        (long_name = 'dividends'),
        y_A      $y^A_t$      (long_name = 'output in agriculture'),
        k_A      $k^A_t$      (long_name = 'capital in agriculture'),
        i_A      $i^A_t$      (long_name = 'investment in agriculture'),
        h_A      $h^A_t$      (long_name = 'labor in agriculture'),
        w_A      $w^A_t$      (long_name = 'wage in agriculture'),
        q_A      $q^A_t$      (long_name = 'relative price of agricultural inv. good'),
        p_A      $p^A_t$      (long_name = 'relative price in agriculture'),
        land     $\ell_t$     (long_name = 'land use'),
        x        $x_t$        (long_name = 'Land investmen'),
        y_N_obs  $y^{N,obs}_t$ (long_name = 'observed non-agricultural output'),
        gdp      $GDP_t$      (long_name = 'aggregate output'),
        NFA      $NFA_t$      (long_name = 'net foreign assets'),
        c_R      $c^R_t$      (long_name = 'rural consumption'),
        c_N      $c^N_t$      (long_name = 'non-agricultural consumption'),
        de       $de_t$       (long_name = 'exchange rate change'),
        c_star   $c^*_t$      (long_name = 'foreign consumption'),
        m_star   $m^*_t$      (long_name = 'foreign SDF'),
        uc_star  $uc^*_t$     (long_name = 'foreign marginal utility'),
        pc_A     $p^{c,A}_t$  (long_name = 'consumption price of agricultural goods'),
        pc_N     $p^{c,N}_t$  (long_name = 'consumption price of non-agricultural goods'),
        rer      $rer_t$      (long_name = 'real exchange rate'),
        r_star   $r^*_t$      (long_name = 'foreign interest rate'),
        n        $n_t$        (long_name = 'Sectoral share'),
        phi      $\phi_t$     (long_name = 'Land cost function'),
        varrho   $\varrho_t$  (long_name = 'Land shadow value'),
        ca_y     $(CA/Y)_t$   (long_name = 'current account to output ratio'),
        dyA      $\Delta y^A_t$ (long_name = 'growth in agricultural output'),
        dy       $\Delta y_t$ (long_name = 'output growth'),
        dc       $\Delta c_t$ (long_name = 'consumption growth'),
        dH       $\Delta H_t$ (long_name = 'change in hours worked'),
        welf     $W_t$        (long_name = 'welfare index'),

        y_obs    $y^{obs}_t$  (long_name = 'observed output'),
        i_obs    $i^{obs}_t$  (long_name = 'observed investment'),
        h_obs    $h^{obs}_t$  (long_name = 'observed hours'),
        smdi_obs $smdi^{obs}_t$ (long_name = 'synthetic meteorological drought index'),
        y_a_obs  $y^{A,obs}_t$ (long_name = 'observed agricultural output'),
        wy_obs   $wy^{obs}_t$ (long_name = 'weather-adjusted output'),
        c_obs    $c^{obs}_t$  (long_name = 'observed consumption'),

        e_z      $e^z_t$      (long_name = 'TFP shock'),
        e_h      $e^h_t$      (long_name = 'labor shock'),
        e_g      $e^g_t$      (long_name = 'government spending shock'),
        e_i      $e^i_t$      (long_name = 'investment-specific shock'),
        e_n      $e^n_t$      (long_name = 'sectoral shock'),
        e_s      $e^s_t$      (long_name = 'supply/weather shock'),
        e_e      $e^e_t$      (long_name = 'exchange rate or external shock');


varexo	eta_z   $ \eta^z_t $   (long_name = 'innovation to TFP shock'),
        eta_h   $ \eta^h_t $   (long_name = 'innovation to labor shock'),
        eta_g   $ \eta^g_t $   (long_name = 'innovation to government spending shock'),
        eta_i   $ \eta^i_t $   (long_name = 'innovation to investment-specific shock'),
        eta_n   $ \eta^n_t $   (long_name = 'innovation to sectoral shock'),
        eta_s   $ \eta^s_t $   (long_name = 'innovation to supply or weather shock'),
        eta_c   $ \eta^c_t $   (long_name = 'innovation to preference or consumption shock'),
        eta_e   $ \eta^e_t $   (long_name = 'innovation to exchange rate or external shock');

parameters	
        beta        $ \beta $          (long_name = 'discount factor'),
        delta_K     $ \delta_K $       (long_name = 'capital depreciation rate'),
        alpha       $ \alpha $         (long_name = 'share of capital in output'),
        sigmaC      $ \sigma_C $       (long_name = 'inverse elasticity of intertemporal substitution'),
        sigmaH      $ \sigma_H $       (long_name = 'inverse Frisch elasticity of labor supply'),
        chi         $ \chi $           (long_name = 'labor disutility parameter'),
        gy          $ g_y $            (long_name = 'government spending to output ratio'),
        b           $ b $              (long_name = 'Habits degreez'),
        chi_I       $ \chi_I $         (long_name = 'investment adjustment cost'),
        iota        $ \iota $          (long_name = 'Labor supply substitution'),
        varphi      $ \varphi $        (long_name = 'share of agricultural goods in consumption'),
        mu          $ \mu $            (long_name = 'Goods substitution in CES'),
        gamma       $ \gamma $         (long_name = 'Share of nonricardian households'),
        Hss         $ \bar{H} $        (long_name = 'Steady state Hours'),
        Tss         $ T^{ss} $         (long_name = 'Transfer nonricardian households'),

        omega       $ \omega $         (long_name = 'Land intensity in technology'),
        tau         $ \tau $           (long_name = 'Scale in land cost'),
        kappa_A     $ \kappa_A $       (long_name = 'Scale TFP in agriculture'),
        Lss         $ L^{ss} $         (long_name = 'steady-state land supply'),
        delta_L     $ \delta_L $       (long_name = 'land depreciation or adjustment cost'),
        psi         $ \psi $           (long_name = 'Land cost parameter'),

        theta1      $ \theta_1 $       (long_name = 'Weather shock elasticity lag 0'),
        theta2      $ \theta_2 $       (long_name = 'Weather shock elasticity lag 1'),
        theta3      $ \theta_3 $       (long_name = 'Weather shock elasticity lag 2'),
        theta4      $ \theta_4 $       (long_name = 'Weather shock elasticity lag 3'),

        alpha_N     $ \alpha_N $       (long_name = 'openness of non-agricultural market'),
        mu_N        $ \mu_N $          (long_name = 'Substitution CES dom vs foreign nonagri goods'),
        alpha_A     $ \alpha_A $       (long_name = 'openness of agricultural market'),
        mu_A        $ \mu_A $          (long_name = 'Substitution CES dom vs foreign agri goods'),
        chi_b       $ \chi_b $         (long_name = 'portfolio adjustment cost'),
        rho_c       $ \rho_c $         (long_name = 'persistence of foreign consumption shock'),
        sigmaC_star $ \sigma_C^* $     (long_name = 'foreign consumption elasticity'),
        b_star      $ b^* $            (long_name = 'foreign habits degree'),

        sig_z       $ \sigma_z $       (long_name = 'std. dev. of TFP shock'),
        sig_h       $ \sigma_h $       (long_name = 'std. dev. of labor shock'),
        sig_g       $ \sigma_g $       (long_name = 'std. dev. of government spending shock'),
        sig_i       $ \sigma_i $       (long_name = 'std. dev. of investment-specific shock'),
        sig_n       $ \sigma_n $       (long_name = 'std. dev. of sectoral shock'),
        sig_s       $ \sigma_s $       (long_name = 'std. dev. of weather or supply shock'),
        sig_c       $ \sigma_c $       (long_name = 'std. dev. of foreign shock'),
        sig_e       $ \sigma_e $       (long_name = 'std. dev. of exchange rate or external shock'),

        rho_z       $ \rho_z $         (long_name = 'persistence of TFP shock'),
        rho_h       $ \rho_h $         (long_name = 'persistence of labor shock'),
        rho_g       $ \rho_g $         (long_name = 'persistence of government spending shock'),
        rho_i       $ \rho_i $         (long_name = 'persistence of investment-specific shock'),
        rho_n       $ \rho_n $         (long_name = 'persistence of sectoral shock'),
        rho_s       $ \rho_s $         (long_name = 'persistence of weather or supply shock'),
        rho_e       $ \rho_e $         (long_name = 'persistence of exchange rate or external shock');

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
Hss     = 1/3; 		% ss Labor
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
	h_N		= Hss; h_A		= h_N;
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
	d = 1;
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

	theta1,				16.1,		,		,		uniform_pdf,		0,				500;

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
,mh_replic=00000,mode_compute=0,mode_file=RBC_q1_mode,load_mh_file,mh_drop=.1,load_results_after_load_mh,nodiagnostic,posterior_nograph
%,bayesian_irf,irf=20
,mh_conf_sig=0.90,conf_sig=0.90
);

% load posterior mean parameters
fnam = fieldnames(oo_.posterior_mean.parameters);
for i1 = 1:size(fnam,1)
	set_param_value(fnam{i1},eval(['oo_.posterior_mean.parameters.' deblank(fnam{i1})]));
end
% backup of params at posterior mean
params=M_.params;



