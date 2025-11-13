
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
        h        $h_t$        (long_name = 'hours worked demand'),
        m        $m_t$        (long_name = 'Stochastic Discount Factor'),

        y_N      $y^N_t$      (long_name = 'output in non-agriculture'),
        h_N      $h^N_t$      (long_name = 'labor in non-agriculture'),
        w_N      $w^N_t$      (long_name = 'wage in non-agriculture'),
        r        $r_t$        (long_name = 'interest rate'),
        p_N      $p^N_t$      (long_name = 'relative price in non-agriculture'),
        d        $d_t$        (long_name = 'dividends'),
        y_A      $y^A_t$      (long_name = 'output in agriculture'),
        h_A      $h^A_t$      (long_name = 'labor in agriculture'),
        w_A      $w^A_t$      (long_name = 'wage in agriculture'),
        p_A      $p^A_t$      (long_name = 'relative price in agriculture'),
        land     $\ell_t$     (long_name = 'land use'),
        x        $x_t$        (long_name = 'Land investmen'),
        gdp      $GDP_t$      (long_name = 'aggregate output'),
        n        $n_t$        (long_name = 'Sectoral share'),
        phi      $\phi_t$     (long_name = 'Land cost function'),
        varrho   $\varrho_t$  (long_name = 'Land shadow value'),
 
        r        $r_t$        (long_name = 'Nominal interest rate'),
        pi       $\pi_t$      (long_name = 'Aggregate inflation'),
        pi_A     $\pi^A_t$    (long_name = 'Inflation in agriculture'),
        pi_N     $\pi^N_t$    (long_name = 'Inflation in non agriculture'),
        mc_N     $mc^N_t$     (long_name = 'Real marginal cost in non agriculture'),
        mc_A     $mc^A_t$     (long_name = 'Real marginal cost in agriculture'),

		
        e_z      $e^z_t$      (long_name = 'TFP shock'),
        e_h      $e^h_t$      (long_name = 'labor shock'),
        e_g      $e^g_t$      (long_name = 'government spending shock'),
        e_n      $e^n_t$      (long_name = 'sectoral shock'),
        e_s      $e^s_t$      (long_name = 'supply/weather shock');


varexo	eta_z   $ \eta^z_t $   (long_name = 'innovation to TFP shock'),
        eta_h   $ \eta^h_t $   (long_name = 'innovation to labor shock'),
        eta_g   $ \eta^g_t $   (long_name = 'innovation to government spending shock'),
        eta_n   $ \eta^n_t $   (long_name = 'innovation to sectoral shock'),
        eta_s   $ \eta^s_t $   (long_name = 'innovation to supply or weather shock');

parameters	
        beta        $ \beta $          (long_name = 'discount factor'),
        delta_K     $ \delta_K $       (long_name = 'capital depreciation rate'),
        alpha       $ \alpha $         (long_name = 'share of capital in output'),
        sigmaC      $ \sigma_C $       (long_name = 'inverse elasticity of intertemporal substitution'),
        sigmaH      $ \sigma_H $       (long_name = 'inverse Frisch elasticity of labor supply'),
        chi         $ \chi $           (long_name = 'labor disutility parameter'),
        gy          $ g_y $            (long_name = 'government spending to output ratio'),
        b           $ b $              (long_name = 'Habits degree'),
        iota        $ \iota $          (long_name = 'Labor supply substitution'),
        varphi      $ \varphi $        (long_name = 'share of agricultural goods in consumption'),
        mu          $ \mu $            (long_name = 'Goods substitution in CES'),
        gamma       $ \gamma $         (long_name = 'Share of nonricardian households'),
        Hss         $ \bar{H} $        (long_name = 'Steady state Hours'),

        omega       $ \omega $         (long_name = 'Land intensity in technology'),
        tau         $ \tau $           (long_name = 'Scale in land cost'),
        kappa_A     $ \kappa_A $       (long_name = 'Scale TFP in agriculture'),
        Lss         $ L^{ss} $         (long_name = 'steady-state land supply'),
        delta_L     $ \delta_L $       (long_name = 'land depreciation or adjustment cost'),
        psi         $ \psi $           (long_name = 'Land cost parameter'),

        theta1      $ \theta_1 $       (long_name = 'Weather shock elasticity lag 0'),

        sig_z       $ \sigma_z $       (long_name = 'std. dev. of TFP shock'),
        sig_h       $ \sigma_h $       (long_name = 'std. dev. of labor shock'),
        sig_g       $ \sigma_g $       (long_name = 'std. dev. of government spending shock'),
        sig_n       $ \sigma_n $       (long_name = 'std. dev. of sectoral shock'),
        sig_s       $ \sigma_s $       (long_name = 'std. dev. of weather or supply shock'),
	
        epsilon_A   $\varepsilon_A$     (long_name = 'Elasticity of substitution in agriculture'),
        epsilon_N   $\varepsilon_N$     (long_name = 'Elasticity of substitution in non agriculture'),
        rho         $\rho$              (long_name = 'Interest rate smoothing parameter'),
        phi_y       $\phi_y$            (long_name = 'Output coefficient in Taylor rule'),
        phi_pi      $\phi_\pi$          (long_name = 'Inflation coefficient in Taylor rule'),
        kappa_N     $\kappa_N$          (long_name = 'Price adjustment cost parameter in non agriculture'),
        kappa_A     $\kappa_A$          (long_name = 'Price adjustment cost parameter in agriculture'),
		
        rho_z       $ \rho_z $         (long_name = 'persistence of TFP shock'),
        rho_h       $ \rho_h $         (long_name = 'persistence of labor shock'),
        rho_g       $ \rho_g $         (long_name = 'persistence of government spending shock'),
        rho_n       $ \rho_n $         (long_name = 'persistence of sectoral shock')
		rho_s       $ \rho_s $         (long_name = 'persistence of weather shock')
 ;

%----------------------------------------------------------------
% 2. Calibration
%----------------------------------------------------------------

alpha   = 0.33;		% share of capital in ouput
beta    = .9883;	% discount factor
delta_K = 0.025;	% depreciation of capital
sigmaC	= 1.64;		% risk aversion consumption
sigmaH	= 3.87;		% labor disutility
gy		= 0.22;		% share of public spendings in gdp
b		= 0.4;		% habits in consumption
iota	= 2.89;		% substitution cost across labor types
Lss		= .4;		% ss land per capita
Hss     = 1/3; 		% ss Labor
psi		= 1.5;		% shape of the land cost function
varphi	= 0.15;		% share of agricultural goods in consumption
mu		= 6.32;		% substitution non-agriculture vs agriculture
omega	= 0.10;		% share of land in agriculture
delta_L	= 0.06;		% land decay rate
theta1	= 29.0;		% damage current
epsilon_A	= 10;
epsilon_N	= 10;
rho			= 0.8; 
phi_y 		= .2;
phi_pi		= 1.5;
kappa_N		= 100;
kappa_A		= 100;
		
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
sig_s	= 0.81;
sig_c	= 0.00;
sig_e	= 0.00;


%----------------------------------------------------------------
% 3. Model
%----------------------------------------------------------------	
model; 
	%% Household
	[name='FOC home asset']
	m(+1)*r/pi(+1)=1;
	[name='labor disutility index']
	hu = (h_N^(1+iota)+h_A^(1+iota))^(1/(1+iota));
	[name='FOC h_N']
	w_N*uc = uN;
	[name='FOC h_A']
	w_A*uc = uA;
	[name='stochastic discount factor']
	m = beta*uc/uc(-1);
	[name='Marginal utility of consumption']
	uc = (c-b*c(-1))^-sigmaC;
	[name='Marginal disutility of labor h_N']
	uN = e_h*chi*hu^sigmaH*(h_N/hu)^iota;
	[name='Marginal disutility of labor h_A']
	uA = e_h*chi*hu^sigmaH*(h_A/hu)^iota;

	% Firms
	[name='Production y_N']
	y_N = e_z*h_N^(1-alpha);
	[name='Production y_A']
	y_A = (d*land(-1))^omega * (e_z*kappa_A*h_A^(1-alpha))^(1-omega);
	[name='labor cost minimization N']
	w_N = mc_N*(1-alpha)*p_N*y_N/h_N;
	[name='labor cost minimization A']
	w_A = mc_A*(1-omega)*(1-alpha)*p_A*y_A/h_A;
	[name='NKPC N']
	(1-epsilon_N)*p_N*y_N + epsilon_N*mc_N*y_N - kappa_N*p_N*pi_N*(pi_N-1)*y_N + kappa_N*m(+1)*p_N(+1)*pi_N(+1)*(pi_N(+1)-1)*y_N(+1);
	[name='NKPC A']
	(1-epsilon_A)*p_A*y_A + epsilon_A*mc_A*y_A - kappa_A*p_N*pi_A*(pi_A-1)*y_A + kappa_A*m(+1)*p_N(+1)*pi_A(+1)*(pi_A(+1)-1)*y_A(+1);
	
	[name='FOC land']
	varrho = m(+1)*(mc_A*omega*p_A(+1)*y_A(+1)/land + ((1-delta_L)*d(+1)*varrho(+1)+phi(+1)/land));
	[name='FOC x']
	p_N	= tau*x^(psi-1)*varrho*d*land(-1);
	[name='land cost']
	phi = tau/(psi)*x^(psi)*d*land(-1);



	[name='Land']
	land = (1-delta_L)*d*land(-1) + phi;
	[name='Damage function']
	d = e_s^-theta1;

	%%% AGGREGATION
	[name='total prod']
	y = (1-n)*p_N*y_N + n*p_A*y_A;
	h = (1-n)*h_N + n*h_A;
	[name='Reallocation shock']
	n = steady_state(n)*e_n;
	[name='Ressources Constraint']
	(1-n)*y_N 	= (1-varphi)*p_N^-mu*c + n*x + gy*steady_state(y_N)*e_g + kappa_N/2*(pi_N-1)^2*y_N + kappa_A/2*(pi_A-1)^2*y_A;
	n*y_A 		= varphi*p_A^-mu*c;
	[name='Gross Domestic Product']
	gdp = y - n*p_N*x;
	[name='relative price']
	1 = (1-varphi) * p_N^(1-mu) + varphi * p_A^(1-mu);
	p_N/p_N(-1) = pi_N/pi;
	p_A/p_A(-1) = pi_A/pi;
	
	[name='Monetary policy']
	log(r/STEADY_STATE(r)) = rho*log(r(-1)/STEADY_STATE(r)) + (1-rho)*(phi_y*log(gdp/STEADY_STATE(gdp)) + phi_pi*log(pi));
	
	[name='shocks']
	log(e_z) = rho_z*log(e_z(-1))+sig_z*eta_z/100;
	log(e_h) = rho_h*log(e_h(-1))+sig_h*eta_h/100;
	log(e_g) = rho_g*log(e_g(-1))+sig_g*eta_g/100;
	log(e_n) = rho_n*log(e_n(-1))+sig_n*eta_n/100;
	log(e_s) = rho_s*log(e_s(-1))+sig_s*eta_s/100;
end;

%----------------------------------------------------------------
% 4. Computation
%----------------------------------------------------------------

steady_state_model;
	pi		= 1; pi_A = 1; pi_N = 1;
	mc_N = (epsilon_N-1)/ epsilon_N;
	mc_A = (epsilon_A-1)/ epsilon_A;
	e_z		= 1; e_h = 1; e_g = 1; e_i = 1; e_n = 1; e_s = 1;
	r		= 1/beta;
	m 		= beta;
	h_N		= Hss; 
	h_A		= h_N;
	hu 		= (h_N^(1+iota)+h_A^(1+iota))^(1/(1+iota));
	p_N		= 1;	p_A		= 1;
	q_N		= 1;	q_A		= 1;
	y_N		= h_N^(1-alpha);
	w_N		= mc_N*(1-alpha)*y_N/h_N;	
	w_A		= w_N;	
	y_A		= h_A*w_A/(mc_A*(1-omega)*(1-alpha));
	land	= Lss;
	phi		= delta_L*land;
	varrho	= (mc_A*omega*y_A/land + phi/land)/(1/m-(1-delta_L));
	x		= psi*phi*varrho;
	tau		= 1/(x^(psi-1)*varrho*land);
	kappa_A = (y_A/(land^omega*(h_A^(1-alpha))^(1-omega)))^(1/(1-omega));
	n		= ((1-gy)*y_N)/((1-varphi)/varphi*y_A+y_N+x);
	c		= n*y_A/varphi;	
	c_star 	= c; 
	y 		= (1-n)*p_N*y_N + n*p_A*y_A;
	h 		= (1-n)*h_N + n*h_A;
	gdp 	= y - n*x;
	m_star 	= beta;
	d 		= 1;
	c_N		= c; c_R = c;	
	uc 		= ((c-b*c)^-sigmaC);
	chi 	= w_N*uc/(hu^sigmaH*(h_N/hu)^iota);
	uN 		= e_h*chi*hu^sigmaH*(h_N/hu)^iota;
	uA 		= e_h*chi*hu^sigmaH*(h_A/hu)^iota;
end;

M_.Sigma_e = eye(M_.exo_nbr);

resid;
%steady;
check;



stoch_simul(order=1);




varnames = char('gdp','p_N','p_A','y_A','y_N','land','pi','pi_A','r');
nx = 3; ny = 3;

ix = strmatch('eta_s',M_.exo_names,'exact');
figure;
for iy=1:size(varnames,1)
	subplot(nx,ny,iy)
		idx=strmatch(deblank(varnames(iy,:)),M_.endo_names,'exact');
		yx = eval(['oo_.irfs.' deblank(varnames(iy,:)) '_' M_.exo_names{ix}]);
		if oo_.dr.ys(idx) ~=0 || oo_.dr.ys(idx) ~=1
			yx = 100*yx/oo_.dr.ys(idx);
		else
			ys = 100*yx;
		end
		plot(yx,'linewidth',1,'Color',[235, 64, 52]/255,'Linewidth',1.5)

		title([M_.endo_names_long{idx} ' - ' M_.endo_names_tex{idx}])
		grid on
end
