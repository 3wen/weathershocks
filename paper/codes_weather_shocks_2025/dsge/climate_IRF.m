function [] = climate_IRF(param_name,param_vals,ee,yvar,SSdev,M_,oo_,options_)
%CLIMATE_IRF Summary of this function goes here
%   Detailed explanation goes here

    if nargin < 5
       SSdev = 0;
    end
    if ~options_.irf
        options_.irf = 55;
    end
    
    idp = strmatch(param_name,M_.param_names,'exact');
    M = M_;
    oo = oo_;
    M.params(idp) = param_vals(1);
%    [dr,~,M,~,oo] = resol(0,M,options_,oo_);
    [oo.dr,~,M.params] = resol(0, M, options_, oo.dr, oo.dr.ys, oo.exo_steady_state, oo.exo_det_steady_state);
    y0            = simult_(M,options_,oo.dr.ys,oo.dr,ee,1)';
    ss0           = oo.dr.ys;
    M.params(idp) = param_vals(2);
    [oo.dr,~,M.params] =resol(0, M, options_, oo.dr, oo.dr.ys, oo.exo_steady_state, oo.exo_det_steady_state);
    y1            = simult_(M,options_,oo.dr.ys,oo.dr,ee,1)';
    M.params(idp) = param_vals(3);
    [oo.dr,~,M.params] = resol(0, M, options_, oo.dr, oo.dr.ys, oo.exo_steady_state, oo.exo_det_steady_state);
    % y2            = simult_(oo.dr.ys,dr,ee,1)';
    y2            = simult_(M,options_,oo.dr.ys,oo.dr,ee,1)';
    idtot = [];
    idSTART = find(sum(ee,2)~=0);
    idSTART = idSTART(1);
    Tid=1:25;
    T  = Tid' - idSTART;
    figure('NumberTitle', 'off', 'Name', ['Sensitivity of ' param_name]);
    for i1 = 1:size(yvar,1)
        subplot(2,4,i1)
            idx = strmatch(deblank(yvar(i1,:)),M_.endo_names,'exact');
            idtot = [idtot idx];
            
            if SSdev && ss0(idx) ~= 0
   
                y0(:,idx) = (y0(:,idx)/ss0(idx)-1)*100;
                y1(:,idx) = (y1(:,idx)/ss0(idx)-1)*100;
                y2(:,idx) = (y2(:,idx)/ss0(idx)-1)*100;
            end
            
            hold on;
            ymin = min([y0(Tid,idx)' y1(Tid,idx)' y2(Tid,idx)']);
            ymax = max([y0(Tid,idx)' y1(Tid,idx)' y2(Tid,idx)']);

            fill([-4 0 0 -4], [ymin ymin ymax ymax], [0.85 0.85 0.85], 'EdgeColor', 'none');
            plot(T,y0(Tid,idx),'-','Color',[48, 117, 191]/255,'LineWidth',2)
            plot(T,y1(Tid,idx),'s-','Color',[59, 143, 73]/255,'LineWidth',2,'MarkerIndices', 1:4:length(T),'MarkerSize', 3)
            plot(T,y2(Tid,idx),'o-','Color',[191, 57, 48]/255,'LineWidth',2,'MarkerIndices', 1:4:length(T),'MarkerSize', 3)
            yline(0, '--', 'Color', [0.5 0.5 0.5]);
            hold off;
            ylim([ymin ymax])
            title(yvar(i1,:))
            xlim([min(T) max(T)])
            ylim([min([y0(Tid,idx) ; y1(Tid,idx) ; y2(Tid,idx)]) ...
            max([y0(Tid,idx) ; y1(Tid,idx) ; y2(Tid,idx)])])
            grid on;
    end
    legend('',[param_name ' = ' num2str(param_vals(1))],[param_name ' = ' num2str(param_vals(2))],[param_name ' = ' num2str(param_vals(3))])
%    matrix_to_txt(  [T y0(Tid,idtot)],...
%                    ['IRF_' param_name num2str(0) '.txt'],char('Time',yvar))
%    matrix_to_txt(  [T y1(Tid,idtot)],...
%                    ['IRF_' param_name num2str(1) '.txt'],char('Time',yvar))
%    matrix_to_txt(  [T y2(Tid,idtot)],...
%                    ['IRF_' param_name num2str(2) '.txt'],char('Time',yvar))
    

end

