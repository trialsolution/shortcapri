* starting values for model variables
* ===================================

    v_tradeFlows.l(R,R1,XX)      = p_tradeFlows(R,R1,XX,"Cur");
    v_tradeFlows.fx(r,r,XX)      = 0;
    v_arm2Quant.l(R,XX)          = DATA(R,"ARM2",XX,"CUR");
    v_arm2Price.l(R,XX)          = DATA(R,"ARM2P",XX,"CUR");
    v_impPrice.l(R,R1,XX)        = p_impPrice(R,R1,XX,"CUR") ;
    v_impPrice.fx(R,R,XX)        = 0;
    v_arm1Price.l(R,XX)          = DATA(R,"ARM1P",XX,"CUR");
    v_domSales.l(R,XX)           = DATA(R,"DSales",XX,"CUR");
    v_arm1Quant.l(R,XX)          = DATA(R,"ARM1",XX,"CUR");
    v_marketPrice.l(R,XX)        = DATA(R,"PMRK",XX,"CUR");
    v_prodPrice.l(R,XX1)         = DATA(R,"PPri",XX1,"CUR");
    v_prodPrice.FX(R,"inpe")     = DATA(R,"PPri","inpe","CUR");
    v_consQuant.L(R,XX)          = DATA(R,"HCon",XX,"CUR");
    v_consQuant.fx(R,"inpe")     = 0;
    v_expQuant.L(R,xX)           = sum(R1 $ (not sameas(r,r1)), p_tradeFlows(R1,R,XX,"Cur"));
    v_consPrice.L(R,XX)          = DATA(R,"CPRI",XX,"CUR");
    v_consPrice.fx(R,"inpe")     = DATA(R,"CPRI","inpe","CUR");
    v_prodQuant.L(R,XX)          = data(R,"PROD",XX,"CUR");



* setting starting values for the demand system
* --------------------------------------------------------------

     v_GLDemandFS.L(R) = SUM(XX1 $ p_pdGl(R,XX1,"CUR"), v_consPrice.L(R,XX1) * p_pdGL(R,XX1,"CUR")*1.E-3);

     v_GLDemandGS.L(R) = SUM( (XX1,YY1) $ p_pbGL(R,XX1,YY1,"CUR"),
                 p_pbGL(R,XX1,YY1,"CUR") * SQRT(v_consPrice.L(R,XX1)*v_consPrice.L(R,YY1)*1.E-6) );

     v_GLDemandGis.L(R,XX1) $ (v_consPrice.L(R,XX1) gt eps) = SUM( YY1 $ p_pbGl(R,XX1,YY1,"CUR"),
                                     p_pbGL(R,XX1,YY1,"CUR")
                                     * SQRT(v_consPrice.L(R,YY1) / v_consPrice.L(R,XX1)));

