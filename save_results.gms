$ontext

   CAPRI project

   GAMS file : SAVE_RESULTS.GMS

   @purpose  : saves scenario results in the reporting parameter p_results
   @author   :
   @date     : 19.07.13
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy :

$offtext
********************************************************************************




       p_results(R,"","PMRK",XX,%1) =   v_marketPrice.L(R,XX);
       p_results(R,"","PPRI",XX1,%1) =   v_prodPrice.L(R,XX1);
       p_results(R,"","CPRI",XX1,%1) =   v_consPrice.L(R,XX1);
       p_results(R,R1,"impp",XX,%1) =   v_impPrice.L(R,R1,XX);
       p_results(R,"","arm1p",XX1,%1) =  v_arm1Price.L(R,XX1);
       p_results(R,"","arm2p",XX,%1) =  v_arm2Price.L(R,XX);

* --- quantity variables
         p_results(R,"","EXPorts",XX,%1) =   v_expQuant.L(R,XX);
         p_results(R,"","arm1",XX,%1) =   v_arm1Quant.L(R,XX);

         p_results(R,"","arm2",XX,%1) =  v_arm2Quant.L(R,XX);
         p_results(R,"","Hcon",XX1,%1)  =  v_consQuant.L(R,XX1);
         p_results(R,"","Prod",XX,%1)  =  v_prodQuant.L(R,XX);


         p_results(R,R1,"trade",XX,%1)    = v_tradeFlows.L(R,R1,XX);
         p_results(R,"","Imports",XX,%1)  = sum(R1 $ (not sameas(R,R1)), v_tradeFlows.L(R,R1,XX));
         p_results(R,"","DSales",XX,%1)   = v_domSales.L(R,XX);

* --- tariffs are either endogenous or exogenous
         p_results(R,R1,"tariff",XX,%1) =   %2(R,R1,XX);
$ifi %1 == "SIM_AVE"          p_results(R,R1,"tariff",XX,%1) $ p_doubleZero(R,R1,XX,"cur") = 0;

