



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


         p_results(R,R1,"trade",XX,%1)  = v_tradeFlows.L(R,R1,XX);
         p_results(R,"","DSales",XX,%1)  = v_domSales.L(R,XX);