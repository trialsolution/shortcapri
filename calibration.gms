* calibrate producer price margins
* --------------------------------

    pv_prodPriceMarg.fx(R,XX) $ DATA(R,"Prod",XX,"CUR")

   =  [v_prodPrice.L(R,XX) -  (DATA(R,"AREP",XX,"CUR")/DATA(R,"Yild",XX,"CUR")) $ DATA(R,"Yild",XX,"CUR")]
       /
      [v_marketPrice.L(R,XX) + DATA(R,"PSE",XX,"CUR")];




* Calibrate the distribution parameters of the Armington share equations
* ----------------------------------------------------------------------

   p_dpCESTrade(R,R1,XX) $ (p_tradeFlows(R,R1,XX,"Cur") and (not sameas(R,R1)))

         = v_tradeFlows.l(R,R1,XX)/(  v_arm2Quant.l(R,XX)

                 * [ v_arm2Price.l(R,XX) / v_impPrice.l(R,R1,XX) ] ** p_rhoArm2(R,XX));

    p_dpCESTrade(R,R,XX) $ (data(R,"DSales",XX,"Cur") )

         = v_domSales.l(R,XX)/(  v_arm1Quant.l(R,XX)

                 * [ v_arm1Price.l(R,XX) / v_marketPrice.l(R,XX) ] ** p_rhoArm1(R,XX));


    p_dpCESTrade(R,"RW",XX) $ (v_arm2Quant.l(R,XX) )

         = v_arm2Quant.l(R,XX)/(  v_arm1Quant.l(R,XX)

                 * [ v_arm1Price.l(R,XX) / v_arm2Price.l(R,XX) ] ** p_rhoArm1(R,XX));


* test the calibration of the Armington system
* --------------------------------------------

parameter p_checkArmington;

p_checkArmington(R," ",XX,"domsales")
 =
     data(R,"DSales",XX,"cur") -

            { v_arm1Quant.L(R,XX)
                 * p_dpCESTrade(R,R,XX)
                 * [ v_arm1Price.L(R,XX) / v_marketPrice.L(R,XX) ] ** p_rhoArm1(R,XX) };


p_checkArmington(R,XX," ","arm2quant")
 =
       data(R,"arm2",XX,"cur") -
              { v_arm1Quant.L(R,XX)
                 * p_dpCESTrade(R,"RW",XX)
                 * [ v_arm1Price.L(R,XX) / v_arm2Price.L(R,XX) ] ** p_rhoArm1(R,XX) };

p_checkArmington(R,R1,XX,"tflows") $ (not SAMEAS(R,R1))

=    p_tradeFlows(R,R1,XX,"Cur")

  - [ v_arm2Quant.L(R,XX)
                 * p_dpCESTrade(R,R1,XX)
                 * [ v_arm2Price.L(R,XX) / v_impPrice.L(R,R1,XX) ] ** p_rhoArm2(R,XX) ];

display "check the calibration of the Armington system", p_checkArmington;


* shift the supply function to observed "prod"
* --------------------------------------------

  p_cnstNQSupp(R,XX) $ DATA(R,"Prod",XX,"CUR")
    = DATA(R,"Prod",XX,"CUR")
           - SUM( YY1 $ (DATA(R,"PPri",YY1,"CUR") and (not sameas(YY1,"INPE"))),
                         p_hessNQSupp(R,XX,YY1,"CUR")
                          * v_prodPrice.L(R,YY1)/v_prodPrice.l(R,"INPE"));

* check prodNQ calibration
parameter  p_checkProdNQ;

p_checkProdNQ(R,XX1) $ DATA(R,"Prod",XX1,"CUR")  =
    v_prodQuant.L(R,XX1)
     - (p_cnstNQSupp(R,XX1)
            + SUM( YY1 $ (DATA(R,"PPri",YY1,"CUR") and (NOT SAMEAS(YY1,"INPE"))),
                           p_hessNQSupp(R,XX1,YY1,"CUR")
                         * v_prodPrice.L(R,YY1)/v_prodPrice.L(R,"Inpe")));

display "check the calibraiton of the NQ production functions", p_checkProdNQ;



* setting starting values after calibration of the demand system
* --------------------------------------------------------------

     v_GLDemandFS.L(R) = SUM(XX1 $ p_pdGl(R,XX1,"CUR"), v_consPrice.L(R,XX1) * p_pdGL(R,XX1,"CUR")*1.E-3);

     v_GLDemandGS.L(R) = SUM( (XX1,YY1) $ p_pbGL(R,XX1,YY1,"CUR"),
                 p_pbGL(R,XX1,YY1,"CUR") * SQRT(v_consPrice.L(R,XX1)*v_consPrice.L(R,YY1)*1.E-6) );

     v_GLDemandGis.L(R,XX1) $ (v_consPrice.L(R,XX1) gt eps) = SUM( YY1 $ p_pbGl(R,XX1,YY1,"CUR"),
                                     p_pbGL(R,XX1,YY1,"CUR")
                                     * SQRT(v_consPrice.L(R,YY1) / v_consPrice.L(R,XX1)));


* test the calibration of the demand system
* -----------------------------------------

parameter p_checkDemand;

*  --  these are the X_i's
       p_checkDemand(R,XX1,"Xi_pHead")
        =   p_store(R,XX1,"p_qx","demand")
             -
           {(v_GLDemandGis.L(R,XX1)/v_GLDemandGS.L(R)
           * ( DATA(R,"Ince","Levl","CUR")/DATA(R,"INHA","LEVL","CUR") - v_GLDemandFS.L(R))
              + p_pdGL(R,XX1,"CUR"))} ;


       p_checkDemand(R,XX1,"Xi_tot")
          =  data(r,"hcon",xx1,"cur")
              -
          {(v_GLDemandGis.L(R,XX1)/v_GLDemandGS.L(R)
           * ( DATA(R,"Ince","Levl","CUR")/DATA(R,"INHA","LEVL","CUR") - v_GLDemandFS.L(R))
              + p_pdGL(R,XX1,"CUR")) * data(R,"inha","levl","cur") / 1000} ;


       p_checkDemand(R,XX1,"consPrice") = v_consPrice.L(R,XX1) - p_store(R,XX1,"p_price","demand") * 1000;

       p_checkDemand(R," ","Y") = p_store(R," ","p_valueSum","demand") - [DATA(R,"Ince","Levl","CUR")/DATA(R,"INHA","LEVL","CUR")];

       p_checkDemand(R,XX1,"Di") =   v_GLparD.L(R,XX1) - p_pdGL(R,XX1,"CUR");

       p_checkDemand(R," ","F") =  v_GLDemandFS.L(R) - v_GLDemF.L(R);

       p_checkDemand(R," ","G") =  v_GLDemandGS.L(R) - v_GLDemG.L(R);

       p_checkDemand(R,XX1,"Gi") = v_GLDemandGis.L(R,XX1) - v_GLDemGi.L(R,XX1);

display "check the initialization of the demand system", p_checkDemand;



* store the initialized model on 'bas'
$batinclude 'save_results.gms' '"BAS"' 'p_tarAdval'



* TEST run, (see if solving the model with the initial points gives back the calibration point)
* ---------


* MCP formulation
solve m_GlobalMarket using mcp;


* store the result of the test run on 'CAL'
$batinclude 'save_results.gms' '"CAL"'  'p_tarAdval'


* check if the test run gave back the calibration values
parameter p_checkPrices, p_checkBalances;

      p_checkBalances(R,XX,"diff_to_data","HCON") = v_consQuant.L(R,XX) - data(R,"hcon",XX,"cur") ;
      p_checkBalances(R,XX,"diff_to_data","PROD") = v_prodQuant.L(R,XX) - data(R,"prod",XX,"cur") ;
      p_checkBalances(R,XX,"diff_to_data","Exports") = v_expQuant.L(R,XX) - data(R,"Exports",XX,"cur");


       p_checkPrices(R,XX,"diff_to_data","CPRI") = v_consPrice.L(R,XX) - data(R,"CPRI",XX,"cur");
       p_checkPrices(R,XX,"diff_to_data","PPRI") = v_prodPrice.L(R,XX) - data(R,"PPRI",XX,"cur");
       p_checkPrices(R,XX,"diff_to_data","PMRK") = v_marketPrice.L(R,XX) - data(R,"PMRK",XX,"cur");
       p_checkPrices(R,XX,"diff_to_data","ARM1P") = v_arm1Price.L(R,XX) - data(R,"arm1P",XX,"cur");
       p_checkPrices(R,XX,"diff_to_data","ARM2P") = v_arm2Price.L(R,XX) - data(R,"arm2P",XX,"cur");


display "check calibration test run", p_checkPrices,p_checkBalances;

