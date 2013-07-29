$ontext

        Construct consistent balances at the calibration point
        in CAPRI it's traditionally called 'data calibration', that's where the file name data_cal is originated

    In CAPRI the data balancing problem is driven by some calibration models, but here it's only a sequence of calculations
$offtext





* Create consistent Market Balances
*==================================
*
* balance:    PROD - EXPT + IMPT = DSALES + ARM2 = ARM1 = HCON (+ processing + feed)
* s.t.    ARM2 = sum(p_tradeFlows)
*---------------------------------

   DATA(R,"Imports",XX,"CUR") = sum(R1, p_tradeflows(R,R1,XX,"cur"));
   DATA(R,"Exports",XX,"CUR") = sum(R1, p_tradeflows(R1,R,XX,"cur"));


*  arm2 quantities refer to utilities; but in the calibration point utility = quantity
   DATA(R,"ARM2",XX,"CUR") = sum(R1 $ (not sameas(R,R1)), p_tradeFlows(R,R1,XX,"Cur"));

   DATA(R,"DSales",XX,"CUR") = DATA(R,"PROD",XX,"CUR") - DATA(R,"Exports",XX,"CUR") ;

   DATA(R,"ARM1",XX,"CUR") = DATA(R,"DSales",XX,"CUR") + DATA(R,"ARM2",XX,"CUR");

   DATA(R,"HCON",XX,"CUR") = DATA(R,"ARM1",XX,"CUR") ;



* Create consistent prices (consistent with the price linkage equations in the market model)
*===========================================================================================
*
* the chain of defining the price variables:
* i) v_marketPrice => v_impPrice => v_arm2Price => v_arm1Price => v_consPrice
* ii) v_marketPrice => v_prodPrice
*---------------------------------


    p_impPrice(R,R1,XX,"CUR")
               = [DATA(R1,"PMRK",XX,"CUR") + p_tc(R,R1,XX,"CUR")]
                 *  [ 1. + 0.01 * p_tarAdVal(R,R1,XX) $ ( (NOT p_doubleZero(R,R1,XX,"CUR")) $ (NOT SAMEAS(R,R1)))]  ;


    DATA(R,"Arm2P",XX,"CUR") =
             [SUM(R1 $ ((NOT SAMEAS(R,R1)) $ p_tradeFlows(R,R1,XX,"CUR")),
                      p_impPrice(R,R1,XX,"cur") * p_tradeFlows(R,R1,XX,"cur"))]

              /DATA(R,"ARM2",XX,"CUR") ;



     DATA(R,"ARM1P",XX,"CUR") = (DATA(R,"PMRK",XX,"CUR") * DATA(R,"DSales",XX,"CUR")
                               + DATA(R,"ARM2P",XX,"CUR") * DATA(R,"Arm2",XX,"CUR"))
                               / DATA(R,"ARM1",XX,"CUR");

     data(R,"CSE",XX,"cur") = 0.;

    DATA(R,"CPRI",XX,"CUR") =  DATA(R,"ARM1P",XX,"CUR") + DATA(R,"CSE",XX,"CUR");



* CALIBRATION OF THE DEMAND SYSTEM
*=================================
*p_elasDem(R,XX1,YY1) =
$include 'include\onec\calibrate_GL_demand_one.gms'

*abort "after first GL calib";

* CALIBRATION OF THE SUPPLY SYSTEM
*================================



* As we only have 1 product, we skip the previous calibration process and simply assume a supply elasticity

  p_elasSup(R,XX,XX) = 100.;

* Hessian calculated with normalized prod. price
  p_hessNQSupp(R,XX,XX,"CUR") = p_elasSup(R,XX,XX) * DATA(R,"Prod",XX,"Cur")
                                / (DATA(R,"PPri",XX,"Cur")/DATA(R,"PPri","Inpe","Cur"));