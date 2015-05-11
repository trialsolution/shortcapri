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

*  transportation costs are not considered...
    p_impPrice(R,R1,XX,"CUR") = DATA(R1,"PMRK",XX,"CUR") *  [ 1. + 0.01 * p_tarAdVal(R,R1,XX) $ ( (NOT p_doubleZero(R,R1,XX,"CUR")) $ (NOT SAMEAS(R,R1)))]  ;


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
$include 'include\base\calibrate_GL_demand.gms'



* CALIBRATION OF THE SUPPLY SYSTEM
*================================


* Definition of some auxiliary variables/parameters
*--------------------------------------------------
variable   v_ela(R,XX1,XX1)  "calibrated (corrected) supply elasticities in line with some basic regulatory conditions";
variable   v_obje            "objective of the supply system calibration, squared deviations";
equation   FitElas_          "Minimise absolute squares between given and calibrated elasticities"     ;
parameter  p_oriElas(R,XX1,XX1)  "starting values for the elasticities";


*
*    --- average squared asolute difference as objective, own price and income elasticities receive a weight of 10
*        compared to cross-price elasticities
*
 FitElas_ ..

   v_obje =E=

      SUM( (R,XX1,YY1) $ (p_qx(R,XX1) and (p_qx(R,YY1) OR SAMEAS(YY1,"Ince") )),
*
*     --- first term: squared absolute differences (also in case of non-existant elasticities)
*
         (SQR( v_ela(R,XX1,YY1) - p_oriElas(R,XX1,YY1)) $ (p_oriElas(R,XX1,YY1)
                                                                  or(   (not( sameas(XX1,"INPE")))
                                                                      and (not(sameas(YY1,"INPE")))))
*
*     --- second term: squared relative differences
*         (with a small correction to avoid crazy effects of very small and allow for missing elasticities)
*
        + SQR( (v_ela(R,XX1,YY1) - p_oriElas(R,XX1,YY1))
              /(abs(p_oriElas(R,XX1,YY1))+0.05)) $ (p_oriElas(R,XX1,YY1)
                                                              or(     (not (sameas(XX1,"INPE")))
                                                                  and (not (sameas(YY1,"INPE")))))
          )
*
*     --- higher weights for own price and income elasticities
*
              * (1. + 9. $ (SAMEAS(XX1,YY1) OR (SAMEAS(YY1,"Ince") and p_oriElas(R,XX1,"INCE")))))
     /

*
*     --- scale the objective
*

     SUM( (R,XX1,YY1) $ (p_qx(R,XX1) and (p_qx(R,YY1) OR SAMEAS(YY1,"Ince") )),
                (1. + 9. $ (SAMEAS(XX1,YY1) OR (SAMEAS(YY1,"Ince") and p_oriElas(R,XX1,"INCE")))));



$include 'include\base\calibrate_NQ_supply_model.gms'
