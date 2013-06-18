$ontext

        Construct consistent balances at the calibration point
        in CAPRI it's traditionally called 'data calibration', that's where the file name data_cal is originated

    In CAPRI the data balancing problem is driven by some calibration models, but here it's only a sequence of calculations
$offtext



* INPUT DATA
option seed=1234;


* substitution elasticities
p_rhoArm2(R,XX) = 10;
p_rhoArm1(R,XX) = 8;



* Policy settings...
* tariffs
 p_tarAdVal(R,R1,XX) $ (not (sameas(r,r1))) = 25;
* FTAs
 p_doubleZero(R,R1,XX,"CUR")  = 0;




* data cube entries

   DATA(R,"INHA","Levl","cur")= uniform(800,1000);

* income is in euros
   DATA(R,"Ince","Levl","cur")= uniform(8000,10000)*DATA(R,"INHA","Levl","cur");

   DATA(R,"PMRK",XX,"CUR") = 1000 * uniform(90,100) / 100;

   DATA(R,"PPRI",XX,"CUR") = 900 * uniform(90,100) / 100;
   DATA(R,"PPRI","Inpe","cur") = 1000;

* the producer price margin will be defined according to the above random sample (pv_prodPriceMarg)



* balance:    PROD - EXPT + IMPT = DSALES + ARM2 = ARM1 = HCON (+ processing + feed)
* s.t.    ARM2 = sum(p_tradeFlows)


   DATA(R,"PROD",XX,"CUR") = uniform(500,1000);
   p_tradeFlows(R,R1,XX,"Cur") $ (not sameas(r,r1)) = uniform(50,100);

   DATA(R,"Imports",XX,"CUR") = sum(R1, p_tradeflows(R,R1,XX,"cur"));
   DATA(R,"Exports",XX,"CUR") = sum(R1, p_tradeflows(R1,R,XX,"cur"));


*  arm2 quantities refer to utilities; but in the calibration point utility = quantity
   DATA(R,"ARM2",XX,"CUR") = sum(R1 $ (not sameas(R,R1)), p_tradeFlows(R,R1,XX,"Cur"));

   DATA(R,"DSales",XX,"CUR") = DATA(R,"PROD",XX,"CUR") - DATA(R,"Exports",XX,"CUR") ;

   DATA(R,"ARM1",XX,"CUR") = DATA(R,"DSales",XX,"CUR") + DATA(R,"ARM2",XX,"CUR");

   DATA(R,"HCON",XX,"CUR") = DATA(R,"ARM1",XX,"CUR") ;




* the chain of defining the price variables:
* i) v_marketPrice => v_impPrice => v_arm2Price => v_arm1Price => v_consPrice
* ii) v_marketPrice => v_prodPrice



   p_impPrice(R,R1,XX,"CUR") = DATA(R1,"PMRK",XX,"CUR") *  [ 1. + 0.01 * p_tarAdVal(R,R1,XX) $ ( (NOT p_doubleZero(R,R1,XX,"CUR")) $ (NOT SAMEAS(R,R1)))]  ;

    DATA(R,"Arm2P",XX,"CUR") =
             [SUM(R1 $ ((NOT SAMEAS(R,R1)) $ p_tradeFlows(R,R1,XX,"CUR")),
                      p_impPrice(R,R1,XX,"cur") * p_tradeFlows(R,R1,XX,"cur"))]

              /DATA(R,"ARM2",XX,"CUR") ;



     DATA(R,"ARM1P",XX,"CUR") = (DATA(R,"PMRK",XX,"CUR") * DATA(R,"DSales",XX,"CUR")
                               + DATA(R,"ARM2P",XX,"CUR") * DATA(R,"Arm2",XX,"CUR"))
                               / DATA(R,"ARM1",XX,"CUR");

     data(R,"CSE",XX,"cur") = 50.;

   DATA(R,"CPRI",XX,"CUR") =  DATA(R,"ARM1P",XX,"CUR") + DATA(R,"CSE",XX,"CUR");
   DATA(R,"Cpri","Inpe","cur") = 1000;



* supply and demand elasticities
* ------------------------------

parameters
*         p_elasDem(R,XX1,YY1) "demand elasticities"
         p_elasSup(R,XX1,YY1) "supply elasticities"

         p_budgetShare(R,XX1) "budget shares"
         p_qx(R,*)                    "Quantity produced or demanded"
         p_price(R,*)                 "Price"
         p_valueShare(R,XX1)          "Value share"
         p_valueSum(R)                "Income or feed costs or revenues"
;



*$include 'calibrate_demand_elasticities.gms'

* price converted to eur per kg
        p_price(R,XX)    =  DATA(R,"Cpri",XX,"CUR")/1000;


*
*       --- total income per head (for Addi_ condition) and budget shares  eur/head
*
        p_valueSum(R)        = DATA(R,"Ince","Levl","cur")/DATA(R,"INHA","Levl","cur");


*  consumption quantities are in kg/head
        p_qx(R,XX) = DATA(R,"HCon",XX,"CUR") / DATA(R,"INHA","Levl","CUR") * 1000.;
        p_qx(R,XX) $ (NOT p_price(R,XX)) = 0;
        P_budgetShare(R,XX) = p_qx(R,XX)  * p_price(R,XX) / p_valueSum(R);

*
*       --- the non-agr. product ("INPE")
*
        P_budgetShare(R,"Inpe")  = 1. - SUM(XX, P_budgetShare(R,XX));
        p_price(R,"Inpe")    = DATA(R,"Cpri","Inpe","cur")/1000;
        p_qx(R,"Inpe")       = (P_budgetShare(R,"Inpe") * p_valueSum(R))/p_price(R,"Inpe");




* fixed demand elasticities
* note that homogeneity of degree zero and additivity applies (sum of income elasticities weighte with budget shares = 1)

table p_elasDem(R,XX1,YY1)

                X1         X2        INPE        INCE
R1.X1          -0.1       0.005                 2.E-5
R1.X2          .005      -0.1                   2.E-5
R1.INPE       -5.E-5     -5.E-5

R2.X1          -0.1      .005                   2.E-5
R2.X2          .005      -0.1                   2.E-5
R2.INPE       -5.E-5     -5.E-5

R3.X1          -0.1      .005                   2.E-5
R3.X2          .005      -0.1                   2.E-5
R3.INPE       -5.E-5     -5.E-5

;

* calculate remaining items of the elasticity matrix
p_elasDem(R,XX1,"INPE") $ [ (not sameas(XX1,"INPE")) and (not sameas(XX1,"INCE")) ] = 0 - sum(YY1 $ (not sameas(YY1,"INPE")), p_elasDem(R,XX1,YY1));
p_elasDem(R,"INPE","INCE") $ P_budgetShare(R,"INPE") 
        = [1 - sum(XX1 $ [(not sameas(XX1,"INPE")) and P_budgetShare(R,XX1)], p_elasDem(R,XX1,"INCE") * P_budgetShare(R,XX1))] / P_budgetShare(R,"INPE");
p_elasDem(R,"INPE","INPE") = 0 - sum(YY1 $ (not sameas(YY1,"INPE")), p_elasDem(R,"INPE",YY1));


* calculate the parameters of the GL demand system from the elasticities



*  -- the parameters below will also be used in the calibration of the supply functions
*     So here we need to set them back to zero. But first Let's store them...
parameter p_store;

         p_store(R,XX1,"p_qx","demand") = p_qx(R,XX1);
         p_store(R,XX1,"p_price","demand") = p_price(R,XX1);
         p_store(R," ","p_valueSum","demand") = p_valueSum(R);

* .. and then use the option kills		 
    option kill=p_qx;
    option kill=p_price;
    option kill=p_valueSum;

execute_unload "test.gdx";
abort "aborted for testing";
$stop



$include 'calibrate_supply_elasticities.gms'

