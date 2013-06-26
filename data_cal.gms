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
R1.INPE       0.005     0.005

R2.X1          -0.1      .005                   2.E-5
R2.X2          .005      -0.1                   2.E-5
R2.INPE       0.005     0.005

R3.X1          -0.1      .005                   2.E-5
R3.X2          .005      -0.1                   2.E-5
R3.INPE       0.005     0.005

;

* calculate remaining items of the elasticity matrix
p_elasDem(R,XX1,"INPE") $ [ (not sameas(XX1,"INPE")) and (not sameas(XX1,"INCE")) ] = 0 - sum(YY1 $ (not sameas(YY1,"INPE")), p_elasDem(R,XX1,YY1));
p_elasDem(R,"INPE","INCE") $ P_budgetShare(R,"INPE")
        = [1 - sum(XX1 $ [(not sameas(XX1,"INPE")) and P_budgetShare(R,XX1)], p_elasDem(R,XX1,"INCE") * P_budgetShare(R,XX1))] / P_budgetShare(R,"INPE");
p_elasDem(R,"INPE","INPE") = 0 - sum(YY1 $ (not sameas(YY1,"INPE")), p_elasDem(R,"INPE",YY1));


* calculate the parameters of the GL demand system from the elasticities
* set up a dummy NLP model to solve for the parameters of the GL demand system

 Variables

     v_GLDemF(R)                   "F term of Generalized Leontief demand system"
     v_GLDemG(R)                   "G term of Generalized Leontief demand system"
     v_GLDemGi(R,XX1)              "First derivative of G term of Generalized Leontief demand system"
     v_GLparD(R,XX1)               "Commitment parameter D of Generalized Leontief demand system"
     V_B(R,XX1,YY1)                "B parameter of Generalized Leontief demand system"
     v_GLDemGij(R,XX1,YY1)         "Second derivative of G term of Generalized Leontief demand system"
     v_dummy

;


 Equations
*
*                --- different equations relating to the demand system
*
     F_          "Definition of F term of Generalized Leontief demand system"
     G_          "Definition of G term of Generalized Leontief demand system"
     Gi_         "Definition of first derivative of G term of Generalized Leontief demand system"
     Xi_         "Definition of demand quantities from Generalized Leontief demand system"
     DP_         "Definition of price elasticity for Generalized Leontief demand system"
     Gij_        "Definition of second derivative of G term of Generalized Leontief demand system"
     dummyobj_
     ;



 F_(R) ..
*
     v_GLDemF(R)  =E= SUM(XX1 $ p_qx(R,XX1), p_price(R,XX1) * v_GLparD(R,XX1));
*
* ---- definition of function G for the GL expenditure system
*
 G_(R) ..
*
     v_GLDemG(R) =E= SUM( (XX1,YY1)  $ (p_qx(R,XX1) and p_qx(R,YY1)),
                                  (    V_B(R,XX1,YY1) $ ( XX1.pos LE YY1.pos )
                                     + V_B(R,YY1,XX1) $ ( XX1.pos GT YY1.pos ) ) * SQRT(p_price(R,XX1)*p_price(R,YY1)) );

*
* ---- definition of first derivatives of G called Gi for GL
*
 Gi_(R,XX1) $ p_qx(R,XX1) ..

     v_GLDemGi(R,XX1) =E= SUM( YY1 $ p_qx(R,YY1), (   V_B(R,XX1,YY1) $ ( XX1.pos LE YY1.pos )
                                                 + V_B(R,YY1,XX1) $ ( XX1.pos GT YY1.pos ) ) * SQRT(p_price(R,YY1)/p_price(R,XX1)));
*
* ----- fit to given demand quantities (RHS: marshallian demands)
*       (valuesum = income - f= value of commitment: available income after commitment are covered)
*
 Xi_(R,XX1) $ p_qx(R,XX1)..

     p_qx(R,XX1) =E= v_GLDemGi(R,XX1)/v_GLDemG(R) * ( p_valueSum(R) - v_GLDemF(R)) + v_GLparD(R,XX1);
*
 DP_(R,XX1,YY1) $ (p_qx(R,XX1) And p_qx(R,YY1) AND (NOT SAMEAS(XX1,YY1) )) ..

   p_elasDem(R,XX1,YY1)
*
      =E=  ((  v_GLDemGij(R,XX1,YY1)/v_GLDemG(R)
*
              -v_GLDemGi(R,XX1)*v_GLDemGi(R,YY1) / SQR(v_GLDemG(R)) ) * (p_valueSum(R)-v_GLDemF(R))
*
              -v_GLDemGi(R,XX1)/v_GLDemG(R)  * v_GLparD(R,YY1)) * p_price(R,YY1)/p_qx(R,XX1);
*


* Note that with the '.pos' trick we always use the upper triangular part of V_B.
* As a result the estimated V_B matrix is guaranteed to be symmetric
* Unfortunately, as a side-effect, the resulting model will not be squared (missing lower triangular part of V_B(i,j)) => solve with dummy NLP

 Gij_(R,XX1,YY1) $ (p_qx(R,XX1) And p_qx(R,YY1) and (NOT SAMEAS(XX1,YY1))) ..
*
     v_GLDemGij(R,XX1,YY1) =E=   ((    V_B(R,XX1,YY1) $ ( XX1.pos LE YY1.pos )
                                 + V_B(R,YY1,XX1) $ ( XX1.pos GT YY1.pos ) ) * 0.5/SQRT( p_price(R,YY1) * p_price(R,XX1))) $ (NOT SAMEAS(XX1,YY1));

 dummyobj_ .. v_dummy =e= 10;


model GL_demandSystem /F_, G_, Gi_, Xi_, DP_, Gij_, dummyobj_/;


* --- initialize the variables in the GL demand system


*        v_GLDemG.LO(R) = 1.E-6;

* the value of minimum commitments is somewhere between 10% and 99% of the total value
        v_GLDemF.LO(R) = 0.1 * p_valueSum(R);
        v_GLDemF.UP(R) = p_valueSum(R) * 0.99;




* non-diagonal elements of the B matrix are positive
        V_B.LO(R,XX1,YY1) $ (NOT SAMEAS(XX1,YY1)) = 0;
        V_B.L (R,XX1,YY1) = +0.5;

* the diagonal of the B matrix is usually negative except the INPE,INPE pair
        V_B.up(R,XX,XX) = eps;
        V_B.L (R,XX,XX) = -1.;
        V_B.L (R,"inpe","inpe") = 1.;
        V_B.Lo (R,"inpe","inpe") = eps;

        V_B.FX(R,XX1,YY1) $ ( (NOT p_qx(R,XX1)) OR (NOT p_qx(R,YY1)) ) = 0;


* security device: no demand, no D coefficient
        v_GLparD.FX(R,XX1) $ (NOT p_qx(R,XX1)) = 0;

        v_GLparD.LO(R,XX1) $ p_qx(R,XX1) = -(p_qx(R,XX1)+0.1) * 10.;
        v_GLparD.L(R,XX1)  $ p_qx(R,XX1) =  (p_qx(R,XX1)+0.1) * 0.90;
        v_GLparD.UP(R,XX1) $ p_qx(R,XX1) = +(p_qx(R,XX1)+0.1) * 10.;
*



*
*       --- initialis variable of expenditure system from given starting point
*
        v_GLDemF.L(R) = SUM( XX1, v_GLparD.L(R,XX1) * p_price(R,XX1));
*
        v_GLDemG.L(R) = SUM( (XX1,YY1)  $ (p_qx(R,XX1) and p_qx(R,YY1)),
                                     (    V_B.l(R,XX1,YY1) $ ( XX1.pos LE YY1.pos )
                                        + V_B.l(R,YY1,XX1) $ ( XX1.pos GT YY1.pos ) ) *     SQRT(p_price(R,XX1)*p_price(R,YY1)) );
*
        v_GLDemGi.l(R,XX1) $ p_qx(R,XX1) = SUM( YY1 $ p_qx(R,YY1),
                                      (   V_B.l(R,XX1,YY1) $ ( XX1.pos LE YY1.pos )
                                        + V_B.l(R,YY1,XX1) $ ( XX1.pos GT YY1.pos ) )  *     SQRT(p_price(R,YY1)/p_price(R,XX1)));
*
        v_GLDemGij.L(R,XX1,YY1) $ (p_qx(R,XX1) AND p_qx(R,YY1))
                        =   ((    V_B.l(R,XX1,YY1) $ ( XX1.pos LE YY1.pos )
                                + V_B.l(R,YY1,XX1) $ ( XX1.pos GT YY1.pos ) ) * 0.5/SQRT( p_price(R,YY1) * p_price(R,XX1))) $ (NOT SAMEAS(XX1,YY1))

                        +  ( - 0.5 * SUM(ZZ1 $ (p_qx(R,ZZ1) and (NOT SAMEAS(ZZ1,XX1))),
                                 (    V_B.l(R,XX1,ZZ1) $ ( XX1.pos LE ZZ1.pos )
                                    + V_B.l(R,ZZ1,XX1) $ ( XX1.pos GT ZZ1.pos ) ) * SQRT( p_price(R,ZZ1))) * p_price(R,XX1)**(-1.5)) $ SAMEAS(XX1,YY1);

execute_unload "check.gdx";
display p_valuesum, v_GLDemF.L, v_GLparD.L, p_qx, p_price;


solve GL_demandSystem using NLP maximizing v_dummy;


      p_pdGL(R,XX1,"CUR")       = v_GLparD.L(R,XX1) $ p_qx(R,XX1);

* upper triangular part
       p_pbGL(R,XX1,YY1,"CUR")   = V_B.l(R,XX1,YY1) $ ( (XX1.pos LE YY1.pos ) and p_qx(R,XX1) and p_qx(R,YY1));
* lower triangular part
       p_pbGL(R,XX1,YY1,"CUR") $ ( (XX1.pos gt YY1.pos ) and p_pbGL(R,YY1,XX1,"CUR"))  = p_pbGL(R,YY1,XX1,"CUR");



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

variable v_ela(R,XX1,XX1);
variable v_obje;
equation     FitElas_    "Minimise absolute squares between given and calibrated elasticities"     ;
parameter p_oriElas(R,XX1,XX1);


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



$include 'calibrate_supply_elasticities.gms'

