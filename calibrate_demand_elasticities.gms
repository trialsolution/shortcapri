
SET elasType "Type of elasticities" / Supply,Demand/;


$ontext
Properties of the elasticity matrix

1.        sum of elasticities in a row should be zero (Homog_)
2.        sum of income elasticities (columnwise) weighted with the budget share equals to 1 (Addi_)
3.        own price elasticities are negative
4.        income elasticities are positive
$offtext



parameter p_oriElas(R,XX1,XX1)  "elasticities";

p_oriElas(R,XX,XX) = -0.1;


parameter p_budgetShare(R,XX1);


* Calculate the respective parameters of the GL demand system
* this is called 'trimming' in CAPRI
* ===========================================================

*following taken mostly from trim_elas_decl.gms


* --------------------------------------------------------------------------------------
*
*                 DECLARATIONS
*
* --------------------------------------------------------------------------------------
*
 Equations
    Homog_      "Homogeniety of degree zero for elasticities in prices"
    Addi_       "Additivity of income elasticities weighted with budget shares"
*
*                --- different equations relating to the demand system
*
     F_          "Definition of F term of Generalized Leontief demand system"
     G_          "Definition of G term of Generalized Leontief demand system"
     Gi_         "Definition of first derivative of G term of Generalized Leontief demand system"
     Xi_         "Definition of demand quantities from Generalized Leontief demand system"
     DP_         "Definition of price elasticity for Generalized Leontief demand system"
     DY_         "Definition of income elasticity for Generalized Leontief demand system"
     Gij_        "Definition of second derivative of G term of Generalized Leontief demand system"

      FitElas_    "Minimise absolute squares between given and calibrated elasticities"

     ;


 Variables

     v_Ela(R,XX1,YY1)                "Calibrated elasticities"
     v_GLDemF(R)                   "F term of Generalized Leontief demand system"
     v_GLDemG(R)                   "G term of Generalized Leontief demand system"
     v_GLDemGi(R,XX1)              "First derivative of G term of Generalized Leontief demand system"
     v_GLparD(R,XX1)               "Commitment parameter D of Generalized Leontief demand system"
     V_B(R,XX1,YY1)                "B parameter of Generalized Leontief demand system"
     v_GLDemGij(R,XX1,YY1)         "Second derivative of G term of Generalized Leontief demand system"

     v_obje

 Parameter

     p_qx(R,*)                    "Quantity produced or demanded"
     p_price(R,*)                 "Price"

     p_valueShare(R,XX1)          "Value share"
     p_valueSum(R)                "Income or feed costs or revenues"

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

   v_ela(R,XX1,YY1)
*
      =E=  ((  v_GLDemGij(R,XX1,YY1)/v_GLDemG(R)
*
              -v_GLDemGi(R,XX1)*v_GLDemGi(R,YY1) / SQR(v_GLDemG(R)) ) * (p_valueSum(R)-v_GLDemF(R))
*
              -v_GLDemGi(R,XX1)/v_GLDemG(R)  * v_GLparD(R,YY1)) * p_price(R,YY1)/p_qx(R,XX1);
*


* Note that with the '.pos' trick we always use the upper triangular part of V_B.
* As a result the estimated V_B matrix is guaranteed to be symmetric

 Gij_(R,XX1,YY1) $ (p_qx(R,XX1) And p_qx(R,YY1) and (NOT SAMEAS(XX1,YY1))) ..
*
     v_GLDemGij(R,XX1,YY1) =E=   ((    V_B(R,XX1,YY1) $ ( XX1.pos LE YY1.pos )
                                 + V_B(R,YY1,XX1) $ ( XX1.pos GT YY1.pos ) ) * 0.5/SQRT( p_price(R,YY1) * p_price(R,XX1))) $ (NOT SAMEAS(XX1,YY1));


*
*    --- adding up of income elasticities weighted with budget shares (only for demand)
*
 Addi_(R) $ SUM(XX1, P_budgetShare(R,XX1)) ..

     SUM(XX1 $ P_budgetShare(R,XX1), v_Ela(R,XX1,"Ince") * P_budgetShare(R,XX1)) =E= 1.;
*
*    --- homogeniety in prices: sum of elasiticities must be zero
*        (including the non-agricultural products, not covered feed and income)
*
 Homog_(R,XX1) $ p_qx(R,XX1) ..
*
     SUM(YY1 $ (p_qx(R,YY1) or sameas(YY1,"INCE")), v_Ela(R,XX1,YY1)) + 1 =E= 1;


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


*$offlisting

 Model m_trimDem "Calibration model for Generalized Leontief demand system"
                   / F_,G_,GI_,Xi_,DP_,Gij_,FitElas_,homog_,Addi_/;


 m_trimDem.Limcol   = 0;
 m_trimDem.Limrow   = 0;
 m_trimDem.Solprint  = 1;
 m_trimDem.Holdfixed = 1;
 m_trimDem.Iterlim   = 10000;


* CALIBRATION STARTS
* ==================


* price converted to eur per kg
        p_price(R,XX)    =  DATA(R,"Cpri",XX,"CUR")/1000;


*
*       --- total income per head (for Addi_ condition) and budget shares  eur/head
*
        p_valueSum(R)        = DATA(R,"Ince","Levl","cur")/DATA(R,"INHA","Levl","cur");


*
* -- calculate p_qx in line with the above budget shares and the total budget
*
*          p_qx(R,XX)= (P_budgetShare(R,XX)*p_valueSum(R)) / p_price(R,XX) ;

*       DATA(R,"HCon",XX,"CUR") = (p_qx(R,XX)*DATA(R,"INHA","Levl","CUR")) / 1000.;



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



*       ---- start values and bounds for elasticities

        v_ela.l(R,XX1,YY1) = p_oriElas(R,XX1,YY1);

*       ---- fix to zero if no demand

        v_ela.fx(R,XX1,YY1) $ ( (NOT p_qx(R,XX1)) or ((NOT p_qx(R,YY1))) and (NOT SAMEAS(YY1,"ince")) ) = 0;
*
*        v_GLDemG.FX(R) = 10000;

*
        v_GLDemG.LO(R) = 1.E-6;


        v_GLDemF.LO(R) = 0.1 * p_valueSum(R);
        v_GLDemF.UP(R) = p_valueSum(R) * 0.99;

        V_B.LO(R,XX1,YY1) $ (NOT SAMEAS(XX1,YY1)) = 0;
*        V_B.up(R,XX,XX) = eps;
        V_B.L (R,XX1,YY1) = +0.5;
        V_B.L (R,XX,XX) = -1.;
*        V_B.L (R,"inpe","inpe") = 1.;
*        V_B.Lo (R,"inpe","inpe") = eps;

        v_GLparD.FX(R,XX1) $ (NOT p_qx(R,XX1)) = 0;

        v_GLparD.LO(R,XX1) $ p_qx(R,XX1) = -(p_qx(R,XX1)+0.1) * 10.;
        v_GLparD.L(R,XX1)  $ p_qx(R,XX1) =  (p_qx(R,XX1)+0.1) * 0.90;
        v_GLparD.UP(R,XX1) $ p_qx(R,XX1) = +(p_qx(R,XX1)+0.1) * 10.;
*


        V_B.FX(R,XX1,YY1) $ ( (NOT p_qx(R,XX1)) OR (NOT p_qx(R,YY1)) ) = 0;




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


        v_ela.l(R,XX1,YY1) $ (p_qx(R,XX1) and p_qx(R,YY1) and v_GLDemG.L(R))
                                 =  ( (   v_GLDemGij.L(R,XX1,YY1)/v_GLDemG.L(R)
*
                                          -v_GLDemGi.l(R,XX1)*v_GLDemGi.l(R,YY1) / SQR(v_GLDemG.L(R)) ) * (p_valueSum(R)-v_GLDemF.L(R))
*
                                          -v_GLDemGi.l(R,XX1)/v_GLDemG.L(R)  * v_GLparD.L(R,YY1)) * p_price(R,YY1)/p_qx(R,XX1);

*
        v_ela.l(R,XX1,"Ince") $ (p_qx(R,XX1) and v_GLDemG.L(R)) = v_GLDemGi.l(R,XX1) / v_GLDemG.L(R) * p_valueSum(R)/p_qx(R,XX1);
*
*       --- elasticities cannot be greater 10
*
        v_ela.lo(R,XX1,YY1) = -10;
        v_ela.up(R,XX1,YY1) = +10;
*
*       --- ensure that income elasticities are not lower then -0.1
*
        v_ela.lo(R,XX1,"Ince") $ p_qx(R,XX1) = -0.1;




             v_obje.l =

      SUM( (R,XX1,YY1) $ (p_qx(R,XX1) and (p_qx(R,YY1) OR SAMEAS(YY1,"Ince") )),
*
*     --- first term: squared absolute differences
*
          (SQR( v_ela.l(R,XX1,YY1) - p_oriElas(R,XX1,YY1)) $ (p_oriElas(R,XX1,YY1) ))
*
*     --- second term: squared relative differences (with a small correction to avoid crazy effects of very small elasticities)
*
        + SQR( (v_ela.l(R,XX1,YY1) - p_oriElas(R,XX1,YY1))/(abs(p_oriElas(R,XX1,YY1))+0.05)) $ (p_oriElas(R,XX1,YY1) gt eps)

*
*     --- higher weights for own price and income elasticities
*
              * (1. + 9. $ (SAMEAS(XX1,YY1) OR SAMEAS(YY1,"Ince")
                            or ((1 eq 1) and SAMEAS(YY1,"Inpe") and (NOT SAMEAS(XX1,"Inpe")) and p_oriElas(R,XX1,"INCE")))))
     /

*
*     --- scale the objective
*

     SUM( (R,XX1,YY1) $ (p_qx(R,XX1) and (p_qx(R,YY1) OR SAMEAS(YY1,"Ince") )),
                (1. + 9. $ (SAMEAS(XX1,YY1) OR SAMEAS(YY1,"Ince")
                            or ((1 eq 1) and SAMEAS(YY1,"Inpe") and (NOT SAMEAS(XX1,"Inpe")) and p_oriElas(R,XX1,"INCE"))  )));



 solve m_trimDem using nlp minimizing v_obje;

 p_elasDem(R,XX1,YY1) = v_ela.L(R,XX1,YY1);


       p_pdGL(R,XX1,"CUR")       = v_GLparD.L(R,XX1) $ p_qx(R,XX1);

* upper triangular part
       p_pbGL(R,XX1,YY1,"CUR")   = V_B.l(R,XX1,YY1) $ ( (XX1.pos LE YY1.pos ) and p_qx(R,XX1) and p_qx(R,YY1));
* lower triangular part
       p_pbGL(R,XX1,YY1,"CUR") $ ( (XX1.pos gt YY1.pos ) and p_pbGL(R,YY1,XX1,"CUR"))  = p_pbGL(R,YY1,XX1,"CUR");



parameter store_px;

         store_px(R,XX1,"cons_per_head") = p_qx(R,XX1);


* Note that the numerical solution for V_B does not provide a completely symmetric matrix
* using only the upper triangular or the full matrix gives slightly different results
parameter check_calibrated_G;


  check_calibrated_G(R) = SUM( (XX1,YY1)  $ (p_qx(R,XX1) and p_qx(R,YY1)),
                                  (    V_B.L(R,XX1,YY1) ) * SQRT(p_price(R,XX1)*p_price(R,YY1)) );

display check_calibrated_G;


*  -- the parameters below will also be used in the calibration of the supply functions
*     Let's store them then get rid of them
parameter p_store;

         p_store(R,XX1,"p_qx","demand") = p_qx(R,XX1);
         p_store(R,XX1,"p_price","demand") = p_price(R,XX1);
         p_store(R," ","p_valueSum","demand") = p_valueSum(R);

    option kill=p_qx;
    option kill=p_price;
    option kill=p_valueSum;

