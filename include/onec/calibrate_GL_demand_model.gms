*<%REGION File header%>
*=============================================================================
* File      : calibrate_GL_demand_model.gms
* Author    : mihalyh
* Version   : 1.0
* Date      : 25.07.2013 11:30:43
* Changed   : 26.07.2013 10:01:33
* Changed by: mihalyh
* Remarks   :
$ontext

$offtext
*=============================================================================
*<%/REGION File header%>


parameters


         p_budgetShare(R,XX1) "budget shares"
         p_qx(R,*)                    "Quantity produced or demanded"
         p_price(R,*)                 "Price"
         p_valueSum(R)                "Income or feed costs or revenues"
;

* priors for demand elasticity calibration
* note that homogeneity of degree zero and additivity should apply over the matrix (sum of income elasticities weighte with budget shares = 1)

table p_elasDem(R,XX1,YY1)   "demand elasticities"

                X1       INPE        INCE
R1.X1          -0.1                  0.005

R1.INPE                  -0.05       1.005

R2.X1          -0.1                  0.005

R2.INPE                  -0.05       1.005

R3.X1          -0.1                  0.005

R3.INPE                  -0.05       1.005

;



* Note that the GL expenditure function is of a flexible functional form
* => it can be approximate arbitrary differentiable demand functions to the first order.
*
*
* Technically: set up a dummy NLP model to solve for the parameters of the GL demand system
*-------------------------------------------------------------------------------------------

 Variables

     v_GLDemF(R)                   "F term of Generalized Leontief demand system"
     v_GLDemG(R)                   "G term of Generalized Leontief demand system"
     v_GLDemGi(R,XX1)              "First derivative of G term of Generalized Leontief demand system"
     v_GLparD(R,XX1)               "Commitment parameter D of Generalized Leontief demand system"
     V_B(R,XX1,YY1)                "B parameter of Generalized Leontief demand system"
     v_GLDemGij(R,XX1,YY1)         "Second derivative of G term of Generalized Leontief demand system"
     v_obje                        "objective of the SSQ fitting"

     pv_elasDem(R,XX1,YY1)          "demand elasticities, calibrated values"
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


      DY_         "Definition of income elasticity for Generalized Leontief demand system"


*
*    ---  Regulatory conditions
*
             Homog_      "Homogeniety of degree zero for elasticities in prices"
             Addi_       "Additivity of income elasticities weighted with budget shares"

       FitElas_    "Minimise absolute squares between given and calibrated elasticities"
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

   pv_elasDem(R,XX1,YY1)
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



*
* ----- define point income elasticities
*
 DY_(R,XX1) $ p_qx(R,XX1)  ..

       pv_elasDem(R,XX1,"Ince") =E= v_GLDemGi(R,XX1) / v_GLDemG(R) * p_valueSum(R)/p_qx(R,XX1);


*
*    --- adding up of income elasticities weighted with budget shares (only for demand)
*
 Addi_(R) $ SUM(XX1, P_budgetShare(R,XX1)) ..

     SUM(XX1 $ P_budgetShare(R,XX1), pv_elasDem(R,XX1,"Ince") * P_budgetShare(R,XX1)) =E= 1.;

*
*    --- homogeniety in prices: sum of elasiticities must be zero
*        (including the non-agricultural products, not covered feed and income)
*
 Homog_(R,XX1) $ p_qx(R,XX1) ..

     SUM(YY1 $ (p_qx(R,YY1) or sameas(YY1,"INCE")), pv_elasDem(R,XX1,YY1)) + 1 =E= 1;


*
*    --- average squared asolute difference as objective, own price and income elasticities receive a weight of 10
*        compared to cross-price elasticities
*
 FitElas_ ..

   v_obje =E=

      SUM( (R,XX1,YY1) $ (p_qx(R,XX1) and (p_qx(R,YY1) OR SAMEAS(YY1,"Ince"))),
*
*     --- first term: squared absolute differences (also in case of non-existant elasticities)
*
         (SQR( pv_elasDem(R,XX1,YY1) - p_elasDem(R,XX1,YY1)) $ (p_elasDem(R,XX1,YY1)
                                                                  or(     (not (sameas(XX1,"INPE")))
                                                                      and (not (sameas(YY1,"INPE")))))
*
*     --- second term: squared relative differences
*         (with a small correction to avoid crazy effects of very small and allow for missing elasticities)
*
        + SQR( (pv_elasDem(R,XX1,YY1) - p_elasDem(R,XX1,YY1))
              /(abs(p_elasDem(R,XX1,YY1))+0.08)) $ (p_elasDem(R,XX1,YY1)
                                                              or(     (not (sameas(XX1,"INPE")))
                                                                  and (not (sameas(YY1,"INPE")))))
          )
*
*     --- higher weights for own price and income elasticities
*
              * [1. + 9. $ (SAMEAS(XX1,YY1))   ])
* OR (SAMEAS(YY1,"Ince") and p_elasDem(R,XX1,"INCE")))


     /

*
*     --- scale the objective
*

     SUM( (R,XX1,YY1) $ (p_qx(R,XX1) and (p_qx(R,YY1) OR SAMEAS(YY1,"Ince"))),
                [1. + 9. $ (SAMEAS(XX1,YY1) )]   );

*OR (SAMEAS(YY1,"Ince") and p_elasDem(R,XX1,"INCE"))


model GL_demandSystem /F_, G_, Gi_, Xi_, DP_, Gij_,  DY_, Homog_, Addi_, Fitelas_/;



*============================   End Of File   ================================
