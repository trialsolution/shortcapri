
* DECLARATIONS
* ============


* Note that the  calibration uses equations/variables/parameters defined in 'calibrate_demand_elasticities.gms'
* in CAPRI the declarations are in one file


parameter
         p_oriElas(R,XX1,XX1)     "Priors on the supply elasticity matrix"
;

variables
     v_ela(R,XX1,XX1)              "calibrated supply elasticity"
     v_hess(R,XX1,YY1)             "Hessian or marginal effects"
     V_LU(R,XX1,YY1)               "Lower upper cholesky decomp"
;


equations
     Hess_       "Define marginal effects from prices and quantities at given point"
     Chol_       "Cholesky decomposition of marginal effects to ensure correct curvature"
     HomogN_     "Homogeneity condition for elasticities"
     FitElasNQ_  "objective func. for supply elasticity trimming"


;

*
*    --- calculate marginal effect from price and quantity for current elasticity estimate
*        (a) the income elasticities are fixed to zero for feed and supply
*        (b) Q is defined as a netput (negative for feed and demand)
*
 Hess_(R,XX1,YY1) $ (p_qx(R,XX1) and p_qx(R,YY1) and p_price(R,XX1) and p_price(R,YY1) ) ..

  (   v_hess(R,XX1,YY1) $ (XX1.pos LE YY1.pos)
    + v_hess(R,YY1,XX1) $ (XX1.pos GT YY1.pos))

       / max(1.E-3,sqrt( abs(p_qx(R,XX1)/p_price(R,YY1)) * abs(p_qx(R,YY1)/p_price(R,XX1)) )) + 1.E-6

     =E= (v_Ela(R,XX1,YY1) * p_qx(R,XX1) / p_price(R,YY1))

       / max(1.E-3,sqrt( abs(p_qx(R,XX1)/p_price(R,YY1)) * abs(p_qx(R,YY1)/p_price(R,XX1)) )) + 1.E-6;

*
*    --- Cholesky decomp ensures correct curvature of effects
*
 Chol_(R,XX1,YY1) $ ( (XX1.pos LE YY1.pos) and p_qx(R,XX1) and p_qx(R,YY1) and p_price(R,XX1) and p_price(R,YY1)
                           and (not sameas(XX1,"INPE")) and (not sameas(YY1,"INPE"))) ..

  (    v_hess(R,XX1,YY1) $ (XX1.pos LE YY1.pos)
     + v_hess(R,YY1,XX1) $ (XX1.pos GT YY1.pos) )
       / max(1.E-3,sqrt( abs(p_qx(R,XX1)/p_price(R,YY1)) * abs(p_qx(R,YY1)/p_price(R,XX1)) )) + 1.E-6

      =E= (SUM(ZZ1 $ (p_qx(R,ZZ1) and p_price(R,ZZ1) and (NOT SAMEAS(ZZ1,"INPE"))),
                                                             V_LU(R,XX1,ZZ1) * V_LU(R,YY1,ZZ1)))
       / max(1.E-3,sqrt( abs(p_qx(R,XX1)/p_price(R,YY1)) * abs(p_qx(R,YY1)/p_price(R,XX1)) )) + 1.E-6;


  HomogN_(R,XX1) $ p_qx(R,XX1) ..


     SUM(YY1 $ p_qx(R,YY1),
       (   v_hess(R,XX1,YY1) $ (XX1.pos LE YY1.pos)
         + v_hess(R,YY1,XX1) $ (XX1.pos GT YY1.pos))

*
*        --- convert to elasticities used net prices
*
         *  (
              p_price(R,YY1) )/p_qx(R,XX1)) +1 =E= 1;




 FitElasNQ_ ..

   v_obje =E=

      SUM{ (R,XX1,YY1) $ (p_qx(R,XX1) and p_qx(R,YY1)),
*
*     --- first term: squared absolute differences (also in case of non-existant elasticities)
*
         [SQR( v_ela(R,XX1,YY1) - p_oriElas(R,XX1,YY1)) $ (p_oriElas(R,XX1,YY1)
                                                                  or(     (not (sameas(XX1,"INPE")))
                                                                      and (sameas(YY1,"INPE"))))
*
*     --- second term: squared relative differences
*         (with a small correction to avoid crazy effects of very small and allow for missing elasticities)
*
        + SQR( (v_ela(R,XX1,YY1) - p_oriElas(R,XX1,YY1))
              /(abs(p_oriElas(R,XX1,YY1))+0.05)) $ (p_oriElas(R,XX1,YY1)
                                                              or(     (not (sameas(XX1,"INPE")))
                                                                  and (not (sameas(YY1,"INPE")))))
          ]
*
*     --- higher weights for own price and income elasticities
*
              * (1. + 9. $ (SAMEAS(XX1,YY1) )) }
     /

*
*     --- scale the objective
*

     SUM( (R,XX1,YY1) $ (p_qx(R,XX1) and p_qx(R,YY1)),
                (1. + 9. $ (SAMEAS(XX1,YY1))));






 Model m_trimElas "Calibration model for normalized quadratic behavioural funcitons"
                   / Hess_,Chol_,FitElasNQ_,HomogN_/;

 m_trimElas.Limcol   = 0;
 m_trimElas.Limrow   = 0;
 m_trimElas.Solprint  = 1;
 m_trimElas.Holdfixed = 1;
 m_trimElas.Iterlim   = 10000;