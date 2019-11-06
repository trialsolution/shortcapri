*<%REGION File header%>
*=============================================================================
* File      : calibrate_NQ_supply_one.gms
* Author    : mihalyh
* Version   : 1.0
* Date      : 30.07.2013 11:29:19
* Changed   : 30.07.2013 12:04:32
* Changed by: mihalyh
* Remarks   :
$ontext

$offtext
*=============================================================================
*<%/REGION File header%>

*
*    --- security device (delete possible remaining values from demand system calibration; shared parameters)
*
option kill = p_oriElas;
option kill = p_qx;
option kill = p_price;
option kill = p_valueSum;

*
*       --- set base data: production quantities and producer prices
*                          (exclude special cases where production is determined not by
*                           the profit function, but e.g. by processing equations)
*
*
        p_qx(R,XX)    = DATA(R,"Prod",XX,"Cur") ;
*
* prices are normalized with "INPE.PPRI"
        p_price(R,XX)  = (DATA(R,"PPri",XX,"Cur")/DATA(R,"PPri","Inpe","Cur")) $ p_qx(R,XX);
*
*       --- revenues from agricultural goods
*
        p_valueSum(R)  = SUM(XX, p_qx(R,XX) * p_price(R,XX));
*
        p_price(R,"Inpe")  = 1.;

        p_qx(R,"Inpe") = -(p_valueSum(R) * 0.3);

* original elasticities
        p_oriElas(R,XX1,XX1) = p_elasSup(R,XX1,XX1);
        p_oriElas(R,XX1,YY1) $ (not sameas(XX1,YY1)) = 0.1;
*



     V_LU.l(R,XX1,YY1) = 0;
     V_LU.fx(R,XX1,YY1) $ ((NOT p_qx(R,XX1)) or (NOT p_qx(R,YY1))) = 0;
     V_LU.l(R,XX1,YY1)  $ (p_qx(R,XX1) and p_qx(R,YY1) and (XX1.pos LT YY1.pos))  = -0.001;
*     V_LU.l(R,XX1,XX1)  $ p_qx(R,XX1) = Sqrt( MAX(1.E-04,abs(v_ela.l(R,XX1,XX1) * p_qx(R,XX1) / (p_priceN(R,XX1) + p_price(R,XX1) $ (not p_priceN(R,XX1))))));
     V_LU.l(R,XX1,XX1)  $ p_qx(R,XX1) = Sqrt( MAX(1.E-04,abs(p_oriElas(R,XX1,XX1) * p_qx(R,XX1) / p_price(R,XX1)))) ;

     V_LU.UP(R,XX1,YY1) $ (p_qx(R,XX1) and p_qx(R,YY1)) =  SQRT(MAX(1.E-04,abs(p_qx(R,XX1)*p_qx(R,YY1))))*100;
     V_LU.lo(R,XX1,XX1) $ p_qx(R,XX1) = max(1.E-8,sqrt( abs( 0.01 * p_qx(R,XX1) / p_price(R,XX1))));


* L is upper triangular
     V_LU.fx(R,XX1,YY1) $ (XX1.pos GT YY1.pos)    = 0;


    V_LU.l(R,XX1,YY1) $ ( (NOT p_qx(R,YY1)) or (NOT p_qx(R,XX1))) = 0;



* initialize Hessian with L^T * L
     v_hess.l(R,XX1,YY1) $ ( (xx1.pos LE YY1.pos) and p_qx(R,XX1) and p_qx(R,YY1) and p_price(R,XX1) and p_price(R,YY1)
                                and (not sameas(xx1,"INPE")) and (not sameas(yy1,"INPE")))
        = SUM( ZZ1 $ (p_qx(R,ZZ1) and p_price(R,ZZ1) and (not sameas(ZZ1,"INPE"))), V_LU.l(R,XX1,ZZ1) * V_LU.l(R,YY1,ZZ1));

     v_hess.l(R,YY1,XX1) $ ( (xx1.pos GT YY1.pos) and p_qx(R,XX1) and p_qx(R,YY1) and p_price(R,XX1) and p_price(R,YY1)
                                and (not sameas(xx1,"INPE")) and (not sameas(yy1,"INPE")))

        = SUM( ZZ1 $ (p_qx(R,ZZ1) and p_price(R,ZZ1) and (not sameas(ZZ1,"INPE"))), V_LU.l(R,XX1,ZZ1) * V_LU.l(R,YY1,ZZ1));

     v_hess.FX(R,XX1,YY1) $ ( (not p_qx(R,XX1)) or (not p_qx(R,YY1)) ) = 0.;



* initialize Hessian: Epsilon_i,j = H_i,j * p_j / q_i
    v_ela.L(R,XX1,YY1) $ (p_qx(R,XX1) and p_qx(R,YY1) and p_price(R,XX1) and p_price(R,YY1))
       =  ((  v_hess.l(R,XX1,YY1) $ (XX1.pos LE YY1.pos)
            + v_hess.l(R,YY1,XX1) $ (XX1.pos GT YY1.pos)) * p_price(R,YY1) / p_qx(R,XX1));


*
*    ---- calculate residually from homogeniety condition
*
     v_ela.L(R,XX1,"INPE")  = -SUM(YY1 $ (p_qx(R,YY1) and (not sameas(YY1,"INPE"))), v_ela.l(R,XX1,YY1));

*
     v_hess.l(R,XX1,"INPE")    = v_ela.L(R,XX1,"INPE") * p_qx(R,XX1)/p_price(R,"INPE");

     v_hess.l(R,"INPE",XX1)    = v_hess.l(R,XX1,"INPE");
*
     v_ela.l(R,"INPE",XX1)  = v_hess.l(R,"INPE",XX1)   * p_price(R,XX1)/p_qx(R,"INPE");
*
*    ---- calculate own price ealsticity for the non-agr. product
*         residually from homogeniety condition
*
     v_ela.L(R,"INPE","INPE") = -SUM(YY1 $ (p_qx(R,YY1) and (not sameas(yy1,"INPE"))), v_ela.l(R,"INPE",YY1));

*
*    --- try to fix the own price elasticities (reliable info)
*
     v_ela.fx(R,XX,XX) = v_ela.L(R,XX,XX);
*
*    ---- and define from there the Hessian
*
     v_hess.l(R,"INPE","INPE")   = v_ela.L(R,"INPE","INPE") * p_qx(R,"INPE")/p_price(R,"INPE");



*    initialize the objective (i.e. variable to be minimized)
  v_obje.L =

      SUM{ (RUNR,XX1,YY1) $ (p_qx(RUNR,XX1) and p_qx(RUNR,YY1)),
*
*     --- first term: squared absolute differences (also in case of non-existant elasticities)
*
         [SQR( v_ela.L(RUNR,XX1,YY1) - p_oriElas(RUNR,XX1,YY1)) $ (p_oriElas(RUNR,XX1,YY1)
                                                                  or(     (not (sameas(XX1,"INPE")))
                                                                      and (sameas(YY1,"INPE"))))
*
*     --- second term: squared relative differences
*         (with a small correction to avoid crazy effects of very small and allow for missing elasticities)
*
        + SQR( (v_ela.L(RUNR,XX1,YY1) - p_oriElas(RUNR,XX1,YY1))
              /(abs(p_oriElas(RUNR,XX1,YY1))+0.05)) $ (p_oriElas(RUNR,XX1,YY1)
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

     SUM( (RUNR,XX1,YY1) $ (p_qx(RUNR,XX1) and p_qx(RUNR,YY1)),
                (1. + 9. $ (SAMEAS(XX1,YY1))));


*execute_unload "temp\supply_trimming_before.gdx";

     m_trimElas.solprint = 1;
*     m_trimElas.iterlim  = 0;
     
     Solve m_trimElas Using NLP Minimizing v_obje;
     if(m_trimElas.numinfes ne 0, abort "trimming supply elasticities failed");

  p_elasSup(R,XX1,YY1) $ [(not sameas(XX1,"INCE")) $ (not sameas(YY1,"INCE"))] = v_ela.L(R,XX1,YY1);

  p_hessNQSupp(R,XX1,YY1,"CUR")   =  v_hess.l(R,XX1,YY1) $ (XX1.pos LE YY1.pos);

* fill up lower triangular of the Hessian (symmetric matrix)
  p_hessNQSupp(R,XX1,YY1,"CUR") $ [ (XX1.pos gt YY1.pos) and p_hessNQSupp(R,YY1,XX1,"CUR") ] = p_hessNQSupp(R,YY1,XX1,"CUR");


*execute_unload "temp\supply_trimming.gdx";
*abort "stopped for debug";

* .. and then use the option kills to clean up
    option kill=p_qx;
    option kill=p_price;
    option kill=p_valueSum;

    option kill=v_ela;
    option kill=v_LU;
    option kill=v_hess;

*============================   End Of File   ================================
