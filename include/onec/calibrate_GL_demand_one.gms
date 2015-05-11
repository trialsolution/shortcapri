********************************************************************************
$ontext

   CAPRI project

   GAMS file : CALIBRATE_GL_DEMAND.GMS

   @purpose  : calibrates the GL demand system
   @author   :
   @date     : 23.07.13
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy :

$offtext
********************************************************************************



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




* calibrate the Generalized Leontief Expenditure system (and the resulting demand functions)
* to the matrix of elasticities above
*===========================================================================================




* --- initialize the variables in the GL demand system


*        v_GLDemG.LO(R) = 1.E-6;

* the value of minimum commitments is somewhere between 10% and 99% of the total value
        v_GLDemF.LO(R) = 0.1 * p_valueSum(R);
        v_GLDemF.UP(R) = p_valueSum(R) * 0.99;




* non-diagonal elements of the B matrix are positive
*        V_B.LO(R,XX1,YY1) $ (NOT SAMEAS(XX1,YY1)) = 0;
        V_B.L (R,XX1,YY1) = +0.5;

* the diagonal of the B matrix is usually negative except the INPE,INPE pair
*        V_B.up(R,XX,XX) = eps;
        V_B.L (R,XX,XX) = -1.;
        V_B.L (R,"inpe","inpe") = 1.;
*        V_B.Lo (R,"inpe","inpe") = eps;

        V_B.FX(R,XX1,YY1) $ ( (NOT p_qx(R,XX1)) OR (NOT p_qx(R,YY1)) ) = 0;


* security device: no demand, no D coefficient
        v_GLparD.FX(R,XX1) $ (NOT p_qx(R,XX1)) = 0;

        v_GLparD.LO(R,XX1) $ p_qx(R,XX1) = -(p_qx(R,XX1)+0.1) * 10.;
        v_GLparD.L(R,XX1)  $ p_qx(R,XX1) =  (p_qx(R,XX1)+0.1) * 0.5;
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


        pv_elasDem.L(R,XX1,YY1) $ p_elasDem(R,XX1,YY1) = p_elasDem(R,XX1,YY1);
        pv_elasDem.FX(R,XX,XX)  $ p_elasDem(R,XX,XX) = p_elasDem(R,XX,XX);


execute_unload "before_GL_trimming.gdx";



display "budget share in GL trimming", p_budgetShare, p_valueSum;

*   --- use advanced basis to speed up solution
    GL_demandSystem.savepoint = 1;
$if exist Gl_demandSystem_p.gdx execute_loadpoint "Gl_demandSystem_p.gdx";

solve GL_demandSystem using NLP minimizing v_obje;
if(GL_demandSystem.numinfes ne 0, abort   "problem with trimming the demand system");
*
*    --- store the calibrated elasticities in the parameter of the priors
*
*       p_elasDem(R,XX1,YY1) = pv_elasDem.L(R,XX1,YY1);


       p_pdGL(R,XX1,"CUR")       = v_GLparD.L(R,XX1) $ p_qx(R,XX1);

* upper triangular part
       p_pbGL(R,XX1,YY1,"CUR")   = V_B.l(R,XX1,YY1) $ ( (XX1.pos LE YY1.pos ) and p_qx(R,XX1) and p_qx(R,YY1));
* lower triangular part
       p_pbGL(R,XX1,YY1,"CUR") $ ( (XX1.pos gt YY1.pos ) and p_pbGL(R,YY1,XX1,"CUR"))  = p_pbGL(R,YY1,XX1,"CUR");

display "GL system calibrated values:" , p_pbGL, p_pdGL;

*execute_unload "after_GL_trimming.gdx";
*abort "stopped for debugging.gdx";


*  -- the parameters below will also be used in the calibration of the supply functions
*     So here we need to set them back to zero. But first Let's store them on the appropriate parameter...

         p_store(R,XX1,"p_qx","demand")         = p_qx(R,XX1);
         p_store(R,XX1,"p_price","demand")      = p_price(R,XX1);
         p_store(R," ","p_valueSum","demand")   = p_valueSum(R);


* .. and then use option kills to get rid off unnecessary params. variables
    option kill=p_qx;
    option kill=p_price;
    option kill=p_valueSum;

    option kill=v_B;
    option kill=pv_elasDem;
    option kill=v_GLparD;
    option kill=v_GLDemF;
    option kill=v_GLDemG;
    option kill=v_GLDemGi;
    option kill=v_GLDemGij;
