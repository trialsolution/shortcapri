*notes:
*simplifications
* 1: no distinction between RMS and RM. they all become R
* no processing industry, no biofuels => demand is only humand consumption
* tariffs are exogenous, only ad-valorem (see p_tarAdVal)


* DECLARATIONS, MODEL DEFINITION


sets
        R     'regions' /R1, R2, R3/
        XX_commodities    'commodities' / 'X1', 'X2'/
        XX_add            'additional (non-agricultural) goods' / inpe "non-agricultural goods", ince "income elasticities"/
        basCalCur 'time' / "BAS", "CAL", "CUR"/

;

set XX_all /set.XX_commodities, set.XX_add/;

set XX1(XX_all) /set.XX_commodities, set.XX_add/;
set XX(XX1) /set.XX_commodities/;


alias(R,R1,R2);
alias(R,RUNR);


alias(XX,YY,ZZ);
alias(XX1,YY1,ZZ1);

parameters
          data(*,*,*,*) 'capri data cube'


           p_dpCESTrade(R,*,XX)
           p_tradeFlows(R,R,XX,basCalCur)                "Physical import flows, destination first"
           p_doubleZero(R,R,XX,basCalCur)                    "Double zero aggreements"
           p_impPrice(R,R,XX,basCalCur)                     "import price"

          p_tarAdVal(R,R,XX)                      "Ad-valorem tariffs"


*       parameters of the demand system
           p_pdGL(R,XX1,basCalCur)                "Constant terms in Generalised Leontief Expenditure function"
           p_pbGL(R,XX1,YY1,basCalCur)            "Terms in form of square roots in Generalised Leontief Expenditure function"

*       parameters of the supply functions
           p_hessNQSupp(R,*,*,basCalCur)          "Hessian of normalised quadratic profit function"
           p_cnstNQSupp(R,*)                      "Constant of normalised quadratic profit function"

*       Armington substitution elasticities ( 2 levels )
           p_rhoArm1(R,XX1)                         "Substitution parameter first stage"
           p_rhoArm2(R,XX1)                         "Substitution parameter second stage"

;

parameter p_results(*,*,*,*,*) "reporting parameter";


equations

* Behavioural part
*-----------------
*          Demand system: Generalised Leontief form
           GlDemandFS_(R)              "Part of GL demand function"
           GLDemandGS_(R)              "Part of GL demand function"
           GLDemandGiS_(R,XX)          "Part of GL demand function"
           XiS_(R,XX)                  "Part of GL demand function"
*          Supply: from a Normalised Quadratic profit function; linear in normalized prices
           ProdNQ_(R,XX1)              "Supply function from NQ profit function"


* Armington system
*-----------------
*         CES share equations
           arm2QuantShares_(R,XX)     "Share equation for arm2Quant in total demand, CGE style"
           domSalesShares_(R,XX)      "Share equation for doemstic sales in total demand, CGE style"
           importShares_(R,R1,XX)    "Share equations for imports flows in total imports, CGE style"
*          value equations
           arm1Val_(R,XX)             "Value of domestic sales plus imports"
           arm2Val_(R,XX)             "Value of imports"

* Balances
*---------
           ArmBal1_(R,XX)             "Adding up of human consumption feed and processing"
           SupBalM_(R,XX)             "Supply balance of all individual countries"
           expQuant_(R,XX)            "Sum of exports of each region"

* Price linkages
*---------------
           PPri_(R,XX)               "Producer price inside aggregates"
           impPrice_(R,R1,XX)        "Definition of import price"
           CPRI_(R,XX)               "Consumer Price"


* dummy for NLP formulation
           dummy
;


variables

*
* --- intermediate variables of GL demand function
*
           v_GLDemandFS(R)
           v_GLDemandGS(R)
           v_GLDemandGiS(R,XX1)

* --- price variables
           v_marketPrice(R,XX)               "Domestic maret price [Euro/ton], anchor price for producer/consumer prices"
           v_prodPrice(R,XX1)                "Producer price [Euro/ton]"
           v_consPrice(R,XX1)                 "Consumer price [Euro/ton]"
           v_impPrice(R,R1,XX)                "Import price for goods from RM1 in RM after border, including tariffs, [curr Euro/ton]"
           v_arm1Price(R,XX1)                  "Armington first stage price [Euro/utility point]"
           v_arm2Price(R,XX)                   "Armington second stage price [Euro/uility point]"

* --- quantity variables
           v_expQuant(R,XX1)                 "Sum of exports [1000 tons]"
           v_arm1Quant(R,XX)                   "Armington first stage aggregate, beware: uility points, in calibration point [1000 tons]"
           v_arm2Quant(R,XX1)                  "Armington second stage aggregate, beware: uility points, in calibration point [1000 tons]"
           v_consQuant(R,XX1)              "Human consumption, total (for all population) [1000 tons]"
           v_prodQuant(R,XX1)              "Production [1000 tons]"
           v_tradeFlows(R,R,XX)           "Imports matrix by origin [1000 tons]"
           v_domSales(R,XX)                "Domestic sales [1000 tons]"
           pv_prodPriceMarg(R,XX)          "price margin (calibrated)"


           v_dummy

;

positive variables v_tradeFlows, v_consPrice, v_prodPrice, v_marketPrice, v_Arm1Price, v_Arm2Price ;





* Demand system
*==============

* consumer price is converted to per kg (*1.E-3); same during the calibration of the demand system

GLDemandFS_(R) ..

     v_GLDemandFS(R)
      =E= SUM(XX1 $ p_pdGl(R,XX1,"CUR"),
                             v_consPrice(R,XX1) * p_pdGL(R,XX1,"CUR")*1.E-3);

 GLDemandGS_(R) ..
     v_GLDemandGS(R) =E= SUM( (XX1,YY1) $ p_pbGL(R,XX1,YY1,"CUR"),
                                          p_pbGL(R,XX1,YY1,"CUR")
                                          * SQRT(v_consPrice(R,XX1)*v_consPrice(R,YY1)*1.E-6) );

 GLDemandGiS_(R,XX) $ (  (v_consQuant.LO(R,XX) ne v_consQuant.UP(R,XX))
                          $ DATA(R,"HCon",XX,"CUR"))  ..

     v_GLDemandGis(R,XX)
                 =E= SUM( YY1 $ p_pbGl(R,XX,YY1,"CUR"),
                                     p_pbGL(R,XX,YY1,"CUR")
                                   * SQRT(v_consPrice(R,YY1) / data(R,"cpri",xx,"cur")) * sqrt(data(R,"cpri",xx,"cur")/v_consPrice(R,XX)));

 XiS_(R,XX)  $ (  (v_consQuant.LO(R,XX) ne v_consQuant.UP(R,XX))
                   $ DATA(R,"HCon",XX,"CUR")) ..
*
       v_consQuant(R,XX)
     =E=
       ((v_GLDemandGis(R,XX)/v_GLDemandGS(R)
           * ( DATA(R,"Ince","Levl","CUR")/DATA(R,"INHA","LEVL","CUR") - v_GLDemandFS(R))
              + p_pdGL(R,XX,"CUR")) * DATA(R,"INHA","LEVL","CUR")) * 1.E-3  ;

* Supply
*=======

ProdNQ_(R,XX1)
                     $ DATA(R,"Prod",XX1,"CUR")  ..

    v_prodQuant(R,XX1)
       =E= (p_cnstNQSupp(R,XX1)
            + SUM( YY1 $ (DATA(R,"PPri",YY1,"CUR") and (NOT SAMEAS(YY1,"INPE"))),
                           p_hessNQSupp(R,XX1,YY1,"CUR")
                         * v_prodPrice(R,YY1)/v_prodPrice(R,"Inpe")));


* Armington
*==========

 arm2QuantShares_(R,XX) $ ( v_arm2Quant.up(R,XX) $ (v_arm2Quant.LO(R,XX) ne v_arm2Quant.UP(R,XX)))  ..

     v_arm2Quant(R,XX) /(DATA(R,"ARM2",XX,"CUR")+1.)

              =E=  [ v_arm1Quant(R,XX)/(DATA(R,"ARM2",XX,"CUR")+1.)
                 * p_dpCESTrade(R,"RW",XX)
                 * [ v_arm1Price(R,XX) / v_arm2Price(R,XX) ] ** p_rhoArm1(R,XX) ];


 domSalesShares_(R,XX) $ ( v_domSales.up(R,XX) $ (v_domSales.LO(R,XX) ne v_domSales.UP(R,XX))) ..

     v_domSales(R,XX)/(p_tradeFlows(R,R,XX,"CUR")+1.)

              =E= [ v_arm1Quant(R,XX)/(p_tradeFlows(R,R,XX,"CUR")+1.)
                 * p_dpCESTrade(R,R,XX)
                 * [ v_arm1Price(R,XX) / v_marketPrice(R,XX) ] ** p_rhoArm1(R,XX) ];


 importShares_(R,R1,XX) $ ( p_tradeFlows(R,R1,XX,"Cur") $ (not SAMEAS(R,R1))
                          $  (v_tradeFlows.LO(R,R1,XX) NE v_tradeFlows.UP(R,R1,XX)) ) ..

     v_tradeFlows(R,R1,XX)/(p_tradeFlows(R,R1,XX,"Cur")+1.)

              =E= [ v_arm2Quant(R,XX)/(p_tradeFlows(R,R1,XX,"Cur")+1.)
                 * p_dpCESTrade(R,R1,XX)
                 * [ v_arm2Price(R,XX) / v_impPrice(R,R1,XX) ] ** p_rhoArm2(R,XX) ];



 arm1Val_(R,XX) $ ( (v_domSales.up(R,XX) ne 0) or (v_Arm2Quant.up(R,XX) ne 0) )..
*
       v_arm1Price(R,XX) * v_arm1Quant(R,XX) * 0.001
     / (data(R,"Arm1P",XX,"CUR") * DATA(R,"Arm1",XX,"CUR") * 0.001 + 1)

       =E= (   v_marketPrice(R,XX) * v_domSales(R,XX)  $ DATA(R,"DSales",XX,"CUR")
             + v_arm2Price(R,XX)   * v_arm2Quant(R,XX) $ DATA(R,"Arm2",XX,"CUR")   ) * 0.001
     / (data(R,"Arm1P",XX,"CUR") * DATA(R,"Arm1",XX,"CUR") * 0.001 + 1);




 arm2Val_(R,XX) $ ( v_Arm2Quant.lo(R,XX) ne v_Arm2Quant.up(R,XX)) ..

    v_arm2Price(R,XX) * v_arm2Quant(R,XX) * 0.001
    / ( DATA(R,"Arm2P",XX,"CUR") * DATA(R,"Arm2",XX,"CUR") * 0.001 + 1)

       =E=
             [SUM(R1 $ ((NOT SAMEAS(R,R1)) $ p_tradeFlows(R,R1,XX,"CUR")),
                      v_impPrice(R,R1,XX)*v_tradeFlows(R,R1,XX))/1000.]
    / ( DATA(R,"Arm2P",XX,"CUR") * DATA(R,"Arm2",XX,"CUR") * 0.001 + 1);



* Balances
*=========

 ArmBal1_(R,XX) $ (v_arm1Quant.lo(R,XX) ne v_arm1Quant.up(R,XX)) ..

     v_arm1Quant(R,XX) / [DATA(R,"arm1",XX,"CUR") + 1]

            =E=
*                     --- human consumption
                          v_consQuant(R,XX)  $ DATA(R,"HCon",XX,"CUR")

                      /  [DATA(R,"Arm1",XX,"CUR") + 1] ;


 SupBalM_(R,XX) ..

     v_domSales(R,XX)
        / (DATA(R,"DSales",XX,"CUR")+DATA(R,"Prod",XX,"CUR") + 1 )

         =E=

        (   v_prodQuant(R,XX) $ DATA(R,"Prod",XX,"CUR")
            - v_expQuant(R,XX)
        )
        /( DATA(R,"DSales",XX,"CUR")+DATA(R,"Prod",XX,"CUR")  + 1 );



 expQuant_(R,XX) $ (v_expQuant.lo(R,XX) ne v_expQuant.up(R,XX)) ..

        v_expQuant(R,XX)
        /(DATA(R,"Exports",XX,"CUR") + 1)
*
         =E= SUM(R1 $ ((NOT SAMEAS(R,R1))$ p_tradeFlows(R1,R,XX,"CUR")), v_tradeFlows(R1,R,XX))
        /(DATA(R,"Exports",XX,"CUR") + 1);


* Price Linkages
*===============
PPri_(R,XX) $ DATA(R,"Prod",XX,"CUR")   ..

      v_prodPrice(R,XX)/(DATA(R,"PPRI",XX,"CUR")+1)
         =E=

*     --- Internal market prices from OECD calculations
          [(v_marketPrice(R,XX) + DATA(R,"PSE",XX,"CUR") )
*     --- difference in percentage terms from base year
             * pv_prodPriceMarg(R,XX)

*     --- plus area / per head payment
             + (DATA(R,"AREP",XX,"CUR")/DATA(R,"Yild",XX,"CUR")) $ DATA(R,"Yild",XX,"CUR")] / (DATA(R,"PPRI",XX,"CUR")+1);
*

CPRI_(R,XX) $ data(R,"Hcon",XX,"CUR")..

       v_consPrice(R,XX) =e= v_arm1Price(R,XX) + data(R,"CSE",XX,"cur");



impPrice_(R,R1,XX) $ (p_tradeFlows(R,R1,XX,"CUR") and (not SAMEAS(R,R1))) ..

     v_impPrice(R,R1,XX)/(p_impPrice(R,R1,XX,"CUR")+1)   =E=
*

       (v_marketPrice(R1,XX))
*       --- add valorem tariff
          *  [ 1. + 0.01 * p_tarAdVal(R,R1,XX) $ ( (NOT p_doubleZero(R,R1,XX,"CUR")) $ (NOT SAMEAS(R,R1)))]
         /(p_impPrice(R,R1,XX,"CUR")+1);



dummy.. v_dummy =e= 10;


model m_GlobalMarket /
*          Demand system: Generalised Leontief form
           GlDemandFS_.v_GLDemandFS,
           GLDemandGS_.v_GLDemandGS,
           GLDemandGiS_.v_GLDemandGiS,
           XiS_.v_consQuant,

*          Supply: from a Normalised Quadratic profit function; linear in normalized prices
           ProdNQ_.V_prodQuant,

*         CES share equations
           arm2QuantShares_.v_Arm2Quant,
           domSalesShares_.v_domSales,
           importShares_.v_tradeFlows,
*          value equations
           arm1Val_.v_Arm1Price,
           arm2Val_.v_Arm2Price,

*         balances
           ArmBal1_.v_Arm1Quant,
           SupBalM_.v_marketPrice,
           expQuant_.v_expQuant,

*         price linkages
           PPri_.v_prodPrice,
           impPrice_.v_impPrice,
           CPRI_.v_consPrice

/;



model m_GlobalMarket_nlp /

* Behavioural part
*-----------------
*          Demand system: Generalised Leontief form
           GlDemandFS_
           GLDemandGS_
           GLDemandGiS_
           XiS_
*          Supply: from a Normalised Quadratic profit function; linear in normalized prices
           ProdNQ_


* Armington system
*-----------------
*         CES share equations
           arm2QuantShares_
           domSalesShares_
           importShares_
*          value equations
           arm1Val_
           arm2Val_

* Balances
*---------
           ArmBal1_
           SupBalM_
           expQuant_

* Price linkages
*---------------
           PPri_
           impPrice_
           CPRI_


* dummy for NLP formulation
           dummy
/;


* INPUT DATA
option seed=1234;


* substitution elasticities
p_rhoArm2(R,XX) = 10;
p_rhoArm1(R,XX) = 8;



* Policy settings...
* tariffs
 p_tarAdVal(R,R1,XX) $ (not (sameas(r,r1))) = 10;
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


* starting values for model variables





    v_tradeFlows.l(R,R1,XX) = p_tradeFlows(R,R1,XX,"Cur");
    v_tradeFlows.fx(r,r,XX) = 0;
    v_arm2Quant.l(R,XX) = DATA(R,"ARM2",XX,"CUR");
    v_arm2Price.l(R,XX) = DATA(R,"ARM2P",XX,"CUR");
    v_impPrice.l(R,R1,XX) = p_impPrice(R,R1,XX,"CUR") ;
    v_impPrice.fx(R,R,XX) = 0;
    v_arm1Price.l(R,XX) = DATA(R,"ARM1P",XX,"CUR");
    v_domSales.l(R,XX) = DATA(R,"DSales",XX,"CUR");
    v_arm1Quant.l(R,XX) = DATA(R,"ARM1",XX,"CUR");
    v_marketPrice.l(R,XX) = DATA(R,"PMRK",XX,"CUR");
    v_prodPrice.l(R,XX1) = DATA(R,"PPri",XX1,"CUR");
    v_prodPrice.FX(R,"inpe") = DATA(R,"PPri","inpe","CUR");
    v_consQuant.L(R,XX)=DATA(R,"HCon",XX,"CUR");
    v_consQuant.fx(R,"inpe")=0;
    v_expQuant.L(R,xX)= sum(R1 $ (not sameas(r,r1)), p_tradeFlows(R1,R,XX,"Cur"));
    v_consPrice.L(R,XX) = DATA(R,"CPRI",XX,"CUR");
    v_consPrice.fx(R,"inpe") = DATA(R,"CPRI","inpe","CUR");



     v_prodQuant.L(R,XX) = data(R,"PROD",XX,"CUR");


* CALIBRATION (WITH TEST)



parameters
         p_elasDem(R,XX1,YY1) "demand elasticities"
         p_elasSup(R,XX1,YY1) "supply elasticities"
;



$include 'calibrate_demand_elasticities.gms'
$include 'calibrate_supply_elasticities.gms'


*$stop

* calibrate producer price margins
    pv_prodPriceMarg.fx(R,XX) $ DATA(R,"Prod",XX,"CUR")

   =  [v_prodPrice.L(R,XX) -  (DATA(R,"AREP",XX,"CUR")/DATA(R,"Yild",XX,"CUR")) $ DATA(R,"Yild",XX,"CUR")]
       /
      [v_marketPrice.L(R,XX) + DATA(R,"PSE",XX,"CUR")];




* Calibrate the distribution parameters of the Armington share equations
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

display "check Armington system", p_checkArmington;


* shift the supply function to observed "prod"
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

display "check NQ production function calibratin", p_checkProdNQ;




* starting values after calibration of the demand system
     v_GLDemandFS.L(R) = SUM(XX1 $ p_pdGl(R,XX1,"CUR"), v_consPrice.L(R,XX1) * p_pdGL(R,XX1,"CUR")*1.E-3);

     v_GLDemandGS.L(R) = SUM( (XX1,YY1) $ p_pbGL(R,XX1,YY1,"CUR"),
                 p_pbGL(R,XX1,YY1,"CUR") * SQRT(v_consPrice.L(R,XX1)*v_consPrice.L(R,YY1)*1.E-6) );

     v_GLDemandGis.L(R,XX1) $ (v_consPrice.L(R,XX1) gt eps) = SUM( YY1 $ p_pbGl(R,XX1,YY1,"CUR"),
                                     p_pbGL(R,XX1,YY1,"CUR")
                                     * SQRT(v_consPrice.L(R,YY1) / v_consPrice.L(R,XX1)));


* test the calibration of the demand system
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
$batinclude 'save_results.gms' '"BAS"'



solve m_GlobalMarket using mcp;
*solve m_GlobalMarket_nlp maximizing v_dummy using nlp;


* store the result of the test run on 'CAL'
$batinclude 'save_results.gms' '"CAL"'


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

*$stop




* SCENARIO (FTA between R1 and R2)
*================================

 p_doubleZero("R1","R2",XX,"CUR")  = 1;
 p_doubleZero("R2","R1",XX,"CUR")  = 1;


solve m_GlobalMarket using mcp;


* save scenario results on "cur"
$batinclude 'save_results.gms' '"CUR"'

