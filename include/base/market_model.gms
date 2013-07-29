* DECLARATIONS, MODEL DEFINITION
* ==============================


sets
        R     'regions' /R1, R2, R3/
        XX_commodities    'agricultural commodities' / 'X1', 'X2'/
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
           data(*,*,*,*)                           "an equivalent of the capri data cube (stores most of the balances and prices)"


           p_dpCESTrade(R,*,XX)
           p_tradeFlows(R,R,XX,basCalCur)          "Physical import flows, destination first"
           p_doubleZero(R,R,XX,basCalCur)          "Double zero aggreements"
           p_impPrice(R,R,XX,basCalCur)            "import price"

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

           v_flipflop

;

positive variables v_tradeFlows, v_consPrice, v_prodPrice, v_marketPrice, v_Arm1Price, v_Arm2Price ;


 dummy.. v_flipflop =e= 10;


* Demand system
*==============

* Note that consumer price is converted to per kg (*1.E-3): that's the numeraire
*  same trick during the calibration of the demand system

GLDemandFS_(R) ..

     v_GLDemandFS(R)
      =E= SUM(XX1 $ p_pdGl(R,XX1,"CUR"),
                             v_consPrice(R,XX1) * p_pdGL(R,XX1,"CUR")*1.E-3);

 GLDemandGS_(R) ..

     v_GLDemandGS(R) =E= SUM( (XX1,YY1) $ (abs(p_pbGL(R,XX1,YY1,"CUR")) gt eps),
                                          p_pbGL(R,XX1,YY1,"CUR")
                                          * SQRT(v_consPrice(R,XX1)*v_consPrice(R,YY1)*1.E-6) );

 GLDemandGiS_(R,XX) $ (  (v_consQuant.LO(R,XX) ne v_consQuant.UP(R,XX))
                          $ DATA(R,"HCon",XX,"CUR"))  ..

     v_GLDemandGis(R,XX)
                 =E= SUM( YY1 $ (abs(p_pbGl(R,XX,YY1,"CUR")) gt eps),
                                     p_pbGL(R,XX,YY1,"CUR")
                                   * SQRT(v_consPrice(R,YY1) / data(R,"cpri",xx,"cur")) * sqrt(data(R,"cpri",xx,"cur")/v_consPrice(R,XX)));

 XiS_(R,XX)  $ (  (v_consQuant.LO(R,XX) ne v_consQuant.UP(R,XX))
                   $ DATA(R,"HCon",XX,"CUR")) ..
*
       v_consQuant(R,XX)
     =E=
         [
*      G_i/G * (Y-F)
         v_GLDemandGis(R,XX)/v_GLDemandGS(R)
           * ( DATA(R,"Ince","Levl","CUR")/DATA(R,"INHA","LEVL","CUR") - v_GLDemandFS(R))
*      plus the consumption independent of prices and income
              + p_pdGL(R,XX,"CUR")]

*          times population
              * DATA(R,"INHA","LEVL","CUR")
*          conversion from kg to tons
              * 1.E-3  ;

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
*          Demand system: Generalised Leontief form
           GlDemandFS_
           GLDemandGS_
           GLDemandGiS_
           XiS_

*          Supply: from a Normalised Quadratic profit function; linear in normalized prices
           ProdNQ_

*         CES share equations
           arm2QuantShares_
           domSalesShares_
           importShares_
*          value equations
           arm1Val_
           arm2Val_

*         balances
           ArmBal1_
           SupBalM_
           expQuant_

*         price linkages
           PPri_
           impPrice_
           CPRI_

           dummy

/;

  m_GlobalMarket.solprint = 0;

