$title shortcapri

* Short, demonstrative version of the CAPRI market model
*=======================================================

* 2 products, 3 regions
* supply functions derived from Normalized Quadratic profit functions
* Generalized Leontief demand system
* Two stage Armington (CES formulation)
* No distinction between RMS and RM regions. They all become R
* No processing industry, no feed, no biofuels... => demand is only humand consumption
* Tariffs are exogenous, only ad-valorem (see p_tarAdVal)


* DECLARATIONS, MODEL DEFINITION
* ==============================
$include 'market_model.gms'


* MARKET BALANCING (consistent data set at the calibration point)
* ================
$include 'data_cal.gms'



* starting values for model variables
* ===================================
$include 'prep_market.gms'



* CALIBRATION PHASE (WITH TESTS)
* =======================
$include 'calibration.gms'



* SIMULATION engine starts here
* ========================


*## SCENARIO (FTA between R1 and R2, implemented here simply as a double zero agreement)

 p_doubleZero("R1","R2","X1","CUR")  = 1;
 p_doubleZero("R2","R1","X1","CUR")  = 1;


* MCP formulation
solve m_GlobalMarket using mcp;

* save scenario results on "sim_1"
$batinclude 'save_results.gms' '"SIM_1"' 'p_tarAdval'




*=============================================
* introduce endogenous tariffs under TRQ
* 2 ways of doing it:
* (1) with sigmoid function
* (2) by orthogonality constraints in MCP
*=============================================

parameter
        p_trqBilat(R,R,XX,*,*) "bilateral TRQ regimes"
        p_trq_fillrate(R,R,XX,*) "fill rate of the TRQs"
        p_premium_rate(R,R,XX)   "quota premium rate"
;

variable
        v_tariff(R,R,XX)  "applied tariff rate (ad valorem)"

;

equation
        impPrice_trq_(R,R,XX)   "import price with endogenous applied tariff rates"
;

impPrice_trq_(R,R1,XX) $ (p_tradeFlows(R,R1,XX,"CUR") and (not SAMEAS(R,R1))) ..

     v_impPrice(R,R1,XX)/(p_impPrice(R,R1,XX,"CUR")+1)   =E=
*

       (v_marketPrice(R1,XX))
*       --- add valorem tariff
          *  [ 1. + 0.01 * v_tariff(R,R1,XX) $ ( (NOT p_doubleZero(R,R1,XX,"CUR")) $ (NOT SAMEAS(R,R1)))]
         /(p_impPrice(R,R1,XX,"CUR")+1);

* you can skip the sigmoid representation and go to the orthogonal constraints solution here
*$goto orthogonal

equation
        trq_sigmoid_(R,R,XX) "calculates endogenous multiplier (0,1) ==> endogenous applied tariff rate under TRQ"
        applied_tariff_(R,R,XX) "applied tariff rates"
;

variable
        v_trq_multiplier(R,R,XX) "endogenous multiplier (0,1) ==> endogenous applied tariff rate under TRQ"
;



scalar sigmoid_slope "slope term of the sigmoid function (to make it steep enough)";
parameter  p_sigmoid_calib(R,R1,XX) "calibration term of the sigmoid function";

sigmoid_slope = 400;
p_sigmoid_calib(R,R1,XX) =0;



trq_sigmoid_(R,R1,XX) $ [p_trqBilat(R,R1,XX,"trqnt","cur")  $  p_tradeFlows(R,R1,XX,"CUR")] ..

         v_trq_multiplier(R,R1,XX)
                          =e= sigmoid[ sigmoid_slope * (v_tradeFlows(R,R1,XX) + p_sigmoid_calib(R,R1,XX) - p_trqBilat(R,R1,XX,"trqnt","cur") )
                                                        / p_trqBilat(R,R1,XX,"trqnt","cur")
                                       ];


applied_tariff_(R,R1,XX) $ [p_trqBilat(R,R1,XX,"trqnt","cur")  $  p_tradeFlows(R,R1,XX,"CUR")] ..

          v_tariff(R,R1,XX) =e= p_trqBilat(R,R1,XX,"taPref","cur")
                              + [v_trq_multiplier(R,R1,XX) * (p_trqBilat(R,R1,XX,"taMFN","cur") - p_trqBilat(R,R1,XX,"taPref","cur"))];


* modified model under        trq regime

model m_GlobalMarket_trq /
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
           impPrice_trq_.v_impPrice,
           CPRI_.v_consPrice,

*         TRQ instruments
          trq_sigmoid_.v_trq_multiplier,
          applied_tariff_.v_tariff

/;



* we assume that the baseline tariffs are the same as with the original model
* => no need for a full re-calibration, only the sigmoid curve needs to be calibrated (see calculation below)


* === check if the model is still calibrated

* no FTAs in baseline (revert scenario)
 p_doubleZero("R1","R2",XX,"CUR")  = 0;
 p_doubleZero("R2","R1",XX,"CUR")  = 0;


* === initialize new variables (baseline values)
v_tariff.L(R,R1,XX) = p_tarAdVal(R,R1,XX);

* --- assume an (arbitrary) fill rate in the baseline between 'R1' and 'R3'; let it be a small underfill
p_trqBilat('R1','R3',XX,"trqnt","cur") =   p_tradeFlows('R1','R3',XX,"Cur") * 1.03;
*p_trqBilat('R3','R1',XX,"trqnt","cur") =   p_tradeFlows('R3','R1',XX,"Cur") * 1.03;

* --- following the Bouet et al. method it follows that the applied rate is an arithmetic average of MFN and Pref
*p_trqBilat(R,R1,XX,"taPref","cur") $ p_trqBilat(R,R1,XX,"trqnt","cur") =  v_tariff.L(R,R1,XX) / 2;
p_trqBilat(R,R1,XX,"taPref","cur") $ p_trqBilat(R,R1,XX,"trqnt","cur") =  0;
p_trqBilat(R,R1,XX,"taMFN","cur") $ p_trqBilat(R,R1,XX,"trqnt","cur") =  v_tariff.L(R,R1,XX) * 3;


p_premium_rate(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")
               = [v_tariff.L(R,R1,XX) -  p_trqBilat(R,R1,XX,"taPref","cur")]
                 / [  p_trqBilat(R,R1,XX,"taMFN","cur") - p_trqBilat(R,R1,XX,"taPref","cur") ];


v_trq_multiplier.L(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")
                = p_premium_rate(R,R1,XX);

* calibrate the sigmoid curves
* note that the inverse of the sigmoid function is the logit(x) = log(x) - log(1-x)
         p_sigmoid_calib(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")
                 =
                 { p_trqBilat(R,R1,XX,"trqnt","cur") * [log(p_premium_rate(R,R1,XX)) - log(1 - p_premium_rate(R,R1,XX))] / sigmoid_slope }
                 + p_trqBilat(R,R1,XX,"trqnt","cur") - p_tradeFlows(R,R1,XX,"CUR");


* fix those tariffs without TRQ
v_tariff.FX(R,R1,XX) $ (not p_trqBilat(R,R1,XX,"trqnt","cur")) = p_tarAdVal(R,R1,XX);


* MCP formulation
solve m_GlobalMarket_trq using mcp;


* store the result of the test run in the p_results parameter
$batinclude 'save_results.gms' '"CAL2"' 'v_tariff.L'


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


display "test calibration, trq's with sigmoid function", p_checkPrices,p_checkBalances;


p_trq_fillrate(R,R1,XX,"CAL2") $ p_trqBilat(R,R1,XX,"trqnt","cur")
               =   v_tradeFlows.L(R,R1,XX) / p_trqBilat(R,R1,XX,"trqnt","cur");



* SCENARIO UNDER TRQ regime
* =========================

* let's repeat the FTA scneario, but now the basline assumes TRQ regimes!
* first solve it with sigmoid representation...
* ... then with the orthogonal constraints representation
* ------------


 p_doubleZero("R1","R2","X1","CUR")  = 1;
 p_doubleZero("R2","R1","X1","CUR")  = 1;

* MCP formulation
solve m_GlobalMarket_trq using mcp;

* save scenario results on "sim_trq"
$batinclude 'save_results.gms' '"sim_trq"' 'v_tariff.L'

p_trq_fillrate(R,R1,XX,"sim_trq") $ p_trqBilat(R,R1,XX,"trqnt","cur")
               =   v_tradeFlows.L(R,R1,XX) / p_trqBilat(R,R1,XX,"trqnt","cur");




$label orthogonal
* ----------------
* introduce endogenous tariff under TRQ with the orthogonality conditions approach
* ==================

variables
         v_import_in(R,R,XX) "in quota imports"
         v_import_out(R,R,XX) "out of quota imports"
         v_quota_premium_rate(R,R,XX) "quota premium rate"
;

equations
         import_identity_(R,R,XX) "imports equals inquota plus out of quota"
         regime_underfill_(R,R,XX)  "upper bound on in quota trade"
         bound_premium_rate_(R,R,XX) "upper bound on premium rate"
         applied_tariff_orth_(R,R,XX) "premium rate"
;



import_identity_(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")..
                 v_tradeFlows(R,R1,XX) =g=  v_import_in(R,R1,XX) + v_import_out(R,R1,XX);

regime_underfill_(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")..
           p_trqBilat(R,R1,XX,"trqnt","cur") - v_import_in(R,R1,XX) =g= 0;

bound_premium_rate_(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")..
           p_trqBilat(R,R1,XX,"taMFN","cur") - p_trqBilat(R,R1,XX,"taPref","cur") =g= v_quota_premium_rate(R,R1,XX) ;


applied_tariff_orth_(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")..
            v_tariff(R,R1,XX)   =e=   p_trqBilat(R,R1,XX,"taPref","cur") + v_quota_premium_rate(R,R1,XX) ;



* modified model under        trq regime --- orthogonality conditions

model m_GlobalMarket_orth /
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
           impPrice_trq_.v_impPrice,
           CPRI_.v_consPrice,

*         TRQ instruments
          applied_tariff_orth_.v_tariff,
          import_identity_.v_import_in,
          regime_underfill_.v_quota_premium_rate,
          bound_premium_rate_.v_import_out
/;




* === check if the model is still calibrated

* no FTAs in baseline (revert scenario changes)
 p_doubleZero("R1","R2",XX,"CUR")  = 0;
 p_doubleZero("R2","R1",XX,"CUR")  = 0;


* === initialize new variables (baseline values)
v_tariff.L(R,R1,XX) = p_tarAdVal(R,R1,XX);

* --- the fill rate must be 100% so the premium rate can be chosen arbitrarily
p_trqBilat('R1','R3',XX,"trqnt","cur") =   p_tradeFlows('R1','R3',XX,"Cur") * 1.;
p_trqBilat(R,R1,XX,"taPref","cur") $ p_trqBilat(R,R1,XX,"trqnt","cur") =  0;
p_trqBilat(R,R1,XX,"taMFN","cur") $ p_trqBilat(R,R1,XX,"trqnt","cur") =  v_tariff.L(R,R1,XX) * 3;




v_quota_premium_rate.LO(R,R1,XX) = 0;
v_quota_premium_rate.UP(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqNT","cur")
                      = p_trqBilat(R,R1,XX,"taMFN","cur") - p_trqBilat(R,R1,XX,"taPref","cur");

v_quota_premium_rate.L(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")    =  v_tariff.L(R,R1,XX) -  p_trqBilat(R,R1,XX,"taPref","cur");
v_quota_premium_rate.FX(R,R1,XX) $ (not p_trqBilat(R,R1,XX,"trqnt","cur") ) = 0  ;


v_import_in.LO(R,R1,XX) = 0;
v_import_out.LO(R,R1,XX) = 0;

v_import_in.L(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")
            = min(p_tradeFlows(R,R1,XX,"cur"), p_trqBilat(R,R1,XX,"trqnt","cur"));

v_import_in.FX(R,R1,XX) $ (not p_trqBilat(R,R1,XX,"trqnt","cur")) = 0;

v_import_out.L(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")
            = p_tradeFlows(R,R1,XX,"cur") - v_import_in.L(R,R1,XX);

v_import_out.FX(R,R1,XX) $ (not p_trqBilat(R,R1,XX,"trqnt","cur")) = 0;


* fix those tariffs without TRQ
v_tariff.FX(R,R1,XX) $ (not p_trqBilat(R,R1,XX,"trqnt","cur")) = p_tarAdVal(R,R1,XX);


* MCP formulation
solve m_GlobalMarket_orth using mcp;


* store the result of the test run on 'CAL'
$batinclude 'save_results.gms' '"CAL_orth"' 'v_tariff.L'


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

p_trq_fillrate(R,R1,XX,"CAL_orth") $ p_trqBilat(R,R1,XX,"trqnt","cur")
               =   v_tradeFlows.L(R,R1,XX) / p_trqBilat(R,R1,XX,"trqnt","cur");


* ------------
* let's repeat the FTA scneario but now under the TRQ regime!
* ------------


 p_doubleZero("R1","R2","X1","CUR")  = 1;
 p_doubleZero("R2","R1","X1","CUR")  = 1;

* MCP formulation
solve m_GlobalMarket_orth using mcp;

* save scenario results on "sim_orth"
$batinclude 'save_results.gms' '"sim_orth"' 'v_tariff.L'

p_trq_fillrate(R,R1,XX,"sim_orth") $ p_trqBilat(R,R1,XX,"trqnt","cur")
               =   v_tradeFlows.L(R,R1,XX) / p_trqBilat(R,R1,XX,"trqnt","cur");




* SAVE ALL RESULTS IN A GDX container
* ====================================

execute_unload 'results.gdx';

