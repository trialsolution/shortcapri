$title shortcapri

* Short, demonstrative version of the CAPRI market model with ONE COMMODITY
*=======================================================

* 1 products, 3 regions
* supply functions derived from Normalized Quadratic profit functions
* Generalized Leontief demand system
* Two stage Armington (CES formulation)
* No distinction between RMS and RM regions. They all become R
* No processing industry, no feed, no biofuels... => demand is only humand consumption
* Tariffs are exogenous, only ad-valorem (see p_tarAdVal)


$offlisting


file modellog /modellog.txt/;
put modellog;

$if not exist .\results  execute  'mkdir results'
$if not exist .\temp     execute  'mkdir temp'

* The Basic market model (model definition)
* =========================================
$include 'include\onec\market_model_one.gms'


*
*    --- parameters for the money metric calculations
*
parameters
          PS_CAL(R,XX1)              "price in calib. point"
          PS_Y(R,XX1)                "price in simulation"
          p_welfareRes(R,*,XX1,*)    "Welfare result"
;

*
*    ---  Additional equations for introducing TRQ instruments
*

*=============================================
* introduce endogenous tariffs under TRQ
*  2 ways of doing it:
* (1) with sigmoid function
* (2) by orthogonality constraints in MCP
*
* The two associated models:
*  (1) m_GlobalMarket_trq
*  (2) m_GlobalMarket_orth
*=============================================


* -- some common elements for TRQ representation
*-----------------------------------------------------------------
$include  "include\trq\trq_common.gms"


* specific to the sigmoid representation
*-----------------------------------------------------------------
$include  "include\trq\trq_sigmoid.gms"

* specific to the orthogonal conditions representation
*-----------------------------------------------------------------
$include  "include\trq\trq_orthogonal.gms"


* --- Debugging parameters
parameters
          p_checkPrices(R,XX,*,*)      "differences from actual price levels and the one in the data cube"
          p_checkBalances(R,XX,*,*)    "differences from actual price levels and the one in the data cube"
          p_checkArmington(R,*,XX,*)   "checks the correct initialization of the three Armington equations"
          p_checkProdNQ(R,XX1)         "checks the calibration of the production functions (derived from NQ profit func.)"
          p_checkDemand(R,*,*)         "checks the calibration of the GL expenditure system"
;

*
*    --- LOOP on different Armington elasticities (sensitivity analysis)
*

* Note: we need to bring some parameter definitions outside the loop...
sets
    fta_countries(R)   "countries negotiating an FTA"                /R1, R2/
    third_countries(R) "third countries with respect to the FTA"
;

third_countries(R) $ (not fta_countries(R)) = yes;


parameter  p_store(R,*,*,*) "stores the initial point of the demand system";

* --- definition of the GL trimming model
$include 'include\onec\calibrate_GL_demand_model.gms'

* --- definition of the NQ trimming model
$include  'include\base\calibrate_NQ_supply_model.gms'


parameter
         p_elasSup(R,XX1,YY1)       "supply elasticities"
         p_elasSup_check(R,XX1,YY1) "debugging parameter for supplye elasticites"
;

* DATA INPUT
*===========
$include 'include\base\data_prep.gms'



*!   --- SWITCH: Put R1 exports to zero (R1 only importer country)
*
*p_tradeFlows(R,"R1",XX,"Cur") = 0;



* MARKET BALANCING (consolidation, i.e. creating a consistent data set at the calibration point)
* =============================================================================================
$include 'include\onec\data_cal_one.gms'


* starting values for model variables
*-----------------------------------
$include 'include\base\prep_market.gms'



* CALIBRATION OF ARMINGTON PLUS SHIFT OF SUPPLY FUNCTIONS (WITH TESTS)
* =======================

$include 'include\base\calibration.gms'


* --- some reporting parameters
parameters
        p_trade_diversion(R,XX,*) "measure of overall trade diversion in the system"
        p_trade_diversion_relative(R,XX,*) "measure of overall trade diversion in the system"
        p_trade_creation(R,XX,*)  "trade creation effects"
        p_trade_creation_relative(R,XX,*)  "trade creation effects relative to Arm1"
;



* RUN calibration test with the full system
*=========================================



* TEST run, (see if solving the model with the initial points gives back the calibration point)
* ---------
*option iterlim=0;
solve m_GlobalMarket using mcp;
if(m_GlobalMarket.numinfes ne 0, abort "calibration test failed -- infesasibilities");
if(m_GlobalMarket.numredef ne 0, abort "calibration test failed -- redefs");
*solve m_GlobalMarket_nlp using nlp minimizing v_flipflop;


* store the result of the test run on 'CAL'
$batinclude 'include\base\save_results.gms' '"CAL"'  'p_tarAdval'

$include 'include\base\test_calibration.gms'

$batinclude  'include\base\money_metric.gms' 'CAL'

* SIMULATION engine starts here
* =============================


*## SCENARIO (FTA between R1 and R2, implemented here simply as a double zero agreement)

 p_doubleZero("R1","R2",XX,"CUR")  = 1;
 p_doubleZero("R2","R1",XX,"CUR")  = 1;



* MCP formulation
solve m_GlobalMarket using mcp;
if(m_GlobalMarket.numinfes ne 0, abort "scenario run failed -- infesasibilities");
if(m_GlobalMarket.numredef ne 0, abort "scenario run failed -- redefs");


* save scenario results on "sim_AVE"
$batinclude  'include\base\save_results.gms' '"SIM_AVE"' 'p_tarAdval';

$batinclude  'include\base\money_metric.gms' 'SIM_AVE'

*
*   --- reporting
*
$batinclude 'include\trq\report_trade_diversion.gms' 'sim_ave';




*
*    ---   TRQs under the sigmoid representation
*

* CALIBRATION PHASE
*-----------------------------------------------------------------

*
*     ----- we assume that the applied tariff rates are the same as in the setup of the original model
*           => no need for a full re-calibration, only the sigmoid curve needs to be calibrated (see calculation below)
*
*     === BUT we check if the model is still calibrated for security reasons...

* --- no FTAs in baseline (revert modifications of the scenario)
 p_doubleZero("R1","R2",XX,"CUR")  = 0;
 p_doubleZero("R2","R1",XX,"CUR")  = 0;


* === initialize new variables (at baseline values)
v_tariff.L(R,R1,XX) = p_tarAdVal(R,R1,XX);



* === calibration of the TRQs ===

*
* --- assume a 100% fill rate in the baseline between 'R1' and 'R3';
*     (we achive this by defining the quota at the observed import level)
*
p_trqBilat('R1','R3',XX,"trqnt","cur") =   p_tradeFlows('R1','R3',XX,"Cur") * 1.0;


*
* --- we assume that the applied in the baseline represents a high level of protection <=> close to the MFN rate
*     we achieve this by defining a premium rate close to the MFN
*

*
*   --- first we set the preferential and MFN rates
*

*
  p_prefrate_init(R,R1,XX) = 0;
  p_trqBilat(R,R1,XX,"taPref","cur") $ p_trqBilat(R,R1,XX,"trqnt","cur") =  v_tariff.L(R,R1,XX) * p_prefrate_init(R,R1,XX);

*
*  --- because the sigmoid can not take zero values the baseline applied rate can not be equal to either the preferential or the MFN rate
*      => we set the MFN a little bit above the baseline applied rate
*
  p_MFNrate_init(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur") = 2.;
  p_trqBilat(R,R1,XX,"taMFN","cur") $ p_trqBilat(R,R1,XX,"trqnt","cur") =  v_tariff.L(R,R1,XX) * p_MFNrate_init(R,R1,XX);



*
*   --- the quota premium rate is the difference between the applied level and the preferential one
*
   p_premium_rate(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")
               = [v_tariff.L(R,R1,XX) -  p_trqBilat(R,R1,XX,"taPref","cur")];


*
*    --- the multiplier is between zero and one
*
   v_trq_multiplier.L(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")
                = p_premium_rate(R,R1,XX)
                 / [  p_trqBilat(R,R1,XX,"taMFN","cur") - p_trqBilat(R,R1,XX,"taPref","cur") ];

*
*    --- calibration of the sigmoid curves
*        we shift the sigmoid curve so that the intersection of the sigmoid and the observed trade is at the observed premium rate
*        [note that the inverse of the sigmoid function is the logit(x) = log(x) - log(1-x)]
*
         p_sigmoid_calib(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")
                 =
                 { p_trqBilat(R,R1,XX,"trqnt","cur") * [log(v_trq_multiplier.L(R,R1,XX)) - log(1 - v_trq_multiplier.L(R,R1,XX))] / sigmoid_slope }
                 + p_trqBilat(R,R1,XX,"trqnt","cur") - p_tradeFlows(R,R1,XX,"CUR");


*
*    --- fix those tariffs without TRQ
*
v_tariff.FX(R,R1,XX) $ (not p_trqBilat(R,R1,XX,"trqnt","cur")) = p_tarAdVal(R,R1,XX);


*   ---  calibration test for the model with TRQ instruments
solve m_GlobalMarket_trq using mcp;
if(m_GlobalMarket_trq.numinfes ne 0, abort "calibration test with TRQs failed -- infesasibilities");
if(m_GlobalMarket_trq.numredef ne 0, abort "calibration test with TRQs failed -- redefs");

* store the result of the test run in the p_results parameter
$batinclude 'include\base\save_results.gms' '"CAL_sigm"' 'v_tariff.L'

$include 'include\base\test_calibration.gms'


$batinclude  'include\base\money_metric.gms' 'CAL_SIGM'

p_trq_fillrate(R,R1,XX,"CAL_sigm") $ p_trqBilat(R,R1,XX,"trqnt","cur")
               =   v_tradeFlows.L(R,R1,XX) / p_trqBilat(R,R1,XX,"trqnt","cur");



* SCENARIO UNDER TRQ regime
* =========================

* let's repeat the FTA scneario, but now the basline assumes TRQ regimes!
* first solve it with sigmoid representation...
* ... then we move to the orthogonal constraints representation
* ------------


 p_doubleZero("R1","R2",XX,"CUR")  = 1;
 p_doubleZero("R2","R1",XX,"CUR")  = 1;

* MCP formulation
solve m_GlobalMarket_trq using mcp;
if(m_GlobalMarket_trq.numinfes ne 0, abort "simulation run with TRQs failed -- infesasibilities");
if(m_GlobalMarket_trq.numredef ne 0, abort "simulation run with TRQs failed -- redefs");

* save scenario results on "sim_sigm"
$batinclude 'include\base\save_results.gms' '"sim_sigm"' 'v_tariff.L'

$batinclude  'include\base\money_metric.gms' 'SIM_SIGM'
*
*   --- reporting
*
$batinclude 'include\trq\report_trade_diversion.gms' 'sim_sigm'




$label orthogonal

*
*    ---  TRQs introduced with the orthogonality conditions
*
* no FTAs in baseline (revert scenario changes)
 p_doubleZero("R1","R2",XX,"CUR")  = 0;
 p_doubleZero("R2","R1",XX,"CUR")  = 0;


* === initialize the tariff variable (at baseline values)
v_tariff.L(R,R1,XX) = p_tarAdVal(R,R1,XX);



* === calibration of the TRQ instruments ===

*
* --- the fill rate must be 100% so the premium rate can be chosen arbitrarily
*     these settings are identical to the ones above in case of the sigmoid representation
*
p_trqBilat('R1','R3',XX,"trqnt","cur") =   p_tradeFlows('R1','R3',XX,"Cur") * 1.;
p_trqBilat(R,R1,XX,"taPref","cur") $ p_trqBilat(R,R1,XX,"trqnt","cur") =  v_tariff.L(R,R1,XX) * p_prefrate_init(R,R1,XX);
p_trqBilat(R,R1,XX,"taMFN","cur")  $ p_trqBilat(R,R1,XX,"trqnt","cur") =  v_tariff.L(R,R1,XX) * p_MFNrate_init(R,R1,XX);



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


* fix tariffs without TRQ
v_tariff.FX(R,R1,XX) $ (not p_trqBilat(R,R1,XX,"trqnt","cur")) = p_tarAdVal(R,R1,XX);



*
*    --- test run for the orth. cond. representation
*
solve m_GlobalMarket_orth using mcp;
if(m_GlobalMarket_orth.numinfes ne 0, abort "calibration test with orthogonal constraints failed -- infesasibilities");
if(m_GlobalMarket_orth.numredef ne 0, abort "calibration test with orthogonal constraints failed -- redefs");

* store the result of the test run on 'CAL'
$batinclude 'include\base\save_results.gms' '"CAL_orth"' 'v_tariff.L'

$include 'include\base\test_calibration.gms'

$batinclude  'include\base\money_metric.gms' 'CAL_ORTH'

p_trq_fillrate(R,R1,XX,"CAL_orth") $ p_trqBilat(R,R1,XX,"trqnt","cur")
               =   v_tradeFlows.L(R,R1,XX) / p_trqBilat(R,R1,XX,"trqnt","cur");


* ------------
* let's repeat the FTA scneario but now under the TRQ regime!
* ------------


 p_doubleZero("R1","R2",XX,"CUR")  = 1;
 p_doubleZero("R2","R1",XX,"CUR")  = 1;

* MCP formulation
solve m_GlobalMarket_orth using mcp;
if(m_GlobalMarket_orth.numinfes ne 0, abort "simulatio run with orthogonal constraints failed -- infesasibilities");
if(m_GlobalMarket_orth.numredef ne 0, abort "simulation run with orthogonal constraints failed -- redefs");

* save scenario results on "sim_orth"
$batinclude 'include\base\save_results.gms' '"sim_orth"' 'v_tariff.L'

$batinclude  'include\base\money_metric.gms' 'SIM_ORTH'

*
*  -- reporting
*
$batinclude 'include\trq\report_trade_diversion.gms' 'sim_orth'



* SAVE ALL RESULTS IN A GDX container
* ====================================

execute_unload 'results\results_onec.gdx';