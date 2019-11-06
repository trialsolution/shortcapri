$title shortcapri

* Short, demonstrative version of the CAPRI market model with ONE COMMODITY
*==========================================================================


* Didactic model prepared for the CAPRI training session 2019, Seville, Spain
* author: Mihaly Himics


* GENERAL REMARKS
* ---------------
*
* 1 products, 3 regions
* supply functions derived from Normalized Quadratic profit functions
* Generalized Leontief demand system
* Two stage Armington (CES formulation)
* No distinction between RMS and RM regions, as in the CAPRI market model. Here they are all R regions
* No processing industry, no feed, no biofuels... => demand is only human consumption
* Tariffs are exogenous, only ad-valorem (see p_tarAdVal)


* Two equivalent formulations for the market model:
*   1) NLP Non-linear maximization of a dummy objective
*   2) MCP (Mixed Complementarity Problem) with orthogonality constraints

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




*=============================================
* introduce endogenous tariffs under TRQ
*  With sigmoid function-representation
*  A sigmoid function mimics the regime switch from in-quota to out-of-quota
*
* The associated market model:
*  m_GlobalMarket_trq
*=============================================



* --- Debugging parameters
parameters
          p_checkPrices(R,XX,*,*)      "differences from actual price levels and the one in the data cube"
          p_checkBalances(R,XX,*,*)    "differences from actual price levels and the one in the data cube"
          p_checkArmington(R,*,XX,*)   "checks the correct initialization of the three Armington equations"
          p_checkProdNQ(R,XX1)         "checks the calibration of the production functions (derived from NQ profit func.)"
          p_checkDemand(R,*,*)         "checks the calibration of the GL expenditure system"
;


*  Possible policy scenario: Free Trade Agreement
*  Full elimination of tariffs

sets
    fta_countries(R)   "countries negotiating an FTA"                /R1, R2/
    third_countries(R) "third countries with respect to the FTA"
;

third_countries(R) $ (not fta_countries(R)) = yes;


parameter  p_store(R,*,*,*) "stores the initial point of the demand system";


*
*  Elasticity trimming: make demand/supply system consistent with micro theory
*  ---------------------------------------------------------------------------


* --- definition of the GL trimming model
$include 'include\onec\calibrate_GL_demand_model.gms'

* --- definition of the NQ trimming model
$include  'include\base\calibrate_NQ_supply_model.gms'


parameter
         p_elasSup(R,XX1,YY1)       "supply elasticities"
         p_elasSup_check(R,XX1,YY1) "debugging parameter for supply elasticites"
;

* DATA INPUT
*===========
$include 'include\base\data_prep.gms'



* MARKET BALANCING (consolidation, i.e. creating a consistent data set at the calibration point)
* Consistent here means that the data are consistent with the equations
*  => the equations can be calibrated against the data
* =============================================================================================
$include 'include\onec\data_cal_one.gms'


* Prepare for simulation after parameters are calibrated and data are consolidated
*  starting values for model variables
*-----------------------------------
$include 'include\base\prep_market.gms'



* CALIBRATION OF ARMINGTON PLUS SHIFT OF SUPPLY FUNCTIONS (WITH TESTS)
* ====================================================================

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
*m_GlobalMarket.iterlim=1;
*m_GlobalMarket.solprint = 1;

solve m_GlobalMarket using mcp;
if(m_GlobalMarket.numinfes ne 0, abort "calibration test failed -- infesasibilities");
if(m_GlobalMarket.numredef ne 0, abort "calibration test failed -- redefs");

*abort "check listing file for infeasibilities";

* alternative NLP solve
*solve m_GlobalMarket_nlp using nlp minimizing v_flipflop;


* store the result of the test run on 'CAL'
$batinclude 'include\base\save_results.gms' '"CAL"'  'p_tarAdval'

* check if the test run replicates the initial point
$include 'include\base\test_calibration.gms'

* money metric (welfare) calculations
$batinclude  'include\base\money_metric.gms' 'CAL'



*  skip simulation with ontext-offtext

$ontext


* SIMULATION engine starts here
* =============================


*## SCENARIO (FTA between R1 and R2, implemented here simply as a double zero agreement)

 p_doubleZero("R1","R2",XX,"CUR")  = 1;
 p_doubleZero("R2","R1",XX,"CUR")  = 1;



* MCP formulation
solve m_GlobalMarket using mcp;
if(m_GlobalMarket.numinfes ne 0, abort "scenario run failed -- infesasibilities");
if(m_GlobalMarket.numredef ne 0, abort "scenario run failed -- redefs");


*
*   --- reporting
*


* store scenario results under "sim_AVE"
$batinclude  'include\base\save_results.gms' '"SIM_AVE"' 'p_tarAdval';

$batinclude  'include\base\money_metric.gms' 'SIM_AVE'
$offtext




* SAVE ALL RESULTS IN A GDX container
* ====================================

execute_unload 'results\results_onec.gdx';
