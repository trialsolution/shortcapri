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


$offlisting

* DECLARATIONS, MODEL DEFINITION
* ==============================
$include 'market_model.gms'


* DATA INPUT
*===========
$include 'data_prep.gms'


* MARKET BALANCING (consolidation, i.e. creating a consistent data set at the calibration point)
* =============================================================================================
$include 'data_cal.gms'


* starting values for model variables
*-----------------------------------
$include 'prep_market.gms'

* CALIBRATION OF ARMINGTON PLUS SHIFT OF SUPPLY FUNCTIONS (WITH TESTS)
* =======================
parameter p_checkPrices, p_checkBalances;
$include 'calibration.gms'



* RUN calibration test with the full system
*=========================================



* TEST run, (see if solving the model with the initial points gives back the calibration point)
* ---------
solve m_GlobalMarket using mcp;
*solve m_GlobalMarket_nlp using nlp minimizing v_flipflop;


* store the result of the test run on 'CAL'
$batinclude 'save_results.gms' '"CAL"'  'p_tarAdval'

$include 'test_calibration.gms'



* SIMULATION engine starts here
* ========================


*## SCENARIO (FTA between R1 and R2, implemented here simply as a double zero agreement)

 p_doubleZero("R1","R2",XX,"CUR")  = 1;
 p_doubleZero("R2","R1",XX,"CUR")  = 1;

set fta_countries(R) /R1, R2/;
set third_countries(R);
third_countries(R) $ (not fta_countries(R)) = yes;


solve m_GlobalMarket using mcp;

* save scenario results on "sim_AVE"
$batinclude 'save_results.gms' '"SIM"' 'p_tarAdval'



* SAVE ALL RESULTS IN A GDX container
* ====================================

execute_unload 'results.gdx';
