********************************************************************************
$ontext

   CAPRI project

   GAMS file : DATA_PREP.GMS

   @purpose  : input data for the market model
   @author   :
   @date     : 23.07.13
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy :

$offtext
********************************************************************************


* INPUT DATA
*===========



* some random noise is added to create differences in prices/balances in the calibration point
*-------------------------------------------------------------------------------
option seed=1234;


* Armington substitution elasticities
*-----------------------------------
p_rhoArm2(R,XX) = 5;
p_rhoArm1(R,XX) = 2;



* Policy settings
*-------------------

* --- tariffs
   p_tarAdVal(R,R1,XX) $ (not (sameas(r,r1))) = 25;
* No FTAs by defaults
   p_doubleZero(R,R1,XX,"CUR")  = 0;




* data cube entries
*------------------

* --- some general rules: quantities are in tons, inhabitants are in head,


   DATA(R,"INHA","Levl","cur")=100;

* income, i.e. observed expenditure (EUR/year)
* includes also the expenditures for non-agricultural goods
*---------------------------------------------

   DATA(R,"Ince","Levl","cur")= uniform(80000,100000)*DATA(R,"INHA","Levl","cur");


* price settings
*---------------

* INPE is the numeraire
* the price of 1kg equals unity (here the prices are for tons)
* note that consumer prices of the agricultural goods will be calculated from the import prices
   DATA(R,"PPRI","Inpe","cur") = 1000;
   DATA(R,"Cpri","Inpe","cur") = 1000;

* some random noise in market prices of agricultural goods (set lower than the numeraire)
* necessary to better initialize the Armington system (price difference between domestic and imported goods)
   DATA(R,"PMRK",XX,"CUR") = 1000 * uniform(80,90) / 100;

* producer prices are lower than market prices with a minimum margin of 10%
*   DATA(R,"PPRI",XX,"CUR") = min(DATA(R,"PMRK",XX,"CUR")*.9, 900 * uniform(90,100) / 100);
   DATA(R,"PPRI",XX,"CUR") = 800 * uniform(80,90) / 100;

* the producer price margin will be defined according to the above random sample (pv_prodPriceMarg)



* production and trade flows (random initialization)
* the trade flows are one magnitude less
*---------------------------------------------------

   DATA(R,"PROD",XX,"CUR") = uniform(500,1000);
   p_tradeFlows(R,R1,XX,"Cur") $ (not sameas(r,r1)) = uniform(50,100);

