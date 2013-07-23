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
p_rhoArm2(R,XX) = 10;
p_rhoArm1(R,XX) = 8;



* Policy settings
*-------------------

* --- tariffs
   p_tarAdVal(R,R1,XX) $ (not (sameas(r,r1))) = 10;
* No FTAs by defaults
   p_doubleZero(R,R1,XX,"CUR")  = 0;




* data cube entries
*------------------

   DATA(R,"INHA","Levl","cur")= uniform(800,1000);

* income, i.e. observed expenditure (EUR/year)
* includes also the expenditures for non-agricultural goods
*---------------------------------------------

   DATA(R,"Ince","Levl","cur")= uniform(8000,10000)*DATA(R,"INHA","Levl","cur");



   DATA(R,"PMRK",XX,"CUR") = 1000 * uniform(90,100) / 100;
   DATA(R,"PPRI",XX,"CUR") = 900 * uniform(90,100) / 100;
   DATA(R,"PPRI","Inpe","cur") = 1000;

* the producer price margin will be defined according to the above random sample (pv_prodPriceMarg)



* production and trade flows (random initialization)
* the trade flows are one magnitude less
*---------------------------------------------------

   DATA(R,"PROD",XX,"CUR") = uniform(500,1000);
   p_tradeFlows(R,R1,XX,"Cur") $ (not sameas(r,r1)) = uniform(50,100);
