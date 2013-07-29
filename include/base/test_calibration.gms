********************************************************************************
$ontext

   CAPRI project

   GAMS file : TEST_CALIBRATION.GMS

   @purpose  : check if the test run after a calibration replicates the baseline values
   @author   :
   @date     : 19.07.13
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy :

$offtext
********************************************************************************

* check if the test run gave back the calibration values


      p_checkBalances(R,XX,"diff_to_data","HCON") = v_consQuant.L(R,XX) - data(R,"hcon",XX,"cur") ;
      p_checkBalances(R,XX,"diff_to_data","PROD") = v_prodQuant.L(R,XX) - data(R,"prod",XX,"cur") ;
      p_checkBalances(R,XX,"diff_to_data","Exports") = v_expQuant.L(R,XX) - data(R,"Exports",XX,"cur");


       p_checkPrices(R,XX,"diff_to_data","CPRI") = v_consPrice.L(R,XX) - data(R,"CPRI",XX,"cur");
       p_checkPrices(R,XX,"diff_to_data","PPRI") = v_prodPrice.L(R,XX) - data(R,"PPRI",XX,"cur");
       p_checkPrices(R,XX,"diff_to_data","PMRK") = v_marketPrice.L(R,XX) - data(R,"PMRK",XX,"cur");
       p_checkPrices(R,XX,"diff_to_data","ARM1P") = v_arm1Price.L(R,XX) - data(R,"arm1P",XX,"cur");
       p_checkPrices(R,XX,"diff_to_data","ARM2P") = v_arm2Price.L(R,XX) - data(R,"arm2P",XX,"cur");


if ( [sum( (R,XX), abs(p_checkBalances(R,XX,"diff_to_data","HCON")) + abs(p_checkBalances(R,XX,"diff_to_data","PROD"))
                 + abs(p_checkBalances(R,XX,"diff_to_data","Exports"))) gt .5],

               abort "balances are not correctly calibrated", p_checkBalances;
);


if ( [sum( (R,XX),        abs(p_checkPrices(R,XX,"diff_to_data","CPRI"))
                     +   abs(p_checkPrices(R,XX,"diff_to_data","PPRI"))
                     +   abs(p_checkPrices(R,XX,"diff_to_data","PMRK"))
                     +   abs(p_checkPrices(R,XX,"diff_to_data","ARM1P"))
                     +   abs(p_checkPrices(R,XX,"diff_to_data","ARM2P"))) gt .5],

               abort "prices are not correctly calibrated", p_checkPrices;
);

