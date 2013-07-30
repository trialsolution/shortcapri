*<%REGION File header%>
*=============================================================================
* File      : money_metric.gms
* Author    : mihalyh
* Version   : 1.0
* Date      : 30.07.2013 12:27:36
* Changed   : 30.07.2013 16:46:31
* Changed by: mihalyh
* Remarks   :
$ontext
                calculate welfare based on a money metric
$offtext
*=============================================================================
*<%/REGION File header%>

*
*    --- consumer's income to reach the utility level at ref. prices
*
     p_welfareRes(R,"CSSP",XX1,"%1") = 0;

*
*    --- set prices in the ref. state
*
 PS_CAL(R,XX1)  = p_results(R,"","CPRI",XX1,"CAL");
 PS_Y(R,XX1)    = PS_CAL(R,XX1);

*
*    --- initialize current price (will be updated one at a time in the loop)
*

   LOOP ( XX1,




*
*      --- set price in current market to price in simulation
*          ( all markets "after" XX are still at reference prices)
*
       PS_Y(R,XX1)   = p_results(R,"","Cpri",XX1,"%1");
*
*  --- calculate FS for prices at calibration point (used below)
*
      v_GLDemandFS.l(R) = SUM(ZZ1 $ p_results(R,"","pdGL",XX1,"%1"), p_results(R,"","pdGL",XX1,"%1") * PS_CAL(R,ZZ1))/1000.;
*
*      --- equivalent variation in evaluation step XX1 in mio euro:
*           increment in utility due to the changed consumption of XX1
*
       p_welfareRes(R,"CSSP",XX1,"%1") $ p_results(R,XX1,"pbGL",XX1,"%1")
*
*        --- value of function G = SUMij Bij Pi-0.5 Pj-0.5
*
          =    SUM( (YY1,ZZ1) $ p_results(R,YY1,"pbGL",ZZ1,"%1"),
                     p_results(R,YY1,"pbGL",ZZ1,"%1")
                          * SQRT(PS_CAL(R,YY1) *  PS_CAL(R,ZZ1) * 1.E-6))
             / SUM( (YY1,ZZ1) $ p_pbGL(R,YY1,ZZ1,"CUR"),
                     p_results(R,YY1,"pbGL",ZZ1,"%1")
                          * SQRT(PS_Y(R,YY1) * PS_Y(R,ZZ1) * 1.E-6))
*
              *  (p_results(R,"","Ince","LEVL","%1")/p_results(R,"","INHA","LEVL","%1")
                    - SUM(ZZ1 $ p_results(R,"","pdGL",XX1,"%1"), p_results(R,"","pdGL",XX1,"%1") * PS_Y(R,ZZ1))/1000.)
*
               -(p_results(R,"","Ince","LEVL","%1")/p_results(R,"","INHA","LEVL","%1") -  v_GLDemandFS.l(R));

*
*      --- replace base year price in market just evaluated by price in simulation
*
       PS_CAL(R,XX1) = PS_Y(R,XX1);
*
   );



*
*   --- from per capita to per total population
*       (change in expenditure calculated above plus expenditure in calibration point)
*
   p_welfareRes(R,"CSSP",XX1,"%1") =  p_results(R,"","Inha","LEVL","CAL")/1000. * p_welfareRes(R,"CSSP",XX1,"%1")
                                          + p_results(R,"","Hcon",XX1,"CAL") * p_results(R,"","Cpri",XX1,"CAL") * 0.001;
*

     option kill=PS_Y;
     option kill=PS_CAL;


*
*  ------ profit of agricultural industry ----------------------------------------------------------------------------------
*
*

   p_welfareRes(R,"ProfitAgr",XX1,"%1") = 0;

   PS_CAL(R,XX1)   = P_results(R,"","PPri",XX1,"CAL")/p_results(R,"","PPri","Inpe","CAL");
   PS_Y(R,XX1)     = PS_CAL(R,XX1);
*
*
   LOOP ( XX,
*
*      --- set price in current market to price in simulation
*          ( all markets "after" XX are still at reference prices)
*
       PS_Y(R,XX)   = p_results(R,"","PPRI",XX,"%1")/p_results(R,"","PPri","Inpe","CAL");
*
       p_welfareRes(R,"ProfitAgr",XX,"%1")
           =        SUM(  ZZ1 $ p_cnstNQSupp(R,ZZ1),
                           p_cnstNQSupp(R,ZZ1) * (PS_Y(R,ZZ1)  - PS_CAL(R,ZZ1))) * 0.001

            + 0.5 * SUM( (ZZ1,YY1) $ [ p_hessNQSupp(R,ZZ1,YY1,"CUR") $ (not sameas(ZZ1,"INPE")) $ (not sameas(YY1,"INPE")) ],
                              p_hessNQSupp(R,ZZ1,YY1,"CUR")
                            * ( PS_Y  (R,ZZ1)*PS_Y  (R,YY1)
                               -PS_CAL(R,ZZ1)*PS_CAL(R,YY1))) * 0.001;

display ps_cal, ps_y, p_welfareRes;

       PS_CAL(R,XX) = PS_Y(R,XX);
   );
*
*  ---- undo normalisation and add 50 % of revenues as profit
*
   p_welfareRes(R,"ProfitAgr",XX,"%1")
                                  = p_welfareRes(R,"ProfitAgr",XX,"%1") * p_results(R,"","PPri","Inpe","CAL")
                                  + p_results(R,"","Prod",XX,"CAL")*p_results(R,"","PPri",XX,"CAL") * 0.5 * 0.001 $ p_hessNQSupp(R,XX,XX,"CUR");


          option kill=PS_Y;
          option kill=PS_CAL;


*
*    ---  TRQ rent calculation
*
          p_welfareRes(R,"TrqRent_exporters",XX1,"%1") = 0;
          p_welfareRes(R1,"TrqRent_government",XX1,"%1") = 0;

$iftheni.outer not "%1" == "SIM_AVE"
$iftheni.inner not "%1" == "CAL"

   p_welfareRes(R,"TrqRent_exporters",XX,"%1") $ sum(R1, p_trqBilat(R1,R,XX,"trqnt","cur"))
                =  sum{R1 $ p_trqBilat(R1,R,XX,"trqnt","cur"),
                        [p_results(R1,R,"tariff",XX,"%1") -  p_trqBilat(R1,R,XX,"TaPref","cur")]/100
                         * (p_results(R,"","PMRK",XX,"%1") + p_tc(R1,R,XX,"cur")) / 1000
                         * min(p_trqBilat(R1,R,XX,"trqnt","cur"), p_results(R1,R,"trade",XX,"%1"))
                         *0.5};

   p_welfareRes(R1,"TrqRent_government",XX,"%1") $ sum(R, p_trqBilat(R1,R,XX,"trqnt","cur"))
                = sum(R $ p_trqBilat(R1,R,XX,"trqnt","cur"),  p_welfareRes(R,"TrqRent_exporters",XX,"%1")) ;

$endif.inner
$endif.outer





*
*    ---  Tariff revenues
*
        p_welfareRes(R,"Tariff_revenues",XX1,"%1") = 0;

*   --- In the basic model tariffs are parameters


$iftheni %1 == SIM_AVE

         p_welfareRes(R,"Tariff_revenues",XX,"%1")
                 = sum(R1 $ (not p_doubleZero(R,R1,XX,"cur")), p_tarAdval(R,R1,XX)/100
                                                    * (p_results(R1,"","PMRK",XX,"%1") + p_tc(R,R1,XX,"cur")) /1000
                                                    * p_results(R,R1,"trade",XX,"%1"));

$elseifi  %1 == CAL

            p_welfareRes(R,"Tariff_revenues",XX,"%1")
                 = sum(R1 $ (not p_doubleZero(R,R1,XX,"cur")), p_tarAdval(R,R1,XX)/100
                                                    * (p_results(R1,"","PMRK",XX,"%1") + p_tc(R,R1,XX,"cur")) / 1000
                                                    * p_results(R,R1,"trade",XX,"%1"));


$endif




$iftheni.outer not "%1" == "SIM_AVE"
$iftheni.inner not "%1" == "CAL"

         p_welfareRes(R,"Tariff_revenues",XX,"%1")
                 =

*   ---       NOrmal tariffs
                  sum{R1 $ [(not p_doubleZero(R,R1,XX,"cur")) $ (not p_trqBilat(R,R1,XX,"trqnt","cur"))],
                                  p_tarAdval(R,R1,XX)/100
                                  *  (p_results(R1,"","PMRK",XX,"%1") + p_tc(R,R1,XX,"cur")) / 1000
                                  *  p_results(R,R1,"trade",XX,"%1")}

*   ---     plus tariffs under TRQ
             + sum{R1 $ [(not p_doubleZero(R,R1,XX,"cur")) $ p_trqBilat(R,R1,XX,"trqnt","cur")],
                                  min(p_results(R,R1,"trade",XX,"%1"), p_trqBilat(R,R1,XX,"trqnt","cur")) * p_trqBilat(R,R1,XX,"taPref","cur")/100
                                                                       * (p_results(R1,"","PMRK",XX,"%1") + p_tc(R,R1,XX,"cur")) /1000


                                  + max(0,p_results(R,R1,"trade",XX,"%1") - p_trqBilat(R,R1,XX,"trqnt","cur")) * p_trqBilat(R,R1,XX,"taMfN","cur")/100
                                                                      *  (p_results(R1,"","PMRK",XX,"%1") + p_tc(R,R1,XX,"cur")) /1000
                   };

$endif.inner
$endif.outer


          p_welfareRes(R,"Total",XX1,"%1")
          =
             p_welfareRes(R,"Tariff_revenues",XX1,"%1")
             + p_welfareRes(R,"TrqRent_government",XX1,"%1")
             + p_welfareRes(R,"TrqRent_exporters",XX1,"%1")
             + p_welfareRes(R,"ProfitAgr",XX1,"%1")
             + p_welfareRes(R,"CSSP",XX1,"%1");

*============================   End Of File   ================================