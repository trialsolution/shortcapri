********************************************************************************
$ontext

   CAPRI project

   GAMS file : REPORT_TRADE_DIVERSION.GMS

   @purpose  : report trade diversion impacts after each scenario solve
   @author   :
   @date     : 19.07.13
   @since    :
   @refDoc   :
   @seeAlso  :
   @calledBy :

$offtext
********************************************************************************




p_trade_diversion(R,XX,"%1") $ fta_countries(R)
   =
*    --- decrease in 3rd country imports (prevent negatives)
          min[ max(0,sum(R1 $ third_countries(R1), p_tradeFlows(R,R1,XX,"CUR") - v_tradeFlows.L(R,R1,XX)))
*   ---  increase in imports from FTA partners (prevent negatives)
              ,max(0, sum(R2 $ [fta_countries(R2) $ (not sameas(R,R2))], v_tradeFlows.L(R,R2,XX) - p_tradeFlows(R,R2,XX,"CUR")))
              ];


*
*    --- trade diversion relative to the Arm1
*
p_trade_diversion_relative(R,XX,"%1")
                      $ fta_countries(R)
                         =
                                      p_trade_diversion(R,XX,"%1") /
                                       p_results(R,"","ARM1",XX,"%1");


*
*    --- trade creation = replace part of DomSales with imports from FTA partners
*
p_trade_creation(R,XX,"%1") $ fta_countries(R)
                 =
                   min[
*   --- Increase in imports from partners
                  max(0, sum(R1 $ [fta_countries(R1) $ (not sameas(R,R1))], v_tradeFlows.L(R,R1,XX) - p_tradeFlows(R,R1,XX,"CUR")))
*   --- Decrease in DomSales
                  ,max(0, p_results(R,"","DSales",XX,"CAL") - p_results(R,"","DSales",XX,"%1"))
                      ];


*
*  ---   trade createion = change (increase) of import shares (Arm2) in the total demand (Arm1)
*
p_trade_creation_relative(R,XX,"%1") $  fta_countries(R)

       =                    p_trade_creation(R,XX,"%1")
                              / p_results(R,"","arm1",XX,"%1");
*
*    --- TRQ fill rates if TRQ instruments are explicitely modelled
*
$iftheni not %1 == sim_ave

p_trq_fillrate(R,R1,XX,"%1") $ p_trqBilat(R,R1,XX,"trqnt","cur")
               =   v_tradeFlows.L(R,R1,XX) / p_trqBilat(R,R1,XX,"trqnt","cur");

$endif