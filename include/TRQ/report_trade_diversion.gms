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


p_trade_diversion(XX,"%1") = sum[ (R1,R2) $ (fta_countries(R1) $ third_countries(R2)),
                                         p_tradeFlows(R1,R2,XX,"CUR") - v_tradeFlows.L(R1,R2,XX) ];

p_trade_diversion_relative(XX,"%1")
                      $ sum[ (R1,R2) $ (fta_countries(R1) $ third_countries(R2)),
                                         p_tradeFlows(R1,R2,XX,"CUR") ]
                         =
                                      p_trade_diversion(XX,"%1") /
                                       sum[ (R1,R2) $ (fta_countries(R1) $ third_countries(R2)),
                                         p_tradeFlows(R1,R2,XX,"CUR") ];



p_trade_diversion_bilat(R1,R2,XX,"%1") $ (fta_countries(R1) $ third_countries(R2))
   = p_tradeFlows(R1,R2,XX,"CUR") - v_tradeFlows.L(R1,R2,XX);


*
*  ---   trade createion = change (increase) of import shares (Arm2) in the total demand (Arm1)
*
p_trade_creation(R,XX,"%1") $ [ fta_countries(R)
                              $ p_results(R,"","arm1",XX,"cal") $ p_results(R,"","arm2",XX,"cal")]

       = [p_results(R,"","arm2",XX,"%1")  /  p_results(R,"","arm1",XX,"%1")]
                              / [p_results(R,"","arm2",XX,"cal")  /  p_results(R,"","arm1",XX,"cal")];
*
*    --- TRQ fill rates if TRQ instruments are explicitely modelled
*
$iftheni not %1 == sim_ave

p_trq_fillrate(R,R1,XX,"%1") $ p_trqBilat(R,R1,XX,"trqnt","cur")
               =   v_tradeFlows.L(R,R1,XX) / p_trqBilat(R,R1,XX,"trqnt","cur");

$endif

