*<%REGION File header%>
*=============================================================================
* File      : trq_sigmoid.gms
* Author    : mihalyh
* Version   : 1.0
* Date      : 24.07.2013 10:26:04
* Changed   : 24.07.2013 10:34:38
* Changed by: mihalyh
* Remarks   :
$ontext

$offtext
*=============================================================================
*<%/REGION File header%>

equation
        trq_sigmoid_(R,R,XX)     "calculates endogenous multiplier (0,1) ==> endogenous applied tariff rate under TRQ"
        applied_tariff_(R,R,XX)  "applied tariff rates (multiplier x difference between MFN and Pref rates)"
;

variable
        v_trq_multiplier(R,R,XX) "endogenous multiplier (0,1) ==> endogenous applied tariff rate under TRQ"
;



scalar     sigmoid_slope "slope term of the sigmoid function (to make it steep enough)";
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

*
*   --- extended model: trq regime included with sigmoid representation
*

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




*============================   End Of File   ================================