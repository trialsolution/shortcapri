*<%REGION File header%>
*=============================================================================
* File      : trq_orthogonal.gms
* Author    : mihalyh
* Version   : 1.0
* Date      : 24.07.2013 10:28:37
* Changed   : 24.07.2013 12:34:00
* Changed by: mihalyh
* Remarks   :
$ontext

$offtext
*=============================================================================
*<%/REGION File header%>

* ----------------
* introduce endogenous tariff under TRQ with the orthogonality conditions approach
* ==================

variables
         v_import_in(R,R,XX)           "in quota imports"
         v_import_out(R,R,XX)          "out of quota imports"
         v_quota_premium_rate(R,R,XX)  "quota premium rate"
;

equations
         import_identity_(R,R,XX)     "imports equals inquota plus out of quota"
         regime_underfill_(R,R,XX)    "upper bound on in quota trade"
         bound_premium_rate_(R,R,XX)  "upper bound on premium rate"
         applied_tariff_orth_(R,R,XX) "premium rate"
;



import_identity_(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")..
                 v_tradeFlows(R,R1,XX) =g=  v_import_in(R,R1,XX) + v_import_out(R,R1,XX);

regime_underfill_(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")..
           p_trqBilat(R,R1,XX,"trqnt","cur") - v_import_in(R,R1,XX) =g= 0;

bound_premium_rate_(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")..
           p_trqBilat(R,R1,XX,"taMFN","cur") - p_trqBilat(R,R1,XX,"taPref","cur") =g= v_quota_premium_rate(R,R1,XX) ;


applied_tariff_orth_(R,R1,XX) $ p_trqBilat(R,R1,XX,"trqnt","cur")..
            v_tariff(R,R1,XX)   =e=   p_trqBilat(R,R1,XX,"taPref","cur") + v_quota_premium_rate(R,R1,XX) ;



* modified model under        trq regime --- orthogonality conditions

model m_GlobalMarket_orth /
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
          applied_tariff_orth_.v_tariff,
          import_identity_.v_import_in,
          regime_underfill_.v_quota_premium_rate,
          bound_premium_rate_.v_import_out
/;


    m_GlobalMarket_orth.solprint = 0; 

*============================   End Of File   ================================