*<%REGION File header%>
*=============================================================================
* File      : trq_common.gms
* Author    : mihalyh
* Version   : 1.0
* Date      : 24.07.2013 10:32:53
* Changed   : 24.07.2013 10:33:10
* Changed by: mihalyh
* Remarks   :
$ontext

$offtext
*=============================================================================
*<%/REGION File header%>

parameter
        p_trqBilat(R,R,XX,*,*)   "bilateral TRQ regimes"
        p_trq_fillrate(R,R,XX,*) "fill rate of the TRQs"
        p_premium_rate(R,R,XX)   "quota premium rate"
        p_prefrate_init(R,R1,XX) "multiplicative parameter to set the Pref rate of the TRQ around the applied rate in the calibration point"
        p_MFNrate_init(R,R,XX)   "multiplicative parameter to set the MFN rate of the TRQ around the applied rate in the calibration point"

;

variable
        v_tariff(R,R,XX)  "applied tariff rate (ad valorem)"

;

equation
        impPrice_trq_(R,R,XX)   "import price with endogenous applied tariff rates"
;

impPrice_trq_(R,R1,XX) $ (p_tradeFlows(R,R1,XX,"CUR") and (not SAMEAS(R,R1))) ..

     v_impPrice(R,R1,XX)/(p_impPrice(R,R1,XX,"CUR")+1)   =E=
*

       (v_marketPrice(R1,XX))
*       --- add valorem tariff
          *  [ 1. + 0.01 * v_tariff(R,R1,XX) $ ( (NOT p_doubleZero(R,R1,XX,"CUR")) $ (NOT SAMEAS(R,R1)))]
         /(p_impPrice(R,R1,XX,"CUR")+1);

*============================   End Of File   ================================