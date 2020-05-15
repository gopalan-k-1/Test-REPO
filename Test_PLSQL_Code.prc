CREATE OR REPLACE PROCEDURE          P_CALCULATE_PROMO_COST(lv_Promo_id S_SRC.ROW_ID%type) -- removed lc_apply_filter varchar2 default 'N' by IDC_AC on 29-Oct-08 as the scalabilty is handled at the promotion level.
IS

----------------------------------------------------------------------------------
-- This routine is designed to calculate the variable costs of all activities on -
-- A promotion.  It stores the costs once calculated on the S_MDF_ALLOC record   -
-- based on the PROMO_ID linked to the activity record (row_id).                 -
-- Note: several records might be updated with this routine                      -
-- Note2: This routine will only update costs of pricing activities              -
--        (SRC_CD_FMT_ID in ('Price Featuring', 'Pricing'))                      -
-- 2007/10/25 MAK - updated update command to also update the X_INTERFACED_DT    -
--                  field on S_SRC to store the last time it was calculated.     -
-- 2008/01/10 MAK - New Changes required to roll the promotion costs up to the   -
--                  promotion header level.  Will now also calculate the         -
--                  following:                                                   -
--                  Planned Amount (S_MDF.X_PLANNED_AMT)                         -
--                  Committed Amount (S_MDF.X_COMMITTED_AMT)                     -
--                  Actuals Fixed Cost Booked (S_SRC.ACTL_FIXED_COST)            -
--                  Actual Variable Cost (S_SRC.X_ACTL_VAR_COST)                 -
--                  Est. Net Incremental Net Real (S_SRC.X_R_NOS)        -
--                  Base SU (S_SRC.X_FCST_BASE_VOLUME_SU)                        -
--                  Total Fund Cost (S_MDF_ALLOC.EST_MDF_AMT)                    -
-- 2008/01/18 MAK - New Changes: There will be three cost types (stored at the   -
--                  S_SRC activity record in FIXED_COST_TYPE_CD:                 -
--                  "Fixed" -- We don't do anything with these records           -
--                  "Variable" -- The existing cost calculation is done          -
--                  "% Fund" -- New calculation for cost based on transactions.  -
-- 2008/01/28 MAK - Updated based on the new dated price list requirement.       -
--                  Previously, the list price was pulled from the price stored  -
--                  on the promotion header, but with Optima 6, we will now link -
--                  from the promted product baseline record to the price list   -
--                  and pull the price from the price list item record.          -
-- 2008/01/29 MAK - Updated based on new information - previously, when computing-
--                  the cost of activities, we only considered pricing activities-
--                  but when we compute the actuals, we take into account the    -
--                  payments made against those activities.  This is stored in   -
--                  S_SRC.ACTL_FIXED_COST.  We will go through all activities on -
--                  the promotion, and just set the activity cost to zero if it  -
--                  is not a pricing activity.                                   -
-- 2008/01/30 MAK - The discount rate on the Activity Prod record was previously -
--                  linked to the S_SRC_PRD_BASLN record through the PROD_ID,    -
--                  but with Optima 6, the activity discount rate can be applied -
--                  higher than GTIN level.  In fact, based on the activity      -
--                  level X_PRICING_LEVEL field (on S_SRC), it can be 1/3 levels:-
--                  'Brand', 'Category', or 'GTIN/Promoted Group'.  If the       -
--                  level is 'GTIN/Promoted Group' then the routine will work    -
--                  the same, if not, we need to join to the S_CTLG_CAT_PROD     -
--                  table to get all GTINS.                                      -
-- 2008/02/06 MAK - Discovered the possibility with the "% Fund" cost type code  -
--                  where there might not be any allocation records associated   -
--                  yet - need to check for this to avoid a "no records returned"-
--                  condition.  Also changed the IF logic to calculate % Fund    -
--                  regardless of whether or not it's a pricing activity.        -
-- 2008/02/08 MAK - Slight change to % Fund Cost type - the Percent Forcast was  -
--                  previously only multiplied by the total forecast, not all    -
--                  the fields.  It should be multiplied by everything.          -
-- 2008/02/13 MAK - Discovered one problem with the routine for activity cost:   -
--                  Since you can have multiple categories/brands, we need to    -
--                  Also join the promoted product to the activity product thru: -
--                  ACTIV_PROD.CTLG_CAT_ID = PROMO_PROD.CTLG_CAT_ID.             -
-- 2007/02/14 MAK - Changed the join to S_PRI_LST_ITEM to an outer join because  -
--                  if the discount type is "Amount Per BUOM", then we still     -
--                  need to consider that GTIN even though it might not have a   -
--                  list price.                                                  -
-- 2008/02/18 MAK - Change to the way X_COMMITTED_AMT is calculated; previously, -
--                  it was only the allocated amount, but committed amount       -
--                  actually needs to subtract the booked amount to be accurate, -
--                  so added an outer join to the S_SRC_PAYMENT table to fix.    -
-- 2008/03/19 MAK - Change to X_COMMITTED_AMT again; If the booked amount for any-
--                  particular allocation is greater than the committed amount,  -
--                  we just add zero for committed amt.  Changed the routine to  -
--                  look at each row of S_MDF_ALLOC and calc each total committed-
--                  amt, and discard if negative (using greater() function with 0)
----------------------------------------------------------------------------------

lv_Activity_id        S_SRC.row_id%type;
lv_Fixed_Cost_Type    S_SRC.FIXED_COST_TYPE_CD%type;
lv_Activity_cost      S_MDF_ALLOC.X_FCST_VARIABLE_COST%type;
lv_Actl_Fixed_Cost    S_SRC.ACTL_FIXED_COST%type;
lv_Promo_Split_Cost   S_SRC.ACTL_FIXED_COST%type;
lv_Actl_Var_Cost      S_SRC.X_ACTL_VAR_COST%type;
lv_Fcst_Incr_NOS      S_SRC.X_FCST_INCR_NOS%type;						;
lv_Base_SU            S_SRC.X_FCST_BASE_VOLUME_SU%type;
lv_Booked_Total       S_SRC_PAYMENT.CG_TRADE_FUND_TOT%type;
lv_Split_Total        S_SRC.X_CORP_ACT_PERCENTAGE_SPLIT%type;
--lv_Activity_type      S_SRC.SRC_CD_FMT_ID%type;   --Commented by IDC_SAS : R9 :28-Jul-2009 : QC 336
lv_Activity_type      S_SRC.X_ACTIVITY_TYPE%type;   --Modified by IDC_SAS : R9 :28-Jul-2009 : QC 336 : S_SRC coloumn name changed
lv_Pricing_Level      S_SRC.X_PRICING_LEVEL%type;
lv_NumberOfRecs       Number(22,0);
lv_Incr_GIV           S_MDF_ALLOC.X_FCST_VARIABLE_COST%type;
lv_TT_Percent          S_PROD_BASELINE.X_TT_PERCENT%type;
lv_fcst_incr          S_SRC.X_FCST_INCR_NR%TYPE; --Added by IDC_VH;09-Jul-2008;Rel 7;Calculate the X_FCST_INCR_NR
lv_src_cd             S_SRC.SRC_CD%TYPE;
lv_cost_indicator     S_SRC.X_COST_INDICATOR%TYPE;-- Added by IDC _SS on 25-Jan-10   WET97
lv_forecast_model     S_SRC.X_FORECAST_MODEL%TYPE;
lv_Rev_Act_Cost       S_MDF_ALLOC.X_REVISED_VAR_COST%type; -- Added by IDC_SS on 27th Jan 2011 for TSP RVC calculation
lv_curr_month number :=0;  -- Added by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)
lv_pfy_start date := NULL; -- Added by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)
lv_pfy_end date := NULL;   -- Added by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)
lv_cfy_start date := NULL;   -- Added by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)
lv_cfy_end date := NULL;   -- Added by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)
lv_nfy_start date := NULL;   -- Added by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)
LV_NFY_END date := null;   -- Added by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)
LV_FUND_CLAS VARCHAR2(30) := NULL;--added by IDC_SC as a part of R14.1 for rounding logic of TFI fund
lv_anaplan_id VARCHAR2(60):=NULL;--added by IDC_SC as a part of ANAPLAN
CURSOR cACTIVITY_LIST IS ----Added the below query by IDC_TF to filter Annual Agreement activities.Rel8,16-Feb-2009
--select act.ROW_ID, act.FIXED_COST_TYPE_CD, act.SRC_CD_FMT_ID, act.X_PRICING_LEVEL, promo.SRC_CD from S_SRC act, S_SRC promo  --Commented by IDC_SAS : R9 :28-Jul-2009 : QC 336
SELECT ACT.ROW_ID, ACT.FIXED_COST_TYPE_CD, ACT.X_ACTIVITY_TYPE, ACT.X_PRICING_LEVEL, PROMO.SRC_CD,ACT.X_COST_INDICATOR,PROMO.X_FORECAST_MODEL,
PROMO.x_anaplan_id -- IDC_SC Anaplan
from S_SRC ACT, S_SRC PROMO--Modified by IDC_SAS : R9 :28-Jul-2009 : QC 336 : S_SRC coloumn name changed
where act.PAR_SRC_ID = promo.ROW_ID
and ACT.SUB_TYPE = 'PG_TFMAAI_PROMOTION_ACTIVITY'-- Added lv_cost_indicator by IDC _SS on 25-Jan-10   WET97
--and promo.SRC_CD <> 'Annual Agreement' --Commented by IDC_TF ,Rel8 QC 1627
and promo.ROW_ID = lv_Promo_id;
--Commented the below query by IDC_TF to filter Annual Agreement activities.Rel8,16-Feb-2009
/*select S_SRC.ROW_ID, S_SRC.FIXED_COST_TYPE_CD, S_SRC.SRC_CD_FMT_ID, S_SRC.X_PRICING_LEVEL from S_SRC
where S_SRC.PAR_SRC_ID = lv_Promo_id
and S_SRC.SUB_TYPE = 'PG_TFMAAI_PROMOTION_ACTIVITY'; */
/* Reverted the changes applied for CR131 at the activity level as now the CR131 is handled in the Pkg_Optima_Cost_Calc at the promotion level*/
--Added by IDC_TF 16-sep-2008,Rel7,QC1526 to include the ship start date and ship end date filter
/*select ACT.ROW_ID,
       ACT.FIXED_COST_TYPE_CD,
       ACT.SRC_CD_FMT_ID,
       ACT.X_PRICING_LEVEL
from S_SRC ACT,S_SRC PROMO
where ACT.PAR_SRC_ID = lv_Promo_id
AND ACT.PAR_SRC_ID = PROMO.ROW_ID -- Filter added by Santhosh on 09/16/08
AND ACT.SUB_TYPE = 'PG_TFMAAI_PROMOTION_ACTIVITY'
--AND ACT.SRC_CD_FMT_ID in ('Price Featuring','Pricing') -- Commented by Santhosh on 09/16/08
AND PROMO.SHIP_START_DT<Decode(lc_appy_filter,'Y',PROMO.SHIP_START_DT+1,SYSDATE)
AND PROMO.SHIP_END_DT>Decode(lc_appy_filter,'Y',PROMO.SHIP_END_DT-1,SYSDATE)
UNION                       --- UNION clause added by IDC_AC for defect 2562 on 28-OCT-08
   Select ACT.ROW_ID,
       ACT.FIXED_COST_TYPE_CD,
       ACT.SRC_CD_FMT_ID,
       ACT.X_PRICING_LEVEL
       from S_MDF_ALLOC Alloc, S_SRC_PAYMENT Payment, S_SRC Act
      where
            Payment.CG_MDF_ALLOC_ID = Alloc.ROW_ID
      and   Payment.PAY_STAT_CD = 'Booked'
      and   Payment.X_SAP_DOC_TYPE <> '2A'
      and   Alloc.promo_id = Act.ROW_ID
      and   Trunc(payment.last_upd) between trunc(sysdate)-2 and trunc(sysdate)
      and   act.par_src_id = lv_Promo_id;*/
--Commented by IDC_TF 16-sep-2008,Rel7,QC1526 to include the ship start date and ship end date filter
/*select S_SRC.ROW_ID, S_SRC.FIXED_COST_TYPE_CD, S_SRC.SRC_CD_FMT_ID, S_SRC.X_PRICING_LEVEL from S_SRC
where S_SRC.PAR_SRC_ID = lv_Promo_id
and S_SRC.SUB_TYPE = 'PG_TFMAAI_PROMOTION_ACTIVITY';
--and SRC_CD_FMT_ID in ('Price Featuring','Pricing'); --Removed 1/29/08 MAK*/

CURSOR cPERCENT_SPLIT IS
SELECT NVL(S_SRC.X_CORP_ACT_PERCENTAGE_SPLIT,0), S_SRC.PAR_SRC_ID from S_SRC
where NAME = lv_Promo_id
and SUB_TYPE = 'PG_TFMAAI_PERCENT_SPLIT';

BEGIN



/* Added by IDC_AC on 29-Jun-2009 for CEEMEA 407 to update fixed cost at the fund level*/

Update S_MDF SET  X_AVAIL_FIXED_AMT = --(Select SUM(NVL(ALLOC.X_FCST_FIXED_COST,0))   --Commented by IDC_AS on 19th Oct 2010 for PR 80151
                                                                   ROUND_DISPLAY(round((Select ROUND(SUM(NVL(ALLOC.X_FCST_FIXED_COST,0)),2)  --Modified by IDC_AS on 19th Oct 2010 for PR 80151
                                    from S_MDF_ALLOC alloc, S_SRC promo, S_SRC activity
                                    where alloc.MDF_ID = S_MDF.ROW_ID and alloc.promo_id = activity.row_id
                                      and activity.par_src_id = promo.row_id
                                      and promo.status_cd in ('Confirmed','Planned','Revised') --added revised by IDC_SS on 5-feb-10 for wet97
                                      and (activity.X_STOPPED_DT is null or activity.X_STOPPED_DT > sysdate
                                           or NVL(activity.X_PAY_STOP_ACTIVITY_FLG,'Nothing') = 'Y')
                                      and NVL(promo.X_APPR_STATUS_CD,'Nothing') <> 'Rejected'),3)) ,-- added a check on approver status email sent by santhosh on 15-Oct-2009 by IDC_AC
                    LAST_UPD = SYS_EXTRACT_UTC(SYSTIMESTAMP) -- Updated by IDC_AM for P#2163 to update the last_upd to UTC time
where S_MDF.ROW_ID in
    (Select distinct MDF_ID from S_SRC Activity, S_MDF_ALLOC Alloc
     where Alloc.Promo_id = Activity.ROW_ID
     and   Activity.PAR_SRC_ID = lv_Promo_id);


   /* Added by IDC_AC on 29-Jun-2009 for CEEMEA 407 to update fixed cost at the fund level*/


    Update S_MDF set X_AVAIL_FIXED_AMT =ROUND_DISPLAY(ROUND ( (NVL(X_AVAIL_FIXED_AMT,0) + (select NVL(SUM(NVL(SSP.CG_TRADE_FUND_TOT,0)),0)  --Modified by IDC_AS on 19th Oct 2010 for PR 80151 --added NVL for defect 1972 R9 by IDC_AC
                                                                  from S_SRC_PAYMENT SSP,
                                     S_MDF_ALLOC alloc, S_SRC promo, S_SRC activity, S_LST_OF_VAL val, S_BU bu,-- H_TFM_INTERFACE_ORGANIZATIONS org
                                     S_LST_OF_VAL lov
                                    where alloc.MDF_ID =S_MDF.row_id
                                      and alloc.promo_id = activity.row_id
                                      and SSP.CG_MDF_ALLOC_ID = alloc.row_id
                                      and SSP.PAY_STAT_CD = 'Booked'
                                      and activity.par_src_id = promo.row_id
                                      and SSP.X_SAP_DOC_TYPE = val.val
                                      AND  LOV.WS_MIN_VER <= 9999 AND  LOV.WS_MAX_VER > 9999 AND  LOV.WS_INACTIVE_FLG = 'N' 
                                    AND  LOV.WS_ID = (SELECT ROW_ID FROM S_WORKSPACE WHERE NAME = 'MAIN' and REPOSITORY_ID =(SELECT ROW_ID FROM S_REPOSITORY WHERE NAME = 'Siebel Repository'))--Added by IDC_HB for IP 18 Upgrade 2018
                                      and RTRIM (substr( SSP.X_SIEBEL_ID,1,2)) = lov.val--org.GEOGRAPHY_ID
                                      and lov.name=bu.name                    --added by IDC_KS; 18-Nov-09;
                                      and lov.type='COUNTRY_CODE'             --added by IDC_KS; 18-Nov-09;
                                      AND  VAL.WS_MIN_VER <= 9999 AND  VAL.WS_MAX_VER > 9999 AND  VAL.WS_INACTIVE_FLG = 'N' 
                                        AND  VAL.WS_ID = (SELECT ROW_ID FROM S_WORKSPACE WHERE NAME = 'MAIN' and REPOSITORY_ID =(SELECT ROW_ID FROM S_REPOSITORY WHERE NAME = 'Siebel Repository'))--Added by IDC_HB for IP 18 Upgrade 2018
                                      --and org.ORGANIZATION = bu.name        --commented by IDC_KS; 18-Nov-09;
                                      and BU.ROW_ID = VAL.BU_ID
                                      and val.type='PG_TFMIB_FI_DOC_PAY_MEANS'
                                      and VAL.X_FILTER_VAL not in ('Off Invoice','On Invoice')
                                      and (promo.status_cd = 'Completed'  or
                                      (promo.status_cd in ('Confirmed','Planned','Revised')  and  --added Revised by IDC_SS on 05-feb-1- for WET97
                                        activity.X_STOPPED_DT is not null and activity.X_STOPPED_DT < sysdate
                                           and NVL(activity.X_PAY_STOP_ACTIVITY_FLG,'N') = 'N'))
                                       and NVL(promo.X_APPR_STATUS_CD,'Nothing') <> 'Rejected'   ) ) ,2)) , --Modified by IDC_AS on 19th Oct 2010 for PR 80151 -- added a check on approver status email sent by santhosh on 15-Oct-2009 by IDC_AC
                    LAST_UPD = SYS_EXTRACT_UTC(SYSTIMESTAMP) -- Updated by IDC_AM for P#2163 to update the last_upd to UTC time
    where S_MDF.ROW_ID in
    (Select distinct MDF_ID from S_SRC Activity, S_MDF_ALLOC Alloc
     WHERE ALLOC.PROMO_ID = ACTIVITY.ROW_ID
     and   Activity.PAR_SRC_ID = lv_Promo_id);




    OPEN CACTIVITY_LIST;
    Fetch cACTIVITY_LIST into lv_Activity_id, lv_Fixed_Cost_Type, lv_Activity_type, lv_Pricing_Level, lv_src_cd,lv_cost_indicator,lv_forecast_model,lv_anaplan_id;--IDC_SC Anaplan-- Added lv_cost_indicator by IDC _SS on 25-Jan-10   WET97
    While cACTIVITY_LIST%FOUND
    LOOP
        lv_Activity_Cost := 0;
        lv_Rev_Act_Cost  := 0;
       -- if lv_Activity_Type in ('Price Featuring','Pricing') AND --Commented by IDC_VH;23-June-2008;Rel7;FR54.2;Update variable cost calculation
       -- Variable Cost routine need to support all activities for discount products, provided an activity has a discount product associated to it.


       IF lv_forecast_model LIKE  'TSP%' AND lv_src_cd ='Annual Agreement'  THEN--lv_forecast_model LIKE  'TSP%' Modify by IDC_RJ on 06-Dec-10 for QC 1109


        --   IF lv_Fixed_Cost_Type = 'Variable GIV' THEN    -- Commented by IDC_SS on 07 Feb 2011 used the join in the SQL below while calculating EVC and RVC

             If lv_Pricing_Level <> 'GTIN/Promoted Group' THEN  --Modify by IDC_SG on 10-Dec-10 for QC 1290

             -- Added by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)

             BEGIN

               select to_number(to_char(sysdate,'MM')) into lv_curr_month from dual;

               if lv_curr_month between 1 and 6
               THEN
                   select add_months(trunc(sysdate,'YYYY'),-6) into lv_cfy_start from dual;
                   select last_day(add_months(trunc(sysdate,'YYYY'),5)) into lv_cfy_end from dual;

                   select add_months(add_months(trunc(sysdate,'YYYY'),-6),-12) into lv_pfy_start from dual;
                   select add_months(last_day(add_months(trunc(sysdate,'YYYY'),5)),-12) into lv_pfy_end from dual;

                   select add_months(add_months(trunc(sysdate,'YYYY'),-6),12) into lv_nfy_start from dual;
                   select add_months(last_day(add_months(trunc(sysdate,'YYYY'),5)),12) into lv_nfy_end from dual;
               else
                    select add_months(trunc(sysdate,'YYYY'),6) into lv_cfy_start from dual;
                    select add_months(last_day(add_months(trunc(sysdate,'YYYY'),5)),12) into lv_cfy_end from dual;

                    select add_months(add_months(trunc(sysdate,'YYYY'),6),-12) into lv_pfy_start from dual;
                    select add_months(add_months(last_day(add_months(trunc(sysdate,'YYYY'),5)),12),-12) into lv_pfy_end from dual;

                    select add_months(add_months(trunc(sysdate,'YYYY'),6),12) into lv_nfy_start from dual;
                    select add_months(add_months(last_day(add_months(trunc(sysdate,'YYYY'),5)),12),12) into lv_nfy_end from dual;
               end if;

               EXCEPTION
                WHEN OTHERS THEN
                        Pkg_Interface_Error_Handling.insert_error_record (
                                 'CRITICAL',                                                            -- Severity
                                 'Lookup Failure',                                                      -- Error code
                                 'PKG_OPTIMA_PROMO_COST',                                                   -- Source
                                 'PROCEDURE  P_NIGHTLY_PROMO_COST',                                           -- Unit
                                 'Error Identifying the FY start and end dates',  -- Desc Text
                                  SQLERRM,                                                               -- sql_error
                                  SQLCODE,                                                              -- sql_code
                                  NULL,                                                                 -- ext_ref_id
                                  NULL,                                            -- table_name
                                  NULL,                                                                 -- column_name
                                  NULL,                                                      -- Organisation
                                  NULL,                                          -- Value1
                                  NULL,--,                                                              -- Value2
                                  NULL,                                                                 -- Value3
                                  NULL,                                                                 -- Value4
                                  NULL,                                                                 -- Value5
                                  NULL);

               END;

             -- End of Additions by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)


         /*
                 SELECT SUM ((activ_prod.x_src_act_discount_rate / 100)* basln.giv_forecast)
                   INTO lv_Activity_Cost
                  FROM s_src promo,
                       s_src act,
                       s_src promo_cat,
                       s_src activ_prod,
                       cx_tsp_cat_bsln basln,
                       s_period sp
                 WHERE act.par_src_id = promo.row_id
                   AND promo_cat.par_src_id = promo.row_id
                   AND act.par_src_id = promo_cat.par_src_id
                   AND promo_cat.ctlg_cat_id = basln.ctlg_cat_id
                   AND promo.sub_type = 'PLAN_ACCOUNT_PROMOTION'
                   AND act.sub_type = 'PG_TFMAAI_PROMOTION_ACTIVITY'
                   AND promo_cat.sub_type = 'PLAN_ACCT_PROMOTION_CATEGORY'
                   AND promo.pr_accnt_id = basln.ou_ext_id
                   AND activ_prod.par_src_id = act.row_id
                   AND activ_prod.ctlg_cat_id = basln.ctlg_cat_id
                   AND activ_prod.ctlg_cat_id = promo_cat.ctlg_cat_id         -- Added by IDC_SS for R11 QC 1635 on 29-Dec-2010
                   AND activ_prod.sub_type = 'PG_TFMAAI_ACTIVITY_PRODUCT'
                   AND sp.row_id = promo.period_id
                   AND sp.period_cd = 'Fiscal Year'
                   AND act.row_id = lv_Activity_id
                   AND basln.period_id IN (
                          SELECT row_id
                            FROM s_period
                           WHERE period_cd = 'Month'
                             AND bu_id = '0-R9NH'
                             AND TRUNC(promo.SHIP_START_DT,'MM') <= START_DT
                             AND LAST_DAY(promo.SHIP_END_DT) >= END_DT);

                 -- Added by IDC_SS on 27th Jan 2011 for TSP RVC calculations  used f_ffactualization_date function which will return the actualization date
                 -- if the period end_dt for that month is less then the actualization date we will consider actual Giv  else we will take forecast
                 SELECT SUM(NVL(activ_prod.x_src_act_discount_rate / 100,0) * (CASE WHEN per.end_dt <= F_FF_ACTUALIZATION_DATE  THEN NVL(basln.ACTUAL_GIV,0) ELSE  NVL(basln.GIV_FORECAST,0) END))
                     calc INTO lv_Rev_Act_Cost
                 FROM s_src promo,
                       s_src act,
                       s_src promo_cat,
                       s_src activ_prod,
                       cx_tsp_cat_bsln basln,
                       s_period sp,
                       s_period per
                 WHERE act.par_src_id = promo.row_id
                   AND promo_cat.par_src_id = promo.row_id
                   AND act.par_src_id = promo_cat.par_src_id
                   AND promo_cat.ctlg_cat_id = basln.ctlg_cat_id
                   AND promo.sub_type = 'PLAN_ACCOUNT_PROMOTION'
                   AND act.sub_type = 'PG_TFMAAI_PROMOTION_ACTIVITY'
                   AND promo_cat.sub_type = 'PLAN_ACCT_PROMOTION_CATEGORY'
                   AND promo.pr_accnt_id = basln.ou_ext_id
                   AND activ_prod.par_src_id = act.row_id
                   AND activ_prod.ctlg_cat_id = basln.ctlg_cat_id
                   AND activ_prod.ctlg_cat_id = promo_cat.ctlg_cat_id
                   AND activ_prod.sub_type = 'PG_TFMAAI_ACTIVITY_PRODUCT'
                   AND sp.row_id = promo.period_id
                   AND sp.period_cd = 'Fiscal Year'
                   AND act.row_id = lv_Activity_id
                   and per.row_id = basln.period_id
                   and per.period_cd = 'Month'
                   AND basln.period_id IN (
                          SELECT row_id
                            FROM s_period
                           WHERE period_cd = 'Month'
                             AND bu_id = '0-R9NH'
                             AND TRUNC(promo.SHIP_START_DT,'MM') <= START_DT
                             AND LAST_DAY(promo.SHIP_END_DT) >= END_DT);
               END IF;

           elsif lv_Fixed_Cost_Type IN ('Variable NIV') THEN  -- removed Variable NIV2  by IDC_SS on 27th jan 2011 as per Santhosh mail for annual agreement we will use only variable GIV and variable NIV

                If lv_Pricing_Level <> 'GTIN/Promoted Group' THEN --Modify by IDC_SG on 10-Dec-10 for QC 1290

                 SELECT SUM ((activ_prod.x_src_act_discount_rate / 100)* basln.niv_forecast)
                   INTO lv_Activity_Cost
                  FROM s_src promo,
                       s_src act,
                       s_src promo_cat,
                       s_src activ_prod,
                       cx_tsp_cat_bsln basln,
                       s_period sp
                 WHERE act.par_src_id = promo.row_id
                   AND promo_cat.par_src_id = promo.row_id
                   AND act.par_src_id = promo_cat.par_src_id
                   AND promo_cat.ctlg_cat_id = basln.ctlg_cat_id
                   AND promo.sub_type = 'PLAN_ACCOUNT_PROMOTION'
                   AND act.sub_type = 'PG_TFMAAI_PROMOTION_ACTIVITY'
                   AND promo_cat.sub_type = 'PLAN_ACCT_PROMOTION_CATEGORY'
                   AND promo.pr_accnt_id = basln.ou_ext_id
                   AND activ_prod.par_src_id = act.row_id
                   AND activ_prod.ctlg_cat_id = basln.ctlg_cat_id
                   AND activ_prod.ctlg_cat_id = promo_cat.ctlg_cat_id         -- Added by IDC_SS for R11 QC 1635 on 29-Dec-2010
                   AND activ_prod.sub_type = 'PG_TFMAAI_ACTIVITY_PRODUCT'
                   AND sp.row_id = promo.period_id
                   AND sp.period_cd = 'Fiscal Year'
                   AND act.row_id = lv_Activity_id
                   AND basln.period_id IN (
                          SELECT row_id
                            FROM s_period
                           WHERE period_cd = 'Month'
                             AND bu_id = '0-R9NH'
                             AND TRUNC(promo.SHIP_START_DT,'MM') <= START_DT
                             AND LAST_DAY(promo.SHIP_END_DT) >= END_DT);

                 -- Added by IDC_SS on 27th Jan 2011 for TSP RVC calculations
                 SELECT SUM(NVL(activ_prod.x_src_act_discount_rate / 100,0) * (CASE WHEN per.end_dt <= F_FF_ACTUALIZATION_DATE  THEN NVL(basln.ACTUAL_NIV,0) ELSE  NVL(basln.NIV_FORECAST,0) END))
                     calc INTO lv_Rev_Act_Cost
                 FROM s_src promo,
                       s_src act,
                       s_src promo_cat,
                       s_src activ_prod,
                       cx_tsp_cat_bsln basln,
                       s_period sp,
                       s_period per
                 WHERE act.par_src_id = promo.row_id
                   AND promo_cat.par_src_id = promo.row_id
                   AND act.par_src_id = promo_cat.par_src_id
                   AND promo_cat.ctlg_cat_id = basln.ctlg_cat_id
                   AND promo.sub_type = 'PLAN_ACCOUNT_PROMOTION'
                   AND act.sub_type = 'PG_TFMAAI_PROMOTION_ACTIVITY'
                   AND promo_cat.sub_type = 'PLAN_ACCT_PROMOTION_CATEGORY'
                   AND promo.pr_accnt_id = basln.ou_ext_id
                   AND activ_prod.par_src_id = act.row_id
                   AND activ_prod.ctlg_cat_id = basln.ctlg_cat_id
                   AND activ_prod.ctlg_cat_id = promo_cat.ctlg_cat_id
                   AND activ_prod.sub_type = 'PG_TFMAAI_ACTIVITY_PRODUCT'
                   AND sp.row_id = promo.period_id
                   AND sp.period_cd = 'Fiscal Year'
                   AND act.row_id =lv_Activity_id
                   and per.row_id = basln.period_id
                   and per.period_cd = 'Month'
                   AND basln.period_id IN (
                          SELECT row_id
                            FROM s_period
                           WHERE period_cd = 'Month'
                             AND bu_id = '0-R9NH'
                             AND TRUNC(promo.SHIP_START_DT,'MM') <= START_DT
                             AND LAST_DAY(promo.SHIP_END_DT) >= END_DT);


                       */
                -- Added by IDC_SS for TSP RVC Calculations merged the four queries into one for performance
                     SELECT --SUM ((activ_prod.x_src_act_discount_rate / 100)* (decode(act.FIXED_COST_TYPE_CD,'Variable GIV',( case when basln.ou_ext_id in (select distinct ou_ext_id from s_org_ctlg_cat where type_cd='CURRENT') then NVL(basln.GIV_FORECAST,0) else 0 end)  --Commented by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy) --Modified by IDC_BC for defect 3906 QMR 11.1  Added check on S_ORG_CTLG_CAT table
                                                                        --,act.FIXED_COST_TYPE_CD  --Commented by IDC_AS on 8th Feb 2011
                              --                                          ,'Variable NIV',( case when basln.ou_ext_id in (select distinct ou_ext_id from s_org_ctlg_cat where type_cd='CURRENT') then NVL(basln.NIV_FORECAST,0) else 0 end) --Commented by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)  --Modified by IDC_BC for defect 3906 QMR 11.1  Added check on S_ORG_CTLG_CAT table
                              --                                          ))                          --Commented by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)
                              --        ) EVC, --Commented by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)
                             SUM ((activ_prod.x_src_act_discount_rate / 100)* (decode(act.FIXED_COST_TYPE_CD,'Variable GIV',( case when (promo.prog_start_dt between lv_pfy_start and lv_pfy_end)
                                                                                                                                      then (case when basln.ou_ext_id in (select distinct ou_ext_id from s_org_ctlg_cat where type_cd='PREVIOUS') then NVL(basln.GIV_FORECAST,0) else 0 end) --Added by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)
                                                                                                                                   when (promo.prog_start_dt between lv_cfy_start and lv_cfy_end)
                                                                                                                                      then (case when basln.ou_ext_id in (select distinct ou_ext_id from s_org_ctlg_cat where type_cd='CURRENT') then NVL(basln.GIV_FORECAST,0) else 0 end)
                                                                                                                                   when (promo.prog_start_dt between lv_nfy_start and lv_nfy_end)
                                                                                                                                      then (case when basln.ou_ext_id in (select distinct ou_ext_id from s_org_ctlg_cat where type_cd='NEXT') then NVL(basln.GIV_FORECAST,0) else 0 end)
                                                                                                                                    ELSE 0
                                                                                                                               end)
                                                                        --,act.FIXED_COST_TYPE_CD  --Commented by IDC_AS on 8th Feb 2011
                                                                        ,'Variable NIV',(case when (promo.prog_start_dt between lv_pfy_start and lv_pfy_end)
                                                                                                                  then (case when basln.ou_ext_id in (select distinct ou_ext_id from s_org_ctlg_cat where type_cd='PREVIOUS') then NVL(basln.NIV_FORECAST,0) else 0 end)
                                                                                               when  (promo.prog_start_dt between lv_cfy_start and lv_cfy_end)
                                                                                                                  then (case when basln.ou_ext_id in (select distinct ou_ext_id from s_org_ctlg_cat where type_cd='CURRENT') then NVL(basln.NIV_FORECAST,0) else 0 end)
                                                                                               when (promo.prog_start_dt between lv_nfy_start and lv_nfy_end)
                                                                                                                  then (case when basln.ou_ext_id in (select distinct ou_ext_id from s_org_ctlg_cat where type_cd='NEXT') then NVL(basln.NIV_FORECAST,0) else 0 end)
                                                                                               ELSE 0
                                                                                        end)
                                                                        ))
                                      ) EVC,
                          -- SUM(NVL(activ_prod.x_src_act_discount_rate / 100,0) *
                          --      decode(act.FIXED_COST_TYPE_CD,'Variable GIV',(CASE WHEN per.end_dt <= F_FF_ACTUALIZATION_DATE  THEN NVL(basln.ACTUAL_GIV,0) ELSE  ( case when basln.ou_ext_id in (select distinct ou_ext_id from s_org_ctlg_cat where type_cd='CURRENT') then NVL(basln.GIV_FORECAST,0) else 0 end) END)  --Commented by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)--Modified by IDC_BC for defect 3906 QMR 11.1  Added check on S_ORG_CTLG_CAT table
                                      --,act.FIXED_COST_TYPE_CD  --Commented by IDC_AS on 8th Feb 2011
                          --            ,'Variable NIV',(CASE WHEN per.end_dt <= F_FF_ACTUALIZATION_DATE  THEN NVL(basln.ACTUAL_NIV,0) ELSE  ( case when basln.ou_ext_id in (select distinct ou_ext_id from s_org_ctlg_cat where type_cd='CURRENT') then NVL(basln.NIV_FORECAST,0) else 0 end) END)   --Commented by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)--Modified by IDC_BC for defect 3906 QMR 11.1  Added check on S_ORG_CTLG_CAT table
                          --            )
                          --            )
                          SUM(NVL(activ_prod.x_src_act_discount_rate / 100,0) *
                                (Case When (Promo.Prog_Start_Dt Between Lv_Pfy_Start And Lv_Pfy_End)
                                      Then Decode(Act.Fixed_Cost_Type_Cd,'Variable GIV', (Case When Basln.Ou_Ext_Id In (Select Distinct Ou_Ext_Id From S_Org_Ctlg_Cat Where Type_Cd='PREVIOUS') Then (Case When Per.End_Dt < F_Ff_Actualization_Date  Then Nvl(Actual_Giv,0) Else Nvl(Basln.Giv_Forecast,0) End) Else 0 End) --Added by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)
                                                                        ,'Variable NIV',(Case When Basln.Ou_Ext_Id In (Select Distinct Ou_Ext_Id From S_Org_Ctlg_Cat Where Type_Cd='PREVIOUS') Then (Case When Per.End_Dt < F_Ff_Actualization_Date  Then Nvl(Actual_Niv,0) Else Nvl(Basln.Niv_Forecast,0) End) Else 0 End)
                                                   ,0 )
                                      When (Promo.Prog_Start_Dt Between Lv_Cfy_Start And Lv_Cfy_End)
                                      Then Decode(Act.Fixed_Cost_Type_Cd,'Variable GIV',(Case When Basln.Ou_Ext_Id In (Select Distinct Ou_Ext_Id From S_Org_Ctlg_Cat Where Type_Cd='CURRENT') Then (Case When Per.End_Dt < F_Ff_Actualization_Date  Then Nvl(Actual_Giv,0) Else Nvl(Basln.Giv_Forecast,0) End) Else 0 End)
                                                                         ,'Variable NIV',(Case When Basln.Ou_Ext_Id In (Select Distinct Ou_Ext_Id From S_Org_Ctlg_Cat Where Type_Cd='CURRENT') Then (Case When Per.End_Dt < F_Ff_Actualization_Date  Then Nvl(Actual_Niv,0) Else Nvl(Basln.Niv_Forecast,0) End) Else 0 End)
                                           ,0 )
                                      when (promo.prog_start_dt between lv_nfy_start and lv_nfy_end)
                                      Then Decode(Act.Fixed_Cost_Type_Cd,'Variable GIV',(Case When Basln.Ou_Ext_Id In (Select Distinct Ou_Ext_Id From S_Org_Ctlg_Cat Where Type_Cd='NEXT') Then (Case When Per.End_Dt < F_Ff_Actualization_Date  Then Nvl(Actual_Giv,0) Else Nvl(Basln.Giv_Forecast,0) End) Else 0 End)
                                                                        ,'Variable NIV',(Case When Basln.Ou_Ext_Id In (Select Distinct Ou_Ext_Id From S_Org_Ctlg_Cat Where Type_Cd='NEXT') Then (Case When Per.End_Dt < F_Ff_Actualization_Date  Then Nvl(Actual_Niv,0) Else Nvl(Basln.Niv_Forecast,0) End) Else 0 End)
                                            ,0 )
                                  end)
                              )
                                 rvc INTO lv_Activity_Cost,lv_Rev_Act_Cost
                             FROM s_src promo,
                                   s_src act,
                                   s_src promo_cat,
                                   s_src activ_prod,
                                   cx_tsp_cat_bsln basln,
                                   s_period sp,
                                   s_period per
                             WHERE act.par_src_id = promo.row_id
                               AND promo_cat.par_src_id = promo.row_id
                               AND act.par_src_id = promo_cat.par_src_id
                               AND promo_cat.ctlg_cat_id = basln.ctlg_cat_id
                               AND promo.sub_type = 'PLAN_ACCOUNT_PROMOTION'
                               AND act.sub_type = 'PG_TFMAAI_PROMOTION_ACTIVITY'
                               AND promo_cat.sub_type = 'PLAN_ACCT_PROMOTION_CATEGORY'
                               AND promo.pr_accnt_id = basln.ou_ext_id
                               AND activ_prod.par_src_id = act.row_id
                               AND activ_prod.ctlg_cat_id = basln.ctlg_cat_id
                               AND activ_prod.ctlg_cat_id = promo_cat.ctlg_cat_id
                               AND activ_prod.sub_type = 'PG_TFMAAI_ACTIVITY_PRODUCT'
                               AND sp.row_id = promo.period_id
                               AND sp.period_cd = 'Fiscal Year'
                               AND act.row_id =lv_Activity_id
                               AND act.FIXED_COST_TYPE_CD in ('Variable GIV','Variable NIV')
                               and per.row_id = basln.period_id
                               and per.period_cd = 'Month'
                               AND basln.period_id IN (
                                      SELECT row_id
                                        FROM s_period
                                       WHERE period_cd = 'Month'
                                         AND bu_id = '0-R9NH'
                                         AND TRUNC(promo.SHIP_START_DT,'MM') <= START_DT
                                         AND LAST_DAY(promo.SHIP_END_DT) >= END_DT);

           END IF;


            IF lv_src_cd = 'Annual Agreement' THEN



               update s_mdf_alloc
               set
               X_REVISED_VAR_COST=ROUND_DISPLAY(round(GREATEST(NVL(lv_Rev_Act_Cost,0),0),3)),  -- Added by IDC_SS on 27th Jan 2011 for TSP RVC calculations
               X_FCST_VARIABLE_COST=ROUND_DISPLAY(round(GREATEST(NVL(lv_Activity_cost,0),0),3)),
               est_mdf_amt=ROUND_DISPLAY(round((decode(lv_cost_indicator,'Estimated Variable Cost',NVL(X_FCST_FIXED_COST,0) + NVL(lv_Activity_Cost,0),
                            'Revised Variable Cost',NVL(X_FCST_FIXED_COST,0) + NVL(lv_Rev_Act_Cost,0),
                            'Manual Cost Overwrite',NVL(X_MANUAL_COST,0),
                             NVL(X_FCST_FIXED_COST,0) + NVL(lv_Rev_Act_Cost,0))),3)),
               LAST_UPD = SYS_EXTRACT_UTC(SYSTIMESTAMP) -- Updated by IDC_AM for P#2163 to update the last_upd to UTC time
               where promo_id=lv_Activity_id;



            END IF;

       else
       /* selcting fund class for rounding errors in R14.1 added by IDC_SC*/
           select X_FND_CLAS into LV_FUND_CLAS
           from S_MDF where ROW_ID
           in (select MDF_ID from S_MDF_ALLOC where PROMO_ID = LV_ACTIVITY_ID);

       IF lv_Fixed_Cost_Type IN ('Variable GIV','Volume') THEN
            -- If we get into this part of the routine, we have some Variable pricing activities to process
            -- Calculate cost of activity:
            If lv_Pricing_Level = 'GTIN/Promoted Group' THEN -- New check - this means use the old routine...
                select ROUND(decode(lv_anaplan_id,null,
                  NVL(SUM(    -- RATE START
                                     DECODE(UPPER (activ.x_price_featuring_disc_type),'AMOUNT PER BUOM', activ_prod.x_src_act_discount_rate,
                                                   activ_prod.x_src_act_discount_rate* item.DFLT_COST_UNIT/ 100 ) -- 1/28/08 MAK dated price list change
                               * ROUND( (--Added ROUND for BUOM by IDC_TF for Rel9,QC121
                                       ( (ROUND( ( (NVL(basln.prd_bsln_pct* DECODE(SIGN (baseline.planned_sales),-1, 0,baseline.planned_sales)/ 100,0) )/ prod.x_g_sufctor), 7 )  )  --Updated the rounding to 7 by IDC_AM for BUOM Rounding logic for R13 on 30th May 2012   --  Baseline Planning SU
                                         + ROUND    -- Added ROUND for BUOM by IDC for CR545
                                            ( ( ( DECODE( promo_cat.X_INCREMENTAL_MODEL,
                                                   'Absolute', NVL (basln.cg_incr_sales, 0),
                                                   'Percent', (NVL(basln.prd_bsln_pct* DECODE(SIGN (baseline.planned_sales),-1, 0,baseline.planned_sales)/ 100,0)
                                                               * NVL (basln.X_INCREMENTAL_PCT / 100, 0)
                                                               ) ) -- end of + Decode
                                        ) / prod.x_g_sufctor) , 7  )  ) --Updated the rounding to 7 by IDC_AM for BUOM Rounding logic for R13 on 30th May 2012 --suf factor
                                      -- )-- *(activ.X_REDEMPTION_RATE/100) --Commented and Added by IDC_TF,CO2446,taking redemption rate from promoted product level--Added by IDC_VH;Rel 7;FR54.2;Update variable cost calculation
                                      -- ) --End of ROUND for BUOM  --commented by IDC_TF for CR106 R9 on 28-Sep-09;Included redemption rate and round
                                     *(promo_prod.X_REDEMPTION_RATE/100)
                                     ) --Added by IDC_TF for CR106 R9 on 28-Sep-09;Included redemption rate and round
                                     , 7 ) ) --Updated the rounding to 7 by IDC_AM for BUOM Rounding logic for R13 on 30th May 2012 --END OF SUM
                                 ,0)--End of NVL
                             ,
                             NVL(SUM(    -- RATE START FOR ANAPLAN
                                     activ_prod.x_src_act_discount_rate *
                                     decode(UPPER (activ.x_price_featuring_disc_type),'AMOUNT PER BUOM', basln.X_ANA_TOT_BUOM, basln.X_ANA_TOT_GIV/100)
                                     *(promo_prod.X_REDEMPTION_RATE/100)--IDC_SC Anaplan
                                     )--END OF SUM
                                 ,0))--End of NVL
                             ,2)    --Modified by IDC_AS on 09th Oct for rounding off variable cost to 2 decimal places because of production issue PR 80151
                INTO lv_Activity_cost
              FROM   S_SRC activ,
                       S_SRC activ_prod,
                       S_PROD_INT prod,
                       S_SRC promo_cat,
                       S_SRC promo_prod,
                       S_SRC_PRD_BASLN basln,
                       S_PRI_LST_ITEM item, -- added 1/28/08 MAK
                       S_PROD_BASELINE baseline
                Where activ_prod.par_src_id = activ.ROW_ID
                    and activ_prod.ctlg_cat_id = promo_prod.ctlg_cat_id -- 2/13/08 MAK
                    and activ_prod.prod_id = prod.row_id
                    and prod.row_id = promo_prod.prod_id
                    and prod.x_g_sufctor > 0 --  Added by IDC_TF 24-Feb-2009,Rel8 ,QC625, To exclude records that have ZERO suffctor
                    and activ.par_src_id = promo_cat.par_src_id
                    and promo_prod.par_src_id = promo_cat.row_id
                    and promo_prod.row_id = basln.src_id
                    and basln.prod_baseline_id = baseline.row_id
                    and promo_cat.sub_type = 'PLAN_ACCT_PROMOTION_CATEGORY'
                    and activ.ROW_ID = lv_Activity_id
                    AND ITEM.X_PERIOD_ID(+) = BASLN.X_PERIOD_ID  -- added 1/28/08 MAK
                    AND ITEM.PROD_ID(+) = BASLN.X_PROD_ID        -- added 1/28/08 MAK
                    AND ITEM.PRI_LST_ID(+) = BASLN.X_PRI_LST_ID; -- added 1/28/08 MAK
            Else -- category or brand level - have to also join to S_CTLG_CAT_PROD (MAK 1/29/08)
                select ROUND(decode(lv_anaplan_id,null,
                  NVL(SUM(    -- RATE START
                                     DECODE(UPPER (activ.x_price_featuring_disc_type),'AMOUNT PER BUOM', activ_prod.x_src_act_discount_rate,
                                                   activ_prod.x_src_act_discount_rate* item.DFLT_COST_UNIT/ 100 ) -- 1/28/08 MAK dated price list change
                               * ROUND( (--Added ROUND for BUOM by IDC_TF for Rel9,QC121
                                       ( (ROUND( ( (NVL(basln.prd_bsln_pct* DECODE(SIGN (baseline.planned_sales),-1, 0,baseline.planned_sales)/ 100,0) )/ prod.x_g_sufctor), 7 )  )  --Updated the rounding to 7 by IDC_AM for BUOM Rounding logic for R13 on 30th May 2012   --  Baseline Planning SU
                                         + ROUND    -- Added ROUND for BUOM by IDC for CR545
                                            ( ( ( DECODE( promo_cat.X_INCREMENTAL_MODEL,
                                                   'Absolute', NVL (basln.cg_incr_sales, 0),
                                                   'Percent', (NVL(basln.prd_bsln_pct* DECODE(SIGN (baseline.planned_sales),-1, 0,baseline.planned_sales)/ 100,0)
                                                               * NVL (basln.X_INCREMENTAL_PCT / 100, 0)
                                                               ) ) -- end of + Decode
                                        ) / prod.x_g_sufctor) , 7  )  ) --Updated the rounding to 7 by IDC_AM for BUOM Rounding logic for R13 on 30th May 2012 --suf factor
                                      -- )-- *(activ.X_REDEMPTION_RATE/100) --Commented and Added by IDC_TF,CO2446,taking redemption rate from promoted product level--Added by IDC_VH;Rel 7;FR54.2;Update variable cost calculation
                                      -- ) --End of ROUND for BUOM  --commented by IDC_TF for CR106 R9 on 28-Sep-09;Included redemption rate and round
                                     *(promo_prod.X_REDEMPTION_RATE/100)
                                     ) --Added by IDC_TF for CR106 R9 on 28-Sep-09;Included redemption rate and round
                                     , 7 ) ) --Updated the rounding to 7 by IDC_AM for BUOM Rounding logic for R13 on 30th May 2012 --END OF SUM
                                 ,0)--End of NVL
                             ,
                             NVL(SUM(    -- RATE START FOR ANAPLAN
                                     activ_prod.x_src_act_discount_rate *
                                     decode(UPPER (activ.x_price_featuring_disc_type),'AMOUNT PER BUOM', basln.X_ANA_TOT_BUOM, basln.X_ANA_TOT_GIV/100)
                                     *(promo_prod.X_REDEMPTION_RATE/100)--IDC_SC Anaplan
                                     )--END OF SUM
                                 ,0))--End of NVL
                             ,2)    --Modified by IDC_AS on 09th Oct for rounding off variable cost to 2 decimal places because of production issue PR 80151
                INTO lv_Activity_cost
              FROM   S_SRC activ,
                       S_SRC activ_prod,
                       S_SRC promo_prod,
                       S_SRC promo_cat,
                       S_SRC_PRD_BASLN basln,
                       S_PRI_LST_ITEM item, -- added 1/28/08 MAK
                       S_PROD_BASELINE baseline,
                       S_PROD_INT prod--,
                       --S_CTLG_CAT_PROD SCCP -- Commented by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)
                Where activ_prod.par_src_id = activ.ROW_ID
                    --and activ_prod.prod_id = prod.row_id -- no longer join directly to the product for category level activity product records
                    and activ_prod.ctlg_cat_id = promo_prod.ctlg_cat_id -- 2/13/08 MAK
                    --and activ_prod.ctlg_cat_id = SCCP.Ctlg_cat_id  -- Commented by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)-- Instead we join activity product to S_CTLG_CAT_PROD
                    --and SCCP.PROD_ID = prod.row_id                 -- Commented by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)-- and then to the product
                    and prod.row_id = promo_prod.prod_id
                    and prod.x_g_sufctor > 0 --  Added by IDC_TF 24-Feb-2009,Rel8 ,QC625, To exclude records that have ZERO suffctor
                    and activ.par_src_id = promo_cat.par_src_id
                    and promo_prod.par_src_id = promo_cat.row_id
                    and promo_prod.row_id = basln.src_id
                    and basln.prod_baseline_id = baseline.row_id
                    and promo_cat.sub_type = 'PLAN_ACCT_PROMOTION_CATEGORY'
                    and activ.ROW_ID = lv_Activity_id
                    AND ITEM.X_PERIOD_ID(+) = BASLN.X_PERIOD_ID  -- added 1/28/08 MAK
                    AND ITEM.PROD_ID(+) = BASLN.X_PROD_ID        -- added 1/28/08 MAK
                    AND ITEM.PRI_LST_ID(+) = BASLN.X_PRI_LST_ID; -- added 1/28/08 MAK
            End If; -- Pricing Level Check
            ---Elsif lv_Fixed_Cost_Type = 'Variable NIV'  commented by IDC_TF Aug-5-2008 Rel 7.
            Elsif lv_Fixed_Cost_Type in('Variable NIV','Variable NIV2') THEN ---Addedby IDC_TF Aug-5-2008 Rel 7. Added Variable NIV2 to handle the calculation for the same.
            -- If we get into this part of the routine, we have some Variable pricing activities to process
            -- Calculate cost of activity:
            If lv_Pricing_Level = 'GTIN/Promoted Group' THEN -- New check - this means use the old routine...
                SELECT  ROUND(NVL(SUM(    -- RATE START
                                     (ACTIV_PROD.X_SRC_ACT_DISCOUNT_RATE / 100) *
                                     decode(lv_anaplan_id,null,((NVL(basln.prd_bsln_pct* DECODE(SIGN (baseline.planned_sales),-1, 0,baseline.planned_sales)/ 100,0) --  Baseline Planning SU
                                         + DECODE( promo_cat.X_INCREMENTAL_MODEL,
                                                   'Absolute', NVL (basln.cg_incr_sales, 0),
                                                   'Percent', (NVL(basln.prd_bsln_pct* DECODE(SIGN (baseline.planned_sales),-1, 0,baseline.planned_sales)/ 100,0)
                                                               * NVL (basln.X_INCREMENTAL_PCT / 100, 0)
                                                               ) ) -- end of + Decode
                                        ) ----* x_nivsu_conv Commented by IDC_TF;Rel 7; QC223; 17-july-2008 NIVSU factor will be zero for null and '0' values.
                                        * NVL(baseline.X_NIVSU_CONV,0)-- NIV/SU Conversion factor added by IDC_TF ref7/QC 223 17-july-2008
                                       --) *(activ.X_REDEMPTION_RATE/100) --Commented and Added by IDC_TF,CO2446,taking redemption rate from promoted product level--Added by IDC_VH;Rel 7;FR54.2;Update variable cost calculation
                                     ),basln.X_ANA_TOT_NIV) *(promo_prod.X_REDEMPTION_RATE/100)--IDC_SC Anaplan
                                     )--END OF SUM
                                 ,0)--End of NVL
                              --,7)   --Commented by IDC_AS on 09th Oct for rounding off variable cost to 2 decimal places because of production issue PR 80151
                             ,2)    --Modified by IDC_AS on 09th Oct for rounding off variable cost to 2 decimal places because of production issue PR 80151
                INTO lv_Activity_cost
              FROM   S_SRC activ,
                       S_SRC activ_prod,
                       S_PROD_INT prod,
                       S_SRC promo_cat,
                       S_SRC promo_prod,
                       S_SRC_PRD_BASLN basln,
                       S_PRI_LST_ITEM item, -- added 1/28/08 MAK
                       S_PROD_BASELINE baseline
                Where activ_prod.par_src_id = activ.ROW_ID
                    and activ_prod.ctlg_cat_id = promo_prod.ctlg_cat_id -- 2/13/08 MAK
                    and activ_prod.prod_id = prod.row_id
                    and prod.row_id = promo_prod.prod_id
                    and activ.par_src_id = promo_cat.par_src_id
                    and promo_prod.par_src_id = promo_cat.row_id
                    and promo_prod.row_id = basln.src_id
                    and basln.prod_baseline_id = baseline.row_id
                    and promo_cat.sub_type = 'PLAN_ACCT_PROMOTION_CATEGORY'
                    and activ.ROW_ID = lv_Activity_id
                    AND ITEM.X_PERIOD_ID(+) = BASLN.X_PERIOD_ID  -- added 1/28/08 MAK
                    AND ITEM.PROD_ID(+) = BASLN.X_PROD_ID        -- added 1/28/08 MAK
                    AND ITEM.PRI_LST_ID(+) = BASLN.X_PRI_LST_ID; -- added 1/28/08 MAK
            Else -- category or brand level - have to also join to S_CTLG_CAT_PROD (MAK 1/29/08)
                SELECT ROUND(NVL(SUM(     -- RATE START
                                     (decode(lv_anaplan_id,null,((NVL(basln.prd_bsln_pct* DECODE(SIGN (baseline.planned_sales),-1, 0,baseline.planned_sales)/ 100,0) --  Baseline Planning SU
                                         + DECODE( promo_cat.X_INCREMENTAL_MODEL,
                                                   'Absolute', NVL (basln.cg_incr_sales, 0),
                                                   'Percent', (NVL(basln.prd_bsln_pct* DECODE(SIGN (baseline.planned_sales),-1, 0,baseline.planned_sales)/ 100,0)
                                                               * NVL (basln.X_INCREMENTAL_PCT / 100, 0)
                                                               ) ) -- end of + Decode
                                        ) ----* x_nivsu_conv Commented by IDC_TF;Rel 7; QC223; 17-july-2008 NIVSU factor will be zero for null and '0' values.
                                        * NVL(baseline.X_NIVSU_CONV,0)),basln.X_ANA_TOT_NIV)-- NIV/SU Conversion factor added by IDC_TF;Rel 7; QC223; 17-july-2008 NIVSU factor will be zero for null and '0' values.
                                        * (activ_prod.x_src_act_discount_rate / 100)
                                      --) *(activ.X_REDEMPTION_RATE/100) --Commented and Added by IDC_TF,CO2446,taking redemption rate from promoted product level--Added by IDC_VH;Rel 7;FR54.2;Update variable cost calculation
                                     ) *(promo_prod.X_REDEMPTION_RATE/100)
                                     )--END OF SUM
                                 ,0)--End of NVL
                              --,7)   --Commented by IDC_AS on 09th Oct for rounding off variable cost to 2 decimal places because of production issue PR 80151
                             ,2)    --Modified by IDC_AS on 09th Oct for rounding off variable cost to 2 decimal places because of production issue PR 80151
                INTO lv_Activity_cost
              FROM   S_SRC activ,
                       S_SRC activ_prod,
                       S_SRC promo_prod,
                       S_SRC promo_cat,
                       S_SRC_PRD_BASLN basln,
                       S_PRI_LST_ITEM item, -- added 1/28/08 MAK
                       S_PROD_BASELINE baseline,
                       S_PROD_INT prod--,
                       --S_CTLG_CAT_PROD SCCP -- Commented by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy)
                Where activ_prod.par_src_id = activ.ROW_ID
                    -- and activ_prod.prod_id = prod.row_id -- no longer join directly to the product for category level activity product records
                    and activ_prod.ctlg_cat_id = promo_prod.ctlg_cat_id -- 2/13/08 MAK
                    --and activ_prod.ctlg_cat_id = SCCP.Ctlg_cat_id -- Commented by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy) -- Instead we join activity product to S_CTLG_CAT_PROD
                    --and SCCP.PROD_ID = prod.row_id                -- Commented by IDC_AM on 6th June 2012 for PM00014098 (Promo Cost Discrepancy) -- and then to the product
                    and prod.row_id = promo_prod.prod_id
                    and activ.par_src_id = promo_cat.par_src_id
                    and promo_prod.par_src_id = promo_cat.row_id
                    and promo_prod.row_id = basln.src_id
                    and basln.prod_baseline_id = baseline.row_id
                    and promo_cat.sub_type = 'PLAN_ACCT_PROMOTION_CATEGORY'
                    and activ.ROW_ID = lv_Activity_id
                    AND ITEM.X_PERIOD_ID(+) = BASLN.X_PERIOD_ID  -- added 1/28/08 MAK
                    AND ITEM.PROD_ID(+) = BASLN.X_PROD_ID        -- added 1/28/08 MAK
                    AND ITEM.PRI_LST_ID(+) = BASLN.X_PRI_LST_ID; -- added 1/28/08 MAK
            end if; -- Pricing Level Check
            /*Removed round upto 2 decimal places logic for TFI funds as a part of R14.1 change by IDC_SC*/
            elsif (lv_Fixed_Cost_Type = '% Fund' and UPPER(LV_FUND_CLAS) in  ('KBD1 TFI FIXED','KBD1 TFI LIVE','KBD2 TFI LIVE','KBD2 TFI FIXED') )then
           Select count(*) into lv_NumberOfRecs from S_MDF_ALLOC ALLOC, S_MDF MDF
            Where ALLOC.MDF_ID = MDF.ROW_ID and ALLOC.PROMO_ID = lv_Activity_id;
            if LV_NUMBEROFRECS > 0 then
                Select  (NVL(ALLOC.X_PERC_FUND_FORECAST,0)/100) *  --Modified by IDC_AS on 09th Oct for rounding off variable cost to 2 decimal places because of production issue PR 80151
                         (NVL(MDF.X_TOTAL_FORECAST_AMT, 0)
                            + NVL(MDF.X_TRANSFER_IN_AMT,0)
                            - NVL(MDF.X_TRANSFER_OUT_AMT,0)
                            + NVL((Select SUM(NVL(TXN.TXN_AMT,0)) FROM S_MDF_TXN TXN where TXN.TO_MDF_ID = MDF.ROW_ID
                                and TXN.TYPE_CD = 'Adjustment'),0)
                            - NVL(MDF.X_AVAIL_FIXED_AMT,0) )  --removed rounding up to 2 decimal places as a fix of rounding error for TFI fund in R14.1 added by IDC_SC
                INTO lv_Activity_cost
                FROM S_MDF_ALLOC ALLOC, S_MDF MDF
                where ALLOC.MDF_ID = MDF.ROW_ID
                  and ALLOC.PROMO_ID = LV_ACTIVITY_ID;
                   else
                lv_Activity_cost:=0;
                  end if;
        Elsif lv_Fixed_Cost_Type = '% Fund' THEN -- New type of cost
            -- Need to check for zero records returned if there are no allocation records: (2/6/08 MAK)
            Select count(*) into lv_NumberOfRecs from S_MDF_ALLOC ALLOC, S_MDF MDF
            Where ALLOC.MDF_ID = MDF.ROW_ID and ALLOC.PROMO_ID = lv_Activity_id;
            if LV_NUMBEROFRECS > 0 then
                Select ROUND( (NVL(ALLOC.X_PERC_FUND_FORECAST,0)/100) *  --Modified by IDC_AS on 09th Oct for rounding off variable cost to 2 decimal places because of production issue PR 80151
                         (NVL(MDF.X_TOTAL_FORECAST_AMT, 0)
                            + NVL(MDF.X_TRANSFER_IN_AMT,0)
                            - NVL(MDF.X_TRANSFER_OUT_AMT,0)
                            + NVL((Select SUM(NVL(TXN.TXN_AMT,0)) FROM S_MDF_TXN TXN where TXN.TO_MDF_ID = MDF.ROW_ID
                                and TXN.TYPE_CD = 'Adjustment'),0)
                            - NVL(MDF.X_AVAIL_FIXED_AMT,0) ),2)  --Modified by IDC_AS on 09th Oct for rounding off variable cost to 2 decimal places because of production issue PR 80151 --added NVL for defect 1972 R9 by IDC_AC-- Added by IDC on 16-Sep-09 for FR53.2 R9.
                INTO lv_Activity_cost
                FROM S_MDF_ALLOC ALLOC, S_MDF MDF
                Where ALLOC.MDF_ID = MDF.ROW_ID
                  and ALLOC.PROMO_ID = lv_Activity_id;
            Else
                lv_Activity_cost:=0;
            End If; -- Check for zero
         End If; -- Fixed Cost Type/Activity Type


        /* Added By IDC_SS 20-JAN-10 R10 WEt97 it wil calculate promo cost on the basis of new field (X_COST_INDICATOR
        at activity level by default annual agreement wil be calculated on revised variable cost and others wil be
        calculated on estimated variable cost*/
            IF lv_src_cd = 'Annual Agreement' THEN


               update s_mdf_alloc
               set
               est_mdf_amt=ROUND_DISPLAY(round((decode(lv_cost_indicator,'Estimated Variable Cost',NVL(X_FCST_FIXED_COST,0) + NVL(X_FCST_VARIABLE_COST,0),
                            'Revised Variable Cost',NVL(X_FCST_FIXED_COST,0) + NVL(X_REVISED_VAR_COST,0),
                            'Manual Cost Overwrite',NVL(X_MANUAL_COST,0),
                             NVL(X_FCST_FIXED_COST,0) + NVL(X_REVISED_VAR_COST,0))),3)),
               LAST_UPD = SYS_EXTRACT_UTC(SYSTIMESTAMP) -- Updated by IDC_AM for P#2163 to update the last_upd to UTC time
               where PROMO_ID=LV_ACTIVITY_ID;
               /*Removed round upto 2 decimal places logic for TFI funds as a part of R14.1 change by IDC_SC*/
           ELSIF (LV_SRC_CD <> 'Annual Agreement' and LV_FIXED_COST_TYPE = '% Fund' and UPPER(LV_FUND_CLAS) in  ('KBD1 TFI FIXED','KBD1 TFI LIVE','KBD2 TFI LIVE','KBD2 TFI FIXED'))
            then
               update s_mdf_alloc
               --set X_FCST_VARIABLE_COST = NVL(lv_Activity_cost,0), --Commented by IDC_AS on 10th Mar 2010 for Defect 810 - Variable cost should cannot be Negative
               --set X_FCST_VARIABLE_COST = ROUND_DISPLAY(round(GREATEST(NVL(lv_Activity_cost,0),0),3)), --Modified by IDC_AS on 10th Mar 2010 for Defect 810
                 set X_FCST_VARIABLE_COST = ROUND_DISPLAY(GREATEST(NVL(lv_Activity_cost,0),0)), -- removed round upto 2 decimal places logic as a part of R14.1 change by IDC_SC
                   est_mdf_amt=ROUND_DISPLAY(round((decode(lv_cost_indicator,'Estimated Variable Cost',GREATEST(NVL(X_FCST_FIXED_COST,0) + NVL(X_FCST_VARIABLE_COST,0)),
                                  'Revised Variable Cost',GREATEST(NVL(X_FCST_FIXED_COST,0) + NVL(X_REVISED_VAR_COST,0)),
                                  'Manual Cost Overwrite',ABS(NVL(X_MANUAL_COST,0)),
                                  GREATEST(NVL(X_FCST_FIXED_COST,0) + NVL(X_FCST_VARIABLE_COST,0)))),3)),
                   LAST_UPD = SYS_EXTRACT_UTC(SYSTIMESTAMP) -- Updated by IDC_AM for P#2163 to update the last_upd to UTC time
               where PROMO_ID=LV_ACTIVITY_ID;
               else
               update s_mdf_alloc
               --set X_FCST_VARIABLE_COST = NVL(lv_Activity_cost,0), --Commented by IDC_AS on 10th Mar 2010 for Defect 810 - Variable cost should cannot be Negative
               set X_FCST_VARIABLE_COST = ROUND_DISPLAY(ROUND(GREATEST(NVL(LV_ACTIVITY_COST,0),0),3)), --Modified by IDC_AS on 10th Mar 2010 for Defect 810
                                    est_mdf_amt=ROUND_DISPLAY(round((decode(lv_cost_indicator,'Estimated Variable Cost',GREATEST(NVL(X_FCST_FIXED_COST,0) + NVL(X_FCST_VARIABLE_COST,0)),
                                  'Revised Variable Cost',GREATEST(NVL(X_FCST_FIXED_COST,0) + NVL(X_REVISED_VAR_COST,0)),
                                  'Manual Cost Overwrite',ABS(NVL(X_MANUAL_COST,0)),
                                  GREATEST(NVL(X_FCST_FIXED_COST,0) + NVL(X_FCST_VARIABLE_COST,0)))),3)),
                   LAST_UPD = SYS_EXTRACT_UTC(systimestamp) -- Updated by IDC_AM for P#2163 to update the last_upd to UTC time
               where promo_id=lv_Activity_id;

           end if;


           END IF;


       /*    commented by IDC_SS for R10 WET97
       IF lv_src_cd = 'Annual Agreement' THEN

        -- We have the activity cost, now we just need to update the S_MDF_ALLOC record
        -- New for 2008: We're updating the EST_MDF_AMT also, not just the X_FCST_VARIABLE_COST
        Update S_MDF_ALLOC
          set -- EST_MDF_AMT = NVL(lv_Activity_cost,0) + NVL(X_FCST_FIXED_COST,0) commented by IDC_TF rel 7 10-july-2008;
              EST_MDF_AMT = GREATEST(NVL(X_FCST_VARIABLE_COST,0),NVL(X_REVISED_VAR_COST,0)) --Added by IDC_TF rel 7; 10-july-2008 adding the revised variable cost column to get the estimated fund amount.
                      + NVL(X_FCST_FIXED_COST,0)
          where PROMO_ID = lv_Activity_id;

         Else

         -- We have the activity cost, now we just need to update the S_MDF_ALLOC record
        -- New for 2008: We're updating the EST_MDF_AMT also, not just the X_FCST_VARIABLE_COST
        Update S_MDF_ALLOC
          set X_FCST_VARIABLE_COST = NVL(lv_Activity_cost,0),
             -- EST_MDF_AMT = NVL(lv_Activity_cost,0) + NVL(X_FCST_FIXED_COST,0) commented by IDC_TF rel 7 10-july-2008;
              EST_MDF_AMT = GREATEST(NVL(X_FCST_VARIABLE_COST,0),NVL(X_REVISED_VAR_COST,0)) --Added by IDC_TF rel 7; 10-july-2008 adding the revised variable cost column to get the estimated fund amount.
                      + NVL(X_FCST_FIXED_COST,0)
          where PROMO_ID = lv_Activity_id;


         End If;*/

      -- New Part of the Routine: Now we have a set of new data to calculate for the promotion:
      -- ACTL_FIXED_COST (Actuals fixed cost booked)
      -- X_ACTL_VAR_COST (Actual variable cost)

      -- First The Actuals Fixed Cost Booked; Note that this is the sum of booked amounts on the activity
      -- plus the sum times all the split records.  Each % Split record does have an associated cost, but
      -- we don't care about that, just the total.
/* Commented by IDC_AC on 29-Jun-2009 for Cosmos off invoice reversals to remove the DOC type hardcoding for WE S10/ CR 23 FR 27.1*/
      -- Get the booked total:
--      Select SUM(Payment.CG_TRADE_FUND_TOT) into lv_Booked_Total from S_MDF_ALLOC Alloc, S_SRC_PAYMENT Payment
--      where Alloc.Promo_id = lv_Activity_id
--      and   Payment.CG_MDF_ALLOC_ID = Alloc.ROW_ID
--      and   Payment.PAY_STAT_CD = 'Booked'
--      and   Payment.X_SAP_DOC_TYPE <> '2A';

/* Added by IDC_AC on 29-Jun-2009 for Cosmos off invoice reversals to remove the DOC type hardcoding for WE S10/ CR 23 FR 27.1*/
          Select SUM(Payment.CG_TRADE_FUND_TOT) into lv_Booked_Total from S_MDF_ALLOC Alloc,
          S_SRC_PAYMENT Payment, S_LST_OF_VAL val, --H_TFM_INTERFACE_ORGANIZATIONS org,
          S_BU bu,S_LST_OF_VAL lov
      where Alloc.Promo_id = lv_Activity_id
      and   Payment.CG_MDF_ALLOC_ID = Alloc.ROW_ID
      and   Payment.PAY_STAT_CD = 'Booked'
      and   Payment.X_SAP_DOC_TYPE = val.val
      and   RTRIM (substr( Payment.X_SIEBEL_ID,1,2)) = lov.val--org.GEOGRAPHY_ID
      and   lov.type='COUNTRY_CODE'             --added by IDC_KS; 18-Nov-09;Thailand has TI
      and   lov.name=bu.name                    --added by IDC_KS; 18-Nov-09;

      AND  LOV.WS_MIN_VER <= 9999 AND  LOV.WS_MAX_VER > 9999 AND  LOV.WS_INACTIVE_FLG = 'N' 
     AND  LOV.WS_ID = (SELECT ROW_ID FROM S_WORKSPACE WHERE NAME = 'MAIN' and REPOSITORY_ID =(SELECT ROW_ID FROM S_REPOSITORY WHERE NAME = 'Siebel Repository'))--Added by IDC_HB for IP 18 Upgrade 2018


      --and   org.ORGANIZATION = bu.name        --Commented by IDC_KS; 18-Nov-09;
      and   BU.ROW_ID = VAL.BU_ID
      and   val.type='PG_TFMIB_FI_DOC_PAY_MEANS'

        AND  VAL.WS_MIN_VER <= 9999 AND  VAL.WS_MAX_VER > 9999 AND  VAL.WS_INACTIVE_FLG = 'N' 
        AND  VAL.WS_ID = (SELECT ROW_ID FROM S_WORKSPACE WHERE NAME = 'MAIN' and REPOSITORY_ID =(SELECT ROW_ID FROM S_REPOSITORY WHERE NAME = 'Siebel Repository'))--Added by IDC_HB for IP 18 Upgrade 2018


      and   VAL.X_FILTER_VAL not in ('Off Invoice','On Invoice');-- IDC_AC changed for defect 626, the On invoice doc type can be set up for multiple Doc types

--      Commented by IDC_TF on 07-Aug-08 Rel-7
--       -- And the sum of split percentages:
--       Select NVL(SUM(X_CORP_ACT_PERCENTAGE_SPLIT),0) into lv_Split_Total from S_SRC
--       where PAR_SRC_ID = lv_Activity_id
--       and SUB_TYPE = 'PG_TFMAAI_PERCENT_SPLIT';

/* Commented by IDC_AC on 29-Jun-2009 for Cosmos off invoice reversals to remove the DOC type hardcoding for WE S10/ CR 23 FR 27.1*/
      -- And Actual Variable Cost:
--      Select SUM(Payment.CG_TRADE_FUND_TOT) into lv_Actl_Var_Cost from S_MDF_ALLOC Alloc, S_SRC_PAYMENT Payment, S_LST_OF_VAL val
--      Where Alloc.Promo_id = lv_Activity_id
--      ---and   Payment.CG_MDF_ALLOC_ID = Alloc.ROW_ID  Commented by IDC_TF  6-Aug-08 rel7
--      and   Payment.X_SIEBEL_ID = Alloc.X_SIEBEL_ID --added on 6-Aug-08 rel7 replacing the relation Payment.CG_MDF_ALLOC_ID = Alloc.ROW_ID with Payment.X_SIEBEL_ID = Alloc.X_SIEBEL_ID
--      and   Payment.PAY_STAT_CD = 'Booked'
--      and   Payment.X_SAP_DOC_TYPE = val.fi_doc_type-- Payment.X_SAP_DOC_TYPE = '2A'
--      and
--      and   Payment.PAR_PAYMENT_ID is null;
/* Added by IDC_AC on 29-Jun-2009 for Cosmos off invoice reversals to remove the DOC type hardcoding for WE S10/ CR 23 FR 27.1*/
      Select SUM(Payment.CG_TRADE_FUND_TOT) into lv_Actl_Var_Cost from S_MDF_ALLOC Alloc, S_SRC_PAYMENT Payment,
      S_LST_OF_VAL val, --H_TFM_INTERFACE_ORGANIZATIONS org,
      S_BU bu,S_LST_OF_VAL lov
      Where Alloc.Promo_id = lv_Activity_id
      ---and   Payment.CG_MDF_ALLOC_ID = Alloc.ROW_ID  Commented by IDC_TF  6-Aug-08 rel7
      and   Payment.X_SIEBEL_ID = Alloc.X_SIEBEL_ID --added on 6-Aug-08 rel7 replacing the relation Payment.CG_MDF_ALLOC_ID = Alloc.ROW_ID with Payment.X_SIEBEL_ID = Alloc.X_SIEBEL_ID
      and   Payment.PAY_STAT_CD = 'Booked'
      and   Payment.X_SAP_DOC_TYPE = val.val-- Payment.X_SAP_DOC_TYPE = '2A'
      and   RTRIM (substr( Payment.X_SIEBEL_ID,1,2)) = lov.val--org.GEOGRAPHY_ID
      and   lov.name=bu.name                    --added by IDC_KS; 18-Nov-09;
      and   lov.type='COUNTRY_CODE'             --added by IDC_KS; 18-Nov-09;

    AND  LOV.WS_MIN_VER <= 9999 AND  LOV.WS_MAX_VER > 9999 AND  LOV.WS_INACTIVE_FLG = 'N' 
    AND  LOV.WS_ID = (SELECT ROW_ID FROM S_WORKSPACE WHERE NAME = 'MAIN' and REPOSITORY_ID =(SELECT ROW_ID FROM S_REPOSITORY WHERE NAME = 'Siebel Repository'))--Added by IDC_HB for IP 18 Upgrade 2018


      --and   org.ORGANIZATION = bu.name        --commented by IDC_KS; 18-Nov-09;
      and   BU.ROW_ID = VAL.BU_ID
      and   val.type='PG_TFMIB_FI_DOC_PAY_MEANS'
      AND  VAL.WS_MIN_VER <= 9999 AND  VAL.WS_MAX_VER > 9999 AND  VAL.WS_INACTIVE_FLG = 'N' 
    AND  VAL.WS_ID = (SELECT ROW_ID FROM S_WORKSPACE WHERE NAME = 'MAIN' and REPOSITORY_ID =(SELECT ROW_ID FROM S_REPOSITORY WHERE NAME = 'Siebel Repository'))--Added by IDC_HB for IP 18 Upgrade 2018


      and   VAL.X_FILTER_VAL in ('Off Invoice','On Invoice')
      and   Payment.PAR_PAYMENT_ID is null;

        lv_Actl_Fixed_Cost := lv_Booked_Total;
        -- Update Costs (Fixed, Variable) for the activity:

        Update S_SRC set ACTL_FIXED_COST = ROUND_DISPLAY(round(NVL(lv_Actl_Fixed_Cost,0),3)), X_ACTL_VAR_COST = ROUND_DISPLAY(round(NVL(lv_Actl_Var_Cost,0),3)),
        LAST_UPD = SYS_EXTRACT_UTC(SYSTIMESTAMP) -- Updated by IDC_AM for P#2163 to update the last_upd to UTC time
        Where row_id = lv_Activity_id;


        -- Cannot Commit in Triggers
        -- Commit;

        -- Get next activity record
        Fetch cACTIVITY_LIST into lv_Activity_id, lv_Fixed_Cost_Type, lv_Activity_type, lv_Pricing_Level,lv_src_cd,lv_cost_indicator,lv_forecast_model,lv_anaplan_id; --IDC_SC Anaplan Added by IDC _SS on 25-Jan-10   WET97
    End Loop;
    Close cACTIVITY_LIST;


  --  Commented by IDC_AC on 14-Jul-2008 as first we should multiply "Inc GIV"  and TT% and then we need to roll up
    -- Added by IDC_TF;09-Jul-2008;Rel 7;For incremental Net real calculation.
--
--             SELECT /*+ ORDERED */ ROUND(SUM(NVL(
--                                 item.DFLT_COST_UNIT
--                                 * DECODE(promo_cat.X_INCREMENTAL_MODEL,
--                                        'Absolute', NVL (basln.cg_incr_sales, 0),
--                                        'Percent', (NVL(basln.prd_bsln_pct
--                                                    * DECODE(SIGN (baseline.planned_sales),-1, 0,baseline.planned_sales)/ 100,0)
--                                                    * NVL (basln.X_INCREMENTAL_PCT / 100, 0)
--                                                    )--End of NVL
--                                       )  -- end of + Decode
--                                       / prod.x_g_sufctor --suf factor
--                                 ,0)--End of NVL
--                                 )--End of sum
--                             ,7) -- End of Round (7 decimal places)
--                             ,SUM(1 - (NVL(baseline.X_TT_PERCENT, 0)/100))
--              INTO   lv_incr_giv
--                     ,lv_TT_Percent
--              FROM   S_SRC promo,
--                       S_PROD_INT prod,
--                       S_SRC promo_cat,
--                       S_SRC promo_prod,
--                       S_SRC_PRD_BASLN basln,
--                       S_PRI_LST_ITEM item,
--                       S_PROD_BASELINE baseline
--                Where promo.row_id=lv_promo_id
--                    and promo_cat.sub_type = 'PLAN_ACCT_PROMOTION_CATEGORY'
--                    and promo_cat.par_src_id=promo.row_id
--                    and promo_prod.par_src_id = promo_cat.row_id
--                    and prod.row_id = promo_prod.prod_id
--                    and promo_prod.row_id = basln.src_id
--                    and basln.prod_baseline_id = baseline.row_id
--                    AND ITEM.X_PERIOD_ID(+) = BASLN.X_PERIOD_ID
--                    AND ITEM.PROD_ID(+) = BASLN.X_PROD_ID
--                    AND ITEM.PRI_LST_ID(+) = BASLN.X_PRI_LST_ID;

  --  Added by IDC_AC on 14-Jul-2008 as first we should multiply "Inc GIV" and TT% and then we need to roll up      -- Added by IDC_TF;09-Jul-2008;Rel 7;For incremental Net real calculation.
SELECT  ROUND (SUM(decode(lv_anaplan_id,null,NVL(
                                 item.DFLT_COST_UNIT
                                 * ROUND(DECODE(promo_cat.X_INCREMENTAL_MODEL,
                                        'Absolute', NVL (basln.cg_incr_sales, 0),
                                        'Percent', (NVL(basln.prd_bsln_pct
                                                    * DECODE(SIGN (baseline.planned_sales),-1, 0,baseline.planned_sales)/ 100,0)
                                                    * NVL (basln.X_INCREMENTAL_PCT / 100, 0)
                                                    )--End of NVL
                                       )  -- end of + Decode
                                       / PROD.X_G_SUFCTOR )--suf factor     -- Added Round Function by IDC_KG for defect 3246 on 03/03/2011
                                 ,0),basln.X_ANA_INC_GIV) * (1 - (NVL(baseline.X_TT_PERCENT, 0)/100))),2)--IDC_SC Anaplan
              INTO  lv_fcst_incr
              FROM   S_SRC promo,
                       S_PROD_INT prod,
                       S_SRC promo_cat,
                       S_SRC promo_prod,
                       S_SRC_PRD_BASLN basln,
                       S_PRI_LST_ITEM item,
                       S_PROD_BASELINE baseline
                Where promo.row_id=lv_promo_id
                    and promo_cat.sub_type = 'PLAN_ACCT_PROMOTION_CATEGORY'
                    and promo_cat.par_src_id=promo.row_id
                    and promo_prod.par_src_id = promo_cat.row_id
                    and prod.row_id = promo_prod.prod_id
                    and prod.x_g_sufctor > 0 --  Added by IDC_TF 24-Feb-2009,Rel8 ,QC625, To exclude records that have ZERO suffctor
                    and promo_prod.row_id = basln.src_id
                    and basln.prod_baseline_id = baseline.row_id
                    AND ITEM.X_PERIOD_ID(+) = BASLN.X_PERIOD_ID
                    AND ITEM.PROD_ID(+) = BASLN.X_PROD_ID
                    AND ITEM.PRI_LST_ID(+) = BASLN.X_PRI_LST_ID;


                    ---Added by IDC_TF;09-Jul-2008;Rel 7;Net incremental real calculation.
               --  lv_fcst_incr:=(lv_Incr_GIV*lv_TT_percent);  --  Commented by IDC_AC on 14-Jul-2008 as first we should multiply "Inc GIV" and TT% and then we need to roll up --Added by IDC_VH;09-Jul-2008;Rel 7;Calculate the X_FCST_INCR_NR amt using this calculation


                -- UPDATE S_SRC SET X_FCST_INCR_NR =lv_fcst_incr commented by IDC_TF on 08-Aug-08,included col update at the end
                 -- WHERE ROW_ID=lv_promo_id;
                  -- Cannot commit in triggers Commented next line by Santhosh on 07/10/08.
                  --commit;

  -- Now Estimated Net Incrmental Net Real:  This will require adding up the incremental from SVP.
  -- Note, we need the following values: X_PERCENTAGE_CANNIBALIZATION (at the promotion header),
  -- X_INCREMENTAL_MODEL (at the Category level), X_GIV_CONV_FACTOR (at the promoted product level),
  -- and either the S_SRC_PRD_BASLN.CG_INCR_SALES or the X_INCREMENTAL_PCT field depending on the
  -- incremental model ('Absolute' or 'Percent').  If it's a percent, then we need to also get the
  -- baseline values from S_PROD_BASELINE.PLANNED_SALES. We will traverse the tree from Promo to
  -- category to promoted product, to promotion baseline to baseline.  We can also pull base in SU
  -- from this query:

  Select SUM(decode(lv_anaplan_id,null,(DECODE(promo_cat.X_INCREMENTAL_MODEL,
                   'Absolute', NVL(basln.cg_incr_sales,0),
                   'Percent', (NVL(basln.prd_bsln_pct* DECODE(SIGN (baseline.planned_sales),-1, 0,baseline.planned_sales)/ 100,0)
                               * NVL (basln.X_INCREMENTAL_PCT / 100, 0)))
             --* NVL(promo_prod.X_GIV_CONV_FACTOR,0 ) Added by IDC_VH;08-JULY-2008;GIV replaced by NIV
             ----*DECODE(baseline.X_NIVSU_CONV,NULL,1,0,1,baseline.X_NIVSU_CONV)--Added by IDC_VH;08-JULY-2008;GIV replaced by NIV
             ----Above line Commented by IDC_TF;Rel 7; QC223; 17-july-2008 NIVSU factor will be zero for null and '0' values.
             * NVL(baseline.X_NIVSU_CONV,0)),basln.X_ANA_INC_NIV)--IDC_SC Anaplan NIV/SU Conversion factor added by IDC_TF;Rel 7; QC223; 17-july-2008 NIVSU factor will be zero for null and '0' values.
             * (1 - NVL(promo.X_PERCENTAGE_CANNIBALIZATION, 0)/100)),
         --SUM(NVL(basln.prd_bsln_pct* DECODE(SIGN (baseline.planned_sales),-1, 0,baseline.planned_sales)/ 100,0)   --Commented by IDC_AS on 19th Oct 2010 for PR 80151
           ROUND(SUM(NVL( basln.prd_bsln_pct* DECODE(SIGN (baseline.planned_sales),-1, 0,baseline.planned_sales)/ 100,0)),2 )  --Modified by IDC_AS on 19th Oct 2010 for PR 80151
   into lv_Fcst_Incr_NOS, lv_Base_SU
  FROM   S_SRC promo,
           S_SRC promo_prod,
           S_SRC promo_cat,
           S_SRC_PRD_BASLN basln,
           S_PROD_BASELINE baseline
    Where promo_prod.par_src_id = promo_cat.row_id
      and promo_cat.par_src_id = promo.row_id
        and promo_prod.row_id = basln.src_id
        and basln.prod_baseline_id = baseline.row_id
        and promo_cat.sub_type = 'PLAN_ACCT_PROMOTION_CATEGORY'
        and promo.row_id = lv_Promo_id;

    -- and Update the promotion record (and roll up the activity costs):
    --Added by IDC_TF on 07-Aug=08 Rel7
    lv_Promo_Split_Cost := 0;
    Open cPERCENT_SPLIT;
    Fetch cPERCENT_SPLIT into lv_Split_Total, lv_Activity_id;
    While cPERCENT_SPLIT%FOUND
    LOOP
      -- And the sum of split percentages:
      -- Get the booked total:
      /* Commented by IDC_AC on 29-Jun-2009 for Cosmos off invoice reversals to remove the DOC type hardcoding for WE S10/ CR 23 FR 27.1*/
--      Select SUM(Payment.CG_TRADE_FUND_TOT) into lv_Booked_Total from S_MDF_ALLOC Alloc, S_SRC_PAYMENT Payment
--      where Alloc.Promo_id = lv_Activity_id
--      and   Payment.CG_MDF_ALLOC_ID = Alloc.ROW_ID
--      and   Payment.PAY_STAT_CD = 'Booked'
--      and   Payment.X_SAP_DOC_TYPE <> '2A';
  /* Added by IDC_AC on 29-Jun-2009 for Cosmos off invoice reversals to remove the DOC type hardcoding for WE S10/ CR 23 FR 27.1*/
      Select SUM(Payment.CG_TRADE_FUND_TOT) into lv_Booked_Total from S_MDF_ALLOC Alloc, S_SRC_PAYMENT Payment, S_LST_OF_VAL val,
      --H_TFM_INTERFACE_ORGANIZATIONS org,
      S_BU bu,S_LST_OF_VAL lov
      where Alloc.Promo_id = lv_Activity_id
      and   Payment.CG_MDF_ALLOC_ID = Alloc.ROW_ID
      and   Payment.PAY_STAT_CD = 'Booked'
      and   Payment.X_SAP_DOC_TYPE = val.val
      and   RTRIM (substr( Payment.X_SIEBEL_ID,1,2)) = lov.val--org.GEOGRAPHY_ID
      and   lov.name=bu.name                    --added by IDC_KS; 18-Nov-09;
      and   lov.type='COUNTRY_CODE'             --added by IDC_KS; 18-Nov-09;
      AND  LOV.WS_MIN_VER <= 9999 AND  LOV.WS_MAX_VER > 9999 AND  LOV.WS_INACTIVE_FLG = 'N' 
    AND  LOV.WS_ID = (SELECT ROW_ID FROM S_WORKSPACE WHERE NAME = 'MAIN' and REPOSITORY_ID =(SELECT ROW_ID FROM S_REPOSITORY WHERE NAME = 'Siebel Repository'))--Added by IDC_HB for IP 18 Upgrade 2018


      --and   org.ORGANIZATION = bu.name        --commented by IDC_KS; 18-Nov-09;
      and   BU.ROW_ID = VAL.BU_ID
      and   val.type='PG_TFMIB_FI_DOC_PAY_MEANS'
      AND  VAL.WS_MIN_VER <= 9999 AND  VAL.WS_MAX_VER > 9999 AND  VAL.WS_INACTIVE_FLG = 'N' 
     AND  VAL.WS_ID = (SELECT ROW_ID FROM S_WORKSPACE WHERE NAME = 'MAIN' and REPOSITORY_ID =(SELECT ROW_ID FROM S_REPOSITORY WHERE NAME = 'Siebel Repository'))--Added by IDC_HB for IP 18 Upgrade 2018


      and   VAL.X_FILTER_VAL not in ('Off Invoice','On Invoice');-- IDC_AC changed for defect 626, the On invoice doc type can be set up for multiple Doc types

      lv_Promo_Split_Cost := lv_Promo_Split_Cost + (lv_Booked_Total * lv_Split_Total / 100);
        Fetch cPERCENT_SPLIT into lv_Split_Total, lv_Activity_id;
    End Loop;
    Close cPERCENT_SPLIT;

    Select SUM(NVL(ACTL_FIXED_COST,0)), SUM(NVL(X_ACTL_VAR_COST,0))
    into lv_Actl_Fixed_Cost, lv_Actl_Var_Cost
    from S_SRC
    where PAR_SRC_ID = lv_Promo_id;

  Update S_SRC set ACTL_FIXED_COST = ROUND_DISPLAY(round(NVL(lv_Actl_Fixed_Cost,0) + NVL(lv_Promo_Split_Cost, 0),3)),
                   X_FCST_INCR_NR =ROUND_DISPLAY(round(lv_fcst_incr,3)), --added by IDC_TF on 08-Aug-08, updating col for Net incremental real calculation.
                   X_ACTL_VAR_COST = ROUND_DISPLAY(round(NVL(lv_Actl_Var_Cost,0),3)),
                   X_FCST_INCR_NOS = ROUND_DISPLAY(round(NVL(lv_Fcst_Incr_NOS,0),3)),
                   X_FCST_BASE_VOLUME_SU = ROUND_DISPLAY(round(nvl(Lv_Base_SU,0),3)),
                   X_INTERFACED_DT = sysdate,
                   X_EST_TOTAL_COST =ROUND_DISPLAY(ROUND(NVL(lv_Actl_Fixed_Cost,0) + NVL(lv_Actl_Var_Cost,0) + NVL(lv_Promo_Split_Cost, 0),2)),  --Modified by IDC_AS on 19th Oct 2010 for PR 80151 -- Added by IDC_VH;Rel 7;20-June-2008;U158 (Sum of Fixed + Percent Split Cost + Variable Cost).
                   LAST_UPD = SYS_EXTRACT_UTC(SYSTIMESTAMP) -- Updated by IDC_AM for P#2163 to update the last_upd to UTC time
  Where ROW_ID = lv_Promo_id;




    -- Also have to calculate the planned amount and committed amount and store it in the S_MDF record.
    -- Note that an account fund (S_MDF) record can actually be associated to multiple promotions.  Each
    -- activity record can be linked to multiple S_MDF_ALLOC records, and each S_MDF_ALLOC record is
    -- linked to ONE S_MDF fund record.

    -- Formulas: Planned Amount: (S_MDF.X_PLANNED_AMT):  SUM(S_MDF_ALLOC.EST_MDF_AMT) if the status
    -- of the promotion in question (S_SRC.STATUS_CD) = 'Planned' and the Approver status (S_SRC.X_APPR_STATUS_CD)
    -- does NOT equal 'Rejected'

    -- Committed Amount (S_MDF.X_COMMITTED_AMT): SUM(S_MDF_ALLOC.EST_MDF_AMT) if the status is (Confirmed' or 'Completed')
    -- for the promotion in question, and the corresponding activity.X_STOPPED_DT is not null and < today and
    -- the activity X_PAY_STOP_ACTIVITY_FLG <> 'Y'.

  Update S_MDF set X_COMMITTED_AMT = (Select ROUND_DISPLAY(ROUND(SUM(greatest(NVL(ALLOC.EST_MDF_AMT,0) - (select NVL(SUM(NVL(SSP.CG_TRADE_FUND_TOT,0)),0)
                                                                  FROM S_SRC_PAYMENT SSP WHERE SSP.CG_MDF_ALLOC_ID(+) = ALLOC.ROW_ID
                                                                                           and SSP.PAY_STAT_CD(+) = 'Booked'),0)) , 2))  --Modified by IDC_AS on 19th Oct 2010 for PR 80151
                                    from S_MDF_ALLOC alloc, S_SRC promo, S_SRC activity
                                    where alloc.MDF_ID = S_MDF.ROW_ID and alloc.promo_id = activity.row_id
                                      and activity.par_src_id = promo.row_id
                                      and promo.status_cd in ('Confirmed')  -- added by IDC_SS R11 GLobal_R11_Actuals on 08 Jul 10
                                      and (activity.X_STOPPED_DT is null or activity.X_STOPPED_DT > sysdate
                                           or NVL(activity.X_PAY_STOP_ACTIVITY_FLG,'Nothing') = 'Y')),
                     X_PLANNED_AMT = (Select ROUND_DISPLAY(ROUND( SUM(greatest(NVL(ALLOC.EST_MDF_AMT,0) - (select NVL(SUM(NVL(SSP.CG_TRADE_FUND_TOT,0)),0)
                                                                  from S_SRC_PAYMENT SSP where SSP.CG_MDF_ALLOC_ID(+) = alloc.row_id
                                                                                           and SSP.PAY_STAT_CD(+) = 'Booked'),0)),2))  --Modified by IDC_AS on 19th Oct 2010 for PR 80151 --- added by IDC_AC on 06-oct-2009 for SP115 R9-- Booked amount should be deducted from Planned amount.
                                      from S_MDF_ALLOC alloc, S_SRC activity, S_SRC promo
                                        where alloc.MDF_ID = S_MDF.ROW_ID and alloc.promo_id = activity.row_id
                                        and activity.par_src_id = promo.row_id and promo.status_cd in ('Planned','Revised') --added by IDC_SS on 05-feb-10 for WET97
                                        and NVL(promo.X_APPR_STATUS_CD,'Nothing') <> 'Rejected'
                                        and (activity.X_STOPPED_DT is null or activity.X_STOPPED_DT > sysdate
                                              or NVL(activity.X_PAY_STOP_ACTIVITY_FLG,'Nothing') = 'Y'))  ,--Added by IDC_AS on 29th Apr 2010 for Defect 2918
                    LAST_UPD = SYS_EXTRACT_UTC(SYSTIMESTAMP) -- Updated by IDC_AM for P#2163 to update the last_upd to UTC time
    where S_MDF.ROW_ID in
    (Select distinct MDF_ID from S_SRC Activity, S_MDF_ALLOC Alloc
     where Alloc.Promo_id = Activity.ROW_ID
     and   Activity.PAR_SRC_ID = lv_Promo_id);

End P_CALCULATE_PROMO_COST;
/


GRANT EXECUTE, DEBUG ON P_CALCULATE_PROMO_COST TO BPT_USER
/

GRANT EXECUTE, DEBUG ON P_CALCULATE_PROMO_COST TO EIMUSER
/

GRANT EXECUTE, DEBUG ON P_CALCULATE_PROMO_COST TO SEBLIOA
/

GRANT EXECUTE, DEBUG ON P_CALCULATE_PROMO_COST TO SIEBEL_SUPPORT_ROLE
/

GRANT EXECUTE, DEBUG ON P_CALCULATE_PROMO_COST TO SSE_ROLE
/
