*&---------------------------------------------------------------------*
*& Report ZSCM_MONTHLY_TT_DISTANCE
*&---------------------------------------------------------------------*
*& Description: Calculate cumulative monthly distance travelled by
*&              each Transport Truck based on TD shipment data
*& Author:      SCM Development Team
*& Date:        2026-01-02
*& Version:     2.1
*& Change Log:  v1.0 - Initial version with ZTRSTLMNT
*&              v1.1 - Updated for TD Shipments, removed logging
*&              v1.2 - Route fetched directly from ZTRSTLMNT table
*&              v1.3 - Route from OIGSV, Distance from TVRO
*&              v1.4 - Variant name defined as constant
*&              v1.5 - Added child table ZSCM_MTH_TT_DET
*&              v1.6 - Added REST API integration
*&              v2.0 - Refactored to OOP, new shipment selection logic
*&                     (YTTSPOD->VBRP->OIGSI), Distance from OIGSS
*&              v2.1 - Parallel cursor optimization for nested loops
*&---------------------------------------------------------------------*
REPORT zscm_monthly_tt_distance.

*----------------------------------------------------------------------*
* GLOBAL TYPE DEFINITIONS
*----------------------------------------------------------------------*
* Type for POD billing documents from YTTSPOD
* SELECT fields: billno, ro_out_date, truck_no, mfrgr
TYPES: BEGIN OF gty_pod,
         billno      TYPE vbeln_vf,       "Billing Document
         ro_out_date TYPE datum,          "RO Out Date
         truck_no    TYPE ytruck_no,      "Truck Number
         mfrgr       TYPE mfrgr,          "Material Freight Group
       END OF gty_pod.

* Type for VBRP delivery references
* SELECT fields: vbeln, vgbel
TYPES: BEGIN OF gty_vbrp,
         vbeln TYPE vbeln_vf,             "Billing Document
         vgbel TYPE vgbel,                "Delivery
       END OF gty_vbrp.

* Type for LIPS delivery item (for MFRGR)
* SELECT fields: vbeln, mfrgr
TYPES: BEGIN OF gty_lips,
         vbeln TYPE vbeln_vl,             "Delivery
         mfrgr TYPE mfrgr,                "Material Freight Group
       END OF gty_lips.

* Type for OIGSI shipment-delivery assignment
* SELECT fields: tknum, vbeln
TYPES: BEGIN OF gty_oigsi,
         tknum TYPE tknum,                "Shipment Number
         vbeln TYPE vbeln_vl,             "Delivery
       END OF gty_oigsi.

* Type for OIGSS shipment stage (distance)
* SELECT fields: tknum, distz, medst
TYPES: BEGIN OF gty_oigss,
         tknum TYPE tknum,                "Shipment Number
         distz TYPE tvro_distz,           "Distance
         medst TYPE meins,                "Unit of Measure
       END OF gty_oigss.

* Type for validated shipments
TYPES: BEGIN OF gty_shipment,
         tknum       TYPE tknum,          "Shipment Number
         truck_no    TYPE ytruck_no,      "Truck Number (Vehicle)
         mfrgr       TYPE mfrgr,          "Material Freight Group
         lifnr       TYPE lifnr,          "Vendor
         ro_out_date TYPE datum,          "RO Out Date
         distance    TYPE p DECIMALS 3,   "Distance from OIGSS
         dist_unit   TYPE meins,          "Distance Unit
       END OF gty_shipment.

* Type for output records
TYPES: BEGIN OF gty_output,
         vehl_no        TYPE ytruck_no,   "Vehicle Number
         gjahr          TYPE gjahr,       "Year
         monat          TYPE monat,       "Month
         total_distance TYPE p DECIMALS 3,"Total Distance
         distance_unit  TYPE meins,       "Distance Unit
         trip_count     TYPE i,           "Number of Trips
         lifnr          TYPE lifnr,       "Primary Vendor
         status         TYPE char10,      "Status (New/Updated/Test)
         created_by     TYPE ernam,
         created_on     TYPE erdat,
         created_tm     TYPE erzet,
         changed_by     TYPE aenam,
         changed_on     TYPE aedat,
         changed_tm     TYPE aezet,
       END OF gty_output.

* Type for detail records
TYPES: BEGIN OF gty_detail,
         vehl_no       TYPE ytruck_no,    "Vehicle Number
         gjahr         TYPE gjahr,        "Year
         monat         TYPE monat,        "Month
         tknum         TYPE tknum,        "Shipment Number
         mfrgr         TYPE mfrgr,        "Material Freight Group
         lifnr         TYPE lifnr,        "Vendor
         ro_out_date   TYPE datum,        "RO Out Date
         route         TYPE route,        "Route (not used, kept for compatibility)
         distance      TYPE p DECIMALS 3, "Shipment Distance
         distance_unit TYPE meins,        "Distance Unit
       END OF gty_detail.

* Type for vendor count aggregation
TYPES: BEGIN OF gty_vendor_count,
         vehl_no TYPE ytruck_no,
         lifnr   TYPE lifnr,
         count   TYPE i,
       END OF gty_vendor_count.

* Type for API record
TYPES: BEGIN OF gty_api_record,
         vehl_no        TYPE string,
         gjahr          TYPE string,
         monat          TYPE string,
         total_distance TYPE string,
         distance_unit  TYPE string,
         trip_count     TYPE string,
         lifnr          TYPE string,
       END OF gty_api_record.

* Type for API request
TYPES: BEGIN OF gty_api_request,
         records TYPE STANDARD TABLE OF gty_api_record WITH DEFAULT KEY,
       END OF gty_api_request.

* Type for MFRGR range
TYPES: gty_mfrgr_range TYPE RANGE OF mfrgr.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS: gc_variant_name TYPE rvari_vnam VALUE 'SCM_MONTH_TT_DIST'.
CONSTANTS: gc_api_url_var  TYPE rvari_vnam VALUE 'SCM_API_URL'.
CONSTANTS: gc_api_endpoint TYPE string     VALUE '/api/truck-distance'.
CONSTANTS: gc_status_c     TYPE char1      VALUE 'C'.

*----------------------------------------------------------------------*
* TABLES DECLARATION (for selection screen)
*----------------------------------------------------------------------*
TABLES: yttspod.

*----------------------------------------------------------------------*
* GLOBAL DATA
*----------------------------------------------------------------------*
DATA: gt_output       TYPE STANDARD TABLE OF gty_output,
      gt_details      TYPE STANDARD TABLE OF gty_detail,
      gv_first_day    TYPE sy-datum,
      gv_last_day     TYPE sy-datum,
      gv_records_new  TYPE i,
      gv_records_upd  TYPE i,
      gv_records_skip TYPE i,
      gv_details_cnt  TYPE i,
      gv_api_url_base TYPE string.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  PARAMETERS:     p_monat TYPE monat OBLIGATORY.
  PARAMETERS:     p_gjahr TYPE gjahr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
  SELECT-OPTIONS: s_vehl  FOR yttspod-truck_no.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
  PARAMETERS:     p_test  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* CLASS DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
             validate_input,
             execute,
             display_output.

  PRIVATE SECTION.
    DATA: mt_mfrgr        TYPE gty_mfrgr_range,
          mt_pod          TYPE STANDARD TABLE OF gty_pod,
          mt_vbrp         TYPE STANDARD TABLE OF gty_vbrp,
          mt_lips         TYPE STANDARD TABLE OF gty_lips,
          mt_oigsi        TYPE STANDARD TABLE OF gty_oigsi,
          mt_oigss        TYPE STANDARD TABLE OF gty_oigss,
          mt_shipments    TYPE STANDARD TABLE OF gty_shipment,
          mt_vendor_count TYPE STANDARD TABLE OF gty_vendor_count,
          mv_api_url      TYPE string.

    METHODS: calculate_date_range,
             fetch_mfrgr_config,
             fetch_api_url_config,
             select_pod_records,
             get_deliveries_from_vbrp,
             get_mfrgr_from_lips,
             get_shipments_from_oigsi,
             validate_all_deliveries,
             get_distances_from_oigss,
             build_shipment_data,
             aggregate_distance,
             determine_primary_vendor,
             save_to_database,
             send_data_to_api,
             prepare_json_from_db
               IMPORTING it_data       TYPE STANDARD TABLE
               RETURNING VALUE(rv_json) TYPE string,
             call_rest_api
               IMPORTING iv_json TYPE string,
             display_alv,
             display_summary.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

*----------------------------------------------------------------------*
* CONSTRUCTOR
*----------------------------------------------------------------------*
  METHOD constructor.
    " Initialize instance
    CLEAR: mt_mfrgr, mt_pod, mt_vbrp, mt_lips, mt_oigsi, mt_oigss,
           mt_shipments, mt_vendor_count, mv_api_url.
  ENDMETHOD.

*----------------------------------------------------------------------*
* VALIDATE_INPUT
*----------------------------------------------------------------------*
  METHOD validate_input.
    " Validate month
    IF p_monat < '01' OR p_monat > '12'.
      MESSAGE e001(00) WITH 'Invalid month entered. Must be 01-12.'.
    ENDIF.

    " Validate year
    IF p_gjahr < '1900' OR p_gjahr > '2099'.
      MESSAGE e001(00) WITH 'Invalid year entered. Must be 1900-2099.'.
    ENDIF.
  ENDMETHOD.

*----------------------------------------------------------------------*
* EXECUTE - Main processing method
*----------------------------------------------------------------------*
  METHOD execute.
    " Step 1: Calculate date range for the month
    me->calculate_date_range( ).

    " Step 2: Fetch valid Material Freight Groups from config
    me->fetch_mfrgr_config( ).

    " Step 3: Fetch API URL from config
    me->fetch_api_url_config( ).

    " Step 4: Select POD records from YTTSPOD
    me->select_pod_records( ).

    " Step 5: Get deliveries from VBRP for billing documents
    me->get_deliveries_from_vbrp( ).

    " Step 5a: Get MFRGR from LIPS for deliveries
    me->get_mfrgr_from_lips( ).

    " Step 6: Get shipments from OIGSI for deliveries
    me->get_shipments_from_oigsi( ).

    " Step 7: Validate all deliveries of shipments meet conditions
    me->validate_all_deliveries( ).

    " Step 8: Get distances from OIGSS for shipments
    me->get_distances_from_oigss( ).

    " Step 9: Build shipment data combining all sources
    me->build_shipment_data( ).

    " Step 10: Aggregate distance per vehicle
    me->aggregate_distance( ).

    " Step 11: Save to database or mark as test mode
    IF p_test IS INITIAL.
      me->save_to_database( ).
      me->send_data_to_api( ).
    ELSE.
      " Mark all as Test Mode
      DATA: lw_output TYPE gty_output.
      FIELD-SYMBOLS: <lfs_output> TYPE gty_output.

      LOOP AT gt_output ASSIGNING <lfs_output>.
        <lfs_output>-status = 'Test Mode'.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

*----------------------------------------------------------------------*
* CALCULATE_DATE_RANGE
*----------------------------------------------------------------------*
  METHOD calculate_date_range.
    DATA: lv_last_day   TYPE i,
          lv_year_str   TYPE char4,
          lv_month_str  TYPE char2,
          lv_day_str    TYPE char2.

    " Calculate first day of month
    lv_year_str  = p_gjahr.
    lv_month_str = p_monat.
    CONCATENATE lv_year_str lv_month_str '01' INTO gv_first_day.

    " Calculate last day of month using function module
    CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
      EXPORTING
        iv_date           = gv_first_day
      IMPORTING
        ev_month_end_date = gv_last_day
      EXCEPTIONS
        OTHERS            = 1.

    IF sy-subrc <> 0.
      " Fallback calculation for last day
      CASE p_monat.
        WHEN '01' OR '03' OR '05' OR '07' OR '08' OR '10' OR '12'.
          lv_last_day = 31.
        WHEN '04' OR '06' OR '09' OR '11'.
          lv_last_day = 30.
        WHEN '02'.
          " Check for leap year
          IF p_gjahr MOD 4 = 0 AND ( p_gjahr MOD 100 <> 0 OR p_gjahr MOD 400 = 0 ).
            lv_last_day = 29.
          ELSE.
            lv_last_day = 28.
          ENDIF.
      ENDCASE.
      lv_day_str = lv_last_day.
      CONCATENATE lv_year_str lv_month_str lv_day_str INTO gv_last_day.
    ENDIF.
  ENDMETHOD.

*----------------------------------------------------------------------*
* FETCH_MFRGR_CONFIG
*----------------------------------------------------------------------*
  METHOD fetch_mfrgr_config.
    DATA: lt_config TYPE STANDARD TABLE OF zlog_exec_var,
          lw_config TYPE zlog_exec_var,
          lw_mfrgr  TYPE LINE OF gty_mfrgr_range.

    FIELD-SYMBOLS: <lfs_config> TYPE zlog_exec_var.

    " Select valid Material Freight Groups from config table
    SELECT mandt name numb mfrgr active
      FROM zlog_exec_var
      INTO CORRESPONDING FIELDS OF TABLE lt_config
      WHERE name   = gc_variant_name
        AND active = 'X'.

    IF sy-subrc <> 0 OR lt_config IS INITIAL.
      MESSAGE e001(00) WITH 'No active Material Freight Groups found for variant' gc_variant_name.
    ENDIF.

    " Build range table for MFRGR
    LOOP AT lt_config ASSIGNING <lfs_config>.
      lw_mfrgr-sign   = 'I'.
      lw_mfrgr-option = 'EQ'.
      lw_mfrgr-low    = <lfs_config>-mfrgr.
      APPEND lw_mfrgr TO mt_mfrgr.
    ENDLOOP.
  ENDMETHOD.

*----------------------------------------------------------------------*
* FETCH_API_URL_CONFIG
*----------------------------------------------------------------------*
  METHOD fetch_api_url_config.
    DATA: lw_config TYPE zlog_exec_var.

    " Fetch API URL from configuration table
    SELECT SINGLE mandt name remarks active
      FROM zlog_exec_var
      INTO CORRESPONDING FIELDS OF lw_config
      WHERE name   = gc_api_url_var
        AND active = 'X'.

    IF sy-subrc = 0.
      mv_api_url = lw_config-remarks.
      CONDENSE mv_api_url.
      gv_api_url_base = mv_api_url.

      IF mv_api_url IS INITIAL.
        MESSAGE s001(00) WITH 'API URL is empty. API call will be skipped.' DISPLAY LIKE 'W'.
      ENDIF.
    ELSE.
      CLEAR: mv_api_url, gv_api_url_base.
      MESSAGE s001(00) WITH 'API URL not configured. API call will be skipped.' DISPLAY LIKE 'I'.
    ENDIF.
  ENDMETHOD.

*----------------------------------------------------------------------*
* SELECT_POD_RECORDS
*----------------------------------------------------------------------*
  METHOD select_pod_records.
    " Select POD records from YTTSPOD where RO_OUT_DATE is in month
    " and STATUS = 'C'
    SELECT billno ro_out_date truck_no mfrgr
      FROM yttspod
      INTO TABLE mt_pod
      WHERE ro_out_date >= gv_first_day
        AND ro_out_date <= gv_last_day
        AND status      =  gc_status_c
        AND truck_no    IN s_vehl
        AND truck_no    <> space.

    IF sy-subrc <> 0 OR mt_pod IS INITIAL.
      MESSAGE s001(00) WITH 'No POD records found for the selection criteria' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.
  ENDMETHOD.

*----------------------------------------------------------------------*
* GET_DELIVERIES_FROM_VBRP
*----------------------------------------------------------------------*
  METHOD get_deliveries_from_vbrp.
    DATA: lt_billno TYPE STANDARD TABLE OF vbeln_vf,
          lw_billno TYPE vbeln_vf.

    FIELD-SYMBOLS: <lfs_pod>  TYPE gty_pod,
                   <lfs_vbrp> TYPE gty_vbrp.

    CHECK mt_pod IS NOT INITIAL.

    " Collect unique billing document numbers
    LOOP AT mt_pod ASSIGNING <lfs_pod>.
      lw_billno = <lfs_pod>-billno.
      APPEND lw_billno TO lt_billno.
    ENDLOOP.

    SORT lt_billno.
    DELETE ADJACENT DUPLICATES FROM lt_billno.

    " Get deliveries from VBRP for these billing documents
    " Note: MFRGR is retrieved from LIPS in the next step
    IF lt_billno IS NOT INITIAL.
      SELECT vbeln vgbel
        FROM vbrp
        INTO TABLE mt_vbrp
        FOR ALL ENTRIES IN lt_billno
        WHERE vbeln = lt_billno-table_line
          AND vgbel <> space.
    ENDIF.

    IF mt_vbrp IS INITIAL.
      MESSAGE s001(00) WITH 'No deliveries found in VBRP' DISPLAY LIKE 'W'.
    ENDIF.
  ENDMETHOD.

*----------------------------------------------------------------------*
* GET_MFRGR_FROM_LIPS
*----------------------------------------------------------------------*
*& Get Material Freight Groups from LIPS (Delivery Item) and filter
*& by configuration in ZLOG_EXEC_VAR
*----------------------------------------------------------------------*
  METHOD get_mfrgr_from_lips.
    DATA: lt_vbeln        TYPE STANDARD TABLE OF vbeln_vl,
          lt_lips_all     TYPE STANDARD TABLE OF gty_lips,
          lt_vbrp_filtered TYPE STANDARD TABLE OF gty_vbrp,
          lw_vbeln        TYPE vbeln_vl.

    FIELD-SYMBOLS: <lfs_vbrp>     TYPE gty_vbrp,
                   <lfs_lips>     TYPE gty_lips,
                   <lfs_lips_all> TYPE gty_lips.

    CHECK mt_vbrp IS NOT INITIAL.

    " Collect unique delivery numbers
    LOOP AT mt_vbrp ASSIGNING <lfs_vbrp>.
      lw_vbeln = <lfs_vbrp>-vgbel.
      APPEND lw_vbeln TO lt_vbeln.
    ENDLOOP.

    SORT lt_vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_vbeln.

    " Get MFRGR from LIPS for these deliveries
    " Filter by MFRGR matching configuration
    IF lt_vbeln IS NOT INITIAL.
      SELECT vbeln mfrgr
        FROM lips
        INTO TABLE lt_lips_all
        FOR ALL ENTRIES IN lt_vbeln
        WHERE vbeln = lt_vbeln-table_line
          AND mfrgr IN mt_mfrgr.
    ENDIF.

    IF lt_lips_all IS INITIAL.
      MESSAGE s001(00) WITH 'No matching MFRGR found in deliveries' DISPLAY LIKE 'W'.
      CLEAR mt_vbrp.
      RETURN.
    ENDIF.

    " Keep unique delivery-MFRGR combinations
    SORT lt_lips_all BY vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_lips_all COMPARING vbeln.

    " Store in mt_lips for later use
    mt_lips = lt_lips_all.

    " Filter mt_vbrp to keep only deliveries with matching MFRGR
    SORT mt_lips BY vbeln.

    LOOP AT mt_vbrp ASSIGNING <lfs_vbrp>.
      READ TABLE mt_lips
        WITH KEY vbeln = <lfs_vbrp>-vgbel
        BINARY SEARCH
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        APPEND <lfs_vbrp> TO lt_vbrp_filtered.
      ENDIF.
    ENDLOOP.

    mt_vbrp = lt_vbrp_filtered.

    IF mt_vbrp IS INITIAL.
      MESSAGE s001(00) WITH 'No deliveries with matching MFRGR' DISPLAY LIKE 'W'.
    ENDIF.
  ENDMETHOD.

*----------------------------------------------------------------------*
* GET_SHIPMENTS_FROM_OIGSI
*----------------------------------------------------------------------*
  METHOD get_shipments_from_oigsi.
    DATA: lt_vbeln TYPE STANDARD TABLE OF vbeln_vl,
          lw_vbeln TYPE vbeln_vl.

    FIELD-SYMBOLS: <lfs_vbrp> TYPE gty_vbrp.

    CHECK mt_vbrp IS NOT INITIAL.

    " Collect unique delivery numbers
    LOOP AT mt_vbrp ASSIGNING <lfs_vbrp>.
      lw_vbeln = <lfs_vbrp>-vgbel.
      APPEND lw_vbeln TO lt_vbeln.
    ENDLOOP.

    SORT lt_vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_vbeln.

    " Get shipments from OIGSI for these deliveries
    IF lt_vbeln IS NOT INITIAL.
      SELECT tknum vbeln
        FROM oigsi
        INTO TABLE mt_oigsi
        FOR ALL ENTRIES IN lt_vbeln
        WHERE vbeln = lt_vbeln-table_line
          AND tknum <> space.
    ENDIF.

    IF mt_oigsi IS INITIAL.
      MESSAGE s001(00) WITH 'No shipments found in OIGSI' DISPLAY LIKE 'W'.
    ENDIF.
  ENDMETHOD.

*----------------------------------------------------------------------*
* VALIDATE_ALL_DELIVERIES
*----------------------------------------------------------------------*
  METHOD validate_all_deliveries.
    " For each shipment, get all its deliveries and verify all meet
    " the condition: RO_OUT_DATE in range AND STATUS = 'C'
    DATA: lt_tknum          TYPE STANDARD TABLE OF tknum,
          lt_ship_del       TYPE STANDARD TABLE OF gty_oigsi,
          lt_all_deliveries TYPE STANDARD TABLE OF vbeln_vl,
          lt_pod_check      TYPE STANDARD TABLE OF gty_pod,
          lt_valid_tknum    TYPE STANDARD TABLE OF tknum,
          lt_invalid_tknum  TYPE STANDARD TABLE OF tknum,
          lw_tknum          TYPE tknum,
          lw_vbeln          TYPE vbeln_vl,
          lv_all_valid      TYPE abap_bool,
          lv_found          TYPE abap_bool.

    FIELD-SYMBOLS: <lfs_oigsi>    TYPE gty_oigsi,
                   <lfs_ship_del> TYPE gty_oigsi,
                   <lfs_pod>      TYPE gty_pod.

    CHECK mt_oigsi IS NOT INITIAL.

    " Get unique shipment numbers
    LOOP AT mt_oigsi ASSIGNING <lfs_oigsi>.
      lw_tknum = <lfs_oigsi>-tknum.
      APPEND lw_tknum TO lt_tknum.
    ENDLOOP.

    SORT lt_tknum.
    DELETE ADJACENT DUPLICATES FROM lt_tknum.

    " Get ALL deliveries for these shipments from OIGSI
    IF lt_tknum IS NOT INITIAL.
      SELECT tknum vbeln
        FROM oigsi
        INTO TABLE lt_ship_del
        FOR ALL ENTRIES IN lt_tknum
        WHERE tknum = lt_tknum-table_line.
    ENDIF.

    " Get unique deliveries
    LOOP AT lt_ship_del ASSIGNING <lfs_ship_del>.
      lw_vbeln = <lfs_ship_del>-vbeln.
      APPEND lw_vbeln TO lt_all_deliveries.
    ENDLOOP.

    SORT lt_all_deliveries.
    DELETE ADJACENT DUPLICATES FROM lt_all_deliveries.

    " Get billing documents for all deliveries from VBRP
    DATA: lt_bill_from_vbrp TYPE STANDARD TABLE OF vbeln_vf,
          lt_vbrp_all       TYPE STANDARD TABLE OF gty_vbrp,
          lw_billno         TYPE vbeln_vf.

    FIELD-SYMBOLS: <lfs_vbrp_all> TYPE gty_vbrp.

    IF lt_all_deliveries IS NOT INITIAL.
      SELECT vbeln vgbel
        FROM vbrp
        INTO TABLE lt_vbrp_all
        FOR ALL ENTRIES IN lt_all_deliveries
        WHERE vgbel = lt_all_deliveries-table_line.

      " Collect billing documents
      LOOP AT lt_vbrp_all ASSIGNING <lfs_vbrp_all>.
        lw_billno = <lfs_vbrp_all>-vbeln.
        APPEND lw_billno TO lt_bill_from_vbrp.
      ENDLOOP.

      SORT lt_bill_from_vbrp.
      DELETE ADJACENT DUPLICATES FROM lt_bill_from_vbrp.
    ENDIF.

    " Check POD status for all billing documents
    IF lt_bill_from_vbrp IS NOT INITIAL.
      SELECT billno ro_out_date truck_no mfrgr
        FROM yttspod
        INTO TABLE lt_pod_check
        FOR ALL ENTRIES IN lt_bill_from_vbrp
        WHERE billno = lt_bill_from_vbrp-table_line.
    ENDIF.

    " Sort for parallel cursor and binary search
    SORT lt_ship_del BY tknum vbeln.
    SORT lt_vbrp_all BY vgbel.
    SORT lt_pod_check BY billno.

    " Variables for parallel cursor
    DATA: lv_tabix TYPE sy-tabix.

    " Validate each shipment using parallel cursor technique
    LOOP AT lt_tknum INTO lw_tknum.
      lv_all_valid = abap_true.

      " Find starting position for this shipment using binary search
      READ TABLE lt_ship_del TRANSPORTING NO FIELDS
        WITH KEY tknum = lw_tknum
        BINARY SEARCH.

      IF sy-subrc = 0.
        lv_tabix = sy-tabix.

        " Loop from starting position (parallel cursor)
        LOOP AT lt_ship_del ASSIGNING <lfs_ship_del> FROM lv_tabix.
          " Exit when shipment number changes
          IF <lfs_ship_del>-tknum <> lw_tknum.
            EXIT.
          ENDIF.

          " Find billing doc for this delivery
          READ TABLE lt_vbrp_all ASSIGNING <lfs_vbrp_all>
            WITH KEY vgbel = <lfs_ship_del>-vbeln
            BINARY SEARCH.

          IF sy-subrc = 0.
            " Check POD status for billing doc
            READ TABLE lt_pod_check ASSIGNING <lfs_pod>
              WITH KEY billno = <lfs_vbrp_all>-vbeln
              BINARY SEARCH.

            IF sy-subrc = 0.
              " Check if RO_OUT_DATE is in range and conditions met
              IF <lfs_pod>-ro_out_date < gv_first_day OR
                 <lfs_pod>-ro_out_date > gv_last_day.
                lv_all_valid = abap_false.
                EXIT.
              ENDIF.
            ELSE.
              " POD record not found for this billing doc
              lv_all_valid = abap_false.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF lv_all_valid = abap_true.
        APPEND lw_tknum TO lt_valid_tknum.
      ELSE.
        APPEND lw_tknum TO lt_invalid_tknum.
        gv_records_skip = gv_records_skip + 1.
      ENDIF.
    ENDLOOP.

    " Filter mt_oigsi to keep only valid shipments
    DATA: lt_oigsi_filtered TYPE STANDARD TABLE OF gty_oigsi.

    SORT lt_valid_tknum.

    LOOP AT mt_oigsi ASSIGNING <lfs_oigsi>.
      READ TABLE lt_valid_tknum
        WITH KEY table_line = <lfs_oigsi>-tknum
        BINARY SEARCH
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        APPEND <lfs_oigsi> TO lt_oigsi_filtered.
      ENDIF.
    ENDLOOP.

    mt_oigsi = lt_oigsi_filtered.
  ENDMETHOD.

*----------------------------------------------------------------------*
* GET_DISTANCES_FROM_OIGSS
*----------------------------------------------------------------------*
  METHOD get_distances_from_oigss.
    DATA: lt_tknum      TYPE STANDARD TABLE OF tknum,
          lt_oigss_all  TYPE STANDARD TABLE OF gty_oigss,
          lw_tknum      TYPE tknum.

    FIELD-SYMBOLS: <lfs_oigsi>     TYPE gty_oigsi,
                   <lfs_oigss>     TYPE gty_oigss,
                   <lfs_oigss_all> TYPE gty_oigss.

    CHECK mt_oigsi IS NOT INITIAL.

    " Get unique shipment numbers
    LOOP AT mt_oigsi ASSIGNING <lfs_oigsi>.
      lw_tknum = <lfs_oigsi>-tknum.
      APPEND lw_tknum TO lt_tknum.
    ENDLOOP.

    SORT lt_tknum.
    DELETE ADJACENT DUPLICATES FROM lt_tknum.

    " Get distances from OIGSS for shipments
    " Get first record for each shipment (lowest sequence)
    IF lt_tknum IS NOT INITIAL.
      SELECT tknum distz medst
        FROM oigss
        INTO TABLE lt_oigss_all
        FOR ALL ENTRIES IN lt_tknum
        WHERE tknum = lt_tknum-table_line
          AND distz <> 0.
    ENDIF.

    " Sort by shipment number to get first record per shipment
    SORT lt_oigss_all BY tknum.

    " Keep only first record per shipment
    DATA: lv_prev_tknum TYPE tknum.
    CLEAR lv_prev_tknum.

    LOOP AT lt_oigss_all ASSIGNING <lfs_oigss_all>.
      IF <lfs_oigss_all>-tknum <> lv_prev_tknum.
        APPEND <lfs_oigss_all> TO mt_oigss.
        lv_prev_tknum = <lfs_oigss_all>-tknum.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

*----------------------------------------------------------------------*
* BUILD_SHIPMENT_DATA
*----------------------------------------------------------------------*
  METHOD build_shipment_data.
    DATA: lw_shipment TYPE gty_shipment,
          lw_vbrp     TYPE gty_vbrp,
          lw_pod      TYPE gty_pod.

    FIELD-SYMBOLS: <lfs_oigsi> TYPE gty_oigsi,
                   <lfs_oigss> TYPE gty_oigss,
                   <lfs_vbrp>  TYPE gty_vbrp,
                   <lfs_lips>  TYPE gty_lips,
                   <lfs_pod>   TYPE gty_pod.

    CHECK mt_oigsi IS NOT INITIAL.

    " Sort tables for binary search
    SORT mt_oigss BY tknum.
    SORT mt_vbrp BY vgbel.
    SORT mt_lips BY vbeln.
    SORT mt_pod BY billno.

    " Build shipment records combining all data
    LOOP AT mt_oigsi ASSIGNING <lfs_oigsi>.
      CLEAR lw_shipment.

      lw_shipment-tknum = <lfs_oigsi>-tknum.

      " Get distance from OIGSS
      READ TABLE mt_oigss ASSIGNING <lfs_oigss>
        WITH KEY tknum = <lfs_oigsi>-tknum
        BINARY SEARCH.

      IF sy-subrc = 0.
        lw_shipment-distance  = <lfs_oigss>-distz.
        lw_shipment-dist_unit = <lfs_oigss>-medst.
      ELSE.
        " No distance found - skip this shipment
        gv_records_skip = gv_records_skip + 1.
        CONTINUE.
      ENDIF.

      " Get delivery info from VBRP (to get billing doc)
      READ TABLE mt_vbrp ASSIGNING <lfs_vbrp>
        WITH KEY vgbel = <lfs_oigsi>-vbeln
        BINARY SEARCH.

      IF sy-subrc = 0.
        " Get MFRGR from LIPS (delivery item)
        READ TABLE mt_lips ASSIGNING <lfs_lips>
          WITH KEY vbeln = <lfs_vbrp>-vgbel
          BINARY SEARCH.

        IF sy-subrc = 0.
          lw_shipment-mfrgr = <lfs_lips>-mfrgr.
        ENDIF.

        " Get truck number and RO_OUT_DATE from POD
        READ TABLE mt_pod ASSIGNING <lfs_pod>
          WITH KEY billno = <lfs_vbrp>-vbeln
          BINARY SEARCH.

        IF sy-subrc = 0.
          lw_shipment-truck_no    = <lfs_pod>-truck_no.
          lw_shipment-ro_out_date = <lfs_pod>-ro_out_date.
        ENDIF.
      ENDIF.

      " Skip if no truck number
      IF lw_shipment-truck_no IS INITIAL.
        gv_records_skip = gv_records_skip + 1.
        CONTINUE.
      ENDIF.

      " Apply vehicle filter
      IF s_vehl IS NOT INITIAL.
        IF NOT lw_shipment-truck_no IN s_vehl.
          CONTINUE.
        ENDIF.
      ENDIF.

      APPEND lw_shipment TO mt_shipments.
    ENDLOOP.

    " Remove duplicates (same shipment may appear multiple times)
    SORT mt_shipments BY tknum.
    DELETE ADJACENT DUPLICATES FROM mt_shipments COMPARING tknum.
  ENDMETHOD.

*----------------------------------------------------------------------*
* AGGREGATE_DISTANCE
*----------------------------------------------------------------------*
  METHOD aggregate_distance.
    DATA: lt_work    TYPE STANDARD TABLE OF gty_output,
          lw_work    TYPE gty_output,
          lw_detail  TYPE gty_detail,
          lw_vc      TYPE gty_vendor_count.

    FIELD-SYMBOLS: <lfs_ship>   TYPE gty_shipment,
                   <lfs_work>   TYPE gty_output,
                   <lfs_vc>     TYPE gty_vendor_count.

    CHECK mt_shipments IS NOT INITIAL.

    " Clear output tables
    CLEAR: gt_output, gt_details, mt_vendor_count.

    " Process each shipment
    LOOP AT mt_shipments ASSIGNING <lfs_ship>.

      " Build detail record
      CLEAR lw_detail.
      lw_detail-vehl_no       = <lfs_ship>-truck_no.
      lw_detail-gjahr         = p_gjahr.
      lw_detail-monat         = p_monat.
      lw_detail-tknum         = <lfs_ship>-tknum.
      lw_detail-mfrgr         = <lfs_ship>-mfrgr.
      lw_detail-lifnr         = <lfs_ship>-lifnr.
      lw_detail-ro_out_date   = <lfs_ship>-ro_out_date.
      lw_detail-distance      = <lfs_ship>-distance.
      lw_detail-distance_unit = <lfs_ship>-dist_unit.
      APPEND lw_detail TO gt_details.

      " Check if vehicle already exists in work table
      READ TABLE lt_work ASSIGNING <lfs_work>
        WITH KEY vehl_no = <lfs_ship>-truck_no.

      IF sy-subrc = 0.
        " Update existing vehicle record
        <lfs_work>-total_distance = <lfs_work>-total_distance + <lfs_ship>-distance.
        <lfs_work>-trip_count     = <lfs_work>-trip_count + 1.
      ELSE.
        " Create new vehicle record
        CLEAR lw_work.
        lw_work-vehl_no        = <lfs_ship>-truck_no.
        lw_work-gjahr          = p_gjahr.
        lw_work-monat          = p_monat.
        lw_work-total_distance = <lfs_ship>-distance.
        lw_work-distance_unit  = <lfs_ship>-dist_unit.
        lw_work-trip_count     = 1.
        lw_work-lifnr          = <lfs_ship>-lifnr.
        APPEND lw_work TO lt_work.
      ENDIF.

      " Track vendor counts
      CLEAR lw_vc.
      lw_vc-vehl_no = <lfs_ship>-truck_no.
      lw_vc-lifnr   = <lfs_ship>-lifnr.
      lw_vc-count   = 1.
      APPEND lw_vc TO mt_vendor_count.
    ENDLOOP.

    " Determine primary vendor for each vehicle
    me->determine_primary_vendor( ).

    " Update vendor in work table
    DATA: lt_vendor_agg TYPE STANDARD TABLE OF gty_vendor_count.

    FIELD-SYMBOLS: <lfs_agg> TYPE gty_vendor_count.

    " Aggregate vendor counts
    SORT mt_vendor_count BY vehl_no lifnr.

    LOOP AT mt_vendor_count ASSIGNING <lfs_vc>.
      READ TABLE lt_vendor_agg ASSIGNING <lfs_agg>
        WITH KEY vehl_no = <lfs_vc>-vehl_no
                 lifnr   = <lfs_vc>-lifnr.
      IF sy-subrc = 0.
        <lfs_agg>-count = <lfs_agg>-count + 1.
      ELSE.
        lw_vc-vehl_no = <lfs_vc>-vehl_no.
        lw_vc-lifnr   = <lfs_vc>-lifnr.
        lw_vc-count   = 1.
        APPEND lw_vc TO lt_vendor_agg.
      ENDIF.
    ENDLOOP.

    " Sort by count descending to get primary vendor
    SORT lt_vendor_agg BY vehl_no count DESCENDING.

    " Update work table with primary vendor
    LOOP AT lt_work ASSIGNING <lfs_work>.
      READ TABLE lt_vendor_agg ASSIGNING <lfs_agg>
        WITH KEY vehl_no = <lfs_work>-vehl_no.
      IF sy-subrc = 0.
        <lfs_work>-lifnr = <lfs_agg>-lifnr.
      ENDIF.
    ENDLOOP.

    " Move to global output table
    gt_output = lt_work.
    gv_details_cnt = lines( gt_details ).
  ENDMETHOD.

*----------------------------------------------------------------------*
* DETERMINE_PRIMARY_VENDOR
*----------------------------------------------------------------------*
  METHOD determine_primary_vendor.
    " This method is a placeholder - logic integrated in aggregate_distance
  ENDMETHOD.

*----------------------------------------------------------------------*
* SAVE_TO_DATABASE
*----------------------------------------------------------------------*
  METHOD save_to_database.
    DATA: lw_db     TYPE zscm_mth_tt_dist,
          lt_db     TYPE STANDARD TABLE OF zscm_mth_tt_dist,
          lt_exist  TYPE STANDARD TABLE OF zscm_mth_tt_dist,
          lw_det    TYPE zscm_mth_tt_det,
          lt_det    TYPE STANDARD TABLE OF zscm_mth_tt_det.

    FIELD-SYMBOLS: <lfs_out>   TYPE gty_output,
                   <lfs_exist> TYPE zscm_mth_tt_dist,
                   <lfs_det>   TYPE gty_detail.

    CHECK gt_output IS NOT INITIAL.

    " Get existing header records for the month
    SELECT mandt vehl_no gjahr monat total_distance distance_unit
           trip_count lifnr created_by created_on created_tm
           changed_by changed_on changed_tm
      FROM zscm_mth_tt_dist
      INTO TABLE lt_exist
      WHERE gjahr = p_gjahr
        AND monat = p_monat.

    SORT lt_exist BY vehl_no gjahr monat.

    " Build header database table
    LOOP AT gt_output ASSIGNING <lfs_out>.
      CLEAR lw_db.

      " Check if record exists
      READ TABLE lt_exist ASSIGNING <lfs_exist>
        WITH KEY vehl_no = <lfs_out>-vehl_no
                 gjahr   = <lfs_out>-gjahr
                 monat   = <lfs_out>-monat
        BINARY SEARCH.

      IF sy-subrc = 0.
        " Update existing record
        lw_db-mandt          = sy-mandt.
        lw_db-vehl_no        = <lfs_out>-vehl_no.
        lw_db-gjahr          = <lfs_out>-gjahr.
        lw_db-monat          = <lfs_out>-monat.
        lw_db-total_distance = <lfs_out>-total_distance.
        lw_db-distance_unit  = <lfs_out>-distance_unit.
        lw_db-trip_count     = <lfs_out>-trip_count.
        lw_db-lifnr          = <lfs_out>-lifnr.
        lw_db-created_by     = <lfs_exist>-created_by.
        lw_db-created_on     = <lfs_exist>-created_on.
        lw_db-created_tm     = <lfs_exist>-created_tm.
        lw_db-changed_by     = sy-uname.
        lw_db-changed_on     = sy-datum.
        lw_db-changed_tm     = sy-uzeit.
        <lfs_out>-status     = 'Updated'.
        gv_records_upd       = gv_records_upd + 1.
      ELSE.
        " New record
        lw_db-mandt          = sy-mandt.
        lw_db-vehl_no        = <lfs_out>-vehl_no.
        lw_db-gjahr          = <lfs_out>-gjahr.
        lw_db-monat          = <lfs_out>-monat.
        lw_db-total_distance = <lfs_out>-total_distance.
        lw_db-distance_unit  = <lfs_out>-distance_unit.
        lw_db-trip_count     = <lfs_out>-trip_count.
        lw_db-lifnr          = <lfs_out>-lifnr.
        lw_db-created_by     = sy-uname.
        lw_db-created_on     = sy-datum.
        lw_db-created_tm     = sy-uzeit.
        <lfs_out>-status     = 'New'.
        gv_records_new       = gv_records_new + 1.
      ENDIF.

      APPEND lw_db TO lt_db.
    ENDLOOP.

    " Build detail database table
    LOOP AT gt_details ASSIGNING <lfs_det>.
      CLEAR lw_det.
      lw_det-mandt         = sy-mandt.
      lw_det-vehl_no       = <lfs_det>-vehl_no.
      lw_det-gjahr         = <lfs_det>-gjahr.
      lw_det-monat         = <lfs_det>-monat.
      lw_det-tknum         = <lfs_det>-tknum.
      lw_det-mfrgr         = <lfs_det>-mfrgr.
      lw_det-lifnr         = <lfs_det>-lifnr.
      lw_det-ro_out_date   = <lfs_det>-ro_out_date.
      lw_det-route         = <lfs_det>-route.
      lw_det-distance      = <lfs_det>-distance.
      lw_det-distance_unit = <lfs_det>-distance_unit.
      lw_det-created_by    = sy-uname.
      lw_det-created_on    = sy-datum.
      lw_det-created_tm    = sy-uzeit.
      APPEND lw_det TO lt_det.
    ENDLOOP.

    " Delete existing detail records for the month
    DELETE FROM zscm_mth_tt_det
      WHERE gjahr = p_gjahr
        AND monat = p_monat.

    " Modify header table (insert or update)
    MODIFY zscm_mth_tt_dist FROM TABLE lt_db.

    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE e001(00) WITH 'Header table update failed'.
    ENDIF.

    " Insert detail records
    IF lt_det IS NOT INITIAL.
      INSERT zscm_mth_tt_det FROM TABLE lt_det.

      IF sy-subrc <> 0.
        ROLLBACK WORK.
        MESSAGE e001(00) WITH 'Detail table insert failed'.
      ENDIF.
    ENDIF.

    " Commit all changes
    COMMIT WORK AND WAIT.
  ENDMETHOD.

*----------------------------------------------------------------------*
* SEND_DATA_TO_API
*----------------------------------------------------------------------*
  METHOD send_data_to_api.
    DATA: lt_api_data TYPE STANDARD TABLE OF zscm_mth_tt_dist,
          lv_json     TYPE string.

    " Skip if API URL is not configured
    IF mv_api_url IS INITIAL.
      MESSAGE s001(00) WITH 'API URL not configured. Skipping API call.' DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.

    " Read data from database for the processed month
    SELECT mandt vehl_no gjahr monat total_distance distance_unit
           trip_count lifnr created_by created_on created_tm
           changed_by changed_on changed_tm
      FROM zscm_mth_tt_dist
      INTO TABLE lt_api_data
      WHERE gjahr = p_gjahr
        AND monat = p_monat.

    IF lt_api_data IS INITIAL.
      MESSAGE s001(00) WITH 'No data found to send to API.' DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.

    " Prepare JSON from database records
    lv_json = me->prepare_json_from_db( lt_api_data ).

    IF lv_json IS INITIAL.
      MESSAGE e001(00) WITH 'Failed to prepare JSON data'.
    ENDIF.

    " Call REST API
    me->call_rest_api( lv_json ).
  ENDMETHOD.

*----------------------------------------------------------------------*
* PREPARE_JSON_FROM_DB
*----------------------------------------------------------------------*
  METHOD prepare_json_from_db.
    DATA: lw_record     TYPE zscm_mth_tt_dist,
          lt_records    TYPE STANDARD TABLE OF gty_api_record,
          lw_api_rec    TYPE gty_api_record,
          lw_api_req    TYPE gty_api_request,
          lv_distance   TYPE string,
          lv_trip_cnt   TYPE string,
          lv_year       TYPE char4,
          lv_month      TYPE char2,
          lo_error      TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lfs_record> TYPE zscm_mth_tt_dist.

    CLEAR rv_json.

    " Build API record structure from database
    LOOP AT it_data ASSIGNING <lfs_record>.
      CLEAR lw_api_rec.

      " Convert fields to string format
      lw_api_rec-vehl_no = <lfs_record>-vehl_no.

      lv_year = <lfs_record>-gjahr.
      lw_api_rec-gjahr = lv_year.

      lv_month = <lfs_record>-monat.
      lw_api_rec-monat = lv_month.

      " Format distance
      WRITE <lfs_record>-total_distance TO lv_distance DECIMALS 3.
      CONDENSE lv_distance NO-GAPS.
      lw_api_rec-total_distance = lv_distance.

      " Distance unit
      IF <lfs_record>-distance_unit IS NOT INITIAL.
        lw_api_rec-distance_unit = <lfs_record>-distance_unit.
      ELSE.
        lw_api_rec-distance_unit = 'KM'.
      ENDIF.

      " Trip count
      WRITE <lfs_record>-trip_count TO lv_trip_cnt.
      CONDENSE lv_trip_cnt NO-GAPS.
      lw_api_rec-trip_count = lv_trip_cnt.

      " Vendor
      IF <lfs_record>-lifnr IS NOT INITIAL.
        lw_api_rec-lifnr = <lfs_record>-lifnr.
      ENDIF.

      APPEND lw_api_rec TO lt_records.
    ENDLOOP.

    " Convert to JSON using /UI2/CL_JSON
    TRY.
        lw_api_req-records = lt_records.

        CALL METHOD /ui2/cl_json=>serialize
          EXPORTING
            data        = lw_api_req
            pretty_name = /ui2/cl_json=>pretty_mode-none
          RECEIVING
            r_json      = rv_json.

      CATCH cx_root INTO lo_error.
        MESSAGE s001(00) WITH 'JSON serialization failed' DISPLAY LIKE 'E'.
        CLEAR rv_json.
    ENDTRY.
  ENDMETHOD.

*----------------------------------------------------------------------*
* CALL_REST_API
*----------------------------------------------------------------------*
  METHOD call_rest_api.
    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_url         TYPE string,
          lv_response    TYPE string,
          lv_status      TYPE i,
          lv_reason      TYPE string,
          lv_length      TYPE i,
          lo_error       TYPE REF TO cx_root.

    " Build complete API URL
    CONCATENATE mv_api_url gc_api_endpoint INTO lv_url.

    TRY.
        " Create HTTP client
        CALL METHOD cl_http_client=>create_by_url
          EXPORTING
            url                = lv_url
          IMPORTING
            client             = lo_http_client
          EXCEPTIONS
            argument_not_found = 1
            plugin_not_active  = 2
            internal_error     = 3
            OTHERS             = 4.

        IF sy-subrc <> 0.
          MESSAGE s001(00) WITH 'Failed to create HTTP client' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        " Set request method to POST
        lo_http_client->request->set_method( 'POST' ).

        " Set content type
        lo_http_client->request->set_content_type( 'application/json' ).

        " Set request body (JSON)
        lv_length = strlen( iv_json ).
        lo_http_client->request->set_cdata( iv_json ).

        " Convert length to string for header
        DATA: lv_len_str TYPE string.
        lv_len_str = lv_length.
        CONDENSE lv_len_str NO-GAPS.

        lo_http_client->request->set_header_field(
          name  = 'Content-Length'
          value = lv_len_str ).

        " Send request
        lo_http_client->send(
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5 ).

        IF sy-subrc <> 0.
          MESSAGE s001(00) WITH 'Failed to send HTTP request' DISPLAY LIKE 'E'.
          lo_http_client->close( ).
          RETURN.
        ENDIF.

        " Receive response
        lo_http_client->receive(
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            OTHERS                     = 4 ).

        IF sy-subrc <> 0.
          MESSAGE s001(00) WITH 'Failed to receive HTTP response' DISPLAY LIKE 'E'.
          lo_http_client->close( ).
          RETURN.
        ENDIF.

        " Get response status
        lo_http_client->response->get_status(
          IMPORTING
            code   = lv_status
            reason = lv_reason ).

        " Get response body
        lo_http_client->response->get_cdata(
          RECEIVING
            data = lv_response ).

        " Check response status
        IF lv_status = 200.
          MESSAGE s001(00) WITH 'Data successfully sent to Fr8first API.'.
        ELSE.
          MESSAGE s001(00) WITH 'API call returned status:' lv_status DISPLAY LIKE 'W'.
        ENDIF.

        " Close HTTP client
        lo_http_client->close( ).

      CATCH cx_root INTO lo_error.
        MESSAGE s001(00) WITH 'API call failed' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

*----------------------------------------------------------------------*
* DISPLAY_OUTPUT
*----------------------------------------------------------------------*
  METHOD display_output.
    me->display_alv( ).
    me->display_summary( ).
  ENDMETHOD.

*----------------------------------------------------------------------*
* DISPLAY_ALV
*----------------------------------------------------------------------*
  METHOD display_alv.
    DATA: lo_alv       TYPE REF TO cl_salv_table,
          lo_columns   TYPE REF TO cl_salv_columns_table,
          lo_column    TYPE REF TO cl_salv_column,
          lo_functions TYPE REF TO cl_salv_functions_list,
          lo_display   TYPE REF TO cl_salv_display_settings,
          lo_msg       TYPE REF TO cx_salv_msg,
          lo_not_found TYPE REF TO cx_salv_not_found.

    CHECK gt_output IS NOT INITIAL.

    TRY.
        " Create ALV instance
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = gt_output ).

        " Get columns object
        lo_columns = lo_alv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        " Set column texts
        TRY.
            lo_column = lo_columns->get_column( 'VEHL_NO' ).
            lo_column->set_short_text( 'Vehicle' ).
            lo_column->set_medium_text( 'Vehicle No' ).
            lo_column->set_long_text( 'Vehicle/Truck Number' ).

            lo_column = lo_columns->get_column( 'GJAHR' ).
            lo_column->set_short_text( 'Year' ).
            lo_column->set_medium_text( 'Fiscal Year' ).
            lo_column->set_long_text( 'Fiscal Year' ).

            lo_column = lo_columns->get_column( 'MONAT' ).
            lo_column->set_short_text( 'Month' ).
            lo_column->set_medium_text( 'Month' ).
            lo_column->set_long_text( 'Month' ).

            lo_column = lo_columns->get_column( 'TOTAL_DISTANCE' ).
            lo_column->set_short_text( 'Distance' ).
            lo_column->set_medium_text( 'Total Distance' ).
            lo_column->set_long_text( 'Total Distance Travelled' ).

            lo_column = lo_columns->get_column( 'DISTANCE_UNIT' ).
            lo_column->set_short_text( 'Unit' ).
            lo_column->set_medium_text( 'Dist Unit' ).
            lo_column->set_long_text( 'Distance Unit' ).

            lo_column = lo_columns->get_column( 'TRIP_COUNT' ).
            lo_column->set_short_text( 'Trips' ).
            lo_column->set_medium_text( 'Trip Count' ).
            lo_column->set_long_text( 'Number of Trips' ).

            lo_column = lo_columns->get_column( 'LIFNR' ).
            lo_column->set_short_text( 'Vendor' ).
            lo_column->set_medium_text( 'Vendor Code' ).
            lo_column->set_long_text( 'Primary Transporter Vendor' ).

            lo_column = lo_columns->get_column( 'STATUS' ).
            lo_column->set_short_text( 'Status' ).
            lo_column->set_medium_text( 'Status' ).
            lo_column->set_long_text( 'Record Status' ).

            " Hide audit fields in ALV
            lo_column = lo_columns->get_column( 'CREATED_BY' ).
            lo_column->set_visible( abap_false ).
            lo_column = lo_columns->get_column( 'CREATED_ON' ).
            lo_column->set_visible( abap_false ).
            lo_column = lo_columns->get_column( 'CREATED_TM' ).
            lo_column->set_visible( abap_false ).
            lo_column = lo_columns->get_column( 'CHANGED_BY' ).
            lo_column->set_visible( abap_false ).
            lo_column = lo_columns->get_column( 'CHANGED_ON' ).
            lo_column->set_visible( abap_false ).
            lo_column = lo_columns->get_column( 'CHANGED_TM' ).
            lo_column->set_visible( abap_false ).

          CATCH cx_salv_not_found INTO lo_not_found.
            " Column not found - ignore
        ENDTRY.

        " Enable all standard ALV functions
        lo_functions = lo_alv->get_functions( ).
        lo_functions->set_all( abap_true ).

        " Set display settings
        lo_display = lo_alv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'Monthly Transport Truck Distance Calculation' ).

        " Display ALV
        lo_alv->display( ).

      CATCH cx_salv_msg INTO lo_msg.
        MESSAGE lo_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

*----------------------------------------------------------------------*
* DISPLAY_SUMMARY
*----------------------------------------------------------------------*
  METHOD display_summary.
    DATA: lv_total     TYPE i,
          lv_total_str TYPE string,
          lv_new_str   TYPE string,
          lv_upd_str   TYPE string,
          lv_det_str   TYPE string,
          lv_skip_str  TYPE string.

    lv_total = lines( gt_output ).

    IF p_test = abap_true.
      lv_total_str = lv_total.
      CONDENSE lv_total_str NO-GAPS.
      MESSAGE s001(00) WITH 'Test Mode:' lv_total_str 'vehicles processed. No data saved.'.
    ELSE.
      lv_new_str = gv_records_new.
      lv_upd_str = gv_records_upd.
      lv_det_str = gv_details_cnt.
      CONDENSE: lv_new_str NO-GAPS, lv_upd_str NO-GAPS, lv_det_str NO-GAPS.
      MESSAGE s001(00) WITH 'Completed:' lv_new_str 'new,' lv_upd_str 'updated records.'.
      MESSAGE s001(00) WITH lv_det_str 'shipment detail records saved.'.
    ENDIF.

    IF gv_records_skip > 0.
      lv_skip_str = gv_records_skip.
      CONDENSE lv_skip_str NO-GAPS.
      MESSAGE s001(00) WITH lv_skip_str 'records skipped (missing data)' DISPLAY LIKE 'W'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* GLOBAL OBJECTS
*----------------------------------------------------------------------*
DATA: go_report TYPE REF TO lcl_report.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  " Set default values to previous month
  DATA: lv_init_date TYPE sy-datum.

  lv_init_date = sy-datum.
  lv_init_date+6(2) = '01'.
  lv_init_date = lv_init_date - 1.
  p_monat = lv_init_date+4(2).
  p_gjahr = lv_init_date(4).

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CREATE OBJECT go_report.
  go_report->validate_input( ).

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CREATE OBJECT go_report.
  go_report->execute( ).

*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  go_report->display_output( ).

*----------------------------------------------------------------------*
* TEXT ELEMENTS (to be maintained in SE38)
*----------------------------------------------------------------------*
* TEXT-001: Selection Parameters
* TEXT-002: Optional Filters
* TEXT-003: Processing Options
