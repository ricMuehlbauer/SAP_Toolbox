REPORT lanc_op_societarias.
*-----------------------------------------------------------------------
* Módulo...: FI
* Descrição: Lançamentos Operações Societárias
* Autor....: Ricardo Muehlbauer
* Data.....: 10/03/2023
*
* [HISTÓRICO]
* ======== ========== ========== =======================================
* Data Autor Request Descrição
* ======== ========== ========== =======================================
* 10/03/23 TRICARDOM E03K9AFQH3 Desenvolvimento inicial
* ======== ========== ========== =======================================

*----------------------------------------------------------------------*
* Tables
*----------------------------------------------------------------------*
TABLES: bsis,"#EC CI_USAGE_OK[2296016]
        skat, ztbfi_contarazao, ztbfi_lanc_op_so, sscrfields.

*----------------------------------------------------------------------*
* Tipos *
*----------------------------------------------------------------------*
DATA: BEGIN OF w_documentos_bapi.
DATA: index TYPE lvc_index.
      INCLUDE TYPE ztbfi_lanc_op_so.
DATA: END OF w_documentos_bapi.

DATA: BEGIN OF w_alv.
DATA: farol TYPE c LENGTH 4.
      INCLUDE TYPE ztbfi_lanc_op_so.
DATA: END OF w_alv.

TYPES: BEGIN OF y_log,
         index        TYPE lvc_index,
         status       TYPE c,
         message(220) TYPE c,
       END OF y_log.


CLASS lcl_event_handler DEFINITION DEFERRED.
*----------------------------------------------------------------------*
* Tabelas Internas Globais *
*----------------------------------------------------------------------*
DATA: t_documentos      TYPE TABLE OF ztbfi_lanc_op_so,
      t_documentos_bapi LIKE TABLE OF w_documentos_bapi,
      t_alv             LIKE TABLE OF w_alv,
      t_fieldcat        TYPE lvc_t_fcat,
      t_currency        TYPE TABLE OF bapiaccr09,
      t_account         TYPE TABLE OF bapiacgl09,
      t_return          TYPE TABLE OF bapiret2,
      t_log             TYPE TABLE OF y_log.


*----------------------------------------------------------------------*
* WorkAreas Globais *
*----------------------------------------------------------------------*
DATA: w_layout   TYPE lvc_s_layo,
      w_header   TYPE bapiache09,
      w_currency TYPE bapiaccr09,
      w_account  TYPE bapiacgl09.

*----------------------------------------------------------------------*
* CONSTANTS Globais *
*----------------------------------------------------------------------*
CONSTANTS: c_code       TYPE programm VALUE 'ZGLRFI0450_LANC_OP_SOCIETARIAS',
           c_referencia TYPE zdegl_param VALUE 'REFERENCIA',
           c_tipo_doc   TYPE zdegl_param VALUE 'TIPODOC',
           c_green      TYPE icon-id VALUE '@08@',
           c_yellow     TYPE icon-id VALUE '@09@',
           c_red        TYPE icon-id VALUE '@0A@'.

*----------------------------------------------------------------------*
* RANGES Globais *
*----------------------------------------------------------------------*
* R_WERKS FOR T001W-WERKS

*----------------------------------------------------------------------*
* Variavel Globais *
*----------------------------------------------------------------------*
DATA: v_grid                TYPE REF TO cl_gui_alv_grid,
      v_container           TYPE REF TO cl_gui_custom_container,
      v_custom_control_name TYPE scrfname VALUE 'ALV-SCREEN',
      v_save                TYPE c.

DATA: w_stable        TYPE lvc_s_stbl,
      v_event_handler TYPE REF TO lcl_event_handler,
* data for event handling
      w_f4            TYPE lvc_s_f4,
      t_f4            TYPE lvc_t_f4.


*----------------------------------------------------------------------*
*Class DEFINITION *
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    METHODS:
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING sender
                  e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display,

      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_ucomm,

      hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
          e_row_id
          e_column_id
          es_row_no.

ENDCLASS.

*----------------------------------------------------------------------*
*Class IMPLEMENTATION *
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_toolbar.
    DATA: w_toolbar TYPE stb_button.

    CLEAR w_toolbar.
    MOVE 3 TO w_toolbar-butn_type.
    APPEND w_toolbar TO e_object->mt_toolbar.
    CLEAR w_toolbar.
    MOVE 'CONT' TO w_toolbar-function.                      "#EC NOTEXT
* MOVE icon_calculation TO ls_toolbar-icon.
    MOVE 'Contabilizar Lançamentos Societários'(202) TO w_toolbar-quickinfo.
    MOVE 'Contabilizar Lançamentos Societários' TO w_toolbar-text.
    MOVE ' ' TO w_toolbar-disabled.                         "#EC NOTEXT
    APPEND w_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command .
    DATA: v_rows TYPE lvc_t_row.

    CASE e_ucomm.
      WHEN 'CONT'.
        CALL METHOD v_grid->get_selected_rows
          IMPORTING
            et_index_rows = v_rows.
        IF sy-subrc IS INITIAL.
          CLEAR: t_documentos_bapi, t_log.

          LOOP AT v_rows INTO DATA(w_rows).
            READ TABLE t_alv INDEX w_rows-index INTO DATA(w_documentos_aux).
            IF w_documentos_aux-farol EQ c_green AND w_documentos_aux-status NE 'Contabilizado'.
              MOVE-CORRESPONDING w_documentos_aux TO w_documentos_bapi.
              w_documentos_bapi-index = w_rows-index.
              APPEND w_documentos_bapi TO t_documentos_bapi.
            ELSEIF w_documentos_aux-status EQ 'Contabilizado'.
              MESSAGE TEXT-m06 TYPE 'S' DISPLAY LIKE 'E'.
              RETURN.
            ELSE.
              MESSAGE TEXT-m05 TYPE 'S' DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.
          ENDLOOP.
          PERFORM zf_bapi_acc_document_post.
        ELSE.
          MESSAGE TEXT-m03 TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
    ENDCASE.

  ENDMETHOD. "handle_user_command

*ONF4
  METHOD on_f4.

    DATA: t_return TYPE TABLE OF ddshretval.
    DATA: v_hkont TYPE skat-saknr.

    SELECT hkont
    FROM ztbfi_continvest
    INTO TABLE @DATA(t_continvest).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield   = 'HKONT'
        value_org  = 'S'
      TABLES
        value_tab  = t_continvest
        return_tab = t_return.

    IF sy-subrc = 0.
      READ TABLE t_return INTO DATA(w_return) INDEX 1.

      READ TABLE t_alv ASSIGNING FIELD-SYMBOL(<fsl_documentos>) INDEX es_row_no-row_id.
      <fsl_documentos>-hkont2 = w_return-fieldval.

      v_hkont = |{ w_return-fieldval ALPHA = IN }|.

      SELECT SINGLE txt50
      INTO <fsl_documentos>-skat2
      FROM skat
      WHERE saknr EQ v_hkont AND
      ktopl = 'PC01' AND
      spras = 'P'.

      IF sy-subrc IS NOT INITIAL.
        CLEAR <fsl_documentos>-skat2.
        MESSAGE TEXT-m01 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

      IF <fsl_documentos>-hkont2 IS INITIAL AND <fsl_documentos>-rmvct IS INITIAL AND
      <fsl_documentos>-vbund IS INITIAL AND <fsl_documentos>-xblnr IS INITIAL AND
      <fsl_documentos>-sgtxt IS INITIAL AND <fsl_documentos>-skat2 IS INITIAL.
        <fsl_documentos>-farol = c_red.
      ELSEIF <fsl_documentos>-hkont2 IS NOT INITIAL AND <fsl_documentos>-rmvct IS NOT INITIAL AND
      <fsl_documentos>-vbund IS NOT INITIAL AND <fsl_documentos>-xblnr IS NOT INITIAL AND
      <fsl_documentos>-sgtxt IS NOT INITIAL AND <fsl_documentos>-skat2 IS NOT INITIAL.
        <fsl_documentos>-farol = c_green.
      ELSE.
        <fsl_documentos>-farol = c_red.
      ENDIF.

    ENDIF.

*Refresh the screen without changing the view

    w_stable-row = '1'.
    CALL METHOD v_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

  METHOD handle_data_changed.

    DATA : t_modi  TYPE TABLE OF lvc_s_modi,
           w_modis TYPE lvc_s_modi.

    t_modi = er_data_changed->mt_mod_cells .
    LOOP AT t_modi INTO w_modis.

      CASE w_modis-fieldname. "Case para forçar a atualização na itab, assim sendo possivel setar a cor do farol.
        WHEN 'HKONT2'.
          READ TABLE t_alv INDEX w_modis-row_id ASSIGNING FIELD-SYMBOL(<fs_alv>).
          <fs_alv>-hkont2 = w_modis-value.
        WHEN 'RMVCT'.
          READ TABLE t_alv INDEX w_modis-row_id ASSIGNING <fs_alv>.
          <fs_alv>-rmvct = w_modis-value.
        WHEN 'VBUND'.
          READ TABLE t_alv INDEX w_modis-row_id ASSIGNING <fs_alv>.
          <fs_alv>-vbund = w_modis-value.
        WHEN 'XBLNR'.
          READ TABLE t_alv INDEX w_modis-row_id ASSIGNING <fs_alv>.
          <fs_alv>-xblnr = w_modis-value.
        WHEN 'SGTXT'.
          READ TABLE t_alv INDEX w_modis-row_id ASSIGNING <fs_alv>.
          <fs_alv>-sgtxt = w_modis-value.
      ENDCASE.
    ENDLOOP.

    LOOP AT t_alv ASSIGNING <fs_alv>.
      IF <fs_alv>-hkont2 IS INITIAL AND <fs_alv>-rmvct IS INITIAL AND
      <fs_alv>-vbund IS INITIAL AND <fs_alv>-xblnr IS INITIAL AND
      <fs_alv>-sgtxt IS INITIAL.
        <fs_alv>-farol = c_red.
      ELSEIF <fs_alv>-hkont2 IS NOT INITIAL AND <fs_alv>-rmvct IS NOT INITIAL AND
      <fs_alv>-vbund IS NOT INITIAL AND <fs_alv>-xblnr IS NOT INITIAL AND
      <fs_alv>-sgtxt IS NOT INITIAL.
        <fs_alv>-farol = c_green.
      ELSE.
        <fs_alv>-farol = c_yellow.
      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0.
      w_stable-row = '1'.
      CALL METHOD v_grid->refresh_table_display
        EXPORTING
          is_stable = w_stable.
    ENDIF.

  ENDMETHOD. "handle_data_changed

  METHOD hotspot_click.

    CLEAR w_alv.

    v_grid->get_current_cell(
      IMPORTING
        es_row_id = e_row_id ).

    READ TABLE t_alv INTO w_alv
    INDEX e_row_id-index.
    IF sy-subrc = 0.
      IF e_column_id = 'BELNR2'.
        SET PARAMETER ID 'BUK' FIELD w_alv-bukrs.
        SET PARAMETER ID 'BLN' FIELD w_alv-belnr2.
        SET PARAMETER ID 'GJR' FIELD w_alv-gjahr2.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.

  SELECTION-SCREEN FUNCTION KEY 1.

  SELECT-OPTIONS: s_bukrs FOR bsis-bukrs, "#EC CI_USAGE_OK[2296016] "#EC CI_USAGE_OK[2296016] "#EC CI_USAGE_OK[2296016] "#EC CI_USAGE_OK[2296016] "#EC CI_USAGE_OK[2296016]
  s_dats FOR bsis-budat. "#EC CI_USAGE_OK[2296016] "#EC CI_USAGE_OK[2296016] "#EC CI_USAGE_OK[2296016] "#EC CI_USAGE_OK[2296016] "#EC CI_USAGE_OK[2296016]

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
  PARAMETERS: p_layout(12) MATCHCODE OBJECT zshfi_layout_lanc_op_soc.
SELECTION-SCREEN: END OF BLOCK b2.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  CONCATENATE icon_tools TEXT-001 INTO sscrfields-functxt_01 SEPARATED BY space.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF sscrfields-ucomm = 'FC01'.

    CALL TRANSACTION 'ZGLFI911'.

  ENDIF.

*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM zf_autorizacao.
  PERFORM zf_seleciona_dados.
  PERFORM zf_processa_dados.
  PERFORM zf_monta_fieldcat_header.
  PERFORM zf_build_layout.
  CALL SCREEN 9000.

*&---------------------------------------------------------------------*
*& Form ZF_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM zf_seleciona_dados .

*Seleciona parametro Referencias Coligadas
  SELECT SINGLE zvlpar
  INTO @DATA(l_referencia)
  FROM ztbbc_parametros
  WHERE programm EQ @c_code AND
  zparam EQ @c_referencia.

*Seleciona parametro Tipo doc Coligadas
  SELECT SINGLE zvlpar
  INTO @DATA(l_tipo_doc)
  FROM ztbbc_parametros
  WHERE programm EQ @c_code AND
  zparam EQ @c_tipo_doc.


*Seleciona Contas Razão cadastradas no Cluster
  SELECT hkont
  INTO TABLE @DATA(lt_conta_razao)
  FROM ztbfi_contarazao.

  SELECT bsis~bukrs
  bsis~hkont
  skat~txt50
  bsis~zuonr
  bsis~belnr
  bsis~buzei
  bsis~gjahr
  bsis~shkzg
  bsis~bschl
  bsis~budat
  bsis~bldat
  bsis~dmbtr
  bsis~waers
  bsis~gsber
  INTO CORRESPONDING FIELDS OF TABLE t_documentos
  FROM bsis AS bsis "#EC CI_USAGE_OK[2296016] "#EC CI_USAGE_OK[2296016]
  INNER JOIN skat AS skat ON
  skat~saknr EQ bsis~hkont AND
  skat~ktopl EQ 'PC01' AND
  skat~spras EQ sy-langu
  FOR ALL ENTRIES IN lt_conta_razao
  WHERE bsis~hkont EQ lt_conta_razao-hkont AND
  bsis~bukrs IN s_bukrs AND
  bsis~budat IN s_dats AND
  bsis~zuonr EQ l_referencia AND
  bsis~blart EQ l_tipo_doc.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE TEXT-m01 TYPE 'E'.
  ELSE.
    SELECT
    bukrs,
    hkont,
    belnr,
    buzei,
    gjahr,
    timestamp,
    hkont2,
    skat2,
    rmvct,
    vbund,
    xblnr,
    sgtxt,
    belnr2,
    gjahr2,
    status,
    log
    INTO TABLE @DATA(t_lanc_op)
    FROM ztbfi_lanc_op_so
    FOR ALL ENTRIES IN @t_documentos
    WHERE bukrs EQ @t_documentos-bukrs AND
    hkont EQ @t_documentos-hkont AND
    belnr EQ @t_documentos-belnr AND
    buzei EQ @t_documentos-buzei AND
    gjahr EQ @t_documentos-gjahr.

    IF sy-subrc IS INITIAL.
      SORT t_lanc_op STABLE BY bukrs ASCENDING
      hkont ASCENDING
      belnr ASCENDING
      buzei ASCENDING
      gjahr ASCENDING
      timestamp DESCENDING.

      LOOP AT t_documentos ASSIGNING FIELD-SYMBOL(<fs_documentos>).
        READ TABLE t_lanc_op INTO DATA(w_lanc_op) WITH KEY bukrs = <fs_documentos>-bukrs
        hkont = <fs_documentos>-hkont
        belnr = <fs_documentos>-belnr
        buzei = <fs_documentos>-buzei
        gjahr = <fs_documentos>-gjahr BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          MOVE-CORRESPONDING w_lanc_op TO <fs_documentos>.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_PROCESSA_DADOS
*&---------------------------------------------------------------------*
FORM zf_processa_dados .

  LOOP AT t_documentos ASSIGNING FIELD-SYMBOL(<fs_documentos>).

    IF <fs_documentos>-shkzg EQ 'S'.
      <fs_documentos>-shkzg = 'D'.
      <fs_documentos>-shkzg2 = 'C'.
    ELSEIF <fs_documentos>-shkzg EQ 'H'.
      <fs_documentos>-shkzg = 'C'.
      <fs_documentos>-shkzg2 = 'D'.
      <fs_documentos>-dmbtr = <fs_documentos>-dmbtr * ( -1 ).
    ENDIF.

    IF <fs_documentos>-bschl EQ '50'.
      <fs_documentos>-bschl2 = '40'.
    ELSEIF <fs_documentos>-bschl EQ '40'.
      <fs_documentos>-bschl2 = '50'.
    ENDIF.

  ENDLOOP.

  MOVE-CORRESPONDING t_documentos TO t_alv.

  LOOP AT t_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).
    IF <fs_alv>-hkont2 IS INITIAL AND <fs_alv>-rmvct IS INITIAL AND
    <fs_alv>-vbund IS INITIAL AND <fs_alv>-xblnr IS INITIAL AND
    <fs_alv>-sgtxt IS INITIAL.
      <fs_alv>-farol = c_red.
    ELSEIF <fs_alv>-hkont2 IS NOT INITIAL AND <fs_alv>-rmvct IS NOT INITIAL AND
    <fs_alv>-vbund IS NOT INITIAL AND <fs_alv>-xblnr IS NOT INITIAL AND
    <fs_alv>-sgtxt IS NOT INITIAL.
      <fs_alv>-farol = c_green.
    ELSE.
      <fs_alv>-farol = c_yellow.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_MONTA_FIELDCAT_HEADER
*&---------------------------------------------------------------------*
FORM zf_monta_fieldcat_header .

  DATA: w_fieldcat TYPE lvc_s_fcat.

  w_fieldcat-fieldname = 'FAROL'.
  w_fieldcat-scrtext_s = 'Farol'.
  w_fieldcat-col_pos = -1.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'BUKRS'.
  w_fieldcat-scrtext_s = 'Empresa'.
  w_fieldcat-scrtext_m = 'Empresa'.
  w_fieldcat-scrtext_l = 'Empresa'.
  w_fieldcat-col_pos = 0.
  w_fieldcat-outputlen = 4.
  w_fieldcat-key = 'X'.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'HKONT'.
  w_fieldcat-scrtext_s = 'CntRazBanc'.
  w_fieldcat-scrtext_m = 'Conta Razão Banco'.
  w_fieldcat-scrtext_l = 'Conta Razão Banco'.
  w_fieldcat-col_pos = 1.
  w_fieldcat-outputlen = 10.
  w_fieldcat-key = 'X'.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'TXT50'.
  w_fieldcat-scrtext_m = 'DescContBanc'.
  w_fieldcat-scrtext_m = 'Desc Conta Banco'.
  w_fieldcat-scrtext_l = 'Descrição Conta Banco'.
  w_fieldcat-col_pos = 2.
  w_fieldcat-outputlen = 50.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'ZUONR'.
  w_fieldcat-scrtext_s = 'Atribuição'.
  w_fieldcat-scrtext_m = 'Atribuição'.
  w_fieldcat-scrtext_l = 'Atribuição'.
  w_fieldcat-col_pos = 3.
  w_fieldcat-outputlen = 18.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'BELNR'.
  w_fieldcat-scrtext_s = 'N.Doc'.
  w_fieldcat-scrtext_m = 'N.Documento'.
  w_fieldcat-scrtext_l = 'N. Documento'.
  w_fieldcat-col_pos = 4.
  w_fieldcat-outputlen = 10.
  w_fieldcat-key = 'X'.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'BUZEI'.
  w_fieldcat-scrtext_s = 'Item'.
  w_fieldcat-scrtext_m = 'Item'.
  w_fieldcat-scrtext_l = 'Item'.
  w_fieldcat-col_pos = 5.
  w_fieldcat-outputlen = 3.
  w_fieldcat-key = 'X'.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'GJAHR'.
  w_fieldcat-scrtext_s = 'Exercício'.
  w_fieldcat-scrtext_m = 'Exercício'.
  w_fieldcat-scrtext_l = 'Exercício'.
  w_fieldcat-col_pos = 6.
  w_fieldcat-outputlen = 4.
  w_fieldcat-key = 'X'.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'SHKZG'.
  w_fieldcat-scrtext_s = 'CodDebCred'.
  w_fieldcat-scrtext_m = 'Cod Débito/Crédito'.
  w_fieldcat-scrtext_l = 'Código Débito/Crédito'.
  w_fieldcat-col_pos = 7.
  w_fieldcat-outputlen = 1.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'BSCHL'.
  w_fieldcat-scrtext_m = 'Chave Lanc'.
  w_fieldcat-scrtext_m = 'Chave Lançamento'.
  w_fieldcat-scrtext_l = 'Chave Lançamento'.
  w_fieldcat-col_pos = 8.
  w_fieldcat-outputlen = 2.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'BUDAT'.
  w_fieldcat-scrtext_s = 'Data Lanc'.
  w_fieldcat-scrtext_m = 'Data de Lançamento'.
  w_fieldcat-scrtext_l = 'Data de Lançamento'.
  w_fieldcat-col_pos = 9.
  w_fieldcat-outputlen = 8.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'BLDAT'.
  w_fieldcat-scrtext_s = 'Data Doc'.
  w_fieldcat-scrtext_m = 'Data do Documento'.
  w_fieldcat-scrtext_l = 'Data do Documento'.
  w_fieldcat-col_pos = 10.
  w_fieldcat-outputlen = 8.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'DMBTR'.
  w_fieldcat-scrtext_s = 'Montante'.
  w_fieldcat-scrtext_m = 'Montante em MI'.
  w_fieldcat-scrtext_l = 'Montante em MI'.
  w_fieldcat-col_pos = 11.
  w_fieldcat-outputlen = 13.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'WAERS'.
  w_fieldcat-scrtext_s = 'Cod Moeda'.
  w_fieldcat-scrtext_m = 'Código da Moeda'.
  w_fieldcat-scrtext_l = 'Código da Moeda'.
  w_fieldcat-col_pos = 12.
  w_fieldcat-outputlen = 5.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'GSBER'.
  w_fieldcat-scrtext_s = 'Divisão'.
  w_fieldcat-scrtext_m = 'Divisão'.
  w_fieldcat-scrtext_l = 'Divisão'.
  w_fieldcat-col_pos = 13.
  w_fieldcat-outputlen = 4.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'HKONT2'.
  w_fieldcat-scrtext_s = 'ContContra'.
  w_fieldcat-scrtext_m = 'Cont.Contra'.
  w_fieldcat-scrtext_l = 'Conta Contrapartida'.
  w_fieldcat-ref_field = 'HKONT'.
* w_fieldcat-ref_table = 'ZSTFI_CONTINVEST'.
  w_fieldcat-edit = 'X'.
  w_fieldcat-col_pos = 14.
  w_fieldcat-outputlen = 10.
  w_fieldcat-f4availabl = 'X'.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'SKAT2'.
  w_fieldcat-scrtext_s = 'DscContInv'.
  w_fieldcat-scrtext_m = 'Desc Cont Invest'.
  w_fieldcat-scrtext_l = 'Descrição da Conta Investimento'.
  w_fieldcat-col_pos = 15.
  w_fieldcat-outputlen = 50.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'SHKZG2'.
  w_fieldcat-scrtext_s = 'D/C Contra'.
  w_fieldcat-scrtext_m = 'Cod Deb/Cred Contra'.
  w_fieldcat-scrtext_l = 'Código Débito/Crédito Contrapartida'.
  w_fieldcat-col_pos = 16.
  w_fieldcat-outputlen = 1.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'BSCHL2'.
  w_fieldcat-scrtext_s = 'LancContra'.
  w_fieldcat-scrtext_m = 'Chave Lanc Contra'.
  w_fieldcat-scrtext_l = 'Chave Lançamento Contrapartida'.
  w_fieldcat-col_pos = 17.
  w_fieldcat-outputlen = 2.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'RMVCT'.
  w_fieldcat-scrtext_s = 'Tipo Mov'.
  w_fieldcat-scrtext_m = 'Tipo Movimento'.
  w_fieldcat-scrtext_l = 'Tipo Movimento'.
  w_fieldcat-ref_field = 'RMVCT'.
  w_fieldcat-ref_table = 'ZSTFI_TIPOMOV'.
  w_fieldcat-edit = 'X'.
  w_fieldcat-col_pos = 18.
  w_fieldcat-outputlen = 3.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'VBUND'.
  w_fieldcat-scrtext_s = 'SocPar'.
  w_fieldcat-scrtext_m = 'Soc Parceira'.
  w_fieldcat-scrtext_l = 'Sociedade Parceira'.
  w_fieldcat-ref_field = 'RCOMP_D'.
  w_fieldcat-ref_table = 'ZSTFI_EMPRESA_COLIGADA'.
  w_fieldcat-edit = 'X'.
  w_fieldcat-col_pos = 19.
  w_fieldcat-outputlen = 6.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'XBLNR'.
  w_fieldcat-scrtext_s = 'Referência'.
  w_fieldcat-scrtext_m = 'Referência'.
  w_fieldcat-scrtext_l = 'Referência'.
  w_fieldcat-ref_field = 'XBLNR'.
  w_fieldcat-ref_table = 'ZSTFI_REFERENCIA'.
  w_fieldcat-edit = 'X'.
  w_fieldcat-col_pos = 20.
  w_fieldcat-outputlen = 16.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'SGTXT'.
  w_fieldcat-scrtext_s = 'Texto'.
  w_fieldcat-scrtext_m = 'Texto'.
  w_fieldcat-scrtext_l = 'Texto'.
  w_fieldcat-edit = 'X'.
  w_fieldcat-col_pos = 21.
  w_fieldcat-outputlen = 50.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'BELNR2'.
  w_fieldcat-scrtext_s = 'DocContSoc'.
  w_fieldcat-scrtext_m = 'N. Doc Contábil Soc.'.
  w_fieldcat-scrtext_l = 'N. Documento Contábil Societário'.
  w_fieldcat-col_pos = 22.
  w_fieldcat-outputlen = 10.
  w_fieldcat-hotspot = 'X'.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'GJAHR2'.
  w_fieldcat-scrtext_s = 'ExeDocCSoc'.
  w_fieldcat-scrtext_m = 'Exer.Doc.ContábilSoc.'.
  w_fieldcat-scrtext_l = 'Exercício Documento Contábil Societário'.
  w_fieldcat-col_pos = 23.
  w_fieldcat-outputlen = 4.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'STATUS'.
  w_fieldcat-scrtext_s = 'Status'.
  w_fieldcat-scrtext_m = 'Status'.
  w_fieldcat-scrtext_l = 'Status'.
  w_fieldcat-col_pos = 24.
  w_fieldcat-outputlen = 50.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.
  w_fieldcat-fieldname = 'LOG'.
  w_fieldcat-scrtext_s = 'Log'.
  w_fieldcat-scrtext_m = 'Log'.
  w_fieldcat-scrtext_l = 'Log'.
  w_fieldcat-col_pos = 25.
  w_fieldcat-outputlen = 255.
  APPEND w_fieldcat TO t_fieldcat.
  CLEAR w_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_BUILD_LAYOUT
*&---------------------------------------------------------------------*
FORM zf_build_layout .
* Set layout field for field attributes(i.e. input/output)
  w_layout-zebra = 'X'.
  w_layout-cwidth_opt = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM zf_display_alv .

  DATA: w_variant TYPE disvariant.

  w_variant-report = sy-repid.
  IF p_layout IS NOT INITIAL.
    w_variant-variant = p_layout.
  ENDIF.

  CALL METHOD v_grid->set_ready_for_input.

  CALL METHOD v_grid->set_table_for_first_display
    EXPORTING
      is_layout       = w_layout
      is_variant      = w_variant
      i_save          = 'A'
*     it_toolbar_excluding = pt_exclude[]
    CHANGING
      it_outtab       = t_alv
      it_fieldcatalog = t_fieldcat.

  CLEAR t_f4.
  w_f4-fieldname = 'HKONT2'.
  w_f4-register = 'X'.
* w_f4-chngeafter = 'X'. "§7b
  APPEND w_f4 TO t_f4.
  CALL METHOD v_grid->register_f4_for_fields
    EXPORTING
      it_f4 = t_f4.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'ZGFI_LANC_OP_SOCIETA'.
* SET TITLEBAR 'xxx'.
  IF v_grid IS INITIAL.

    CREATE OBJECT v_container
      EXPORTING
        container_name = v_custom_control_name.

    CREATE OBJECT v_grid "Criação do objeto que exibirá o alv
      EXPORTING
        i_parent = v_container.

    PERFORM zf_display_alv.

    CREATE OBJECT v_event_handler.
    SET HANDLER v_event_handler->handle_data_changed FOR v_grid.
    SET HANDLER v_event_handler->handle_user_command FOR v_grid.
    SET HANDLER v_event_handler->handle_toolbar FOR v_grid.
    SET HANDLER: v_event_handler->on_f4 FOR ALL INSTANCES.
    SET HANDLER v_event_handler->hotspot_click FOR v_grid.

    CALL METHOD v_grid->set_toolbar_interactive.

    CALL METHOD v_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    v_grid->refresh_table_display( ).

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module USER_COMMAND_9000 INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      IF v_grid IS NOT INITIAL.
        CALL METHOD v_grid->check_changed_data
          IMPORTING
            e_valid = DATA(l_valid).

        IF l_valid IS NOT INITIAL.
          v_save = 'X'.
          PERFORM zf_salva_dados USING v_save.
        ENDIF.

      ENDIF.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form ZF_SET_TIMESTAMP
*&---------------------------------------------------------------------*
FORM zf_salva_dados USING v_save TYPE c.
  DATA l_timestamp TYPE c LENGTH 15.

  CLEAR t_documentos.

  MOVE-CORRESPONDING t_alv TO t_documentos.

  DELETE t_documentos WHERE hkont2 IS INITIAL AND "Remove as linhas que nao foram modificadas
  skat2 IS INITIAL AND
  rmvct IS INITIAL AND
  vbund IS INITIAL AND
  xblnr IS INITIAL AND
  sgtxt IS INITIAL AND
  belnr2 IS INITIAL AND
  gjahr2 IS INITIAL AND
  status IS INITIAL AND
  log IS INITIAL.

  IF v_save EQ abap_true.
    DELETE t_documentos WHERE status EQ 'Contabilizado'. "Remove as linhas Contabilizadas
    IF t_documentos IS INITIAL.
      MESSAGE TEXT-m07 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDIF.

  CONCATENATE sy-datlo sy-timlo INTO l_timestamp.

  LOOP AT t_documentos ASSIGNING FIELD-SYMBOL(<fs_documentos>). "Set timestamp
    <fs_documentos>-timestamp = l_timestamp.
  ENDLOOP.

  MODIFY ztbfi_lanc_op_so FROM TABLE t_documentos.
  IF sy-subrc IS INITIAL.
    MESSAGE TEXT-m02 TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_BAPI_ACC_DOCUMENT_POST
*&---------------------------------------------------------------------*
FORM zf_bapi_acc_document_post .

  CONSTANTS: c_glvor TYPE glvor VALUE 'RFBU',
             c_blart TYPE blart VALUE 'SA'.

  DATA w_log TYPE y_log.

  LOOP AT t_documentos_bapi INTO w_documentos_bapi.

    CLEAR: w_header, t_currency, t_account, w_currency, w_account, t_return.

* Monta Header
    w_header-bus_act = c_glvor.
    w_header-comp_code = w_documentos_bapi-bukrs.
    w_header-fisc_year = w_documentos_bapi-gjahr.
    w_header-doc_date = w_documentos_bapi-bldat.
    w_header-pstng_date = w_documentos_bapi-budat.
    w_header-doc_type = c_blart.
    w_header-ref_doc_no = w_documentos_bapi-xblnr.
    w_header-username = sy-uname.
    w_header-header_txt = w_documentos_bapi-xblnr.

* Monta Accountgl Item 1
    w_account-itemno_acc = '1'.
    w_account-gl_account = w_documentos_bapi-hkont.
    w_account-alloc_nmbr = w_documentos_bapi-zuonr.
    w_account-item_text = w_documentos_bapi-sgtxt.
    w_account-bus_area = w_documentos_bapi-gsber.
    w_account-cs_trans_t = w_documentos_bapi-rmvct.
    w_account-trade_id = w_documentos_bapi-vbund.
    w_account-value_date = w_documentos_bapi-budat.

    APPEND w_account TO t_account.

* Monta Accountgl Item 2
    CLEAR w_account.
    w_account-itemno_acc = '2'.
    w_account-gl_account = w_documentos_bapi-hkont2.

    w_account-gl_account = |{ w_account-gl_account ALPHA = IN }|.

    w_account-alloc_nmbr = w_documentos_bapi-zuonr.
    w_account-item_text = w_documentos_bapi-sgtxt.
    w_account-bus_area = w_documentos_bapi-gsber.
    w_account-cs_trans_t = w_documentos_bapi-rmvct.
    w_account-trade_id = w_documentos_bapi-vbund.
    w_account-value_date = w_documentos_bapi-budat.

    APPEND w_account TO t_account.

* Monta CurrencyAmount Item 1
    w_currency-itemno_acc = '1'.
    w_currency-currency = w_documentos_bapi-waers.
    IF ( ( w_documentos_bapi-shkzg EQ 'C' ) AND ( w_documentos_bapi-dmbtr GT 0 ) ) OR
    ( ( w_documentos_bapi-shkzg EQ 'D' ) AND ( w_documentos_bapi-dmbtr LT 0 ) ) .
      w_currency-amt_doccur = w_documentos_bapi-dmbtr.
    ELSE.
      w_currency-amt_doccur = w_documentos_bapi-dmbtr * ( -1 ). "#EC CI_FLDEXT_OK[2610650]
    ENDIF.

    APPEND w_currency TO t_currency.

* Monta CurrencyAmount Item 2
    CLEAR w_currency.
    w_currency-itemno_acc = '2'.
    w_currency-currency = w_documentos_bapi-waers.
    IF ( ( w_documentos_bapi-shkzg2 EQ 'C' ) AND ( w_documentos_bapi-dmbtr GT 0 ) ) OR
    ( ( w_documentos_bapi-shkzg2 EQ 'D' ) AND ( w_documentos_bapi-dmbtr LT 0 ) ) .
      w_currency-amt_doccur = w_documentos_bapi-dmbtr.
    ELSE.
      w_currency-amt_doccur = w_documentos_bapi-dmbtr * ( -1 ). "#EC CI_FLDEXT_OK[2610650]
    ENDIF.

    APPEND w_currency TO t_currency.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST' "#EC CI_USAGE_OK[2438131]
      EXPORTING "#EC CI_USAGE_OK[2628704]
        documentheader = w_header
      TABLES
        accountgl      = t_account
        currencyamount = t_currency
        return         = t_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    IF t_return IS NOT INITIAL.
* DELETE t_return WHERE id = 'RW'.
      LOOP AT t_return INTO DATA(w_return).
        w_log-message = w_return-message.
        w_log-index = w_documentos_bapi-index.
        w_log-status = w_return-type.
        APPEND w_log TO t_log.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

  IF t_log IS NOT INITIAL.

    LOOP AT t_log INTO w_log.
      READ TABLE t_alv INDEX w_log-index ASSIGNING FIELD-SYMBOL(<fs_alv>).
      IF sy-subrc IS INITIAL.
        IF w_log-status EQ 'S'.
          <fs_alv>-belnr2 = w_log-message+25(10).
          <fs_alv>-gjahr2 = w_log-message+40(4).
          <fs_alv>-status = 'Contabilizado'.
          <fs_alv>-log = w_log-message.
        ELSE.
          CONCATENATE <fs_alv>-log w_log-message INTO <fs_alv>-log SEPARATED BY '/'.
        ENDIF.
      ENDIF.
    ENDLOOP.
    v_grid->refresh_table_display( ).
    CLEAR v_save.
    PERFORM zf_salva_dados USING v_save.
    PERFORM zf_log_popup.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_LOG_POPUP
*&---------------------------------------------------------------------*
FORM zf_log_popup .
  DATA: t_fieldcat_log TYPE slis_t_fieldcat_alv,
        w_fieldcat_log TYPE slis_fieldcat_alv.

  CLEAR w_fieldcat_log.

  w_fieldcat_log-fieldname = 'INDEX'.
  w_fieldcat_log-tabname = 'T_LOG'.
  w_fieldcat_log-rollname = 'LINHA'.
  w_fieldcat_log-seltext_m = 'LINHA'.
  w_fieldcat_log-outputlen = 5.
  w_fieldcat_log-no_out = space.
  w_fieldcat_log-col_pos = 1.
  APPEND w_fieldcat_log TO t_fieldcat_log.
  CLEAR w_fieldcat_log.
  w_fieldcat_log-fieldname = 'MESSAGE'.
  w_fieldcat_log-tabname = 'T_LOG'.
  w_fieldcat_log-rollname = 'MESSAGE'.
  w_fieldcat_log-seltext_m = 'MENSSAGEM'.
  w_fieldcat_log-outputlen = 220.
  w_fieldcat_log-no_out = space.
  w_fieldcat_log-col_pos = 2.
  APPEND w_fieldcat_log TO t_fieldcat_log.
  CLEAR w_fieldcat_log.


  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title               = 'LOG DE ERROS'
      i_selection           = space
      i_zebra               = 'X'
      i_tabname             = 'T_LOG'
      it_fieldcat           = t_fieldcat_log[]
      i_screen_start_line   = 5
      i_screen_end_line     = 25
      i_screen_start_column = 10
      i_screen_end_column   = 120
    TABLES
      t_outtab              = t_log
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_AUTORIZACAO
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM zf_autorizacao .

  SELECT bukrs
  FROM t001
  INTO TABLE @DATA(t_t001)
  WHERE bukrs IN @s_bukrs.

  IF sy-subrc IS INITIAL.
    LOOP AT t_t001 INTO DATA(l_bukrs).

      AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
      ID 'BUKRS' FIELD l_bukrs
      ID 'ACTVT' FIELD '02'.

      IF sy-subrc IS NOT INITIAL.
        " Falta autorização para empresa
        MESSAGE s368(00) DISPLAY LIKE sy-abcde+4(1) WITH TEXT-e01 l_bukrs.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDLOOP.
  ELSE.
    " Empresa não encontrada.
    MESSAGE s368(00) DISPLAY LIKE sy-abcde+4(1) WITH TEXT-e02.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.