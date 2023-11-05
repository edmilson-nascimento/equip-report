*&---------------------------------------------------------------------*
*& Report /yga/fix_equi_status
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /yga/fix_equi_status.

TABLES:
  equi .


CLASS class_report DEFINITION .

  PUBLIC SECTION .

    "! <p class="shorttext synchronized" lang="pt">Metodo construtor</p>
    METHODS constructor
      IMPORTING
        !im_equi TYPE range_t_equnr .

    "! <p class="shorttext synchronized" lang="pt">Busca dados de equipamentos</p>
    METHODS get_data .

    "! <p class="shorttext synchronized" lang="pt">Exibe dados encontrados</p>
    METHODS show .

    "! <p class="shorttext synchronized" lang="pt">Exibe status de processamento</p>
    METHODS process
      IMPORTING
        !percent  TYPE i OPTIONAL
        !total    TYPE i OPTIONAL
        !currency TYPE i OPTIONAL
        !message  TYPE char50 .

  PROTECTED SECTION .

  PRIVATE SECTION .

    TYPES:
      BEGIN OF ty_out,
        equnr TYPE equi-equnr,
        spras TYPE eqkt-spras,
        eqktx TYPE eqkt-eqktx,
        eqktu TYPE eqkt-eqktu,
        sttxt TYPE itobattr-sttxt,
      END OF ty_out,
      tab_out TYPE TABLE OF ty_out.

    DATA:
      salv_table TYPE REF TO cl_salv_table,
      gt_equi    TYPE range_t_equnr,
      gt_outtab  TYPE tab_out.

    "! <p class="shorttext synchronized" lang="pt">Mantem processamento apos ALV exibido</p>
    METHODS on_user_command
                FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

    "! <p class="shorttext synchronized" lang="pt">Retorna TRUE para confirmação</p>
    METHODS confirm
      RETURNING
        VALUE(rv_result) TYPE sap_bool .

    "! <p class="shorttext synchronized" lang="pt">Preenche dados de SHDB para processamento</p>
    METHODS create_shdb
      IMPORTING
        !im_equi TYPE equi-equnr
      EXPORTING
        ex_data  TYPE tab_bdcdata .

    "! <p class="shorttext synchronized" lang="pt">Preenche dados de SHDB para processamento</p>
    METHODS process_shdb
      IMPORTING
        !im_data  TYPE tab_bdcdata
      EXPORTING
        ex_return TYPE bapiret2_t .


ENDCLASS .


CLASS class_report IMPLEMENTATION .


  METHOD constructor .

    IF ( lines( im_equi ) EQ 0 ) .
      RETURN .
    ENDIF .

    me->gt_equi = im_equi .

  ENDMETHOD .

  METHOD get_data .

    CLEAR me->gt_outtab .

    IF ( lines( me->gt_equi ) EQ 0 ) .
      RETURN .
    ENDIF .

    me->process(
      EXPORTING percent  = 10
                message  = 'Buscando dados de Equipamento...' ) .

    SELECT equnr, objnr
      FROM equi
      INTO TABLE @DATA(lt_data)
     WHERE equnr IN @me->gt_equi .
    IF ( sy-subrc NE 0 ) .
      RETURN .
    ENDIF .

    SELECT equnr, spras, eqktx, eqktu
      FROM eqkt
      INTO TABLE @me->gt_outtab
       FOR ALL ENTRIES IN @lt_data
     WHERE equnr EQ @lt_data-equnr
       AND spras EQ @sy-langu .
    IF ( sy-subrc NE 0 ) .
      RETURN .
    ENDIF .

    SELECT j~objnr, j~stat, j~inact,
           t~istat, t~spras, t~txt04
      FROM jest AS j
      LEFT JOIN tj02t AS t
        ON j~stat EQ t~istat
      INTO TABLE @DATA(lt_status)
       FOR ALL ENTRIES IN @lt_data
     WHERE j~objnr EQ @lt_data-objnr
       AND j~inact EQ @abap_false
       AND t~spras EQ @sy-langu .

    " Informando status
    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

      ASSIGN me->gt_outtab[ equnr = <fs_data>-equnr ] TO FIELD-SYMBOL(<fs_out>) .
      IF ( <fs_out> IS NOT ASSIGNED ) .
        CONTINUE .
      ENDIF .

      <fs_out>-sttxt = REDUCE #( INIT s   TYPE string
                                  FOR l IN lt_status
                                WHERE ( objnr = <fs_data>-objnr )
                                 NEXT s = COND string(
                                            WHEN s = '' THEN l-txt04
                                            ELSE |{ s } / { l-txt04 }| ) ) .



      UNASSIGN <fs_out> .

    ENDLOOP .

  ENDMETHOD .


  METHOD show .

    DATA:
      columns TYPE REF TO cl_salv_columns_table,
      events  TYPE REF TO cl_salv_events_table,
      display TYPE REF TO cl_salv_display_settings.

    IF ( lines( me->gt_outtab ) EQ 0 ) .
      RETURN .
    ENDIF .

    TRY .

        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = me->salv_table
          CHANGING
            t_table      = me->gt_outtab
        ) .

        columns = salv_table->get_columns( ) .
        IF ( columns IS BOUND ) .
          columns->set_optimize( cl_salv_display_settings=>true ).
        ENDIF .

        events = me->salv_table->get_event( ).
        IF ( events IS BOUND ) .
          SET HANDLER me->on_user_command FOR events.
        ENDIF .

        salv_table->set_screen_status(
          pfstatus      = 'STANDARD_FULLSCREEN'
          report        = sy-repid
          set_functions = me->salv_table->c_functions_all
        ).


        display = salv_table->get_display_settings( ) .
        IF ( display IS BOUND ) .
          DATA(title) = CONV lvc_title( 'Corrigir status de Equipamento' ) .
          title = |{ title } ({ lines( me->gt_outtab ) } registros)| .
          display->set_list_header( title ).
          display->set_striped_pattern( cl_salv_display_settings=>true ) .
        ENDIF .

        salv_table->display( ).

      CATCH cx_salv_msg .

    ENDTRY.

  ENDMETHOD .


  METHOD process .

    IF ( percent IS INITIAL ) AND
       ( ( total IS INITIAL ) AND currency IS INITIAL ) .
      RETURN .
    ENDIF .

    IF ( percent IS NOT INITIAL ) .
      DATA(percentage) = percent .
    ELSE .
      percentage = 10 .
    ENDIF .

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = percentage  " Size of Bar ( 0 <= PERCENTAGE <= 100 )
        text       = message.    " Text to be Displayed

  ENDMETHOD .


  METHOD on_user_command .

    BREAK ex135415 .

    CASE e_salv_function .
      WHEN '' .

      WHEN OTHERS .

    ENDCASE .

  ENDMETHOD .


  METHOD confirm .

    CONSTANTS:
      lc_sim TYPE char1 VALUE '' .

    DATA:
      answer TYPE char1 .

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'            " Title of dialog box
*       diagnose_object       = space            " Diagnosis text (maintain via SE61)
        text_question         = 'Tem certeza que deseja processar todos os itens listados?'                 " Question text in dialog box
        text_button_1         = TEXT-c01         " Text on the first pushbutton
*       icon_button_1         = space            " Icon on first pushbutton
        text_button_2         = TEXT-c02         " Text on the second pushbutton
*       icon_button_2         = space            " Icon on second pushbutton
        default_button        = '1'              " Cursor position
        display_cancel_button = ' '              " Button for displaying cancel pushbutton
*       userdefined_f1_help   = space            " User-Defined F1 Help
*       start_column          = 25               " Column in which the POPUP begins
*       start_row             = 6                " Line in which the POPUP begins
*       popup_type            =                  " Icon type
*       iv_quickinfo_button_1 = space            " Quick Info on First Pushbutton
*       iv_quickinfo_button_2 = space            " Quick Info on Second Pushbutton
      IMPORTING
        answer                = answer                 " Return values: '1', '2', 'A'
*      TABLES
*       parameter             =                  " Text transfer table for parameter in text
      EXCEPTIONS
        text_not_found        = 1                " Diagnosis text not found
        OTHERS                = 2.

    IF ( sy-subrc EQ 0 ) AND
       ( answer EQ lc_sim ) .
      rv_result = abap_on .
    ELSE .
      rv_result = abap_off .
    ENDIF .


  ENDMETHOD .


  METHOD create_shdb .

    CLEAR ex_data .

    IF ( im_equi IS INITIAL ).
      RETURN .
    ENDIF .

*          REFRESH up_bdc.
*          CLEAR up_bdc.
*          up_bdc-program  = 'SAPMFDTA'.
*          up_bdc-dynpro   = '100'.
*          up_bdc-dynbegin = 'X'.
*          APPEND up_bdc.
*          CLEAR up_bdc.
*          up_bdc-fnam     = 'REGUT-RENUM'.
*          up_bdc-fval     = tab_ausgabe-renum.
*          APPEND up_bdc.
*          CLEAR up_bdc.
*          up_bdc-fval     = '/8'.
*          up_bdc-fnam     = 'BDC_OKCODE'.
*          APPEND up_bdc.
*          CLEAR up_bdc.
*          up_bdc-program  = 'SAPMFDTA'.
*          up_bdc-dynpro   = '200'.
*          up_bdc-dynbegin = 'X'.
*          APPEND up_bdc.
*          CLEAR up_bdc.
*          up_bdc-fval     = '/BDA'.
*          up_bdc-fnam     = 'BDC_OKCODE'.
*          APPEND up_bdc.
*          CALL TRANSACTION up_fdta USING up_bdc MODE 'E'.


    ex_data = VALUE #(
      ( program  = 'SAPMIEQ0'
        dynpro   = '0101'
        dynbegin = 'X' )
      ( fnam     = '/00'
        fval     = 'BDC_OKCODE' )
      ( fnam     = 'RM63E-EQUNR'
        fval     = CONV #( |{ im_equi ALPHA = OUT }| ) )

      ( fnam     = '=MV'
        fval     = 'BDC_OKCODE' )

      ( program  = 'SAPMIEQ0'
        dynpro   = '1800'
        dynbegin = 'X' )

      ( fnam     = '=MVA'
        fval     = 'BDC_OKCODE' )

      ( fnam     = 'RISA0-V_OF_STOCK'
        fval     = 'X' )

      ( fnam     = 'X'
        fval     = '' )

      ( fnam     = '=MVA'
        fval     = 'BDC_OKCODE' )

      ( fnam     = 'X'
        fval     = 'RISA0-V_TO_STOCK' )

      ( fnam     = '=BU'
        fval     = 'BDC_OKCODE' )

    ).


  ENDMETHOD .


  METHOD process_shdb .

    CLEAR ex_return .

    IF ( im_data IS INITIAL ).
      RETURN .
    ENDIF .

  ENDMETHOD .


ENDCLASS .

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
  s_equnr  FOR  equi-equnr OBLIGATORY .
SELECTION-SCREEN END OF BLOCK b1.


INITIALIZATION .

START-OF-SELECTION .

  DATA(obj) = NEW class_report( im_equi = s_equnr[] ) .
  IF ( obj IS BOUND ) .
    obj->get_data( ) .
  ENDIF.

end-OF-SELECTION .

  IF ( obj IS BOUND ) .
    obj->show( ) .
  ENDIF.