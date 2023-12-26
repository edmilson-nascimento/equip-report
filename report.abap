*&---------------------------------------------------------------------*
*& Report /yga/fix_equi_status
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /yga/fix_equi_status.

TABLES:
  equi, jcds .


CLASS class_report DEFINITION .

  PUBLIC SECTION .

    "! <p class="shorttext synchronized" lang="pt">Configuraçoes iniciais</p>
    CLASS-METHODS initial .
    "! <p class="shorttext synchronized" lang="pt">Convert status para o valor interno</p>
    CLASS-METHODS get_stat
      IMPORTING
        !im_stat         TYPE tj02t-txt04
      RETURNING
        VALUE(rv_result) TYPE tj02t-istat .
    "! <p class="shorttext synchronized" lang="pt">Metodo construtor</p>
    METHODS constructor
      IMPORTING
        !im_equi  TYPE range_t_equnr
        !im_udate TYPE trg_date
        !im_lidi  TYPE tj02t-istat
        !im_deps  TYPE tj02t-istat .
    "! <p class="shorttext synchronized" lang="pt">Retorna TRUE quando se tem o filtro valido</p>
    METHODS is_valid_filter
      RETURNING
        VALUE(result) TYPE sap_bool .
    "! <p class="shorttext synchronized" lang="pt">Busca dados de equipamentos</p>
    METHODS get_data .
    "! <p class="shorttext synchronized" lang="pt">Retorna TRUE caso existam dados para exibição/processamento</p>
    METHODS has_data
      RETURNING
        VALUE(result) TYPE sap_bool .
    "! <p class="shorttext synchronized" lang="pt">Exibe dados encontrados</p>
    METHODS show .
    "! <p class="shorttext synchronized" lang="pt">Exibe status de processamento</p>
    METHODS progress
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
      tab_out TYPE TABLE OF ty_out,

      BEGIN OF ty_equi,
        equnr TYPE equi-equnr,
        objnr TYPE equi-objnr,
      END OF ty_equi,
      tab_equi TYPE SORTED TABLE OF ty_equi
               WITH UNIQUE KEY equnr,
      BEGIN OF ty_eqkt,
        equnr TYPE eqkt-equnr,
        spras TYPE eqkt-spras,
        eqktx TYPE eqkt-eqktx,
        eqktu TYPE eqkt-eqktu,
      END OF ty_eqkt,
      tab_eqkt TYPE SORTED TABLE OF ty_eqkt
               WITH UNIQUE KEY equnr spras,
      BEGIN OF ty_status,
        objnr TYPE jest-objnr,
        stat  TYPE jest-stat,
        inact TYPE jest-inact,
        istat TYPE tj02t-istat,
        spras TYPE tj02t-spras,
        txt04 TYPE tj02t-txt04,
      END OF ty_status,
      tab_status TYPE STANDARD TABLE OF ty_status
                  WITH DEFAULT KEY .

    CONSTANTS:
      gc_package_size TYPE i VALUE 2500 .

    DATA:
      salv_table  TYPE REF TO cl_salv_table,
      gt_equi     TYPE range_t_equnr,
      gt_udate    TYPE trg_date,
      gv_lidi     TYPE tj02t-istat,
      gv_deps     TYPE tj02t-istat,
      gt_messages TYPE bapiret2_t,
      gt_outtab   TYPE tab_out,
      gv_cursor   TYPE cursor.

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
    "! <p class="shorttext synchronized" lang="pt">Executa o processamento dos itens listados</p>
    METHODS process .
    "! <p class="shorttext synchronized" lang="pt">Preenche dados de SHDB para processamento</p>
    METHODS process_shdb
      IMPORTING
        !im_data  TYPE tab_bdcdata
      EXPORTING
        ex_return TYPE bapiret2_t .
    "! <p class="shorttext synchronized" lang="pt">Retorna as mensagens no formato BAPIRET2_T</p>
    METHODS convert_message
      IMPORTING
        !im_data         TYPE tab_bdcmsgcoll
      RETURNING
        VALUE(rt_result) TYPE bapiret2_t .
    "! <p class="shorttext synchronized" lang="pt">Retorna dados de Equipamentos</p>
    METHODS get_equi
      RETURNING
        VALUE(rt_result) TYPE tab_equi .
    "! <p class="shorttext synchronized" lang="pt">Retorna descrição de Equipamentos</p>
    METHODS get_desc
      IMPORTING
        !im_data         TYPE tab_equi
      RETURNING
        VALUE(rt_result) TYPE tab_eqkt .
    "! <p class="shorttext synchronized" lang="pt">Retorna Status de Equipamentos</p>
    METHODS get_status
      IMPORTING
        !im_data         TYPE tab_equi
      RETURNING
        VALUE(rt_result) TYPE tab_status .

ENDCLASS .


CLASS class_report IMPLEMENTATION .


  METHOD initial .

    LOOP AT SCREEN .
      IF ( screen-group1 EQ 'P1' ) .
        screen-input = 0 .
        MODIFY SCREEN .
      ENDIF .
    ENDLOOP.


    " Restrict select-options
    DATA(ls_restrict) = VALUE sscr_restrict(
        opt_list_tab = VALUE #(
          ( name       = 'OBJECTKEY1'
            options-bt = abap_on )
        )
        ass_tab      = VALUE #(
          ( kind    = 'S'
            name    = 'S_UDATE'
            sg_main = 'I'
            sg_addy = space
            op_main = 'OBJECTKEY1' )
        )
    ).

    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING
        restriction = ls_restrict .

  ENDMETHOD .


  METHOD get_stat .

    IF ( im_stat IS INITIAL ) .
      RETURN .
    ENDIF .

    SELECT istat, spras, txt04
     UP TO 1 ROWS
      FROM tj02t
      INTO @DATA(ls_data)
     WHERE spras EQ @sy-langu
       AND txt04 EQ @im_stat .
    ENDSELECT .

    rv_result = COND #( WHEN sy-subrc EQ 0
                        THEN ls_data-istat
                        ELSE space ) .

  ENDMETHOD .


  METHOD constructor .

    IF ( lines( im_equi ) EQ 0 ) .
      RETURN .
    ENDIF .

    me->gt_equi = im_equi .
    me->gv_lidi = im_lidi .
    me->gv_deps = im_deps .

  ENDMETHOD .


  METHOD is_valid_filter .

    IF ( lines( me->gt_equi ) GT 0 ) .
      result = abap_on .
      RETURN .
    ENDIF .

    IF ( lines( me->gt_udate ) GT 0 ) .
      result = abap_on .
    ENDIF .

  ENDMETHOD .


  METHOD get_data .

    CLEAR me->gt_outtab .

    IF ( lines( me->gt_equi ) EQ 0 ) .
      RETURN .
    ENDIF .

    me->progress(
      EXPORTING percent  = 10
                message  = CONV #( |{ 'Obter dados de Equipamentos...'(m01) }| ) ).

    DATA(lt_data) = me->get_equi( ) .

    me->progress(
      EXPORTING percent  = 45
                message  = CONV #( |{ 'Obter descrição de Equipamentos...'(m02) }| ) ).

    DATA(lt_desc) = me->get_desc( lt_data ) .

    me->progress(
      EXPORTING percent  = 70
                message  = CONV #( |{ 'Obter Status de Equipamentos...'(m03) }| ) ).

    DATA(lt_status) = me->get_status( lt_data ) .

    me->progress(
      EXPORTING percent  = 85
                message  = CONV #( |{ 'Processar Status de Equipamentos...'(m04) }| ) ) .

    " Informando status
    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

      DATA(lv_message) = CONV char50( |{ 'Processar'(m09) } { sy-tabix }| ) .
      lv_message = |{ lv_message } { 'de'(m10) } { lines( lt_data ) }{ '...'(m11) }| .

      me->progress(
        total    = lines( lt_data )
        currency = sy-tabix
        message  = lv_message ) .

      DATA(description) = VALUE #( lt_desc[ equnr = <fs_data>-equnr ] OPTIONAL ) .

      " Aplicando filtor por status
      IF ( line_exists( lt_status[ objnr = <fs_data>-objnr
                                   stat  = me->gv_lidi ] ) )
         AND
        ( line_exists( lt_status[ objnr = <fs_data>-objnr
                                   stat  = me->gv_deps ] ) ) .
      ELSE .
        CONTINUE .
      ENDIF .

      DATA(current_sttxt) = REDUCE itobattr-sttxt(
        INIT s   TYPE string
         FOR l IN lt_status
       WHERE ( objnr = <fs_data>-objnr )
        NEXT s = COND string( WHEN s = ''
                              THEN l-txt04
                              ELSE |{ s } / { l-txt04 }| ) ) .

      APPEND VALUE #( equnr = <fs_data>-equnr
                      spras = description-spras
                      eqktx = description-eqktx
                      eqktu = description-eqktu
                      sttxt = current_sttxt )
          TO me->gt_outtab .

      CLEAR:
        description, current_sttxt .

    ENDLOOP .

  ENDMETHOD .


  METHOD has_data .

    result = COND #(
      WHEN lines( me->gt_outtab ) EQ 0
      THEN abap_off
      ELSE abap_on
    ).

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
          DATA(title) = CONV lvc_title( 'Corrigir status de Equipamento'(m05) ) .
          title = COND #( WHEN lines( me->gt_outtab ) GT 1
                          THEN |{ title } ({ lines( me->gt_outtab ) } { 'registros'(m06) })|
                          ELSE |{ title } { '(1 registro)'(m07) }| ) .
          display->set_list_header( title ).
          display->set_striped_pattern( cl_salv_display_settings=>true ) .
        ENDIF .

        salv_table->display( ) .

      CATCH cx_salv_msg .

    ENDTRY.

  ENDMETHOD .


  METHOD progress .

    DATA:
      percentage TYPE i .

    " Não sera exibido quando for em background
    IF ( sy-batch EQ abap_true ) .
      RETURN .
    ENDIF .

    IF ( percent IS INITIAL ) AND
       ( ( total IS INITIAL ) AND currency IS INITIAL ) .
      RETURN .
    ENDIF .

    IF ( percent IS NOT INITIAL ) .
      percentage = percent .
    ELSE .
      percentage = ( currency * 100 ) / total.
    ENDIF .

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = percentage  " Size of Bar ( 0 <= PERCENTAGE <= 100 )
        text       = message.    " Text to be Displayed

  ENDMETHOD .


  METHOD on_user_command .

    CASE e_salv_function .

      WHEN 'RUN' .
        IF ( me->confirm( ) EQ abap_true ) .
          me->process( ) .
          me->get_data( ) .
          me->salv_table->refresh( ) .
        ENDIF .

      WHEN OTHERS .

    ENDCASE .

  ENDMETHOD .


  METHOD confirm .

    CONSTANTS:
      lc_sim TYPE char1 VALUE '1' .

    DATA:
      answer TYPE char1 .

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = TEXT-000         " Title of dialog box
*       diagnose_object       = space            " Diagnosis text (maintain via SE61)
        text_question         = TEXT-c00         " Question text in dialog box
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


  METHOD process .

    DATA:
      lt_return TYPE bapiret2_t .

    CLEAR me->gt_messages .

    IF ( lines( me->gt_outtab ) EQ 0 ) .
      RETURN .
    ENDIF .

    me->progress( EXPORTING percent  = 10
                            message  = 'Processar Equipamentos...'(m08)
    ).

    LOOP AT me->gt_outtab ASSIGNING FIELD-SYMBOL(<fs_data>).

      DATA(lv_message) = CONV char50( |{ 'Processar'(m09) } { sy-tabix }| ) .
      lv_message = |{ lv_message } { 'de'(m10) } { lines( me->gt_outtab ) }{ '...'(m11) }| .

      me->progress(
        EXPORTING
          total    = lines( me->gt_outtab )
          currency = sy-tabix
          message  = lv_message
      ).

      me->create_shdb(
        EXPORTING
          im_equi = <fs_data>-equnr
        IMPORTING
          ex_data = DATA(lt_data)
      ).

      IF ( lines( lt_data ) EQ 0 ) .
        CONTINUE .
      ENDIF .

      me->process_shdb(
        EXPORTING
          im_data   = lt_data
        IMPORTING
          ex_return = lt_return
      ).

      " Exibe a mensagem de erro que foi retornada
      IF ( line_exists( lt_return[ type = if_xo_const_message=>error ] ) ) .
        me->gt_messages = VALUE #( BASE me->gt_messages ( LINES OF lt_return ) ) .
        CONTINUE .
      ENDIF .

      " Mantem mensagem de sucesso, caso existe uma
      IF ( line_exists( lt_return[ type = if_xo_const_message=>success ] ) ) .
        me->gt_messages = VALUE #( BASE me->gt_messages ( LINES OF lt_return ) ) .
        CONTINUE .
      ENDIF .

      " Caso de processamento que não tenha mensagem de erro e possa retornar vazio
      APPEND VALUE bapiret2(
        type       = if_xo_const_message=>success
        id         = 'IS'
        number     = 817
        message_v1 = CONV #( |{ <fs_data>-equnr ALPHA = OUT }| ) )
      TO me->gt_messages .

    ENDLOOP .

    IF ( lines( me->gt_messages ) GT 0 ) .
      CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
        EXPORTING
          it_message = me->gt_messages. " BAPI Return Table
    ENDIF .

  ENDMETHOD .


  METHOD create_shdb .

    CLEAR ex_data .

    IF ( im_equi IS INITIAL ).
      RETURN .
    ENDIF .

    ex_data = VALUE #(
      ( program  = 'SAPMIEQ0'
        dynpro   = '0100'
        dynbegin = 'X' )
      ( fnam     = 'RM63E-EQUNR'
        fval     = CONV #( |{ im_equi ALPHA = OUT }| ) )
      ( fnam     = 'BDC_OKCODE'
        fval     = '/00' )

      ( program  = 'SAPMIEQ0'
        dynpro   = '0101'
        dynbegin = 'X' )
      ( fnam     = 'BDC_OKCODE'
        fval     = '=MV' )

      ( program  = 'SAPMIEQ0'
        dynpro   = '1800'
        dynbegin = 'X' )
      ( fnam     = 'BDC_OKCODE'
        fval     = '=MVA' )
      ( fnam     = 'RISA0-V_OF_STOCK'
        fval     = 'X' )

      ( program  = 'SAPMIEQ0'
        dynpro   = '0101'
        dynbegin = 'X' )
      ( fnam     = 'BDC_OKCODE'
        fval     = '=MV' )

      ( program  = 'SAPMIEQ0'
        dynpro   = '1800'
        dynbegin = 'X' )
      ( fnam     = 'BDC_OKCODE'
        fval     = '=MVA' )
      ( fnam     = 'RISA0-V_TO_STOCK'
        fval     = 'X' )

      ( program  = 'SAPMIEQ0'
        dynpro   = '0101'
        dynbegin = 'X' )
      ( fnam     = 'BDC_OKCODE'
        fval     = '=BU' )
    ).

  ENDMETHOD .


  METHOD process_shdb .

    DATA:
*     dismode    TYPE ctu_params-dismode VALUE 'A', "Exibir telas
      dismode    TYPE ctu_params-dismode VALUE 'N', "Background
*     updmode    TYPE ctu_params-updmode VALUE 'S', "Sincrono
      updmode    TYPE ctu_params-updmode VALUE 'A', "Assincrono
      lt_message TYPE tab_bdcmsgcoll.

    CLEAR ex_return .

    IF ( im_data IS INITIAL ).
      RETURN .
    ENDIF .

    DATA(ls_options) = VALUE ctu_params(
      dismode  = dismode
      updmode  = updmode
      cattmode = abap_off
      defsize  = abap_off
      racommit = abap_on
      nobinpt  = abap_on
      nobiend  = abap_off
    ) .

    CALL TRANSACTION 'IE02'
               USING im_data
             OPTIONS FROM ls_options
            MESSAGES INTO lt_message .

    ex_return = me->convert_message( lt_message ) .

  ENDMETHOD .


  METHOD convert_message .

    IF ( lines( im_data ) EQ 0 ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
      TABLES
        imt_bdcmsgcoll = im_data
        ext_return     = rt_result.

  ENDMETHOD .



  METHOD get_equi .

    TYPES:
      BEGIN OF ty_status,
        objnr TYPE jcds-objnr,
        stat  TYPE jcds-stat,
        chgnr TYPE jcds-chgnr,
        usnam TYPE jcds-usnam,
        udate TYPE jcds-udate,
        utime TYPE jcds-utime,
        tcode TYPE jcds-tcode,
      END OF ty_status,
      tab_statys TYPE STANDARD TABLE OF ty_status.

    DATA:
      lt_status TYPE tab_status .

    IF ( lines( me->gt_equi )  EQ 0 ) AND
       ( lines( me->gt_udate ) EQ 0 ) .
      RETURN .
    ENDIF .

    " Filtro apenas por Data de modificação
    IF ( lines( me->gt_equi )  EQ 0 ) AND
       ( lines( me->gt_udate ) GT 0 ) .

      TRY .
          OPEN CURSOR WITH HOLD @me->gv_cursor FOR
          SELECT objnr, stat, chgnr, usnam, udate, utime, tcode
            FROM jcds
           WHERE udate IN @me->gt_udate .
          DO .
            FETCH NEXT CURSOR @gv_cursor
            APPENDING TABLE @lt_status PACKAGE SIZE @me->gc_package_size .

            IF ( sy-subrc NE 0 ).
              EXIT.
            ENDIF.

            DATA(message) = CONV char50( |{ lines( rt_result ) } Equip. recuperados...| ) .
            me->progress( percent  = 10
                          message  = message ).
          ENDDO .
          CLOSE CURSOR me->gv_cursor.
        CATCH cx_sy_open_sql_db .
      ENDTRY.

*    me->gt_equi = VALUE range_t_equnr(
*      FOR l IN im_data
*      ( sign   = rsmds_c_sign-including
*        option = rsmds_c_option-equal
*        low    = l-equnr )
*    ).
*

    ENDIF .

    TRY .
        OPEN CURSOR WITH HOLD @me->gv_cursor FOR

        SELECT equnr, objnr
          FROM equi
         WHERE equnr IN @me->gt_equi .

        DO .
          FETCH NEXT CURSOR @gv_cursor
          APPENDING TABLE @rt_result PACKAGE SIZE @me->gc_package_size .

          IF ( sy-subrc NE 0 ).
            EXIT.
          ENDIF.

          message = |{ lines( rt_result ) } Equip. recuperados...| .
          me->progress( percent  = 10
                        message  = message ).
        ENDDO .

        CLOSE CURSOR me->gv_cursor.
      CATCH cx_sy_open_sql_db .
    ENDTRY.


  ENDMETHOD .



  METHOD get_desc .


    IF ( lines( im_data ) EQ 0 ) .
      RETURN .
    ENDIF .

    DATA(lr_equi) = VALUE range_t_equnr(
      FOR l IN im_data
      ( sign   = rsmds_c_sign-including
        option = rsmds_c_option-equal
        low    = l-equnr )
    ).

    TRY .
        OPEN CURSOR WITH HOLD @me->gv_cursor FOR

        SELECT equnr, spras, eqktx, eqktu
          FROM eqkt
         WHERE equnr IN @lr_equi
           AND spras EQ @sy-langu .

        DO .
          FETCH NEXT CURSOR @me->gv_cursor
          APPENDING TABLE @rt_result PACKAGE SIZE @me->gc_package_size .

          IF ( sy-subrc NE 0 ).
            EXIT.
          ENDIF.

          DATA(message) = CONV char50( |{ lines( rt_result ) } Reg. desc. recuperados...| ) .
          me->progress( percent  = 50
                        message  = message ).
        ENDDO .

        CLOSE CURSOR me->gv_cursor.
      CATCH cx_sy_open_sql_db .
    ENDTRY.

  ENDMETHOD .


  METHOD get_status .

    IF ( lines( im_data ) EQ 0 ) .
      RETURN .
    ENDIF .

    DATA(lr_equi) = VALUE range_t_equnr(
      FOR l IN im_data
      ( sign   = rsmds_c_sign-including
        option = rsmds_c_option-equal
        low    = l-equnr )
    ).

    TRY .
        OPEN CURSOR WITH HOLD @me->gv_cursor FOR

        SELECT j~objnr, j~stat, j~inact,
               t~istat, t~spras, t~txt04
          FROM jest AS j
          LEFT JOIN tj02t AS t
            ON j~stat EQ t~istat
         WHERE j~objnr IN @lr_equi
           AND j~inact EQ @abap_false
           AND t~spras EQ @sy-langu .

        DO .
          FETCH NEXT CURSOR @me->gv_cursor
          APPENDING TABLE @rt_result PACKAGE SIZE @me->gc_package_size .

          IF ( sy-subrc NE 0 ).
            EXIT.
          ENDIF.

          DATA(message) = CONV char50( |{ lines( rt_result ) } Reg. Status recuperados...| ) .
          me->progress( percent  = 75
                        message  = message ).
        ENDDO .

        CLOSE CURSOR @me->gv_cursor.
      CATCH cx_sy_open_sql_db .
    ENDTRY.

  ENDMETHOD .


ENDCLASS .

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
  s_equnr  FOR  equi-equnr OBLIGATORY,
  s_udate  FOR  jcds-udate OBLIGATORY.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS:
  p_deps TYPE tj02t-txt04 MODIF ID p1 DEFAULT 'DEPS',
  p_lidi TYPE tj02t-txt04 MODIF ID p1 DEFAULT 'LIDI'.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF BLOCK b1.


INITIALIZATION .
  class_report=>initial( ) .

AT SELECTION-SCREEN OUTPUT.


START-OF-SELECTION .

  DATA(obj) =
    NEW class_report( im_equi  = s_equnr[]
                      im_udate = s_udate[]
                      im_lidi  = class_report=>get_stat( p_lidi )
                      im_deps  = class_report=>get_stat( p_deps ) ) .
  IF ( obj IS BOUND ) .

    IF ( obj->is_valid_filter( ) EQ abap_false ) .
      MESSAGE i880(/yga/jump) .
      LEAVE LIST-PROCESSING.
    ENDIF .

    obj->get_data( ) .

  ENDIF.

end-OF-SELECTION .

  IF ( obj IS BOUND ) .

    IF ( obj->has_data( ) EQ abap_true ) .
      obj->show( ) .
    ELSE .
      MESSAGE i000(>0) WITH 'Não existem dados para o filtro informado.'(m12) .
    ENDIF .
  ENDIF.
  
  
  
*