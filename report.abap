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

    METHODS constructor
      IMPORTING
        !im_equi TYPE range_t_equnr .

    METHODS get_data .

    METHODS show .

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
      gt_equi   TYPE range_t_equnr,
      gt_outtab TYPE tab_out.


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
*      AND j~inact EQ @abap_false
       AND t~spras EQ @sy-langu .

    " Informando status
    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

      ASSIGN me->gt_outtab[ equnr = <fs_data>-equnr ] TO FIELD-SYMBOL(<fs_out>) .
      IF ( <fs_out> IS NOT ASSIGNED ) .
        CONTINUE .
      ENDIF .

      <fs_out>-sttxt = REDUCE #( INIT s type ilom_sttxs
                                      sep = ''
                                  FOR l IN lt_status
                                WHERE ( objnr = <fs_data>-objnr )
                                 NEXT s = s && sep && l-txt04 sep = space ).

      UNASSIGN <fs_out> .

    ENDLOOP .

  ENDMETHOD .


  METHOD show .

    DATA:
      salv_table TYPE REF TO cl_salv_table,
      columns    TYPE REF TO cl_salv_columns_table,
      display    TYPE REF TO cl_salv_display_settings.

    IF ( lines( me->gt_outtab ) EQ 0 ) .
      RETURN .
    ENDIF .

    TRY .

        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = salv_table
          CHANGING
            t_table      = me->gt_outtab
        ) .

        columns = salv_table->get_columns( ) .
        IF ( columns IS BOUND ) .
          columns->set_optimize( cl_salv_display_settings=>true ).
        ENDIF .

        salv_table->set_screen_status(
          pfstatus      = 'STANDARD_FULLSCREEN'
          report        = 'SAPLKKBL'
          set_functions = salv_table->c_functions_all
        ).

        display = salv_table->get_display_settings( ) .
        IF ( display IS BOUND ) .
          display->set_striped_pattern( cl_salv_display_settings=>true ) .
        ENDIF .

        salv_table->display( ).

      CATCH cx_salv_msg .
*        catch cx_salv_not_found .
*        catch cx_salv_existing .
*        catch cx_salv_data_error .
*        catch cx_salv_object_not_found .

    ENDTRY.

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