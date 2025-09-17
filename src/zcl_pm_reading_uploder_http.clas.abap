class ZCL_PM_READING_UPLODER_HTTP definition
  public
  create public .

public section.

    TYPES  : BEGIN OF sty_01,

               Plant           TYPE STRing,
               Date1           TYPE string,
               Time            TYPE string,
               Counterreading(13)  TYPE P DECIMALS 2,
               Uom             TYPE string,
               Text            TYPE string,
               User1           TYPE string,
               Difference      TYPE STRING,
               MeasuringPoint(12)  TYPE n,



             END OF sty_01.

    CLASS-DATA: it_TAB  TYPE TABLE OF sty_01,
                wa_TAB  TYPE           sty_01.


 TYPES  : BEGIN OF sty_hed,
            TableData LIKE it_TAB ,
          END OF sty_hed .

    CLASS-DATA: wa_hed     TYPE  sty_hed .



  interfaces IF_HTTP_SERVICE_EXTENSION .
  INTERFACES if_oo_adt_classrun_out.
protected section.



private section.
ENDCLASS.



CLASS ZCL_PM_READING_UPLODER_HTTP IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.



     DATA(req) = request->get_form_fields(  ).


    response->set_header_field( i_name = 'Access-Control-Allow-Origin' i_value = '*' ).
    response->set_header_field( i_name = 'Access-Control-Allow-Credentials' i_value = 'true' ).

    DATA(body)  = request->get_text(  )  .
   xco_cp_json=>data->from_string( body )->write_to( REF #(  wa_hed ) ).

data json TYPE string.

LOOP AT wa_hed-TableData INTO DATA(wa_head).

*uper case wa_head-Uom.
TRANSLATE wa_head-Uom TO UPPER CASE.

DATA HJ TYPE STRING.

"CAST( HG FLTP )

 SELECT SINGLE  from @wa_hed-TableData   as a FIELDS
   cast( counterreading as FLTP )  as pick
 WHERE measuringpoint = @wa_head-measuringpoint
  into @data(prod_total).

       DATA(lv_DATE)  = wa_head-date1+0(4)  && wa_head-date1+5(2)  && wa_head-date1+8(2).
*
*       DATA(IV_DATE)  = wa_head-date1+0(2)  && wa_head-date1+3(2)  && wa_head-date1+6(4).
*************DATE MONTH YEAR************
*       DATA lastchangedate TYPE string.
*      lastchangedate = |{  IV_DATE+0(2) }-{  IV_DATE+2(2) }-{  IV_DATE+4(4) }|  .
***********************202040301***********************
*       DATA lastDATE TYPE string.
*      lastDATE = |{  wa_head-date1+6(4) }{  wa_head-date1+3(2) }{  wa_head-date1+0(2) }|  .
     DATA lv_measuringpoint TYPE string.
      lv_measuringpoint   = |{ wa_head-measuringpoint   ALPHA = IN }|.

MODIFY ENTITIES OF i_measurementdocumenttp_2
       ENTITY measurementdocument
        CREATE
          FIELDS ( measuringpoint
                   measurementcounterreading
                   measurementreadingentryuom
                   MsmtRdngDate
                   MeasurementDocumentText

                   )
          WITH VALUE #( (
                            %cid     = 'My%CID_1'
                           %data    = VALUE #( measuringpoint               = wa_head-measuringpoint
                                               measurementcounterreading    = prod_total
                                               measurementreadingentryuom = wa_head-Uom
                                               MsmtRdngDate = lv_DATE
                                               MeasurementDocumentText = wa_head-text

                                               ) ) )


       MAPPED   DATA(ls_mapped)
       FAILED   DATA(ls_failed)
       REPORTED DATA(ls_reported_modify).

    COMMIT ENTITIES BEGIN
      RESPONSE OF i_measurementdocumenttp_2
      FAILED   DATA(ls_save_failed)
      REPORTED DATA(ls_save_reported).

       IF ls_save_failed IS NOT INITIAL.
      DATA(message2) = ls_save_reported-measurementdocument[ 1 ]-%msg->if_message~get_text( ).
      DATA(msz_2) = |Error { message2 } |.
      json   =   msz_2.
    Else.


    LOOP AT ls_mapped-measurementdocument ASSIGNING FIELD-SYMBOL(<mapped_early>).
      CONVERT KEY OF i_measurementdocumenttp_2
      FROM <mapped_early>-measurementdocument
      TO DATA(ls_md_final_key).
    ENDLOOP.

 DATA(GRN)  = <mapped_early>-MeasurementDocument .
 data result type string .
 result = <mapped_early>-MeasurementDocument .
 json =  |Measurement Document Generated |.
  endif.

  COMMIT ENTITIES END.
data no TYPE string.
no = no + 1.
IF json IS NOT INITIAL.

CONCATENATE no json INTO json SEPARATED BY space .
data(json2) = json.
data(lv_production) =  json2 .
data : reponse TYPE string .
 CONCATENATE reponse ','  lv_production INTO reponse.
ENDIF.
clear : lv_production, json,prod_total .


ENDLOOP.

  response->set_text( reponse  )  .

  endmethod.


  METHOD IF_OO_ADT_CLASSRUN_OUT~main.

  ENDMETHOD.
ENDCLASS.
