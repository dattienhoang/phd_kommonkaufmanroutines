;This routine allows a user to set the parameters for the
;'BPass' routine by Weeks/Crocker.


;**************************************************************

;If a change in slider value is detected, the new value will be
;saved
PRO SMF_BPass_Events, event
Widget_Control, event.top, Get_UValue=info, /No_Copy

Widget_Control, event.id, Get_UValue=slider
Widget_Control, event.id, Get_Value=value

CASE slider OF
    'PARTICLE':  BEGIN
             value =  (*info.BP_N+1) > value  ;Keep noise smaller than particle size
             *info.BP_P=value
             ENDCASE
    'NOISE':  BEGIN ;Remember, slider is multiplied by 10 to give precision of 0.1
          value =  *info.BP_P*10 < value  ;Keep noise smaller than particle size
                *info.BP_N = (0.1 * value)
                Widget_Control, info.labelID, Set_Value= 'Noise Size: ' +String(value/10.0,Format='(F5.1)')
             ENDCASE
ENDCASE

Widget_Control, event.top, Set_UValue=info, /No_Copy
END




;********************************
;If display is selected, show the BPass'ed image.  Otherwise close the widget.
;If 'cancel' is selected, then the BPass parameters will revert to their
;initial settings and the original image will be returned.  For  the accept button
;the filtered Image is written to the pointer before closing the widget
PRO SMF_BPass_Buttons, event
Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, event.id, Get_UValue=button

CASE button OF
    'Cancel' :  BEGIN
                *info.BPImage=info.PreviousTry
                *info.BP_P=info.DefaultP
                *info.BP_N=info.DefaultN
                *info.Cancelled = 1
                Widget_Control, event.top, /Destroy
                ENDCASE
    'Accept' :  BEGIN
                *info.BPImage = TKH_Convolve_Filter(info.image, *info.BP_N, *info.BP_P)
                Widget_Control, event.top, /Destroy
                ENDCASE
    'Display':  BEGIN
                *info.BPImage = TKH_Convolve_Filter(info.image, *info.BP_N, *info.BP_P)
                Erase
                TVSCL, *info.BPImage
                Widget_Control, event.top, Set_UValue=info, /No_Copy
                ENDCASE
ENDCASE

END


;***********************************
PRO SMF_BPass_Cleanup, event
;Nothing to cleanup this time
END

;***************************************
;Here is the main part of the program including widget definition module

FUNCTION SMF_BPass, Image, $                 ;Image to be Filtered
              BPImage,  $                    ;Filtered image to be returned
              BPParam=BPParam, $             ;Default noise size can be loaded byuser
              PreviousTry=PreviousTry, $     ;A previous BPass image can also be loaded
              GROUP_LEADER=Group_Leader


;Make sure there is a 2D image that we are dealing with
Dim_Image = Size(image, /N_Dimensions)
IF Dim_Image NE 2 THEN Message, 'Image must be a 2D array', /NoName

;If default values supplied, apply them
IF N_Elements(BPParam) EQ 0 THEN BEGIN
DefaultP=5
DefaultN=1.0
ENDIF ELSE BEGIN
DefaultP = BPParam.PSize
DefaultN = BPParam.NSize
ENDELSE

;If there exists a previously aquired BPImage, then save that as a backup
IF N_Elements(PreviousTry) NE 0 THEN BEGIN
    Dim_BPImage = Size(PreviousTry, /N_Dimensions)
    IF Dim_BPImage NE 2 THEN Message, 'PreviousTry image must be a 2D array', /NoName
    previousTry = PreviousTry
ENDIF ELSE previousTry = image


S_Image=Size(image)


;Define widgets
;
;Sliders only give integer values, so we will multiply the noise slider scale
;by ten, so that it has a precision of 0.1.
tlb = Widget_Base(column=1, Title='Seledct Band Pass Parameters', $
                        /Modal, Group_Leader = Group_Leader)
slider = Widget_Slider(tlb, Value=DefaultP, Min = 3, Max = (S_Image[1]-1)/2, $
         Title = 'Particle Size', UValue='PARTICLE')
slider = Widget_Slider(tlb, Value=Fix(DefaultN*10), Min = 0, Max = 10*(S_Image[2]-1)/4, $
         UValue='NOISE', /Suppress_Value)
labelID = Widget_Label(tlb, xsize = 150, Value='Noise Size: ' + String(DefaultN, Format='(F5.1)'))

ButtonWidth = 75
buttonBase = Widget_Base(tlb, row=1, Event_Pro = 'SMF_BPAss_Buttons')
Accept_button = Widget_Button(buttonBase, Value = 'Accept', UValue = 'Accept', xsize = ButtonWidth)
button = Widget_Button(buttonBase, Value = 'Display', UValue = 'Display', $
                   xsize = ButtonWidth)
button = Widget_Button(buttonBase, Value = 'Cancel', UValue = 'Cancel', xsize = ButtonWidth)

;Realize widgets
Widget_Control, tlb, /realize

;Create pointer to hold BPass filtered image
ptr=Ptr_New(Image)
ptr_P =Ptr_New(DefaultP)
ptr_N =Ptr_New(DefaultN)
ptr_C = Ptr_New(0)

;Create structure to carry all the useful information around
info = {Image:Image, $
       BPImage:ptr, $
       BP_P:ptr_P, $
       BP_N:ptr_N, $
       DefaultP:DefaultP, $
       DefaultN:DefaultN, $
       Cancelled:ptr_C, $
       PreviousTry:PreviousTry, $
       labelID:labelID}


;Store pointer in TLB Uvalue
Widget_Control, tlb, Set_UValue = info, /No_Copy

;Place keyboard focus on 'Accept' button
Widget_Control, Accept_button, /input_Focus

;Register the widgets and end the widget definition module
XManager, 'SMF_BPass', tlb, Event_Handler = 'SMF_BPass_Events', $
       Cleanup = 'SMF_BPass_Cleanup'

;Package the BPass parameters and the filtered image as a structure and return it to the user

BPResult = {BPImage:*ptr, PSize:*ptr_P, NSize:*ptr_N, Cancelled:*Ptr_C}

Ptr_Free, ptr
Ptr_Free, ptr_P
Ptr_Free, ptr_N
Ptr_Free, ptr_C


RETURN, BPResult

END
