;This routine allows a user to set the parameters for the
;'Feature' routine by Weeks/Crocker.
;It returns the 'Feature array' including five columns:
;1)X-Coordinate
;2)Y-Coordinate
;3)Brightness
;4)Radius of Gyration
;5)Eccentricity
;
;and also the Particle Size and Masscut parameters used to find those features.
;
;*******************************
;*******************************

;I should think of some general events that go here...
PRO SMF_Feature_Events, event
Widget_Control, event.top, Get_UValue=info, /No_Copy

Widget_Control, event.top, Set_UValue=info, /No_Copy
END

;***********************************************
;If a change in slider value is detected, the new value will be
;saved
PRO SMF_Feature_Slider, event
Widget_Control, event.top, Get_UValue=info, /No_Copy

Widget_Control, event.id, Get_UValue=slider
Widget_Control, event.id, Get_Value=value
CASE StrUpCase(slider) OF
    'PARTICLE':  (*info.Params).particleSize=value
    'MASSCUT': (*info.Params).masscut=value
    'SIZE' : info.radius = value
ENDCASE


Widget_Control, event.top, Set_UValue=info, /No_Copy
END

;********************************
;Event handler for button presses
PRO SMF_Feature_Buttons, event
Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, event.id, Get_UValue=button

CASE StrUpCase(button) OF
    'CANCEL' :  BEGIN
                *info.features = Replicate(-1, 5) ; Output dummy values
             ptr_Free, info.Params ;Erase any memory of parameters used
                 Widget_Control, event.top, /Destroy
                  RETURN
                ENDCASE

    'CIRCLE' : info.circle = 1

    'DISC' : info.circle = 0

    'ACCEPT' : BEGIN          ;Data is already in the pointers; just quit
                Widget_Control, event.top, /Destroy
                RETURN
               ENDCASE

;    'CALCULATE' : BEGIN
;
;
;                ENDCASE

    'DISPLAY' : BEGIN
               ;Check if the Masscut parameter was set above its default value. If so
               ;apply that keyword; otherwise, leave it off the command
               IF (*info.params).masscut LE 1 THEN $
                       *info.Features = Feature(info.image,(*info.Params).ParticleSize)$
               ELSE *info.Features = Feature(info.image,(*info.Params).ParticleSize, Masscut = (*info.Params).masscut)

              ;Extract maximum brightness of features and set that as the new Masscut maximum
              Widget_Control, info.MassSlider, Set_Slider_Max=0.95*Max((*info.Features)[2,*])
              Widget_Control, info.Histogram, Sensitive=1

           ;Display the number of features found; activate the masscut control slider
              Widget_Control, info.NumFeatures, Set_Value =  (Size(*info.Features))[2]
         Widget_Control, info.massSlider, sensitive=1
             ;Display position of features by overlaying circles unless there are too many
                Erase
                IF N_Elements(*info.features) GT 15000 THEN BEGIN
                   Widget_Control, info.Accept, Sensitive=0
                     Plot, (*info.features)[2,*], (*info.features)[1,*], psym=6, $
                     XTitle='Brightness', YTitle='Y-Coordinate'
                     ENDIF ELSE BEGIN
                       IF info.circle EQ 1 THEN BEGIN
                       SMF_Overlay_Circles, info.image , (*info.Features)[0:1,*], info.radius,$
                                       colorID=255
                       ENDIF ELSE SMF_Overlay_Circles, info.image , (*info.Features)[0:1,*], $
                               info.radius, colorID=255, /disc
                        Widget_Control, info.Accept, Sensitive=1
                    ENDELSE

             ENDCASE

    'HISTOGRAM' : BEGIN
                   Erase
                   IF ptr_Valid(info.features) THEN Plot, (*info.features)[2,*], $
                            (*info.features)[1,*], psym=6, XTitle='Brightness', $
                            YTitle='Y-Coordinate'
                ENDCASE

ENDCASE

Widget_Control, event.top, Set_UValue=info, /No_Copy
END





;***********************************
PRO SMF_Feature_Cleanup, event


END

;***************************************
;This function takes an image and returns an array of positions and
;magnitudes of bright spots with a size set by the user
FUNCTION SMF_Feature, Image, FeatureInfo, ParticleSize=ParticleSize, WID=WID, top=top,$
         MASSCUT=MASSCUT, _Extra=_Extra, GROUP_LEADER=Group_Leader

;Make sure there is a 2D image that we are dealing with
Dim_Image = Size(image, /N_Dimensions)
S_Image=Size(image)
IF Dim_Image NE 2 THEN Message, 'Image must be a 2D array', /NoName


;If default values supplied, apply them, otherwise set them to reasonable values
IF N_Elements(ParticleSize) NE 1 THEN ParticleSize = 5
IF ~Keyword_Set(MASSCUT) THEN masscut=0L
IF ~Keyword_Set(top) THEN top = !d.Table_Size-1

;This can be changed to remember the previous mass-cut intensity, but currently
;I am reinitializing it to zero everytime this subroutiine is called to
;avoid conflicts if the user has done some reprocessing
IF ~Keyword_Set(MASSCUT) THEN MaxIntensity=0 ELSE MaxIntensity=masscut
;MaxIntensity=0L
;masscut=0L

;Define widgets
tlb = Widget_Base(column=1, Title='Find Features', $
                        /modal, Group_Leader=Group_Leader)
slider = Widget_Slider(tlb, Value=particle, Min = 3, Max = (S_Image[1]-1)/2, $
         Title = 'Particle Size', UValue='PARTICLE', Event_Pro = 'SMF_Feature_Slider')
massSlider = Widget_Slider(tlb, Value= MaxIntensity, Min = 0, Max = (1 > MaxIntensity), $
         Title = 'Intensity Cutoff', UValue='MASSCUT', Event_Pro = 'SMF_Feature_Slider', $
         Sensitive=0)

NFeatures = CW_Field(tlb, frame=2, Title = 'Number of features found: ', /Integer,$
              /NoEdit, Value = 0, UValue='NumFeatures')


ButtonWidth = 125
buttonBase1 = Widget_Base(tlb, row=1, /align_center, Event_Pro = 'SMF_Feature_Buttons')
;button = Widget_Button(buttonBase1, Value = 'Calculate', UValue = 'Calculate', $
;                   xsize = ButtonWidth, sensitive=0)
Calc_button = Widget_Button(buttonBase1, Value = 'Calculate and Display', UValue = 'Display', $
                   xsize = ButtonWidth)
ButtonHistogram = Widget_Button(buttonBase1, Value = 'Display Histogram', UValue = 'Histogram', $
                   Sensitive= 0, xsize = ButtonWidth)


OverlayBase = Widget_Base(tlb, frame=2, column=1, /align_center, Event_Pro = 'SMF_Feature_Buttons')
label = Widget_Label(OverlayBase,frame=1, Value = 'Feature Overlay Parameters')
circlebutton = Widget_Base(OverlayBase, row=1, /align_center, /exclusive)
button1 = Widget_Button(circlebutton, Value='Disc', UValue='Disc')
button2 = Widget_Button(circlebutton, Value='Circle', UValue='Circle')

sizeSlider = Widget_Base(OverlayBase, row=1, /align_center)
Slider = Widget_Slider(sizeSlider, Title = 'Overlay Size', Value=5, UValue='Size',$
                 Event_Pro = 'SMF_Feature_Slider')


buttonBase3 = Widget_Base(tlb, row=1, /align_center, Event_Pro = 'SMF_Feature_Buttons')
buttonAccept = Widget_Button(buttonBase3, Value = 'Accept', UValue = 'Accept', $
                 Sensitive=0, xsize = ButtonWidth)
button = Widget_Button(buttonBase3, Value = 'Cancel', UValue = 'Cancel', xsize = ButtonWidth)

;Realize widgets, set initial radio button selection
Widget_Control, tlb, /realize
Widget_Control, button2, Set_Button=1

;Create pointers to hold information to be returned to the caller
ptr_Features =Ptr_New(/Allocate_Heap)
Params = {ParticleSize:ParticleSize, Masscut:Masscut}
ptr_Params=ptr_New(Params)

info = {Image:Image, $
       Params: ptr_Params, $
       top:top, $
       MaxIntensity:MaxIntensity, $
       Features:ptr_Features, $
       radius:5, $
       circle:1, $
       Histogram: ButtonHistogram, $
       Accept: ButtonAccept, $
       MassSlider:MassSlider, $
       NumFeatures:NFeatures }

;Store pointer in TLB Uvalue
Widget_Control, tlb, Set_UValue = info, /No_Copy

;Position the widget in the cnter of the screen
CenterTLB, tlb

;Place keyboard focus on 'calculate button'
Widget_Control, Calc_button, /input_focus

;Register the widgets and end the widget definition module
XManager, 'SMF_Feature', tlb, Event_Handler = 'SMF_Feature_Events', $
       Cleanup = 'SMF_Feature_Cleanup', GROUP_LEADER=Group_Leader
;Restore color table
Tvlct, r, g, b, /get

top=!d.Table_Size-3
r(top)=[255b, 0b, 0b]
g(top)=[0b, 255b, 0b]
b(top)=[0b, 0b, 255b]
Tvlct, r, g, b



;Package the BPass parameters and the filtered image as a structure and
;return it to the user.  If the routine was cancelled and no parameters were
; calculated, return an array of -1's as the feature info and the default
;particle size and masscut values

Features = *ptr_features

IF ptr_Valid(ptr_Params) THEN BEGIN
Params = *ptr_Params
Ptr_Free, ptr_Params
ENDIF ELSE Params = {ParticleSize:ParticleSize, Masscut:Masscut}

FeatureInfo = {Features:Features, ParticleSize:Params.ParticleSize, Masscut:Params.Masscut}
RETURN, FeatureInfo

print,'masscut',masscut
print,'particlesize',particlesize

END
