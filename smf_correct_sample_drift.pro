;This function will help to correct for sample movement during
;data collection.

;The  movie is split into a number of segments, determined by the user.
;The frames within each segment are summed, so that each molecule
;should be fairly distinct.

;To find the best possible estimate for the 'shift' experienced
;by the sample during the time between two segments, two methods can be 
;employed.  
;In the first method the elements
;of segment n and segment n+1 are multiplied -- when there is strong
;overlap the product will be quite large, while poor overlap will
;result in a lower, more uniform background.  Segment n+1 is shifted
;horizontally and verticlally until the maximum product is
;obtained.  The coordinates of this shift are then stored in an
;array for each segment.  
;The second method computes the difference between two adjacent segments
; and computes the sum of the squares of those risiduals.

;The 'Overlap Residuals' method is more reliable.

;The movement of the sample is assumed to
;be linear between segment midpoints, a poor assumption, but one
;that is close enough to ensure that the molecules don't deviate
;outside the averaging window used by the SMF_GUI program.
;Furthermore, the initial and final linear interpolations are
;extended to the initial and final frames respectively.


;***************************************
PRO SMF_Drift_Filter, event
Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, event.id, Get_UValue = uvalue


CASE uvalue OF
    'Filter': info.Filter = 1B - info.Filter
    'SpotSize': BEGIN
           Widget_Control, event.id, Get_Value = temp
           info.SpotSize=temp
             END
    ELSE: Print, 'Umm, we have a problem...'
ENDCASE
Widget_Control, info.SpotSlider, Sensitive = info.Filter

Widget_Control, event.top, Set_UValue=info, /No_Copy
END

;****************************************
PRO  SMF_Merit_Calculation, Segment, info


CASE info.MeritMethod OF
    'Overlap Product': BEGIN
    ;Merit = Segment(n)*(segment(n-1)+segment(0)) or = Segment(n)*segment(n-1)
;    Previous=((0.5*(*info.Segments)[*,*,i-1]) + 0.5*Shift((*info.Segments)[*,*,0], 1*TS[i-1,*]))

    FOR j=0,4 DO BEGIN
       FOR k=0, 4 DO BEGIN
         (*info.Merits)[j,k,segment-1] =Total( ((*info.Segments)[*,*,segment-1])$
               * (Shift((*info.Segments)[*,*,segment], (j-2), (k-2))))
       ENDFOR
    ENDFOR


    ENDCASE
    'Overlap Residuals': BEGIN
    ;Merit = -(Segment(n) - Segment(n-1))^2
    ;This is a least squared fit, taken to be negative so that the
    ;smallest absolute deviation is the largest 'merit' in the matrix

    FOR j=0,4 DO BEGIN
       FOR k=0, 4 DO BEGIN
         (*info.Merits)[j,k,segment-1] = - Total( ((*info.Segments)[*,*,segment-1] $
               - Shift((*info.Segments)[*,*,segment], (j-2), (k-2)))^2)
       ENDFOR
    ENDFOR

    ENDCASE
    ELSE: a=Dialog_Message('Oops -- the programmer screwed up.')
ENDCASE

    Print, 'Merit matrix found for segment #', segment
    ;Now find the maximum of the merit function for small movements
    Max_Merit = Max((*info.Merits)[*,*,segment-1], Coord)
    (*info.Shifts)[segment,0] = ((Coord MOD 25) MOD 5)-2 ;X-shift of max merit
    (*info.Shifts)[segment,1] = Fix((Coord MOD 25)/5)-2 ;Y-shift of max merit


END

;**************************************
PRO SMF_Drift_Display_Update, info


    TS = Total(*info.Shifts, 1, /cumulative)
    CASE info.Display OF ;Display the quantity of interest
       'Unshifted': BEGIN
              TV, (*info.segments)[*,*,info.segment]
         ENDCASE
       'Shifted': BEGIN
               TV, Shift((*info.segments)[*,*,info.segment], $
                  TS[info.segment,*])
         ENDCASE
       'Product' : BEGIN
                info.Segment = info.Segment > 1
          Seg_Prod = (0.5*Shift((*info.segments)[*,*,info.segment-1], $
             TS[info.segment-1,*]) + 0.5* $
             SHIFT((*info.segments)[*,*,info.overlap],TS[info.overlap,*]) * $
                 (SHIFT((*info.segments)[*,*,info.segment],TS[info.segment,*])))
                 TVSCL, Seg_Prod, top=252
         ENDCASE
       'Overlap' : BEGIN
               TVSCL, FIX(SHIFT((*info.segments)[*,*,info.segment], $
                TS[info.segment,*])) + FIX(SHIFT((*info.segments)[*,*,info.overlap], $
                TS[info.segment,*])), top=252
         ENDCASE
    ENDCASE


END

;***************************************
PRO SMF_Correct_Sample_Drift_Events, event
Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, /Hourglass


Widget_Control, event.id, Get_Value=buttonValue
CASE buttonValue OF
   'Accept': BEGIN ; Process data here: 1)Calculate shifts for individual frames, 
                    ;2)rename binary file to preserve original data, 3) shift frames,
                    ;4)trim borders off of movie
   
    ;First to check  the shift to ensure that it is less
    ;than half the width of the frame (ie the user hasn't
    ;wrapped around the display screen)

    Shifts = *info.Shifts
    FOR i=1, (Size(shifts))[1]-1 DO BEGIN
       Temp_Shift = (Shifts[i,0] LT 0) ? (Shifts[i,0] + info.s[0]) : $
       (Shifts[i,0] - info.s[0])

       Shifts[i,0] = (abs(Shifts[i,0]) LT abs(Temp_Shift)) ?  $
         Shifts[i,0] : Temp_Shift

       Temp_Shift = (Shifts[i,1] LT 0) ? (Shifts[i,1] + info.s[1]) : $
       (Shifts[i,1] - info.s[1])

       Shifts[i,1] = (abs(Shifts[i,1]) LT abs(Temp_Shift)) ?  $
         Shifts[i,1] : Temp_Shift
    ENDFOR

   ;Calculate the shift for each frame by interpolating the shift array

   Shifts = Total(*info.Shifts, 1, /cumulative, /integer)
   X_Shifts = Round(Interpol(Float(Shifts[*,0]), $
           ((*info.Frames)[0:info.N_Segments-1] + 0.5*info.S[2]/info.N_Segments),$
            indgen(info.S[2])))
   Y_Shifts = Round(Interpol(Float(Shifts[*,1]), $
           ((*info.Frames)[0:info.N_Segments-1] + 0.5*info.S[2]/info.N_Segments),$
            indgen(info.S[2])))

;Print, 'Total Shifts at the end of it all (by segment):'
;Print, Transpose(Shifts)
    ;Manually extend the interpolation to the first frame and last frame
    Interp_Range = info.S[2]/info.N_Segments
    X_Shifts[0:(Interp_Range-1)] = Round((Indgen(Interp_Range, /Float) $
          - Interp_Range) * (Float(Shifts[0,0])/Interp_Range))
    Y_Shifts[0:(Interp_Range-1)] = Round((Indgen(Interp_Range, /Float) $
          - Interp_Range) * (Float(Shifts[0,1])/Interp_Range))

    X_Shifts[info.S[2]-Interp_Range] = $
           X_Shifts[(info.S[2]-1)-Interp_Range] + $
           Round( Indgen(Interp_Range, /Float) *  $
           (Float( Shifts[info.N_Segments-1,0] - $
           Shifts[info.N_Segments-2,0]) /Interp_Range))
    Y_Shifts[info.S[2]-Interp_Range] = $
           Y_Shifts[(info.S[2]-1)-Interp_Range] + $
           Round( Indgen(Interp_Range, /Float) *  $
           (Float(Shifts[info.N_Segments-1,1] - $
           Shifts[info.N_Segments-2,1])/Interp_Range))

  ;Save a copy of the original data:
  IF File_Test(info.oldfile) EQ 0 THEN File_Copy, info.file, info.oldfile
   
     ;Check to make sure that the user specified border value exceeds the
    ;maximum shift -- if not substitute the larger value
    Total_Shift = Ceil(Abs(Total(*info.Shifts, 1, /cumulative)))
    X_Trim = info.Border_Width>(CEIL(Max(Total_Shift[*,0])/2))
    Y_Trim = info.Border_Width>(CEIL(Max(Total_Shift[*,1])/2))
  
  ;Create new filename to reflect the new image size (due to trimming)
  New_File = StrSplit(info.file, '-', /extract)
  N_Filename_Parts = N_Elements(New_File)
  New_File = (N_Filename_Parts GT 1) ? StrJoin(New_File(0:N_Filename_Parts-2)) : New_File
  New_File = StrCompress(New_File + '-' + string(info.S[0]-2*X_Trim) + 'x' + $
                  string(info.S[1] - 2* Y_Trim) + '.bin', /remove_all)
  
  
  ;Open existing data as an associated variable; open a new binary file to hold 
  ;the shifted data 
  
  OpenR, lun, info.file, /get_lun
  image = assoc(lun, info.Frame_Format)
  
  OpenW, new_lun, New_File, /get_lun
  

    ;Now apply these shifts to the the actual data
    FOR i=0,info.S[2]-1 DO BEGIN
  ;     (*info.image)(*,*,i) = Shift((*info.image)[*,*,i], X_Shifts[i], Y_Shifts[i])
       Temp = Shift(image[i], X_Shifts[i], Y_Shifts[i])

     ;Trim off borders, permamnently discarding the edges since they
     ;may contain data that has been shifted preiodically around the
     ;image array, then write the frame to the binary file
     Temp = Temp[X_trim:(info.S[0]-X_trim-1), Y_trim:(info.S[1]-Y_trim-1)]
     WriteU, new_lun, Temp
    ENDFOR

  Close, lun, new_lun
  Free_lun, lun, new_lun

;Make sure the main program looks at the trimmed movie, rather than the original by
;reassigning the file pointer
  *info.File_Pointer = New_File

           Widget_Control, event.top, Set_UValue=info, /No_Copy
           Widget_Control, event.top, /Destroy
             ENDCASE

    'Cancel': BEGIN ; Nothing needs to change
           Widget_Control, event.top, Set_UValue=info, /No_Copy
           Widget_Control, event.top, /Destroy
           ENDCASE
ENDCASE

END


;*****************************************************************
;Allow sliders to change frame settings
PRO SMF_Drift_Info, event

Widget_Control, event.top, Get_UValue=info, /No_Copy

Widget_Control, event.id, Get_UValue = uValue
Widget_Control, event.id, Get_Value = Value

;Write error  checking code here:
;     -Border too big (or too small)
;     -too many segments
;


;Divide the movie up into segments
If uValue EQ 'Segments' THEN BEGIN
    Widget_Control, event.id, Get_Value = N_Segments
    info.N_Segments = N_Segments

    ;Reinitialize the data pointers to prepare for the change in format
    *info.Segments = BytArr(info.S[0],info.S[1], N_Segments)
    *info.Merits = FltArr(5,5,N_Segments-1)
    *info.Shifts = IntArr(N_Segments, 2)

    ;Reset the slider limits for viewing the output
    Widget_Control, info.SegmentSlider, Set_Slider_Max=N_Segments-1
    Widget_Control, info.OverlapSlider, Set_Slider_Max=N_Segments-2

    ;Define the first frame of each segment; append a value for the last frame
    Frames = Round(Indgen(N_Segments+1, /Float) * (Float(info.S[2])/N_Segments))
    *info.Frames = Frames

ENDIF ;(of setting # of segments)


;Record a border value
If uValue EQ 'Border' THEN BEGIN
Widget_Control, event.id, Get_Value = info.BorderWidth
ENDIF ;(of setting border width)


;Choose how the overlap merit is calculated between segments
IF uValue EQ 'Merit Function' THEN BEGIN
 info.MeritMethod = event.str
ENDIF; ;(of choosing merit function)


;Here is the gruntwork of the program
;1)Compute segments
IF uValue EQ 'Compute' THEN BEGIN

;Create an associated variable for the binary movie file
OpenR, lun, info.file, /get_lun
Image = Assoc(lun, info.Frame_Format)

For i=0,info.N_Segments-1 DO BEGIN
    ;Total up individual frames into segments
    Segment = LonArr(info.S[0], info.S[1])
    FOR j = (*info.frames)[i],((*info.frames)[i+1]-1) DO Segment += image[j]
    ;Strip off the background 'signals' and reduce contrast of overly
    ;bright peaks by using the histogram and bytscl functions to trim
    ;off extreme values.
    Seg_Hist = Histogram(Segment, Reverse_Indices=ri, NBins=100)
    ;Pull out the intensity value of a fairly bright (but not
    ;maximally so) pixel -- arbitrary chosen at about the 99.9th
    ;percentile -- to become our maximum.
    Seg_Max = Segment[ri[Long(100 + 0.999*N_Elements(Segment))]]
    ;Similarly, set the minimum to exclude any contribution from the
    ;background counts.  Lets use an intensity value from the 90th
    ;percentile
    Seg_Min = Segment[ri[Long(100 + 0.90*N_Elements(Segment))]]

    Segment = Bytscl(Segment, min=Seg_Min, max=Seg_Max, top=252)
    (*info.Segments)[*,*,i] = segment
;    Print, Mean((*info.Segments)[*,*,i])
ENDFOR

;close binary movie file
Close, lun
Free_lun, lun

;1b) Optionally, apply a smoothing filter to the segments before shifting
IF info.Filter EQ 1 THEN BEGIN
    Segments = TKH_Convolve_filter(*info.Segments, 1.1, info.SpotSize)
    *info.Segments = BytScl(Segments)
ENDIF

;2)Compute the optimal shift for each segment relative to the one before it
FOR i=1,info.N_Segments-1 DO BEGIN
    ;Calculate any cumulative shifts so far
    TS = Total(*info.shifts, 1, /cumulative, /integer)
    ;Now compute merit factor for small shifts of each segment.
    ;Create a 5x5 matrix, probing moves of up to two pixels in
    ;every direction.  This matrix is stored in the 'info' structure.
    ;Then pick the shift that maximizes the 'Merit' value and store
    ;in the info structure.
    SMF_Merit_Calculation, i, info

ENDFOR

Print, 'Total shifts (X shifts first, then Y shifts):'
Print, Transpose(TS)

ENDIF ;(of calculating everything)


IF uValue EQ 'Shift' THEN BEGIN
    ;Allow user to adjust the shift matrix; after screwing with the
    ;shift matrix the user will only be able to 'Cancel' or 'Accept'
    ;the results.  If s/he wants to go back and try it again, s/he
    ;will need to 'Cancel' and reload the widget.

    ;First deactivate the computation features -- assume the user has
    ;decided on the correct number of segments.
    Widget_Control, info.InfoID, sensitive=0

    ;Now activate Keyboard Events for the draw widget
    Widget_Control, info.displayID, Draw_Keyboard_events=1
ENDIF ;(of 'Manual shift' selection)


Widget_Control, event.top, Set_UValue=info, /No_Copy
END


;**************************************************************
;Allow user to flip between the various segments individually,
;or with overlap between subsequent frames.
;All events (sliders, button)are dealt with by this event handler.
PRO SMF_Drift_Display, event
Widget_Control, event.top, Get_UValue=info, /No_Copy

eventName = TAG_NAMES(event, /STRUCTURE_NAME); What kind of event is this?



IF eventName EQ 'WIDGET_SLIDER' THEN BEGIN   ; Code for segment choice events.
    Widget_Control, event.id, Get_uValue = uValue
    CASE uValue OF
       'Segment': BEGIN
        info.segment = event.Value ;Read out the segment # of interest
        info.Overlap = info.Overlap < (info.Segment-1 > 0)
       ENDCASE
       'Overlap':    BEGIN ;Read out the segment # for overlapping
        info.Overlap = event.Value < (info.Segment-1 > 0)
       ENDCASE
    ENDCASE

    ;Check that the overlap setting is still okay
    Widget_Control, info.OverlapSlider, Set_Value = info.Overlap

    SMF_Drift_Display_Update, info ;Update the display
ENDIF; (of the segment choice code)

;Exclusive radio buttons toggle the display mode
IF eventName EQ 'WIDGET_BUTTON' THEN BEGIN
;    info.display = event.uValue ;Put display option into info structure
    Widget_Control, event.id, Get_uValue=bob
    info.display=bob
    ;Adjust slider maximum accordingly
    IF bob EQ 'Product' THEN BEGIN
        Widget_Control, info.SegmentSlider, Set_Slider_Min = 1
    ENDIF ELSE Widget_Control, info.SegmentSlider, Set_Slider_Min = 0
    ;Update the display
    SMF_Drift_Display_Update, info

ENDIF; (of radio buttons)


;Now the code for shifting the pictures manually.  Each time an arrow key
;is pressed, it will move the current segment inthe indicated direction.
;The info.Shifts matrix is updated, such that only that frame is moved --
;the following frames remain unchanged.
IF eventName EQ 'WIDGET_DRAW' THEN BEGIN
    CASE event.key OF
    ;Left arrow - X_shift decreases by 1
    5:BEGIN
        (*info.shifts)[info.segment,0] = (*info.shifts)[info.segment,0]-1
        IF info.segment LT (info.N_Segments-1) THEN $
           (*info.shifts)[info.segment+1,0] = (*info.shifts)[info.segment+1,0]+1
      ENDCASE
    ;Right arrow - X_Shift increases by 1
    6:BEGIN
        (*info.shifts)[info.segment,0] = (*info.shifts)[info.segment,0]+1
        IF info.segment LT (info.N_Segments-1) THEN $
           (*info.shifts)[info.segment+1,0] = (*info.shifts)[info.segment+1,0]-1
      ENDCASE
    ;Down arrow - Y_Shift decreases by 1
    8:BEGIN
        (*info.shifts)[info.segment,1] = (*info.shifts)[info.segment,1]-1
        IF info.segment LT (info.N_Segments-1) THEN $
           (*info.shifts)[info.segment+1,1] = (*info.shifts)[info.segment+1,1]+1
     ENDCASE
    ;Up arrow - Y_Shift increases by 1
    7:BEGIN
        (*info.shifts)[info.segment,1] = (*info.shifts)[info.segment,1]+1
        IF info.segment LT (info.N_Segments-1) THEN $
           (*info.shifts)[info.segment+1,1] = (*info.shifts)[info.segment+1,1]-1
      ENDCASE
    ENDCASE

;Now update the display
SMF_Drift_Display_Update, info

ENDIF; (of display keyboard events)



WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
END

;**********************************
PRO SMF_Correct_Sample_Drift_Cleanup, tlb
Widget_Control, tlb, Get_UValue=info, /No_Copy
;Clean up all the pointers in the info structure (except the image pointer)



Ptr_Free, info.Segments
Ptr_Free, info.Shifts
Ptr_Free, info.Merits
Ptr_Free, info.Frames



END

;***************************************************

PRO SMF_Correct_Sample_Drift, external_info, GROUP_LEADER=Group_Leader

S = external_info.S_Image ;Get image size from main program

N_Frames = S[2]
file_pointer = external_info.Associated_Name
file = *external_info.Associated_Name
oldfile = StrSplit(file, '.', /extract)
N_File_Parts = N_Elements(oldfile)
IF N_File_Parts GT 1 THEN oldfile = StrCompress(oldfile[0:N_File_Parts-2] $
          + '_unshifted.' + oldfile[N_File_Parts-1]) ELSE $
          oldfile = strcompress(file + '_unshifted')

; Extract the first frame of the movie for display
OpenR, lun, file, /get_lun
Full_Movie = assoc(lun, *external_info.Image_Template)
image = Full_Movie[0] 
Close, lun
Free_lun, lun


;Start defining widgets

;Top level base widget.
tlb = Widget_Base(Column=1, Title='Adjust position of frames to counter sample movement',$
           group_leader=GROUP_LEADER)

;Widget to allow user to filter images before trying to correct for sample drift.  If
;the data isn't very noisy this is completely unnecessary.
FilterBase = Widget_Base(tlb, Row =1, Event_Pro = 'SMF_Drift_Filter')
FilterButtonBase = Widget_Base(FilterBase, /nonexclusive)
FilterButton = Widget_Button(FilterButtonBase,  Value='Filter Image First?', $
          UValue = 'Filter')
SpotSlider = Widget_Slider(Filterbase, Min=1, Max = (S[1]<S[2])/10 , UValue = 'SpotSize',$
              title='Spot Size (pixels)', value = 3, Sensitive =0 )


;Widget to allow adjustment of # of frames averaged in each segment.  Also allow
;adjustment of the border that will be stripped away from the movie (to allow
;for the shifting done by this program)

InfoID = Widget_Base(tlb, Row=1, Event_Pro = 'SMF_Drift_Info')
SegmentsID = Widget_Slider(InfoID, Value=(N_Frames/100), Min=0, Max=(N_Frames-1)/30, $
              UValue = 'Segments', title='Number of Segments', xsize=150)
BorderID = Widget_Slider(InfoID, Value=3, Min=1, Max=25,$
               UValue = 'Border', title='Border Width (pixels)', xsize=150)
MeritBase = Widget_Base(InfoID, column=1)
label = Widget_Label(MeritBase, Value = 'Merit Calculation Technique')
combobox = Widget_Combobox(MeritBase, uValue = 'Merit Function',  $
          Value = ['Overlap Residuals','Overlap Product'])
button = Widget_Button(InfoID, Value='Compute Shifts', UValue='Compute')


;Widget for image display
displayBase = Widget_Base(tlb, column=1, /Base_Align_Center, $
          Event_Pro='SMF_Drift_Display')
displayID = Widget_Draw(displayBase, xsize=S[0], YSize=S[1])
DisplayControls = Widget_Base(displayBase, row =1)
SegmentSlider = Widget_Slider(DisplayControls, Title='Segment Displayed',$
              Value = 0, Max=N_Frames/100 -1, UValue='Segment')
OverlapSlider = Widget_Slider(DisplayControls, Title='Overlap Segment',$
              Value = 0, Max=N_Frames/100 -2, UValue='Overlap')
ShiftButtonID = Widget_Button(DisplayControls, Value = 'Adjust Shifts',$
              UValue = 'Shift', Event_Pro='SMF_Drift_Info')
MetricTextID = Widget_Text(DisplayControls, Value = 'Fit Metric= N/A')
DisplayChoiceID = Widget_Base(displayBase, row=1, /exclusive)
button = Widget_Button(DisplayChoiceID, Value = 'Unshifted segments',Uvalue = 'Unshifted')
button = Widget_Button(DisplayChoiceID, Value = 'Shifted segments', Uvalue = 'Shifted')
button = Widget_Button(DisplayChoiceID, Value = 'Segment Overlap', Uvalue = 'Overlap')
button = Widget_Button(DisplayChoiceID, Value = 'Segment Product', Uvalue = 'Product')

;Widget for buttons
buttonsize=75
buttonbase = Widget_Base(tlb, row=1, /align_center)
button = Widget_Button(buttonbase, value = 'Accept',  xsize = buttonsize)
button = Widget_Button(buttonbase, value = 'Cancel',  xsize = buttonsize)


;Realize Widgets
Widget_Control, tlb, /realize
CenterTLB, tlb

;Get the draw widget's window address
Widget_Control, DisplayID, Get_Value = WID
WSET, WID


TVSCL, image


;Create an anonymous structure to store info required by the program
info = { image:image, $ ;first frame of original image
       Frame_Format : *external_info.Image_Template, $; For associated variable
       File: File, $  ;binary file of movie to be accessed as associated variable
       File_pointer: File_Pointer, $;pointer to assoc file - changed once movie is trimmed
       Oldfile: oldfile, $ ;filename for the backup file  if the movie is shifted
       Filter: 0B, $ ;should the image be filtered before processing?
       SpotSize: 3, $ ;Size of spot to be used in filtering routine, if used at all
       N_Segments: N_Frames/100, $
       Border_Width:3, $
       Display:'Unshifted', $ ;What quantity is displayed in window
       MeritMethod: 'Overlap Residuals', $ ;Which method used to compute merit function
       SpotSlider:SpotSlider, $ ;ID of Filter size slider
       DisplayID:DisplayID, $ ;ID of draw widget
       InfoID:InfoID, $ ;Widget ID of slider controls base
       ShiftButtonID: ShiftButtonID, $ ; Widget ID for manual shifting of frame
       SegmentSlider:SegmentSlider, $ ; Widget ID of segment selection slider
       OverlapSlider:OverlapSlider, $ ; Widget ID of overlap selection slider
       MetricTextID:MetricTextID, $
       S: S, $ ; Image size
       Segment: 0 ,$ ; The active segment for display
       Overlap: 0, $ ; The active segment for overlap and product display
       Segments: Ptr_New(BytArr(S[0],S[1], (N_Frames/100))), $ ;Sum image for each segment
;       Products: Ptr_New(FltArr(S[0],S[1], 10)), $ ; Multiplied segments
       Shifts: Ptr_New(IntArr((N_Frames/100),2)), $ ; Matrix of dX, dY for each segment
       Merits: Ptr_New(FltArr(5,5,(N_Frames/100))), $ ; merits of the various local shifts
       Frames: Ptr_New(Round(Indgen((N_Frames/100 + 1), /Float) * (S[2]/(N_Frames/100)))) $ ; The frame numbers for each segment
                    }



;Put the info structure into the 'User Value'
Widget_Control, tlb, Set_UValue=info, /No_Copy

;Register the program and end the widget definition module
XManager, 'SMF_Correct_Sample_Drift', tlb, $
    Event_Handler='SMF_Correct_Sample_Drift_Events', $
    Cleanup = 'SMF_Correct_Sample_Drift_Cleanup', $
    GROUP_LEADER=Group_Leader


END
