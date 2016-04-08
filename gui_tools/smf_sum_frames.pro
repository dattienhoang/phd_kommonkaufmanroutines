PRO SMF_Sum_Frames_Events, event
Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, /Hourglass

print, 'info.imagesize[0]', info.imagesize[0]
print, 'info.imagesize[1]', info.imagesize[1]

;Calculate the summed Image
summedImage=lon64arr(info.imagesize[0],info.imagesize[1])
OpenR, lun, info.Image_File, /get_lun
image = Assoc(lun, info.Frame_Format)
;next line allows sum of desired frames
FOR i=info.Startframe, info.endframe-1 DO summedimage += image[i]
;FOR i=info.endframe-50, info.endframe-1 DO summedimage += image[i]
;next line only allows summing of 50 frames
;FOR i=info.endframe-50, info.endframe-1 DO summedimage += image[i]
;print, 'start frame', info.startframe
print, 'start frame',info.Startframe;-50
print, 'end frame', info.endframe-1

;startframe = 0
;endframe = info.endframe-1
;print, 'start frame', info.startframe

;-------FOR OTP ANALYSIS--------------------------------------
;startframe= info.endframe-50
;endframe= info.endframe-1
;FOR i=startframe, endframe DO summedimage += image[i]
;-------------------------------------------------------------

Close, lun
Free_lun, lun

;*info.summedimage=summedimage
;print, max(*info.summedimage)

;frames = info.endframe - info.startframe
;*info.summedimage=BytScl(summedimage, min=frames*1000, max=frames*5000)
*info.summedimage=BytScl(summedimage)

;print, 'sum from frame', startframe
;print, 'to frame', endframe
Widget_Control, event.id, Get_Value=buttonValue
CASE buttonValue OF
   'Accept': BEGIN
           Widget_Control, event.top, Set_UValue=info, /No_Copy
           Widget_Control, event.top, /Destroy
             ENDCASE
    'Apply': BEGIN
           Widget_Control, event.top, Set_UValue=info, /No_Copy
         Erase
         TVSCL, summedImage
         ENDCASE
ENDCASE

END



PRO SMF_Sum_Frames_Quit, event
Widget_Control, event.top, /Destroy

END



;Allow sliders to change frame settings
PRO SMF_Sum_Frames_Slider, event

Widget_Control, event.top, Get_UValue=info, /No_Copy

Widget_Control, event.id, Get_UValue = slider
Widget_Control, event.id, Get_Value = frame

;Check to make sure that the first and last frames don't overlap
IF slider EQ 'Start' THEN info.StartFrame = (frame < info.EndFrame-1)
IF slider EQ 'End' THEN  info.EndFrame = (frame > info.StartFrame+1)

IF slider EQ 'Start' THEN Widget_Control, event.id, Set_Value = info.StartFrame
IF slider EQ 'End' THEN Widget_Control, event.id, Set_Value = info.EndFrame


Widget_Control, event.top, Set_UValue=info, /No_Copy
END



; Cleanup procedure to deal with pointers in the info structure - not necessary here
PRO SMF_Sum_Frames_Cleanup, tlb


END



FUNCTION SMF_Sum_Frames, image_File, ImageSize, Frame_Format = Frame_Format, $
                      InitialSum=InitialSum, GROUP_LEADER=Group_Leader


;Check to make sure the correct data is supplied
IF N_Elements(image_File) EQ 0 THEN Message, 'A multi-frame movie is Required.', /NoName

;Find the image size, and the possible number of frames
;ImageSize = Size(*image)
IF ImageSize[2] LE 2 THEN Message, 'A multi-frame movie is Required.', /NoName
Startframe = 0
EndFrame = ImageSize[2]-1
N_Frames = ImageSize[2]

;Find the correct frame info for the associated variable
IF N_Elements(Frame_Format) EQ 0 THEN Frame_Format = UIntArr(ImageSize[0:1])


;If an initial sum is supplied it will allow the user to cancel out of the program 
;without changing it
IF N_Elements(InitialSum) EQ 0 THEN summedImage = Long(*image[*,*,0]) $
          ELSE summedImage = InitialSum



;Start defining widgets

;Top level base widget.
tlb = Widget_Base(Column=1, Title='Sum Frames for Fluorophore Identification',$
           group_leader=GROUP_LEADER, /modal)

;Widget to display total # of frames
frameBaseID = Widget_Base(tlb, Row=1)
label = Widget_Label(framebaseID, Value='The total number of frames is: ')
text = Widget_Text(framebaseID, Value = string(ImageSize[2]))

;Widget to allow adjustment of start and end frames for average
averageBaseID = Widget_Base(tlb, Row=1, Event_Pro = 'SMF_Sum_Frames_Slider')
startID = Widget_Slider(averageBaseID, Value=0, Min=0, Max=(N_Frames-1), $
              UValue = 'Start', title='Start Frame', xsize=150)
endID = Widget_Slider(averageBaseID, Value=(N_Frames/2)-1, Min=0, Max=(N_Frames-1),$
               UValue = 'End', title='Final Frame', xsize=150)

;Widget for buttons
buttonsize=75
buttonbase = Widget_Base(tlb, row=1, /align_center)
Accept_button = Widget_Button(buttonbase, value = 'Accept',  xsize = buttonsize)
button = Widget_Button(buttonbase, value = 'Apply',  xsize = buttonsize)
button = Widget_Button(buttonbase, value = 'Cancel',  xsize = buttonsize, $
                   Event_Pro = 'SMF_Sum_Frames_Quit')


;Realize Widgets
Widget_Control, tlb, /realize
CenterTLB, tlb


;Create pointer to hold the summed image; Put a single frame in it to start with
ptr = ptr_New(summedImage)


;Create an anonymous structure to store info required by the program
info = { image_file:image_file, $  ;path to binary movie file
       ImageSize:[Imagesize[0],Imagesize[1]], $
       StartFrame:StartFrame, $
       EndFrame:EndFrame, $
       Frame_Format : Frame_Format, $
       SummedImage:ptr }



;Put the info structure into the 'User Value'
Widget_Control, tlb, Set_UValue=info, /No_Copy

;Set keyboard focus on 'accept' button
Widget_control, Accept_button, /Input_Focus

;Register the program and end the widget definition module
XManager, 'SMF_SUM_FRAMES', tlb, $
    Event_Handler='SMF_Sum_Frames_Events', $
    Cleanup='SMF_Sum_Frames_Cleanup', $
    GROUP_LEADER=Group_Leader


;summedimage=BytScl(*ptr)
summedimage=*ptr
;print, max(summedimage)
Ptr_Free, ptr

RETURN, summedimage

END
