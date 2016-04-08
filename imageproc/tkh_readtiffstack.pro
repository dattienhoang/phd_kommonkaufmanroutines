;readtiffstack.pro
;Eric Corwin
;Monday, November 20
;a function to read in a tiff stack into a single variable
;
;TKH slightly modified it to read in 16bit Tiff stacks; 2006
;TKH further modified it to read in either 8bit or 16bit stacks
;depending on the file itself.  Also added Byte-scale option for 16bit
;Tiffs.
;TKH added some print outputs to allow user to monitro progress for large files
;QUIET keyword added to suppress that output
;
;imagefile is the file to be read
;rect is the rectangular region of the image to be read. It has the form
;[x,y,width,height] measured in pixels from lower left corner (rh coord sys)

function tkh_readtiffstack, imagefile ,rect=rect,start_frame=start_frame, $
   stop_frame=stop_frame, ByteScale=ByteScale, Quiet = Quiet, Max_Frame=Max_Frame

File_Time = SysTime(1) ; Get system time

ok = query_tiff(imagefile, info)
Print, 'Finished querying Tif file.  It took' + $
    string(systime(1)-File_Time, Format = '(I3)') + ' seconds.'
F_start = 0L
F_stop = Long(info.num_images-1)
pixels = info.dimensions[0]*info.dimensions[1]
IF N_Elements(Max_Frame) EQ 0 THEN Max_Frame = F_Stop

if (keyword_set(start_frame)) then F_Start = start_frame
if (keyword_set(stop_frame)) then F_Stop = stop_frame

N_Frames = (Max_Frame < F_Stop) - F_Start + 1

;Initialize an image array of the correct dimensions and either a
;16-bit or 8-bit data type

IF info.bits_per_sample EQ 16 THEN BEGIN
    if (keyword_set(rect)) then $
       imgarr = uintarr(rect[2]-rect[0],rect[3]-rect[1],N_Frames) $
    else imgarr=uintarr(info.dimensions[0],info.dimensions[1],N_Frames)

ENDIF ELSE BEGIN
help, /memory
    if (keyword_set(rect)) then $
       imgarr = bytarr(rect[2]-rect[0],rect[3]-rect[1],N_Frames) $
    else imgarr=bytarr(info.dimensions[0],info.dimensions[1],N_Frames, /nozero)
help, /memory
ENDELSE



if ok EQ 1 then begin

    Loop_Time = SysTime(1) ; Get system time

    for i=F_start , (F_stop < (F_Start + Max_Frame - 1)) do begin

       if (keyword_set(rect)) then $
         img=read_tiff(imagefile,image_index=i,sub_rect=rect) $
       else img=read_tiff(imagefile,image_index=i)

       imgarr[*,*,i-F_start] = img
       IF Keyword_Set(Quiet) EQ 0 THEN IF ((i+1) MOD 500) EQ 0 THEN BEGIN

            Print, 'Now loading frame ', i+1, ' of ', (F_stop - F_start + 1)
            Print, 'Time elapsed while reading this file: ' + $
                   String(systime(1)-File_time, format = '(I6)') + ' seconds.   ' + $
                   '   Time elapsed while reading the last 500 frames: ' + $
                   String(systime(1)-Loop_time, format = '(F7.2)') + ' seconds. '
            Wait, 0.02
            Loop_Time = SysTime(1) ; Get system time
        ENDIF; of displaying progress

    endfor
endif

IF ((Keyword_Set(ByteScale) EQ 1) AND (info.bits_per_sample EQ 16)) $
     THEN imgarr=BytScl(imgarr)

return, imgarr
end
