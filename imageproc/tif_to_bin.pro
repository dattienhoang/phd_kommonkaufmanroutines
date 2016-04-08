;Converts Tiff file(s) to binary file.
;  This program can be passed a filename, or array of filenames.  The filepath
; of the last file converted is passed back out of the program as the parameter
; 'Bin_File_Out' if the user sets that parameter to a variable when the program
; is called, as is the image size (as is the base of the bin filename as
;'Bin_File_Base'
;
;Input Parameters:   File
;Output Parameters:  Image_Size, Bin_File_Out, Bin_File_Base

PRO tif_to_bin, file=file, Image_Size = Image_Size,$
 bin_file_out = bin_file_out, bin_file_base = bin_file_base

;Allow user to select files to convert
file = (N_Elements(file) NE 0) ? File : $
            Dialog_Pickfile(Title = 'Please choose TIFF data files', $
               Filter = '*.tif', /Multiple_Files, Get_Path = Tiff_Path)

N_Files = N_Elements(file)

Print
Print, 'Reading the following files:'
FOR i=0, N_Files-1 DO Print, file[i]
Wait, 0.05 ; Let display update

;Allow user to select destination for files
Destination_folder=Dialog_Pickfile(Title='Please destination folder for the .bin file(s)',$
            /Directory, path = Tiff_Path)

Print, ''
Print, 'Writing BIN file(s) to the followng directory:'
Print, Destination_folder
print, ''
Wait, 0.05 ; Update display

FOR i=1, N_Files  DO BEGIN
   This_File = File[i-1]; Pick a single file to read in

     IF This_File NE '' THEN BEGIN

     Print
     Print, 'Reading: ', This_file
     Wait, 0.02

       ;Now read in the file
       File_Time = SysTime(1) ; Get system time

       ok = query_tiff(This_File, info)
       Print, 'Finished querying TIFF file.  It took' + $
       string(systime(1)-File_Time, Format = '(I3)') + ' seconds.'
       Wait, 0.02

      pixels = info.dimensions[0]*info.dimensions[1]
      N_Frames = Long(info.num_images)
      Image_Size = [ info.dimensions[0], info.dimensions[1], info.num_images ]

      ;Initialize a binary filename based on the TIFF filename
      Temp_Filename = File_Basename(This_File)
      Temp_Filename = StrSplit(Temp_Filename, '.', /extract)
      N_Name_Parts = N_Elements(Temp_Filename) ;If name contains multiple '.'s then
      Temp_Filename =  (N_Name_Parts GT 2) ? $ ;only strip off the file extension
              StrJoin(Temp_Filename[0:N_Name_Parts-2]): Temp_Filename[0]
      Bin_Filepath = StrCompress(Destination_Folder + Temp_Filename + '-' + $
            String(info.dimensions[0]) + 'x' + String(info.dimensions[1]) + '.bin', $
            /remove_all)
      Bin_File_Out = Bin_Filepath ; export filename and filename baseif requested
      Bin_File_Base = StrCompress(Destination_Folder + Temp_Filename, /remove_all)
      OpenW, lun, Bin_Filepath, /get_lun ; Open the binary file to be written to

  ; If file exists, read it in.
  IF ok EQ 1 then begin
    Loop_Time = SysTime(1) ; Get system time

    FOR j=0 , (N_Frames - 1)  DO BEGIN
       img=UInt(read_tiff(This_File,image_index=j)) ; Read in a single frame
       WriteU, lun, img                       ;Write that frame to binary file

       IF ((j+1) MOD 500) EQ 0 THEN BEGIN ; Keep the user updated as to progress
            Print, 'Now loading frame ', j+1, ' of ', N_Frames
            Print, 'Time elapsed while reading this file: ' + $
                   String(systime(1)-File_time, format = '(I6)') + ' seconds.   ' + $
                   '   Time elapsed while reading the last 500 frames: ' + $
                   String(systime(1)-Loop_time, format = '(F7.2)') + ' seconds. '
            Wait, 0.02
            Loop_Time = SysTime(1) ; Get system time for beginning of next 500 frames
        ENDIF; of displaying progress

    endfor
endif


     Print, 'This file was saved as: ', Bin_Filepath
     Close, lun
     Free_lun, lun

     Print, StrCompress('(' + String(N_Files-i) + ' files remaining)')
     Wait, 0.02

     ENDIF
ENDFOR

Print, 'Completed file conversions.'
PRINT, 'Goodbye'
END
