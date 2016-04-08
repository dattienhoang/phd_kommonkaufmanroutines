; ********** start of TKH_Convolve_Filter.pro
;+
; NAME:
;     TKH_Convolve_Filter
; PURPOSE:
;     Implements a real-space bandpass filter which suppress
;     pixel noise and slow-scale image variations while
;     retaining information of a characteristic size.
;
; CATEGORY:
;     Image Processing
; CALLING SEQUENCE:
;     Result = TKH_Convolve_Filter( image, lnoise, lobject )
; INPUTS:
;     image:   The two-dimensional array to be filtered.
;     lnoise: Characteristic lengthscale of noise in pixels.
;      Additive noise averaged over this length should
;      vanish. MAy assume any positive floating value.
;     lobject: A length in pixels somewhat larger than a typical
;      object.
; OUTPUTS:
;     Result: filtered image, returned as BYTE type
; PROCEDURE:
;     simple 'wavelet' convolution yields spatial bandpass filtering.
; NOTES:
; MODIFICATION HISTORY:
;     This was written by Toby Herman, 11/07.
;     It is based on the program 'BPass.Pro' from Grier/Crocker:
;       Written by David G. Grier, The University of Chicago, 2/93.
;       Greatly revised version DGG 5/95.
;       Added /field keyword JCC 12/95.
;       Revised & added 'stack','voxel' capability JCC 5/97.
;
;       This code 'bpass.pro' is copyright 1997, John C. Crocker and
;       David G. Grier.  It should be considered 'freeware'- and may be
;       distributed freely in its original form when properly attributed.
;-
FUNCTION TKH_Convolve_Filter, image, lnoise, lobject, field = field, noclip=noclip

N_Frames = N_Elements(image(0,0,*))


;on_error, 2          ; go to caller on error

b = float( lnoise )
w = round( lobject > (2. * b) )
N = 2*w + 1

;Define 1-D Gaussians
r = (findgen( N ) - w)/(2. * b)
xpt = exp( -r^2 )
xpt = xpt / total(xpt)
factor = ( total(xpt^2) - 1/N )

gx = xpt
gy = transpose(gx)


;Define 2-D Gaussian
Mask = (Shift(Dist(N,N), w, w))^2
Mask = Mask/ Total(Mask) ; Normalize



;Define background convolution function
bx = fltarr(N) - 1./N
by = transpose(bx)

Back = Make_Array(N,N, value =(1.0/N)) ;2-D version


;I don't know what this is for...
if keyword_set( field ) then begin
    if N mod 4 eq 1 then indx = 2*indgen(w+1)$
         else indx = 1+ (2*indgen(w))
    gy = gy(indx)
    gy = gy/total(gy)

    nn = n_elements(indx)
    by = fltarr(nn) - 1./nn
endif

;Create the results array
Result = Float(image)

; do x and y convolutions in 1-D
FOR i = 0,N_Frames-1 DO BEGIN
    g = convol( float(image(*,*,i)), gx )
    g = convol( g, gy )

    b = convol( float(image(*,*,i)), bx )
    b = convol( b, by )

    Result(*,*,i) = g-b
ENDFOR

;Do convolution in 2-D
;FOR i = 0, N_Frames-1 DO BEGIN
;    spots = convol( float(image(*,*,i)), Mask )
;
;    background = convol( float(image(*,*,i)), Back )
;
;    Result(*,*,i) = g-b
;ENDFOR




if keyword_set( noclip ) then $
    return,Result/factor $
else $
    return,Result/factor > 0

end

;*********** end of bpass.pro
