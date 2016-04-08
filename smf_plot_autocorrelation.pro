;This procedure calculates the autocorrelation of the linear
;dichroism of a single fluorophore.  It can be calculated in a
;number of ways; the choice is made via the 'Method' keyword.
;The autocorrelation is only calculated out to the time limit
;specified by the user.  A fit to the autocorrelation function
;is also performed, with fitting parameters and a best fit line
;output along with the raw autocorrelation data.
;
;
;If an autocorrelation function has already been calculated, it can be
;passed to the fit function.  In that case, only the fit will be calculated


FUNCTION SMF_Plot_Autocorrelation, times, $
   Intensity, $  ;Total intensity
   LinDich, $       ;Linear Dichroism
   Autocorrelation = ACF, $   ;if an ACF was already calculated
   ACF_Err = ACF_Err, $ ;Errors to go with the ACF above
   Velocity_Interval = Velocity_Interval, $  ;time int. to calculate velocity
   Overplot = Overplot, $ ; should this ACF be plotted over an existing one?
   Method=method, $ ;Calculation method
   Stretched=Stretched, $    ; Fit with stretched exponential?
   LinInfo = LinInfo, $ ;Info that will be needed for a linear fit
   NoFit = NoFit, $       ;If keyword set, no fitting is done
   MaxFrame = MaxFrame, $      ; Maximum time of autocorrelation calculation in number of frames
   Max_Display_Time = Max_Display_Time, $ ; Maximum of time axis of plot
   Color = Color       ;User determined plot color


MIN_POINTS = 5 ;minimum number of points in acf required or minimum number of points to do linear fit on

;If no user supplied ACF method
IF N_Elements(Method) EQ 0 THEN Method = 'Basic_ACF'

;If no user supplied color, then pick a value near the top of the color table
IF N_Elements(Color) EQ 0 THEN  Color = 240

;If no maximum time is provided, then assume user wants ACF to cover all times
IF N_Elements(MaxFrame) EQ 0 THEN MaxFrame = N_Elements(times)-3
IF maxframe GT 10 then begin ;make sure there is data to calculate acf

 IF N_Elements(Max_Display_Time) EQ 0 THEN Max_Display_Time = times[MaxFrame]

;If no linear fit parameters are given, then assign some defaults
IF N_Elements(LinInfo) EQ 0 THEN LinInfo= {start:0, length: 3}

;If no velocity interval is given, then assign a default value of 1 timestep
IF N_Elements(Velocity_Interval) EQ 0 THEN Velocity_Interval= 1


N_frames=(Size(times))[1]
N_Points =  (MaxFrame - 1) < (N_Frames - 1) ;Max timediff for ACF (in # of frames) ; z = a < b, z equals the smaller number
timestep = times[1]-times[0]

;some global variable intialization
fit_length = 0.0
r_sq = 0.0
tau = 0

;If an ACF was already calculated and passed to this routine,
; use it to determine how many points to fit
IF N_Elements(ACF) GT 3 THEN BEGIN ;three points avoids that at fit only array index LT 0
   Autocorrelation = ACF
   N_Points = N_Elements(ACF)
   IF N_Elements(ACF_Err) NE N_Points THEN print, 'need to supply acf_err'
;r_sq = 0.0
;fit_length = lininfo.length
  print, 'N_Points of ACF', N_Points
  print, 'ACF_Err elemebts',  N_Elements(ACF_Err)
   GOTO, FitOnly
ENDIF

;Calculate each element of the ACF individually starting with tau=0
;and increasing until tau_max is reached
Autocorrelation = Make_Array(N_Points+1, /Float, Value=!values.f_NAN)
ACF_Err = Make_Array(N_Points+1, /Float, Value=!values.f_NAN)
ACF_Var = Make_Array(N_Points+1, /Float, Value=!values.f_NAN)



CASE StrUpCase(Method) OF
    'BASIC_ACF' : BEGIN ;
         LD_Ave = Mean(LinDich, /nan)           ;Overall average of linear dichroism
       FOR i=0D, N_Points DO BEGIN
        LD = LinDich[0:(N_frames-i-1)]
        LD_Shifted = LinDich[i:N_frames-1]
        LD_Product = (LD-LD_Ave)*(LD_Shifted-LD_Ave)
        good = Where(Finite(LD_Product), count)
        IF count GT 2 THEN BEGIN
            Prod_Info = Moment(LD_Product[good], SDev = Prod_Dev)
            Squar_Info = Moment((LD[good]-LD_Ave)^2, SDev = Squar_Dev)
            ;Now calculate the autocorrelation as the ratio of the means
            Autocorrelation[i] = (Prod_Info[0])/(Squar_Info[0])
            ;The error we will denote as the StErr of the autocorrelation
            ACF_Err[i] = ((Prod_Dev^2 + Squar_Dev^2)/count)^0.5
        ENDIF ELSE Autocorrelation[i] = !values.F_NaN
       ENDFOR
        ENDCASE

     'ORRIT' : BEGIN ;
       ;For the Orrit correlation function, all values will be shifted by 1
       ;Make a large array to allow self and cross terms to be calculated
       ;in contrast to all other acf mean is not subtracted from property
       FOR i=0, N_Points DO BEGIN
        LD = LinDich[0:(N_frames-i-1)] + 1
        LD_Shifted = LinDich[i:N_frames-1] + 1
        LD_Product = (LD)*(LD_Shifted)
        good = Where(Finite(LD_Product), count)
        IF count GT 2 THEN BEGIN
            Prod_Info = Moment(LD_Product[good], SDev = Prod_Dev)
            ;different from all the acf first calculate mean and then square
            LD_Info = Moment((LD[good]), SDev = LD_Dev)
            ;Now calculate the autocorrelation as the ratio of the means
            Autocorrelation[i] = (Prod_Info[0]/(LD_Info[0]^2)) -1
            ;The error we will denote as the StErr of the autocorrelation
            ACF_Err[i] = ((Prod_Dev^2 + LD_Dev^2)/count)^0.5
        ENDIF ELSE Autocorrelation[i] = !values.F_NaN
       ENDFOR
    ENDCASE

    'DIETER_LD': BEGIN
       ;We will use the same method to calculate the ACF, but later
       ;we will fit it with a linear equation
       mean_LD = mean(LinDich, /nan)
       FOR i=0, N_Points DO BEGIN
        LD = LinDich[0:(N_frames-i-1)]      ;LD(t)
        LD_Shifted = LinDich[i:N_frames-1]  ;LD(t+tau)
        LD_Product = (LD-mean_LD)*(LD_Shifted-mean_LD)          ;An array of products of individual terms
        good = Where(Finite(LD_Product), count)       ;Indices of valid points
        IF count GT 2 THEN BEGIN
            Prod_Info = Moment(LD_Product[good], SDev = Prod_Dev)
            Squar_Info = Moment((LD[good]-mean_LD)^2, SDev = Squar_Dev)
            ;Now calculate the autocorrelation as the ratio of the means
            Autocorrelation[i] = (Prod_Info[0])/(Squar_Info[0])
            ;The error we will denote as the StErr of the autocorrelation
            ACF_Err[i] = ((Prod_Dev^2 + Squar_Dev^2)/count)^0.5
        ENDIF ELSE Autocorrelation[i] = !values.F_NaN
       ENDFOR
    ENDCASE


    'DIETER_INTENSITY': BEGIN
       good = Where(Finite(LinDich), count, complement = bad) ; Only use values
       Intensity[bad] = !Values.F_NaN   ;at times when the intensity exceeds threshhold
       Int_Ave = mean(intensity,/nan)        ;Overall average of Intensity
       FOR i=0, N_Points DO BEGIN
        Int = Intensity[0:(N_frames-i-1)]
        Int_Shifted = Intensity[i:N_frames-1]
        Int_Product = (Int-Int_Ave)*(Int_Shifted-Int_Ave)
        good = Where(Finite(Int_Product), count)
        IF count GT 2 THEN BEGIN
            Prod_Info = Moment(Int_Product[good], SDev = Prod_Dev)
            Squar_Info = Moment((Int[good]-Int_Ave)^2, SDev = Squar_Dev)
            ;Now calculate the autocorrelation as the ratio of the means
            Autocorrelation[i] = (Prod_Info[0])/(Squar_Info[0])
            ;The error we will denote as the StErr of the autocorrelation
            ACF_Err[i] = ((Prod_Dev^2 + Squar_Dev^2)/count)^0.5
        ENDIF ELSE Autocorrelation[i] = !values.F_NaN
       ENDFOR
    ENDCASE


    'DIETER_DELTA_I': BEGIN
     ;Calculate |d(I(t))/dt|
        good = Where(Finite(LinDich), count, complement = bad) ; Only use values
        Intensity[bad] = !Values.F_NaN ;at times when the intensity exceeds threshhold

       DIT = Abs((1.0*(Intensity - Shift(Intensity, 1))/timestep)[1:N_Frames-1])
       Mean_DIT = Mean(DIT, /nan)
       FOR i=0, N_Points-1 DO BEGIN
        D_Int = DIT[0:(N_frames-i-2)]
        D_Int_Shifted = DIT[i:N_frames-2]
        D_Int_Product = (D_Int - Mean_DIT)*(D_Int_Shifted - Mean_DIT)
        good = Where(Finite(D_Int_Product), count)
        IF count GT 2 THEN BEGIN
            Prod_Info = Moment(D_Int_Product[good], SDev = Prod_Dev)
            Squar_Info = Moment((D_Int[good]-MEAN_DIT)^2, SDev = Squar_Dev)
            ;Now calculate the autocorrelation as the ratio of the means
            Autocorrelation[i] = (Prod_Info[0])/(Squar_Info[0])
            ;The error we will denote as the StErr of the autocorrelation
            ACF_Err[i] = ((Prod_Dev^2 + Squar_Dev^2)/count)^0.5
        ENDIF ELSE Autocorrelation[i] = !values.F_NaN
       ENDFOR
    ENDCASE

    'ANGULAR_VELOCITY': BEGIN  ;Calculate |d(LD(t))/dt|
       D_LD = Abs((1.0*(LinDich - Shift(LinDich, Velocity_Interval))/ $
                 (Velocity_Interval*timestep))[Velocity_Interval:N_Frames-1])
       Mean_D_LD = Mean(D_LD, /nan)

       FOR i=0, N_Points-Velocity_Interval DO BEGIN
        D_LinDich = D_LD[0:(N_frames-i-Velocity_Interval-1)]
        D_LinDich_Shifted = D_LD[i:N_frames-Velocity_Interval-1]
        D_LinDich_Product = (D_LinDich-Mean_D_LD)*(D_LinDich_Shifted-Mean_D_LD)
        good = Where(Finite(D_LinDich_Product), count)
        IF count GT 2 THEN BEGIN
            Prod_Info = Moment(D_LinDich_Product[good], SDev = Prod_Dev)
            Squar_Info = Moment((D_LinDich[good]-MEAN_D_LD)^2, SDev = Squar_Dev)
            ;Now calculate the autocorrelation as the ratio of the means
            Autocorrelation[i] = (Prod_Info[0])/(Squar_Info[0])
            ;The error we will denote as the StErr of the autocorrelation
            ACF_Err[i] = ((Prod_Dev^2 + Squar_Dev^2)/count)^0.5
        ENDIF ELSE Autocorrelation[i] = !values.F_NaN
       ENDFOR
    ENDCASE
ENDCASE


;Calculate variance using Bartletts formula (VDB version)
;FOR i=0, (N_Points/2 - 2) DO BEGIN
; ACF_Var[i] = ABS(Total( (Shift(Autocorrelation, i)*(Shift(Autocorrelation, -i))[i:N_Points - i -2]) )$
;       + Total( Autocorrelation * Autocorrelation[i]))/ (N_Points -1 - 2*i)
;ENDFOR


;Make sure there are points to plot before continuing
Dummy = Where(Finite(Autocorrelation), count)
IF count GE MIN_POINTS THEN BEGIN
    
FitOnly:

;Now compute a fit to the autocorrelation function using a (stretched)
;exponential.  The fit will be made using the MPFIT routines from
;Craig Markwardt.

;First create arrays of the x- and y-values to use.  Don't include the
; t=0 point since it is, by definition, 1.  By leaving it out you are
;allowing the fit to account for background random noise between frames.
;This noise has a characteristic time of 1 frame, and will skew the
;resulting fit.
;If the ACF_Err data is defined (it is supposed to be the standard
;error of the ACF), then use that as the error for the fit function.
;Also, truncate the ACF to be fit at the point where ACF<ACF_Err

x = times[1:N_Points-1]
y = Autocorrelation[1:N_points-1]

; check that ACF_Err wasn't just set to 1, then truncate ACF at point
; where ACF < ACF_Err, or after four points where ACF always less than ACF_Err

dummy = where(finite(acf_err), count)
IF count GT 0 THEN BEGIN
    Merit = Autocorrelation - abs(ACF_Err)
    insignificant = Where(Merit LT 0 OR autocorrelation LT 0.1, count)
    IF count GT 0 THEN BEGIN
      fit_length = insignificant[0]
      x = times[1:((MIN_POINTS -1)>insignificant[0])] ;a>b bigger one of a or b is taken
      y = Autocorrelation[1:((MIN_POINTS -1)>insignificant[0])]
    ENDIF
ENDIF
;Now, only include finite values (i.e. not NaN values)
good_ACF = Where(Finite(y), count)

;If there are not enough finite values for the ACF, do not compute a fit
IF ((count LE (MIN_POINTS-1)) OR Keyword_Set(NoFit)) THEN BEGIN
ACF_Fit = Replicate(!Values.F_Nan, N_Points -1)
Fit_Parameters = Replicate(!Values.F_Nan, 3)
GOTO, NoACF
ENDIF

;x and y do not include t=0 point
x=x[Good_ACF]
y=y[Good_ACF]

; get rid of any pesky NaN values in ACF_Err(from computation errors)
bad = Where(~Finite(ACF_Err), count)
IF count GT 0 THEN ACF_Err[bad] = 1


;now the fitting action starts
    ;fit function used is given in kww.pro
    ;y = p[0]*exp((-x/p[1])^p[2])
    ;p[0]: exponential prefactor
    ;p[1]: relaxation time tau
    ;p[2]: beta exponent
    
    ;set parameters: intial value, fixed? (0=floating, 1= fixed),limited? (1 = yes, 0 = no), limits[lower,upper]
    ;initialize Parinfo structure for all fitting parameters
    ParInfo = Replicate({value: 0.D, fixed:0, limited:[0,0], limits:[0.d, 0.d]}, 3)
       
    ;pre-factor parameter
    ParInfo[0].value = 1.0 ;initial guess 
    ParInfo[0].limited = [1,1]
    ParInfo[0].limits[0] = 0.3 ; at time 0 acf needs to be bigger than 0.3
    ParInfo[0].limits[1] = 1.2 ;at time zero acf cannot be higher than 1 gives some tolerance of 20%
    
    ;tau parameter
    tau_estimate = where(y LT 0.4, count)
    if count GT 0 then ParInfo[1].value = x[tau_estimate[0]]
    ParInfo[1].limited = [1,1]
    ;tau cannot be smaller than 3 frames and larger than half trajectory length
    
    ;12/09/09 changed lower limit from times[3] to times[1] more fast molecules
    ;included but some slow ones as well which could not be fit before!!!
    ParInfo[1].limits = [times[1],floor(max(times)/2.0)] ;this is times in seconds

    ;beta parameter
    ParInfo[2].value = 1.0
    IF ~Keyword_Set(Stretched) THEN ParInfo[2].fixed = 1
    ParInfo[2].limited = [1,1]     
    ParInfo[2].limits = [0.30,2.0] ;smallest possible beta = 0.3 and highest = 2.0
    
    p=make_array(3, value = !values.f_NAN)
    p[0:2] = parinfo[*].value
    ;Here is the actual fitting command
    Fit_Parameters = MPFITEXPR('kww(x,p[0:2])', x, y, error,$ 
                            PARINFO = ParInfo, NPrint = 25, /quiet, $
                            ftol=1d-10, xtol=1d-10 , maxiter=500, $
                            resdamp = 0, NITER = dummy, status = status)
    ACF_Fit=kww(times,Fit_Parameters); time zero is not fitted
    ;r_sq = r_square(y, acf_fit[1:n_elements(y) -1])
    ;if r_sq LT 0.90 then fit_parameters= [!values.f_NAN, !values.f_NAN, !values.f_NAN]
    if status NE 1 then begin
    if status NE 3 then begin 
    fit_parameters = [!values.f_NAN, !values.f_NAN, !values.f_NAN]
    endif
    endif

;****************DIETER_LD case starts here*****************************************************
   ;check whether Dieter_LD was choosen is yes then do linear fit 
   temp=strcmp(StrUpCase(Method), 'DIETER_LD')
   if temp EQ 1 AND finite(Fit_parameters[1]) EQ 1 then begin ;only calculate linear fit if exponential fit gave a result
   length = floor(0.20 * Fit_parameters[1] / (times[1]-times[0])) ; the number of points used for linear fit is based on tau from exponential fit
   lininfo.length = (MIN_POINTS -1) > length
    ;first pull out only the data that are supposed to be fit, then get a linear fit
    ;The result is given in the form: [Intercept, Slope]
    start = LinInfo.start
    finish = LinInfo.length + start
    ;print, 'finish - start', finish - start
    ; in case the acf in this segment is much faster than the average tau
    finish = finish < (n_elements(y) -1) ;ensure that fit_length not longer than entire acf
    if n_elements(y) GE 4 then begin; y does not include t=0, so 4 points is enough for linear fit, 5 point requirement includes t=0 point
    p=LinFit(x[start:finish], y[start:finish])
    ;Put parameters into array for passing back to main program.
    ;Element '0' is just a placeholder, element '1' is the time constant
    Fit_Parameters = [0.0, (-p[0]/p[1]), !Values.F_NaN]
    ;check that tau longer than threee frames and shorter than half the trajectory length
    Fit_Parameters[1]=((Fit_Parameters[1] GT floor(max(times)/2.0)) OR (Fit_Parameters[1] LT times[3]))$
              ? !Values.F_NaN : Fit_Parameters[1]    
    acf_fit = p[0]+p[1]*times
    ENDIF else fit_parameters =[!values.f_NAN, !values.f_NAN, !values.f_NAN]   
;******************* END of DIETER_LD CASE ********************************************************************  

ENDIF; end of if loop the check that more then min_points in acf


NoACF: ;Skip to here to avoid fitting the ACF
;if choosen BASIC acf, calculate tau_correlate from tau_fit
   temp=strcmp(StrUpCase(Method), 'BASIC_ACF')
   ;if temp EQ 1 AND finite(fit_parameters[1]) EQ 1 AND finite(fit_parameters[2]) EQ 1 then BEGIN
   ;fit_parameters[1] = kww_tau(fit_parameters[1],fit_parameters[2]) ;transform tau_fit to tau_correlate
   ;endif
 ;  if autocorrelation[1] LT 0.3679 then fit_parameters[*] = !values.f_NAN   
results = {ACF:Autocorrelation, ACF_Fit:Acf_Fit, ACF_Err:ACF_Err,$
Fit_Parameters:Fit_Parameters, r_sq:r_sq, fit_length: fit_length}
Return, results

ENDIF ELSE BEGIN ;If there weren't enough points in the ACF ...

   ERASE ; Clear screen
   XYOUTS, 'ACF not calculable', Charsize=2

   Results = {ACF:!Values.F_NaN, ACF_Fit:!Values.F_NaN, ACF_Err:!Values.F_NaN, $
         Fit_Parameters:[!Values.F_NaN,!Values.F_NaN,!Values.F_NaN], r_sq:!Values.F_NaN}
   Return, results

ENDELSE
endif else begin; check over maxframe
   ERASE ; Clear screen
   XYOUTS, 'ACF not calculable', Charsize=2

   Results = {ACF:!Values.F_NaN, ACF_Fit:!Values.F_NaN, ACF_Err:!Values.F_NaN, $
         Fit_Parameters:[!Values.F_NaN,!Values.F_NaN,!Values.F_NaN], r_sq:!Values.F_NaN}
   Return, results

endelse
END; of plot_autocorrelation program

