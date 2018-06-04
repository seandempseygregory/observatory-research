pro darks,npixels
;+
; NAME:
;       DARKS
;
; PURPOSE:
;       Determines the relationship between total number of dark frames
;       and the amount of non-image related thermal and electronic noise removed
;
; CALLING SEQUENCE:
;       DARKS, NPIXELS
;
; INPUTS:
;       NPIXELS - this parameter sets the number of random pixels values to be pulled from each frame
;
; VARIABLES AND ARRAYS:
;       npixels - number of pixels to be used for analysis. User defined with NPIXELS parameter.
;       darkfiles - An array filled with the file paths to the dark frames. User defined using file selection dialog.
;       plot_directory - Contains directory path used for saving plots to, User defined using file selection dialog box.
;       sz - number of dark frames selected, value set by size of darkfiles array;
;       fitsize - resolution of dark frames (used to determine max values for pixel coordinates). Assumes all dark frames selected are same resolution.
;       noisearr - A user defined array filled with noise percentages to be evaluated (written as decimal values).
;       meanv - A array to be filled with the average pixel value for each of the chosen pixels over all frames. Size determined by npixels variable.
;       sigma - A array to be filled with the standard deviation for each of the chosen pixels over all frames. Size determined by npixels variable.
;       numdarks - An array used to hold values for the number of dark frames needed for each noise percentage for each individual pixel value.
;                  Size is determined by npixels variable.
;       ndarks - Array used to hold the average values for the number of dark frames needed for each noise percentage for all pixel values.
;                Size is determined by size of noisearr array.
;       xpixel & ypixel - Arrays filled with random pixel coordinates . Sizes are determined by the x and y resolutions of dark frames given by fitsize variable.
;       pixel_Values - A 2-dimensional array filled with the values of the pixels, given by xpixel and ypixel arrays, for each individual frame.
;                      Size is determined by sz and npixels variables.
;       pixelvalues - A 2-dimensional array filled with the values of the pixels, given by xpixel and ypixel arrays, for each averaged frame.
;                      Size is determined by sz and npixels variables.
;       numberofframes - An array used in pixel value vs # of frames averaged plot. Gives the number of frames averaged together. Size determined by sz variable
;
; DETAILED DESCRIPTION:
;       FILE INPUT & ARRAY INITIALIZATION - Present user with file selection dialog used to selection the dark frames that are to be read.
;                                          Initializes the arrays and variables needed to run the procedure.
;       FILE READING & PIXEL VALUE EXTRACTION - Present user with file selection dialog used to selection the dark frames that are to be read.
;                                               Initializes the arrays and variables needed to run the procedure.
;       MEAN PIXEL VALUE DETERMINATION - Present user with file selection dialog used to selection the dark frames that are to be read.
;                                        Initializes the arrays and variables needed to run the procedure.
;       PIXEL VALUE ANALYSIS - Present user with file selection dialog used to selection the dark frames that are to be read.
;                              Initializes the arrays and variables needed to run the procedure.
;       PLOTTING OF # OF FRAMES VS NOISE % - Present user with file selection dialog used to selection the dark frames that are to be read.
;                                            Initializes the arrays and variables needed to run the procedure.
;
; INFO:
;      AUTHOR - Sean Dempsey-Gregory
;              State University of New York at Fredonia Physics Department
;
;      LAST DATE MODIFIED -  10/12/2017
; -

;*******************************************
;     FILE INPUT & ARRAY INITIALIZATION
;*******************************************
  darkfiles = DIALOG_PICKFILE(TITLE="Select Dark Files",  FILTER='*.fit',/MULTIPLE_FILES)
  plot_directory = DIALOG_PICKFILE(TITLE="Choose directory to save plots and text files to to",/DIRECTORY)
  t=systime(/seconds)
  sz=size(darkfiles, /N_ELEMENTS)
  print,'Number of dark files selected =',sz
  fits_read,darkfiles[0],image,header
  fitsize=size(image)
  noisearr=[0.00005,0.00006,0.00007,0.00008,0.00009,0.0001,0.0002,0.0003,0.0004,0.0005,0.0006,0.0007,0.0008,0.0009,0.001,0.002,0.003,0.004,0.005,0.006,0.007,0.008,0.009,0.01]
  meanv=dblarr(npixels)
  sigma=dblarr(npixels)
  numdarks=dblarr(npixels)
  ndarks=dblarr(n_elements(noisearr))
  xpixel=fix(randomu(seed,npixels)*(fitsize[1]-1))
  ypixel=fix(randomu(seed,npixels)*(fitsize[2]-1))
  pixel_Values=dblarr(sz,npixels)
  pixelvalues=dblarr(sz,npixels)
  numberofframes=intarr(sz)
  nframesarr=[1,5,10,50,100,500,1000]
  sigmamean=dblarr(n_elements(nframesarr),npixels)
  percerr=dblarr(n_elements(nframesarr),npixels)
  meanpercerr=dblarr(n_elements(nframesarr))
  meansigmamean=dblarr(n_elements(nframesarr))
  sigmapercerr=dblarr(n_elements(nframesarr))
  for l=0,sz-1 do begin
    numberofframes[l]=l+1
  endfor

;*******************************************
;   FILE READING & PIXEL VALUE EXTRACTION
;*******************************************
  for i=0L,sz-1 do begin
    if (i MOD 100) EQ 0 then print, i
    fits_read,darkfiles[i],image,header
    for j=0,npixels-1 do begin
      pixel_Values[i,j]=image[xpixel[j],ypixel[j]]
    endfor
    image=image*1.0D
    if i eq 0 then z=image else (z=(((z*i)+image)/(i+1)))
    for h=0L,npixels-1 do begin
      pixelvalues[i,h]=z[xpixel[h],ypixel[h]]
    endfor
  endfor

;*******************************************
;       MEAN PIXEL VALUE DETERMINATION
;*******************************************
  openw,2,plot_directory+'/darkpixels.txt'
  for k=0,npixels-1 do begin
    pixel_Value=pixel_Values[*,k]
    meanv[k]=mean(pixel_Value)
    sigma[k]=stddev(pixel_Value)
    printf,2,xpixel[k],ypixel[k],meanv[k],sigma[k]
    if ((k MOD 20) EQ 0) then begin
    set_plot,'ps'
      device,filename=plot_directory+'/pixelvalue_histogram_'+strtrim(string(xpixel[k]),2)+'_'+strtrim(string(ypixel[k]),2)+'.eps',/encaps,xsize=20,ysize=20
      !p.thick=4
      !x.thick=4
      !y.thick=4
      !p.charthick=4
      !p.charsize=1.6
      hist_plot,pixel_Values[*,k],bin=20,xtitle='Pixel Value',ytitle='Number In Each Bin',xrange=[0.9,1.1]*mean(pixel_Values[*,k]),/xstyle,/full,/fill
      device,/close
    endif
  endfor
  ;Histogram Plot of Mean Pixel Value
  set_plot,'ps'
    device,filename=plot_directory+'/mean_histogram.eps',/encaps,xsize=20,ysize=20
    !p.thick=4
    !x.thick=4
    !y.thick=4
    !p.charthick=4
    !p.charsize=1.6
    hist_plot,meanv,bin=1,xtitle='Mean Value',ytitle='Number In Each Bin',/full,xrange=[0.97,1.03]*mean(meanv),/xstyle,/fill
    device,/close
  ;Histogram Plot of Standard Deviation
  set_plot,'ps'
    device,filename=plot_directory+'/sigma_histogram.eps',/encaps,xsize=20,ysize=20
    !p.thick=4
    !x.thick=4
    !y.thick=4
    !p.charthick=4
    !p.charsize=1.6
    hist_plot,sigma,bin=1,xtitle='Standard Deviation Value',ytitle='Number In Each Bin',xrange=[0.75,1.25]*mean(sigma),/xstyle,/full,/fill
    device,/close
  close,2

;*******************************************
;           PIXEL VALUE ANALYSIS
;*******************************************
  openw,1,plot_directory+'/darkpercent.txt'
  for u=0, n_elements(noisearr)-1 do begin
    for s=0,npixels-1 do begin
      darkmean=meanv[s]
      pixelvalue=pixelvalues[*,s]
      ;Plots pixelvalue vs # of frames averaged
      ;set_plot,'ps'
         ;device,filename=plot_directory+'/darkmean_'+strtrim(string(xpixel[s]),2)+'_'+strtrim(string(ypixel[s]),2)+'.eps',/encaps,xsize=20,ysize=20
         ;!p.thick=4
         ;!x.thick=4
         ;!y.thick=4
         ;!p.charthick=4
         ;!p.charsize=1.6
         ;plot,numberofframes,pixelvalue,psym=7,xtitle='# of Frames Averaged',ytitle='Pixel Value',yrange=[0.97,1.03]*meanv[s],/ystyle
         ;oplot,[0,900],[meanv[s],meanv[s]],linestyle=2
         ;device,/close
      pixels=(abs(pixelvalue - darkmean)) / darkmean
      result = where( pixels LT noisearr[u], count, COMPLEMENT=result_c, NCOMPLEMENT=count_c)
      numdarks[s]=result_c[count_c-1]
    endfor
    ;print,'Average number of darks to achieve desired level of noise =',ceil(mean(numdarks))
    ndarks[u]=mean(numdarks)
    printf,1,noisearr[u],ndarks[u]
  endfor
  close,1
  openw,3,plot_directory+'/percentError.txt'
  printf,3,'Percent Error','   Standard Deviation of the Mean','   Standard Deviation of Percent Error',format='(a13,a33,a38)'
  for v=0,n_elements(nframesarr)-1 do begin
    for p=0,npixels-1 do begin
    percerr[v,p] = ((sigma[p]/SQRT(nframesarr[v]))/meanv[p])*100
    sigmamean[v,p] = (sigma[p]/SQRT(nframesarr[v]))
    endfor
    meanpercerr[v]=mean(percerr[v,*])
    meansigmamean[v]=mean(sigmamean[v,*])
    sigmapercerr[v]=stddev(percerr[v,*])
    printf,3,meanpercerr[v],'     ',meansigmamean[v],'               ',sigmapercerr[v],format='(e8.2,a10,e8.2,a30,e8.2)'
  endfor
  close,3


;*******************************************
;    PLOTTING OF # OF FRAMES VS NOISE %
;*******************************************
  symbol=dindgen(20)*2*!pi/19
  usersym,sin(symbol),cos(symbol),/fill
  set_plot,'ps'
    device,filename=plot_directory+'/darkpercent.eps',/encaps,xsize=20,ysize=20
    !p.thick=4
    !x.thick=4
    !y.thick=4
    !p.charthick=4
    !p.charsize=1.6
    plot,noisearr*100,ndarks,psym=8,xtitle='% level',ytitle='# of Darks',/xl,xrange=[1.1,0.008],/xstyle,/yl
    device,/close
  set_plot,'ps'
    device,filename=plot_directory+'/percenterror.eps',/encaps,xsize=20,ysize=20
    !p.thick=4
    !x.thick=4
    !y.thick=4
    !p.charthick=4
    !p.charsize=1.6
    plot,nframesarr,meanpercerr,psym=8,xtitle='N!DFrames!N',ytitle='Mean % Error',xrange=[0.7,1400],/xl,/xstyle,/yl
    ;errplot,nframesarr,meanpercerr-sigmapercerr,meanpercerr+sigmapercerr
    device,/close
  set_plot,'ps'
    device,filename=plot_directory+'/sigmamean.eps',/encaps,xsize=20,ysize=20
    !p.thick=4
    !x.thick=4
    !y.thick=4
    !p.charthick=4
    !p.charsize=1.6
    plot,nframesarr,meansigmamean,psym=8,xtitle='N!DFrames!N',ytitle='Mean Standard Deviation of the Mean (Counts)',xrange=[0.7,1400],/xl,/xstyle,/yl
    errplot,nframesarr,meansigmamean-sigmapercerr,meansigmamean+sigmapercerr
    device,/close
  t2=systime(/seconds)
  print,'Total Time= ',t2-t,' Seconds'
end
