; INFO
;
;NAME:
;       FLATS
;
; PURPOSE:
;             Produces plots that show the consistency of flat field frames across different nights
;
; INFO:
;      AUTHOR - Sean Dempsey-Gregory
;              State University of New York at Fredonia Physics Department
;
;      LAST DATE MODIFIED -  08/27/2018

pro flats
  ;Displays a dialog box for the user to select the flat field frames, more than one file must be selected,
  ;the first file read in is used as a reference frame, this is determined alphabetically by filename
  print,'Select flats for analysis'
  flats = DIALOG_PICKFILE(TITLE="Select Flats",  FILTER='*.fit',/MULTIPLE_FILES)
  t=systime(/seconds)
  sz=(size(flats, /N_ELEMENTS))-1
  print,'Number of files selected =',sz+1

; Read in reference flat and determine the number of pixels in the frames, also initializes arrays
  fits_read,flats[0],image,header
  fitsize=size(image)
  totalpixels=(fitsize[1]*fitsize[2])
  pixelvalues=dblarr(totalpixels,sz)
  images=dblarr(totalpixels,sz)
  refFlat=image
  date=sxpar(header,'date-obs')
  date=strmid(date,0,10)
  dates=strarr(sz)
  flatnum=indgen(sz+1)
  binarr=[0.955,0.965,0.975,0.985,0.995,1.005,1.015,1.025,1.035,1.045]
  means=fltarr(10,sz)
  sigmas=fltarr(10,sz)

  print,'Reference flat from ',date         ; NOTE: Dates are read from the header of the fit file and are in GMT

;Reads in the rest of the flat field frames, divides the frame by the reference frame and records all relevant data to arrays
  for i=0,sz-1 do begin
    fits_read,flats[i+1],image,header
    datetemp=sxpar(header,'date-obs')
    dates[i]=strmid(datetemp,0,10)
    pixelvalues[*,i]=(image/refFlat)
    images[*,i]=image
    print,'Processed flat from ',dates[i]
  endfor

;Seperates the pixel of the reference frame into 10 bins
  w1=where((refFlat GE 0.95) AND (refFlat LT 0.96))
  w2=where((refFlat GE 0.96) AND (refFlat LT 0.97))
  w3=where((refFlat GE 0.97) AND (refFlat LT 0.98))
  w4=where((refFlat GE 0.98) AND (refFlat LT 0.99))
  w5=where((refFlat GE 0.99) AND (refFlat LT 1.00))
  w6=where((refFlat GE 1.00) AND (refFlat LT 1.01))
  w7=where((refFlat GE 1.01) AND (refFlat LT 1.02))
  w8=where((refFlat GE 1.02) AND (refFlat LT 1.03))
  w9=where((refFlat GE 1.03) AND (refFlat LT 1.04))
  w10=where((refFlat GE 1.04) AND (refFlat LT 1.05))

;Using the bins created above, creates 10 bins for each non-reference frame,
;and fills them with the corresponing pixel values according tho the pixels in the original bins
;Also determines the mean and standard deviation of each of the 10 bins
  for i=0,sz-1 do begin
    q=images[*,i]
    newimage1=q[w1]
    newimage2=q[w2]
    newimage3=q[w3]
    newimage4=q[w4]
    newimage5=q[w5]
    newimage6=q[w6]
    newimage7=q[w7]
    newimage8=q[w8]
    newimage9=q[w9]
    newimage10=q[w10]
    means[0,i]=mean(newimage1)
    means[1,i]=mean(newimage2)
    means[2,i]=mean(newimage3)
    means[3,i]=mean(newimage4)
    means[4,i]=mean(newimage5)
    means[5,i]=mean(newimage6)
    means[6,i]=mean(newimage7)
    means[7,i]=mean(newimage8)
    means[8,i]=mean(newimage9)
    means[9,i]=mean(newimage10)
    sigmas[0,i]=stddev(newimage1)
    sigmas[1,i]=stddev(newimage2)
    sigmas[2,i]=stddev(newimage3)
    sigmas[3,i]=stddev(newimage4)
    sigmas[4,i]=stddev(newimage5)
    sigmas[5,i]=stddev(newimage6)
    sigmas[6,i]=stddev(newimage7)
    sigmas[7,i]=stddev(newimage8)
    sigmas[8,i]=stddev(newimage9)
    sigmas[9,i]=stddev(newimage10)
  endfor

;Creates a unique file directory based on the current date and time
  caldat,systime(/julian),month,day,year,hour,minute
  plot_directory='flats_'+strcompress(string(month,day,year,hour,minute),/remove_all)
  FILE_MKDIR,plot_directory
  symbol=dindgen(20)*2*!pi/19
  usersym,sin(symbol),cos(symbol),/fill

;Plots a histogram of pixel values divided by the reference pixel values, for each of the non-reference frames
;Prints the standard deviation on the plot
  for i=0,sz-1 do begin
    a=stddev(pixelvalues[*,i])
    set_plot,'ps'
      device,filename=plot_directory+'/hist_'+dates[i]+'.eps',/encaps,xsize=20,ysize=20
      !p.thick=4
      !x.thick=4
      !y.thick=4
      !p.charthick=4
      !p.charsize=1.6
      hist_plot,pixelvalues[*,i],bin=0.001,xtitle='Pixel Value',ytitle='Number In Each Bin',/full,xrange=[0.9,1.1],/xstyle
      xyouts,3000,17000,a,/device
      device,/close
    print,'Created File: '+plot_directory+'/hist_'+dates[i]+'.eps'

;Plots the mean value of each of the 10 bins for all of the non-reference frames, with error bars representing the standard deviation
    set_plot,'ps'
      device,filename=plot_directory+'/plot_'+dates[i]+'.eps',/encaps,xsize=20,ysize=20
      !p.thick=4
      !x.thick=4
      !y.thick=4
      !p.charthick=4
      !p.charsize=1.6
      plot,binarr,means[*,i],psym=8,XRange=[0.945,1.055],/XStyle,yrange=[0.95,1.05],/ystyle
      oploterr,binarr,means[*,i],sigmas[*,i]
      device,/close
    print,'Created File: '+plot_directory+'/plot_'+dates[i]+'.eps'
  endfor

  t2=systime(/seconds)
  print,'Total Time= ',t2-t,' Seconds'
end
