pro flats
  print,'Select flats for analysis'
  flats = DIALOG_PICKFILE(TITLE="Select Flats",  FILTER='*.fit',/MULTIPLE_FILES)
  t=systime(/seconds)
  sz=(size(flats, /N_ELEMENTS))-1
  print,'Number of files selected =',sz+1
  caldat,systime(/julian),month,day,year,hour,minute
  plot_directory='flats_'+strcompress(string(month,day,year,hour,minute),/remove_all)
  fits_read,flats[0],image,header
  fitsize=size(image)
  totalpixels=(fitsize[1]*fitsize[2])
  pixelvalues=fltarr(totalpixels,sz)
  mFlat=image
  date=sxpar(header,'date-obs')
  date=strmid(date,0,10)
  dates=strarr(sz)
  print,'Reference flat from ',date
  for i=0,sz-1 do begin
    if i GT 0 then begin
      t4=t3
    endif
    t3=systime(/seconds)
    pixel=0L
    fits_read,flats[i+1],image,header
    datetemp=sxpar(header,'date-obs')
    dates[i]=strmid(datetemp,0,10)
    print,dates[i]
    if i GT 0 then begin
      print,'Time=',t3-t4,'seconds'
    endif
    for j=0,(fitsize[1]-1) do begin
      for k=0,(fitsize[2]-1) do begin
        pixelvalues[pixel,i]=(image[j,k]/mFlat[j,k])
        pixel=(pixel+1)
      endfor
    endfor
  endfor
  FILE_MKDIR,plot_directory
  for i=0,sz-1 do begin
    print,'Average', mean(pixelvalues[*,i])
    set_plot,'ps'
      device,filename=plot_directory+'/histogram'+'_'+dates[i]+'.eps',/encaps,xsize=20,ysize=20
      !p.thick=4
      !x.thick=4
      !y.thick=4
      !p.charthick=4
      !p.charsize=1.6
      hist_plot,pixelvalues[*,i],bin=0.001,xtitle='Pixel Value',ytitle='Number In Each Bin',/full,xrange=[0.9,1.1],/xstyle,/fill
      device,/close
    print,'Created File:'+plot_directory+'/histogram'+'_'+dates[i]+'.eps'
  endfor
  t2=systime(/seconds)
  print,'Total Time= ',t2-t,' Seconds'
end
