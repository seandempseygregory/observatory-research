pro flats
  print,'Select flats for analysis'
  flats = DIALOG_PICKFILE(TITLE="Select Flats",  FILTER='*.fit',/MULTIPLE_FILES)
  t=systime(/seconds)
  sz=(size(flats, /N_ELEMENTS))-1
  print,'Number of files selected =',sz+1
  caldat,systime(/julian),month,day,year,hour,minute
  plot_directory='flats_'+strcompress(string(month,day,year,hour,minute),/remove_all)
  npixels=25
  fits_read,flats[0],image,header
  fitsize=size(image)
  totalpixels=(fitsize[1]*fitsize[2])
  pixelvalues=fltarr(totalpixels,sz)
  refpixelvalues=fltarr(totalpixels)
  pix_val=fltarr(npixels,sz+1)
  mFlat=image
  date=sxpar(header,'date-obs')
  date=strmid(date,0,10)
  dates=strarr(sz)
  xpixel=fix(randomu(seed,npixels)*(fitsize[1]-1))
  ypixel=fix(randomu(seed,npixels)*(fitsize[2]-1))
  flatnum=indgen(sz+1,start=1)

  print,'Reference flat from ',date
  pixel=0L
  for i=0,npixels-1 do begin
    pix_val[i,0]=mFlat[xpixel[i],ypixel[i]]
  endfor
  for j=0,(fitsize[1]-1) do begin
    for k=0,(fitsize[2]-1) do begin
      refpixelvalues[pixel]=mFlat[j,k]
      pixel=(pixel+1)
    endfor
  endfor

  for i=0,sz-1 do begin
    pixel=0L
    fits_read,flats[i+1],image,header
    datetemp=sxpar(header,'date-obs')
    dates[i]=strmid(datetemp,0,10)
    print,'Processed flat from ',dates[i]
    for j=0,(fitsize[1]-1) do begin
      for k=0,(fitsize[2]-1) do begin
        pixelvalues[pixel,i]=(image[j,k]/mFlat[j,k])
        pixel=(pixel+1)
      endfor
    endfor
    for w=0,npixels-1 do begin
      pix_val[w,i+1]=image[xpixel[w],ypixel[w]]
    endfor
  endfor

  FILE_MKDIR,plot_directory

  set_plot,'ps'
    device,filename=plot_directory+'/pixelvalueVdate.eps',/encaps,xsize=20,ysize=20
    !p.thick=4
    !x.thick=4
    !y.thick=4
    !p.charthick=4
    !p.charsize=1.6
    plot,[0,0],[0,0],xtitle='Flat Number',ytitle='Pixel Value',xrange=[1,sz+1],yrange=[0.9,1.1],/ystyle,/xstyle
    for i=0,npixels-1 do begin
      oplot,flatnum,pix_val[i,*]
    endfor
    device,/close
  print,'Created File: '+plot_directory+'/pixelvalueVdate.eps'

  symbol=dindgen(20)*2*!pi/19
  usersym,sin(symbol),cos(symbol),/fill
  plotvalues=fltarr((totalpixels/8000),sz+1)
  k=0L
  for j=0,totalpixels-1 do begin
    if k EQ floor(totalpixels/8000) then break
    if ((j MOD 8000) EQ 0) then begin
      plotvalues[k,0]=refpixelvalues[j]
      k=k+1
    endif
  endfor
  for i=1,sz do begin
    k=0L
    for j=0,totalpixels-1 do begin
      if k EQ floor(totalpixels/8000) then break
      if ((j MOD 8000) EQ 0) then begin
        plotvalues[k,i]=pixelvalues[j,i-1]
        k=k+1
      endif
    endfor
  endfor


  for i=0,sz-1 do begin
    set_plot,'ps'
      device,filename=plot_directory+'/plot_'+dates[i]+'.eps',/encaps,xsize=20,ysize=20
      !p.thick=4
      !x.thick=4
      !y.thick=4
      !p.charthick=4
      !p.charsize=1.6
      plot,plotvalues[*,0],plotvalues[*,i+1],psym=8,ytitle='2nd Pixel Value',xtitle='Reference Pixel Value',xrange=[0.9,1.1],/xstyle,yrange=[0.9,1.1],/ystyle
      device,/close
    print,'Created File: '+plot_directory+'/plot_'+dates[i]+'.eps'
  endfor

  for i=0,sz-1 do begin
    print,'Average', mean(pixelvalues[*,i])
    set_plot,'ps'
      device,filename=plot_directory+'/hist_'+dates[i]+'.eps',/encaps,xsize=20,ysize=20
      !p.thick=4
      !x.thick=4
      !y.thick=4
      !p.charthick=4
      !p.charsize=1.6
      hist_plot,pixelvalues[*,i],bin=0.001,xtitle='Pixel Value',ytitle='Number In Each Bin',/full,xrange=[0.9,1.1],/xstyle,/fill
      xyouts,3000,17000,mean(pixelvalues[*,i]),/device
      device,/close
    print,'Created File: '+plot_directory+'/hist_'+dates[i]+'.eps'
  endfor
  t2=systime(/seconds)
  print,'Total Time= ',t2-t,' Seconds'
end
