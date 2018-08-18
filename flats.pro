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
  pixelvalues=dblarr(totalpixels,sz)
  images=dblarr(totalpixels,sz)
  rFlat=image
  date=sxpar(header,'date-obs')
  date=strmid(date,0,10)
  dates=strarr(sz)
  flatnum=indgen(sz+1)
  binarr=[0.955,0.965,0.975,0.985,0.995,1.005,1.015,1.025,1.035,1.045]
  means=fltarr(10,sz)
  sigmas=fltarr(10,sz)
  print,'Reference flat from ',date

  for i=0,sz-1 do begin
    fits_read,flats[i+1],image,header
    datetemp=sxpar(header,'date-obs')
    dates[i]=strmid(datetemp,0,10)
    pixelvalues[*,i]=(image/rFlat)
    images[*,i]=image
    print,'Processed flat from ',dates[i]
  endfor

  refFlat=dblarr(totalpixels)
  pixel=0L
  for j=0,(fitsize[2]-1) do begin
    for k=0,(fitsize[1]-1) do begin
      refFlat[pixel]=rFlat[k,j]
      pixel=(pixel+1)
    endfor
  endfor

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
  FILE_MKDIR,plot_directory
  symbol=dindgen(20)*2*!pi/19
  usersym,sin(symbol),cos(symbol),/fill

  for i=0,sz-1 do begin
    a=stddev(pixelvalues[*,i])
    set_plot,'svg'
      device,filename=plot_directory+'/hist_'+dates[i]+'.svg',xsize=20,ysize=20
      !p.thick=4
      !x.thick=4
      !y.thick=4
      !p.charthick=4
      !p.charsize=1.6
      hist_plot,pixelvalues[*,i],bin=0.001,xtitle='Pixel Value',ytitle='Number In Each Bin!C!C',/full,xrange=[0.9,1.1],/xstyle
      xyouts,175,500,a,/device
      device,/close
    print,'Created File: '+plot_directory+'/hist_'+dates[i]+'.svg'

    set_plot,'svg'
      device,filename=plot_directory+'/plot_'+dates[i]+'.svg',xsize=20,ysize=20
      !p.thick=4
      !x.thick=4
      !y.thick=4
      !p.charthick=4
      !p.charsize=1.6
      plot,binarr,means[*,i],psym=8,XTickV=binarr,XTicks=11,XRange=[0.945,1.055],/XStyle,yrange=[0.95,1.05],/ystyle
      oploterr,binarr,means[*,i],sigmas[*,i]
      device,/close
    print,'Created File: '+plot_directory+'/plot_'+dates[i]+'.svg'
  endfor
  t2=systime(/seconds)
  print,'Total Time= ',t2-t,' Seconds'
end
