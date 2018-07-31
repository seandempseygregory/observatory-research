pro flatscom
  horflats = DIALOG_PICKFILE(TITLE="Select Horizon Flats",  FILTER='*.fit',/MULTIPLE_FILES)
  overflats = DIALOG_PICKFILE(TITLE="Select Overhead Flats",  FILTER='*.fit',/MULTIPLE_FILES)
  sz=size(horflats, /N_ELEMENTS)
  caldat,systime(/julian),month,day,year,hour,minute
  plot_directory='flats_'+strcompress(string(month,day,year,hour,minute),/remove_all)
  FILE_MKDIR,plot_directory
  fits_read,horflats[0],image,header
  fitsize=size(image)
  totalpixels=(fitsize[1]*fitsize[2])
  pixelvalues=fltarr(totalpixels,sz)
  for i=0,sz-1 do begin
    pixel=0L
    fits_read,horflats[i],image,header
    mFlat=image
    fits_read,overflats[i],image,header
    for j=0,(fitsize[1]-1) do begin
      for k=0,(fitsize[2]-1) do begin
        pixelvalues[pixel,i]=(image[j,k]/mFlat[j,k])
        pixel=(pixel+1)
      endfor
    endfor
  endfor
  for i=0,sz-1 do begin
    print,'Average', mean(pixelvalues[*,i])
    set_plot,'ps'
      device,filename=plot_directory+'/histogram'+'_'+strtrim(string(i),2)+'.eps',/encaps,xsize=20,ysize=20
      !p.thick=4
      !x.thick=4
      !y.thick=4
      !p.charthick=4
      !p.charsize=1.6
      hist_plot,pixelvalues[*,i],bin=0.001,xtitle='Pixel Value',ytitle='Number In Each Bin',/full,xrange=[0.9,1.1],/xstyle,/fill
      device,/close
  endfor
end
