pro flats
  mainflat = DIALOG_PICKFILE(TITLE="Select Main Flat",  FILTER='*.fit')
  flats = DIALOG_PICKFILE(TITLE="Select Other Flats",  FILTER='*.fit',/MULTIPLE_FILES)
  sz=size(flats, /N_ELEMENTS)
  fits_read,mainflat,image,header
  fitsize=size(image)
  totalpixels=(fitsize[1]*fitsize[2])
  pixelvalues=fltarr(totalpixels)
  mFlat=image
  pixel=0
  for i=0,sz-1 do begin
    fits_read,flats[i],image,header
    for j=0,(fitsize[1]-1) do begin
      for k=0,(fitsize[2]-1) do begin
        pixelvalues[pixel]=(image[j,k]/mFlat[j,k])
        pixel=pixel+1
      endfor
    endfor
  endfor
  print,pixelvalues
  print,'Average', mean(pixelvalues)
end
