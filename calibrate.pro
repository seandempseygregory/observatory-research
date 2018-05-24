pro calibrate

  ;Displays dialog windows for user to select .fit files
  print,"Select .fit Files to calibrate"
  files = DIALOG_PICKFILE(TITLE="Select .fit Files to calibrate",  FILTER='*.fit',/MULTIPLE_FILES)
  print, "Select master dark"
  dark = DIALOG_PICKFILE(TITLE="Select Master Dark",  FILTER='*.fit')
  print, "Select master flats"
  flats = DIALOG_PICKFILE(TITLE="Select Master Flats",  FILTER='*.fit',/MULTIPLE_FILES)
  t=systime(/seconds)
  sz=size(files, /N_ELEMENTS)
  nflats=size(flats, /N_ELEMENTS)
  print,'Number of files selected =',sz
  fits_read,files[0],image,header
  fitsize=size(image)
  filters=fltarr(nflats)
  fits_read,dark,mdark,header

  ;****************************************
  ;Determine filter type for master flats
  ;****************************************
  for i=0,nflats-1 do begin
    fits_read,flats[i],image,header
    x=strcompress(sxpar(header,'filter'),/remove_all)
    if x EQ 'Red' then (rflat=image) else if x EQ 'Green' then (gflat=image) else if x EQ 'Blue' then (bflat=image)
  endfor

  ;****************************************
  ;Calibration
  ;****************************************
  for i=0,sz-1 do begin
    fits_read,files[i],image,header
    x=strcompress(sxpar(header,'filter'),/remove_all)
    if x EQ 'Red' then (flat=rflat) else if x EQ 'Green' then (flat=gflat) else if x EQ 'Blue' then (flat=bflat)
    image=((image-mdark)/flat)
    fits_write,'calibrated_files/calibrated_'+strtrim(string(i+1),1)+'_'+x+'.fit',image
    modfits,'calibrated_files/calibrated_'+strtrim(string(i+1),1)+'_'+x+'.fit',0,header
  endfor
end
