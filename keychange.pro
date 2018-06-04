pro keychange,key,newvalue

  ;Displays dialog windows for user to select .fit files
  print,"Select .fit Files to edit"
  files = DIALOG_PICKFILE(TITLE="Select .fit Files to calibrate",  FILTER='*.fit',/MULTIPLE_FILES)
  sz=size(files, /N_ELEMENTS)
  print,'Number of files selected =',sz
  t=systime(/seconds)
  caldat,systime(/julian),month,day,year,hour,minute
  directory='modified_'+strcompress(string(month,day,year,hour,minute),/remove_all)
  FILE_MKDIR,directory

  for i=0,sz-1 do begin
    fits_read,files[i],image,header
    sxaddpar,header,key,newvalue
    fits_write,directory+'/'+strtrim(string(i+1),1)+'.fit',image,header
  endfor
  t2=systime(/seconds)
  print,'Total Time= ',t2-t,' Seconds'
end
