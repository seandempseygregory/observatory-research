pro filtertest

  ;Displays dialog windows for user to select .fit files
  print,"Select .fit files to test"
  files = DIALOG_PICKFILE(TITLE="Select .fit files to test",  FILTER='*.fit',/MULTIPLE_FILES)
  sz=size(files, /N_ELEMENTS)
  print,'Number of files selected =',sz
  t=systime(/seconds)
  ;caldat,systime(/julian),month,day,year,hour,minute
  ;directory='filtertest_'+strcompress(string(month,day,year,hour,minute),/remove_all)
  ;FILE_MKDIR,directory
  filter_types={filtername:['Red','Green','Blue','U','B','V','R','I'],filternumber:[1,2,3,4,5,6,7,8]}
  for i=0,sz-1 do begin
    fits_read,files[i],image,header
    filtname=strcompress(sxpar(header,'filter'),/remove_all)
    for j=0,7 do begin
      if filtname EQ (filter_types.filtername[j] )then filtnum=filter_types.filternumber[j]
    endfor
    print,filtname,filtnum
    ;fits_write,directory+'/test_'+filtname+'_'+strtrim(string(i+1),1)+'.fit',image,header
  endfor
  t2=systime(/seconds)
  print,'Total Time= ',t2-t,' Seconds'
end
