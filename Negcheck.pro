pro negcheck
  files = DIALOG_PICKFILE(TITLE="Select Files",  FILTER='*.fit',/MULTIPLE_FILES)
  t=systime(/seconds)
  sz=size(files, /N_ELEMENTS)
  print,'Number of files selected =',sz
  fits_read,files[0],image,header
  fitsize=size(image)
  num=dblarr(sz)
  totalpixels=(fitsize[1]*fitsize[2])
  for i=0,sz-1 do begin
    num[i]=0
    print,i
    fits_read,files[i],image,header
    for j=0,(fitsize[1]-1) do begin
      for k=0,(fitsize[2]-1) do begin
        pixelvalue=image[j,k]
        if pixelvalue LT 0 then (num[i]=num[i]+1)
      endfor
    endfor
  endfor
  print,num
  print,'average =',mean(num)
  print,'percent=',(mean(num)/totalpixels)*100
end
