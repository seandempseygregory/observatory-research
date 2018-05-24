pro master

  ;Displays dialog window for user to select .fit files
  files = DIALOG_PICKFILE(TITLE="Select .fit Files",  FILTER='*.fit',/MULTIPLE_FILES)
  t=systime(/seconds)
  sz=size(files, /N_ELEMENTS)
  print,'Number of files selected =',sz
  fits_read,files[0],image,header
  fitsize=size(image)
  flats=fltarr(3,sz)
  darks=fltarr(2,sz)
  
  ;****************************************
  ;Reading in and initial sorting of files
  ;****************************************
  ndarks=0
  nflats=0
  for i=0,sz-1 do begin
    fits_read,files[i],image,header
    if sxpar(header,'picttype') EQ 3 then begin     ;If fit file is Dark Frame output position in files array and integration time into darks array
      darks[0,ndarks]=i
      darks[1,ndarks]=sxpar(header,'exptime')
      ndarks=ndarks+1
    endif else begin
      if sxpar(header,'picttype') EQ 4 then begin   ;If fit file is Flat Field Frame output position in files array,filter type,and integration time into flats array
        flats[0,nflats]=i
        x=strcompress(sxpar(header,'filter'),/remove_all)
        if x EQ 'Red' then (filter=1) else if x EQ 'Green' then (filter=2) else if x EQ 'Blue' then (filter=3) else (filter=99)
        flats[1,nflats]=filter
        flats[2,nflats]=sxpar(header,'exptime')
        nflats=nflats+1
      endif else begin
        print,'Could not determine type of frame.', files[i]       ;Prints error if file is not labeled as a dark or flat in the file header
        stop
        break
      endelse
    endelse
  endfor

  ; Trims the flats and darks arrays to get rid of null entries.
  flatstrim=fltarr(3,nflats)
  darkstrim=fltarr(2,ndarks)
  for i=0,nflats-1 do begin
    flatstrim[0,i]=flats[0,i]
    flatstrim[1,i]=flats[1,i]
    flatstrim[2,i]=flats[2,i]
  endfor
  for i=0,ndarks-1 do begin
    darkstrim[0,i]=darks[0,i]
    darkstrim[1,i]=darks[1,i]
  endfor
  delvar,darks,flats
  darks=darkstrim
  flats=flatstrim
  delvar,darkstrim,flatstrim

  print,'Number of Dark Frames imported =',ndarks
  print,'Number of Flat Frames imported =',nflats

  ;****************************************
  ;Sorting of files
  ;****************************************
  darkSortIndex=bsort(darks[1,*])
  for i=0,1 do darks[i,*]=darks[i,darkSortIndex]

  flatSortIndex=bsort(flats[2,*])
  for i=0,2 do flats[i,*]=flats[i,flatSortIndex]
  flatSortIndex=bsort(flats[1,*])
  for i=0,2 do flats[i,*]=flats[i,flatSortIndex]

  ;****************************************
  ;Pre-Analysis of Flats
  ;****************************************

  ;Determines which different integration times are present
  nExps=1
  exptimes=fltarr(nflats+ndarks)
  pExptime=flats[2,0]
  exptimes[0]=pExptime
  for i=1,nflats-1 do begin
    exptime=flats[2,i]
    if exptime NE pExptime then begin
       nExps=nExps+1
       exptimes[nExps-1]=exptime
     endif
    pExptime=exptime
  endfor
  pExptime=darks[1,0]
  for i=1,ndarks-1 do begin
    exptime=darks[1,i]
    if exptime NE pExptime then begin
       nExps=nExps+1
       exptimes[nExps-1]=exptime
     endif
    pExptime=exptime
  endfor
  nExps=size(uniq(exptimes,sort(exptimes)),/n_elements)
  exptimesTrim=fltarr(nExps)
  exptimesTrim=exptimes[uniq(exptimes,sort(exptimes))]
  delvar,exptimes
  exptimes=fltarr(nExps-1)
  for i=1,nExps-1 do begin
    exptimes[i-1]=exptimesTrim[i]
  endfor
  delvar,exptimesTrim
  nExps=nExps-1
  print,'Number of different Exposure Times =',nExps
  ;Determines which different filters are present
  nFilts=1
  filters=fltarr(nflats)
  pFilter=flats[1,0]
  filters[0]=pFilter
  for i=1,nflats-1 do begin
    filter=flats[1,i]
    if filter NE pFilter then begin
       nFilts=nFilts+1
       filters[nFilts-1]=filter
     endif
    pFilter=filter
  endfor
  filtersTrim=fltarr(nFilts)
  for i=0,nFilts-1 do filtersTrim[i]=filters[i]
  filters=filtersTrim
  delvar,filtersTrim
  print,'Number of different Filters Used =',nFilts
  ;****************************************
  ;Creation of Master Darks
  ;****************************************
  fullCount = 0
  for i=0,nExps-1 do begin
    tempCount=0

    repeat begin
      if tempCount EQ 0 then begin
        fits_read,files[darks[0,fullCount]],image,header
        mDark=image*1.0D
      endif else begin
        mDark=(mDark+image)
      endelse
    fullCount=fullCount+1
    tempCount=tempCount+1
    if (fullCount LT nDarks) then begin
      fits_read,files[darks[0,fullCount]],image,header
      image=image*1.0D
    endif else begin
      break
    endelse
  endrep until (sxpar(header,'exptime') NE exptimes[i])
  mDark=mDark/tempCount

  fits_write,'master_dark_'+strtrim(string(exptimes[i]),1)+'s.fit',mDark
  print,'Created File: master_dark_'+strtrim(string(exptimes[i]),1)+'s.fit'
  endfor

;****************************************
;Creation of Master Flats
;****************************************

fullCount = 0
for i=0,nFilts-1 do begin
  filter=flats[1,fullCount]
  tempCount=0
  repeat begin
    fits_read,files[flats[0,fullCount]],image,header
    flat=image*1.0D
    expTime=flats[2,fullCount]
    flag=0
    for j=0,nExps-1 do begin
      if exptimes[j] EQ expTime then flag=1
    endfor
    if flag EQ 0 then begin
      print,'ERROR: There is no'+strtrim(string(expTime),1)+'s master dark file present'
      break
    endif
    fits_read,'master_dark_'+strtrim(string(expTime),1)+'s.fit',image,header
    dark=image*1.0D
    if tempCount EQ 0 then begin
      mFlat=((flat-dark)/(median(flat-dark)))
    endif else begin
      mFlat=((mFlat+((flat-dark)/(median(flat-dark)))))
    endelse
    tempCount=tempCount+1
    fullCount=fullCount+1
    if fullCount EQ nFlats then break
  endrep until (flats[1,fullCount]) NE filter
  if flag EQ 0 then break
  if filter EQ 1 then (filterColor='Red') else if filter EQ 2 then (filterColor='Green') else if filter EQ 3 then (filterColor='Blue') else (filterColor='Unknown')
  mFlat=mFlat/tempCount
  fits_write,'master_flat_'+filterColor+'.fit',mFlat
  h=headfits('master_flat_'+filterColor+'.fit')
  sxaddpar,h,'FILTER',filterColor
  modfits,'master_flat_'+filterColor+'.fit',0,h
  print,'Created File: master_flat_'+filterColor+'.fit'
  if (fullCount EQ nFlats-1) then break
endfor

t2=systime(/seconds)
print,'Total Time= ',t2-t,' Seconds'
end
