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
  filter_types={filtername:['Red','Green','Blue','U','B','V','R','I'],filternumber:[1,2,3,4,5,6,7,8]}
  nfilters=size(filter_types.filternumber, /N_ELEMENTS)
  caldat,systime(/julian),month,day,year,hour,minute
  directory='masters_'+strcompress(string(month,day,year,hour,minute),/remove_all)

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
        filter=9999
        for j=0,nfilters-1 do begin
          if x EQ (filter_types.filtername[j] )then filter=filter_types.filternumber[j]
        endfor
        if filter EQ 9999 then begin
          print,'*****ERROR**** Could not determine filter type.', files[i]
          stop
        endif
        flats[1,nflats]=filter
        flats[2,nflats]=sxpar(header,'exptime')
        nflats=nflats+1
      endif else begin
        print,''
        print,'*****ERROR**** Could not determine type of frame.', files[i]       ;Prints error if file is not labeled as a dark or flat in the file header
        print,''
        stop
        break
      endelse
    endelse
  endfor

  if ndarks EQ 0 then begin
    print,''
    print,'*****ERROR**** No dark frames provided'
    print,''
    stop
  endif
  if nflats EQ 0 then begin
    print,''
    print,'*****ERROR**** No flat field frames provided'
    print,''
    stop
  endif

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
  darkexptimes=fltarr(ndarks)
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
  flatexptimes=exptimes
  pExptime=darks[1,0]
  darkexptimes[0]=pExptime
  temp=1
  for i=1,ndarks-1 do begin
    exptime=darks[1,i]
    if exptime NE pExptime then begin
       nExps=nExps+1
       temp=temp+1
       exptimes[nExps-1]=exptime
       darkexptimes[temp-1]=exptime
     endif
    pExptime=exptime
  endfor

  nExps=size(uniq(exptimes,sort(exptimes)),/n_elements)
  exptimesTrim=fltarr(nExps)
  exptimesTrim=exptimes[uniq(exptimes,sort(exptimes))]
  delvar,exptimes
  nflatExps=size(uniq(flatexptimes,sort(flatexptimes)),/n_elements)
  flatexptimesTrim=fltarr(nflatExps)
  flatexptimesTrim=flatexptimes[uniq(flatexptimes,sort(flatexptimes))]
  delvar,flatexptimes
  ndarkExps=size(uniq(darkexptimes,sort(darkexptimes)),/n_elements)
  darkexptimesTrim=fltarr(ndarkExps)
  darkexptimesTrim=darkexptimes[uniq(darkexptimes,sort(darkexptimes))]
  delvar,darkexptimes

  exptimes=fltarr(nExps-1)
  for i=1,nExps-1 do begin
    exptimes[i-1]=exptimesTrim[i]
  endfor
  delvar,exptimesTrim
  flatexptimes=fltarr(nflatExps-1)
  for i=1,nflatExps-1 do begin
    flatexptimes[i-1]=flatexptimesTrim[i]
  endfor
  delvar,flatexptimesTrim
  darkexptimes=fltarr(ndarkExps-1)
  for i=1,ndarkExps-1 do begin
    darkexptimes[i-1]=darkexptimesTrim[i]
  endfor
  delvar,darkexptimesTrim

for i=0,nflatExps-2 do begin
  flatexptime=flatexptimes[i]
  flag=0
  for j=0,ndarkExps-2 do begin
    darkexptime=darkexptimes[j]
    if darkexptime EQ flatexptime then flag=1
  endfor
  if flag EQ 0 then begin
    print,''
    print,'*****ERROR**** There are no '+strtrim(string(flatexpTime),1)+'s dark frames'
    print,''
    stop
  endif
endfor

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

  FILE_MKDIR,directory

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
    sxaddpar,h,'EXPTIME',exptimes[i]
    sxaddpar,h,'PICTYPE',3
    sxaddpar,h,'IMGTYPE','Dark Frame'
    fits_write,directory+'/master_dark_'+strtrim(string(exptimes[i]),1)+'s.fit',mDark,h
    print,'Created File: '+directory+'/master_dark_'+strtrim(string(exptimes[i]),1)+'s.fit'
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
      for j=0,nExps-1 do begin
        if exptimes[j] EQ expTime then flag=1
      endfor
      fits_read,directory+'/master_dark_'+strtrim(string(expTime),1)+'s.fit',image,header
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
    for j=0,nfilters-1 do begin
      if filter EQ filter_types.filternumber[j] then filterColor=(filter_types.filtername[j] )
    endfor
    mFlat=mFlat/tempCount
    sxaddpar,g,'FILTER',filterColor
    sxaddpar,g,'PICTYPE',4
    sxaddpar,g,'IMGTYPE','Flat Field'
    fits_write,directory+'/master_flat_'+filterColor+'.fit',mFlat,g
    print,'Created File:'+directory+' master_flat_'+filterColor+'.fit'
    if (fullCount EQ nFlats-1) then break
  endfor

  t2=systime(/seconds)
  print,'Total Time= ',t2-t,' Seconds'
end
