type
  TVertical = record red,green,blue,alpha,count: TAccumulator; end;
  PVertical = ^TVertical;
var
  verticals: PVertical;
  left,right,width,height: NativeInt;
  iRadiusX,iRadiusY: NativeInt;
  factExtraX,factExtraY: NativeUInt;

  procedure PrepareVerticals;
  var
    xb,yb: NativeInt;
    psrc,p: PByte;
    pvert : PVertical;
    {%H-}a2: NativeUInt;
    delta: PtrInt;
    srcPixSize: Integer;
  begin
    delta := ASource.RowSize;
    if ASource.LineOrder = riloBottomToTop then delta := -delta;
    srcPixSize := ASource.Colorspace.GetSize;

    fillchar(verticals^, width*sizeof(TVertical), 0);
    psrc := ASource.GetPixelAddress(left,ABounds.Top);
    pvert := verticals;
    if factExtraY = 0 then
    begin
      for xb := left to right-1 do
      begin
        p := psrc;
        for yb := 0 to iRadiusY-1 do
        begin
          if yb = height then break;
          {$IFDEF PARAM_BYTEMASK}
          pvert^.green += p^;
          pvert^.alpha += 1;
          {$ELSE}
          with PBGRAPixel(p)^ do
          if alpha <> 0 then
          begin
            a2 := alpha;
            {$HINTS OFF}
            pvert^.red += red * a2;
            pvert^.green += green * a2;
            pvert^.blue += blue * a2;
            pvert^.alpha += a2;
            {$HINTS ON}
          end;
          {$ENDIF}
          inc(pvert^.count);
          inc(p, delta);
        end;
        inc(pvert);
        inc(psrc, srcPixSize);
      end;
    end else
    begin
      for xb := left to right-1 do
      begin
        p := psrc;
        for yb := 0 to iRadiusY-1 do
        begin
          if yb = height then break;
          {$IFDEF PARAM_BYTEMASK}
          pvert^.green += p^ * factMainY;
          pvert^.alpha += factMainY;
          {$ELSE}
          with PBGRAPixel(p)^ do
          if alpha <> 0 then
          begin
            a2 := alpha * factMainY;
            {$HINTS OFF}
            pvert^.red += red * a2;
            pvert^.green += green * a2;
            pvert^.blue += blue * a2;
            pvert^.alpha += a2;
            {$HINTS ON}
          end;
          {$ENDIF}
          inc(pvert^.count, factMainY);
          inc(p, delta);
        end;
        if iRadiusY < height then
        begin
          {$IFDEF PARAM_BYTEMASK}
          pvert^.green += p^ * factExtraY;
          pvert^.alpha += factExtraY;
          {$ELSE}
          with PBGRAPixel(p)^ do
          if alpha <> 0 then
          begin
            a2 := alpha * factExtraY;
            {$HINTS OFF}
            pvert^.red += red * a2;
            pvert^.green += green * a2;
            pvert^.blue += blue * a2;
            pvert^.alpha += a2;
            {$HINTS ON}
          end;
          {$ENDIF}
          inc(pvert^.count, factExtraY);
        end;
        inc(pvert);
        inc(psrc, srcPixSize);
      end;
    end;
  end;

  procedure NextVerticals(y: integer);
  var
    psrc0,psrc1,psrc2,psrc3: PByte;
    pvert : PVertical;
    xb: NativeInt;
    {%H-}a2: NativeUInt;
    srcPixSize: Integer;
  begin
    pvert := verticals;
    if y-iRadiusY-1 >= ABounds.Top then
      psrc1 := ASource.GetPixelAddress(left, y-iRadiusY-1)
    else
      psrc1 := nil;
    if y+iRadiusY < ABounds.Bottom then
      psrc2 := ASource.GetPixelAddress(left, y+iRadiusY)
    else
      psrc2 := nil;
    srcPixSize := ASource.Colorspace.GetSize;
    if factExtraY = 0 then
    begin
      for xb := width-1 downto 0 do
      begin
        if psrc1 <> nil then
        begin
          {$IFDEF PARAM_BYTEMASK}
          pvert^.green -= psrc1^;
          pvert^.alpha -= 1;
          {$ELSE}
          with PBGRAPixel(psrc1)^ do
          if alpha <> 0 then
          begin
            {$HINTS OFF}
            pvert^.red -= red * alpha;
            pvert^.green -= green * alpha;
            pvert^.blue -= blue * alpha;
            pvert^.alpha -= alpha;
            {$HINTS ON}
          end;
          {$ENDIF}
          dec(pvert^.count);
          inc(psrc1,srcPixSize);
        end;
        if psrc2 <> nil then
        begin
          {$IFDEF PARAM_BYTEMASK}
          pvert^.green += psrc2^;
          pvert^.alpha += 1;
          {$ELSE}
          with PBGRAPixel(psrc2)^ do
          if alpha <> 0 then
          begin
            {$HINTS OFF}
            pvert^.red += red * alpha;
            pvert^.green += green * alpha;
            pvert^.blue += blue * alpha;
            pvert^.alpha += alpha;
            {$HINTS ON}
          end;
          {$ENDIF}
          inc(pvert^.count);
          inc(psrc2,srcPixSize);
        end;
        inc(pvert);
      end;
    end else
    begin
      if y-iRadiusY-2 >= ABounds.Top then
        psrc0 := ASource.GetPixelAddress(left,y-iRadiusY-2)
      else
        psrc0 := nil;
      if y+iRadiusY+1 < ABounds.Bottom then
        psrc3 := ASource.GetPixelAddress(left,y+iRadiusY+1)
      else
        psrc3 := nil;
      for xb := width-1 downto 0 do
      begin
        if psrc0 <> nil then
        begin
          {$IFDEF PARAM_BYTEMASK}
          pvert^.green -= psrc0^*factExtraY;
          pvert^.alpha -= factExtraY;
          {$ELSE}
          with PBGRAPixel(psrc0)^ do
          if alpha <> 0 then
          begin
            a2 := alpha*factExtraY;
            {$HINTS OFF}
            pvert^.red -= red * a2;
            pvert^.green -= green * a2;
            pvert^.blue -= blue * a2;
            pvert^.alpha -= a2;
            {$HINTS ON}
          end;
          {$ENDIF}
          dec(pvert^.count,factExtraY);
          inc(psrc0,srcPixSize);
        end;
        if psrc1 <> nil then
        begin
          {$IFDEF PARAM_BYTEMASK}
          pvert^.green -= psrc1^*(factMainY - factExtraY);
          pvert^.alpha -= (factMainY - factExtraY);
          {$ELSE}
          with PBGRAPixel(psrc1)^ do
          if alpha <> 0 then
          begin
            a2 := alpha*(factMainY - factExtraY);
            {$HINTS OFF}
            pvert^.red -= red * a2;
            pvert^.green -= green * a2;
            pvert^.blue -= blue * a2;
            pvert^.alpha -= a2;
            {$HINTS ON}
          end;
          {$ENDIF}
          dec(pvert^.count, factMainY - factExtraY);
          inc(psrc1,srcPixSize);
        end;
        if psrc2 <> nil then
        begin
          {$IFDEF PARAM_BYTEMASK}
          pvert^.green += psrc2^*(factMainY - factExtraY);
          pvert^.alpha += (factMainY - factExtraY);
          {$ELSE}
          with PBGRAPixel(psrc2)^ do
          if alpha <> 0 then
          begin
            a2 := alpha*(factMainY - factExtraY);
            {$HINTS OFF}
            pvert^.red += red * a2;
            pvert^.green += green * a2;
            pvert^.blue += blue * a2;
            pvert^.alpha += a2;
            {$HINTS ON}
          end;
          {$ENDIF}
          inc(pvert^.count, factMainY - factExtraY);
          inc(psrc2,srcPixSize);
        end;
        if psrc3 <> nil then
        begin
          {$IFDEF PARAM_BYTEMASK}
          pvert^.green += psrc3^*factExtraY;
          pvert^.alpha += factExtraY;
          {$ELSE}
          with PBGRAPixel(psrc3)^ do
          if alpha <> 0 then
          begin
            a2 := alpha*factExtraY;
            {$HINTS OFF}
            pvert^.red += red * a2;
            pvert^.green += green * a2;
            pvert^.blue += blue * a2;
            pvert^.alpha += a2;
            {$HINTS ON}
          end;
          {$ENDIF}
          inc(pvert^.count,factExtraY);
          inc(psrc3,srcPixSize);
        end;
        inc(pvert);
      end;
    end;
  end;

  procedure MainLoop;
  var
    xb,yb,xdest: NativeInt;
    pdest: PByte;
    pvert : PVertical;
    sumRed,sumGreen,sumBlue,sumAlpha,sumCount,
    sumRed2,sumGreen2,sumBlue2,sumAlpha2,sumCount2,
    {%H-}sumRed3,sumGreen3,{%H-}sumBlue3,sumAlpha3,{%H-}sumCount3: TAccumulator;
    destPixSize: Integer;
  begin
    destPixSize := ADestination.Colorspace.GetSize;
    for yb := ABounds.Top to ABounds.Bottom-1 do
    begin
      NextVerticals(yb);
      if Assigned(ACheckShouldStop) and ACheckShouldStop(yb) then exit;
      pdest := ADestination.GetPixelAddress(left,yb);
      sumRed := 0;
      sumGreen := 0;
      sumBlue := 0;
      sumAlpha := 0;
      sumCount := 0;
      pvert := verticals;
      for xb := 0 to iRadiusX-1 do
      begin
        if xb = width then break;
        sumRed += pvert^.red;
        sumGreen += pvert^.green;
        sumBlue += pvert^.blue;
        sumAlpha += pvert^.alpha;
        sumCount += pvert^.count;
        inc(pvert);
      end;
      if factExtraX <> 0 then
      begin
        for xdest := 0 to width-1 do
        begin
          sumRed2 := 0;
          sumGreen2 := 0;
          sumBlue2 := 0;
          sumAlpha2 := 0;
          sumCount2 := 0;
          if xdest-iRadiusX-1 >= 0 then
          begin
            pvert := verticals+(xdest-iRadiusX-1);
            sumRed -= pvert^.red;
            sumGreen -= pvert^.green;
            sumBlue -= pvert^.blue;
            sumAlpha -= pvert^.alpha;
            sumCount -= pvert^.count;

            sumRed2 += pvert^.red;
            sumGreen2 += pvert^.green;
            sumBlue2 += pvert^.blue;
            sumAlpha2 += pvert^.alpha;
            sumCount2 += pvert^.count;
          end;
          if xdest+iRadiusX < width then
          begin
            pvert := verticals+(xdest+iRadiusX);
            sumRed += pvert^.red;
            sumGreen += pvert^.green;
            sumBlue += pvert^.blue;
            sumAlpha += pvert^.alpha;
            sumCount += pvert^.count;
          end;
          if xdest+iRadiusX+1 < width then
          begin
            pvert := verticals+(xdest+iRadiusX+1);
            sumRed2 += pvert^.red;
            sumGreen2 += pvert^.green;
            sumBlue2 += pvert^.blue;
            sumAlpha2 += pvert^.alpha;
            sumCount2 += pvert^.count;
          end;
          sumAlpha3 := sumAlpha*factMainX + sumAlpha2*factExtraX;
          {$IFDEF PARAM_BYTEMASK}
          if sumAlpha3 > 0 then
          begin
            sumGreen3 := sumGreen*factMainX + sumGreen2*factExtraX;
            pdest^ := (sumGreen3+(sumAlpha3 shr 1)) div sumAlpha3;
          end else pdest^ := 0;
          {$ELSE}
          begin
            sumCount3 := sumCount*factMainX + sumCount2*factExtraX;
            if (sumAlpha3 >= (sumCount3+1) shr 1) and (sumCount3 > 0) then
            with PBGRAPixel(pdest)^ do
            begin
              sumRed3 := sumRed*factMainX + sumRed2*factExtraX;
              sumGreen3 := sumGreen*factMainX + sumGreen2*factExtraX;
              sumBlue3 := sumBlue*factMainX + sumBlue2*factExtraX;
              red := (sumRed3+(sumAlpha3 shr 1)) div sumAlpha3;
              green := (sumGreen3+(sumAlpha3 shr 1)) div sumAlpha3;
              blue := (sumBlue3+(sumAlpha3 shr 1)) div sumAlpha3;
              alpha := (sumAlpha3+(sumCount3 shr 1)) div sumCount3;
            end else
              PBGRAPixel(pdest)^ := BGRAPixelTransparent;
          end;
          {$ENDIF}
          inc(pdest, destPixSize);
        end;
      end else
      begin
        for xdest := 0 to width-1 do
        begin
          if xdest-iRadiusX-1 >= 0 then
          begin
            pvert := verticals+(xdest-iRadiusX-1);
            sumRed -= pvert^.red;
            sumGreen -= pvert^.green;
            sumBlue -= pvert^.blue;
            sumAlpha -= pvert^.alpha;
            sumCount -= pvert^.count;
          end;
          if xdest+iRadiusX < width then
          begin
            pvert := verticals+(xdest+iRadiusX);
            sumRed += pvert^.red;
            sumGreen += pvert^.green;
            sumBlue += pvert^.blue;
            sumAlpha += pvert^.alpha;
            sumCount += pvert^.count;
          end;
          {$IFDEF PARAM_BYTEMASK}
          if sumAlpha > 0 then
            pdest^ := (sumGreen+(sumAlpha shr 1)) div sumAlpha
            else pdest^ := 0;
          {$ELSE}
          if (sumAlpha >= (sumCount+1) shr 1) then
          with PBGRAPixel(pdest)^ do
          begin
            red := (sumRed+(sumAlpha shr 1)) div sumAlpha;
            green := (sumGreen+(sumAlpha shr 1)) div sumAlpha;
            blue := (sumBlue+(sumAlpha shr 1)) div sumAlpha;
            alpha := (sumAlpha+(sumCount shr 1)) div sumCount;
          end else
            PBGRAPixel(pdest)^ := BGRAPixelTransparent;
          {$ENDIF}
          inc(pdest, destPixSize);
        end;
      end;
    end;
  end;

begin
  if (ABounds.Right <= ABounds.Left) or (ABounds.Bottom <= ABounds.Top) then exit;
  iRadiusX := floor(ARadiusX+0.5/factMainX);
  iRadiusY := floor(ARadiusY+0.5/factMainY);
  factExtraX := trunc(frac(ARadiusX+0.5/factMainX)*factMainX);
  factExtraY := trunc(frac(ARadiusY+0.5/factMainY)*factMainY);

  if (iRadiusX <= 0) and (iRadiusY <= 0) and (factExtraX <= 0) and (factExtraY <= 0) then
  begin
    oldClip := ADestination.IntersectClip(ABounds);
    ADestination.PutImage(0,0,ASource,dmSet);
    ADestination.ClipRect := oldClip;
    exit;
  end;
  left := ABounds.left;
  right := ABounds.right;
  width := right-left;
  height := ABounds.bottom-ABounds.top;
  ASource.LoadFromBitmapIfNeeded;

  getmem(verticals, width*sizeof(TVertical));
  try
    PrepareVerticals;
    MainLoop;
  finally
    freemem(verticals);
  end;
end;
{$UNDEF PARAM_BYTEMASK}
