// SPDX-License-Identifier: LGPL-3.0-linking-exception
procedure IncAcc(var ADest: TAccumulator; ADelta: TAccumulator); inline;
begin
  {$IFDEF PARAM_USE_INC64}
  inc64(ADest, ADelta);
  {$ELSE}
  inc(ADest, ADelta);
  {$ENDIF}
end;
procedure DecAcc(var ADest: TAccumulator; ADelta: TAccumulator); inline;
begin
  {$IFDEF PARAM_USE_INC64}
  dec64(ADest, ADelta);
  {$ELSE}
  dec(ADest, ADelta);
  {$ENDIF}
end;

type
  TVertical = record red,green,blue,alpha,count: TAccumulator; end;
  PVertical = ^TVertical;
var
  verticals: PVertical;
  left,right,width,height: Int32or64;
  iRadiusX,iRadiusY: Int32or64;
  factExtraX,factExtraY: UInt32or64;

  procedure PrepareVerticals;
  var
    xb,yb: Int32or64;
    psrc,p: PByte;
    pvert : PVertical;
    {%H-}a2: UInt32or64;
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
          IncAcc(pvert^.green, p^);
          IncAcc(pvert^.alpha, 1);
          {$ELSE}
          with PBGRAPixel(p)^ do
          if alpha <> 0 then
          begin
            a2 := alpha;
            {$HINTS OFF}
            IncAcc(pvert^.red, red * a2);
            IncAcc(pvert^.green, green * a2);
            IncAcc(pvert^.blue, blue * a2);
            IncAcc(pvert^.alpha, a2);
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
          IncAcc(pvert^.green, p^ * factMainY);
          IncAcc(pvert^.alpha, factMainY);
          {$ELSE}
          with PBGRAPixel(p)^ do
          if alpha <> 0 then
          begin
            a2 := alpha * factMainY;
            {$HINTS OFF}
            IncAcc(pvert^.red, red * a2);
            IncAcc(pvert^.green, green * a2);
            IncAcc(pvert^.blue, blue * a2);
            IncAcc(pvert^.alpha, a2);
            {$HINTS ON}
          end;
          {$ENDIF}
          inc(pvert^.count, factMainY);
          inc(p, delta);
        end;
        if iRadiusY < height then
        begin
          {$IFDEF PARAM_BYTEMASK}
          IncAcc(pvert^.green, p^ * factExtraY);
          IncAcc(pvert^.alpha, factExtraY);
          {$ELSE}
          with PBGRAPixel(p)^ do
          if alpha <> 0 then
          begin
            a2 := alpha * factExtraY;
            {$HINTS OFF}
            IncAcc(pvert^.red, red * a2);
            IncAcc(pvert^.green, green * a2);
            IncAcc(pvert^.blue, blue * a2);
            IncAcc(pvert^.alpha, a2);
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
    xb: Int32or64;
    {%H-}a2: UInt32or64;
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
          DecAcc(pvert^.green, psrc1^);
          DecAcc(pvert^.alpha, 1);
          {$ELSE}
          with PBGRAPixel(psrc1)^ do
          if alpha <> 0 then
          begin
            {$HINTS OFF}
            DecAcc(pvert^.red, red * alpha);
            DecAcc(pvert^.green, green * alpha);
            DecAcc(pvert^.blue, blue * alpha);
            DecAcc(pvert^.alpha, alpha);
            {$HINTS ON}
          end;
          {$ENDIF}
          dec(pvert^.count);
          inc(psrc1,srcPixSize);
        end;
        if psrc2 <> nil then
        begin
          {$IFDEF PARAM_BYTEMASK}
          IncAcc(pvert^.green, psrc2^);
          IncAcc(pvert^.alpha, 1);
          {$ELSE}
          with PBGRAPixel(psrc2)^ do
          if alpha <> 0 then
          begin
            {$HINTS OFF}
            IncAcc(pvert^.red, red * alpha);
            IncAcc(pvert^.green, green * alpha);
            IncAcc(pvert^.blue, blue * alpha);
            IncAcc(pvert^.alpha, alpha);
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
          DecAcc(pvert^.green, psrc0^*factExtraY);
          DecAcc(pvert^.alpha, factExtraY);
          {$ELSE}
          with PBGRAPixel(psrc0)^ do
          if alpha <> 0 then
          begin
            a2 := alpha*factExtraY;
            {$HINTS OFF}
            DecAcc(pvert^.red, red * a2);
            DecAcc(pvert^.green, green * a2);
            DecAcc(pvert^.blue, blue * a2);
            DecAcc(pvert^.alpha, a2);
            {$HINTS ON}
          end;
          {$ENDIF}
          dec(pvert^.count,factExtraY);
          inc(psrc0,srcPixSize);
        end;
        if psrc1 <> nil then
        begin
          {$IFDEF PARAM_BYTEMASK}
          DecAcc(pvert^.green, psrc1^*(factMainY - factExtraY));
          DecAcc(pvert^.alpha, (factMainY - factExtraY));
          {$ELSE}
          with PBGRAPixel(psrc1)^ do
          if alpha <> 0 then
          begin
            a2 := alpha*(factMainY - factExtraY);
            {$HINTS OFF}
            DecAcc(pvert^.red, red * a2);
            DecAcc(pvert^.green, green * a2);
            DecAcc(pvert^.blue, blue * a2);
            DecAcc(pvert^.alpha, a2);
            {$HINTS ON}
          end;
          {$ENDIF}
          dec(pvert^.count, factMainY - factExtraY);
          inc(psrc1,srcPixSize);
        end;
        if psrc2 <> nil then
        begin
          {$IFDEF PARAM_BYTEMASK}
          IncAcc(pvert^.green, psrc2^*(factMainY - factExtraY));
          IncAcc(pvert^.alpha, (factMainY - factExtraY));
          {$ELSE}
          with PBGRAPixel(psrc2)^ do
          if alpha <> 0 then
          begin
            a2 := alpha*(factMainY - factExtraY);
            {$HINTS OFF}
            IncAcc(pvert^.red, red * a2);
            IncAcc(pvert^.green, green * a2);
            IncAcc(pvert^.blue, blue * a2);
            IncAcc(pvert^.alpha, a2);
            {$HINTS ON}
          end;
          {$ENDIF}
          inc(pvert^.count, factMainY - factExtraY);
          inc(psrc2,srcPixSize);
        end;
        if psrc3 <> nil then
        begin
          {$IFDEF PARAM_BYTEMASK}
          IncAcc(pvert^.green, psrc3^*factExtraY);
          IncAcc(pvert^.alpha, factExtraY);
          {$ELSE}
          with PBGRAPixel(psrc3)^ do
          if alpha <> 0 then
          begin
            a2 := alpha*factExtraY;
            {$HINTS OFF}
            IncAcc(pvert^.red, red * a2);
            IncAcc(pvert^.green, green * a2);
            IncAcc(pvert^.blue, blue * a2);
            IncAcc(pvert^.alpha, a2);
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
    xb,yb,xdest: Int32or64;
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
        IncAcc(sumRed, pvert^.red);
        IncAcc(sumGreen, pvert^.green);
        IncAcc(sumBlue, pvert^.blue);
        IncAcc(sumAlpha, pvert^.alpha);
        IncAcc(sumCount, pvert^.count);
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
            DecAcc(sumRed, pvert^.red);
            DecAcc(sumGreen, pvert^.green);
            DecAcc(sumBlue, pvert^.blue);
            DecAcc(sumAlpha, pvert^.alpha);
            DecAcc(sumCount, pvert^.count);

            IncAcc(sumRed2, pvert^.red);
            IncAcc(sumGreen2, pvert^.green);
            IncAcc(sumBlue2, pvert^.blue);
            IncAcc(sumAlpha2, pvert^.alpha);
            IncAcc(sumCount2, pvert^.count);
          end;
          if xdest+iRadiusX < width then
          begin
            pvert := verticals+(xdest+iRadiusX);
            IncAcc(sumRed, pvert^.red);
            IncAcc(sumGreen, pvert^.green);
            IncAcc(sumBlue, pvert^.blue);
            IncAcc(sumAlpha, pvert^.alpha);
            IncAcc(sumCount, pvert^.count);
          end;
          if xdest+iRadiusX+1 < width then
          begin
            pvert := verticals+(xdest+iRadiusX+1);
            IncAcc(sumRed2, pvert^.red);
            IncAcc(sumGreen2, pvert^.green);
            IncAcc(sumBlue2, pvert^.blue);
            IncAcc(sumAlpha2, pvert^.alpha);
            IncAcc(sumCount2, pvert^.count);
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
            DecAcc(sumRed, pvert^.red);
            DecAcc(sumGreen, pvert^.green);
            DecAcc(sumBlue, pvert^.blue);
            DecAcc(sumAlpha, pvert^.alpha);
            DecAcc(sumCount, pvert^.count);
          end;
          if xdest+iRadiusX < width then
          begin
            pvert := verticals+(xdest+iRadiusX);
            IncAcc(sumRed, pvert^.red);
            IncAcc(sumGreen, pvert^.green);
            IncAcc(sumBlue, pvert^.blue);
            IncAcc(sumAlpha, pvert^.alpha);
            IncAcc(sumCount, pvert^.count);
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
{$UNDEF PARAM_USE_INC64}
