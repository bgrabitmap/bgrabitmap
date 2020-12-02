// SPDX-License-Identifier: LGPL-3.0-linking-exception

var
  blurRowY,blurRowX: packed array of UInt32or64;
  iRadiusX,iRadiusY: Int32or64;
  weightFactor: UInt32or64;

  { Compute weights of pixels in a row }
  procedure ComputeBlurRow;
  var
    i: Int32or64;
    ofs: single;
  begin
    SetLength(blurRowX, 2*iRadiusX+1);
    if frac(radiusX)=0 then ofs := 1 else ofs := frac(radiusX);
    for i := 0 to iRadiusX do
    begin
      blurRowX[i] := round((i+ofs)*weightFactor);
      blurRowX[high(blurRowX)-i] := blurRowX[i];
    end;
    SetLength(blurRowY, 2*iRadiusY+1);
    if frac(radiusY)=0 then ofs := 1 else ofs := frac(radiusY);
    for i := 0 to iRadiusY do
    begin
      blurRowY[i] := round((i+ofs)*weightFactor);
      blurRowY[high(blurRowY)-i] := blurRowY[i];
    end;
  end;


var
  srcDelta,srcPixSize,
  verticalWeightShift, horizontalWeightShift: Int32or64;
  ys1,ys2: Int32or64;

  { Compute blur result in a vertical direction }
  procedure ComputeVerticalRow(psrc: PByte; var sums: TRowSum; pw: PNativeUInt; count: Int32or64);
  begin
    while count > 0 do
    with sums do
    begin
      dec(count);
      AccumulatePixel(psrc, pw^, sums, verticalWeightShift);
      inc(pw);
      inc(psrc,srcDelta);
    end;
  end;

var
  psum, psumEnd: PRowSum;
  sums: packed array of TRowSum;
  sumStartIndex: Int32or64;
  total: TRowSum;
  extendedTotal: TExtendedRowSum;
  yb,xb,xs,x,xEnd: Int32or64;
  pw: PNativeUInt;
  psrc, pdest: PByte;
  bmpWidth,bmpHeight : Int32or64;
  accumulationFactor: double;
  bounds: TRect;
  highSum: Int32or64;
  tempDest: TCustomUniversalBitmap;
  destPixSize: Integer;

begin
  radiusX := round(radiusX*10)*0.1;
  radiusY := round(radiusY*10)*0.1;
  if (radiusX <= 0) and (radiusY <= 0) then
  begin
    ADestination.PutImage(0,0,bmp,dmSet);
    exit;
  end;
  iRadiusX := ceil(radiusX);
  iRadiusY := ceil(radiusY);
  if (frac(radiusX)=0) and (frac(radiusY)=0) then
    weightFactor:= 1
  else
    weightFactor:= 10;
  bmpWidth := bmp.Width;
  bmpHeight := bmp.Height;
  //create output
  if (ADestination.Width <> bmp.Width) or (ADestination.Height <> bmp.Height) then
    raise exception.Create('Dimension mismatch');
  bounds := bmp.GetImageBounds;
  if bounds.IsEmpty then exit;
  bounds.Left   := max(0, bounds.Left - iRadiusX);
  bounds.Top    := max(0, bounds.Top - iRadiusY);
  bounds.Right  := min(bmp.Width, bounds.Right + iRadiusX);
  bounds.Bottom := min(bmp.Height, bounds.Bottom + iRadiusY);
  bounds.Intersect(ABounds);
  if bounds.IsEmpty then exit;

  if radiusX*radiusY >= 100 then
  begin
    tempDest := ADestination.NewBitmap;
    tempDest.SetSize(ADestination.Width,ADestination.Height);
    FilterBlurBox(bmp,bounds,radiusX/3.2,radiusY/3.2,tempDest);
    FilterBlurBox(tempDest,bounds,radiusX/2.9,radiusY/2.9,ADestination);
    FilterBlurBox(ADestination,bounds,radiusX/3.2,radiusY/3.2,tempDest);
    FilterBlurBox(tempDest,bounds,radiusX/2.3,radiusY/2.3,ADestination, ACheckShouldStop);
    tempDest.Free;
    exit;
  end;

  accumulationFactor := (iRadiusY+2)*(iRadiusY+1) div 2 + (iRadiusY+1)*iRadiusY div 2;
  accumulationFactor := accumulationFactor * sqr(weightFactor);
  verticalWeightShift := 0;
  while accumulationFactor > (high(TRegularRowValue) shr BitMargin) + 1 do
  begin
    inc(verticalWeightShift);
    accumulationFactor := accumulationFactor * 0.5;
  end;
  horizontalWeightShift:= 0;
  accumulationFactor := accumulationFactor *
         ((iRadiusX+2)*(iRadiusX+1) div 2 + (iRadiusX+1)*iRadiusX div 2) *
         sqr(weightFactor);
  while accumulationFactor > (high(TRegularRowValue) shr BitMargin) + 1 do
  begin
    inc(horizontalWeightShift);
    accumulationFactor := accumulationFactor * 0.5;
  end;
  ComputeBlurRow;
  //current vertical sums
  setlength(sums, 2*iRadiusX+1);
  highSum := high(Sums);
  psumEnd := @sums[highSum];
  inc(psumEnd);
  if bmp.LineOrder = riloTopToBottom then
    srcDelta := bmp.RowSize else srcDelta := -bmp.RowSize;
  srcPixSize := bmp.Colorspace.GetSize;
  destPixSize := ADestination.Colorspace.GetSize;
  bmp.LoadFromBitmapIfNeeded;

  xEnd := bounds.left-iRadiusX+highSum;
  if xEnd >= bmpWidth then xEnd := bmpWidth-1;
  //loop through destination bitmap
  for yb := bounds.top to bounds.bottom-1 do
  begin
    if (ACheckShouldStop <> nil) and ACheckShouldStop(yb) then break;
    //evalute available vertical range
    if yb - iRadiusY < 0 then
      ys1 := iRadiusY - yb
    else
      ys1 := 0;
    if yb + iRadiusY >= bmpHeight then
      ys2 := bmpHeight-1 - yb + iRadiusY
    else
      ys2 := 2*iRadiusY;

    { initial vertical rows are computed here. Later,
      for each pixel, vertical sums are shifted, so there
      is only one vertical sum to calculate }
    fillchar(sums[0],sizeof(TRowSum)*length(sums),0);
    x := bounds.left-iRadiusX;
    if x < 0 then
    begin
      xs := -x;
      x := 0;
    end else
      xs := 0;
    psrc := bmp.GetPixelAddress(x, yb-iRadiusY+ys1);
    psum := @sums[xs];
    pw := @blurRowY[ys1];
    while true do
    begin
      ComputeVerticalRow(psrc,psum^,pw,ys2-ys1+1);
      inc(x);
      inc(psrc, srcPixSize);
      if x > xEnd then break;
      inc(psum);
    end;
    sumStartIndex := 0;

    pdest := ADestination.GetPixelAddress(bounds.Left, yb);
    for xb := bounds.left to bounds.right-1 do
    begin
      //add vertical rows
      pw := @blurRowX[0];
      psum := @sums[sumStartIndex];
      if horizontalWeightShift > 4 then
      begin //we don't want to loose too much precision
        fillchar({%H-}extendedTotal,sizeof(extendedTotal),0);
        for xs := highSum downto 0 do
        begin
          AccumulateExtended(extendedTotal, psum, pw^);
          inc(pw);
          inc(psum);
          if psum >= psumEnd then pSum := @sums[0];
        end;
        ComputeExtendedAverage(extendedTotal, pdest);
      end else
      if horizontalWeightShift > 0 then
      begin //lossy but efficient way
        fillchar({%H-}total,sizeof(total),0);
        for xs := highSum downto 0 do
        begin
          AccumulateShr(total, psum, pw^, horizontalWeightShift);
          inc(pw);
          inc(psum);
          if psum >= psumEnd then pSum := @sums[0];
        end;
        ComputeClampedAverage(total, pdest);
      end else
      begin //normal way
        {$hints off}
        fillchar(total,sizeof(total),0);
        {$hints on}
        for xs := highSum downto 0 do
        begin
          AccumulateNormal(total, psum, pw^);
          inc(pw);
          inc(psum);
          if psum >= psumEnd then pSum := @sums[0];
        end;
        ComputeAverage(total, pdest)
      end;
      inc(pdest, destPixSize);
      //shift vertical rows
      psum := @sums[sumStartIndex];
      fillchar(psum^,sizeof(TRowSum),0);
      if x < bmpWidth then
      begin
        ComputeVerticalRow(psrc,psum^,@blurRowY[ys1],ys2-ys1+1);
        inc(x);
        inc(psrc, srcPixSize);
      end;
      inc(sumStartIndex);
      if sumStartIndex > highSum then sumStartIndex := 0;
    end;
  end;
  ADestination.InvalidateBitmap;
end;

