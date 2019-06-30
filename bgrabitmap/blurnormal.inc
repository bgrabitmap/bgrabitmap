type
  PWeightedPixel = ^TWeightedPixel;
  TWeightedPixel = packed record
    Coord: TPoint;
    Weight: NativeInt;
    PtrOfs: NativeInt;
  end;

var
  maskWidth,maskHeight: integer;
  blurOfs: TPoint;
  ppixel: PWeightedPixel;
  Pixel: array of TWeightedPixel;
  PixelArrayLineStart: array of integer;
  DiffPixel: array of TWeightedPixel;
  DiffPixelArrayLineStart: array of integer;

  bmpWidth,bmpHeight: NativeInt;

  procedure LoadMask(out ABlurOfs: TPoint);
  var x,y,n: NativeInt;
      tempWeight: NativeInt;
      diffMask: array of packed array of NativeInt;
      p: PByteMask;
      qty,pStride: integer;
      srcLineDelta, srcPixSize, srcLineOfs: PtrInt;
  begin
    ABlurOfs := point(blurMask.Width shr 1, blurMask.Height shr 1);

    //count number of non empty pixels
    maskWidth := blurMask.Width;
    maskHeight := blurMask.Height;
    n := 0;
    for y:= 0 to maskHeight-1 do
    begin
      blurMask.ScanMoveTo(0,Y);
      x := maskWidth;
      while x > 0 do
      begin
        qty := x;
        blurMask.ScanNextMaskChunk(qty, p, pStride);
        dec(x, qty);
        while qty > 0 do
        begin
          if p^.gray <> 0 then inc(n);
          inc(p, pStride);
          dec(qty);
        end;
      end;
    end;

    //initialize arrays
    setlength(diffMask, maskHeight, maskWidth+1);
    for y := 0 to maskHeight - 1 do
      fillchar(diffMask[y,0], (maskWidth+1)*sizeof(NativeInt), 0);

    if bmp.LineOrder = riloTopToBottom then
      srcLineDelta := bmp.RowSize else
      srcLineDelta := -bmp.RowSize;
    srcPixSize := bmp.Colorspace.GetSize;

    setlength(Pixel, n);
    setlength(PixelArrayLineStart, maskHeight+1);  //stores the first pixel of each line
    n := 0;
    //compute mask variations and initial mask pixel list
    srcLineOfs := (0-ABlurOfs.Y)*srcLineDelta;
    for y := 0 to maskHeight - 1 do
    begin
      PixelArrayLineStart[y] := n;
      blurMask.ScanMoveTo(0,Y);
      x := 0;
      while x < maskWidth do
      begin
        qty := maskWidth - x;
        blurMask.ScanNextMaskChunk(qty, p, pStride);
        while qty > 0 do
        begin
          tempWeight := p^.gray;
          dec(diffMask[y,x], tempWeight);
          inc(diffMask[y,x+1], tempWeight);

          if tempWeight <> 0 then
          begin
            Pixel[n].Weight := tempWeight;
            Pixel[n].Coord := Point(x,y);
            Pixel[n].PtrOfs := srcLineOfs + (x-ABlurOfs.X)*srcPixSize;
            Inc(n);
          end;
          inc(x);
          inc(p, pStride);
          dec(qty);
        end;
      end;
      inc(srcLineOfs, srcLineDelta);
    end;
    PixelArrayLineStart[maskHeight] := n;

    //count number of diff pixels
    n := 0;
    for y := 0 to maskHeight - 1 do
      for x := 0 to maskWidth do
        if diffMask[y,x] <> 0 then Inc(n);

    //initialize arrays
    setlength(DiffPixel, n);
    setlength(DiffPixelArrayLineStart, maskHeight+1);  //stores the first pixel of each diff line
    n := 0;
    //compute diff pixel list
    srcLineOfs := (0-ABlurOfs.Y)*srcLineDelta;
    for y := 0 to maskHeight - 1 do
    begin
      DiffPixelArrayLineStart[y] := n;
      for x := 0 to maskWidth do
      begin
        tempWeight := diffMask[y,x];
        if tempWeight <> 0 then
        begin
          DiffPixel[n].Weight := tempWeight;
          DiffPixel[n].Coord := Point(x-1,y);
          DiffPixel[n].PtrOfs := srcLineOfs + (x-ABlurOfs.X-1)*srcPixSize;
          Inc(n);
        end;
      end;
      inc(srcLineOfs, srcLineDelta);
    end;
    DiffPixelArrayLineStart[maskHeight] := n;
  end;

  function PrepareScan(AWantedBounds: TRect; out AClippedBounds: TRect): boolean;
  begin
    //evaluate required bounds taking blur radius into acount
    AClippedBounds := bmp.GetImageBounds;
    if IsRectEmpty(AClippedBounds) then
    begin
      result := false;
      exit;
    end;
    AClippedBounds.Left   := max(0, AClippedBounds.Left - blurOfs.X);
    AClippedBounds.Top    := max(0, AClippedBounds.Top - blurOfs.Y);
    AClippedBounds.Right  := min(bmpWidth, AClippedBounds.Right + maskWidth - 1 - blurOfs.X);
    AClippedBounds.Bottom := min(bmpHeight, AClippedBounds.Bottom + maskHeight - 1 - blurOfs.Y);
    if not IntersectRect(AClippedBounds, AClippedBounds, AWantedBounds) then
    begin
      result := false;
      exit;
    end;

    result := true;
  end;

var
  bounds: TRect;
  yb, xb: NativeInt;
  mindy, maxdy, n, nStart, nCount, nDiffStart, nDiffCount: NativeInt;
  bmpX,bmpXBase,bmpYBase: NativeInt;
  pdest : PByte;
  psrc : PByte;
  srcPixSize,destPixSize: integer;

begin
  bmpWidth := bmp.Width;
  bmpHeight:= bmp.Height;
  bmp.LoadFromBitmapIfNeeded;

  if (ADestination.Width <> bmpWidth) or (ADestination.Height <> bmpHeight) then
    raise exception.Create('Dimension mismatch');

  LoadMask(blurOfs);
  if not PrepareScan(ABounds, bounds) then exit; //nothing to do

  bmpYBase := bounds.Top - blurOfs.Y;
  srcPixSize := bmp.Colorspace.GetSize;
  destPixSize := ADestination.Colorspace.GetSize;

  //loop through destination
  for yb := bounds.Top to bounds.Bottom - 1 do
  begin
    if (ACheckShouldStop <> nil) and ACheckShouldStop(yb) then break;
    psrc := bmp.GetPixelAddress(bounds.Left, yb);
    pdest := ADestination.GetPixelAddress(bounds.Left, yb);
    //compute vertical range
    mindy := max(-blurOfs.Y, -yb);
    maxdy := min(blurMask.Height - 1 - blurOfs.Y, bmpHeight - 1 - yb);

    AClearSum(AData);

    bmpXBase := bounds.Left-blurOfs.X;
    nStart := PixelArrayLineStart[mindy+blurOfs.Y];
    nCount  := PixelArrayLineStart[maxdy+blurOfs.Y+1]-nStart;
    ppixel:= @Pixel[nStart];
    //go through pixel list of the current vertical range
    for n := nCount-1 downto 0 do
    begin
      bmpX := bmpXBase+ppixel^.Coord.x;
      //check horizontal range
      if (bmpX >= 0) and (bmpX < bmpWidth) then
        AAccumulate(AData, psrc + ppixel^.PtrOfs, ppixel^.Weight);
      inc(ppixel);
    end;
    AComputeAverage(AData, pdest);

    nDiffStart := DiffPixelArrayLineStart[mindy+blurOfs.Y];
    nDiffCount := DiffPixelArrayLineStart[maxdy+blurOfs.Y+1]-nDiffStart;

    if nDiffCount < nCount then
    begin
      for xb := bounds.Left+1 to Bounds.Right - 1 do
      begin
        Inc(pdest, destPixSize);
        inc(bmpXBase);
        inc(psrc, srcPixSize);

        ppixel:= @DiffPixel[nDiffStart];
        for n := nDiffCount-1 downto 0 do
        begin
          bmpX := bmpXBase+ppixel^.Coord.x;
          if (bmpX >= 0) and (bmpX < bmpWidth) then
            AAccumulate(AData, psrc + ppixel^.PtrOfs, ppixel^.Weight);
          inc(ppixel);
        end;
        AComputeAverage(AData, pDest);
      end;
    end else
    begin
      for xb := bounds.Left+1 to Bounds.Right - 1 do
      begin
        Inc(pdest, destPixSize);
        inc(bmpXBase);
        inc(psrc, srcPixSize);

        AClearSum(AData);

        ppixel:= @Pixel[nStart];
        for n := nCount-1 downto 0 do
        begin
          bmpX := bmpXBase+ppixel^.Coord.x;
          //check horizontal range
          if (bmpX >= 0) and (bmpX < bmpWidth) then
            AAccumulate(AData, psrc + ppixel^.PtrOfs, ppixel^.Weight);
          inc(ppixel);
        end;
        AComputeAverage(AData, pdest);
      end;
    end;

    inc(bmpYBase);
  end;
  ADestination.InvalidateBitmap;
end;

