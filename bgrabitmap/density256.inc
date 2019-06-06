{$IFDEF INCLUDE_FILLDENSITY}
{$UNDEF INCLUDE_FILLDENSITY}

	{$IFNDEF PARAM_SINGLESEGMENT}
	begin
	  { this loops fill one scanline of densities by adding 256 for full horizontal pixels }

	  for i := 0 to nbinter div 2 - 1 do
	  begin
		x1 := inter[i + i].interX;
		x2 := inter[i + i + 1].interX;
	{$ENDIF}

		if (x1 <> x2) and (x1 < maxx + 1) and (x2 >= minx) then
		begin
		  if x1 < minx then
			x1 := minx;
		  if x2 >= maxx + 1 then
			x2 := maxx + 1;
		  ix1  := floor(x1);
		  ix2  := floor(x2);

		  //here it may go one pixel further if x2 is an integer
		  if ix1 < densMinx then densMinx := ix1;
		  if ix2 > densMaxx then densMaxx := ix2;

		  if ix1 = ix2 then
			inc( (density + (ix1 - minx))^, round((x2-ix2)*256) - round((x1-ix1)*256) )
		  else
		  begin
			inc( (density + (ix1 - minx))^, 256 - round((x1 - ix1)*256) );
			if (ix2 <= maxx) then
			  inc( (density + (ix2 - minx))^, round((x2 - ix2)*256) );
		  end;
		  if ix2 > ix1 + 1 then
		  begin
			AddDensity(density, ix1+1 - minx, ix2-(ix1+1), 256);
		  end;
		end;
	
	{$IFNDEF PARAM_SINGLESEGMENT}
	  end;
	end;
	{$ENDIF}
	
{$UNDEF PARAM_SINGLESEGMENT}
{$ENDIF}

{$IFDEF INCLUDE_RENDERDENSITY}
{$UNDEF INCLUDE_RENDERDENSITY}
  begin
    if densMinX <= densMaxX then
    begin
      if densMinX < minx then densMinX := minx;
      if densMaxX > maxx then densMaxX := maxx;
      xb := densMinX;
      pdens := density + (densMinX-minx);
      pDest := bmp.GetPixelAddress(densMinX,yb);
      brush.MoveTo(@ctx, pDest,xb,yb);
      while xb<=densMaxX do
      begin
        tempDensity:= pDens^;
        inc(pDens);
        inc(xb);
        drawCount := 1;
        if tempDensity>=256{$ifdef PARAM_ANTIALIASINGFACTOR} shl AntialiasPrecisionShift{$endif} then
        begin
          while (xb<=densMaxX) and (pDens^ >= 256{$ifdef PARAM_ANTIALIASINGFACTOR} shl AntialiasPrecisionShift{$endif}) do
          begin
            inc(xb);
            inc(pDens);
            inc(drawCount);
          end;
          brush.PutNextPixels(@ctx, 65535, drawCount);
        end else
        begin
          while (xb<=densMaxX) and (pDens^ = tempDensity) do
          begin
            inc(xb);
            inc(pDens);
            inc(drawCount);
          end;
          if tempDensity = 0 then
            brush.PutNextPixels(@ctx, 0, drawCount )
          else
          begin
            dec(tempDensity);
            brush.PutNextPixels(@ctx, (tempDensity shl (8{$ifdef PARAM_ANTIALIASINGFACTOR}-AntialiasPrecisionShift{$endif}))
                              + (tempDensity{$ifdef PARAM_ANTIALIASINGFACTOR} shr (2*AntialiasPrecisionShift){$endif}), drawCount );
          end;
        end;
      end;
    end;
  end
  
{$undef PARAM_ANTIALIASINGFACTOR}
{$ENDIF}
