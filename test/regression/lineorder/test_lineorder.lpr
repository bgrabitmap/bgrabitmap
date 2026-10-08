program TestDuplicateLineOrder;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  {$IFDEF LCL}Interfaces,{$ENDIF}
  SysUtils, BGRABitmap, BGRABitmapTypes, BGRAGraphics, BGRADefaultBitmap;
var
  Buffer: array[0..5] of TBGRAPixel;
  Source: TBGRAPtrBitmap;
  CopyImage: TBGRACustomBitmap;
  Order: TRawImageLineOrder;
  CopyProperties: Boolean;
  Mode: TResampleMode;
  x, y: Integer;
procedure CheckPixels(Image: TBGRACustomBitmap; const Operation: string);
var
  px, py: Integer;
begin
  for py := 0 to 2 do
    for px := 0 to 1 do
      if Image.GetPixel(px, py) <> Source.GetPixel(px, py) then
        raise Exception.CreateFmt('%s flipped pixels at %d,%d (source order %d, destination order %d)',
          [Operation, px, py, Ord(Source.LineOrder), Ord(Image.LineOrder)]);
end;
begin
  try
    for Order := Low(TRawImageLineOrder) to High(TRawImageLineOrder) do
    begin
      Source := TBGRAPtrBitmap.Create(2, 3, @Buffer[0]);
      try
        Source.LineOrder := Order;
        for y := 0 to 2 do
          for x := 0 to 1 do
            Source.SetPixel(x, y, BGRA(20 + y * 70, 10 + x * 80, 30, 255));
        for CopyProperties := False to True do
        begin
          CopyImage := Source.Duplicate(CopyProperties);
          try CheckPixels(CopyImage, 'Duplicate');
          finally CopyImage.Free; end;
          for Mode := Low(TResampleMode) to High(TResampleMode) do
          begin
            CopyImage := Source.Resample(2, 3, Mode, CopyProperties);
            try CheckPixels(CopyImage, 'Same-size resample');
            finally CopyImage.Free; end;
          end;
        end;
      finally Source.Free; end;
    end;
    WriteLn('PASS: external buffers in both line orders, Duplicate and same-size resampling');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ', E.Message);
      Halt(1);
    end;
  end;
end.
