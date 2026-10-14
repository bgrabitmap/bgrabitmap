program TestAPNG;
{$mode objfpc}{$H+}
uses {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Classes, SysUtils, FPImage, BGRAPNGComn, BGRAWritePNG, BGRAReadPNG;
var
  First, Second, Decoded: TFPMemoryImage;
  Writer: TBGRAWriterPNG;
  Reader: TBGRAReaderPNG;
  Stream: TMemoryStream;
  Frames: TPNGArrayOfFrameToWrite;
  Color: TFPColor;
  x, y, i: Integer;
begin
  if (GetChunkType('acTL') <> TChunkTypes(ctacTL)) or
     (GetChunkType('fcTL') <> TChunkTypes(ctfcTL)) or
     (GetChunkType('fdAT') <> TChunkTypes(ctfdAT)) or
     (GetChunkCode(TChunkTypes(ctfdAT)) <> 'fdAT') or
     not IsAnimatedChunkType(TChunkTypes(ctacTL)) or
     IsAnimatedChunkType(ctIDAT) then
    raise Exception.Create('APNG chunk mapping failed');
  First := TFPMemoryImage.Create(2, 2);
  Second := TFPMemoryImage.Create(2, 2);
  Decoded := TFPMemoryImage.Create(0, 0);
  Writer := TBGRAWriterPNG.Create;
  Reader := TBGRAReaderPNG.Create;
  Stream := TMemoryStream.Create;
  try
    Color.Red := 65535; Color.Green := 0; Color.Blue := 0; Color.Alpha := 65535;
    for y := 0 to 1 do for x := 0 to 1 do First.Colors[x, y] := Color;
    Color.Red := 0; Color.Blue := 65535;
    for y := 0 to 1 do for x := 0 to 1 do Second.Colors[x, y] := Color;
    SetLength(Frames, 2);
    Frames[0].Image := First;
    Frames[1].Image := Second;
    for i := 0 to 1 do
    begin
      Frames[i].FrameControl.Width := 2;
      Frames[i].FrameControl.Height := 2;
      Frames[i].FrameControl.DelayNum := 1;
      Frames[i].FrameControl.DelayDenom := 10;
    end;
    Writer.AnimationWrite(Stream, First, Frames, 3);
    Stream.Position := 0;
    Reader.ImageRead(Stream, Decoded);
    if (Reader.FrameCount <> 2) or (Reader.LoopCount <> 3) then
      raise Exception.Create('APNG frame or loop count changed');
    for i := 0 to 1 do
    begin
      Reader.LoadFrame(i, Decoded);
      for y := 0 to 1 do for x := 0 to 1 do
      begin
        Color := Decoded.Colors[x, y];
        if ((i = 0) and ((Color.Red <> 65535) or (Color.Blue <> 0))) or
           ((i = 1) and ((Color.Red <> 0) or (Color.Blue <> 65535))) or
           (Color.Green <> 0) or (Color.Alpha <> 65535) then
          raise Exception.Create('APNG frame pixels changed');
      end;
    end;
    WriteLn('PASS: APNG two frames, pixels, loop count');
  finally
    Stream.Free;
    Reader.Free;
    Writer.Free;
    Decoded.Free;
    Second.Free;
    First.Free;
  end;
end.
