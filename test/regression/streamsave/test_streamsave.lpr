program TestStreamSave;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes;

const
  Prefix: array[0..6] of Byte = (17, 42, 0, 255, 81, 9, 13);

procedure CheckStream(AStream: TStream; AFormat: TBGRAImageFormat);
var
  Source, Loaded: TBGRABitmap;
  Expected: TMemoryStream;
  ActualPrefix: array[0..High(Prefix)] of Byte;
  ActualData: array of Byte;
  StartPosition: Int64;
  X, Y: Integer;
begin
  Source := TBGRABitmap.Create(3, 2, BGRA(12, 34, 56));
  Loaded := TBGRABitmap.Create;
  Expected := TMemoryStream.Create;
  try
    Source.SetPixel(1, 1, BGRA(200, 100, 50));
    Source.SaveToStreamAs(Expected, AFormat);
    AStream.Size := 0;
    AStream.WriteBuffer(Prefix, SizeOf(Prefix));
    StartPosition := AStream.Position;
    if AFormat = ifPNG then
      Source.SaveToStreamAsPng(AStream)
    else
      Source.SaveToStreamAs(AStream, AFormat);
    if AStream.Position <> StartPosition + Expected.Size then
      raise Exception.Create('Wrong position after saving');
    AStream.Position := 0;
    AStream.ReadBuffer(ActualPrefix, SizeOf(ActualPrefix));
    if not CompareMem(@ActualPrefix, @Prefix, SizeOf(Prefix)) then
      raise Exception.Create('Saving overwrote the prefix');
    SetLength(ActualData, Expected.Size);
    AStream.ReadBuffer(ActualData[0], Length(ActualData));
    if not CompareMem(@ActualData[0], Expected.Memory, Expected.Size) then
      raise Exception.Create('Embedded image differs from standalone image');
    // PNG reading must also work from the embedded image's starting position.
    if AFormat = ifPNG then
    begin
      AStream.Position := StartPosition;
      Loaded.LoadFromStream(AStream);
      if (Loaded.Width <> Source.Width) or (Loaded.Height <> Source.Height) then
        raise Exception.Create('Embedded PNG dimensions changed');
      for Y := 0 to Source.Height - 1 do
        for X := 0 to Source.Width - 1 do
          if Loaded.GetPixel(X, Y) <> Source.GetPixel(X, Y) then
            raise Exception.Create('Embedded PNG pixels changed');
    end;
  finally
    Expected.Free;
    Loaded.Free;
    Source.Free;
  end;
end;

var
  Stream: TStream;
  FileName: string;
begin
  Stream := TMemoryStream.Create;
  try
    CheckStream(Stream, ifPNG);
    CheckStream(Stream, ifBMP);
  finally
    Stream.Free;
  end;
  FileName := GetTempFileName;
  try
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      CheckStream(Stream, ifPNG);
      CheckStream(Stream, ifBMP);
    finally
      Stream.Free;
    end;
  finally
    DeleteFile(FileName);
  end;
  WriteLn('PASS: PNG/BMP stream position, prefix, encoded bytes and PNG pixels');
end.
