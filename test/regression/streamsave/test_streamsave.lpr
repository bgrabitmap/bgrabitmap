program TestStreamSave;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRAWritePNG;

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
      Source.SaveToStreamAsPng(AStream, False)
    else
      Source.SaveToStreamAs(AStream, AFormat, False);
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

procedure CheckTruncationModes(AStream: TStream);
var
  Source: TBGRABitmap;
  Expected: TMemoryStream;
  Writer: TBGRAWriterPNG;
  Data: array of Byte;
  Tail: array[0..18] of Byte;
  StartPosition: Int64;
  Mode, I: Integer;
  Truncate: Boolean;
begin
  Source := TBGRABitmap.Create(3, 2, BGRA(20, 40, 60));
  Expected := TMemoryStream.Create;
  Writer := TBGRAWriterPNG.Create;
  try
    Source.SaveToStreamAsPng(Expected);
    for Mode := 0 to 8 do
    begin
      Truncate := Mode in [0, 1, 2, 3, 8];
      StartPosition := SizeOf(Prefix);
      if Mode = 7 then StartPosition := 0;
      SetLength(Data, StartPosition + Expected.Size + SizeOf(Tail));
      FillChar(Data[0], Length(Data), 91);
      AStream.Size := 0;
      AStream.WriteBuffer(Data[0], Length(Data));
      AStream.Position := StartPosition;
      case Mode of
        0: Source.SaveToStreamAs(AStream, ifPNG);
        1: Source.SaveToStreamAsPng(AStream);
        2: Source.SaveToStream(AStream, Writer);
        3: Source.SaveToStreamAs(AStream, ifPNG, True);
        4, 7: Source.SaveToStreamAs(AStream, ifPNG, False);
        5: Source.SaveToStreamAsPng(AStream, False);
        6: Source.SaveToStream(AStream, Writer, False);
        8: Source.SaveToStream(AStream, Writer, True);
      end;
      if Truncate then
      begin
        if (AStream.Size <> Expected.Size) or (AStream.Position <> Expected.Size) then
          raise Exception.Create('Default/True did not truncate');
        AStream.Position := 0;
      end else
      begin
        if (AStream.Size <> Length(Data)) or
           (AStream.Position <> StartPosition + Expected.Size) then
          raise Exception.Create('False truncated the stream or left wrong position');
        AStream.ReadBuffer(Tail, SizeOf(Tail));
        for I := 0 to High(Tail) do
          if Tail[I] <> 91 then raise Exception.Create('False damaged trailing data');
        AStream.Position := 0;
        for I := 0 to StartPosition - 1 do
        begin
          AStream.ReadBuffer(Tail[0], 1);
          if Tail[0] <> 91 then raise Exception.Create('False damaged prefix');
        end;
      end;
      AStream.ReadBuffer(Data[0], Expected.Size);
      if not CompareMem(@Data[0], Expected.Memory, Expected.Size) then
        raise Exception.Create('Wrong image bytes after truncation mode test');
    end;
  finally
    Writer.Free;
    Expected.Free;
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
    CheckTruncationModes(Stream);
  finally
    Stream.Free;
  end;
  FileName := GetTempFileName;
  try
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      CheckStream(Stream, ifPNG);
      CheckStream(Stream, ifBMP);
      CheckTruncationModes(Stream);
    finally
      Stream.Free;
    end;
  finally
    DeleteFile(FileName);
  end;
  WriteLn('PASS: PNG/BMP stream position, prefix, encoded bytes and PNG pixels');
end.
