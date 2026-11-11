program TestWebPEffort;
{$mode objfpc}{$H+}
uses
  {$ifdef UNIX}cthreads, cwstring,{$endif}
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRAWriteWebP, libwebp;
type
  TObservedWriter = class(TBGRAWriterWebP)
    Config: TWebPConfig;
    procedure ConfigureEncoder(var AConfig: TWebPConfig); override;
  end;
  TExactWriter = class(TObservedWriter)
    KeepHiddenRGB: Boolean;
    procedure ConfigureEncoder(var AConfig: TWebPConfig); override;
  end;
  TInvalidWriter = class(TBGRAWriterWebP)
    procedure ConfigureEncoder(var AConfig: TWebPConfig); override;
  end;
  TBrokenStream = class(TMemoryStream)
    function Write(const Buffer; Count: LongInt): LongInt; override;
  end;
procedure TObservedWriter.ConfigureEncoder(var AConfig: TWebPConfig);
begin
  inherited ConfigureEncoder(AConfig);
  Config := AConfig;
end;
procedure TExactWriter.ConfigureEncoder(var AConfig: TWebPConfig);
begin
  inherited ConfigureEncoder(AConfig);
  AConfig.method := 4;
  AConfig.quality := 70;
  AConfig.exact := Ord(KeepHiddenRGB);
  Config := AConfig;
end;
procedure TInvalidWriter.ConfigureEncoder(var AConfig: TWebPConfig);
begin
  inherited ConfigureEncoder(AConfig);
  AConfig.method := 99;
end;
function TBrokenStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  raise EWriteError.Create('Deliberate output failure');
end;
var Checks: Integer;
procedure Check(Value: Boolean; const Description: string);
begin
  Inc(Checks);
  if not Value then raise Exception.Create(Description);
end;
procedure CheckPixels(Source: TBGRABitmap; Encoded: TMemoryStream;
  Lossless, Exact: Boolean);
var decoded: PByte; width, height, x, y: Integer; p, q: TBGRAPixel;
begin
  decoded := WebPDecodeBGRA(Encoded.Memory, Encoded.Size, @width, @height);
  Check(decoded <> nil, 'Decode failed');
  try
    Check((width = Source.Width) and (height = Source.Height), 'Dimensions changed');
    for y := 0 to height-1 do
      for x := 0 to width-1 do
      begin
        p := Source.GetPixel(x,y);
        q := PBGRAPixel(decoded)[y*width+x];
        Check(q.alpha = p.alpha, 'Alpha changed');
        if Lossless and ((p.alpha <> 0) or Exact) then
          Check(q = p, 'Lossless pixel changed');
      end;
  finally WebPFree(decoded); end;
end;
procedure CompareLegacy(Source: TBGRABitmap; Encoded: TMemoryStream;
  Quality: Single; Lossless: Boolean);
var data: PByte; size: SizeUInt; temp: TBGRABitmap;
begin
  temp := Source.Duplicate as TBGRABitmap;
  data := nil;
  try
    if temp.LineOrder = riloBottomToTop then temp.VerticalFlip;
    if TBGRAPixel_RGBAOrder then
    begin
      if Lossless then
        size := WebPEncodeLosslessRGBA(temp.DataByte,temp.Width,temp.Height,temp.RowSize,data)
      else size := WebPEncodeRGBA(temp.DataByte,temp.Width,temp.Height,temp.RowSize,Quality,data);
    end else
    begin
      if Lossless then
        size := WebPEncodeLosslessBGRA(temp.DataByte,temp.Width,temp.Height,temp.RowSize,data)
      else size := WebPEncodeBGRA(temp.DataByte,temp.Width,temp.Height,temp.RowSize,Quality,data);
    end;
    Check(size > 0, 'Legacy encode failed');
    Check((size = SizeUInt(Encoded.Size)) and CompareMem(data,Encoded.Memory,size),
      'Output differs from the old API');
  finally
    if data <> nil then WebPFree(data);
    temp.Free;
  end;
end;
procedure Run;
var image, before: TBGRABitmap; writer: TObservedWriter; stream: TMemoryStream;
  exactWriter: TExactWriter; invalidWriter: TInvalidWriter; broken: TBrokenStream;
  x,y,effort,mode,orderIndex,qualityIndex,sourceOrder: Integer; p: TBGRAPixel;
  rejected: Boolean;
const Qualities: array[0..2] of Single = (0,60,100);
begin
  if ParamCount > 0 then
    Check(LibWebPLoad(ParamStr(1)), 'Specified libwebp not found')
  else
    Check(LibWebPLoad, 'libwebp not found');
  WriteLn('libwebp encoder version: ', WebPGetEncoderVersion());
  image := TBGRABitmap.Create(32,24);
  writer := TObservedWriter.Create;
  stream := TMemoryStream.Create;
  try
    Check(writer.CompressionEffort = 6, 'Unexpected effort default');
    Check(writer.Lossless and (writer.QualityPercent = 100), 'Existing defaults changed');
    for y := 0 to image.Height-1 do
      for x := 0 to image.Width-1 do
      begin
        p := BGRA((x*73+y*17) mod 256,(x*29+y*61) mod 256,(x*19+y*43) mod 256);
        case (x+y) mod 4 of
          0: p.alpha := 0;
          1: p.alpha := 73;
          2: p.alpha := 190;
        end;
        image.SetPixel(x,y,p);
      end;
    for orderIndex := 0 to 1 do
    begin
      if orderIndex = 1 then image.LineOrder := riloBottomToTop;
      sourceOrder := Ord(image.LineOrder);
      before := image.Duplicate as TBGRABitmap;
      try
        for mode := 0 to 1 do
          for effort := 0 to 9 do
          begin
            writer.Lossless := mode = 1;
            writer.QualityPercent := 60;
            writer.CompressionEffort := effort;
            stream.Clear;
            image.SaveToStream(stream,writer);
            CheckPixels(image,stream,writer.Lossless,false);
            Check(image.Equals(before), 'Source image modified');
            Check(Ord(image.LineOrder) = sourceOrder, 'Source line order modified');
            if effort = 6 then
            begin
              Check(writer.Config.method = 4, 'Default method is not 4');
              if writer.Lossless then
                Check(writer.Config.quality = 75, 'Default lossless effort is not 75');
            end;
          end;
        writer.Lossless := false;
        writer.CompressionEffort := 6;
        for qualityIndex := 0 to High(Qualities) do
        begin
          writer.QualityPercent := Qualities[qualityIndex];
          stream.Clear;
          image.SaveToStream(stream,writer);
          Check(writer.Config.method = 4, 'Default lossy method is not 4');
          CompareLegacy(image,stream,writer.QualityPercent,false);
        end;
      finally before.Free; end;
    end;
    exactWriter := TExactWriter.Create;
    try
      exactWriter.KeepHiddenRGB := true;
      stream.Clear;
      image.SaveToStream(stream,exactWriter);
      CheckPixels(image,stream,true,true);
      Check((exactWriter.Config.quality = 70) and (exactWriter.Config.method = 4)
        and (exactWriter.Config.exact = 1), 'Subclass configuration ignored');
      exactWriter.KeepHiddenRGB := false;
      stream.Clear;
      image.SaveToStream(stream,exactWriter);
      CompareLegacy(image,stream,0,true);
    finally exactWriter.Free; end;
    rejected := false;
    try writer.CompressionEffort := -1; except on ERangeError do rejected := true; end;
    Check(rejected and (writer.CompressionEffort = 6), 'Negative effort accepted');
    rejected := false;
    try writer.CompressionEffort := 10; except on ERangeError do rejected := true; end;
    Check(rejected and (writer.CompressionEffort = 6), 'Effort above 9 accepted');
    invalidWriter := TInvalidWriter.Create;
    try
      stream.Clear;
      rejected := false;
      try image.SaveToStream(stream,invalidWriter); except on Exception do rejected := true; end;
      Check(rejected and (stream.Size = 0), 'Invalid subclass config was not rejected');
    finally invalidWriter.Free; end;
    broken := TBrokenStream.Create;
    try
      rejected := false;
      try image.SaveToStream(broken,writer); except on EWriteError do rejected := true; end;
      Check(rejected, 'Stream exception not propagated');
      stream.Clear;
      image.SaveToStream(stream,writer);
      CheckPixels(image,stream,false,false);
    finally broken.Free; end;
  finally
    stream.Free;
    writer.Free;
    image.Free;
    LibWebPUnload;
  end;
end;
begin
  try
    Run;
    WriteLn(Checks,' checks passed');
  except
    on E: Exception do begin WriteLn('FAIL: ', E.Message); Halt(1); end;
  end;
end.
