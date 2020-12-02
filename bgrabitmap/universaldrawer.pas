// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit UniversalDrawer;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, FPImage, BGRABitmapTypes, BGRAGraphics, BGRAPen, BGRAArrow;

type

  { TUniversalDrawer }

  TUniversalDrawer = class(TCustomUniversalDrawer)

    class function GetMaxColorChannelDepth(ADest: TCustomUniversalBitmap): byte;

    {==== Load and save files ====}

    //there are UTF8 functions that are different from standard function as those
    //depend on TFPCustomImage that does not clearly handle UTF8

    {** Load image from a file. ''filename'' is an ANSI string }
    class procedure LoadFromFile(ADest: TCustomUniversalBitmap; const AFilename: string); overload; override;
    class procedure LoadFromFile(ADest: TCustomUniversalBitmap; const AFilename: string; AOptions: TBGRALoadingOptions); overload; override;
    {** Load image from a file with the specified image reader. ''filename'' is an ANSI string }
    class procedure LoadFromFile(ADest: TCustomUniversalBitmap; const AFilename:String; AHandler:TFPCustomImageReader); overload; override;
    class procedure LoadFromFile(ADest: TCustomUniversalBitmap; const AFilename:String; AHandler:TFPCustomImageReader; AOptions: TBGRALoadingOptions); overload; override;
    {** Load image from a file. ''filename'' is an UTF8 string }
    class procedure LoadFromFileUTF8(ADest: TCustomUniversalBitmap; const AFilenameUTF8: string; AOptions: TBGRALoadingOptions = []); overload; override;
    {** Load image from a file with the specified image reader. ''filename'' is an UTF8 string }
    class procedure LoadFromFileUTF8(ADest: TCustomUniversalBitmap; const AFilenameUTF8: string; AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions = []); overload; override;
    {** Load image from a stream. Format is detected automatically }
    class procedure LoadFromStream(ADest: TCustomUniversalBitmap; AStream: TStream); overload; override;
    class procedure LoadFromStream(ADest: TCustomUniversalBitmap; AStream: TStream; AOptions: TBGRALoadingOptions); overload; override;
    {** Load image from a stream. The specified image reader is used }
    class procedure LoadFromStream(ADest: TCustomUniversalBitmap; AStream: TStream; AHandler: TFPCustomImageReader); overload; override;
    class procedure LoadFromStream(ADest: TCustomUniversalBitmap; AStream: TStream; AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions); overload; override;
    {** Load image from an embedded Lazarus resource. Format is detected automatically }
    class procedure LoadFromResource(ADest: TCustomUniversalBitmap; AFilename: string); overload; override;
    class procedure LoadFromResource(ADest: TCustomUniversalBitmap; AFilename: string; AOptions: TBGRALoadingOptions); overload; override;
    {** Load image from an embedded Lazarus resource. The specified image reader is used }
    class procedure LoadFromResource(ADest: TCustomUniversalBitmap; AFilename: string; AHandler: TFPCustomImageReader); overload; override;
    class procedure LoadFromResource(ADest: TCustomUniversalBitmap; AFilename: string; AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions); overload; override;

    {** Save image to a file. The format is guessed from the file extension. ''filename'' is an ANSI string }
    class procedure SaveToFile(ASource: TCustomUniversalBitmap; const AFilename: string); overload; override;
    {** Save image to a file with the specified image writer. ''filename'' is an ANSI string }
    class procedure SaveToFile(ASource: TCustomUniversalBitmap; const AFilename: string; AHandler:TFPCustomImageWriter); overload; override;
    {** Save image to a file. The format is guessed from the file extension. ''filename'' is an ANSI string }
    class procedure SaveToFileUTF8(ASource: TCustomUniversalBitmap; const AFilenameUTF8: string); overload; override;
    {** Save image to a file with the specified image writer. ''filename'' is an UTF8 string }
    class procedure SaveToFileUTF8(ASource: TCustomUniversalBitmap; const AFilenameUTF8: string; AHandler:TFPCustomImageWriter); overload; override;

    {** Save image to a stream in the specified image format }
    class procedure SaveToStreamAs(ASource: TCustomUniversalBitmap; AStream: TStream; AFormat: TBGRAImageFormat); override;
    {** Save image to a stream in PNG format }
    class procedure SaveToStreamAsPng(ASource: TCustomUniversalBitmap; AStream: TStream); override;

    {==== Pixelwise drawing ====}

    class function CheckRectBounds(var x,y,x2,y2: integer; minsize: integer): boolean;
    class function CheckAntialiasRectBounds(var x, y, x2, y2: single; w: single): boolean;

    {** Draws an aliased line from (x1,y1) to (x2,y2) using Bresenham's algorithm.
        ''DrawLastPixel'' specifies if (x2,y2) must be drawn. }
    class procedure DrawLine(ADest: TCustomUniversalBitmap; x1, y1, x2, y2: integer; const ABrush: TUniversalBrush; DrawLastPixel: boolean; AAlpha: Word = 65535); override;
    {** Draws an antialiased line from (x1,y1) to (x2,y2) using an improved version of Bresenham's algorithm
        ''c'' specifies the color. ''DrawLastPixel'' specifies if (x2,y2) must be drawn }
    class procedure DrawLineAntialias(ADest: TCustomUniversalBitmap; x1, y1, x2, y2: integer; const ABrush: TUniversalBrush; DrawLastPixel: boolean; AAlpha: Word = 65535); overload; override;
    {** Draws an antialiased line with two colors ''c1'' and ''c2'' as dashes of length ''dashLen''.
        ''DashPos'' can be used to specify the start dash position and to retrieve the dash position at the end
        of the line, in order to draw a polyline with consistent dashes }
    class procedure DrawLineAntialias(ADest: TCustomUniversalBitmap; x1, y1, x2, y2: integer; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; var DashPos: integer; DrawLastPixel: boolean; AAlpha: Word = 65535); override;

    class procedure DrawPolyLine(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush: TUniversalBrush; DrawLastPixel: boolean; AAlpha: Word = 65535); override;
    class procedure DrawPolyLineAntialias(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush: TUniversalBrush; DrawLastPixel: boolean; AAlpha: Word = 65535); overload; override;
    class procedure DrawPolyLineAntialias(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; DrawLastPixel: boolean; AAlpha: Word = 65535); overload; override;

    class procedure DrawPolygon(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush: TUniversalBrush; AAlpha: Word = 65535); override;
    class procedure DrawPolygonAntialias(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; override;
    class procedure DrawPolygonAntialias(ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush1, ABrush2: TUniversalBrush; ADashLen: integer; AAlpha: Word = 65535); overload; override;

    {** Draw the border of a rectangle }
    class procedure Rectangle(ADest: TCustomUniversalBitmap; x, y, x2, y2: integer; const ABrush: TUniversalBrush; AAlpha: Word = 65535); overload; override;
    {** Draw a filled rectangle with a border }
    class procedure Rectangle(ADest: TCustomUniversalBitmap; x, y, x2, y2: integer; const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word = 65535); overload; override;

    class procedure RoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX, DY: integer; const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word = 65535); overload; override;
    class procedure RoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX, DY: integer; const ABorderBrush: TUniversalBrush; AAlpha: Word = 65535); overload; override;
    class procedure FillRoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX, DY: integer; const AFillBrush: TUniversalBrush; AAlpha: Word = 65535); override;

    class procedure FillShape(ADest: TCustomUniversalBitmap; AShape: TBGRACustomFillInfo; AFillMode: TFillMode; ABrush: TUniversalBrush; AAlpha: Word = 65535); override;
    class procedure FillPoly(ADest: TCustomUniversalBitmap; const APoints: array of TPointF; AFillMode: TFillMode; ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean = true; AAlpha: Word = 65535); override;

    {==== Using pen ====}
    class function CreatePenStroker: TBGRACustomPenStroker; override;
    class function CreateArrow: TBGRACustomArrow; override;

    class procedure RectangleAntialias(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; x, y, x2, y2: single;
                       const ABrush: TUniversalBrush; AWidth: single); override;
    class procedure DrawPolygonAntialias(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker;
                       const APoints: array of TPointF; const ABrush: TUniversalBrush; AWidth: single); overload; override;

    class procedure Ellipse(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; x, y, rx, ry: single;
        const ABrush: TUniversalBrush; AWidth: single; AAlpha: Word=65535); overload; override;
    class procedure Ellipse(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; const AOrigin, AXAxis, AYAxis: TPointF;
        const ABrush: TUniversalBrush; AWidth: single; AAlpha: Word=65535); overload; override;
    class procedure EllipseAntialias(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; x, y, rx, ry: single;
        const ABrush: TUniversalBrush; AWidth: single); overload; override;
    class procedure EllipseAntialias(ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; const AOrigin, AXAxis, AYAxis: TPointF;
        const ABrush: TUniversalBrush; AWidth: single); overload; override;

    {==== Filling ====}
    class procedure FillRectAntialias(ADest: TCustomUniversalBitmap;
                    x, y, x2, y2: single; const ABrush: TUniversalBrush;
                    APixelCenteredCoordinates: boolean = true); override;
    class procedure FillRoundRectAntialias(ADest: TCustomUniversalBitmap;
                    x,y,x2,y2, rx,ry: single; const ABrush: TUniversalBrush;
                    AOptions: TRoundRectangleOptions = []; APixelCenteredCoordinates: boolean = true); override;
    class procedure FillShapeAntialias(ADest: TCustomUniversalBitmap;
                    AShape: TBGRACustomFillInfo; AFillMode: TFillMode;
                    ABrush: TUniversalBrush); override;
    class procedure FillPolyAntialias(ADest: TCustomUniversalBitmap;
                    const APoints: array of TPointF; AFillMode: TFillMode;
                    ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean); override;
    class procedure FillEllipseAntialias(ADest: TCustomUniversalBitmap;
                    x, y, rx, ry: single; const ABrush: TUniversalBrush); overload; override;
    class procedure FillEllipseAntialias(ADest: TCustomUniversalBitmap;
                    const AOrigin, AXAxis, AYAxis: TPointF; const ABrush: TUniversalBrush); overload; override;

    //filters
    class procedure FilterBlurRadial(ASource: TCustomUniversalBitmap; const ABounds: TRect;
                              radiusX, radiusY: single; blurType: TRadialBlurType;
                              ADest: TCustomUniversalBitmap); override;
    class procedure FilterBlurMotion(ASource: TCustomUniversalBitmap; const ABounds: TRect;
                              distance: single; angle: single; oriented: boolean;
                              ADest: TCustomUniversalBitmap); override;
    class procedure FilterCustomBlur(ASource: TCustomUniversalBitmap; const ABounds: TRect;
                              mask: TCustomUniversalBitmap;
                              ADest: TCustomUniversalBitmap); override;

  end;

implementation

uses BGRAPolygon, BGRAPolygonAliased, BGRAPath, BGRAFillInfo, BGRAUTF8,
  BGRAReadBMP, BGRAReadJpeg, BGRAWritePNG, BGRAWriteTiff,
  BGRAFilterBlur, Math, FPWritePNM;

{ TUniversalDrawer }

class function TUniversalDrawer.GetMaxColorChannelDepth(ADest: TCustomUniversalBitmap): byte;
var
  idxAlpha, i: Integer;
  bits: Byte;
begin
  result := 0;
  idxAlpha := ADest.Colorspace.IndexOfAlphaChannel;
  for i := 0 to ADest.Colorspace.GetChannelCount-1 do
    if i <> idxAlpha then
    begin
      bits := ADest.Colorspace.GetChannelBitDepth(i);
      if bits > result then result := bits;
    end;
end;

class procedure TUniversalDrawer.LoadFromFile(ADest: TCustomUniversalBitmap;
  const AFilename: string);
begin
  LoadFromFileUTF8(ADest, SysToUtf8(AFilename));
end;

class procedure TUniversalDrawer.LoadFromFile(ADest: TCustomUniversalBitmap;
  const AFilename: string; AOptions: TBGRALoadingOptions);
begin
  LoadFromFileUTF8(ADest, SysToUtf8(AFilename), AOptions);
end;

class procedure TUniversalDrawer.LoadFromFile(ADest: TCustomUniversalBitmap;
  const AFilename: String; AHandler: TFPCustomImageReader);
begin
  LoadFromFileUTF8(ADest, SysToUtf8(AFilename), AHandler);
end;

class procedure TUniversalDrawer.LoadFromFile(ADest: TCustomUniversalBitmap;
  const AFilename: String; AHandler: TFPCustomImageReader;
  AOptions: TBGRALoadingOptions);
begin
  LoadFromFileUTF8(ADest, SysToUtf8(AFilename), AHandler, AOptions);
end;

class procedure TUniversalDrawer.LoadFromFileUTF8(
  ADest: TCustomUniversalBitmap; const AFilenameUTF8: string;
  AOptions: TBGRALoadingOptions);
var
  stream: TStream;
  format: TBGRAImageFormat;
  reader: TFPCustomImageReader;
begin
  stream := TFileStreamUTF8.Create(AFilenameUTF8, fmOpenRead or fmShareDenyWrite);
  try
    format := DetectFileFormat(Stream, ExtractFileExt(AFilenameUTF8));
    reader := CreateBGRAImageReader(format);
    try
      ADest.LoadFromStream(stream, reader, AOptions);
    finally
      reader.Free;
    end;
  finally
    stream.Free;
  end;
end;

class procedure TUniversalDrawer.LoadFromFileUTF8(
  ADest: TCustomUniversalBitmap; const AFilenameUTF8: string;
  AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions);
var
  stream: TStream;
begin
  stream := TFileStreamUTF8.Create(AFilenameUTF8, fmOpenRead or fmShareDenyWrite);
  try
    ADest.LoadFromStream(stream, AHandler, AOptions);
  finally
    stream.Free;
  end;
end;

class procedure TUniversalDrawer.LoadFromStream(ADest: TCustomUniversalBitmap;
  AStream: TStream);
begin
  ADest.LoadFromStream(AStream, [loKeepTransparentRGB]);
end;

class procedure TUniversalDrawer.LoadFromStream(ADest: TCustomUniversalBitmap;
  AStream: TStream; AOptions: TBGRALoadingOptions);
var
  format: TBGRAImageFormat;
  reader: TFPCustomImageReader;
begin
  format := DetectFileFormat(AStream);
  reader := CreateBGRAImageReader(format);
  try
    ADest.LoadFromStream(AStream, reader, AOptions);
  finally
    reader.Free;
  end;
end;

class procedure TUniversalDrawer.LoadFromStream(ADest: TCustomUniversalBitmap;
  AStream: TStream; AHandler: TFPCustomImageReader);
begin
  ADest.LoadFromStream(AStream, AHandler, [loKeepTransparentRGB]);
end;

class procedure TUniversalDrawer.LoadFromStream(ADest: TCustomUniversalBitmap; AStream: TStream; AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions);
var OldBmpOption: TBMPTransparencyOption;
  OldJpegPerf: TJPEGReadPerformance;
begin
  if (loBmpAutoOpaque in AOptions) and (AHandler is TBGRAReaderBMP) then
  begin
    OldBmpOption := TBGRAReaderBMP(AHandler).TransparencyOption;
    TBGRAReaderBMP(AHandler).TransparencyOption := toAuto;
    TFPCustomImage(ADest).LoadFromStream(AStream, AHandler);
    TBGRAReaderBMP(AHandler).TransparencyOption := OldBmpOption;
  end else
  if (loJpegQuick in AOptions) and (AHandler is TBGRAReaderJpeg) then
  begin
    OldJpegPerf := TBGRAReaderJpeg(AHandler).Performance;
    TBGRAReaderJpeg(AHandler).Performance := jpBestSpeed;
    TFPCustomImage(ADest).LoadFromStream(AStream, AHandler);
    TBGRAReaderJpeg(AHandler).Performance := OldJpegPerf;
  end else
    TFPCustomImage(ADest).LoadFromStream(AStream, AHandler);
  if not (loKeepTransparentRGB in AOptions) then
    ADest.ClearTransparentPixels;
end;

class procedure TUniversalDrawer.LoadFromResource(
  ADest: TCustomUniversalBitmap; AFilename: string);
begin
  LoadFromResource(ADest, AFilename, [loKeepTransparentRGB]);
end;

class procedure TUniversalDrawer.LoadFromResource(
  ADest: TCustomUniversalBitmap; AFilename: string;
  AOptions: TBGRALoadingOptions);
var
  stream: TStream;
  format: TBGRAImageFormat;
  reader: TFPCustomImageReader;
  ext: String;
begin
  stream := BGRAResource.GetResourceStream(AFilename);
  try
    ext := Uppercase(ExtractFileExt(AFilename));
    if (ext = '.BMP') and BGRAResource.IsWinResource(AFilename) then
    begin
      reader := TBGRAReaderBMP.Create;
      TBGRAReaderBMP(reader).Subformat := bsfHeaderless;
    end else
    begin
      format := DetectFileFormat(stream, ext);
      reader := CreateBGRAImageReader(format);
    end;
    try
      ADest.LoadFromStream(stream, reader, AOptions);
    finally
      reader.Free;
    end;
  finally
    stream.Free;
  end;
end;

class procedure TUniversalDrawer.LoadFromResource(
  ADest: TCustomUniversalBitmap; AFilename: string;
  AHandler: TFPCustomImageReader);
begin
  LoadFromResource(ADest, AFilename, AHandler, [loKeepTransparentRGB]);
end;

class procedure TUniversalDrawer.LoadFromResource(
  ADest: TCustomUniversalBitmap; AFilename: string;
  AHandler: TFPCustomImageReader; AOptions: TBGRALoadingOptions);
var
  stream: TStream;
begin
  stream := BGRAResource.GetResourceStream(AFilename);
  try
    ADest.LoadFromStream(stream, AHandler, AOptions);
  finally
    stream.Free;
  end;
end;

class procedure TUniversalDrawer.SaveToFile(ASource: TCustomUniversalBitmap;
  const AFilename: string);
begin
  SaveToFileUTF8(ASource, SysToUtf8(AFilename));
end;

class procedure TUniversalDrawer.SaveToFile(ASource: TCustomUniversalBitmap;
  const AFilename: string; AHandler: TFPCustomImageWriter);
begin
  SaveToFileUTF8(ASource, SysToUtf8(AFilename), AHandler);
end;

class procedure TUniversalDrawer.SaveToFileUTF8(
  ASource: TCustomUniversalBitmap; const AFilenameUTF8: string);
var
  writer: TFPCustomImageWriter;
  format: TBGRAImageFormat;
  ext: String;
begin
  format := SuggestImageFormat(AFilenameUTF8);
  if (format = ifXPixMap) and (ASource.NbPixels > 32768) then //xpm is slow so avoid big images
    raise exception.Create('Image is too big to be saved as XPM');
  writer := CreateBGRAImageWriter(Format, ASource.HasTransparentPixels);
  if GetMaxColorChannelDepth(ASource) > 8 then
  begin
    if writer is TBGRAWriterPNG then TBGRAWriterPNG(writer).WordSized := true;
  end;
  if writer is TFPWriterPNM then
  begin
    ext := LowerCase(ExtractFileExt(AFilenameUTF8));
    if ext = '.pbm' then TFPWriterPNM(writer).ColorDepth:= pcdBlackWhite else
    if ext = '.pgm' then TFPWriterPNM(writer).ColorDepth:= pcdGrayscale else
    if ext = '.ppm' then TFPWriterPNM(writer).ColorDepth:= pcdRGB;
  end;
  try
    SaveToFileUTF8(ASource, AFilenameUTF8, writer);
  finally
    writer.free;
  end;
end;

class procedure TUniversalDrawer.SaveToFileUTF8(
  ASource: TCustomUniversalBitmap; const AFilenameUTF8: string;
  AHandler: TFPCustomImageWriter);
var
  stream: TFileStreamUTF8;
begin
   stream := TFileStreamUTF8.Create(AFilenameUTF8, fmCreate);
   try
     TFPCustomImage(ASource).SaveToStream(stream, AHandler);
   finally
     stream.Free;
   end;
end;

class procedure TUniversalDrawer.SaveToStreamAs(
  ASource: TCustomUniversalBitmap; AStream: TStream; AFormat: TBGRAImageFormat);
var writer: TFPCustomImageWriter;
begin
  writer := CreateBGRAImageWriter(AFormat, ASource.HasTransparentPixels);
  if GetMaxColorChannelDepth(ASource) > 8 then
  begin
    if writer is TBGRAWriterPNG then TBGRAWriterPNG(writer).WordSized := true;
  end;
  try
    TFPCustomImage(ASource).SaveToStream(AStream, writer)
  finally
    writer.Free;
  end;
end;

class procedure TUniversalDrawer.SaveToStreamAsPng(
  ASource: TCustomUniversalBitmap; AStream: TStream);
begin
  SaveToStreamAs(ASource, AStream, ifPNG);
end;

class function TUniversalDrawer.CheckRectBounds(
  var x, y, x2, y2: integer; minsize: integer): boolean;
var
  temp: integer;
begin
  //swap coordinates if needed
  if (x > x2) then
  begin
    temp := x;
    x    := x2;
    x2   := temp;
  end;
  if (y > y2) then
  begin
    temp := y;
    y    := y2;
    y2   := temp;
  end;
  result := (x2 - x > minsize) and (y2 - y > minsize);
end;

class function TUniversalDrawer.CheckAntialiasRectBounds(var x, y, x2,
  y2: single; w: single): boolean;
var
  temp: Single;
begin
  if (x > x2) then
  begin
    temp := x;
    x    := x2;
    x2   := temp;
  end;
  if (y > y2) then
  begin
    temp := y;
    y    := y2;
    y2   := temp;
  end;

  result := (x2 - x > w) and (y2 - y > w);
end;

class procedure TUniversalDrawer.DrawLine(ADest: TCustomUniversalBitmap; x1,
  y1, x2, y2: integer; const ABrush: TUniversalBrush; DrawLastPixel: boolean;
  AAlpha: Word);
type
  TDrawPixelProc = procedure(x,y: Int32or64; const ABrush: TUniversalBrush; AAlpha: Word = 65535) of object;
var
  Y, X: integer;
  DX, DY, SX, SY, E: integer;
  drawPixelProc: TDrawPixelProc;
  skip: Boolean;
  r: TRect;
  E64: Int64;
begin
  r := ADest.ClipRect;
  skip := false;
  if ABrush.DoesNothing or (AAlpha= 0) then skip := true;
  if (x1 < r.Left) and (x2 < r.Left) then skip := true;
  if (x1 >= r.Right) and (x2 >= r.Right) then skip := true;
  if (y1 < r.Top) and (y2 < r.Top) then skip := true;
  if (y1 >= r.Bottom) and (y2 >= r.Bottom) then skip := true;
  if skip then exit;

  if (Y1 = Y2) then
  begin
    if (X1 = X2) then
    begin
      if DrawLastPixel then ADest.DrawPixel(X1, Y1, ABrush, AAlpha);
    end else
    begin
      if not DrawLastPixel then
      begin
        if X2 > X1 then dec(X2) else inc(X2);
      end;
      ADest.HorizLine(X1,Y1,X2, ABrush, AAlpha);
    end;
    Exit;
  end else
  if (X1 = X2) then
  begin
    if not DrawLastPixel then
    begin
      if Y2 > Y1 then dec(Y2) else inc(Y2);
    end;
    ADest.VertLine(X1,Y1,Y2, ABrush, AAlpha);
    Exit;
  end;

  DX := X2 - X1;
  DY := Y2 - Y1;

  if DX < 0 then
  begin
    SX := -1;
    DX := -DX;
  end
  else SX := 1;

  if DY < 0 then
  begin
    SY := -1;
    DY := -DY;
  end
  else SY := 1;

  DX := DX shl 1;
  DY := DY shl 1;

  drawPixelProc := @ADest.DrawPixel;
  X := X1;
  Y := Y1;
  if DX > DY then
  begin
    E := DY - DX shr 1;
    if (X < r.Left) and (SX > 0) then
    begin
      E64 := E+int64(DY)*(r.Left-X)+DX;
      E := (E64 mod DX)-DX;
      Inc(Y, (E64 div DX)*SY);
      X := r.Left;
    end;
    if (X >= r.Right) and (SX < 0) then
    begin
      E64 := E+int64(DY)*(X-(r.Right-1))+DX;
      E := (E64 mod DX)-DX;
      Inc(Y, (E64 div DX)*SY);
      X := r.Right-1;
    end;
    if (X2 < r.Left-1) and (SX < 0) then X2 := r.Left-1;
    if (X2 > r.Right) and (SX > 0) then X2 := r.Right;
    while X <> X2 do
    begin
      drawPixelProc(X, Y, ABrush, AAlpha);
      if E >= 0 then
      begin
        Inc(Y, SY);
        Dec(E, DX);
      end;
      Inc(X, SX);
      Inc(E, DY);
    end;
  end
  else
  begin
    E := DX - DY shr 1;
    if (Y < r.Top) and (SY > 0) then
    begin
      E64 := E+int64(DX)*(r.Top-Y)+DY;
      E := (E64 mod DY)-DY;
      Inc(X, (E64 div DY)*SX);
      Y := r.Top;
    end;
    if (Y >= r.Bottom) and (SY < 0) then
    begin
      E64 := E+int64(DX)*(Y-(r.Bottom-1))+DY;
      E := (E64 mod DY)-DY;
      Inc(X, (E64 div DY)*SX);
      Y := r.Bottom-1;
    end;
    if (Y2 < r.Top-1) and (SY < 0) then Y2 := r.Top-1;
    if (Y2 > r.Bottom) and (SY > 0) then Y2 := r.Bottom;
    while Y <> Y2 do
    begin
      drawPixelProc(X, Y, ABrush, AAlpha);
      if E >= 0 then
      begin
        Inc(X, SX);
        Dec(E, DY);
      end;
      Inc(Y, SY);
      Inc(E, DX);
    end;
  end;

  if DrawLastPixel then
    drawPixelProc(X2, Y2, ABrush, AAlpha);
end;

class procedure TUniversalDrawer.DrawLineAntialias(ADest: TCustomUniversalBitmap;
  x1, y1, x2, y2: integer; const ABrush: TUniversalBrush;
  DrawLastPixel: boolean; AAlpha: Word = 65535);
var
  dashPos: integer;
begin
  dashPos := 0;
  DrawLineAntialias(ADest,x1,y1,x2,y2, ABrush,ABrush,$1000000,dashPos,DrawLastPixel,AAlpha);
end;

class procedure TUniversalDrawer.DrawLineAntialias(ADest: TCustomUniversalBitmap;
  x1, y1, x2, y2: integer; const ABrush1, ABrush2: TUniversalBrush;
  ADashLen: integer; var DashPos: integer; DrawLastPixel: boolean;
  AAlpha: Word = 65535);
var
  curBrush: PUniversalBrush;

  procedure SkipDash(ACount: integer);
  begin
    if ACount = 0 then exit;
    DashPos := PositiveMod(DashPos+ACount, ADashLen+ADashLen);
    if DashPos < ADashLen then curBrush := @ABrush1 else curBrush := @ABrush2;
  end;

var
  X, Y, DX, DY, SX, SY, E,count, skipAfter: integer;
  curAlpha: Word;
  skip: Boolean;
  r: TRect;
  E64: Int64;
begin
  r := ADest.ClipRect;
  skip := false;
  if (ABrush1.DoesNothing and ABrush2.DoesNothing) or (AAlpha=0) then skip := true;
  if (x1 < r.Left) and (x2 < r.Left) then skip := true;
  if (x1 >= r.Right) and (x2 >= r.Right) then skip := true;
  if (y1 < r.Top) and (y2 < r.Top) then skip := true;
  if (y1 >= r.Bottom) and (y2 >= r.Bottom) then skip := true;

  if ADashLen<=0 then ADashLen := 1;
  if skip then
  begin
    count := max(abs(x2-x1),abs(y2-y1));
    if DrawLastPixel then inc(count);
    SkipDash(count);
    exit;
  end;

  DashPos := PositiveMod(DashPos,ADashLen+ADashLen);
  if DashPos < ADashLen then curBrush := @ABrush1 else curBrush := @ABrush2;

  if (Y1 = Y2) and (X1 = X2) then
  begin
    if DrawLastPixel then
    begin
      ADest.DrawPixel(X1, Y1, curBrush^, AAlpha);
      inc(DashPos);
      if DashPos = ADashLen + ADashLen then DashPos := 0;
    end;
    Exit;
  end;

  DX := X2 - X1;
  DY := Y2 - Y1;
  if DX < 0 then
  begin
    SX := -1;
    DX := -DX;
  end else SX := 1;

  if DY < 0 then
  begin
    SY := -1;
    DY := -DY;
  end else SY := 1;

  DX := DX shl 1;
  DY := DY shl 1;
  X := X1;
  Y := Y1;
  if DX > DY then
  begin
    E := 0;
    if (X < r.Left) and (SX > 0) then
    begin
      E64 := E+int64(DY)*(r.Left-X);
      E := E64 mod DX;
      Inc(Y, (E64 div DX)*SY);
      SkipDash(r.Left-X);
      X := r.Left;
    end;
    if (X >= r.Right) and (SX < 0) then
    begin
      E64 := E+int64(DY)*(X-(r.Right-1));
      E := E64 mod DX;
      Inc(Y, (E64 div DX)*SY);
      SkipDash(X-(r.Right-1));
      X := r.Right-1;
    end;
    if (X2 < r.Left-1) and (SX < 0) then
    begin
      skipAfter := (r.Left-1)-X2;
      X2 := r.Left-1;
    end else
    if (X2 > r.Right) and (SX > 0) then
    begin
      skipAfter := X2-r.Right;
      X2 := r.Right;
    end else
      skipAfter := 0;
    while X <> X2 do
    begin
      curAlpha := AAlpha * E div DX;
      ADest.DrawPixel(X, Y, curBrush^, AAlpha - curAlpha);
      ADest.DrawPixel(X, Y + SY, curBrush^, curAlpha);
      Inc(E, DY);
      if E >= DX then
      begin
        Inc(Y, SY);
        Dec(E, DX);
      end;
      Inc(X, SX);

      Inc(DashPos);
      if DashPos = ADashLen then
        curBrush := @ABrush2
      else
      if DashPos = ADashLen + ADashLen then
      begin
        curBrush := @ABrush1;
        DashPos := 0;
      end;
    end;
  end
  else
  begin
    E := 0;
    if (Y < r.Top) and (SY > 0) then
    begin
      E64 := E+int64(DX)*(r.Top-Y);
      E := E64 mod DY;
      Inc(X, (E64 div DY)*SX);
      SkipDash(r.Top-Y);
      Y := r.Top;
    end;
    if (Y >= r.Bottom) and (SY < 0) then
    begin
      E64 := E+int64(DX)*(Y-(r.Bottom-1));
      E := E64 mod DY;
      Inc(X, (E64 div DY)*SX);
      SkipDash(Y-(r.Bottom-1));
      Y := r.Bottom-1;
    end;
    if (Y2 < r.Top-1) and (SY < 0) then
    begin
      skipAfter := (r.Top-1)-Y2;
      Y2 := r.Top-1;
    end else
    if (Y2 > r.Bottom) and (SY > 0) then
    begin
      skipAfter := Y2-r.Bottom;
      Y2 := r.Bottom;
    end else
      skipAfter := 0;
    while Y <> Y2 do
    begin
      curAlpha := AAlpha * E div DY;
      ADest.DrawPixel(X, Y, curBrush^, AAlpha - curAlpha);
      ADest.DrawPixel(X + SX, Y, curBrush^, curAlpha);
      Inc(E, DX);
      if E >= DY then
      begin
        Inc(X, SX);
        Dec(E, DY);
      end;
      Inc(Y, SY);

      Inc(DashPos);
      if DashPos = ADashLen then
        curBrush := @ABrush2
      else
      if DashPos = ADashLen + ADashLen then
      begin
        curBrush := @ABrush1;
        DashPos := 0;
      end;
    end;
  end;
  if DrawLastPixel then
  begin
    ADest.DrawPixel(X2, Y2, curBrush^, AAlpha);
    inc(DashPos);
    if DashPos = ADashLen + ADashLen then DashPos := 0;
  end;
  SkipDash(skipAfter);
end;

class procedure TUniversalDrawer.DrawPolyLine(ADest: TCustomUniversalBitmap;
  const points: array of TPoint; const ABrush: TUniversalBrush;
  DrawLastPixel: boolean; AAlpha: Word);
var i,start: integer;
begin
  if ABrush.DoesNothing then exit;
  start := 0;
  for i := 0 to high(points) do
  if IsEmptyPoint(points[i]) then start := i+1 else
  begin
    if (i = high(points)) or IsEmptyPoint(points[i+1]) then
    begin
      if (i = start) and DrawLastPixel then ADest.DrawPixel(points[i].x,points[i].y, ABrush,AAlpha);
    end else
      DrawLine(ADest, points[i].x,points[i].Y,points[i+1].x,points[i+1].y, ABrush,
        DrawLastPixel and ((i=high(points)-1) or IsEmptyPoint(points[i+2])), AAlpha);
  end;
end;

class procedure TUniversalDrawer.DrawPolyLineAntialias(
  ADest: TCustomUniversalBitmap; const points: array of TPoint;
  const ABrush: TUniversalBrush; DrawLastPixel: boolean; AAlpha: Word);
var i,start: integer;
begin
  if ABrush.DoesNothing then exit;
  start := 0;
  for i := 0 to high(points) do
  if IsEmptyPoint(points[i]) then start := i+1 else
  begin
    if (i = high(points)) or IsEmptyPoint(points[i+1]) then
    begin
      if (i = start) and DrawLastPixel then ADest.DrawPixel(points[i].x,points[i].y, ABrush,AAlpha);
    end else
      DrawLineAntialias(ADest, points[i].x,points[i].Y,points[i+1].x,points[i+1].y, ABrush,
        DrawLastPixel and ((i=high(points)-1) or IsEmptyPoint(points[i+2])), AAlpha);
  end;
end;

class procedure TUniversalDrawer.DrawPolyLineAntialias(
  ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush1,
  ABrush2: TUniversalBrush; ADashLen: integer; DrawLastPixel: boolean;
  AAlpha: Word);
var i,start, dashPos: integer;
begin
  if ABrush1.DoesNothing and ABrush2.DoesNothing then exit;
  start := 0;
  dashPos := 0;
  for i := 0 to high(points) do
  if IsEmptyPoint(points[i]) then start := i+1 else
  begin
    if (i = high(points)) or IsEmptyPoint(points[i+1]) then
    begin
      if (i = start) and DrawLastPixel then
      begin
        if dashPos < ADashLen then
          ADest.DrawPixel(points[i].x,points[i].y, ABrush1,AAlpha)
        else
          ADest.DrawPixel(points[i].x,points[i].y, ABrush2,AAlpha);
        inc(dashPos);
        if dashPos = ADashLen*2 then dashPos := 0;
      end;
    end else
      DrawLineAntialias(ADest, points[i].x,points[i].Y,points[i+1].x,points[i+1].y,
        ABrush1,ABrush2,ADashLen,dashPos,
        DrawLastPixel and ((i=high(points)-1) or IsEmptyPoint(points[i+2])), AAlpha);
  end;
end;

class procedure TUniversalDrawer.DrawPolygon(ADest: TCustomUniversalBitmap;
  const points: array of TPoint; const ABrush: TUniversalBrush; AAlpha: Word);
var i,start: integer;
begin
  if ABrush.DoesNothing then exit;
  start := 0;
  for i := 0 to high(points) do
  if IsEmptyPoint(points[i]) then start := i+1 else
  begin
    if (i = high(points)) or IsEmptyPoint(points[i+1]) then
    begin
      if i = start then ADest.DrawPixel(points[i].x,points[i].y, ABrush,AAlpha)
      else if (i > start) then
        DrawLine(ADest, points[i].x,points[i].Y,points[start].x,points[start].y, ABrush, false, AAlpha);
    end else
      DrawLine(ADest, points[i].x,points[i].Y,points[i+1].x,points[i+1].y, ABrush, false, AAlpha);
  end;
end;

class procedure TUniversalDrawer.DrawPolygonAntialias(
  ADest: TCustomUniversalBitmap; const points: array of TPoint;
  const ABrush: TUniversalBrush; AAlpha: Word);
var i,start: integer;
begin
  if ABrush.DoesNothing then exit;
  start := 0;
  for i := 0 to high(points) do
  if IsEmptyPoint(points[i]) then start := i+1 else
  begin
    if (i = high(points)) or IsEmptyPoint(points[i+1]) then
    begin
      if i = start then ADest.DrawPixel(points[i].x,points[i].y, ABrush,AAlpha)
      else if (i > start) then
        DrawLineAntialias(ADest, points[i].x,points[i].Y,points[start].x,points[start].y, ABrush, false, AAlpha);
    end else
      DrawLineAntialias(ADest, points[i].x,points[i].Y,points[i+1].x,points[i+1].y, ABrush, false, AAlpha);
  end;
end;

class procedure TUniversalDrawer.DrawPolygonAntialias(
  ADest: TCustomUniversalBitmap; const points: array of TPoint; const ABrush1,
  ABrush2: TUniversalBrush; ADashLen: integer; AAlpha: Word);
var i,start, dashPos: integer;
begin
  if ABrush1.DoesNothing and ABrush2.DoesNothing then exit;
  start := 0;
  dashPos := 0;
  for i := 0 to high(points) do
  if IsEmptyPoint(points[i]) then start := i+1 else
  begin
    if (i = high(points)) or IsEmptyPoint(points[i+1]) then
    begin
      if i = start then
      begin
        if dashPos < ADashLen then
          ADest.DrawPixel(points[i].x,points[i].y, ABrush1,AAlpha)
        else
          ADest.DrawPixel(points[i].x,points[i].y, ABrush2,AAlpha);
        inc(dashPos);
        if dashPos = ADashLen*2 then dashPos := 0;
      end
      else if (i > start) then
        DrawLineAntialias(ADest, points[i].x,points[i].Y,points[start].x,points[start].y,
                          ABrush1,ABrush2,ADashLen,dashPos, false, AAlpha);
    end else
      DrawLineAntialias(ADest, points[i].x,points[i].Y,points[i+1].x,points[i+1].y,
                        ABrush1,ABrush2,ADashLen,dashPos, false, AAlpha);
  end;
end;

class procedure TUniversalDrawer.Rectangle(ADest: TCustomUniversalBitmap; x, y, x2, y2: integer;
  const ABrush: TUniversalBrush; AAlpha: Word);
begin
  if not CheckRectBounds({%H-}x,{%H-}y,{%H-}x2,{%H-}y2,1) or ABrush.DoesNothing then exit;
  ADest.HorizLine(x, y, x2-1, ABrush, AAlpha);
  if y2-y > 2 then
  begin
    ADest.VertLine(x, y+1, y2-2, ABrush, AAlpha);
    ADest.VertLine(x2-1, y+1, y2-2, ABrush, AAlpha);
  end;
  ADest.HorizLine(x, y2-1, x2-1, ABrush, AAlpha);
end;

class procedure TUniversalDrawer.Rectangle(ADest: TCustomUniversalBitmap; x, y, x2, y2: integer;
  const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word);
begin
  if not CheckRectBounds({%H-}x,{%H-}y,{%H-}x2,{%H-}y2,1) then exit;
  Rectangle(ADest, x, y, x2, y2, ABorderBrush, AAlpha);
  ADest.FillRect(x+1, y+1, x2-1, y2-1, AFillBrush, AAlpha);
end;

class procedure TUniversalDrawer.RoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX, DY: integer;
  const ABorderBrush, AFillBrush: TUniversalBrush; AAlpha: Word);
begin
  BGRAPolygonAliased.BGRARoundRectAliased(ADest, X1,Y1,X2,Y2,DX,DY,ABorderBrush,AFillBrush,AAlpha);
end;

class procedure TUniversalDrawer.RoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX, DY: integer;
  const ABorderBrush: TUniversalBrush; AAlpha: Word);
begin
  BGRAPolygonAliased.BGRARoundRectAliased(ADest, X1,Y1,X2,Y2,DX,DY,ABorderBrush,ABorderBrush,AAlpha,false,true);
end;

class procedure TUniversalDrawer.FillRoundRect(ADest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer; DX,
  DY: integer; const AFillBrush: TUniversalBrush; AAlpha: Word);
begin
  BGRAPolygonAliased.BGRARoundRectAliased(ADest, X1,Y1,X2,Y2,DX,DY,AFillBrush,AFillBrush,AAlpha);
end;

class procedure TUniversalDrawer.FillShape(ADest: TCustomUniversalBitmap;
  AShape: TBGRACustomFillInfo; AFillMode: TFillMode; ABrush: TUniversalBrush;
  AAlpha: Word);
begin
  BGRAPolygon.FillShapeAliased(ADest, AShape, ABrush, AAlpha, AFillMode = fmWinding);
end;

class procedure TUniversalDrawer.FillPoly(ADest: TCustomUniversalBitmap;
  const APoints: array of TPointF; AFillMode: TFillMode;
  ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean; AAlpha: Word);
begin
  BGRAPolygon.FillPolyAliased(ADest, APoints, ABrush, AAlpha, AFillMode = fmWinding, APixelCenteredCoordinates);
end;

class function TUniversalDrawer.CreatePenStroker: TBGRACustomPenStroker;
begin
  result := TBGRAPenStroker.Create;
end;

class function TUniversalDrawer.CreateArrow: TBGRACustomArrow;
begin
  result := TBGRAArrow.Create;
end;

class procedure TUniversalDrawer.RectangleAntialias(
  ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; x, y, x2, y2: single;
  const ABrush: TUniversalBrush; AWidth: single);
var
  hw, bevel: Single;
begin
  if (APen.Style = psClear) or (AWidth = 0) then exit;

  if not CheckAntialiasRectBounds(x,y,x2,y2, AWidth) then
  begin
    hw := AWidth/2;
    if APen.JoinStyle = pjsBevel then
    begin
      bevel := (2 - sqrt(2)) * hw;
      FillRoundRectAntialias(ADest, x - hw, y - hw, x2 + hw, y2 + hw, bevel,bevel, ABrush,
        [rrTopLeftBevel, rrTopRightBevel, rrBottomLeftBevel, rrBottomRightBevel]);
    end else
    if APen.JoinStyle = pjsRound then
      FillRoundRectAntialias(ADest, x - hw, y - hw, x2 + hw, y2 + hw, hw,hw, ABrush)
    else
      FillRectAntialias(ADest, x - hw, y - hw, x2 + hw, y2 + hw, ABrush);
  end else
  if (APen.JoinStyle = pjsMiter) and (APen.Style = psSolid) and (APen.MiterLimit > 1.4142) then
  begin
    hw := AWidth/2;
    FillPolyAntialias(ADest, [PointF(x-hw,y-hw),PointF(x2+hw,y-hw),PointF(x2+hw,y2+hw),PointF(x-hw,y2+hw),EmptyPointF,
                PointF(x+hw,y2-hw),PointF(x2-hw,y2-hw),PointF(x2-hw,y+hw),PointF(x+hw,y+hw)],
                fmWinding, ABrush, true);
  end else
    DrawPolygonAntialias(ADest, APen, [Pointf(x,y),Pointf(x2,y),Pointf(x2,y2),Pointf(x,y2)], ABrush, AWidth);
end;

class procedure TUniversalDrawer.DrawPolygonAntialias(
  ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker;
  const APoints: array of TPointF; const ABrush: TUniversalBrush; AWidth: single);
begin
  FillPolyAntialias(ADest, APen.ComputePolygon(APoints, AWidth), ADest.FillMode, ABrush, true);
end;

class procedure TUniversalDrawer.Ellipse(ADest: TCustomUniversalBitmap;
  APen: TBGRACustomPenStroker; x, y,rx, ry: single;
  const ABrush: TUniversalBrush; AWidth: single; AAlpha: Word);
begin
  if (APen.Style = psClear) or (AWidth = 0) then exit;
  if (APen.Style = psSolid) then
    BGRAPolygon.BorderEllipse(ADest, x, y, rx, ry, AWidth, ABrush, AAlpha)
  else
  begin
    if ABrush.DoesNothing then exit;
    FillPoly(ADest, APen.ComputePolygon(BGRAPath.ComputeEllipse(x,y,rx,ry),AWidth),
             ADest.FillMode, ABrush, true, AAlpha);
  end;
end;

class procedure TUniversalDrawer.Ellipse(ADest: TCustomUniversalBitmap;
  APen: TBGRACustomPenStroker; const AOrigin, AXAxis, AYAxis: TPointF;
  const ABrush: TUniversalBrush; AWidth: single; AAlpha: Word);
begin
  if (APen.Style = psClear) or (AWidth = 0) or ABrush.DoesNothing then exit;
  FillPoly(ADest, APen.ComputePolygon(BGRAPath.ComputeEllipse(AOrigin, AXAxis, AYAxis), AWidth),
           ADest.FillMode, ABrush, true, AAlpha);
end;

class procedure TUniversalDrawer.EllipseAntialias(
  ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker;
  x, y, rx, ry: single; const ABrush: TUniversalBrush; AWidth: single);
begin
  if (APen.Style = psClear) or (AWidth = 0) then exit;
  if (APen.Style = psSolid) then
    BGRAPolygon.BorderEllipseAntialias(ADest, x, y, rx, ry, AWidth, ABrush)
  else
  begin
    if ABrush.DoesNothing then exit;
    FillPolyAntialias(ADest, APen.ComputePolygon(BGRAPath.ComputeEllipse(x,y,rx,ry),AWidth),
             ADest.FillMode, ABrush, true);
  end;
end;

class procedure TUniversalDrawer.EllipseAntialias(
  ADest: TCustomUniversalBitmap; APen: TBGRACustomPenStroker; const AOrigin,
  AXAxis, AYAxis: TPointF; const ABrush: TUniversalBrush; AWidth: single);
begin
  if (APen.Style = psClear) or (AWidth = 0) or ABrush.DoesNothing then exit;
  FillPolyAntialias(ADest, APen.ComputePolygon(BGRAPath.ComputeEllipse(AOrigin, AXAxis, AYAxis), AWidth),
           ADest.FillMode, ABrush, true);
end;

class procedure TUniversalDrawer.FillRectAntialias(
  ADest: TCustomUniversalBitmap; x, y, x2, y2: single;
  const ABrush: TUniversalBrush; APixelCenteredCoordinates: boolean);
var
  fi: TFillRectangleInfo;
begin
  if ABrush.DoesNothing then exit;
  fi := TFillRectangleInfo.Create(x,y,x2,y2,APixelCenteredCoordinates);
  FillShapeAntialias(ADest, fi, fmAlternate, ABrush);
  fi.Free;
end;

class procedure TUniversalDrawer.FillRoundRectAntialias(
  ADest: TCustomUniversalBitmap; x, y, x2, y2, rx, ry: single;
  const ABrush: TUniversalBrush; AOptions: TRoundRectangleOptions;
  APixelCenteredCoordinates: boolean);
var
  fi: TFillRoundRectangleInfo;
begin
  if ABrush.DoesNothing or (x = x2) or (y = y2) then exit;
  fi := TFillRoundRectangleInfo.Create(x,y,x2,y2, rx,ry, AOptions, APixelCenteredCoordinates);
  FillShapeAntialias(ADest, fi, fmAlternate, ABrush);
  fi.Free;
end;

class procedure TUniversalDrawer.FillShapeAntialias(
  ADest: TCustomUniversalBitmap; AShape: TBGRACustomFillInfo;
  AFillMode: TFillMode; ABrush: TUniversalBrush);
begin
  BGRAPolygon.FillShapeAntialias(ADest, AShape, ABrush, AFillMode = fmWinding);
end;

class procedure TUniversalDrawer.FillPolyAntialias(
  ADest: TCustomUniversalBitmap; const APoints: array of TPointF;
  AFillMode: TFillMode; ABrush: TUniversalBrush;
  APixelCenteredCoordinates: boolean);
begin
  BGRAPolygon.FillPolyAntialias(ADest, APoints, ABrush,
    AFillMode = fmWinding, APixelCenteredCoordinates);
end;

class procedure TUniversalDrawer.FillEllipseAntialias(
  ADest: TCustomUniversalBitmap; x, y, rx, ry: single;
  const ABrush: TUniversalBrush);
begin
  BGRAPolygon.FillEllipseAntialias(ADest, x, y, rx, ry, ABrush);
end;

class procedure TUniversalDrawer.FillEllipseAntialias(
  ADest: TCustomUniversalBitmap; const AOrigin, AXAxis, AYAxis: TPointF;
  const ABrush: TUniversalBrush);
var
  pts: array of TPointF;
begin
  if (AOrigin.y = AXAxis.y) and (AOrigin.x = AYAxis.x) then
    FillEllipseAntialias(ADest, AOrigin.x,AOrigin.y,
      abs(AXAxis.x-AOrigin.x),abs(AYAxis.y-AOrigin.y), ABrush)
  else
  if (AOrigin.x = AXAxis.x) and (AOrigin.y = AYAxis.y) then
    FillEllipseAntialias(ADest, AOrigin.x,AOrigin.y,
      abs(AYAxis.x-AOrigin.x),abs(AXAxis.y-AOrigin.y), ABrush)
  else
  begin
    if ABrush.DoesNothing then exit;
    pts := BGRAPath.ComputeEllipse(AOrigin,AXAxis,AYAxis);
    FillPolyAntialias(ADest, pts, fmAlternate, ABrush, true);
  end;
end;

class procedure TUniversalDrawer.FilterBlurRadial(
  ASource: TCustomUniversalBitmap; const ABounds: TRect; radiusX,
  radiusY: single; blurType: TRadialBlurType; ADest: TCustomUniversalBitmap);
begin
  BGRAFilterBlur.FilterBlurRadial(ASource, ABounds,
                radiusX,radiusY, blurType, ADest, nil);
end;

class procedure TUniversalDrawer.FilterBlurMotion(
  ASource: TCustomUniversalBitmap; const ABounds: TRect;
  distance: single; angle: single; oriented: boolean;
  ADest: TCustomUniversalBitmap);
begin
  BGRAFilterBlur.FilterBlurMotion(ASource, ABounds,
           distance, angle, oriented, ADest, nil);
end;

class procedure TUniversalDrawer.FilterCustomBlur(
  ASource: TCustomUniversalBitmap; const ABounds: TRect;
  mask: TCustomUniversalBitmap; ADest: TCustomUniversalBitmap);
begin
  BGRAFilterBlur.FilterBlurCustom(ASource, ABounds,
      mask, ADest, nil);
end;

initialization

  UniDrawerClass := TUniversalDrawer;

end.
