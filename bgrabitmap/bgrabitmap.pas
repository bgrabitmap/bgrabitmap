// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
 /**************************************************************************\
                                bgrabitmap.pas
                                --------------
                 Free easy-to-use memory bitmap 32-bit,
                 8-bit for each channel, transparency.
                 Channels can be in the following orders:
                 - B G R A (recommended for Windows, required for fpGUI)
                 - R G B A (recommended for Gtk and MacOS)

                 - Drawing primitives
                 - Resample
                 - Reference counter
                 - Drawing on LCL canvas
                 - Loading and saving images

                 Note : line order can change, so if you access
                 directly to bitmap data, check LineOrder value
                 or use Scanline to compute position.


       --> Include BGRABitmap and BGRABitmapTypes in the 'uses' clause.
}

unit BGRABitmap;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

{ Compiler directives are used to include the best version according
  to the platform }

uses
  BGRAClasses, BGRABitmapTypes, FPImage, SysUtils,
{$IFDEF BGRABITMAP_USE_FPGUI}
    BGRAfpGUIBitmap,
{$ELSE}
	{$IFDEF BGRABITMAP_USE_LCL}
	  {$IFDEF LCLwin32}
		BGRAWinBitmap,
	  {$ELSE}
		{$IFDEF LCLgtk}
		BGRAGtkBitmap,
		{$ELSE}
		  {$IFDEF LCLgtk2}
		BGRAGtkBitmap,
		  {$ELSE}
			{$IF defined(LCLqt) or defined(LCLqt5)}
		BGRAQtBitmap,
			{$ELSE}
              {$IFDEF DARWIN}
                BGRAMacBitmap,
              {$ELSE}
		BGRALCLBitmap,
              {$ENDIF}
			{$ENDIF}
		  {$ENDIF}
		{$ENDIF}
	  {$ENDIF}
	{$ELSE}
	  {$IFDEF BGRABITMAP_USE_MSEGUI}
            BGRAMSEguiBitmap,
          {$ELSE}
            BGRANoGuiBitmap,
          {$ENDIF}
	{$ENDIF}
{$ENDIF}
  BGRAGraphics;

type
{$IFDEF BGRABITMAP_USE_FPGUI}
  TBGRABitmap = class(TBGRAfpGUIBitmap)
{$ELSE}
    {$IFDEF BGRABITMAP_USE_LCL}
      {$IFDEF LCLwin32}
        TBGRABitmap = class(TBGRAWinBitmap)
      {$ELSE}
        {$IFDEF LCLgtk}
        TBGRABitmap = class(TBGRAGtkBitmap)
        {$ELSE}
          {$IFDEF LCLgtk2}

        { TBGRABitmap }

        TBGRABitmap = class(TBGRAGtkBitmap)
          {$ELSE}
            {$IF defined(LCLqt) or defined(LCLqt5)}
        TBGRABitmap = class(TBGRAQtBitmap)
            {$ELSE}
              {$IFDEF DARWIN}
        TBGRABitmap = class(TBGRAMacBitmap)
              {$ELSE}
        TBGRABitmap = class(TBGRALCLBitmap)
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ELSE}
      {$IFDEF BGRABITMAP_USE_MSEGUI}
        TBGRABitmap = class(TBGRAMSEguiBitmap)
      {$ELSE}
        TBGRABitmap = class(TBGRANoGUIBitmap)
      {$ENDIF}
    {$ENDIF}
{$ENDIF}
  public
    function NewBitmap: TBGRABitmap; overload; override;
    function NewBitmap(AWidth, AHeight: integer): TBGRABitmap; overload; override;
    function NewBitmap(AWidth, AHeight: integer; const Color: TBGRAPixel): TBGRABitmap; overload; override;
    function NewBitmap(AWidth, AHeight: integer; AColor: Pointer): TBGRABitmap; overload; override;
    function NewBitmap(Filename: string): TBGRABitmap; overload; override;
    function NewBitmap(Filename: string; AIsUtf8: boolean): TBGRABitmap; overload; override;
    function NewBitmap(Filename: string; AIsUtf8: boolean; AOptions: TBGRALoadingOptions): TBGRABitmap; overload; override;
    function NewBitmap(AFPImage: TFPCustomImage): TBGRABitmap; overload; override;
    function NewReference: TBGRABitmap; override;
    function GetUnique: TBGRABitmap; override;
    function Duplicate(DuplicateProperties: Boolean = False): TBGRABitmap; overload; override;
    function Duplicate(DuplicateProperties, DuplicateXorMask: Boolean) : TBGRABitmap; overload; override;
    function GetPart(const ARect: TRect): TBGRABitmap; override;
    function CreateBrushTexture(ABrushStyle: TBrushStyle; APatternColor, ABackgroundColor: TBGRAPixel;
                AWidth: integer = 8; AHeight: integer = 8; APenWidth: single = 1): TBGRABitmap; override;
    function Resample(newWidth, newHeight: integer;
      mode: TResampleMode = rmFineResample): TBGRABitmap; override;
    function RotateCW: TBGRABitmap; override;
    function RotateCCW: TBGRABitmap; override;
    function RotateUD: TBGRABitmap; override;
    function FilterSmartZoom3(Option: TMedianOption): TBGRABitmap; override;
    function FilterMedian(Option: TMedianOption): TBGRABitmap; override;
    function FilterSmooth: TBGRABitmap; override;
    function FilterSharpen(Amount: single = 1): TBGRABitmap; overload; override;
    function FilterSharpen(ABounds: TRect; Amount: single = 1): TBGRABitmap; overload; override;
    function FilterContour(AGammaCorrection: boolean = false): TBGRABitmap; override;
    function FilterPixelate(pixelSize: integer; useResample: boolean; filter: TResampleFilter = rfLinear): TBGRABitmap; override;
    function FilterBlurRadial(radius: single; blurType: TRadialBlurType): TBGRABitmap; overload; override;
    function FilterBlurRadial(const ABounds: TRect; radius: single; blurType: TRadialBlurType): TBGRABitmap; overload; override;
    function FilterBlurRadial(radiusX, radiusY: single; blurType: TRadialBlurType): TBGRABitmap; overload; override;
    function FilterBlurRadial(const ABounds: TRect; radiusX, radiusY: single; blurType: TRadialBlurType): TBGRABitmap; overload; override;
    function FilterBlurMotion(distance: single; angle: single; oriented: boolean): TBGRABitmap; overload; override;
    function FilterBlurMotion(const ABounds: TRect; distance: single; angle: single; oriented: boolean): TBGRABitmap; overload; override;
    function FilterCustomBlur(mask: TCustomUniversalBitmap): TBGRABitmap; overload; override;
    function FilterCustomBlur(const ABounds: TRect; mask: TCustomUniversalBitmap): TBGRABitmap; overload; override;
    function FilterEmboss(angle: single; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGRABitmap; overload; override;
    function FilterEmboss(angle: single; ABounds: TRect; AStrength: integer= 64; AOptions: TEmbossOptions = []): TBGRABitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean): TBGRABitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel): TBGRABitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel; var Offset: TPoint): TBGRABitmap; overload; override;
    function FilterGrayscale: TBGRABitmap; overload; override;
    function FilterGrayscale(ABounds: TRect): TBGRABitmap; overload; override;
    function FilterNormalize(eachChannel: boolean = True): TBGRABitmap; overload; override;
    function FilterNormalize(ABounds: TRect; eachChannel: boolean = True): TBGRABitmap; overload; override;
    function FilterRotate(origin: TPointF; angle: single; correctBlur: boolean = false): TBGRABitmap; override;
    function FilterAffine(AMatrix: TAffineMatrix; correctBlur: boolean = false): TBGRABitmap; override;
    function FilterSphere: TBGRABitmap; override;
    function FilterTwirl(ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRABitmap; overload; override;
    function FilterTwirl(ABounds: TRect; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRABitmap; overload; override;
    function FilterCylinder: TBGRABitmap; override;
    function FilterPlane: TBGRABitmap; override;
  end;

// draw a bitmap from pure data
procedure BGRABitmapDraw(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
  VerticalFlip: boolean; AWidth, AHeight: integer; Opaque: boolean);
  
{ Replace the content of the variable Destination with the variable
  Temp and frees previous object contained in Destination.
  
  This function is useful as a shortcut for :
 
  var
    temp: TBGRABitmap;
  begin
    ...
    temp := someBmp.Filter... as TBGRABitmap;
    someBmp.Free;
    someBmp := temp;
  end;
  
  which becomes :
  
  begin
    ...
    BGRAReplace(someBmp, someBmp.Filter... );
  end;
}
procedure BGRAReplace(var Destination: TBGRABitmap; Temp: TObject);

implementation

uses BGRAReadBMP, BGRAReadBmpMioMap, BGRAReadGif,
  BGRAReadIco, BGRAReadJpeg, BGRAReadLzp, BGRAReadPCX,
  BGRAReadPng, BGRAWritePNG, BGRAReadPSD, BGRAReadTGA, BGRAReadXPM,
  BGRAWriteLzp, BGRAReadWebP, BGRAWriteWebP;

var
  tempBmp: TBGRABitmap;

procedure BGRABitmapDraw(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
  VerticalFlip: boolean; AWidth, AHeight: integer; Opaque: boolean);
var
  LineOrder: TRawImageLineOrder;
begin
  if tempBmp = nil then
    tempBmp := TBGRABitmap.Create;
  if VerticalFlip then
    LineOrder := riloBottomToTop
  else
    LineOrder := riloTopToBottom;
  if Opaque then
    tempBmp.DataDrawOpaque(ACanvas, Rect, AData, LineOrder, AWidth, AHeight)
  else
    tempBmp.DataDrawTransparent(ACanvas, Rect, AData, LineOrder, AWidth, AHeight);
end;

procedure BGRAReplace(var Destination: TBGRABitmap; Temp: TObject);
begin
  Destination.Free;
  Destination := Temp as TBGRABitmap;
end;

{ TBGRABitmap }

function TBGRABitmap.NewBitmap: TBGRABitmap;
begin
  Result:=inherited NewBitmap as TBGRABitmap;
end;

function TBGRABitmap.NewBitmap(AWidth, AHeight: integer): TBGRABitmap;
begin
  Result:=inherited NewBitmap(AWidth, AHeight) as TBGRABitmap;
end;

function TBGRABitmap.NewBitmap(AWidth, AHeight: integer; const Color: TBGRAPixel
  ): TBGRABitmap;
begin
  Result:=inherited NewBitmap(AWidth, AHeight, Color) as TBGRABitmap;
end;

function TBGRABitmap.NewBitmap(AWidth, AHeight: integer; AColor: Pointer
  ): TBGRABitmap;
begin
  Result:=inherited NewBitmap(AWidth, AHeight, AColor) as TBGRABitmap;
end;

function TBGRABitmap.NewBitmap(Filename: string): TBGRABitmap;
begin
  Result:=inherited NewBitmap(Filename) as TBGRABitmap;
end;

function TBGRABitmap.NewBitmap(Filename: string; AIsUtf8: boolean): TBGRABitmap;
begin
  Result:=inherited NewBitmap(Filename, AIsUtf8) as TBGRABitmap;
end;

function TBGRABitmap.NewBitmap(Filename: string; AIsUtf8: boolean;
  AOptions: TBGRALoadingOptions): TBGRABitmap;
begin
  Result:=inherited NewBitmap(Filename, AIsUtf8, AOptions) as TBGRABitmap;
end;

function TBGRABitmap.NewBitmap(AFPImage: TFPCustomImage): TBGRABitmap;
begin
  Result:=inherited NewBitmap(AFPImage) as TBGRABitmap;
end;

function TBGRABitmap.NewReference: TBGRABitmap;
begin
  Result:=inherited NewReference as TBGRABitmap;
end;

function TBGRABitmap.GetUnique: TBGRABitmap;
begin
  Result:=inherited GetUnique as TBGRABitmap;
end;

function TBGRABitmap.Duplicate(DuplicateProperties: Boolean): TBGRABitmap;
begin
  Result:=inherited Duplicate(DuplicateProperties) as TBGRABitmap;
end;

function TBGRABitmap.Duplicate(DuplicateProperties, DuplicateXorMask: Boolean
  ): TBGRABitmap;
begin
  Result:=inherited Duplicate(DuplicateProperties, DuplicateXorMask) as TBGRABitmap;
end;

function TBGRABitmap.GetPart(const ARect: TRect): TBGRABitmap;
begin
  Result:=inherited GetPart(ARect) as TBGRABitmap;
end;

function TBGRABitmap.CreateBrushTexture(ABrushStyle: TBrushStyle;
  APatternColor, ABackgroundColor: TBGRAPixel; AWidth: integer;
  AHeight: integer; APenWidth: single): TBGRABitmap;
begin
  Result:=inherited CreateBrushTexture(ABrushStyle, APatternColor,
    ABackgroundColor, AWidth, AHeight, APenWidth) as TBGRABitmap;
end;

function TBGRABitmap.Resample(newWidth, newHeight: integer; mode: TResampleMode
  ): TBGRABitmap;
begin
  Result:=inherited Resample(newWidth, newHeight, mode) as TBGRABitmap;
end;

function TBGRABitmap.RotateCW: TBGRABitmap;
begin
  Result:=inherited RotateCW as TBGRABitmap;
end;

function TBGRABitmap.RotateCCW: TBGRABitmap;
begin
  Result:=inherited RotateCCW as TBGRABitmap;
end;

function TBGRABitmap.RotateUD: TBGRABitmap;
begin
  Result:=inherited RotateUD as TBGRABitmap;
end;

function TBGRABitmap.FilterSmartZoom3(Option: TMedianOption): TBGRABitmap;
begin
  Result:=inherited FilterSmartZoom3(Option) as TBGRABitmap;
end;

function TBGRABitmap.FilterMedian(Option: TMedianOption): TBGRABitmap;
begin
  Result:=inherited FilterMedian(Option) as TBGRABitmap;
end;

function TBGRABitmap.FilterSmooth: TBGRABitmap;
begin
  Result:=inherited FilterSmooth as TBGRABitmap;
end;

function TBGRABitmap.FilterSharpen(Amount: single): TBGRABitmap;
begin
  Result:=inherited FilterSharpen(Amount) as TBGRABitmap;
end;

function TBGRABitmap.FilterSharpen(ABounds: TRect; Amount: single): TBGRABitmap;
begin
  Result:=inherited FilterSharpen(ABounds, Amount) as TBGRABitmap;
end;

function TBGRABitmap.FilterContour(AGammaCorrection: boolean = false): TBGRABitmap;
begin
  Result:=inherited FilterContour(AGammaCorrection) as TBGRABitmap;
end;

function TBGRABitmap.FilterPixelate(pixelSize: integer; useResample: boolean;
  filter: TResampleFilter): TBGRABitmap;
begin
  Result:=inherited FilterPixelate(pixelSize, useResample, filter) as TBGRABitmap;
end;

function TBGRABitmap.FilterBlurRadial(radius: single; blurType: TRadialBlurType
  ): TBGRABitmap;
begin
  Result:=inherited FilterBlurRadial(radius, blurType) as TBGRABitmap;
end;

function TBGRABitmap.FilterBlurRadial(const ABounds: TRect; radius: single;
  blurType: TRadialBlurType): TBGRABitmap;
begin
  Result:=inherited FilterBlurRadial(ABounds, radius, blurType) as TBGRABitmap;
end;

function TBGRABitmap.FilterBlurRadial(radiusX, radiusY: single;
  blurType: TRadialBlurType): TBGRABitmap;
begin
  Result:=inherited FilterBlurRadial(radiusX, radiusY, blurType) as TBGRABitmap;
end;

function TBGRABitmap.FilterBlurRadial(const ABounds: TRect; radiusX, radiusY: single;
  blurType: TRadialBlurType): TBGRABitmap;
begin
  Result:=inherited FilterBlurRadial(ABounds, radiusX, radiusY, blurType) as TBGRABitmap;
end;

function TBGRABitmap.FilterBlurMotion(distance: single; angle: single;
  oriented: boolean): TBGRABitmap;
begin
  Result:=inherited FilterBlurMotion(distance, angle, oriented) as TBGRABitmap;
end;

function TBGRABitmap.FilterBlurMotion(const ABounds: TRect; distance: single;
  angle: single; oriented: boolean): TBGRABitmap;
begin
  Result:=inherited FilterBlurMotion(ABounds, distance, angle, oriented) as TBGRABitmap;
end;

function TBGRABitmap.FilterCustomBlur(mask: TCustomUniversalBitmap
  ): TBGRABitmap;
begin
  Result:=inherited FilterCustomBlur(mask) as TBGRABitmap;
end;

function TBGRABitmap.FilterCustomBlur(const ABounds: TRect;
  mask: TCustomUniversalBitmap): TBGRABitmap;
begin
  Result:=inherited FilterCustomBlur(ABounds, mask) as TBGRABitmap;
end;

function TBGRABitmap.FilterEmboss(angle: single; AStrength: integer;
  AOptions: TEmbossOptions): TBGRABitmap;
begin
  Result:=inherited FilterEmboss(angle, AStrength, AOptions) as TBGRABitmap;
end;

function TBGRABitmap.FilterEmboss(angle: single; ABounds: TRect;
  AStrength: integer; AOptions: TEmbossOptions): TBGRABitmap;
begin
  Result:=inherited FilterEmboss(angle, ABounds, AStrength, AOptions) as TBGRABitmap;
end;

function TBGRABitmap.FilterEmbossHighlight(FillSelection: boolean): TBGRABitmap;
begin
  Result:=inherited FilterEmbossHighlight(FillSelection) as TBGRABitmap;
end;

function TBGRABitmap.FilterEmbossHighlight(FillSelection: boolean;
  BorderColor: TBGRAPixel): TBGRABitmap;
begin
  Result:=inherited FilterEmbossHighlight(FillSelection, BorderColor) as TBGRABitmap;
end;

function TBGRABitmap.FilterEmbossHighlight(FillSelection: boolean;
  BorderColor: TBGRAPixel; var Offset: TPoint): TBGRABitmap;
begin
  Result:=inherited FilterEmbossHighlight(FillSelection, BorderColor, Offset) as TBGRABitmap;
end;

function TBGRABitmap.FilterGrayscale: TBGRABitmap;
begin
  Result:=inherited FilterGrayscale as TBGRABitmap;
end;

function TBGRABitmap.FilterGrayscale(ABounds: TRect): TBGRABitmap;
begin
  Result:=inherited FilterGrayscale(ABounds) as TBGRABitmap;
end;

function TBGRABitmap.FilterNormalize(eachChannel: boolean): TBGRABitmap;
begin
  Result:=inherited FilterNormalize(eachChannel) as TBGRABitmap;
end;

function TBGRABitmap.FilterNormalize(ABounds: TRect; eachChannel: boolean
  ): TBGRABitmap;
begin
  Result:=inherited FilterNormalize(ABounds, eachChannel) as TBGRABitmap;
end;

function TBGRABitmap.FilterRotate(origin: TPointF; angle: single;
  correctBlur: boolean): TBGRABitmap;
begin
  Result:=inherited FilterRotate(origin, angle, correctBlur) as TBGRABitmap;
end;

function TBGRABitmap.FilterAffine(AMatrix: TAffineMatrix; correctBlur: boolean
  ): TBGRABitmap;
begin
  Result:=inherited FilterAffine(AMatrix, correctBlur) as TBGRABitmap;
end;

function TBGRABitmap.FilterSphere: TBGRABitmap;
begin
  Result:=inherited FilterSphere as TBGRABitmap;
end;

function TBGRABitmap.FilterTwirl(ACenter: TPoint; ARadius: Single;
  ATurn: Single; AExponent: Single): TBGRABitmap;
begin
  Result:=inherited FilterTwirl(ACenter, ARadius, ATurn, AExponent) as TBGRABitmap;
end;

function TBGRABitmap.FilterTwirl(ABounds: TRect; ACenter: TPoint;
  ARadius: Single; ATurn: Single; AExponent: Single): TBGRABitmap;
begin
  Result:=inherited FilterTwirl(ABounds, ACenter, ARadius, ATurn, AExponent) as TBGRABitmap;
end;

function TBGRABitmap.FilterCylinder: TBGRABitmap;
begin
  Result:=inherited FilterCylinder as TBGRABitmap;
end;

function TBGRABitmap.FilterPlane: TBGRABitmap;
begin
  Result:=inherited FilterPlane as TBGRABitmap;
end;

initialization

  //this variable is assigned to access appropriate functions
  //depending on the platform
  BGRABitmapFactory := TBGRABitmap;

finalization

  tempBmp.Free;

end.

