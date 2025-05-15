// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ @abstract(Provides the 32-bit RGBA bitmap type adapted to your system.)

  Pixels are of TBGRAPixel type based on sRGB colorspace with transparency (TBGRAPixelColorspace).

  To use, you generally need to add BGRABitmapTypes as well to the **uses** clause.

  Channels can be in the following orders:
  - B G R A (recommended for Windows, required for fpGUI)
  - R G B A (recommended for Gtk and MacOS)

  **Bitmap units**: BGRABitmap, ExpandedBitmap, BGRAGrayscaleMask, LinearRGBABitmap, WordXYZABitmap, XYZABitmap.
}
unit BGRABitmap;

{ Provides:
  - Drawing primitives
  - Resample
  - Reference counter
  - Drawing on LCL canvas
  - Loading and saving images }

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

{ Compiler directives are used to include the best version according
  to the platform }

uses
  BGRAClasses, BGRABitmapTypes, FPImage, SysUtils, BGRAUnits,
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
  		    {$IFDEF LCLgtk3}
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
  {* Import version for fpGUI }
  TBGRABitmap = class(TBGRAfpGUIBitmap)
{$ELSE}
    {$IFDEF BGRABITMAP_USE_LCL}
      {$IFDEF LCLwin32}
        {* Import version for Windows }

        { TBGRABitmap }

        TBGRABitmap = class(TBGRAWinBitmap)
      {$ELSE}
        {$IFDEF LCLgtk}
        {* Import version for Linux GTK }
        TBGRABitmap = class(TBGRAGtkBitmap)
        {$ELSE}
         {$IFDEF LCLgtk2}
        {* Import version for Linux GTK2 }
        TBGRABitmap = class(TBGRAGtkBitmap)
         {$ELSE}
          {$IFDEF LCLgtk3}
          {* Import version for Linux GTK2 }
          TBGRABitmap = class(TBGRAGtkBitmap)
            {$ELSE}
            {$IF defined(LCLqt) or defined(LCLqt5)}
        {* Import version for Qt }
        TBGRABitmap = class(TBGRAQtBitmap)
            {$ELSE}
              {$IFDEF DARWIN}
        {* Import version for MacOS }
        TBGRABitmap = class(TBGRAMacBitmap)
              {$ELSE}
        {* Import version for other systems }{ Cross-platform 32-bit RGBA image compatible with Lazarus Component Library }
        TBGRABitmap = class(TBGRALCLBitmap)
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
         {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ELSE}
      {$IFDEF BGRABITMAP_USE_MSEGUI}
        {* Import version for MSEgui }
        TBGRABitmap = class(TBGRAMSEguiBitmap)
      {$ELSE}
        {* Import version without a GUI }{ Standalone cross-platform 32-bit RGBA image }
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
    function GetPart(const ARect: TRect; ACopyProperties: Boolean=False): TBGRABitmap; override;
    function CreateBrushTexture(ABrushStyle: TBrushStyle; APatternColor, ABackgroundColor: TBGRAPixel;
                AWidth: integer = 8; AHeight: integer = 8; APenWidth: single = 1): TBGRABitmap; override;
    function Resample(newWidth, newHeight: integer;
      mode: TResampleMode = rmFineResample; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function Resample(NewWidth, NewHeight: Single; ASizeUnit: TCSSUnit;
      mode: TResampleMode = rmFineResample; ACopyProperties: Boolean=True): TBGRABitmap; overload; override;
    function RotateCW(ACopyProperties: Boolean=False): TBGRABitmap; override;
    function RotateCCW(ACopyProperties: Boolean=False): TBGRABitmap; override;
    function RotateUD(ACopyProperties: Boolean=False): TBGRABitmap; override;
    {$IFNDEF BGRABITMAP_CORE}
    function FilterSmartZoom3(Option: TMedianOption; ACopyProperties: Boolean=False): TBGRABitmap; override;
    function FilterMedian(Option: TMedianOption; ACopyProperties: Boolean=False): TBGRABitmap; override;
    function FilterSmooth(ACopyProperties: Boolean=False): TBGRABitmap; override;
    function FilterSharpen(Amount: single = 1; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterSharpen(ABounds: TRect; Amount: single = 1; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterContour(AGammaCorrection: boolean = false; ACopyProperties: Boolean=False): TBGRABitmap; override;
    function FilterPixelate(pixelSize: integer; useResample: boolean; filter: TResampleFilter = rfLinear; ACopyProperties: Boolean=False): TBGRABitmap; override;
    function FilterBlurRadial(radius: single; blurType: TRadialBlurType; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterBlurRadial(const ABounds: TRect; radius: single; blurType: TRadialBlurType; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterBlurRadial(radiusX, radiusY: single; blurType: TRadialBlurType; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterBlurRadial(const ABounds: TRect; radiusX, radiusY: single; blurType: TRadialBlurType; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterBlurMotion(distance: single; angle: single; oriented: boolean; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterBlurMotion(const ABounds: TRect; distance: single; angle: single; oriented: boolean; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterCustomBlur(mask: TCustomUniversalBitmap; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterCustomBlur(const ABounds: TRect; mask: TCustomUniversalBitmap; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterEmboss(angle: single; AStrength: integer= 64; AOptions: TEmbossOptions = []; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterEmboss(angle: single; ABounds: TRect; AStrength: integer= 64; AOptions: TEmbossOptions = []; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterEmbossHighlight(FillSelection: boolean; BorderColor: TBGRAPixel; var Offset: TPoint; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterGrayscale(ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterGrayscale(ABounds: TRect; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterNormalize(eachChannel: boolean = True; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterNormalize(ABounds: TRect; eachChannel: boolean = True; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterRotate(origin: TPointF; angle: single; correctBlur: boolean = false; ACopyProperties: Boolean=False): TBGRABitmap; override;
    function FilterAffine(AMatrix: TAffineMatrix; correctBlur: boolean = false; ACopyProperties: Boolean=False): TBGRABitmap; override;
    function FilterSphere(ACopyProperties: Boolean=False): TBGRABitmap; override;
    function FilterTwirl(ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterTwirl(ABounds: TRect; ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3; ACopyProperties: Boolean=False): TBGRABitmap; overload; override;
    function FilterCylinder(ACopyProperties: Boolean=False): TBGRABitmap; override;
    function FilterPlane(ACopyProperties: Boolean=False): TBGRABitmap; override;
    {$ENDIF}
  end;

{* Draw a bitmap from pure data }
procedure BGRABitmapDraw(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
  VerticalFlip: boolean; AWidth, AHeight: integer; Opaque: boolean);
  
{* Replace the content of the variable Destination with the variable
  _Temp_ and frees previous object contained in _Destination_.
  
  This function is useful as a shortcut for :
  ```pascal
  var
    temp: TBGRABitmap;
  begin
    ...
    temp := someBmp.Filter... as TBGRABitmap;
    someBmp.Free;
    someBmp := temp;
  end;
  ```
  
  which becomes :
  ```pascal
  begin
    ...
    BGRAReplace(someBmp, someBmp.Filter... );
  end;
  ```
}
procedure BGRAReplace(var Destination: TBGRABitmap; Temp: TObject);

implementation

uses BGRAReadBMP, BGRAWriteBMP, BGRAReadPng, BGRAWritePNG
  {$IFNDEF BGRABITMAP_CORE},
  BGRAReadBmpMioMap, BGRAReadGif, BGRAReadIco, BGRAReadPSD, BGRAReadTGA, BGRAReadXPM,
  BGRAReadJpeg, BGRAWriteJpeg, BGRAReadLzp, BGRAWriteLzp, BGRAReadPCX, BGRAWritePCX,
  BGRAReadWebP, BGRAWriteWebP, BGRAReadAVIF, BGRAWriteAVIF
    {$IFDEF BGRABITMAP_EXTENDED_COLORSPACE}, BGRAReadTiff, BGRAWriteTiff{$ENDIF}
  {$ENDIF};

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

function TBGRABitmap.GetPart(const ARect: TRect; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited GetPart(ARect, ACopyProperties) as TBGRABitmap;
end;

function TBGRABitmap.CreateBrushTexture(ABrushStyle: TBrushStyle;
  APatternColor, ABackgroundColor: TBGRAPixel; AWidth: integer;
  AHeight: integer; APenWidth: single): TBGRABitmap;
begin
  Result:=inherited CreateBrushTexture(ABrushStyle, APatternColor,
    ABackgroundColor, AWidth, AHeight, APenWidth) as TBGRABitmap;
end;

function TBGRABitmap.Resample(newWidth, newHeight: integer; mode: TResampleMode; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited Resample(newWidth, newHeight, mode, ACopyProperties) as TBGRABitmap;
end;

function TBGRABitmap.Resample(NewWidth, NewHeight: Single; ASizeUnit: TCSSUnit;
  mode: TResampleMode; ACopyProperties: Boolean): TBGRABitmap;
begin
  Result:=inherited Resample(NewWidth, NewHeight, ASizeUnit, mode, ACopyProperties) as TBGRABitmap;
end;

function TBGRABitmap.RotateCW(ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited RotateCW(ACopyProperties) as TBGRABitmap;
end;

function TBGRABitmap.RotateCCW(ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited RotateCCW(ACopyProperties) as TBGRABitmap;
end;

function TBGRABitmap.RotateUD(ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited RotateUD(ACopyProperties) as TBGRABitmap;
end;

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterSmartZoom3(Option: TMedianOption; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterSmartZoom3(Option, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterMedian(Option: TMedianOption; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterMedian(Option, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterSmooth(ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterSmooth(ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterSharpen(Amount: single; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterSharpen(Amount, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterSharpen(ABounds: TRect; Amount: single; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterSharpen(ABounds, Amount, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterContour(AGammaCorrection: boolean = false; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterContour(AGammaCorrection, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterPixelate(pixelSize: integer; useResample: boolean;
  filter: TResampleFilter; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterPixelate(pixelSize, useResample, filter, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterBlurRadial(radius: single; blurType: TRadialBlurType; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterBlurRadial(radius, blurType, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterBlurRadial(const ABounds: TRect; radius: single;
  blurType: TRadialBlurType; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterBlurRadial(ABounds, radius, blurType, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterBlurRadial(radiusX, radiusY: single;
  blurType: TRadialBlurType; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterBlurRadial(radiusX, radiusY, blurType, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterBlurRadial(const ABounds: TRect; radiusX, radiusY: single;
  blurType: TRadialBlurType; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterBlurRadial(ABounds, radiusX, radiusY, blurType, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterBlurMotion(distance: single; angle: single;
  oriented: boolean; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterBlurMotion(distance, angle, oriented, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterBlurMotion(const ABounds: TRect; distance: single;
  angle: single; oriented: boolean; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterBlurMotion(ABounds, distance, angle, oriented, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterCustomBlur(mask: TCustomUniversalBitmap; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterCustomBlur(mask, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterCustomBlur(const ABounds: TRect;
  mask: TCustomUniversalBitmap; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterCustomBlur(ABounds, mask, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterEmboss(angle: single; AStrength: integer;
  AOptions: TEmbossOptions; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterEmboss(angle, AStrength, AOptions, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterEmboss(angle: single; ABounds: TRect;
  AStrength: integer; AOptions: TEmbossOptions; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterEmboss(angle, ABounds, AStrength, AOptions, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterEmbossHighlight(FillSelection: boolean; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterEmbossHighlight(FillSelection, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterEmbossHighlight(FillSelection: boolean;
  BorderColor: TBGRAPixel; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterEmbossHighlight(FillSelection, BorderColor, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterEmbossHighlight(FillSelection: boolean;
  BorderColor: TBGRAPixel; var Offset: TPoint; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterEmbossHighlight(FillSelection, BorderColor, Offset, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterGrayscale(ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterGrayscale(ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterGrayscale(ABounds: TRect; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterGrayscale(ABounds, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterNormalize(eachChannel: boolean; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterNormalize(eachChannel, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterNormalize(ABounds: TRect; eachChannel: boolean; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterNormalize(ABounds, eachChannel, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterRotate(origin: TPointF; angle: single;
  correctBlur: boolean; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterRotate(origin, angle, correctBlur, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterAffine(AMatrix: TAffineMatrix; correctBlur: boolean; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterAffine(AMatrix, correctBlur, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterSphere(ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterSphere(ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterTwirl(ACenter: TPoint; ARadius: Single;
  ATurn: Single; AExponent: Single; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterTwirl(ACenter, ARadius, ATurn, AExponent, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterTwirl(ABounds: TRect; ACenter: TPoint;
  ARadius: Single; ATurn: Single; AExponent: Single; ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterTwirl(ABounds, ACenter, ARadius, ATurn, AExponent, ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterCylinder(ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterCylinder(ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}function TBGRABitmap.FilterPlane(ACopyProperties: Boolean=False): TBGRABitmap;
begin
  Result:=inherited FilterPlane(ACopyProperties) as TBGRABitmap;
end;{$ENDIF}

initialization

  //this variable is assigned to access appropriate functions
  //depending on the platform
  BGRABitmapFactory := TBGRABitmap;

finalization

  tempBmp.Free;

end.

