// SPDX-License-Identifier: LGPL-3.0-linking-exception
{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}
type
  TBGRACustomBitmap = class;

{=== IBGRAScanner ===}
  {* Interface for a scanner. A scanner is like an image, but its content
     has no limit and it can be calculated on the fly. It is like a
     infinite readonly image.
   *
   * Note: it must not implement reference counting even if it is an interface
   *
   * ''TBGRACustomBitmap'' implements this interface and the content is repeated
     horizontally and vertically. There are also various classes
     in ''BGRAGradientScanner'' unit that generate gradients on the fly and
     in ''BGRATransform'' unit that provide geometrical transformations of images }
  IBGRAScanner = interface
    {** Move to the position (''X'',''Y'') for the next call to ''ScanNextPixel'' }
    procedure ScanMoveTo(X,Y: Integer);
    {** Scan the pixel at the current location and increments ''X'' }
    function ScanNextPixel: TBGRAPixel;
    function ScanNextExpandedPixel: TExpandedPixel;
    {** Scan at any location using floating point coordinates }
    function ScanAt(X,Y: Single): TBGRAPixel;
    function ScanAtExpanded(X,Y: Single): TExpandedPixel;
    {** Scan at any location using integer coordinates }
    function ScanAtInteger(X,Y: integer): TBGRAPixel;
    function ScanAtIntegerExpanded(X,Y: integer): TExpandedPixel;
    {** Copy a row of pixels from ''X'' to ''X''+''count''-1 to a specified destination
        ''pdest''. ''mode'' indicates how to combine with existing data }
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode);
    procedure ScanSkipPixels(ACount: integer);
    {** Returns True if the function ''ScanPutPixels'' is available. Otherwise
        you need to call ''ScanNextPixel'' and combine pixels for example
        with ''SetPixel'' }
    function IsScanPutPixelsDefined: boolean;
    {** Returns the corresponding OpenGL texture. The value is ''nil'' if no texture is associated. **}
    function GetTextureGL: IUnknown;
    function GetImageBoundsWithin(const ARect: TRect; Channel: TChannel = cAlpha; ANothingValue: Byte = 0): TRect; overload;
    function GetImageBoundsWithin(const ARect: TRect; Channels: TChannels; ANothingValue: Byte = 0): TRect; overload;
    function ProvidesScanline(ARect: TRect): boolean;
    function GetScanlineAt(X,Y: integer): PBGRAPixel;
    function GetScanCustomColorspace: TColorspaceAny;
    procedure ScanNextCustomChunk(var ACount: integer; out APixels: Pointer);
    procedure ScanNextMaskChunk(var ACount: integer; out AMask: PByteMask; out AStride: integer);
    function ScanAtIntegerMask(X,Y: integer): TByteMask;
    function ScanAtMask(X,Y: Single): TByteMask;
  end;

  {** A type of function of a scanner that returns the content at floating point coordinates }
  TScanAtFunction = function (X,Y: Single): TBGRAPixel of object;
  {** A type of function of a scanner that returns the content at integer coordinates }
  TScanAtIntegerFunction = function (X,Y: Integer): TBGRAPixel of object;
  {** A type of function of a scanner that returns the next pixel }
  TScanNextPixelFunction = function: TBGRAPixel of object;

  { TBGRACustomScanner }
  {* Base class for implementing ''IBGRAScanner'' interface }
  TBGRACustomScanner = class(IBGRAScanner)
  private
    FCurX,FCurY: integer;
    FOwnCur: boolean;
    FChunk: packed array[0..3] of TExpandedPixel;
  public
    function ScanAtInteger(X,Y: integer): TBGRAPixel; virtual;
    function ScanAtIntegerExpanded(X,Y: integer): TExpandedPixel; virtual;
    procedure ScanMoveTo(X,Y: Integer); virtual;
    function ScanNextPixel: TBGRAPixel; virtual;
    function ScanNextExpandedPixel: TExpandedPixel; virtual;
    function ScanAt(X,Y: Single): TBGRAPixel; virtual; abstract;
    function ScanAtExpanded(X,Y: Single): TExpandedPixel; virtual;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); virtual;
    procedure ScanSkipPixels(ACount: integer); virtual;
    function IsScanPutPixelsDefined: boolean; virtual;
    function GetTextureGL: IUnknown; virtual;
    function GetImageBoundsWithin(const ARect: TRect; Channel: TChannel = cAlpha; ANothingValue: Byte = 0): TRect; overload; virtual;
    function GetImageBoundsWithin(const ARect: TRect; Channels: TChannels; ANothingValue: Byte = 0): TRect; overload; virtual;
    function ProvidesScanline({%H-}ARect: TRect): boolean; virtual;
    function GetScanlineAt({%H-}X,{%H-}Y: integer): PBGRAPixel; virtual;
    function GetScanCustomColorspace: TColorspaceAny; virtual;
    procedure ScanNextCustomChunk(var ACount: integer; out APixels: Pointer); virtual;
    procedure ScanNextMaskChunk(var ACount: integer; out AMask: PByteMask; out AStride: integer); virtual;
    function ScanAtIntegerMask(X,Y: integer): TByteMask; virtual;
    function ScanAtMask(X,Y: Single): TByteMask; virtual;
  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
  end;
{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}
{ TBGRACustomScanner }
{ The abstract class record the position so that a derived class
  need only to redefine ScanAt }

function TBGRACustomScanner.ScanAtInteger(X, Y: integer): TBGRAPixel;
begin
  result := ScanAt(X,Y);
end;

function TBGRACustomScanner.ScanAtIntegerExpanded(X, Y: integer
  ): TExpandedPixel;
begin
  result := GammaExpansion(ScanAtInteger(X,Y));
end;

procedure TBGRACustomScanner.ScanMoveTo(X, Y: Integer);
begin
  FCurX := X;
  FCurY := Y;
  FOwnCur := true;
end;

{ Call ScanAt to determine pixel value }
function TBGRACustomScanner.ScanNextPixel: TBGRAPixel;
begin
  result := ScanAt(FCurX,FCurY);
  Inc(FCurX);
end;

function TBGRACustomScanner.ScanNextExpandedPixel: TExpandedPixel;
begin
  result := GammaExpansion(ScanNextPixel);
end;

function TBGRACustomScanner.ScanAtExpanded(X, Y: Single): TExpandedPixel;
begin
  result := GammaExpansion(ScanAt(X,Y));
end;

{$hints off}
procedure TBGRACustomScanner.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
begin
  //do nothing
end;

procedure TBGRACustomScanner.ScanSkipPixels(ACount: integer);
begin
  if FOwnCur then inc(FCurX, ACount)
  else
  begin
    while ACount > 0 do
    begin
      ScanNextPixel;
      dec(ACount);
    end;
  end;
end;

{$hints on}

function TBGRACustomScanner.IsScanPutPixelsDefined: boolean;
begin
  result := false;
end;

function TBGRACustomScanner.GetTextureGL: IUnknown;
begin
  result := nil;
end;

function TBGRACustomScanner.GetImageBoundsWithin(const ARect: TRect;
  Channel: TChannel; ANothingValue: Byte): TRect;
begin
  result := InternalGetImageBoundsWithin(nil,self,ARect,[Channel],ANothingValue);
end;

function TBGRACustomScanner.GetImageBoundsWithin(const ARect: TRect;
  Channels: TChannels; ANothingValue: Byte): TRect;
begin
  result := InternalGetImageBoundsWithin(nil,self,ARect,Channels,ANothingValue);
end;

function TBGRACustomScanner.ProvidesScanline(ARect: TRect): boolean;
begin
  result := false;
end;

function TBGRACustomScanner.GetScanlineAt(X, Y: integer): PBGRAPixel;
begin
  result := nil;
end;

function TBGRACustomScanner.GetScanCustomColorspace: TColorspaceAny;
begin
  result := TExpandedPixelColorspace;
end;

procedure TBGRACustomScanner.ScanNextCustomChunk(var ACount: integer; out
  APixels: Pointer);
var
  i: Integer;
  p: PExpandedPixel;
begin
  if ACount > length(FChunk) then ACount := length(FChunk);
  p := @FChunk[low(FChunk)];
  APixels := p;
  for i := ACount-1 downto 0 do
  begin
    p^ := ScanNextExpandedPixel;
    inc(p);
  end;
end;

procedure TBGRACustomScanner.ScanNextMaskChunk(var ACount: integer; out AMask: PByteMask; out AStride: integer);
var
  cs: TColorspaceAny;
  pPixels: Pointer;
begin
  cs := GetScanCustomColorspace;
  if cs = TBGRAPixelColorspace then
  begin
    ScanNextCustomChunk(ACount, pPixels);
    AMask := @PBGRAPixel(pPixels)^.green;
    AStride := sizeof(TBGRAPixel);
  end else
  if cs = TByteMaskColorspace then
  begin
    ScanNextCustomChunk(ACount, pPixels);
    AMask := PByteMask(pPixels);
    AStride := sizeof(TByteMask);
  end else
    raise exception.Create('This scanner does not provide a mask.');
end;

function TBGRACustomScanner.ScanAtIntegerMask(X,Y: integer): TByteMask;
begin
  if GetScanCustomColorspace = TBGRAPixelColorspace then
    result := ScanAtInteger(X,Y).green
  else
    result := ScanAtMask(X,Y);
end;

function TBGRACustomScanner.ScanAtMask(X,Y: Single): TByteMask;
begin
  if GetScanCustomColorspace = TBGRAPixelColorspace then
    result := ScanAt(X,Y).green
  else
    raise exception.Create('This scanner does not provide a mask.');
end;

{ Interface gateway }
function TBGRACustomScanner.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := longint(E_NOINTERFACE);
end;

{ There is no automatic reference counting, but it is compulsory to define these functions }
function TBGRACustomScanner._AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TBGRACustomScanner._Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

{$ENDIF}