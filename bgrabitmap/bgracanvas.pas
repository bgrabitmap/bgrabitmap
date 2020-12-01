// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRACanvas;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

uses
  BGRAClasses, SysUtils, BGRAGraphics, FPImage, BGRABitmapTypes
  {$IFDEF BGRABITMAP_USE_FPCANVAS}, FPCanvas{$ENDIF};

type

  { TBGRAColoredObject }

  TBGRAColoredObject = class
  private
    function GetColor: TColor;
    function GetOpacity: Byte;
    procedure SetColor(const AValue: TColor);
    procedure SetOpacity(const AValue: Byte);
  public
    BGRAColor: TBGRAPixel;
    procedure Assign(Source: TObject); virtual;
    property Color: TColor read GetColor write SetColor;
    property Opacity: Byte read GetOpacity write SetOpacity;
  end;

  { TBGRAPen }

  TBGRAPen = class(TBGRAColoredObject)
  private
    FPenMode: TPenMode;
    function GetActualColor: TBGRAPixel;
    function GetActualDrawMode: TDrawMode;
    function GetActualWidth: integer;
    function GetCustomPenStyle: TBGRAPenStyle;
    function GetInvisible: boolean;
    function GetPenStyle: TPenStyle;
    procedure SetCustomPenStyle(const AValue: TBGRAPenStyle);
    procedure SetPenMode(AValue: TPenMode);
    procedure SetPenStyle(const AValue: TPenStyle);
  protected
    FCustomPenStyle:  TBGRAPenStyle;
    FPenStyle: TPenStyle;
  public
    Width: Integer;
    EndCap: TPenEndCap;
    JoinStyle: TPenJoinStyle;
    constructor Create;
    procedure Assign(Source: TObject); override;
    procedure GetUniversalBrush(out ABrush: TUniversalBrush);
    property Style: TPenStyle read GetPenStyle Write SetPenStyle;
    property Mode: TPenMode read FPenMode write SetPenMode;
    property CustomStyle: TBGRAPenStyle read GetCustomPenStyle write SetCustomPenStyle;
    property ActualWidth: integer read GetActualWidth;
    property ActualColor: TBGRAPixel read GetActualColor;
    property ActualDrawMode: TDrawMode read GetActualDrawMode;
    property Invisible: boolean read GetInvisible;
  end;

  { TBGRABrush }

  TBGRABrush = class(TBGRAColoredObject)
  private
    function GetActualColor: TBGRAPixel;
    function GetActualDrawMode: TDrawMode;
    function GetInvisible: boolean;
    procedure SetBackColor(const AValue: TBGRAPixel);
    procedure SetBrushStyle(const AValue: TBrushStyle);
    procedure SetTexture(AValue: IBGRAScanner);
  protected
    FStyle, FStyleBeforeTexture: TBrushStyle;
    FBackColor: TBGRAPixel;
    FTexture: IBGRAScanner;
    FInternalBitmap: TBGRACustomBitmap;
    FInternalBitmapColor: TBGRAPixel;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TObject); override;
    procedure GetUniversalBrush(ABitmapPrototype: TBGRACustomBitmap; out ABrush: TUniversalBrush);
    function BuildTexture(Prototype: TBGRACustomBitmap): IBGRAScanner;
    property Style: TBrushStyle read FStyle write SetBrushStyle;
    property BackColor: TBGRAPixel read FBackColor write SetBackColor;
    property ActualColor: TBGRAPixel read GetActualColor;
    property ActualDrawMode: TDrawMode read GetActualDrawMode;
    property Invisible: boolean read GetInvisible;
    property Texture: IBGRAScanner read FTexture write SetTexture;
  end;

  { TBGRAFont }

  TBGRAFont = class(TBGRAColoredObject)
  private
    function GetAntialiasing: Boolean;
    procedure SetAntialiasing(const AValue: Boolean);
  public
    Name:   string;
    Height: Integer;
    Style:  TFontStyles;
    Quality : TBGRAFontQuality;
    Orientation:  integer;
    Texture:      IBGRAScanner;
    constructor Create;
    procedure Assign(Source: TObject); override;
    property Antialiasing: Boolean read GetAntialiasing write SetAntialiasing;

  end;

  { TBGRACanvas }

  TBGRACanvas = class
    procedure SetBrush(const AValue: TBGRABrush);
    procedure SetPen(const AValue: TBGRAPen);
    function GetPixelColor(X, Y: Integer): TColor;
    procedure SetPixelColor(X, Y: Integer; const AValue: TColor);
  private
    function GetClipping: Boolean;
    function GetClipRect: TRect;
    function GetExpandedPixel(X, Y: Integer): TExpandedPixel;
    function GetFPPixelColor(X, Y: Integer): TFPColor;
    function GetHeight: integer;
    function GetWidth: integer;
    procedure SetClipping(const AValue: Boolean);
    procedure SetClipRect(const AValue: TRect);
    procedure SetExpandedPixel(X, Y: Integer; const AValue: TExpandedPixel);
    procedure SetFont(const AValue: TBGRAFont);
    procedure SetFPPixelColor(X, Y: Integer; const AValue: TFPColor);
    function ComputeEllipseC(x1, y1, x2, y2: integer; out cx,cy,rx,ry: single): boolean;
    function CheckRectangle(var x1, y1, x2, y2: integer; out tx,ty: integer): boolean;

  protected
    FBitmap: TBGRACustomBitmap;
    FBrush: TBGRABrush;
    FPen: TBGRAPen;
    FPenPos: TPoint;
    FFont : TBGRAFont;
    FInactiveClipRect: TRect;
    FClippingOn: Boolean;
    procedure ApplyPenStyle;
    procedure ApplyFont;
    function NoPen: boolean;
    function NoBrush: boolean;
  public
    AntialiasingMode: TAntialiasingMode;
    FillMode : TFillMode;
    TextStyle : TTextStyle;
    DrawFontBackground : boolean;
    constructor Create(ABitmap: TBGRACustomBitmap);
    destructor Destroy; override;
    procedure MoveTo(x,y: integer); overload;
    procedure MoveTo(p: TPoint); overload;
    procedure LineTo(x,y: integer); overload;
    procedure LineTo(p: TPoint); overload;
    procedure Line(x1,y1,x2,y2: integer); overload;
    procedure Line(p1,p2: TPoint); overload;
    procedure Arc(x1,y1,x2,y2,sx,sy,ex,ey: integer);
    procedure Arc(x1,y1,x2,y2,StartDeg16,LengthDeg16: integer);
    procedure Arc65536(x1,y1,x2,y2: integer; start65536,end65536: word; Options: TArcOptions);
    procedure Chord(x1,y1,x2,y2,sx,sy,ex,ey: integer);
    procedure Chord(x1,y1,x2,y2,StartDeg16,LengthDeg16: integer);
    procedure Pie(x1,y1,x2,y2,sx,sy,ex,ey: integer);
    procedure Pie(x1,y1,x2,y2,StartDeg16,LengthDeg16: integer);
    procedure RadialPie(x1,y1,x2,y2,StartDeg16,LengthDeg16: integer);
    procedure Ellipse(x1,y1,x2,y2: integer);
    procedure Ellipse(const bounds: TRect);
    procedure Rectangle(x1,y1,x2,y2: integer; Filled: Boolean = True);
    procedure Rectangle(const bounds: TRect; Filled: Boolean = True);
    procedure Frame(x1,y1,x2,y2: integer);
    procedure Frame(const bounds: TRect);
    procedure RoundRect(x1,y1,x2,y2: integer; dx,dy: integer);
    procedure RoundRect(const bounds: TRect; dx,dy: integer);
    procedure EllipseC(x,y,rx,ry: integer);
    procedure FillRect(x1,y1,x2,y2: integer);
    procedure FillRect(const bounds: TRect);
    procedure FrameRect(x1,y1,x2,y2: integer; width: integer = 1);
    procedure FrameRect(const bounds: TRect; width: integer = 1);
    procedure Frame3D(var bounds: TRect; width: integer; Style: TGraphicsBevelCut); overload;
    procedure Frame3D(var bounds: TRect; width: integer; Style: TGraphicsBevelCut; LightColor: TBGRAPixel; ShadowColor: TBGRAPixel); overload;
    procedure GradientFill(ARect: TRect; AStart, AStop: TColor;
      ADirection: TGradientDirection; GammaCorrection: Boolean = false);
    procedure FloodFill(X, Y: Integer; FillColor: TColor; FillStyle: TFillStyle);
    procedure FloodFill(X, Y: Integer; FillColor: TBGRAPixel; FillStyle: TFillStyle);
    procedure FloodFill(X, Y: Integer);
    procedure Polygon(const APoints: array of TPoint);
    procedure Polygon(const Points: array of TPoint;
                      Winding: Boolean;
                      StartIndex: Integer = 0;
                      NumPts: Integer = -1);
    procedure Polygon(Points: PPoint; NumPts: Integer;
                      Winding: boolean = False);
    procedure PolygonF(const APoints: array of TPointF);
    procedure PolygonF(const APoints: array of TPointF; Winding: Boolean; FillOnly: Boolean = False);
    procedure Polyline(const APoints: array of TPoint);
    procedure Polyline(const Points: array of TPoint;
                      StartIndex: Integer;
                      NumPts: Integer = -1);
    procedure Polyline(Points: PPoint; NumPts: Integer);
    procedure PolylineF(const APoints: array of TPointF);
    procedure PolyBezier(Points: PPoint; NumPts: Integer;
                         Filled: boolean = False;
                         Continuous: boolean = False);
    procedure PolyBezier(const Points: array of TPoint;
                         Filled: boolean = False;
                         Continuous: boolean = False);
    procedure Draw(X,Y: Integer; SrcBitmap: TBGRACustomBitmap); overload;
    procedure Draw(X,Y: Integer; SrcBitmap: TBitmap); overload;
    procedure CopyRect(X,Y: Integer; SrcBitmap: TBGRACustomBitmap; SrcRect: TRect);
    procedure StretchDraw(DestRect: TRect; SrcBitmap: TBGRACustomBitmap; HorizFlip: Boolean = false; VertFlip: Boolean = false);
    procedure DrawFocusRect(bounds: TRect);
    procedure CopyRect(Dest: TRect; SrcBmp: TBGRACustomBitmap;
                       Source: TRect); virtual;

    procedure TextOut(X,Y: Integer; const Text: String);
    procedure TextRect(const ARect: TRect; X, Y: integer; const Text: string);
    procedure TextRect(ARect: TRect; X, Y: integer; const Text: string;
                       const Style: TTextStyle);
    function TextExtent(const Text: string): TSize;
    function TextHeight(const Text: string): Integer;
    function TextWidth(const Text: string): Integer;

    property Pen: TBGRAPen read FPen write SetPen;
    property PenPos : TPoint read FPenPos write FPenPos;
    property Brush: TBGRABrush read FBrush write SetBrush;
    property Font: TBGRAFont read FFont write SetFont;
    property Pixels[X,Y: Integer]: TColor read GetPixelColor write SetPixelColor;
    property GammaExpandedPixels[X,Y: Integer]: TExpandedPixel read GetExpandedPixel write SetExpandedPixel;
    property Colors[X,Y: Integer]: TFPColor read GetFPPixelColor write SetFPPixelColor;
    property Height: integer read GetHeight;
    property Width : integer read GetWidth;
    property ClipRect: TRect read GetClipRect write SetClipRect;
    property Clipping: Boolean read GetClipping write SetClipping;
  end;

implementation

uses BGRAPen, BGRAPath, BGRAPolygon, BGRAPolygonAliased, Math, BGRABlend;

{ TBGRAFont }

function TBGRAFont.GetAntialiasing: Boolean;
begin
  result := Quality <> fqSystem;
end;

procedure TBGRAFont.SetAntialiasing(const AValue: Boolean);
begin
  if AValue = Antialiasing then exit;
  if AValue then
    Quality := fqFineAntialiasing
  else
    Quality := fqSystem;
end;

constructor TBGRAFont.Create;
begin
  Name := 'default';
  Height := 12;
  Style := [];
  Antialiasing := False;
  Orientation := 0;
  Texture := nil;
  BGRAColor := BGRABlack;
end;

procedure TBGRAFont.Assign(Source: TObject);
var sf: TBGRAFont;
    f: TFont;
    {$IFDEF BGRABITMAP_USE_FPCANVAS}cf: TFPCustomFont;{$ENDIF}
begin
  if Source is TFont then
  begin
    f := TFont(Source);
    Color := f.Color;
    Opacity := 255;
    Style := f.Style;
    Name := f.Name;
    Orientation := f.Orientation;
    if f.Height= 0 then
      Height := 16 else
       Height := f.Height;
  end else
  if Source is TBGRAFont then
  begin
    sf := Source as TBGRAFont;
    Name := sf.Name;
    Height := sf.Height;
    Style := sf.Style;
    Quality := sf.Quality;
    Orientation := sf.Orientation;
    Texture := sf.Texture;
  end
  {$IFDEF BGRABITMAP_USE_FPCANVAS}else
  if Source is TFPCustomFont then
  begin
    cf := Source as TFPCustomFont;
    Color := FPColorToTColor(cf.FPColor);
    Style := [];
    if cf.Bold then include(Style, fsBold);
    if cf.Italic then include(Style, fsItalic);
    if cf.Underline then include(Style, fsUnderline);
{$IF FPC_FULLVERSION>=20602} //changed in 2.6.2 and 2.7    
    if cf.StrikeThrough then include(Style, fsStrikeOut);
{$ELSE}
    if cf.StrikeTrough then include(Style, fsStrikeOut);
{$ENDIF}
    Name := cf.Name;
    //Orientation := cf.Orientation;
    if cf.Size = 0 then
      Height := 16 else
       Height := round(cf.Size*1.8);
  end{$ENDIF};
  inherited Assign(Source);
end;

{ TBGRABrush }

function TBGRABrush.GetActualColor: TBGRAPixel;
begin
  if (Style = bsClear) or (Opacity = 0) then
    result := BGRAPixelTransparent
  else
    result := BGRAColor;
end;

function TBGRABrush.GetActualDrawMode: TDrawMode;
begin
  result := dmDrawWithTransparency;
end;

function TBGRABrush.GetInvisible: boolean;
begin
  result := (texture = nil) and ((style = bsClear) or ((style= bsSolid) and (bgracolor.alpha = 0))
    or ((bgracolor.alpha = 0) and (BackColor.alpha = 0)));
end;

procedure TBGRABrush.SetBackColor(const AValue: TBGRAPixel);
begin
  if FBackColor=AValue then exit;
  FBackColor:=AValue;
  FreeAndNil(FInternalBitmap);
end;

procedure TBGRABrush.SetBrushStyle(const AValue: TBrushStyle);
begin
  if FStyle=AValue then exit;
  FStyle:=AValue;
  if FStyle <> bsImage then FTexture := nil;
  FreeAndNil(FInternalBitmap);
end;

procedure TBGRABrush.SetTexture(AValue: IBGRAScanner);
begin
  if FTexture=AValue then Exit;
  FTexture:=AValue;
  if Assigned(AValue) then
  begin
    if FStyle <> bsImage then
    begin
      FStyleBeforeTexture:= FStyle;
      FStyle:= bsImage;
    end;
  end else
  begin
    if FStyle = bsImage then
      FStyle := FStyleBeforeTexture;
  end;
end;

constructor TBGRABrush.Create;
begin
  BGRAColor := BGRAWhite;
  FInternalBitmap := nil;
  FInternalBitmapColor := BGRAPixelTransparent;
  FStyle := bsSolid;
  FStyleBeforeTexture:= Style;
  FTexture := nil;
  FBackColor := BGRAPixelTransparent;
end;

destructor TBGRABrush.Destroy;
begin
  Texture := nil;
  FInternalBitmap.Free;
  inherited Destroy;
end;

procedure TBGRABrush.Assign(Source: TObject);
var sb: TBGRABrush;
    b: TBrush;
begin
  if Source is TBGRABrush then
  begin
    sb := Source as TBGRABrush;
    Texture := sb.Texture;
    BackColor := sb.BackColor;
    Style := sb.Style;
  end else
  if Source is TBrush then
  begin
    b := Source as TBrush;
    Color := b.Color;
    Opacity := 255;
    Style := b.Style;
  end;
  inherited Assign(Source);
end;

procedure TBGRABrush.GetUniversalBrush(ABitmapPrototype: TBGRACustomBitmap; out ABrush: TUniversalBrush);
var
  tex: IBGRAScanner;
  c: TBGRAPixel;
begin
  tex := BuildTexture(ABitmapPrototype);
  if tex <> nil then
    BGRAScannerBrush(ABrush, tex, ActualDrawMode)
  else
  begin
    c := ActualColor;
    BGRASolidBrushIndirect(ABrush, @c, ActualDrawMode);
  end;
end;

function TBGRABrush.BuildTexture(Prototype: TBGRACustomBitmap): IBGRAScanner;
begin
  //user-defined texture
  if Texture <> nil then
    result := texture
  else
  begin
    //free pattern if color has changed
    if (FInternalBitmap <> nil) and (FInternalBitmapColor <> BGRAColor) then
      FreeAndNil(FInternalBitmap);

    //styles that do not have pattern
    if Style in[bsSolid,bsClear] then
      result := nil
    else
    begin
      //create pattern if needed
      if FInternalBitmap = nil then
      begin
        FInternalBitmap := CreateBrushTexture(Prototype, Style, BGRAColor,BackColor);
        FInternalBitmapColor := BGRAColor;
      end;
      result := FInternalBitmap;
    end;
  end;
end;

{ TBGRAPen }

function TBGRAPen.GetActualColor: TBGRAPixel;
begin
  if (Style = psClear) or (Opacity = 0) then
    result := BGRAPixelTransparent
  else
  begin
    case Mode of
    pmBlack: result := BGRABlack;
    pmWhite: result := BGRAWhite;
    pmNop: result := BGRAPixelTransparent;
    pmNot: result := BGRA(255,255,255,0);
    pmCopy: result := BGRAColor;
    pmNotCopy: result := BGRA(not BGRAColor.red, not BGRAColor.green, not BGRAColor.blue, BGRAColor.alpha);
    pmMerge, pmNotMerge, pmMask, pmNotMask, pmXor: result := BGRA(BGRAColor.red, BGRAColor.green, BGRAColor.blue, 0);
    pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmNotXor: result := BGRA(not BGRAColor.red, not BGRAColor.green, not BGRAColor.blue, 0);
    else
      raise exception.Create('Unhandled pen mode');
    end;
  end;
end;

function TBGRAPen.GetActualDrawMode: TDrawMode;
begin
  case Mode of
    pmBlack, pmWhite, pmNop, pmCopy, pmNotCopy:
      result := dmDrawWithTransparency;
    else
      {pmNot, pmXor, pmNotXor and others}
      result := dmXor;
  end;
end;

function TBGRAPen.GetActualWidth: integer;
begin
  if width < 1 then result := 1 else
    result := Width;
end;

function TBGRAPen.GetCustomPenStyle: TBGRAPenStyle;
begin
  result := DuplicatePenStyle(FCustomPenStyle);
end;

function TBGRAPen.GetInvisible: boolean;
var b: TUniversalBrush;
begin
  GetUniversalBrush(b);
  result := b.DoesNothing;
end;

function TBGRAPen.GetPenStyle: TPenStyle;
begin
  Result:= FPenStyle;
end;

procedure TBGRAPen.SetCustomPenStyle(const AValue: TBGRAPenStyle);
begin
  FCustomPenStyle := DuplicatePenStyle(AValue);
  FPenStyle:= BGRAToPenStyle(AValue);
end;

procedure TBGRAPen.SetPenMode(AValue: TPenMode);
begin
  if FPenMode=AValue then Exit;
  FPenMode:=AValue;
end;

procedure TBGRAPen.SetPenStyle(const AValue: TPenStyle);
begin
  if AValue = psPattern then exit;
  FCustomPenStyle := PenStyleToBGRA(AValue);
  FPenStyle := AValue;
end;

constructor TBGRAPen.Create;
begin
  Width := 1;
  EndCap := pecRound;
  JoinStyle := pjsRound;
  Style := psSolid;
  BGRAColor := BGRABlack;
  Mode := pmCopy;
end;

procedure TBGRAPen.Assign(Source: TObject);
var sp: TBGRAPen;
    p: TPen;
begin
  if Source is TBGRAPen then
  begin
    sp := Source as TBGRAPen;
    Mode := sp.Mode;
    Width := sp.Width;
    EndCap := sp.EndCap;
    JoinStyle := sp.JoinStyle;
    Style := sp.Style;
    CustomStyle := sp.CustomStyle;
  end else
  if Source is TPen then
  begin
    p := Source as TPen;
    Mode := p.Mode;
    Width := p.Width;
    EndCap := p.EndCap;
    JoinStyle := p.JoinStyle;
    Style := p.Style;
    Color := p.Color;
    Opacity := 255;
  end;
  inherited Assign(Source);
end;

type
  PBGRAPenBrushFixedData = ^TBGRAPenBrushFixedData;
  TBGRAPenBrushFixedData = record
    BGRA: TBGRAPixel;
    NotResult: boolean;
  end;

procedure BGRAPenSkipPixels({%H-}AFixedData: Pointer;
  AContextData: PUniBrushContext; {%H-}AAlpha: Word; ACount: integer);
begin
  inc(PBGRAPixel(AContextData^.Dest), ACount);
end;

procedure BGRAPenMergePixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PBGRAPixel;
  merged: TBGRAPixel;
begin
  if AAlpha <= $80 then
  begin
    inc(PBGRAPixel(AContextData^.Dest), ACount);
    exit;
  end;
  pDest := PBGRAPixel(AContextData^.Dest);
  with PBGRAPenBrushFixedData(AFixedData)^ do
  begin
    while ACount > 0 do
    begin
      merged.red := pDest^.red or PBGRAPenBrushFixedData(AFixedData)^.BGRA.red;
      merged.green := pDest^.green or PBGRAPenBrushFixedData(AFixedData)^.BGRA.green;
      merged.blue := pDest^.blue or PBGRAPenBrushFixedData(AFixedData)^.BGRA.blue;
      if NotResult then
      begin
        merged.red := not merged.red;
        merged.green := not merged.green;
        merged.blue := not merged.blue;
      end;
      if AAlpha >= $ff7f then
        pDest^ := merged else
        pDest^ := GammaCompression(MergeBGRA(GammaExpansion(pDest^), not AAlpha,
                                             GammaExpansion(merged), AAlpha));
      inc(pDest);
      dec(ACount);
    end;
  end;
  PBGRAPixel(AContextData^.Dest) := pDest;
end;

procedure BGRAPenMaskPixels(AFixedData: Pointer;
  AContextData: PUniBrushContext; AAlpha: Word; ACount: integer);
var
  pDest: PBGRAPixel;
  merged: TBGRAPixel;
begin
  if AAlpha <= $80 then
  begin
    inc(PBGRAPixel(AContextData^.Dest), ACount);
    exit;
  end;
  pDest := PBGRAPixel(AContextData^.Dest);
  with PBGRAPenBrushFixedData(AFixedData)^ do
  begin
    while ACount > 0 do
    begin
      merged.red := pDest^.red and PBGRAPenBrushFixedData(AFixedData)^.BGRA.red;
      merged.green := pDest^.green and PBGRAPenBrushFixedData(AFixedData)^.BGRA.green;
      merged.blue := pDest^.blue and PBGRAPenBrushFixedData(AFixedData)^.BGRA.blue;
      if NotResult then
      begin
        merged.red := not merged.red;
        merged.green := not merged.green;
        merged.blue := not merged.blue;
      end;
      if AAlpha >= $ff7f then
        pDest^ := merged else
        pDest^ := GammaCompression(MergeBGRA(GammaExpansion(pDest^), not AAlpha,
                                             GammaExpansion(merged), AAlpha));
      inc(pDest);
      dec(ACount);
    end;
  end;
  PBGRAPixel(AContextData^.Dest) := pDest;
end;

procedure TBGRAPen.GetUniversalBrush(out ABrush: TUniversalBrush);
var c: TBGRAPixel;
begin
  if Opacity = 0 then
  begin
    TBGRACustomBitmap.IdleBrush(ABrush);
    exit;
  end;
  c := ActualColor;
  case Mode of
    pmMerge, pmNotMerge, pmMergeNotPen, pmMaskPenNot: //or-based
      begin
        ABrush.Colorspace:= TBGRAPixelColorspace;
        ABrush.InternalInitContext:= nil;
        PBGRAPenBrushFixedData(@ABrush.FixedData)^.BGRA := c;
        if Mode in [pmNotMerge, pmMaskPenNot] then
        begin
          ABrush.DoesNothing := false;
          PBGRAPenBrushFixedData(@ABrush.FixedData)^.NotResult:= true;
          ABrush.InternalPutNextPixels:= @BGRAPenMergePixels;
        end else
        begin
          ABrush.DoesNothing:= (c.red = 0) and (c.green = 0) and (c.blue = 0);
          PBGRAPenBrushFixedData(@ABrush.FixedData)^.NotResult:= false;
          if ABrush.DoesNothing then
            ABrush.InternalPutNextPixels:= @BGRAPenSkipPixels
          else
            ABrush.InternalPutNextPixels:= @BGRAPenMergePixels;
        end;
      end;
    pmMask, pmNotMask, pmMaskNotPen, pmMergePenNot: //and-based
      begin
        ABrush.Colorspace:= TBGRAPixelColorspace;
        ABrush.InternalInitContext:= nil;
        PBGRAPenBrushFixedData(@ABrush.FixedData)^.BGRA := c;
        if Mode in [pmNotMask, pmMergePenNot] then
        begin
          ABrush.DoesNothing := false;
          PBGRAPenBrushFixedData(@ABrush.FixedData)^.NotResult:= true;
          ABrush.InternalPutNextPixels:= @BGRAPenMaskPixels;
        end else
        begin
          ABrush.DoesNothing:= (c.red = 255) and (c.green = 255) and (c.blue = 255);
          PBGRAPenBrushFixedData(@ABrush.FixedData)^.NotResult:= false;
          if ABrush.DoesNothing then
            ABrush.InternalPutNextPixels:= @BGRAPenSkipPixels
          else
            ABrush.InternalPutNextPixels:= @BGRAPenMaskPixels;
        end;
      end;
    else //draw-based and xor-based
      BGRASolidBrushIndirect(ABrush, @c, ActualDrawMode);
  end;
end;

{ TBGRAColoredObject }

function TBGRAColoredObject.GetColor: TColor;
begin
  result := BGRAToColor(BGRAColor);
end;

function TBGRAColoredObject.GetOpacity: Byte;
begin
  result := BGRAColor.alpha;
end;

procedure TBGRAColoredObject.SetColor(const AValue: TColor);
begin
  BGRAColor := ColorToBGRA(AValue,BGRAColor.alpha);
end;

procedure TBGRAColoredObject.SetOpacity(const AValue: Byte);
begin
  BGRAColor.alpha := AValue;
end;

procedure TBGRAColoredObject.Assign(Source: TObject);
var so: TBGRAColoredObject;
begin
  if Source is TBGRAColoredObject then
  begin
    so := Source as TBGRAColoredObject;
    BGRAColor := so.BGRAColor;
  end;
end;

{ TBGRACanvas }

procedure TBGRACanvas.SetBrush(const AValue: TBGRABrush);
begin
  if FBrush=AValue then exit;
  FBrush.Assign(AValue);
end;

procedure TBGRACanvas.SetPen(const AValue: TBGRAPen);
begin
  if FPen=AValue then exit;
  FPen.Assign(AValue);
end;

function TBGRACanvas.GetPixelColor(X, Y: Integer): TColor;
begin
  result := BGRAToColor(FBitmap.GetPixel(x,y));
end;

procedure TBGRACanvas.SetPixelColor(X, Y: Integer; const AValue: TColor);
begin
  FBitmap.SetPixel(x,y,ColorToBGRA(AValue));
end;

function TBGRACanvas.GetClipping: Boolean;
begin
  result := FClippingOn;
end;

function TBGRACanvas.GetClipRect: TRect;
begin
  if not Clipping then
    result := FInactiveClipRect else
    result := FBitmap.ClipRect;
end;

function TBGRACanvas.GetExpandedPixel(X, Y: Integer): TExpandedPixel;
begin
  result := GammaExpansion(FBitmap.GetPixel(X,Y));
end;

function TBGRACanvas.GetFPPixelColor(X, Y: Integer): TFPColor;
begin
  result := BGRAToFPColor(FBitmap.GetPixel(x,y));
end;

function TBGRACanvas.GetHeight: integer;
begin
  result := FBitmap.Height;
end;

function TBGRACanvas.GetWidth: integer;
begin
  result := FBitmap.Width;
end;

procedure TBGRACanvas.SetClipping(const AValue: Boolean);
begin
  FClippingOn := AValue;
  if not AValue then FBitmap.NoClip else
    FBitmap.ClipRect := FInactiveClipRect;
end;

procedure TBGRACanvas.SetClipRect(const AValue: TRect);
begin
  FInactiveClipRect := AValue;
  if FClippingOn then
  begin
    FBitmap.ClipRect := AValue;
    FInactiveClipRect := FBitmap.ClipRect;
  end;
end;

procedure TBGRACanvas.SetExpandedPixel(X, Y: Integer;
  const AValue: TExpandedPixel);
begin
  FBitmap.SetPixel(x,y,GammaCompression(AValue));
end;

procedure TBGRACanvas.SetFont(const AValue: TBGRAFont);
begin
  if FFont=AValue then exit;
  FFont.Assign(AValue);
end;

procedure TBGRACanvas.SetFPPixelColor(X, Y: Integer; const AValue: TFPColor);
begin
  FBitmap.SetPixel(x,y,FPColorToBGRA(AValue));
end;

function TBGRACanvas.ComputeEllipseC(x1, y1, x2, y2: integer; out cx, cy, rx,
  ry: single): boolean;
begin
  cx := (x1+x2-1)/2;
  cy := (y1+y2-1)/2;
  rx := abs((x2-x1)/2);
  ry := abs((y2-y1)/2);
  result := (rx<>0) and (ry<>0);
end;

function TBGRACanvas.CheckRectangle(var x1, y1, x2, y2: integer; out tx, ty: integer
  ): boolean;
var
  temp: integer;
begin
  if x1 > x2 then
  begin
    temp := x1;
    x1 := x2;
    x2 := temp;
  end;
  if y1 > y2 then
  begin
    temp := y1;
    y1 := y2;
    y2 := temp;
  end;
  tx := x2-x1;
  ty := y2-y1;
  result := (tx<>0) and (ty <>0);
end;

procedure TBGRACanvas.ApplyPenStyle;
var
  TempPenStyle: TBGRAPenStyle;
  i: Integer;
begin
  FBitmap.JoinStyle := FPen.JoinStyle;
  FBitmap.LineCap := FPen.EndCap;
  if FPen.Width = 1 then
  begin
    SetLength(TempPenStyle, length(FPen.CustomStyle));
    for i := 0 to high(TempPenStyle) do
      TempPenStyle[i] := FPen.CustomStyle[i]*4;
    FBitmap.CustomPenStyle := TempPenStyle;
  end else
    FBitmap.CustomPenStyle := FPen.CustomStyle;
end;

procedure TBGRACanvas.ApplyFont;
begin
  FBitmap.FontName := Font.Name;
  FBitmap.FontHeight := -Font.Height;
  FBitmap.FontStyle := Font.Style;
  FBitmap.FontQuality := Font.Quality;
  FBitmap.FontOrientation := Font.Orientation;
end;

function TBGRACanvas.NoPen: boolean;
begin
  result := Pen.Invisible;
end;

function TBGRACanvas.NoBrush: boolean;
begin
  result := Brush.Invisible;
end;

constructor TBGRACanvas.Create(ABitmap: TBGRACustomBitmap);
begin
  FBitmap := ABitmap;
  AntialiasingMode := amOn;
  FPen := TBGRAPen.Create;
  FPenPos := Point(0,0);
  FFont := TBGRAFont.Create;
  FBrush := TBGRABrush.Create;
  FClippingOn := False;
  FInactiveClipRect := FBitmap.ClipRect;
  FillMode := fmWinding;
  DrawFontBackground := True;
end;

destructor TBGRACanvas.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  FFont.Free;
end;

procedure TBGRACanvas.MoveTo(x, y: integer);
begin
  MoveTo(Point(x,y));
end;

procedure TBGRACanvas.MoveTo(p: TPoint);
begin
  FPenPos := p;
end;

procedure TBGRACanvas.LineTo(x, y: integer);
var pts: array of TPointF;
  b: TUniversalBrush;
begin
  Pen.GetUniversalBrush(b);
  if not b.DoesNothing then
  begin
    //1 pixel-wide solid pen is rendered with pixel line
    if (Pen.Style = psSolid) and (Pen.ActualWidth = 1) then
    begin
      if AntialiasingMode = amOff then
        FBitmap.DrawLine(FPenPos.x,FPenPos.y,x,y, b, False)
      else
        FBitmap.DrawLineAntialias(FPenPos.x,FPenPos.y,x,y, b, False);
    end else
    begin
      ApplyPenStyle;
      if AntialiasingMode = amOff then
      begin
        pts := FBitmap.ComputeWidePolyline([PointF(FPenPos.x,FPenPos.y),PointF(x,y)],Pen.ActualWidth);
        FBitmap.FillPoly(pts, b);
      end else
        FBitmap.DrawLineAntialias(FPenPos.x,FPenPos.y,x,y, b, Pen.ActualWidth);
    end;
  end;
  MoveTo(x,y);
end;

procedure TBGRACanvas.LineTo(p: TPoint);
begin
  LineTo(p.x,p.y);
end;

procedure TBGRACanvas.Line(x1, y1, x2, y2: integer);
begin
  MoveTo(x1,y1);
  LineTo(x2,y2);
end;

procedure TBGRACanvas.Line(p1, p2: TPoint);
begin
  MoveTo(p1);
  LineTo(p2);
end;

procedure TBGRACanvas.Arc(x1, y1, x2, y2, sx, sy, ex, ey: integer);
var
  angle1,angle2: word;
  cx,cy,rx,ry: single;
begin
  if not ComputeEllipseC(x1,y1,x2,y2,cx,cy,rx,ry) then exit;
  angle1 := round(arctan2(-(sy-cy)/ry,(sx-cx)/rx)*65536/(2*Pi));
  angle2 := round(arctan2(-(ey-cy)/ry,(ex-cx)/rx)*65536/(2*Pi));
  Arc65536(x1,y1,x2,y2,angle1, angle2, []);
end;

procedure TBGRACanvas.Arc(x1, y1, x2, y2, StartDeg16, LengthDeg16: integer);
begin
  if LengthDeg16 > 360*16 then LengthDeg16 := 360*16;
  Arc65536(x1,y1,x2,y2,StartDeg16*512 div 45, (StartDeg16+LengthDeg16)*512 div 45, []);
end;

procedure TBGRACanvas.Arc65536(x1, y1, x2, y2: integer; start65536, end65536: word; Options: TArcOptions);
var cx,cy,rx,ry,w: single;
    arcPts,penPts: array of TPointF;
    multi: TBGRAMultishapeFiller;
    tex: IBGRAScanner;
    hasFill, hasPen: Boolean;
    b: TUniversalBrush;
begin
  if NoPen and NoBrush then exit;
  if not ComputeEllipseC(x1,y1,x2,y2,cx,cy,rx,ry) then exit;

  DecF(rx, 0.50);
  DecF(ry, 0.50);
  w := Pen.ActualWidth;

  if AntialiasingMode = amOff then
  begin
    if not NoPen and not Odd(Pen.ActualWidth) then
    begin
      DecF(rx, 0.01);
      DecF(ry, 0.01);
    end;
  end;

  if NoPen then
  begin
    DecF(cx, 0.5);
    DecF(cy, 0.5);
    DecF(rx, 0.2);
    DecF(ry, 0.2);
    if (rx<0) or (ry<0) then exit;
  end;

  arcPts := ComputeArc65536(cx,cy,rx,ry,start65536,end65536);
  if (aoPie in Options) and (start65536 <> end65536) then
  begin
    setlength(arcPts,length(arcPts)+1);
    arcPts[high(arcPts)] := PointF(cx,cy);
  end;

  hasFill := (aoFillPath in Options) and not NoBrush;
  hasPen := not NoPen;
  if hasPen then
  begin
    ApplyPenStyle;
    if (aoClosePath in Options) or (aoPie in Options) then
      penPts := FBitmap.ComputeWidePolygon(arcPts,w)
      else penPts := FBitmap.ComputeWidePolyline(arcPts,w);
  end;

  if hasPen and (Pen.ActualDrawMode <> dmDrawWithTransparency) then
  begin
    if hasFill then
    begin
      Brush.GetUniversalBrush(FBitmap, b);
      if AntialiasingMode <> amOff then
        FBitmap.FillPolyAntialias(arcPts, b)
        else FBitmap.FillPoly(arcPts, b);
    end;
    if hasPen then
    begin
      Pen.GetUniversalBrush(b);
      if AntialiasingMode <> amOff then
        FBitmap.FillPolyAntialias(penPts, b)
        else FBitmap.FillPoly(penPts, b);
    end;
  end else
  begin
    multi := TBGRAMultishapeFiller.Create;
    multi.Antialiasing := AntialiasingMode <> amOff;
    multi.FillMode := FillMode;
    multi.PolygonOrder := poLastOnTop;
    multi.AliasingIncludeBottomRight := True;
    if hasFill then
    begin
      tex := Brush.BuildTexture(FBitmap);
      if tex <> nil then
        multi.AddPolygon(arcPts, tex) else
        multi.AddPolygon(arcPts, Brush.ActualColor);
    end;
    if hasPen then
      multi.AddPolygon(penPts, Pen.ActualColor);
    multi.Draw(FBitmap, Brush.ActualDrawMode);
    multi.Free;
  end;
end;

procedure TBGRACanvas.Chord(x1, y1, x2, y2, sx, sy, ex, ey: integer);
var
  angle1,angle2: word;
  cx,cy,rx,ry: single;
begin
  if not ComputeEllipseC(x1,y1,x2,y2,cx,cy,rx,ry) then exit;
  angle1 := round(arctan2(-(sy-cy)/ry,(sx-cx)/rx)*65536/(2*Pi)) and 65535;
  angle2 := round(arctan2(-(ey-cy)/ry,(ex-cx)/rx)*65536/(2*Pi)) and 65535;
  Arc65536(x1,y1,x2,y2,angle1, angle2, [aoClosePath,aoFillPath]);
end;

procedure TBGRACanvas.Chord(x1, y1, x2, y2, StartDeg16, LengthDeg16: integer);
begin
  if LengthDeg16 > 360*16 then LengthDeg16 := 360*16;
  Arc65536(x1,y1,x2,y2,StartDeg16*512 div 45, (StartDeg16+LengthDeg16)*512 div 45,[aoClosePath,aoFillPath]);
end;

procedure TBGRACanvas.Pie(x1, y1, x2, y2, sx, sy, ex, ey: integer);
var
  angle1,angle2: word;
  cx,cy,rx,ry: single;
begin
  if not ComputeEllipseC(x1,y1,x2,y2,cx,cy,rx,ry) then exit;
  angle1 := round(arctan2(-(sy-cy)/ry,(sx-cx)/rx)*65536/(2*Pi)) and 65535;
  angle2 := round(arctan2(-(ey-cy)/ry,(ex-cx)/rx)*65536/(2*Pi)) and 65535;
  Arc65536(x1,y1,x2,y2,angle1, angle2, [aoPie,aoFillPath]);
end;

procedure TBGRACanvas.Pie(x1, y1, x2, y2, StartDeg16, LengthDeg16: integer);
begin
  if LengthDeg16 > 360*16 then LengthDeg16 := 360*16;
  Arc65536(x1,y1,x2,y2,StartDeg16*512 div 45, (StartDeg16+LengthDeg16)*512 div 45,[aoPie,aoFillPath]);
end;

procedure TBGRACanvas.RadialPie(x1, y1, x2, y2, StartDeg16, LengthDeg16: integer
  );
begin
  Pie(x1,y1,x2,y2,StartDeg16,LengthDeg16);
end;

procedure TBGRACanvas.Ellipse(x1, y1, x2, y2: integer);
var cx,cy,rx,ry,w: single;
    tex: IBGRAScanner;
    multi: TBGRAMultishapeFiller;
    pb, bb: TUniversalBrush;
begin
  if NoPen and NoBrush then exit;
  if (AntialiasingMode = amOff) and not NoPen and (Pen.Style = psSolid) and (Pen.ActualWidth = 1) then
  begin
    Pen.GetUniversalBrush(pb);
    Brush.GetUniversalBrush(FBitmap, bb);
    BGRARoundRectAliased(FBitmap, x1,y1,x2,y2, abs(x2-x1),abs(y2-y1), pb,bb, 65535);
    exit;
  end;
  if not ComputeEllipseC(x1,y1,x2,y2,cx,cy,rx,ry) then exit;
  w := Pen.ActualWidth;
  DecF(rx, 0.50);
  DecF(ry, 0.50);

  if AntialiasingMode = amOff then
  begin
    if not NoPen and not Odd(Pen.ActualWidth) then
    begin
      DecF(rx, 0.01);
      DecF(ry, 0.01);
    end;
  end;

  if NoPen then
  begin
    DecF(cx, 0.5);
    DecF(cy, 0.5);
    DecF(rx, 0.2);
    DecF(ry, 0.2);
    if (rx<0) or (ry<0) then exit;
  end;

  if not NoPen and (Pen.ActualDrawMode <> dmDrawWithTransparency) then
  begin
    if not NoBrush then
    begin
      Brush.GetUniversalBrush(FBitmap, bb);
      if AntialiasingMode <> amOff then
        FBitmap.FillEllipseAntialias(cx,cy,rx,ry, bb)
        else FBitmap.FillEllipseInRect(
               rect(round(cx-rx+0.5), round(cy-ry+0.5), round(cx+rx+0.5), round(cy+ry+0.5)), bb);
    end;
    if not NoPen then
    begin
      Pen.GetUniversalBrush(pb);
      ApplyPenStyle;
      if AntialiasingMode <> amOff then
        FBitmap.EllipseAntialias(cx,cy,rx,ry, pb, w)
      else FBitmap.Ellipse(cx,cy,rx,ry, pb, w);
    end;
  end else
  begin
    multi := TBGRAMultishapeFiller.Create;
    multi.Antialiasing := AntialiasingMode <> amOff;
    multi.PolygonOrder := poLastOnTop;
    multi.AliasingIncludeBottomRight := True;
    if not NoBrush then
    begin
      tex := Brush.BuildTexture(FBitmap);
      if tex <> nil then
        multi.AddEllipse(cx,cy,rx,ry,tex)
      else
        multi.AddEllipse(cx,cy,rx,ry,Brush.ActualColor);
    end;
    if not NoPen then
    begin
      if (Pen.Style = psSolid) and multi.Antialiasing then
        multi.AddEllipseBorder(cx,cy,rx,ry,w,Pen.ActualColor)
      else
      begin
        ApplyPenStyle;
        multi.AddPolygon(FBitmap.ComputeWidePolygon(ComputeEllipse(cx,cy,rx,ry),w),Pen.ActualColor);
      end;
    end;
    multi.Draw(FBitmap);
    multi.Free;
  end;
end;

procedure TBGRACanvas.Ellipse(const bounds: TRect);
begin
  Ellipse(bounds.left,bounds.top,bounds.right,bounds.Bottom);
end;

procedure TBGRACanvas.Rectangle(x1, y1, x2, y2: integer; Filled: Boolean = True);
var tx,ty: integer;
    tex: IBGRAScanner;
    multi: TBGRAMultishapeFiller;
    b: TUniversalBrush;
begin
  if NoBrush then Filled := false;
  if NoPen and not Filled then exit;
  if not CheckRectangle(x1,y1,x2,y2,tx,ty) then exit;

  if NoPen then
    FillRect(x1,y1,x2-1,y2-1) //one pixel
  else
  begin
    dec(x2);
    dec(y2);

    if not NoPen and (Pen.ActualDrawMode <> dmDrawWithTransparency) then
    begin
      if AntialiasingMode <> amOff then
      begin
        if Filled then FillRect(x1,y1,x2,y2);
        ApplyPenStyle;
        Pen.GetUniversalBrush(b);
        FBitmap.RectangleAntialias(x1,y1,x2,y2, b, Pen.ActualWidth);
      end else
        PolygonF([PointF(x1,y1), PointF(x2,y1), PointF(x2,y2), PointF(x1,y2)]);
    end else
    begin
      multi := TBGRAMultishapeFiller.Create;
      multi.Antialiasing := AntialiasingMode <> amOff;
      multi.PolygonOrder := poLastOnTop;
      if Filled then
      begin
        tex := Brush.BuildTexture(FBitmap);
        if tex <> nil then
          multi.AddRectangle(x1,y1,x2,y2,tex)
          else multi.AddRectangle(x1,y1,x2,y2,Brush.ActualColor);
      end;
      if not NoPen then
      begin
        ApplyPenStyle;
        if (Pen.Style = psSolid) and (Pen.JoinStyle = pjsMiter) and (FBitmap.JoinMiterLimit > 1.4142) then
          multi.AddRectangleBorder(x1,y1,x2,y2, Pen.ActualWidth, Pen.ActualColor)
        else
          multi.AddPolygon(FBitmap.ComputeWidePolygon(
            [PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)], Pen.ActualWidth),
            Pen.ActualColor);
      end;
      multi.Draw(FBitmap);
      multi.Free;
    end;
  end;
end;

procedure TBGRACanvas.Rectangle(const bounds: TRect; Filled: Boolean = True);
begin
  Rectangle(bounds.left,bounds.top,bounds.right,bounds.Bottom, Filled);
end;

procedure TBGRACanvas.Frame(x1, y1, x2, y2: integer);
begin
  Rectangle(x1,y1,x2,y2,False);
end;

procedure TBGRACanvas.Frame(const bounds: TRect);
begin
  Rectangle(bounds,False);
end;

procedure TBGRACanvas.RoundRect(x1, y1, x2, y2: integer; dx,dy: integer);
var tx,ty: integer;
    tex: IBGRAScanner;
    multi: TBGRAMultishapeFiller;
    x1f,y1f,x2f,y2f: single;
    pb, fb: TUniversalBrush;
begin
  if not CheckRectangle(x1,y1,x2,y2,tx,ty) then exit;
  if not NoPen and (AntialiasingMode = amOff) and (Pen.Style = psSolid) and (Pen.ActualWidth = 1) then
  begin
    ApplyPenStyle;
    Pen.GetUniversalBrush(pb);
    Brush.GetUniversalBrush(FBitmap, fb);
    FBitmap.RoundRect(x1,y1,x2,y2, dx,dy, pb, fb);
  end else
  begin
    dec(x2);
    dec(y2);
    if not NoPen and (Pen.ActualDrawMode <> dmDrawWithTransparency) then
      PolygonF(FBitmap.ComputeRoundRect(x1,y1,x2,y2, dx/2,dy/2, [])) else
    begin
      multi := TBGRAMultishapeFiller.Create;
      multi.Antialiasing := AntialiasingMode <> amOff;
      multi.PolygonOrder := poLastOnTop;
      if not NoBrush then
      begin
        if NoPen then
        begin
          x1f := x1-0.5;
          y1f := y1-0.5;
          x2f := x2+0.5;
          y2f := y2+0.5;
        end else
        begin
          x1f := x1;
          y1f := y1;
          x2f := x2;
          y2f := y2;
        end;
        tex := Brush.BuildTexture(FBitmap);
        if tex <> nil then
          multi.AddRoundRectangle(x1f,y1f,x2f,y2f,dx/2,dy/2,tex)
        else
          multi.AddRoundRectangle(x1f,y1f,x2f,y2f,dx/2,dy/2,Brush.ActualColor);
      end;
      if not NoPen then
      begin
        ApplyPenStyle;
        if Pen.Style = psSolid then
          multi.AddRoundRectangleBorder(x1,y1,x2,y2,dx/2,dy/2, Pen.ActualWidth ,Pen.ActualColor)
        else
          multi.AddPolygon(FBitmap.ComputeWidePolygon(ComputeRoundRect(x1,y1,x2,y2,dx/2,dy/2), Pen.ActualWidth),
                           Pen.ActualColor);
      end;
      multi.Draw(FBitmap);
      multi.Free;
    end;
  end;
end;

procedure TBGRACanvas.RoundRect(const bounds: TRect; dx,dy: integer);
begin
  RoundRect(bounds.left,bounds.top,bounds.right,bounds.Bottom,dx,dy);
end;

procedure TBGRACanvas.EllipseC(x, y, rx, ry: integer);
begin
  Ellipse (Rect(x-rx,y-ry,x+rx,y+ry));
end;

procedure TBGRACanvas.FillRect(x1, y1, x2, y2: integer);
var
  b: TUniversalBrush;
begin
  if NoBrush then exit;
  Brush.GetUniversalBrush(FBitmap, b);
  FBitmap.FillRect(x1,y1,x2,y2, b);
end;

procedure TBGRACanvas.FillRect(const bounds: TRect);
begin
  FillRect(bounds.left,bounds.top,bounds.right,bounds.Bottom);
end;

procedure TBGRACanvas.FrameRect(x1, y1, x2, y2: integer; width: integer = 1);
var
  Temp: integer;
  b: TUniversalBrush;
  ofs: single;
begin
  if (x1= x2) or (y1 =y2) or NoBrush then exit;
  if x1 > x2 then
  begin
    Temp := x1;
    x1 := x2;
    x2 := Temp;
  end;
  if y1 > y2 then
  begin
    Temp := y1;
    y1 := y2;
    y2 := Temp;
  end;
  dec(x2);
  dec(y2);

  Brush.GetUniversalBrush(FBitmap, b);
  FBitmap.PenStyle := psSolid;
  FBitmap.JoinStyle := pjsMiter;
  if not odd(width) and (AntialiasingMode = amOff) then
    ofs := 0.5 else ofs := 0;
  FBitmap.RectangleAntialias(x1+ofs, y1+ofs, x2+ofs, y2+ofs, b, width);
end;

procedure TBGRACanvas.FrameRect(const bounds: TRect; width: integer = 1);
begin
  FrameRect(bounds.left,bounds.top,bounds.right,bounds.Bottom,width);
end;

procedure TBGRACanvas.Frame3D(var bounds: TRect; width: integer;
  Style: TGraphicsBevelCut);
begin
  Frame3D(bounds,width,style,ColorToBGRA(clRgbBtnHighlight),ColorToBGRA(clRgbBtnShadow));
end;

procedure TBGRACanvas.Frame3D(var bounds: TRect; width: integer;
  Style: TGraphicsBevelCut; LightColor: TBGRAPixel; ShadowColor: TBGRAPixel);
var temp: TBGRAPixel;
    multi: TBGRAMultishapeFiller;
    color1,color2: TBGRAPixel;
begin
  if width <= 0 then exit;
  color1 := LightColor;
  color2 := ShadowColor;
  if Style = bvLowered then
  begin
    temp := color1;
    color1 := color2;
    color2 := temp;
  end;
  if Style in [bvLowered,bvRaised] then
  begin
    multi := TBGRAMultishapeFiller.Create;
    multi.Antialiasing := AntialiasingMode <> amOff;
    multi.AddPolygon([PointF(bounds.Left-0.5,bounds.Top-0.5),PointF(bounds.Right-0.5,bounds.Top-0.5),
                      PointF(bounds.Right-0.5-width,bounds.Top-0.5+width),PointF(bounds.Left-0.5+width,bounds.Top-0.5+width),
                      PointF(bounds.Left-0.5+width,bounds.Bottom-0.5-width),PointF(bounds.Left-0.5,bounds.Bottom-0.5)],color1);
    multi.AddPolygon([PointF(bounds.Right-0.5,bounds.Bottom-0.5),PointF(bounds.Left-0.5,bounds.Bottom-0.5),
                      PointF(bounds.Left-0.5+width,bounds.Bottom-0.5-width),PointF(bounds.Right-0.5-width,bounds.Bottom-0.5-width),
                      PointF(bounds.Right-0.5-width,bounds.Top-0.5+width),PointF(bounds.Right-0.5,bounds.Top-0.5)],color2);
    multi.Draw(FBitmap);
    multi.Free;
  end;
  bounds.Inflate(-width,-width);
end;

procedure TBGRACanvas.GradientFill(ARect: TRect; AStart, AStop: TColor;
  ADirection: TGradientDirection; GammaCorrection: Boolean = false);
var
  Count: Integer;

  procedure NotGammaCorrected;
  var
    c: TBGRAPixel;
    I: Integer;
    BDiff,GDiff,RDiff: Integer;
    BStop,BStart: Byte;
    GStop,GStart: Byte;
    RStop,RStart: Byte;
  begin
      RedGreenBlue(ColorToRGB(AStart), RStart, GStart, BStart);
      RedGreenBlue(ColorToRGB(AStop),  RStop,  GStop,  BStop);

      RDiff := RStop - RStart;
      GDiff := GStop - GStart;
      BDiff := BStop - BStart;

      for I := 0 to Count-1 do
      begin
        c := BGRA(RStart + (i * RDiff) div Count,
                  GStart + (i * GDiff) div Count,
                  BStart + (i * BDiff) div Count);

        if ADirection = gdHorizontal then
          FBitmap.SetVertLine(ARect.Left+I,ARect.Top,ARect.Bottom-1,c)
        else
          FBitmap.SetHorizLine(ARect.Left,ARect.Top+I,ARect.Right-1,c);
      end;
  end;

  procedure GammaCorrected;
  var
    ec: TExpandedPixel;
    c: TBGRAPixel;
    I: Integer;
    BDiff,GDiff,RDiff: Integer;
    CStart,CStop: TExpandedPixel;
  begin
    CStart := GammaExpansion(ColorToBGRA(AStart));
    CStop := GammaExpansion(ColorToBGRA(AStop));

    RDiff := CStop.red - CStart.red;
    GDiff := CStop.green - CStart.green;
    BDiff := CStop.blue - CStart.blue;

    for I := 0 to Count-1 do
    begin
      ec.red := CStart.red + (i * RDiff) div Count;
      ec.green := CStart.green + (i * GDiff) div Count;
      ec.blue := CStart.blue + (i * BDiff) div Count;
      ec.alpha := $ffff;
      c := GammaCompression(ec);

      if ADirection = gdHorizontal then
        FBitmap.SetVertLine(ARect.Left+I,ARect.Top,ARect.Bottom-1,c)
      else
        FBitmap.SetHorizLine(ARect.Left,ARect.Top+I,ARect.Right-1,c);
    end;
  end;

begin
  with ARect do
    if (Right <= Left) or (Bottom <= Top) then
      Exit;

  if ADirection = gdVertical then
    Count := ARect.Bottom - ARect.Top
  else
    Count := ARect.Right - ARect.Left;

  if GammaCorrection then
    GammaCorrected else
    NotGammaCorrected;
end;

procedure TBGRACanvas.FloodFill(X, Y: Integer; FillColor: TColor;
  FillStyle: TFillStyle);
begin
  FloodFill(X,Y,ColorToBGRA(FillColor,255),FillStyle);
end;

procedure TBGRACanvas.FloodFill(X, Y: Integer; FillColor: TBGRAPixel;
  FillStyle: TFillStyle);
var
  b: TUniversalBrush;
begin
  if FillStyle = fsSurface then
  begin
    if FBitmap.GetPixel(X,Y) <> FillColor then exit;
    Brush.GetUniversalBrush(FBitmap, b);
    FBitmap.FloodFill(X,Y, b, false);
  end;
   //fsBorder not handled
end;

procedure TBGRACanvas.FloodFill(X, Y: Integer);
begin
  FloodFill(X,Y,FBitmap.GetPixel(X,Y),fsSurface);
end;

procedure TBGRACanvas.Polygon(const APoints: array of TPoint);
begin
  Polygon(@APoints[0],length(APoints), FillMode = fmWinding);
end;

procedure TBGRACanvas.Polygon(const Points: array of TPoint; Winding: Boolean;
  StartIndex: Integer; NumPts: Integer);
begin
  if (StartIndex < 0) or (StartIndex >= length(Points)) then exit;
  if NumPts < 0 then NumPts := length(Points)-StartIndex;
  Polygon(@Points[StartIndex], NumPts, Winding);
end;

procedure TBGRACanvas.Polygon(Points: PPoint; NumPts: Integer; Winding: boolean);
var
  ptsF: array of TPointF;
  i: Integer;
  Ofs: TPointF;
begin
  if NoPen and NoBrush then exit;
  if NoPen then Ofs := PointF(-0.5,-0.5) else Ofs := PointF(0,0);
  setlength(ptsF, NumPts);
  for i := 0 to NumPts-1 do
  begin
    ptsF[i] := PointF(Points^.x,Points^.y)+Ofs;
    inc(Points);
  end;
  PolygonF(ptsF, Winding);
end;

procedure TBGRACanvas.PolygonF(const APoints: array of TPointF);
begin
  PolygonF(APoints, FillMode = fmWinding);
end;

procedure TBGRACanvas.PolygonF(const APoints: array of TPointF; Winding: Boolean; FillOnly: Boolean = False);
var
  hasPen, hasBrush: Boolean;
  b: TUniversalBrush;
  penPts: ArrayOfTPointF;
  multi: TBGRAMultishapeFiller;
  tex: IBGRAScanner;
begin
  hasPen := not NoPen and not FillOnly;
  hasBrush := not NoBrush;
  if not HasPen and not HasBrush then exit;
  if hasPen and (Pen.ActualDrawMode <> dmDrawWithTransparency) then
  begin
    ApplyPenStyle;
    Pen.GetUniversalBrush(b);
    penPts := FBitmap.ComputeWidePolygon(APoints, Pen.ActualWidth);
    if AntialiasingMode = amOff then
      FBitmap.FillPoly(penPts, b)
      else FBitmap.FillPolyAntialias(penPts, b);
    if hasBrush then
      PolygonF(APoints, Winding, true);
  end else
  begin
    multi := TBGRAMultishapeFiller.Create;
    multi.Antialiasing := AntialiasingMode <> amOff;
    if Winding then multi.FillMode := fmWinding else
      multi.FillMode := fmAlternate;
    multi.PolygonOrder := poLastOnTop;
    if hasBrush then
    begin
      tex := Brush.BuildTexture(FBitmap);
      if tex <> nil then
        multi.AddPolygon(APoints, tex)
        else multi.AddPolygon(APoints, Brush.ActualColor);
    end;
    if hasPen then
    begin
      ApplyPenStyle;
      multi.AddPolygon(FBitmap.ComputeWidePolygon(APoints,Pen.ActualWidth), Pen.ActualColor);
    end;
    multi.Draw(FBitmap);
    multi.Free
  end;
end;

procedure TBGRACanvas.Polyline(const APoints: array of TPoint);
begin
  Polyline(@APoints[0],length(APoints));
end;

procedure TBGRACanvas.Polyline(const Points: array of TPoint; StartIndex: Integer; NumPts: Integer);
begin
  if (StartIndex < 0) or (StartIndex >= length(Points)) then exit;
  if NumPts < 0 then NumPts := length(Points)-StartIndex;
  Polyline(@Points[StartIndex],NumPts);
end;

procedure TBGRACanvas.Polyline(Points: PPoint; NumPts: Integer);
var
  i: Integer;
  ptsF: array of TPointF;
  oldPos: TPoint;
begin
  if NoPen or (NumPts <= 0) then exit;

  if (Pen.Style = psSolid) and (Pen.ActualWidth = 1) then
  begin
    oldPos := FPenPos;
    MoveTo(Points^.x,Points^.y);
    for i := 1 to NumPts-1 do
    begin
      inc(Points);
      LineTo(Points^.x,Points^.y);
    end;
    FPenPos := oldPos;
    exit;
  end;

  setlength(ptsF, NumPts);
  for i := 0 to NumPts-1 do
  begin
    ptsF[i] := PointF(Points^.x,Points^.y);
    inc(Points);
  end;
  PolylineF(ptsF);
end;

procedure TBGRACanvas.PolylineF(const APoints: array of TPointF);
var ptsF: Array of TPointF;
begin
  if NoPen then exit;
  ApplyPenStyle;
  FBitmap.FillMode := fmWinding;
  ptsF := FBitmap.ComputeWidePolyline(APoints,Pen.ActualWidth);
  if AntialiasingMode = amOff then
    FBitmap.FillPoly(ptsF,Pen.ActualColor,dmDrawWithTransparency) else
    FBitmap.FillPolyAntialias(ptsF,Pen.ActualColor);
end;

procedure TBGRACanvas.PolyBezier(Points: PPoint; NumPts: Integer;
  Filled: boolean; Continuous: boolean);
var
  beziers: array of TCubicBezierCurve;
  nbBeziers,i: integer;
  PrevPt: TPointF;
  spline: array of TPointF;
begin
  if NumPts < 4 then exit;
  if Continuous then
  begin
    nbBeziers := 1+(NumPts-4) div 3;
    setlength(beziers, nbBeziers);
    PrevPt := PointF(Points^.x,Points^.y);
    inc(Points);
    for i := 0 to nbBeziers-1 do
    begin
      beziers[i].p1 := prevPt;
      beziers[i].c1 := PointF(Points^.x,Points^.y);
      inc(Points);
      beziers[i].c2 := PointF(Points^.x,Points^.y);
      inc(Points);
      beziers[i].p2 := PointF(Points^.x,Points^.y);
      inc(Points);
      prevPt := beziers[i].p2;
    end;
  end else
  begin
    nbBeziers := NumPts div 4;
    setlength(beziers, nbBeziers);
    for i := 0 to nbBeziers-1 do
    begin
      beziers[i].p1 := PointF(Points^.x,Points^.y);
      inc(Points);
      beziers[i].c1 := PointF(Points^.x,Points^.y);
      inc(Points);
      beziers[i].c2 := PointF(Points^.x,Points^.y);
      inc(Points);
      beziers[i].p2 := PointF(Points^.x,Points^.y);
      inc(Points);
    end;
  end;
  spline := ComputeBezierSpline(beziers);
  if Filled then
    PolygonF(spline) else
    PolylineF(spline);
end;

procedure TBGRACanvas.PolyBezier(const Points: array of TPoint;
  Filled: boolean; Continuous: boolean);
begin
  PolyBezier(@Points[0],length(Points),Filled,Continuous);
end;

procedure TBGRACanvas.Draw(X, Y: Integer; SrcBitmap: TBGRACustomBitmap);
begin
  FBitmap.PutImage(X,Y,SrcBitmap,dmDrawWithTransparency);
end;

procedure TBGRACanvas.Draw(X, Y: Integer; SrcBitmap: TBitmap);
begin
  FBitmap.PutImage(X,Y,SrcBitmap,dmDrawWithTransparency);
end;

procedure TBGRACanvas.CopyRect(X, Y: Integer; SrcBitmap: TBGRACustomBitmap;
  SrcRect: TRect);
begin
  FBitmap.PutImagePart(X,Y,SrcBitmap,SrcRect,dmDrawWithTransparency);
end;

procedure TBGRACanvas.StretchDraw(DestRect: TRect; SrcBitmap: TBGRACustomBitmap; HorizFlip: Boolean = false; VertFlip: Boolean = false);
var Stretched: TBGRACustomBitmap;
    temp: Integer;
begin
  with DestRect do
  begin
    if (Left= Right) or (Top =Bottom) then exit;
    if Left > Right then
    begin
      Temp := Left;
      Left := Right+1;
      Right := Temp+1;
      HorizFlip := not HorizFlip;
    end;
    if Top > Bottom then
    begin
      Temp := Top;
      Top := Bottom+1;
      Bottom := Temp+1;
      VertFlip := not VertFlip;
    end;
  end;
  if (DestRect.Right-DestRect.Left <> SrcBitmap.Width) or
     (DestRect.Bottom-DestRect.Top <> SrcBitmap.Height) or
     HorizFlip or VertFlip then
  begin
    if AntialiasingMode = amOff then
      Stretched := SrcBitmap.Resample(DestRect.Right-DestRect.Left,DestRect.Bottom-DestRect.Top,rmSimpleStretch) else
      Stretched := SrcBitmap.Resample(DestRect.Right-DestRect.Left,DestRect.Bottom-DestRect.Top,rmFineResample);
    if HorizFlip then Stretched.HorizontalFlip;
    if VertFlip then Stretched.VerticalFlip;
    FBitmap.PutImage(DestRect.Left,DestRect.Top,Stretched,dmDrawWithTransparency);
    Stretched.Free;
  end else
    FBitmap.PutImage(DestRect.Left,DestRect.Top,SrcBitmap,dmDrawWithTransparency);
end;

procedure TBGRACanvas.DrawFocusRect(bounds: TRect);
var
  temp: Integer;
  xb,yb: integer;
  c: TBGRAPixel;
begin
  c := Brush.ActualColor;
  if (c.red = 0) and (c.Green =0) and (c.Blue =0) then exit;
  c.alpha := 0;
  with bounds do
  begin
    if (Left= Right) or (Top =Bottom) then exit;
    if Left > Right then
    begin
      Temp := Left;
      Left := Right;
      Right := Temp;
    end;
    if Top > Bottom then
    begin
      Temp := Top;
      Top := Bottom;
      Bottom := Temp;
    end;
    dec(Right);
    dec(Bottom);
    for xb := max(FBitmap.ClipRect.Left, bounds.Left+1) to min(FBitmap.ClipRect.Right-1,bounds.Right-1) do
    begin
      if odd(xb) xor odd(Top) then FBitmap.XorPixel(xb,Top,c);
      if odd(xb) xor odd(Bottom) then FBitmap.XorPixel(xb,Bottom,c);
    end;
    for yb := max(FBitmap.ClipRect.Top, bounds.Top) to min(FBitmap.ClipRect.Bottom-1,bounds.Bottom) do
    begin
      if odd(yb) xor odd(Left) then FBitmap.XorPixel(Left,yb,c);
      if odd(yb) xor odd(Right) then FBitmap.XorPixel(Right,yb,c);
    end;
  end;
end;

procedure TBGRACanvas.CopyRect(Dest: TRect; SrcBmp: TBGRACustomBitmap;
  Source: TRect);
var TempBmp: TBGRACustomBitmap;
  Temp: Integer;
  FlipHoriz,FlipVert: Boolean;
begin
  if (Dest.Right-Dest.Left = Source.Right-Source.Left) and (Dest.Bottom-Dest.Top = Source.Bottom-Source.Top) and
     (Dest.Right > Dest.Left) and (Dest.Bottom > Dest.Top) then
  begin
    CopyRect(Dest.Left,Dest.Top, SrcBmp, Source);
    exit;
  end;
  if (Source.Left = Source.Right) or (Source.Bottom = Source.Top) or
    (Dest.Left = Dest.Right) or (Dest.Bottom = Dest.Top) then exit;
  if Source.Left > Source.Right then
  begin
    Temp := Source.Left;
    Source.Left := Source.Right+1;
    Source.Right := Temp+1;
    FlipHoriz := True;
  end else
    FlipHoriz := false;
  if Source.Top > Source.Bottom then
  begin
    Temp := Source.Top;
    Source.Top := Source.Bottom+1;
    Source.Bottom := Temp+1;
    FlipVert := True;
  end else
    FlipVert := false;
  TempBmp := SrcBmp.GetPart(Source);
  StretchDraw(Dest,TempBmp,FlipHoriz,FlipVert);
  TempBmp.Free;
end;

procedure TBGRACanvas.TextOut(X, Y: Integer; const Text: String);
var size: TSize;
    c,s: single;
begin
  ApplyFont;
  if DrawFontBackground then
  begin
    size := TextExtent(Text);
    c := cos(Font.Orientation*Pi/1800);
    s := -sin(Font.Orientation*Pi/1800);
    PolygonF([PointF(X,Y),PointF(X+c*size.cx,Y+s*size.cx),
              PointF(X+c*size.cx-s*size.cy,Y+s*size.cx+c*size.cy),
              PointF(X-s*size.cy,Y+c*size.cy)],False,True);
  end;
  if Font.Texture <> nil then
    FBitmap.TextOut(x,y,Text,Font.Texture) else
    FBitmap.TextOut(x,y,Text,Font.BGRAColor);
end;

procedure TBGRACanvas.TextRect(const ARect: TRect; X, Y: integer;
  const Text: string);
begin
  ApplyFont;
  if Font.Texture <> nil then
    FBitmap.TextRect(ARect,x,y,Text,self.TextStyle,Font.Texture) else
    FBitmap.TextRect(ARect,x,y,Text,self.TextStyle,Font.BGRAColor);
end;

procedure TBGRACanvas.TextRect(ARect: TRect; X, Y: integer; const Text: string;
  const Style: TTextStyle);
begin
  ApplyFont;
  if Font.Texture <> nil then
    FBitmap.TextRect(ARect,x,y,Text,Style,Font.Texture) else
    FBitmap.TextRect(ARect,x,y,Text,Style,Font.BGRAColor);
end;

function TBGRACanvas.TextExtent(const Text: string): TSize;
begin
  ApplyFont;
  result := FBitmap.TextSize(Text);
end;

{$hints off}
function TBGRACanvas.TextHeight(const Text: string): Integer;
begin
  ApplyFont;
  result := FBitmap.TextSize(Text).cy;
end;
{$hints on}

function TBGRACanvas.TextWidth(const Text: string): Integer;
begin
  ApplyFont;
  result := FBitmap.TextSize(Text).cx;
end;

end.

