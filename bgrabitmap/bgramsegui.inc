// SPDX-License-Identifier: LGPL-3.0-linking-exception
{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}
type
  TColor = msegraphutils.colorty;

const
  clNone = msegraphutils.cl_none;
  clDefault = msegraphutils.cl_default;
  clBlack = msegraphutils.cl_black;
  clWhite = msegraphutils.cl_white;

function ColorToRGB(c: TColor): TColor;
function clRgbBtnHighlight: TColor;
function clRgbBtnShadow: TColor;
function GetScreenDPIX: integer;
function GetScreenDPIY: integer;

type
  {* How to draw the end of line }
  TPenEndCap = msegraphics.capstylety;

const
    {** Draw a half-disk at the end of the line. The diameter of the disk is
        equal to the pen width. }
    pecRound = msegraphics.cs_round;
    {** Draw a half-square. The size of the square is equal to the pen width.
        This is visually equivalent to extend the line of half the pen width }
    pecSquare = msegraphics.cs_projecting;
    {** The line ends exactly at the end point }
    pecFlat = msegraphics.cs_butt;

type
  {* How to join segments. This makes sense only for geometric pens (that
     have a certain width) }
  TPenJoinStyle = msegraphics.joinstylety;

const
    {** Segments are joined by filling the gap with an arc }
    pjsRound = msegraphics.js_round;
    {** Segments are joind by filling the gap with an intermediary segment }
    pjsBevel = msegraphics.js_bevel;
    {** Segments are joined by extending them up to their intersection.
        There is a miter limit so that if the intersection is too far,
        an intermediary segment is used }
    pjsMiter = msegraphics.js_miter;

type
  {* Style to use for the pen. The unit for the pattern is the width of the
     line }
  TPenStyle = msegraphics.dashesstringty;
  TPenMode = msegraphics.rasteropty;

const
  {** Pen is continuous }
  psSolid = TPenStyle('');
  {** Pen is dashed. The dash have a length of 3 unit and the gaps of 1 unit }
  psDash = TPenStyle(#3#1);
  {** Pen is dotted. The dots have a length of 1 unit and the gaps of 1 unit }
  psDot = TPenStyle(#1#1);
  {** Pattern is a dash of length 3 followed by a dot of length 1, separated by a gap of length 1 }
  psDashDot = TPenStyle(#3#1#1#1);
  {** Dash of length 3, and two dots of length 1 }
  psDashDotDot = TPenStyle(#3#1#1#1#1#1);

  //the following are not directly compatible with mseGUI
  {** Pen is not drawn }
  psClear = TPenStyle(#0#0);
  {** Custom pattern used }
  psPattern = TPenStyle(#0#0#0#0);

  pmBlack = msegraphics.rop_clear;
  pmWhite = msegraphics.rop_set;
  pmNop = msegraphics.rop_nop;
  pmNot = msegraphics.rop_not;
  pmCopy = msegraphics.rop_copy;
  pmNotCopy = msegraphics.rop_notcopy;
  pmMergePenNot = msegraphics.rop_notor;
  pmMaskPenNot = msegraphics.rop_notand;
  pmMergeNotPen = msegraphics.rop_ornot;
  pmMaskNotPen = msegraphics.rop_andnot;
  pmMerge = msegraphics.rop_or;
  pmNotMerge = msegraphics.rop_nor;
  pmMask = msegraphics.rop_and;
  pmNotMask = msegraphics.rop_nand;
  pmXor = msegraphics.rop_xor;
  pmNotXor = msegraphics.rop_notxor;

type
  TPen = class
  private
    FColor: TColor;
    FMode: TPenMode;
    FEndCap: TPenEndCap;
    FJoinStyle: TPenJoinStyle;
    FStyle: TPenStyle;
    FWidth: integer;
  public
    constructor Create;
    {** Color of the pen }
    property Color: TColor read FColor write FColor;
    {** Operation done on pixels }
    property Mode: TPenMode read FMode write FMode;
    {** End cap of the pen: how to draw the ends of the lines }
    property EndCap: TPenEndCap read FEndCap write FEndCap;
    {** Join style: how to join the segments of a polyline }
    property JoinStyle: TPenJoinStyle read FJoinStyle write FJoinStyle;
    {** Pen style: solid, dash, dot... }
    property Style : TPenStyle read FStyle write FStyle;
    {** Pen width in pixels }
    property Width : Integer read FWidth write FWidth;
  end;

  TBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal,
                 bsBDiagonal, bsCross, bsDiagCross, bsImage, bsPattern);

type
  { TBrush }
  {* A class describing a brush }
  TBrush = class
  private
    FColor: TColor;
    FStyle: TBrushStyle;
  public
    constructor Create;
    {** Color of the brush }
    property Color: TColor read FColor write FColor;
    {** Style of the brush: solid, diagonal lines, horizontal lines... }
    property Style: TBrushStyle read FStyle write FStyle;
  end;

type
  {$DEFINE TCanvas}
  TCanvas = msegraphics.tcanvas;
  {$DEFINE TBitmap}
  TBitmap = msebitmap.tbitmap;
  TRawImage = TBitmap;
  TFont = msegraphics.tfont;
  {$DEFINE TFontStyle}
  TFontStyle = msegraphutils.fontstylety;
  TFontStyles = msegraphutils.fontstylesty;

  TBitmapHelper = class helper for TBitmap
  private
    function GetMasked: boolean;
    function GetTransparent: boolean;
    function GetTransparentMode: TTransparentMode;
    function GetTransparentColor: TColor;
  public
    class function Create: TBitmap; overload;
    property Masked: boolean read GetMasked;
    property Transparent: boolean read GetTransparent;
    property TransparentMode: TTransparentMode read GetTransparentMode;
    property TransparentColor: TColor read GetTransparentColor;
  end;

  TCanvasHelper = class helper for TCanvas
    procedure Draw(X,Y: integer; AGraphic: TGraphic); overload;
    procedure StretchDraw(ARect: TRect; AGraphic: TGraphic); overload;
    procedure Draw(X,Y: integer; ABitmap: TBitmap); overload;
    procedure StretchDraw(ARect: TRect; ABitmap: TBitmap); overload;
  end;

  {$DEFINE TFontQuality}
  TFontQuality = (fqDefault, fqNonAntialiased, fqAntialiased, fqCleartype);

  TFontHelper = class helper for TFont
  private
    function GetOrientation: integer;
    procedure SetOrientation(AOrientation: integer);
    function GetFontQuality: TFontQuality;
    procedure SetFontQuality(AQuality: TFontQuality);
    function GetHeightHelper: integer;
    procedure SetHeightHelper(AHeight: integer);
  public
    property Orientation: integer read GetOrientation write SetOrientation;
    property Quality: TFontQuality read GetFontQuality write SetFontQuality;
    property Height: integer read GetHeightHelper write SetHeightHelper;
  end;

const
  fsBold = msegraphutils.fs_bold;
  fsItalic = msegraphutils.fs_italic;
  fsUnderline = msegraphutils.fs_underline;
  fsStrikeOut = msegraphutils.fs_strikeout;

operator :=(AClassesPoint: TClassesPoint): TPoint;
operator :=(AMSERect: TMSERect): TRect;
operator :=(ARect: TRect): TMSERect;
{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}
function ColorToRGB(c: TColor): TColor;
var triple: rgbtriplety;
begin
  triple := msegraphics.colortorgb(c);
  result := msegraphics.rgbtocolor(triple.red, triple.green, triple.blue);
end;

function clRgbBtnHighlight: TColor;
begin
  result := ColorToRGB(msegraphutils.cl_highlight);
end;

function clRgbBtnShadow: TColor;
begin
  result := ColorToRGB(msegraphutils.cl_shadow);
end;

function GetScreenDPIX: integer;
begin
  result := 96;
end;

function GetScreenDPIY: integer;
begin
  result := 96;
end;

{ TPen }

constructor TPen.Create;
begin
  inherited Create;
  Mode := pmCopy;
  Style := psSolid;
  Width := 1;
  Color := cl_black;
  EndCap:= pecRound;
  JoinStyle:= pjsRound;
end;

{ TBrush }

constructor TBrush.Create;
begin
  Color := cl_white;
  Style := bsSolid;
end;

operator :=(AClassesPoint: TClassesPoint): TPoint;
begin
  result.x := AClassesPoint.X;
  result.y := AClassesPoint.Y;
end;

operator :=(AMSERect: TMSERect): TRect;
begin
  result.Left := AMSERect.x;
  result.Top := AMSERect.y;
  result.Right:= AMSERect.x + AMSERect.cx;
  result.Bottom:= AMSERect.y + AMSERect.cy;
end;

operator :=(ARect: TRect): TMSERect;
begin
  result.x := ARect.Left;
  result.y := ARect.Top;
  result.cx := ARect.Right - ARect.Left;
  result.cy := ARect.Bottom - ARect.Top;
end;

class function TBitmapHelper.Create: TBitmap;
begin
  result := TBitmap.Create(bmk_rgb);
end;

function TBitmapHelper.GetMasked: boolean;
begin
  if self is TMaskedBitmap then
    result := TMaskedBitmap(self).masked
  else
    result := false;
end;

function TBitmapHelper.GetTransparent: boolean;
begin
  result := TransparentColor <> clNone;
end;

function TBitmapHelper.GetTransparentMode: TTransparentMode;
begin
  if TMaskedBitmap(self).TransparentColor = clDefault then
    result := tmAuto
  else
    result := tmFixed;
end;

function TBitmapHelper.GetTransparentColor: TColor;
begin
  if self is TMaskedBitmap then
    result := TMaskedBitmap(self).TransparentColor
    else result := clNone;
end;

procedure TCanvasHelper.Draw(X,Y: integer; ABitmap: TBitmap);
begin
  ABitmap.Paint(Self, Point(X,Y));
End;

procedure TCanvasHelper.StretchDraw(ARect: TRect; ABitmap: TBitmap);
begin
  ABitmap.Paint(Self, ARect);
End;

procedure TCanvasHelper.Draw(X,Y: integer; AGraphic: TGraphic);
begin
  AGraphic.Draw(self, rect(x,y,x+AGraphic.Width,y+AGraphic.Height));
End;

procedure TCanvasHelper.StretchDraw(ARect: TRect; AGraphic: TGraphic);
begin
  AGraphic.Draw(self, ARect);
End;

function TFontHelper.GetOrientation: integer;
begin
  result := 0;
end;

procedure TFontHelper.SetOrientation(AOrientation: integer);
begin
  if AOrientation <> 0 then
    raise exception.Create('Only orientation 0 allowed at font level');
end;

function TFontHelper.GetFontQuality: TFontQuality;
begin
  if foo_antialiased2 in options then result := fqClearType else
  if foo_antialiased in options then result := fqAntialiased else
  if foo_nonantialiased in options then result := fqNonAntialiased else
    result := fqDefault;
end;

procedure TFontHelper.SetFontQuality(AQuality: TFontQuality);
begin
  options := options - [foo_nonantialiased, foo_antialiased, foo_antialiased2];
  case AQuality of
  fqNonAntialiased: options := options + [foo_nonantialiased];
  fqAntialiased: options := options + [foo_antialiased];
  fqClearType: options := options + [foo_antialiased2];
  end;
end;

function TFontHelper.GetHeightHelper: integer;
begin
  result := round(inherited Height * 1.5);
end;

procedure TFontHelper.SetHeightHelper(AHeight: integer);
begin
  if AHeight < 0 then
     inherited Height := -AHeight
   else
     inherited Height := round(AHeight/1.5);
end;

{$ENDIF}
