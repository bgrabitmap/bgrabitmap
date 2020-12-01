// SPDX-License-Identifier: LGPL-3.0-linking-exception
{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}

type
  {* How to draw the end of line }
  TPenEndCap = TFPPenEndCap;

const
    {** Draw a half-disk at the end of the line. The diameter of the disk is
        equal to the pen width. }
    pecRound = FPCanvas.pecRound;
    {** Draw a half-square. The size of the square is equal to the pen width.
        This is visually equivalent to extend the line of half the pen width }
    pecSquare = FPCanvas.pecSquare;
    {** The line ends exactly at the end point }
    pecFlat = FPCanvas.pecFlat;

type
  {* How to join segments. This makes sense only for geometric pens (that
     have a certain width) }
  TPenJoinStyle = TFPPenJoinStyle;

const
    {** Segments are joined by filling the gap with an arc }
    pjsRound = FPCanvas.pjsRound;
    {** Segments are joind by filling the gap with an intermediary segment }
    pjsBevel = FPCanvas.pjsBevel;
    {** Segments are joined by extending them up to their intersection.
        There is a miter limit so that if the intersection is too far,
        an intermediary segment is used }
    pjsMiter = FPCanvas.pjsMiter;

type
  {* Style to use for the pen. The unit for the pattern is the width of the
     line }
  TPenStyle = TFPPenStyle;
  TPenMode = TFPPenMode;

const
  {** Pen is continuous }
  psSolid = FPCanvas.psSolid;
  {** Pen is dashed. The dash have a length of 3 unit and the gaps of 1 unit }
  psDash = FPCanvas.psDash;
  {** Pen is dotted. The dots have a length of 1 unit and the gaps of 1 unit }
  psDot = FPCanvas.psDot;
  {** Pattern is a dash of length 3 followed by a dot of length 1, separated by a gap of length 1 }
  psDashDot = FPCanvas.psDashDot;
  {** Dash of length 3, and two dots of length 1 }
  psDashDotDot = FPCanvas.psDashDotDot;
  {** Pen is not drawn }
  psClear = FPCanvas.psClear;
  {** Not used. Provided for compatibility }
  psInsideframe = FPCanvas.psInsideframe;
  {** Custom pattern used }
  psPattern = FPCanvas.psPattern;

  pmBlack = FPCanvas.pmBlack;
  pmWhite = FPCanvas.pmWhite;
  pmNop = FPCanvas.pmNop;
  pmNot = FPCanvas.pmNot;
  pmCopy = FPCanvas.pmCopy;
  pmNotCopy = FPCanvas.pmNotCopy;
  pmMergePenNot = FPCanvas.pmMergePenNot;
  pmMaskPenNot = FPCanvas.pmMaskPenNot;
  pmMergeNotPen = FPCanvas.pmMergeNotPen;
  pmMaskNotPen = FPCanvas.pmMaskNotPen;
  pmMerge = FPCanvas.pmMerge;
  pmNotMerge = FPCanvas.pmNotMerge;
  pmMask = FPCanvas.pmMask;
  pmNotMask = FPCanvas.pmNotMask;
  pmXor = FPCanvas.pmXor;
  pmNotXor = FPCanvas.pmNotXor;

type
  { TPen }
  {* A class containing a pen }
  TPen = class(TFPCustomPen)
  private
    function GetColor: TColor;
    procedure SetColor(AValue: TColor);
  public
    constructor Create; override;
    {** Color of the pen }
    property Color: TColor read GetColor write SetColor;
    {** Operation done on pixels }
    property Mode;
    {** End cap of the pen: how to draw the ends of the lines }
    property EndCap;
    {** Join style: how to join the segments of a polyline }
    property JoinStyle;
    {** Pen style: solid, dash, dot... }
    property Style;
    {** Pen width in pixels }
    property Width;
  end;

  {* Pattern when filling with a brush. It is used in BGRACanvas but can
     also be created with TBGRABitmap.CreateBrushTexture function }
  TBrushStyle = TFPBrushStyle;

const
  {** Fill with the current color }
  bsSolid = FPCanvas.bsSolid;
  {** Does not fill at all }
  bsClear = FPCanvas.bsClear;
  {** Draw horizontal lines }
  bsHorizontal = FPCanvas.bsHorizontal;
  {** Draw vertical lines }
  bsVertical = FPCanvas.bsVertical;
  {** Draw diagonal lines from top-left to bottom-right }
  bsFDiagonal = FPCanvas.bsFDiagonal;
  {** Draw diagonal lines from bottom-left to top-right }
  bsBDiagonal = FPCanvas.bsBDiagonal;
  {** Draw both horizontal and vertical lines }
  bsCross = FPCanvas.bsCross;
  {** Draw both diagonal lines }
  bsDiagCross = FPCanvas.bsDiagCross;
  {** Fills with an image }
  bsImage = FPCanvas.bsImage;

type
  { TBrush }
  {* A class describing a brush }
  TBrush = class(TFPCustomBrush)
  private
    function GetColor: TColor;
    procedure SetColor(AValue: TColor);
  public
    constructor Create; override;
    {** Color of the brush }
    property Color: TColor read GetColor write SetColor;
    {** Style of the brush: solid, diagonal lines, horizontal lines... }
    property Style;
  end;

  { TFont }
  {* Contains the description of a font }
  TFont = class(TFPCustomFont)
  private
    FPixelsPerInch, FHeight: Integer;
    FQuality: TFontQuality;
    FStyle: TFontStyles;
    function GetColor: TColor;
    function GetHeight: Integer;
    function GetSize: Integer;
    function GetStyle: TFontStyles;
    procedure SetColor(AValue: TColor);
    procedure SetHeight(AValue: Integer);
    procedure SetQuality(AValue: TFontQuality);
    procedure SetStyle(AValue: TFontStyles);
  protected
    procedure SetSize(AValue: Integer); override;
  public
    constructor Create; override;
    {** Pixels per inches }
    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch;
    {** Color of the font }
    property Color: TColor read GetColor write SetColor;
    {** Height of the font in pixels. When the number is negative, it indicates a size in pixels }
    property Height: Integer read GetHeight write SetHeight;
    {** Size of the font in inches. When the number is negative, it indicates a height in inches }
    property Size: Integer read GetSize write SetSize;
    {** Quality of the font rendering }
    property Quality: TFontQuality read FQuality write SetQuality;
    {** Style to apply to the text }
    property Style: TFontStyles read GetStyle write SetStyle;
  end;

{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}

{ TPen }

procedure TPen.SetColor(AValue: TColor);
begin
  FPColor := TColorToFPColor(AValue);
end;

function TPen.GetColor: TColor;
begin
  result := FPColorToTColor(FPColor);
end;

constructor TPen.Create;
begin
  inherited Create;
  Mode := pmCopy;
  Style := psSolid;
  Width := 1;
  FPColor := colBlack;
  EndCap:= pecRound;
  JoinStyle:= pjsRound;
end;

{ TBrush }

function TBrush.GetColor: TColor;
begin
  result := FPColorToTColor(FPColor);
end;

procedure TBrush.SetColor(AValue: TColor);
begin
  FPColor := TColorToFPColor(AValue);
end;

constructor TBrush.Create;
begin
  inherited Create;
  FPColor := colWhite;
end;

{ TFont }

function TFont.GetColor: TColor;
begin
  result := FPColorToTColor(FPColor);
end;

function TFont.GetHeight: Integer;
begin
  result := FHeight;
end;

function TFont.GetSize: Integer;
begin
  Result := inherited Size;
end;

function TFont.GetStyle: TFontStyles;
begin
  result := FStyle;
end;

procedure TFont.SetColor(AValue: TColor);
begin
  FPColor := TColorToFPColor(AValue);
end;

procedure TFont.SetHeight(AValue: Integer);
begin
  if Height <> AValue then
  begin
    FHeight := AValue;
    inherited SetSize(-MulDiv(AValue, 72, FPixelsPerInch));
  end;
end;

procedure TFont.SetQuality(AValue: TFontQuality);
begin
  if FQuality=AValue then Exit;
  FQuality:=AValue;
end;

procedure TFont.SetSize(AValue: Integer);
begin
  if Size <> AValue then
  begin
    inherited SetSize(AValue);
    FHeight := -MulDiv(AValue, FPixelsPerInch, 72);
  end;
end;

procedure TFont.SetStyle(AValue: TFontStyles);
begin
  if FStyle <> AValue then
  begin
    FStyle := AValue;
    inherited SetFlags(5, fsBold in FStyle);
    inherited SetFlags(6, fsItalic in FStyle);
    inherited SetFlags(7, fsUnderline in FStyle);
    inherited SetFlags(8, fsStrikeOut in FStyle);
  end;
end;

constructor TFont.Create;
begin
  FPixelsPerInch := GetScreenDPIY;
  FQuality := fqDefault;
  FPColor := colBlack;
end;

{$ENDIF}
