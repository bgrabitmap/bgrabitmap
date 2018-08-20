unit uhorseshoe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes,
  BGRAGradientScanner;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbColorspace: TComboBox;
    cbReferenceWhite: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    lblZ: TLabel;
    lblZ2: TLabel;
    lblMaxY: TLabel;
    lblMaxX: TLabel;
    lblMin: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    tbZ: TTrackBar;
    tbZ2: TTrackBar;
    vsGradient: TBGRAVirtualScreen;
    cbYAxis: TComboBox;
    cbXAxis: TComboBox;
    Panel1: TPanel;
    Splitter1: TSplitter;
    vsHorseshoe: TBGRAVirtualScreen;
    procedure cbColorspaceChange(Sender: TObject);
    procedure cbReferenceWhiteChange(Sender: TObject);
    procedure cbXAxisChange(Sender: TObject);
    procedure cbYAxisChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbZChange(Sender: TObject);
    procedure vsGradientRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure vsHorseshoeRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure UpdateSelectedAxis;
    procedure UpdateSelectedColorspace;
  private
    ZName, Z2Name: string;
    ZFactor, Z2Factor: single;
    function SelectedColorspace: TColorspaceAny;
  public

  end;

  { THorseShoeScanner }

  THorseShoeScanner = class(TBGRACustomScanner)
  protected
    FOrigin: TPointF;
    FWidth,FHeight,FXStep,FYStep: single;
    FXYZ: TXYZA;
  public
    constructor Create(AOrigin: TPointF; AWidth,AHeight: single);
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ THorseShoeScanner }

constructor THorseShoeScanner.Create(AOrigin: TPointF; AWidth, AHeight: single);
begin
  FOrigin := AOrigin;
  FWidth:= AWidth;
  FHeight:= AHeight;
  FXStep := 1/FWidth;
  FYStep := 1/FHeight;
end;

function THorseShoeScanner.ScanAt(X, Y: Single): TBGRAPixel;
var
  xyz: TXYZA;
begin
  xyz := TXYZA.New((X-FOrigin.X)*FXStep,(Y-FOrigin.Y)*FYStep,0);
  xyz.Z := 1-(xyz.X+xyz.Y);
  result := xyz.ToBGRAPixel;
end;

procedure THorseShoeScanner.ScanMoveTo(X, Y: Integer);
begin
  FXYZ := TXYZA.New((X-FOrigin.X)*FXStep,(Y-FOrigin.Y)*FYStep,0);
  FXYZ.Z := 1-(FXYZ.X+FXYZ.Y);
end;

function THorseShoeScanner.ScanNextPixel: TBGRAPixel;
begin
  result := FXYZ.ToBGRAPixel;
  FXYZ.X += FXStep;
  FXYZ.Z -= FXStep;
end;

{ TForm1 }

procedure TForm1.vsHorseshoeRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  squareWidth,squareHeight: single;
  squareOrigin: TPointF;

  function xyzToPointF(xyz: TXYZA): TPointF;
  var
    n: single;
  begin
    n := xyz.X+xyz.Y+xyz.Z;
    if n = 0 then
      result := EmptyPointF
    else
      result := PointF(squareOrigin.x+xyz.X/n*squareWidth,squareOrigin.y+xyz.Y/n*squareHeight);
  end;

  procedure DrawHorseShoe;
  var
    i,j: Integer;
    xyz: TXYZA;
    pts: array of TPointF;
    scan: THorseShoeScanner;
    min,max: TPointF;
    marginX,marginY: single;
  begin
    squareOrigin := PointF(0,0);
    squareWidth:= 1;
    squareHeight:= 1;

    setlength(pts, length(SpectralLocus));
    xyz.alpha := 1;
    j := 0;
    for i := low(SpectralLocus) to high(SpectralLocus) do
    begin
      xyz.X := SpectralLocus[i].X;
      xyz.Y := SpectralLocus[i].Y;
      xyz.Z := SpectralLocus[i].Z;
      pts[j] := xyzToPointF(xyz);
      if i = low(SpectralLocus) then
      begin
        min := pts[j];
        max := pts[j];
      end else
      begin
        if pts[j].x < min.x then min.x := pts[j].x
        else if pts[j].x > max.x then max.x := pts[j].x;
        if pts[j].y < min.y then min.y := pts[j].y
        else if pts[j].y > max.y then max.y := pts[j].y;
      end;
      inc(j);
    end;

    marginX := Bitmap.Width * 0.02;
    marginY := Bitmap.Height * 0.02;
    squareWidth:= (Bitmap.Width-1-2*marginX)/max.x;
    squareHeight := -(Bitmap.Height-1-2*marginY)/max.y;
    squareOrigin := PointF(marginX,Bitmap.Height-1-marginY);

    for j := 0 to high(pts) do
      pts[j] := squareOrigin + PointF(pts[j].x*squareWidth,pts[j].y*squareHeight);

    scan := THorseShoeScanner.Create(squareOrigin, squareWidth, squareHeight);
    pts := Bitmap.ComputeOpenedSpline(pts, ssCrossingWithEnds);
    Bitmap.FillPolyAntialias(pts,scan);
    Bitmap.DrawPolygonAntialias(pts,BGRABlack, (Bitmap.Width+Bitmap.Height)/600);
    scan.Free;
  end;

  procedure DrawColorspace;
  var
    xyz: TXYZA;
    i,j, channelCount, bitCount: Integer;
    colorspace: TColorspaceAny;
    colorValue: pointer;
    dotSize: single;
    pt: TPointF;
    min,max: array of single;
  begin
    dotSize := (Bitmap.Width+Bitmap.Height)/400;
    colorspace := SelectedColorspace;
    getmem(colorValue, colorspace.GetSize);
    channelCount:= colorspace.GetChannelCount;
    setlength(min, channelCount);
    setlength(max, channelCount);
    for j := 0 to channelCount-1 do
    begin
      min[j] := colorspace.GetMinValue(j);
      max[j] := colorspace.GetMaxValue(j);
    end;

    if colorspace.GetChannelName(channelCount-1) = 'Alpha' then
    begin
      colorspace.SetChannel(colorValue, channelCount-1, max[channelCount-1]);
      dec(channelCount);
    end;
    bitCount := channelCount*3;

    for i := 0 to (1 shl bitCount) - 1 do
    begin
      for j := 0 to channelCount-1 do
        colorspace.SetChannel(colorValue, j, min[j] + (max[j]-min[j]) * ((i shr (j*3)) and 7)/7);
      colorspace.Convert(colorValue^, xyz, TXYZAColorspace);
      pt := xyzToPointF(xyz);
      if not isEmptyPointF(pt) then
        Bitmap.FillEllipseAntialias(pt.x,pt.y, dotSize,dotSize, BGRA(0,0,0,128));
    end;
    freemem(colorValue);
  end;


begin
  Bitmap.Fill(BGRAWhite);

  DrawHorseShoe;

  DrawColorspace;

end;

procedure TForm1.UpdateSelectedAxis;
var
  colorspace: TColorspaceAny;

  procedure UpdateZCombo(zIndex: integer; var ZName: string; var zFactor: single; lblZ: TLabel; tbZ: TTrackBar);
  begin
    if zIndex = -1 then
    begin
      lblZ.Caption := '';
      tbZ.Enabled := false;
    end else
    begin
      ZName := colorspace.GetChannelName(zIndex);
      lblZ.Caption := ZName;
      tbZ.Enabled := true;
      if colorspace.GetMaxValue(zIndex)-colorspace.GetMinValue(zIndex) < 10 then
        zFactor:= 100
      else
        zFactor := 1;
      tbZ.Min := round(colorspace.GetMinValue(zIndex)*zFactor);
      tbZ.Max := round(colorspace.GetMaxValue(zIndex)*zFactor);
      if ZName = 'Alpha' then
        tbZ.Position := tbZ.Max
      else
        tbZ.Position := (tbZ.Max+tbZ.Min) div 2;
    end;
  end;

var
  i,j: Integer;
  zIndex,z2Index: integer;
begin
  colorspace := SelectedColorspace;
  j := 0;
  zIndex := -1;
  z2Index:= -1;
  for i := 0 to colorspace.GetChannelCount-1 do
  begin
    if (colorspace.GetChannelName(i) <> cbXAxis.Text) and
       (colorspace.GetChannelName(i) <> cbYAxis.Text) then
    begin
      case j of
      0: zIndex := i;
      1: z2Index:= i;
      end;
      inc(j);
    end;
  end;

  UpdateZCombo(zIndex, ZName, zFactor, lblZ, tbZ);
  UpdateZCombo(z2Index, Z2Name, z2Factor, lblZ2, tbZ2);

  vsGradient.DiscardBitmap;
end;

procedure TForm1.UpdateSelectedColorspace;
var
  colorspace: TColorspaceAny;
  i: Integer;
begin
  colorspace := SelectedColorspace;
  cbXAxis.Items.Clear;
  cbXAxis.Style := csDropDownList;
  for i := 0 to colorspace.GetChannelCount-1 do
    cbXAxis.Items.Add(colorspace.GetChannelName(i));
  cbXAxis.ItemIndex:= 0;

  cbYAxis.Items.Clear;
  cbYAxis.Style := csDropDownList;
  for i := 0 to colorspace.GetChannelCount-1 do
    cbYAxis.Items.Add(colorspace.GetChannelName(i));
  cbYAxis.ItemIndex:= 1;

  cbReferenceWhite.Enabled := colorspace.HasReferenceWhite;

  UpdateSelectedAxis;
  vsHorseshoe.DiscardBitmap;
end;

function TForm1.SelectedColorspace: TColorspaceAny;
var
  i: Integer;
begin
  for i := 0 to ColorspaceCollection.GetCount-1 do
    if ColorspaceCollection.GetItem(i).GetName = cbColorspace.Text then
      exit(ColorspaceCollection.GetItem(i));

  result := TBGRAPixelColorspace;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
  csName: String;
begin
  cbColorspace.Items.Clear;
  cbColorspace.Style := csDropDownList;
  for i := 0 to ColorspaceCollection.GetCount-1 do
  begin
    csName := ColorspaceCollection.GetItem(i).GetName;
    if (csName <> 'Color') and (csName <> 'BGRAPixel') and (csName <> 'ExpandedPixel') then
      cbColorspace.Items.Add(csName);
  end;
  cbColorspace.ItemIndex := 0;

  cbReferenceWhite.Items.Clear;
  cbReferenceWhite.Style := csDropDownList;
  for i := 0 to GetReferenceWhiteCount-1 do
    with GetReferenceWhiteByIndex(i) do
    begin
      cbReferenceWhite.Items.Add(inttostr(ObserverAngle)+'° '+Illuminant);
      if (ObserverAngle = GetReferenceWhite.ObserverAngle) and (Illuminant = GetReferenceWhite.Illuminant) then
        cbReferenceWhite.ItemIndex := cbReferenceWhite.Items.Count-1;
    end;

  UpdateSelectedColorspace;
end;

procedure TForm1.tbZChange(Sender: TObject);
begin
  vsGradient.DiscardBitmap;
end;

procedure TForm1.cbColorspaceChange(Sender: TObject);
begin
  UpdateSelectedColorspace;
end;

procedure TForm1.cbReferenceWhiteChange(Sender: TObject);
begin
  if cbReferenceWhite.ItemIndex <> -1 then
  begin
    SetReferenceWhite(GetReferenceWhiteByIndex(cbReferenceWhite.ItemIndex));
    vsGradient.DiscardBitmap;
    vsHorseshoe.DiscardBitmap;
  end;
end;

procedure TForm1.cbXAxisChange(Sender: TObject);
begin
  UpdateSelectedAxis;
end;

procedure TForm1.cbYAxisChange(Sender: TObject);
begin
  UpdateSelectedAxis;
end;

procedure TForm1.vsGradientRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  colorspace: TColorspaceAny;
  rowData, p: PByte;
  valueSize, rowDataSize, y, x: integer;
  idxAlpha, idxChX, idxChY, idxZ: integer;
  maxAlpha, zValue, minChX, minChY, maxChX, maxChY, valX, valXStep, valY: single;
  s: string;
  temp: TBGRABitmap;
begin
  colorspace := SelectedColorspace;
  valueSize := colorspace.GetSize;
  rowDataSize := Bitmap.Width * valueSize;
  getmem(rowData, rowDataSize);
  fillchar(rowData^, rowDataSize, 0);

  idxAlpha := colorspace.IndexOfChannel('Alpha');
  if idxAlpha <> -1 then
  begin
    maxAlpha := colorspace.GetMaxValue(idxAlpha);
    p := rowData;
    for x := 0 to Bitmap.Width-1 do
    begin
      colorspace.SetChannel(p, idxAlpha, maxAlpha);
      inc(p, valueSize);
    end;
  end;

  idxZ := colorspace.IndexOfChannel(ZName);
  if idxZ <> -1 then
  begin
    zValue := tbZ.Position/ZFactor;
    p := rowData;
    for x := 0 to Bitmap.Width-1 do
    begin
      colorspace.SetChannel(p, idxZ, zValue);
      inc(p, valueSize);
    end;
  end;

  idxZ := colorspace.IndexOfChannel(Z2Name);
  if idxZ <> -1 then
  begin
    zValue := tbZ2.Position/Z2Factor;
    p := rowData;
    for x := 0 to Bitmap.Width-1 do
    begin
      colorspace.SetChannel(p, idxZ, zValue);
      inc(p, valueSize);
    end;
  end;

  idxChX := cbXAxis.ItemIndex;
  minChX := colorspace.GetMinValue(idxChX);
  maxChX := colorspace.GetMaxValue(idxChX);
  idxChY := cbYAxis.ItemIndex;
  minChY := colorspace.GetMinValue(idxChY);
  maxChY := colorspace.GetMaxValue(idxChY);

  WriteStr(s, minChY:0:2, '\', minChX:0:2);
  lblMin.Caption := s;
  WriteStr(s, maxChX:0:2);
  lblMaxX.Caption := s;
  WriteStr(s, maxChY:0:2);
  lblMaxY.Caption := s;

  temp := TBGRABitmap.Create(Bitmap.Width,Bitmap.Height);
  for y := 0 to Bitmap.Height-1 do
  begin
    valY := (1-y/(Bitmap.Height-1))*(maxChY-minChY) + minChY;
    p := rowData;
    for x := 0 to Bitmap.Width-1 do
    begin
      colorspace.SetChannel(p, idxChY, valY);
      inc(p, valueSize);
    end;

    valX := minChX;
    valXStep := 1/(Bitmap.Width-1)*(maxChX-minChX);
    p := rowData;
    for x := 0 to Bitmap.Width-1 do
    begin
      colorspace.SetChannel(p, idxChX, valX);
      valX += valXStep;
      inc(p, valueSize);
    end;

    colorspace.Convert(rowData^, temp.ScanLine[y]^, TBGRAPixelColorspace, Bitmap.Width);
  end;
  Bitmap.DrawCheckers(Bitmap.ClipRect, CSSWhite, CSSSilver);
  Bitmap.PutImage(0,0, temp, dmDrawWithTransparency);
  temp.Free;

  freemem(rowData, rowDataSize);
end;

end.

