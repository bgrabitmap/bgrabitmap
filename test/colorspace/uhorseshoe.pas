unit uhorseshoe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, BGRAVirtualScreen, BGRABitmap, BGRAClasses, BGRABitmapTypes,
  BGRAGradientScanner;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbColorspace: TComboBox;
    cbReferenceWhite: TComboBox;
    cbOverflow: TComboBox;
    cbFluorescent: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
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
    procedure cbFluorescentClick(Sender: TObject);
    procedure cbOverflowChange(Sender: TObject);
    procedure cbReferenceWhiteChange(Sender: TObject);
    procedure cbXAxisChange(Sender: TObject);
    procedure cbYAxisChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbZChange(Sender: TObject);
    procedure vsGradientRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure vsHorseshoeRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure UpdateSelectedAxis;
    procedure UpdateSelectedColorspace;
    procedure UpdateRGBOverlow;
  private
    ZName, Z2Name: string;
    ZFactor, Z2Factor: single;
    function SelectedColorspace: TColorspaceAny;
    procedure UpdateReferenceWhiteFromCombo;
  public

  end;

  { THorseShoeScanner }

  THorseShoeScanner = class(TBGRACustomScanner)
  protected
    FOrigin: TPointF;
    FWidth,FHeight,FXStep,FYStep: single;
    FXYZ: TXYZA;
    FHorseShoeGrayAmount,FHorseShoeGrayLevel: single;
  public
    constructor Create(AOrigin: TPointF; AWidth,AHeight: single;
                       AHorseShoeGrayAmount,AHorseShoeGrayLevel: single);
    procedure AdaptColorToRGBDisplay(var xyz: TXYZA);
    function ScanAtXYZ(X,Y: Single): TXYZA;
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    procedure ScanMoveTo(X,Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
  end;

var
  Form1: TForm1;

implementation

uses XYZABitmap;

const
  OptimalReflectStep = 250;
  OptimalReflectArraySize = OptimalReflectStep;
  OptimalReflectBorderStep = OptimalReflectStep div 10;

var
  OptimalReflectXYZ: array[0..OptimalReflectArraySize,0..OptimalReflectArraySize] of record
                       min,max: single;
                     end;
{  optimalXYZMin,optimalXYZMax: TXYZA; }
{  labMin,labMax: TLabA;
  lchMin,lchMax: TLChA;}

function IsOptimalReflect(xyz: TXYZA): boolean;
begin
  with GetReferenceWhite do
  begin
    xyz.X /= X;
    xyz.Y /= Y;
    xyz.Z /= Z;
  end;
  if (xyz.Y >= 0) and (xyz.Y <= 1) and
     (xyz.X >= 0) and (xyz.X <= 1) and
     (xyz.Z >= 0) and (xyz.Z <= 1) then
  begin
    xyz.X := sqrt(xyz.X);
    xyz.Z := sqrt(xyz.Z);
    with OptimalReflectXYZ[round(xyz.X*OptimalReflectStep),round(xyz.Z*OptimalReflectStep)] do
      if (min <> EmptySingle) and (xyz.Y >= min) and (xyz.Y <= max) then exit(true);
  end;
  result := false;
end;

procedure AddOptimalReflect(xyz: TXYZA);
{var
  lab: TLabA;
  lch: TLChA; }
begin
{  if xyz.X < optimalXYZMin.X then optimalXYZMin.X := xyz.X;
  if xyz.Y < optimalXYZMin.Y then optimalXYZMin.Y := xyz.Y;
  if xyz.Z < optimalXYZMin.Z then optimalXYZMin.Z := xyz.Z;
  if xyz.X > optimalXYZMax.X then optimalXYZMax.X := xyz.X;
  if xyz.Y > optimalXYZMax.Y then optimalXYZMax.Y := xyz.Y;
  if xyz.Z > optimalXYZMax.Z then optimalXYZMax.Z := xyz.Z;  }
{  lab := xyz.ToLabA(ReferenceWhite2E);
  if lab.L < labMin.L then labMin.L := lab.L;
  if lab.a < labMin.a then labMin.a := lab.a;
  if lab.b < labMin.b then labMin.b := lab.b;
  if lab.L > labMax.L then labMax.L := lab.L;
  if lab.a > labMax.a then labMax.a := lab.a;
  if lab.b > labMax.b then labMax.b := lab.b;

  lch := lab.ToLChA;
  if lch.L < lchMin.L then lchMin.L := lch.L;
  if lch.C < lchMin.C then lchMin.C := lch.C;
  if lch.h < lchMin.h then lchMin.h := lch.h;
  if lch.L > lchMax.L then lchMax.L := lch.L;
  if lch.C > lchMax.C then lchMax.C := lch.C;
  if lch.h > lchMax.h then lchMax.h := lch.h; }

  if (xyz.Y >= 0) and (xyz.Y <= 1) and
     (xyz.X >= 0) and (xyz.X <= 1) and
     (xyz.Z >= 0) and (xyz.Z <= 1) then
  begin
    xyz.X := sqrt(xyz.X);
    xyz.Z := sqrt(xyz.Z);

    with OptimalReflectXYZ[round(xyz.X*OptimalReflectStep),
      round(xyz.Z*OptimalReflectStep)] do
    begin
      if min = EmptySingle then
      begin
        min := xyz.Y;
        max := xyz.Y;
      end else
      begin
        if xyz.Y < min then min := xyz.Y;
        if xyz.Y > max then max := xyz.Y;
      end;
    end;
  end;
end;

{$R *.lfm}

{ THorseShoeScanner }

constructor THorseShoeScanner.Create(AOrigin: TPointF; AWidth, AHeight: single;
                   AHorseShoeGrayAmount,AHorseShoeGrayLevel: single);
begin
  FOrigin := AOrigin;
  FWidth:= AWidth;
  FHeight:= AHeight;
  FHorseShoeGrayAmount:= AHorseShoeGrayAmount;
  FHorseShoeGrayLevel:= AHorseShoeGrayLevel;
  FXStep := 1/FWidth*(1-FHorseShoeGrayAmount);
  FYStep := 1/FHeight*(1-FHorseShoeGrayAmount);
end;

procedure THorseShoeScanner.AdaptColorToRGBDisplay(var xyz:TXYZA);
begin
  xyz.X := xyz.X*(1-FHorseShoeGrayAmount) + FHorseShoeGrayLevel*FHorseShoeGrayAmount;
  xyz.Y := xyz.Y*(1-FHorseShoeGrayAmount) + FHorseShoeGrayLevel*FHorseShoeGrayAmount;
  xyz.Z := xyz.Z*(1-FHorseShoeGrayAmount) + FHorseShoeGrayLevel*FHorseShoeGrayAmount;
end;

function THorseShoeScanner.ScanAtXYZ(X, Y: Single): TXYZA;
begin
  result := TXYZA.New((X-FOrigin.X)/FWidth,(Y-FOrigin.Y)/FHeight,0);
  result.Z := 1-(result.X+result.Y);
  AdaptColorToRGBDisplay(result);
end;

function THorseShoeScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  result := ScanAtXYZ(X,Y).ToBGRAPixel;
end;

procedure THorseShoeScanner.ScanMoveTo(X, Y: Integer);
begin
  FXYZ := ScanAtXYZ(X,Y);
end;

function THorseShoeScanner.ScanNextPixel: TBGRAPixel;
begin
  result := FXYZ.ToBGRAPixel;
  IncF(FXYZ.X, FXStep);
  DecF(FXYZ.Z, FXStep);
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
    if (n <= 0) or (xyz.X < 0) or (xyz.Y < 0) or (xyz.Z < 0) then
      result := EmptyPointF
    else
      result := PointF(squareOrigin.x+xyz.X/n*squareWidth,squareOrigin.y+xyz.Y/n*squareHeight);
  end;

  procedure DrawHorseShoe;
  const HorseShoeMargin = 0.02;
  var
    i,j: Integer;
    xyz: TXYZA;
    pts: array of TPointF;
    scan: THorseShoeScanner;
    min,max: TPointF;
    marginX,marginY: single;
    prevRefWhite: TXYZReferenceWhite;
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

    marginX := Bitmap.Width * HorseShoeMargin;
    marginY := Bitmap.Height * HorseShoeMargin;
    squareWidth:= (Bitmap.Width-1-2*marginX)/max.x;
    squareHeight := -(Bitmap.Height-1-2*marginY)/max.y;
    squareOrigin := PointF(marginX,Bitmap.Height-1-marginY);

    for j := 0 to high(pts) do
      pts[j] := squareOrigin + PointF(pts[j].x*squareWidth,pts[j].y*squareHeight);

    // spectral locus is normalized for equal illuminant
    prevRefWhite := GetReferenceWhite;
    SetReferenceWhite(2, 'E');

    if XYZToRGBOverflowMin = xroClipToTarget then
      scan := THorseShoeScanner.Create(squareOrigin, squareWidth, squareHeight, 0.36, 0)
    else
      scan := THorseShoeScanner.Create(squareOrigin, squareWidth, squareHeight, 0.5, 0.25);

    pts := Bitmap.ComputeOpenedSpline(pts, ssCrossingWithEnds);
    Bitmap.FillPolyAntialias(pts,scan);
    Bitmap.DrawPolygonAntialias(pts,BGRABlack, (Bitmap.Width+Bitmap.Height)/600);
    scan.Free;

    SetReferenceWhite(prevRefWhite);
  end;

  procedure DrawColorspace;
  const bitsPerChannel = 3;
    maxPerChannel = (1 shl bitsPerChannel)-1;
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

    if colorspace.IndexOfAlphaChannel = channelCount-1 then
    begin
      colorspace.SetChannel(colorValue, channelCount-1, max[channelCount-1]);
      dec(channelCount);
    end;
    bitCount := channelCount*bitsPerChannel;

    for i := 0 to (1 shl bitCount) - 1 do
    begin
      for j := 0 to channelCount-1 do
        colorspace.SetChannel(colorValue, j, min[j] + (max[j]-min[j]) * ((i shr (j*bitsPerChannel)) and maxPerChannel)/maxPerChannel);
      colorspace.Convert(colorValue^, xyz, TXYZAColorspace);
      pt := xyzToPointF(xyz);
      if not isEmptyPointF(pt) then
        Bitmap.FillEllipseAntialias(pt.x,pt.y, dotSize,dotSize, BGRA(0,0,0,128));
    end;
    freemem(colorValue);
  end;


begin
  Bitmap.Fill(CSSGray);

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

  cbYAxis.Items.Clear;
  cbYAxis.Style := csDropDownList;
  for i := 0 to colorspace.GetChannelCount-1 do
    cbYAxis.Items.Add(colorspace.GetChannelName(i));

  if (colorspace = TXYZAColorspace) or (colorspace = TWordXYZAColorspace) then
  begin
    cbXAxis.ItemIndex:= colorspace.IndexOfChannel('X');
    cbYAxis.ItemIndex:= colorspace.IndexOfChannel('Z');
  end else
  if (colorspace = TYCbCr601Colorspace) or (colorspace = TYCbCr601JPEGColorspace) or
   (colorspace = TYCbCr709Colorspace) or (colorspace = TYCbCr709JPEGColorspace) then
  begin
    cbXAxis.ItemIndex:= colorspace.IndexOfChannel('BlueDiff');
    cbYAxis.ItemIndex:= colorspace.IndexOfChannel('RedDiff');
  end else
  if colorspace = TLabAColorspace then
  begin
    cbXAxis.ItemIndex:= colorspace.IndexOfChannel('a');
    cbYAxis.ItemIndex:= colorspace.IndexOfChannel('b');
  end else
  if colorspace = TLChAColorspace then
  begin
    cbXAxis.ItemIndex:= colorspace.IndexOfChannel('Hue');
    cbYAxis.ItemIndex:= colorspace.IndexOfChannel('Chroma');
  end else
  begin
    cbXAxis.ItemIndex:= 0;
    if colorspace.GetChannelCount> 1 then
      cbYAxis.ItemIndex:= 1
    else
      cbYAxis.ItemIndex:= 0;
  end;

  cbReferenceWhite.Enabled := cfReferenceWhiteIndependent in colorspace.GetFlags;
  if (colorspace = TXYZAColorspace) or (colorspace = TWordXYZAColorspace) then SetReferenceWhite(2, 'E')
  else UpdateReferenceWhiteFromCombo;

  UpdateSelectedAxis;
  vsHorseshoe.DiscardBitmap;
end;

procedure TForm1.UpdateRGBOverlow;
begin
  XYZToRGBOverflowMin:= TColorspaceOverflow(cbOverflow.ItemIndex);
  XYZToRGBOverflowMax:= TColorspaceOverflow(cbOverflow.ItemIndex);
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

procedure TForm1.UpdateReferenceWhiteFromCombo;
begin
  if cbReferenceWhite.ItemIndex <> -1 then
  begin
    SetReferenceWhite(GetReferenceWhiteByIndex(cbReferenceWhite.ItemIndex));
    vsGradient.DiscardBitmap;
    vsHorseshoe.DiscardBitmap;
  end;
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
    if (csName <> 'Color') and (csName <> 'BGRAPixel') and (csName <> 'ExpandedPixel')
     and (csName <> 'FPColor') then
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

  cbOverflow.ItemIndex:= ord(xroPreserveHue);
  UpdateRGBOverlow;
end;

procedure TForm1.tbZChange(Sender: TObject);
begin
  vsGradient.DiscardBitmap;
end;

procedure TForm1.cbColorspaceChange(Sender: TObject);
begin
  UpdateSelectedColorspace;
end;

procedure TForm1.cbFluorescentClick(Sender: TObject);
begin
  vsGradient.DiscardBitmap;
end;

procedure TForm1.cbOverflowChange(Sender: TObject);
begin
  vsGradient.DiscardBitmap;
  vsHorseshoe.DiscardBitmap;
  UpdateRGBOverlow;
end;

procedure TForm1.cbReferenceWhiteChange(Sender: TObject);
begin
  UpdateReferenceWhiteFromCombo;
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
  xyzaBuf: array of TXYZA;
begin
  colorspace := SelectedColorspace;
  valueSize := colorspace.GetSize;
  rowDataSize := Bitmap.Width * valueSize;
  getmem(rowData, rowDataSize);
  fillchar(rowData^, rowDataSize, 0);

  idxAlpha := colorspace.IndexOfAlphaChannel;
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
  if idxChY = -1 then
  begin
    minChY := 0;
    maxChY := 1;
  end else
  begin
    minChY := colorspace.GetMinValue(idxChY);
    maxChY := colorspace.GetMaxValue(idxChY);
  end;

  WriteStr(s, minChY:0:2, '\', minChX:0:2);
  lblMin.Caption := s;
  if maxChX > 250 then
    WriteStr(s, maxChX:0:0)
  else
    WriteStr(s, maxChX:0:2);
  lblMaxX.Caption := s;
  if maxChY > 250 then
    WriteStr(s, maxChY:0:0)
  else
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
      IncF(valX, valXStep);
      if valX>maxChX then valX := maxChX;
      inc(p, valueSize);
    end;

    if (cfHasImaginaryColors in colorspace.GetFlags) and (XYZToRGBOverflowMin <> xroClipToTarget) then
    begin
      setlength(xyzaBuf, Bitmap.Width);
      colorspace.Convert(rowData^, xyzaBuf[0], TXYZAColorspace, Bitmap.Width);
      if cbFluorescent.Checked then
      begin
        for x := 0 to Bitmap.Width-1 do
        begin
          if not IsRealColor(xyzaBuf[x]) then
            xyzaBuf[x] := XYZATransparent;
        end;
      end else
      begin
        for x := 0 to Bitmap.Width-1 do
        begin
          if not IsOptimalReflect(xyzaBuf[x]) then
            xyzaBuf[x] := XYZATransparent;
        end;
      end;
      TXYZAColorspace.Convert(xyzaBuf[0], temp.ScanLine[y]^, TBGRAPixelColorspace, Bitmap.Width, @ReferenceWhite2D65);
    end else
    if (not cbFluorescent.Checked) and ((cfHasImaginaryColors in colorspace.GetFlags) or (colorspace = TAdobeRGBAColorspace)) then
    begin
      setlength(xyzaBuf, Bitmap.Width);
      colorspace.Convert(rowData^, xyzaBuf[0], TXYZAColorspace, Bitmap.Width);
      for x := 0 to Bitmap.Width-1 do
      begin
        if not IsOptimalReflect(xyzaBuf[x]) then
          xyzaBuf[x] := XYZATransparent;
      end;
      TXYZAColorspace.Convert(xyzaBuf[0], temp.ScanLine[y]^, TBGRAPixelColorspace, Bitmap.Width);
    end else
      colorspace.Convert(rowData^, temp.ScanLine[y]^, TBGRAPixelColorspace, Bitmap.Width);
  end;
  Bitmap.DrawCheckers(Bitmap.ClipRect, CSSGray, CSSSilver);
  Bitmap.PutImage(0,0, temp, dmDrawWithTransparency);
  temp.Free;

  freemem(rowData, rowDataSize);
end;

var i,j,k,l,m,jMod: integer;
  xyz, xyzMax, xyzMain: TXYZA;
  spectralLocusNormalizedSum: array[low(SpectralLocus)..high(SpectralLocus)] of TXYZA;

initialization

  //writeln('Computing reflective color bounds...');
  for i := 0 to OptimalReflectArraySize do
    for j := 0 to OptimalReflectArraySize do
      OptimalReflectXYZ[i,j].min := EmptySingle;
  xyzMax.X := 0;
  xyzMax.Y := 0;
  xyzMax.Z := 0;
  for i := 0 to high(SpectralLocus) do
  begin
    IncF(xyzMax.X, SpectralLocus[i].X);
    IncF(xyzMax.Y, SpectralLocus[i].Y);
    IncF(xyzMax.Z, SpectralLocus[i].Z);
  end;
  for i := 0 to high(SpectralLocus) do
    spectralLocusNormalizedSum[i] := TXYZA.New(SpectralLocus[i].X/xyzMax.X/OptimalReflectBorderStep,
      SpectralLocus[i].Y/xyzMax.Y/OptimalReflectBorderStep,
      SpectralLocus[i].Z/xyzMax.Z/OptimalReflectBorderStep);
  {optimalXYZMin := CSSSilver;
  optimalXYZMax := CSSSilver;}
  {labMin := CSSSilver;
  labMax := CSSSilver;
  lchMin := CSSSilver;
  lchMax := CSSSilver;}
  AddOptimalReflect(BGRABlack);

  for i := 0 to high(SpectralLocus) do
  begin
    xyzMain.X := 0;
    xyzMain.Y := 0;
    xyzMain.Z := 0;
    jMod := i;
    for k := 1 to length(SpectralLocus) do
    begin
      if k = 1 then
      begin
        xyz := xyzMain;
        for l := 1 to OptimalReflectBorderStep*8 do
        begin
          with spectralLocusNormalizedSum[i] do
          begin
            IncF(xyz.X, X*0.125);
            IncF(xyz.Y, Y*0.125);
            IncF(xyz.Z, Z*0.125);
          end;
          AddOptimalReflect(xyz);
        end;
      end else
      for l := 1 to OptimalReflectBorderStep do
      begin
        xyz := xyzMain;
        with spectralLocusNormalizedSum[i] do
        begin
          IncF(xyz.X, l*X);
          IncF(xyz.Y, l*Y);
          IncF(xyz.Z, l*Z);
        end;
        for m := 1 to OptimalReflectBorderStep do
        begin
          with spectralLocusNormalizedSum[jMod] do
          begin
            IncF(xyz.X, X);
            IncF(xyz.Y, Y);
            IncF(xyz.Z, Z);
          end;
          AddOptimalReflect(xyz);
        end;
      end;

      if k >= 2 then
      begin
        with spectralLocusNormalizedSum[jMod] do
        begin
          IncF(xyzMain.X, X*OptimalReflectBorderStep);
          IncF(xyzMain.Y, Y*OptimalReflectBorderStep);
          IncF(xyzMain.Z, Z*OptimalReflectBorderStep);
        end;
      end;
      inc(jMod);
      if jMod = length(SpectralLocus) then jMod := 0;
    end;
  end;

  //writeln('xyz min ',optimalXYZMin.x,', ',optimalXYZMin.y,', ',optimalXYZMin.z);
  //writeln('xyz max ',optimalXYZMax.x,', ',optimalXYZMax.y,', ',optimalXYZMax.z);
  {writeln('Lab min ',labMin.L,', ',labMin.a,', ',labMin.b);
  writeln('Lab max ',labMax.L,', ',labMax.a,', ',labMax.b);
  writeln('LCh min ',lChMin.L,', ',lChMin.C,', ',lChMin.h);
  writeln('LCh max ',lChMax.L,', ',lChMax.C,', ',lChMax.h);}
end.

