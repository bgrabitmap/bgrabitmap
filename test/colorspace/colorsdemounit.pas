unit ColorsDemoUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, Spin, ExtCtrls,
  BGRABitmap, BGRABitmapTypes, BGRAColorEx;

type

  { TForm1 }

  TForm1 = class(TForm)
    A_se: TFloatSpinEdit;
    A_tb: TTrackBar;
    B2_se: TFloatSpinEdit;
    B2_tb: TTrackBar;
    Dec_edt: TEdit;
    Alpha_se: TFloatSpinEdit;
    Alpha_tb: TTrackBar;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    lB_se1: TFloatSpinEdit;
    lB_tb1: TTrackBar;
    lG_se1: TFloatSpinEdit;
    lG_tb1: TTrackBar;
    lH2_se: TFloatSpinEdit;
    lH2_tb: TTrackBar;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    lH_se: TFloatSpinEdit;
    lH_tb: TTrackBar;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    lB_se: TFloatSpinEdit;
    Label30: TLabel;
    Grayscale_pnl: TPanel;
    Invert_pnl: TPanel;
    lL_se: TFloatSpinEdit;
    lL_tb: TTrackBar;
    lR_se1: TFloatSpinEdit;
    lR_tb1: TTrackBar;
    lS2_se: TFloatSpinEdit;
    lS2_tb: TTrackBar;
    sB_se: TFloatSpinEdit;
    lB_tb: TTrackBar;
    sB_tb: TTrackBar;
    C2_se: TFloatSpinEdit;
    C2_tb: TTrackBar;
    C_se: TFloatSpinEdit;
    C_tb: TTrackBar;
    lG_se: TFloatSpinEdit;
    sG_se: TFloatSpinEdit;
    lG_tb: TTrackBar;
    sG_tb: TTrackBar;
    H2_se: TFloatSpinEdit;
    H2_tb: TTrackBar;
    H3_se: TFloatSpinEdit;
    H3_tb: TTrackBar;
    Hex_edt: TEdit;
    H_se: TFloatSpinEdit;
    H_tb: TTrackBar;
    K_se: TFloatSpinEdit;
    K_tb: TTrackBar;
    L2_se: TFloatSpinEdit;
    L2_tb: TTrackBar;
    L3_se: TFloatSpinEdit;
    L3_tb: TTrackBar;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    L_se: TFloatSpinEdit;
    L_tb: TTrackBar;
    M_se: TFloatSpinEdit;
    M_tb: TTrackBar;
    Name_edt: TEdit;
    Color_pnl: TPanel;
    lR_se: TFloatSpinEdit;
    sR_se: TFloatSpinEdit;
    lR_tb: TTrackBar;
    sR_tb: TTrackBar;
    S2_se: TFloatSpinEdit;
    S2_tb: TTrackBar;
    S_se: TFloatSpinEdit;
    lS_se: TFloatSpinEdit;
    S_tb: TTrackBar;
    gamma_tb: TTrackBar;
    lS_tb: TTrackBar;
    V_se: TFloatSpinEdit;
    gamma_se: TFloatSpinEdit;
    lL2_se: TFloatSpinEdit;
    V_tb: TTrackBar;
    lL2_tb: TTrackBar;
    X_se: TFloatSpinEdit;
    X_tb: TTrackBar;
    Y2_se: TFloatSpinEdit;
    Y2_tb: TTrackBar;
    Y_se: TFloatSpinEdit;
    Y_tb: TTrackBar;
    Z_se: TFloatSpinEdit;
    Z_tb: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure gamma_seChange(Sender: TObject);
    procedure gamma_tbChange(Sender: TObject);
  private
    col: TColorEx;
    ChangingColors: boolean;
    procedure InitControls;
    procedure UserInputChange(Sender: TObject);
    procedure UpdateColorControls(SourceTag: integer);
    function FindComponentByTag(ATag: integer; AClassName: string): TComponent;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  BGRASetGamma(2.2);
  InitControls;
end;

procedure TForm1.gamma_seChange(Sender: TObject);
begin
  if ChangingColors then
    exit;
  ChangingColors := True;
  BGRASetGamma(gamma_se.Value);
  gamma_tb.Position := round(gamma_se.Value * 100);
  UpdateColorControls(-1);
  ChangingColors := False;
end;

procedure TForm1.gamma_tbChange(Sender: TObject);
begin
  if ChangingColors then
    exit;
  ChangingColors := True;
  gamma_se.Value := gamma_tb.Position / 100;
  BGRASetGamma(gamma_se.Value);
  UpdateColorControls(-1);
  ChangingColors := False;
end;

procedure TForm1.InitControls;
var
  i: integer;
  tb: TTrackBar;
  fse: TFloatSpinEdit;

  procedure SetControlsValues(ca: array of TTrackBar; Mi, Mx, Fr: integer);
  var
    i: integer;
  begin
    for i := 0 to Length(ca) - 1 do
    begin
      with ca[i] do
      begin
        Min := Mi;
        Max := Mx;
        Frequency := Fr;
      end;
    end;
  end;

begin
  ChangingColors := True;

  SetControlsValues([lR_tb, lG_tb, lB_tb], 0, 100, 10);
  SetControlsValues([sR_tb, sG_tb, sB_tb], 0, 255, 10);
  SetControlsValues([lR_tb1, lG_tb1, lB_tb1], 0, 255, 10);
  SetControlsValues([H_tb], 0, 360, 10);
  SetControlsValues([S_tb, L_tb], 0, 100, 10);
  SetControlsValues([H2_tb], 0, 360, 10);
  SetControlsValues([S2_tb, V_tb], 0, 100, 10);
  SetControlsValues([X_tb, Y2_tb, Z_tb], 0, 100, 10);
  SetControlsValues([L2_tb], 0, 100, 10);
  SetControlsValues([A_tb, B2_tb], -128, 127, 10);
  SetControlsValues([C_tb, M_tb, Y_tb, K_tb], 0, 100, 10);
  SetControlsValues([L3_tb], 0, 100, 10);
  SetControlsValues([C2_tb], 0, 180, 10);
  SetControlsValues([H3_tb], 0, 360, 10);
  SetControlsValues([Alpha_tb], 0, 100, 10);
  SetControlsValues([lH_tb, lH2_tb], 0, 360, 10);
  SetControlsValues([lS_tb, lL_tb, lS2_tb, lL2_tb], 0, 100, 10);

  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i] is TTrackBar then
    begin
      if Components[i].Tag <> 0 then
      begin
        tb := TTrackBar(Components[i]);
        tb.OnChange := @UserInputChange;
        fse := TFloatSpinEdit(FindComponentByTag(Components[i].Tag, 'TFloatSpinEdit'));
        if (fse <> nil) then
        begin
          fse.MinValue := tb.Min;
          fse.MaxValue := tb.Max;
          fse.OnChange := @UserInputChange;
        end;
      end;
    end;
  end;

  Hex_edt.OnChange := @UserInputChange;
  Dec_edt.OnChange := @UserInputChange;
  Name_edt.OnChange := @UserInputChange;

  gamma_se.Value := BGRAGetGamma;
  gamma_tb.Position := round(gamma_se.Value * 100);

  col.AsBGRAPixel := BGRABlack;
  UpdateColorControls(-1);
  ChangingColors := False;
end;

procedure TForm1.UserInputChange(Sender: TObject);
var
  t: integer;
  v: single;
begin
  if ChangingColors then
    Exit;
  ChangingColors := True;
  t := TComponent(Sender).Tag;
  if Sender is TFloatSpinEdit then
  begin
    if not TryStrToFloat(TFloatSpinEdit(Sender).Text, v) then
      v := 0;
    TTrackBar(FindComponentByTag(t, 'TTrackBar')).Position := round(v);
  end;
  if Sender is TTrackBar then
    TFloatSpinEdit(FindComponentByTag(t, 'TFloatSpinEdit')).Text := IntToStr(TTrackBar(Sender).Position);

  UpdateColorControls(t);

  ChangingColors := False;
end;

procedure TForm1.UpdateColorControls(SourceTag: integer);
var
  i: integer;
  tb: TTrackBar;
  fse: TFloatSpinEdit;
begin
  case SourceTag of
    1, 2, 3: col.AsLinearRGBA := TLinearRGBA.New(lR_se.Value / 100, lG_se.Value / 100, lB_se.Value / 100, Alpha_se.Value / 100);
    4, 5, 6: col.AsStdHSLA := TStdHSLA.New(H_se.Value, S_se.Value / 100, L_se.Value / 100, Alpha_se.Value / 100);
    7, 8, 9: col.AsStdHSVA := TStdHSVA.New(H2_se.Value, S2_se.Value / 100, V_se.Value / 100, Alpha_se.Value / 100);
    17, 18, 19: col.AsXYZA := TXYZA.New(X_se.Value / 100, Y2_se.Value / 100, Z_se.Value / 100, Alpha_se.Value / 100);
    10, 11, 12: col.AsLabA := TLabA.New(L2_se.Value, A_se.Value, B2_se.Value, Alpha_se.Value / 100);
    13, 14, 15, 16: col.AsStdCMYK := TStdCMYK.New(C_se.Value / 100, M_se.Value / 100, Y_se.Value / 100, K_se.Value / 100);
    20, 21, 22: col.AsLChA := TLChA.New(L3_se.Value, C2_se.Value, H3_se.Value, Alpha_se.Value / 100);
    23: col.AsHex := Hex_edt.Text;
    24: col.AsDecimal := StrToInt(Dec_edt.Text);
    25, 26, 27: col.AsStdRGBA := TStdRGBA.New(sR_se.Value / 255, sG_se.Value / 255, sB_se.Value / 255, Alpha_se.Value / 100);
    28: col.Name := Name_edt.Text;
    29: col.AlphaPercent := Alpha_se.Value;
    50, 51, 52: col.AsHSLAPixel := THSLAPixel.New(round(lH_se.Value/360*65536) and $ffff, round(lS_se.Value / 100*65535), round(lL_se.Value / 100*65535), round(Alpha_se.Value / 100*65535));
    53, 54, 55: col.AsGSBAPixel := TGSBAPixel.New(round(lH2_se.Value/360*65536) and $ffff, round(lS2_se.Value / 100*65535), round(lL2_se.Value / 100*65535), round(Alpha_se.Value / 100*65535));
    60, 61, 62: col.AsAdobeRGBA := TAdobeRGBA.New(round(lR_se1.Value), round(lG_se1.Value), round(lB_se1.Value), round(Alpha_se.Value / 100 * 255));
  end;

  if not (SourceTag in [1, 2, 3]) then
    with col.AsLinearRGBA do
    begin
      lR_se.Value := red * 100;
      lG_se.Value := green * 100;
      lB_se.Value := blue * 100;
    end;
  if not (SourceTag in [4, 5, 6]) then
    with col.AsStdHSLA do
    begin
      H_se.Value := hue;
      S_se.Value := saturation * 100;
      L_se.Value := lightness * 100;
    end;
  if not (SourceTag in [7, 8, 9]) then
    with col.AsStdHSVA do
    begin
      H2_se.Value := hue;
      S2_se.Value := saturation * 100;
      V_se.Value := value * 100;
    end;
  if not (SourceTag in [17, 18, 19]) then
    with col.AsXYZA do
    begin
      X_se.Value := X * 100;
      Y2_se.Value := Y * 100;
      Z_se.Value := Z * 100;
    end;
  if not (SourceTag in [10, 11, 12]) then
    with col.AsLabA do
    begin
      L2_se.Value := L;
      A_se.Value := a;
      B2_se.Value := b;
    end;
  if not (SourceTag in [25, 26, 27]) then
    with col.AsStdRGBA do
    begin
      sR_se.Value := red * 255;
      sG_se.Value := green * 255;
      sB_se.Value := blue * 255;
    end;
  if not (SourceTag in [13, 14, 15, 16]) then
    with col.AsStdCMYK do
    begin
      C_se.Value := C * 100;
      M_se.Value := M * 100;
      Y_se.Value := Y * 100;
      K_se.Value := K * 100;
    end;
  if not (SourceTag in [20, 21, 22]) then
    with col.AsLchA do
    begin
      L3_se.Value := L;
      C2_se.Value := c;
      H3_se.Value := h;
    end;
  if SourceTag <> 23 then
    Hex_edt.Text := col.AsHex;
  if SourceTag <> 24 then
    Dec_edt.Text := IntToStr(col.AsDecimal);
  if SourceTag <> 28 then
    Name_edt.Text := col.Name;
  if SourceTag <> 29 then
    Alpha_se.Value := col.AlphaPercent;
  if not (SourceTag in [50,51,52]) then
    with col.AsHSLAPixel do
    begin
      lH_se.Value := hue/65536 * 360;
      lS_se.Value := saturation/65535 * 100;
      lL_se.Value := lightness/65535 * 100;
    end;
  if not (SourceTag in [53,54,55]) then
    with col.AsGSBAPixel do
    begin
      lH2_se.Value := hue/65536 * 360;
      lS2_se.Value := saturation/65535 * 100;
      lL2_se.Value := lightness/65535 * 100;
    end;
  if not (SourceTag in [60,61,62]) then
    with col.AsAdobeRGBA do
    begin
      lR_se1.Value := red;
      lG_se1.Value := green;
      lB_se1.Value := blue;
    end;

  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i] is TTrackBar then
    begin
      if Components[i].Tag <> 0 then
      begin
        tb := TTrackBar(Components[i]);
        tb.OnChange := @UserInputChange;
        fse := TFloatSpinEdit(FindComponentByTag(Components[i].Tag, 'TFloatSpinEdit'));
        if (fse <> nil) then
          tb.Position := round(fse.Value);
      end;
    end;
  end;

  Color_pnl.Color := col.AsColor;
  Grayscale_pnl.Color := col.AsGrayscale.AsColor;
  Invert_pnl.Color := col.AsInvert.AsColor;
end;

function TForm1.FindComponentByTag(ATag: integer; AClassName: string): TComponent;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ComponentCount - 1 do
    if (Components[i].Tag = ATag) and (Components[i].ClassName = AClassName) then
      Result := Components[i];
end;

end.
