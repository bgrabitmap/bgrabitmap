unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, BCButton, BGRAVirtualScreen, BGRALayers, BGRAStreamLayers,
  BGRABitmap, BGRABitmapTypes, BGRALayerOriginal;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCFlipX: TBCButton;
    BCFlipY: TBCButton;
    BCRotCW: TBCButton;
    BCRotCCW: TBCButton;
    BCColor1: TBCButton;
    BCColor2: TBCButton;
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    ColorDialog1: TColorDialog;
    cbInterp: TComboBox;
    cbRepeat: TComboBox;
    cbGradientType: TComboBox;
    procedure BCFlipXClick(Sender: TObject);
    procedure BCFlipYClick(Sender: TObject);
    procedure BCRotCWClick(Sender: TObject);
    procedure BCRotCCWClick(Sender: TObject);
    procedure BCColor1Click(Sender: TObject);
    procedure BCColor2Click(Sender: TObject);
    procedure BGRAVirtualScreen1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1MouseMove(Sender: TObject; {%H-}Shift: TShiftState;
      {%H-}X, {%H-}Y: Integer);
    procedure BGRAVirtualScreen1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure cbGradientTypeChange(Sender: TObject);
    procedure cbInterpChange(Sender: TObject);
    procedure cbRepeatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure OriginalChange(Sender: TObject; {%H-}AOriginal: TBGRALayerCustomOriginal);
  public
    FLayers: TBGRALayeredBitmap;
  end;

var
  Form1: TForm1;

implementation

uses BGRATransform, BGRASVGOriginal, BGRAGradientOriginal;

{$R *.lfm}

{ TForm1 }

procedure TForm1.BCFlipXClick(Sender: TObject);
begin
  FLayers.HorizontalFlip;
end;

procedure TForm1.BCFlipYClick(Sender: TObject);
begin
  FLayers.VerticalFlip;
end;

procedure TForm1.BCRotCWClick(Sender: TObject);
begin
  FLayers.RotateCW;
end;

procedure TForm1.BCRotCCWClick(Sender: TObject);
begin
  FLayers.RotateCCW;
end;

procedure TForm1.BCColor1Click(Sender: TObject);
var
  orig: TBGRALayerGradientOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerGradientOriginal;
  ColorDialog1.Color:= orig.StartColor;
  if ColorDialog1.Execute then orig.StartColor := ColorToBGRA(ColorDialog1.Color);
end;

procedure TForm1.BCColor2Click(Sender: TObject);
var
  orig: TBGRALayerGradientOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerGradientOriginal;
  ColorDialog1.Color:= orig.EndColor;
  if ColorDialog1.Execute then orig.EndColor := ColorToBGRA(ColorDialog1.Color);
end;

procedure TForm1.BGRAVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  newCursor: TOriginalEditorCursor;
begin
  FLayers.MouseDown(Button=mbRight,Shift,X,Y,newCursor);
  case newCursor of
  oecDefault: BGRAVirtualScreen1.Cursor := crDefault;
  oecMove: BGRAVirtualScreen1.Cursor := crSize;
  end;
end;

procedure TForm1.BGRAVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  newCursor: TOriginalEditorCursor;
begin
  FLayers.MouseMove(Shift,X,Y,newCursor);
  case newCursor of
  oecDefault: BGRAVirtualScreen1.Cursor := crDefault;
  oecMove: BGRAVirtualScreen1.Cursor := crSize;
  end;
end;

procedure TForm1.BGRAVirtualScreen1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  newCursor: TOriginalEditorCursor;
begin
  FLayers.MouseUp(Button=mbRight,Shift,X,Y,newCursor);
  case newCursor of
  oecDefault: BGRAVirtualScreen1.Cursor := crDefault;
  oecMove: BGRAVirtualScreen1.Cursor := crSize;
  end;
end;

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  FLayers.Draw(Bitmap, 0,0);
  FLayers.DrawEditor(Bitmap, 0, 0,0, 8);
end;

procedure TForm1.cbGradientTypeChange(Sender: TObject);
var
  orig: TBGRALayerGradientOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerGradientOriginal;
  orig.GradientType := TGradientType(cbGradientType.ItemIndex);
end;

procedure TForm1.cbInterpChange(Sender: TObject);
var
  orig: TBGRALayerGradientOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerGradientOriginal;
  orig.ColorInterpolation := TBGRAColorInterpolation(cbInterp.ItemIndex);
end;

procedure TForm1.cbRepeatChange(Sender: TObject);
var
  orig: TBGRALayerGradientOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerGradientOriginal;
  orig.Repetition := TBGRAGradientRepetition(cbRepeat.ItemIndex);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  grad: TBGRALayerGradientOriginal;
  svg: TBGRALayerSVGOriginal;
  img: TBGRALayerImageOriginal;
  idxBike, idxImg: Integer;
begin
  FLayers := TBGRALayeredBitmap.Create(640,480);
  flayers.OnOriginalChange := @OriginalChange;

  cbGradientType.ItemIndex:= 0;
  cbInterp.ItemIndex := 0;
  cbRepeat.ItemIndex := 0;

  grad := TBGRALayerGradientOriginal.Create;
  grad.StartColor := CSSSkyBlue;
  grad.EndColor := CSSOrange;
  grad.GradientType:= gtLinear;
  grad.Origin := PointF(FLayers.Width/2,100);
  grad.XAxis := grad.origin+PointF(0,250);
  grad.SaveToFile(Application.Location + 'grad.data');    //save original definition
  grad.Free;

  grad := TBGRALayerGradientOriginal.Create;
  grad.LoadFromFile(Application.Location + 'grad.data'); // load original definition
  FLayers.AddLayerFromOwnedOriginal(grad);

  svg := TBGRALayerSVGOriginal.Create;
  svg.LoadFromFile(Application.Location + 'bicycling.svg');
  idxBike := FLayers.AddLayerFromOwnedOriginal(svg);
  FLayers.LayerOpacity[idxBike] := 192;
  FLayers.LayerOriginalMatrix[idxBike] := AffineMatrixTranslation((FLayers.Width-svg.Width*0.5)/2,(FLayers.Height-svg.Height*0.5)/2)*
                                          AffineMatrixScale(0.5,0.5)*
                                          AffineMatrixTranslation(svg.Width/2,svg.Height/2)*
                                          AffineMatrixRotationDeg(30)*
                                          AffineMatrixTranslation(-svg.Width/2,-svg.Height/2);
  FLayers.RenderLayerFromOriginal(idxBike);

  img := TBGRALayerImageOriginal.Create;
  img.LoadFromFile(Application.Location + 'lazarus.jpg');
  idxImg := FLayers.AddLayerFromOwnedOriginal(img);
  FLayers.LayerOriginalMatrix[idxImg] := AffineMatrixTranslation(0, FLayers.Height - img.Height);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FLayers.Free;
end;

procedure TForm1.OriginalChange(Sender: TObject;
  AOriginal: TBGRALayerCustomOriginal);
begin
  BGRAVirtualScreen1.DiscardBitmap;
end;

end.

