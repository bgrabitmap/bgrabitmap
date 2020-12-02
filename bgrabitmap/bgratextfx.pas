// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRATextFX;

{$mode objfpc}{$H+}

{
  Font rendering units : BGRAText, BGRATextFX, BGRAVectorize, BGRAFreeType

  This unit provide text effects. The simplest way to render effects is to use TBGRATextEffectFontRenderer class.
  To do this, create an instance of this class and assign it to a TBGRABitmap.FontRenderer property. Now functions
  to draw text like TBGRABitmap.TextOut will use the chosen renderer. To set the effects, keep a variable containing
  the TBGRATextEffectFontRenderer class and modify ShadowVisible and other effects parameters.

  The TBGRATextEffectFontRenderer class makes use of other classes depending on the situation. For example,
  TBGRATextEffect, which is also in this unit, provides effects on a text mask. But the renderer also uses
  BGRAVectorize unit in order to have big texts or to rotate them at will.

  Note that you may need TBGRATextEffect if you want to have more control over text effects, especially
  if you always draw the same text. Keeping the same TBGRATextEffect object will avoid creating the text
  mask over and over again.

  TextShadow function is a simple function to compute an image containing a text with shadow.

}

interface

uses
  BGRAClasses, SysUtils, BGRAGraphics, BGRABitmapTypes, BGRAPhongTypes, BGRAText,
  BGRACustomTextFX, BGRAVectorize;

type
  TBGRATextEffect = class;

  { TBGRATextEffectFontRenderer }

  TBGRATextEffectFontRenderer = class(TBGRASystemFontRenderer)
  private
    function GetShaderLightPosition: TPoint;
    function GetShaderLightPositionF: TPointF;
    function GetVectorizedRenderer: TBGRAVectorizedFontRenderer;
    procedure SetShaderLightPosition(const AValue: TPoint);
    procedure SetShaderLightPositionF(const AValue: TPointF);
  protected
    FShaderOwner: boolean;
    FShader: TCustomPhongShading;
    FVectorizedRenderer: TBGRAVectorizedFontRenderer;
    function ShadowActuallyVisible :boolean;
    function ShaderActuallyActive: boolean;
    function OutlineActuallyVisible: boolean;
    procedure Init;
    function VectorizedFontNeeded(AOrientation: integer): boolean;
    procedure InternalTextOutAngle(ADest: TBGRACustomBitmap; x, y: single; AOrientation: integer; sUTF8: string; c: TBGRAPixel; texture: IBGRAScanner;
                              align: TAlignment; AShowPrefix: boolean = false; ARightToLeft: boolean = false); override;
  public
    ShaderActive: boolean;

    ShadowVisible: boolean;
    ShadowColor: TBGRAPixel;
    ShadowRadius: integer;
    ShadowOffset: TPoint;
    ShadowQuality: TRadialBlurType;

    OutlineColor: TBGRAPixel;
    OutlineWidth: single;
    OutlineVisible,OuterOutlineOnly: boolean;
    OutlineJoin: TPenJoinStyle;
    OutlineTexture: IBGRAScanner;
    constructor Create; overload;
    constructor Create(AShader: TCustomPhongShading; AShaderOwner: boolean); overload;
    destructor Destroy; override;
    function TextSize(sUTF8: string): TSize; overload; override;
    function TextSizeAngle(sUTF8: string; orientationTenthDegCCW: integer): TSize; override;
    function TextSize(sUTF8: string; AMaxWidth: integer; {%H-}ARightToLeft: boolean): TSize; overload; override;
    function TextFitInfo(sUTF8: string; AMaxWidth: integer): integer; override;
    property Shader: TCustomPhongShading read FShader;
    property ShaderLightPosition: TPoint read GetShaderLightPosition write SetShaderLightPosition;
    property ShaderLightPositionF: TPointF read GetShaderLightPositionF write SetShaderLightPositionF;
    property VectorizedFontRenderer: TBGRAVectorizedFontRenderer read GetVectorizedRenderer;
  end;

  { TBGRATextEffect }

  TBGRATextEffect = class(TBGRACustomTextEffect)
  protected
    procedure InitImproveReadability(AText: string; Font: TFont; SubOffsetX,SubOffsetY: single);
    procedure Init(AText: string; Font: TFont; Antialiasing: boolean; SubOffsetX,SubOffsetY: single; GrainX, GrainY: Integer);
    procedure InitWithFontName(AText: string; AFontName: string; AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean; SubOffsetX,SubOffsetY: single);
  public
    constructor Create(AText: string; Font: TFont; Antialiasing: boolean); overload;
    constructor Create(AText: string; Font: TFont; Antialiasing: boolean; SubOffsetX,SubOffsetY: single); overload;
    constructor Create(AText: string; Font: TFont; Antialiasing: boolean; SubOffsetX,SubOffsetY: single; GrainX, GrainY: Integer); overload;
    constructor Create(AText: string; AFontName: string; AFullHeight: integer; Antialiasing: boolean); overload;
    constructor Create(AText: string; AFontName: string; AFullHeight: integer; Antialiasing: boolean; SubOffsetX,SubOffsetY: single); overload;
    constructor Create(AText: string; AFontName: string; AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean); overload;
    constructor Create(AText: string; AFontName: string; AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean; SubOffsetX,SubOffsetY: single); overload;
  end;

function TextShadow(AWidth,AHeight: Integer; AText: String; AFontHeight: Integer; ATextColor,AShadowColor: TBGRAPixel;
    AOffSetX,AOffSetY: Integer; ARadius: Integer = 0; AFontStyle: TFontStyles = []; AFontName: String = 'Default'; AShowText: Boolean = True; AFontQuality: TBGRAFontQuality = fqFineAntialiasing): TBGRACustomBitmap;

procedure BGRATextOutImproveReadability(bmp: TBGRACustomBitmap; AFont: TFont; xf,yf: single; text: string; color: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; mode : TBGRATextOutImproveReadabilityMode);

implementation

uses BGRAGradientScanner, Math, BGRAGrayscaleMask, BGRAPath, BGRATransform,
  BGRAPolygon, BGRAPen;

procedure BGRATextOutImproveReadability(bmp: TBGRACustomBitmap; AFont: TFont; xf,yf: single; text: string; color: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; mode : TBGRATextOutImproveReadabilityMode);
var
  useClearType,clearTypeRGBOrder: boolean;
  metric: TFontPixelMetric;
  deltaX: single;
  x,y,yb,cury,fromy: integer;
  toAdd: integer;
  lines: array[0..3] of integer;
  parts: array[0..3] of TGrayscaleMask;
  n,nbLines: integer;
  alphaMax: UInt32or64;
  pmask: PByte;
  fx: TBGRATextEffect;
  FxFont: TFont;
  prevCenter, newCenter, diffCenter: single;
  xThird: integer;

begin
  useClearType:= mode in[irClearTypeRGB,irClearTypeBGR];
  clearTypeRGBOrder := mode <> irClearTypeBGR;
  deltaX := xf-floor(xf);
  x := round(floor(xf));

  FxFont := TFont.Create;
  FxFont.Assign(AFont);
  FxFont.Height := fxFont.Height*FontAntialiasingLevel;
  metric := GetLCLFontPixelMetric(FxFont);
  if not metric.Defined or (metric.Lineheight < 8*FontAntialiasingLevel) or (metric.Lineheight >= 24*FontAntialiasingLevel) then
  begin
    fxFont.Free;
    if useClearType then
    begin
      if ClearTypeRGBOrder then
        BGRATextOut(bmp, AFont, fqFineClearTypeRGB, xf,yf, text, color, tex, align)
      else
        BGRATextOut(bmp, AFont, fqFineClearTypeBGR, xf,yf, text, color, tex, align)
    end else
      BGRATextOut(bmp, AFont, fqFineAntialiasing, xf,yf, text, color, tex, align);
    exit;
  end;

  if (metric.Baseline-metric.xLine) mod FontAntialiasingLevel >= FontAntialiasingLevel div 3 then
  begin
    toAdd := FontAntialiasingLevel- ((metric.Baseline-metric.xLine) mod FontAntialiasingLevel);
    for yb := 1 to toAdd div 2 do
    begin
      if metric.xLine > 0 then dec(metric.xLine);
      if metric.Baseline < metric.Lineheight then inc(metric.Baseline);
    end;
  end;
  if metric.CapLine >= metric.xLine then metric.CapLine := -1 else
  begin
    if (metric.xLine-metric.CapLine) mod FontAntialiasingLevel >= FontAntialiasingLevel div 2 then
    begin
      toAdd := FontAntialiasingLevel - (metric.xLine-metric.CapLine) mod FontAntialiasingLevel;
      dec(metric.CapLine, toAdd);
      if metric.CapLine <= 0 then metric.CapLine := -1;
    end;
  end;

  nbLines := 0;
  lines[nbLines] := metric.CapLine+1;
  inc(nbLines);
  lines[nbLines] := metric.xLine+1;
  inc(nbLines);
  lines[nbLines] := metric.Baseline+1;
  inc(nbLines);
  lines[nbLines] := metric.Lineheight+1;
  inc(nbLines);

  if not useClearType then
    fx := TBGRATextEffect.Create(text,FxFont,False,deltaX*FontAntialiasingLevel,0,FontAntialiasingLevel,FontAntialiasingLevel) else
    fx := TBGRATextEffect.Create(text,FxFont,False,0,0,3,0);

  if fx.TextMask = nil then
  begin
    fx.Free;
    FxFont.Free;
    exit;
  end;
  alphaMax := 0;
  prevCenter := 0;
  newCenter := 0;
  for yb := 0 to nbLines-1 do
  begin
    if yb= 0 then fromy := 0
     else fromy := lines[yb-1];

    if lines[yb] > fromy then
    begin
      if useClearType then
        parts[yb] := TGrayscaleMask.CreateDownSample(fx.TextMask,
                       round(fx.TextMask.Width / FontAntialiasingLevel * 3),
                       round((lines[yb] - fromy) / FontAntialiasingLevel),
                       rect(0, fromy, fx.TextMask.Width, lines[yb]) )
      else
        parts[yb] := TGrayscaleMask.CreateDownSample(fx.TextMask,
                       round(fx.TextMask.Width / FontAntialiasingLevel),
                       round((lines[yb] - fromy) / FontAntialiasingLevel),
                       rect(0, fromy, fx.TextMask.Width, lines[yb]) );

      if alphaMax < 255 then
      begin
        pmask := parts[yb].Data;
        for n := parts[yb].NbPixels-1 downto 0 do
        begin
          if pmask^ > alphaMax then alphaMax := pmask^;
          inc(pmask);
        end;
      end;

      if yb < 2 then
      begin
        IncF(newCenter, parts[yb].Height);
        IncF(prevCenter, lines[yb]-fromy);
      end else
      if yb = 2 then
      begin
        IncF(newCenter, parts[yb].Height/2);
        IncF(prevCenter, (lines[yb]-fromy)/2);
      end;
    end else
      parts[yb] := nil;
  end;

  prevCenter := prevCenter / FontAntialiasingLevel;
  diffCenter := prevCenter-newCenter;
  y := round( yf + diffCenter );

  xThird := 0;
  if useClearType then
  begin
    case align of
    taCenter: xThird:= xThird+round(((fx.TextMaskOffset.x-fx.TextWidth/2)/FontAntialiasingLevel+deltaX)*3);
    taRightJustify: xThird:= xThird+round(((fx.TextMaskOffset.x-fx.TextWidth)/FontAntialiasingLevel+deltaX)*3);
    else xThird:= xThird+round((fx.TextMaskOffset.x/FontAntialiasingLevel+deltaX)*3);
    end;
  end else
  begin
    case align of
    taCenter: x:= x+round((fx.TextMaskOffset.x-fx.TextWidth/2)/FontAntialiasingLevel);
    taRightJustify: x:= x+round((fx.TextMaskOffset.x-fx.TextWidth)/FontAntialiasingLevel);
    else x:= x+round(fx.TextMaskOffset.x/FontAntialiasingLevel);
    end;
  end;
  cury := y+round(fx.TextMaskOffset.y/FontAntialiasingLevel);
  for yb := 0 to nbLines-1 do
  if parts[yb] <> nil then
  begin
    if (alphaMax > 0) and (alphaMax < 255) then
    begin
      pmask := parts[yb].data;
      for n := parts[yb].NbPixels-1 downto 0 do
      begin
        pmask^ := pmask^*255 div alphaMax;
        inc(pmask);
      end;
    end;
    if useClearType then
      BGRAFillClearTypeGrayscaleMask(bmp,x,cury,xThird,parts[yb],color,tex,ClearTypeRGBOrder)
    else if mode = irMask then
      parts[yb].Draw(bmp,x,cury)
    else
    begin
      if tex <> nil then
        parts[yb].DrawAsAlpha(bmp,x,cury,tex) else
        parts[yb].DrawAsAlpha(bmp,x,cury,color);
    end;
    inc(cury,parts[yb].Height);
    parts[yb].Free;
  end;

  fx.Free;
  FxFont.Free;
end;

procedure BGRAReplace(var Destination: TBGRACustomBitmap; Temp: TObject);
begin
  Destination.Free;
  Destination := Temp as TBGRACustomBitmap;
end;

function TextShadow(AWidth,AHeight: Integer; AText: String; AFontHeight: Integer; ATextColor,AShadowColor: TBGRAPixel;
  AOffSetX,AOffSetY: Integer; ARadius: Integer = 0; AFontStyle: TFontStyles = []; AFontName: String = 'Default'; AShowText: Boolean = True;
  AFontQuality: TBGRAFontQuality = fqFineAntialiasing): TBGRACustomBitmap;
var
  bmpOut,bmpSdw: TBGRACustomBitmap; OutTxtSize: TSize; OutX,OutY: Integer;
begin
  bmpOut:= BGRABitmapFactory.Create(AWidth,AHeight);
  bmpOut.FontAntialias:= True;
  bmpOut.FontHeight:= AFontHeight;
  bmpOut.FontStyle:= AFontStyle;
  bmpOut.FontName:= AFontName;
  bmpOut.FontQuality:= AFontQuality;

  OutTxtSize:= bmpOut.TextSize(AText);
  OutX:= Round(AWidth/2) - Round(OutTxtSize.cx/2);
  OutY:= Round(AHeight/2) - Round(OutTxtSize.cy/2);

  bmpSdw:= BGRABitmapFactory.Create(OutTxtSize.cx+2*ARadius,OutTxtSize.cy+2*ARadius);
  bmpSdw.FontAntialias:= True;
  bmpSdw.FontHeight:= AFontHeight;
  bmpSdw.FontStyle:= AFontStyle;
  bmpSdw.FontName:= AFontName;
  bmpSdw.FontQuality:= AFontQuality;

  bmpSdw.TextOut(ARadius,ARadius,AText,AShadowColor);
  BGRAReplace(bmpSdw,bmpSdw.FilterBlurRadial(ARadius,rbFast));
  bmpOut.PutImage(OutX+AOffSetX-ARadius,OutY+AOffSetY-ARadius,bmpSdw,dmDrawWithTransparency);
  bmpSdw.Free;

  if AShowText = True then bmpOut.TextOut(OutX,OutY,AText,ATextColor);

  Result:= bmpOut;
end;

{ TBGRATextEffectFontRenderer }

function TBGRATextEffectFontRenderer.GetShaderLightPosition: TPoint;
begin
  if FShader = nil then
    result := point(0,0)
  else
    result := FShader.LightPosition;
end;

function TBGRATextEffectFontRenderer.GetShaderLightPositionF: TPointF;
begin
  if FShader = nil then
    result := pointF(0,0)
  else
    result := FShader.LightPositionF;
end;

function TBGRATextEffectFontRenderer.GetVectorizedRenderer: TBGRAVectorizedFontRenderer;
begin
  FVectorizedRenderer.FontEmHeight := FontEmHeight;
  FVectorizedRenderer.FontName := FontName;
  FVectorizedRenderer.FontOrientation:= FontOrientation;
  FVectorizedRenderer.FontQuality := FontQuality;
  FVectorizedRenderer.FontStyle:= FontStyle;

  FVectorizedRenderer.ShadowColor := ShadowColor;
  FVectorizedRenderer.ShadowVisible := ShadowVisible;
  FVectorizedRenderer.ShadowOffset := ShadowOffset;
  FVectorizedRenderer.ShadowRadius := ShadowRadius;

  FVectorizedRenderer.OutlineColor := OutlineColor;
  FVectorizedRenderer.OutlineVisible := OutlineVisible;
  FVectorizedRenderer.OutlineWidth := OutlineWidth;
  FVectorizedRenderer.OutlineTexture := OutlineTexture;
  FVectorizedRenderer.OuterOutlineOnly := OuterOutlineOnly;
  FVectorizedRenderer.OutlineJoin := OutlineJoin;
  result := FVectorizedRenderer;
end;

procedure TBGRATextEffectFontRenderer.SetShaderLightPosition(const AValue: TPoint);
begin
  if FShader <> nil then
    FShader.LightPosition := AValue;
end;

procedure TBGRATextEffectFontRenderer.SetShaderLightPositionF(const AValue: TPointF);
begin
  if FShader <> nil then
    FShader.LightPositionF := AValue;
end;

function TBGRATextEffectFontRenderer.ShadowActuallyVisible: boolean;
begin
  result := ShadowVisible and (ShadowColor.alpha <> 0);
end;

function TBGRATextEffectFontRenderer.ShaderActuallyActive: boolean;
begin
  result := (FShader <> nil) and ShaderActive;
end;

function TBGRATextEffectFontRenderer.OutlineActuallyVisible: boolean;
begin
  result := (OutlineWidth <> 0) and ((OutlineTexture <> nil) or (OutlineColor.alpha <> 0)) and OutlineVisible;
end;

procedure TBGRATextEffectFontRenderer.Init;
begin
  ShaderActive := true;

  ShadowColor := BGRABlack;
  ShadowVisible := false;
  ShadowOffset := Point(5,5);
  ShadowRadius := 5;
  ShadowQuality:= rbFast;

  OutlineColor := BGRAPixelTransparent;
  OutlineVisible := True;
  OutlineWidth:= DefaultOutlineWidth;
  OuterOutlineOnly:= false;
  OutlineJoin := pjsMiter;
  FVectorizedRenderer := TBGRAVectorizedFontRenderer.Create;
end;

function TBGRATextEffectFontRenderer.VectorizedFontNeeded(AOrientation: integer): boolean;
  function IsBigFont: boolean;
  var textsz: TSize;
  begin
    textsz := inherited InternalTextSize('Hg',False);
    result := (not OutlineActuallyVisible and (textsz.cy >= 24)) or
       (OutlineActuallyVisible and (textsz.cy > 42));
  end;
var bAntialiasing, bSpecialOutline, bOriented, bEffectVectorizedSupported: boolean;
begin
  bAntialiasing := FontQuality in [fqFineAntialiasing,fqFineClearTypeRGB,fqFineClearTypeBGR];
  bSpecialOutline:= OutlineActuallyVisible and (abs(OutlineWidth) <> DefaultOutlineWidth);
  bOriented := AOrientation <> 0;
  bEffectVectorizedSupported := OutlineActuallyVisible or ShadowActuallyVisible or ShaderActuallyActive;
  result := bSpecialOutline or
            (bAntialiasing and IsBigFont) or
            (bOriented and bEffectVectorizedSupported);
end;

procedure TBGRATextEffectFontRenderer.InternalTextOutAngle(
  ADest: TBGRACustomBitmap; x, y: single; AOrientation: integer; sUTF8: string; 
  c: TBGRAPixel; texture: IBGRAScanner; align: TAlignment;  
  AShowPrefix: boolean = false; ARightToLeft: boolean = false);

  procedure DrawFX(fx: TBGRATextEffect; x,y: single; outline: boolean);
    procedure DoOutline;
    begin
      if OutlineActuallyVisible then
      begin
        if OutlineTexture <> nil then
          fx.DrawOutline(ADest,round(x),round(y), OutlineTexture, align)
        else
          fx.DrawOutline(ADest,round(x),round(y), OutlineColor, align);
      end;
    end;
  begin
    if ShadowActuallyVisible then
    begin
      fx.ShadowQuality := ShadowQuality;
      fx.DrawShadow(ADest,round(x)+ShadowOffset.X,round(y)+ShadowOffset.Y,ShadowRadius,ShadowColor, align);
    end;
    if outline and OuterOutlineOnly then DoOutline;
    if texture <> nil then
    begin
      if ShaderActuallyActive then
        fx.DrawShaded(ADest,floor(x),floor(y), Shader, round(fx.TextSize.cy*0.05), texture, align)
      else
        fx.Draw(ADest,round(x),round(y), texture, align);
    end else
    begin
      if ShaderActuallyActive then
        fx.DrawShaded(ADest,floor(x),floor(y), Shader, round(fx.TextSize.cy*0.05), c, align)
      else
        fx.Draw(ADest,round(x),round(y), c, align);
    end;
    if outline and not OuterOutlineOnly then DoOutline;
  end;

  procedure ComplexVectorized;
  var
    p: TBGRAPath;
    w: single;
    f: TBGRAMultishapeFiller;
    s: TBGRAPenStroker;
    mask, shaded: TBGRACustomBitmap;
    boundsF: TRectF;
    b: TRect;
    fx: TBGRATextEffect;
    oldShaderLightPos: TPoint;
    h: integer;
  begin
    p := TBGRAPath.Create;
    try
      p.translate(x-0.5,y-0.5);
      p.rotateDeg(-AOrientation/10);
      VectorizedFontRenderer.CopyTextPathTo(p,0,0,sUTF8,align,ARightToLeft);
      if abs(OutlineWidth) < 3 then
        w := abs(OutlineWidth)*2/3
      else
        w := abs(OutlineWidth)-1;
      if p.IsEmpty then
        boundsF := EmptyRectF
      else
      begin
        boundsF := p.GetBounds;
        DecF(boundsF.Left, 1);
        DecF(boundsF.Top, 1);
        IncF(boundsF.Right, 1);
        IncF(boundsF.Bottom, 1);
        if ShadowActuallyVisible then
        begin
          DecF(boundsF.Left, ShadowRadius);
          DecF(boundsF.Top, ShadowRadius);
          IncF(boundsF.Right, ShadowRadius);
          IncF(boundsF.Bottom, ShadowRadius);
        end;
        boundsF := TRectF.Intersect(boundsF, RectF(0,0,ADest.Width,ADest.Height));
      end;
      if not boundsF.IsEmpty then
      begin
        with boundsF do
          b := rect(floor(left),floor(top),ceil(right),ceil(bottom));
        shaded := nil;
        try
          if ShaderActuallyActive or ShadowActuallyVisible then
          begin
            mask := BGRABitmapFactory.Create(b.Width,b.Height,BGRABlack);
            try
              mask.LinearAntialiasing:= true;
              mask.FillPath(p, AffineMatrixTranslation(-b.Left,-b.Top), BGRAWhite);
              fx := TBGRATextEffect.Create(mask, false, mask.Width,mask.Height, Point(0,0));
              if ShaderActuallyActive then
              begin
                shaded := ADest.NewBitmap(mask.Width,mask.Height);
                oldShaderLightPos := Shader.LightPosition;
                Shader.LightPosition := Point(Shader.LightPosition.X - b.Left,
                                              Shader.LightPosition.Y - b.Top); 
                h := VectorizedFontRenderer.TextSize('Hg').cy;
                if texture <> nil then
                  fx.DrawShaded(shaded, 0,0, Shader, round(h*0.05), texture, taLeftJustify)
                else
                  fx.DrawShaded(shaded, 0,0, Shader, round(h*0.05), c, taLeftJustify);
                Shader.LightPosition := oldShaderLightPos;
                shaded.AlphaFill(255);
                shaded.ScanOffset := Point(-b.Left,-b.Top);
              end;
              if ShadowActuallyVisible then
              begin
                fx.ShadowQuality := ShadowQuality;
                fx.DrawShadow(ADest,b.Left+ShadowOffset.X,b.Top+ShadowOffset.Y,ShadowRadius,ShadowColor, taLeftJustify);
              end;
              fx.Free;
            finally
              mask.Free;
            end;
          end;
          s := nil;
          f := TBGRAMultishapeFiller.Create;
          if shaded<>nil then
            f.AddPathFill(p, shaded) else
          if texture<>nil then
            f.AddPathFill(p, texture)
          else
            f.AddPathFill(p, c);
          if OutlineActuallyVisible then
          begin
            s := TBGRAPenStroker.Create;
            f.AddPathStroke(p, OutlineColor, w, s);
          end;
          if OuterOutlineOnly then
            f.PolygonOrder:= poFirstOnTop
          else
            f.PolygonOrder:= poLastOnTop;
          if ADest.LinearAntialiasing then
            f.Draw(ADest, dmLinearBlend)
          else
            f.Draw(ADest, dmDrawWithTransparency);
          f.Free;
          s.Free;
        finally
          shaded.Free;
        end;
      end;
    finally
      p.Free;
    end;
  end;

var fx: TBGRATextEffect;
begin
  if VectorizedFontNeeded(AOrientation) then
  begin
    if ShaderActuallyActive or ShadowActuallyVisible then
      ComplexVectorized else
    begin
      if texture<>nil then
        VectorizedFontRenderer.TextOutAngle(ADest,x,y,AOrientation,sUTF8,texture,align,ARightToLeft)
      else
        VectorizedFontRenderer.TextOutAngle(ADest,x,y,AOrientation,sUTF8,c,align,ARightToLeft);
    end;
  end else
  if (AOrientation = 0) and (ShaderActuallyActive or ShadowActuallyVisible or OutlineActuallyVisible) then
  begin
    fx := TBGRATextEffect.Create(sUTF8, FFont, 
       FontQuality in[fqFineAntialiasing,fqFineClearTypeBGR,fqFineClearTypeRGB], 
       x-floor(x), y-floor(y));
    DrawFX(fx, x,y, true);
    fx.Free;
  end else
    inherited InternalTextOutAngle(ADest,x,y,AOrientation,sUTF8,c,texture,align,AShowPrefix,ARightToLeft);
end;

constructor TBGRATextEffectFontRenderer.Create;
begin
  inherited Create;
  FShader := nil;
  FShaderOwner:= false;
  Init;
end;

constructor TBGRATextEffectFontRenderer.Create(AShader: TCustomPhongShading;
  AShaderOwner: boolean);
begin
  inherited Create;
  Init;
  FShader := AShader;
  FShaderOwner := AShaderOwner;
end;

destructor TBGRATextEffectFontRenderer.Destroy;
begin
  if FShaderOwner then FShader.Free;
  FVectorizedRenderer.Free;
  inherited Destroy;
end;

function TBGRATextEffectFontRenderer.TextSize(sUTF8: string): TSize;
begin
  if VectorizedFontNeeded(0) then
    result := VectorizedFontRenderer.TextSize(sUTF8)
  else
    result := inherited TextSize(sUTF8);
end;

function TBGRATextEffectFontRenderer.TextSizeAngle(sUTF8: string; 
  orientationTenthDegCCW: integer): TSize; 
begin
  if VectorizedFontNeeded(orientationTenthDegCCW) then
    result := VectorizedFontRenderer.TextSizeAngle(sUTF8, orientationTenthDegCCW)
  else
    result := inherited TextSizeAngle(sUTF8, orientationTenthDegCCW); 
end;

function TBGRATextEffectFontRenderer.TextSize(sUTF8: string;
  AMaxWidth: integer; ARightToLeft: boolean): TSize;
begin
  if VectorizedFontNeeded(FontOrientation) then
    result := VectorizedFontRenderer.TextSize(sUTF8, AMaxWidth, ARightToLeft)
  else
    result := inherited TextSize(sUTF8, AMaxWidth, ARightToLeft);
end;

function TBGRATextEffectFontRenderer.TextFitInfo(sUTF8: string;
  AMaxWidth: integer): integer;
begin
  if VectorizedFontNeeded(FontOrientation) then
    result := VectorizedFontRenderer.TextFitInfo(sUTF8, AMaxWidth)
  else
    result := inherited TextFitInfo(sUTF8, AMaxWidth)
end;

{ TBGRATextEffect }

procedure TBGRATextEffect.InitImproveReadability(AText: string; Font: TFont;
  SubOffsetX, SubOffsetY: single);
var size: TSize;
  overhang: integer;
  temp: TBGRACustomBitmap;
begin
  FShadowQuality:= rbFast;
  if SubOffsetX < 0 then SubOffsetX := 0;
  if SubOffsetY < 0 then SubOffsetY := 0;
  size := BGRATextSize(Font, fqFineAntialiasing, AText, FontAntialiasingLevel);
  FTextSize := size;
  if size.cy = 0 then FTextSize.cy := BGRATextSize(Font, fqFineAntialiasing, 'Hg', FontAntialiasingLevel).cy;
  overhang := size.cy div 2;
  inc(size.cx, 2*overhang + ceil(SubOffsetX) );
  inc(size.cy, 2 + ceil(SubOffsetY) );

  FOffset := Point(-overhang,-1); //include overhang
  temp := BGRABitmapFactory.Create(size.cx, size.cy, BGRABlack);
  BGRATextOutImproveReadability(temp, Font, overhang+SubOffsetX,1+SubOffsetY, AText, BGRAWhite, nil, taLeftJustify, irMask);
  FTextMask := TGrayscaleMask.Create;
  FTextMask.CopyFrom(temp, cGreen);
  temp.Free;
end;

constructor TBGRATextEffect.Create(AText: string; Font: TFont;
  Antialiasing: boolean; SubOffsetX,SubOffsetY: single);
begin
  Init(AText, Font, Antialiasing, SubOffsetX, SubOffsetY, 0,0);
end;

constructor TBGRATextEffect.Create(AText: string; Font: TFont;
  Antialiasing: boolean; SubOffsetX, SubOffsetY: single; GrainX, GrainY: Integer
  );
begin
  Init(AText, Font, Antialiasing, SubOffsetX, SubOffsetY, GrainX, GrainY);
end;

constructor TBGRATextEffect.Create(AText: string; AFontName: string;
  AFullHeight: integer; Antialiasing: boolean);
begin
  InitWithFontName(AText, AFontName, AFullHeight, [], Antialiasing, 0, 0);
end;

constructor TBGRATextEffect.Create(AText: string; AFontName: string;
  AFullHeight: integer; Antialiasing: boolean; SubOffsetX, SubOffsetY: single);
begin
  InitWithFontName(AText, AFontName, AFullHeight, [], Antialiasing, SubOffsetX, SubOffsetY);
end;

constructor TBGRATextEffect.Create(AText: string; AFontName: string;
  AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean);
begin
  InitWithFontName(AText, AFontName, AFullHeight, AStyle, Antialiasing, 0, 0);
end;

constructor TBGRATextEffect.Create(AText: string; AFontName: string;
  AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean; SubOffsetX,
  SubOffsetY: single);
begin
  InitWithFontName(AText, AFontName, AFullHeight, AStyle, Antialiasing, SubOffsetX, SubOffsetY);
end;

procedure TBGRATextEffect.Init(AText: string; Font: TFont; Antialiasing: boolean; SubOffsetX,SubOffsetY: single; GrainX, GrainY: Integer);
const FXAntialiasingLevel = FontAntialiasingLevel;
var temp: TBGRACustomBitmap;
	tempBmp: TBitmap;
    size: TSize;
    p: PByte;
    n,v,maxAlpha: integer;
    alpha: byte;
    sizeX,sizeY: integer;
    onePixel: integer;
    quality: TBGRAFontQuality;
    iSubX,iSubY: integer;
begin
  if IsLclFontRendererFine then Antialiasing := false;
  FShadowQuality := rbFast;
  if Antialiasing and Assigned(BGRATextOutImproveReadabilityProc) then
  begin
    InitImproveReadability(AText, Font, SubOffsetX,SubOffsetY);
    exit;
  end;
  if Antialiasing and not IsLclFontRendererFine then
    quality := fqFineAntialiasing
  else
    quality := fqSystem;
  size := BGRAOriginalTextSize(Font,quality,AText,FXAntialiasingLevel);
  if (size.cx = 0) or (size.cy = 0) then
  begin
    size := BGRATextSize(Font,quality,'Hg',FXAntialiasingLevel);
    FTextSize.cx := 0;
    FTextSize.cy := size.cy;
    FOffset := Point(0,0);
    exit;
  end;
  FTextSize := size;

  sizeX := size.cx+size.cy;
  sizeY := size.cy;

  iSubX := 0;
  iSubY := 0;
  if SubOffsetX < 0 then SubOffsetX := 0;
  if SubOffsetY < 0 then SubOffsetY := 0;

  if Antialiasing then
  begin
    sizeX := (sizeX + FXAntialiasingLevel-1);
    dec(sizeX, sizeX mod FXAntialiasingLevel);

    sizeY := (sizeY + FXAntialiasingLevel-1);
    dec(sizeY, sizeY mod FXAntialiasingLevel);

    if SubOffsetX <> 0 then
    begin
      inc(sizeX, ceil(SubOffsetX*FXAntialiasingLevel) );
      iSubX := round(SubOffsetX*FXAntialiasingLevel);
    end;
    if SubOffsetY <> 0 then
    begin
      inc(sizeY, ceil(SubOffsetY*FXAntialiasingLevel) );
      iSubY := round(SubOffsetY*FXAntialiasingLevel);
    end;

    OnePixel := FXAntialiasingLevel;
  end else
  begin
    OnePixel := 1;

    if SubOffsetX <> 0 then
    begin
      iSubX := round(SubOffsetX);
      inc(sizeX, iSubX);
    end;
    if SubOffsetY <> 0 then
    begin
      iSubY := round(SubOffsetY);
      inc(sizeY, iSubY);
    end;
  end;
  FOffset := Point(-size.cy div 2,-OnePixel); //include overhang

  if GrainX > 0 then
  begin
    SizeX := SizeX+ (GrainX-1);
    dec(SizeX, SizeX mod GrainX);
  end;
  if GrainY > 0 then
  begin
    SizeY := SizeY+ (GrainY-1);
    dec(SizeY, SizeY mod GrainY);
  end;
  if RenderTextOnBitmap then
  begin
    tempBmp := TBitmap.Create;
    tempBmp.Width := sizeX;
    tempBmp.Height := sizeY+2*OnePixel;
    BitmapFillRect(tempBmp, rect(0,0,tempBmp.Width,tempBmp.Height), clBlack);
    tempBmp.Canvas.Font := Font;
    tempBmp.Canvas.Font.Orientation := 0;
    tempBmp.Canvas.Font.Height := Font.Height*OnePixel;
    tempBmp.Canvas.Font.Color := clWhite;
    tempBmp.Canvas.Font.Quality := FontDefaultQuality;
    BitmapTextOut(tempBmp, Point(-FOffset.X+iSubX, -FOffset.Y+iSubY), AText);
    temp := BGRABitmapFactory.Create(tempBmp);
    tempBmp.Free;
  end else
  begin
    temp := BGRABitmapFactory.Create(sizeX, sizeY+2*OnePixel,clBlack);
    temp.Canvas.Font := Font;
    temp.Canvas.Font.Orientation := 0;
    temp.Canvas.Font.Height := Font.Height*OnePixel;
    temp.Canvas.Font.Color := clWhite;
    temp.Canvas.Font.Quality := FontDefaultQuality;
    BitmapTextOut(temp.Bitmap, Point(-FOffset.X+iSubX, -FOffset.Y+iSubY), AText);
  end;

  if Antialiasing then
  begin
    FTextSize.cx := round(FTextSize.cx/FXAntialiasingLevel);
    FTextSize.cy := round(FTextSize.cy/FXAntialiasingLevel);
    FOffset := Point(round(FOffset.X/FXAntialiasingLevel),round(FOffset.Y/FXAntialiasingLevel));

    FTextMask := TGrayscaleMask.CreateDownSample(temp, round(temp.width/FXAntialiasingLevel),
                   round(temp.Height/FXAntialiasingLevel));
    temp.Free;

    maxAlpha := 0;
    p := FTextMask.Data;
    for n := FTextMask.NbPixels - 1 downto 0 do
    begin
      alpha    := P^;
      if alpha > maxAlpha then maxAlpha := alpha;
      Inc(p);
    end;
    if maxAlpha <> 0 then
    begin
      p := FTextMask.Data;
      for n := FTextMask.NbPixels - 1 downto 0 do
      begin
        p^:= integer(p^ * 255) div maxAlpha;
        Inc(p);
      end;
    end;
  end
  else
  begin
    FTextMask := TGrayscaleMask.Create(temp, cGreen);
    temp.Free;

    p := FTextMask.data;
    for n := FTextMask.NbPixels-1 downto 0 do
      p^ := GammaExpansionTab[p^] shr 8;
  end;
end;

procedure TBGRATextEffect.InitWithFontName(AText: string; AFontName: string;
  AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean; SubOffsetX, SubOffsetY: single);
var lFont: TFont;
begin
  lFont := TFont.Create;
  lFont.Name := AFontName;
  lFont.Height := AFullHeight * FontFullHeightSign;
  lFont.Style := AStyle;
  Init(AText, lFont, Antialiasing, SubOffsetX, SubOffsetY, 0,0);
  lFont.Free;
end;

constructor TBGRATextEffect.Create(AText: string; Font: TFont;
  Antialiasing: boolean);
begin
  Init(AText, Font, Antialiasing, 0,0,0,0);
end;

initialization

  BGRATextOutImproveReadabilityProc := @BGRATextOutImproveReadability;

end.

