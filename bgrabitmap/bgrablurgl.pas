// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Blur effect using OpenGL shaders }
unit BGRABlurGL;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, BGRAOpenGL3D, BGRABitmapTypes, BGRACanvasGL, BGRAOpenGLType;

type
  { Shader computing a blurred image }
  TBGLBlurShader = class(TBGLShader3D)
  private
    function GetAllocatedTextureSize: TPoint;
    function GetDirection: TPointF;
    function GetImageIndex: integer;
    function GetRadius: Single;
    function GetTextureSize: TPoint;
    procedure SetAllocatedTextureSize(AValue: TPoint);
    procedure SetDirection(AValue: TPointF);
    procedure SetImageIndex(AValue: integer);
    procedure SetRadius(AValue: Single);
    procedure SetTextureSize(AValue: TPoint);
  protected
    FTextureSize, FAllocatedTextureSize: TUniformVariablePoint;
    FImageIndex: TUniformVariableInteger;
    FDirection: TUniformVariablePointF;
    FRadius: TUniformVariableSingle;
    FBlurType: TRadialBlurType;
    procedure StartUse; override;
    property AllocatedTextureSize: TPoint read GetAllocatedTextureSize write SetAllocatedTextureSize;
  public
    constructor Create(ACanvas: TBGLCustomCanvas; ABlurType: TRadialBlurType);
    function FilterBlurMotion(ATexture: IBGLTexture): IBGLTexture; overload;
    function FilterBlurMotion(ATexture: IBGLTexture; ADirection: TPointF): IBGLTexture; overload;
    function FilterBlurRadial(ATexture: IBGLTexture): IBGLTexture;
    property ImageIndex: integer read GetImageIndex write SetImageIndex;
    property TextureSize: TPoint read GetTextureSize write SetTextureSize;
    property Direction: TPointF read GetDirection write SetDirection;
    property Radius: Single read GetRadius write SetRadius;
    property BlurType: TRadialBlurType read FBlurType;
  end;

implementation

uses BGRAOpenGL;

{ TBGLBlurShader }

function TBGLBlurShader.GetAllocatedTextureSize: TPoint;
begin
  result := FAllocatedTextureSize.Value;
end;

function TBGLBlurShader.GetDirection: TPointF;
begin
  result := FDirection.Value;
end;

function TBGLBlurShader.GetImageIndex: integer;
begin
  result := FImageIndex.Value;
end;

function TBGLBlurShader.GetRadius: Single;
begin
  result := FRadius.Value;
  if FBlurType = rbPrecise then result := result * 10;
end;

function TBGLBlurShader.GetTextureSize: TPoint;
begin
  result := FTextureSize.Value;
end;

procedure TBGLBlurShader.SetAllocatedTextureSize(AValue: TPoint);
begin
  FAllocatedTextureSize.Value := AValue;
end;

procedure TBGLBlurShader.SetDirection(AValue: TPointF);
begin
  FDirection.Value := AValue;
end;

procedure TBGLBlurShader.SetImageIndex(AValue: integer);
begin
  FImageIndex.Value := AValue;
end;

procedure TBGLBlurShader.SetRadius(AValue: Single);
begin
  if FBlurType = rbPrecise then AValue := AValue/10;
  FRadius.Value := AValue;
end;

procedure TBGLBlurShader.SetTextureSize(AValue: TPoint);
begin
  FTextureSize.Value:= AValue;
end;

constructor TBGLBlurShader.Create(ACanvas: TBGLCustomCanvas; ABlurType: TRadialBlurType);
var weightFunc: string;
  overflowFunc: string;
begin
  FBlurType:= ABlurType;
  case ABlurType of
  rbNormal,rbPrecise: weightFunc:=
'   float sigma = max(0.1,radius/1.8);'#10+
'	float normalized = x/sigma;'#10 +
'	return 1/(2.506628274631*sigma)*exp(-0.5*normalized*normalized);';
  rbCorona: weightFunc := 'return max(0, 1-abs(x-radius));';
  rbFast: weightFunc := 'return max(0,radius+1-x);';
  else {rbBox,rbDisk}
    weightFunc := 'if (x <= radius) return 1; else return max(0,radius+1-x);';
  end;

  if GetOpenGLVersion >= 200 then
  begin
    overflowFunc :=
'vec2 overflowTexCoord(vec2 coord)'#10 +
'{'#10 +
'return coord;'#10 +
'}'#10
  end else
  begin
    // fix repeat mode on older version
    overflowFunc :=
'vec2 overflowTexCoord(vec2 coord)'#10 +
'{'#10 +
'return fract(coord / textureSize) * textureSize;'#10 +
'}'#10
  end;

  inherited Create(ACanvas,
'void main(void) {'#10 +
'	gl_Position = gl_ProjectionMatrix * gl_Vertex;'#10 +
'    texCoord = vec2(gl_MultiTexCoord0);'#10 +
'}',

'uniform sampler2D image;'#10 +
'uniform ivec2 textureSize;'#10 +
'uniform ivec2 allocatedTextureSize;'#10 +
'uniform vec2 direction;'#10 +
'uniform float radius;'#10 +
'out vec4 FragmentColor;'#10 +

'float computeWeight(float x)'#10 +
'{'#10 +
weightFunc + #10 +
'}'#10 +

overflowFunc +

'void main(void)'#10 +
'{'#10 +
'	int range = int(radius+1.5);'#10 +

'	float weight = computeWeight(0);'#10 +
'	float totalWeight = weight;'#10 +
'	FragmentColor = texture2D( image, texCoord ) * weight;'#10 +

'	for (int i=1; i<=range; i++) {'#10 +
'		weight = computeWeight(i);'#10 +
'		FragmentColor += texture2D( image, overflowTexCoord(texCoord*allocatedTextureSize + i*direction)/allocatedTextureSize ) * weight;'#10 +
'		FragmentColor += texture2D( image, overflowTexCoord(texCoord*allocatedTextureSize - i*direction)/allocatedTextureSize ) * weight;'#10 +
'		totalWeight += 2*weight;'#10 +
'	}'#10 +

'	FragmentColor /= totalWeight;'#10 +
'}',

'varying vec2 texCoord;', '130');

  FImageIndex := UniformInteger['image'];
  FTextureSize := UniformPoint['textureSize'];
  FAllocatedTextureSize := UniformPoint['allocatedTextureSize'];
  FDirection := UniformPointF['direction'];
  FRadius := UniformSingle['radius'];

  ImageIndex:= 0;
  Direction := PointF(1,0);
  TextureSize := Point(1,1);
  AllocatedTextureSize := Point(1,1);
  Radius := 0;
end;

function TBGLBlurShader.FilterBlurRadial(ATexture: IBGLTexture): IBGLTexture;
var horiz: IBGLTexture;
begin
  horiz := FilterBlurMotion(ATexture, PointF(1,0));
  result := FilterBlurMotion(horiz, PointF(0,1));
end;

function TBGLBlurShader.FilterBlurMotion(ATexture: IBGLTexture): IBGLTexture;
var previousBuf,buf: TBGLCustomFrameBuffer;
  previousShader: TBGLCustomShader;
begin
  previousBuf := Canvas.ActiveFrameBuffer;
  buf := Canvas.CreateFrameBuffer(ATexture.Width, ATexture.Height);
  Canvas.ActiveFrameBuffer := buf;
  Canvas.Fill(BGRAPixelTransparent);

  TextureSize := Point(ATexture.Width,ATexture.Height);
  AllocatedTextureSize := Point(ATexture.AllocatedWidth, ATexture.AllocatedHeight);
  previousShader := Canvas.Lighting.ActiveShader;
  Canvas.Lighting.ActiveShader := self;

  ATexture.Draw(0, 0); //perform horiz blur

  Canvas.Lighting.ActiveShader := previousShader;
  Canvas.ActiveFrameBuffer := previousBuf;
  result := buf.MakeTextureAndFree;
end;

function TBGLBlurShader.FilterBlurMotion(ATexture: IBGLTexture;
  ADirection: TPointF): IBGLTexture;
var prevDir: TPointF;
begin
  prevDir := Direction;
  Direction := ADirection;
  result := FilterBlurMotion(ATexture);
  Direction := prevDir;
end;

procedure TBGLBlurShader.StartUse;
begin
  inherited StartUse;
  FImageIndex.Update;
  FTextureSize.Update;
  FAllocatedTextureSize.Update;
  FDirection.Update;
  FRadius.Update;
end;

end.


