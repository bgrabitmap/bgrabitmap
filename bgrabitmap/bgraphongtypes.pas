// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAPhongTypes;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes;

type

{ TCustomPhongShading }

 TCustomPhongShading = class
   protected
     FLightPosition3D: TPoint3D;
     function GetLightPosition: TPoint;
     procedure SetLightPosition(AValue: TPoint);
     function GetLightPositionF: TPointF;
     procedure SetLightPositionF(AValue: TPointF);
     function GetLightPositionZ: integer;
     procedure SetLightPositionZ(AValue: integer);

   public

   { Render the specified map on the destination bitmap with one solid color. Map altitude
     indicate the global height of the map. }
   procedure Draw(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: single; ofsX,ofsY: integer;
                  Color : TBGRAPixel);  overload; virtual; abstract;

   { Render with a color map of the same size as the height map. Map altitude
     indicate the global height of the map. }
   procedure Draw(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: single; ofsX,ofsY: integer;
                  ColorMap : TBGRACustomBitmap);  overload; virtual; abstract;

   { Render with a scanner. Map altitude
     indicate the global height of the map. }
   procedure DrawScan(dest: TBGRACustomBitmap; map: TBGRACustomBitmap; mapAltitude: single; ofsX,ofsY: integer;
                  ColorScan : IBGRAScanner); virtual; abstract;

   property LightPosition: TPoint read GetLightPosition write SetLightPosition;
   property LightPositionZ: integer read GetLightPositionZ write SetLightPositionZ;
   property LightPositionF: TPointF read GetLightPositionF write SetLightPositionF;
   property LightPosition3D: TPoint3D read FLightPosition3D write FLightPosition3D;
 end;

implementation

{ TCustomPhongShading }

function TCustomPhongShading.GetLightPositionF: TPointF;
begin
  result := PointF(FLightPosition3D.X,FLightPosition3D.Y);
end;

procedure TCustomPhongShading.SetLightPositionF(AValue: TPointF);
begin
  FLightPosition3D.X := AValue.X;
  FLightPosition3D.Y := AValue.Y;
end;

function TCustomPhongShading.GetLightPosition: TPoint;
begin
  result := Point(round(FLightPosition3D.X),round(FLightPosition3D.Y));
end;

procedure TCustomPhongShading.SetLightPosition(AValue: TPoint);
begin
  FLightPosition3D.X := AValue.X;
  FLightPosition3D.Y := AValue.Y;
end;

function TCustomPhongShading.GetLightPositionZ: integer;
begin
  result := round(FLightPosition3D.Z);
end;

procedure TCustomPhongShading.SetLightPositionZ(AValue: integer);
begin
  FLightPosition3D.Z := AValue;
end;

end.

