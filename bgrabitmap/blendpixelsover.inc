// SPDX-License-Identifier: LGPL-3.0-linking-exception
{************************* blend over ***************************}

procedure FastBlendPixelsWithOpacity(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
begin
  while Count > 0 do
        begin
          FastBlendPixelInline(pdest, psrc^, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure DrawPixelsWithOpacity(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
begin
  while Count > 0 do
        begin
          DrawPixelInlineWithAlphaCheck(pdest, psrc^, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearMultiplyPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearMultiplyPixelInline(@temp, psrc^);  //same look with non linear
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearMultiplyPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearMultiplyPixelInline(@temp, psrc^);  //same look with non linear
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure AddPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          AddPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure AddPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          AddPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearAddPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearAddPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearAddPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearAddPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure ColorBurnPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          ColorBurnPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure ColorBurnPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          ColorBurnPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure ColorDodgePixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          ColorDodgePixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure ColorDodgePixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          ColorDodgePixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure DividePixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          DividePixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure DividePixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          DividePixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure ReflectPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          ReflectPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure ReflectPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          ReflectPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure GlowPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          GlowPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure GlowPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          GlowPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure NiceGlowPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          NiceGlowPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure NiceGlowPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          NiceGlowPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure OverlayPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          OverlayPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure OverlayPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          OverlayPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearOverlayPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearOverlayPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearOverlayPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearOverlayPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure DifferencePixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          DifferencePixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure DifferencePixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          DifferencePixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearDifferencePixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearDifferencePixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearDifferencePixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearDifferencePixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure ExclusionPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          ExclusionPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure ExclusionPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          ExclusionPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearExclusionPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearExclusionPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearExclusionPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearExclusionPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearSubtractPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearSubtractPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearSubtractPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearSubtractPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearSubtractInversePixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearSubtractInversePixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearSubtractInversePixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearSubtractInversePixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure SubtractPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          SubtractPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure SubtractPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          SubtractPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure SubtractInversePixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          SubtractInversePixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure SubtractInversePixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          SubtractInversePixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure NegationPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          NegationPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure NegationPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          NegationPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearNegationPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearNegationPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearNegationPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearNegationPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LightenPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LightenPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LightenPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LightenPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure DarkenPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          DarkenPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure DarkenPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          DarkenPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure ScreenPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          ScreenPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure ScreenPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          ScreenPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure SoftLightPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          SoftLightPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure SvgSoftLightPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          SvgSoftLightPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure SoftLightPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          SoftLightPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure SvgSoftLightPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          SvgSoftLightPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure HardLightPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          HardLightPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure HardLightPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          HardLightPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure BlendXorPixelsLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          BlendXorPixelInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure BlendXorPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          BlendXorPixelInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure MaskPixelsDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
const alphaMax = 255*7*255;
var
  alpha, opacity256: UInt32or64;
  newAlpha: byte;
begin
  if Opacity >= 128 then opacity256 := Opacity+1 else opacity256 := Opacity;
  while Count > 0 do
        begin
          if pdest^.alpha > 0 then
          begin
            alpha := (psrc^.green shl 2) + (psrc^.red shl 1) + psrc^.blue;
            alpha := alpha*psrc^.alpha + (255*7)*(not psrc^.alpha);
            newAlpha := (pdest^.alpha * alpha + (alphaMax shr 1)) div alphaMax;
            pdest^.alpha := (pdest^.alpha*UInt32or64(256 - opacity256) + newAlpha*opacity256) shr 8;
            if pdest^.alpha = 0 then pdest^ := BGRAPixelTransparent;
          end;
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearMultiplySaturationLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearMultiplySaturationInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearHueLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearHueInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearColorLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearColorInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearLightnessLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearLightnessInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearSaturationLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearSaturationInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure CorrectedHueLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          CorrectedHueInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure CorrectedColorLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          CorrectedColorInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure CorrectedLightnessLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          CorrectedLightnessInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure CorrectedSaturationLinearOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          CorrectedSaturationInline(@temp, psrc^);
          FastBlendPixelInline(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearMultiplySaturationDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearMultiplySaturationInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearHueDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearHueInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearColorDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearColorInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearLightnessDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearLightnessInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure LinearSaturationDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          LinearSaturationInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure CorrectedHueDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          CorrectedHueInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure CorrectedColorDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          CorrectedColorInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure CorrectedLightnessDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          CorrectedLightnessInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

procedure CorrectedSaturationDrawOver(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);
var temp: TBGRAPixel;
begin
  while Count > 0 do
        begin
          temp := pdest^;
          CorrectedSaturationInline(@temp, psrc^);
          DrawPixelInlineWithAlphaCheck(pdest, temp, opacity);
          Inc(pdest);
          Inc(psrc);
          Dec(Count);
        end;
end;

{************************** table ****************************************}

type
  TBlendPixelsOverProc = procedure(pdest: PBGRAPixel; psrc: PBGRAPixel; Count: integer; Opacity: byte);

const
  BlendPixelsOverProc: array[Boolean, TBlendOperation] of TBlendPixelsOverProc =
    ( (@FastBlendPixelsWithOpacity, @DrawPixelsWithOpacity,
      @LightenPixelsDrawOver, @ScreenPixelsDrawOver, @AddPixelsDrawOver, @LinearAddPixelsDrawOver, @ColorDodgePixelsDrawOver, @DividePixelsDrawOver, @NiceGlowPixelsDrawOver, @SoftLightPixelsDrawOver, @HardLightPixelsDrawOver,
      @GlowPixelsDrawOver, @ReflectPixelsDrawOver, @LinearOverlayPixelsDrawOver, @OverlayPixelsDrawOver, @DarkenPixelsDrawOver, @LinearMultiplyPixelsDrawOver, @ColorBurnPixelsDrawOver,
      @DifferencePixelsDrawOver, @LinearDifferencePixelsDrawOver, @ExclusionPixelsDrawOver, @LinearExclusionPixelsDrawOver, @SubtractPixelsDrawOver, @LinearSubtractPixelsDrawOver,
      @SubtractInversePixelsDrawOver, @LinearSubtractInversePixelsDrawOver, @NegationPixelsDrawOver, @LinearNegationPixelsDrawOver, @BlendXorPixelsDrawOver, @SvgSoftLightPixelsDrawOver,
      @MaskPixelsDrawOver, @LinearMultiplySaturationDrawOver, @LinearHueDrawOver, @LinearColorDrawOver, @LinearLightnessDrawOver, @LinearSaturationDrawOver,
      @CorrectedHueDrawOver, @CorrectedColorDrawOver, @CorrectedLightnessDrawOver, @CorrectedSaturationDrawOver),
      (@FastBlendPixelsWithOpacity, @FastBlendPixelsWithOpacity,
      @LightenPixelsLinearOver, @ScreenPixelsLinearOver, @AddPixelsLinearOver, @LinearAddPixelsLinearOver, @ColorDodgePixelsLinearOver, @DividePixelsLinearOver, @NiceGlowPixelsLinearOver, @SoftLightPixelsLinearOver, @HardLightPixelsLinearOver,
      @GlowPixelsLinearOver, @ReflectPixelsLinearOver, @LinearOverlayPixelsLinearOver, @OverlayPixelsLinearOver, @DarkenPixelsLinearOver, @LinearMultiplyPixelsLinearOver, @ColorBurnPixelsLinearOver,
      @DifferencePixelsLinearOver, @LinearDifferencePixelsLinearOver, @ExclusionPixelsLinearOver, @LinearExclusionPixelsLinearOver, @SubtractPixelsLinearOver, @LinearSubtractPixelsLinearOver,
      @SubtractInversePixelsLinearOver, @LinearSubtractInversePixelsLinearOver, @NegationPixelsLinearOver, @LinearNegationPixelsLinearOver, @BlendXorPixelsLinearOver, @SvgSoftLightPixelsLinearOver,
      @MaskPixelsDrawOver, @LinearMultiplySaturationLinearOver, @CorrectedHueLinearOver, @LinearColorLinearOver, @LinearLightnessLinearOver, @LinearSaturationLinearOver,
      @CorrectedHueLinearOver, @CorrectedColorLinearOver, @CorrectedLightnessLinearOver, @CorrectedSaturationLinearOver));

{************************* calling procedure ***************************}

procedure BlendPixelsOver(pdest: PBGRAPixel; psrc: PBGRAPixel;
  blendOp: TBlendOperation; Count: integer; opacity: byte; linearBlend: boolean; excludeChannels: TChannels = []);
const BufSize = 8;
var
  destBuf: array[0..BufSize-1] of TBGRAPixel;
  i: PtrInt;
  blendProc: TBlendPixelsOverProc;
begin
  if (excludeChannels = [cRed,cGreen,cBlue,cAlpha]) or
     ((blendOp = boMask) and (cAlpha in excludeChannels)) or
     (opacity = 0) then exit;

  blendProc := BlendPixelsOverProc[linearblend, blendOp];
  if excludeChannels = [] then
    blendProc(pdest, psrc, count, opacity) else
  begin
    while Count >= BufSize do
    begin
      move(pdest^, {%H-}destBuf, sizeof(destBuf));
      blendProc(@destBuf, psrc, BufSize, opacity);
      for i := 0 to BufSize-1 do
      begin
        if not (cRed in excludeChannels) then pdest^.red := destBuf[i].red;
        if not (cGreen in excludeChannels) then pdest^.green := destBuf[i].green;
        if not (cBlue in excludeChannels) then pdest^.blue := destBuf[i].blue;
        if not (cAlpha in excludeChannels) then pdest^.alpha := destBuf[i].alpha;
        inc(pdest);
      end;
      dec(count, BufSize);
    end;
    if count > 0 then
    begin
      move(pdest^, {%H-}destBuf, count*sizeof(TBGRAPixel));
      blendProc(@destBuf, psrc, count, opacity);
      for i := 0 to count-1 do
      begin
        if not (cRed in excludeChannels) then pdest^.red := destBuf[i].red;
        if not (cGreen in excludeChannels) then pdest^.green := destBuf[i].green;
        if not (cBlue in excludeChannels) then pdest^.blue := destBuf[i].blue;
        if not (cAlpha in excludeChannels) then pdest^.alpha := destBuf[i].alpha;
        inc(pdest);
      end;
    end;
  end;
end;

