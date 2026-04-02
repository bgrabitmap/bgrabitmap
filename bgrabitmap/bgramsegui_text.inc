// SPDX-License-Identifier: LGPL-3.0-linking-exception
procedure BitmapTextOut(ABitmap: TBitmap; ACoord: TPoint; AText: string);
var sUTF16: UnicodeString;
begin 
  sUTF16 := UTF8ToUTF16(AText);
  inc(ACoord.Y, ABitmap.Canvas.Font.GlyphHeight-1);
  ABitmap.Canvas.DrawString(sUTF16, ACoord);
end;

procedure BitmapTextOutAngle(ABitmap: TBitmap; ACoord: TPoint; AText: string; AOrientation: integer);
var sUTF16: UnicodeString;
  angleRad: single;
  ofs: TPointF;
begin
  angleRad := AOrientation*Pi/1800;
  ofs := PointF(ACoord.X, ACoord.Y) +
         PointF(sin(angleRad), cos(angleRad))*(ABitmap.Canvas.Font.GlyphHeight-1);
  sUTF16 := UTF8ToUTF16(AText);
  ABitmap.Canvas.DrawString(sUTF16, Point(round(ofs.X), round(ofs.Y)), 
                            nil, false, angleRad);
end;

procedure BitmapTextRect(ABitmap: TBitmap; ARect: TRect; ACoord: TPoint; 
  AText: string; const AStyle: TTextStyle);
var sUTF16: UnicodeString;
  AParts: TStringList;
  partialWidths: array of integer;
  oldLineWidth, totalWidth, i, y: integer;
begin
  AParts := TStringList.Create;
  If AStyle.ShowPrefix then
  begin
    SplitByPrefix(AText, AParts);
  end else
    AParts.Add(AText);
  sUTF16 := UTF8ToUTF16(AText);
  setlength(partialWidths, AParts.Count);
  totalWidth := 0;
  for i := 0 to AParts.Count-1 do
  begin
    partialWidths[i]:= ABitmap.Canvas.GetStringWidth(UTF8ToUTF16(AParts[i]));
    Inc(totalWidth, partialWidths[i]);
  end;
  case AStyle.Alignment of
  taRightJustify: ACoord.X := ARect.Right - totalWidth;
  taCenter: ACoord.X := ARect.Left + (ARect.Width - totalWidth) div 2;
  end;
  case AStyle.Layout of
  tlBottom: ACoord.Y := ARect.Bottom - ABitmap.Canvas.Font.Height;
  tlCenter: ACoord.Y := ARect.Top + (ARect.Height - ABitmap.Canvas.Font.Height) div 2;
  end;
  inc(ACoord.Y, ABitmap.Canvas.Font.GlyphHeight-1);
  oldLineWidth := ABitmap.Canvas.LineWidth;
  for i := 0 to AParts.Count-1 do
  begin
    ABitmap.Canvas.DrawString(UTF8ToUTF16(AParts[i]), ACoord);
    if (i and 1) = 1 then
    begin
      ABitmap.Canvas.LineWidth := ABitmap.Canvas.Font.LineWidth;
      y := ACoord.Y + ABitmap.Canvas.Font.Descent div 2 + 1;
      ABitmap.Canvas.DrawLine(Point(ACoord.X, y),
        Point(ACoord.X + partialWidths[i] - 1, y),
        ABitmap.Canvas.Font.Color);
    end;
    inc(ACoord.X, partialWidths[i]);
  end;
  ABitmap.Canvas.LineWidth := oldLineWidth;
end;

function BitmapTextExtent(ABitmap: TBitmap; AText: string): TSize;
var sUTF16: UnicodeString;
begin
  sUTF16 := UTF8ToUTF16(AText);
  result := Size(ABitmap.Canvas.GetStringWidth(sUTF16), ABitmap.Canvas.Font.Height);
end;

function BitmapTextExtentStyle(ABitmap: TBitmap; AText: string; AMaxWidth: integer;
  const AStyle: TTextStyle): TSize;
var sUTF16: UnicodeString;
begin
  if AStyle.ShowPrefix then // prefix not handled
    AText := RemovePrefix(AText);
  sUTF16 := UTF8ToUTF16(AText);
  result := Size(ABitmap.Canvas.GetStringWidth(sUTF16), ABitmap.Canvas.Font.Height);
end;

function BitmapTextExtentAngle(ABitmap: TBitmap; AText: string; AOrientation: integer): TSize;
begin
  result := BitmapTextExtent(ABitmap, AText);
end;

function BitmapTextFitInfo(ABitmap: TBitmap; AText: string; AMaxWidth: integer): integer;
var 
  bytePos, charLen: integer;  
  sUTF16: UnicodeString; 
begin
  result := 0;
  bytePos := 1;
  while bytePos <= length(AText) do
  begin
    charLen := UTF8CharacterLength(@AText[bytePos]);
    sUTF16 := UTF8ToUTF16(copy(AText, 1, bytePos+charLen-1));
    if ABitmap.Canvas.GetStringWidth(sUTF16) > AMaxWidth then break;
    inc(result);
    inc(bytePos,charLen);
  end;
end;

function BitmapTextFitInfoAngle(ABitmap: TBitmap; AText: string; 
  AMaxWidth: integer; AOrientation: integer): integer;
begin
  result := BitmapTextFitInfo(ABitmap, AText, AMaxWidth);
end;

procedure BitmapFillRect(ABitmap: TBitmap; ARect: TRect; AColor: TColor);
begin
  ABitmap.Canvas.FillRect(ARect, AColor);
end;
