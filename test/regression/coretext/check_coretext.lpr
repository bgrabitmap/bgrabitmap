program CheckCoreText;
{$mode objfpc}{$H+}{$codepage utf8}
{$IFDEF LCLCOCOA}
{$modeswitch objectivec1}
uses Interfaces,Forms,MacOSAll,CocoaAll,CocoaGDIObjects,Types,SysUtils,Classes,Math,BGRABitmap,BGRABitmapTypes,BGRACocoaText,BGRAGraphics;
const Text:UTF8String='Été déjà — office fi ffi gypqj';
var pool:NSAutoreleasePool;em,bgIndex,colorIndex,i,maxDiff,diff,e,qi,x,y,c:Integer;
 folder,prefix:string;font:TFont;
 line:CTLineRef;ctx:CGContextRef;space:CGColorSpaceRef;str:CFStringRef;attrs:CFDictionaryRef;astr:CFAttributedStringRef;nsf:NSFont;direct,composed,full,small,empty:TBGRABitmap;info:CGBitmapInfo;qual:TBGRAFontQuality;
 bg,col:TBGRAPixel;p,q:PBGRAPixel;log,validation:TStringList;
begin
 try
 Application.Initialize;pool:=NSAutoreleasePool.alloc.init;
 try
  folder:=IncludeTrailingPathDelimiter(ParamStr(1));log:=TStringList.Create;validation:=TStringList.Create;
  space:=CGColorSpaceCreateDeviceRGB;
  try
   log.Add('size,background,color,different,max_diff');
   for em in [5,8,15,20,28,40] do begin
    font:=TFont.Create;font.Name:='default';font.Height:=-em;nsf:=TCocoaFont(Pointer(font.Handle)).Font;
    str:=CFStringCreateWithBytes(nil,PByte(PChar(Text)),Length(Text),kCFStringEncodingUTF8,false);
    attrs:=CFDictionaryRef(NSDictionary.dictionaryWithObjectsAndKeys(nsf,kCTFontAttributeName,kCFBooleanTrue,kCTForegroundColorFromContextAttributeName,nil));
    astr:=CFAttributedStringCreate(nil,str,attrs);CFRelease(str);
    line:=CTLineCreateWithAttributedString(astr);CFRelease(astr);
    try
     for bgIndex:=0 to 2 do for colorIndex:=0 to 1 do begin
      if bgIndex=0 then bg:=BGRAWhite else if bgIndex=1 then bg:=BGRA(45,70,95) else bg:=BGRAPixelTransparent;
      if colorIndex=0 then col:=BGRABlack else col:=BGRA(20,90,180,160);
      direct:=TBGRABitmap.Create(900,100,bg);composed:=TBGRABitmap.Create(900,100,bg);
      try
       if TBGRAPixel_RGBAOrder then info:=kCGImageAlphaNoneSkipLast or kCGBitmapByteOrder32Big else info:=kCGImageAlphaNoneSkipFirst or kCGBitmapByteOrder32Little;
       if bgIndex=2 then begin
        info:=info and not kCGBitmapAlphaInfoMask;
        if TBGRAPixel_RGBAOrder then info:=info or kCGImageAlphaPremultipliedLast
        else info:=info or kCGImageAlphaPremultipliedFirst;
       end;
       ctx:=CGBitmapContextCreate(direct.Data,900,100,8,3600,space,info);
       if ctx=nil then raise Exception.Create('Missing native context');
       try
        CGContextSetTextMatrix(ctx,CGAffineTransformIdentity);
        CGContextSetShouldAntialias(ctx,1);CGContextSetShouldSmoothFonts(ctx,0);
        CGContextSetRGBFillColor(ctx,col.red/255,col.green/255,col.blue/255,1);
        CGContextSetAlpha(ctx,col.alpha/255);
        CGContextSetTextPosition(ctx,40,100-25-nsf.ascender);CTLineDraw(line,ctx);CGContextFlush(ctx);
       finally CGContextRelease(ctx) end;
       composed.FontName:='default';composed.FontHeight:=em;composed.FontQuality:=fqFineAntialiasing;composed.TextOut(40,25,Text,col);
       maxDiff:=0;diff:=0;p:=direct.Data;q:=composed.Data;
       for i:=0 to direct.NbPixels-1 do begin
        if LongWord(p^)<>LongWord(q^) then Inc(diff);
        if bgIndex=2 then begin
         e:=Abs(Integer(p^.red)-(Integer(q^.red)*q^.alpha+127) div 255);if e>maxDiff then maxDiff:=e;
         e:=Abs(Integer(p^.green)-(Integer(q^.green)*q^.alpha+127) div 255);if e>maxDiff then maxDiff:=e;
         e:=Abs(Integer(p^.blue)-(Integer(q^.blue)*q^.alpha+127) div 255);if e>maxDiff then maxDiff:=e;
        end else begin
        e:=Abs(Integer(p^.red)-q^.red);if e>maxDiff then maxDiff:=e;
        e:=Abs(Integer(p^.green)-q^.green);if e>maxDiff then maxDiff:=e;
        e:=Abs(Integer(p^.blue)-q^.blue);if e>maxDiff then maxDiff:=e;
        end;
        e:=Abs(Integer(p^.alpha)-q^.alpha);if e>maxDiff then maxDiff:=e;
        Inc(p);Inc(q);
       end;
       log.Add(Format('%d,%d,%d,%d,%d',[em,bgIndex,colorIndex,diff,maxDiff]));
       if maxDiff>2 then begin log.SaveToFile(folder+'blend-checks.csv');direct.SaveToFile(folder+'failed-direct.png');composed.SaveToFile(folder+'failed-native.png');raise Exception.CreateFmt('Native blend differs: size=%d bg=%d color=%d max=%d',[em,bgIndex,colorIndex,maxDiff]) end;
       if bgIndex=2 then begin
        p:=direct.Data;
        for i:=0 to direct.NbPixels-1 do begin
         if p^.alpha>0 then begin
          p^.red:=Byte(Min(255,(Integer(p^.red)*255+p^.alpha div 2) div p^.alpha));
          p^.green:=Byte(Min(255,(Integer(p^.green)*255+p^.alpha div 2) div p^.alpha));
          p^.blue:=Byte(Min(255,(Integer(p^.blue)*255+p^.alpha div 2) div p^.alpha));
         end;
         Inc(p);
        end;
       end;
       prefix:=folder+Format('blend-%d-%d-%d-',[em,bgIndex,colorIndex]);
       direct.SaveToFile(prefix+'direct.png');composed.SaveToFile(prefix+'native.png');
      finally direct.Free;composed.Free end;
     end;
     for qi:=0 to 2 do begin
      case qi of 0:qual:=fqFineAntialiasing;1:qual:=fqFineClearTypeRGB;2:qual:=fqFineClearTypeBGR end;
      full:=TBGRABitmap.Create(300,80,BGRAPixelTransparent);full.FontHeight:=em;full.FontQuality:=qual;
      small:=TBGRABitmap.Create(80,40,BGRAPixelTransparent);small.FontHeight:=em;small.FontQuality:=qual;empty:=small.Duplicate;
      try
       small.TextOut(0,0,'',BGRABlack);
       small.TextOut(0,0,'hidden',BGRAPixelTransparent);
       if not CompareMem(small.Data,empty.Data,small.NbPixels*4) then raise Exception.Create('No-op failed');
       full.TextOut(20.25,20.5,'MMMMMMMM',BGRABlack);
       small.TextOut(-11.75,-7.5,'MMMMMMMM',BGRABlack);
       c:=0;
       for y:=0 to small.Height-1 do for x:=0 to small.Width-1 do
        if LongWord(small.GetPixel(x,y))<>LongWord(full.GetPixel(x+32,y+28)) then Inc(c);
       if c<>0 then raise Exception.Create('Simple entry clipping failed');
       validation.Add(Format('PASS size=%d quality=%d no-op and fractional crop',[em,qi]));
      finally full.Free;small.Free;empty.Free end;
     end;
    finally CFRelease(line);font.Free end;
   end;
   log.SaveToFile(folder+'blend-checks.csv');
   validation.SaveToFile(folder+'native-validation.txt');
  finally CGColorSpaceRelease(space);log.Free;validation.Free end;
 finally pool.release end;
 except on E:Exception do begin WriteLn(StdErr,E.ClassName+': '+E.Message);Halt(1) end end;
end.

{$ELSE}
begin
  WriteLn('SKIP: check_coretext requires the Cocoa LCL widgetset.');
end.
{$ENDIF}
