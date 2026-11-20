program CheckLayout;
{$mode objfpc}{$H+}{$codepage utf8}
{$IFDEF LCLCOCOA}
{$modeswitch objectivec1}
uses Interfaces,Forms,CocoaAll,Types,SysUtils,Classes,BGRABitmap,BGRABitmapTypes,BGRAGraphics,BGRAUTF8,LazUTF8;
const Names:array[0..2] of string=('default','Arial','Times New Roman');
 Fits:array[0..2] of UTF8String=('AV office fi ffi','déjà Á','a😀b');
 Text:UTF8String='ÉÀ gypqj Á office fi ffi العربية 漢字';
var pool:NSAutoreleasePool;f,st,em,qi,pi,x,y,i,n,fit,k,ti:integer;
 a,b,full,small,texture:TBGRABitmap;style:TFontStyles;q:TBGRAFontQuality;
 p1,p2:PBGRAPixel;col:TBGRAPixel;s,ref:TSize;log:TStringList;clip:TRect;raw,part:string;
procedure Same(A,B:TBGRABitmap;const What:string;Tolerance:integer=0);
var j,d:integer;p,q:PBGRAPixel;
begin
 p:=A.Data;q:=B.Data;
 for j:=0 to A.NbPixels-1 do begin
 d:=Abs(integer(p^.red)-q^.red);if Abs(integer(p^.green)-q^.green)>d then d:=Abs(integer(p^.green)-q^.green);
 if Abs(integer(p^.blue)-q^.blue)>d then d:=Abs(integer(p^.blue)-q^.blue);
 if Abs(integer(p^.alpha)-q^.alpha)>d then d:=Abs(integer(p^.alpha)-q^.alpha);
 if d>Tolerance then raise Exception.CreateFmt('%s: pixel %d differs by %d font=%d style=%d em=%d q=%d',[What,j,d,f,st,em,qi]);Inc(p);Inc(q) end;
 Inc(n);
end;
procedure Setup(B:TBGRABitmap);
begin B.FontName:=Names[f];B.FontHeight:=em;B.FontStyle:=style;B.FontQuality:=q end;
begin
 try
 Application.Initialize;log:=TStringList.Create;n:=0;
 col:=BGRA(20,90,180,160);texture:=TBGRABitmap.Create(4,4,col);
 for f:=0 to 2 do for st:=0 to 2 do for em in [5,8,15,20,28,40] do begin
 pool:=NSAutoreleasePool.alloc.init;
 case st of 0:style:=[];1:style:=[fsBold];2:style:=[fsItalic] end;
 for qi:=0 to 4 do begin
 case qi of 0:q:=fqSystem;1:q:=fqSystemClearType;2:q:=fqFineAntialiasing;3:q:=fqFineClearTypeRGB;4:q:=fqFineClearTypeBGR end;
 a:=TBGRABitmap.Create(900,140,BGRAWhite);b:=a.Duplicate;Setup(a);Setup(b);
 try
 s:=a.TextSize(Text);
 if qi=0 then ref:=s else if (s.cx<>ref.cx) or (s.cy<>ref.cy) then raise Exception.Create('Quality changed native metrics');Inc(n);
 a.TextOut(40.25,40.5,Text,col);b.TextOut(40.25,40.5,Text,texture);
 Same(a,b,'solid texture',1);
 b.Fill(BGRAWhite);clip:=Rect(55,45,170,80);b.ClipRect:=clip;b.TextOut(40.25,40.5,Text,col);b.ClipRect:=Rect(0,0,b.Width,b.Height);
 for y:=0 to b.Height-1 do for x:=0 to b.Width-1 do
  if (x<clip.Left) or (x>=clip.Right) or (y<clip.Top) or (y>=clip.Bottom) then a.SetPixel(x,y,BGRAWhite);
 Same(a,b,'ClipRect');
 for pi:=0 to 3 do begin
 full:=TBGRABitmap.Create(900,140,BGRAPixelTransparent);small:=TBGRABitmap.Create(150,70,BGRAPixelTransparent);Setup(full);Setup(small);
 try
 full.TextOut(40.25,40+pi*0.125,Text,col);
 small.TextOut(-9.75,-10+pi*0.125,Text,col);
 for y:=0 to small.Height-1 do for x:=0 to small.Width-1 do
  if LongWord(small.GetPixel(x,y))<>LongWord(full.GetPixel(x+50,y+50)) then raise Exception.Create('Fractional crop');Inc(n);
 finally full.Free;small.Free end;
 end;
 a.Fill(BGRAWhite);b.Fill(BGRAWhite);a.TextOut(300,20,Text,col,taRightJustify,false);b.TextOut(300-s.cx,20,Text,col,taLeftJustify,false);Same(a,b,'right alignment');
 a.Fill(BGRAWhite);b.Fill(BGRAWhite);a.TextOut(300,20,Text,col,taCenter,false);b.TextOut(300-s.cx/2,20,Text,col,taLeftJustify,false);Same(a,b,'center alignment');
 for ti:=0 to 2 do begin
  raw:=Fits[ti];
  k:=UTF8Length(raw);
  if a.TextFitInfo(raw,0)<>0 then raise Exception.Create('zero fit');
  if a.TextFitInfo(raw,a.TextSize(raw).cx+1)<>k then raise Exception.Create('full fit');
  for i:=1 to a.TextSize(raw).cx do begin
   fit:=a.TextFitInfo(raw,i);if (fit<0) or (fit>k) then raise Exception.Create('fit range');
   part:=UTF8Copy(raw,1,fit);if a.TextSize(part).cx>i+1 then raise Exception.CreateFmt('fit overflow font=%d style=%d em=%d q=%d width=%d fit=%d measured=%d text=%s',[f,st,em,qi,i,fit,a.TextSize(part).cx,raw]);
  end;Inc(n);
 end;
 finally a.Free;b.Free end;
 end;
 pool.release;
 end;
 texture.Free;log.Add(IntToStr(n)+' checks passed');log.SaveToFile(ParamStr(1)+'/layout-validation.txt');log.Free;
 WriteLn(n,' checks passed');
 except on E:Exception do begin WriteLn(StdErr,E.ClassName,': ',E.Message,' context ',f,'/',st,'/',em,'/',qi,' raw=',raw,' i=',i,' fit=',fit,' part=',part);for k:=1 to Length(part) do Write(StdErr,IntToHex(Ord(part[k]),2));WriteLn(StdErr);Halt(1) end end;
end.

{$ELSE}
begin
  WriteLn('SKIP: check_layout requires the Cocoa LCL widgetset.');
end.
{$ENDIF}
