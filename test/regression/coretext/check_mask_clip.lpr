program CheckMaskClip;
{$mode objfpc}{$H+}
uses Interfaces,Forms,Types,SysUtils,BGRABitmap,BGRABitmapTypes,BGRAGrayscaleMask;
var mask:TGrayscaleMask;a,b:TBGRABitmap;x,third,l,r,y,xx,order,n,width:integer;clip:TRect;
begin
 try
 Application.Initialize;n:=0;
 for width in [1,2,3,4,5,7,8,12,63] do begin
 mask:=TGrayscaleMask.Create(width,3,0);
 for y:=0 to 2 do for xx:=0 to width-1 do mask.SetPixel(xx,y,Byte((xx*47+y*83) mod 256));
 for x:=-4 to 4 do for third:=-2 to 4 do for l:=0 to 10 do for r:=l+1 to 14 do for order:=0 to 1 do begin
 a:=TBGRABitmap.Create(28,6,BGRA(45,70,95));b:=a.Duplicate;clip:=Rect(l,1,r,4);
 BGRAFillClearTypeGrayscaleMask(a,x,1,third,mask,BGRA(20,90,180,160),nil,order=0);
 b.ClipRect:=clip;
 BGRAFillClearTypeGrayscaleMask(b,x,1,third,mask,BGRA(20,90,180,160),nil,order=0);
 for y:=0 to 5 do for xx:=0 to 27 do begin
 if (xx<l) or (xx>=r) or (y<1) or (y>=4) then a.SetPixel(xx,y,BGRA(45,70,95));
 if LongWord(a.GetPixel(xx,y))<>LongWord(b.GetPixel(xx,y)) then raise Exception.CreateFmt('clip differs x=%d third=%d clip=%d..%d at %d,%d',[x,third,l,r,xx,y]);
 end;
 a.Free;b.Free;Inc(n);
 end;
 mask.Free;end;WriteLn(n,' mask clipping checks passed');
 except on E:Exception do begin WriteLn(StdErr,E.ClassName,': ',E.Message);Halt(1) end end;
end.
