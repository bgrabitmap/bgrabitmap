
program demo;

{$IFDEF FPC}{$MODE objfpc}{$H+}{$ENDIF}
{$IFDEF FPC}
{$IFDEF mswindows}{$APPTYPE gui}{$ENDIF}
{$ENDIF}

uses
{$IFDEF FPC}{$IFDEF unix}
  cthreads, 
{$ENDIF}{$ENDIF}
  msegui, mseforms, main;
  
begin
  application.createform(tmainfo, mainfo);
  application.run;
end.
