// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ ====================================================
  BGRAPapers Unit

  2023 Massimo Magnano

  List of paper sizes in inches and cm
}
unit BGRAPapers;

{$mode objfpc}{$H+}

interface

uses Types, Graphics;

type
  TPaperSize = packed record
    name:String[10];
    w, h:Single;
  end;

  TPaperSizes=array of TPaperSize;
  PPaperSizes=^TPaperSizes;

const
  Paper_A_cm: TPaperSizes=(
  (name:'A0'; w:84.1; h:118.9), (name:'A1'; w:59.4; h:84.1), (name:'A2'; w:42.0; h:59.4),
  (name:'A3'; w:29.7; h:42.0), (name:'A4'; w:21.0; h:29.7), (name:'A5'; w:14.8; h:21.0),
  (name:'A6'; w:10.5; h:14.8), (name:'A7'; w:7.4; h:10.5), (name:'A8'; w:5.2; h:7.4),
  (name:'A9'; w:3.7; h:5.2), (name:'A10'; w:2.6; h:3.7)
  );

  Paper_A_inch: TPaperSizes=(
  (name:'A0'; w:33.1; h:46.8), (name:'A1'; w:23.4; h:33.1), (name:'A2'; w:16.5; h:23.4),
  (name:'A3'; w:11.7; h:16.5), (name:'A4'; w:8.3; h:11.7), (name:'A5'; w:5.8; h:8.3),
  (name:'A6'; w:4.1; h:5.8), (name:'A7'; w:2.9; h:4.1), (name:'A8'; w:2.0; h:2.9),
  (name:'A9'; w:1.5; h:2.0), (name:'A10'; w:1.0; h:1.5)
  );

  Paper_B_cm: TPaperSizes=(
  (name:'B0'; w:100.0; h:141.4), (name:'B1'; w:70.7; h:100.0), (name:'B2'; w:50.0; h:70.7),
  (name:'B3'; w:35.3; h:50.0), (name:'B4'; w:25.0; h:35.3), (name:'B5'; w:17.6; h:25.0),
  (name:'B6'; w:12.5; h:17.6), (name:'B7'; w:8.8; h:12.5), (name:'B8'; w:6.2; h:8.8),
  (name:'B9'; w:4.4; h:6.2), (name:'B10'; w:3.1; h:4.4)
  );

  Paper_B_inch: TPaperSizes=(
  (name:'B0'; w:39.4; h:55.7), (name:'B1'; w:27.8; h:39.4), (name:'B2'; w:19.7; h:27.8),
  (name:'B3'; w:13.9; h:19.7), (name:'B4'; w:9.8; h:13.9), (name:'B5'; w:6.9; h:9.8),
  (name:'B6'; w:4.9; h:6.9), (name:'B7'; w:3.5; h:4.9), (name:'B8'; w:2.4; h:3.5),
  (name:'B9'; w:1.7; h:2.4), (name:'B10'; w:1.2; h:1.7)
  );

  Paper_C_cm: TPaperSizes=(
  (name:'C0'; w:91.7; h:129.7), (name:'C1'; w:64.8; h:91.7), (name:'C2'; w:45.8; h:64.8),
  (name:'C3'; w:32.4; h:45.8), (name:'C4'; w:22.9; h:32.4), (name:'C5'; w:16.2; h:22.9),
  (name:'C6'; w:11.4; h:16.2), (name:'C7'; w:8.1; h:11.4), (name:'C8'; w:5.7; h:8.1),
  (name:'C9'; w:4.0; h:5.7), (name:'C10'; w:2.8; h:4.0)
  );

  Paper_C_inch: TPaperSizes=(
  (name:'C0'; w:36.1; h:51.1), (name:'C1'; w:25.5; h:36.1), (name:'C2'; w:18.0; h:25.5),
  (name:'C3'; w:12.8; h:18.0), (name:'C4'; w:9.0; h:12.8), (name:'C5'; w:6.4; h:9.0),
  (name:'C6'; w:4.5; h:6.4), (name:'C7'; w:3.2; h:4.5), (name:'C8'; w:2.2; h:3.2),
  (name:'C9'; w:1.6; h:2.2), (name:'C10'; w:1.1; h:1.6)
  );

  Photo_cm: TPaperSizes=(
  (name:''; w:7; h:10),
  (name:''; w:9; h:12), (name:''; w:9; h:13),
  (name:''; w:10; h:10), (name:''; w:10; h:15),
  (name:''; w:13; h:13), (name:''; w:13; h:18),
  (name:''; w:15; h:20), (name:''; w:15; h:21),
  (name:''; w:20; h:20), (name:''; w:20; h:24), (name:''; w:20; h:25), (name:''; w:20; h:30),
  (name:''; w:30; h:30), (name:''; w:30; h:40), (name:''; w:30; h:45),
  (name:''; w:40; h:40), (name:''; w:40; h:50), (name:''; w:40; h:60)
  );

  PaperSizes_Names: array of String[10]=('ISO A', 'ISO B', 'ISO C', 'Photo');

var
  PaperSizes_cm :array of TPaperSizes;
  PaperSizes_inch :array of TPaperSizes;

function Sizes_InchToCm(const APapers:TPaperSizes):TPaperSizes;
function Sizes_CmToInch(const APapers:TPaperSizes):TPaperSizes;

implementation

function Sizes_InchToCm(const APapers: TPaperSizes): TPaperSizes;
var
   i:Integer;

begin
  Result :=Copy(APapers, 0, Length(APapers));
  for i:=Low(Result) to High(Result) do
  begin
    Result[i].w :=Result[i].w*2.54;
    Result[i].h :=Result[i].h*2.54;
  end;
end;

function Sizes_CmToInch(const APapers: TPaperSizes): TPaperSizes;
var
   i:Integer;

begin
  Result :=Copy(APapers, 0, Length(APapers));
  for i:=Low(Result) to High(Result) do
  begin
    Result[i].w :=Result[i].w/2.54;
    Result[i].h :=Result[i].h/2.54;
  end;
end;

initialization
   PaperSizes_cm :=[Paper_A_cm, Paper_B_cm, Paper_C_cm, Photo_cm];
   PaperSizes_inch :=[Paper_A_inch, Paper_B_inch, Paper_C_inch, Sizes_CmToInch(Photo_cm)];

end.

