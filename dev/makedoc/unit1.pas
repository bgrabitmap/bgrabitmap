// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  fgl;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    EPath: TEdit;
    Label1: TLabel;
    Memo1: TMemo;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses LazUTF8, FileUtil, LazFileUtils, RegExpr, StrUtils;

const
  BoldKeywords : array[1..58] of string = ('var','procedure','function','and',
    'or','xor','not','if','then','case','begin','end','of',
    'exit','new','class','is','const','div','do','downto','to','else','for',
    'in','mod','nil','object','record','repeat','self','shl','shr','string',
    'unit','until','uses','while','array','interface', 'out', 'constructor',
    'property','read','write','default', 'packed', 'operator', 'inline',
    'overload', 'virtual', 'abstract', 'helper', 'ifdef', 'endif', 'set',
    'specialize', 'generic');

type
  TDocumentationPages = specialize TFPGMap<string, string>;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    EPath.Text := SelectDirectoryDialog1.FileName;
end;

procedure HighlightKeywords(var s: string);
const keywordChars = ['a'..'z','A'..'Z'];
  moreKeywordChars = ['a'..'z','A'..'Z','0'..'9','_'];
var
  i,start: Integer;
  w,wlower: string;
  j: Integer;
  found, first: boolean;
begin
  i := 1;
  s := StringReplace(s, '''', '&apos;', [rfReplaceAll]);
  s := StringReplace(s, '{%H-}', '', [rfReplaceAll]);
  first := true;
  while i <= length(s) do
  begin
    if s[i] in keywordChars then
      begin
        start := i;
        inc(i);
        while i <= length(s) do
        begin
          if not (s[i] in moreKeywordChars) then break;
          inc(i);
        end;
        w := copy(s,start,i-start);
        wlower := lowercase(w);
        found := false;
        for j := low(BoldKeywords) to high(BoldKeywords) do
          if BoldKeywords[j] = wlower then
            begin
              delete(s, start, length(w));
              dec(i, length(w));
              w := ''''''''+wlower+'''''''';
              insert(w, s, start);
              inc(i, length(w));
              found := true;
              break;
            end;
        if not found and first then
          begin
            delete(s, start, length(w));
            dec(i, length(w));
            first := copy(s, start, 1) = ',';
            w := ''''''+w+'''''';
            insert(w, s, start);
            inc(i, length(w));
            continue;
          end;
        first := false;
      end else
        inc(i);
  end;
end;

procedure AdaptMarkdown(var s: string);
var r: TRegExpr;
begin
  r := TRegExpr.Create('([^A-Z0-9]|^)_([A-Z0-9]+([_.][A-Z0-9]+)*)_([^A-Z0-9]|$)'); r.ModifierI:= true;
  s := r.Replace(s, '$1''''$2''''$4', true);
  r.Free;

  r := TRegExpr.Create('\*\*([A-Z0-9]+([_.][A-Z0-9]+)*)\*\*'); r.ModifierI:= true;
  s := r.Replace(s, '''''''$1''''''', true);
  r.Free;

  r := TRegExpr.Create('([^\\]|^)\[([^\]]+)\]\(https://wiki.freepascal.org/(\w+)\)'); r.ModifierI:= true;
  s := r.Replace(s, '$1[[$3|$2]]', true);
  r.Free;

  r := TRegExpr.Create('([^\\]|^)\[([^\]]+)\]\(([-\w:/.]+)\)'); r.ModifierI:= true;
  s := r.Replace(s, '$1[$3 $2]', true);
  r.Free;

  r := TRegExpr.Create('```pascal([^`]+)```');
  s := r.Replace(s, '<syntaxhighlight>$1</syntaxhighlight>', true);
  r.Free;

  r := TRegExpr.Create('\^([0-9]+)');
  s := r.Replace(s, '<sup>$1</sup>', true);
  r.Free;

  s := StringReplace(s, '\[', '[', [rfReplaceAll]);
  s := StringReplace(s, ' --> ', ' &rarr; ', [rfReplaceAll]);
  s := StringReplace(s, ' <-- ', ' &larr; ', [rfReplaceAll]);
end;

procedure MakeDocFor(AFilename: string; APages: TDocumentationPages);
var
  t: textfile;
  fileoutput,s,bgcolor: String;
  description, element: String;
  comStart,comEnd, idxColor: integer;
  oddRow,indented : boolean;
  docName, colorStr: string;
  tableOpened, inCode, bulletPoint, prevBulletPoint: boolean;

  procedure openTable;
  begin
    if not tableOpened then
      begin
        fileoutput := fileoutput + '<table style="border-collapse: collapse;">'+lineending;
        oddRow := true;
        tableOpened:= true;
      end;
  end;

  procedure closeTable;
  begin
    if tableOpened then
    begin
      fileoutput := fileoutput + '</table>'+LineEnding;
      tableOpened:= false;
    end;
  end;

  procedure flushOutput;
  var
    docIndex: Integer;
  begin
    if fileoutput <> '' then
    begin
      closeTable;
      if not APages.Find(docName, docIndex) then
      begin
        docIndex := APages.Add(docName, '=== ' + docName + ' ===' + LineEnding);
      end;
      APages.Data[docIndex] := APages.Data[docIndex] + fileoutput;
      fileoutput:= '';
    end;
  end;

begin
  docName := ExtractFileName(AFilename);
  fileoutput := '';
  tableOpened:= false;
  assignfile(t, UTF8ToSys(AFilename));
  reset(t);
  while not eof(t) do
  begin
    readln(t,s);

    comStart:= pos('{====',s);
    if comStart <> 0 then
    begin
      comEnd:= pos('====}',s);
      if comEnd <> 0 then
      begin
        closeTable;
        fileoutput := fileoutput + trim(copy(s,comStart+1,comEnd+3 -(comStart+1)+1)) + LineEnding;
        continue;
      end;
    end;

    comStart:= pos('{===',s);
    if comStart <> 0 then
    begin
      comEnd:= pos('===}',s);
      if comEnd <> 0 then
      begin
        flushOutput;
        docName:= trim(copy(s,comStart+4,comEnd-1 -(comStart+4)+1));
        continue;
      end;
    end;

    comStart:= pos('{* ',s+' ');
    indented:= false;
    inCode := false;
    if comStart <> 0 then
      inc(comStart, 2)
    else
    begin
      comStart := pos('{** ',s+' ');
      if comStart <> 0 then
        inc(comStart, 3);
      indented := true;
    end;
    if comStart<>0 then
      begin
        delete(s,1,comStart-1);
        comStart := 1;
        description := '';
        comEnd := pos('}',s);
        if comEnd = 0 then
        begin
          prevBulletPoint := false;
          description := description + trim(copy(s,comStart,length(s)-comStart+1));
          while not eof(t) do
          begin
            readln(t,s);
            bulletPoint := false;
            s := trim(s);
            if s.StartsWith('```') then inCode := not inCode;
            if not inCode then
            begin
              s := StringReplace(s, '<=', '&le;', [rfReplaceAll]);
              s := StringReplace(s, '>=', '&ge;', [rfReplaceAll]);
            end;
            if s = '' then description := description + '<p>'
            else
            begin
              comEnd := pos('}',s);
              if comEnd > 0 then s := trim(copy(s,1,comEnd-1));
              if not inCode then
              begin
                if s.StartsWith('- ') then
                begin
                  description := description + IfThen(prevBulletPoint,'',LineEnding)+'* '+s.Substring(2)+LineEnding;
                  bulletPoint := true;
                end
                else
                  description := description + ' '+s;
              end;
              if comEnd > 0 then break
              else if inCode then description := description + LineEnding;
            end;
            prevBulletPoint:= bulletPoint;
          end;
        end
        else
          description := description + trim(copy(s,comStart,comEnd-comStart));

        AdaptMarkdown(description);

        while pos('[#',description) <> 0 do
        begin
          idxColor := pos('[#',description);
          colorStr := copy(description, idxColor, 9);
          if (length(colorStr) = 9) and colorStr.EndsWith(']') then
            begin
              delete(description, idxColor, length(colorStr));
              insert('<span style="width:8px; height: 8px; display: inline-block; border: 1px solid black; background: '+copy(colorStr,2,length(colorStr)-2)+';"></span>', description, idxColor);
            end;
        end;

        if not eof(t) then
          readln(t,element) else element := '?';

        HighlightKeywords(element);
        element := trim(element);

        openTable;
        if oddRow then bgcolor := 'white' else bgcolor := '#f0f0ff';

        if indented then
        begin
          fileoutput := fileoutput + '<tr><td width="10%"></td><td colspan="2" style="background: '+bgcolor+';">'+element+'</td></tr>'+LineEnding;
          fileoutput := fileoutput + '<tr><td width="10%"></td><td width="10%" style="background: '+bgcolor+';"></td>'+
             '<td style="border: 1px solid #e0e0a0; background: #ffffe4;">'+description+'</td></tr>'+LineEnding;
        end else
        begin
          fileoutput := fileoutput + '<tr style="background: '+bgcolor+';"><td colspan="3">'+element+'</td></tr>'+LineEnding;
          fileoutput := fileoutput + '<tr style="background: '+bgcolor+';"><td width="10%"></td>'+
             '<td style="border: 1px solid #e0e0a0; background: #ffffe4;" colspan="2">'+description+'</td></tr>'+LineEnding;
        end;

        fileoutput := fileoutput + '<tr style="height: 8px;"><td colspan="3"></td></tr>'+LineEnding;
        oddRow := not oddRow;
      end;
  end;
  closefile(t);
  flushOutput;
end;

function ExportPages(APages: TDocumentationPages; APath: string): string;
var
  i: Integer;
  u: textfile;
  outname, fullname, currentContent, fileoutput: String;
begin
  result := '';
  if APages.Count = 0 then exit;
  CreateDirUTF8(APath+DirectorySeparator+'doc');
  for i := 0 to APages.Count-1 do
  begin
    outname := 'doc'+DirectorySeparator+APages.Keys[i]+'.txt';
    fullname := APath+outname;
    fileoutput := APages.Data[i];
    if FileExistsUTF8(fullname) then
    begin
      currentContent := ReadFileToString(fullname);
      if currentContent <> fileoutput then
      begin
        assignfile(u, UTF8ToSys(fullname));
        rewrite(u);
        write(u, fileoutput);
        closefile(u);
        result := result + outname + ' (updated)' + LineEnding;
      end else
      begin
        result := result + outname + ' (unchanged)' + LineEnding;
      end;
    end else
    begin
      result := result + outname + ' (created)' + LineEnding;
      assignfile(u, UTF8ToSys(fullname));
      rewrite(u);
      write(u, fileoutput);
      closefile(u);
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var sr: TSearchRec;
  output,ext: string;
  basePath,path: string;
  pages: TDocumentationPages;
begin
  memo1.Text := 'Analyzing...';
  memo1.Update;
  basePath := ExtractFilePath(ParamStr(0));
  {$IFDEF DARWIN}
  if basePath.EndsWith('/MacOS/') then
    basePath := ExpandFileNameUTF8(basePath+'../../../');
  {$ENDIF}
  path := ExpandFileNameUTF8(AppendPathDelim(EPath.Text), basePath);
  if FindFirstUTF8(path+'*.*', faAnyFile, sr) = 0 then
  begin
    pages := TDocumentationPages.Create;
    pages.Sorted:= true;
    repeat
      if sr.Attr and (faDirectory or faVolumeId or faSymLink) <> 0 then continue;
      ext := AnsiLowerCase(ExtractFileExt(sr.Name));
      if (ext = '.pas') or (ext = '.inc') then
        MakeDocFor(path+sr.Name, pages);
    until FindNextUTF8(sr) <> 0;
    FindCloseUTF8(sr);

    output := ExportPages(pages, path);
    if output = '' then
      Memo1.Text := 'No output'
    else
      Memo1.text := output;
    pages.Free;
  end
  else
    Memo1.Text := 'Nothing to do';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EPath.Text := StringReplace(EPath.Text, '/', PathDelim, [rfReplaceAll]);
end;

end.

