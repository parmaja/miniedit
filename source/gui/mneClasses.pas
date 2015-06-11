unit mneClasses;

{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, Contnrs,
  LCLintf, LCLType,
  Dialogs, EditorOptions, SynEditHighlighter, SynEditSearch, SynEdit,
  Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  SynHighlighterSQL, SynHighlighterXML, SynHighlighterApache,
  SynHighlighterPython;

type
  TSQLFile = class(TTextEditorFile)
  public
  end;

  TApacheFile = class(TTextEditorFile)
  public
  end;

  TINIFile = class(TTextEditorFile)
  public
  end;

  TTXTFile = class(TTextEditorFile)
  public
  end;

  TXMLFile = class(TTextEditorFile)
  public
    procedure NewSource; override;
  end;

  TSQLFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TApacheFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TINIFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TTXTFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TXMLFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  { TmneEngine }

function ColorToRGBHex(Color: TColor): string;
function RGBHexToColor(Value: string): TColor;

const
  sSoftwareRegKey = 'Software\miniEdit\';

function GetFileImageIndex(const FileName: string): integer;

function GetHighlighterAttriAtRowColEx2(SynEdit: TCustomSynEdit; const XY: TPoint; var Token: string; var TokenType, Start: integer; var Attri: TSynHighlighterAttributes; var Range: Pointer): boolean;

implementation

uses
  IniFiles, mnStreams, mnUtils;

function ColorToRGBHex(Color: TColor): string;
var
  aRGB: TColorRef;
begin
  aRGB := ColorToRGB(Color);
  FmtStr(Result, '%s%.2x%.2x%.2x', ['#', GetRValue(aRGB), GetGValue(aRGB), GetBValue(aRGB)]);
end;

function RGBHexToColor(Value: string): TColor;
var
  R, G, B: byte;
begin
  if LeftStr(Value, 1) = '#' then
    Delete(Value, 1, 1);
  if Value <> '' then
  begin
    if Length(Value) = 3 then
    begin
      R := StrToIntDef('$' + Copy(Value, 1, 1) + Copy(Value, 1, 1), 0);
      G := StrToIntDef('$' + Copy(Value, 2, 1) + Copy(Value, 2, 1), 0);
      B := StrToIntDef('$' + Copy(Value, 3, 1) + Copy(Value, 3, 1), 0);
      Result := RGB(R, G, B);
    end
    else
    begin
      R := StrToIntDef('$' + Copy(Value, 1, 2), 0);
      G := StrToIntDef('$' + Copy(Value, 3, 2), 0);
      B := StrToIntDef('$' + Copy(Value, 5, 2), 0);
      Result := RGB(R, G, B);
    end;
  end
  else
    Result := clBlack;
end;

type
  TSynCustomHighlighterHack = class(TSynCustomHighlighter);

function GetHighlighterAttriAtRowColEx2(SynEdit: TCustomSynEdit; const XY: TPoint; var Token: string; var TokenType, Start: integer; var Attri: TSynHighlighterAttributes; var Range: Pointer): boolean;
var
  PosX, PosY: integer;
  Line: string;
  aToken: string;
begin
  with SynEdit do
  begin
    TokenType := 0;
    Token := '';
    Attri := nil;
    Result := False;
    PosY := XY.Y - 1;
    if Assigned(Highlighter) and (PosY >= 0) and (PosY < Lines.Count) then
    begin
      Line := Lines[PosY];
      if PosY = 0 then
        Highlighter.ResetRange
      else
        Highlighter.SetRange(TSynCustomHighlighterHack(Highlighter).CurrentRanges.Range[PosY - 1]);
      Highlighter.SetLine(Line, PosY);
      PosX := XY.X;
      Range := Highlighter.GetRange;
      if PosX > 0 then
        while not Highlighter.GetEol do
        begin
          Start := Highlighter.GetTokenPos + 1;
          aToken := Highlighter.GetToken;
          Range := Highlighter.GetRange;
          if (PosX >= Start) and (PosX < Start + Length(aToken)) then
          begin
            Attri := Highlighter.GetTokenAttribute;
            TokenType := Highlighter.GetTokenKind;
            Token := aToken;
            Result := True;
            exit;
          end;
          Highlighter.Next;
        end;
    end;
  end;
end;

{ TmneEngine }

function GetFileImageIndex(const FileName: string): Integer;
var
  AExtensions: TStringList;
  s: string;
begin
  s := ExtractFileExt(FileName);
  if LeftStr(s, 1) = '.' then
    s := Copy(s, 2, MaxInt);

  AExtensions := TStringList.Create;
  try
    Engine.Perspective.Groups[0].EnumExtensions(AExtensions);//TODO bad bad
    if AExtensions.IndexOf(s) >= 0 then
      Result := 2
    else
      Result := 1;//any file
  finally
    AExtensions.Free;
  end;
end;

{ TSQLFileCategory }

function TSQLFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynSQLSyn.Create(nil);
end;

{ TTApacheFileCategory }

function TApacheFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynApacheSyn.Create(nil);
end;

{ TINIFileCategory }

function TINIFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

{ TTXTFileCategory }

function TTXTFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

{ TXMLFileCategory }

function TXMLFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynXMLSyn.Create(nil);
end;

{ TXMLFile }

procedure TXMLFile.NewSource;
begin
  SynEdit.Text := '<?xml version="1.0" encoding="iso-8859-1" ?>';
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('');
  SynEdit.CaretY := 2;
  SynEdit.CaretX := 3;
end;

initialization
  with Engine do
  begin
    //Categories.Add('', TTXTFile, TTXTFileCategory);
    Categories.Add(TTXTFileCategory.Create('txt'));
    Categories.Add(TSQLFileCategory.Create('sql'));
    Categories.Add(TApacheFileCategory.Create('apache', []));
    Categories.Add(TINIFileCategory.Create('ini'));
    Categories.Add(TXMLFileCategory.Create('xml'));

    Groups.Add(TTXTFile, 'txt', 'TXT files', 'txt', ['txt'], []);
    Groups.Add(TSQLFile, 'sql', 'SQL files', 'SQL', ['sql'], [fgkAssociated, fgkMember, fgkBrowsable]);
    Groups.Add(TApacheFile, 'htaccess', 'htaccess files', 'apache', ['htaccess', 'conf'], [fgkAssociated, fgkBrowsable]);
    Groups.Add(TINIFile, 'xml', 'XML files', 'xml', ['xml'], [fgkMember, fgkBrowsable]);
    Groups.Add(TXMLFile, 'ini', 'INI files', 'ini', ['ini'], [fgkAssociated, fgkBrowsable]);
  end;
  Engine.AddInstant('Python', ['py'], TSynPythonSyn, []);
end.
