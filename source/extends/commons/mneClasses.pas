unit mneClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, Contnrs, StdCtrls,
  LCLintf, LCLType,
  Dialogs, EditorOptions, SynEditHighlighter, SynEditSearch, SynEdit,
  Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  SynHighlighterXML, mnSynHighlighterApache, mnSynHighlighterConfig, SynHighlighterINI,
  SynHighlighterPython;

type

  { TmneProjectFileCategory }

  TmneProjectFileCategory = class(TVirtualCategory)
  protected
    function GetIsText: Boolean; override;
  public
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  end;

  TApacheFile = class(TTextEditorFile)
  public
  end;

  TINIConfigFile = class(TTextEditorFile)
  public
  end;

  TCFGConfigFile = class(TTextEditorFile)
  public
  end;

  TYamlConfigFile = class(TTextEditorFile)
  public
  end;

  TTXTFile = class(TTextEditorFile)
  public
  end;

  TXMLFile = class(TSourceEditorFile)
  public
    procedure NewContent; override;
  end;

  { TApacheFileCategory }

  TApacheFileCategory = class(TTextFileCategory)
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  { TCFGFileCategory }

  TCFGFileCategory = class(TTextFileCategory)
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  { TYamlFileCategory }

  TYamlFileCategory = class(TTextFileCategory)
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  { TTXTFileCategory }

  TTXTFileCategory = class(TTextFileCategory)
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  { TMDFileCategory }

  TMDFileCategory = class(TTextFileCategory)
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  { TXMLFileCategory }

  TXMLFileCategory = class(TTextFileCategory)
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  { TmneEngine }

function ColorToRGBHex(Color: TColor; ColorPrefix: string = '#'; Reverse: Boolean = false): string;
function RGBHexToColor(Value: string; ColorPrefix: string = '#'; Reverse: Boolean = false): TColor;
function CheckBoxStateToStates(State: TCheckBoxState): TThreeStates;
function StatesToCheckBoxState(State: TThreeStates): TCheckBoxState;

const
  sSoftwareRegKey = 'Software\miniEdit\';

function GetHighlighterAttriAtRowColExtend(SynEdit: TCustomSynEdit; const XY: TPoint; out Token: string; out TokenType, Start: integer; out Attri: TSynHighlighterAttributes; out Range: Pointer): boolean;

implementation

uses
  IniFiles, mnStreams, mnUtils;

function ColorToRGBHex(Color: TColor; ColorPrefix: string; Reverse: Boolean): string;
var
  aRGB: TColorRef;
begin
  aRGB := ColorToRGB(Color);
  Result := '';//temporary
  if Reverse then
    FmtStr(Result, '%s%.2x%.2x%.2x', [ColorPrefix, GetBValue(aRGB), GetGValue(aRGB), GetRValue(aRGB)])
  else
    FmtStr(Result, '%s%.2x%.2x%.2x', [ColorPrefix, GetRValue(aRGB), GetGValue(aRGB), GetBValue(aRGB)]);
end;

function RGBHexToColor(Value: string; ColorPrefix: string; Reverse: Boolean): TColor;
var
  R, G, B: byte;
  S: byte;
begin
  if LeftStr(Value, 1) = ColorPrefix then
    Delete(Value, 1, 1);
  if Value <> '' then
  begin
    if Length(Value) = 3 then
    begin
      R := StrToIntDef('$' + Copy(Value, 1, 1) + Copy(Value, 1, 1), 0);
      G := StrToIntDef('$' + Copy(Value, 2, 1) + Copy(Value, 2, 1), 0);
      B := StrToIntDef('$' + Copy(Value, 3, 1) + Copy(Value, 3, 1), 0);
    end
    else
    begin
      if Length(Value) > 6 then
        Value := RightStr(Value, 6);
      R := StrToIntDef('$' + Copy(Value, 1, 2), 0);
      G := StrToIntDef('$' + Copy(Value, 3, 2), 0);
      B := StrToIntDef('$' + Copy(Value, 5, 2), 0);
    end;
    if Reverse then
    begin
      S := R;
      R := B;
      B := S;
    end;
    Result := RGB(R, G, B);
  end
  else
    Result := clBlack;
end;

function CheckBoxStateToStates(State: TCheckBoxState): TThreeStates;
const
  r: array[TCheckBoxState] of TThreeStates  = (stateFalse, stateTrue, stateNone);
begin
  Result := r[State];
end;

function StatesToCheckBoxState(State: TThreeStates): TCheckBoxState;
const
  r: array[TThreeStates] of TCheckBoxState  = (cbGrayed, cbUnchecked, cbChecked);
begin
  Result := r[State];
end;

type
  TSynCustomHighlighterHack = class(TSynCustomHighlighter);

function GetHighlighterAttriAtRowColExtend(SynEdit: TCustomSynEdit; const XY: TPoint; out Token: string; out TokenType, Start: integer; out Attri: TSynHighlighterAttributes; out Range: Pointer): boolean;
var
  PosX, PosY: integer;
  Line: string;
  aToken: string;
begin
  TokenType := 0;
  Token := '';
  Attri := nil;
  Result := False;
  PosY := XY.Y - 1;
  with SynEdit do
  begin
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
          if (PosX >= Start) and (PosX <= Start + Length(aToken)) then
          begin
            Attri := Highlighter.GetTokenAttribute;
            TokenType := Highlighter.GetTokenKind;
            //Token := MidStr(aToken, 1, PosX - Start);
            Token := aToken;
            Result := True;
            exit;
          end;
          Highlighter.Next;
        end;
    end;
  end;
end;

{ TmneProjectFileCategory }

function TmneProjectFileCategory.GetIsText: Boolean;
begin
  Result := False;
end;

function TmneProjectFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

procedure TmneProjectFileCategory.InitMappers;
begin
end;

{ TYamlFileCategory }

function TYamlFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynApacheSyn.Create(nil);
end;

procedure TYamlFileCategory.InitMappers;
begin
  with Highlighter as TSynApacheSyn do
  begin
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(SectionAttri, attDirective);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
  end;
end;

{ TMDFileCategory }

function TMDFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

procedure TMDFileCategory.InitMappers;
begin
end;

{ TCFGFileCategory }

function TCFGFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynConfigSyn.Create(nil);
end;

procedure TCFGFileCategory.InitMappers;
begin
  with Highlighter as TSynConfigSyn do
  begin
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(DocumentAttri, attDocument);
    Mapper.Add(SectionAttri, attDirective);
    Mapper.Add(KeyAttri, attVariable);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
  end;
end;

{ TTApacheFileCategory }

function TApacheFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynApacheSyn.Create(nil);
end;

procedure TApacheFileCategory.InitMappers;
begin
  with Highlighter as TSynApacheSyn do
  begin
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(SectionAttri, attDirective);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
  end;
end;

{ TTXTFileCategory }

function TTXTFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

procedure TTXTFileCategory.InitMappers;
begin
end;

{ TXMLFileCategory }

function TXMLFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynXMLSyn.Create(nil);
end;

procedure TXMLFileCategory.InitMappers;
begin
  with Highlighter as TSynXMLSyn do
  begin
    Mapper.Add(ElementAttri, attVariable);
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(EntityRefAttri, attIdentifier);
    Mapper.Add(ProcessingInstructionAttri, attDirective);
    Mapper.Add(CDATAAttri, attEmbed);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(DocTypeAttri, attComment);
    Mapper.Add(AttributeAttri, attVariable);
    Mapper.Add(NamespaceAttributeAttri, attVariable);
    Mapper.Add(AttributeValueAttri, attQuotedString);
    Mapper.Add(NamespaceAttributeAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
  end;
end;

{ TXMLFile }

procedure TXMLFile.NewContent;
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
    Categories.Add(TmneProjectFileCategory.Create(DefaultProject.Tendency, 'mne-project', 'MiniEdit project'));
    Groups.Add(nil, 'mne-project', 'Project', TmneProjectFileCategory, ['.mne-project'], [fgkAssociated, fgkBrowsable, fgkUneditable]);

    Categories.Add(TTXTFileCategory.Create(DefaultProject.Tendency, 'txt', 'Text'));
    Groups.Add(TTXTFile, 'txt', 'Text', TTXTFileCategory, ['.txt', '.text', '.log', 'readme'], [fgkDefault]);
    Groups.Add(TTXTFile, 'motd', 'motd', TTXTFileCategory, ['.motd'], []);

    Categories.Add(TMDFileCategory.Create(DefaultProject.Tendency, 'md', 'Markdown'));
    Groups.Add(TTXTFile, 'md', 'MarkDown', TMDFileCategory, ['.md'], []);

    Categories.Add(TApacheFileCategory.Create(DefaultProject.Tendency, 'apache', 'Apache Config', []));
    Groups.Add(TApacheFile, 'htaccess', 'htaccess', TApacheFileCategory, ['.htaccess', '.conf'], [fgkAssociated, fgkBrowsable]);

    Categories.Add(TXMLFileCategory.Create(DefaultProject.Tendency, 'xml', 'XML files'));
    Groups.Add(TXMLFile, 'xml', 'XML', TXMLFileCategory, ['.xml', '.lpi'], [fgkAssociated, fgkBrowsable]);

    Categories.Add(TCFGFileCategory.Create(DefaultProject.Tendency, 'cfg', 'Config and INI'));
    Groups.Add(TINIConfigFile, 'ini', 'INI', TCFGFileCategory, ['.ini', '.pls', '.isl'], [fgkAssociated, fgkBrowsable]);
    Groups.Add(TCFGConfigFile, 'cfg', 'Config', TCFGFileCategory, ['.cfg', '.conf', '.config', 'config'], [fgkAssociated, fgkBrowsable]);

    Categories.Add(TYamlFileCategory.Create(DefaultProject.Tendency, 'yaml', 'YAML'));
    Groups.Add(TYamlConfigFile, 'yaml', 'YAML', TYamlFileCategory, ['.yaml'], [fgkAssociated, fgkBrowsable]);
  end;
end.
