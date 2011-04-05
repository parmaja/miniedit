unit mneClasses;

{$mode delphi}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

{$DEFINE SYN_HEREDOC}

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, Contnrs,
  LCLintf,
  Dialogs, EditorOptions, SynEditHighlighter, SynEditSearch, SynEdit,
  Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  SynHighlighterCSS, SynHighlighterSQL, SynHighlighterXML, SynHighlighterApache,
  SynHighlighterJScript, SynHighlighterHTMLPHP, SynHighlighterPas;

type
  TphpFile = class(TEditorFile)
  protected
    procedure NewSource; override;
    //procedure EditorPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType); override;
  public
  end;

  TCssFile = class(TEditorFile)
  public
  end;

  TJSFile = class(TEditorFile)
  public
  end;

  THTMLFile = class(TEditorFile)
  public
  end;

  TSQLFile = class(TEditorFile)
  public
  end;

  TApacheFile = class(TEditorFile)
  public
  end;

  TINIFile = class(TEditorFile)
  public
  end;

  TTXTFile = class(TEditorFile)
  public
  end;

  TXMLFile = class(TEditorFile)
  public
    procedure NewSource; override;
  end;
  //

  { TPASFile }

  TPASFile = class(TEditorFile)
  protected
    procedure NewSource; override;
  public
  end;

  TPHPFileCategory = class(TFileCategory)
  private
    procedure ExtractKeywords(Files, Variables, Identifiers: TStringList);
  protected
    procedure DoAddCompletion(AKeyword: string; AKind: integer);
    function CreateHighlighter: TSynCustomHighlighter; override;
    procedure OnExecuteCompletion(Kind: TSynCompletionType; Sender: TObject; var CurrentInput: string; var x, y: integer; var CanExecute: boolean); override;
  public
  end;

  TCSSFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TJSFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
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

  { TPASFileCategory }

  TPASFileCategory = class(TFileCategory)
  private
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;


  //
  TmneEngine = class(TEditorEngine)
  protected
  public
    constructor Create; override;
  end;

function ColorToRGBHex(Color: TColor): string;
function RGBHexToColor(Value: string): TColor;

const
  sSoftwareRegKey = 'Software\LightPHPEdit\';

function GetFileImageIndex(const FileName: string): integer;

implementation

uses
  IniFiles, mnXMLStreams;

function ColorToRGBHex(Color: TColor): string;
{var
  aRGB: COLORREF;}
begin
{  aRGB := ColorToRGB(Color);
  FmtStr(Result, '%s%.2x%.2x%.2x', ['#', GetRValue(aRGB), GetGValue(aRGB), GetBValue(aRGB)]);}
end;

function RGBHexToColor(Value: string): TColor;
{var
  R, G, B: byte;}
begin
{  if LeftStr(Value, 1) = '#' then
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
    Result := clBlack;}
end;

{ TmneEngine }

function GetFileImageIndex(const FileName: string): integer;
var
  AExtensions: TStringList;
  s: string;
begin
  s := ExtractFileExt(FileName);
  if LeftStr(s, 1) = '.' then
    s := Copy(s, 2, MaxInt);

  AExtensions := TStringList.Create;
  try
    Engine.Groups[0].EnumExtensions(AExtensions);
    if AExtensions.IndexOf(s) >= 0 then
      Result := 2
    else
      Result := 1;//any file
  finally
    AExtensions.Free;
  end;
end;

{ TPASFileCategory }

function TPASFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynPASSyn.Create(nil);
end;

{ TPASFile }

procedure TPASFile.NewSource;
begin
  inherited NewSource;
end;

constructor TmneEngine.Create;
begin
  inherited;
  Categories.Add('HTML/PHP', TphpFile, TPHPFileCategory, [fckPublish]);
  Categories.Add('CSS', TCssFile, TCSSFileCategory, [fckPublish]);
  Categories.Add('JS', TJSFile, TJSFileCategory, [fckPublish]);
  Categories.Add('htaccess', TApacheFile, TApacheFileCategory, []);
  Categories.Add('SQL', TSQLFile, TSQLFileCategory);
  Categories.Add('INI', TINIFile, TINIFileCategory);
  Categories.Add('TXT', TTXTFile, TTXTFileCategory);
  Categories.Add('XML', TXMLFile, TXMLFileCategory);
  Categories.Add('XML', TPASFile, TPASFileCategory);

  Groups.Add('PHP Files', 'php', 'HTML/PHP', ['php'], [fgkExecutable, fgkPublish, fgkBrowsable, fgkMainIcon]);
  Groups.Add('PHPX Files', 'phpx', 'HTML/PHP', ['phpx'], [fgkExecutable, fgkPublish, fgkBrowsable, fgkMainIcon]);
  Groups.Add('HTML Files', 'html', 'HTML/PHP', ['html', 'tpl'], [fgkPublish, fgkBrowsable]);
  Groups.Add('CSS Files', 'css', 'CSS', ['css'], [fgkPublish, fgkBrowsable]);
  Groups.Add('Java Script Files', 'js', 'JS', ['js'], [fgkPublish, fgkBrowsable]);
  Groups.Add('SQL files', 'sql', 'SQL', ['sql'], [fgkPublish, fgkBrowsable]);
  Groups.Add('htaccess files', 'Apache', 'htaccess', ['htaccess', 'conf'], [fgkBrowsable]);
  Groups.Add('XML files', 'XML', 'XML', ['xml'], [fgkPublish, fgkBrowsable]);
  Groups.Add('INI files', 'ini', 'ini', ['ini'], []);
  Groups.Add('TXT files', 'TXT', 'TXT', ['txt'], []);
  Extenstion := 'mne-project';
end;

{ TphpFile }

procedure TphpFile.NewSource;
begin
  SynEdit.Text := '<?php';
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('?>');
  SynEdit.CaretY := 2;
  SynEdit.CaretX := 3;
end;

{ TCSSFileCategory }

function TCSSFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynCSSSyn.Create(nil);
end;

{ TPHPFileCategory }

function TPHPFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynHTMLPHPSyn.Create(nil);
end;

(*
procedure TphpFile.EditorPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
const
  OpenChars: array[0..3] of Char = ('{', '[', '(', '<');
  CloseChars: array[0..3] of Char = ('}', ']', ')', '>');
  function InBrackets(C: Char): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to SizeOf(OpenChars) - 1 do
    begin
      Result := (C = OpenChars[i]) or (C = CloseChars[i]);
      if Result then
        break;
    end;
  end;

  function CharToPixels(P: TBufferCoord): TPoint;
  begin
    Result := SynEdit.RowColumnToPixels(SynEdit.BufferToDisplayPos(P));
  end;
var
  P: TBufferCoord;
  Pix: TPoint;
  D: TDisplayCoord;
  S: string;
  C: Char;
  I: Integer;
  Attri: TSynHighlighterAttributes;
  start: Integer;
  TmpCharA, TmpCharB: Char;
  aTokenID: TtkTokenKind;
  aTokenType, aStart: Integer;
  aRange: pointer;
//  aProcessor: Integer;
begin
  inherited;
  if not SynEdit.SelAvail and (SynEdit.Highlighter <> nil) then
  begin
    P := SynEdit.CaretXY;
    D := SynEdit.DisplayXY;

    Start := SynEdit.SelStart;

    if (Start > 0) and (Start <= length(SynEdit.Text)) then
      TmpCharA := SynEdit.Text[Start]
    else
      TmpCharA := #0;

    if (Start < length(SynEdit.Text)) then
      TmpCharB := SynEdit.Text[Start + 1]
    else
      TmpCharB := #0;

    if InBrackets(TmpCharA) or InBrackets(TmpCharB) then
    begin
      if not InBrackets(TmpCharB) then
      begin
        S := TmpCharA;
        P.Char := P.Char - 1;
      end
      else
        S := TmpCharB;

      GetHighlighterAttriAtRowColEx2(SynEdit, P, S, aTokenType, aStart, Attri, aRange);
//      aProcessor := RangeToProcessor(aRange); // we must use it in next code
      if S <> '' then
        C := S[1]
      else
        C := #0;
      aTokenID := TtkTokenKind(aTokenType);
      if (C <> #0) and (aTokenID = tkSymbol) then
      begin
        for i := low(OpenChars) to High(OpenChars) do
        begin
          if (C = OpenChars[i]) or (C = CloseChars[i]) then
          begin
            Pix := CharToPixels(P);

            SynEdit.Canvas.Brush.Style := bsSolid; //Clear;
            SynEdit.Canvas.Font.Assign(SynEdit.Font);
            SynEdit.Canvas.Font.Style := Attri.Style;

            if (TransientType = ttAfter) then
            begin
              SynEdit.Canvas.Font.Color := TSynHTMLPHPSyn(SynEdit.Highlighter).BracketsAttri.Foreground;
              SynEdit.Canvas.Brush.Color := TSynHTMLPHPSyn(SynEdit.Highlighter).BracketsAttri.Background;
            end
            else
            begin
              SynEdit.Canvas.Font.Color := Attri.Foreground;
              SynEdit.Canvas.Brush.Color := Attri.Background;
            end;
            if SynEdit.Canvas.Font.Color = clNone then
              SynEdit.Canvas.Font.Color := SynEdit.Font.Color;
            if SynEdit.Canvas.Brush.Color = clNone then
              SynEdit.Canvas.Brush.Color := SynEdit.Color;

            SynEdit.Canvas.TextOut(Pix.X, Pix.Y, C);
            P := SynEdit.GetMatchingBracketEx(P);

            if (P.Char > 0) and (P.Line > 0) then
            begin
              Pix := CharToPixels(P);
              if Pix.X > SynEdit.Gutter.Width then
              begin
                if C = OpenChars[i] then
                  SynEdit.Canvas.TextOut(Pix.X, Pix.Y, CloseChars[i])
                else
                  SynEdit.Canvas.TextOut(Pix.X, Pix.Y, OpenChars[i]);
              end;
            end;
          end; //if
        end; //for i :=
        SynEdit.Canvas.Brush.Style := bsSolid;
      end;
    end;
  end;
end;

*)

procedure TPHPFileCategory.DoAddCompletion(AKeyword: string; AKind: integer);
begin
(*  Completion.InsertList.Add(AKeyword);
  case AKind of
    Ord(tkHTML): Completion.ItemList.Add('HTML \column{}\style{+B}\color{' + ColorToString((Highlighter as TSynHTMLPHPSyn).HtmlAttri.Foreground) + '}' + AKeyword + '\style{-B}');
    Ord(tkVariable): Completion.ItemList.Add('variable \column{}\style{+B}\color{' + ColorToString((Highlighter as TSynHTMLPHPSyn).VariableAttri.Foreground) + '}' + AKeyword + '\style{-B}');
    Ord(tkValue): Completion.ItemList.Add('value \column{}\style{+B}\color{' + ColorToString((Highlighter as TSynHTMLPHPSyn).ValueAttri.Foreground) + '}' + AKeyword + '\style{-B}');
    Ord(tkFunction): Completion.ItemList.Add('function \column{}\style{+B}\color{' + ColorToString((Highlighter as TSynHTMLPHPSyn).FunctionAttri.Foreground) + '}' + AKeyword + '\style{-B}');
    Ord(tkKeyword): Completion.ItemList.Add('keyword \column{}\style{+B}\color{' + ColorToString((Highlighter as TSynHTMLPHPSyn).KeywordAttribute.Foreground) + '}' + AKeyword + '\style{-B}');
    Ord(tkIdentifier): Completion.ItemList.Add('identifier \column{}\style{+B}\color{' + ColorToString((Highlighter as TSynHTMLPHPSyn).IdentifierAttribute.Foreground) + '}' + AKeyword + '\style{-B}');
    Ord(tkSQL): Completion.ItemList.Add('SQL \column{}\style{+B}\color{' + ColorToString((Highlighter as TSynHTMLPHPSyn).StringAttribute.Foreground) + '}' + AKeyword + '\style{-B}');
  end;
  *)
end;

procedure TPHPFileCategory.OnExecuteCompletion(Kind: TSynCompletionType; Sender: TObject; var CurrentInput: string; var x, y: integer; var CanExecute: boolean);
var
  aVariables: THashedStringList;
  aIdentifiers: THashedStringList;
  s: string;
  i, r: integer;
  aSynEdit: TCustomSynEdit;
  aProcessor: byte;
  aHTMLProcessor, aPHPProcessor: integer;
  aTokenType, aStart: integer;
  aRange: pointer;
  P: TPoint;
  Attri: TSynHighlighterAttributes;
  aFiles: TStringList;
begin
  inherited;
  Screen.Cursor := crHourGlass;
  try
    //Completion.ClearList;
    //aSynEdit := (Sender as TSynCompletion).Form.CurrentEditor;
    if (aSynEdit <> nil) and (Highlighter is TSynHTMLPHPSyn) then
    begin
      aPHPProcessor := (Highlighter as TSynHTMLPHPSyn).Processors.IndexOf('php');
      aHTMLProcessor := (Highlighter as TSynHTMLPHPSyn).Processors.IndexOf('html');
      P := aSynEdit.CaretXY;
      GetHighlighterAttriAtRowColEx2(aSynEdit, P, S, aTokenType, aStart, Attri, aRange);
      aProcessor := RangeToProcessor(aRange);
      if aTokenType = Ord(tkProcessor) then
        CanExecute := False
      else if aProcessor = aHTMLProcessor then
      begin
        //Completion.Title := 'HTML';
        EnumerateKeywords(Ord(tkKeyword), sHTMLKeywords, Highlighter.IdentChars, DoAddCompletion);
      end
      else if aProcessor = aPHPProcessor then
      begin
        if aTokenType = Ord(tkComment) then
          CanExecute := False
        else if aTokenType = Ord(tkString) then
        begin
          EnumerateKeywords(Ord(tkSQL), sSQLKeywords, Highlighter.IdentChars, DoAddCompletion);
        end
        else
        begin
          //Completion.Title := 'PHP';
          //load keyowrds
          EnumerateKeywords(Ord(tkKeyword), sPHPControls, Highlighter.IdentChars, DoAddCompletion);
          EnumerateKeywords(Ord(tkKeyword), sPHPKeywords, Highlighter.IdentChars, DoAddCompletion);
          EnumerateKeywords(Ord(tkFunction), sPHPFunctions, Highlighter.IdentChars, DoAddCompletion);
          EnumerateKeywords(Ord(tkValue), sPHPConstants, Highlighter.IdentChars, DoAddCompletion);
          // load a variable
          aVariables := THashedStringList.Create;
          aIdentifiers := THashedStringList.Create;

          //Add system variables
          ExtractStrings([','], [], PChar(sPHPVariables), aVariables);
          for i := 0 to aVariables.Count - 1 do
            aVariables[i] := '$' + aVariables[i];

          //extract keywords from external files
          if (Engine.Projects.IsOpened) and (Engine.Projects.Current.RootDir <> '') then
          begin
            if Engine.Options.CollectAutoComplete then
            begin
              if ((GetTickCount - Engine.Projects.Current.CachedAge) > (Engine.Options.CollectTimeout * 1000)) then
              begin
                Engine.Projects.Current.CachedVariables.Clear;
                Engine.Projects.Current.CachedIdentifiers.Clear;
                aFiles := TStringList.Create;
                try
                  EnumFileList('', Engine.Projects.Current.RootDir, '*.php', aFiles, 1000, Engine.Projects.IsOpened);
                  r := aFiles.IndexOf(Engine.Files.Current.Name);
                  if r >= 0 then
                    aFiles.Delete(r);
                  ExtractKeywords(aFiles, Engine.Projects.Current.CachedVariables, Engine.Projects.Current.CachedIdentifiers);
                finally
                  aFiles.Free;
                end;
              end;
              aVariables.AddStrings(Engine.Projects.Current.CachedVariables);
              aIdentifiers.AddStrings(Engine.Projects.Current.CachedIdentifiers);
              Engine.Projects.Current.CachedAge := GetTickCount;
            end;
          end;
          //add current file variables
          try
            Highlighter.ResetRange;
            for i := 0 to aSynEdit.Lines.Count - 1 do
            begin
              Highlighter.SetLine(aSynEdit.Lines[i], 1);
              while not Highlighter.GetEol do
              begin
                if (Highlighter.GetTokenPos <> (aStart - 1)) and (RangeToProcessor(Highlighter.GetRange) = aPHPProcessor) then
                begin
                  if (Highlighter.GetTokenKind = Ord(tkVariable)) then
                  begin
                    s := Highlighter.GetToken;
                    if (s <> '$') and (aVariables.IndexOf(s) < 0) then
                    begin
                      aVariables.Add(s);
                    end;
                  end
                  else if (Highlighter.GetTokenKind = Ord(tkIdentifier)) then
                  begin
                    s := Highlighter.GetToken;
                    if aIdentifiers.IndexOf(s) < 0 then
                    begin
                      aIdentifiers.Add(s);
                    end;
                  end;
                end;
                Highlighter.Next;
              end;
            end;

            for i := 0 to aVariables.Count - 1 do
              DoAddCompletion(aVariables[i], Ord(tkVariable));

            for i := 0 to aIdentifiers.Count - 1 do
              DoAddCompletion(aIdentifiers[i], Ord(tkIdentifier));
          finally
            aIdentifiers.Free;
            aVariables.Free;
          end;
        end;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TPHPFileCategory.ExtractKeywords(Files, Variables, Identifiers: TStringList);
var
  aFile: TStringList;
  s: string;
  i, f: integer;
  aPHPProcessor: integer;
  aHighlighter: TSynHTMLPHPSyn;
begin
  aHighlighter := TSynHTMLPHPSyn.Create(nil);
  aFile := TStringList.Create;
  try
    aPHPProcessor := aHighlighter.Processors.Find('php').Index;
    for f := 0 to Files.Count - 1 do
    begin
      aFile.LoadFromFile(Files[f]);
      aHighlighter.ResetRange;
      for i := 0 to aFile.Count - 1 do
      begin
        aHighlighter.SetLine(aFile[i], 1);
        while not aHighlighter.GetEol do
        begin
          if (RangeToProcessor(aHighlighter.GetRange) = aPHPProcessor) then
          begin
            if (aHighlighter.GetTokenKind = Ord(tkVariable)) then
            begin
              s := aHighlighter.GetToken;
              if (s <> '$') and (Variables.IndexOf(s) < 0) then
                Variables.Add(s);
            end
            else if (aHighlighter.GetTokenKind = Ord(tkIdentifier)) then
            begin
              s := aHighlighter.GetToken;
              if Identifiers.IndexOf(s) < 0 then
                Identifiers.Add(s);
            end;
          end;
          aHighlighter.Next;
        end;
      end;
    end;
  finally
    aHighlighter.Free;
    aFile.Free;
  end;
end;

{ TJSFileCategory }

function TJSFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynJScriptSyn.Create(nil);
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

end.
