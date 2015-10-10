unit mnePHPClasses;

{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics,
  Contnrs, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  SynHighlighterCSS, SynHighlighterSQL, SynHighlighterXML,
  SynHighlighterJScript, SynHighlighterXHTML, SynHighlighterMultiProc,
  EditorDebugger, EditorClasses, PHP_xDebug, mneClasses;

type

  { TPHPFile }

  TPHPFile = class(TSourceEditorFile)
  protected
  public
    procedure NewContent; override;
    procedure OpenInclude; override;
    function CanOpenInclude: Boolean; override;
  end;

  { TXHTMLFile }

  TXHTMLFile = class(TPHPFile)
  protected
  public
    procedure NewContent; override;
  end;

  { TCssFile }

  TCssFile = class(TSourceEditorFile)
  public
  end;

  { TJSFile }

  TJSFile = class(TSourceEditorFile)
  public
  end;

  { THTMLFile }

  THTMLFile = class(TSourceEditorFile)
  public
  end;

  { TXHTMLFileCategory }

  TXHTMLFileCategory = class(TTextFileCategory)
  private
    procedure ExtractKeywords(Files, Variables, Identifiers: TStringList);
  protected
    procedure InitMappers; override;
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddCompletion(AKeyword: string; AKind: integer);
    procedure DoExecuteCompletion(Sender: TObject); override;
  public
  end;

  { TCSSFileCategory }

  TCSSFileCategory = class(TTextFileCategory)
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  { TJSFileCategory }

  TJSFileCategory = class(TTextFileCategory)
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  TPHPRunMode = (prunConsole, prunUrl);

  { TPHPProject }

  { TPHPProjectOptions }

  TPHPProjectOptions = class(TEditorProjectOptions)
  private
    FRootUrl: string;
    FRunMode: TPHPRunMode;
  public
    constructor Create; override;
    function Show: Boolean; override;
  published
    property RunMode: TPHPRunMode read FRunMode write FRunMode;
    property RootUrl: string read FRootUrl write FRootUrl;
  end;
{
}
  { TPHPPerspective }

  { TPHPTendency }

  TPHPTendency = class(TEditorTendency)
  private
    FHTMLHelpFile: string;
    FPHPHelpFile: string;
    FPHPPath: string;
  protected
    function CreateDebugger: TEditorDebugger; override;
    function CreateOptions: TEditorProjectOptions; override;
    procedure Init; override;
  public
    procedure Run; override;
    constructor Create; override;
    procedure Show; override;
  published
    property PHPPath: string read FPHPPath write FPHPPath;
    property PHPHelpFile: string read FPHPHelpFile write FPHPHelpFile;
    property HTMLHelpFile: string read FHTMLHelpFile write FHTMLHelpFile;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, HTMLProcessor, PHPProcessor, SynEditStrConst, mnePHPConfigForms;

{ TPHPProject }

constructor TPHPProjectOptions.Create;
begin
  inherited;
end;

function TPHPProjectOptions.Show: Boolean;
begin
  Result :=inherited Show;
end;

{ TXHTMLFile }

procedure TXHTMLFile.NewContent;
begin
  SynEdit.Text :=   '<?xml version="1.0" encoding="UTF-8"?>';
  SynEdit.Lines.Add('<!DOCTYPE html PUBLIC');
  SynEdit.Lines.Add('  "-//W3C//DTD XHTML 1.0 Strict//EN"');
  SynEdit.Lines.Add('  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">');
  SynEdit.Lines.Add('<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">');
  SynEdit.Lines.Add('  <head>');
  SynEdit.Lines.Add('    <title></title>');
  SynEdit.Lines.Add('  </head>');
  SynEdit.Lines.Add('  <body>');
  SynEdit.Lines.Add('  </body>');
  SynEdit.Lines.Add('</html>');

  SynEdit.CaretY := 2;
  SynEdit.CaretX := 9;
end;

{ TPHPFile }

procedure TPHPFile.NewContent;
begin
  SynEdit.Text := '<?php';
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('?>');
  SynEdit.CaretY := 2;
  SynEdit.CaretX := 3;
end;

procedure TPHPFile.OpenInclude;
var
  P: TPoint;
  Attri: TSynHighlighterAttributes;
  aToken: string;
  aTokenType: integer;
  aStart: integer;

  function TryOpen: boolean;
  begin
    if (aToken[1] = '/') or (aToken[1] = '\') then
      aToken := RightStr(aToken, Length(aToken) - 1);
    aToken := Engine.ExpandFileName(aToken);
    Result := FileExists(aToken);
    if Result then
      Engine.Files.OpenFile(aToken);
  end;

begin
  inherited;
  if Engine.Files.Current <> nil then
  begin
    if Engine.Files.Current.Group.Category is TXHTMLFileCategory then
    begin
      P := SynEdit.CaretXY;
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      aToken := DequoteStr(aToken);
      if (aToken <> '') and (TtkTokenKind(aTokenType) = tkString) then
      begin
        aToken := StringReplace(aToken, '/', '\', [rfReplaceAll, rfIgnoreCase]);
        if not TryOpen then
        begin
          aToken := ExtractFileName(aToken);
          TryOpen;
        end;
      end;
    end;
  end;
end;

function TPHPFile.CanOpenInclude: Boolean;
var
  P: TPoint;
  Attri: TSynHighlighterAttributes;
  aToken: string;
  aTokenType: integer;
  aStart: integer;
begin
  Result := False;
  if (Group <> nil) then
  begin
    if Group.Category is TXHTMLFileCategory then
    begin
      P := SynEdit.CaretXY;
      aToken := '';
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      Result := (aToken <> '') and (TtkTokenKind(aTokenType) = tkString);
    end;
  end;
end;

{ TPHPTendency }

procedure TPHPTendency.Run;
var
  aFile: string;
  aRoot: string;
  aUrlMode: TPHPRunMode;
begin
  //if (Engine.Files.Current <> nil) and (fgkExecutable in Engine.Files.Current.Group.Kind) then

  if Engine.Tendency.Debug <> nil then
  if Engine.Tendency.Debug.Running then
    Engine.Tendency.Debug.Action(dbaRun)
  else
  begin
    if (Engine.Session.IsOpened) then
    begin
      aFile := Engine.Session.Project.RootDir;
      aFile := ExpandToPath(aFile, Engine.Session.Project.RootDir);
      aUrlMode := (Engine.Session.Project.Options as TPHPProjectOptions).RunMode;
      aRoot := IncludeTrailingPathDelimiter(Engine.Session.Project.RootDir);
    end
    else
    begin
      //Check the file is executable
      if (Engine.Files.Current <> nil) and (fgkExecutable in Engine.Files.Current.Group.Kind) then
      begin
        aFile := Engine.Files.Current.Name;
        aUrlMode := prunConsole;
      end
    end;

    if aFile <> '' then
    begin
      case aUrlMode of
        prunUrl:
        begin
          if Engine.Session.IsOpened then
          begin
            if SameText((MidStr(aFile, 1, Length(aRoot))), aRoot) then
            begin
              aFile := MidStr(aFile, Length(aRoot) + 1, MaxInt);
              aFile := IncludeSlash((Engine.Session.Project.Options as TPHPProjectOptions).RootUrl) + aFile;
              //ShellExecute(0, 'open', PChar(aFile), '', PChar(ExtractFilePath(aFile)), SW_SHOWNOACTIVATE);
            end;
          end;
        end;
        prunConsole:
        begin
          {$ifdef windows}
          ExecuteProcess('cmd ',['/c "php.exe "' + aFile + '" & pause'], []);
          {$endif}

          {$ifdef linux}
          {$endif}

          {$ifdef macos}
          {$endif}
        end;
      end;
    end;
  end;
end;

constructor TPHPTendency.Create;
begin
  inherited Create;
end;

procedure TPHPTendency.Show;
begin
  with TPHPConfigForm.Create(Application) do
  begin
    FTendency := Self;
    Retrive;
    if ShowModal = mrOK then
    begin
      Apply;
    end;
  end;
end;

function TPHPTendency.CreateDebugger: TEditorDebugger;
begin
  Result := TPHP_xDebug.Create;
end;

function TPHPTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TPHPProjectOptions.Create;
end;

procedure TPHPTendency.Init;
begin
  FCapabilities := [capRun, capProjectOptions, capOptions];
  FTitle := 'PHP project';
  FDescription := 'PHP Files, *.php, *.inc';
  FName := 'PHP';
  FImageIndex := -1;
  AddGroup('php', 'html');
  AddGroup('html', 'html');
  AddGroup('css', 'css');
  AddGroup('js', 'js');
end;

{ TCSSFileCategory }

function TCSSFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynCSSSyn.Create(nil);
end;

procedure TCSSFileCategory.InitMappers;
begin
  with Highlighter as TSynCssSyn do
  begin
    Mapper.Add(SpaceAttri, attWhitespace);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(SelectorAttri, attName);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(MeasurementUnitAttri, attVariable);
  end;
end;

{ TXHTMLFileCategory }

function TXHTMLFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynXHTMLSyn.Create(nil);
end;

procedure TXHTMLFileCategory.DoAddCompletion(AKeyword: string; AKind: integer);
begin
  Completion.ItemList.Add(AKeyword);
end;

procedure TXHTMLFileCategory.DoExecuteCompletion(Sender: TObject);
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
    Completion.ItemList.Clear;
    aSynEdit := (Sender as TSynCompletion).TheForm.CurrentEditor as TCustomSynEdit;
    if (aSynEdit <> nil) and (Highlighter is TSynXHTMLSyn) then
    begin
      aPHPProcessor := (Highlighter as TSynXHTMLSyn).Processors.IndexOf('php');
      aHTMLProcessor := (Highlighter as TSynXHTMLSyn).Processors.IndexOf('html');
      P := aSynEdit.CaretXY;
      GetHighlighterAttriAtRowColEx2(aSynEdit, P, S, aTokenType, aStart, Attri, aRange);
      aProcessor := RangeToProcessor(aRange);
      if aTokenType = Ord(tkProcessor) then
        Abort
      //CanExecute := False
      else if aProcessor = aHTMLProcessor then
      begin
        Completion.TheForm.Caption := 'HTML';
        EnumerateKeywords(Ord(tkKeyword), sHTMLKeywords, Highlighter.IdentChars, @DoAddCompletion);
      end
      else if aProcessor = aPHPProcessor then
      begin
        if aTokenType = Ord(tkComment) then
          Abort
        else if aTokenType = Ord(tkString) then
        begin
          //EnumerateKeywords(Ord(tkSQL), sSQLKeywords, Highlighter.IdentChars, @DoAddCompletion);
        end
        else
        begin
          Completion.TheForm.Caption := 'PHP';
          //load keyowrds
          EnumerateKeywords(Ord(tkKeyword), sPHPControls, Highlighter.IdentChars, @DoAddCompletion);
          EnumerateKeywords(Ord(tkKeyword), sPHPKeywords, Highlighter.IdentChars, @DoAddCompletion);
          EnumerateKeywords(Ord(tkFunction), sPHPFunctions, Highlighter.IdentChars, @DoAddCompletion);
          EnumerateKeywords(Ord(tkValue), sPHPConstants, Highlighter.IdentChars, @DoAddCompletion);
          // load a variable
          aVariables := THashedStringList.Create;
          aIdentifiers := THashedStringList.Create;

          //Add system variables
          ExtractStrings([','], [], PChar(sPHPVariables), aVariables);
          for i := 0 to aVariables.Count - 1 do
            aVariables[i] := '$' + aVariables[i];

          //extract keywords from external files
          if (Engine.Session.IsOpened) and (Engine.Session.Project.RootDir <> '') then
          begin
            if Engine.Options.CollectAutoComplete then
            begin
              if ((GetTickCount - Engine.Session.Project.CachedAge) > (Engine.Options.CollectTimeout * 1000)) then
              begin
                Engine.Session.Project.CachedVariables.Clear;
                Engine.Session.Project.CachedIdentifiers.Clear;
                aFiles := TStringList.Create;
                try
                  EnumFileList(Engine.Session.Project.RootDir, '*.php', Engine.Options.IgnoreNames, aFiles, 1000, 3, True, Engine.Session.IsOpened);//TODO check the root dir if no project opened
                  r := aFiles.IndexOf(Engine.Files.Current.Name);
                  if r >= 0 then
                    aFiles.Delete(r);
                  ExtractKeywords(aFiles, Engine.Session.Project.CachedVariables, Engine.Session.Project.CachedIdentifiers);
                finally
                  aFiles.Free;
                end;
              end;
              aVariables.AddStrings(Engine.Session.Project.CachedVariables);
              aIdentifiers.AddStrings(Engine.Session.Project.CachedIdentifiers);
              Engine.Session.Project.CachedAge := GetTickCount;
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

procedure TXHTMLFileCategory.ExtractKeywords(Files, Variables, Identifiers: TStringList);
var
  aFile: TStringList;
  s: string;
  i, f: integer;
  aPHPProcessor: integer;
  aHighlighter: TSynXHTMLSyn;
begin
  aHighlighter := TSynXHTMLSyn.Create(nil);
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

procedure TXHTMLFileCategory.InitMappers;
begin
  with Highlighter as TSynXHTMLSyn do
  begin
    Mapper.Add(WhitespaceAttri, attWhitespace);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(KeywordAttri, attKeyword);
    Mapper.Add(DocumentAttri, attDocument);
    Mapper.Add(ValueAttri, attValue);
    Mapper.Add(FunctionAttri, attStandard);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(HtmlAttri, attOutter);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(VariableAttri, attVariable);
    Mapper.Add(ProcessorAttri, attDirective);
  end;
end;

procedure TXHTMLFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  if FCompletion = nil then
  begin
    FCompletion := TmneSynCompletion.Create(nil);
    FCompletion.Width := 340;
    FCompletion.EndOfTokenChr := '{}()[].<>/\:!&*+-=%;';//do not add $
    FCompletion.OnExecute := @DoExecuteCompletion;
    FCompletion.ShortCut := scCtrl + VK_SPACE;
    FCompletion.CaseSensitive := False;
    //FCompletion.OnPaintItem
  end;
  FCompletion.AddEditor(vSynEdit);
end;

{ TJSFileCategory }

function TJSFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynJScriptSyn.Create(nil);
end;

procedure TJSFileCategory.InitMappers;
begin
  with Highlighter as TSynJScriptSyn do
  begin
    Mapper.Add(SpaceAttri, attWhitespace);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(NonReservedKeyAttri, attName);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(EventAttri, attVariable);
  end;
end;

initialization
  with Engine do
  begin
    Categories.Add(TXHTMLFileCategory.Create('php/html', [fckPublish]));
    Categories.Add(TCSSFileCategory.Create('css', [fckPublish]));
    Categories.Add(TJSFileCategory.Create('js', [fckPublish]));

    Groups.Add(TPHPFile, 'php', 'PHP Files', 'php/html', ['php', 'inc'], [fgkAssociated, fgkExecutable, fgkMember, fgkBrowsable, fgkMain]);
    Groups.Add(TXHTMLFile, 'html', 'HTML Files', 'php/html', ['html', 'xhtml', 'htm', 'tpl'], [fgkAssociated, fgkMember, fgkBrowsable]);
    Groups.Add(TCssFile, 'css', 'CSS Files', 'css', ['css'], [fgkAssociated, fgkMember, fgkBrowsable]);
    Groups.Add(TJSFile,'js', 'Java Script Files', 'js', ['js'], [fgkAssociated, fgkExecutable, fgkMember, fgkBrowsable]);

    Tendencies.Add(TPHPTendency);
  end;
end.
