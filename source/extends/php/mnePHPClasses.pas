unit mnePHPClasses;

{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics,
  LCLVersion, Process, Contnrs, LCLintf, LCLType, Dialogs, EditorOptions,
  SynEditHighlighter, SynEditSearch, SynEdit, Registry, EditorEngine,
  mnXMLRttiProfile, mnXMLUtils, SynEditTypes, SynCompletion,
  SynHighlighterHashEntries, EditorProfiles, SynHighlighterCSS,
  SynHighlighterSQL, SynHighlighterXML, SynHighlighterJScript,
  mnSynHighlighterXHTML, mnSynHighlighterMultiProc, HTMLProcessor,
  EditorClasses, dbgpServers, mneClasses, mneRunFrames, mnePHPTendencyFrames,
  EditorRun;

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

  TXHTMLFileCategory = class(TCodeFileCategory)
  private
  protected
    procedure InitMappers; override;
    procedure ExtractPHPKeywords(Files, Variables, Identifiers: TStringList);
    procedure DoAddKeywords; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoPrepareCompletion(AEditor: TCustomSynEdit); override;
  public
    function CreateHighlighter: TSynCustomHighlighter; override;
    function GetColorPrefix: string; override;
  end;

  { TCSSFileCategory }

  TCSSFileCategory = class(TCodeFileCategory)
  protected
    procedure InitMappers; override;
  public
    function CreateHighlighter: TSynCustomHighlighter; override;
    function GetColorPrefix: string; override;
  end;

  { TJSFileCategory }

  TJSFileCategory = class(TTextFileCategory)
  protected
    procedure InitMappers; override;
  public
    function GetColorPrefix: string; override;
    function CreateHighlighter: TSynCustomHighlighter; override;
  end;

  { TPHPProjectOptions }

  TPHPProjectOptions = class(TEditorProjectOptions)
  private
  public
    constructor Create; override;
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
  end;

  TPHPTendency = class;

  { TPHPDebugger }

  TPHPDebugger = class(TdbgpDebugger)
  protected
    Tendency: TPHPTendency;
  public
    procedure Start; override;
  end;

  { TPHPTendency }

  TPHPTendency = class(TEditorTendency)
  private
    FDebugPort: string;
  protected
    function CreateDebugger: TEditorDebugger; override;
    procedure Created; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    constructor Create; override;
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
    function CreateProjectOptions: TEditorProjectOptions; override;
  published
    property DebugPort: string read FDebugPort write FDebugPort;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, PHPProcessor, SynEditStrConst;

{ TPHPDebugger }

procedure TPHPDebugger.Start;
begin
  Server.Port := Tendency.FDebugPort;
  inherited Start;
end;

{ TPHPProject }

constructor TPHPProjectOptions.Create;
begin
  inherited;
end;

procedure TPHPProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
{var
  aFrame: TPHPProjectFrame;}
begin
  {aFrame := TPHPProjectFrame.Create(AOwner);
  aFrame.Project := AProject;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);}
end;

{ TXHTMLFile }

procedure TXHTMLFile.NewContent;
begin
  SynEdit.Lines.Text := '<!DOCTYPE html>';
  SynEdit.Lines.Add('<html lang="en">');
  SynEdit.Lines.Add('  <head>');
  SynEdit.Lines.Add('    <title></title>');
  SynEdit.Lines.Add('    <meta charset="utf-8">');
  SynEdit.Lines.Add('  </head>');
  SynEdit.Lines.Add('  <body>');
  SynEdit.Lines.Add('  ');
  SynEdit.Lines.Add('  </body>');
  SynEdit.Lines.Add('</html>');

  SynEdit.CaretY := 2;
  SynEdit.CaretX := 7;
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
    aToken := Engine.ExpandFile(aToken);
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

procedure TPHPTendency.DoRun(Info: TmneRunInfo);
var
  aRunItem: TmneRunItem;
begin
  {if rnaCompile in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;

    aRunItem.Info.Command := Info.Command;
    if aRunItem.Info.Command = '' then
      aRunItem.Info.Command := 'php.exe';

    Engine.Session.Run.Start;
  end;}

  if rnaExecute in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.StatusMessage := 'Running';
    aRunItem.Info.Run.Pause := Info.Pause;
    aRunItem.Info.Run.Console := Info.Console;
    aRunItem.Info.Run.Command := Info.Command;
    if aRunItem.Info.Run.Command = '' then
    begin
      {$ifdef windows}
      aRunItem.Info.Run.Command := 'php.exe';
      {$else linux}
      aRunItem.Info.Run.Command := 'php';
      {$endif}
    end;

    if Info.MainFile <> '' then
      aRunItem.Info.Run.AddParam(Info.MainFile);

    aRunItem.Info.Run.AddParam(RunOptions.Params);
    aRunItem.Info.Run.AddParam(Engine.Session.Project.RunOptions.Params);
  end;
end;

constructor TPHPTendency.Create;
begin
  inherited Create;
  FDebugPort := '9000';
end;

procedure TPHPTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
  aTendencyFrame: TPHPTendencyFrame;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := ATendency.RunOptions;
  aFrame.CommandEdit.Items.Add('php');
  aFrame.Caption := 'Options';
  AddFrame(aFrame);

  aTendencyFrame := TPHPTendencyFrame.Create(AOwner);
  aTendencyFrame.Tendency := ATendency;
  aTendencyFrame.Caption := 'PHP Options';
  AddFrame(aTendencyFrame);
end;

function TPHPTendency.CreateDebugger: TEditorDebugger;
begin
  Result := TPHPDebugger.Create;
  (Result as TPHPDebugger).Tendency := Self;
end;

function TPHPTendency.CreateProjectOptions: TEditorProjectOptions;
begin
  Result := TPHPProjectOptions.Create;
end;

procedure TPHPTendency.Created;
begin
  FCapabilities := [capExecute, capDebug, capEval, capTrace, capDebugServer];
  FHaveOptions := True;
  FTitle := 'PHP project';
  FDescription := 'PHP Files, *.php, *.inc';
  FName := 'PHP';
  FImageIndex := -1;
end;

{ TCSSFileCategory }

function TCSSFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynCSSSyn.Create(nil);
end;

procedure TCSSFileCategory.InitMappers;
begin
  with Highlighter as TSynCssSyn do
  begin
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
    {.$if declared(LCL_FULLVERSION) and (LCL_FULLVERSION > 1060000) }
    {.$if (LCLVersion > 1060000) }
    {$if (LCL_FULLVERSION > 1060000) }
    Mapper.Add(SelectorAttri, attVariable);
    Mapper.Add(MeasurementUnitAttri, attVariable);
    {$endif}
  end;
end;

function TCSSFileCategory.GetColorPrefix: string;
begin
  Result := '#';
end;

{ TXHTMLFileCategory }

function TXHTMLFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynXHTMLSyn.Create(nil);
end;

procedure TXHTMLFileCategory.DoPrepareCompletion(AEditor: TCustomSynEdit);
var
  aVariables: THashedStringList;
  aIdentifiers: THashedStringList;
  aCurrent, Token: string;
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
  aSynEdit := Completion.TheForm.CurrentEditor as TCustomSynEdit;
  if (aSynEdit <> nil) and (Highlighter is TSynXHTMLSyn) then
  begin
    aPHPProcessor := (Highlighter as TSynXHTMLSyn).Processors.IndexOf('php');
    aHTMLProcessor := (Highlighter as TSynXHTMLSyn).Processors.IndexOf('html');
    P := aSynEdit.CaretXY;
    GetHighlighterAttriAtRowColExtend(aSynEdit, P, aCurrent, aTokenType, aStart, Attri, aRange);
    aProcessor := RangeToProcessor(aRange);
    if aTokenType = Ord(tkProcessor) then
      Abort
    //CanExecute := False
    else if aProcessor = aHTMLProcessor then
    begin
      Completion.TheForm.Caption := 'HTML';
      EnumerateKeywords(Ord(attKeyword), sHTMLKeywords, Highlighter.IdentChars, @AddKeyword);
    end
    else if aProcessor = aPHPProcessor then
    begin
      if aTokenType = Ord(tkComment) then
        Abort
      else if aTokenType = Ord(tkString) then
      begin
        //EnumerateKeywords(Ord(attEmbed), sSQLKeywords, Highlighter.IdentChars, @AddKeyword);
      end
      else
      begin
        Completion.TheForm.Caption := 'PHP';
        // load a variable
        aVariables := THashedStringList.Create;
        aIdentifiers := THashedStringList.Create;

        //Add system variables
        ExtractStrings([','], [], PChar(sPHPVariables), aVariables);
        for i := 0 to aVariables.Count - 1 do
          aVariables[i] := '$' + aVariables[i];

        //extract keywords from external files
        if Engine.Options.CollectAutoComplete and (Engine.Session.GetRoot <> '') then
        begin
          if ((GetTickCount - CachedAge) > (Engine.Options.CollectTimeout * 1000)) then
          begin
            CachedVariables.Clear;
            CachedIdentifiers.Clear;
            aFiles := TStringList.Create;
            try
              EnumFileList(Engine.Session.GetRoot, '*.php', Engine.Options.IgnoreNames, aFiles, 1000, 3, True);//TODO check the root dir if no project opened
              r := aFiles.IndexOf(Engine.Files.Current.FileName);
              if r >= 0 then
                aFiles.Delete(r);
              ExtractPHPKeywords(aFiles, CachedVariables, CachedIdentifiers);
            finally
              aFiles.Free;
            end;
          end;
          aVariables.AddStrings(CachedVariables);
          aIdentifiers.AddStrings(CachedIdentifiers);
          CachedAge := GetTickCount;
        end;
        //add current file Identifiers and Variables
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
                  Token := Highlighter.GetToken;
                  if (Token <> '$') and (aVariables.IndexOf(Token) < 0) then
                    aVariables.Add(Token);
                end
                else if (Highlighter.GetTokenKind = Ord(tkIdentifier)) then
                begin
                  Token := Highlighter.GetToken;
                  if aIdentifiers.IndexOf(Token) < 0 then
                    aIdentifiers.Add(Token);
                end;
              end;
              Highlighter.Next;
            end;
          end;

          for i := 0 to aVariables.Count - 1 do
            AddKeyword(aVariables[i], 'Variable', attVariable, True);

          for i := 0 to aIdentifiers.Count - 1 do
            AddKeyword(aIdentifiers[i], 'Identifier', attIdentifier, True);
        finally
          aIdentifiers.Free;
          aVariables.Free;
        end;
      end;
    end;
  end;
  Completion.Sort;
end;

function TXHTMLFileCategory.GetColorPrefix: string;
begin
  Result := '#';
end;

procedure TXHTMLFileCategory.ExtractPHPKeywords(Files, Variables, Identifiers: TStringList);
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
    Mapper.Add(WhitespaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(DocumentAttri, attDocument);
    Mapper.Add(KeywordAttri, attKeyword);
    Mapper.Add(TypeAttri, attDataType);
    Mapper.Add(FunctionAttri, attCommon);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(VariableAttri, attVariable);
    Mapper.Add(ProcessorAttri, attDirective);
  end;
end;

procedure TXHTMLFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  Completion.EndOfTokenChr := '{}()[].<>/\:!&*+-=%;';//do not add $
end;

procedure TXHTMLFileCategory.DoAddKeywords;
begin
  inherited;
  //load keyowrds
  EnumerateKeywords(Ord(attKeyword), sPHPControls, Highlighter.IdentChars, @AddKeyword);
  EnumerateKeywords(Ord(attKeyword), sPHPKeywords, Highlighter.IdentChars, @AddKeyword);
  EnumerateKeywords(Ord(attCommon), sPHPFunctions, Highlighter.IdentChars, @AddKeyword);
  EnumerateKeywords(Ord(attCommon), sPHPConstants, Highlighter.IdentChars, @AddKeyword);
end;

{ TJSFileCategory }

function TJSFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynJScriptSyn.Create(nil);
end;

procedure TJSFileCategory.InitMappers;
begin
  with Highlighter as TSynJScriptSyn do
  begin
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment, ord(tkComment));
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(IdentifierAttri, attIdentifier, ord(tkIdentifier));
    Mapper.Add(NonReservedKeyAttri, attVariable, ord(tkVariable));
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(EventAttri, attVariable);   
    Mapper.Add(BracketAttri, attSymbol);
  end;
end;

function TJSFileCategory.GetColorPrefix: string;
begin
  Result := '#';
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TPHPTendency);
    Categories.Add(TXHTMLFileCategory.Create(TPHPTendency, 'php/html','PHP and HTML', [fckIncludes, fckPublish]));
    Categories.Add(TCSSFileCategory.Create(TPHPTendency, 'css', 'CSS', [fckPublish]));
    Categories.Add(TJSFileCategory.Create(TPHPTendency, 'js', 'Javascript JS', [fckPublish]));

    Groups.Add(TPHPFile, 'php', 'PHP', TXHTMLFileCategory, ['.php', '.inc'], [fgkAssociated, fgkBrowsable, fgkMain], [capExecute, capDebug]);
    Groups.Add(TXHTMLFile, 'html', 'HTML', TXHTMLFileCategory, ['.html', '.xhtml', '.htm', '.tpl'], [fgkAssociated, fgkBrowsable]);
    Groups.Add(TCssFile, 'css', 'CSS', TCSSFileCategory, ['.css'], [fgkAssociated, fgkBrowsable]);
    Groups.Add(TJSFile,'js', 'Java Script', TJSFileCategory, ['.js'], [fgkAssociated, fgkBrowsable], [capExecute]);
    Groups.Add(TJSFile,'json', 'JSON', TJSFileCategory, ['.json'], [fgkAssociated, fgkBrowsable]);
  end;
end.
