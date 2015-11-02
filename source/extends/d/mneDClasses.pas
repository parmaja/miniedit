unit mneDClasses;

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
  LazFileUtils, SynHighlighterD, EditorDebugger, EditorClasses, mneClasses,
  mneCompileProjectOptions, EditorRun, DebugClasses, mneConsoleClasses,
  mneConsoleForms, uTerminal;

type

  { TDFile }

  TDFile = class(TSourceEditorFile)
  protected
  public
    procedure NewContent; override;
    procedure OpenInclude; override;
    function CanOpenInclude: Boolean; override;
  end;

  { TDFileCategory }

  TDFileCategory = class(TTextFileCategory)
  private
    procedure ExtractKeywords(Files, Identifiers: TStringList);
  protected
    procedure InitMappers; override;
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddCompletion(AKeyword: string; AKind: integer);
    procedure DoExecuteCompletion(Sender: TObject); override;
  public
  end;

  { TDProjectOptions }

  TDProjectOptions = class(TCompilerProjectOptions)
  private
  public
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddProjectCallBack); override;
  published
  end;

  { TDTendency }

  TDTendency = class(TEditorTendency)
  private
//    FCompiler: string;
  protected
    function CreateDebugger: TEditorDebugger; override;
    function CreateOptions: TEditorProjectOptions; override;
    procedure Init; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    constructor Create; override;
    procedure Show; override;
  published
//    property Compiler: string read FCompiler write FCompiler;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, SynHighlighterMultiProc, SynEditStrConst, mneDConfigForms, mneDProjectFrames;

{ TDProject }

procedure TDProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddProjectCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TCompilerProjectOptionsForm.Create(AOwner);
  (aFrame as TCompilerProjectOptionsForm).Project := AProject;
  aFrame.Caption := 'Compiler';
  AddFrame(aFrame);
  aFrame := TDProjectFrame.Create(AOwner);
  (aFrame as TDProjectFrame).Project := AProject;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

{ TDFile }

procedure TDFile.NewContent;
begin
  SynEdit.Text := cDSample;
end;

{ TDFile }

procedure TDFile.OpenInclude;
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
    if Engine.Files.Current.Group.Category is TDFileCategory then
    begin
      P := SynEdit.CaretXY;
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      aToken := DequoteStr(aToken);
      //if (aToken <> '') and (TtkTokenKind(aTokenType) = tkString) then
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

function TDFile.CanOpenInclude: Boolean;
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
    if Group.Category is TDFileCategory then
    begin
      P := SynEdit.CaretXY;
      aToken := '';
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      //Result := (aToken <> '') and (TtkTokenKind(aTokenType) = tkString);
    end;
  end;
end;

{ TDTendency }

procedure TDTendency.DoRun(Info: TmneRunInfo);
var
  aParams: string;
  s: string;
  i: Integer;
  aPath: string;
  Options: TDProjectOptions;
  aRunItem: TmneRunItem;
begin
  if (Engine.Session.IsOpened) then
    Options := (Engine.Session.Project.Options as TDProjectOptions)
  else
    Options := TDProjectOptions.Create;//Default options

  Engine.Session.Run.Clear;

  if rnaCompile in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;

    aRunItem.Info.Command := Info.Command;
    if aRunItem.Info.Command = '' then
      aRunItem.Info.Command := 'dmd.exe';

    aRunItem.Info.Mode := runOutput;
    aRunItem.Info.Pause := true;
    aRunItem.Info.Title := ExtractFileNameOnly(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;

    aRunItem.Info.Params := Info.MainFile + #13;
    if Options.OutputFile <> '' then
      aRunItem.Info.Params := aRunItem.Info.Params + '-of' + Options.OutputFile + #13;
    //aRunItem.Info.Params := aRunItem.Info.Params + '-color=on' + #13; //not work :(

    for i := 0 to Options.Paths.Count - 1 do
    begin
      aPath := Trim(Options.Paths[i]);
      if aPath <>'' then
      begin
        if Options.ExpandPaths then
          aPath := Engine.ExpandFile(aPath);
        aRunItem.Info.Params := aRunItem.Info.Params + '-I' +aPath + #13;
      end;
    end;

    //aRunItem.Info.Params := aRunItem.Info.Params + '-v'#13;

    if Options.ConfigFile <> '' then
      aRunItem.Info.Params := aRunItem.Info.Params + '@' + Engine.EnvReplace(Options.ConfigFile) + #13;
  end;

  if rnaExecute in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;

    aRunItem.Info.Mode := Options.RunMode;
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Pause := true;
    aRunItem.Info.Title := ExtractFileNameOnly(Info.MainFile);;
    aRunItem.Info.Command := ChangeFileExt(Info.MainFile, '.exe');
    if Options.RunParams <> '' then
      aRunItem.Info.Params := aRunItem.Info.Params + Options.RunParams + #13;
  end;

  Engine.Session.Run.Start;
end;

constructor TDTendency.Create;
begin
  inherited Create;
end;

procedure TDTendency.Show;
begin
  with TDConfigForm.Create(Application) do
  begin
    FTendency := Self;
    Retrieve;
    if ShowModal = mrOK then
    begin
      Apply;
    end;
  end;
end;

function TDTendency.CreateDebugger: TEditorDebugger;
begin
  Result := nil;
end;

function TDTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TDProjectOptions.Create;
end;

procedure TDTendency.Init;
begin
  FCapabilities := [capRun, capCompile, capLink, capProjectOptions, capOptions];
  FTitle := 'D Lang';
  FDescription := 'D Files, *.D, *.inc';
  FName := 'D';
  FImageIndex := -1;
  AddGroup('D', 'html');
  AddGroup('html', 'html');
  AddGroup('css', 'css');
  AddGroup('js', 'js');
end;

{ TDFileCategory }

function TDFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynDSyn.Create(nil);
end;

procedure TDFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  if FCompletion = nil then
  begin
    FCompletion := TmneSynCompletion.Create(nil);
    FCompletion.Width := 340;
    FCompletion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
    FCompletion.OnExecute := @DoExecuteCompletion;
    FCompletion.ShortCut := scCtrl + VK_SPACE;
    FCompletion.CaseSensitive := False;
    //FCompletion.OnPaintItem
  end;
  FCompletion.AddEditor(vSynEdit);
end;

procedure TDFileCategory.DoAddCompletion(AKeyword: string; AKind: integer);
begin
  Completion.ItemList.Add(AKeyword);
end;

procedure TDFileCategory.DoExecuteCompletion(Sender: TObject);
var
  aIdentifiers: THashedStringList;
  Current, Token: string;
  i, r: integer;
  aSynEdit: TCustomSynEdit;
  aTokenType, aStart: integer;
  aRange: pointer;
  P: TPoint;
  Attri: TSynHighlighterAttributes;
  aFiles: TStringList;
begin
  inherited;
  Screen.Cursor := crHourGlass;
  Completion.ItemList.BeginUpdate;
  try
    Completion.ItemList.Clear;
    aSynEdit := (Sender as TSynCompletion).TheForm.CurrentEditor as TCustomSynEdit;
    if (aSynEdit <> nil) and (Highlighter is TSynDSyn) then
    begin
      P := aSynEdit.CaretXY;
      GetHighlighterAttriAtRowColExtend(aSynEdit, P, Current, aTokenType, aStart, Attri, aRange);
      Completion.TheForm.Font.Size := aSynEdit.Font.Size;
      Completion.TheForm.Font.Color := aSynEdit.Font.Color;
      Completion.TheForm.Color := aSynEdit.Color;
      Completion.TheForm.Caption := 'D';
      //Completion.AutoUseSingleIdent := True;
      //CanExecute := False
      if aTokenType = Ord(tkComment) then
        //Abort
      else if aTokenType = Ord(tkString) then
      begin
        //EnumerateKeywords(Ord(tkSQL), sSQLKeywords, Highlighter.IdentChars, @DoAddCompletion);
      end
      else
      begin
        //load keyowrds
        EnumerateKeywords(Ord(tkKeyword), sDKeywords, Highlighter.IdentChars, @DoAddCompletion);
        EnumerateKeywords(Ord(tkFunction), sDFunctions, Highlighter.IdentChars, @DoAddCompletion);
        aIdentifiers := THashedStringList.Create;

        //extract keywords from external files
        if Engine.Options.CollectAutoComplete and (Engine.Session.GetRoot <> '') then
        begin
          if ((GetTickCount - Engine.Session.CachedAge) > (Engine.Options.CollectTimeout * 1000)) then
          begin
            Engine.Session.CachedVariables.Clear;
            Engine.Session.CachedIdentifiers.Clear;
            aFiles := TStringList.Create;
            try
              EnumFileList(Engine.Session.GetRoot, '*.d', Engine.Options.IgnoreNames, aFiles, 1000, 3, True, Engine.Session.IsOpened);//TODO check the root dir if no project opened
              r := aFiles.IndexOf(Engine.Files.Current.Name);
              if r >= 0 then
                aFiles.Delete(r);
              ExtractKeywords(aFiles, Engine.Session.CachedIdentifiers);
            finally
              aFiles.Free;
            end;
          end;
          aIdentifiers.AddStrings(Engine.Session.CachedIdentifiers);
          Engine.Session.CachedAge := GetTickCount;
        end;
        //add current file Identifiers
        try
          Highlighter.ResetRange;
          for i := 0 to aSynEdit.Lines.Count - 1 do
          begin
            Highlighter.SetLine(aSynEdit.Lines[i], 1);
            while not Highlighter.GetEol do
            begin
              if (Highlighter.GetTokenPos <> (aStart - 1)) then
              begin
                if (Highlighter.GetTokenKind = Ord(tkIdentifier)) then
                begin
                  Token := Highlighter.GetToken;
                  if aIdentifiers.IndexOf(Token) < 0 then
                    aIdentifiers.Add(Token);
                end;
              end;
              Highlighter.Next;
            end;
          end;

          for i := 0 to aIdentifiers.Count - 1 do
            DoAddCompletion(aIdentifiers[i], Ord(tkIdentifier));
        finally
          aIdentifiers.Free;
        end;
      end;
    end;
    (Completion.ItemList as TStringList).Sort;
  finally
    Completion.ItemList.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TDFileCategory.ExtractKeywords(Files, Identifiers: TStringList);
var
  aFile: TStringList;
  s: string;
  i, f: integer;
  aHighlighter: TSynDSyn;
begin
  aHighlighter := TSynDSyn.Create(nil);
  aFile := TStringList.Create;
  try
    for f := 0 to Files.Count - 1 do
    begin
      aFile.LoadFromFile(Files[f]);
      aHighlighter.ResetRange;
      for i := 0 to aFile.Count - 1 do
      begin
        aHighlighter.SetLine(aFile[i], 1);
        while not aHighlighter.GetEol do
        begin
          if (aHighlighter.GetTokenKind = Ord(tkIdentifier)) then
          begin
            s := aHighlighter.GetToken;
            if Identifiers.IndexOf(s) < 0 then
              Identifiers.Add(s);
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

procedure TDFileCategory.InitMappers;
begin
  with Highlighter as TSynDSyn do
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

initialization
  with Engine do
  begin
    Categories.Add(TDFileCategory.Create('D', [fckPublish]));

    Groups.Add(TDFile, 'D', 'D Files', 'D', ['d', 'inc'], [fgkAssociated, fgkExecutable, fgkMember, fgkBrowsable, fgkMain]);

    Tendencies.Add(TDTendency);
  end;
end.
