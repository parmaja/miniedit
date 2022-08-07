unit mneBasicClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}
interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics,
  Contnrs, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  LazFileUtils, SynHighlighterVB, EditorClasses, mneClasses,
  mneCompilerProjectFrames, EditorRun,
  mneRunFrames;

type

  { TBasicFile }

  TBasicFile = class(TSourceEditorFile)
  protected
  public
    procedure NewContent; override;
    procedure OpenInclude; override;
    function CanOpenInclude: Boolean; override;
  end;

  { TBasicFileCategory }

  TBasicFileCategory = class(TCodeFileCategory)
  private
  protected
    procedure DoFixTabsSpaces(Sender: TObject);
    procedure InitMappers; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddKeywords; override;
  public
    function CreateHighlighter: TSynCustomHighlighter; override;
    procedure EnumMenuItems(AddItems: TAddClickCallBack); override;
  end;

  { TBasicProjectOptions }

  TBasicProjectOptions = class(TEditorProjectOptions)
  private
  public
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
  end;

  { TBasicTendency }

  TBasicTendency = class(TEditorTendency)
  private
  protected
    procedure Created; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    constructor Create; override;
    procedure HelpKeyword(AWord:string); override;
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
    function CreateOptions: TEditorProjectOptions; override;
    procedure EnumRunCommands(Items: TStrings); override;
  published

  end;

implementation

uses
  {Basic53,}IniFiles, mnStreams, mnUtils, mnSynHighlighterMultiProc, SynEditStrConst, mneBasicProjectFrames;

{ TDProject }

procedure TBasicProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  (aFrame as TRunFrameOptions).Options := AProject.RunOptions;
  AProject.Tendency.EnumRunCommands((aFrame as TRunFrameOptions).CommandEdit.Items);
  aFrame.Caption := 'Run';
  AddFrame(aFrame);
  aFrame := TBasicProjectFrame.Create(AOwner);
  (aFrame as TBasicProjectFrame).Project := AProject;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

{ TBasicFile }

procedure TBasicFile.NewContent;
begin
  //SynEdit.Text := cBasicSample;
end;

{ TBasicFile }

procedure TBasicFile.OpenInclude;
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
    if Engine.Files.Current.Group.Category is TBasicFileCategory then
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

function TBasicFile.CanOpenInclude: Boolean;
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
    if Group.Category is TBasicFileCategory then
    begin
      P := SynEdit.CaretXY;
      aToken := '';
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      //Result := (aToken <> '') and (TtkTokenKind(aTokenType) = tkString);
    end;
  end;
end;

{ TBasicTendency }

procedure TBasicTendency.DoRun(Info: TmneRunInfo);
var
  aRunItem: TmneRunItem;
begin
  Engine.Session.Run.Clear;
  Engine.SendAction(eaClearOutput);

  if rnaExecute in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.Run.Pause := Info.Pause;
    aRunItem.Info.Run.Console := Info.Console;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.StatusMessage := 'Runing ' + Info.MainFile;
    if RunOptions.Require <> '' then
        aRunItem.Info.Run.AddParam('-l '+ RunOptions.Require);
    if rnaDebug in Info.Actions then
        aRunItem.Info.Run.AddParam('-e '+ '"require(''mobdebug'').start()"'); //using mobdebug

    aRunItem.Info.Run.Command := Info.Command;
    if Info.Command = '' then
    begin
      {$ifdef windows}
        aRunItem.Info.Run.Command := 'basic.exe';
      {$else}
        aRunItem.Info.Run.Command := 'basic';
      {$endif}
    end;
    aRunItem.Info.Run.AddParam(' "' + Info.MainFile + '"');
  end
  else if (rnaLint in Info.Actions) or (rnaCompile in Info.Actions) then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Run.Silent := True;
    aRunItem.Info.Run.Console := False;

    if (rnaCompile in Info.Actions) then
    begin
      aRunItem.Info.StatusMessage := 'Compiling ' + Info.MainFile;
      if Info.OutputFile <> '' then
        aRunItem.Info.Run.AddParam('-o "'+ Info.OutputFile + '"');
    end
    else
    begin
      aRunItem.Info.StatusMessage := 'Linting ' + Info.MainFile;
      aRunItem.Info.Run.AddParam('-p ');
    end;

    //aRunItem.Info.Run.Command := Info.LintCommand;
    //if Info.Command = '' then
    //begin
      {$ifdef windows}
        aRunItem.Info.Run.Command := 'basic.exe';
      {$else}
        aRunItem.Info.Run.Command := 'basic';
      {$endif}
      aRunItem.Info.Run.AddParam(' "' + Info.MainFile + '"');
    //end
  end;

  if Engine.Session.Run.Active then //if there is items ready to run
    Engine.Session.Run.Start(Debugger);
end;

constructor TBasicTendency.Create;
begin
  inherited Create;
end;

procedure TBasicTendency.HelpKeyword(AWord: string);
begin
  inherited;
  //OpenURL('' + AWord);
end;

procedure TBasicTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := ATendency.RunOptions;
  EnumRunCommands(aFrame.CommandEdit.Items);
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

function TBasicTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TBasicProjectOptions.Create;
end;

procedure TBasicTendency.EnumRunCommands(Items: TStrings);
begin
  inherited;
  Items.Add('sbasicg -r "?mainfile"');
  Items.Add('sbasicg -r "?file"');
end;

procedure TBasicTendency.Created;
begin
  FCapabilities := [capDebug, capTrace, capDebugServer, capExecute, capCompile, capLint];
  FHaveOptions := True;
  Title := 'Basic Lang';
  FDescription := 'Basic Files, *.bas';
  FName := 'Basic';
  OutputExtension := '.exe';
  FImageIndex := -1;
end;

{ TBasicFileCategory }

function TBasicFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynVBSyn.Create(nil);
end;

procedure TBasicFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  FCompletion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
  IdentifierID := ord(tkIdentifier);
end;

procedure TBasicFileCategory.DoAddKeywords;
begin
  //EnumerateKeywords(Ord(tkKeyword), sBasicKeywords, Highlighter.IdentChars, @AddKeyword);
  //EnumerateKeywords(Ord(tkFunction), sBasicFunctions, Highlighter.IdentChars, @AddKeyword);
end;

procedure TBasicFileCategory.DoFixTabsSpaces(Sender: TObject);
var
  S: string;
begin
  with (Engine.Files.Current as TSourceEditorFile) do
  begin
    SynEdit.BeginUndoBlock;
    try
      S := SynEdit.TextBetweenPoints[Point(1,1), Point(length(SynEdit.Lines[SynEdit.Lines.Count - 1]) + 1, SynEdit.Lines.Count)];
      SynEdit.TextBetweenPoints[Point(1,1), Point(length(SynEdit.Lines[SynEdit.Lines.Count - 1]) + 1, SynEdit.Lines.Count)] := ConvertIndents(S, SynEdit.TabWidth, Tendency.IndentMode);
      //SynEdit.Text := ConvertIndents(SynEdit.Text, SynEdit.TabWidth, Tendency.IndentMode);
    finally
      SynEdit.EndUndoBlock;
    end;
  end;
end;

procedure TBasicFileCategory.EnumMenuItems(AddItems: TAddClickCallBack);
begin
  //AddItems('FixTabsSpaces', 'Fix Tabs/Spaces', @DoFixTabsSpaces);
end;

procedure TBasicFileCategory.InitMappers;
begin
  with Highlighter as TSynVBSyn do
  begin
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(CommentAttri, attComment);
    //Mapper.Add(TypeAttri, attDataType);
    //Mapper.Add(DocumentAttri, attDocument);
    //Mapper.Add(FunctionAttri, attStandard);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
    //Mapper.Add(VariableAttri, attVariable);
    //Mapper.Add(ProcessorAttri, attDirective);
  end;
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TBasicTendency);
    Categories.Add(TBasicFileCategory.Create(TBasicTendency, 'Basic', 'Basic', [fckPublish]));
    Groups.Add(TBasicFile, 'Basic', 'Basic', TBasicFileCategory, ['.bas'], [fgkDefault, fgkAssociated, fgkBrowsable, fgkMain], [capExecute]);
  end;
end.
