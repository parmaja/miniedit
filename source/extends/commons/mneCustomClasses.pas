unit mneCustomClasses;

{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *}

{**
* Loading custom language defeined by user
}
interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics,
  Contnrs, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  FileUtil, mnSynHighlighterD, EditorDebugger, EditorClasses, mneClasses,
  mneCompileProjectOptions, EditorRun, mneConsoleClasses, mnSynHighlighterMultiProc,
  mneConsoleForms;

type

  { TSynCustomSyn }

  TSynCustomSyn = class(TSynMultiProcSyn)
  private
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitProcessors; override;
  published
  end;


  { TCustomFile }

  TCustomFile = class(TSourceEditorFile)
  protected
  public
    procedure NewContent; override;
    procedure OpenInclude; override;
    function CanOpenInclude: Boolean; override;
  end;

  { TCustomFileCategory }

  TCustomFileCategory = class(TCodeFileCategory)
  private
  protected
    procedure InitMappers; override;
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddKeywords; override;
  public
  end;

  { TDProjectOptions }

  TCustomProjectOptions = class(TCompilerProjectOptions)
  private
  public
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
  end;

  { TDTendency }

  TCustomTendency = class(TEditorTendency)
  private
  protected
    function CreateDebugger: TEditorDebugger; override;
    procedure Init; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    constructor Create; override;
    function CreateOptions: TEditorProjectOptions; override;
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
  published
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, SynEditStrConst, mneCompilerTendencyFrames;

{ TSynCustomSyn }

function TSynCustomSyn.GetIdentChars: TSynIdentChars;
begin
  //  Result := TSynValidStringChars + ['&', '#', ';', '$'];
  Result := TSynValidStringChars + ['&', '#', '$'];
end;

function TSynCustomSyn.GetSampleSource: string;
begin
  Result := '';
end;

class function TSynCustomSyn.GetLanguageName: string;
begin
  Result := 'Custom';
end;

constructor TSynCustomSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultFilter := SYNS_FilterD;
end;

procedure TSynCustomSyn.InitProcessors;
begin
  inherited;
  Processors.Add(TDProcessor.Create(Self, 'D'));

  Processors.MainProcessor := 'D';
  Processors.DefaultProcessor := 'D';
end;

{ TDProject }

procedure TCustomProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TCompilerProjectOptionsForm.Create(AOwner);
  (aFrame as TCompilerProjectOptionsForm).FProject := AProject;
  aFrame.Caption := 'Compiler';
  AddFrame(aFrame);
  aFrame := TCompilerProjectOptionsForm.Create(AOwner);
  (aFrame as TCompilerProjectOptionsForm).FProject := AProject;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

{ TCustomFile }

procedure TCustomFile.NewContent;
begin
  SynEdit.Text := cDSample;
end;

{ TCustomFile }

procedure TCustomFile.OpenInclude;
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
    if Engine.Files.Current.Group.Category is TCustomFileCategory then
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

function TCustomFile.CanOpenInclude: Boolean;
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
    if Group.Category is TCustomFileCategory then
    begin
      P := SynEdit.CaretXY;
      aToken := '';
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      //Result := (aToken <> '') and (TtkTokenKind(aTokenType) = tkString);
    end;
  end;
end;

{ TCustomTendency }

procedure TCustomTendency.DoRun(Info: TmneRunInfo);
var
  i: Integer;
  aPath: string;
  Options: TCustomProjectOptions;
  aRunItem: TmneRunItem;
begin
  if (Engine.Session.Project.Options is TCustomProjectOptions) then
    Options := (Engine.Session.Project.Options as TCustomProjectOptions)
  else
    Options := TCustomProjectOptions.Create;//Default options

  Engine.Session.Run.Clear;

  if rnaCompile in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;

    aRunItem.Info.Command := Info.Command;
    if aRunItem.Info.Command = '' then
      aRunItem.Info.Command := 'dmd.exe';

    aRunItem.Info.Mode := runOutput;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;

    aRunItem.Info.Params := Info.MainFile + #13;
    if Info.OutputFile <> '' then
      aRunItem.Info.Params := aRunItem.Info.Params + '-of' + Info.OutputFile + #13;

    aRunItem.Info.Message := 'Compiling ' + Info.OutputFile;
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
    aRunItem.Info.Message := 'Running ' + Info.OutputFile;
    aRunItem.Info.Mode := Options.RunMode;
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Pause := Options.PauseConsole;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.OutputFile);;
    aRunItem.Info.Command := ChangeFileExt(Info.OutputFile, '.exe');
    if Options.RunParams <> '' then
      aRunItem.Info.Params := aRunItem.Info.Params + Options.RunParams + #13;
  end;

  Engine.Session.Run.Start;
end;

constructor TCustomTendency.Create;
begin
  inherited Create;
end;

procedure TCustomTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TCompilerTendencyFrame;
begin
  aFrame := TCompilerTendencyFrame.Create(AOwner);
  aFrame.FTendency := ATendency;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

function TCustomTendency.CreateDebugger: TEditorDebugger;
begin
  Result := nil;
end;

function TCustomTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TCustomProjectOptions.Create;
end;

procedure TCustomTendency.Init;
begin
  FCapabilities := [capRun, capCompile, capLink, capOptions];
  FTitle := 'D Lang';
  FDescription := 'D Files, *.D, *.inc';
  FName := 'D';
  FImageIndex := -1;
  AddGroup('D', 'd');
  AddGroup('cfg', 'cfg');
  AddGroup('ini', 'ini');
  AddGroup('txt', 'txt');
  //AddGroup('json', 'json');
end;

{ TCustomFileCategory }

function TCustomFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynDSyn.Create(nil);
end;

procedure TCustomFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  FCompletion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
  IdentifierID := ord(tkIdentifier);
end;

procedure TCustomFileCategory.DoAddKeywords;
begin
  EnumerateKeywords(Ord(tkKeyword), sDKeywords, Highlighter.IdentChars, @DoAddCompletion);
  EnumerateKeywords(Ord(tkFunction), sDFunctions, Highlighter.IdentChars, @DoAddCompletion);
end;

procedure TCustomFileCategory.InitMappers;
begin
  with Highlighter as TSynDSyn do
  begin
    Mapper.Add(WhitespaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(KeywordAttri, attKeyword);
    Mapper.Add(DocumentAttri, attDocument);
    Mapper.Add(ValueAttri, attValue);
    Mapper.Add(FunctionAttri, attStandard);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(VariableAttri, attVariable);
    Mapper.Add(ProcessorAttri, attDirective);
  end;
end;

initialization
  with Engine do
  begin
{    Categories.Add(TCustomFileCategory.Create('Custom', [fckPublish]));
    Groups.Add(TCustomFile, 'Custom', 'Custom Files', 'Custom', ['d', 'inc'], [fgkAssociated, fgkExecutable, fgkMember, fgkBrowsable, fgkMain]);
    Tendencies.Add(TCustomTendency);}
  end;
end.
