unit mneCustomClasses;

{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
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
  LazFileUtils, mnSynHighlighterD, EditorClasses, mneClasses,
  mneCompilerProjectFrames, EditorRun, mnSynHighlighterMultiProc,
  mneRunFrames;

type

  { TSynCustomSyn }

  TSynCustomSyn = class(TSynMultiProcSyn)
  private
  protected
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
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddKeywords; override;
  public
    function CreateHighlighter: TSynCustomHighlighter; override;
  end;

  { TDProjectOptions }

  TCustomProjectOptions = class(TEditorProjectOptions)
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
    procedure Created; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    function CreateProjectOptions: TEditorProjectOptions; override;
    procedure CreateOptionsFrame(AOwner: TComponent; AddFrame: TAddFrameCallBack); override;
  published
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, SynEditStrConst;

{ TSynCustomSyn }

class function TSynCustomSyn.GetLanguageName: string;
begin
  Result := 'Custom';//TODO save it in variable
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
  aFrame := TCompilerProjectFrame.Create(AOwner);
  (aFrame as TCompilerProjectFrame).Project := AProject;
  aFrame.Caption := 'Compiler';
  AddFrame(aFrame);
  aFrame := TCompilerProjectFrame.Create(AOwner);
  (aFrame as TCompilerProjectFrame).Project := AProject;
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
  aRunItem: TmneRunItem;
  AOptions: TRunOptions;
begin
  AOptions := TRunOptions.Create;//Default options
  try
    AOptions.Copy(RunOptions);
    if (Engine.Session.Active) then
      AOptions.Merge(Engine.Session.Project.RunOptions);

    Engine.Session.Run.Clear;

    if rnaCompile in Info.Actions then
    begin
      aRunItem := Engine.Session.Run.Add;

      aRunItem.Info.Run.Command := Info.Command;
      aRunItem.Info.Run.Silent := True;
      aRunItem.Info.Run.CatchOutput := True;
      aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
      aRunItem.Info.CurrentDirectory := Info.Root;

      aRunItem.Info.Run.AddParam(Info.MainFile);
      if Info.OutputFile <> '' then
        aRunItem.Info.Run.AddParam('-of' + Info.OutputFile);

      aRunItem.Info.StatusMessage := 'Compiling ' + Info.OutputFile;
      //aRunItem.Info.AddParam('-color=on'); //not work :(

      for i := 0 to AOptions.Paths.Count - 1 do
      begin
        aPath := Trim(AOptions.Paths[i]);
        if aPath <>'' then
        begin
          if AOptions.ExpandPaths then
            aPath := Engine.ExpandFile(aPath);
          aRunItem.Info.Run.AddParam('-I' + aPath);
        end;
      end;

      //aRunItem.Info.AddParam('-v');

      if AOptions.ConfigFile <> '' then
        aRunItem.Info.Run.AddParam('@' + ReplaceVariables(AOptions.ConfigFile, []));
    end;

    if rnaExecute in Info.Actions then
    begin
      aRunItem := Engine.Session.Run.Add;
      aRunItem.Info.StatusMessage := 'Running ' + Info.OutputFile;
      aRunItem.Info.CurrentDirectory := Info.Root;
      aRunItem.Info.Run.Pause := Info.Pause;
      aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.OutputFile);;
      aRunItem.Info.Run.Command := ChangeFileExt(Info.OutputFile, '.exe');
      aRunItem.Info.Run.AddParam(AOptions.Params);
      aRunItem.Info.Run.AddParam(Engine.Session.Project.RunOptions.Params);
    end;
  finally
    FreeAndNil(AOptions)
  end
end;

procedure TCustomTendency.CreateOptionsFrame(AOwner: TComponent; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := RunOptions;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

function TCustomTendency.CreateDebugger: TEditorDebugger;
begin
  Result := nil;
end;

function TCustomTendency.CreateProjectOptions: TEditorProjectOptions;
begin
  Result := TCustomProjectOptions.Create;
end;

procedure TCustomTendency.Created;
begin
  FCapabilities := [capExecute, capCompile, capLink];
  FHaveOptions := True;
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

function TCustomFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynDSyn.Create(nil);
end;

procedure TCustomFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  FCompletion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
end;

procedure TCustomFileCategory.DoAddKeywords;
begin
  EnumerateKeywords(Ord(attKeyword), sDKeywords, Highlighter.IdentChars, @AddKeyword);
  EnumerateKeywords(Ord(attCommon), sDFunctions, Highlighter.IdentChars, @AddKeyword);
end;

procedure TCustomFileCategory.InitMappers;
begin
  with Highlighter as TSynDSyn do
  begin
    Mapper.Add(WhitespaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment, ord(tkComment));
    Mapper.Add(KeywordAttri, attKeyword, ord(tkKeyword));
    Mapper.Add(DocumentAttri, attDocument, ord(tkDocument));
    Mapper.Add(FunctionAttri, attCommon, ord(tkFunction));
    Mapper.Add(IdentifierAttri, attIdentifier, ord(tkIdentifier));
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
    Groups.Add(TCustomFile, 'Custom', 'Custom Files', 'Custom', ['.d', '.inc'], [fgkAssociated, fgkExecutable, fgkMember, fgkBrowsable, fgkMain]);
    Tendencies.Add(TCustomTendency);}
  end;
end.
