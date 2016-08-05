unit mneCppClasses;
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
  LazFileUtils, SynHighlighterD, mnSynHighlighterCpp, EditorDebugger, EditorClasses, mneClasses, MsgBox,
  mneCompileProjectOptions, EditorRun, mneConsoleClasses,
  mneConsoleForms;

type

  { TCppFile }

  TCppFile = class(TSourceEditorFile)
  private
  protected
  public
    procedure NewContent; override;
    function CanOpenInclude: Boolean; override;
    procedure OpenInclude; override;
  end;


  { TCppFileCategory }

  TCppFileCategory = class(TCodeFileCategory)
  private
  protected
    procedure DoFormatClick(Sender: TObject);
    procedure InitMappers; override;
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddKeywords; override;
  public
    procedure EnumMenuItems(AddItems: TAddClickCallBack); override;
  end;

  { TDProjectOptions }

  TCppProjectOptions = class(TCompilerProjectOptions)
  private
  public
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
  end;

  { TDTendency }

  TCppTendency = class(TEditorTendency)
  private
  protected
    function CreateDebugger: TEditorDebugger; override;
    procedure Init; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    constructor Create; override;
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
    function CreateOptions: TEditorProjectOptions; override;
  published
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, SynHighlighterMultiProc, SynEditStrConst, mneDTendencyFrames, mneDProjectFrames, LCLProc;

{ TCppFile }

procedure TCppFile.NewContent;
begin
  SynEdit.Text := cDSample;
end;

{ TCppFileCategory }

procedure TCppFileCategory.DoFormatClick(Sender: TObject);
begin

end;

procedure TCppFileCategory.InitMappers;
begin
  with Highlighter as TmnSynCppSyn do
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

procedure TCppFileCategory.EnumMenuItems(AddItems: TAddClickCallBack);
begin
  inherited EnumMenuItems(AddItems);
  AddItems('Format', 'Format', @DoFormatClick, TextToShortCut('Ctrl+Shift+F'));
end;

function TCppFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TmnSynCppSyn.Create(nil);
end;

procedure TCppFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  FCompletion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
  IdentifierID := ord(SynHighlighterMultiProc.tkIdentifier);
end;

procedure TCppFileCategory.DoAddKeywords;
begin
  inherited DoAddKeywords;
end;

{ TCppProject }

procedure TCppProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TCompilerProjectOptionsForm.Create(AOwner);
  (aFrame as TCompilerProjectOptionsForm).FProject := AProject;
  aFrame.Caption := 'Compiler';
  AddFrame(aFrame);
  aFrame := TDProjectFrame.Create(AOwner);
  (aFrame as TDProjectFrame).Project := AProject;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

{ TCppFile }

procedure TCppFile.OpenInclude;
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
    if Engine.Files.Current.Group.Category is TCppFileCategory then
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

function TCppFile.CanOpenInclude: Boolean;
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
    if Group.Category is TCppFileCategory then
    begin
      P := SynEdit.CaretXY;
      aToken := '';
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      //Result := (aToken <> '') and (TtkTokenKind(aTokenType) = tkString);
    end;
  end;
end;

{ TCppTendency }

procedure TCppTendency.DoRun(Info: TmneRunInfo);
var
  aParams: string;
  i: Integer;
  aPath: string;
  Options: TCppProjectOptions;
  aRunItem: TmneRunItem;
begin
  if (Engine.Session.IsOpened) then
    Options := (Engine.Session.Project.Options as TCppProjectOptions)
  else
    Options := TCppProjectOptions.Create;//Default options

  Engine.Session.Run.Clear;

  if rnaCompile in Info.Actions then
  begin
    aParams := '';
    aRunItem := Engine.Session.Run.Add;

    aRunItem.Info.Command := Info.Command;
    if aRunItem.Info.Command = '' then
      aRunItem.Info.Command := 'dmd.exe';

    aRunItem.Info.Mode := runOutput;
    aRunItem.Info.Title := ExtractFileNameOnly(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;

    aPath := Info.MainFile;
    if Options.ExpandPaths then
      aPath := Engine.ExpandFile(aPath);
    if not FileExists(aPath) then
      raise EEditorException.Create('File not exists: ' + aParams);

    aRunItem.Info.Params := aPath + #13;
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
        if not DirectoryExists(aPath) then
          raise EEditorException.Create('Path not exists: ' + aParams);

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
    aRunItem.Info.Mode := Info.Mode;
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Pause := Info.Pause;
    aRunItem.Info.Title := ExtractFileName(Info.OutputFile);;
    aRunItem.Info.Command := Info.RunFile;
    if Options.RunParams <> '' then
      aRunItem.Info.Params := aRunItem.Info.Params + Options.RunParams + #13;
  end;

  Engine.Session.Run.Start;
end;

constructor TCppTendency.Create;
begin
  inherited Create;
end;

procedure TCppTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TDTendencyFrame;
begin
  aFrame := TDTendencyFrame.Create(AOwner);
  aFrame.FTendency := ATendency;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

function TCppTendency.CreateDebugger: TEditorDebugger;
begin
  Result := nil;
end;

function TCppTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TCppProjectOptions.Create;
end;

procedure TCppTendency.Init;
begin
  FCapabilities := [capRun, capCompile, capLink, capOptions];
  FTitle := 'Cpp Lang';
  FDescription := 'Cpp Files, *.cpp, *.c, *.h, *.ino';
  FName := 'Cpp';
  FImageIndex := -1;
  AddGroup('Cpp', 'cpp');
  //AddGroup('json', 'json');
end;

initialization
  with Engine do
  begin
    Categories.Add(TCppFileCategory.Create('Cpp', [fckPublish]));
    Groups.Add(TCppFile, 'Cpp', 'C++ Files', 'Cpp', ['cpp', 'c', 'ino', 'h'], [fgkAssociated, fgkExecutable, fgkMember, fgkBrowsable, fgkMain]);
    Tendencies.Add(TCppTendency);
  end;
end.
