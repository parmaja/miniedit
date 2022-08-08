unit mneGccClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *
 *}

 {
    gcc zkfp.c -o zkfp.dll -shared -static-libgcc -m32 -DBUILD_DLL -I.\libs\include -L D:\programs\win-builds\lib -L .\libs\x86lib -llibzkfp -Wl,-export-all-symbols -Wl,-out-implib,libzkfp.dll.a
 }

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics,
  Contnrs, LazFileUtils, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  mnSynHighlighterCpp, EditorClasses, mneClasses, mnMsgBox,
  mneCompilerProjectFrames, EditorRun, SynHighlighterCpp,
  mneRunFrames;

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
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddKeywords; override;
  public
    procedure EnumMenuItems(AddItems: TAddClickCallBack); override;
    function CreateHighlighter: TSynCustomHighlighter; override;
  end;

  { TDProjectOptions }

  TGccProjectOptions = class(TEditorProjectOptions)
  private
  public
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
  end;

  { TDTendency }

  TGccTendency = class(TEditorTendency)
  private
  protected
    function CreateDebugger: TEditorDebugger; override;
    procedure Created; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    constructor Create; override;
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
    function CreateOptions: TEditorProjectOptions; override;
  published
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, mnSynHighlighterMultiProc, SynEditStrConst, mneGccProjectFrames, LCLProc;

{ TCppFile }

procedure TCppFile.NewContent;
begin
  SynEdit.Text := cCppSample;
end;

{ TCppFileCategory }

procedure TCppFileCategory.DoFormatClick(Sender: TObject);
begin

end;

procedure TCppFileCategory.InitMappers;
begin
  with Highlighter as TSynCppSyn do
  begin
    Mapper.Add(WhitespaceAttribute, attDefault);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(DirecAttri, attDirective);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(AsmAttri, attEmbed);
  end;
  {with Highlighter as TmnSynCppSyn do
  begin
    Mapper.Add(WhitespaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(KeywordAttri, attKeyword);
    Mapper.Add(DocumentAttri, attDocument);
    Mapper.Add(FunctionAttri, attStandard);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(VariableAttri, attVariable);
    Mapper.Add(ProcessorAttri, attDirective);
  end;}
end;

procedure TCppFileCategory.EnumMenuItems(AddItems: TAddClickCallBack);
begin
  inherited EnumMenuItems(AddItems);
  AddItems('Format', 'Format', 'Edit', @DoFormatClick, scCtrl+scShift+VK_F);
end;

function TCppFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynCppSyn.Create(nil);
end;

procedure TCppFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  FCompletion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
  IdentifierID := ord(mnSynHighlighterMultiProc.tkIdentifier);
end;

procedure TCppFileCategory.DoAddKeywords;
begin
  inherited DoAddKeywords;
end;

{ TCppProject }

procedure TGccProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TCompilerProjectFrame.Create(AOwner);
  (aFrame as TCompilerProjectFrame).Project := AProject;
  aFrame.Caption := 'Compiler';
  AddFrame(aFrame);
  aFrame := TGccProjectFrame.Create(AOwner);
  (aFrame as TGccProjectFrame).Project := AProject;
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

{ TGccTendency }

procedure TGccTendency.DoRun(Info: TmneRunInfo);
var
  i: Integer;
  aPath: string;
  aRunItem: TmneRunItem;
begin
  Engine.Session.Run.Clear;

  if rnaCompile in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;

    aRunItem.Info.Run.Command := Info.Command;
    if aRunItem.Info.Run.Command = '' then
      aRunItem.Info.Run.Command := 'cpp.exe';

    aRunItem.Info.Run.Silent := True;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;

    aPath := Info.MainFile;
    if RunOptions.ExpandPaths then
      aPath := Engine.ExpandFile(aPath);
    if not FileExists(aPath) then
      raise EEditorException.Create('File not exists: ' + aPath);

    aRunItem.Info.Run.AddParam(aPath);
    if Info.OutputFile <> '' then
      aRunItem.Info.Run.AddParam('-o ' + Info.OutputFile);

    aRunItem.Info.StatusMessage := 'Compiling ' + Info.OutputFile;
    //aRunItem.Info.AddParam('-color=on'); //not work :(

    for i := 0 to RunOptions.Paths.Count - 1 do
    begin
      aPath := Trim(RunOptions.Paths[i]);
      if aPath <>'' then
      begin
        if RunOptions.ExpandPaths then
          aPath := Engine.ExpandFile(aPath);
        if not DirectoryExists(aPath) then
          raise EEditorException.Create('Path not exists: ' + aPath);

        aRunItem.Info.Run.AddParam('-I ' +aPath);
      end;
    end;

    //aRunItem.Info.AddParam('-v');

    if RunOptions.ConfigFile <> '' then
      aRunItem.Info.Run.AddParam('@' + ReplaceVariables(RunOptions.ConfigFile, []));
  end;

  if rnaExecute in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.StatusMessage := 'Running ' + Info.OutputFile;
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Run.Pause := Info.Pause;
    aRunItem.Info.Title := ExtractFileName(Info.OutputFile);;
    aRunItem.Info.Run.Command := Info.RunFile;
    aRunItem.Info.Run.AddParam(RunOptions.Params);
    aRunItem.Info.Run.AddParam(Engine.Session.Project.RunOptions.Params);
  end;

  Engine.Session.Run.Start(Debugger);
end;

constructor TGccTendency.Create;
begin
  inherited Create;
end;

procedure TGccTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := ATendency.RunOptions;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

function TGccTendency.CreateDebugger: TEditorDebugger;
begin
  Result := nil;
end;

function TGccTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TGccProjectOptions.Create;
end;

procedure TGccTendency.Created;
begin
  FCapabilities := [capExecute, capCompile, capLink];
  FHaveOptions := True;
  FTitle := 'GCC Gcc C/C++';
  FDescription := 'Gcc C/C++ Files, *.cpp, *.c, *.h, *.ino';
  FName := 'Gcc';
  FImageIndex := -1;
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TGccTendency);
    Categories.Add(TCppFileCategory.Create(TGccTendency, 'Gcc', 'GCC C/C++', [fckPublish]));
    Groups.Add(TCppFile, 'cpp', 'C/C++', TCppFileCategory, ['.cpp', '.c', '.ino', '.h', '.def'], [fgkAssociated, fgkBrowsable, fgkMain], [capExecute, capDebug]);
  end;
end.
