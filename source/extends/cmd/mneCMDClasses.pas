unit mneCMDClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, Contnrs, Dialogs,
  LCLintf, LCLType, LazFileUtils,
  EditorOptions, EditorRun, EditorClasses, mneRunFrames,
  SynEditHighlighter, SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  SynHighlighterBat;

type

  { TmneSynCMDSyn }

  TmneSynCMDSyn = class(TSynBATSyn)
  public
  end;

  TmneSynSHSyn = class(TSynBATSyn) //todo TmneSynSHSyn
  public
  end;

  { TCMDFile }

  TCMDFile = class(TSourceEditorFile)
  protected
    procedure NewContent; override;
  public
  end;

  { TCMDFileCategory }

  TCMDFileCategory = class(TTextFileCategory)
  private
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  { TCMDFile }

  { TSHFile }

  TSHFile = class(TSourceEditorFile)
  protected
    procedure NewContent; override;
  public
  end;

  { TSHFileCategory }

  TSHFileCategory = class(TTextFileCategory)
  private
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  { TConsoleTendency }

  TConsoleTendency = class(TEditorTendency)
  protected
    procedure Created; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils;

{ TSHFile }

procedure TSHFile.NewContent;
begin
  inherited NewContent;
end;

{ TSHFileCategory }

function TSHFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
    Result := TmneSynSHSyn.Create(nil);
end;

procedure TSHFileCategory.InitMappers;
begin
  with Highlighter as TmneSynSHSyn do
  begin
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(VariableAttri, attVariable);
    Mapper.Add(NumberAttri, attNumber);
//    Mapper.Add(StringAttribute, attQuotedString);
//    Mapper.Add(SymbolAttribute, attSymbol);
  end
end;

{ TConsoleTendency }

procedure TConsoleTendency.Created;
begin
  FCapabilities := [capExecute, capOptions];
  FName := 'CMD';
  FTitle := 'CMD project';
  FDescription := 'CMD Files, *.CMD';
  FImageIndex := -1;
end;

procedure TConsoleTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := ATendency.RunOptions;
  EnumRunCommands(aFrame.CommandEdit.Items);
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

procedure TConsoleTendency.DoRun(Info: TmneRunInfo);
var
  aRunItem: TmneRunItem;
begin
  Engine.Session.Run.Clear;

  if rnaExecute in Info.Actions then
  begin
    Engine.SendAction(eaClearOutput);

    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.Run.Pause := Info.Pause;
    aRunItem.Info.Run.Console := Info.Console;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    //aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.CurrentDirectory := ExtractFileDir(Info.MainFile);//command need to run in same dir
    aRunItem.Info.StatusMessage := 'Runing ' + Info.MainFile;
    //aRunItem.Info.Run.Command := Info.Command;//not acceptable

    if SameText(ExtractFileExt(Info.MainFile), '.sh') then
    begin
      //aRunItem.Info.Run.AddParam('--login');
      //aRunItem.Info.Run.AddParam('-i');
      {$ifdef windows}
      aRunItem.Info.Run.Command := 'sh';
      {$else}
      aRunItem.Info.Run.Command := 'sh';
      {$endif}
    end
    else //cmd or bat
    begin
      aRunItem.Info.Run.AddParam('/c');
      aRunItem.Info.Run.Command := 'CMD.exe';
    end;

    aRunItem.Info.Run.AddParam(' "' + Info.MainFile + '"');
  end;
  if Engine.Session.Run.Active then //if there is items ready to run
    Engine.Session.Run.Start(Debugger);
end;

{ TCMDFileCategory }

function TCMDFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TmneSynCMDSyn.Create(nil);
end;

procedure TCMDFileCategory.InitMappers;
begin
  with Highlighter as TmneSynCMDSyn do
  begin
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(VariableAttri, attVariable);
    Mapper.Add(NumberAttri, attNumber);
//    Mapper.Add(StringAttribute, attQuotedString);
//    Mapper.Add(SymbolAttribute, attSymbol);
  end
end;

{ TCMDFile }

procedure TCMDFile.NewContent;
begin
  inherited NewContent;
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TConsoleTendency);
    Categories.Add(TCMDFileCategory.Create(TConsoleTendency, 'CMD', 'Command prompt cmd, bat'));
    Categories.Add(TSHFileCategory.Create(TConsoleTendency, 'SH', 'Shell script SH'));
    Groups.Add(TCMDFile, 'CMD', 'CMD', TCMDFileCategory, ['cmd', 'bat'], [fgkExecutable, fgkBrowsable], []);
    Groups.Add(TCMDFile, 'sh', 'sh', TSHFileCategory, ['sh'], [fgkExecutable, fgkBrowsable], []);
  end;
end.
