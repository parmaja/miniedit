unit EditorEngine;
{$mode delphi}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Messages, SysUtils, Forms, StrUtils, Dialogs,Variants, Classes, Controls, Graphics, Contnrs,
  IniFiles, EditorOptions, EditorProfiles, SynEditMarks, SynCompletion, SynEditTypes,
  SynEditMiscClasses, SynEditHighlighter, SynEditKeyCmds, SynEditMarkupBracket, SynEditSearch, SynEdit,
  SynEditTextTrimmer, SynTextDrawer, EditorDebugger, EditorSCM, IAddons,
  {$ifdef Windows}
  dbgpServers,
  PHP_xDebug,
  {$endif}
  mnXMLRttiProfile, mnXMLUtils, mnUtils, LCLType;

type
  TEditorChangeState = set of (ecsChanged, ecsState, ecsRefresh, ecsDebug, ecsEdit, ecsFolder, ecsProjectLoaded);
  TSynCompletionType = (ctCode, ctHint, ctParams);

  TEditorEngine = class;
  TFileCategory = class;
  TFileGroup = class;
  TEditorFile = class;

  EEditorException = class(Exception)
  private
    FErrorLine: Integer;
  public
    property ErrorLine: Integer read FErrorLine write FErrorLine;
  end;

  TProjectMember = class(TCollectionItem)
  private
    FName: string;
    FDescription: string;
    FRunUrl: string;
  public
  published
    property Name: string read FName write FName;
    property RunUrl: string read FRunUrl write FRunUrl;
    property Description: string read FDescription write FDescription;
  end;

  TProjectMembers = class(TCollection)
  private
    FEngine: TEditorEngine;
    function GetItems(Index: Integer): TProjectMember;
    function GetMember(Index: string): TProjectMember;
  protected
  public
    function Find(vName: string): TProjectMember;
    function IsExist(vName: string): Boolean;
    property Engine: TEditorEngine read FEngine;
    property Items[Index: Integer]: TProjectMember read GetItems;
    property Members[Index: string]: TProjectMember read GetMember; default;
  published
  end;

  TEditorDesktopFile = class(TCollectionItem)
  private
    FFileName: string;
    FCaretY: Integer;
    FCaretX: Integer;
    FTopLine: Integer;
  public
  published
    property FileName: string read FFileName write FFileName;
    property CaretX: Integer read FCaretX write FCaretX default 1;
    property CaretY: Integer read FCaretY write FCaretY default 1;
    property TopLine: Integer read FTopLine write FTopLine default 1;
  end;

  TEditorDesktopFiles = class(TCollection)
  private
    FCurrentFile: string;
    function GetItems(Index: Integer): TEditorDesktopFile;
  protected
  public
    function Add(FileName: string): TEditorDesktopFile;
    function Find(vName: string): TEditorDesktopFile;
    function IsExist(vName: string): Boolean;
    property Items[Index: Integer]: TEditorDesktopFile read GetItems; default;
  published
    property CurrentFile: string read FCurrentFile write FCurrentFile;
  end;

  TEditorDesktop = class(TPersistent)
  private
    FEngine: TEditorEngine;
    FFiles: TEditorDesktopFiles;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    property Engine: TEditorEngine read FEngine;
  published
    property Files: TEditorDesktopFiles read FFiles;
  end;

  TRunMode = (prunNone, prunConsole, prunUrl);

  TEditorProject = class(TmnXMLProfile)
  private
    FRunMode: TRunMode;
    FDescription: string;
    FRootUrl: string;
    FRootDir: string;
    FFileName: string;
    FEngine: TEditorEngine;
    FMembers: TProjectMembers;
    FName: string;
    FSaveDesktop: Boolean;
    FDesktop: TEditorDesktop;
    FCachedIdentifiers: THashedStringList;
    FCachedVariables: THashedStringList;
    FCachedAge: DWORD;
  protected
    procedure Loaded(Failed:Boolean); override;
    procedure Saving; override;
  public
    constructor Create(AEngine: TEditorEngine);
    destructor Destroy; override;
    property Engine: TEditorEngine read FEngine;
    property FileName: string read FFileName write FFileName;
    function Save: Boolean;
    function SaveAs: Boolean;
    property CachedVariables: THashedStringList read FCachedVariables;
    property CachedIdentifiers: THashedStringList read FCachedIdentifiers;
    property CachedAge: cardinal read FCachedAge write FCachedAge;
  published
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property RootDir: string read FRootDir write FRootDir;
    property RootUrl: string read FRootUrl write FRootUrl;
    property Members: TProjectMembers read FMembers;
    property RunMode: TRunMode read FRunMode write FRunMode default prunUrl;
    property Desktop: TEditorDesktop read FDesktop;
    property SaveDesktop: Boolean read FSaveDesktop write FSaveDesktop default True;
  end;

  TDebugSupportPlugin = class(TObject)
  private
    FEditorFile: TEditorFile;
  protected
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect; FirstLine, LastLine: integer);
  public
    constructor Create(AOwner: TEditorFile);
  end;

  TEditorFileMode = (efmUnix, efmWindows, efmMac);

  TEditorFile = class(TCollectionItem)
  private
    FName: string;
    FSynEdit: TSynEdit;
    FEdited: Boolean;
    FFileAge: Integer;
    FGroup: TFileGroup;
    FRelated: string;
    FMode: TEditorFileMode;
    function GetEngine: TEditorEngine;
    procedure SetEdited(const Value: Boolean);
    procedure SetGroup(const Value: TFileGroup);
    function GetReadonly: Boolean;
    procedure SetReadonly(const Value: Boolean);
    function GetModeAsText: string;
    procedure SetMode(const Value: TEditorFileMode);
  protected
    procedure Edit;
    procedure DoEdit(Sender: TObject);
    procedure DoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure DoGutterClickEvent(Sender: TObject; X, Y, Line: integer; Mark: TSynEditMark);
    procedure DoSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure UpdateAge;
    function GetHighlighter: TSynCustomHighlighter; virtual;
    procedure NewSource; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Load(FileName: string); virtual;
    procedure Save(FileName: string); virtual;
    procedure SaveFile(AsNewFile: Boolean = False); virtual;
    procedure Reload;
    procedure Show;
    procedure Close;
    function CheckChanged: Boolean;
    property Mode: TEditorFileMode read FMode write SetMode default efmUnix;
    property ModeAsText: string read GetModeAsText;
    property Name: string read FName write FName;
    property Related: string read FRelated write FRelated;
    property SynEdit: TSynEdit read FSynEdit;
    property Engine: TEditorEngine read GetEngine;
    property Edited: Boolean read FEdited write SetEdited;
    property Readonly: Boolean read GetReadonly write SetReadonly;
    property Group: TFileGroup read FGroup write SetGroup;
  published
  end;

  { TEditorFiles }

  TEditorFiles = class(TCollection)
  private
    FCheckChanged: Boolean;
    FCurrent: TEditorFile;
    FEngine: TEditorEngine;
    function GetItems(Index: Integer): TEditorFile;
    function GetCurrent: TEditorFile;
    procedure SetCurrent(const Value: TEditorFile);
    function InternalOpenFile(FileName: string; AppendToRecent: Boolean): TEditorFile;
  protected
    function SetActiveFile(FileName: string): TEditorFile;
  public
    destructor Destroy; override;
    function FindFile(const vFileName: string): TEditorFile;
    function IsExist(vName: string): Boolean;
    function LoadFile(vFileName: string; AppendToRecent: Boolean = True): TEditorFile;
    function ShowFile(vFileName: string): TEditorFile; overload; //open it without add to recent, for debuging
    function ShowFile(const FileName: string; Line: Integer): TEditorFile; overload;
    function OpenFile(vFileName: string): TEditorFile;
    procedure SetCurrentIndex(Index: Integer; vRefresh: Boolean);
    function New: TEditorFile; overload;
    function New(Category, Name, Related: string; Readonly, Executable: Boolean): TEditorFile; overload;
    procedure Open;
    procedure Save;
    procedure SaveAll;
    procedure SaveAs;
    procedure Revert;
    procedure Refresh;
    procedure Next;
    procedure Prior;
    procedure Edited;
    procedure Replace;
    procedure Find;
    procedure FindNext;
    procedure CheckChanged;
    procedure CloseAll;
    function GetEditedCount: Integer;
    property Engine: TEditorEngine read FEngine;
    property Current: TEditorFile read GetCurrent write SetCurrent;
    property Items[Index: Integer]: TEditorFile read GetItems; default;
  published
  end;

  TEditorFileClass = class of TEditorFile;

  TOnEngineChanged = procedure of object;

  TEditorOptions = class(TmnXMLProfile)
  private
    FFileName: string;
    FEngine: TEditorEngine;
    FShowFolder: Boolean;
    FWindowMaxmized: Boolean;
    FBoundRect: TRect;
    FSearchHistory: TStringList;
    FProfile: TEditorProfile;
    FCompilerFolder: string;
    FRecentFiles: TStringList;
    FRecentProjects: TStringList;
    FHelpFiles: TStringList;
    FProjects: TStringList;
    FShowMessages: Boolean;
    FCollectAutoComplete: Boolean;
    FCollectTimeout: DWORD;
    FReplaceHistory: TStringList;
    FSendOutputToNewFile: Boolean;
    FShowOutput: Boolean;
    FAutoStartDebugServer: Boolean;
    FOutputHeight: Integer;
    FMessagesHeight: Integer;
    FFoldersWidth: Integer;
    FSearchFolderHistory: TStringList;
    FExtraExtensions: TStringList;
    procedure SetRecentFiles(const Value: TStringList);
    procedure SetRecentProjects(const Value: TStringList);
    procedure SetProjects(const Value: TStringList);
  protected
  public
    constructor Create(AEngine: TEditorEngine);
    destructor Destroy; override;
    procedure Apply;
    procedure Load(vFileName: string);
    procedure Save;
    procedure Show;
    property Engine: TEditorEngine read FEngine;
    property FileName: string read FFileName write FFileName;
    property BoundRect: TRect read FBoundRect write FBoundRect; //not saved yet
  published
    property CompilerFolder: string read FCompilerFolder write FCompilerFolder;
    property HelpFiles: TStringList read FHelpFiles write FHelpFiles;
    property ExtraExtensions: TStringList read FExtraExtensions write FExtraExtensions;
    property CollectAutoComplete: Boolean read FCollectAutoComplete write FCollectAutoComplete default False;
    property CollectTimeout: DWORD read FCollectTimeout write FCollectTimeout default 60;
    property ShowFolder: Boolean read FShowFolder write FShowFolder default True;
    property ShowMessages: Boolean read FShowMessages write FShowMessages default False;
    property ShowOutput: Boolean read FShowOutput write FShowOutput default False;
    property OutputHeight: Integer read FOutputHeight write FOutputHeight default 100;
    property MessagesHeight: Integer read FMessagesHeight write FMessagesHeight default 100;
    property FoldersWidth: Integer read FFoldersWidth write FFoldersWidth default 180;
    property SendOutputToNewFile: Boolean read FSendOutputToNewFile write FSendOutputToNewFile default False;
    property AutoStartDebugServer: Boolean read FAutoStartDebugServer write FAutoStartDebugServer default False;
    property WindowMaxmized: Boolean read FWindowMaxmized write FWindowMaxmized default False;
    property SearchHistory: TStringList read FSearchHistory;
    property ReplaceHistory: TStringList read FReplaceHistory;
    property SearchFolderHistory: TStringList read FSearchFolderHistory;
    property Profile: TEditorProfile read FProfile;
    property RecentFiles: TStringList read FRecentFiles write SetRecentFiles;
    property RecentProjects: TStringList read FRecentProjects write SetRecentProjects;
    property Projects: TStringList read FProjects write SetProjects;
  end;

  TFileCategoryKind = (fckPublish);
  TFileCategoryKinds = set of TFileCategoryKind;

  TFileCategory = class(TObject)
  private
    FName: string;
    FEditorFileClass: TEditorFileClass;
    FHighlighter: TSynCustomHighlighter;
    FCompletion: TSynCompletion;
    FKind: TFileCategoryKinds;
  protected
    procedure OnExecuteCompletion(Kind: TSynCompletionType; Sender: TObject; var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean); virtual;
    function CreateHighlighter: TSynCustomHighlighter; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function CreateEditorFile(Files: TEditorFiles): TEditorFile; virtual;
    property Name: string read FName write FName;
    property EditorFileClass: TEditorFileClass read FEditorFileClass;
    property Highlighter: TSynCustomHighlighter read FHighlighter;
    property Completion: TSynCompletion read FCompletion;
    property Kind: TFileCategoryKinds read FKind;
  end;

  TFileCategoryClass = class of TFileCategory;

  TFileCategories = class(TObjectList)
  private
    FEngine: TEditorEngine;
    function GetItem(Index: Integer): TFileCategory;
    procedure SetItem(Index: Integer; AObject: TFileCategory);
  public
    constructor Create(AEngine: TEditorEngine);
    function Find(vName: string): TFileCategory;
    procedure Add(const Name: string; EditorFileClass: TEditorFileClass; CategoryClass: TFileCategoryClass; Kind: TFileCategoryKinds = []);
    property Items[Index: Integer]: TFileCategory read GetItem write SetItem; default;
    property Engine: TEditorEngine read FEngine;
  end;

  TFileGroupKind = (fgkExecutable, fgkMainIcon, fgkBrowsable, fgkPublish);
  TFileGroupKinds = set of TFileGroupKind;

  TFileGroup = class(TObject)
  private
    FName: string;
    FDisplayName: string;
    FExtensions: TStringList;
    FKind: TFileGroupKinds;
    FCategory: TFileCategory;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure EnumExtensions(vExtensions:TStringList);
    property Category: TFileCategory read FCategory;
    property Extensions: TStringList read FExtensions;
    property Name: string read FName write FName;
    property Kind: TFileGroupKinds read FKind write FKind;
    property DisplayName: string read FDisplayName write FDisplayName;
  end;

  TFileGroups = class(TObjectList)
  private
    FEngine: TEditorEngine;
    function GetItem(Index: Integer): TFileGroup;
    procedure SetItem(Index: Integer; AObject: TFileGroup);
  public
    constructor Create(AEngine: TEditorEngine);
    function Find(vName: string): TFileGroup;
    procedure EnumExtensions(vExtensions:TStringList);
    function FindExtension(vExtension: string): TFileGroup;
    function CreateFilter(vGroup: TFileGroup = nil): string;
    procedure Add(const DisplayName, Name, Category: string; Extensions: array of string; Kind: TFileGroupKinds = []);
    property Items[Index: Integer]: TFileGroup read GetItem write SetItem; default;
    property Engine: TEditorEngine read FEngine;
  end;

  TEditorProjects = class(TObject)
  private
    FEngine: TEditorEngine;
    FCurrent: TEditorProject;
    procedure SetCurrent(const Value: TEditorProject);
    function GetIsOpened: Boolean;
  public
    destructor Destroy; override;
    procedure Load(FileName: string);
    function New: TEditorProject;
    procedure Close;
    procedure Open;
    property IsOpened: Boolean read GetIsOpened;
    property Current: TEditorProject read FCurrent write SetCurrent;
    property Engine: TEditorEngine read FEngine;
  end;

  TEditorMessagesList = class;

  TEditorMessage = class(TObject)
  private
    FText: string;
  public
    property Text: string read FText write FText;
  end;

  TEditorMessages = class(TObjectList)
  private
    FName: string;
    function GetItem(Index: Integer): TEditorMessage;
    procedure SetItem(Index: Integer; const Value: TEditorMessage);
  public
    function GetText(Index: Integer): string;
    property Name: string read FName write FName;
    property Items[Index: Integer]: TEditorMessage read GetItem write SetItem; default;
  end;

  TEditorMessagesList = class(TObjectList)
  private
    function GetItem(Index: Integer): TEditorMessages;
    procedure SetItem(Index: Integer; const Value: TEditorMessages);
  public
    function Find(Name: string): TEditorMessages;
    function GetMessages(Name: string): TEditorMessages;
    property Items[Index: Integer]: TEditorMessages read GetItem write SetItem; default;
  end;

  TOnFoundEvent = procedure(FileName: string; const Line: string; LineNo, Column, FoundLength: Integer) of object;
  TOnEditorChangeState = procedure(State: TEditorChangeState) of object;

  { TEditorEngine }

  TEditorEngine = class(TObject)
  private
    FSCM: TEditorSCM;
    FUpdateState: TEditorChangeState;
    FUpdateCount: Integer;
    FFiles: TEditorFiles;
    FWindow: TWinControl;
    FOptions: TEditorOptions;
    FSearchEngine: TSynEditSearch;
    FCategories: TFileCategories;
    FGroups: TFileGroups;
    FExtenstion: string;
    FOnChangedState: TOnEditorChangeState;
    FProjects: TEditorProjects;
    FDebug: TEditorDebugger;
    FMessagesList: TEditorMessagesList;
    FBrowseFolder: string;
    //FMacroRecorder: TSynMacroRecorder;
    FWorkSpace: string;
    function GetRoot: string;
    function GetUpdating: Boolean;
    procedure SetBrowseFolder(const Value: string);
    function GetWorkSpace: string;
  protected
    property SearchEngine: TSynEditSearch read FSearchEngine;
    procedure DoChangedState(State: TEditorChangeState); virtual;
    procedure DoMacroStateChange(Sender: TObject);
    function CreateEditorFile(Group: string): TEditorFile; virtual;
    function CreateEditorProject: TEditorProject;
    function FindExtensionCategoryName(Extension: string): string;
    function CreateDebugger: TEditorDebugger;
    function CreateSCM: TEditorSCM;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //
    function SearchReplace(const FileName: string; const ALines: TStringList; const ASearch, AReplace: string; OnFoundEvent: TOnFoundEvent; AOptions: TSynSearchOptions): integer;
    //Recent
    procedure ProcessRecentFile(const FileName: string);
    procedure RemoveRecentFile(const FileName: string);
    procedure ProcessRecentProject(const FileName: string);
    procedure RemoveRecentProject(const FileName: string);
    procedure ProcessProject(const FileName: string);
    procedure RemoveProject(const FileName: string);

    procedure BeginUpdate;
    procedure UpdateState(State: TEditorChangeState);
    property Updating: Boolean read GetUpdating;
    procedure EndUpdate;
    function ExpandFileName(FileName: string): string;
    property Extenstion: string read FExtenstion write FExtenstion;
    property Root: string read GetRoot;
    property WorkSpace: string read GetWorkSpace write FWorkSpace;

    property Categories: TFileCategories read FCategories;
    property Groups: TFileGroups read FGroups;
    property Files: TEditorFiles read FFiles;
    property Projects: TEditorProjects read FProjects;
    property Options: TEditorOptions read FOptions;
    property Debug: TEditorDebugger read FDebug;
    property SCM: TEditorSCM read FSCM;
    property MessagesList: TEditorMessagesList read FMessagesList;
    property Window: TWinControl read FWindow write FWindow;
    property BrowseFolder: string read FBrowseFolder write SetBrowseFolder;
    //property MacroRecorder: TSynMacroRecorder read FMacroRecorder;
    property OnChangedState: TOnEditorChangeState read FOnChangedState write FOnChangedState;
    //debugger
  published
  end;

function SelectFolder(const Caption: string; const Root: WideString; var Directory: string): Boolean;
procedure SpliteStr(S, Separator: string; var Name, Value: string);
procedure EnumFiles(Folder, Filter: string; FileList: TStringList);
procedure EnumFileList(const Root, Path, Files: string; Strings: TStringList; vMaxCount: Integer; Recursive: Boolean);
procedure SaveAsUnix(Strings: TStrings; Stream: TStream);
procedure SaveAsWindows(Strings: TStrings; Stream: TStream);
procedure SaveAsMAC(Strings: TStrings; Stream: TStream);
procedure SaveAsMode(const FileName: string; Mode: TEditorFileMode; Strings: TStrings);
function DetectFileMode(const Contents: string): TEditorFileMode;
function ChangeTabsToSpace(const Contents: string; TabWidth: Integer): string;

var
  Engine: TEditorEngine = nil;

implementation

uses
  SynHighlighterApache, SynHighlighterHTMLPHP, SynHighlighterHashEntries, Registry, SearchForms,
  mneResources;

function SelectFolder(const Caption: string; const Root: WideString; var Directory: string): Boolean;
begin
  Result := SelectDirectory(Caption, Root, Directory);
end;

procedure SpliteStr(S, Separator: string; var Name, Value: string);
var
  p: Integer;
begin
  p := AnsiPos(Separator, S);
  if P <> 0 then
  begin
    Name := Copy(s, 1, p - 1);
    Value := Copy(s, p + 1, MaxInt);
  end
  else
  begin
    Name := s;
    Value := '';
  end;
end;

procedure SaveAsUnix(Strings: TStrings; Stream: TStream);
var
  i, l: Integer;
  S: string;
begin
  l := Strings.Count - 1;
  for i := 0 to l do
  begin
    S := Strings[i];
    if i <> l then
      S := S + #$A;
    Stream.WriteBuffer(Pointer(S)^, Length(S));
  end;
end;

procedure SaveAsWindows(Strings: TStrings; Stream: TStream);
var
  i, l: Integer;
  S: string;
begin
  l := Strings.Count - 1;
  for i := 0 to l do
  begin
    S := Strings[i];
    if i <> l then
      S := S + #$D#$A;
    Stream.WriteBuffer(Pointer(S)^, Length(S));
  end;
end;

procedure SaveAsMAC(Strings: TStrings; Stream: TStream);
var
  i, l: Integer;
  S: string;
begin
  l := Strings.Count - 1;
  for i := 0 to l do
  begin
    S := Strings[i];
    if i <> l then
      S := S + #$D;
    Stream.WriteBuffer(Pointer(S)^, Length(S));
  end;
end;

function TEditorEngine.SearchReplace(const FileName: string; const ALines: TStringList; const ASearch, AReplace: string; OnFoundEvent: TOnFoundEvent; AOptions: TSynSearchOptions): integer;
var
  i: Integer;
  nSearchLen, nReplaceLen, n, nChar: integer;
  nInLine: integer;
  iResultOffset: integer;
  aLine, aReplaceText: string;
  Replaced: Boolean;
begin
  if not Assigned(SearchEngine) then
  begin
    raise ESynEditError.Create('No search engine has been assigned');
  end;

  Result := 0;
  // can't search for or replace an empty string
  if Length(ASearch) = 0 then
    exit;

  i := 0;
  // initialize the search engine
  //SearchEngine.Options := AOptions;
  SearchEngine.Pattern := ASearch;
  // search while the current search position is inside of the search range
  try
    while i < ALines.Count do
    begin
      aLine := ALines[i];
      nInLine := SearchEngine.FindAll(aLine);
      iResultOffset := 0;
      n := 0;
      // Operate on all results in this line.
      Replaced := False;
      while nInLine > 0 do
      begin
        // An occurrence may have been replaced with a text of different length
        nChar := SearchEngine.Results[n] + iResultOffset;
        nSearchLen := SearchEngine.ResultLengths[n];
        Inc(n);
        Dec(nInLine);

        Inc(Result);
        OnFoundEvent(FileName, aLine, i + 1, nChar, nSearchLen);

        if (ssoReplace in AOptions) then
        begin
          //aReplaceText := SearchEngine.Replace(ASearch, AReplace);
          nReplaceLen := Length(aReplaceText);
          aLine := Copy(aLine, 1, nChar - 1) + aReplaceText + Copy(aLine, nChar + nSearchLen, MaxInt);
          if (nSearchLen <> nReplaceLen) then
          begin
            Inc(iResultOffset, nReplaceLen - nSearchLen);
          end;
          Replaced := True;
        end;
      end;
      if Replaced then
        ALines[i] := aLine;
      // search next / previous line
      Inc(i);
    end;
  finally
  end;
end;

{ TEditorEngine }

procedure TEditorOptions.Apply;
var
  i: Integer;
begin
  for i := 0 to Engine.Categories.Count - 1 do
  begin
//    Engine.Categories[i].Completion.Font := Profile.Font;
    //Engine.Categories[i].Completion.Options := Engine.Categories[i].Completion.Options + [scoTitleIsCentered];
    if Engine.Categories[i].Highlighter <> nil then
    begin
      Profile.Highlighters.AssignTo(Engine.Categories[i].Highlighter);
    end;
  end;

  for i := 0 to Engine.Files.Count - 1 do
  begin
    Profile.AssignTo(Engine.Files[i].SynEdit);
  end;
end;

procedure TEditorEngine.BeginUpdate;
begin
  if FUpdateCount = 0 then
  begin
    FUpdateState := [];
  end;
  Inc(FUpdateCount);
end;

procedure TEditorFiles.CheckChanged;
var
  i: Integer;
  b: Boolean;
begin
  if not FCheckChanged then
  begin
    Engine.BeginUpdate;
    FCheckChanged := True;
    b := True;
    try
      for i := 0 to Count - 1 do
      begin
        if not b then
          Items[i].UpdateAge
        else
          b := Items[i].CheckChanged;
      end;
    finally
      FCheckChanged := False;
      Engine.EndUpdate;
    end;
  end;
end;

procedure TEditorFiles.CloseAll;
begin
  Engine.BeginUpdate;
  try
    while Engine.Files.Count > 0 do
      Engine.Files[0].Close;
  finally
    Engine.EndUpdate;
  end;
end;

procedure TEditorProjects.Close;
begin
  Current := nil;
end;

constructor TEditorEngine.Create;
begin
  inherited;
  FMessagesList := TEditorMessagesList.Create;
  //FMacroRecorder := TSynMacroRecorder.Create(nil);
  //FMacroRecorder.OnStateChange := DoMacroStateChange;
  FOptions := TEditorOptions.Create(Self);
  FCategories := TFileCategories.Create(Self);
  FGroups := TFileGroups.Create(Self);
  FSearchEngine := TSynEditSearch.Create;
  FFiles := TEditorFiles.Create(TEditorFile);
  FFiles.FEngine := Self;
  FProjects := TEditorProjects.Create;
  FProjects.FEngine := Self;
  FDebug := CreateDebugger;
  FSCM := CreateSCM;
end;

function TEditorEngine.CreateEditorFile(Group: string): TEditorFile;
var
  aGroup: TFileGroup;
begin
  aGroup := Groups.Find(Group);
  if aGroup <> nil then
    Result := aGroup.Category.CreateEditorFile(Files)
  else
    Result := TEditorFile.Create(Files);
  Result.Group := aGroup;
end;

function TEditorEngine.CreateEditorProject: TEditorProject;
begin
  Result := TEditorProject.Create(Self);
end;

destructor TEditorEngine.Destroy;
begin
  FDebug.Stop;
  FreeAndNil(FSCM);
  FreeAndNil(FFiles);
  FreeAndNil(FProjects);
  FreeAndNil(FCategories);
  FreeAndNil(FGroups);
  FreeAndNil(FSearchEngine);
  FreeAndNil(FOptions);
  //FreeAndNil(FMacroRecorder);
  FreeAndNil(FMessagesList);
  FreeAndNil(FDebug);
  Engine := nil;
  inherited;
end;

procedure EnumFiles(Folder, Filter: string; FileList: TStringList);
var
  Rslt: Integer;
  SearchRec: TSearchRec;
begin
  Folder := IncludeTrailingPathDelimiter(Folder);
  Rslt := FindFirst(Folder + Filter, faAnyFile, SearchRec);
  while Rslt = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      FileList.Add(SearchRec.Name);
    end;
    Rslt := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

procedure EnumFileList(const Root, Path, Files: string; Strings: TStringList; vMaxCount: Integer; Recursive: Boolean);
var
  sr: TSearchRec;
  function FullPath: string;
  begin
    if Root <> '' then
      Result := IncludeTrailingPathDelimiter(Root);
    if Path <> '' then
      Result := IncludeTrailingPathDelimiter(Result + Path);
  end;
begin
  if FindFirst(FullPath + Files, faAnyFile, sr) = 0 then
  begin
    repeat
      Strings.Add(IncludeTrailingPathDelimiter(Path) + sr.Name);
      if Strings.Count > vMaxCount then
        raise Exception.Create('Too many files');
    until (FindNext(sr) <> 0);
  end;

  if Recursive then
    if FindFirst(FullPath + '*.*', faDirectory, sr) = 0 then
    begin
      repeat
        if (sr.Name = '') or (sr.Name[1] = '.') or (sr.Name = '..') or (copy(sr.Name, 1, 5) = '_vti_') or SameText(sr.Name, '.svn') or SameText(sr.Name, '_svn') then
          continue;
        if (sr.Attr and faDirectory) <> 0 then
          EnumFileList(Root, IncludeTrailingPathDelimiter(Path) + sr.Name, Files, Strings, vMaxCount, Recursive)
      until (FindNext(sr) <> 0);
    end;
end;

procedure TEditorFiles.Edited;
begin
  Engine.UpdateState([ecsEdit]);
end;

procedure TEditorEngine.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    if Files.Current <> nil then
      Files.Current.Show;
    if FUpdateState <> [] then
      DoChangedState(FUpdateState);
    FUpdateState := [];
  end;
end;

procedure TEditorFiles.Find;
begin
  if Current <> nil then
    ShowSearchForm(Current.SynEdit, Engine.Options.SearchHistory, Engine.Options.ReplaceHistory, False);
end;

function TEditorEngine.FindExtensionCategoryName(Extension: string): string;
var
  aGroup: TFileGroup;
begin
  if LeftStr(Extension, 1) = '.' then
    Extension := Copy(Extension, 2, MaxInt);
  aGroup := Groups.FindExtension(Extension);
  if aGroup <> nil then
    Result := aGroup.Name
  else
    Result := '';
end;

function TEditorEngine.CreateDebugger: TEditorDebugger;
begin
  {$ifdef WINDOWS}
  Result := TPHP_xDebug.Create;
  Result.Start;
  {$else}
  Result := TEditorDebugger.Create;
  {$endif}
end;

function TEditorEngine.CreateSCM: TEditorSCM;
begin
  Result := TEditorSCM.Create;
end;

function TEditorFiles.FindFile(const vFileName: string): TEditorFile;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameFileName(vFileName, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

procedure TEditorFiles.FindNext;
begin
  if Current <> nil then
    NextSearchText(Current.SynEdit);
end;

function TEditorFiles.GetCurrent: TEditorFile;
begin
  Result := FCurrent;
end;

function TEditorFiles.GetEditedCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Edited then
      Result := Result + 1;
  end;
end;

function TEditorEngine.GetRoot: string;
var
  s: string;
begin
  s := ExtractFilePath(Application.ExeName);
  if Projects.Current.RootDir = '' then
    Result := s
  else
    Result := ExpandToPath(Projects.Current.RootDir, s);
end;

function TEditorFiles.InternalOpenFile(FileName: string; AppendToRecent: Boolean): TEditorFile;
begin
  FileName := ExpandFileName(FileName);
  Result := FindFile(FileName);
  if Result = nil then
  begin
    Result := Engine.CreateEditorFile(Engine.FindExtensionCategoryName(ExtractFileExt(FileName)));
    Result.Load(FileName);
  end;
  if AppendToRecent then
    Engine.ProcessRecentFile(FileName);
end;

procedure TEditorOptions.Load(vFileName: string);
begin
  SafeLoadFromFile(vFileName);
  FileName := vFileName;
  Apply;
end;

procedure TEditorProjects.Load(FileName: string);
var
  aProject: TEditorProject;
begin
  Engine.BeginUpdate;
  try
    Close; //must free before load project for save the desktop and sure to save its files
    aProject := New;
    try
      aProject.LoadFromFile(FileName);
      aProject.FileName := FileName;
    except
      aProject.Free;
      raise;
    end;
    Current := aProject;
    Engine.ProcessRecentProject(FileName);
    Engine.UpdateState([ecsChanged, ecsState, ecsRefresh, ecsProjectLoaded]);
  finally
    Engine.EndUpdate;
  end;
end;

function TEditorFiles.New: TEditorFile;
var
  aGroup: TFileGroup;
begin
  aGroup := Engine.Groups[0];
  Result := Engine.CreateEditorFile(aGroup.Name);
  Result.NewSource;
  Result.Edit;
  Current := Result;
  Engine.UpdateState([ecsChanged, ecsState, ecsRefresh]);
end;

function TEditorFiles.New(Category, Name, Related: string; Readonly, Executable: Boolean): TEditorFile;
begin
  Result := Engine.CreateEditorFile(Category);
  Result.ReadOnly := Readonly;
  Result.Name := Name;
  Result.Related := Related;
  Current := Result;
  Engine.UpdateState([ecsChanged, ecsState, ecsRefresh]);
end;

function TEditorProjects.New: TEditorProject;
begin
  Result := Engine.CreateEditorProject;
end;

procedure TEditorFiles.Next;
var
  i: Integer;
begin
  if Current <> nil then
  begin
    i := Current.Index + 1;
    if i >= Count then
      i := 0;
    SetCurrentIndex(i, True);
  end;
end;

procedure TEditorFiles.Open;
var
  i: Integer;
  aFile: TEditorFile;
  aDialog: TOpenDialog;
begin
  aDialog := TOpenDialog.Create(nil);
  try
    aDialog.Title := 'Open file';
    aDialog.Options := aDialog.Options + [ofAllowMultiSelect];
    aDialog.Filter := Engine.Groups.CreateFilter;
    aDialog.InitialDir := Engine.BrowseFolder;
    aDialog.DefaultExt := Engine.Groups[0].Extensions[0];
    aDialog.FileName := '*' + aDialog.DefaultExt;
    if aDialog.Execute then
    begin
      Engine.BeginUpdate;
      try
        aFile := nil;
        for i := 0 to aDialog.Files.Count - 1 do
        begin
          aFile := InternalOpenFile(aDialog.Files[i], True);
        end;
        if aFile <> nil then
          Current := aFile;
        Engine.UpdateState([ecsChanged, ecsState, ecsRefresh]);
      finally
        Engine.EndUpdate;
      end;
    end;
  finally
    aDialog.Free;
  end;
end;

function TEditorFiles.OpenFile(vFileName: string): TEditorFile;
begin
  if SameText(ExtractFileExt(vFileName), '.' + Engine.Extenstion) then
  begin
    Engine.Projects.Load(vFileName);
    Result := nil; //it is a project not a file.
  end
  else
  begin
    Result := LoadFile(vFileName);
  end;
end;

procedure TEditorProjects.Open;
var
  aDialog: TOpenDialog;
begin
  aDialog := TOpenDialog.Create(nil);
  try
    aDialog.Title := 'Open project';
    aDialog.DefaultExt := Engine.Extenstion;
    aDialog.Filter := 'Project files (*.' + Engine.Extenstion + ')|*.' + Engine.Extenstion + '|All files|*.*';
    aDialog.InitialDir := Engine.BrowseFolder;
    aDialog.FileName := '*' + aDialog.DefaultExt;
    if aDialog.Execute then
    begin
      Load(aDialog.FileName);
    end;
  finally
    aDialog.Free;
  end;
end;

procedure TEditorFiles.Prior;
var
  i: Integer;
begin
  if Current <> nil then
  begin
    i := Current.Index - 1;
    if i < 0 then
      i := Count - 1;
    SetCurrentIndex(i, True);
  end;
end;

procedure TEditorEngine.ProcessRecentFile(const FileName: string);
var
  i: Integer;
begin
  i := Options.RecentFiles.IndexOf(FileName);
  if i >= 0 then
    Options.RecentFiles.Move(i, 0)
  else
    Options.RecentFiles.Insert(0, FileName);
  while Options.RecentFiles.Count > 50 do
    Options.RecentFiles.Delete(50);
end;

procedure TEditorEngine.ProcessRecentProject(const FileName: string);
var
  i: Integer;
begin
  i := Options.RecentProjects.IndexOf(FileName);
  if i >= 0 then
    Options.RecentProjects.Move(i, 0)
  else
    Options.RecentProjects.Insert(0, FileName);
  while Options.RecentProjects.Count > 50 do
    Options.RecentProjects.Delete(50);
end;

procedure TEditorEngine.ProcessProject(const FileName: string);
var
  i: Integer;
begin
  i := Options.Projects.IndexOf(FileName);
  if i >= 0 then
    Options.Projects.Move(i, 0)
  else
    Options.Projects.Insert(0, FileName);
end;

procedure TEditorFiles.Save;
begin
  if Current <> nil then
    Current.SaveFile;
end;

procedure TEditorFiles.SaveAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].SaveFile;
  end;
end;

procedure TEditorFiles.SaveAs;
begin
  if Current <> nil then
    Current.SaveFile(True);
end;

procedure TEditorOptions.Save;
begin
  if (FileName <> '') and DirectoryExists(ExtractFilePath(FileName)) then
  begin
    SaveToFile(FileName);
    Engine.UpdateState([ecsFolder]);
  end;
end;

procedure TEditorFiles.SetCurrent(const Value: TEditorFile);
begin
  if FCurrent <> Value then
  begin
    FCurrent := Value;
    if not Engine.Updating then
      FCurrent.Show;
  end;
end;

procedure TEditorFiles.SetCurrentIndex(Index: Integer; vRefresh: Boolean);
var
  aCurrent: TEditorFile;
begin
  if Count <> 0 then
  begin
    if Index >= Count then
      Index := Count - 1;
    aCurrent := Items[Index];
    if aCurrent <> nil then
    begin
      Current := aCurrent;
    end;
  end;
  if vRefresh then
    Engine.UpdateState([ecsState, ecsRefresh]);
end;

procedure TEditorProjects.SetCurrent(const Value: TEditorProject);
begin
  if FCurrent <> Value then
  begin
    if FCurrent <> nil then
      FreeAndNil(FCurrent);
    FCurrent := Value;
    Engine.UpdateState([ecsChanged, ecsState, ecsRefresh]);
  end;
end;

procedure TEditorOptions.Show;
var
  i: Integer;
  aList: TList;
begin
  with TEditorOptionsForm.Create(Application) do
  begin
    aList := TSynHighlighterList.Create;
    try
      for i := 0 to Engine.Categories.Count - 1 do
      begin
        if Engine.Categories[i].Highlighter <> nil then
          aList.Add(Engine.Categories[i].Highlighter.ClassType);
      end;
      if Execute(Profile, aList) then
      begin
        Apply;
      end;
    finally
      aList.Free;
    end;
    Free;
  end;
end;

procedure TEditorEngine.RemoveProject(const FileName: string);
var
  i: Integer;
begin
  i := Options.Projects.IndexOf(FileName);
  if i >= 0 then
    Options.Projects.Delete(i);
end;

procedure TEditorEngine.RemoveRecentProject(const FileName: string);
var
  i: Integer;
begin
  i := Options.RecentProjects.IndexOf(FileName);
  if i >= 0 then
    Options.RecentProjects.Delete(i);
end;

procedure TEditorEngine.RemoveRecentFile(const FileName: string);
var
  i: Integer;
begin
  i := Options.RecentFiles.IndexOf(FileName);
  Options.RecentFiles.Delete(i);
end;

function TEditorEngine.GetUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

function TEditorEngine.ExpandFileName(FileName: string): string;
begin
  if Projects.Current <> nil then
    Result := ExpandToPath(FileName, Projects.Current.RootDir)
  else if Files.Current <> nil then
    Result := ExpandToPath(FileName, ExtractFilePath(Files.Current.Name))
  else
    Result := FileName;
end;

procedure TEditorEngine.DoChangedState(State: TEditorChangeState);
begin
  if Assigned(FOnChangedState) then
    FOnChangedState(State);
end;

procedure TEditorEngine.UpdateState(State: TEditorChangeState);
begin
  if Updating then
    FUpdateState := FUpdateState + State
  else
    DoChangedState(State);
end;

function TEditorFiles.LoadFile(vFileName: string; AppendToRecent: Boolean): TEditorFile;
begin
  Result := InternalOpenFile(vFileName, AppendToRecent);
  Engine.UpdateState([ecsChanged]);
  if Result <> nil then
    Current := Result;
  Engine.UpdateState([ecsState, ecsRefresh]);
end;

procedure TEditorFiles.Replace;
begin
  if Current <> nil then
    ShowSearchForm(Current.SynEdit, Engine.Options.SearchHistory, Engine.Options.ReplaceHistory, True);
end;

procedure TEditorFiles.Revert;
begin
  if Current <> nil then
  begin
    if Application.MessageBox(PChar('Revert file ' + Current.Name), 'Save', MB_YESNO) = mrYes then
      Current.Load(Current.Name);
  end;
end;

procedure TEditorEngine.SetBrowseFolder(const Value: string);
begin
  FBrowseFolder := Value;
  if FBrowseFolder <> '' then
    FBrowseFolder := IncludeTrailingPathDelimiter(FBrowseFolder);
end;

procedure TEditorEngine.DoMacroStateChange(Sender: TObject);
begin
  UpdateState([ecsState]);
end;

function TEditorEngine.GetWorkSpace: string;
begin
  Result := IncludeTrailingPathDelimiter(FWorkSpace);
end;

{ TProjectMembers }

function TProjectMembers.Find(vName: string): TProjectMember;
var
  i: Integer;
begin
  Result := nil;
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, vName) then
      begin
        Result := Items[i] as TProjectMember;
        break;
      end;
    end;
end;

function TProjectMembers.GetItems(Index: Integer): TProjectMember;
begin
  Result := inherited Items[Index] as TProjectMember;
end;

function TProjectMembers.GetMember(Index: string): TProjectMember;
begin
  Result := Find(Index);
end;

function TProjectMembers.IsExist(vName: string): Boolean;
begin
  Result := Find(vName) <> nil;
end;

{ TEditorFiles }

function TEditorFiles.GetItems(Index: Integer): TEditorFile;
begin
  Result := inherited Items[Index] as TEditorFile;
end;

function TEditorFiles.IsExist(vName: string): Boolean;
begin
  Result := FindFile(vName) <> nil;
end;

function TEditorFiles.SetActiveFile(FileName: string): TEditorFile;
begin
  Result := FindFile(FileName);
  if Result <> nil then
    Current := Result;
end;

destructor TEditorFiles.Destroy;
begin
  inherited Destroy;
end;

function TEditorFiles.ShowFile(vFileName: string): TEditorFile;
begin
  Result := InternalOpenFile(vFileName, False);
  Engine.UpdateState([ecsChanged]);
  if Result <> nil then
    Current := Result;
  Engine.UpdateState([ecsState, ecsRefresh]);
end;

procedure TEditorFiles.Refresh;
begin
  if Current <> nil then
  begin
    Current.SynEdit.Refresh;
  end;
end;

function TEditorFiles.ShowFile(const FileName: string;
  Line: Integer): TEditorFile;
begin
  Result := InternalOpenFile(FileName, False);
  Result.SynEdit.CaretY := Line;
  Result.SynEdit.CaretX := 1;
  Engine.UpdateState([ecsChanged]);
  if Result <> nil then
    Current := Result;
  Engine.UpdateState([ecsState, ecsRefresh]);
end;

{ TEditorFile }

procedure TEditorFile.Edit;
begin
  if not SynEdit.ReadOnly then
    Edited := True;
end;

procedure TEditorFile.Close;
var
  aParent: TEditorEngine;
  i: Integer;
  mr: Integer;
begin
  if Edited then
  begin
    mr := Application.MessageBox(PChar('Save file ' + Name + ' before close?'), 'Save', MB_YESNOCANCEL);
    if mr = mrCancel then
      Abort
    else if mr = mrYes then
      SaveFile
  end;

  i := Index;
  aParent := Engine;
  if aParent.Files.FCurrent = self then
    aParent.Files.FCurrent := nil;
  Free;
  aParent.Files.SetCurrentIndex(i, False);
  aParent.UpdateState([ecsChanged, ecsState, ecsRefresh]);
end;

constructor TEditorFile.Create(Collection: TCollection);
begin
  inherited;
  FSynEdit := TSynEdit.Create(Engine.Window);
  FSynEdit.OnChange := DoEdit;
  FSynEdit.OnStatusChange := DoStatusChange;
  FSynEdit.OnGutterClick := DoGutterClickEvent;
  FSynEdit.OnSpecialLineColors := DoSpecialLineColors;
  FSynEdit.BookMarkOptions.BookmarkImages := EditorResource.BookmarkImages;
  FSynEdit.TrimSpaceType := settLeaveLine;
  FSynEdit.BoundsRect := Engine.Window.ClientRect;
  FSynEdit.BorderStyle := bsNone;
  FSynEdit.Visible := False;
  FSynEdit.Align := alClient;
  FSynEdit.Realign;
  FSynEdit.WantTabs := True;
  FSynEdit.Parent := Engine.Window;
  //TDebugSupportPlugin.Create(Self);
end;

destructor TEditorFile.Destroy;
begin
  FSynEdit.Free;
  inherited;
end;

procedure TEditorFile.DoEdit(Sender: TObject);
begin
  Edit;
  Engine.Files.Edited;
end;

function TEditorFile.GetEngine: TEditorEngine;
begin
  Result := (Collection as TEditorFiles).FEngine;
end;

procedure TEditorFile.Load(FileName: string);
var
  Contents: string;
  Size: Integer;
  Stream: TFileStream;
begin
  FileName := ExpandFileName(FileName);
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  SynEdit.BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(Contents, nil, Size);
    Stream.Read(Pointer(Contents)^, Size);
    Mode := DetectFileMode(Contents);
    if eoTabsToSpaces in SynEdit.Options then
    begin
      Contents := ChangeTabsToSpace(Contents, SynEdit.TabWidth);
    end;
    SynEdit.Lines.Text := Contents; //lll
    Name := FileName;
    Edited := False;
    UpdateAge;
  finally
    SynEdit.EndUpdate;
    Stream.Free;
  end;
end;

procedure SaveAsMode(const FileName: string; Mode: TEditorFileMode; Strings: TStrings);
var
  aStream: TFileStream;
begin
  aStream := TFileStream.Create(FileName, fmCreate);
  try
    case Mode of
      efmWindows: SaveAsWindows(Strings, aStream);
      efmMac: SaveAsMac(Strings, aStream);
    else
      SaveAsUnix(Strings, aStream);
    end;
  finally
    aStream.Free;
  end;
end;

procedure TEditorFile.Save(FileName: string);
begin
  SaveAsMode(FileName, Mode, SynEdit.Lines);
  Name := FileName;
  Edited := False;
  Engine.UpdateState([ecsFolder]);
  UpdateAge;
end;

procedure TEditorFile.SetEdited(const Value: Boolean);
begin
  FEdited := Value;
end;

procedure TEditorFile.Show;
begin
  SynEdit.Visible := True;
  SynEdit.Show;
  SynEdit.BringToFront;
  (Engine.Window.Owner as TCustomForm).ActiveControl := SynEdit;
end;

procedure TEditorFile.SaveFile(AsNewFile: Boolean);
var
  aDialog: TSaveDialog;
  DoSave, DoRecent: Boolean;
  aName: string;
begin
  DoRecent := False;
  aName := '';
  if (FName = '') or AsNewFile then
  begin
    aDialog := TSaveDialog.Create(nil);
    aDialog.Title := 'Save file';
    aDialog.Filter := Engine.Groups.CreateFilter;
    aDialog.InitialDir := Engine.BrowseFolder;
    if Group <> nil then
      aDialog.DefaultExt := Group.Extensions[0]
    else
      aDialog.DefaultExt := Engine.Groups[0].Extensions[0];
    aDialog.FileName := '*' + aDialog.DefaultExt;


    DoSave := aDialog.Execute;
    if DoSave then
    begin
      aName := aDialog.FileName;
      DoRecent := True;
    end;
    aDialog.Free;
  end
  else
  begin
    aName := FName;
    DoSave := True;
  end;

  if DoSave then
  begin
    Save(aName);
    FName := aName;
    if DoRecent then
    begin
      Engine.ProcessRecentFile(aName);
      Engine.UpdateState([ecsRefresh, ecsState, ecsChanged]);
    end
    else
      Engine.UpdateState([ecsState, ecsRefresh]);
  end;
end;

function TEditorFile.CheckChanged: Boolean;
var
  mr: Integer;
begin
  Result := True;
  if (FileExists(Name)) and (FFileAge <> FileAge(Name)) then
  begin
    mr := Application.MessageBox(Pchar(Name + #13' was changed, update it?'), 'Update', MB_YESNOCANCEL);
    if mr = mrYes then
      Reload
    else
      UpdateAge;
    if mr = mrCancel then
      Result := False
  end;
end;

procedure TEditorFile.UpdateAge;
begin
  FFileAge := FileAge(Name);
end;

procedure TEditorFile.Reload;
begin
  Load(Name);
end;

procedure TEditorFile.SetGroup(const Value: TFileGroup);
begin
  if FGroup <> Value then
  begin
    FGroup := Value;
    if FGroup <> nil then
    begin
      FSynEdit.Highlighter := FGroup.Category.Highlighter;
      FGroup.Category.Completion.AddEditor(FSynEdit);
      //Engine.MacroRecorder.AddEditor(FSynEdit);
    end;
    Engine.Options.Profile.AssignTo(FSynEdit);
  end;
end;

function TEditorFile.GetHighlighter: TSynCustomHighlighter;
begin
  if Group <> nil then
    Result := Group.Category.Highlighter
  else
    Result := nil;
end;

function TEditorFile.GetReadonly: Boolean;
begin
  Result := SynEdit.ReadOnly;
end;

procedure TEditorFile.SetReadonly(const Value: Boolean);
begin
  SynEdit.ReadOnly := Value;
end;

procedure TEditorFile.NewSource;
begin

end;

function DetectFileMode(const Contents: string): TEditorFileMode;
var
  i: Integer;
begin
  Result := efmUnix;
  for i := 1 to Length(Contents) do
  begin
    if Contents[i] = #$D then
    begin
      if (i < Length(Contents) - 1) and (Contents[i + 1] = #$A) then
        Result := efmWindows
      else
        Result := efmMac;
      break;
    end
    else if Contents[i] = #$A then
    begin
      Result := efmUnix;
      break;
    end;
  end;
end;

function ChangeTabsToSpace(const Contents: string; TabWidth: Integer): string;
var
  p, l: Integer;
  procedure ScanToEOL;
  var
    i: Integer;
  begin
    i := p;
    while i <= l do
    begin
      if Contents[i] in [#13, #10] then
        break;
      Inc(i);
    end;
    if ((i + 1) <= l) and (Contents[i + 1] in [#13, #10]) then
      inc(i);
    Result := Result + Copy(Contents, p, i - p + 1);
    p := i + 1;
  end;

  procedure ScanSpaces;
  var
    i, c: Integer;
  begin
    i := p;
    c := 0;
    while i <= l do
    begin
      if Contents[i] = ' ' then
        c := c + 1
      else if Contents[i] = #9 then
        c := c + TabWidth
      else
        break;
      Inc(i);
    end;
    Result := Result + RepeatString(' ', c);
    p := i;
  end;
begin
  p := 1;
  l := Length(Contents);
  while p <= l do
  begin
    ScanSpaces;
    ScanToEOL;
  end;
end;

function TEditorFile.GetModeAsText: string;
begin
  case Mode of
    efmUnix: Result := 'Unix';
    efmWindows: Result := 'Windows';
    efmMac: Result := 'Mac';
  end;
end;

procedure TEditorFile.SetMode(const Value: TEditorFileMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    Edit;
    Engine.UpdateState([ecsState, ecsRefresh]);
  end;
end;

procedure TEditorFile.DoSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  if Engine.Debug.ExecutedEdit = Sender then
  begin
    if Engine.Debug.ExecutedLine = Line then
    begin
      Special := True;
      BG := clNavy;
      FG := clWhite;
    end;
  end;
end;

procedure TEditorFile.DoGutterClickEvent(Sender: TObject; X, Y, Line: integer; Mark: TSynEditMark);
var
  aLine: Integer;
begin
  if fgkExecutable in Group.Kind then
  begin
    aLine := SynEdit.RowToScreenRow(SynEdit.CaretY);//zaher
    Engine.Debug.Lock;
    try
      Engine.Debug.Breakpoints.Toggle(Name, aLine);
    finally
      Engine.Debug.Unlock;
    end;
    SynEdit.InvalidateLine(aLine);
  end;
end;

procedure TEditorFile.DoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if ([scLeftChar, scTopLine, scSelection] * Changes) <> [] then
    Engine.UpdateState([ecsState]);
end;

{ TEditorOptions }

constructor TEditorOptions.Create(AEngine: TEditorEngine);
begin
  inherited Create;
  FEngine := AEngine;
  FSearchHistory := TStringList.Create;
  FReplaceHistory := TStringList.Create;
  FSearchFolderHistory := TStringList.Create;
  FProfile := TEditorProfile.Create(nil);
  FHelpFiles := TStringList.Create;
  FExtraExtensions := TStringList.Create;
  FRecentFiles := TStringList.Create;
  FRecentProjects := TStringList.Create;
  FProjects := TStringList.Create;
  FShowFolder := True;
  FShowMessages := False;
  FCollectTimeout := 60;
  FOutputHeight := 100;
  FMessagesHeight := 100;
  FFoldersWidth := 180;
end;

destructor TEditorOptions.Destroy;
begin
  FSearchHistory.Free;
  FReplaceHistory.Free;
  FSearchFolderHistory.Free;
  FHelpFiles.Free;
  FExtraExtensions.Free;
  FProfile.Free;
  FRecentFiles.Free;
  FRecentProjects.Free;
  FProjects.Free;
  inherited;
end;

procedure TEditorOptions.SetProjects(const Value: TStringList);
begin
  if FRecentProjects <> Value then
    FRecentProjects.Assign(Value);
end;

procedure TEditorOptions.SetRecentFiles(const Value: TStringList);
begin
  if FRecentFiles <> Value then
    FRecentFiles.Assign(Value);
end;

procedure TEditorOptions.SetRecentProjects(const Value: TStringList);
begin
  if FRecentProjects <> Value then
    FRecentProjects.Assign(Value);
end;

{ TFileCategories }

procedure TFileCategories.Add(const Name: string; EditorFileClass: TEditorFileClass; CategoryClass: TFileCategoryClass; Kind: TFileCategoryKinds);
var
  aFC: TFileCategory;
begin
  aFC := CategoryClass.Create;
  aFC.FName := Name;
  aFC.FEditorFileClass := EditorFileClass;
  aFC.FKind := Kind;
  inherited Add(aFC);
end;

function TFileGroups.CreateFilter(vGroup: TFileGroup): string;
var
  i, j: Integer;
  s: string;
  aSupported: string;
  AExtensions:TStringList;
begin
  aSupported := '';
  AExtensions := TStringList.Create;
  try
    for i := 0 to Count - 1 do
    begin
      if (vGroup = nil) or (vGroup = Items[i]) then
        if fgkBrowsable in Items[i].Kind then
        begin
          if Result <> '' then
            Result := Result + '|';
          s := '';
          AExtensions.Clear;
          Items[i].EnumExtensions(AExtensions);
          for j := 0 to AExtensions.Count - 1 do
          begin
            if s <> '' then
              s := s + ';';
            s := s + '*.' + AExtensions[j];
            if aSupported <> '' then
              aSupported := aSupported + ';';
            aSupported := aSupported + '*.' + AExtensions[j];
          end;
          Result := Result + Items[i].DisplayName + ' (' + s + ')|' + s;
        end;
    end;
  finally
    AExtensions.Free;
  end;

  if Result <> '' then
    Result := 'All files ('+aSupported+')|' + aSupported + '|' + Result;

  if Result <> '' then
    Result := Result + '|';
  Result := Result + 'Any file (*.*)|*.*';
end;

constructor TFileCategories.Create(AEngine: TEditorEngine);
begin
  inherited Create;
  FEngine := AEngine;
end;

function TFileCategories.Find(vName: string): TFileCategory;
var
  i: Integer;
begin
  Result := nil;
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, vName) then
      begin
        Result := Items[i] as TFileCategory;
        break;
      end;
    end;
end;

function TFileGroups.FindExtension(vExtension: string): TFileGroup;
var
  i, j: Integer;
  AExtensions:TStringList;
begin
  Result := nil;
  if LeftStr(vExtension, 1) = '.' then
    vExtension := Copy(vExtension, 2, MaxInt);
  if vExtension <> '' then
  begin
    AExtensions := TStringList.Create;
    try
      for i := 0 to Count - 1 do
      begin
        AExtensions.Clear;
        Items[i].EnumExtensions(AExtensions);
        for j := 0 to AExtensions.Count - 1 do
        begin
          if SameText(AExtensions[j], vExtension) then
          begin
            Result := Items[i];
            break;
          end;
        end;
      end;
    finally
      AExtensions.Free;
    end;
  end;
end;

function TFileCategories.GetItem(Index: Integer): TFileCategory;
begin
  Result := inherited Items[Index] as TFileCategory;
end;

procedure TFileCategories.SetItem(Index: Integer; AObject: TFileCategory);
begin
  inherited Items[Index] := AObject;
end;

{ TFileCategory }

constructor TFileCategory.Create;
begin
  inherited;
  FHighlighter := CreateHighlighter;
  FCompletion := TSynCompletion.Create(nil);
  //FCompletion.Options := [scoLimitToMatchedText, {scoCaseSensitive, }scoUseInsertList, scoUsePrettyText, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter];
  FCompletion.Width := 340;
  FCompletion.EndOfTokenChr := '{}()[].<>/\:!$&*+-=%';
  //FCompletion.DefaultType := ctCode;
  //FCompletion.OnExecute := OnExecuteCompletion;
  FCompletion.ShortCut := scCtrl + VK_SPACE;
end;

function TFileCategory.CreateEditorFile(Files: TEditorFiles): TEditorFile;
begin
  Result := FEditorFileClass.Create(Files)
end;

function TFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

destructor TFileCategory.Destroy;
begin
  FreeAndNil(FHighlighter);
  FreeAndNil(FCompletion);
  inherited;
end;

procedure TFileCategory.OnExecuteCompletion(Kind: TSynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
begin
end;

{ TEditorProject }

constructor TEditorProject.Create(AEngine: TEditorEngine);
begin
  inherited Create;
  FEngine := AEngine;
  FDesktop := TEditorDesktop.Create;
  FDesktop.FEngine := Engine;
  FMembers := TProjectMembers.Create(TProjectMember);
  FMembers.FEngine := Engine;
  FCachedVariables := THashedStringList.Create;
  FCachedIdentifiers := THashedStringList.Create;
  FSaveDesktop := True;
end;

destructor TEditorProject.Destroy;
begin
  if FileName <> '' then
    Save;
  FMembers.Free;
  FDesktop.Free;
  FCachedVariables.Free;
  FCachedIdentifiers.Free;
  inherited;
end;

procedure TEditorProject.Loaded(Failed:Boolean);
begin
  inherited;
  if not Failed and FSaveDesktop then
    Desktop.Load;
end;

function TEditorProject.Save: Boolean;
begin
  if FileName = '' then
    Result := SaveAs
  else
  begin
    SaveToFile(FileName);
    Engine.ProcessRecentProject(FileName);
    Engine.UpdateState([ecsFolder, ecsChanged, ecsState, ecsRefresh]);
    Result := True;
  end;
end;

function TEditorProject.SaveAs: Boolean;
var
  aDialog: TSaveDialog;
begin
  aDialog := TSaveDialog.Create(nil);
  try
    aDialog.Title := 'Save project';
    aDialog.FileName := Name;
    aDialog.DefaultExt := Engine.Extenstion;
    aDialog.Filter := 'Project files (*.' + Engine.Extenstion + ')|*.' + Engine.Extenstion + '|All files|*.*';
    aDialog.InitialDir := Engine.BrowseFolder;
    aDialog.FileName := '*' + aDialog.DefaultExt;
    Result := aDialog.Execute;
    if Result then
    begin
      FileName := aDialog.FileName;
      Save;
    end;
  finally
    aDialog.Free;
  end;
end;

procedure TEditorProject.Saving;
begin
  inherited;
  if FSaveDesktop then
    Desktop.Save;
end;

{ TFileGroup }

constructor TFileGroup.Create;
begin
  inherited;
  FExtensions := TStringList.Create;
  FKind := [fgkBrowsable];
end;

procedure TFileGroup.EnumExtensions(vExtensions: TStringList);
var
   s:string;
begin
  vExtensions.BeginUpdate;
  try
    vExtensions.AddStrings(Extensions);
    s := Engine.Options.ExtraExtensions.Values[Name];
    ExtractStrings([';'], [' '], PChar(s), vExtensions);
  finally
    vExtensions.EndUpdate;
  end;
end;

destructor TFileGroup.Destroy;
begin
  FExtensions.Free;
  inherited;
end;

{ TFileGroups }

procedure TFileGroups.Add(const DisplayName, Name, Category: string; Extensions: array of string; Kind: TFileGroupKinds);
var
  aCategory: TFileCategory;
  aGroup: TFileGroup;
  i: Integer;
begin
  aCategory := Engine.Categories.Find(Category);
  if aCategory = nil then
    raise Exception.Create('Can not find category ' + Category);
  aGroup := TFileGroup.Create;
  aGroup.FCategory := aCategory;
  aGroup.FDisplayName := DisplayName;
  aGroup.FName := Name;
  aGroup.FKind := Kind;
  for i := 0 to Length(Extensions) - 1 do
    aGroup.Extensions.Add(Extensions[i]);
  inherited Add(aGroup);
end;

function TFileGroups.Find(vName: string): TFileGroup;
var
  i: Integer;
begin
  Result := nil;
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, vName) then
      begin
        Result := Items[i] as TFileGroup;
        break;
      end;
    end;
end;

function TFileGroups.GetItem(Index: Integer): TFileGroup;
begin
  Result := inherited Items[Index] as TFileGroup;
end;

procedure TFileGroups.SetItem(Index: Integer; AObject: TFileGroup);
begin
  inherited Items[Index] := AObject;
end;

constructor TFileGroups.Create(AEngine: TEditorEngine);
begin
  inherited Create;
  FEngine := AEngine;
end;

destructor TEditorProjects.Destroy;
begin
  FCurrent := nil;
  inherited;
end;

function TEditorProjects.GetIsOpened: Boolean;
begin
  Result := FCurrent <> nil;
end;

procedure TFileGroups.EnumExtensions(vExtensions: TStringList);
var
  i:Integer;
begin
  for i := 0 to Count -1 do
  begin
    Items[i].EnumExtensions(vExtensions);
  end;
end;

{ TEditorDesktopFiles }

function TEditorDesktopFiles.Add(FileName: string): TEditorDesktopFile;
begin
  Result := inherited Add as TEditorDesktopFile;
  Result.FileName := FileName;
end;

function TEditorDesktopFiles.Find(vName: string): TEditorDesktopFile;
var
  i: Integer;
begin
  Result := nil;
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].FileName, vName) then
      begin
        Result := Items[i] as TEditorDesktopFile;
        break;
      end;
    end;
end;

function TEditorDesktopFiles.GetItems(Index: Integer): TEditorDesktopFile;
begin
  Result := inherited Items[Index] as TEditorDesktopFile;
end;

function TEditorDesktopFiles.IsExist(vName: string): Boolean;
begin
  Result := Find(vName) <> nil;
end;

{ TDebugSupportPlugin }

type
  THackSynEdit = class(TCustomSynEdit);

procedure TDebugSupportPlugin.AfterPaint(ACanvas: TCanvas; const AClip: TRect; FirstLine, LastLine: integer);
var
  i, x, y, lh: Integer;
  aLine: Integer;
  aRect: TRect;
begin
  inherited;
  lh := FEditorFile.SynEdit.LineHeight;
  if (Engine.Debug.ExecutedEdit = FEditorFile.SynEdit) and (Engine.Debug.ExecutedLine >= 0) then
  begin
    x := 14;
    Y := (lh - EditorResource.SmallImages.Height) div 2 + lh * (FEditorFile.SynEdit.RowToScreenRow(Engine.Debug.ExecutedLine) - FEditorFile.SynEdit.TopLine);
    EditorResource.SmallImages.Draw(ACanvas, X, Y, 3);
  end;

  Engine.Debug.Lock;
  try
    x := 1;
    for i := 0 to Engine.Debug.Breakpoints.Count - 1 do
    begin
      if SameText(Engine.Debug.Breakpoints[i].FileName, FEditorFile.Name) then
      begin
        aLine := Engine.Debug.Breakpoints[i].Line;
        with FEditorFile.SynEdit do
        begin
          if (aLine >= TopLine) and (aLine < TopLine + LinesInWindow) then
          begin
            Y := (lh - EditorResource.SmallImages.Height) div 2 + lh * (RowToScreenRow(aLine) - RowToScreenRow(TopLine));
            EditorResource.SmallImages.Draw(ACanvas, X, Y, 4);

            aRect := Rect(0, LineHeight * (RowToScreenRow(aLine) - RowToScreenRow(TopLine)), ClientWidth, 0);
            aRect.Bottom := aRect.Top + LineHeight;
            ACanvas.Brush.Color := $00CCCCFF;
            ACanvas.Pen.Mode := pmMask;
            ACanvas.Rectangle(aRect);
          end;
        end;
      end;
    end;
  finally
    Engine.Debug.Unlock;
  end;
end;

constructor TDebugSupportPlugin.Create(AOwner: TEditorFile);
begin
  inherited Create;
  FEditorFile := AOwner;
end;

{ TEditorDesktop }

constructor TEditorDesktop.Create;
begin
  FFiles := TEditorDesktopFiles.Create(TEditorDesktopFile);
  inherited;
end;

destructor TEditorDesktop.Destroy;
begin
  FFiles.Free;
  inherited;
end;

procedure TEditorDesktop.Load;
var
  i: Integer;
  aItem: TEditorDesktopFile;
  aFile: TEditorFile;
begin
  Engine.BeginUpdate;
  try
    Engine.Debug.Lock;
    try
{      Engine.Debug.BreakpointsClear;
      for i := 0 to Breakpoints.Count - 1 do
      begin
        Engine.Debug.Breakpoints.Add(Breakpoints[i].FileName, Breakpoints[i].Line);
      end;

      Engine.Debug.Watches.Clear;
      for i := 0 to Watches.Count - 1 do
      begin
        Engine.Debug.Watches.Add(Watches[i].VariableName, Watches[i].Value);
      end;}
    finally
      Engine.Debug.Unlock;
    end;
    Engine.UpdateState([ecsDebug]);

    Engine.Files.CloseAll;
    for i := 0 to Files.Count - 1 do
    begin
      aItem := Files[i];
      if FileExists(aItem.FileName) then
      begin
        aFile := Engine.Files.LoadFile(aItem.FileName, False);
        if aFile <> nil then
        begin
          aFile.SynEdit.CaretX := aItem.CaretX;
          aFile.SynEdit.CaretY := aItem.CaretY;
          aFile.SynEdit.TopLine := aItem.TopLine;
        end;
      end;
    end;
    Engine.Files.SetActiveFile(Files.CurrentFile);
  finally
    Engine.EndUpdate;
    Files.Clear;
  end;
end;

procedure TEditorDesktop.Save;
var
  i: Integer;
  aItem: TEditorDesktopFile;
  aFile: TEditorFile;
begin
{  Breakpoints.Clear;
  Watches.Clear;
  Engine.Debug.Lock;
  try
    for i := 0 to Engine.Debug.Breakpoints.Count - 1 do
    begin
      Breakpoints.Add(Engine.Debug.Breakpoints[i].FileName, Engine.Debug.Breakpoints[i].Line);
    end;

    for i := 0 to Engine.Debug.Watches.Count - 1 do
    begin
      Watches.Add(Engine.Debug.Watches[i].VariableName, Engine.Debug.Watches[i].Value);
    end;
  finally
    Engine.Debug.Unlock;
  end;}

  Files.Clear;
  if Engine.Files.Current <> nil then
    Files.CurrentFile := Engine.Files.Current.Name
  else
    Files.CurrentFile := '';
  for i := 0 to Engine.Files.Count - 1 do
  begin
    aFile := Engine.Files[i];
    aItem := Files.Add(aFile.Name);
    aItem.CaretX := aFile.SynEdit.CaretX;
    aItem.CaretY := aFile.SynEdit.CaretY;
    aItem.TopLine := aFile.SynEdit.TopLine;
  end;
end;

{ TEditorMessages }

function TEditorMessages.GetItem(Index: Integer): TEditorMessage;
begin
  Result := inherited Items[Index] as TEditorMessage;
end;

function TEditorMessages.GetText(Index: Integer): string;
begin
  if Index < Count then
    Result := Items[Index].Text
  else
    Result := '';
end;

procedure TEditorMessages.SetItem(Index: Integer;
  const Value: TEditorMessage);
begin
  inherited Items[Index] := Value;
end;

{ TEditorMessagesList }

function TEditorMessagesList.GetItem(Index: Integer): TEditorMessages;
begin
  Result := inherited Items[Index] as TEditorMessages;
end;

function TEditorMessagesList.GetMessages(Name: string): TEditorMessages;
begin
  Result := Find(Name);
  if Result = nil then
  begin
    Result := TEditorMessages.Create;
    Result.Name := Name;
  end;
  Add(Result);
end;

procedure TEditorMessagesList.SetItem(Index: Integer;
  const Value: TEditorMessages);
begin
  inherited Items[Index] := Value;
end;

function TEditorMessagesList.Find(Name: string): TEditorMessages;
var
  i: Integer;
begin
  Result := nil;
  if Name <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, Name) then
      begin
        Result := Items[i];
        break;
      end;
    end;
end;

end.

