unit EditorEngine;
{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$INTERFACES CORBA} //Needed for interfaces without guid
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{**


  Categories: list
    Category: object have have highlighter, autocomplete ...
      Groups: list
        Group: object Have multiple extension externsions that all same type, Group.Name is not an extension
          Extensions (pas, pp, inc)


Portable Notes:
  Main folder related to project folder, Project folder related to Project File folder
}
interface

uses
  Messages, SysUtils, Forms, StdCtrls, StrUtils, Dialogs, Variants, Classes, Controls, LCLIntf, LConvEncoding,
  FileUtil, LazFileUtils,
  Graphics, Contnrs, Types, IniFiles, EditorOptions, EditorColors, EditorProfiles,
  SynEditMarks, SynCompletion, SynEditTypes, SynEditMiscClasses,
  SynEditHighlighter, SynEditKeyCmds, SynEditMarkupBracket, SynEditSearch, ColorUtils,
  SynEdit, SynEditTextTrimmer, SynTextDrawer, EditorDebugger, SynGutterBase, SynEditPointClasses, SynMacroRecorder,
  dbgpServers, Masks, mnXMLRttiProfile, mnXMLUtils, mnClasses, fgl,
  mnUtils, LCLType, EditorClasses, EditorRun;

type
  TThreeStates = (stateNone, stateFalse, stateTrue);

  TSynCompletionType = (ctCode, ctHint, ctParams);

  TEditorEngine = class;
  TFileCategory = class;
  TFileGroup = class;
  TFileGroups = class;
  TEditorFile = class;
  TEditorProject = class;
  TRunProjectOptions = class;

  EEditorException = class(Exception)
  private
    FErrorLine: integer;
  public
    property ErrorLine: integer read FErrorLine write FErrorLine;
  end;

  IEditorOptions = interface
    ['{3D32B7C6-7D6A-4E95-B616-4374BCDAAD37}']
    procedure Apply;
    procedure Retrieve;
  end;

  IEditorProjectFrame = interface
    ['{C5D191FE-D449-4EB4-9D7D-E3E53DBB89FA}']
    function GetProject: TEditorProject;
    property Project: TEditorProject read GetProject;
  end;

  IEditorControl = interface
    ['{8C2646A1-2738-4830-8107-CF8753D14EBD}']
    function GetMainControl: TWinControl; //Like datagrid in csv form
  end;

  { GXMLItems }

  generic GXMLItems<_Object_> = class(TmnXMLItems) //TODO move it to mnXMLRttiProfile
  private
    function GetItem(Index: Integer): _Object_;
    procedure SetItem(Index: Integer; const Value: _Object_);
  public
    function DoCreateItem(AClass: TmnXMLItemClass):TmnXMLItem; override;
    property Items[Index: Integer]: _Object_ read GetItem write SetItem; default;
  published
  end;

  { TEditorExtension }

  TEditorExtension = class(TObject)
  public
    Name: string;
    ImageIndex: Integer;
  end;

  { TEditorExtensions }

  TEditorExtensions = class(specialize TmnNamedObjectList<TEditorExtension>)
  private
  public
    procedure Add(Name:string; ImageIndex: Integer = -1);
  end;

  { TmnSynEdit }

  TmnSynEdit = class(TSynEdit)
  protected
  public
    procedure ExecuteCommand(Command: TSynEditorCommand; const AChar: TUTF8Char; Data: pointer); override;
  end;

  { TEditorDesktopFile }

  TEditorDesktopFile = class(TCollectionItem)
  private
    FFileName: string;
    FCaretY: integer;
    FCaretX: integer;
    FTopLine: integer;
  public
  published
    property FileName: string read FFileName write FFileName;
    property CaretX: integer read FCaretX write FCaretX default 1;
    property CaretY: integer read FCaretY write FCaretY default 1;
    property TopLine: integer read FTopLine write FTopLine default 1;
  end;

  { TEditorDesktopFiles }

  TEditorDesktopFiles = class(TCollection)
  private
    FCurrentFile: string;
    FCurrentFolder: string;
    function GetItems(Index: integer): TEditorDesktopFile;
  protected
  public
    function Add(FileName: string): TEditorDesktopFile;
    function Find(vName: string): TEditorDesktopFile;
    function IsExist(vName: string): Boolean;
    property Items[Index: integer]: TEditorDesktopFile read GetItems; default;
  published
    property CurrentFile: string read FCurrentFile write FCurrentFile;
    property CurrentFolder: string read FCurrentFolder write FCurrentFolder;
  end;

  { TmneBreakpoint }

  TmneBreakpoint = class(TmnXMLItem)
  private
    FFileName: string;
    FLineNo: Integer;
  public
    ID: integer;
    Handle: integer;
  published
    property FileName: string read FFileName write FFileName;
    property LineNo: Integer read FLineNo write FLineNo;
  end;

  { TmneBreakpoints }

  TmneBreakpoints = class(specialize GXMLItems<TmneBreakpoint>)
  private
  public
    procedure Add(AFileName:string; ALineNo: Integer);
  end;

  { TmneWatch }

  TmneWatch = class(TmnXMLItem)
  private
    FName: string;
  public
    ID: integer;
    Handle: integer;
  published
    property Name: string read FName write FName;
  end;

  { TmneWatches }

  TmneWatches = class(specialize GXMLItems<TmneWatch>)
  private
  public
    procedure Add(AName:string);
  end;

  TEditorDesktop = class(TPersistent)
  private
    FBreakpoints: TmneBreakpoints;
    FWatches: TmneWatches;
    FFiles: TEditorDesktopFiles;
    FProject: TEditorProject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    property Project: TEditorProject read FProject;
  published
    property Breakpoints: TmneBreakpoints read FBreakpoints; //for saving loading Breakpoints
    property Watches: TmneWatches read FWatches; //same

    property Files: TEditorDesktopFiles read FFiles;
  end;

  TAddFrameCallBack = procedure(AFrame: TFrame) of object;
  TAddClickCallBack = procedure(Name, Caption: string; OnClickEvent: TNotifyEvent; ShortCut: TShortCut = 0) of object;


  TFileGroupKind = (
    fgkDefault,
    fgkText, //Is it an Text Editor like SQL or PHP
    fgkUneditable, // Can't be editable
    fgkExecutable,//You can guess what is it :P
    fgkShell,//TODO: Executable but hi priority, override the main file or project tendency
    fgkDebugee, //TODO
    fgkMain,//this can be the main file for project
    fgkResult,//Result file, generated, like: exe or .o or .hex
    fgkBrowsable,//When open file show it in the extension list
    fgkAssociated, //Editor can be the editor of this files, like .php, .inc, but .txt is not
    fgkVirtual //Not a real file, like output console
  );

  TFileGroupKinds = set of TFileGroupKind;

  TFileGroupStyle = (
    fgsFolding
  );

  TFileGroupStyles = set of TFileGroupStyle;

  {
    Tendency
    Run, Compile, Collect file groups and have special properties
  }

  TEditorCapability = (
    capErrors,
    capBrowser,
    capOptions,
    capRun, //Can run this file
    capCompile, //Can compile this file
    capLink, //Can need link before run
    capLint, //Check error of file without compiling or run
    capUpload, //Have upload, like avr projects need to upload to mcu
    capDebug, //we can debug the project/file
    capEval, //Debugger can evaluate
    capTrace, //Steps (Step Into, Step Over etc...)
    capDebugServer //PHP style need to start debug server
  );

  TEditorCapabilities = set of TEditorCapability;

  { TEditorProjectOptions }

  TEditorProjectOptions = class(TPersistent)
  private
    FIndentMode: TIndentMode;
    FOverrideEditorOptions: Boolean;
    FTabWidth: Integer;
  public
    constructor Create; virtual; //do delete it, need to override it
    destructor Destroy; override;
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); virtual;
    procedure CreateProjectPanel(AOwner: TComponent; AProject: TEditorProject; var AFrame: TFrame); virtual;
  published
    property OverrideEditorOptions: Boolean read FOverrideEditorOptions write FOverrideEditorOptions default False;
    property TabWidth: Integer read FTabWidth write FTabWidth default 4;
    property IndentMode: TIndentMode read FIndentMode write FIndentMode default idntTabsToSpaces;
  end;

  { TEditorTendency }

  TEditorTendency = class abstract(TEditorDebugTendency)
  private
    //FEditorOptions: TSynEditorOptions;
    FGroups: TFileGroups;
    FOutputExtension: string;
    FRunOptions: TRunProjectOptions;

    FIndentMode: TIndentMode;
    FOverrideEditorOptions: Boolean;
    FTabWidth: Integer;
    function GetIsDefault: Boolean; virtual; //Keep it private
  protected
    IsPrepared: Boolean;
    FCapabilities: TEditorCapabilities;
    procedure AddGroup(vName, vCategory: string);
    function CreateDebugger: TEditorDebugger; virtual;
    procedure Init; virtual;
    procedure Created; virtual;
    procedure DoRun(Info: TmneRunInfo); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure HelpKeyword(AWord:string); virtual;
    procedure Run(RunActions: TmneRunActions);
    procedure SendMessage(S: string; vMessageType: TNotifyMessageType); override;

    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); virtual;
    function CreateOptions: TEditorProjectOptions; virtual;
    procedure EnumRunCommands(Items: TStrings); virtual;
    function GetDefaultGroup: TFileGroup;
    //OSDepended: When save to file, the filename changed depend on the os system name
    procedure Prepare;
    procedure Unprepare;
    //
    property Capabilities: TEditorCapabilities read FCapabilities;
    property Groups: TFileGroups read FGroups;
    property IsDefault: Boolean read GetIsDefault;
    property OutputExtension: string read FOutputExtension write FOutputExtension;
  published
    //Override options
    property OverrideEditorOptions: Boolean read FOverrideEditorOptions write FOverrideEditorOptions default False;
    property TabWidth: Integer read FTabWidth write FTabWidth default 4;
    property IndentMode: TIndentMode read FIndentMode write FIndentMode default idntTabsToSpaces;

    //property EditorOptions: TSynEditorOptions read FEditorOptions write FEditorOptions;
    property RunOptions: TRunProjectOptions read FRunOptions;// write FRunOptions;
  end;

  TEditorTendencyClass = class of TEditorTendency;

  { TDefaultTendency }
  {
    used only if no Tendency defined
  }

  TDefaultTendency = class(TEditorTendency)
  private
    function GetIsDefault: Boolean; override;
  protected
    procedure Init; override;
    procedure Created; override;
  public
  end;

  { TEditorSCM }

  TEditorSCM = class(TEditorElement)
  private
  protected
    procedure Execute(App, Cmd:string); virtual; abstract;
  public
    constructor Create; override;
    procedure CommitDirectory(Directory: string); virtual; abstract;
    procedure CommitFile(FileName: string); virtual; abstract;
    procedure UpdateDirectory(Directory: string); virtual; abstract;
    procedure UpdateFile(FileName: string); virtual; abstract;
    procedure RevertDirectory(Directory: string); virtual; abstract;
    procedure RevertFile(FileName: string); virtual; abstract;
    procedure DiffFile(FileName: string); virtual; abstract;
    procedure DiffToFile(FileName, ToFileName: string); virtual; abstract;
    procedure AddFile(FileName: string); virtual; abstract;
  end;

  TEditorSCMClass = class of TEditorSCM;

  { TRunProjectOptions }

  TRunProjectInfo = record
    Command: string;
    MainFolder: string;
    Params: string;
    Console: TThreeStates;
    Pause: TThreeStates;
    MainFile: string;
    OutputFile: string;
    Require: string;
    ConfigFile: string;
    ExpandPaths: Boolean;
  end;

  TRunProjectOptions = class(TPersistent)
  private
    FInfo: TRunProjectInfo;
    FPaths: TStrings;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetPaths(AValue: TStrings);
    procedure Merge(AOptions: TRunProjectOptions);
    procedure Copy(AOptions: TRunProjectOptions);
    procedure Assign(Source: TPersistent); override;
  published
    property Pause: TThreeStates read FInfo.Pause write FInfo.Pause default stateTrue;
    property Console: TThreeStates read FInfo.Console write FInfo.Console default stateTrue;
    property Command: string read FInfo.Command write FInfo.Command;
    property Params: string read FInfo.Params write FInfo.Params;
    property Require: string read FInfo.Require write FInfo.Require;
    property MainFolder: string read FInfo.MainFolder write FInfo.MainFolder;
    property MainFile: string read FInfo.MainFile write FInfo.MainFile;
    property OutputFile: string read FInfo.OutputFile write FInfo.OutputFile;
    property ExpandPaths: Boolean read FInfo.ExpandPaths write FInfo.ExpandPaths;
    property ConfigFile: string read FInfo.ConfigFile write FInfo.ConfigFile;
    property Paths: TStrings read FPaths write SetPaths;
  end;

  { TEditorProject }

  TEditorProject = class(TmnXMLProfile)
  private
    FOptions: TEditorProjectOptions;
    FRunOptions: TRunProjectOptions;
    FTendencyName: string;
    FDescription: string;
    FFileName: string;
    FName: string;
    FSaveDesktop: Boolean;
    FDesktop: TEditorDesktop;
    FTendency: TEditorTendency;
    FSCM: TEditorSCM;
    FTitle: string;
    function GetIsActive: Boolean;
    function GetPath: string;
    procedure SetTendency(AValue: TEditorTendency);
    procedure SetTendencyName(AValue: string);
    procedure SetSCM(AValue: TEditorSCM);
  protected
    procedure RecreateOptions;
    procedure RttiCreateObject(var vObject: TObject; vInstance: TObject; vObjectClass: TClass; const vClassName, vName: string); override;
    procedure Loaded(Failed: Boolean); override;
    procedure Saving; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: string); override;
    property FileName: string read FFileName write FFileName;
    property Path: string read GetPath;
    procedure SetSCMClass(SCMClass: TEditorSCM);
    //Tendency here point to one of Engine.Tendencies so it is not owned by project
    property Tendency: TEditorTendency read FTendency write SetTendency;
    property RunOptions: TRunProjectOptions read FRunOptions write FRunOptions;
    property IsActive: Boolean read GetIsActive;
  published
    property Name: string read FName write FName;
    property Title: string read FTitle write FTitle;
    property TendencyName: string read FTendencyName write SetTendencyName;
    //SCM now owned by project and saved or loaded with it, the SCM object so assigned to will be freed with the project
    property SCM: TEditorSCM read FSCM write SetSCM;

    property Description: string read FDescription write FDescription;
    property SaveDesktop: Boolean read FSaveDesktop write FSaveDesktop default True;
    property Desktop: TEditorDesktop read FDesktop stored FSaveDesktop;
    property Options: TEditorProjectOptions read FOptions write FOptions default nil;
  end;

  TRunProject = class(TEditorProject)
  published
    property RunOptions;
  end;

  {* Default project to used when no project opened
  }

  { TDefaultProject }

  TDefaultProject = class sealed(TEditorProject)
  public
  end;

  { TDebugMarksPart }

  TSynDebugMarksPart = class(TSynGutterPartBase)
  protected
    FEditorFile: TEditorFile;
    procedure Init; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
  published
    property MarkupInfo;
  end;

  TEditorLinesMode = (efmUnix, efmWindows, efmMac);
  TEditCapability = set of (ecpAllowCopy, ecpAllowPaste, ecpAllowCut, ecpAllowUndo, ecpAllowRedo);

  { TEditorFile }

  TEditorFile = class(TCollectionItem, IFileEditor)
  private
    FFileEncoding: string;
    FName: string;
    FIsNew: Boolean;
    FIsEdited: Boolean;
    FFileAge: Integer;
    FFileSize: int64;
    FGroup: TFileGroup;
    FRelated: string;
    FLinesMode: TEditorLinesMode;
    function GetCapability: TEditCapability;
    function GetIsText: Boolean;
    function GetNakeName: string;
    function GetPureName: string;
    function GetExtension: string;
    function GetPath: string;
    function GetTendency: TEditorTendency;
    procedure SetFileEncoding(AValue: string);
    procedure SetGroup(const Value: TFileGroup);
    procedure SetIsEdited(const Value: Boolean);
    procedure SetIsNew(AValue: Boolean);
    function GetLinesModeAsText: string;
    procedure SetLinesMode(const Value: TEditorLinesMode);
    procedure SetExtension(AValue: string);
    procedure SetNakeName(AValue: string);
    procedure SetPureName(AValue: string);
  protected
    procedure GroupChanged; virtual;
    function GetIsReadonly: Boolean; virtual;
    procedure SetIsReadonly(const Value: Boolean); virtual;
    function GetControl: TWinControl; virtual;
    procedure DoGetCapability(var vCapability: TEditCapability); virtual;
  protected
    procedure Edit;
    procedure DoEdit(Sender: TObject); virtual;
    procedure DoStatusChange(Sender: TObject; Changes: TSynStatusChanges); virtual;
    procedure UpdateAge; virtual;
    procedure NewContent; virtual;
    procedure DoLoad(FileName: string); virtual; abstract;
    procedure DoSave(FileName: string); virtual; abstract;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Load(FileName: string);
    procedure Save(FileName: string);
    procedure Rename(ToNakeName: string); //only name not with the path
    procedure Delete; //only name not with the path

    procedure SaveFile(Extension: string = ''; AsNewFile: Boolean = False); virtual;
    procedure Show; virtual; //Need to activate it after show to focus editor
    function Visible: Boolean;
    procedure Activate; virtual;
    function Activated: Boolean;
    procedure Close;
    procedure Reload;
    procedure OpenInclude; virtual;
    function CanOpenInclude: Boolean; virtual;
    function CheckChanged(Force: Boolean = False): Boolean;
    //
    procedure GotoLine; virtual;
    procedure Find; virtual;
    procedure FindNext; virtual;
    procedure FindPrevious; virtual;
    procedure Replace; virtual;
    procedure Refresh; virtual;
    function GetHint(HintControl: TControl; CursorPos: TPoint; out vHint: string): Boolean; virtual;
    function GetGlance: string; virtual; //Simple string to show in the corner of mainform
    //
    function GetLanguageName: string; virtual; //TODO need to get more good name to this function
    procedure SetLine(Line: Integer); virtual;
    //Clipboard
    function CanCopy: Boolean;
    function CanPaste: Boolean;
    property Capability: TEditCapability read GetCapability;

    procedure Paste; virtual;
    procedure Copy; virtual;
    procedure Cut; virtual;
    procedure SelectAll; virtual;

    //run the file or run the project depend on the project type (Tendency)
    property LinesMode: TEditorLinesMode read FLinesMode write SetLinesMode default efmUnix;
    property FileEncoding: string read FFileEncoding write SetFileEncoding;
    property LinesModeAsText: string read GetLinesModeAsText;
    property IsText: Boolean read GetIsText;
    property Name: string read FName write FName;
    property NakeName: string read GetNakeName write SetNakeName; //no path with ext
    property PureName: string read GetPureName write SetPureName; //no path no ext
    property Extension: string read GetExtension write SetExtension;
    property Path: string read GetPath;
    property Tendency: TEditorTendency read GetTendency;
    property Related: string read FRelated write FRelated;
    property IsEdited: Boolean read FIsEdited write SetIsEdited; //TODO rename to IsChanged
    property IsNew: Boolean read FIsNew write SetIsNew default False;
    property IsReadOnly: Boolean read GetIsReadonly write SetIsReadonly;
    property Group: TFileGroup read FGroup write SetGroup;
    property Control: TWinControl read GetControl;
  published
  end;

  { TControlEditorFile }

  TControlEditorFile = class(TEditorFile, IControlEditor)
  private
    FControl: TWinControl;
    procedure SetControl(AValue: TWinControl);
  protected
    function GetControl: TWinControl; override;
    function GetIsReadonly: Boolean; override;
    procedure DoLoad(FileName: string); override;
    procedure DoSave(FileName: string); override;
  public
    destructor Destroy; override;
    property Control: TWinControl read GetControl write SetControl;
  end;

  { TTextEditorFile }

  TTextEditorFile = class(TEditorFile, ITextEditor)
  private
    FHighlightLine: Integer;
    FSynEdit: TSynEdit;
    procedure SetHighlightLine(AValue: Integer);
  protected
    LastGotoLine: Integer;
    function GetIsReadonly: Boolean; override;
    procedure SetIsReadonly(const Value: Boolean); override;
    function GetControl: TWinControl; override;
    procedure DoLoad(FileName: string); override;
    procedure DoSave(FileName: string); override;
    procedure GroupChanged; override;
    procedure DoEdit(Sender: TObject); override;
    procedure DoStatusChange(Sender: TObject; Changes: TSynStatusChanges); override;
    procedure DoGutterClickEvent(Sender: TObject; X, Y, Line: integer; Mark: TSynEditMark);
    procedure DoSpecialLineMarkup(Sender: TObject; Line: integer; var Special: Boolean; Markup: TSynSelectedColor);
    procedure DoGetCapability(var vCapability: TEditCapability); override;
    procedure SynEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Find; override;
    procedure FindNext; override;
    procedure FindPrevious; override;
    procedure Replace; override;
    procedure Refresh; override;
    function GetHint(HintControl: TControl; CursorPos: TPoint; out vHint: string): Boolean; override;
    function GetGlance: string; override;
    function EvalByMouse(p: TPoint; out v, s, t: string): boolean;
    function EvalByCursor(out v, s, t: string): boolean;
    procedure UpdateAge; override;
    function GetLanguageName: string; override;

    procedure Copy; override;
    procedure Paste; override;
    procedure Cut; override;
    procedure SelectAll; override;

    procedure SetLine(Line: Integer); override;
    procedure GotoLine; override;
    property SynEdit: TSynEdit read FSynEdit;
    property HighlightLine: Integer read FHighlightLine write SetHighlightLine;
  end;

  TSourceEditorFile = class(TTextEditorFile, IExecuteEditor, IWatchEditor)
  end;

  { TEditorFiles }

  TEditorFiles = class(TCollection)
  private
    FCheckingChanged: Boolean;
    FCurrent: TEditorFile;
    function GetItems(Index: integer): TEditorFile;
    function GetCurrent: TEditorFile;
    procedure SetCurrent(const Value: TEditorFile);
  protected
    function SetActiveFile(FileName: string): TEditorFile;
  public
    destructor Destroy; override;
    function FindFile(const vFileName: string): TEditorFile;
    function IsExist(vName: string): Boolean;

    function InternalOpenFile(FileName: string; AppendToRecent: Boolean): TEditorFile;
    function CreateEditorFile(vExtension: string): TEditorFile;
    function LoadFile(vFileName: string; AppendToRecent: Boolean = True): TEditorFile;
    function ShowFile(vFileName: string): TEditorFile; overload; //open it without add to recent, for debuging
    function ShowFile(const FileName: string; Line: integer): TEditorFile; overload;
    function OpenFile(vFileName: string; ActivateIt: Boolean = false): TEditorFile;
    procedure SetCurrentIndex(Index: integer; vRefresh: Boolean);
    function New(vGroup: TFileGroup = nil): TEditorFile; overload;
    function New(Name: string; Control: TWinControl): TEditorFile; overload;

    procedure Open;
    procedure Save;
    procedure SaveAll;
    procedure ReloadAll;
    procedure CheckAll;
    procedure SaveAs;

    procedure Revert;
    procedure Refresh;
    procedure Next;
    procedure Prior;
    procedure Edited;
    procedure Replace;
    procedure Find;
    procedure FindNext;
    procedure FindPrevious;
    procedure CheckChanged;
    procedure CloseAll;
    procedure CloseOthers;
    function GetEditedCount: integer;
    property CheckingChanged: Boolean read FCheckingChanged write FCheckingChanged; //public to use it in SearchInFiles
    property Current: TEditorFile read GetCurrent write SetCurrent;
    property Items[Index: integer]: TEditorFile read GetItems; default;
  published
  end;

  TSynBreakPointItem = class(TSynObjectListItem)
  public
    IsBreakPoint: Boolean;
  end;

  TSortFolderFiles = (srtfByNames, srtfByExt);
  TShowFolderFiles = (sffRelated, sffKnown, sffAll);
  TEditorFileClass = class of TEditorFile;

  TOnEngineChanged = procedure of object;

  { TEditorOptions }

  TEditorOptions = class(TmnXMLProfile)
  private
    FAutoOpenProject: Boolean;
    FFallbackToText: Boolean;
    FIgnoreNames: string;
    FLastFolder: string;
    FLastProject: string;
    FRecentFolders: TStringList;
    FShowFolder: Boolean;
    FShowFolderFiles: TShowFolderFiles;
    FShowToolbar: Boolean;
    FSortFolderFiles: TSortFolderFiles;
    FWindowMaxmized: Boolean;
    FBoundRect: TRect;
    FShowMessages: Boolean;
    FCollectAutoComplete: Boolean;
    FCollectTimeout: QWORD;
    FReplaceHistory: TStringList;
    FAutoStartDebugServer: Boolean;

    FMessagesHeight: integer;
    FFoldersPanelWidth: integer;

    FExtraExtensions: TStringList;
    FSearchFolderHistory: TStringList;
    FSearchHistory: TStringList;
    FProfile: TEditorProfile;

    FRecentFiles: TStringList;
    FRecentProjects: TStringList;
    FProjects: TStringList;
    function GetAnsiCodePage: Integer;
    procedure SetAnsiCodePage(AValue: Integer);
    procedure SetRecentFiles(const Value: TStringList);
    procedure SetRecentFolders(AValue: TStringList);
    procedure SetRecentProjects(const Value: TStringList);
    procedure SetProjects(const Value: TStringList);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Apply; virtual;
    procedure Load(vWorkspace: string);
    procedure Save(vWorkspace: string);
    procedure OptionsShow;
    procedure ColorsShow;

    property BoundRect: TRect read FBoundRect write FBoundRect; //not saved yet
    property RecentFiles: TStringList read FRecentFiles write SetRecentFiles;
    property RecentFolders: TStringList read FRecentFolders write SetRecentFolders;
    property RecentProjects: TStringList read FRecentProjects write SetRecentProjects;
    property Projects: TStringList read FProjects write SetProjects;

    property Profile: TEditorProfile read FProfile;
    property SearchHistory: TStringList read FSearchHistory;
    property ReplaceHistory: TStringList read FReplaceHistory;
    property SearchFolderHistory: TStringList read FSearchFolderHistory;
  published
    property AutoOpenProject: Boolean read FAutoOpenProject write FAutoOpenProject;
    property LastProject: string read FLastProject write FLastProject;
    property LastFolder: string read FLastFolder write FLastFolder;
    property ExtraExtensions: TStringList read FExtraExtensions write FExtraExtensions;
    property IgnoreNames: string read FIgnoreNames write FIgnoreNames;
    property CollectAutoComplete: Boolean read FCollectAutoComplete write FCollectAutoComplete default False;
    property CollectTimeout: QWORD read FCollectTimeout write FCollectTimeout default 60;
    property ShowFolder: Boolean read FShowFolder write FShowFolder default True;
    property ShowFolderFiles: TShowFolderFiles read FShowFolderFiles write FShowFolderFiles default sffRelated;
    property SortFolderFiles: TSortFolderFiles read FSortFolderFiles write FSortFolderFiles default srtfByNames;
    property ShowMessages: Boolean read FShowMessages write FShowMessages default False;
    property ShowToolbar: Boolean read FShowToolbar write FShowToolbar default True;
    property MessagesHeight: integer read FMessagesHeight write FMessagesHeight default 100;
    property FoldersPanelWidth: integer read FFoldersPanelWidth write FFoldersPanelWidth default 180;
    property AutoStartDebugServer: Boolean read FAutoStartDebugServer write FAutoStartDebugServer default False;
    property FallbackToText: Boolean read FFallbackToText write FFallbackToText default False;
    property AnsiCodePage: Integer read GetAnsiCodePage write SetAnsiCodePage;
    property WindowMaxmized: Boolean read FWindowMaxmized write FWindowMaxmized default False;
    property WindowTop: Integer read FBoundRect.Top write FBoundRect.Top;
    property WindowLeft: Integer read FBoundRect.Left write FBoundRect.Left;
    property WindowRight: Integer read FBoundRect.Right write FBoundRect.Right;
    property WindowBottom: Integer read FBoundRect.Bottom write FBoundRect.Bottom;
  end;

  TmneSynCompletion = class;

  TMap = class(TObject)
    Name: string;
    AttType: TAttributeType;
  end;

  { TMapper }

  TMapper = class(specialize TmnNamedObjectList<TMap>)
  private
  public
    function Add(Attribute: TSynHighlighterAttributes; AttType: TAttributeType): TMap;
  end;

  TFileCategoryKind = (fckPublish);
  TFileCategoryKinds = set of TFileCategoryKind;

  { TFileCategory }

  TFileCategory = class(TEditorElements)
  private
    FName: string;
    FHighlighter: TSynCustomHighlighter;
    FKind: TFileCategoryKinds;
    FMapper: TMapper;
    FTendency: TEditorTendency;
    FTitle: string;
    function GetHighlighter: TSynCustomHighlighter;
    function GetItem(Index: Integer): TFileGroup;
    function GetMapper: TMapper;
  protected
    FCompletion: TmneSynCompletion;
    function DoCreateHighlighter: TSynCustomHighlighter; virtual; abstract;
    procedure InitMappers; virtual; abstract;

    procedure InitCompletion(vSynEdit: TCustomSynEdit); virtual;
    procedure DoAddKeywords; virtual;
    procedure DoAddCompletion(AKeyword: string; AKind: integer); virtual;
    procedure DoExecuteCompletion(Sender: TObject); virtual; //TODO move it to CodeFileCategory

    procedure InitEdit(vSynEdit: TCustomSynEdit); virtual;
    function GetIsText: Boolean; virtual;
  public
    constructor Create(ATendency: TEditorTendency; const vName, vTitle: string; vKind: TFileCategoryKinds = []); virtual;
    constructor Create(ATendency: TEditorTendencyClass; const vName, vTitle: string; vKind: TFileCategoryKinds = []); virtual;
    destructor Destroy; override;
    procedure EnumMenuItems(AddItems: TAddClickCallBack); virtual;
    function CreateHighlighter: TSynCustomHighlighter; //todo replace with doCreate....
    procedure InitHighlighter;
    property Mapper:TMapper read GetMapper write FMapper;
    procedure Apply(AHighlighter: TSynCustomHighlighter; Attributes: TGlobalAttributes);
    property Name: string read FName write FName;
    property Title: string read FTitle write FTitle;
    function Find(vName: string): TFileGroup;
    procedure EnumExtensions(vExtensions: TStringList);
    function GetExtensions: string;
    function GetColorPrefix: string; virtual;
    function FormatColor(Color: TColor): string; virtual;
    function DeformatColor(Str: string): TColor; virtual;
    property Tendency: TEditorTendency read FTendency;
    property IsText: Boolean read GetIsText;
    property Highlighter: TSynCustomHighlighter read GetHighlighter;
    property Completion: TmneSynCompletion read FCompletion;
    property Kind: TFileCategoryKinds read FKind;
    property Items[Index: Integer]: TFileGroup read GetItem; default;
  end;

  TFileCategoryClass = class of TFileCategory;

  { TFileCategories }

  TFileCategories = class(specialize TmnNamedObjectList<TFileCategory>)
  public
    function FindByClass(CategoryClass: TFileCategoryClass): TFileCategory;
    function Add(vCategory: TFileCategory): Integer;
  end;

  { TTextFileCategory }

  TTextFileCategory = class(TFileCategory)
  protected
    function GetIsText: Boolean; override;
  public
  end;

  { TCodeFileCategory }

  TCodeFileCategory = class(TTextFileCategory)
  protected
    IdentifierID: Integer;
    {CommentID: Integer;
    StringID: Integer;} //TODO
    procedure ExtractKeywords(Files, Identifiers: TStringList); virtual;
    procedure DoExecuteCompletion(Sender: TObject); override;
  end;

  { TFileGroup }

  TFileGroup = class(TEditorElement)
  private
    FExtraExtensions: TEditorExtensions;
    FFileClass: TEditorFileClass;
    FExtensions: TEditorExtensions;
    FKind: TFileGroupKinds;
    FCategory: TFileCategory;
    FStyle: TFileGroupStyles;
    procedure SetCategory(AValue: TFileCategory);
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateEditorFile(vFiles: TEditorFiles): TEditorFile;
    procedure EnumExtensions(vExtensions: TStringList; Kind: TFileGroupKinds = []);
    procedure EnumExtensions(vExtensions: TEditorElements);
    property Category: TFileCategory read FCategory write SetCategory;
    property Extensions: TEditorExtensions read FExtensions;
    property ExtraExtensions: TEditorExtensions read FExtraExtensions;
    property Kind: TFileGroupKinds read FKind write FKind;
    property Style: TFileGroupStyles read FStyle write FStyle;
    property FileClass: TEditorFileClass read FFileClass;
  end;

  TFileGroupClass = class of TFileGroup;

  TCreateMaskProc = function(vGroup: TFileGroup): Boolean of object;

  { TFileGroups }

  TFileGroups = class(TEditorElements)
  private
    function GetItem(Index: integer): TFileGroup;
  protected
    procedure InternalAdd(GroupClass: TFileGroupClass; FileClass: TEditorFileClass; const Name, Title: string; Category: TFileCategoryClass; Extensions: array of string; Kind: TFileGroupKinds = []; Style: TFileGroupStyles = []);
  public
    function Find(vName: string): TFileGroup;
    function Find(vName, vCategory: string): TFileGroup;
    function IsExists(AGroup: TFileGroup): Boolean;
    procedure EnumExtensions(vExtensions: TStringList; Kind: TFileGroupKinds = []);
    procedure EnumExtensions(vExtensions: TEditorElements);
    function FindExtension(vExtension: string; vKind: TFileGroupKinds = []): TFileGroup;
    //FullFilter return title of that filter for open/save dialog boxes
    function CreateFilter(FullFilter: Boolean = True; FirstExtension: string = ''; vGroup: TFileGroup = nil; OnlyThisGroup: Boolean = true): string;
    function CreateMask(CreateMaskProc: TCreateMaskProc): string;
    procedure Add(vGroup: TFileGroup);
    procedure Add(FileClass: TEditorFileClass; const Name, Title: string; Category: TFileCategoryClass; Extensions: array of string; Kind: TFileGroupKinds = []; Style: TFileGroupStyles = []);
    property Items[Index: integer]: TFileGroup read GetItem; default;
  end;

  { TTendencies }

  TTendencies = class(TEditorElements)
  private
    function GetItem(Index: integer): TEditorTendency;
  public
    procedure Init;
    function Find(vName: string): TEditorTendency;
    function FindByClass(TendencyClass: TEditorTendencyClass): TEditorTendency;
    function Add(vEditorTendency: TEditorTendencyClass): TEditorTendency;
    procedure Add(vEditorTendency: TEditorTendency);
    property Items[Index: integer]: TEditorTendency read GetItem; default;
  end;

  { TSourceManagements }

  TSourceManagements = class(TEditorElements)
  private
    function GetItem(Index: integer): TEditorSCM;
  public
    function Find(vName: string): TEditorSCM;
    procedure Add(vEditorSCM: TEditorSCMClass);
    property Items[Index: Integer]: TEditorSCM read GetItem; default;
  end;

  { TEditorFormItem }

  TEditorFormItem = class(TObject)
  private
    FObjectClass: TClass;
    FItemClass: TCustomFormClass;
  protected
  public
    property ObjectClass: TClass read FObjectClass;
    property ItemClass: TCustomFormClass read FItemClass;
  end;

  { TEditorFormList }

  TEditorFormList = class(specialize TmnObjectList<TEditorFormItem>)
  private
  public
    function Find(ObjectClass: TClass): TEditorFormItem;
    procedure Add(vObjectClass: TClass; vFormClass: TCustomFormClass);
  end;

  TEditorSessionOptions = class(TmnXMLProfile)
  private
    FDefaultSCM: string;
  public
  published
    property DefaultSCM: string read FDefaultSCM write FDefaultSCM;
  end;

  {
    Session object to manage the current opened project, only one project can open.
  }

  { TEditorSession }

  TEditorSession = class(TObject)
  private
    FIsChanged: Boolean;
    FOptions: TEditorSessionOptions;
    FPanel: TControl;
    FProject: TEditorProject;
    FRun: TmneRun;
    FCachedIdentifiers: THashedStringList;
    FCachedVariables: THashedStringList;
    FCachedAge: QWord;
    function GetActive: Boolean;
    function GetMainFile: string;
    function GetMainFolder: string;
    procedure SetPanel(AValue: TControl);
    procedure SetProject(const Value: TEditorProject);
    procedure SetInternalProject(const Value: TEditorProject);
    procedure SetRun(AValue: TmneRun);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Changed;
    procedure Load(FileName: string);
    function New(Tendency: TEditorTendency = nil): TEditorProject;
    procedure Open;
    procedure Close;
    function Save(AProject: TEditorProject): Boolean;
    function SaveAs(AProject: TEditorProject): Boolean;
    function Save: Boolean;
    function SaveAs: Boolean;
    function GetRoot: string;
    //Is project opened
    property Active: Boolean read GetActive;
    //Current is the opened project, if it is a nil that mean there is no opened project.
    property Project: TEditorProject read FProject write SetProject;
    property Panel: TControl read FPanel write SetPanel;
    //Session Options is depend on the system used not shared between OSs
    property Options: TEditorSessionOptions read FOptions;
    property MainFolder: string read GetMainFolder;
    property MainFile: string read GetMainFile;
    property IsChanged: Boolean read FIsChanged;
    //Process the project running if it is null, process should nil it after finish
    property Run: TmneRun read FRun write SetRun;

    property CachedVariables: THashedStringList read FCachedVariables;
    property CachedIdentifiers: THashedStringList read FCachedIdentifiers;
    property CachedAge: QWord read FCachedAge write FCachedAge;
  end;

  TEditorMessagesList = class;

  TEditorMessage = class(TObject)
  private
    FText: string;
  public
    property Text: string read FText write FText;
  end;

  TEditorMessages = class(specialize TmnObjectList<TEditorMessage>)
  private
    FName: string;
  public
    function GetText(Index: integer): string;
    property Name: string read FName write FName;
  end;

  TEditorMessagesList = class(specialize TmnNamedObjectList<TEditorMessages>)
  private
  public
    function GetMessages(Name: string): TEditorMessages;
  end;

  TEditorNotifyList = specialize TFPGList<INotifyEngine>;

  TOnFoundEvent = procedure(FileName: string; const Line: string; LineNo, Column, FoundLength: integer) of object;
  TOnEditorChangeState = procedure(State: TEditorChangeStates) of object;

  TEngineCache = record
    MainFile: string;
    MainGroup: TFileGroup;
  end;

  { TEditorEngine }

  TEditorEngine = class(TObject)
  private
    FDebugLink: TEditorDebugLink;
    FDefaultProject: TDefaultProject;
    //FForms: TEditorFormList;
    FTendencies: TTendencies;
    FSourceManagements: TSourceManagements;
    FUpdateState: TEditorChangeStates;
    FUpdateCount: integer;
    FFiles: TEditorFiles;
    FFilePanel: TWinControl;
    FProjectPanel: TWinControl;
    FOptions: TEditorOptions;
    FSearchEngine: TSynEditSearch;
    FCategories: TFileCategories;
    FGroups: TFileGroups;
    FExtenstion: string;
    FSession: TEditorSession;
    FMessagesList: TEditorMessagesList;
    FBrowseFolder: string;
    FMacroRecorder: TSynMacroRecorder;
    FWorkSpace: string;
    //Extenstion Cache
    //FExtenstionCache: TExtenstionCache; //TODO
    FEnvironment: TStringList;
    FCache: TEngineCache;
    function GetSCM: TEditorSCM;
    function GetUpdating: Boolean;
    procedure SetBrowseFolder(const Value: string);
    function GetWorkSpace: string;
    function GetMainFileTendency: TEditorTendency;
    function GetCurrentTendency: TEditorTendency;
    function GetTendency: TEditorTendency;
  protected
    FSafeMode: Boolean;
    FInUpdateState: Integer;
    FNotifyObjects: TEditorNotifyList;

    property SearchEngine: TSynEditSearch read FSearchEngine;
    procedure InternalChangedState(State: TEditorChangeStates);
    procedure DoChangedState(State: TEditorChangeStates); virtual;
    procedure DoMacroRecorderChanged(Sender: TObject);
    procedure DoReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure RegisterNotify(NotifyEngine: INotifyEngine);
    procedure UnregisterNotify(NotifyEngine: INotifyEngine);

    procedure Startup(vSafeMode: Boolean = False);
    procedure OpenDefaultProject;
    procedure LoadOptions;
    procedure UpdateOptions;
    procedure SaveOptions;
    procedure Shutdown;
    function IsShutdown: Boolean;

    //I used it for search in files
    function SearchReplace(const FileName: string; const ALines: TStringList; const ASearch, AReplace: string; OnFoundEvent: TOnFoundEvent; AOptions: TSynSearchOptions): integer;
    //Recent
    procedure ProcessRecentFile(const FileName: string);
    procedure RemoveRecentFile(const FileName: string);
    procedure ProcessRecentFolder(const FileName: string);
    procedure RemoveRecentFolder(const FileName: string);
    procedure ProcessRecentProject(const FileName: string);
    procedure RemoveRecentProject(const FileName: string);

    procedure ProcessProject(const FileName: string);
    procedure RemoveProject(const FileName: string);

    procedure BeginUpdate;
    procedure UpdateState(State: TEditorChangeStates);
    property Updating: Boolean read GetUpdating;
    procedure EndUpdate;

    function EnvReplace(S: string; NoRoot: Boolean = False): string; //NoRoot root can be recucrly or not needed
    procedure EnvList(List: TStringList; NoRoot: Boolean = False);
    function ExpandFile(FileName: string): string;
    function GetRoot: string;
    property Extenstion: string read FExtenstion write FExtenstion;

    property WorkSpace: string read GetWorkSpace write FWorkSpace;

    //AddInstant: Create category and file group for highlighter
    //procedure AddInstant(vName: string; vExtensions: array of string; vHighlighterClass: TSynCustomHighlighterClass; vKind: TFileCategoryKinds);

    property Categories: TFileCategories read FCategories;
    property Groups: TFileGroups read FGroups;
    property Tendencies: TTendencies read FTendencies;
    property SourceManagements: TSourceManagements read FSourceManagements;
    //property Forms: TEditorFormList read FForms;
    //
    property Files: TEditorFiles read FFiles;
    property Session: TEditorSession read FSession;
    property Options: TEditorOptions read FOptions;
    property MessagesList: TEditorMessagesList read FMessagesList;
    //FilePanel is a panel or any wincontrol that the editor SynEdit put on it and so on
    property FilePanel: TWinControl read FFilePanel write FFilePanel;
    property ProjectPanel: TWinControl read FProjectPanel write FProjectPanel;
    //BrowseFolder: Current folder
    property BrowseFolder: string read FBrowseFolder write SetBrowseFolder;
    property DebugLink: TEditorDebugLink read FDebugLink;
    property Tendency: TEditorTendency read GetTendency; //It get Project/MainFile/File/Default tendency
    property CurrentTendency: TEditorTendency read GetCurrentTendency; //It get Debug/MainFile/File/Default tendency
    property DefaultProject: TDefaultProject read FDefaultProject; //used when there is no project action, it is assigned to project, but not active
    property SCM: TEditorSCM read GetSCM;
    function GetIsChanged: Boolean;
    property MacroRecorder: TSynMacroRecorder read FMacroRecorder;
    procedure SendLog(S: string);
    procedure SendMessage(S: string; vMessageType: TNotifyMessageType);
    procedure SendMessage(S: string; vMessageType: TNotifyMessageType; vError: TErrorInfo);
    procedure SendAction(EditorAction: TEditorAction);

    property Environment: TStringList read FEnvironment write FEnvironment;
  published
  end;

  { TmneSynCompletion }

  TmneSynCompletion = class(TSynCompletion)
  protected
    function OwnedByEditor: Boolean; override;
  public
  end;

  { TListFileSearcher }

  TListFileSearcher = class(TFileSearcher)
  protected
    procedure DoDirectoryFound; override;
    procedure DoFileFound; override;
  public
    List: TStringList;
  end;

function SelectFolder(const Caption: string; const Root: string; var Directory: string): Boolean;
procedure SpliteStr(S, Separator: string; var Name, Value: string);

procedure SaveAsUnix(Strings: TStrings; Stream: TStream);
procedure SaveAsWindows(Strings: TStrings; Stream: TStream);
procedure SaveAsMAC(Strings: TStrings; Stream: TStream);
procedure SaveAsMode(const FileName: string; Mode: TEditorLinesMode; Strings: Tstrings);

function ConvertToLinesMode(Mode: TEditorLinesMode; Contents: string): string;
function DetectLinesMode(const Contents: string): TEditorLinesMode;

function ConvertIndents(const Contents: string; TabWidth: integer; Options: TIndentMode = idntTabsToSpaces): string;

const
  sEnvVarChar = '?';

type
  //If set Resume to false it will stop loop
  TEnumFilesCallback = procedure(AObject: TObject; const FileName: string; Count, Level:Integer; IsDirectory: Boolean; var Resume: Boolean);

procedure EnumFiles(Folder, Filter: string; FileList: TStringList);
//EnumFileList return false if canceled by callback function
type
  TFileFindTypes = set of (fftDir, fftFile);

function EnumFileList(const Root, Masks, Ignore: string; Callback: TEnumFilesCallback; AObject: TObject; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean; Types: TFileFindTypes = [fftFile]): Boolean;
procedure EnumFileList(const Root, Masks, Ignore: string; Strings: TStringList; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean);
procedure EnumDirList(const Root, Masks, Ignore: string; Strings: TStringList; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean);

procedure EnumIndentMode(vItems: TStrings);

{$ifdef DEBUG}
procedure Nothing;
{$endif}

const
  cFallbackGroup = 'txt';

function IsShutdown: Boolean;
function Engine: TEditorEngine;

const
{$ifdef WINDOWS}
  SysPlatform = 'windows';
{$else}
  SysPlatform = 'linux';
{$endif}

implementation

uses
  SynHighlighterHashEntries, SynGutterCodeFolding,
  Registry, SearchForms, SynEditTextBuffer, GotoForms, mneClasses,
  mneResources, MsgBox, GUIMsgBox;

type
  TEngineLife  = (engnNone, engnStarting, engnStarted, engnShutdowning, engnShutdowned);

var
  FEngineLife: TEngineLife  = engnNone;
  FEngine: TEditorEngine = nil;

function IsShutdown: Boolean;
begin
  if FEngine <> nil then
    Result := FEngine.IsShutdown
  else
    Result := True;
end;

function Engine: TEditorEngine;
begin
  if FEngineLife = engnShutdowned  then
    raise Exception.Create('Engine in shutdown?');
  if FEngine = nil then
    FEngine := TEditorEngine.Create;
  Result := FEngine;
end;

function SelectFolder(const Caption: string; const Root: string; var Directory: string): Boolean;
begin
  Result := SelectDirectory(Caption, Root, Directory);
end;

procedure SpliteStr(S, Separator: string; var Name, Value: string);
var
  p: integer;
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
  i, l: integer;
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
  i, l: integer;
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
  i, l: integer;
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

{ TmnSynEdit }

procedure TmnSynEdit.ExecuteCommand(Command: TSynEditorCommand; const AChar: TUTF8Char; Data: pointer);
begin
  if command = ecLowerCaseBlock then
  begin
    if SelAvail then
      FBlockSelection.SelText := LowerCase(FBlockSelection.SelText);
  end
  else if command = ecUpperCaseBlock then
  begin
    if SelAvail then
      FBlockSelection.SelText := UpperCase(FBlockSelection.SelText);
  end
  else
    inherited ExecuteCommand(Command, AChar, Data);
end;

{ TFileCategories }

function TFileCategories.FindByClass(CategoryClass: TFileCategoryClass): TFileCategory;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].ClassType = CategoryClass then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TFileCategories.Add(vCategory: TFileCategory): Integer;
begin
  if FindByClass(TFileCategoryClass(vCategory.ClassType)) <> nil then
    raise Exception.Create('Category is already exists: ' + vCategory.ClassName);
  Result := inherited Add(vCategory);
end;

{ TEditorExtensions }

procedure TEditorExtensions.Add(Name: string; ImageIndex: Integer);
var
  aItem: TEditorExtension;
begin
  aItem := TEditorExtension.Create;
  aItem.Name := Name;
  aItem.ImageIndex := ImageIndex;
  inherited Add(aItem);
end;

{ GXMLItems }

function GXMLItems.GetItem(Index: Integer): _Object_;
begin
  Result := _Object_(inherited Items[Index]);
end;

procedure GXMLItems.SetItem(Index: Integer; const Value: _Object_);
begin
  Items[Index] := Value;
end;

function GXMLItems.DoCreateItem(AClass: TmnXMLItemClass): TmnXMLItem;
begin
  Result := _Object_.Create;
end;

{ TmneWatches }

procedure TmneWatches.Add(AName: string);
var
  Item: TmneWatch;
begin
  Item := TmneWatch.Create;
  Item.Name := AName;
  inherited Add(Item);
end;

{ TmneBreakpoints }

procedure TmneBreakpoints.Add(AFileName: string; ALineNo: Integer);
var
  Item: TmneBreakpoint;
begin
  Item := TmneBreakpoint.Create;
  Item.FileName := AFileName;
  Item.LineNo := ALineNo;
  inherited Add(Item);
end;

{ TCodeFileCategory }

procedure TCodeFileCategory.ExtractKeywords(Files, Identifiers: TStringList);
var
  aFile: TStringList;
  s: string;
  i, f: integer;
  aHighlighter: TSynCustomHighlighter;
begin
  aHighlighter := CreateHighlighter;
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
          if (aHighlighter.GetTokenKind = IdentifierID) then
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

procedure TCodeFileCategory.DoExecuteCompletion(Sender: TObject);
var
  aIdentifiers: THashedStringList;
  aCurrent, Token: string;
  i, r: integer;
  aLine: Integer;
  aSynEdit: TCustomSynEdit;
  aFiles: TStringList;
begin
  inherited;
  Screen.Cursor := crHourGlass;
  Completion.ItemList.BeginUpdate;
  try
    Completion.ItemList.Clear;
    DoAddKeywords; //TODO check timeout before refill it for speeding

    aSynEdit := (Sender as TSynCompletion).TheForm.CurrentEditor as TCustomSynEdit;
    if (aSynEdit <> nil) and (Highlighter <> nil) then
    begin
      aLine := aSynEdit.CaretY;
      aCurrent := aSynEdit.GetWordAtRowCol(aSynEdit.LogicalCaretXY);

      Completion.TheForm.Font.Size := aSynEdit.Font.Size;
      Completion.TheForm.Font.Color := aSynEdit.Font.Color;
      Completion.TheForm.Color := aSynEdit.Color;
      Completion.TheForm.Caption := Name;
      Completion.SelectedColor := aSynEdit.SelectedColor.Background;
      Completion.TheForm.BackgroundColor := aSynEdit.Color;
      Completion.TheForm.TextColor := aSynEdit.Font.Color;
      Completion.TheForm.DrawBorderColor := aSynEdit.Font.Color;

      Completion.AutoUseSingleIdent := True;

      //load keyowrds
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
            EnumFileList(Engine.Session.GetRoot, GetExtensions, Engine.Options.IgnoreNames, aFiles, 1000, 3, Engine.Session.Active);//TODO check the root dir if no project opened
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
            Token := Highlighter.GetToken;
            if (i <> aLine - 1) or (aCurrent <> Token) then
            begin
              if (Highlighter.GetTokenKind = IdentifierID) and (aIdentifiers.IndexOf(Token) < 0) then
                aIdentifiers.Add(Token);
            end;
            Highlighter.Next;
          end;
        end;

        for i := 0 to aIdentifiers.Count - 1 do
          DoAddCompletion(aIdentifiers[i], IdentifierID);
      finally
        aIdentifiers.Free;
      end;
    end;
    (Completion.ItemList as TStringList).Sort;
  finally
    Completion.ItemList.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

{ TControlEditorFile }

procedure TControlEditorFile.SetControl(AValue: TWinControl);
begin
  if FControl <> AValue then
  begin
    if FControl <> nil then
      FControl.Free;
    FControl := AValue;
    FControl.Align := alClient;
    FControl.Parent := Engine.FilePanel;
    //FControl.Visible := True;
  end;
end;

function TControlEditorFile.GetControl: TWinControl;
begin
  Result := FControl;
end;

function TControlEditorFile.GetIsReadonly: Boolean;
begin
  Result := True;
end;

procedure TControlEditorFile.DoLoad(FileName: string);
begin
end;

procedure TControlEditorFile.DoSave(FileName: string);
begin
end;

destructor TControlEditorFile.Destroy;
begin
  FreeAndNil(FControl);
  inherited Destroy;
end;

{ TEditorProjectOptions }

constructor TEditorProjectOptions.Create;
begin
  inherited Create;
  FTabWidth := 4;
  FIndentMode := idntTabsToSpaces;
end;

destructor TEditorProjectOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TEditorProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
begin
end;

procedure TEditorProjectOptions.CreateProjectPanel(AOwner: TComponent; AProject: TEditorProject; var AFrame: TFrame);
begin
end;

{ TRunProjectOptions }

procedure TRunProjectOptions.SetPaths(AValue: TStrings);
begin
  FPaths.Assign(AValue);
end;

procedure TRunProjectOptions.Merge(AOptions: TRunProjectOptions);
  function iif(v, s: string): string;
  begin
    if s <> '' then
      Result := s
    else
      Result := v;
  end;

  function iif(v, s: boolean): boolean;
  begin
    if s then
      Result := s
    else
      Result := v;
  end;

  function iif(v, s: TThreeStates): TThreeStates;
  begin
    if s = stateNone then
      Result := v
    else
      Result := s;
  end;

begin
  FInfo.Command := iif(FInfo.Command, AOptions.Command);
  FInfo.Params := iif(FInfo.Params, AOptions.Params);
  FInfo.Pause := iif(FInfo.Pause, AOptions.Pause);
  FInfo.Console := iif(FInfo.Pause, AOptions.Console);
  FInfo.MainFile := iif(FInfo.MainFile, AOptions.MainFile);
  FInfo.OutputFile := iif(FInfo.OutputFile, AOptions.OutputFile);
  FInfo.Require := iif(FInfo.Require, AOptions.Require);
  FInfo.ConfigFile := iif(FInfo.ConfigFile, AOptions.ConfigFile);
  FInfo.ExpandPaths := iif(FInfo.ExpandPaths, AOptions.ExpandPaths);
  FPaths.AddStrings(AOptions.Paths);
end;

procedure TRunProjectOptions.Copy(AOptions: TRunProjectOptions);
begin
  FInfo := AOptions.FInfo;
  FPaths.Assign(AOptions.Paths);
end;

procedure TRunProjectOptions.Assign(Source: TPersistent);
begin
  if Source is TRunProjectOptions then
    Copy(Source as TRunProjectOptions)
  else
    inherited Assign(Source);
end;

constructor TRunProjectOptions.Create;
begin
  inherited;
  FPaths := TStringList.Create;
  FInfo.Pause := stateTrue;
  FInfo.Console := stateTrue;
end;

destructor TRunProjectOptions.Destroy;
begin
  FreeAndNil(FPaths);
  inherited Destroy;
end;


{ TTextFileCategory }

function TTextFileCategory.GetIsText: Boolean;
begin
  Result := True;
end;

{ TMapper }

function TMapper.Add(Attribute: TSynHighlighterAttributes; AttType: TAttributeType): TMap;
begin
  Result := TMap.Create;
  Result.Name := Attribute.StoredName;
  Result.AttType := AttType;
  inherited Add(Result); //if there is a bug inside add, you need to fix it by code, so no need to catch it
end;

{ TTextEditorFile }

procedure TTextEditorFile.SetHighlightLine(AValue: Integer);
begin
  if FHighlightLine <> AValue then
  begin
    SynEdit.InvalidateLine(FHighlightLine);
    FHighlightLine :=AValue;
    SynEdit.InvalidateLine(FHighlightLine);
  end;
end;

function TTextEditorFile.GetIsReadonly: Boolean;
begin
  Result := SynEdit.ReadOnly;
end;

procedure TTextEditorFile.SetIsReadonly(const Value: Boolean);
begin
  SynEdit.ReadOnly := Value;
end;

function TTextEditorFile.GetControl: TWinControl;
begin
  Result := SynEdit;
end;

procedure TTextEditorFile.DoLoad(FileName: string);
var
  Contents: string;
  Size: integer;
  Stream: TFileStream;
  IndentMode: TIndentMode;
  Encoded: Boolean;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    SynEdit.BeginUpdate;
    try
      Size := Stream.Size - Stream.Position;
      SetString(Contents, nil, Size);
      Stream.Read(Pointer(Contents)^, Size);
      FileEncoding := GuessEncoding(Contents);
      if not SameText(FileEncoding, EncodingUTF8) then
      begin
        if SameText(FileEncoding, 'ucs2le') or SameText(FileEncoding, 'ucs2be') then //Because ConvertEncodingToUTF8 not removes BOM
          Contents := System.Copy(Contents, 3, MaxInt);
        Contents := ConvertEncodingToUTF8(Contents, FileEncoding, Encoded);
      end;
      LinesMode := DetectLinesMode(Contents);
      IndentMode := Engine.Options.Profile.IndentMode;
      if Tendency.OverrideEditorOptions then
        IndentMode := Tendency.IndentMode;
      if IndentMode > idntNone then
        Contents := ConvertIndents(Contents, SynEdit.TabWidth, IndentMode);
      if IsNew then //there is no undo here
        SynEdit.Lines.Text := Contents
      else
      begin
        SynEdit.BeginUndoBlock;  //adding it to history of undo, so we can undo the revert to changes in by external
        try
          SynEdit.TextBetweenPoints[Point(1, 1), Point(length(SynEdit.Lines[SynEdit.Lines.Count - 1]) + 1, SynEdit.Lines.Count)] := Contents;
          //SynEdit.Text := Contents;
        finally
          SynEdit.EndUndoBlock;
        end;
      end;

    finally
      SynEdit.EndUpdate;
      Stream.Free;
    end;
  finally
  end;
end;

procedure TTextEditorFile.DoSave(FileName: string);
var
  aStream : TFileStream;
  Contents: rawbytestring;
  IndentMode: TIndentMode;
begin
  IndentMode := Engine.Options.Profile.IndentMode;
  if Tendency.OverrideEditorOptions then
    IndentMode := Tendency.IndentMode;

  if IndentMode > idntNone then
    Contents := ConvertIndents(SynEdit.Lines.Text, SynEdit.TabWidth, IndentMode)
  else
    Contents := SynEdit.Lines.Text;

  Contents := ConvertToLinesMode(LinesMode, Contents);

  if not SameText(FileEncoding, EncodingUTF8) then
  begin
    Contents := ConvertEncoding(Contents, EncodingUTF8, FileEncoding, false);
    if FileEncoding = EncodingUTF8 then
      Contents := UTF8BOM + Contents
    else if FileEncoding = EncodingUCS2LE then
      Contents := UTF16LEBOM + Contents
    else if FileEncoding = EncodingUCS2BE then
      Contents := UTF16BEBOM + Contents;
  end;

  aStream := TFileStream.Create(FileName, fmCreate);
  try
    aStream.WriteBuffer(Pointer(Contents)^, Length(Contents));
  finally
    aStream.Free;
  end;
end;

procedure TTextEditorFile.GroupChanged;
begin
  if Group <> nil then
    FSynEdit.Highlighter := FGroup.Category.Highlighter;
  inherited;
  if Group <> nil then
  begin
    FGroup.Category.InitCompletion(FSynEdit);

    //FSynEdit.Gutter.SeparatorPart(0).Index := FSynEdit.Gutter.Parts.Count - 1;//To make this part last one

    //if (fgkExecutable in FGroup.Kind) then //show it make it more comfirtable
      with TSynDebugMarksPart.Create(FSynEdit.Gutter.Parts) do
      begin
        FEditorFile := Self;
        AutoSize := False;
        Width := EditorResource.DebugImages.Width + DEBUG_IMAGE_MARGINES;
      end;

    FGroup.Category.InitEdit(FSynEdit);
  end;
end;

procedure TTextEditorFile.DoEdit(Sender: TObject);
begin
  inherited;
  HighlightLine := -1;
end;

procedure TTextEditorFile.DoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if ([scReadOnly, scCaretX, scCaretY, scLeftChar, scTopLine, scSelection] * Changes) <> [] then
    HighlightLine := -1;
  {if ([scCaretY] * Changes) <> [] then
    (Sender as TSynEdit).InvalidateLine((Sender as TSynEdit).CaretX);}
  inherited;
end;

procedure TTextEditorFile.DoGutterClickEvent(Sender: TObject; X, Y, Line: integer; Mark: TSynEditMark);
var
  aLine: integer;
begin
  if (Tendency.Debug <> nil) and (fgkExecutable in Group.Kind) then
  begin
    aLine := SynEdit.PixelsToRowColumn(Point(X, Y)).y;
    Tendency.Debug.Lock;
    try
      Tendency.Debug.Breakpoints.Toggle(Name, aLine);
    finally
      Tendency.Debug.Unlock;
    end;
    SynEdit.InvalidateLine(aLine);
  end;
end;

procedure TTextEditorFile.DoSpecialLineMarkup(Sender: TObject; Line: integer; var Special: Boolean; Markup: TSynSelectedColor);
var
  aColor: TColor;
begin
  if (Engine.DebugLink.ExecutedControl = Sender) then
  begin
    if Engine.DebugLink.ExecutedLine = Line then
    begin
      Special := True;
      aColor := Engine.Options.Profile.Attributes.Default.Background;
      Markup.Background := MixColors(aColor,  OppositeColor(Engine.Options.Profile.Attributes.Default.Background), 200);
      Markup.Foreground := Engine.Options.Profile.Attributes.Default.Foreground;
    end;
  end
  else if FHighlightLine = Line then
  begin
    Special := True;
    //aColor := Engine.Options.Profile.Attributes.Default.Background;
    //Markup.BackAlpha := 100;
    Markup.Foreground := Engine.Options.Profile.Attributes.Active.Foreground;
    Markup.Background := Engine.Options.Profile.Attributes.Active.Background;
  end
  {else if  (Sender as TSynEdit).CaretY = Line then
  begin
    Special := True;
    Markup.BackAlpha := 100;
    Markup.Background := Engine.Options.Profile.Attributes.Active.Background;
  end;}
end;

procedure TTextEditorFile.DoGetCapability(var vCapability: TEditCapability);
begin
  inherited;

  if SynEdit.SelAvail then
    vCapability := vCapability + [ecpAllowCopy];

  if SynEdit.CanPaste then
    vCapability := vCapability + [ecpAllowPaste];
end;

procedure TTextEditorFile.SynEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [ssCtrl] then
  begin
    if (Key = VK_INSERT) then
    begin
      if not SynEdit.SelAvail then
      begin
        SynEdit.SelectWord;
        SynEdit.CopyToClipboard;
      end;
    end;
  end;
end;

constructor TTextEditorFile.Create(ACollection: TCollection);
begin
  inherited;
  { There is more assigns in TEditorFile.SetGroup and TEditorProfile.Assign}
  FHighlightLine := -1;
  FSynEdit := TmnSynEdit.Create(Engine.FilePanel);
  FSynEdit.OnChange := @DoEdit;
  FSynEdit.OnStatusChange := @DoStatusChange;
  FSynEdit.OnGutterClick := @DoGutterClickEvent;
  FSynEdit.OnSpecialLineMarkup := @DoSpecialLineMarkup;
  FSynEdit.BookMarkOptions.BookmarkImages := EditorResource.BookmarkImages;
  FSynEdit.OnReplaceText := @Engine.DoReplaceText;
  FSynEdit.OnKeyDown := @SynEditKeyDown;

  FSynEdit.TrimSpaceType := settLeaveLine;
  FSynEdit.BoundsRect := Engine.FilePanel.ClientRect;
  FSynEdit.Font.Quality := fqDefault;
  FSynEdit.BorderStyle := bsNone;
  FSynEdit.ShowHint := True;
  FSynEdit.Visible := False;
  FSynEdit.WantTabs := True;
  FSynEdit.MaxLeftChar := 80;
  FSynEdit.ScrollBars := ssAutoBoth;

  FSynEdit.Parent := Engine.FilePanel;
  with FSynEdit.Keystrokes.Add do
  begin
    Key       := VK_DELETE;
    Shift     := [ssCtrl];
    Command   := ecDeleteWord;
  end;

  with FSynEdit.Keystrokes.Add do
  begin
    Key       := VK_K;
    Shift     := [ssCtrl];
    Key2       := VK_O;
    Shift2     := [ssCtrl];
    Command   := ecLowerCaseBlock;
  end;

  with FSynEdit.Keystrokes.Add do
  begin
    Key       := VK_K;
    Shift     := [ssCtrl];
    Key2       := VK_P;
    Shift2     := [ssCtrl];
    Command   := ecUpperCaseBlock;
  end;

  Engine.MacroRecorder.AddEditor(FSynEdit);
end;

destructor TTextEditorFile.Destroy;
begin
  Engine.MacroRecorder.RemoveEditor(FSynEdit);
  FSynEdit.Free;
  inherited;
end;

procedure TTextEditorFile.Assign(Source: TPersistent);
var
  aProfile: TEditorProfile;
begin
  if Source is TEditorProfile then
  begin
    aProfile := Source as TEditorProfile;

    aProfile.AssignTo(SynEdit);

    if (Engine.Session.Active) and Engine.Session.Project.Options.OverrideEditorOptions then
    begin
      SynEdit.TabWidth := Engine.Session.Project.Options.TabWidth;
      SynEdit.BlockIndent := Engine.Session.Project.Options.TabWidth;
    end
    else if (Tendency <> nil) and Tendency.OverrideEditorOptions then
    begin
      //SynEdit.Options := SynEdit.Options - cSynOverridedOptions + Tendency.EditorOptions;
      SynEdit.TabWidth := Tendency.TabWidth;
      SynEdit.BlockIndent := Tendency.TabWidth;
    end;

    if (Group <> nil) and (Group.Category.Highlighter <> nil) then
      Group.Category.Apply(Group.Category.Highlighter, aProfile.Attributes);
  end
  else if (Source is TEditorDesktopFile) then
  begin
    with (Source as TEditorDesktopFile) do
    begin
//      SynEdit.CaretX := CaretX;
      SynEdit.CaretY := CaretY;
      SynEdit.TopLine := TopLine;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TTextEditorFile.AssignTo(Dest: TPersistent);
begin
  if (Dest is TEditorDesktopFile) then
  begin
    with (Dest as TEditorDesktopFile) do
    begin
      CaretX := SynEdit.CaretX;
      CaretY := SynEdit.CaretY;
      TopLine := SynEdit.TopLine;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TTextEditorFile.Find;
begin
  inherited;
  ShowSearchForm(SynEdit, Engine.Options.SearchHistory, Engine.Options.ReplaceHistory, False);
end;

procedure TTextEditorFile.FindNext;
begin
  inherited;
  SearchTextNext(SynEdit);
end;

procedure TTextEditorFile.FindPrevious;
begin
  inherited;
  SearchTextPrevious(SynEdit);
end;

procedure TTextEditorFile.Replace;
begin
  inherited;
  ShowSearchForm(SynEdit, Engine.Options.SearchHistory, Engine.Options.ReplaceHistory, True);
end;

procedure TTextEditorFile.Refresh;
begin
  inherited;
  SynEdit.Refresh;
end;

function TTextEditorFile.GetHint(HintControl: TControl; CursorPos: TPoint; out vHint: string): Boolean;
var
  v, s, t: string;
begin
  if capEval in Tendency.Capabilities then
  begin
    Result := EvalByMouse(CursorPos, v, s, t);
    if Result then
      vHint := v + ':' + t + '=' + #13#10 + s;
  end
  else
    Result := False;
end;

function TTextEditorFile.GetGlance: string;
var
  r: Integer;
begin
  Result := IntToStr(SynEdit.CaretY) + ':' + IntToStr(SynEdit.CaretX);
  if SynEdit.SelAvail then
  begin
    r := SynEdit.BlockEnd.y - SynEdit.BlockBegin.y + 1;
    Result := Result + ' [' + IntToStr(r);
    r := Length(SynEdit.SelText);
    Result := Result + ',' + IntToStr(r) + ']';
  end;
end;

function TTextEditorFile.EvalByMouse(p: TPoint; out v, s, t: string): boolean;
var
  l: variant;
begin
  if Tendency.Debug <> nil then
  begin
    if SynEdit.SelAvail then
      v := SynEdit.SelText
    else
      v := Trim(SynEdit.GetWordAtRowCol(SynEdit.LogicalCaretXY));
    Result := (v <> '') and Tendency.Debug.Watches.GetValue(v, l, t, False);
    s := VarToStrDef(l, '');
  end
  else
    Result := False;
end;

function TTextEditorFile.EvalByCursor(out v, s, t: string): boolean;
var
  l: variant;
begin
  if Tendency.Debug <> nil then
  begin
    if not SynEdit.SelAvail then
      v := Trim(SynEdit.GetWordAtRowCol(SynEdit.LogicalCaretXY))
    else
      v := SynEdit.SelText;
    Result := (v <> '') and Tendency.Debug.Watches.GetValue(v, l, t, False);
    s := VarToStrDef(l, '');
  end
  else
    Result := False;
end;

procedure TTextEditorFile.UpdateAge;
begin
  inherited;
  if SynEdit <> nil then
  begin
    SynEdit.Modified := False;
    SynEdit.MarkTextAsSaved;
  end;
end;

function TTextEditorFile.GetLanguageName: string;
begin
  if (SynEdit <> nil) and (SynEdit.Highlighter <> nil) then
    Result := SynEdit.Highlighter.GetLanguageName
  else
    Result := inherited;
end;

procedure TTextEditorFile.Copy;
begin
  SynEdit.CopyToClipboard
end;

procedure TTextEditorFile.Paste;
begin
  SynEdit.PasteFromClipboard;
end;

procedure TTextEditorFile.Cut;
begin
  SynEdit.CutToClipboard;
end;

procedure TTextEditorFile.SelectAll;
begin
  SynEdit.SelectAll;
end;

procedure TTextEditorFile.SetLine(Line: Integer);
begin
  SynEdit.CaretY := Line;
  SynEdit.CaretX := 1;
end;

procedure TTextEditorFile.GotoLine;
begin
  with TGotoLineForm.Create(Application) do
  begin
    NumberEdit.Text := IntToStr(LastGotoLine);
    if ShowModal = mrOk then
    begin
      if NumberEdit.Text <> '' then
      begin
        LastGotoLine := StrToIntDef(NumberEdit.Text, 0);
        SynEdit.CaretXY := Point(0, LastGotoLine);
      end;
    end;
    Free;
  end;
end;

{ TmneSynCompletion }

function TmneSynCompletion.OwnedByEditor: Boolean;
begin
  Result := False;
end;

{ TEditorSCM }

constructor TEditorSCM.Create;
begin
  inherited Create;
end;

{ TSourceManagements }

function TSourceManagements.GetItem(Index: integer): TEditorSCM;
begin
  Result := inherited Items[Index] as TEditorSCM;
end;

function TSourceManagements.Find(vName: string): TEditorSCM;
begin
  Result := inherited Find(vName) as TEditorSCM;
end;

procedure TSourceManagements.Add(vEditorSCM: TEditorSCMClass);
var
  aItem: TEditorSCM;
begin
  RegisterClass(vEditorSCM);
  aItem := vEditorSCM.Create;
  inherited Add(aItem);
end;

{ TDefaultTendency }

function TDefaultTendency.GetIsDefault: Boolean;
begin
  Result := True;
end;

procedure TDefaultTendency.Init;
begin
  inherited;
end;

procedure TDefaultTendency.Created;
begin
  FTitle := 'Default';
  FName := 'Default';
  FDescription := 'Default project type';
end;

{ TEditorFormList }

function TEditorFormList.Find(ObjectClass: TClass): TEditorFormItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if ObjectClass.InheritsFrom(Items[i].ObjectClass) then
    begin
      Result := Items[i] as TEditorFormItem;
      break;
    end;
  end;
end;

procedure TEditorFormList.Add(vObjectClass: TClass; vFormClass: TCustomFormClass);
var
  aItem: TEditorFormItem;
begin
  aItem := TEditorFormItem.Create;
  aItem.FObjectClass := vObjectClass;
  aItem.FItemClass := vFormClass;
  inherited Add(aItem);
end;

{ TEditorTendency }

procedure TEditorTendency.DoRun(Info: TmneRunInfo);
begin
end;

procedure TEditorTendency.SendMessage(S: string; vMessageType: TNotifyMessageType);
begin
  Engine.SendMessage(S, vMessageType);
end;

procedure TEditorTendency.Prepare;
begin
  if not IsPrepared then
  begin
    Debug := CreateDebugger;
    if (Debug <> nil) and Engine.Options.AutoStartDebugServer then
      Debug.Active := True;
    IsPrepared := True;
  end;
end;

procedure TEditorTendency.Unprepare;
begin
  if Debug <> nil then
  begin
    Debug.Action(dbaResume);
    Debug.Stop;
  end;
  FreeAndNil(FDebug);
end;

function TEditorTendency.GetIsDefault: Boolean;
begin
  Result := False;
end;

procedure TEditorTendency.AddGroup(vName, vCategory: string);
var
  G: TFileGroup;
  C: TFileCategory;
begin
  if vCategory = '' then
    C := nil
  else
    C := Engine.Categories.Find(vName);
  if C = nil then
    G := Engine.Groups.Find(vName)
  else
    G := C.Find(vName);

  if G = nil then
    raise Exception.Create(vName + ' file group not found');
  Groups.Add(G);
end;

function TEditorTendency.CreateDebugger: TEditorDebugger;
begin
  Result := nil;
end;

procedure TEditorTendency.Init;
begin

end;

procedure TEditorTendency.Created;
begin
end;

constructor TEditorTendency.Create;
begin
  inherited;
  FGroups := TFileGroups.Create(False);//it already owned by Engine.Groups
  FRunOptions := TRunProjectOptions.Create;
  FTabWidth := 4;
  FIndentMode := idntTabsToSpaces;
  Created;
end;

destructor TEditorTendency.Destroy;
begin
  FreeAndNil(FRunOptions);
  FreeAndNil(FGroups);
  inherited;
end;

procedure TEditorTendency.HelpKeyword(AWord: string);
begin
end;

procedure TEditorTendency.Run(RunActions: TmneRunActions); //please check dublicate in  M:\home\pascal\projects\miniEdit\source\extends\commons\mneCustomClasses.pas#DoRun
var
  p: TmneRunInfo;
  AOptions: TRunProjectOptions;
begin
  AOptions := TRunProjectOptions.Create;//Default options
  try
    AOptions.Copy(RunOptions);
    if (Engine.Session.Active) then
      AOptions.Merge(Engine.Session.Project.RunOptions);

    p.Actions := RunActions;
    if (Debug <> nil) and (Debug.Running) then
    begin
      if rnaKill in RunActions then
        Engine.CurrentTendency.Debug.Action(dbaReset)
      else if Engine.Session.Run.Active then
        Engine.Session.Run.Show;

      if rnaExecute in RunActions then
      begin
        if rnaCompile in RunActions then
          Debug.Action(dbaRun)
        else
          Debug.Action(dbaResume);
      end;
    end
    else
    begin
      if Engine.Session.Run.Active then
      begin
        //Engine.Log('Already run'); //TODO
        if rnaKill in RunActions then
          Engine.Session.Run.Stop
        else
          Engine.Session.Run.Show;
      end
      else
      begin
        if Debug.Active then
          p.Actions := p.Actions + [rnaDebug];
        if rnaCompile in RunActions then
          Engine.SendAction(eaClearOutput);
        p.Root := Engine.Session.GetRoot;
        p.Command := Engine.EnvReplace(AOptions.Command);

        p.Pause := AOptions.Pause in [stateTrue];
        p.Console := AOptions.Console in [stateTrue, stateNone];

        if Engine.Session = nil then
          raise Exception.Create('No Session active!');
        if Engine.Session.Project = nil then
          raise Exception.Create('No Project opened!');
        p.MainFile := Engine.ExpandFile(Engine.Session.Project.RunOptions.MainFile); //TODO: here need to care about expand file to be similar to env variable

        if (p.MainFile = '') and (Engine.Files.Current <> nil) and (fgkExecutable in Engine.Files.Current.Group.Kind) then
          p.MainFile := Engine.Files.Current.Name;

        if (p.Root = '') then
          p.Root := ExtractFileDir(p.MainFile);

        p.OutputFile := Engine.ExpandFile(Engine.Session.Project.RunOptions.OutputFile);
        if p.OutputFile = '' then
          p.OutputFile := Engine.ExpandFile(RunOptions.OutputFile); //not sure about it i am thinking to remove it, but ok right now, but it will cuz a bug in the future, nevermind...

        if p.OutputFile = '' then
        begin
          p.OutputFile := ExtractFileNameWithoutExt(p.MainFile);
          p.OutputFile := p.OutputFile + OutputExtension;
        end
        else if ExtractFileExt(p.OutputFile) = '' then //usefull to make exe name os undepended
            p.OutputFile := p.OutputFile + OutputExtension;

        p.RunFile := p.OutputFile;
        if ExtractFilePath(p.RunFile) = '' then
          p.RunFile := p.Root + p.RunFile;

        if (p.MainFile <> '') then
        begin
          DoRun(p);
          Engine.UpdateState([ecsDebug]);
        end;
      end;
    end;
  finally
    FreeAndNil(AOptions)
  end
end;

procedure TEditorTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
begin
end;

function TEditorTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TEditorProjectOptions.Create;
end;

procedure TEditorTendency.EnumRunCommands(Items: TStrings);
begin

end;

function TEditorTendency.GetDefaultGroup: TFileGroup;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Groups.Count -1 do
  begin
    if Groups[i].Category.Tendency = Self then
    begin
      Result := Groups[i];
      break;
    end;
  end;

  if Result = nil then
  begin
    if Groups.Count > 0 then
      Result := Groups[0]
    else
      Result := Engine.Groups[0];//first group in all groups, naah //TODO wrong wrong
  end;
end;

{ TTendencies }

function TTendencies.GetItem(Index: integer): TEditorTendency;
begin
  Result := inherited Items[Index] as TEditorTendency;
end;

procedure TTendencies.Init;
var
  i: Integer;
begin
  for i := 0 to Count -1 do
    Items[i].Init;
end;

function TTendencies.Find(vName: string): TEditorTendency;
begin
  Result := inherited Find(vName) as TEditorTendency;
end;

function TTendencies.FindByClass(TendencyClass: TEditorTendencyClass): TEditorTendency;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].ClassType = TendencyClass then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TTendencies.Add(vEditorTendency: TEditorTendencyClass): TEditorTendency;
begin
  RegisterClass(vEditorTendency);
  Result := vEditorTendency.Create;
  Add(Result);
end;

procedure TTendencies.Add(vEditorTendency: TEditorTendency);
begin
  inherited Add(vEditorTendency);
end;

{ TSynDebugMarksPart }

procedure TSynDebugMarksPart.Init;
begin
  inherited;
end;

constructor TSynDebugMarksPart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //  FMouseActions := TSynEditMouseActionsLineNum.Create(self);
  //  FMouseActions.ResetDefaults;
end;

destructor TSynDebugMarksPart.Destroy;
begin
//  FreeAndNil(FMouseActions);
  inherited;
end;

function TEditorEngine.SearchReplace(const FileName: string; const ALines: TStringList; const ASearch, AReplace: string; OnFoundEvent: TOnFoundEvent; AOptions: TSynSearchOptions): integer;
var
  i: integer;
  nSearchLen, nReplaceLen, n, nChar: integer;
  count: integer;
  iResultOffset: integer;
  aLine, aReplaceText: string;
  Replaced: Boolean;
begin
  if not Assigned(SearchEngine) then
    raise ESynEditError.Create('No search engine has been assigned');

  Result := 0;
  // can't search for or replace an empty string
  if Length(ASearch) = 0 then
    exit;

  i := 0;
  // initialize the search engine
  //SearchEngine.Options := AOptions;
  SearchEngine.Pattern := ASearch;
  if (ssoReplace in AOptions) then
    SearchEngine.Replacement := AReplace;
  // search while the current search position is inside of the search range
  try
    while i < ALines.Count do
    begin
      aLine := ALines[i];
      count := SearchEngine.FindAll(aLine);
      iResultOffset := 0;
      n := 0;
      // Operate on all results in this line.
      Replaced := False;
      while count > 0 do
      begin
        // An occurrence may have been replaced with a text of different length
        nChar := SearchEngine.Results[n] + iResultOffset;
        nSearchLen := SearchEngine.ResultLengths[n];
        Inc(n);
        Dec(count);

        Inc(Result);
        OnFoundEvent(FileName, aLine, i + 1, nChar, nSearchLen);

        if (ssoReplace in AOptions) then
        begin
          aReplaceText := SearchEngine.GetReplace(n);
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
      Inc(i);
    end;
  finally
  end;
end;

{ TEditorEngine }

procedure TEditorOptions.Apply;
var
  i: integer;
  List: TFileCategories;
begin
  List := TFileCategories.create(False);
  try
    for i := 0 to Engine.Files.Count - 1 do
    begin
      Engine.Files[i].Assign(Profile);
      if List.IndexOf(Engine.Files[i].Group.Category) < 0 then
        List.Add(Engine.Files[i].Group.Category);
    end;

    for i := 0 to List.Count - 1 do
    begin
      //check if List[i].Completion = nil
      //List[i].Completion.Font := Profile.Font;
      //List[i].Completion.Options := List[i].Completion.Options + [scoTitleIsCentered];

{      if List[i].Highlighter <> nil then
        List[i].Apply(List[i].Highlighter, Profile.Attributes);}
    end;

    List.Free;
  finally
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
  i: integer;
  b: Boolean;
  aList: TObjectList;
begin
  if not FCheckingChanged then
  begin
    Engine.BeginUpdate;
    FCheckingChanged := True;
    aList := TObjectList.Create(False);
    try
      for i := 0 to Count - 1 do
        aList.Add(Items[i]);

      b := True;
      try
        for i := 0 to aList.Count - 1 do
        begin
          if not b then
            (aList.Items[i] as TEditorFile).UpdateAge
          else
            b := (aList.Items[i] as TEditorFile).CheckChanged;
        end;
      finally
        aList.Free;
      end;
    finally
      FCheckingChanged := False;
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

procedure TEditorFiles.CloseOthers;
var
  i: Integer;
  aCur: TEditorFile;
begin
  Engine.BeginUpdate;
  try
    aCur := Engine.Files.Current;
    i := 0;
    while Engine.Files.Count > i do
    begin
      if aCur = Engine.Files[i] then
        i := i + 1
      else
        Engine.Files[i].Close;
    end;
  finally
    Engine.EndUpdate;
  end;
end;

procedure TEditorSession.Close;
begin
  FCachedIdentifiers.Clear;
  FCachedVariables.Clear;
  FCachedAge := 0;
  if (Project <> nil) and (Project.FileName <> '') then
    Project.SaveToFile(Project.FileName);

  Engine.Files.CloseAll;
  if Active then
  begin
    FreeAndNil(FProject);
    Project := Engine.DefaultProject;
  end;
  Engine.UpdateState([ecsChanged, ecsState, ecsRefresh, ecsProject]);
end;

constructor TEditorEngine.Create;
var
  aDefaultTendency: TDefaultTendency;
begin
  inherited;
  FEnvironment := TStringList.Create;
  FMessagesList := TEditorMessagesList.Create;
  FMacroRecorder := TSynMacroRecorder.Create(nil);
  FMacroRecorder.OnStateChange := @DoMacroRecorderChanged;
  FNotifyObjects := TEditorNotifyList.Create;

  FEnvironment.Add('Home=' + SysUtils.GetEnvironmentVariable('HOME'));
  FEnvironment.Add('EXE=' + Application.ExeName);
  FEnvironment.Add('MiniEdit=' + Application.Location);

  //FForms := TEditorFormList.Create(True);
  FOptions := TEditorOptions.Create;
  FCategories := TFileCategories.Create(True);
  FGroups := TFileGroups.Create(True);
  FSourceManagements := TSourceManagements.Create(True);
  FSearchEngine := TSynEditSearch.Create;
  FFiles := TEditorFiles.Create(TEditorFile);
  FDebugLink := TEditorDebugLink.Create(nil);

  FTendencies := TTendencies.Create(True);
  aDefaultTendency := TDefaultTendency.Create;
  Tendencies.Add(aDefaultTendency);

  FDefaultProject := TDefaultProject.Create;
  FDefaultProject.FileName := WorkSpace;//no file name just path
  FDefaultProject.Name := 'Default';
  FDefaultProject.Tendency := aDefaultTendency;

  FSession := TEditorSession.Create;
  Extenstion := 'mne-project';
end;

destructor TEditorEngine.Destroy;
begin
  FreeAndNil(FNotifyObjects);
  if (FEngineLife >= engnStarting) and (FEngineLife <= engnShutdowning) then
    Shutdown;
  FreeAndNil(FDebugLink);
  FreeAndNil(FFiles);
  FreeAndNil(FSession);
  FreeAndNil(FCategories);
  FreeAndNil(FGroups);
  FreeAndNil(FTendencies);
  FreeAndNil(FSearchEngine);
  FreeAndNil(FOptions);
  FreeAndNil(FSourceManagements);
  FreeAndNil(FMacroRecorder);
  FreeAndNil(FMessagesList);
  FreeAndNil(FDefaultProject);
  //FreeAndNil(FForms);
  FreeAndNil(FEnvironment);
  inherited;
end;

procedure TEditorEngine.RegisterNotify(NotifyEngine: INotifyEngine);
begin
  FNotifyObjects.Add(NotifyEngine);
end;

procedure TEditorEngine.UnregisterNotify(NotifyEngine: INotifyEngine);
begin
  FNotifyObjects.Remove(NotifyEngine);
end;

procedure EnumFiles(Folder, Filter: string; FileList: TStringList);
var
  R: integer;
  SearchRec: TSearchRec;
begin
  Folder := IncludeTrailingPathDelimiter(Folder);
  R := FindFirst(Folder + Filter, faAnyFile, SearchRec);
  while R = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      FileList.Add(SearchRec.Name);
    end;
    R := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function EnumFileList(const Root, Masks, Ignore: string; Callback: TEnumFilesCallback; AObject: TObject; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean; Types: TFileFindTypes): Boolean;
var
  Resume: Boolean;
  IgnoreList: TStringList;
  MaskList: TMaskList;
  aCount: Integer;

  procedure DoFind(const Root, Path: string; vLevel: Integer);
  var
    sr: TSearchRec;
    f: string;
  begin
    vLevel := vLevel + 1;
    if fftFile in Types then
    begin
      if FindFirst(Root + Path + '*'{Files}, faAnyFile, sr) = 0 then
      begin
        repeat
          if (sr.Attr and faDirectory) = 0 then //not a dir/it is a file
          begin
            if (sr.Name = '') or
              ((IgnoreList <> nil) and (IgnoreList.IndexOf(sr.Name) >= 0)) or
              not ((MaskList = nil) or (MaskList.Matches(sr.Name))) then
                continue;
            if ReturnFullPath then
              f := Root + IncludePathSeparator(Path) + sr.Name
            else
              f := IncludePathSeparator(Path) + sr.Name;
            Callback(AObject, f, aCount, vLevel, False, Resume);
            if (vMaxCount > 0) and (aCount > vMaxCount) then
              Resume := False;
              //raise Exception.Create('Too many files');
            if not Resume then
              break;
            aCount := aCount + 1;
          end;
        until (FindNext(sr) <> 0);
      end;
    end;

    if ((vMaxLevel = 0) or (vLevel <= vMaxLevel)) or (fftDir in Types) then
      if Resume then
        if FindFirst(Root + Path + '*', faDirectory, sr) = 0 then
        begin
          repeat
            if (sr.Name = '') or (sr.Name[1] = '.') or (sr.Name = '..') or
              ((IgnoreList <> nil) and (IgnoreList.IndexOf(sr.Name) >= 0)) then
                continue;

            if (sr.Attr and faDirectory) <> 0 then
            begin
              if fftDir in Types then
              begin
                if ReturnFullPath then
                  f := Root + IncludePathSeparator(Path) + sr.Name
                else
                  f := IncludePathSeparator(Path) + sr.Name;
                if fftDir in Types then
                  Callback(AObject, f, aCount, vLevel, True, Resume);
                if not Resume then
                  break;
              end;
              if (vMaxLevel = 0) or (vLevel < vMaxLevel) then
                DoFind(Root, IncludePathSeparator(Path + sr.Name), vLevel);
            end;
          until (FindNext(sr) <> 0);
        end;
  end;
begin
  if Ignore <> '' then
  begin
    IgnoreList := TStringList.Create;
    StrToStrings(Ignore, IgnoreList, [';'], [' ']);
    IgnoreList.Sort;
    IgnoreList.Sorted := true;
  end
  else
    IgnoreList := nil;

  if Masks <> '' then
    MaskList := TMaskList.Create(Masks)
  else
    MaskList := nil;
  aCount := 0;
  Resume := true;
  try
    DoFind(IncludeTrailingPathDelimiter(Root), '', 0);
  finally
    FreeAndNil(IgnoreList);
    FreeAndNil(MaskList);
  end;
  Result := Resume;
end;

procedure EnumFileListStringsCallback(AObject: TObject; const FileName: string; Count, Level:Integer; IsDirectory: Boolean; var Resume: Boolean);
begin
  TStringList(AObject).Add(FileName);
end;

procedure EnumFileList(const Root, Masks, Ignore: string; Strings: TStringList; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean);
begin
  EnumFileList(Root, Masks, Ignore, @EnumFileListStringsCallback, Strings, vMaxCount, vMaxLevel, ReturnFullPath);
end;

procedure EnumDirList(const Root, Masks, Ignore: string; Strings: TStringList; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean);
begin
  EnumFileList(Root, Masks, Ignore, @EnumFileListStringsCallback, Strings, vMaxCount, vMaxLevel, ReturnFullPath, [fftDir]);
end;

{$ifdef DEBUG}
procedure Nothing;
begin
  //nothing, just nothing
end;
{$endif}

procedure EnumIndentMode(vItems: TStrings);
begin
  vItems.Clear;
  vItems.Add('Keep it');
  vItems.Add('Tab to Spaces');
  vItems.Add('Spaces to Tabs');
end;

{ TListFileSearcher }

procedure TListFileSearcher.DoDirectoryFound;
begin
  inherited;

end;

procedure TListFileSearcher.DoFileFound;
begin
  inherited;
end;

procedure TEditorFiles.Edited;
begin
  Engine.UpdateState([ecsEdit]);
end;

procedure TEditorEngine.EndUpdate;
begin
  if (FUpdateCount = 1) and (Files.Current <> nil) and not (Files.Current.Visible) then
    Files.Current.Show;
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    if FUpdateState <> [] then
      InternalChangedState(FUpdateState);
    FUpdateState := [];
  end;
end;

procedure TEditorFiles.Find;
begin
  if Current <> nil then
    Current.Find;
end;

procedure TEditorEngine.DoReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
var
  i: Integer;
begin
  for i := 0 to FNotifyObjects.Count -1 do
  begin
    if FNotifyObjects[i] is IEditorNotifyEngine then
      (FNotifyObjects[i] as IEditorNotifyEngine).EngineReplaceText(Sender, ASearch, AReplace, Line, Column, ReplaceAction);
  end;
end;

function TEditorFiles.FindFile(const vFileName: string): TEditorFile;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(vFileName, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

procedure TEditorFiles.FindNext;
begin
  if Current <> nil then
    Current.FindNext;
end;

procedure TEditorFiles.FindPrevious;
begin
  if Current <> nil then
    Current.FindPrevious;
end;

function TEditorFiles.GetCurrent: TEditorFile;
begin
  Result := FCurrent;
end;

function TEditorFiles.GetEditedCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    if Items[i].IsEdited then
      Result := Result + 1;
  end;
end;

function TEditorEngine.GetRoot: string;
begin
  Result := Session.GetRoot
end;

function TEditorEngine.GetSCM: TEditorSCM;
begin
  if Session.Active then
    Result := Session.Project.SCM
  else
    Result := nil;
end;

function TEditorEngine.GetCurrentTendency: TEditorTendency;
begin
  Result := GetMainFileTendency;
  if Result = nil then
  begin
    if (Engine.Files.Current <> nil) and (Engine.Files.Current.Tendency <> nil) then
      Result := Engine.Files.Current.Tendency
    else
      Result := DefaultProject.Tendency;
  end;
end;

function TEditorEngine.GetTendency: TEditorTendency;
begin
  if Session.Active and (Session.Project.Tendency <> nil) and (not Session.Project.Tendency.IsDefault) then
    Result := Session.Project.Tendency
  else
  begin
    Result := GetMainFileTendency;
    if Result = nil then
    begin
      if (Engine.Files.Current <> nil) and (Engine.Files.Current.Tendency <> nil) then
        Result := Engine.Files.Current.Tendency
      else
        Result := DefaultProject.Tendency;
    end
  end
end;

function TEditorFiles.InternalOpenFile(FileName: string; AppendToRecent: Boolean): TEditorFile;
var
  aExt, lFileName: string;
begin
  {$ifdef windows}
  lFileName := ExpandFileName(FileName);
  {$else}
  if ExtractFilePath(FileName) = '' then
    lFileName := IncludeTrailingPathDelimiter(SysUtils.GetCurrentDir()) + FileName
  else lFileName := FileName;
  {$endif}
  Result := FindFile(lFileName);
  if Result = nil then
  begin
    aExt := ExtractFileExt(lFileName);
    if LeftStr(aExt, 1) = '.' then
      aExt := Copy(aExt, 2, MaxInt);

    Result := CreateEditorFile(aExt);
    if (Result = nil) and (Engine.Options.FallbackToText) then
      Result := CreateEditorFile(cFallbackGroup);
    if Result <> nil then
      Result.Load(lFileName);
  end;
  if (Result <> nil) and AppendToRecent then
    Engine.ProcessRecentFile(lFileName);
end;

function TEditorFiles.CreateEditorFile(vExtension: string): TEditorFile;
var
  aGroup: TFileGroup;
begin
  aGroup := Engine.Groups.FindExtension(vExtension);

  {if aGroup = nil then
    raise EEditorException.Create('Cannot open this file type: ' + vExtension);}
  if aGroup = nil then
  begin
    Engine.SendMessage('Cannot open this file type: ' + vExtension, msgtLog);
    Result := nil;
  end
  else
    Result := aGroup.CreateEditorFile(Self);
end;

procedure TEditorOptions.Load(vWorkspace: string);
  procedure SafeLoad(s: TStringList; vName:string);
  begin
    if FileExists(vName) then
      s.LoadFromFile(vName);
  end;
begin
  SafeLoadFromFile(vWorkspace + 'mne-options.xml');
  Profile.SafeLoadFromFile(vWorkspace + 'mne-editor.xml');

  SafeLoad(RecentFiles, vWorkspace + 'mne-recent-files.ini');
  SafeLoad(RecentFolders, vWorkspace + 'mne-recent-folders.ini');
  SafeLoad(RecentProjects, vWorkspace + 'mne-recent-projects.ini');
  SafeLoad(Projects, vWorkspace + 'mne-projects.ini');

  SafeLoad(SearchHistory, vWorkspace + 'mne-search-history.ini');
  SafeLoad(ReplaceHistory, vWorkspace + 'mne-replace-history.ini');
  SafeLoad(SearchFolderHistory, vWorkspace + 'mne-folder-history.ini');

  Apply;
end;

procedure TEditorSession.Load(FileName: string);
var
  aProject: TEditorProject;
begin
  Close; //must free before load project for save the desktop and sure to save its files
  Engine.BeginUpdate;
  try
    aProject := New;
    try
      aProject.LoadFromFile(FileName);
    except
      aProject.Free;
      raise;
    end;
    Project := aProject;
    FIsChanged := False;
    if Active then  //sure if not default project
      Engine.ProcessRecentProject(FileName);
    Engine.UpdateState([ecsChanged, ecsState, ecsRefresh, ecsProject, ecsProjectLoaded]);
  finally
    Engine.EndUpdate;
  end;
end;

function TEditorFiles.New(vGroup: TFileGroup): TEditorFile;
begin
  Engine.BeginUpdate;
  try
    if vGroup = nil then
      raise Exception.Create('Group is not defined');
    Result := vGroup.CreateEditorFile(Self);
    Result.NewContent;
    Result.Edit;
    Current := Result;
    Current.Show;
    Current.Activate;
    Engine.UpdateState([ecsChanged, ecsDebug, ecsState, ecsRefresh]);
  finally
    Engine.EndUpdate;
  end;
end;

function TEditorFiles.New(Name: string; Control: TWinControl): TEditorFile;
begin
  BeginUpdate;
  try
    Result := TControlEditorFile.Create(Engine.Files);
    (Result as TControlEditorFile).Control := Control;
    Result.Name := Name;
    Engine.UpdateState([ecsChanged, ecsDebug, ecsState, ecsRefresh]);
    Current := Result;
  finally
    EndUpdate;
  end;
end;

function TEditorSession.New(Tendency: TEditorTendency): TEditorProject;
begin
  Result := TRunProject.Create;
  if Tendency <> nil then
    Result.TendencyName := Tendency.Name;
end;

procedure TEditorFiles.Next;
var
  i: integer;
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
  i: integer;
  aFile: TEditorFile;
  aDialog: TOpenDialog;
begin
  aDialog := TOpenDialog.Create(nil);
  try
    aDialog.Title := 'Open file';
    //aDialog.Options := aDialog.Options + [ofHideReadOnly, ofFileMustExist, ofAllowMultiSelect];
    aDialog.Options := aDialog.Options + [ofEnableSizing, ofNoChangeDir, ofAllowMultiSelect]; //-ofAutoPreview
    aDialog.Filter := Engine.Groups.CreateFilter;
    aDialog.FilterIndex := 0;
    aDialog.InitialDir := Engine.BrowseFolder;
    if Engine.Tendency.GetDefaultGroup <> nil then
      aDialog.DefaultExt := Engine.Tendency.GetDefaultGroup.Extensions[0].Name
    else
      Engine.SendLog(Engine.Tendency.Name + ' have no default group');
    //aDialog.FileName := '*' + aDialog.DefaultExt;
    if aDialog.Execute then
    begin
      Engine.BeginUpdate;
      try
        aFile := nil;
        for i := 0 to aDialog.Files.Count - 1 do
        begin
          aFile := InternalOpenFile(aDialog.Files[i], True);
          //aFile.IsReadOnly := aDialog. TODO
        end;
        if aFile <> nil then
          Current := aFile;
        Engine.UpdateState([ecsChanged, ecsDebug, ecsState, ecsRefresh]);
      finally
        Engine.EndUpdate;
      end;
    end;
  finally
    aDialog.Free;
  end;
end;

function TEditorFiles.OpenFile(vFileName: string; ActivateIt: Boolean): TEditorFile;
begin
  if SameText(ExtractFileExt(vFileName), '.' + Engine.Extenstion) then
  begin
    Engine.Session.Load(vFileName);
    Result := nil; //it is a project not a file.
  end
  else
  begin
    Result := LoadFile(vFileName);
    if Result <> nil then
    begin
      Current := Result;
      if ActivateIt and not Result.Activated then
        Result.Activate;
    end;
  end;
end;

procedure TEditorSession.Open;
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
      Load(aDialog.FileName);
  finally
    aDialog.Free;
  end;
end;

function TEditorSession.Save(AProject: TEditorProject): Boolean;
begin
  if AProject.FileName = '' then
    Result := SaveAs(AProject)
  else
  begin
    AProject.SaveToFile(AProject.FileName);
    if AProject.IsActive then
      Engine.ProcessRecentProject(AProject.FileName);
    Engine.UpdateState([ecsFolder, ecsChanged, ecsState, ecsRefresh]);
    Result := True;
    FIsChanged := False;
  end;
end;

function TEditorSession.Save: Boolean;
begin
  Result := Save(Project)
end;

function TEditorSession.SaveAs(AProject: TEditorProject): Boolean;
var
  aDialog: TSaveDialog;
begin
  aDialog := TSaveDialog.Create(nil);
  try
    aDialog.Title := 'Save project';
    aDialog.DefaultExt := Engine.Extenstion;
    aDialog.Filter := 'Project files (*.' + Engine.Extenstion + ')|*.' + Engine.Extenstion + '|All files|*.*';
    aDialog.InitialDir := Engine.BrowseFolder;
    aDialog.FileName := AProject.Name + aDialog.DefaultExt;
    Result := aDialog.Execute;
    if Result then
    begin
      AProject.FileName := aDialog.FileName;
      Save(AProject);
    end;
  finally
    aDialog.Free;
  end;
end;

function TEditorSession.SaveAs: Boolean;
begin
  Result := SaveAs(Project)
end;

function TEditorSession.GetRoot: string;
var
  r: string;
begin
  if (Engine.Session.Active) and (Engine.Session.Project.RunOptions.MainFolder <> '') then
    Result := GetMainFolder
  else if (Engine.Session.Active) and (Engine.Session.Project.RunOptions.MainFile <> '') then
  begin
    r := Engine.Session.Project.RunOptions.MainFile;
    r := ExpandToPath(r, Engine.Session.Project.Path);
    Result := ExtractFilePath(Engine.EnvReplace(r, true));
  end
  else if Engine.Files.Current <> nil then
    Result := ExtractFilePath(Engine.Files.Current.Name)
  else if Engine.BrowseFolder <> '' then
    Result := Engine.BrowseFolder
  else
    Result := Application.Location;
  Result := ExpandFileName(IncludePathSeparator(Result));
end;

procedure TEditorFiles.Prior;
var
  i: integer;
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
  i: integer;
begin
  i := Options.RecentFiles.IndexOf(FileName);
  if i >= 0 then
    Options.RecentFiles.Move(i, 0)
  else
    Options.RecentFiles.Insert(0, FileName);
  while Options.RecentFiles.Count > 50 do
    Options.RecentFiles.Delete(50);
  UpdateState([ecsRecents]);
end;

procedure TEditorEngine.ProcessRecentFolder(const FileName: string);
var
  i: integer;
begin
  i := Options.RecentFolders.IndexOf(FileName);
  if i >= 0 then
    Options.RecentFolders.Move(i, 0)
  else
    Options.RecentFolders.Insert(0, FileName);
  while Options.RecentFolders.Count > 50 do
    Options.RecentFolders.Delete(50);
  UpdateState([ecsRecents]);
end;

procedure TEditorEngine.ProcessRecentProject(const FileName: string);
var
  i: integer;
begin
  i := Options.RecentProjects.IndexOf(FileName);
  if i >= 0 then
    Options.RecentProjects.Move(i, 0)
  else
    Options.RecentProjects.Insert(0, FileName);
  while Options.RecentProjects.Count > 50 do
    Options.RecentProjects.Delete(50);
  UpdateState([ecsRecents]);
end;

procedure TEditorEngine.ProcessProject(const FileName: string);
var
  i: integer;
begin
  i := Options.Projects.IndexOf(FileName);
  if i >= 0 then
    Options.Projects.Move(i, 0)
  else
    Options.Projects.Insert(0, FileName);
  UpdateState([ecsRecents]);
end;

procedure TEditorFiles.Save;
begin
  if Current <> nil then
    Current.SaveFile;
end;

procedure TEditorFiles.SaveAll;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].SaveFile;
  end;
end;

procedure TEditorFiles.ReloadAll;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Reload;
  end;
end;

procedure TEditorFiles.CheckAll;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].CheckChanged(True);
  end;
end;

procedure TEditorFiles.SaveAs;
begin
  if Current <> nil then
    Current.SaveFile(ExtractFileExt(Current.Name), True);
end;

procedure TEditorOptions.Save(vWorkspace: string);
begin
  if DirectoryExists(vWorkspace) then
  begin
    Profile.SaveToFile(vWorkspace + 'mne-editor.xml');
    SaveToFile(vWorkspace + 'mne-options.xml');
    RecentFiles.SaveToFile(vWorkspace + 'mne-recent-files.ini');
    RecentFolders.SaveToFile(vWorkspace + 'mne-recent-folders.ini');
    RecentProjects.SaveToFile(vWorkspace + 'mne-recent-projects.ini');
    Projects.SaveToFile(vWorkspace + 'mne-projects.ini');

    SearchHistory.SaveToFile(vWorkspace + 'mne-search-history.ini');
    ReplaceHistory.SaveToFile(vWorkspace + 'mne-replace-history.ini');
    SearchFolderHistory.SaveToFile(vWorkspace + 'mne-folder-history.ini');

    Engine.UpdateState([ecsFolder]);
  end;
end;

procedure TEditorFiles.SetCurrent(const Value: TEditorFile);
begin
  if FCurrent <> Value then
  begin
    FCurrent := Value;
    FCurrent.Show;
    if not Engine.Updating then
      FCurrent.Activate;
    Engine.UpdateState([ecsDebug, ecsRefresh]);
  end;
end;

procedure TEditorFiles.SetCurrentIndex(Index: integer; vRefresh: Boolean);
var
  aCurrent: TEditorFile;
  OldCurrent: TEditorFile;
  a: Boolean;
begin
  if Count <> 0 then
  begin
    Engine.BeginUpdate;
    try
      if Index >= Count then
        Index := Count - 1;
      aCurrent := Items[Index];
      a := (Current <> nil) and Current.Activated;
      if aCurrent <> nil then
      begin
        OldCurrent := Current;
        Current := aCurrent;
        if vRefresh and (OldCurrent <> Current) then
        begin
          if a then
            Current.Activate;
          Engine.UpdateState([ecsState, ecsDebug, ecsRefresh]);
        end;
      end;
    finally
      Engine.EndUpdate;
    end;
  end;
end;

procedure TEditorSession.SetProject(const Value: TEditorProject);
begin
  if FProject <> Value then
  begin
    Engine.BeginUpdate;
    try
      if Active then
        Close;
      SetInternalProject(Value);
      if FProject.Tendency <> nil then
        FProject.Tendency.Prepare; //Prepare debug object and others
      if FProject.SaveDesktop then
        FProject.Desktop.Load;
      Changed;
      Engine.UpdateState([ecsChanged, ecsState, ecsRefresh, ecsProject, ecsProjectLoaded]);
    finally
      Engine.EndUpdate;
    end;
  end;
end;

procedure TEditorSession.SetInternalProject(const Value: TEditorProject);
begin
  FProject := Value;
end;

function TEditorSession.GetActive: Boolean;
begin
  Result := (Project <> nil) and not (Project is TDefaultProject);
end;

function TEditorSession.GetMainFile: string;
begin
  if Project.RunOptions.MainFile <> '' then
    Result := Engine.ExpandFile(Project.RunOptions.MainFile)
  else
    Result := '';
end;

procedure TEditorOptions.OptionsShow;
var
  aSelect: string;
begin
  with TEditorOptionsForm.Create(Application) do
  begin
    Engine.BeginUpdate;
    try
      if (Engine.Files.Current <> nil) then
        aSelect := Engine.Files.Current.GetLanguageName //just to select a language in the combobox
      else
        aSelect := '';
      if Execute(Profile, aSelect) then
      begin
        Apply;
        Engine.SaveOptions;
      end;
      Engine.UpdateState([ecsOptions]);
    finally
      Engine.EndUpdate;
      Free;
    end;
  end;
end;

procedure TEditorOptions.ColorsShow;
var
  aSelect: string;
begin
  with TEditorColorsForm.Create(Application) do
  begin
    Engine.BeginUpdate;
    try
      if (Engine.Files.Current <> nil) then
        aSelect := Engine.Files.Current.Group.Category.Name //just to select a language in the combobox
      else
        aSelect := '';
      if Execute(Profile, aSelect) then
      begin
        Apply;
        Engine.SaveOptions;
      end;
      Engine.UpdateState([ecsOptions]);
    finally
      Engine.EndUpdate;
      Free;
    end;
  end;
end;

procedure TEditorEngine.RemoveProject(const FileName: string);
var
  i: integer;
begin
  i := Options.Projects.IndexOf(FileName);
  if i >= 0 then
    Options.Projects.Delete(i);
  UpdateState([ecsRecents]);
end;

function SortGroupsByTitle(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TFileGroup(Item1).Title, TFileGroup(Item2).Title);
end;

procedure TEditorEngine.Startup(vSafeMode: Boolean);
begin
  FSafeMode := vSafeMode;
  FEngineLife := engnStarting;
  Tendencies.Init;
  Groups.Sort(@SortGroupsByTitle);
  DefaultProject.FileName := WorkSpace + 'mne-default-project.xml';
  try
    LoadOptions;
    //here we will autoopen last files
    //OpenDefaultProject
    //Session.FProject := DefaultProject; nah it is effect on UpdatePanel and ProjectChanged in mainform
  except
    on E: Exception do
      Engine.SendMessage(E.Message, msgtLog);
  end;
  FEngineLife := engnStarted;
end;

procedure TEditorEngine.OpenDefaultProject;
begin
  DefaultProject.SafeLoadFromFile(DefaultProject.FileName);
  Session.Project := DefaultProject;
end;

procedure TEditorEngine.LoadOptions;
var
  aFile: string;
  i: Integer;
begin
  BeginUpdate;
  try
    Options.Load(Workspace);
    Session.Options.SafeLoadFromFile(Workspace + 'mne-options-' + SysPlatform + '.xml');
    for i := 0 to Tendencies.Count - 1 do
    begin
      if capOptions in Tendencies[i].Capabilities then
      begin
        aFile := Workspace + 'mne-tendency-' + LowerCase(Tendencies[i].Name) + '.xml';
        if FileExists(aFile) then
          XMLReadObjectFile(Tendencies[i], aFile);
      end;
    end;
    for i := 0 to FNotifyObjects.Count-1 do
    begin
      if (FNotifyObjects[i] is ISettingNotifyEngine) then
        (FNotifyObjects[i] as ISettingNotifyEngine).LoadOptions;
    end;

    UpdateOptions;
    UpdateState([ecsOptions]);
  finally
    EndUpdate;
  end;
end;

procedure TEditorEngine.UpdateOptions;
var
  i, j: Integer;
  s: string;
  lStrings: TStringList;
begin
  for i := 0 to Groups.Count - 1 do
  begin
    if not Groups[i].Category.Tendency.IsDefault then
    begin
      Groups[i].ExtraExtensions.Clear;

      lStrings := TStringList.Create;
      try
        s := Options.ExtraExtensions.Values[Groups[i].Name];
        if s <> '' then
        begin
          StrToStrings(s, lStrings, [';'], [' ']);
          for j := 0 to lStrings.Count -1 do
          begin
            s := lStrings[j];
            if LeftStr(s, 1) = '.' then
              s := Copy(s, 2, MaxInt);
            Groups[i].ExtraExtensions.Add(s);
          end;
        end;
      finally
        lStrings.Free;
      end;
    end;
  end;
end;

procedure TEditorEngine.SaveOptions;
var
  aFile: string;
  i: integer;
begin
  ForceDirectories(Workspace);
  Options.Save(WorkSpace);
  Session.Options.SaveToFile(Workspace + 'mne-options-' + SysPlatform + '.xml');
  for i := 0 to Tendencies.Count - 1 do
  begin
    if capOptions in Tendencies[i].Capabilities then
    begin
      aFile := Workspace + 'mne-tendency-' + LowerCase(Tendencies[i].Name) + '.xml';
      XMLWriteObjectFile(Tendencies[i], aFile);
    end;
  end;
  for i := 0 to FNotifyObjects.Count-1 do
  begin
    if (FNotifyObjects[i] is ISettingNotifyEngine) then
      (FNotifyObjects[i] as ISettingNotifyEngine).SaveOptions;
  end;
end;

procedure TEditorEngine.Shutdown;
var
  i: Integer;
begin
  if (FEngineLife > engnNone) and (FEngineLife < engnShutdowned) then
  begin
    if Session.Active and (Session.Project.FileName <> '') then
      Session.Save;
    SaveOptions;
    for i := 0 to Tendencies.Count - 1 do
      Tendencies[i].Unprepare;
    Files.Clear;
  end;
  FEngineLife := engnShutdowned;
end;

function TEditorEngine.IsShutdown: Boolean;
begin
  Result := FEngineLife > engnStarted;
end;

procedure TEditorEngine.RemoveRecentProject(const FileName: string);
var
  i: integer;
begin
  i := Options.RecentProjects.IndexOf(FileName);
  if i >= 0 then
    Options.RecentProjects.Delete(i);
  UpdateState([ecsRecents]);
end;

procedure TEditorEngine.RemoveRecentFile(const FileName: string);
var
  i: integer;
begin
  i := Options.RecentFiles.IndexOf(FileName);
  Options.RecentFiles.Delete(i);
  UpdateState([ecsRecents]);
end;

procedure TEditorEngine.RemoveRecentFolder(const FileName: string);
var
  i: integer;
begin
  i := Options.RecentFolders.IndexOf(FileName);
  Options.RecentFolders.Delete(i);
  UpdateState([ecsRecents]);
end;

function TEditorEngine.GetUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

function TEditorEngine.EnvReplace(S: string; NoRoot: Boolean): string;
var
  List: TStringList;
begin
  Result := '';
  if s <> '' then
  begin
    List := TStringList.Create;
    try
      EnvList(List, NoRoot);
      Result := VarReplace(S, List, sEnvVarChar);
    finally
      List.Free;
    end;
  end;
end;

procedure TEditorEngine.EnvList(List: TStringList; NoRoot: Boolean);
var
  MainFile: string;
begin
  List.Assign(Environment);
  if not NoRoot then
  begin
    List.Add('Root=' + GetRoot);
    List.Add('Directory=' + GetRoot);
  end;

  if Files.Current <> nil then
  begin
    List.Add('File=' + Files.Current.Name);
    List.Add('FileName=' + Files.Current.NakeName);
    List.Add('FilePureName=' + Files.Current.PureName);
    List.Add('FilePath=' + Files.Current.Path);
  end;

  if Session.Active then
  begin
    MainFile := Session.Project.RunOptions.MainFile;
    MainFile := ExpandToPath(MainFile, Session.Project.Path);
  end
  else
    MainFile := '';

  if (MainFile = '') and (Files.Current <> nil) then
    MainFile := Files.Current.Name;

  if MainFile <> '' then
  begin
    List.Add('Main=' + MainFile);
    List.Add('MainFile=' + MainFile);
    List.Add('MainFileName=' + ExtractFileName(MainFile));
    List.Add('MainFilePureName=' + ExtractFileNameWithoutExt(MainFile));
    List.Add('MainPath=' + ExtractFilePath(MainFile));
  end;

  if Session.Active then
  begin
    List.Add('Project=' + Session.Project.FileName);
    List.Add('ProjectFile=' + Session.Project.FileName);
    List.Add('ProjectPath=' + Session.Project.Path);
    List.Add('OutputName=' + Session.Project.RunOptions.OutputFile); //TODO need to guess
  end;
end;

function TEditorEngine.ExpandFile(FileName: string): string;
begin
  if FileName <> '' then
    Result := ExpandFileName(ExpandToPath(FileName, Session.GetRoot))
  else
    Result := '';
end;

function TEditorEngine.GetIsChanged: Boolean;
begin
  Result := (Files.GetEditedCount > 0) or Session.IsChanged;
end;

procedure TEditorEngine.SendLog(S: string);
begin
  SendMessage(S, msgtLog);
end;

procedure TEditorEngine.SendMessage(S: string; vMessageType: TNotifyMessageType);
var
  aError: TErrorInfo;
begin
  aError := Default(TErrorInfo);
  SendMessage(S, vMessageType, aError);
end;

procedure TEditorEngine.SendMessage(S: string; vMessageType: TNotifyMessageType; vError: TErrorInfo);
var
  i: Integer;
begin
  if not IsShutdown then
    for i := 0 to FNotifyObjects.Count -1 do
    begin
      if FNotifyObjects[i] is IEditorNotifyEngine then
        (FNotifyObjects[i] as IEditorNotifyEngine).EngineMessage(S, vMessageType, vError);
    end;
end;

procedure TEditorEngine.SendAction(EditorAction: TEditorAction);
var
  i: Integer;
begin
  if not IsShutdown then
  begin
    for i := 0 to FNotifyObjects.Count -1 do
    begin
      if (FNotifyObjects[i] is IEditorNotifyEngine) then
        (FNotifyObjects[i] as IEditorNotifyEngine).EngineAction(EditorAction);
    end;
  end;
end;

procedure TEditorEngine.DoChangedState(State: TEditorChangeStates);
var
  i: Integer;
begin
  if not IsShutdown then
  begin
    for i := 0 to FNotifyObjects.Count -1 do
    begin
      FNotifyObjects[i].ChangeState(State);
    end;
end;
end;

procedure TEditorEngine.UpdateState(State: TEditorChangeStates);
begin
  if Updating then
    FUpdateState := FUpdateState + State
  else //if (FInUpdateState = 0) or not (State in FUpdateState) then
    InternalChangedState(State);
end;

function TEditorFiles.LoadFile(vFileName: string; AppendToRecent: Boolean): TEditorFile;
begin
  try
    try
      Result := InternalOpenFile(vFileName, AppendToRecent);
    finally
      Engine.UpdateState([ecsChanged]);
    end;
  finally
    Engine.UpdateState([ecsState, ecsDebug, ecsRefresh]);
  end;
end;

procedure TEditorFiles.Replace;
begin
  if Current <> nil then
    Current.Replace;
end;

procedure TEditorFiles.Revert;
begin
  if Current <> nil then
  begin
    if MsgBox.Msg.Yes('Revert file ' + Current.Name) then
      Current.Reload;
  end;
end;

procedure TEditorEngine.SetBrowseFolder(const Value: string);
begin
  FBrowseFolder := Value;
  if FBrowseFolder <> '' then
    FBrowseFolder := IncludeTrailingPathDelimiter(FBrowseFolder);
  UpdateState([ecsFolder]);
end;

procedure TEditorEngine.DoMacroRecorderChanged(Sender: TObject);
begin
  UpdateState([ecsState]);
end;

function TEditorEngine.GetWorkSpace: string;
begin
  Result := IncludeTrailingPathDelimiter(FWorkSpace);
end;

function TEditorEngine.GetMainFileTendency: TEditorTendency;
begin
  if (Session.Active) and (Session.Project.RunOptions.MainFile <> '') then
  begin
    if (not SameText(FCache.MainFile, Session.Project.RunOptions.MainFile)) then
    begin
      FCache.MainFile := Session.Project.RunOptions.MainFile;
      FCache.MainGroup := Engine.Groups.FindExtension(ExtractFileExt(FCache.MainFile));
    end;
    Result := FCache.MainGroup.Category.Tendency;
  end
  else
    Result := nil;
end;

procedure TEditorEngine.InternalChangedState(State: TEditorChangeStates);
begin
  Inc(FInUpdateState);
  try
    DoChangedState(State);
  finally
    Dec(FInUpdateState);
  end;
end;

{ TEditorFiles }

function TEditorFiles.GetItems(Index: integer): TEditorFile;
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
  begin
    Current := Result;
    Current.Activate;
  end;
end;

destructor TEditorFiles.Destroy;
begin
  inherited;
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
    Current.Refresh;
end;

function TEditorFiles.ShowFile(const FileName: string; Line: integer): TEditorFile;
begin
  Result := InternalOpenFile(FileName, False);
  Result.SetLine(Line);
  Engine.UpdateState([ecsChanged]);
  if Result <> nil then
    Current := Result;
  Engine.UpdateState([ecsState, ecsRefresh]);
end;

{ TEditorFile }

procedure TEditorFile.Edit;
begin
  if not IsReadOnly then
    IsEdited := True;
end;

procedure TEditorFile.Close;
var
  aParent: TEditorEngine;
  i: integer;
  mr: TmsgChoice;
  a: Boolean;
begin
  if IsEdited then
  begin
    mr := MsgBox.Msg.YesNoCancel('Save file ' + Name + ' before close?');
    if mr = msgcCancel then
      Abort
    else if mr = msgcYes then
      SaveFile;
  end;

  i := Index;
  aParent := Engine;
  aParent.ProcessRecentFile(Name);
  a := (aParent.Files.Current <> nil) and aParent.Files.Current.Activated;
  if aParent.Files.FCurrent = self then
    aParent.Files.FCurrent := nil;
  Free;
  aParent.Files.SetCurrentIndex(i, False);
  if a and (aParent.Files.Current <> nil) then
    aParent.Files.Current.Activate;
  aParent.UpdateState([ecsChanged, ecsState, ecsRefresh]);
end;

procedure TEditorFile.OpenInclude;
begin
end;

function TEditorFile.CanOpenInclude: Boolean;
begin
  Result := False;
end;

constructor TEditorFile.Create(ACollection: TCollection);
begin
  inherited;
  FIsNew := True;
  FIsEdited := False;
  FFileEncoding := 'UTF8';
  FLinesMode := efmUnix;
end;

destructor TEditorFile.Destroy;
begin
  inherited;
end;

procedure TEditorFile.Assign(Source: TPersistent);
begin
end;

procedure TEditorFile.AssignTo(Dest: TPersistent);
begin
end;

procedure TEditorFile.DoEdit(Sender: TObject);
begin
  Edit;
  Engine.Files.Edited;
end;

procedure TEditorFile.Load(FileName: string);
begin
  FileName := ExpandFileName(FileName);
  Name := FileName;
  try
    DoLoad(FileName);//maybe safe loading
  except
    on E: Exception do
    begin
      Engine.SendMessage('Can not load :' + FileName + ' : ' + E.Message, msgtLog);
    end;
  end;
  IsEdited := False;
  IsNew := False;
  UpdateAge;
end;

procedure SaveAsMode(const FileName: string; Mode: TEditorLinesMode; Strings: Tstrings);
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
  DoSave(FileName);
  Name := FileName;
  IsEdited := False;
  IsNew := False;
  Engine.UpdateState([ecsFolder]);
  UpdateAge;
end;

procedure TEditorFile.Rename(ToNakeName: string);
var
  p: string;
  aExt: string;
  aGroup: TFileGroup;
begin
  Engine.BeginUpdate;
  try
    if Name <> '' then
    begin
      p := ExtractFilePath(Name);
      if RenameFile(Name, p + ToNakeName) then
      begin
        Engine.RemoveRecentFile(Name);
        Name := p + ToNakeName;
        Engine.ProcessRecentFile(Name);

        aExt := Extension;
        if LeftStr(aExt, 1) = '.' then
          aExt := MidStr(aExt, 2, MaxInt);
        aGroup := Engine.Groups.FindExtension(aExt);
        Group := aGroup;
      end;
    end
    else
      Name := ToNakeName;
    Engine.UpdateState([ecsRefresh, ecsFolder, ecsState, ecsChanged]);
  finally
    Engine.EndUpdate;
  end;
end;

procedure TEditorFile.Delete;
begin
  Engine.BeginUpdate;
  try
    if Name <> '' then
    begin
      if DeleteFile(Name) then
      begin
        Engine.RemoveRecentFile(Name);
        Name := ExtractFileName(Name);
        IsNew := True;
        IsEdited := True;
      end;
    end;
    Engine.UpdateState([ecsRefresh, ecsFolder, ecsState, ecsChanged]);
  finally
    Engine.EndUpdate;
  end;
end;

procedure TEditorFile.SetIsEdited(const Value: Boolean);
begin
  FIsEdited := Value;
end;

procedure TEditorFile.Show;
begin
  if Control <> nil then
  begin
    Control.Align := alClient;
    Control.Realign;
    Control.Visible := True;
    Control.Show;
    Control.BringToFront;
    //Activate;//no no, bad when return back from another app to miniedit
  end;
end;

function TEditorFile.Visible: Boolean;
begin
  Result := (Control <> nil ) and (Control.Visible);
end;

procedure TEditorFile.SaveFile(Extension:string; AsNewFile: Boolean);
var
  aDialog: TSaveDialog;
  aSave, DoRecent: Boolean;
  aName: string;
begin
  DoRecent := False;
  aName := '';
  if IsNew or (FName = '') or AsNewFile then
  begin
    aDialog := TSaveDialog.Create(nil);
    aDialog.Title := 'Save file';
    aDialog.Filter := Engine.Groups.CreateFilter(True, Extension, Group, False);//put the group of file as the first one
    aDialog.InitialDir := Engine.BrowseFolder;
    if Extension <> '' then
      aDialog.DefaultExt := Extension
    else
    begin
      if Group <> nil then
        aDialog.DefaultExt := Group.Extensions[0].Name
      else
        aDialog.DefaultExt := Engine.Tendency.GetDefaultGroup.Extensions[0].Name;
    end;
    aDialog.FileName := '*' + aDialog.DefaultExt;

    aSave := aDialog.Execute;
    if aSave then
    begin
      aName := aDialog.FileName;
      DoRecent := True;
    end;
    aDialog.Free;
  end
  else
  begin
    aName := FName;
    aSave := True;
  end;

  if aSave then
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

function TEditorFile.CheckChanged(Force: Boolean): Boolean;
var
  mr: TmsgChoice;
  n: Integer;
begin
  Result := True;
  if not IsNew then
  begin
    if (FileExists(Name)) then
    begin
      if ((FFileAge <> FileAge(Name)) or (FFileSize <> FileSize(Name)))  then
      begin
        if Force then
          mr := msgcYes
        else
          mr := MsgBox.Msg.YesNoCancel(Name + #13' was changed, update it?');
        if mr = msgcYes then
          Reload;
        if mr = msgcCancel then
          Result := False
        else
          UpdateAge;
      end;
    end
    else
    begin
      if Force then
        n := -1 //nothing
      else
        n := MsgBox.Msg.Ask(Name + #13' was not found, what do want?', [Choice('&Keep It', msgcYes), Choice('&Close', msgcCancel), Choice('Read only', msgcNo)], 0, 2);
      if n = -1 then //do nothing
      else if n = 0 then //Keep It
        IsNew := True
      else if n = 2 then //Keep It
      begin
        IsEdited := False;
        IsReadOnly := True
      end
      else
        Close;
    end;
  end;
end;

procedure TEditorFile.Activate;
var
  aControl: TWinControl;
begin
  if Control.CanFocus then
  begin
    if Supports(Control, IEditorControl) then
      aControl := (Control as IEditorControl).GetMainControl
    else
      aControl := nil;

    if aControl = nil then
      aControl := Control as TWinControl;

    (Engine.FilePanel.Owner as TCustomForm).ActiveControl := aControl;
  end;
end;

function TEditorFile.Activated: Boolean;
var
  aControl: TWinControl;
begin
  Result := False;
  if (Control <> nil) and Control.CanFocus then
  begin
    if Supports(Control, IEditorControl) then
      aControl := (Control as IEditorControl).GetMainControl
    else
      aControl := nil;

    if aControl = nil then
      aControl := Control as TWinControl;

    Result := (Engine.FilePanel.Owner as TCustomForm).ActiveControl = aControl;
  end;
end;

procedure TEditorFile.GotoLine;
begin
end;

procedure TEditorFile.Find;
begin
end;

procedure TEditorFile.FindNext;
begin
end;

procedure TEditorFile.FindPrevious;
begin
end;

procedure TEditorFile.Replace;
begin
end;

procedure TEditorFile.Refresh;
begin
end;

function TEditorFile.GetHint(HintControl: TControl; CursorPos: TPoint; out vHint: string): Boolean;
begin
  Result := False;
end;

function TEditorFile.GetGlance: string;
begin
  Result := '';
end;

function TEditorFile.GetLanguageName: string;
begin
  Result := '';
end;

procedure TEditorFile.SetLine(Line: Integer);
begin

end;

function TEditorFile.CanCopy: Boolean;
begin
  Result := ecpAllowCopy in Capability;
end;

function TEditorFile.CanPaste: Boolean;
begin
  Result := ecpAllowPaste in Capability
end;

procedure TEditorFile.Paste;
begin
end;

procedure TEditorFile.Copy;
begin
end;

procedure TEditorFile.Cut;
begin
end;

procedure TEditorFile.SelectAll;
begin
end;

procedure TEditorFile.UpdateAge;
begin
  FFileAge := FileAge(Name);
  FFileSize := FileSize(Name);
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
    GroupChanged;
  end;
end;

function TEditorFile.GetCapability: TEditCapability;
begin
  Result := [];
  DoGetCapability(Result);
end;

function TEditorFile.GetIsText: Boolean;
begin
  Result := (Group <> nil) and Group.Category.IsText;
end;

function TEditorFile.GetNakeName: string;
begin
  Result := ExtractFileName(Name);
end;

function TEditorFile.GetExtension: string;
begin
  Result := ExtractFileExt(Name);
end;

function TEditorFile.GetPath: string;
begin
  Result := ExtractFilePath(Name);
end;

function TEditorFile.GetPureName: string;
begin
  Result := ExtractFileNameWithoutExt(NakeName);
end;

function TEditorFile.GetTendency: TEditorTendency;
begin
  if Group <> nil then
    Result := Group.Category.Tendency
  else
    Result := Engine.DefaultProject.Tendency;
end;

procedure TEditorFile.SetFileEncoding(AValue: string);
begin
  if FFileEncoding <> AValue then
  begin
    FFileEncoding := AValue;
    Edit;
    Engine.UpdateState([ecsState, ecsRefresh]);
  end;
end;

procedure TEditorFile.SetExtension(AValue: string);
begin
  if LeftStr(AValue, 1) <> '.' then
    AValue := '.' + AValue;
  Rename(PureName + AValue);
end;

function TEditorFile.GetControl: TWinControl;
begin
  Result := nil;
end;

procedure TEditorFile.DoGetCapability(var vCapability: TEditCapability);
begin
  vCapability := [];
end;

function TEditorFile.GetIsReadonly: Boolean;
begin
  Result := False;//TODO true
end;

procedure TEditorFile.SetIsNew(AValue: Boolean);
begin
  if FIsNew =AValue then
    Exit;
  FIsNew :=AValue;
end;

procedure TEditorFile.SetIsReadonly(const Value: Boolean);
begin
end;

procedure TEditorFile.NewContent;
begin
end;

function ConvertToLinesMode(Mode: TEditorLinesMode; Contents: string): string;
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  Strings.Text := Contents;
  try
    case Mode of
      efmWindows: Strings.LineBreak := #$D#$A;
      efmMac: Strings.LineBreak := #$D;
      else
        Strings.LineBreak := #$A;
    end;
    Result := Strings.Text;
  finally
    Strings.Free;
  end
end;

function DetectLinesMode(const Contents: string): TEditorLinesMode;
var
  i: integer;
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

function ConvertIndents(const Contents: string; TabWidth: integer; Options: TIndentMode): string;
var
  p, l: integer;

  procedure ScanToEOL;
  var
    i: integer;
  begin
    i := p;
    while i <= l do
    begin
      if Contents[i] in [#13, #10] then
        break;
      Inc(i);
    end;
    if ((i + 1) <= l) and (Contents[i + 1] in [#13, #10]) then
      Inc(i);
    Result := Result + Copy(Contents, p, i - p + 1);
    p := i + 1;
  end;

  procedure ScanSpaces;
  var
    i, c, t: integer;
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
    if Options = idntSpacesToTabs then
    begin
      t := c div TabWidth;
      c := c mod TabWidth;
      Result := Result + RepeatString(#9, t);
    end;
    Result := Result + RepeatString(' ', c);
    p := i;
  end;

begin
  Result := '';
  p := 1;
  l := Length(Contents);
  while p <= l do
  begin
    ScanSpaces;
    ScanToEOL;
  end;
end;

function TEditorFile.GetLinesModeAsText: string;
begin
  case LinesMode of
    efmWindows: Result := 'W';
    efmMac: Result := 'M';
    else Result := 'U'; //efmUnix
  end;
end;

procedure TEditorFile.SetLinesMode(const Value: TEditorLinesMode);
begin
  if FLinesMode <> Value then
  begin
    FLinesMode := Value;
    Edit;
    Engine.UpdateState([ecsState, ecsRefresh]);
  end;
end;

procedure TEditorFile.SetNakeName(AValue: string);
begin
  Rename(AValue);
end;

procedure TEditorFile.SetPureName(AValue: string);
begin
  Rename(AValue + Extension);
end;

procedure TEditorFile.GroupChanged;
begin
  Tendency.Prepare; //Prepare its objects like debuggers
  Assign(Engine.Options.Profile);
end;

procedure TEditorFile.DoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if ([scReadOnly, scCaretX, scCaretY, scLeftChar, scTopLine, scSelection] * Changes) <> [] then
    Engine.UpdateState([ecsState]);
end;

{ TEditorOptions }

constructor TEditorOptions.Create;
begin
  inherited Create;
  FSearchHistory := TStringList.Create;
  FReplaceHistory := TStringList.Create;
  FSearchFolderHistory := TStringList.Create;
  FProfile := TEditorProfile.Create;
  FExtraExtensions := TStringList.Create;
  FRecentFiles := TStringList.Create;
  FRecentFolders := TStringList.Create;
  FRecentProjects := TStringList.Create;
  FProjects := TStringList.Create;
  FShowFolder := True;
  FSortFolderFiles := srtfByNames;
  FShowMessages := False;
  FCollectTimeout := 60;
  FMessagesHeight := 100;
  FFoldersPanelWidth := 180;
  FShowToolbar := True;
end;

destructor TEditorOptions.Destroy;
begin
  FSearchHistory.Free;
  FReplaceHistory.Free;
  FSearchFolderHistory.Free;
  FExtraExtensions.Free;
  FProfile.Free;
  FRecentFiles.Free;
  FRecentFolders.Free;
  FRecentProjects.Free;
  FProjects.Free;
  inherited;
end;

procedure TEditorOptions.SetProjects(const Value: TStringList);
begin
  if FProjects <> Value then
    FProjects.Assign(Value);
end;

procedure TEditorOptions.SetRecentFiles(const Value: TStringList);
begin
  if FRecentFiles <> Value then
    FRecentFiles.Assign(Value);
end;

function TEditorOptions.GetAnsiCodePage: Integer;
begin
  Result := SystemAnsiCodePage;
end;

procedure TEditorOptions.SetAnsiCodePage(AValue: Integer);
begin
  SystemAnsiCodePage := AValue;
end;

procedure TEditorOptions.SetRecentFolders(AValue: TStringList);
begin
  if FRecentFolders <> AValue then
    FRecentFolders.Assign(AValue);
end;

procedure TEditorOptions.SetRecentProjects(const Value: TStringList);
begin
  if FRecentProjects <> Value then
    FRecentProjects.Assign(Value);
end;

{ TFileCategories }

function TFileGroups.CreateMask(CreateMaskProc: TCreateMaskProc): string;
  procedure AddIt(AGroup: TFileGroup);
  var
    i: integer;
    s: string;
    AExtensions: TStringList;
  begin
    if fgkBrowsable in AGroup.Kind then
    begin
      s := '';
      AExtensions := TStringList.Create;
      try
        AGroup.EnumExtensions(AExtensions);

        for i := 0 to AExtensions.Count - 1 do
        begin
          if s <> '' then
            s := s + ';';
          s := s + '*.' + AExtensions[i];
          if Result <> '' then
            Result := Result + ';';
          Result := Result + '*.' + AExtensions[i];
        end;
      finally
        AExtensions.Free;
      end;
    end;
  end;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    if (CreateMaskProc = nil) or (CreateMaskProc(Items[i])) then
      AddIt(Items[i]);
  end;
end;

function TFileGroups.CreateFilter(FullFilter: Boolean; FirstExtension: string; vGroup: TFileGroup; OnlyThisGroup: Boolean): string;
var
  aSupported: string;
  procedure AddIt(AGroup: TFileGroup);
  var
    i, n: integer;
    s: string;
    AExtensions: TStringList;
  begin
    if fgkBrowsable in AGroup.Kind then
    begin
      if FullFilter then
        if Result <> '' then
          Result := Result + '|';
      s := '';
      AExtensions := TStringList.Create;
      try
        AGroup.EnumExtensions(AExtensions);
        if (AGroup = vGroup) and (FirstExtension <> '') then
        begin
          n := AExtensions.IndexOf(FirstExtension);
          if n >= 0 then
            AExtensions.Move(n, 0);
        end;

        for i := 0 to AExtensions.Count - 1 do
        begin
          if s <> '' then
            s := s + ';';
          s := s + '*.' + AExtensions[i];
          if aSupported <> '' then
            aSupported := aSupported + ';';
          aSupported := aSupported + '*.' + AExtensions[i];
        end;
        if FullFilter then
          Result := Result + AGroup.Title + ' (' + s + ')|' + s;
      finally
        AExtensions.Free;
      end;
    end;
  end;
var
  i: integer;
  aDefaultGroup: TFileGroup;
begin
  aSupported := '';
  Result := '';
  if LeftStr(FirstExtension, 1) = '.' then
    FirstExtension := MidStr(FirstExtension, 2, MaxInt);
  if (vGroup <> nil) and OnlyThisGroup then
    AddIt(vGroup)
  else
  begin
    if vGroup <> nil then
      aDefaultGroup := vGroup
    else
      aDefaultGroup := Engine.Tendency.GetDefaultGroup;
    if aDefaultGroup <> nil then
      AddIt(aDefaultGroup);
    for i := 0 to Count - 1 do
    begin
      if (Items[i] <> aDefaultGroup) then
        AddIt(Items[i]);
    end;
  end;

  if FullFilter then //Add extra text for open/save dialogs
  begin
    if Result <> '' then
      Result := 'All files (Supported)|' + aSupported + '|' + Result;

    if Result <> '' then
      Result := Result + '|';
    Result := Result + 'Any file (*.*)|*.*';
  end
  else
    Result := aSupported;
end;

procedure TFileGroups.Add(vGroup: TFileGroup);
begin
  if IsExists(vGroup) then
    raise Exception.Create('Group is already exists: ' + vGroup.Name);
  inherited Add(vGroup);
end;

function TFileGroups.FindExtension(vExtension: string; vKind: TFileGroupKinds): TFileGroup;
var
  i, j: integer;
begin
  Result := nil;
  if LeftStr(vExtension, 1) = '.' then
    vExtension := Copy(vExtension, 2, MaxInt);
  if vExtension <> '' then
  begin
    for i := 0 to Count - 1 do
    begin
      if (vKind = []) or (vKind <= Items[i].Kind) then
      begin
        for j := 0 to Items[i].Extensions.Count - 1 do
        begin
          if SameText(Items[i].Extensions[j].Name, vExtension) then
          begin
            Result := Items[i];
            break;
          end;
        end;
        if Result = nil then
          for j := 0 to Items[i].ExtraExtensions.Count - 1 do
          begin
            if SameText(Items[i].ExtraExtensions[j].Name, vExtension) then
            begin
              Result := Items[i];
              break;
            end;
          end;
      end;
      if Result <> nil then
        break;
    end;
  end;
end;

{ TFileCategory }

constructor TFileCategory.Create(ATendency: TEditorTendency; const vName, vTitle: string; vKind: TFileCategoryKinds);
begin
  inherited Create(False); //childs is groups and already added to Groups and freed by it
  FTendency := ATendency;
  FName := vName;
  FTitle := vTitle;
  FKind := vKind;
end;

constructor TFileCategory.Create(ATendency: TEditorTendencyClass; const vName, vTitle: string; vKind: TFileCategoryKinds);
var
  lTendency: TEditorTendency;
begin
  lTendency := Engine.Tendencies.FindByClass(ATendency);
  Create(lTendency, vName, vTitle, vKind);
end;

procedure TFileCategory.EnumExtensions(vExtensions: TStringList);
var
  i: Integer;
begin
  for i  := 0 to Count - 1 do
  begin
    Items[i].EnumExtensions(vExtensions);
  end;
end;

function TFileCategory.GetExtensions: string;
var
  i: Integer;
  strings: TStringList;
begin
  strings := TStringList.Create;
  try
    for i  := 0 to Count - 1 do
    begin
      Items[i].EnumExtensions(strings);
    end;
    Result := '';
    for i := 0 to strings.Count -1 do
    begin
      if Result <> '' then
        Result := Result + ';';
      Result := strings[i];
    end;
  finally
    strings.Free;
  end;
end;

function TFileCategory.GetColorPrefix: string;
begin
  Result := '';
end;

function TFileCategory.FormatColor(Color: TColor): string;
begin
  Result := ColorToRGBHex(Color, GetColorPrefix);
end;

function TFileCategory.DeformatColor(Str: string): TColor;
begin
  Result := RGBHexToColor(Str, GetColorPrefix, false);
end;

procedure TFileCategory.Apply(AHighlighter: TSynCustomHighlighter; Attributes: TGlobalAttributes);
var
  i: Integer;
  M: TMap;
  G: TGlobalAttribute;
  Att: TSynHighlighterAttributes;
begin
  for i := 0 to AHighlighter.AttrCount -1 do
  begin
    Att := AHighlighter.Attribute[i];
    G := nil;

    M := Mapper.Find(Att.Name);
    if M <> nil then
      G := Attributes.Find(M.AttType);

    if G = nil then
      G := Attributes.Default;

    Att.Background := G.Background;
    Att.Foreground := G.Foreground;
    Att.Style := [];
  end;
end;

procedure TFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  if FCompletion = nil then
  begin
    FCompletion := TmneSynCompletion.Create(nil);
    FCompletion.Width := 340;
    FCompletion.OnExecute := @DoExecuteCompletion;
    FCompletion.ShortCut := scCtrl + VK_SPACE;
    FCompletion.CaseSensitive := False;
    //FCompletion.OnPaintItem
  end;
  FCompletion.AddEditor(vSynEdit);
end;

procedure TFileCategory.DoAddKeywords;
begin
end;

procedure TFileCategory.DoAddCompletion(AKeyword: string; AKind: integer);
begin
  Completion.ItemList.Add(AKeyword);
end;

procedure TFileCategory.InitEdit(vSynEdit: TCustomSynEdit);
begin
end;

destructor TFileCategory.Destroy;
begin
  FreeAndNil(FMapper);
  FreeAndNil(FCompletion);
  FreeAndNil(FHighlighter);
  inherited;
end;

procedure TFileCategory.EnumMenuItems(AddItems: TAddClickCallBack);
begin
end;

function TFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := DoCreateHighlighter;
end;

procedure TFileCategory.InitHighlighter;
var
  i: integer;
begin
  if FHighlighter = nil then
  begin
    FHighlighter := CreateHighlighter; //CreateHighlighter maybe return nil so check it again
    if FHighlighter <> nil then
    begin
      for i := 0 to FHighlighter.DividerDrawConfigCount - 1 do
        FHighlighter.DividerDrawConfig[i].MaxDrawDepth := 0;

      if FMapper = nil then
      begin
        FMapper := TMapper.Create;
        InitMappers;
        if FHighlighter.AttrCount < Mapper.Count then
          raise Exception.Create('Mapper count not equal to AttrCount for: ' + FHighlighter.ClassName + ' ' + IntToStr(FHighlighter.AttrCount) + ', Mapper: ' + IntToStr(Mapper.Count));
      end;
      //Apply(FHighlighter, Engine.Options.Profile.Attributes);
    end;
  end;
end;

function TFileCategory.Find(vName: string): TFileGroup;
begin
  Result := inherited Find(vName) as TFileGroup;
end;

function TFileCategory.GetItem(Index: Integer): TFileGroup;
begin
  Result := inherited Items[Index] as TFileGroup;
end;

function TFileCategory.GetMapper: TMapper;
begin
  Result := FMapper;
  if FMapper = nil then
    raise Exception.Create('Mapper is null');
end;

function TFileCategory.GetHighlighter: TSynCustomHighlighter;
begin
  InitHighlighter;
  Result := FHighlighter;
end;

function TFileCategory.GetIsText: Boolean;
begin
  Result := True;
end;

procedure TFileCategory.DoExecuteCompletion(Sender: TObject);
begin
end;

{ TEditorProject }

constructor TEditorProject.Create;
begin
  inherited Create;
  FDesktop := TEditorDesktop.Create;
  FDesktop.FProject := Self;
  FRunOptions := TRunProjectOptions.Create;
  FSaveDesktop := True;
end;

destructor TEditorProject.Destroy;
begin
  FreeAndNil(FRunOptions);
  FreeAndNil(FDesktop);
  FreeAndNil(FOptions);
  inherited;
end;

procedure TEditorProject.LoadFromFile(FileName: string);
begin
  FFileName := FileName;
  inherited LoadFromFile(FileName);
end;

procedure TEditorProject.SetTendencyName(AValue: string);
var
  aTendency: TEditorTendency;
begin
  FTendencyName := AValue;

  aTendency := Engine.Tendencies.Find(TendencyName);
  {if aTendency = nil then
    aTendency := Engine.DefaultTendency;} //TODO not sure

  Tendency := aTendency;

  Engine.UpdateState([ecsChanged, ecsProject]); //TODO move to caller
end;

procedure TEditorProject.SetTendency(AValue: TEditorTendency);
begin
  if FTendency <> AValue then
  begin
    FTendency := AValue;
    RecreateOptions;
  end;
end;

function TEditorProject.GetIsActive: Boolean;
begin
   Result := (Self <> nil) and not (Self is TDefaultProject)
end;

function TEditorSession.GetMainFolder: string;
var
  r: string;
begin
  if (Project.RunOptions.MainFolder <> '') then
  begin
    r := Project.RunOptions.MainFolder;
    r := Engine.EnvReplace(r, true);
    Result := ExpandToPath(r, Project.Path);
  end
  else
    Result := Project.Path;
end;

procedure TEditorSession.SetPanel(AValue: TControl);
begin
  if FPanel <> AValue then
  begin
    FPanel :=AValue;
  end;
end;

function TEditorProject.GetPath: string;
var
  s: string;
begin
  s := ExpandToPath(FileName, Application.Location); //to make it protable
  Result := ExtractFilePath(s);
end;

procedure TEditorProject.SetSCM(AValue: TEditorSCM);
begin
  if FSCM =AValue then exit;
  FreeAndNil(FSCM);
  FSCM :=AValue;
  Engine.UpdateState([ecsChanged, ecsProject]);
end;

procedure TEditorProject.RecreateOptions;
begin
  FOptions.Free;
  FOptions := FTendency.CreateOptions;
end;

procedure TEditorProject.RttiCreateObject(var vObject: TObject; vInstance: TObject; vObjectClass:TClass; const vClassName, vName: string);
begin
  inherited;
  if vObjectClass.InheritsFrom(vObjectClass) then
    vObject := TEditorSCMClass(vObjectClass).Create;
end;

procedure TEditorProject.Loaded(Failed: Boolean);
begin
  inherited;
  if (Tendency = nil) then
    TendencyName := 'Default'; //fix if TendencyName is empty
{  if not Failed and FSaveDesktop then
    Desktop.Load;}
end;

procedure TEditorProject.SetSCMClass(SCMClass: TEditorSCM);
begin
  if (SCMClass = nil) or not((SCM <> nil) and (SCM.ClassType = SCMClass.ClassType)) then
    SCM := nil;
  if (SCMClass <> nil) then
    SCM := TEditorSCMClass(SCMClass.ClassType).Create;
end;

procedure TEditorProject.Saving;
begin
  inherited;
  if FSaveDesktop then
    Desktop.Save;
end;

{ TFileGroup }

procedure TFileGroup.SetCategory(AValue: TFileCategory);
begin
  if FCategory <> AValue then
  begin
    if FCategory <> nil then
      FCategory.Extract(Self);
    FCategory :=AValue;
    if FCategory <> nil then
      FCategory.Add(Self);
  end;
end;

constructor TFileGroup.Create;
begin
  inherited;
  FExtensions := TEditorExtensions.Create;
  FExtraExtensions := TEditorExtensions.Create;
  FKind := [fgkBrowsable];
end;

procedure TFileGroup.EnumExtensions(vExtensions: TStringList; Kind: TFileGroupKinds);
  procedure AddIt(E: string);
  begin
    if vExtensions.IndexOf(E) < 0 then
      vExtensions.Add(E);
  end;

  procedure AddStrings(E: TStringList);
  var
    i: Integer;
  begin
    for i := 0 to E.Count -1 do
      AddIt(E[i]);
  end;

  procedure AddExtensions;
  var
    i: Integer;
  begin
    for i := 0 to Extensions.Count -1 do
      AddIt(Extensions[i].Name);
  end;
var
  s: string;
  lStrings: TStringList;
begin
  vExtensions.BeginUpdate;
  try
    AddExtensions;
    s := Engine.Options.ExtraExtensions.Values[Name];
    if s <> '' then
    begin
      lStrings := TStringList.Create;
      try
        StrToStrings(s, lStrings, [';'], [' ']);
        AddStrings(lStrings);
      finally
        lStrings.Free;
      end;
    end;
  finally
    vExtensions.EndUpdate;
  end;
end;

procedure TFileGroup.EnumExtensions(vExtensions: TEditorElements);
var
  lList:TStringList;
  i: Integer;
  lItem: TEditorElement;
begin
  lList := TStringList.Create;
  try
    EnumExtensions(lList);
    for i := 0 to lList.Count -1 do
    begin
      lItem := TEditorElement.Create;
      lItem.Name := lList[i];
      lItem.Title := lList[i];
      lItem.Description := Title;
      vExtensions.Add(lItem);
    end;
  finally
    lList.Free;
  end;
end;

destructor TFileGroup.Destroy;
begin
  FExtensions.Free;
  FExtraExtensions.Free;
  inherited;
end;

function TFileGroup.CreateEditorFile(vFiles: TEditorFiles): TEditorFile;
begin
  Result := FFileClass.Create(vFiles);
  Result.Group := Self;
end;

{ TFileGroups }

procedure TFileGroups.InternalAdd(GroupClass: TFileGroupClass; FileClass: TEditorFileClass; const Name, Title:string; Category: TFileCategoryClass; Extensions: array of string; Kind: TFileGroupKinds; Style: TFileGroupStyles);
var
  aCategory: TFileCategory;
  aGroup: TFileGroup;
  i: integer;
begin
  aCategory := Engine.Categories.FindByClass(Category);
  if aCategory = nil then
    raise Exception.Create('Can not find category ' + Category.ClassName);
  aGroup:= Find(Name);
  if aGroup <> nil then
    raise Exception.Create(Name + ' already exists');
  aGroup := GroupClass.Create;
  aGroup.FFileClass := FileClass;
  aGroup.FTitle := Title;
  aGroup.FName := Name;
  aGroup.FKind := Kind;
  aGroup.FStyle := Style;
  for i := 0 to Length(Extensions) - 1 do
    aGroup.Extensions.Add(Extensions[i]);
  aGroup.Category := aCategory;
  if fgkDefault in Kind then
    aCategory.Tendency.Groups.Insert(0, aGroup)
  else
    aCategory.Tendency.Groups.Add(aGroup);
  inherited Add(aGroup);
end;

procedure TFileGroups.Add(FileClass: TEditorFileClass; const Name, Title: string; Category: TFileCategoryClass; Extensions: array of string; Kind: TFileGroupKinds; Style: TFileGroupStyles);
begin
  InternalAdd(TFileGroup, FileClass, Name, Title, Category, Extensions, Kind, Style);
end;

function TFileGroups.Find(vName: string): TFileGroup;
begin
  Result := inherited Find(vName) as TFileGroup;
end;

function TFileGroups.Find(vName, vCategory: string): TFileGroup;
var
  i: integer;
begin
  Result := nil;
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, vName) and (Items[i].Category.Name = vCategory) then
      begin
        Result := Items[i];
        break;
      end;
    end;
end;

function TFileGroups.IsExists(AGroup: TFileGroup): Boolean;
var
  i: integer;
begin
  Result := false;
  if AGroup <> nil then
    for i := 0 to Count - 1 do
    begin
      if AGroup = Items[i] then
      begin
        Result := true;
        break;
      end;
    end;
end;

function TFileGroups.GetItem(Index: integer): TFileGroup;
begin
  Result := inherited Items[Index] as TFileGroup;
end;

destructor TEditorSession.Destroy;
begin
  Run.Stop;
  FreeAndNil(FRun);
  FreeAndNil(FOptions);
  FCachedVariables.Free;
  FCachedIdentifiers.Free;
  inherited;
end;

procedure TEditorSession.Changed;
begin
  FIsChanged := True;
  Engine.UpdateState([ecsChanged, ecsState, ecsRefresh, ecsProject]);
end;

procedure TEditorSession.SetRun(AValue: TmneRun);
begin
  if FRun =AValue then Exit;
  FRun :=AValue;
end;

constructor TEditorSession.Create;
begin
  inherited;
  FOptions := TEditorSessionOptions.Create;
  FRun := TmneRun.Create;
  FCachedVariables := THashedStringList.Create;
  FCachedIdentifiers := THashedStringList.Create;
end;

procedure TFileGroups.EnumExtensions(vExtensions: TStringList; Kind: TFileGroupKinds);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    if (Kind = []) or (Items[i].Kind = Kind) then
      Items[i].EnumExtensions(vExtensions);
  end;
end;

procedure TFileGroups.EnumExtensions(vExtensions: TEditorElements);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
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
  i: integer;
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

function TEditorDesktopFiles.GetItems(Index: integer): TEditorDesktopFile;
begin
  Result := inherited Items[Index] as TEditorDesktopFile;
end;

function TEditorDesktopFiles.IsExist(vName: string): Boolean;
begin
  Result := Find(vName) <> nil;
end;

{ TDebugSupportPlugin }

procedure CenterRect(var R1: TRect; R2: TRect);//from posDraws
begin
  OffsetRect(R1, ((R2.Right - R2.Left) div 2) - ((R1.Right - R1.Left) div 2) + (R2.Left - R1.Left), ((R2.Bottom - R2.Top) div 2) - ((R1.Bottom - R1.Top) div 2) + (R2.Top - R1.Top));
end;

procedure TSynDebugMarksPart.Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
var
  i, lh, iw: integer;
  aRect: TRect;

  procedure DrawIndicator(Line: integer; ImageIndex: integer);
  var
    r: TRect;
  begin
    Line := TSynEdit(SynEdit).RowToScreenRow(Line);
    if (Line >= FirstLine) and (Line <= LastLine) then
    begin
      aRect := AClip;
      aRect.Top := Line * lh;
      aRect.Bottom := aRect.Top + lh;
      r := aRect;
      r.Right := r.Left + iw;
      r.Bottom := r.Top + EditorResource.DebugImages.Width;
      CenterRect(r, aRect);
      EditorResource.DebugImages.Draw(Canvas, r.Left, r.Top, ImageIndex);
    end;
  end;
var
  aTendency: TEditorTendency;
begin
  aTendency := FEditorFile.Tendency; //from file Tendency
  //inherited;
  if aTendency.Debug <> nil then
  begin
    lh := TSynEdit(SynEdit).LineHeight;
    iw := EditorResource.DebugImages.Width;

    aTendency.Debug.Lock;
    try
      for i := 0 to aTendency.Debug.Breakpoints.Count - 1 do
      begin
        if SameText(aTendency.Debug.Breakpoints[i].FileName, FEditorFile.Name) then//need improve
          DrawIndicator(aTendency.Debug.Breakpoints[i].Line, DEBUG_IMAGE_BREAKPOINT);
      end;
    finally
      aTendency.Debug.Unlock;
    end;

    if (Engine.DebugLink.ExecutedControl = SynEdit) and (Engine.DebugLink.ExecutedLine >= 0) then
      DrawIndicator(Engine.DebugLink.ExecutedLine, DEBUG_IMAGE_EXECUTE);
  end;
end;

{ TEditorDesktop }

constructor TEditorDesktop.Create;
begin
  FFiles := TEditorDesktopFiles.Create(TEditorDesktopFile);
  FBreakpoints := TmneBreakpoints.create;
  FWatches := TmneWatches.create;
  inherited;
end;

destructor TEditorDesktop.Destroy;
begin
  FFiles.Free;
  FBreakpoints.Free;
  FWatches.Free;
  inherited;
end;

procedure TEditorDesktop.Load;
var
  i: integer;
  aItem: TEditorDesktopFile;
  aFile: TEditorFile;
begin
  if not Engine.FSafeMode and (FEngineLife >= engnStarting) then
  begin
    Engine.BeginUpdate;
    try
      Engine.Files.CloseAll;
      for i := 0 to Files.Count - 1 do
      begin
        aItem := Files[i];
        if FileExists(aItem.FileName) then
        begin
          aFile := Engine.Files.LoadFile(aItem.FileName, False);
          if aFile <> nil then
            aFile.Assign(aItem);
        end;
      end;
      Engine.Files.SetActiveFile(Files.CurrentFile);
      Engine.BrowseFolder := Files.CurrentFolder;

      if (FProject.Tendency <> nil) and (Project.Tendency.Debug <> nil) then
      begin
        Project.Tendency.Debug.Lock;
        try
         Project.Tendency.Debug.Breakpoints.Clear;
          for i := 0 to Breakpoints.Count - 1 do
          begin
            Project.Tendency.Debug.Breakpoints.Add(Breakpoints[i].FileName, Breakpoints[i].LineNo);
          end;

          Project.Tendency.Debug.Watches.Clear;
          for i := 0 to Watches.Count - 1 do
          begin
            Project.Tendency.Debug.Watches.Add(Watches[i].Name);
          end;
        finally
          Project.Tendency.Debug.Unlock;
        end;
        Engine.UpdateState([ecsDebug]);
      end;
    finally
      Engine.EndUpdate;
      Files.Clear;
    end;
  end
end;

procedure TEditorDesktop.Save;
var
  i: integer;
  aItem: TEditorDesktopFile;
  aFile: TEditorFile;
begin
  Breakpoints.Clear;
  Watches.Clear;
  if (FProject.Tendency <> nil) and (Project.Tendency.Debug <> nil) then
  begin
    Project.Tendency.Debug.Lock;
    try
      for i := 0 to Project.Tendency.Debug.Breakpoints.Count - 1 do
      begin
        Breakpoints.Add(Project.Tendency.Debug.Breakpoints[i].FileName, Project.Tendency.Debug.Breakpoints[i].Line);
      end;

      for i := 0 to Project.Tendency.Debug.Watches.Count - 1 do
      begin
        Watches.Add(Project.Tendency.Debug.Watches[i].Name);
      end;
    finally
      Project.Tendency.Debug.Unlock;
    end;
  end;

  Files.CurrentFolder := Engine.BrowseFolder;
  Files.Clear;

  if Engine.Files.Current <> nil then
    Files.CurrentFile := Engine.Files.Current.Name
  else
    Files.CurrentFile := '';

  for i := 0 to Engine.Files.Count - 1 do
  begin
    aFile := Engine.Files[i];
    aItem := Files.Add(aFile.Name);
    aFile.AssignTo(aItem);
  end;
end;

{ TEditorMessages }

function TEditorMessages.GetText(Index: integer): string;
begin
  if Index < Count then
    Result := Items[Index].Text
  else
    Result := '';
end;

{ TEditorMessagesList }

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

finalization
  FreeAndNil(FEngine);
end.
