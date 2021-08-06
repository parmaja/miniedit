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
  SynEdit, SynEditTextTrimmer, SynTextDrawer, SynGutterBase, SynEditPointClasses, SynMacroRecorder,
  dbgpServers, Masks, mnXMLRttiProfile, mnXMLUtils, mnClasses, fgl,
  mnUtils, LCLType, EditorClasses, EditorRun;

type
  TThreeStates = (stateNone, stateFalse, stateTrue);

  TSynCompletionType = (ctCode, ctHint, ctParams);

  TEditorEngine = class;
  TVirtualCategory = class;
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
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteCommand(Command: TSynEditorCommand; const AChar: TUTF8Char; Data: pointer); override;
  end;

  { TEditorDesktopFile }

  TEditorDesktopFile = class(TCollectionItem)
  private
    FFileName: string;
    FFileParams: string;
    FCaretY: integer;
    FCaretX: integer;
    FTopLine: integer;
  public
    constructor Create(ACollection: TCollection); override;
  published
    property FileName: string read FFileName write FFileName;
    property FileParams: string read FFileParams write FFileParams;
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
    fgkBinary, // Like Database SQLite
    fgkExecutable,//You can guess what is it :P
    fgkShell,//TODO: Executable but hi priority, override the main file or project tendency
    fgkDebugee, //TODO
    fgkMain,//this can be the main file for project
    fgkResult,//Result file, generated, like: exe or .o or .hex
    fgkBrowsable,//When open file show it in the extension list
    fgkAssociated, //Editor can be the editor of this files, like .php, .inc, but .txt is not
    //fgkTemporary, //No need to save it to run, but save it when user ask to save
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
    capExecute, //Can run this file
    capStop, //Stop executing or compiling
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

  TEditorTendency = class abstract(TTendency)
  private
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
    function CreateProject: TEditorProject; virtual;
    procedure EnumRunCommands(Items: TStrings); virtual;
    function GetDefaultGroup: TFileGroup;
    //OSDepended: When save to file, the filename changed depend on the os system name
    procedure Prepare;
    procedure Unprepare;
    //
    property Capabilities: TEditorCapabilities read FCapabilities;
    function CanExecute: Boolean;
    property Groups: TFileGroups read FGroups;
    property IsDefault: Boolean read GetIsDefault;
    property OutputExtension: string read FOutputExtension write FOutputExtension;
  published
    //Override options
    property OverrideEditorOptions: Boolean read FOverrideEditorOptions write FOverrideEditorOptions default False;
    property TabWidth: Integer read FTabWidth write FTabWidth default 4;
    property IndentMode: TIndentMode read FIndentMode write FIndentMode default idntTabsToSpaces;

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
    SharedLib: Boolean;
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

    property SharedLib: Boolean read FInfo.SharedLib write FInfo.SharedLib; //dll or so shared lib
  end;

  TCreateMaskProc = function(vGroup: TFileGroup): Boolean of object;

  { TEditorProject }

  TEditorProject = class abstract(TmnXMLProfile)
  private
    FFileFilter: string;
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
    function CreateMask(CreateMaskProc: TCreateMaskProc): string;
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

    property FileFilter: string read FFileFilter write FFileFilter;
  end;

  TTendencyProject = class(TEditorProject)
  public
  published
    property FileFilter;
  end;

  TRunProject = class(TTendencyProject)
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
    FIsChanged: Boolean;
    FFileAge: Integer;
    FFileSize: int64;
    FGroup: TFileGroup;
    FParams: string;
    FRelated: string;
    FLinesMode: TEditorLinesMode;
    FIsTemporary: Boolean;
    FTitle: string;
    function GetCapability: TEditCapability;
    function GetIsText: Boolean;
    function GetNakeName: string;
    function GetPureName: string;
    function GetExtension: string;
    function GetPath: string;
    function GetTendency: TEditorTendency;
    procedure SetFileEncoding(AValue: string);
    procedure SetGroup(const Value: TFileGroup);
    procedure SetIsChanged(const Value: Boolean);
    procedure SetIsNew(AValue: Boolean);
    function GetLinesModeAsText: string;
    procedure SetLinesMode(const Value: TEditorLinesMode);
    procedure SetExtension(AValue: string);
    procedure SetNakeName(AValue: string);
    procedure SetPureName(AValue: string);
  protected
    procedure InitContents; virtual;
    procedure GroupChanged; virtual;
    function GetIsReadonly: Boolean; virtual;
    procedure SetIsReadonly(const Value: Boolean); virtual;
    function GetContent: TWinControl; virtual;
    function GetControl: TWinControl; virtual;
    function GetSynEdit: TSynEdit; virtual;
    procedure DoGetCapability(var vCapability: TEditCapability); virtual;

  protected
    procedure Edit;
    procedure DoEdit(Sender: TObject); virtual;
    procedure DoStatusChange(Sender: TObject; Changes: TSynStatusChanges); virtual;
    procedure UpdateAge; virtual;
    procedure NewContent; virtual;
    procedure DoLoad(FileName: string); virtual; abstract;
    procedure DoSave(FileName: string); virtual; abstract;
    function Execute(RunInfo: TmneRunInfo): Boolean; virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure LoadFromFile(FileName: string);
    procedure Load;
    procedure SaveToFile(FileName: string);
    procedure Save(Force: Boolean; Extension: string = ''; AsNewFile: Boolean = False); virtual;
    procedure Save;

    //Utils for SynEdit loading/saving
    procedure ContentsLoadFromStream(SynEdit: TSynEdit; AStream: TStream);
    procedure ContentsSaveToStream(SynEdit: TSynEdit; AStream: TStream);
    procedure ContentsLoadFromFile(SynEdit: TSynEdit; FileName: string); //used if u have synedit
    procedure ContentsSaveToFile(SynEdit: TSynEdit; FileName: string);

    procedure Rename(ToNakeName: string); //only name not with the path
    procedure Delete; //only name not with the path
    function CanExecute: Boolean;

    procedure Show; virtual; //Need to activate it after show to focus editor
    function Visible: Boolean;
    procedure Activate; virtual;
    function Activated: Boolean;
    procedure Close;
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
    function GetCaption: string; virtual; //Name or Title or *
    function GetLanguageName: string; virtual; //TODO need to get more good name to this function
    procedure SetLine(Line: Integer); virtual;
    procedure SetHighlightLine(AValue: Integer); virtual;

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
    property Params: string read FParams write FParams;
    property Title: string read FTitle write FTitle; //used only if Name is empty for temporary files
    property NakeName: string read GetNakeName write SetNakeName; //no path with ext
    property PureName: string read GetPureName write SetPureName; //no path no ext
    property Extension: string read GetExtension write SetExtension;
    property Path: string read GetPath;
    property Tendency: TEditorTendency read GetTendency;
    property Related: string read FRelated write FRelated;
    property IsChanged: Boolean read FIsChanged write SetIsChanged;
    property IsNew: Boolean read FIsNew write SetIsNew default False;
    property IsTemporary: Boolean read FIsTemporary write FIsTemporary default False;
    property IsReadOnly: Boolean read GetIsReadonly write SetIsReadonly;
    property Group: TFileGroup read FGroup write SetGroup;

    property Content: TWinControl read GetContent; //Container of SynEdit or Grids or Images and all child contrls
    //Control we need to know about it to focus it after open file or changing focus by F6
    property Control: TWinControl read GetControl; //Control if available, maybe Grid or SynEdit or same the content
    property SynEdit: TSynEdit read GetSynEdit; //SynEdit of available
  published
  end;

  { TControlEditorFile }

  TControlEditorFile = class abstract(TEditorFile, IControlEditor)
  private
  protected
  public
  end;

  { TSyntaxEditorFile }

  TSyntaxEditorFile = class abstract(TEditorFile, ITextEditor)
  private
    FHighlightLine: Integer;
  protected
    LastGotoLine: Integer;
    function GetIsReadonly: Boolean; override;
    procedure SetIsReadonly(const Value: Boolean); override;
    function GetContent: TWinControl; override;
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
    procedure BeforeDestruction; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Find; override;
    procedure FindNext; override;
    procedure FindPrevious; override;
    procedure Replace; override;
    procedure Refresh; override;
    procedure SetHighlightLine(AValue: Integer); override;
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
    property HighlightLine: Integer read FHighlightLine write SetHighlightLine;
  end;

  { TTextEditorFile }

  TTextEditorFile = class(TSyntaxEditorFile)
  private
    FSynEdit: TSynEdit;
  protected
    procedure InitContents; override;
    function GetSynEdit: TSynEdit; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  end;

  { TSourceEditorFile }

  TSourceEditorFile = class(TTextEditorFile, IExecuteEditor, IWatchEditor)
  public
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

    function InternalOpenFile(FileName, FileParams: string; AppendToRecent: Boolean): TEditorFile;
    function LoadFile(vFileName, vFileParams: string; AppendToRecent: Boolean = true): TEditorFile;
    function ShowFile(vFileName: string): TEditorFile; overload; //open it without add to recent, for debuging
    function ShowFile(const FileName: string; Line: integer): TEditorFile; overload;
    function OpenFile(vFileName: string; vFileParams: string = ''; ActivateIt: Boolean = false): TEditorFile;
    procedure SetCurrentIndex(Index: integer; vRefresh: Boolean);
    function New(vGroup: TFileGroup = nil): TEditorFile; overload;
    function New(GroupName: string): TEditorFile; overload;

    procedure Open;
    procedure Save(Force: Boolean);
    procedure SaveAll(Force: Boolean);
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

  { TRecentItem }

  TRecentItem = class(TmnXMLItem)
  private
    FName: string;
    FParams: string;
  public
  published
    property Name: string read FName write FName;
    property Params: string read FParams write FParams;
    constructor Create; overload;
    constructor Create(AName: string; AParams: string = ''); overload;
    procedure Assign(Source: TPersistent); override;
  end;

  { TRecentItems }

  TRecentItems = class(specialize GXMLItems<TRecentItem>)
  private
    FMax: Integer;
  protected
  public
    constructor Create(AMax: Integer);
    function IndexOf(AName: string): Integer;
    function Find(AName: string): TRecentItem;
    procedure Insert(AName: string; AParams: string);
    procedure Add(AName: string; AParams: string);
    property Max: Integer read FMax write FMax;
  end;

  TSynBreakPointItem = class(TSynObjectListItem)
  public
    IsBreakPoint: Boolean;
  end;

  TSortFolderFiles = (srtfByNames, srtfByExt);
  TShowFolderFiles = (sffRelated, sffProject, sffKnown, sffAll);
  TEditorFileClass = class of TEditorFile;
  TAutoStartDebug = (asdNo, asdStartup, asdRun);

  TOnEngineChanged = procedure of object;

  { TEditorOptions }

  TEditorOptions = class(TmnXMLProfile)
  private
    FAutoOpenProject: Boolean;
    FFallbackToText: Boolean;
    FIgnoreNames: string;
    FLastFolder: string;
    FLastProject: string;
    FShowFolder: Boolean;
    FShowToolbar: Boolean;
    FShowFolderFiles: TShowFolderFiles;
    FSortFolderFiles: TSortFolderFiles;
    FCustom: TStringList;
    FWindowMaxmized: Boolean;
    FBoundRect: TRect;
    FShowMessages: Boolean;
    FCollectAutoComplete: Boolean;
    FCollectTimeout: QWORD;
    FReplaceHistory: TStringList;
    FAutoStartDebug: TAutoStartDebug;

    FMessagesHeight: integer;
    FFoldersPanelWidth: integer;

    FExtraExtensions: TStringList;
    FSearchFolderHistory: TStringList;
    FSearchHistory: TStringList;
    FProfile: TEditorProfile;

    FRecentFiles: TRecentItems;
    FRecentDatabases: TRecentItems;
    FRecentFolders: TRecentItems;
    FRecentProjects: TRecentItems;
    FProjects: TRecentItems;
    function GetAnsiCodePage: Integer;
    procedure SetAnsiCodePage(AValue: Integer);
    procedure SetRecentFiles(const Value: TRecentItems);
    procedure SetRecentDatabases(AValue: TRecentItems);
    procedure SetRecentFolders(AValue: TRecentItems);
    procedure SetRecentProjects(const Value: TRecentItems);
    procedure SetProjects(const Value: TRecentItems);
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
    property RecentFiles: TRecentItems read FRecentFiles write SetRecentFiles;
    property RecentDatabases: TRecentItems read FRecentDatabases write SetRecentDatabases;
    property RecentFolders: TRecentItems read FRecentFolders write SetRecentFolders;
    property RecentProjects: TRecentItems read FRecentProjects write SetRecentProjects;
    property Projects: TRecentItems read FProjects write SetProjects;

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
    property AutoStartDebug: TAutoStartDebug read FAutoStartDebug write FAutoStartDebug default asdNo;
    property FallbackToText: Boolean read FFallbackToText write FFallbackToText default False;
    property AnsiCodePage: Integer read GetAnsiCodePage write SetAnsiCodePage;
    property WindowMaxmized: Boolean read FWindowMaxmized write FWindowMaxmized default False;
    property WindowTop: Integer read FBoundRect.Top write FBoundRect.Top;
    property WindowLeft: Integer read FBoundRect.Left write FBoundRect.Left;
    property WindowRight: Integer read FBoundRect.Right write FBoundRect.Right;
    property WindowBottom: Integer read FBoundRect.Bottom write FBoundRect.Bottom;
    property Custom: TStringList read FCustom;
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

  TFileCategoryKind = (
    fckPublish //idk not used
  );
  TFileCategoryKinds = set of TFileCategoryKind;

  { TVirtualCategory }

  TVirtualCategory = class(TEditorElements)
  private
    FImageName: string;
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

    function OpenFile(vGroup: TFileGroup; vFiles: TEditorFiles; vFileName, vFileParams: string): TEditorFile; virtual;
  public
    constructor Create(ATendency: TEditorTendency; const vName, vTitle: string; vKind: TFileCategoryKinds = []; vImageName: string = ''); virtual;
    constructor Create(ATendency: TEditorTendencyClass; const vName, vTitle: string; vKind: TFileCategoryKinds = []; vImageName: string = ''); virtual;
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
    property ImageName: string read FImageName write FImageName;
    property Items[Index: Integer]: TFileGroup read GetItem; default;
  end;

  TFileCategoryClass = class of TVirtualCategory;

  { TFileCategories }

  TFileCategories = class(specialize TmnNamedObjectList<TVirtualCategory>)
  public
    function FindByClass(CategoryClass: TFileCategoryClass): TVirtualCategory;
    function Add(vCategory: TVirtualCategory): Integer;
  end;

  { TFileCategory }

  TFileCategory = class(TVirtualCategory) //base class
  protected
    function OpenFile(vGroup: TFileGroup; vFiles: TEditorFiles; vFileName, vFileParams: string): TEditorFile; override;
  public
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
    FCategory: TVirtualCategory;
    FStyle: TFileGroupStyles;
    function GetExtension: string;
    procedure SetCategory(AValue: TVirtualCategory);
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    function OpenFile(vFiles: TEditorFiles; vFileName, vFileParams: string): TEditorFile; virtual;
    procedure EnumExtensions(vExtensions: TStringList; Kind: TFileGroupKinds = []);
    procedure EnumExtensions(vExtensions: TEditorElements);
    property Extension: string read GetExtension; //first one as default extension, do not use the name of group
    property Category: TVirtualCategory read FCategory write SetCategory;
    property Extensions: TEditorExtensions read FExtensions;
    property ExtraExtensions: TEditorExtensions read FExtraExtensions;
    property Kind: TFileGroupKinds read FKind write FKind;
    property Style: TFileGroupStyles read FStyle write FStyle;
    property FileClass: TEditorFileClass read FFileClass; //TODO move to TFileCategory
  end;

  TFileGroupClass = class of TFileGroup;

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

    function FindGroup(vFullName: string; vKind: TFileGroupKinds = []): TFileGroup;
    function OpenFile(vFiles: TEditorFiles; vFileName, vFileParams: string; FallBackGroup: string): TEditorFile;
    //FullFilter return title of that filter for open/save dialog boxes
    function CreateFilter(FullFilter: Boolean = True; FirstExtension: string = ''; vGroup: TFileGroup = nil; OnlyThisGroup: Boolean = true): string;
    function CreateMask(CreateMaskProc: TCreateMaskProc): string;
    procedure Add(vGroup: TFileGroup);
    //First extension in array, is the default extension for the group, do not use name of group as extension
    //ImageName is group name extension already registered in resource
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

    procedure SetProject(const Value: TEditorProject; LoadFiles: Boolean = True);
    //Is project opened
    property Active: Boolean read GetActive;
    //Current is the opened project, if it is a nil that mean there is no opened project.
    property Project: TEditorProject read FProject;
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

    procedure Prepare(vSafeMode: Boolean = False);
    procedure Start; //After Createing MainForm
    procedure LoadOptions;
    procedure UpdateOptions;
    procedure SaveOptions;
    procedure Shutdown;
    function IsShutdown: Boolean;

    //I used it for search in files
    function SearchReplace(const FileName: string; const ALines: TStringList; const ASearch, AReplace: string; OnFoundEvent: TOnFoundEvent; AOptions: TSynSearchOptions): integer;
    //Recent
    procedure ProcessRecentFile(const FileName: string; FileParams: string = '');
    procedure RemoveRecentFile(const FileName: string);
    procedure ProcessRecentDatabase(const AliasName: string; FileParams: string = '');
    procedure RemoveRecentDatabase(const AliasName: string);
    procedure ProcessRecentFolder(const FileName: string; FileParams: string = '');
    procedure RemoveRecentFolder(const FileName: string);
    procedure ProcessRecentProject(const FileName: string; FileParams: string = '');
    procedure RemoveRecentProject(const FileName: string);

    procedure ProcessProject(const FileName: string; FileParams: string = '');
    procedure RemoveProject(const FileName: string);
    function NewProject: Boolean;
    function ChooseTendency(var vTendency: TEditorTendency): Boolean;

    procedure BeginUpdate;
    //Silent do not trigger just set the flag
    procedure Update(State: TEditorChangeStates; Silent: Boolean = False);
    procedure EndUpdate;
    property Updating: Boolean read GetUpdating;
    property UpdateState: TEditorChangeStates read FUpdateState;

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

    procedure SaveAll(Force: Boolean);
    procedure Run;
    procedure Execute;

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

function ResetUpdate(State: TEditorChangeState; var InStates: TEditorChangeStates): Boolean;

function SelectFolder(const Caption: string; const Root: string; var Directory: string): Boolean;
procedure SpliteStr(S, Separator: string; var Name, Value: string);

procedure SaveAsUnix(Strings: TStrings; Stream: TStream);
procedure SaveAsWindows(Strings: TStrings; Stream: TStream);
procedure SaveAsMAC(Strings: TStrings; Stream: TStream);
procedure SaveAsMode(const FileName: string; Mode: TEditorLinesMode; Strings: Tstrings);

function ConvertToLinesMode(Mode: TEditorLinesMode; Contents: string): string;
function DetectLinesMode(const Contents: string): TEditorLinesMode;

//EnumFileList return false if canceled by callback function
type
  TFileFindTypes = set of (fftDir, fftFile);

function EnumFileList(const Root, Masks, Ignore: string; Callback: TEnumFilesCallback; AObject: TObject; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean; Types: TFileFindTypes = [fftFile]): Boolean;
procedure EnumFileList(const Root, Masks, Ignore: string; Strings: TStringList; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean);
procedure EnumDirList(const Root, Masks, Ignore: string; Strings: TStringList; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean);

function ConvertIndents(const Contents: string; TabWidth: integer; Options: TIndentMode = idntTabsToSpaces): string;

const
  sEnvVarChar = '?';

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
  SelectList, mneProjectOptions,
  mneResources, mnMsgBox, GUIMsgBox;

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
  if FEngine = nil then
  begin
    if FEngineLife = engnShutdowned  then
      raise Exception.Create('Engine in shutdown?');
    FEngine := TEditorEngine.Create;
  end;
  Result := FEngine;
end;

function ResetUpdate(State: TEditorChangeState; var InStates: TEditorChangeStates): Boolean;
begin
  Result := State in InStates;
  InStates := InStates - [State];
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

{ TFileCategory }

function TFileCategory.OpenFile(vGroup: TFileGroup; vFiles: TEditorFiles; vFileName, vFileParams: string): TEditorFile;
begin
  Result := vGroup.FFileClass.Create(vFiles);
  Result.Group := vGroup;
  if (vFileName <> '') then
    Result.LoadFromFile(vFileName);
end;

{ TControlFileGroup }

{ TEditorDesktopFile }

constructor TEditorDesktopFile.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  CaretX := 1;
  CaretY := 1;
  TopLine := 1;
end;

{ TRecentItem }

constructor TRecentItem.Create(AName: string; AParams: string);
begin
  Create;
  Name := AName;
  Params := AParams;
end;

procedure TRecentItem.Assign(Source: TPersistent);
begin
  if Source is TRecentItem then
  begin
    FName := (Source as TRecentItem).Name;
    FParams := (Source as TRecentItem).Params;
  end
  else
    inherited;
end;

constructor TRecentItem.Create;
begin
  inherited Create;
end;

{ TRecentItems }

constructor TRecentItems.Create(AMax: Integer);
begin
  inherited Create;
  FMax := AMax;
end;

function TRecentItems.IndexOf(AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, AName) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TRecentItems.Find(AName: string): TRecentItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, AName) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

procedure TRecentItems.Insert(AName: string; AParams: string);
begin
  inherited Insert(0, TRecentItem.Create(AName, AParams));
  while Count > Max do
    Delete(Max);
end;

procedure TRecentItems.Add(AName: string; AParams: string);
var
  i: integer;
begin
  i := IndexOf(AName);
  if i >= 0 then
    Move(i, 0)
  else
    Insert(AName, AParams);
end;

{ TTextEditorFile }

procedure TTextEditorFile.InitContents;
begin
  inherited;
  FSynEdit := TmnSynEdit.Create(Engine.FilePanel);
  SynEdit.Visible := False;
  SynEdit.Parent := Engine.FilePanel;
end;

function TTextEditorFile.GetSynEdit: TSynEdit;
begin
  Result := FSynEdit;
end;

constructor TTextEditorFile.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

destructor TTextEditorFile.Destroy;
begin
  FreeAndNil(FSynEdit);
  inherited Destroy;
end;

{ TSyntaxEditorFile }

constructor TmnSynEdit.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  i := Keystrokes.FindKeycode(VK_NEXT, [ssCtrl]);
  if i >= 0 then
    Keystrokes.Delete(i);
  i := Keystrokes.FindKeycode(VK_PRIOR, [ssCtrl]);
  if i >= 0 then
    Keystrokes.Delete(i);
end;

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

function TFileCategories.FindByClass(CategoryClass: TFileCategoryClass): TVirtualCategory;
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

function TFileCategories.Add(vCategory: TVirtualCategory): Integer;
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

{ TSyntaxEditorFile }

procedure TSyntaxEditorFile.SetHighlightLine(AValue: Integer);
begin
  if FHighlightLine <> AValue then
  begin
    SynEdit.InvalidateLine(FHighlightLine);
    FHighlightLine :=AValue;
    SynEdit.InvalidateLine(FHighlightLine);
  end;
end;

function TSyntaxEditorFile.GetIsReadonly: Boolean;
begin
  Result := SynEdit.ReadOnly;
end;

procedure TSyntaxEditorFile.SetIsReadonly(const Value: Boolean);
begin
  SynEdit.ReadOnly := Value;
end;

function TSyntaxEditorFile.GetContent: TWinControl;
begin
  Result := SynEdit;
end;

procedure TSyntaxEditorFile.DoLoad(FileName: string);
begin
  ContentsLoadFromFile(SynEdit, FileName);
end;

procedure TSyntaxEditorFile.DoSave(FileName: string);
begin
  ContentsSaveToFile(SynEdit, FileName);
end;

procedure TSyntaxEditorFile.GroupChanged;
begin
  if Group <> nil then
    SynEdit.Highlighter := FGroup.Category.Highlighter;
  inherited;
  if Group <> nil then
  begin
    FGroup.Category.InitCompletion(SynEdit);

    //FSynEdit.Gutter.SeparatorPart(0).Index := FSynEdit.Gutter.Parts.Count - 1;//To make this part last one

    //if (fgkExecutable in FGroup.Kind) then //show it make it more comfirtable
      with TSynDebugMarksPart.Create(SynEdit.Gutter.Parts) do
      begin
        FEditorFile := Self;
        AutoSize := False;
        Width := EditorResource.DebugImages.Width + DEBUG_IMAGE_MARGINES;
      end;

    FGroup.Category.InitEdit(SynEdit);
  end;
end;

procedure TSyntaxEditorFile.DoEdit(Sender: TObject);
begin
  inherited;
  HighlightLine := -1;
end;

procedure TSyntaxEditorFile.DoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if ([scReadOnly, scCaretX, scCaretY, scLeftChar, scTopLine, scSelection] * Changes) <> [] then
    HighlightLine := -1;
  {if ([scCaretY] * Changes) <> [] then
    (Sender as TSynEdit).InvalidateLine((Sender as TSynEdit).CaretX);}
  inherited;
end;

procedure TSyntaxEditorFile.DoGutterClickEvent(Sender: TObject; X, Y, Line: integer; Mark: TSynEditMark);
var
  aLine: integer;
begin
  if (Tendency.Debugger <> nil) and (fgkExecutable in Group.Kind) then
  begin
    aLine := SynEdit.PixelsToRowColumn(Point(X, Y)).y;
    DebugManager.Enter;
    try
      Tendency.Debugger.Breakpoints.Toggle(Name, aLine);
    finally
      DebugManager.Leave;
    end;
    SynEdit.InvalidateLine(aLine);
  end;
end;

procedure TSyntaxEditorFile.DoSpecialLineMarkup(Sender: TObject; Line: integer; var Special: Boolean; Markup: TSynSelectedColor);
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

procedure TSyntaxEditorFile.DoGetCapability(var vCapability: TEditCapability);
begin
  inherited;

  if SynEdit.SelAvail then
    vCapability := vCapability + [ecpAllowCopy];

  if SynEdit.CanPaste then
    vCapability := vCapability + [ecpAllowPaste];
end;

procedure TSyntaxEditorFile.SynEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

constructor TSyntaxEditorFile.Create(ACollection: TCollection);
begin
  inherited;
  { There is more assigns in TEditorFile.SetGroup and TEditorProfile.Assign}
  FHighlightLine := -1;
  SynEdit.OnChange := @DoEdit;
  SynEdit.OnStatusChange := @DoStatusChange;
  SynEdit.OnGutterClick := @DoGutterClickEvent;
  SynEdit.OnSpecialLineMarkup := @DoSpecialLineMarkup;
  SynEdit.BookMarkOptions.BookmarkImages := EditorResource.BookmarkImages;
  SynEdit.OnReplaceText := @Engine.DoReplaceText;
  SynEdit.OnKeyDown := @SynEditKeyDown;

  SynEdit.Lines.SkipLastLineBreak := True;
  SynEdit.TrimSpaceType := settLeaveLine;
  SynEdit.BoundsRect := Engine.FilePanel.ClientRect;
  SynEdit.Font.Quality := fqDefault;
  SynEdit.BorderStyle := bsNone;
  SynEdit.ShowHint := True;
  SynEdit.WantTabs := True;
  SynEdit.MaxLeftChar := 80;
  SynEdit.ScrollBars := ssAutoBoth;

  with SynEdit.Keystrokes.Add do
  begin
    Key       := VK_DELETE;
    Shift     := [ssCtrl];
    Command   := ecDeleteWord;
  end;

  with SynEdit.Keystrokes.Add do
  begin
    Key       := VK_K;
    Shift     := [ssCtrl];
    Key2       := VK_O;
    Shift2     := [ssCtrl];
    Command   := ecLowerCaseBlock;
  end;

  with SynEdit.Keystrokes.Add do
  begin
    Key       := VK_K;
    Shift     := [ssCtrl];
    Key2       := VK_P;
    Shift2     := [ssCtrl];
    Command   := ecUpperCaseBlock;
  end;

  Engine.MacroRecorder.AddEditor(SynEdit);
end;

destructor TSyntaxEditorFile.Destroy;
begin
  inherited;
end;

procedure TSyntaxEditorFile.BeforeDestruction;
begin
  Engine.MacroRecorder.RemoveEditor(SynEdit);
  inherited BeforeDestruction;
end;

procedure TSyntaxEditorFile.Assign(Source: TPersistent);
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

procedure TSyntaxEditorFile.AssignTo(Dest: TPersistent);
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

procedure TSyntaxEditorFile.Find;
begin
  inherited;
  ShowSearchForm(SynEdit, Engine.Options.SearchHistory, Engine.Options.ReplaceHistory, False);
end;

procedure TSyntaxEditorFile.FindNext;
begin
  inherited;
  SearchTextNext(SynEdit);
end;

procedure TSyntaxEditorFile.FindPrevious;
begin
  inherited;
  SearchTextPrevious(SynEdit);
end;

procedure TSyntaxEditorFile.Replace;
begin
  inherited;
  ShowSearchForm(SynEdit, Engine.Options.SearchHistory, Engine.Options.ReplaceHistory, True);
end;

procedure TSyntaxEditorFile.Refresh;
begin
  inherited;
  SynEdit.Refresh;
end;

function TSyntaxEditorFile.GetHint(HintControl: TControl; CursorPos: TPoint; out vHint: string): Boolean;
var
  v, s, t: string;
begin
  if capEval in Tendency.Capabilities then
  begin
    Result := EvalByMouse(CursorPos, v, s, t);
    if Result then
      vHint := v + ': ' + t + '=' + #13#10 + s;
  end
  else
    Result := False;
end;

function TSyntaxEditorFile.GetGlance: string;
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

function TSyntaxEditorFile.EvalByMouse(p: TPoint; out v, s, t: string): boolean;
var
  l: variant;
  lp: TPoint;
begin
  if Tendency.Debugger <> nil then
  begin
    if SynEdit.SelAvail then
      v := SynEdit.SelText
    else
    begin
      lp := SynEdit.PixelsToLogicalPos(p);
      v := Trim(SynEdit.GetWordAtRowCol(lp));
      //v := Trim(SynEdit.GetWordAtRowCol(SynEdit.LogicalCaretXY));
    end;
    Result := (v <> '') and Tendency.Debugger.Watches.GetValue(v, l, t, False);
    s := VarToStrDef(l, '');
  end
  else
    Result := False;
end;

function TSyntaxEditorFile.EvalByCursor(out v, s, t: string): boolean;
var
  l: variant;
begin
  if Tendency.Debugger <> nil then
  begin
    if not SynEdit.SelAvail then
      v := Trim(SynEdit.GetWordAtRowCol(SynEdit.LogicalCaretXY))
    else
      v := SynEdit.SelText;
    Result := (v <> '') and Tendency.Debugger.Watches.GetValue(v, l, t, False);
    s := VarToStrDef(l, '');
  end
  else
    Result := False;
end;

procedure TSyntaxEditorFile.UpdateAge;
begin
  inherited;
  if SynEdit <> nil then
  begin
    SynEdit.Modified := False;
    SynEdit.MarkTextAsSaved;
  end;
end;

function TSyntaxEditorFile.GetLanguageName: string;
begin
  if (SynEdit <> nil) and (SynEdit.Highlighter <> nil) then
    Result := SynEdit.Highlighter.GetLanguageName
  else
    Result := inherited;
end;

procedure TSyntaxEditorFile.Copy;
begin
  SynEdit.CopyToClipboard
end;

procedure TSyntaxEditorFile.Paste;
begin
  SynEdit.PasteFromClipboard;
end;

procedure TSyntaxEditorFile.Cut;
begin
  SynEdit.CutToClipboard;
end;

procedure TSyntaxEditorFile.SelectAll;
begin
  SynEdit.SelectAll;
end;

procedure TSyntaxEditorFile.SetLine(Line: Integer);
begin
  SynEdit.CaretY := Line;
  SynEdit.CaretX := 1;
end;

procedure TSyntaxEditorFile.GotoLine;
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
    Debugger := CreateDebugger;
    if (Debugger <> nil) and (Engine.Options.AutoStartDebug = asdStartup) then
      Debugger.Active := True;
    IsPrepared := True;
  end;
end;

procedure TEditorTendency.Unprepare;
begin
  if Debugger <> nil then
  begin
    Debugger.Action(dbaResume);
    Debugger.Stop;
  end;
  FreeAndNil(FDebugger);
end;

function TEditorTendency.CanExecute: Boolean;
begin
  Result := (capExecute in Capabilities) or ((Engine.Files.Current <> nil) and Engine.Files.Current.CanExecute);
end;

function TEditorTendency.GetIsDefault: Boolean;
begin
  Result := False;
end;

procedure TEditorTendency.AddGroup(vName, vCategory: string);
var
  G: TFileGroup;
  C: TVirtualCategory;
begin
  if vCategory = '' then
    C := nil
  else
    C := Engine.Categories.Find(vCategory);
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
  if (Debugger <> nil) and (Engine.Options.AutoStartDebug = asdRun) then
    Debugger.Active := True;

  AOptions := TRunProjectOptions.Create;//Default options
  try
    AOptions.Copy(RunOptions);
    if (Engine.Session.Active) then
      AOptions.Merge(Engine.Session.Project.RunOptions);

    p.Actions := RunActions;
    if (Debugger <> nil) and (Debugger.Running) then
    begin
      if rnaKill in RunActions then
        Engine.CurrentTendency.Debugger.Action(dbaReset)
      else if Engine.Session.Run.Active then
        Engine.Session.Run.Show;

      if rnaExecute in RunActions then
      begin
        if rnaCompile in RunActions then
          Debugger.Action(dbaRun)
        else
          Debugger.Action(dbaResume);
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
        if Debugger.Active then
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

        if (Engine.Session.Project.RunOptions.MainFile <> '') or (Engine.Files.Current = nil) or not Engine.Files.Current.Execute(p) then
          if (p.MainFile <> '') then
            DoRun(p);
        Engine.Update([ecsDebug]);
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

function TEditorTendency.CreateProject: TEditorProject;
begin
  Result := TRunProject.Create;
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
    SetProject(Engine.DefaultProject);
  end;
  Engine.Update([ecsChanged, ecsState, ecsRefresh, ecsProject]);
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
  Extenstion := '.mne-project';
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
              f := Root + IncludePathDelimiter(Path) + sr.Name
            else
              f := IncludePathDelimiter(Path) + sr.Name;
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
                  f := Root + IncludePathDelimiter(Path) + sr.Name
                else
                  f := IncludePathDelimiter(Path) + sr.Name;
                if fftDir in Types then
                  Callback(AObject, f, aCount, vLevel, True, Resume);
                if not Resume then
                  break;
              end;
              if (vMaxLevel = 0) or (vLevel < vMaxLevel) then
                DoFind(Root, IncludePathDelimiter(Path + sr.Name), vLevel);
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
  Engine.Update([ecsEdit]);
end;

procedure TEditorEngine.EndUpdate;
begin
  if FUpdateCount = 1 then
  begin
    if {(FUpdateCount = 1) and } (Files.Current <> nil) and not (Files.Current.Visible) then
      Files.Current.Show;

    if FUpdateState <> [] then
      InternalChangedState(FUpdateState);
    FUpdateState := [];
  end;
  Dec(FUpdateCount);
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
    if FNotifyObjects[i] is INotifyEngineEditor then
      (FNotifyObjects[i] as INotifyEngineEditor).EngineReplaceText(Sender, ASearch, AReplace, Line, Column, ReplaceAction);
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
    if Items[i].IsChanged and not Items[i].IsTemporary then
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

function TEditorFiles.InternalOpenFile(FileName, FileParams: string; AppendToRecent: Boolean): TEditorFile;
var
  lFileName: string;
  FallbackGroup: string;
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
    if (Engine.Options.FallbackToText) then
      FallbackGroup := cFallbackGroup
    else
      FallbackGroup := '';
    Result := Engine.Groups.OpenFile(Self, FileName, FileParams, FallbackGroup);
  end;
  if (Result <> nil) and AppendToRecent then
    Engine.ProcessRecentFile(lFileName);
end;

procedure TEditorOptions.Load(vWorkspace: string);

  procedure SafeLoad(s: TRecentItems; vName:string);
  begin
    if FileExists(vName) then
      s.LoadFromFile(vName);
  end;

  procedure SafeLoad(s: TStringList; vName:string);
  begin
    if FileExists(vName) then
      s.LoadFromFile(vName);
  end;
begin
  SafeLoadFromFile(vWorkspace + 'mne-options.xml');
  Profile.SafeLoadFromFile(vWorkspace + 'mne-editor.xml');

  SafeLoad(RecentFiles, vWorkspace + 'mne-recent-files.xml');
  SafeLoad(RecentDatabases, vWorkspace + 'mne-recent-database.xml');
  SafeLoad(RecentFolders, vWorkspace + 'mne-recent-folders.xml');
  SafeLoad(RecentProjects, vWorkspace + 'mne-recent-projects.xml');
  SafeLoad(Projects, vWorkspace + 'mne-projects.xml');

  SafeLoad(SearchHistory, vWorkspace + 'mne-search-history.ini');
  SafeLoad(ReplaceHistory, vWorkspace + 'mne-replace-history.ini');
  SafeLoad(SearchFolderHistory, vWorkspace + 'mne-folder-history.ini');
  SafeLoad(Custom, vWorkspace + 'mne-custom-options.ini');

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
    SetProject(aProject);
    FIsChanged := False;
    if Active then  //sure if not default project
      Engine.ProcessRecentProject(FileName);
    Engine.Update([ecsChanged, ecsState, ecsRefresh, ecsProject, ecsProjectLoaded]);
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
    Result := vGroup.OpenFile(Self, '', ''); //New file
    Result.NewContent;
    Result.Edit;
    Current := Result;
    Current.Show;
    Current.Activate;
    Engine.Update([ecsChanged, ecsDebug, ecsState, ecsRefresh]);
  finally
    Engine.EndUpdate;
  end;
end;

function TEditorFiles.New(GroupName: string): TEditorFile;
begin
  Result := New(Engine.Groups.Find(GroupName));
end;

function TEditorSession.New(Tendency: TEditorTendency): TEditorProject;
begin
  if Tendency <> nil then
  begin
    Result := Tendency.CreateProject;
    Result.TendencyName := Tendency.Name;
  end
  else
    Result := TRunProject.Create;
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
          aFile := InternalOpenFile(aDialog.Files[i], '', True);
          //aFile.IsReadOnly := aDialog. TODO
        end;
        if aFile <> nil then
          Current := aFile;
        Engine.Update([ecsChanged, ecsDebug, ecsState, ecsRefresh]);
      finally
        Engine.EndUpdate;
      end;
    end;
  finally
    aDialog.Free;
  end;
end;

function TEditorFiles.OpenFile(vFileName, vFileParams: string; ActivateIt: Boolean): TEditorFile;
begin
  if SameText(ExtractFileExt(vFileName), Engine.Extenstion) then //zaher need dot
  begin
    Engine.Session.Load(vFileName);
    Result := nil; //it is a project not a file.
  end
  else
  begin
    Result := LoadFile(vFileName, vFileParams);
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
    Engine.Update([ecsFolder, ecsChanged, ecsState, ecsRefresh]);
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
  Result := ExpandFileName(IncludePathDelimiter(Result));
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

procedure TEditorEngine.ProcessRecentFile(const FileName: string; FileParams: string);
begin
  Options.RecentFiles.Add(FileName, FileParams);
  Update([ecsRecents]);
end;

procedure TEditorEngine.ProcessRecentFolder(const FileName: string; FileParams: string);
begin
  Options.RecentFolders.Add(FileName, FileParams);
  Update([ecsRecents]);
end;

procedure TEditorEngine.ProcessRecentProject(const FileName: string; FileParams: string);
begin
  Options.RecentProjects.Add(FileName, FileParams);
  Update([ecsRecents]);
end;

procedure TEditorEngine.ProcessProject(const FileName: string; FileParams: string);
begin
  Options.Projects.Add(FileName, FileParams);
  Update([ecsRecents]);
end;

procedure TEditorFiles.Save(Force: Boolean);
begin
  if Current <> nil then
    Current.Save(Force);
end;

procedure TEditorFiles.SaveAll(Force: Boolean);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Save(Force);
  end;
end;

procedure TEditorFiles.ReloadAll;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Load;
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
    Current.Save(True, ExtractFileExt(Current.Name), True);
end;

procedure TEditorOptions.Save(vWorkspace: string);
begin
  if DirectoryExists(vWorkspace) then
  begin
    Profile.SaveToFile(vWorkspace + 'mne-editor.xml');
    SaveToFile(vWorkspace + 'mne-options.xml');
    RecentFiles.SaveToFile(vWorkspace + 'mne-recent-files.xml');
    RecentDatabases.SaveToFile(vWorkspace + 'mne-recent-database.xml');
    RecentFolders.SaveToFile(vWorkspace + 'mne-recent-folders.xml');
    RecentProjects.SaveToFile(vWorkspace + 'mne-recent-projects.xml');
    Projects.SaveToFile(vWorkspace + 'mne-projects.xml');

    SearchHistory.SaveToFile(vWorkspace + 'mne-search-history.ini');
    ReplaceHistory.SaveToFile(vWorkspace + 'mne-replace-history.ini');
    SearchFolderHistory.SaveToFile(vWorkspace + 'mne-folder-history.ini');
    Custom.SaveToFile(vWorkspace + 'mne-custom-options.ini');

    Engine.Update([ecsFolder]);
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
    Engine.Update([ecsDebug, ecsRefresh]);
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
          Engine.Update([ecsState, ecsDebug, ecsRefresh]);
        end;
      end;
    finally
      Engine.EndUpdate;
    end;
  end;
end;

procedure TEditorSession.SetProject(const Value: TEditorProject; LoadFiles: Boolean);
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
      if FProject.SaveDesktop and LoadFiles then
        FProject.Desktop.Load;
      Changed;
      Engine.Update([ecsChanged, ecsState, ecsRefresh, ecsProject, ecsProjectLoaded]);
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
      Engine.Update([ecsOptions]);
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
      Engine.Update([ecsOptions]);
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
  Update([ecsRecents]);
end;

function TEditorEngine.ChooseTendency(var vTendency: TEditorTendency): Boolean;
var
  aName: string;
begin
  if (vTendency <> nil) then
    aName := vTendency.Name
  else
    aName := '';
  Result := ShowSelectList('Select project type', Tendencies, [slfSearch], aName); //slfIncludeNone
  if Result then
    vTendency := Tendencies.Find(aName);
end;

function TEditorEngine.NewProject: Boolean;
var
  aProject: TEditorProject;
  aTendency: TEditorTendency;
begin
  Result := False;
  Engine.BeginUpdate;
  try
    if (not Engine.Session.Active) or (Engine.Session.Save) then
    begin
      aTendency := nil;
      if ChooseTendency(aTendency) then
      begin
        aProject := Engine.Session.New(aTendency);
        if ShowProjectForm(aProject) then
        begin
          Engine.Session.SetProject(aProject);
          Result := True;
        end
        else
          aProject.Free;
      end;
    end;
  finally
    Engine.EndUpdate;
  end;
end;

function SortGroupsByTitle(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TFileGroup(Item1).Title, TFileGroup(Item2).Title);
end;

procedure TEditorEngine.Prepare(vSafeMode: Boolean);
begin
  FSafeMode := vSafeMode;
  FEngineLife := engnStarting;
  Tendencies.Init;
  Groups.Sort(@SortGroupsByTitle);
  DefaultProject.FileName := WorkSpace + 'mne-default-project.xml';
  try
    LoadOptions;
    DefaultProject.SafeLoadFromFile(DefaultProject.FileName);
    //here we will autoopen last files
    //Session.FProject := DefaultProject; //nah it is effect on UpdatePanel and ProjectChanged in mainform
  except
    on E: Exception do
      Engine.SendMessage(E.Message, msgtLog);
  end;
  FEngineLife := engnStarted;
end;

procedure TEditorEngine.Start;
var
  lFilePath: string;
begin
  if (ParamCount > 0) then
  begin
    if SameText(ParamStr(1), '/dde') then //idk what is it :-1
      lFilePath := DequoteStr(ParamStr(2))
    else
      lFilePath := DequoteStr(ParamStr(1));
    BrowseFolder := ExtractFilePath(lFilePath);
    //The filename is expanded, if necessary, in EditorEngine.TEditorFiles.InternalOpenFile
    Session.SetProject(DefaultProject, False);
    Files.OpenFile(lFilePath, '');
  end
  else
  begin
    if Engine.Options.AutoOpenProject and ((Options.LastProject <> '')  and FileExists(Options.LastProject)) then
      Session.Load(Options.LastProject)
    else
    begin
      BeginUpdate;
      try
        BrowseFolder := Options.LastFolder;
        Session.SetProject(DefaultProject);
      finally
        EndUpdate;
      end;
    end;
  end;
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
    //After loading options
    for i := 0 to FNotifyObjects.Count-1 do
    begin
      if (FNotifyObjects[i] is INotifyEngineSetting) then
        (FNotifyObjects[i] as INotifyEngineSetting).LoadOptions;
    end;

    UpdateOptions;
    Update([ecsOptions]);
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
    //if not Groups[i].Category.Tendency.IsDefault then //some text and cfg must loaded
    begin
      Groups[i].ExtraExtensions.Clear;

      lStrings := TStringList.Create;
      try
        s := Groups[i].Name;
        s := Options.ExtraExtensions.Values[s];
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
  for i := 0 to FNotifyObjects.Count-1 do //before Options.Save(WorkSpace) maybe it save into options
  begin
    if (FNotifyObjects[i] is INotifyEngineSetting) then
      (FNotifyObjects[i] as INotifyEngineSetting).SaveOptions;
  end;
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
  Update([ecsRecents]);
end;

procedure TEditorEngine.RemoveRecentFile(const FileName: string);
var
  i: integer;
begin
  i := Options.RecentFiles.IndexOf(FileName);
  Options.RecentFiles.Delete(i);
  Update([ecsRecents]);
end;

procedure TEditorEngine.ProcessRecentDatabase(const AliasName: string; FileParams: string);
begin
  Options.RecentDatabases.Add(AliasName, FileParams);
  Update([ecsRecents]);
end;

procedure TEditorEngine.RemoveRecentDatabase(const AliasName: string);
var
  i: integer;
begin
  i := Options.RecentDatabases.IndexOf(AliasName);
  Options.RecentFiles.Delete(i);
  Update([ecsRecents]);
end;

procedure TEditorEngine.RemoveRecentFolder(const FileName: string);
var
  i: integer;
begin
  i := Options.RecentFolders.IndexOf(FileName);
  Options.RecentFolders.Delete(i);
  Update([ecsRecents]);
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
      if FNotifyObjects[i] is INotifyEngineEditor then
        (FNotifyObjects[i] as INotifyEngineEditor).EngineMessage(S, vMessageType, vError);
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
      if (FNotifyObjects[i] is INotifyEngineEditor) then
        (FNotifyObjects[i] as INotifyEngineEditor).EngineAction(EditorAction);
    end;
  end;
end;

procedure TEditorEngine.SaveAll(Force: Boolean);
begin
  Session.Save;
  Files.SaveAll(Force);
end;

procedure TEditorEngine.Run;
begin
  if Files.Count > 0 then
    SaveAll(False);
  CurrentTendency.Run([rnaCompile, rnaExecute]);
end;

procedure TEditorEngine.Execute;
begin
  if Files.Count > 0 then
    SaveAll(False);
  CurrentTendency.Run([rnaExecute]);
end;

procedure TEditorEngine.DoChangedState(State: TEditorChangeStates);
var
  i: Integer;
begin
  if not IsShutdown then
  begin
    for i := 0 to FNotifyObjects.Count -1 do
    begin
      if FNotifyObjects[i] is INotifyEngineState then
        (FNotifyObjects[i] as INotifyEngineState).ChangeState(State);
    end;
end;
end;

procedure TEditorEngine.Update(State: TEditorChangeStates; Silent: Boolean);
begin
  if Updating or Silent then
    FUpdateState := FUpdateState + State
  else //if (FInUpdateState = 0) or not (State in FUpdateState) then
    InternalChangedState(State);
end;

function TEditorFiles.LoadFile(vFileName, vFileParams: string; AppendToRecent: Boolean): TEditorFile;
begin
  try
    try
      Result := InternalOpenFile(vFileName, vFileParams, AppendToRecent);
    finally
      Engine.Update([ecsChanged]);
    end;
  finally
    Engine.Update([ecsState, ecsDebug, ecsRefresh]);
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
    if MsgBox.Yes('Revert file ' + Current.Name) then
      Current.Load;
  end;
end;

procedure TEditorEngine.SetBrowseFolder(const Value: string);
begin
  FBrowseFolder := Value;
  if FBrowseFolder <> '' then
    FBrowseFolder := IncludeTrailingPathDelimiter(FBrowseFolder);
  Update([ecsFolder]);
end;

procedure TEditorEngine.DoMacroRecorderChanged(Sender: TObject);
begin
  Update([ecsState]);
end;

function TEditorEngine.GetWorkSpace: string;
begin
  Result := IncludeTrailingPathDelimiter(FWorkSpace);
end;

function TEditorEngine.GetMainFileTendency: TEditorTendency;
begin
  if (Session.Project <> nil) and (Session.Project.RunOptions.MainFile <> '') then //do not use (Session.Active) maybe it is default project
  begin
    if (not SameText(FCache.MainFile, Session.Project.RunOptions.MainFile)) then
    begin
      FCache.MainFile := Session.Project.RunOptions.MainFile;
      FCache.MainGroup := Engine.Groups.FindGroup(ExtractFileName(FCache.MainFile));
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
  Result := InternalOpenFile(vFileName, '', False);
  Engine.Update([ecsChanged]);
  if Result <> nil then
    Current := Result;
  Engine.Update([ecsState, ecsRefresh]);
end;

procedure TEditorFiles.Refresh;
begin
  if Current <> nil then
    Current.Refresh;
end;

function TEditorFiles.ShowFile(const FileName: string; Line: integer): TEditorFile;
begin
  Result := InternalOpenFile(FileName, '', False);
  Result.SetLine(Line);
  Engine.Update([ecsChanged]);
  if Result <> nil then
    Current := Result;
  Engine.Update([ecsState, ecsRefresh]);
end;

{ TEditorFile }

procedure TEditorFile.Edit;
begin
  if not IsReadOnly then
    IsChanged := True;
end;

procedure TEditorFile.Close;
var
  i: integer;
  mr: TmsgChoice;
  a: Boolean;
begin
  if (Name <> '') or not IsTemporary then
  begin
    if IsChanged then
    begin
      mr := MsgBox.YesNoCancel('Save file ' + Name + ' before close?');
      if mr = msgcCancel then
        Abort
      else if mr = msgcYes then
        Save(True);
    end;
    Engine.ProcessRecentFile(Name);
  end;

  i := Index;
  a := (Engine.Files.Current <> nil) and Engine.Files.Current.Activated;
  if Engine.Files.FCurrent = self then
    Engine.Files.FCurrent := nil;
  Free;
  Engine.Files.SetCurrentIndex(i, False);
  if a and (Engine.Files.Current <> nil) then
    Engine.Files.Current.Activate;
  Engine.Update([ecsChanged, ecsState, ecsRefresh]);
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
  FIsChanged := False;
  FIsTemporary := False;
  FFileEncoding := 'UTF8';
  FLinesMode := efmUnix;
  InitContents;
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

procedure TEditorFile.LoadFromFile(FileName: string);
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
  IsChanged := False;
  IsTemporary := False;
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
      efmWindows:
        SaveAsWindows(Strings, aStream);
      efmMac:
        SaveAsMac(Strings, aStream);
      else
        SaveAsUnix(Strings, aStream);
    end;
  finally
    aStream.Free;
  end;
end;

procedure TEditorFile.SaveToFile(FileName: string);
begin
  DoSave(FileName);
  Name := FileName;
  IsChanged := False;
  IsTemporary := False;
  IsNew := False;
  Engine.Update([ecsFolder]);
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
        if LeftStr(aExt, 1) <> '.' then
          aExt := '.' + aExt;
        aGroup := Engine.Groups.FindGroup(aExt);
        Group := aGroup;
      end;
    end
    else
      Name := ToNakeName;
    Engine.Update([ecsRefresh, ecsFolder, ecsState, ecsChanged]);
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
        IsChanged := True;
      end;
    end;
    Engine.Update([ecsRefresh, ecsFolder, ecsState, ecsChanged]);
  finally
    Engine.EndUpdate;
  end;
end;

function TEditorFile.CanExecute: Boolean;
begin
  if Group = nil then
    Result := False
  else
    Result := fgkExecutable in Group.Kind;
end;

procedure TEditorFile.SetIsChanged(const Value: Boolean);
begin
  FIsChanged := Value;
end;

procedure TEditorFile.Show;
begin
  if Content <> nil then
  begin
    Content.Align := alClient;
    Content.Realign;
    Content.Visible := True;
    Content.Show;
    Content.BringToFront;
    //Activate;//no no, bad when return back from another app to miniedit
  end;
end;

function TEditorFile.Visible: Boolean;
begin
  Result := (Content <> nil ) and (Content.Visible);
end;

procedure TEditorFile.Save(Force: Boolean; Extension: string; AsNewFile: Boolean);
var
  aDialog: TSaveDialog;
  aSave, DoRecent: Boolean;
  aName: string;
begin
  DoRecent := False;
  aName := '';
  if (((Name <> '') or not IsTemporary) or Force) then
  begin
    if (IsNew or (FName = '') or AsNewFile) then
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
  end
  else
    aSave := False;

  if aSave then
  begin
    SaveToFile(aName);
    FName := aName;
    if DoRecent then
    begin
      Engine.ProcessRecentFile(aName);
      Engine.Update([ecsRefresh, ecsState, ecsChanged]);
    end
    else
      Engine.Update([ecsState, ecsRefresh]);
  end;
end;

procedure TEditorFile.Save;
begin
  Save(True);
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
          mr := MsgBox.YesNoCancel(Name + #13' was changed, update it?');
        if mr = msgcYes then
          Load;
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
        n := MsgBox.Ask(Name + #13' was not found, what do want?', [Choice('&Keep It', msgcYes), Choice('&Close', msgcCancel), Choice('Read only', msgcNo)], 0, 2);
      if n = -1 then //do nothing
      else if n = 0 then //Keep It
        IsNew := True
      else if n = 2 then //Keep It
      begin
        IsChanged := False;
        IsTemporary := False;
        IsReadOnly := True
      end
      else
        Close;
    end;
  end;
end;

procedure TEditorFile.Activate;
begin
  if (Content <> nil) and Content.CanFocus then //Content not Control, Control maybe will focused later, maybe buggy but that the right
    Application.MainForm.ActiveControl := Control;
end;

function TEditorFile.Activated: Boolean;
begin
  Result := False;
  if (Content <> nil) and Content.CanFocus then
    Result := Application.MainForm.ActiveControl = Control;
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

function TEditorFile.GetCaption: string;
begin
  if Name = '' then
  begin
    if Group <> nil then
      Result := Group.Name
    else
      Result := 'Unkown';
    if IsTemporary then
      Result := '>' + Result
    else
      Result := '*' + Result;
  end
  else
    Result := ExtractFileName(Name);
end;

function TEditorFile.GetLanguageName: string;
begin
  Result := '';
end;

procedure TEditorFile.SetLine(Line: Integer);
begin

end;

procedure TEditorFile.SetHighlightLine(AValue: Integer);
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

procedure TEditorFile.Load;
begin
  LoadFromFile(Name);
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

function TEditorFile.GetControl: TWinControl;
begin
  Result := GetContent;
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
    Engine.Update([ecsState, ecsRefresh]);
  end;
end;

procedure TEditorFile.SetExtension(AValue: string);
begin
  if LeftStr(AValue, 1) <> '.' then
    AValue := '.' + AValue;
  Rename(PureName + AValue);
end;

function TEditorFile.GetContent: TWinControl;
begin
  Result := nil;
end;

function TEditorFile.GetSynEdit: TSynEdit;
begin
  Result := nil;
end;

procedure TEditorFile.DoGetCapability(var vCapability: TEditCapability);
begin
  vCapability := [];
end;

procedure TEditorFile.ContentsLoadFromStream(SynEdit: TSynEdit; AStream: TStream);
var
  Contents: string;
  Size: integer;
  IndentMode: TIndentMode;
  Encoded: Boolean;
begin
  SynEdit.BeginUpdate;
  try
    Size := AStream.Size - AStream.Position;
    SetString(Contents, nil, Size);
    AStream.Read(Pointer(Contents)^, Size);
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
  end;
end;

procedure TEditorFile.ContentsSaveToStream(SynEdit: TSynEdit; AStream: TStream);
var
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

  AStream.WriteBuffer(Pointer(Contents)^, Length(Contents));
end;

procedure TEditorFile.ContentsLoadFromFile(SynEdit: TSynEdit; FileName: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    ContentsLoadFromStream(SynEdit, AStream);
  finally
    AStream.Free;
  end;
end;

procedure TEditorFile.ContentsSaveToFile(SynEdit: TSynEdit; FileName: string);
var
  aStream : TFileStream;
begin
  aStream := TFileStream.Create(FileName, fmCreate);
  try
    ContentsSaveToStream(SynEdit, aStream);
  finally
    aStream.Free;
  end;
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

function TEditorFile.Execute(RunInfo: TmneRunInfo): Boolean;
begin
  Result := False;
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
    Engine.Update([ecsState, ecsRefresh]);
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

procedure TEditorFile.InitContents;
begin
end;

procedure TEditorFile.GroupChanged;
begin
  Tendency.Prepare; //Prepare its objects like debuggers
  Assign(Engine.Options.Profile);
end;

procedure TEditorFile.DoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if ([scReadOnly, scCaretX, scCaretY, scLeftChar, scTopLine, scSelection] * Changes) <> [] then
    Engine.Update([ecsState]);
end;

{ TEditorOptions }

constructor TEditorOptions.Create;
begin
  inherited Create;
  FCustom := TStringList.Create;
  FSearchHistory := TStringList.Create;
  FReplaceHistory := TStringList.Create;
  FSearchFolderHistory := TStringList.Create;
  FProfile := TEditorProfile.Create;
  FExtraExtensions := TStringList.Create;
  FRecentFiles := TRecentItems.Create(50);
  FRecentDatabases := TRecentItems.Create(50);
  FRecentFolders := TRecentItems.Create(50);
  FRecentProjects := TRecentItems.Create(50);
  FProjects := TRecentItems.Create(50);
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
  FRecentDatabases.Free;
  FRecentFolders.Free;
  FRecentProjects.Free;
  FProjects.Free;
  FCustom.Free;
  inherited;
end;

procedure TEditorOptions.SetProjects(const Value: TRecentItems);
begin
  if FProjects <> Value then
    FProjects.Assign(Value);
end;

procedure TEditorOptions.SetRecentFiles(const Value: TRecentItems);
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

procedure TEditorOptions.SetRecentDatabases(AValue: TRecentItems);
begin
  if FRecentDatabases <> AValue then
    FRecentDatabases.Assign(AValue);
end;

procedure TEditorOptions.SetRecentFolders(AValue: TRecentItems);
begin
  if FRecentFolders <> AValue then
    FRecentFolders.Assign(AValue);
end;

procedure TEditorOptions.SetRecentProjects(const Value: TRecentItems);
begin
  if FRecentProjects <> Value then
    FRecentProjects.Assign(Value);
end;

procedure GroupAddFitler(var Result: string; AGroup: TFileGroup);
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

{ TFileCategories }

function TFileGroups.CreateMask(CreateMaskProc: TCreateMaskProc): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    if (CreateMaskProc = nil) or (CreateMaskProc(Items[i])) then
      GroupAddFitler(Result, Items[i]);
  end;
end;

function TFileGroups.CreateFilter(FullFilter: Boolean; FirstExtension: string; vGroup: TFileGroup; OnlyThisGroup: Boolean): string;
var
  aSupported: string;
  procedure AddIt(AGroup: TFileGroup);
  var
    i, n: integer;
    s, e: string;
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
          e := AExtensions[i];

          if s <> '' then
            s := s + ';';

          if LeftStr(e, 1) = '.' then
            s := s + '*' + e
          else
            s := s + e;
        end;

        if FullFilter then
          Result := Result + AGroup.Title + ' (' + s + ')|' + s;

        if aSupported <> '' then
          aSupported := aSupported + ';';
        aSupported := aSupported + s;

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

function TFileGroups.FindGroup(vFullName: string; vKind: TFileGroupKinds): TFileGroup;
var
  i, j: integer;
  s: string;
begin
  Result := nil;
  if vFullName <> '' then
  begin
    //if have extension we will search for extension only, if not, we will search for name
    if Pos('.', vFullName) > 0 then //
      s := ExtractFileExt(vFullName)  //externsion only with dot
    else
      s := vFullName;
    for i := 0 to Count - 1 do
    begin
      if (vKind = []) or (vKind <= Items[i].Kind) then
      begin
        if Items[i].Extensions.Find(s) <> nil then
        begin
          Result := Items[i];
          break;
        end;
        if Result = nil then
          if Items[i].ExtraExtensions.Find(s) <> nil then
          begin
            Result := Items[i];
            break;
          end;
      end;
      if Result <> nil then
        break;
    end;
  end;
end;

function TFileGroups.OpenFile(vFiles: TEditorFiles; vFileName, vFileParams: string; FallBackGroup: string): TEditorFile;
var
  aGroup: TFileGroup;
  s: string;
begin
  s := ExtractFileName(vFileName);
  aGroup := Engine.Groups.FindGroup(s);

  if (aGroup <> nil) and (FallBackGroup <> '') then
    aGroup := Engine.Groups.Find(FallBackGroup);

  {if aGroup = nil then
    raise EEditorException.Create('Cannot open this file type: ' + vExtension);}
  if aGroup = nil then
  begin
    Engine.SendMessage('Cannot open this file type: ' + s, msgtLog);
    Result := nil;
  end
  else
    Result := aGroup.OpenFile(vFiles, vFileName, vFileParams);
end;

{ TVirtualCategory }

constructor TVirtualCategory.Create(ATendency: TEditorTendency; const vName, vTitle: string; vKind: TFileCategoryKinds; vImageName: string);
begin
  inherited Create(False); //childs is groups and already added to Groups and freed by it
  FTendency := ATendency;
  FName := vName;
  FTitle := vTitle;
  FKind := vKind;
  FImageName := vImageName;
end;

constructor TVirtualCategory.Create(ATendency: TEditorTendencyClass; const vName, vTitle: string; vKind: TFileCategoryKinds; vImageName: string);
var
  lTendency: TEditorTendency;
begin
  lTendency := Engine.Tendencies.FindByClass(ATendency);
  Create(lTendency, vName, vTitle, vKind, ImageName);
end;

procedure TVirtualCategory.EnumExtensions(vExtensions: TStringList);
var
  i: Integer;
begin
  for i  := 0 to Count - 1 do
  begin
    Items[i].EnumExtensions(vExtensions);
  end;
end;

function TVirtualCategory.GetExtensions: string;
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

function TVirtualCategory.GetColorPrefix: string;
begin
  Result := '';
end;

function TVirtualCategory.FormatColor(Color: TColor): string;
begin
  Result := ColorToRGBHex(Color, GetColorPrefix);
end;

function TVirtualCategory.DeformatColor(Str: string): TColor;
begin
  Result := RGBHexToColor(Str, GetColorPrefix, false);
end;

procedure TVirtualCategory.Apply(AHighlighter: TSynCustomHighlighter; Attributes: TGlobalAttributes);
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

procedure TVirtualCategory.InitCompletion(vSynEdit: TCustomSynEdit);
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

procedure TVirtualCategory.DoAddKeywords;
begin
end;

procedure TVirtualCategory.DoAddCompletion(AKeyword: string; AKind: integer);
begin
  Completion.ItemList.Add(AKeyword);
end;

procedure TVirtualCategory.InitEdit(vSynEdit: TCustomSynEdit);
begin
end;

destructor TVirtualCategory.Destroy;
begin
  FreeAndNil(FMapper);
  FreeAndNil(FCompletion);
  FreeAndNil(FHighlighter);
  inherited;
end;

procedure TVirtualCategory.EnumMenuItems(AddItems: TAddClickCallBack);
begin
end;

function TVirtualCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := DoCreateHighlighter;
end;

procedure TVirtualCategory.InitHighlighter;
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

function TVirtualCategory.Find(vName: string): TFileGroup;
begin
  Result := inherited Find(vName) as TFileGroup;
end;

function TVirtualCategory.GetItem(Index: Integer): TFileGroup;
begin
  Result := inherited Items[Index] as TFileGroup;
end;

function TVirtualCategory.GetMapper: TMapper;
begin
  Result := FMapper;
  if FMapper = nil then
    raise Exception.Create('Mapper is null');
end;

function TVirtualCategory.GetHighlighter: TSynCustomHighlighter;
begin
  InitHighlighter;
  Result := FHighlighter;
end;

function TVirtualCategory.GetIsText: Boolean;
begin
  Result := True;
end;

function TVirtualCategory.OpenFile(vGroup: TFileGroup; vFiles: TEditorFiles; vFileName, vFileParams: string): TEditorFile;
begin
  Result := nil;
end;

procedure TVirtualCategory.DoExecuteCompletion(Sender: TObject);
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

  Engine.Update([ecsChanged, ecsProject]); //TODO move to caller
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
  Engine.Update([ecsChanged, ecsProject]);
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

function TEditorProject.CreateMask(CreateMaskProc: TCreateMaskProc): string;
var
  AExtension: String;
  AExtensions: TStringList;
  aGroup: TFileGroup;
begin
  Result := '';
  AExtensions := TStringList.Create;
  AExtensions.Delimiter := ';';
  AExtensions.DelimitedText := Engine.Session.Project.FileFilter;
  for AExtension in AExtensions do
  begin
    aGroup := Engine.Tendency.Groups.FindGroup(AExtension);
    if aGroup <> nil then
    begin
      GroupAddFitler(Result, aGroup);
      CreateMaskProc(aGroup);
    end;
  end;
  AExtensions.Free;
end;

procedure TEditorProject.Saving;
begin
  inherited;
  if FSaveDesktop then
    Desktop.Save;
end;

{ TFileGroup }

procedure TFileGroup.SetCategory(AValue: TVirtualCategory);
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

function TFileGroup.GetExtension: string;
begin
  if Extensions.Count = 0 then
    raise Exception.Create('no default extention for group: ' + Name);
  Result := Extensions[0].Name;
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

function TFileGroup.OpenFile(vFiles: TEditorFiles; vFileName, vFileParams: string): TEditorFile;
begin
  Result := Category.OpenFile(Self, vFiles, vFileName, vFileParams);
end;

{ TFileGroups }

procedure TFileGroups.InternalAdd(GroupClass: TFileGroupClass; FileClass: TEditorFileClass; const Name, Title:string; Category: TFileCategoryClass; Extensions: array of string; Kind: TFileGroupKinds; Style: TFileGroupStyles);
var
  aCategory: TVirtualCategory;
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
  if aCategory.Tendency = nil then
    raise Exception.Create(Name + ' have not Tendency');
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
  Engine.Update([ecsChanged, ecsState, ecsRefresh, ecsProject]);
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
  aLine: Integer;
begin
  aTendency := FEditorFile.Tendency; //from file Tendency
  //inherited;
  if aTendency.Debugger <> nil then
  begin
    lh := TSynEdit(SynEdit).LineHeight;
    iw := EditorResource.DebugImages.Width;

    DebugManager.Enter;
    try
      for i := FirstLine to LastLine do
      begin
        aLine := TSynEdit(SynEdit).ScreenRowToRow(i);
        if aTendency.Debugger.Breakpoints.IsExists(FEditorFile.Name, aLine) then
          DrawIndicator(i, DEBUG_IMAGE_BREAKPOINT);
      end;

    finally
      DebugManager.Leave;
    end;

    if (Engine.DebugLink.ExecutedControl = SynEdit) and (Engine.DebugLink.ExecutedLine >= 0) then
    begin
      aLine := TSynEdit(SynEdit).RowToScreenRow(Engine.DebugLink.ExecutedLine);//TODO use TextXYToScreenXY
      DrawIndicator(aLine, DEBUG_IMAGE_EXECUTE);
    end;
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
          aFile := Engine.Files.LoadFile(aItem.FileName, aItem.FileParams, False);
          if aFile <> nil then
            aFile.Assign(aItem);
        end;
      end;
      Engine.Files.SetActiveFile(Files.CurrentFile);
      Engine.BrowseFolder := Files.CurrentFolder;

      if (FProject.Tendency <> nil) and (Project.Tendency.Debugger <> nil) then
      begin
        DebugManager.Enter;
        try
         Project.Tendency.Debugger.Breakpoints.Clear;
          for i := 0 to Breakpoints.Count - 1 do
          begin
            Project.Tendency.Debugger.Breakpoints.Add(Breakpoints[i].FileName, Breakpoints[i].LineNo);
          end;

          Project.Tendency.Debugger.Watches.Clear;
          for i := 0 to Watches.Count - 1 do
          begin
            Project.Tendency.Debugger.Watches.Add(Watches[i].Name);
          end;
        finally
          DebugManager.Leave;
        end;
        Engine.Update([ecsDebug]);
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
  if (FProject.Tendency <> nil) and (Project.Tendency.Debugger <> nil) then
  begin
    DebugManager.Enter;
    try
      for i := 0 to Project.Tendency.Debugger.Breakpoints.Count - 1 do
      begin
        Breakpoints.Add(Project.Tendency.Debugger.Breakpoints[i].FileName, Project.Tendency.Debugger.Breakpoints[i].Line);
      end;

      for i := 0 to Project.Tendency.Debugger.Watches.Count - 1 do
      begin
        Watches.Add(Project.Tendency.Debugger.Watches[i].Name);
      end;
    finally
      DebugManager.Leave;
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
