unit EditorEngine;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$modeswitch arrayoperators}
{$INTERFACES CORBA}//Needed for interfaces without guid
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}

{**

  FileName = c:\temp\myfile.ext
  File = myfile.ext
  NickName = myfile
  FilePath = c:\temp\

  Categories: list
    Category: object have have highlighter, autocomplete ...
      Groups: list
        Group: object Have multiple extension externsions that all same type, Group.Name is not an extension
          Extensions (pas, pp, inc)


Portable Notes:
  Main folder related to project folder, Project folder related to Project File folder
  Paths in Variables should not delimited by PathDelimiter, use ExcludePathDelimiter()
}
interface

uses
  Messages, SysUtils, Forms, StdCtrls, StrUtils, DateUtils, Dialogs, Variants, Classes, Menus, Controls,
  LMessages, LCLIntf, LConvEncoding, LazUTF8, fgl, LCLType,
  FileUtil, LazFileUtils, Math, Masks,
  Graphics, Contnrs, Types, IniFiles,
  EditorOptions, EditorColors, EditorProfiles,
  SynBeautifier, SynEditMarks, mnSynCompletion, mnSynParamsHint, SynEditAutoComplete,
  SynEditTypes, SynEditMiscClasses, SynEditPlugins, SynPluginTemplateEdit,
  SynEditHighlighter, SynEditKeyCmds, SynEditMarkupBracket, SynEditSearch, ColorUtils,
  SynEdit, SynEditTextTrimmer, SynTextDrawer, SynGutterBase, SynEditPointClasses, SynMacroRecorder,
  mncCSV, mnXMLRttiProfile, mnXMLUtils, mnClasses,
  mnUtils, EditorClasses, EditorRun;

type
  TThreeStates = (stateNone, stateFalse, stateTrue);

  TSynCompletionType = (ctCode, ctHint, ctParams);

  TEditorEngine = class;
  TVirtualCategory = class;
  TVirtualCategoryClass = class of TVirtualCategory;
  TFileGroup = class;
  TFileGroups = class;
  TEditorFile = class;
  TEditorProject = class;
  TRunOptions = class;

  EEditorException = class(Exception)
  private
    FErrorLine: Integer;
  public
    property ErrorLine: Integer read FErrorLine write FErrorLine;
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
    function DoCreateItem(AClass: TmnXMLItemClass): TmnXMLItem; override;
    property Items[Index: Integer]: _Object_ read GetItem write SetItem; default;
  published
  end;

  { TStringsHelper }

  TStringsHelper = class helper for TStrings
  public
    procedure Merge(FromStrings: TStrings; Overwrite: Boolean = False);
    procedure Merge(Name, Value: String; Overwrite: Boolean = False);
  end;

  TScannedValue = class(TmnObject)
  public
    Line: Integer;
    X1,X2: Integer;
    Text: string;
    Value: string;
  end;

  { TScannedValues }

  TScannedValues = class(specialize TmnObjectList<TScannedValue>)
  public
    procedure Add(
      Line: Integer;
      X1,X2: Integer;
      Text: string;
      Value: string
    );
  end;

  { TKeywordItem }

  TKeywordItem = class(TmnNamedObject)
  private
    FParams: TStringList;
  public
    Display: string;
    AttributeType: TAttributeType;
    AttributeName: string; //* keyword, const, function, event, value etc
    Template: string; //* name(count: integer) { | }
    Description: string;
    Temp: Boolean; //*
    constructor Create;
    destructor Destroy; override;
    property Params: TStringList read FParams;
  end;

  { TKeywordList }

  TKeywordList = class(specialize TmnNamedObjectList<TKeywordItem>)
  public
    procedure LoadFromFile(FileName: string); virtual;
    procedure Clean;// Delete temp items
    function AddItem(AName, ADisplay, AttributeName: string; AttributeType: TAttributeType; AsTemp: Boolean = False): TKeywordItem;
    procedure Sort;
  end;

  { TmneSynCompletionForm }

  TmneSynCompletionForm = class(TSynVirtualCompletionForm)
  private
    FKeywords: TKeywordList;
  protected
    function GetItemText(Index: Integer): string; override;
    function GetItemDisplay(Index: Integer): string; override;
    function GetItemsCount: Integer; override;
  public
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    property Keywords: TKeywordList read FKeywords;
  end;

  { TmneSynCompletion }

  TmneSynCompletion = class(TSynBaseCompletion)
  protected
    function GetCompletionFormClass: TSynVirtualCompletionFormClass; override;
    function OwnedByEditor: Boolean; override;

    procedure OnSynCompletionNextChar(Sender: TObject);
    procedure OnSynCompletionPrevChar(Sender: TObject);
    procedure OnSynCompletionKeyPress(Sender: TObject; var Key: Char);
    procedure OnSynCompletionUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Sort; override;
    procedure Clear; override;
  end;

  TmneSynEditCommand = (mscUUID); // msPaused = paused recording

  { TmneSynEditPlugin }

  TmneSynEditPlugin = class(TAbstractSynHookerPlugin)
  protected
    FCommandIDs: array [TmneSynEditCommand] of TSynEditorCommand;
    FShortCuts: array [TmneSynEditCommand] of TShortCut;
    procedure OnCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var aChar: TUTF8Char; Data: pointer; HandlerData: pointer); override;

    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
  public
    constructor Create(aOwner: TComponent); override;
  end;

  { TmneSynBeautifier }

  TmneSynBeautifier = class(TSynBeautifier)
  protected
    procedure DoBeforeCommand(const ACaret: TSynEditCaret; var Command: TSynEditorCommand); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TEditorExtension }

  TEditorExtension = class(TObject)
  public
    Name: String;
    ImageIndex: Integer;
  end;

  { TEditorExtensions }

  TEditorExtensions = class(specialize TmnNamedObjectList<TEditorExtension>)
  private
  public
    procedure Add(Name: String; ImageIndex: Integer = -1);
  end;

  TTextEditorFile = class;

  { TmneSynEdit }

  TmneSynEdit = class(TSynEdit)
  protected
    FEditorFile: TTextEditorFile;
    FmneSynBeautifier: TmneSynBeautifier;
    procedure DoOnShowHint(HintInfo: PHintInfo); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteCommand(Command: TSynEditorCommand; const AChar: TUTF8Char; Data: pointer); override;
    property EditorFile: TTextEditorFile read FEditorFile;
  end;

  { TEditorDesktopFile }

  TEditorDesktopFile = class(TCollectionItem)
  private
    FFileName: String;
    FFileParams: String;
    FCaretY: Integer;
    FCaretX: Integer;
    FTopLine: Integer;
  public
    constructor Create(ACollection: TCollection); override;
  published
    property FileName: String read FFileName write FFileName;
    property FileParams: String read FFileParams write FFileParams;
    property CaretX: Integer read FCaretX write FCaretX default 1;
    property CaretY: Integer read FCaretY write FCaretY default 1;
    property TopLine: Integer read FTopLine write FTopLine default 1;
  end;

  { TEditorDesktopFiles }

  TEditorDesktopFiles = class(TCollection)
  private
    FCurrentFile: String;
    FCurrentFolder: String;
    function GetItems(Index: Integer): TEditorDesktopFile;
  protected
  public
    function Add(FileName: String): TEditorDesktopFile;
    function Find(vName: String): TEditorDesktopFile;
    function IsExist(vName: String): Boolean;
    property Items[Index: Integer]: TEditorDesktopFile read GetItems; default;
  published
    property CurrentFile: String read FCurrentFile write FCurrentFile;
    property CurrentFolder: String read FCurrentFolder write FCurrentFolder;
  end;

  { TmneBreakpoint }

  TmneBreakpoint = class(TmnXMLItem)
  private
    FFileName: String;
    FLineNo: Integer;
  public
    ID: Integer;
    Handle: Integer;
  published
    property FileName: String read FFileName write FFileName;
    property LineNo: Integer read FLineNo write FLineNo;
  end;

  { TmneBreakpoints }

  TmneBreakpoints = class(specialize GXMLItems<TmneBreakpoint>)
  private
  public
    procedure Add(AFileName: String; ALineNo: Integer);
  end;

  { TmneWatch }

  TmneWatch = class(TmnXMLItem)
  private
    FName: String;
  public
    ID: Integer;
    Handle: Integer;
  published
    property Name: String read FName write FName;
  end;

  { TmneWatches }

  TmneWatches = class(specialize GXMLItems<TmneWatch>)
  private
  public
    procedure Add(AName: String);
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
  TAddClickCallBack = procedure(Name, Caption, ParentMenu: String; OnClickEvent: TNotifyEvent; ShortCut: TShortCut = 0) of object;

  TEnumVariablesSkip = (evsFile, evsCategory, evsTendicy, evsProject, evsEngine, evsDefaultPath, evsEnviroment);
  TEnumVariablesSkips = set of TEnumVariablesSkip;

  TFileGroupKind = (
    fgkDefault,
    fgkText, //Is it an Text Editor like SQL or PHP
    fgkUneditable, // Can't be editable
    fgkBinary, // Like Database SQLite
    fgkShell,//TODO: Executable but hi priority, override the main file or project tendency
    fgkMain,//this can be the main file for project
    fgkResult,//Result file, generated, like: exe or .o or .hex
    fgkBrowsable,//When open file show it in the extension list
    fgkAssociated, //Editor can be the editor of this files, like .php, .inc, but .txt is not
    //fgkTemporary, //No need to save it to run, but save it when user ask to save
    fgkFolding,
    fgkVirtual //Not a real file, like output console
    );

  TFileGroupKinds = set of TFileGroupKind;

  {
    Tendency
    Run, Compile, Collect file groups and have special properties
  }

  TRunCapability = (
    capErrors,
    capStop, //Stop executing or compiling
    capLint, //Check error of file without compiling or run
    capCompile, //Can compile this file
    capExecute, //Can run this file
    capLink, //Can need link before run
    capDebug, //we can debug the project/file
    capEval, //Debugger can evaluate
    capTrace, //Steps (Step Into, Step Over etc...)
    capUpload, //Have upload, like avr projects need to upload to mcu
    capDebugServer, //PHP style need to start debug server
    capTransaction //Database
    );

  TRunCapabilities = set of TRunCapability;

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
    FDefaultPath: string;
    FEnableMacros: Boolean;
    FGroups: TFileGroups;
    FOutputExtension: String;
    FRunOptions: TRunOptions;

    FIndentMode: TIndentMode;
    FOverrideEditorOptions: Boolean;
    FTabWidth: Integer;
    function GetIsDefault: Boolean; virtual; //Keep it private
  protected
    IsPrepared: Boolean;
    FCapabilities: TRunCapabilities;
    FHaveOptions: Boolean;
    procedure AddGroup(vName: String; vCategory: TVirtualCategory);
    procedure AddGroup(vName, vCategory: String);
    procedure AddGroup(vName: String; vCategoryClass: TVirtualCategoryClass);
    function CreateDebugger: TEditorDebugger; virtual;
    procedure Init; virtual;
    procedure Created; virtual;
    procedure DoRun(Info: TmneRunInfo); virtual;
    procedure InternalRun(RunActions: TmneRunActions);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure HelpKeyword(AWord: String); virtual;
    procedure Run(RunActions: TmneRunActions);
    procedure SendMessage(S: String; vMessageType: TNotifyMessageType); override;

    procedure CreateOptionsFrame(AOwner: TComponent; AddFrame: TAddFrameCallBack); virtual;
    function CreateProjectOptions: TEditorProjectOptions; virtual;
    function CreateProject: TEditorProject; virtual;
    property HaveOptions: Boolean read FHaveOptions;

    procedure PrepareSynEdit(SynEdit: TSynEdit); virtual;
    procedure EnumRunCommands(Items: TStrings); virtual;
    function GetDefaultGroup: TFileGroup;
    //OSDepended: When save to file, the filename changed depend on the os system name
    procedure Prepare;
    procedure Unprepare;
    //
    property Capabilities: TRunCapabilities read FCapabilities;
    function Can(ACapability: TRunCapability): Boolean;
    function CanAny(ACapability: TRunCapabilities): Boolean;

    procedure EnumVariables(Values: TStringList; EnumSkips: TEnumVariablesSkips); virtual;
    function ReplaceVariables(S: String; EnumSkips: TEnumVariablesSkips): String; virtual;

    procedure UpdatePath; virtual;

    property Groups: TFileGroups read FGroups;
    property IsDefault: Boolean read GetIsDefault;
    property OutputExtension: String read FOutputExtension write FOutputExtension;
    property DefaultPath: string read FDefaultPath;
  published
    //Override options
    property OverrideEditorOptions: Boolean read FOverrideEditorOptions write FOverrideEditorOptions default False;
    property TabWidth: Integer read FTabWidth write FTabWidth default 4;
    property IndentMode: TIndentMode read FIndentMode write FIndentMode default idntTabsToSpaces;

    property RunOptions: TRunOptions read FRunOptions;// write FRunOptions;
    property EnableMacros: Boolean read FEnableMacros write FEnableMacros default False;
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
    procedure Execute(App, Cmd: String); virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure CommitDirectory(Directory: String); virtual; abstract;
    procedure CommitFile(FileName: String); virtual; abstract;
    procedure UpdateDirectory(Directory: String); virtual; abstract;
    procedure UpdateFile(FileName: String); virtual; abstract;
    procedure RevertDirectory(Directory: String); virtual; abstract;
    procedure RevertFile(FileName: String); virtual; abstract;
    procedure DiffFile(FileName: String); virtual; abstract;
    procedure DiffToFile(FileName, ToFileName: String); virtual; abstract;
    procedure AddFile(FileName: String); virtual; abstract;
  end;

  TEditorSCMClass = class of TEditorSCM;

  { TRunOptions }

  TRunProjectInfo = record
    Command: String;
    MainPath: String;
    Params: String;
    Console: TThreeStates;
    Pause: TThreeStates;
    MainFile: String;
    OutputFile: String;
    Require: String;
    ConfigFile: String;
    ExpandPaths: Boolean;
    SharedLib: Boolean;
  end;

  TRunOptions = class(TPersistent)
  private
    FInfo: TRunProjectInfo;
    FPaths: TStrings;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetPaths(AValue: TStrings);
    procedure Merge(AOptions: TRunOptions);
    procedure Copy(AOptions: TRunOptions);
    procedure Assign(Source: TPersistent); override;
  published
    property Pause: TThreeStates read FInfo.Pause write FInfo.Pause default stateTrue;
    property Console: TThreeStates read FInfo.Console write FInfo.Console default stateTrue;
    property Command: String read FInfo.Command write FInfo.Command;
    property Params: String read FInfo.Params write FInfo.Params;
    property Require: String read FInfo.Require write FInfo.Require;
    property MainPath: String read FInfo.MainPath write FInfo.MainPath;
    property MainFile: String read FInfo.MainFile write FInfo.MainFile;
    property OutputFile: String read FInfo.OutputFile write FInfo.OutputFile;
    property ExpandPaths: Boolean read FInfo.ExpandPaths write FInfo.ExpandPaths;
    property ConfigFile: String read FInfo.ConfigFile write FInfo.ConfigFile;
    property Paths: TStrings read FPaths write SetPaths;

    property SharedLib: Boolean read FInfo.SharedLib write FInfo.SharedLib; //dll or so shared lib
  end;

  TCreateMaskProc = function(vGroup: TFileGroup): Boolean of object;

  { TEditorProject }

  TEditorProject = class abstract(TmnXMLProfile)
  private
    FDefaultPath: string;
    FIgnoreNames: String;
    FFileFilter: String;
    FOptions: TEditorProjectOptions;
    FRunOptions: TRunOptions;
    FTendencyName: String;
    FDescription: String;
    FFileName: String;
    FName: String;
    FSaveDesktop: Boolean;
    FDesktop: TEditorDesktop;
    FTendency: TEditorTendency;
    FSCM: TEditorSCM;
    FTitle: String;
    function GetIsActive: Boolean;
    function GetPath: String;
    procedure SetTendency(AValue: TEditorTendency);
    procedure SetTendencyName(AValue: String);
    procedure SetSCM(AValue: TEditorSCM);
  protected
    procedure RecreateOptions;
    procedure RttiCreateObject(var vObject: TObject; vInstance: TObject; vObjectClass: TClass; const vClassName, vName: String); override;
    procedure Loaded(Failed: Boolean); override;
    procedure Saving; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: String); override;
    property FileName: String read FFileName write FFileName;
    property Path: String read GetPath;
    procedure UpdatePath;
    procedure SetSCMClass(SCMClass: TEditorSCM);
    function CreateMask(CreateMaskProc: TCreateMaskProc): String;
    //Tendency here point to one of Engine.Tendencies so it is not owned by project
    property Tendency: TEditorTendency read FTendency write SetTendency;
    property RunOptions: TRunOptions read FRunOptions write FRunOptions;
    property IsActive: Boolean read GetIsActive;
    property DefaultPath: string read FDefaultPath;
  published
    property Name: String read FName write FName;
    property Title: String read FTitle write FTitle;
    property TendencyName: String read FTendencyName write SetTendencyName;
    //SCM now owned by project and saved or loaded with it, the SCM object so assigned to will be freed with the project
    property SCM: TEditorSCM read FSCM write SetSCM;

    property Description: String read FDescription write FDescription;
    property SaveDesktop: Boolean read FSaveDesktop write FSaveDesktop default True;
    property Desktop: TEditorDesktop read FDesktop stored FSaveDesktop;
    property Options: TEditorProjectOptions read FOptions write FOptions default nil;

    property FileFilter: String read FFileFilter write FFileFilter;
    property IgnoreNames: String read FIgnoreNames write FIgnoreNames;
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
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: Integer); override;
  published
    property MarkupInfo;
  end;

  TSwitchControls = specialize TFPGList<TWinControl>;

  TEditorLinesMode = (efmUnix, efmWindows, efmMac);
  TEditCapabilities = set of (ecpAllowCopy, ecpAllowPaste, ecpAllowCut, ecpAllowUndo);

  { TEditorFile }

  TEditorFile = class(TCollectionItem, IFileEditor)
  private
    FFileEncoding: String;
    FFileName: String;
    FIsNew: Boolean;
    FIsChanged: Boolean;
    FFileDate: TDateTime;
    FFileSize: Int64;
    FGroup: TFileGroup;
    FParams: String;
    FRelated: String;
    FLinesMode: TEditorLinesMode;
    FIsTemporary: Boolean;
    FTitle: String;
    function GetEditCapability: TEditCapabilities;
    function GetRunCapability: TRunCapabilities;
    function GetIsText: Boolean;
    function GetBaseName: String;
    function GetNickName: String;
    function GetExtension: String;
    function GetPath: String;
    function GetTendency: TEditorTendency;
    procedure SetFileEncoding(AValue: String);
    procedure SetGroup(const Value: TFileGroup);
    procedure SetIsChanged(const Value: Boolean);
    procedure SetIsNew(AValue: Boolean);
    function GetLinesModeAsText: String;
    procedure SetLinesMode(const Value: TEditorLinesMode);
    procedure SetExtension(AValue: String);
    procedure SetBaseName(AValue: String);
    procedure SetNickName(AValue: String);
  protected
    procedure InitContents; virtual;
    procedure GroupChanged; virtual;
    function GetIsReadonly: Boolean; virtual;
    procedure SetIsReadonly(const Value: Boolean); virtual;
    function GetContent: TWinControl; virtual;
    function GetControl: TWinControl; virtual;
    function GetSynEdit: TSynEdit; virtual;
    procedure DoGetEditCapability(var vEditCapability: TEditCapabilities); virtual;
    procedure DoGetRunCapability(var vRunCapability: TRunCapabilities); virtual;
  protected
    procedure Edit;
    procedure DoEdit(Sender: TObject); virtual;
    procedure DoStatusChange(Sender: TObject; Changes: TSynStatusChanges); virtual;
    procedure Update; virtual;
    procedure UpdateAge; virtual;
    procedure NewContent; virtual;
    procedure DoLoad(FileName: String); virtual; abstract;
    procedure DoSave(FileName: String); virtual; abstract;
    function Execute(RunInfo: TmneRunInfo): Boolean; virtual;
  public
    constructor Create(ACollection: TCollection); override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure LoadFromFile(AFileName: String);
    procedure Load;
    procedure SaveToFile(AFileName: String);
    procedure Save(Force: Boolean; Extension: String = ''; AsNewFile: Boolean = False); virtual;
    procedure Save;

    //Utils for SynEdit loading/saving
    procedure ContentsLoadFromStream(SynEdit: TSynEdit; AStream: TStream);
    procedure ContentsSaveToStream(SynEdit: TSynEdit; AStream: TStream);
    procedure ContentsLoadFromFile(SynEdit: TSynEdit; FileName: String); //used if u have synedit
    procedure ContentsSaveToFile(SynEdit: TSynEdit; FileName: String);

    procedure Rename(ToName: String); //only name not with the path
    procedure Delete; //only name not with the path

    procedure Show; virtual; //Need to activate it after show to focus editor
    procedure Hide; virtual;
    function Visible: Boolean;
    procedure Activate; virtual;
    function Activated: Boolean;
    procedure Close;
    procedure OpenInclude; virtual;
    function CanAddRecentFiles: Boolean; virtual;
    function CanOpenInclude: Boolean; virtual;
    function CheckChanged(Force: Boolean = False): Boolean;
    //
    procedure GotoLine; virtual;
    procedure Find; virtual;
    procedure FindNext; virtual;
    procedure FindPrevious; virtual;
    procedure Replace; virtual;
    procedure Refresh; virtual;
    function GetHint(HintControl: TControl; CursorPos: TPoint; out vHint: String): Boolean; virtual;
    function GetGlance: String; virtual; //Simple string to show in the corner of mainform
    //
    function GetCaption: String; virtual; //Name or Title or *
    function GetLanguageName: String; virtual; //TODO need to get more good name to this function
    procedure SetLine(Line: Integer); virtual;
    procedure SetHighlightLine(AValue: Integer); virtual;

    //Clipboard
    function CanCopy: Boolean;
    function CanPaste: Boolean;
    function CanUndo: Boolean;
    property EditCapability: TEditCapabilities read GetEditCapability;
    property RunCapability: TRunCapabilities read GetRunCapability;

    procedure Paste; virtual;
    procedure Copy; virtual;
    procedure Cut; virtual;
    procedure SelectAll; virtual;

    procedure Undo; virtual;
    procedure Redo; virtual;
    procedure ClearUndo; virtual;

    procedure ScanValues(Values: TStringList; UpdateValues: Boolean = False);
    procedure EnumVariables(Values: TStringList; EnumSkips: TEnumVariablesSkips); virtual;
    function ReplaceVariables(S: String; EnumSkips: TEnumVariablesSkips; AValues: TStringList = nil): String;

    //When main form needs to switch focus (F6), add to the list all control can take focus
    procedure EnumSwitchControls(vList: TSwitchControls); virtual;
    //run the file or run the project depend on the project tendenc
    property LinesMode: TEditorLinesMode read FLinesMode write SetLinesMode default efmUnix;
    property FileEncoding: String read FFileEncoding write SetFileEncoding;
    property LinesModeAsText: String read GetLinesModeAsText;
    property IsText: Boolean read GetIsText;
    property FileName: String read FFileName write FFileName;
    property BaseName: String read GetBaseName write SetBaseName; //no path with ext
    property NickName: String read GetNickName write SetNickName; //no path without ext
    property Title: String read FTitle write FTitle; //used only if Name is empty for temporary files
    property Path: String read GetPath;
    property FileDate: TDateTime read FFileDate;
    property Params: String read FParams write FParams;
    property Extension: String read GetExtension write SetExtension;
    property Tendency: TEditorTendency read GetTendency;
    property Related: String read FRelated write FRelated;
    property IsChanged: Boolean read FIsChanged write SetIsChanged;
    property IsNew: Boolean read FIsNew write SetIsNew default False;
    property IsTemporary: Boolean read FIsTemporary write FIsTemporary default False;
    property IsReadOnly: Boolean read GetIsReadonly write SetIsReadonly;
    property Group: TFileGroup read FGroup write SetGroup;

    property Content: TWinControl read GetContent; //Container of SynEdit or Grids or Images and all child contrls
    //Control we need to know about it to focus it after open file or changing focus by F6
    property Control: TWinControl read GetControl; //Control if available, maybe Grid or SynEdit or same the content
    //TODO Should not have this here
    property SynEdit: TSynEdit read GetSynEdit; //SynEdit if available
  published
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
    procedure DoLoad(AFileName: String); override;
    procedure DoSave(AFileName: String); override;
    procedure GroupChanged; override;
    procedure DoEdit(Sender: TObject); override;
    procedure DoStatusChange(Sender: TObject; Changes: TSynStatusChanges); override;
    procedure DoGutterClickEvent(Sender: TObject; X, Y, Line: Integer; Mark: TSynEditMark);
    procedure DoSpecialLineMarkup(Sender: TObject; Line: Integer; var Special: Boolean; Markup: TSynSelectedColor);
    procedure DoGetEditCapability(var vEditCapability: TEditCapabilities); override;
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
    function GetHint(HintControl: TControl; CursorPos: TPoint; out vHint: String): Boolean; override;
    function GetGlance: String; override;
    function EvalByMouse(p: TPoint; out v, s, t: String): Boolean;
    function EvalByCursor(out v, s, t: String): Boolean;
    procedure UpdateAge; override;
    function GetLanguageName: String; override;

    procedure Copy; override;
    procedure Paste; override;
    procedure Cut; override;
    procedure SelectAll; override;

    procedure Undo; override;
    procedure Redo; override;
    procedure ClearUndo; override;

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

  { TControlEditorFile }

  TControlEditorFile = class abstract(TEditorFile, IControlEditor)
  private
  protected
  public
  end;

  TOpenFileOption = (ofoActivate, ofoByParam, ofoCheckExists);
  TOpenFileOptions = set of TOpenFileOption;

  { TEditorFiles }

  TEditorFiles = class(TCollection)
  private
    FCheckingChanged: Boolean;
    FCurrent: TEditorFile;
    function GetItems(Index: Integer): TEditorFile;
    function GetCurrent: TEditorFile;
    procedure SetCurrent(const Value: TEditorFile);
  protected
    function SetActiveFile(FileName: String): TEditorFile;
  public
    destructor Destroy; override;
    function FindFile(const vFileName: String): TEditorFile;
    function IsExist(vName: String): Boolean;

    function InternalOpenFile(FileName, FileParams: String; AppendToRecent: Boolean): TEditorFile;
    function LoadFile(vFileName, vFileParams: String; AppendToRecent: Boolean = True): TEditorFile;
    function ShowFile(vFileName: String): TEditorFile; overload; //open it without add to recent, for debuging
    function ShowFile(const FileName: String; Line: Integer): TEditorFile; overload;
    function OpenFile(vFileName: String; vFileParams: String = ''; OpenFileOptions: TOpenFileOptions = []): TEditorFile;
    procedure SetCurrentIndex(Index: Integer; vRefresh: Boolean);
    function New(vGroup: TFileGroup = nil): TEditorFile; overload;
    function New(GroupName: String): TEditorFile; overload;

    procedure Open;
    procedure Save(Force: Boolean);
    procedure SaveAll(Force: Boolean);
    procedure CancelAll;
    procedure ReloadAll;
    procedure CheckAll;
    procedure UpdateAll;
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
    procedure CheckChanged(AFileName: string; Force: Boolean); //slient function
    procedure CloseAll;
    procedure CloseOthers;
    function GetEditedCount: Integer;
    property CheckingChanged: Boolean read FCheckingChanged write FCheckingChanged; //public to use it in SearchInFiles
    property Current: TEditorFile read GetCurrent write SetCurrent;
    property Items[Index: Integer]: TEditorFile read GetItems; default;
  published
  end;

  { TRecentItem }

  TRecentItem = class(TmnXMLItem)
  private
    FName: String;
    FParams: String;
  public
  published
    property Name: String read FName write FName;
    property Params: String read FParams write FParams;
    constructor Create; overload;
    constructor Create(AName: String; AParams: String = ''); overload;
    procedure Assign(Source: TPersistent); override;
  end;

  { TRecentItems }

  TRecentItems = class(specialize GXMLItems<TRecentItem>)
  private
    FMax: Integer;
  protected
  public
    constructor Create(AMax: Integer);
    function IndexOf(AName: String): Integer;
    function Find(AName: String): TRecentItem;
    procedure Insert(AName: String; AParams: String);
    procedure Add(AName: String; AParams: String);
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
    FMainPath: String;
    FFallbackToText: Boolean;
    FIgnoreNames: String;
    FLastFolder: String;
    FLastProject: String;
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

    FMessagesHeight: Integer;
    FFoldersPanelWidth: Integer;

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
    procedure Load(vWorkspace: String);
    procedure Save(vWorkspace: String);
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
    property MainPath: String read FMainPath write FMainPath;
    property LastProject: String read FLastProject write FLastProject;
    property LastFolder: String read FLastFolder write FLastFolder;
    property ExtraExtensions: TStringList read FExtraExtensions write FExtraExtensions;
    property IgnoreNames: String read FIgnoreNames write FIgnoreNames;
    property CollectAutoComplete: Boolean read FCollectAutoComplete write FCollectAutoComplete default False;
    property CollectTimeout: QWORD read FCollectTimeout write FCollectTimeout default 60;
    property ShowFolder: Boolean read FShowFolder write FShowFolder default True;
    property ShowFolderFiles: TShowFolderFiles read FShowFolderFiles write FShowFolderFiles default sffRelated;
    property SortFolderFiles: TSortFolderFiles read FSortFolderFiles write FSortFolderFiles default srtfByNames;
    property ShowMessages: Boolean read FShowMessages write FShowMessages default False;
    property ShowToolbar: Boolean read FShowToolbar write FShowToolbar default True;
    property MessagesHeight: Integer read FMessagesHeight write FMessagesHeight default 100;
    property FoldersPanelWidth: Integer read FFoldersPanelWidth write FFoldersPanelWidth default 180;
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

  TMap = class(TObject)
    Name: String;
    AttType: TAttributeType;
    TokenID: Integer;
    Attribute: TSynHighlighterAttributes;
    ForignName: string;
  end;

  { TMapper }

  TMapper = class(specialize TmnNamedObjectList<TMap>)
  private
  public
    function Add(Attribute: TSynHighlighterAttributes; AttType: TAttributeType; TokenID: Integer = -1): TMap;
    function Add(ForignName: string; Attribute: TSynHighlighterAttributes; AttType: TAttributeType; TokenID: Integer = -1): TMap;
    function IndexOfAttribute(AttributeType: TAttributeType): Integer;
    function FindByAttribute(AttributeType: TAttributeType): TMap;
    function FindByTokenID(TokenID: Integer): TMap;
    function AttributeName(AttributeType: TAttributeType): string;
    function TokenIDOf(AttributeType: TAttributeType): Integer;
  end;

  TFileCategoryKind = (
    fckPublish, //idk not used
    fckIncludes //File can have indclude external files
    );
  TFileCategoryKinds = set of TFileCategoryKind;

  { TVirtualCategory }

  TVirtualCategory = class(TEditorElements)
  private
    FImageName: String;
    FName: String;
    FHighlighter: TSynCustomHighlighter;
    FKind: TFileCategoryKinds;
    FMapper: TMapper;
    FTendency: TEditorTendency;
    FTitle: String;
    function GetHighlighter: TSynCustomHighlighter;
    function GetItem(Index: Integer): TFileGroup;
    function GetMapper: TMapper;
  protected
    FKeywords: TKeywordList;
    FCompletion: TmneSynCompletion;
    FAutoComplete: TSynEditAutoComplete;

    function GetFileCaption(AFile: TEditorFile; FileName: String): String; virtual;

    //-----------
    procedure InitMappers; virtual; abstract;

    procedure InitEdit(vSynEdit: TCustomSynEdit); virtual;
    procedure EndEdit(vSynEdit: TCustomSynEdit); virtual;


    //run once but when category ini
    procedure GetHintString(Token: string; ParamIndex: Integer; out AHint: String); virtual;

    procedure AddKeyword(AKeyword: String; AKind: Integer); deprecated;
    function AddKeyword(AKeyword: String; AttributeName: string; AKind: TAttributeType; Temp: Boolean = False): TKeywordItem; virtual;

    procedure DoAddKeywords; virtual;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); virtual;

    procedure DoPrepareCompletion(AEditor: TCustomSynEdit); virtual; //TODO move it to CodeFileCategory
    procedure PrepareCompletion(ASender: TSynBaseCompletion; var ACurrentString: String; var APosition: Integer; var AnX, AnY: Integer; var AnResult: TOnBeforeExeucteFlags);
    //-----------

    function GetIsText: Boolean; virtual;

    // If filename is empty, that new we need new file
    function OpenFile(vGroup: TFileGroup; vFiles: TEditorFiles; vFileName, vFileParams: String): TEditorFile; virtual;

  public
    constructor Create(ATendency: TEditorTendency; const vName, vTitle: String; vKind: TFileCategoryKinds = []; vImageName: String = ''); virtual;
    constructor Create(ATendency: TEditorTendencyClass; const vName, vTitle: String; vKind: TFileCategoryKinds = []; vImageName: String = ''); virtual;
    destructor Destroy; override;
    procedure EnumMenuItems(AddItems: TAddClickCallBack); virtual;
    function CreateHighlighter: TSynCustomHighlighter; virtual; abstract;
    procedure InitHighlighter;
    property Mapper: TMapper read GetMapper write FMapper;
    procedure Apply(AHighlighter: TSynCustomHighlighter; Attributes: TGlobalAttributes);
    property Name: String read FName write FName;
    property Title: String read FTitle write FTitle;
    function Find(vName: String): TFileGroup;
    procedure EnumExtensions(vExtensions: TStringList);
    function GetExtensions: String;

    procedure ScanValues(AFile: TEditorFile; Values: TStringList; UpdateValues: Boolean = False); virtual;

    procedure EnumVariables(Values: TStringList; EnumSkips: TEnumVariablesSkips); virtual;
    function ReplaceVariables(S: String; Values: TStringList; EnumSkips: TEnumVariablesSkips): String;
    function ReplaceVariables(S: String; EnumSkips: TEnumVariablesSkips): String;

    function GetColorPrefix: String; virtual;
    function FormatColor(Color: TColor): String; virtual;
    function DeformatColor(Str: String): TColor; virtual;
    property Tendency: TEditorTendency read FTendency;
    property IsText: Boolean read GetIsText;
    property Highlighter: TSynCustomHighlighter read GetHighlighter;
    property Completion:  TmneSynCompletion read FCompletion;
    property AutoComplete: TSynEditAutoComplete read FAutoComplete;
    property Kind: TFileCategoryKinds read FKind;
    property ImageName: String read FImageName write FImageName;
    property Items[Index: Integer]: TFileGroup read GetItem; default;
    property Keywords: TKeywordList read FKeywords;
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
    function OpenFile(vGroup: TFileGroup; vFiles: TEditorFiles; vFileName, vFileParams: String): TEditorFile; override;
  public
  end;

  { TTextFileCategory }

  TTextFileCategory = class(TFileCategory)
  private
    FCachedAge: QWord;
    FCachedIdentifiers: THashedStringList;
    FCachedVariables: THashedStringList;
  protected
    function GetIsText: Boolean; override;
  public
    procedure Created; override;
    destructor Destroy; override;
    property CachedVariables: THashedStringList read FCachedVariables; //move to category
    property CachedIdentifiers: THashedStringList read FCachedIdentifiers;
    property CachedAge: QWord read FCachedAge write FCachedAge;
  end;

  { TCodeFileCategory }

  TCodeFileCategory = class(TTextFileCategory)
  protected
    procedure ScanValues(AFile: TEditorFile; Values: TStringList; UpdateValues: Boolean = False); override;
    procedure CacheKeywords(Files: TStringList); virtual;
    procedure DoPrepareCompletion(AEditor: TCustomSynEdit); override;
  end;

  { TFileGroup }

  TFileGroup = class(TEditorElement)
  private
    FCapapility: TRunCapabilities;
    FExtraExtensions: TEditorExtensions;
    FFileClass: TEditorFileClass;
    FExtensions: TEditorExtensions;
    FKind: TFileGroupKinds;
    FCategory: TVirtualCategory;
    function GetExtension: String;
    procedure SetCategory(AValue: TVirtualCategory);
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Init; virtual;
    function OpenFile(vFiles: TEditorFiles; vFileName, vFileParams: String): TEditorFile; virtual;
    procedure EnumExtensions(vExtensions: TStringList; Kind: TFileGroupKinds = []);
    procedure EnumExtensions(vExtensions: TEditorElements);
    property Extension: String read GetExtension; //first one as default extension, do not use the name of group
    property Category: TVirtualCategory read FCategory write SetCategory;
    property Extensions: TEditorExtensions read FExtensions;
    property ExtraExtensions: TEditorExtensions read FExtraExtensions;
    property Kind: TFileGroupKinds read FKind write FKind;
    property Capapility: TRunCapabilities read FCapapility write FCapapility;
    property FileClass: TEditorFileClass read FFileClass; //TODO move to TFileCategory
  end;

  TFileGroupClass = class of TFileGroup;

  { TFileGroups }

  TFileGroups = class(TEditorElements)
  private
    function GetItem(Index: Integer): TFileGroup;
  protected
    procedure InternalAdd(GroupClass: TFileGroupClass; FileClass: TEditorFileClass; const Name, Title: String; Category: TFileCategoryClass; Extensions: array of String; Kind: TFileGroupKinds = []; Capapility: TRunCapabilities = []);
  public
    procedure Init; virtual;

    function Find(vName: String): TFileGroup;
    function Find(vName, vCategory: String): TFileGroup;
    function IsExists(AGroup: TFileGroup): Boolean;

    procedure EnumExtensions(vExtensions: TStringList; Kind: TFileGroupKinds = []);
    procedure EnumExtensions(vExtensions: TEditorElements);

    function FindGroup(vFullName: String; vKind: TFileGroupKinds = []): TFileGroup;
    function OpenFile(vFiles: TEditorFiles; vFileName, vFileParams: String; FallBackGroup: String): TEditorFile;
    //FullFilter return title of that filter for open/save dialog boxes
    function CreateFilter(FullFilter: Boolean = True; FirstExtension: String = ''; vGroup: TFileGroup = nil; OnlyThisGroup: Boolean = True): String;
    function CreateMask(CreateMaskProc: TCreateMaskProc): String;
    procedure Add(vGroup: TFileGroup);
    //First extension in array, is the default extension for the group, do not use name of group as extension
    //ImageName is group name extension already registered in resource
    procedure Add(FileClass: TEditorFileClass; const Name, Title: String; Category: TFileCategoryClass; Extensions: array of String; Kind: TFileGroupKinds = []; Capapility: TRunCapabilities = []);
    property Items[Index: Integer]: TFileGroup read GetItem; default;
  end;

  { TTendencies }

  TTendencies = class(TEditorElements)
  private
    function GetItem(Index: Integer): TEditorTendency;
  public
    procedure Init;
    function Find(vName: String): TEditorTendency;
    function FindByClass(TendencyClass: TEditorTendencyClass): TEditorTendency;
    function Add(vEditorTendency: TEditorTendencyClass): TEditorTendency;
    procedure Add(vEditorTendency: TEditorTendency);
    property Items[Index: Integer]: TEditorTendency read GetItem; default;
  end;

  { TSourceManagements }

  TSourceManagements = class(TEditorElements)
  private
    function GetItem(Index: Integer): TEditorSCM;
  public
    function Find(vName: String): TEditorSCM;
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
    FDefaultSCM: String;
  public
  published
    property DefaultSCM: String read FDefaultSCM write FDefaultSCM;
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
    function GetActive: Boolean;
    function GetMainFile: String;
    function GetMainPath: String;
    procedure SetPanel(AValue: TControl);
    procedure SetInternalProject(const Value: TEditorProject);
    procedure SetRun(AValue: TmneRun);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Changed;
    procedure Load(FileName: String);
    function New(Tendency: TEditorTendency = nil): TEditorProject;
    procedure Open;
    procedure Close;
    function Save(AProject: TEditorProject): Boolean;
    function SaveAs(AProject: TEditorProject): Boolean;
    function Save: Boolean;
    function SaveAs: Boolean;

    function GetRoot: String;

    procedure SetProject(const Value: TEditorProject; LoadFiles: Boolean = True);
    //Is project opened
    property Active: Boolean read GetActive;
    //Current is the opened project, if it is a nil that mean there is no opened project.
    property Project: TEditorProject read FProject;
    property Panel: TControl read FPanel write SetPanel;
    //Session Options is depend on the system used not shared between OSs
    property Options: TEditorSessionOptions read FOptions;
    property MainPath: String read GetMainPath;
    property MainFile: String read GetMainFile;
    property IsChanged: Boolean read FIsChanged;
    //Process the project running if it is null, process should nil it after finish
    property Run: TmneRun read FRun write SetRun;

  end;

  TEditorMessagesList = class;

  TEditorMessage = class(TObject)
  private
    FText: String;
  public
    property Text: String read FText write FText;
  end;

  TEditorMessages = class(specialize TmnObjectList<TEditorMessage>)
  private
    FName: String;
  public
    function GetText(Index: Integer): String;
    property Name: String read FName write FName;
  end;

  TEditorMessagesList = class(specialize TmnNamedObjectList<TEditorMessages>)
  private
  public
    function GetMessages(Name: String): TEditorMessages;
  end;

  TEditorNotifyList = specialize TFPGList<INotifyEngine>;

  TOnFoundEvent = procedure(FileName: String; const Line: String; LineNo, Column, FoundLength: Integer) of object;
  TOnEditorChangeState = procedure(State: TEditorChangeStates) of object;

  TEngineCache = record
    MainFile: String;
    MainGroup: TFileGroup;
  end;

  { TEditorEngine }

  TEditorEngine = class(TObject)
  private
    FDebugLink: TEditorDebugLink;
    FDefaultPath: string;
    FDefaultProject: TDefaultProject;
    FTendencies: TTendencies;
    FSourceManagements: TSourceManagements;
    FUpdateState: TEditorChangeStates;
    FUpdateCount: Integer;
    FFiles: TEditorFiles;
    FFilePanel: TWinControl;
    FProjectPanel: TWinControl;
    FOptions: TEditorOptions;
    FSearchEngine: TSynEditSearch;
    FCategories: TFileCategories;
    FGroups: TFileGroups;
    FExtenstion: String;
    FSession: TEditorSession;
    FMessagesList: TEditorMessagesList;
    FBrowseFolder: String;
    FMacroRecorderPlugin: TSynMacroRecorder;
    FEditorPlugin: TmneSynEditPlugin;
    FHintParams: TSynShowParamsHint;

    FWorkSpace: String;
    FEnvironment: TStringList;
    FCache: TEngineCache;
    function GetSCM: TEditorSCM;
    function GetUpdating: Boolean;
    procedure SetBrowseFolder(const Value: String);
    function GetWorkSpace: String;
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
    procedure DoGetHintString(AEditor: TCustomSynEdit; Token: string; ParamIndex: Integer; out AHint: String);
    procedure DoGetHintExists(AEditor: TCustomSynEdit; Token: string; FunctionsOnly: Boolean; var Exists: Boolean); virtual;
    procedure DoReplaceText(Sender: TObject; const ASearch, AReplace: String; Line, Column: Integer; var ReplaceAction: TSynReplaceAction);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure RegisterNotify(NotifyEngine: INotifyEngine);
    procedure UnregisterNotify(NotifyEngine: INotifyEngine);

    procedure Prepare(vSafeMode: Boolean = False);
    procedure Start; //After Creating MainForm
    procedure LoadOptions;
    procedure UpdatePath;
    procedure UpdateOptions;
    procedure SaveOptions;
    procedure Shutdown;
    function IsShutdown: Boolean;

    //I used it for search in files
    function SearchReplace(const FileName: String; const ALines: TStringList; const ASearch, AReplace: String; OnFoundEvent: TOnFoundEvent; AOptions: TSynSearchOptions): Integer;
    //Recent
    procedure ProcessRecentFile(const FileName: String; FileParams: String = '');
    procedure RemoveRecentFile(const FileName: String);
    procedure ProcessRecentDatabase(const AliasName: String; FileParams: String = '');
    procedure RemoveRecentDatabase(const AliasName: String);
    procedure ProcessRecentFolder(const FileName: String; FileParams: String = '');
    procedure RemoveRecentFolder(const FileName: String);
    procedure ProcessRecentProject(const FileName: String; FileParams: String = '');
    procedure RemoveRecentProject(const FileName: String);

    procedure ProcessProject(const FileName: String; FileParams: String = '');
    procedure RemoveProject(const FileName: String);
    function NewProject: Boolean;
    function ChooseTendency(var vTendency: TEditorTendency): Boolean;

    procedure BeginUpdate;
    //Silent do not trigger just set the flag
    procedure Update(State: TEditorChangeStates; Silent: Boolean = False);
    procedure EndUpdate;
    property Updating: Boolean read GetUpdating;
    property UpdateState: TEditorChangeStates read FUpdateState;

    procedure EnumVariables(Values: TStringList; EnumSkips: TEnumVariablesSkips);
    function ReplaceVariables(S: String; EnumSkips: TEnumVariablesSkips): String;
    function  ExpandFile(FileName: String): String;
    function GetRoot: String;
    property Extenstion: String read FExtenstion write FExtenstion;

    property WorkSpace: String read GetWorkSpace write FWorkSpace;

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
    property BrowseFolder: String read FBrowseFolder write SetBrowseFolder;
    property DebugLink: TEditorDebugLink read FDebugLink;
    property Tendency: TEditorTendency read GetTendency; //It get Project/MainFile/File/Default tendency
    property CurrentTendency: TEditorTendency read GetCurrentTendency; //It get Debug/MainFile/File/Default tendency
    property DefaultProject: TDefaultProject read FDefaultProject; //used when there is no project action, it is assigned to project, but not active
    property SCM: TEditorSCM read GetSCM;
    function GetIsChanged: Boolean;
    property MacroRecorder: TSynMacroRecorder read FMacroRecorderPlugin;
    property EditorPlugin: TmneSynEditPlugin read FEditorPlugin;
    property HintParams: TSynShowParamsHint read FHintParams;
    procedure SendLog(S: String);
    procedure SendMessage(S: String; vMessageType: TNotifyMessageType);
    procedure SendMessage(S: String; vMessageInfo: TMessageInfo);
    procedure SendAction(EditorAction: TEditorAction);

    procedure SaveAll(Force: Boolean);
    procedure Run;
    procedure Execute;

    property DefaultPath: string read FDefaultPath;
    property Environment: TStringList read FEnvironment write FEnvironment;
  published
  end;

  { TListFileSearcher }

  TListFileSearcher = class(TFileSearcher)
  protected
    procedure DoDirectoryFound; override;
    procedure DoFileFound; override;
  public
    List: TStringList;
  end;

function InternalReplaceVariables(S: String; Values: TStringList): String;

function ResetUpdate(State: TEditorChangeState; var InStates: TEditorChangeStates): Boolean;

function SelectFolder(const Caption: String; const Root: String; var Directory: String): Boolean;

procedure SaveAsUnix(Strings: TStrings; Stream: TStream);
procedure SaveAsWindows(Strings: TStrings; Stream: TStream);
procedure SaveAsMAC(Strings: TStrings; Stream: TStream);
procedure SaveAsMode(const FileName: String; Mode: TEditorLinesMode; Strings: TStrings);

function DetectLinesMode(const Contents: String): TEditorLinesMode;
function ConvertLineIndents(const Line: String; TabWidth: Integer; Options: TIndentMode = idntTabsToSpaces): String;
//function ConvertIndents(const Contents: String; TabWidth: Integer; Options: TIndentMode = idntTabsToSpaces): String;
//function CorrectFileText(Contents: String; Mode: TEditorLinesMode; TabWidth: Integer; IndentMode: TIndentMode = idntTabsToSpaces): String;

//EnumFileList return false if canceled by callback function
type
  TFileFindTypes = set of (fftDir, fftFile);

procedure CancelSearch;
function EnumFileList(const Root, Masks, Ignore: String; Callback: TEnumFilesCallback; AObject: TObject; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean; Types: TFileFindTypes = [fftFile]): Boolean;
procedure EnumFileList(const Root, Masks, Ignore: String; Strings: TStringList; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean);
procedure EnumDirList(const Root, Masks, Ignore: String; Strings: TStringList; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean);


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
  TEngineLife = (engnNone, engnStarting, engnStarted, engnShutdowning, engnShutdowned);

var
  FEngineLife: TEngineLife = engnNone;
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
    if FEngineLife = engnShutdowned then
      raise Exception.Create('Engine in shutdown?');
    FEngine := TEditorEngine.Create;
  end;
  Result := FEngine;
end;

function InternalReplaceVariables(S: String; Values: TStringList): String;
begin
  if s <> '' then
  begin
    Result := VarReplace(S, Values, sEnvVarChar, '', ['@']); //* @ for file macro
  end
  else
    Result := '';
end;

function ResetUpdate(State: TEditorChangeState; var InStates: TEditorChangeStates): Boolean;
begin
  Result := State in InStates;
  InStates := InStates - [State];
end;

function SelectFolder(const Caption: String; const Root: String; var Directory: String): Boolean;
begin
  Result := SelectDirectory(Caption, Root, Directory);
end;

procedure SaveAsUnix(Strings: TStrings; Stream: TStream);
var
  i, l: Integer;
  S: String;
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
  S: String;
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
  S: String;
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

{ TmneSynBeautifier }

procedure TmneSynBeautifier.DoBeforeCommand(const ACaret: TSynEditCaret; var Command: TSynEditorCommand);
var
  Line: string;
  CurrentSpaces: Integer;
  Tabs, NewSpaces: Integer;

  LogSpacePos, FillSpace: Integer;
  LogCaret: TPoint;
begin
  if (Command = ecDeleteLastChar) then //* Do not inherited it, i do not want AutoIndent work in Delete
  begin
    if AutoIndent and (ACaret.CharPos > 1) and (not CurrentEditor.ReadOnly) //Should be but not implmented ```and (eoSpacesToTabs in CurrentEditor.Options)
      and (eoTabIndent in CurrentEditor.Options)
		    //and ((not CurrentEditor.SelAvail) or (eoPersistentBlock in CurrentEditor.Options2))
		then
		begin
      CurrentLines.UndoList.CurrentReason := ecSmartUnindent;
      Line := ACaret.LineText;
      CurrentSpaces := GetIndentForLine(CurrentEditor, Line, true); // physical, desired pos
      if (CurrentSpaces>0) and (CurrentSpaces = ACaret.CharPos - 1) then
      begin
        //removing spaces not same size of TabWidth
        Tabs := (CurrentSpaces div TSynEdit(CurrentEditor).TabWidth);
        NewSpaces := Tabs * TSynEdit(CurrentEditor).TabWidth;
        //if not, remove last tab (4)
        if NewSpaces = CurrentSpaces then
          NewSpaces := (Tabs - 1) * TSynEdit(CurrentEditor).TabWidth;

        LogSpacePos := CurrentLines.PhysicalToLogicalCol(Line, ACaret.LinePos-1, NewSpaces + 1);
        FillSpace := NewSpaces + 1 - CurrentLines.LogicalToPhysicalCol(Line, ACaret.LinePos-1, LogSpacePos);
        LogCaret := ACaret.LineBytePos;
        CurrentLines.EditDelete(LogSpacePos, ACaret.LinePos, LogCaret.X - LogSpacePos);
        if FillSpace > 0 then
          CurrentLines.EditInsert(LogSpacePos, ACaret.LinePos, StringOfChar(' ', FillSpace));
        ACaret.CharPos := NewSpaces + 1;
        Command := ecNone;
      end;
    end;
    //do not inherited i we dont unindent it
  end
  else
    inherited DoBeforeCommand(ACaret, Command);
end;

constructor TmneSynBeautifier.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IndentType := sbitCopySpaceTab;
end;

{ TKeywordItem }

constructor TKeywordItem.Create;
begin
  inherited Create;
  FParams := TStringList.Create;
end;

destructor TKeywordItem.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

{ TmneSynCompletionForm }

function TmneSynCompletionForm.GetItemText(Index: Integer): string;
begin
  Result := FKeywords[Index].Name;
end;

function TmneSynCompletionForm.GetItemDisplay(Index: Integer): string;
begin
  Result := FKeywords[Index].Display;
end;

function TmneSynCompletionForm.GetItemsCount: Integer;
begin
  Result := FKeywords.Count;
end;

constructor TmneSynCompletionForm.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  SmartEdit := True;
end;

destructor TmneSynCompletionForm.Destroy;
begin
  inherited Destroy;
end;

function TmneSynCompletion.GetCompletionFormClass: TSynVirtualCompletionFormClass;
begin
  Result := TmneSynCompletionForm;
end;

{ TKeywordList }

procedure TKeywordList.LoadFromFile(FileName: string);
begin
end;

procedure TKeywordList.Clean;
var
  i: Integer;
begin
  i := 0;
  while i < Count do
  begin
    if Self[i].Temp then
      Delete(i)
    else
      Inc(i);
  end;
end;

function TKeywordList.AddItem(AName, ADisplay, AttributeName: string; AttributeType: TAttributeType; AsTemp: Boolean): TKeywordItem;
begin
  Result := TKeywordItem.Create;
  Result.Name := AName;
  Result.Display := ADisplay;
  Result.AttributeType := AttributeType;
  Result.AttributeName := AttributeName;
  Result.Temp := AsTemp;
  //Result.AttributeName:= ;
  Add(Result);
end;

function KeywordListCompare(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TKeywordItem(Item1).Name, TKeywordItem(Item2).Name);
end;

procedure TKeywordList.Sort;
begin
  inherited Sort(@KeywordListCompare);
end;

{ TScannedValues }

procedure TScannedValues.Add(Line: Integer; X1, X2: Integer; Text: string; Value: string);
var
  item: TScannedValue;
begin
  item := TScannedValue.Create;
  item.Line := Line;
  item.X1 := X1;
  item.X2 := X2;
  item.Text := Text;
  item.Value := Value;
  inherited Add(item);
end;

{ TStringsHelper }

procedure TStringsHelper.Merge(FromStrings: TStrings; Overwrite: Boolean);
var
  i: Integer;
begin
  for i := 0 to FromStrings.Count - 1 do
  begin
    if Overwrite or (IndexOfName(FromStrings.Names[i]) < 0) then
      Values[FromStrings.Names[i]] := FromStrings.ValueFromIndex[i];
  end;
end;

procedure TStringsHelper.Merge(Name, Value: String; Overwrite: Boolean);
begin
  if Overwrite or (IndexOfName(Name) < 0) then
    Values[Name] := Value;
end;

{ TmneSynEditPlugin }

procedure TmneSynEditPlugin.OnCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand; var aChar: TUTF8Char; Data: pointer; HandlerData: pointer);
var
  aGUID: TGUID;
begin
  if Command = FCommandIDs[mscUUID] then
  begin
    CreateGUID(aGUID);
    TCustomSynEdit(Sender).InsertTextAtCaret(LowerCase(RemoveEncloseStr(GUIDToString(aGUID), '{', '}')));
    Handled := True;
  end;
end;

procedure TmneSynEditPlugin.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited;
  HookEditor(AValue, FCommandIDs[mscUUID], 0, FShortCuts[mscUUID], []);
  AValue.RegisterCommandHandler(@OnCommand, Self, [hcfPreExec]);
end;

procedure TmneSynEditPlugin.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  inherited;
  UnHookEditor(AValue, FCommandIDs[mscUUID], FShortCuts[mscUUID]);
  AValue.UnregisterCommandHandler(@OnCommand);
end;

constructor TmneSynEditPlugin.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FCommandIDs[mscUUID] := AllocatePluginKeyRange(1);
  FShortCuts[mscUUID] := Menus.ShortCut(Ord('G'), [ssCtrl, ssShift]);
end;

{ TFileCategory }

function TFileCategory.OpenFile(vGroup: TFileGroup; vFiles: TEditorFiles; vFileName, vFileParams: String): TEditorFile;
begin
  if vGroup = nil then
    raise Exception.Create('Group is nil');
  if (vFileName <> '') then
    vFileName := ExpandFileName(vFileName);
  Result := vGroup.FFileClass.Create(vFiles);
  Result.Group := vGroup;
  if (vFileName <> '') and FileExists(vFileName) then
    Result.LoadFromFile(vFileName)
end;

{ TEditorDesktopFile }

constructor TEditorDesktopFile.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  CaretX := 1;
  CaretY := 1;
  TopLine := 1;
end;

{ TRecentItem }

constructor TRecentItem.Create(AName: String; AParams: String);
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

function TRecentItems.IndexOf(AName: String): Integer;
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

function TRecentItems.Find(AName: String): TRecentItem;
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

procedure TRecentItems.Insert(AName: String; AParams: String);
begin
  inherited Insert(0, TRecentItem.Create(AName, AParams));
  while Count > Max do
    Delete(Max);
end;

procedure TRecentItems.Add(AName: String; AParams: String);
var
  i: Integer;
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
  FSynEdit := TmneSynEdit.Create(Engine.FilePanel);
  SynEdit.Visible := False;
  (SynEdit as TmneSynEdit).FEditorFile := Self;
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

procedure TmneSynEdit.DoOnShowHint(HintInfo: PHintInfo);
var
  s: String;
begin
  inherited DoOnShowHint(HintInfo);
  if FEditorFile <> nil then
  begin
    FEditorFile.GetHint(Self, HintInfo^.CursorPos, s);
    //  CanShow := s <> '';

    if s <> '' then
      with HintInfo^ do
      begin
        HintStr := s;
        HideTimeout := 10000;
        ReshowTimeout := 500;
      end;
  end;
end;

constructor TmneSynEdit.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  FmneSynBeautifier := TmneSynBeautifier.Create(Self);
  Beautifier := FmneSynBeautifier;

  i := Keystrokes.FindKeycode(VK_NEXT, [ssCtrl]);
  if i >= 0 then
    Keystrokes.Delete(i);
  i := Keystrokes.FindKeycode(VK_PRIOR, [ssCtrl]);
  if i >= 0 then
    Keystrokes.Delete(i);
end;

destructor TmneSynEdit.Destroy;
begin
  Beautifier := nil;
  FreeAndNil(FmneSynBeautifier);
  inherited Destroy;
end;

procedure TmneSynEdit.ExecuteCommand(Command: TSynEditorCommand; const AChar: TUTF8Char; Data: pointer);
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

procedure TEditorExtensions.Add(Name: String; ImageIndex: Integer);
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

procedure TmneWatches.Add(AName: String);
var
  Item: TmneWatch;
begin
  Item := TmneWatch.Create;
  Item.Name := AName;
  inherited Add(Item);
end;

{ TmneBreakpoints }

procedure TmneBreakpoints.Add(AFileName: String; ALineNo: Integer);
var
  Item: TmneBreakpoint;
begin
  Item := TmneBreakpoint.Create;
  Item.FileName := AFileName;
  Item.LineNo := ALineNo;
  inherited Add(Item);
end;

{ TCodeFileCategory }

procedure TCodeFileCategory.ScanValues(AFile: TEditorFile; Values: TStringList; UpdateValues: Boolean);

  function ScanID(s: String; start: Integer; out Stop: integer; out Name, Value: String): Boolean;
  begin
    Stop := Start;
    while true do
    begin
      if (Stop > Length(s)) or CharInSet(s[Stop], sWhitespace + [':', '=']) then
      begin
        Name := MidStr(S, Start, Stop - Start);
        Value := Trim(MidStr(S, Stop + 1, MaxInt));
        Result := True;
        Break;
      end
      else
        Inc(Stop);
    end;
  end;

var
  aToken: String;
  aTokenPos: Integer;
  aCommentKind: Integer;
  p: Integer;
  aSynEdit: TCustomSynEdit;
  aName, aValue: String;
  aLineNumber: Integer;
  aLine: string;
  Stop: Integer;
  ScannedValues: TScannedValues;

  procedure UpdateValue(Name, Value: string);
  var
    x1, x2: Integer;
    //    s: string;
  begin
    x1 := aTokenPos + Stop;
    x2 := Max(Length(aLine) + 1, x1 + length(Value));
    ScannedValues.Add(aLineNumber, x1, x2, Name, Value);

{    s := MidStr(aLine, 1, x1);
    s := s + Value;
    s := s + MidStr(aLine, x2, MaxInt);}
    //aSynEdit.Lines[aLineNumber] := s;
(*
    p1 := Point(aTokenPos + Stop + 1, aLineNumber + 1);
    p2 := Point(Max(Length(aLine) + 1, p1.x + length(Value)), aLineNumber + 1); //to the end of line, baaah no but temporary
    aSynEdit.TextBetweenPointsEx[p1, p2, scamIgnore] := Value;
*)
  end;

var
  item: TScannedValue;
  rev: Integer;
  p1, p2: TPoint;
begin
  inherited;
  aCommentKind := Mapper.TokenIDOf(attDocument);
  if aCommentKind < 0 then
    aCommentKind := Mapper.TokenIDOf(attComment);
  if aCommentKind >= 0 then
  begin
    ScannedValues:=TScannedValues.Create;
    aSynEdit := AFile.SynEdit;
    Screen.Cursor := crHourGlass;
    try
      if (aSynEdit <> nil) and (Highlighter <> nil) then
      begin
        try
          Highlighter.ResetRange;
          for aLineNumber := 0 to aSynEdit.Lines.Count - 1 do
          begin
            aLine := aSynEdit.Lines[aLineNumber];
            Highlighter.SetLine(aLine, aLineNumber);
            while not Highlighter.GetEol do
            begin
              aToken := Highlighter.GetToken;
              aTokenPos := Highlighter.GetTokenPos;
              if (Highlighter.GetTokenKind = aCommentKind) then
              begin
                p := Pos('@', aToken);
                if p > 0 then
                begin
                  if ScanID(aToken, p {+ 1}, Stop, aName, aValue) then //we take @ with names
                  begin
                    aName := Trim(aName);
                    aValue := Trim(aValue);
                    if aName = '@updated' then
                    begin
                      aValue := ' "' + ISODateToStr(AFile.FileDate, '-', ' ', True) + '"';
                      UpdateValue(aName, aValue);
                    end
                    else if aName = '@timestamp' then
                    begin
                      aValue := ' ' + IntToStr(DateTimeToUnix(AFile.FileDate));
                      UpdateValue(aName, aValue);
                    end
                    else if aName = '@revision' then
                    begin
                      rev := StrToIntDef(aValue, 0) + 1;
                      aValue := ' ' + IntToStr(rev);
                      UpdateValue(aName, aValue);
                    end
                    {else if aName = 'filename' then
                    begin
                      aValue := ' ' + AFile.Name;
                      UpdateValue(aValue);
                    end;}
                  end;
                  Values.AddPair(aName, aValue);
                end;
              end;
              Highlighter.Next;
            end;
          end;
        finally
        end;
      end;
      if UpdateValues then
      begin
        aSynEdit.BeginUpdate(False);
        try
          for item in ScannedValues do
          begin
            {
            nop no undo work into
            Line := aSynEdit.Lines[item.Line];
            aSynEdit.Lines[Item.Line] := MidStr(aLine, 1, item.x1) + Item.Value + MidStr(aLine, item.x2, MaxInt);
            }
            p1 := Point(item.X1 + 1, item.Line + 1);
            p2 := Point(Item.X2, item.Line + 1);
            aSynEdit.TextBetweenPointsEx[p1, p2, scamIgnore] := Item.Value;

          end;
        finally
          aSynEdit.EndUpdate;
        end;
      end;
    finally
      FreeAndNil(ScannedValues);
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TCodeFileCategory.CacheKeywords(Files: TStringList);
var
  aFile: TStringList;
  s: String;
  i, f: Integer;
  aHighlighter: TSynCustomHighlighter;
  aIdentifierID, aVariableID: Integer;
begin
  aIdentifierID := Mapper.TokenIDOf(attIdentifier);
  aVariableID := Mapper.TokenIDOf(attVariable);
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
          if (aHighlighter.GetTokenKind = aIdentifierID) then
          begin
            s := aHighlighter.GetToken;
            if CachedIdentifiers.IndexOf(s) < 0 then
              CachedIdentifiers.Add(s);
          end
          else if (aHighlighter.GetTokenKind = aVariableID) then
          begin
            s := aHighlighter.GetToken;
            if CachedVariables.IndexOf(s) < 0 then
              CachedVariables.Add(s);
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

procedure TCodeFileCategory.DoPrepareCompletion(AEditor: TCustomSynEdit);
var
  aCurrent, Token: String;
  i, r: Integer;
  aLine: Integer;
  aSynEdit: TCustomSynEdit;
  aFiles: TStringList;
  aIdentifierID, aVariableID: Integer;
begin
  inherited;
  Screen.Cursor := crHourGlass;
  aIdentifierID := Mapper.TokenIDOf(attIdentifier);
  aVariableID := Mapper.TokenIDOf(attVariable);

  aSynEdit := Completion.TheForm.CurrentEditor as TCustomSynEdit;
  if (aSynEdit <> nil) and (Highlighter <> nil) then
  begin
    aLine := aSynEdit.CaretY;
    aCurrent := aSynEdit.GetWordAtRowCol(aSynEdit.LogicalCaretXY);

    //load keyowrds
    //extract keywords from external files
    if (fckIncludes in Kind) and Engine.Options.CollectAutoComplete and (Engine.Session.GetRoot <> '') then
    begin
      if ((GetTickCount - CachedAge) > (Engine.Options.CollectTimeout * 1000)) then
      begin
        CachedVariables.Clear;
        CachedIdentifiers.Clear;
        aFiles := TStringList.Create;
        try
          EnumFileList(Engine.Session.GetRoot, GetExtensions, Engine.Options.IgnoreNames, aFiles, 1000, 0, Engine.Session.Active);//TODO check the root dir if no project opened
          r := aFiles.IndexOf(Engine.Files.Current.FileName);
          if r >= 0 then
            aFiles.Delete(r);
          CacheKeywords(aFiles);
        finally
          aFiles.Free;
        end;
      end;
      CachedAge := GetTickCount;
    end;

    //add current file Identifiers
    Highlighter.ResetRange;
    for i := 0 to aSynEdit.Lines.Count - 1 do
    begin
      Highlighter.SetLine(aSynEdit.Lines[i], 1);
      while not Highlighter.GetEol do
      begin
        Token := Highlighter.GetToken;
        if (i <> aLine - 1) or (aCurrent <> Token) then //TODO what is (i <> aLine - 1) ????
        begin
          if (Highlighter.GetTokenKind = aIdentifierID) then
          begin
            if (Keywords.Find(Token) = nil) then
              AddKeyword(Token, 'Identifier', attIdentifier, True);
          end
          else if (Highlighter.GetTokenKind = aVariableID) and (Keywords.Find(Token) = nil) then
            AddKeyword(Token, 'Variable', attVariable, True)
        end;
        Highlighter.Next;
      end;
    end;
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
  inherited;
end;

procedure TEditorProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
begin
end;

procedure TEditorProjectOptions.CreateProjectPanel(AOwner: TComponent; AProject: TEditorProject; var AFrame: TFrame);
begin
end;

{ TRunOptions }

procedure TRunOptions.SetPaths(AValue: TStrings);
begin
  FPaths.Assign(AValue);
end;

procedure TRunOptions.Merge(AOptions: TRunOptions);

  function iif(v, s: String): String;
  begin
    if s <> '' then
      Result := s
    else
      Result := v;
  end;

  function iif(v, s: Boolean): Boolean;
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

procedure TRunOptions.Copy(AOptions: TRunOptions);
begin
  FInfo := AOptions.FInfo;
  FPaths.Assign(AOptions.Paths);
end;

procedure TRunOptions.Assign(Source: TPersistent);
begin
  if Source is TRunOptions then
    Copy(Source as TRunOptions)
  else
    inherited Assign(Source);
end;

constructor TRunOptions.Create;
begin
  inherited;
  FPaths := TStringList.Create;
  FInfo.Pause := stateTrue;
  FInfo.Console := stateTrue;
end;

destructor TRunOptions.Destroy;
begin
  FreeAndNil(FPaths);
  inherited Destroy;
end;


{ TTextFileCategory }

function TTextFileCategory.GetIsText: Boolean;
begin
  Result := True;
end;

procedure TTextFileCategory.Created;
begin
  inherited;
  FCachedVariables := THashedStringList.Create;
  FCachedIdentifiers := THashedStringList.Create;
end;

destructor TTextFileCategory.Destroy;
begin
  FCachedVariables.Free;
  FCachedIdentifiers.Free;
  inherited;
end;

{ TMapper }

function TMapper.Add(Attribute: TSynHighlighterAttributes; AttType: TAttributeType; TokenID: Integer): TMap;
begin
  Result := TMap.Create;
  Result.Attribute := Attribute;
  Result.Name := Attribute.StoredName;
  Result.AttType := AttType;
  Result.TokenID := TokenID;
  inherited Add(Result); //if there is a bug inside add, you need to fix it by code, so no need to catch it
end;

function TMapper.Add(ForignName: string; Attribute: TSynHighlighterAttributes; AttType: TAttributeType; TokenID: Integer): TMap;
begin
 Result := TMap.Create;
 Result.Attribute := Attribute;
 Result.Name := Attribute.StoredName;
 Result.AttType := AttType;
 Result.TokenID := TokenID;
 Result.ForignName := ForignName;
 inherited Add(Result); //if there is a bug inside add, you need to fix it by code, so no need to catch it
end;

function TMapper.IndexOfAttribute(AttributeType: TAttributeType): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if Items[i].AttType = AttributeType then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TMapper.FindByAttribute(AttributeType: TAttributeType): TMap;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].AttType = AttributeType then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TMapper.FindByTokenID(TokenID: Integer): TMap;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].TokenID = TokenID then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TMapper.AttributeName(AttributeType: TAttributeType): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    if Items[i].AttType = AttributeType then
    begin
      Result := Items[i].Name;
      break;
    end;
  end;
end;

function TMapper.TokenIDOf(AttributeType: TAttributeType): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if Items[i].AttType = AttributeType then
    begin
      Result := Items[i].TokenID;
      break;
    end;
  end;
end;

{ TSyntaxEditorFile }

procedure TSyntaxEditorFile.SetHighlightLine(AValue: Integer);
begin
  if FHighlightLine <> AValue then
  begin
    SynEdit.InvalidateLine(FHighlightLine);
    FHighlightLine := AValue;
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

procedure TSyntaxEditorFile.DoLoad(AFileName: String);
begin
  ContentsLoadFromFile(SynEdit, AFileName);
end;

procedure TSyntaxEditorFile.DoSave(AFileName: String);
begin
  ContentsSaveToFile(SynEdit, AFileName);
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
  end
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

procedure TSyntaxEditorFile.DoGutterClickEvent(Sender: TObject; X, Y, Line: Integer; Mark: TSynEditMark);
var
  aLine: Integer;
begin
  if (Tendency.Debugger <> nil) and (capDebug in Group.Capapility) then
  begin
    aLine := SynEdit.PixelsToRowColumn(Point(X, Y)).y;
    DebugManager.Enter;
    try
      Tendency.Debugger.Breakpoints.Toggle(FileName, aLine);
    finally
      DebugManager.Leave;
    end;
    SynEdit.InvalidateLine(aLine);
  end;
end;

procedure TSyntaxEditorFile.DoSpecialLineMarkup(Sender: TObject; Line: Integer; var Special: Boolean; Markup: TSynSelectedColor);
var
  aColor: TColor;
begin
  if (Engine.DebugLink.ExecutedControl = Sender) then
  begin
    if Engine.DebugLink.ExecutedLine = Line then
    begin
      Special := True;
      aColor := Engine.Options.Profile.Attributes.Default.Background;
      Markup.Background := MixColors(aColor, ContrastColor(Engine.Options.Profile.Attributes.Default.Background), 200);
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
  end;
  {else if  (Sender as TSynEdit).CaretY = Line then
  begin
    Special := True;
    Markup.BackAlpha := 100;
    Markup.Background := Engine.Options.Profile.Attributes.Active.Background;
  end;}
end;

procedure TSyntaxEditorFile.DoGetEditCapability(var vEditCapability: TEditCapabilities);
begin
  inherited;

  if SynEdit.SelAvail then
    vEditCapability := vEditCapability + [ecpAllowCopy];

  if SynEdit.CanPaste then
    vEditCapability := vEditCapability + [ecpAllowPaste];
  vEditCapability := vEditCapability + [ecpAllowUndo];
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
    Key := VK_DELETE;
    Shift := [ssCtrl];
    Command := ecDeleteWord;
  end;

  with SynEdit.Keystrokes.Add do
  begin
    Key := VK_K;
    Shift := [ssCtrl];
    Key2 := VK_L;
    Shift2 := [ssCtrl];
    Command := ecLowerCaseBlock;
  end;

  with SynEdit.Keystrokes.Add do
  begin
    Key := VK_K;
    Shift := [ssCtrl];
    Key2 := VK_U;
    Shift2 := [ssCtrl];
    Command := ecUpperCaseBlock;
  end;

  with SynEdit.Keystrokes.Add do
  begin
    Key := VK_SPACE;
    Shift := [ssShift];
    Command := ecAutoCompletion;
  end;

  Engine.MacroRecorder.AddEditor(SynEdit);
  Engine.EditorPlugin.AddEditor(SynEdit);
  Engine.HintParams.AddEditor(SynEdit);
end;

destructor TSyntaxEditorFile.Destroy;
begin
  inherited;
end;

procedure TSyntaxEditorFile.BeforeDestruction;
begin
  Engine.HintParams.RemoveEditor(SynEdit);
  Engine.MacroRecorder.RemoveEditor(SynEdit);
  Engine.EditorPlugin.RemoveEditor(SynEdit);
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
    Tendency.PrepareSynEdit(SynEdit);
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

function TSyntaxEditorFile.GetHint(HintControl: TControl; CursorPos: TPoint; out vHint: String): Boolean;
var
  v, s, t: String;
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

function TSyntaxEditorFile.GetGlance: String;
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

function TSyntaxEditorFile.EvalByMouse(p: TPoint; out v, s, t: String): Boolean;
var
  l: Variant;
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

function TSyntaxEditorFile.EvalByCursor(out v, s, t: String): Boolean;
var
  l: Variant;
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

function TSyntaxEditorFile.GetLanguageName: String;
begin
  if (SynEdit <> nil) and (SynEdit.Highlighter <> nil) then
    Result := SynEdit.Highlighter.GetLanguageName
  else
    Result := inherited;
end;

procedure TSyntaxEditorFile.Copy;
begin
  SynEdit.CopyToClipboard;
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

procedure TSyntaxEditorFile.Undo;
begin
  inherited;
  SynEdit.Undo;
end;

procedure TSyntaxEditorFile.Redo;
begin
  inherited
  SynEdit.Redo;
end;

procedure TSyntaxEditorFile.ClearUndo;
begin
  inherited;
  SynEdit.ClearUndo;
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

procedure TmneSynCompletion.OnSynCompletionNextChar(Sender: TObject);
var
  NewPrefix: String;
  Line: String;
  LogCaret: TPoint;
  CharLen: LongInt;
  AddPrefix: String;
begin
  if Editor = nil then
    exit;
  LogCaret:=Editor.LogicalCaretXY;
  if LogCaret.Y>Editor.Lines.Count then
    exit;
  Line:=Editor.Lines[LogCaret.Y-1];
  if LogCaret.X>length(Line) then exit;
  CharLen:=UTF8CodepointSize(@Line[LogCaret.X]);
  AddPrefix:=copy(Line,LogCaret.X,CharLen);
  if not Editor.IsIdentChar(AddPrefix) then exit;
  NewPrefix:=CurrentString+AddPrefix;
  inc(LogCaret.X);
  Editor.LogicalCaretXY:=LogCaret;
  CurrentString:=NewPrefix;
end;

procedure TmneSynCompletion.OnSynCompletionPrevChar(Sender: TObject);
var
  NewPrefix: String;
  NewLen: LongInt;
begin
  NewPrefix:=CurrentString;
  if NewPrefix='' then exit;
  if Editor=nil then exit;
  Editor.CaretX:=Editor.CaretX-1;
  NewLen:=UTF8FindNearestCharStart(PChar(NewPrefix),length(NewPrefix),
                                   length(NewPrefix)-1);
  NewPrefix:=copy(NewPrefix,1,NewLen);
  CurrentString:=NewPrefix;
end;

procedure TmneSynCompletion.OnSynCompletionKeyPress(Sender: TObject; var Key: Char);
begin
  if (System.Pos(Key,EndOfTokenChr)>0) then begin
    TheForm.OnValidate(Sender,Key,[]);
    Key:=#0;
  end;
end;

procedure TmneSynCompletion.OnSynCompletionUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if (length(UTF8Key)=1)
  and (System.Pos(UTF8Key[1],EndOfTokenChr)>0) then begin
    TheForm.OnValidate(Sender,UTF8Key,[]);
    UTF8Key:='';
  end;
end;

constructor TmneSynCompletion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnKeyNextChar:=@OnSynCompletionNextChar;
  OnKeyPrevChar:=@OnSynCompletionPrevChar;
  OnKeyPress:=@OnSynCompletionKeyPress;
  OnUTF8KeyPress:=@OnSynCompletionUTF8KeyPress;
end;

procedure TmneSynCompletion.Sort;
begin
  inherited Sort;
  (TheForm as TmneSynCompletionForm).Keywords.Sort;
end;

procedure TmneSynCompletion.Clear;
begin
 inherited Sort;
 (TheForm as TmneSynCompletionForm).Keywords.Clear;
end;

{ TEditorSCM }

constructor TEditorSCM.Create;
begin
  inherited Create;
end;

destructor TEditorSCM.Destroy;
begin
  inherited Destroy;
end;

{ TSourceManagements }

function TSourceManagements.GetItem(Index: Integer): TEditorSCM;
begin
  Result := inherited Items[Index] as TEditorSCM;
end;

function TSourceManagements.Find(vName: String): TEditorSCM;
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
  FDescription := 'Default project tendency';
end;

{ TEditorFormList }

function TEditorFormList.Find(ObjectClass: TClass): TEditorFormItem;
var
  i: Integer;
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

procedure TEditorTendency.SendMessage(S: String; vMessageType: TNotifyMessageType);
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

function TEditorTendency.Can(ACapability: TRunCapability): Boolean;
begin
  Result := (ACapability in Capabilities) or ((Engine.Files.Current <> nil) and (ACapability in Engine.Files.Current.Group.Capapility));
end;

function TEditorTendency.CanAny(ACapability: TRunCapabilities): Boolean;
begin
  Result := ((ACapability * Capabilities) <> []) or ((Engine.Files.Current <> nil) and ((ACapability * Engine.Files.Current.Group.Capapility)<>[]));
end;

procedure TEditorTendency.EnumVariables(Values: TStringList; EnumSkips: TEnumVariablesSkips);
begin
  if not (evsTendicy in EnumSkips) then
  begin
    Values.Merge('DefaultPath', ExcludePathDelimiter(DefaultPath));
    Values.Merge('MainPath', ExcludePathDelimiter(DefaultPath));
  end;
  Engine.EnumVariables(Values, EnumSkips + [evsTendicy]);
end;

function TEditorTendency.ReplaceVariables(S: String; EnumSkips: TEnumVariablesSkips): String;
var
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    EnumVariables(Values, EnumSkips);
    Result := InternalReplaceVariables(S, Values);
  finally
    Values.Free;
  end;
end;

procedure TEditorTendency.UpdatePath;
begin
  if RunOptions.MainPath <> '' then
    FDefaultPath := ExpandToPath(RunOptions.MainPath, Engine.DefaultPath)
   else
    FDefaultPath := Engine.DefaultPath;
end;

function TEditorTendency.GetIsDefault: Boolean;
begin
  Result := False;
end;

procedure TEditorTendency.AddGroup(vName: String; vCategory: TVirtualCategory);
var
  G: TFileGroup;
begin
  if vCategory = nil then
    G := Engine.Groups.Find(vName)
  else
    G := vCategory.Find(vName);

  if G = nil then
    raise Exception.Create(vName + ' file group not found');
  Groups.Add(G);
end;

procedure TEditorTendency.AddGroup(vName, vCategory: String);
var
  C: TVirtualCategory;
begin
  if vCategory = '' then
    C := nil
  else
    C := Engine.Categories.Find(vCategory);
  AddGroup(vName, C);
end;

procedure TEditorTendency.AddGroup(vName: String; vCategoryClass: TVirtualCategoryClass);
var
  C: TVirtualCategory;
begin
  if vCategoryClass = nil then
    C := nil
  else
    C := Engine.Categories.FindByClass(vCategoryClass);
  AddGroup(vName, C);
end;

function TEditorTendency.CreateDebugger: TEditorDebugger;
begin
  Result := nil;
end;

procedure TEditorTendency.Init;
begin
  ImageIndex := EditorResource.GetImageIndex(Name, -1);
  Groups.Init;
end;

procedure TEditorTendency.Created;
begin
end;

constructor TEditorTendency.Create;
begin
  inherited;
  FGroups := TFileGroups.Create(False);//it already owned by Engine.Groups
  FRunOptions := TRunOptions.Create;
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

procedure TEditorTendency.HelpKeyword(AWord: String);
begin
end;

procedure TEditorTendency.Run(RunActions: TmneRunActions); //please check dublicate in  M:\home\pascal\projects\miniEdit\source\extends\commons\mneCustomClasses.pas#DoRun
begin
  if (rnaExecute in RunActions) then
  begin
	  if (capCompile in Capabilities) and not (capExecute in Capabilities) then
      RunActions := RunActions + [rnaCompile] - [rnaExecute]
    else if (capLint in Capabilities) and not (capExecute in Capabilities) then
      RunActions := RunActions + [rnaLint] - [rnaExecute];
  end;
  InternalRun(RunActions);
end;

procedure TEditorTendency.InternalRun(RunActions: TmneRunActions); //please check dublicate in  M:\home\pascal\projects\miniEdit\source\extends\commons\mneCustomClasses.pas#DoRun
var
  p: TmneRunInfo;
  AOptions: TRunOptions;
begin
  if (Debugger <> nil) and (Engine.Options.AutoStartDebug = asdRun) then
    Debugger.Active := True;

  AOptions := TRunOptions.Create;//Default options
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

        if (rnaCompile in RunActions) or (rnaLint in RunActions) then
          Engine.SendAction(eaClearOutput);

        p.Root := Engine.Session.GetRoot;
        p.Command := ReplaceVariables(AOptions.Command, []);

        p.Pause := AOptions.Pause in [stateTrue];
        p.Console := AOptions.Console in [stateTrue, stateNone];

        if Engine.Session = nil then
          raise Exception.Create('No Session active!');
        if Engine.Session.Project = nil then
          raise Exception.Create('No Project opened!');
        p.MainFile := Engine.ExpandFile(Engine.Session.Project.RunOptions.MainFile); //TODO: here need to care about expand file to be similar to env variable

        if (p.MainFile = '') and (Engine.Files.Current <> nil) then
          p.MainFile := Engine.Files.Current.FileName;

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
          begin
            DoRun(p);
            Engine.Session.Run.Start(Debugger, p.Root);
          end;
        Engine.Update([ecsDebug]);
      end;
    end;
  finally
    FreeAndNil(AOptions)
  end;
end;

procedure TEditorTendency.CreateOptionsFrame(AOwner: TComponent; AddFrame: TAddFrameCallBack);
begin
end;

function TEditorTendency.CreateProjectOptions: TEditorProjectOptions;
begin
  Result := TEditorProjectOptions.Create;
end;

function TEditorTendency.CreateProject: TEditorProject;
begin
  Result := TRunProject.Create;
end;

procedure TEditorTendency.PrepareSynEdit(SynEdit: TSynEdit);
begin
end;

procedure TEditorTendency.EnumRunCommands(Items: TStrings);
begin
end;

function TEditorTendency.GetDefaultGroup: TFileGroup;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Groups.Count - 1 do
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

function TTendencies.GetItem(Index: Integer): TEditorTendency;
begin
  Result := inherited Items[Index] as TEditorTendency;
end;

procedure TTendencies.Init;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Init;
end;

function TTendencies.Find(vName: String): TEditorTendency;
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

function TEditorEngine.SearchReplace(const FileName: String; const ALines: TStringList; const ASearch, AReplace: String; OnFoundEvent: TOnFoundEvent; AOptions: TSynSearchOptions): Integer;
var
  i: Integer;
  nSearchLen, nReplaceLen, n, nChar: Integer;
  Count: Integer;
  iResultOffset: Integer;
  aLine, aReplaceText: String;
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
      Count := SearchEngine.FindAll(aLine);
      iResultOffset := 0;
      n := 0;
      // Operate on all results in this line.
      Replaced := False;
      while Count > 0 do
      begin
        // An occurrence may have been replaced with a text of different length
        nChar := SearchEngine.Results[n] + iResultOffset;
        nSearchLen := SearchEngine.ResultLengths[n];
        Inc(n);
        Dec(Count);

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
  i: Integer;
  List: TFileCategories;
begin
  List := TFileCategories.Create(False);
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

    Engine.HintParams.Hint.BorderColor := Profile.Attributes.Highlighted.Background;
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
  i: Integer;
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

procedure TEditorFiles.CheckChanged(AFileName: string; Force: Boolean);
var
  aFile: TEditorFile;
begin
  Engine.BeginUpdate;
  try
    aFile := FindFile(AFileName);
    if aFile <> nil then
      aFile.CheckChanged(Force);
  finally
    Engine.EndUpdate;
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
  FMacroRecorderPlugin := TSynMacroRecorder.Create(nil);
  FMacroRecorderPlugin.OnStateChange := @DoMacroRecorderChanged;
  FEditorPlugin := TmneSynEditPlugin.Create(nil);
  FHintParams := TSynShowParamsHint.Create(nil);
  FHintParams.OnGetHintString := @DoGetHintString;
  FHintParams.OnGetHintExists := @DoGetHintExists;
  FHintParams.UsePrettyText := True;
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
  FreeAndNil(FMacroRecorderPlugin);
  FreeAndNil(FHintParams);
  FreeAndNil(FEditorPlugin);
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

var
  FCancel: Boolean = False;

procedure CancelSearch;
begin
  FCancel := True;
end;

function EnumFileList(const Root, Masks, Ignore: String; Callback: TEnumFilesCallback; AObject: TObject; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean; Types: TFileFindTypes): Boolean;
var
  Resume: Boolean;
  IgnoreList: TStringList;
  MaskList: TMaskList;
  aCount: Integer;

  procedure DoFind(const Root, Path: String; vLevel: Integer);
  var
    sr: TSearchRec;
    f: String;
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
              ((IgnoreList <> nil) and (IgnoreList.IndexOf(sr.Name) >= 0)) or not ((MaskList = nil) or (MaskList.Matches(sr.Name))) then
              continue;
            if ReturnFullPath then
              f := Root + IncludePathDelimiter(Path) + sr.Name
            else
              f := IncludePathDelimiter(Path) + sr.Name;
            Callback(AObject, f, aCount, vLevel, False, Resume);
            if (aCount mod 100) = 0 then
              Application.ProcessMessages;
            if FCancel or ((vMaxCount > 0) and (aCount > vMaxCount)) then
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
              if (aCount mod 10) = 0 then
                Application.ProcessMessages;
              if (vMaxLevel = 0) or (vLevel < vMaxLevel) then
                DoFind(Root, IncludePathDelimiter(Path + sr.Name), vLevel);
            end;
          until (FindNext(sr) <> 0);
        end;
  end;

begin
  FCancel := False;
  if Ignore <> '' then
  begin
    IgnoreList := TStringList.Create;
    StrToStrings(Ignore, IgnoreList, [';'], [' ']);
    IgnoreList.Sort;
    IgnoreList.Sorted := True;
  end
  else
    IgnoreList := nil;

  if Masks <> '' then
    MaskList := TMaskList.Create(Masks)
  else
    MaskList := nil;
  aCount := 0;
  Resume := True;
  try
    DoFind(IncludeTrailingPathDelimiter(Root), '', 0);
  finally
    FreeAndNil(IgnoreList);
    FreeAndNil(MaskList);
  end;
  Result := Resume;
end;

procedure EnumFileListStringsCallback(AObject: TObject; const FileName: String; Count, Level: Integer; IsDirectory: Boolean; var Resume: Boolean);
begin
  TStringList(AObject).Add(FileName);
end;

procedure EnumFileList(const Root, Masks, Ignore: String; Strings: TStringList; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean);
begin
  EnumFileList(Root, Masks, Ignore, @EnumFileListStringsCallback, Strings, vMaxCount, vMaxLevel, ReturnFullPath);
end;

procedure EnumDirList(const Root, Masks, Ignore: String; Strings: TStringList; vMaxCount, vMaxLevel: Integer; ReturnFullPath: Boolean);
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
var
  strings: array[low(TIndentMode)..high(TIndentMode)] of string = ('Keep it', 'Tab to Spaces', 'Spaces to Tabs', 'Align Tabs', 'Align Spaces');
  s: string;
begin
  vItems.Clear;
  for s in strings do
    vItems.Add(s);
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

procedure TEditorEngine.DoReplaceText(Sender: TObject; const ASearch, AReplace: String; Line, Column: Integer; var ReplaceAction: TSynReplaceAction);
var
  i: Integer;
begin
  for i := 0 to FNotifyObjects.Count - 1 do
  begin
    if FNotifyObjects[i] is INotifyEngineEditor then
      (FNotifyObjects[i] as INotifyEngineEditor).EngineReplaceText(Sender, ASearch, AReplace, Line, Column, ReplaceAction);
  end;
end;

procedure TEditorEngine.UpdatePath;
begin
  if Options.MainPath <> '' then
    FDefaultPath := ExpandToPath(Options.MainPath, Application.Location)
  else
    FDefaultPath := Application.Location;
end;

function TEditorFiles.FindFile(const vFileName: String): TEditorFile;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(vFileName, Items[i].FileName) then
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

function TEditorFiles.GetEditedCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    if Items[i].IsChanged and not Items[i].IsTemporary then
      Result := Result + 1;
  end;
end;

function TEditorEngine.GetRoot: String;
begin
  Result := Session.GetRoot;
end;

function TEditorEngine.GetSCM: TEditorSCM;
begin
  if Session.Project <> nil then
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
    end;
  end;
end;

function TEditorFiles.InternalOpenFile(FileName, FileParams: String; AppendToRecent: Boolean): TEditorFile;
var
  lFileName: String;
  FallbackGroup: String;
begin
  {$ifdef windows}
  lFileName := ExpandFileName(FileName);
  {$else}
  if ExtractFilePath(FileName) = '' then
    lFileName := IncludeTrailingPathDelimiter(SysUtils.GetCurrentDir()) + FileName
  else
    lFileName := FileName;
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
  if (Result <> nil) and AppendToRecent and Result.CanAddRecentFiles then
    Engine.ProcessRecentFile(lFileName);
end;

procedure TEditorOptions.Load(vWorkspace: String);

  procedure SafeLoad(s: TRecentItems; vName: String);
  begin
    if FileExists(vName) then
      s.LoadFromFile(vName);
  end;

  procedure SafeLoad(s: TStringList; vName: String);
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

procedure TEditorSession.Load(FileName: String);
var
  aProject: TEditorProject;
begin
  Close; //* TODO:  wrong for first time loading,  must free before load project for save the desktop and sure to save its files
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
    if FProject.IsActive then
      Engine.BrowseFolder := FProject.RunOptions.MainPath;
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

function TEditorFiles.New(GroupName: String): TEditorFile;
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

function TEditorFiles.OpenFile(vFileName: String; vFileParams: String; OpenFileOptions: TOpenFileOptions): TEditorFile;
begin
  if SameText(ExtractFileExt(vFileName), Engine.Extenstion) then //zaher need dot
  begin
    Engine.Session.Load(vFileName);
    Result := nil; //it is a project not a file.
  end
  else
  begin
    if not (ofoCheckExists in OpenFileOptions) or FileExists(vFileName) then
    begin
      Result := LoadFile(vFileName, vFileParams);
      if Result <> nil then
      begin
        Current := Result;
        if (ofoActivate in OpenFileOptions) and not Result.Activated then
          Result.Activate;
        if (ofoByParam in OpenFileOptions) and Result.CanAddRecentFiles and (not Engine.Session.Active) then
          if Count = 1 then //first one
            Engine.BrowseFolder := ExtractFilePath(vFileName);
      end;
    end
    else
      Result := nil;
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
  Result := Save(Project);
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
  Result := SaveAs(Project);
end;

function TEditorSession.GetRoot: String;
var
  r: String;
begin
  if (Engine.Session.Active) and (Engine.Session.Project.RunOptions.MainPath <> '') then
    Result := GetMainPath
  else if (Engine.Session.Active) and (Engine.Session.Project.RunOptions.MainFile <> '') then
  begin
    r := Engine.Session.Project.RunOptions.MainFile;
    r := ExpandToPath(r, Engine.Session.Project.DefaultPath);
    Result := ExtractFilePath(Engine.ReplaceVariables(r, [evsDefaultPath]));
  end
  else if Engine.Files.Current <> nil then
    Result := Engine.Files.Current.Path
  else if Engine.BrowseFolder <> '' then
    Result := Engine.BrowseFolder
  else
    Result := Application.Location;
  Result := ExpandFileName(IncludePathDelimiter(Result));
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

procedure TEditorEngine.ProcessRecentFile(const FileName: String; FileParams: String);
begin
  Options.RecentFiles.Add(FileName, FileParams);
  Update([ecsRecents]);
end;

procedure TEditorEngine.ProcessRecentFolder(const FileName: String; FileParams: String);
begin
  Options.RecentFolders.Add(FileName, FileParams);
  Update([ecsRecents]);
end;

procedure TEditorEngine.ProcessRecentProject(const FileName: String; FileParams: String);
begin
  Options.RecentProjects.Add(FileName, FileParams);
  Update([ecsRecents]);
end;

procedure TEditorEngine.ProcessProject(const FileName: String; FileParams: String);
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
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Save(Force);
  end;
end;

procedure TEditorFiles.CancelAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].IsChanged := False;
  end;
end;

procedure TEditorFiles.ReloadAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Load;
  end;
end;

procedure TEditorFiles.CheckAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].CheckChanged(True);
  end;
end;

procedure TEditorFiles.UpdateAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Update;
  end;
end;

procedure TEditorFiles.SaveAs;
begin
  if Current <> nil then
    Current.Save(True, ExtractFileExt(Current.FileName), True);
end;

procedure TEditorOptions.Save(vWorkspace: String);
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
var
  i: Integer;
begin
  if FCurrent <> Value then
  begin
    FCurrent := Value;
    for i := 0 to Count - 1 do
    begin
      if Items[i] <> FCurrent then
        Items[i].Hide;
    end;
    FCurrent.Show;
    if not Engine.Updating then
      FCurrent.Activate;
    Engine.Update([ecsDebug, ecsRefresh]);
  end;
end;

procedure TEditorFiles.SetCurrentIndex(Index: Integer; vRefresh: Boolean);
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

function TEditorSession.GetMainFile: String;
begin
  if Project.RunOptions.MainFile <> '' then
    Result := Engine.ExpandFile(Project.RunOptions.MainFile)
  else
    Result := '';
end;

procedure TEditorOptions.OptionsShow;
var
  aSelect: String;
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
  aSelect: String;
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

procedure TEditorEngine.RemoveProject(const FileName: String);
var
  i: Integer;
begin
  i := Options.Projects.IndexOf(FileName);
  if i >= 0 then
    Options.Projects.Delete(i);
  Update([ecsRecents]);
end;

function TEditorEngine.ChooseTendency(var vTendency: TEditorTendency): Boolean;
var
  aName: String;
begin
  if (vTendency <> nil) then
    aName := vTendency.Name
  else
    aName := '';
  Result := ShowSelectList('Select project tendency', Tendencies, [slfSearch], aName, EditorResource.FileImages); //slfIncludeNone
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
  lFilePath: String;
begin
  if (ParamCount > 0) then
  begin
    lFilePath := DequoteStr(ParamStr(1));
    if FileExists(lFilePath) then
    begin
      Session.SetProject(DefaultProject, False);
      Files.OpenFile(lFilePath, '', [ofoActivate, ofoByParam]);
    end
    else
    begin
		  if DirectoryExists(lFilePath) then
        BrowseFolder := lFilePath
      else
        BrowseFolder := Options.LastFolder;
      Session.SetProject(DefaultProject, False);
    end;
    //BrowseFolder := ExtractFilePath(lFilePath);//nop maybe it is a temp folder check CanAddRecentFiles
    //The filename is expanded, if necessary, in EditorEngine.TEditorFiles.InternalOpenFile
  end
  else
  begin
    if Engine.Options.AutoOpenProject and ((Options.LastProject <> '') and FileExists(Options.LastProject)) then
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
  aFile: String;
  i: Integer;
begin
  BeginUpdate;
  try
    Options.Load(Workspace);
    UpdatePath;
    Session.Options.SafeLoadFromFile(Workspace + 'mne-options-' + SysPlatform + '.xml');
    for i := 0 to Tendencies.Count - 1 do
    begin
      if Tendencies[i].HaveOptions then
      begin
        aFile := Workspace + 'mne-tendency-' + LowerCase(Tendencies[i].Name) + '.xml';
        if FileExists(aFile) then
          XMLReadObjectFile(Tendencies[i], aFile);
        Tendencies[i].UpdatePath;
      end;
    end;
    //After loading options
    for i := 0 to FNotifyObjects.Count - 1 do
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
  s: String;
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
          for j := 0 to lStrings.Count - 1 do
          begin
            s := lStrings[j];
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
  aFile: String;
  i: Integer;
begin
  ForceDirectories(Workspace);
  for i := 0 to FNotifyObjects.Count - 1 do //before Options.Save(WorkSpace) maybe it save into options
  begin
    if (FNotifyObjects[i] is INotifyEngineSetting) then
      (FNotifyObjects[i] as INotifyEngineSetting).SaveOptions;
  end;
  Options.Save(WorkSpace);
  Session.Options.SaveToFile(Workspace + 'mne-options-' + SysPlatform + '.xml');
  for i := 0 to Tendencies.Count - 1 do
  begin
    if Tendencies[i].HaveOptions then
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
  if BrowseFolder <> '' then
    Options.LastFolder := BrowseFolder;
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

procedure TEditorEngine.RemoveRecentProject(const FileName: String);
var
  i: Integer;
begin
  i := Options.RecentProjects.IndexOf(FileName);
  if i >= 0 then
    Options.RecentProjects.Delete(i);
  Update([ecsRecents]);
end;

procedure TEditorEngine.RemoveRecentFile(const FileName: String);
var
  i: Integer;
begin
  i := Options.RecentFiles.IndexOf(FileName);
  Options.RecentFiles.Delete(i);
  Update([ecsRecents]);
end;

procedure TEditorEngine.ProcessRecentDatabase(const AliasName: String; FileParams: String);
begin
  Options.RecentDatabases.Add(AliasName, FileParams);
  Update([ecsRecents]);
end;

procedure TEditorEngine.RemoveRecentDatabase(const AliasName: String);
var
  i: Integer;
begin
  i := Options.RecentDatabases.IndexOf(AliasName);
  Options.RecentFiles.Delete(i);
  Update([ecsRecents]);
end;

procedure TEditorEngine.RemoveRecentFolder(const FileName: String);
var
  i: Integer;
begin
  i := Options.RecentFolders.IndexOf(FileName);
  Options.RecentFolders.Delete(i);
  Update([ecsRecents]);
end;

function TEditorEngine.GetUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TEditorEngine.EnumVariables(Values: TStringList; EnumSkips: TEnumVariablesSkips);
var
  MainFile: String;
begin
  if not (evsEngine in EnumSkips) then
  begin
    Values.Merge('Date', ISODateToStr(Now, '-', ' ', False));
    Values.Merge('DateTime', ISODateToStr(Now, '-', ' ', True));
    Values.Merge('TimeStamp', IntToStr(DateTimeToUnix(Now)));

    Values.Merge('FileDate', ISODateToStr(Now, '-', '-', '-', False));
    Values.Merge('FileDateTime', ISODateToStr(Now, '-', '-', '-', True));

    if not (evsEnviroment in EnumSkips) then
      Values.Merge(Environment);

    if not (evsDefaultPath in EnumSkips) then
    begin
      Values.Merge('Root', GetRoot);
    end;

    if Files.Current <> nil then
      Files.Current.EnumVariables(Values, EnumSkips + [evsEngine]);

    Tendency.EnumVariables(Values, EnumSkips + [evsEngine]);

    if Session.Active then
    begin
      MainFile := Session.Project.RunOptions.MainFile;
      MainFile := ExpandToPath(MainFile, Session.Project.DefaultPath);
    end
    else
      MainFile := '';

    if (MainFile = '') and (Files.Current <> nil) then
      MainFile := Files.Current.FileName;

    if MainFile <> '' then
    begin
      Values.Merge('Main', MainFile);
      Values.Merge('MainFile', MainFile);
      Values.Merge('MainFileName', ExtractFileName(MainFile));
      Values.Merge('MainFileNickName', ExtractFileNameWithoutExt(ExtractFileName(MainFile)));
      Values.Merge('MainPath', ExcludePathDelimiter(ExtractFilePath(MainFile)));
    end;

    if Session.Active then
    begin
      if not (evsProject in EnumSkips) then
      begin
        Values.Merge('Project', Session.Project.FileName);
        Values.Merge('ProjectFile', Session.Project.FileName);
        Values.Merge('ProjectPath', ExcludePathDelimiter(Session.Project.DefaultPath));
        Values.Merge('OutputName', Session.Project.RunOptions.OutputFile); //TODO need to gues)s
      end;
    end;
  end;
end;

function TEditorEngine.ReplaceVariables(S: String; EnumSkips: TEnumVariablesSkips): String;
var
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    EnumVariables(Values, EnumSkips);
    Result := InternalReplaceVariables(S, Values);
  finally
    Values.Free;
  end;
end;

function TEditorEngine.ExpandFile(FileName: String): String;
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

procedure TEditorEngine.SendLog(S: String);
begin
  SendMessage(S, msgtLog);
end;

procedure TEditorEngine.SendMessage(S: String; vMessageType: TNotifyMessageType);
var
  aMessageInfo: TMessageInfo;
begin
  aMessageInfo := Default(TMessageInfo);
  aMessageInfo.MessageType := vMessageType;
  SendMessage(S, aMessageInfo);
end;

procedure TEditorEngine.SendMessage(S: String; vMessageInfo: TMessageInfo);
var
  i: Integer;
begin
  if not IsShutdown then
    for i := 0 to FNotifyObjects.Count - 1 do
    begin
      if FNotifyObjects[i] is INotifyEngineEditor then
        (FNotifyObjects[i] as INotifyEngineEditor).EngineMessage(S, vMessageInfo);
    end;
end;

procedure TEditorEngine.SendAction(EditorAction: TEditorAction);
var
  i: Integer;
begin
  if not IsShutdown then
  begin
    for i := 0 to FNotifyObjects.Count - 1 do
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
    for i := 0 to FNotifyObjects.Count - 1 do
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

function TEditorFiles.LoadFile(vFileName, vFileParams: String; AppendToRecent: Boolean): TEditorFile;
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
    if MsgBox.Yes('Revert file ' + Current.FileName) then
      Current.Load;
  end;
end;

procedure TEditorEngine.SetBrowseFolder(const Value: String);
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

procedure TEditorEngine.DoGetHintString(AEditor: TCustomSynEdit; Token: string; ParamIndex: Integer; out AHint: String);
begin
  AHint := '';//* We do not want to show dummy hint
  if (AEditor is TmneSynEdit) then
  begin
    if (AEditor as TmneSynEdit).EditorFile <> nil then
    begin
      (AEditor as TmneSynEdit).EditorFile.Group.Category.GetHintString(Token, ParamIndex, AHint);
    end;
  end;
end;

procedure TEditorEngine.DoGetHintExists(AEditor: TCustomSynEdit; Token: string; FunctionsOnly: Boolean; var Exists: Boolean);
var
  aKeyword: TKeywordItem;
begin
  if (AEditor is TmneSynEdit) then
  begin
    if (AEditor as TmneSynEdit).EditorFile <> nil then
    begin
      aKeyword := (AEditor as TmneSynEdit).EditorFile.Group.Category.Keywords.Find(Token);
      Exists := (aKeyword <> nil) and (not FunctionsOnly or (aKeyword.AttributeType in [attCommon]));
    end;
  end
  else
    Exists := False;
end;

function TEditorEngine.GetWorkSpace: String;
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

function TEditorFiles.GetItems(Index: Integer): TEditorFile;
begin
  Result := inherited Items[Index] as TEditorFile;
end;

function TEditorFiles.IsExist(vName: String): Boolean;
begin
  Result := FindFile(vName) <> nil;
end;

function TEditorFiles.SetActiveFile(FileName: String): TEditorFile;
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

function TEditorFiles.ShowFile(vFileName: String): TEditorFile;
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

function TEditorFiles.ShowFile(const FileName: String; Line: Integer): TEditorFile;
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
  i: Integer;
  mr: TmsgChoice;
  a: Boolean;
begin
  if (FileName <> '') or not IsTemporary then
  begin
    if IsChanged then
    begin
      mr := MsgBox.YesNoCancel('Save file ' + FileName + ' before close?');
      if mr = msgcCancel then
        Abort
      else if mr = msgcYes then
        Save(True);
    end;
    if (FileName <> '') and CanAddRecentFiles then
      Engine.ProcessRecentFile(FileName);
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

procedure TEditorFile.BeforeDestruction;
begin
  if GetSynEdit <> nil then
    Group.Category.EndEdit(GetSynEdit);
  inherited;
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

procedure TEditorFile.LoadFromFile(AFileName: String);
{var
  aBackupFileName: string;}
begin
  FileName := ExpandFileName(AFileName);
  FileName := AFileName;
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
  {if Tendency.EnableMacros then
  begin
    Engine.Files.Current.ScanValues(Values);
    aBackupFileName := DequoteStr(Values.Values['@localfile']);
    aBackupFileName := ExpandToPath(aBackupFileName , Engine.Session.Project.DefaultPath);
    if (aBackupFileName <> '') and FileExists(aBackupFileName) and not SameFileName(aBackupFileName, Engine.Files.Current.FileName) then
    begin
      Engine.SendMessage('There is a backup file check if there is a differents: ' + aBackupFileName, msgtStatus);
      //DequoteStr(Values.Values['@localfile']);
      //MsgBox.Yes()
    end;
  end;}
end;

procedure SaveAsMode(const FileName: String; Mode: TEditorLinesMode; Strings: TStrings);
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

procedure TEditorFile.SaveToFile(AFileName: String);
var
  Values: TStringList;
  BackupFileName: String;
  i: Integer;
  aName, aValue: string;
begin
  FFileDate := Now;
  Values := TStringList.Create;
  try
    if Tendency.EnableMacros then
      ScanValues(Values, True);

    //* check to backup file
    if Tendency.EnableMacros then
    begin
      try
        for i := 0 to Values.Count -1 do
        begin
          aName := Values.Names[i];
          aValue := Values.ValueFromIndex[i];
          if SameText(aName, '@localfile') then
          begin
              BackupFileName := DequoteStr(aValue, '"');
              //BackupFileName := ReplaceVariables(BackupFileName, [], Values);
              BackupFileName := ExpandToPath(BackupFileName, Engine.Session.Project.DefaultPath);
              if not SameFileName(BackupFileName, AFileName) then //not the same file
              begin
                DoSave(BackupFileName);
                Engine.Files.CheckChanged(BackupFileName, True);
                //TODO Send update age if opened in the editor
                Engine.SendLog('Saved as backup: ' + BackupFileName);
              end;
          end;
         end;
      except
        on E: Exception do
        begin
          Engine.SendMessage(E.Message, msgtLog);
        end;
      end;
    end;
  finally
    FreeAndNil(Values);
  end;
  DoSave(AFileName);
  FileName := AFileName;
  IsChanged := False;
  IsTemporary := False;
  IsNew := False;
  FileSetDate(FileName, FileDate);
  Engine.Update([ecsFolder]);
  UpdateAge;
end;

procedure TEditorFile.Rename(ToName: String);
var
  p: String;
  aExt: String;
  aGroup: TFileGroup;
begin
  Engine.BeginUpdate;
  try
    if FileName <> '' then
    begin
      p := ExtractFilePath(FileName);
      if RenameFile(FileName, p + ToName) then
      begin
        Engine.RemoveRecentFile(FileName);
        FileName := p + ToName;
        Engine.ProcessRecentFile(FileName);

        aExt := Extension;
        if LeftStr(aExt, 1) <> '.' then
          aExt := '.' + aExt;
        aGroup := Engine.Groups.FindGroup(aExt);
        Group := aGroup;
      end;
    end
    else
      FileName := ToName;
    Engine.Update([ecsRefresh, ecsFolder, ecsState, ecsChanged]);
  finally
    Engine.EndUpdate;
  end;
end;

procedure TEditorFile.Delete;
begin
  Engine.BeginUpdate;
  try
    if FileName <> '' then
    begin
      if DeleteFile(FileName) then
      begin
        Engine.RemoveRecentFile(FileName);
        FileName := ExtractFileName(FileName);
        IsNew := True;
        IsChanged := True;
      end;
    end;
    Engine.Update([ecsRefresh, ecsFolder, ecsState, ecsChanged]);
  finally
    Engine.EndUpdate;
  end;
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

procedure TEditorFile.Hide;
begin
  if Content <> nil then
  begin
    //    Content.Visible := False;
    Content.Hide;
    Content.SendToBack;
    //Activate;//no no, bad when return back from another app to miniedit
  end;
end;

function TEditorFile.Visible: Boolean;
begin
  Result := (Content <> nil) and (Content.Visible);
end;

procedure TEditorFile.Save(Force: Boolean; Extension: String; AsNewFile: Boolean);
var
  aDialog: TSaveDialog;
  aSave, DoRecent: Boolean;
  aName: String;
begin
  DoRecent := False;
  aName := '';
  if (((FileName <> '') or not IsTemporary) or Force) then
  begin
    if (IsNew or (FFileName = '') or AsNewFile) then
    begin
      aDialog := TSaveDialog.Create(nil);
      aDialog.Title := 'Save file';
      aDialog.Filter := Engine.Groups.CreateFilter(True, Extension, Group, False);//put the group of file as the first one

      if FileName <> '' then
      begin
        aDialog.InitialDir := ExtractFilePath(FileName);
        aDialog.FileName := FileName;
      end
      else
      begin
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
      end;

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
      aName := FFileName;
      aSave := True;
    end;
  end
  else
    aSave := False;

  if aSave then
  begin
    SaveToFile(aName);
    FFileName := aName;
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
  if (FileName <> '') and (FileExists(FileName)) then //even if marked as new
  begin
    if IsNew or ((FFileDate <> FileDateToDateTime(FileAge(FileName))) or (FFileSize <> FileSize(FileName))) then
    begin
      if Force or (Engine.Options.Profile.AutoUpdateFile and not IsChanged) then
        mr := msgcYes
      else
        mr := MsgBox.YesNoCancel(FileName + #13' was changed, update it?');
      if mr = msgcYes then
        Load;
      if mr = msgcCancel then
        Result := False
      else
        UpdateAge;
    end;
  end
  else if not IsNew then
  begin
    if Force then
      n := -1 //nothing
    else
      n := MsgBox.Ask(FileName + #13' was not found, what do want?', [Choice('&Keep It', msgcYes), Choice('&Close', msgcCancel), Choice('Read only', msgcNo)], 0, 2);
    if n = -1 then //do nothing
    else if n = 0 then //Keep It
      IsNew := True
    else if n = 2 then //Keep It
    begin
      IsChanged := False;
      IsTemporary := False;
      IsReadOnly := True;
    end
    else
      Close;
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

function TEditorFile.GetHint(HintControl: TControl; CursorPos: TPoint; out vHint: String): Boolean;
begin
  Result := False;
end;

function TEditorFile.GetGlance: String;
begin
  Result := '';
end;

function TEditorFile.GetCaption: String;
begin
  if FileName = '' then
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
    Result := ExtractFileName(FileName);
  if Group <> nil then
    Result := Group.Category.GetFileCaption(Self, Result);
end;

function TEditorFile.GetLanguageName: String;
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
  Result := ecpAllowCopy in EditCapability;
end;

function TEditorFile.CanPaste: Boolean;
begin
  Result := ecpAllowPaste in EditCapability;
end;

function TEditorFile.CanUndo: Boolean;
begin
  Result := ecpAllowUndo in EditCapability;
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

procedure TEditorFile.Undo;
begin

end;

procedure TEditorFile.Redo;
begin

end;

procedure TEditorFile.ClearUndo;
begin

end;

procedure TEditorFile.ScanValues(Values: TStringList; UpdateValues: Boolean);
var
  i: Integer;
begin
  Group.Category.ScanValues(Self, Values, UpdateValues);
  for i := 0 to Values.Count-1 do
    Values[i] := ReplaceVariables(Values[i], [], Values);
end;

procedure TEditorFile.EnumVariables(Values: TStringList; EnumSkips: TEnumVariablesSkips);
begin
  if not (evsFile in EnumSkips) then
  begin
    Values.Merge('File', FileName);
    Values.Merge('FileName', FileName);
    Values.Merge('FileBaseName', BaseName);
    Values.Merge('BaseName', BaseName);
    Values.Merge('FileNickName', NickName);
    Values.Merge('NickName', NickName);
    Values.Merge('FilePath', ExcludePathDelimiter(Path));
  end;
  Group.Category.EnumVariables(Values, EnumSkips + [evsFile]);
end;

function TEditorFile.ReplaceVariables(S: String; EnumSkips: TEnumVariablesSkips; AValues: TStringList): String;
var
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    EnumVariables(Values, EnumSkips);
    if AValues <> nil then
      Values.Merge(AValues);
    Result := InternalReplaceVariables(S, Values);
  finally
    Values.Free;
  end;
end;

procedure TEditorFile.EnumSwitchControls(vList: TSwitchControls);
begin
  if Control <> nil then
    vList.Add(Control);
end;

procedure TEditorFile.UpdateAge;
begin
  FFileDate := FileDateToDateTime(FileAge(FileName));
  FFileSize := FileSize(FileName);
  IsNew := False;
end;

procedure TEditorFile.Load;
begin
  LoadFromFile(FileName);
end;

procedure TEditorFile.SetGroup(const Value: TFileGroup);
begin
  if FGroup <> Value then
  begin
    FGroup := Value;
    GroupChanged;
  end;
end;

function TEditorFile.GetEditCapability: TEditCapabilities;
begin
  Result := [];
  DoGetEditCapability(Result);
end;

function TEditorFile.GetRunCapability: TRunCapabilities;
begin
  Result := [];
  DoGetRunCapability(Result);
end;

function TEditorFile.GetControl: TWinControl;
begin
  Result := GetContent;
end;

function TEditorFile.GetIsText: Boolean;
begin
  Result := (Group <> nil) and Group.Category.IsText;
end;

function TEditorFile.GetBaseName: String;
begin
  Result := ExtractFileName(FileName);
end;

function TEditorFile.GetExtension: String;
begin
  Result := ExtractFileExt(FileName);
end;

function TEditorFile.GetPath: String;
begin
  Result := ExtractFilePath(FileName);
end;

function TEditorFile.GetNickName: String;
begin
  Result := ExtractFileNameWithoutExt(BaseName);
end;

function TEditorFile.GetTendency: TEditorTendency;
begin
  if Group <> nil then
    Result := Group.Category.Tendency
  else
    Result := Engine.DefaultProject.Tendency;
end;

procedure TEditorFile.SetFileEncoding(AValue: String);
begin
  if FFileEncoding <> AValue then
  begin
    FFileEncoding := AValue;
    Edit;
    Engine.Update([ecsState, ecsRefresh]);
  end;
end;

procedure TEditorFile.SetExtension(AValue: String);
begin
  if LeftStr(AValue, 1) <> '.' then
    AValue := '.' + AValue;
  Rename(NickName + AValue);
end;

function TEditorFile.GetContent: TWinControl;
begin
  Result := nil;
end;

function TEditorFile.GetSynEdit: TSynEdit;
begin
  Result := nil;
end;

procedure TEditorFile.DoGetEditCapability(var vEditCapability: TEditCapabilities);
begin
  vEditCapability := [];
end;

procedure TEditorFile.DoGetRunCapability(var vRunCapability: TRunCapabilities);
begin
  vRunCapability := [];
end;

procedure TEditorFile.ContentsLoadFromStream(SynEdit: TSynEdit; AStream: TStream);
var
  Contents: String;
  Size: Integer;
  Encoded: Boolean;
begin
  SynEdit.BeginUpdate;
  try
    Size := AStream.Size - AStream.Position;
    SetString(Contents, nil, Size);
    AStream.Read(Pointer(Contents)^, Size);
    FileEncoding := GuessEncoding(Contents);
    if FileEncoding = '' then
      FileEncoding := 'UTF8';
    if not SameText(FileEncoding, EncodingUTF8) then
      Contents := ConvertEncodingToUTF8(Contents, FileEncoding, Encoded);
    LinesMode := DetectLinesMode(Contents);
    if IsNew then //there is no undo here
    begin
      if (Contents <> '') and ((Contents[Length(Contents)] = #13) or (Contents[Length(Contents)] = #10)) then
        Contents := Contents + #13#10; //TODO stupid idea because SetText of strings ignore last line
      SynEdit.Lines.Text := Contents
    end
    else
    begin //allow to make undo when reloaded file
      SynEdit.BeginUndoBlock;  //adding it to history of undo, so we can undo the revert to changes in by external
      try
        //SynEdit.Lines.Text := Contents
        //SynEdit.Text := Contents
        SynEdit.TextBetweenPoints[Point(1, 1), Point(length(SynEdit.Lines[SynEdit.Lines.Count - 1]) + 1, SynEdit.Lines.Count)] := Contents;
      finally
        SynEdit.EndUndoBlock;
      end;
    end;

  finally
    SynEdit.EndUpdate;
  end;
end;

{procedure TEditorFile.ContentsSaveToStream(SynEdit: TSynEdit; AStream: TStream);
var
  Contents: Rawbytestring;
  IndentMode: TIndentMode;
begin
  IndentMode := Engine.Options.Profile.IndentMode;
  if Tendency.OverrideEditorOptions then
    IndentMode := Tendency.IndentMode;

  Contents := CorrectFileText(SynEdit.Lines.Text, LinesMode, SynEdit.TabWidth, IndentMode);

  if not SameText(FileEncoding, EncodingUTF8) then
  begin
    Contents := ConvertEncoding(Contents, EncodingUTF8, FileEncoding, False);
    if FileEncoding = EncodingUTF8 then
      Contents := UTF8BOM + Contents
    else if FileEncoding = EncodingUCS2LE then
      Contents := UTF16LEBOM + Contents
    else if FileEncoding = EncodingUCS2BE then
      Contents := UTF16BEBOM + Contents;
  end;

  AStream.WriteBuffer(Pointer(Contents)^, Length(Contents));
end;}

procedure TEditorFile.ContentsSaveToStream(SynEdit: TSynEdit; AStream: TStream);
var
//  Contents: Rawbytestring;
  aToEncoding, aLineEnding: string;
  IndentMode: TIndentMode;
  Line: Rawbytestring;
  i: Integer;
begin
  IndentMode := Engine.Options.Profile.IndentMode;
  if Tendency.OverrideEditorOptions then
    IndentMode := Tendency.IndentMode;

  if not SameText(FileEncoding, EncodingUTF8) then
  begin
    if FileEncoding = EncodingUTF8BOM then
      AStream.WriteBuffer(UTF8BOM, Length(UTF8BOM))
    else if FileEncoding = EncodingUCS2LE then
      AStream.WriteBuffer(UTF16LEBOM, Length(UTF16LEBOM))
    else if FileEncoding = EncodingUCS2BE then
      AStream.WriteBuffer(@UTF16BEBOM[1], Length(UTF16BEBOM))
  end;

  if SameText(RightStr(FileEncoding, 3), 'BOM') then
    aToEncoding := SubStr(FileEncoding, 1, -3)
  else
    aToEncoding := FileEncoding;

  case LinesMode of
    efmWindows:
		  aLineEnding := #$D#$A;
    efmMac:
		  aLineEnding := #$D;
    else
      aLineEnding := #$A;
  end;

  for i := 0 to SynEdit.Lines.Count - 1 do
  begin
    Line := ConvertLineIndents(SynEdit.Lines[i], SynEdit.TabWidth, IndentMode);
    if (i < SynEdit.Lines.Count - 1) then // Not Last Line
      Line := Line + aLineEnding;
    Line := ConvertEncoding(Line, EncodingUTF8, aToEncoding, False);
    AStream.WriteBuffer(Pointer(Line)^, Length(Line));
  end;
end;

procedure TEditorFile.ContentsLoadFromFile(SynEdit: TSynEdit; FileName: String);
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

procedure TEditorFile.ContentsSaveToFile(SynEdit: TSynEdit; FileName: String);
var
  aStream: TFileStream;
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
  if FIsNew = AValue then
    Exit;
  FIsNew := AValue;
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

function TEditorFile.CanAddRecentFiles: Boolean;
begin
  Result := True;
end;

function DetectLinesMode(const Contents: String): TEditorLinesMode;
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

function ConvertLineIndents(const Line: String; TabWidth: Integer; Options: TIndentMode): String;
var
  l, i, c, t: Integer;
begin
  Result := '';
  c := 0;
  l := Length(Line);
  i := 1;
  while i <= l do
  begin
    if Line[i] = ' ' then
      c := c + 1
    else if Line[i] = #9 then
      c := c + TabWidth
    else
      break;
    Inc(i);
  end;

  t := 0;
  if Options = idntSpacesToTabs then
  begin
    t := c div TabWidth;
    c := c mod TabWidth;
  end
  else if Options = idntAlignSpaces then
    c := Round(c / TabWidth) * TabWidth
  else if Options = idntAlignTabs then
  begin
    c := Round(c / TabWidth) * TabWidth;
    t := c div TabWidth;
    c := c mod TabWidth; //* or c := 0
  end;

  Result := Copy(Line, i, MaxInt);
  Result := RepeatString(#9, t) + RepeatString(' ', c)+ Copy(Line, i, MaxInt);
end;

{ bug when one line without eol
function ConvertIndents(const Contents: String; TabWidth: Integer; Options: TIndentMode): String;
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
      Inc(i);
    Result := Result + Copy(Contents, p, i - p + 1);
    p := i + 1;
  end;

  procedure ScanSpaces;
  var
    i, c, t: Integer;
  begin
    i := p;
    c := 0;
    t := 0;
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
    end
    else if Options = idntAlignSpaces then
      c := Ceil(c / TabWidth) * TabWidth
    else if Options = idntAlignTabs then
    begin
      c := Ceil(c / TabWidth) * TabWidth;
      t := c div TabWidth;
      c := c mod TabWidth; //* or c := 0
    end;

    Result := Result + RepeatString(#9, t);
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
end;}

{function CorrectFileText(Contents: String; Mode: TEditorLinesMode; TabWidth: Integer; IndentMode: TIndentMode): String;
var
  Strings: TStringList;
  i: Integer;
begin
  Strings := TStringList.Create;
  try
    Strings.Text := Contents;
    if IndentMode > idntNone then
      for i := 0 to Strings.Count-1 do
        Strings[i] := ConvertIndents(Strings[i], TabWidth, IndentMode);

    case Mode of
      efmWindows: Strings.LineBreak := #$D#$A;
      efmMac: Strings.LineBreak := #$D;
      else
        Strings.LineBreak := #$A;
    end;
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;}

function TEditorFile.GetLinesModeAsText: String;
begin
  case LinesMode of
    efmWindows: Result := 'W';
    efmMac: Result := 'M';
    else
      Result := 'U'; //efmUnix
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

procedure TEditorFile.SetBaseName(AValue: String);
begin
  Rename(AValue);
end;

procedure TEditorFile.SetNickName(AValue: String);
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

procedure TEditorFile.Update;
begin
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

procedure GroupAddFitler(var Result: String; AGroup: TFileGroup);
var
  i: Integer;
  s: String;
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
        s := s + '*' + AExtensions[i];
        if Result <> '' then
          Result := Result + ';';
        Result := Result + '*' + AExtensions[i];
      end;
    finally
      AExtensions.Free;
    end;
  end;
end;

{ TFileCategories }

function TFileGroups.CreateMask(CreateMaskProc: TCreateMaskProc): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    if (CreateMaskProc = nil) or (CreateMaskProc(Items[i])) then
      GroupAddFitler(Result, Items[i]);
  end;
end;

function TFileGroups.CreateFilter(FullFilter: Boolean; FirstExtension: String; vGroup: TFileGroup; OnlyThisGroup: Boolean): String;
var
  aSupported: String;

  procedure AddIt(AGroup: TFileGroup);
  var
    i, n: Integer;
    s, e: String;
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
  i: Integer;
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

function TFileGroups.FindGroup(vFullName: String; vKind: TFileGroupKinds): TFileGroup;
var
  i: Integer;
  s: String;
begin
  Result := nil;
  if vFullName <> '' then
  begin
    //if have extension we will search for extension only, if not, we will search for name
    if Pos('.', vFullName) > 1 then //>1 not >0 , not .ini
      s := ExtractFileExt(vFullName)  //extension only with dot
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

function TFileGroups.OpenFile(vFiles: TEditorFiles; vFileName, vFileParams: String; FallBackGroup: String): TEditorFile;
var
  aGroup: TFileGroup;
  s: String;
begin
  s := ExtractFileName(vFileName);
  aGroup := Engine.Groups.FindGroup(s);

  if (aGroup = nil) and (FallBackGroup <> '') then
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

constructor TVirtualCategory.Create(ATendency: TEditorTendency; const vName, vTitle: String; vKind: TFileCategoryKinds; vImageName: String);
begin
  inherited Create(False); //childs is groups and already added to Groups and freed by it
  FKeywords := TKeywordList.Create;
  FTendency := ATendency;
  FName := vName;
  FTitle := vTitle;
  FKind := vKind;
  FImageName := vImageName;
end;

constructor TVirtualCategory.Create(ATendency: TEditorTendencyClass; const vName, vTitle: String; vKind: TFileCategoryKinds; vImageName: String);
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
  for i := 0 to Count - 1 do
  begin
    Items[i].EnumExtensions(vExtensions);
  end;
end;

function TVirtualCategory.GetExtensions: String;
var
  i: Integer;
  strings: TStringList;
begin
  strings := TStringList.Create;
  try
    for i := 0 to Count - 1 do
    begin
      Items[i].EnumExtensions(strings);
    end;
    Result := '';
    for i := 0 to strings.Count - 1 do
    begin
      if Result <> '' then
        Result := Result + ';';
      Result := strings[i];
    end;
  finally
    strings.Free;
  end;
end;

procedure TVirtualCategory.EnumVariables(Values: TStringList; EnumSkips: TEnumVariablesSkips);
begin
  if not (evsCategory in EnumSkips) then
  begin
  end;
  Tendency.EnumVariables(Values, EnumSkips + [evsCategory]);
end;

function TVirtualCategory.ReplaceVariables(S: String; Values: TStringList; EnumSkips: TEnumVariablesSkips): String;
begin
  EnumVariables(Values, EnumSkips);
  Result := InternalReplaceVariables(S, Values);
end;

function TVirtualCategory.ReplaceVariables(S: String; EnumSkips: TEnumVariablesSkips): String;
var
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    Result := ReplaceVariables(S, Values, EnumSkips);
  finally
    Values.Free;
  end;
end;

function TVirtualCategory.GetColorPrefix: String;
begin
  Result := '';
end;

function TVirtualCategory.FormatColor(Color: TColor): String;
begin
  Result := ColorToRGBHex(Color, GetColorPrefix);
end;

function TVirtualCategory.DeformatColor(Str: String): TColor;
begin
  Result := RGBHexToColor(Str, GetColorPrefix, False);
end;

procedure TVirtualCategory.Apply(AHighlighter: TSynCustomHighlighter; Attributes: TGlobalAttributes);
var
  i: Integer;
  M: TMap;
  G: TGlobalAttribute;
  Att: TSynHighlighterAttributes;
begin
  for i := 0 to AHighlighter.AttrCount - 1 do
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
  if FAutoComplete = nil then
  begin
    FAutoComplete := TSynEditAutoComplete.Create(nil);
    AutoComplete.EndOfTokenChr := '{}()[].<>/\:!&*+-=%;,';
  end;
  AutoComplete.AddEditor(vSynEdit);

  if FCompletion = nil then
  begin
    FCompletion := TmneSynCompletion.Create(nil);
    (Completion.TheForm as TmneSynCompletionForm).FKeywords := Keywords;
    Completion.Width := vSynEdit.ClientWidth div 3;
    if Completion.Width < 360 then
      Completion.Width := 360;
    Completion.OnBeforeExecute := @PrepareCompletion;
    Completion.ShortCut := scCtrl + VK_SPACE;
    Completion.CaseSensitive := False;
    Completion.SelectedColor := vSynEdit.SelectedColor.Background;
    Completion.EndOfTokenChr := '{}()[].<>/\:!&*+-=%;,';
    Completion.UsePrettyText := True;
  end;
  Completion.AddEditor(vSynEdit);

  Completion.TheForm.Caption := Name;
  Completion.TheForm.Font.Size := vSynEdit.Font.Size;
  Completion.TheForm.Font.Color := vSynEdit.Font.Color;
  Completion.TheForm.Color := vSynEdit.Color;
  Completion.TheForm.BackgroundColor := vSynEdit.Color;
  Completion.TheForm.TextColor := vSynEdit.Font.Color;
  Completion.TheForm.DrawBorderColor := Engine.Options.Profile.Attributes.Highlighted.Background;
  Completion.TheForm.DrawBorderWidth := 1;

  Completion.AutoUseSingleIdent := True;

  if AutoComplete.AutoCompleteList.Count = 0 then
    if FileExistsUTF8(LowerCase(Application.Location + FName + '.template')) then
      AutoComplete.AutoCompleteList.LoadFromFile(LowerCase(Application.Location + Name + '.template'));

  if Keywords.Count = 0 then
  begin
    DoAddKeywords;
  end;
end;

procedure TVirtualCategory.AddKeyword(AKeyword: String; AKind: Integer);
var
  s: string;
  map: TMap;
begin
  //s := '\style{+B}' + Engine.Options.Profile.Attributes.AttributeName(AKind) + '\style{-B} : ' + AKeyword;
  map := Mapper.FindByTokenID(AKind);
  if map <> nil then
    s := map.Name
  else
    s := '';
  AddKeyword(AKeyword, s, TAttributeType(AKind), False);
end;

function TVirtualCategory.AddKeyword(AKeyword: String; AttributeName: string; AKind: TAttributeType; Temp: Boolean): TKeywordItem;
var
  s: string;
  i: Integer;
  map: TMap;
begin
  //s := '\style{+B}' + Engine.Options.Profile.Attributes.AttributeName(AKind) + '\style{-B} : ' + AKeyword;
  map := Mapper.FindByAttribute(AKind);
  if map = nil then
    raise Exception.Create('Highlighter Map not exists');
  if AttributeName = '' then
    AttributeName := map.Name;
  s := AttributeName + ': \tab{6}\color{$'+IntToHex(map.Attribute.Foreground)+'}' + AKeyword;
  i := AutoComplete.Completions.IndexOf(AKeyword);
  if i>=0 then
    s := s + '\hspace{16}' + AutoComplete.CompletionComments[i];
  //Completion.AddItem(S, AKeyword, TObject(IntPtr(Ord(AKind))));
  Result := Keywords.AddItem(AKeyword, S, AttributeName, AKind, Temp);
end;

procedure TVirtualCategory.DoAddKeywords;
begin
end;

procedure TVirtualCategory.DoPrepareCompletion(AEditor: TCustomSynEdit);
begin
end;

procedure TVirtualCategory.PrepareCompletion(ASender: TSynBaseCompletion; var ACurrentString: String; var APosition: Integer; var AnX, AnY: Integer; var AnResult: TOnBeforeExeucteFlags);
begin
  Keywords.Clean;
  Completion.BeginUpdate;
  try
    DoPrepareCompletion(ASender.TheForm.CurrentEditor);
    Completion.Sort;
  finally
    Completion.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TVirtualCategory.InitEdit(vSynEdit: TCustomSynEdit);
begin
end;

procedure TVirtualCategory.EndEdit(vSynEdit: TCustomSynEdit);
begin
  AutoComplete.RemoveEditor(vSynEdit);
  Completion.RemoveEditor(vSynEdit);
end;

procedure TVirtualCategory.GetHintString(Token: string; ParamIndex: Integer; out AHint: String);
var
  KeywordItem: TKeywordItem;
begin
  KeywordItem := Keywords.Find(Token);
  if KeywordItem <> nil then
    AHint := KeywordItem.Description
  else
    AHint := '';
end;

destructor TVirtualCategory.Destroy;
begin
  FreeAndNil(FMapper);
  FreeAndNil(FCompletion);
  FreeAndNil(FKeywords);
  FreeAndNil(FHighlighter);
  inherited;
end;

procedure TVirtualCategory.EnumMenuItems(AddItems: TAddClickCallBack);
begin
end;

procedure TVirtualCategory.InitHighlighter;
var
  i: Integer;
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

function TVirtualCategory.Find(vName: String): TFileGroup;
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

function TVirtualCategory.GetFileCaption(AFile: TEditorFile; FileName: String): String;
begin
  Result := FileName;
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

function TVirtualCategory.OpenFile(vGroup: TFileGroup; vFiles: TEditorFiles; vFileName, vFileParams: String): TEditorFile;
begin
  Result := nil;
end;

procedure TVirtualCategory.ScanValues(AFile: TEditorFile; Values: TStringList; UpdateValues: Boolean);
begin
end;

{ TEditorProject }

constructor TEditorProject.Create;
begin
  inherited Create;
  FDesktop := TEditorDesktop.Create;
  FDesktop.FProject := Self;
  FRunOptions := TRunOptions.Create;
  FSaveDesktop := True;
end;

destructor TEditorProject.Destroy;
begin
  FreeAndNil(FRunOptions);
  FreeAndNil(FDesktop);
  FreeAndNil(FOptions);
  FreeAndNil(FSCM);
  inherited;
end;

procedure TEditorProject.LoadFromFile(FileName: String);
begin
  FFileName := FileName;
  inherited;
  UpdatePath;
end;

procedure TEditorProject.SetTendencyName(AValue: String);
var
  aTendency: TEditorTendency;
begin
  FTendencyName := AValue;

  aTendency := Engine.Tendencies.Find(TendencyName);
  {if aTendency = nil then
    aTendency := Engine.DefaultTendency;}//TODO not sure

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
  Result := (Self <> nil) and not (Self is TDefaultProject);
end;

function TEditorSession.GetMainPath: String;
begin
  Result := Project.DefaultPath;
end;

procedure TEditorSession.SetPanel(AValue: TControl);
begin
  if FPanel <> AValue then
  begin
    FPanel := AValue;
  end;
end;

function TEditorProject.GetPath: String;
var
  s: String;
begin
  s := ExpandToPath(FileName, Application.Location); //to make it protable
  Result := ExtractFilePath(s);
end;

procedure TEditorProject.SetSCM(AValue: TEditorSCM);
begin
  if FSCM = AValue then exit;
  FreeAndNil(FSCM);
  FSCM := AValue;
  Engine.Update([ecsChanged, ecsProject]);
end;

procedure TEditorProject.RecreateOptions;
begin
  FOptions.Free;
  FOptions := FTendency.CreateProjectOptions;
end;

procedure TEditorProject.RttiCreateObject(var vObject: TObject; vInstance: TObject; vObjectClass: TClass; const vClassName, vName: String);
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
  UpdatePath;
{  if not Failed and FSaveDesktop then
    Desktop.Load;}
end;

procedure TEditorProject.SetSCMClass(SCMClass: TEditorSCM);
begin
  if (SCMClass = nil) or not ((SCM <> nil) and (SCM.ClassType = SCMClass.ClassType)) then
    SCM := nil;
  if (SCMClass <> nil) then
    SCM := TEditorSCMClass(SCMClass.ClassType).Create;
end;

function TEditorProject.CreateMask(CreateMaskProc: TCreateMaskProc): String;
var
  AExtension: String;
  AExtensions: TStringList;
  aGroup: TFileGroup;
begin
  Result := '';
  AExtensions := TStringList.Create;
  try
    AExtensions.Delimiter := ';';
    AExtensions.DelimitedText := Engine.Session.Project.FileFilter;
    for AExtension in AExtensions do
    begin
      aGroup := Engine.Groups.FindGroup(AExtension);
      if aGroup <> nil then
      begin
        GroupAddFitler(Result, aGroup);
        CreateMaskProc(aGroup);
      end;
    end;
  finally
    AExtensions.Free;
  end;
end;

procedure TEditorProject.Saving;
begin
  inherited;
  if FSaveDesktop then
    Desktop.Save;
end;

procedure TEditorProject.UpdatePath;
begin
  if Self is TDefaultProject then
  begin
    if RunOptions.MainPath <> '' then
      FDefaultPath := ExpandToPath(RunOptions.MainPath, Tendency.DefaultPath)
    else
      FDefaultPath := Tendency.DefaultPath
 end
 else
 begin
   if RunOptions.MainPath <> '' then
     FDefaultPath := ExpandToPath(RunOptions.MainPath, Path)
   else
     FDefaultPath := Path;
 end;
end;

{ TFileGroup }

procedure TFileGroup.SetCategory(AValue: TVirtualCategory);
begin
  if FCategory <> AValue then
  begin
    if FCategory <> nil then
      FCategory.Extract(Self);
    FCategory := AValue;
    if FCategory <> nil then
      FCategory.Add(Self);
  end;
end;

function TFileGroup.GetExtension: String;
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

  procedure AddIt(E: String);
  begin
    if vExtensions.IndexOf(E) < 0 then
      vExtensions.Add(E);
  end;

  procedure AddStrings(E: TStringList);
  var
    i: Integer;
  begin
    for i := 0 to E.Count - 1 do
      AddIt(E[i]);
  end;

  procedure AddExtensions;
  var
    i: Integer;
  begin
    for i := 0 to Extensions.Count - 1 do
      AddIt(Extensions[i].Name);
  end;

var
  s: String;
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
  lList: TStringList;
  i: Integer;
  lItem: TEditorElement;
begin
  lList := TStringList.Create;
  try
    EnumExtensions(lList);
    for i := 0 to lList.Count - 1 do
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

procedure TFileGroup.Init;
begin
  if (Extensions.Count>0) then
    ImageIndex := EditorResource.GetImageIndex(Extensions[0].Name);
end;

function TFileGroup.OpenFile(vFiles: TEditorFiles; vFileName, vFileParams: String): TEditorFile;
begin
  Result := Category.OpenFile(Self, vFiles, vFileName, vFileParams);
end;

{ TFileGroups }

procedure TFileGroups.InternalAdd(GroupClass: TFileGroupClass; FileClass: TEditorFileClass; const Name, Title: String; Category: TFileCategoryClass; Extensions: array of String; Kind: TFileGroupKinds; Capapility: TRunCapabilities);
var
  aCategory: TVirtualCategory;
  aGroup: TFileGroup;
  i: Integer;
begin
  aCategory := Engine.Categories.FindByClass(Category);
  if aCategory = nil then
    raise Exception.Create('Can not find category ' + Category.ClassName);
  aGroup := Find(Name);
  if aGroup <> nil then
    raise Exception.Create(Name + ' already exists');
  aGroup := GroupClass.Create;
  aGroup.FFileClass := FileClass;
  aGroup.FTitle := Title;
  aGroup.FName := Name;
  aGroup.FKind := Kind;
  aGroup.FCapapility := Capapility;
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

procedure TFileGroups.Init;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    Items[i].Init;
  end;
end;

procedure TFileGroups.Add(FileClass: TEditorFileClass; const Name, Title: String; Category: TFileCategoryClass; Extensions: array of String; Kind: TFileGroupKinds; Capapility: TRunCapabilities);
begin
  InternalAdd(TFileGroup, FileClass, Name, Title, Category, Extensions, Kind, Capapility);
end;

function TFileGroups.Find(vName: String): TFileGroup;
begin
  Result := inherited Find(vName) as TFileGroup;
end;

function TFileGroups.Find(vName, vCategory: String): TFileGroup;
var
  i: Integer;
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
  i: Integer;
begin
  Result := False;
  if AGroup <> nil then
    for i := 0 to Count - 1 do
    begin
      if AGroup = Items[i] then
      begin
        Result := True;
        break;
      end;
    end;
end;

function TFileGroups.GetItem(Index: Integer): TFileGroup;
begin
  Result := inherited Items[Index] as TFileGroup;
end;

destructor TEditorSession.Destroy;
begin
  Run.Stop;
  FreeAndNil(FRun);
  FreeAndNil(FOptions);
  inherited;
end;

procedure TEditorSession.Changed;
begin
  FIsChanged := True;
  Engine.Update([ecsChanged, ecsState, ecsRefresh, ecsProject]);
end;

procedure TEditorSession.SetRun(AValue: TmneRun);
begin
  if FRun = AValue then Exit;
  FRun := AValue;
end;

constructor TEditorSession.Create;
begin
  inherited;
  FOptions := TEditorSessionOptions.Create;
  FRun := TmneRun.Create;
end;

procedure TFileGroups.EnumExtensions(vExtensions: TStringList; Kind: TFileGroupKinds);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if (Kind = []) or (Items[i].Kind = Kind) then
      Items[i].EnumExtensions(vExtensions);
  end;
end;

procedure TFileGroups.EnumExtensions(vExtensions: TEditorElements);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].EnumExtensions(vExtensions);
  end;
end;

{ TEditorDesktopFiles }

function TEditorDesktopFiles.Add(FileName: String): TEditorDesktopFile;
begin
  Result := inherited Add as TEditorDesktopFile;
  Result.FileName := FileName;
end;

function TEditorDesktopFiles.Find(vName: String): TEditorDesktopFile;
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

function TEditorDesktopFiles.IsExist(vName: String): Boolean;
begin
  Result := Find(vName) <> nil;
end;

{ TDebugSupportPlugin }

procedure CenterRect(var R1: TRect; R2: TRect);//from posDraws
begin
  OffsetRect(R1, ((R2.Right - R2.Left) div 2) - ((R1.Right - R1.Left) div 2) + (R2.Left - R1.Left), ((R2.Bottom - R2.Top) div 2) - ((R1.Bottom - R1.Top) div 2) + (R2.Top - R1.Top));
end;

procedure TSynDebugMarksPart.Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: Integer);
var
  i, lh, iw: Integer;
  aRect: TRect;

  procedure DrawIndicator(Line: Integer; ImageIndex: Integer);
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

    if aTendency.Debugger.Breakpoints <> nil then
    begin
      DebugManager.Enter;
      try
        for i := FirstLine to LastLine do
        begin
          aLine := TSynEdit(SynEdit).ScreenRowToRow(i);
          if aTendency.Debugger.Breakpoints.IsExists(FEditorFile.FileName, aLine) then
            DrawIndicator(i, DEBUG_IMAGE_BREAKPOINT);
        end;

      finally
        DebugManager.Leave;
      end;
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
  FBreakpoints := TmneBreakpoints.Create;
  FWatches := TmneWatches.Create;
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
  i: Integer;
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

      if (Engine.Files.SetActiveFile(Files.CurrentFile) = nil) and (Engine.Files.Count > 0) then
      begin
        Engine.Files.Current := Engine.Files[0];
        Engine.Files.Current.Activate;
      end;

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
  end;
end;

procedure TEditorDesktop.Save;
var
  i: Integer;
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

  if (Engine.Files.Current <> nil) and (not Engine.Files.Current.IsNew) then
    Files.CurrentFile := Engine.Files.Current.FileName
  else
    Files.CurrentFile := '';

  for i := 0 to Engine.Files.Count - 1 do
  begin
    aFile := Engine.Files[i];
    aItem := Files.Add(aFile.FileName);
    aFile.AssignTo(aItem);
  end;
end;

{ TEditorMessages }

function TEditorMessages.GetText(Index: Integer): String;
begin
  if Index < Count then
    Result := Items[Index].Text
  else
    Result := '';
end;

{ TEditorMessagesList }

function TEditorMessagesList.GetMessages(Name: String): TEditorMessages;
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
