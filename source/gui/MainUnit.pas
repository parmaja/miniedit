unit MainUnit;
{$mode objfpc}
{$M+}{$H+}
{**
 * Mini Edit
 *
 * @license   GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}
 {
    TODO
    https://github.com/heliosroots/fp-vscode/tree/master/sources
 }
interface

uses
  SysUtils, Variants, Classes, Messages, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ImgList, Menus, ToolWin, Buttons, FileCtrl,
  mnMsgBox, contnrs, LConvEncoding,
  LCLVersion, LMessages, lCLType, LCLIntf, LCLProc, EditorRun, process, fgl, Math,
  ShellCtrls, ActnList, EditorEngine, mneClasses,
  StdActns, Grids, SynEditHighlighter, SynEdit, IAddons, ntvThemes, ntvSplitters,
  SynHighlighterSQL, EditorClasses, ntvImgBtns, ntvPanels,
  {$ifdef WINDOWS}
  Windows, //For attach process thread
  {$endif}

  {$ifdef WINDOWS}
  TSVN_SCM, TGIT_SCM,
  {$endif}
  ntvTabSets, SynEditPlugins,
  synhighlighterunixshellscript, SynHighlighterPas, SynHighlighterMulti,
  mnStreams, mnClasses,
  //Addons
  {$ifdef Windows}
  mneAssociateForm,
  {$endif}
  mnePHPIniForm,
  //end of addons
  mndEngines, mndManagerForms,
  mneTendencyOptions, mneResources, IniFiles, mnFields, simpleipc, mnUtils,
  ntvTabs, ntvPageControls, SynEditMiscClasses, SynEditMarkupSpecialLine,
  mncDB,
  SynHighlighterAny, Types;

type
  TOutputLine = class(TObject)
  public
    LogLine: Integer; //line number in log editor/list
    Info: TMessageInfo;
  end;

  TOutputs = class(specialize TmnObjectList<TOutputLine>)
  private
  public
  end;

  TTabSetDragObject = class(TDragObject)
  public
    TabIndex: integer;
  end;

  TRecentMenuItem = class(TMenuItem)
  public
    Params: string;
  end;

  { TMainForm }

  TMainForm = class(TForm, INotifyEngine, INotifyEngineState, INotifyEngineEditor)
    ClearUndoAct: TAction;
    RedoAct: TAction;
    UndoAct: TAction;
    CompareLocalAct: TAction;
    FileTendencyAct: TAction;
    DBStartAct: TAction;
    BackwordSwitchFocusAct: TAction;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    CompareLocalMnu: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    NewSQLAct: TAction;
    DBRollbackAct: TAction;
    DBCommitAct: TAction;
    FilesFilterEdit: TEdit;
    FilesFilterClearBtn: TntvImgBtn;
    FolderCloseBtn1: TntvImgBtn;
    FolderPathLbl: TLabel;
    MenuItem23: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    ExploreFileFolder2Act: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem5: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Separator1: TMenuItem;
    ShowProjectFilterAct: TAction;
    DBCreateDatabaseAct: TAction;
    DBBrowseAct: TAction;
    DBDisconnectAct: TAction;
    DBConnectAct: TAction;
    GotoFileFolderAct: TAction;
    BrowserTabs: TntvPageControl;
    CallStackGrid: TStringGrid;
    DatabasePnl: TntvPanel;
    FileList: TListView;
    FolderPnl: TPanel;
    BrowserPnl: TntvPanel;
    LogEdit: TSynEdit;
    LogSyn: TSynAnySyn;
    ProjectFilterMnu: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem45: TMenuItem;
    CloseFileMnu: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem48: TMenuItem;
    ReconnectDatabasesMnu: TMenuItem;
    MenuItem50: TMenuItem;
    SepN8: TMenuItem;
    N6: TMenuItem;
    MenuItem47: TMenuItem;
    Sep1N6: TMenuItem;
    MessagesGrid: TStringGrid;
    MessagesTabs: TntvPageControl;
    MessagesPnl: TntvPanel;
    OutputEdit: TSynEdit;
    ProjectPnl: TntvPanel;
    SearchGrid: TStringGrid;
    ShowToolbarMnu: TMenuItem;
    ShowToolbarAct: TAction;
    CloseAllAct: TAction;
    EditorColorsAct: TAction;
    CreateFolderAct: TAction;
    CloseOthersAct: TAction;
    ChangeExtAct: TAction;
    FileEncodeBtn: TntvImgBtn;
    LinesModeBtn: TntvImgBtn;
    EncodeModeMenu: TPopupMenu;
    DatabaseMnu: TMenuItem;
    DBConnectMnu: TMenuItem;
    DBDisconnectMnu: TMenuItem;
    FilesSearchTimer: TTimer;
    CommitBtn: TToolButton;
    RollbackBtn: TToolButton;
    MessagePanel: TPanel;
    ToolButton9: TToolButton;
    UTF8BOMMnu: TMenuItem;
    UC16BEBOMMnu: TMenuItem;
    MainFileAct: TAction;
    MenuItem1: TMenuItem;
    MenuItem35: TMenuItem;
    ChangeExtMnu: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    EditorColorsMnu: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MainFilePopupMnu: TPopupMenu;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    UC16LEBOMMnu: TMenuItem;
    ResetMainFileAct: TAction;
    MenuItem32: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem36: TMenuItem;
    SetAsRootFolderAct: TAction;
    SetAsMainFileAc: TAction;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    SCMAddFileAct: TAction;
    FileCloseBtn: TntvImgBtn;
    BugSignBtn: TntvImgBtn;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    SCMAddFileMnu: TMenuItem;
    RecentFoldersMnu: TMenuItem;
    ShowSpecialCharsAct: TAction;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MainFileBtn: TToolButton;
    SynAnySyn1: TSynAnySyn;
    ToolButton3: TToolButton;
    ToolButton8: TToolButton;
    UTF8Mnu: TMenuItem;
    TypeOptionsForMnu: TMenuItem;
    TendenciesOptionsAct: TAction;
    DBGCompileAct: TAction;
    MenuItem22: TMenuItem;
    GotoFileFolderMnu: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    TypeOptionsMnu: TMenuItem;
    TypeOptionsAct: TAction;
    DeleteAct: TAction;
    FileHeaderPanel: TPanel;
    FileNameLbl: TLabel;
    FileTabs: TntvTabSet;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    AnsiMnu: TMenuItem;
    WatchesGrid: TStringGrid;
    WorkspaceMnu: TMenuItem;
    RenameAct: TAction;
    FindPreviousAct: TAction;
    MenuItem17: TMenuItem;
    SortByExtensionsAct: TAction;
    SortByNamesAct: TAction;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    NewMnu: TMenuItem;
    NewAct: TAction;
    MenuItem13: TMenuItem;
    SelectSCMTypeAct: TAction;
    TendencyPnl: TPanel;
    ProjectTypeMnu: TMenuItem;
    SelectProjectTendencyAct: TAction;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    RefreshFilesAct: TAction;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    ShowAllFilterAct: TAction;
    ShowKnownFilterAct: TAction;
    ShowTendencyFilterAct: TAction;
    ApplicationProperties: TApplicationProperties;
    MainMenu: TMainMenu;
    file1: TMenuItem;
    MenuItem2: TMenuItem;
    AllKnownFilterMnu: TMenuItem;
    AllFilterMnu: TMenuItem;
    TendencyFilterMnu: TMenuItem;
    IPCServer: TSimpleIPCServer;
    veiw1: TMenuItem;
    Help1: TMenuItem;
    OpenMnu: TMenuItem;
    SaveMnu: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    About1: TMenuItem;
    OptionsMnu: TMenuItem;
    ShowTree1: TMenuItem;
    ReopenFilesMnu: TMenuItem;
    TopToolbar: TToolBar;
    MainBar: TToolBar;
    BackBtn: TToolButton;
    ForwardBtn: TToolButton;
    StopBtn: TToolButton;
    ToolButton7: TToolButton;
    FoldersBtn: TToolButton;
    ActionList: TActionList;
    FoldersAct: TAction;
    SaveAct: TAction;
    OpenAct: TAction;
    SaveAsAct: TAction;
    Saveas1: TMenuItem;
    GeneralOptionsAct: TAction;
    SepN6: TMenuItem;
    NextAct: TAction;
    PriorAct: TAction;
    CloseAct: TAction;
    New1: TMenuItem;
    Prior1: TMenuItem;
    N2: TMenuItem;
    ExitAct: TFileExit;
    ToolButton1: TToolButton;
    SaveAllAct: TAction;
    AboutAct: TAction;
    HelpKeywordAct: TAction;
    About2: TMenuItem;
    DBGRunAct: TAction;
    DBGLintAct: TAction;
    ProjectMnu: TMenuItem;
    Run2: TMenuItem;
    Check1: TMenuItem;
    ToolsMnu: TMenuItem;
    FolderMenu: TPopupMenu;
    FolderOpenAllAct: TAction;
    OpenAll1: TMenuItem;
    FolderOpenAct: TAction;
    FolderOpenAct1: TMenuItem;
    FindAct: TAction;
    FindNextAct: TAction;
    EditMnu: TMenuItem;
    Find1: TMenuItem;
    Findnext1: TMenuItem;
    HelpIndex1: TMenuItem;
    EditorOptionsAct: TAction;
    N3: TMenuItem;
    EditorOptions1: TMenuItem;
    N4: TMenuItem;
    CloseAllFilesAct: TAction;
    Close1: TMenuItem;
    ProjectOptionsMnu: TMenuItem;
    ProjectOptionsAct: TAction;
    New2: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs2: TMenuItem;
    NewProjectAct: TAction;
    OpenProjectAct: TAction;
    SaveProjectAct: TAction;
    SaveAsProjectAct: TAction;
    SelectFileAct: TAction;
    AddFileToProjectAct: TAction;
    N7: TMenuItem;
    AddFile1: TMenuItem;
    SelectFile1: TMenuItem;
    ReopenProjectMnu: TMenuItem;
    CloseProjectAct: TAction;
    CloseProject1: TMenuItem;
    ExploreFolderAct: TAction;
    OpenFolder1: TMenuItem;
    ToolButton2: TToolButton;
    MessagesAct: TAction;
    Messages1: TMenuItem;
    LineModeMenu: TPopupMenu;
    UnixMnu: TMenuItem;
    WindowsMnu: TMenuItem;
    MacMnu: TMenuItem;
    ManageAct: TAction;
    OpenFolder2: TMenuItem;
    ProjectExploreFolderAct: TAction;
    OpenIncludeAct: TAction;
    OpenInclude1: TMenuItem;
    CopyAct: TAction;
    CutAct: TAction;
    PasteAct: TAction;
    SelectAllAct: TAction;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    SelectAll1: TMenuItem;
    EditorPopupMenu: TPopupMenu;
    OpenInclude2: TMenuItem;
    Find2: TMenuItem;
    Findnext2: TMenuItem;
    N9: TMenuItem;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    SelectAll2: TMenuItem;
    N10: TMenuItem;
    EditorOptions2: TMenuItem;
    GotoLineAct: TAction;
    GotoLine1: TMenuItem;
    BrowseFolderMnu: TMenuItem;
    ReplaceAct: TAction;
    Replace1: TMenuItem;
    RevertAct: TAction;
    Revert1: TMenuItem;
    ExploreFileFolderMnu: TMenuItem;
    OpenColorAct: TAction;
    Open2: TMenuItem;
    OpenColor1: TMenuItem;
    N11: TMenuItem;
    SCMMnu: TMenuItem;
    CommitMnu: TMenuItem;
    DiffMnu: TMenuItem;
    CommitFileMnu: TMenuItem;
    SCMCommitAct: TAction;
    SCMDiffFileAct: TAction;
    SCMCommitFileAct: TAction;
    SCMUpdateFileAct: TAction;
    SCMRevertAct: TAction;
    SCMUpdateAct: TAction;
    UpdateMnu: TMenuItem;
    UpdateFileMnu: TMenuItem;
    RevertMnu: TMenuItem;
    DBGDebugModeAct: TAction;
    ExecuteMnu: TMenuItem;
    StartServer1: TMenuItem;
    DBGStepIntoAct: TAction;
    DBGStepOverAct: TAction;
    DBGResetAct: TAction;
    N12: TMenuItem;
    StepInto1: TMenuItem;
    StepOver1: TMenuItem;
    Reset2: TMenuItem;
    DBGExecuteAct: TAction;
    ResumeMnu: TMenuItem;
    DBGStepOutAct: TAction;
    DBGStepOutAct1: TMenuItem;
    RunBtn: TToolButton;
    N5: TMenuItem;
    AddWatch1: TMenuItem;
    WatchesPopupMenu: TPopupMenu;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    N13: TMenuItem;
    Refresh1: TMenuItem;
    DBGToggleBreakpointAct: TAction;
    N14: TMenuItem;
    DBGToggleBreakpoint1: TMenuItem;
    oggleBreakpoint1: TMenuItem;
    ClientPnl: TPanel;
    EditorsPnl: TPanel;
    DBGRunToCursorAct: TAction;
    RunToCursor1: TMenuItem;
    DBGBreakpointsAct: TAction;
    Breakpoints1: TMenuItem;
    FilePopupMenu: TPopupMenu;
    OpenFolder3: TMenuItem;
    CopyFileNameAct: TAction;
    Copyfilename1: TMenuItem;
    DBGAddWatchAct: TAction;
    AddWatch2: TMenuItem;
    StatusPanel: TPanel;
    MessageLabel: TPanel;
    CursorPnl: TPanel;
    FolderHomeAct: TAction;
    Home1: TMenuItem;
    StatusTimer: TTimer;
    MessagesPopup: TPopupMenu;
    Clear1: TMenuItem;
    FindInFilesAct: TAction;
    FindinFiles1: TMenuItem;
    NextMessageAct: TAction;
    PriorMessageAct: TAction;
    NextMessage1: TMenuItem;
    PriorMessage1: TMenuItem;
    N15: TMenuItem;
    StatePnl: TPanel;
    SCMCompareToAct: TAction;
    CompareToMnu: TMenuItem;
    SwitchFocusAct: TAction;
    SwitchFocus1: TMenuItem;
    N16: TMenuItem;
    procedure BackwordSwitchFocusActExecute(Sender: TObject);
    procedure ClearUndoActExecute(Sender: TObject);
    procedure ClearUndoActUpdate(Sender: TObject);
    procedure CompareLocalActExecute(Sender: TObject);
    procedure DBDisconnectActExecute(Sender: TObject);
    procedure FileTendencyActExecute(Sender: TObject);
    procedure NewSQLActExecute(Sender: TObject);
    procedure AnsiMnuClick(Sender: TObject);
    procedure ApplicationPropertiesActivate(Sender: TObject);
    procedure ApplicationPropertiesHint(Sender: TObject);
    procedure ApplicationPropertiesShowHint(var HintStr: string; var CanShow: boolean; var HintInfo: THintInfo);
    procedure CallStackGridDblClick(Sender: TObject);
    procedure ChangeExtActExecute(Sender: TObject);
    procedure CloseAllActExecute(Sender: TObject);
    procedure CloseOthersActExecute(Sender: TObject);
    procedure DBBrowseActExecute(Sender: TObject);
    procedure DBConnectActExecute(Sender: TObject);
    procedure CreateFolderActExecute(Sender: TObject);
    procedure DatabaseActExecute(Sender: TObject);
    procedure DBConnectMnuClick(Sender: TObject);
    procedure DBCreateDatabaseActExecute(Sender: TObject);
    procedure DBDisconnectMnuClick(Sender: TObject);
    procedure DBGCompileActExecute(Sender: TObject);
    procedure DeleteActExecute(Sender: TObject);
    procedure EditorColorsActExecute(Sender: TObject);
    procedure EditorPopupMenuPopup(Sender: TObject);
    procedure EditorsPnlClick(Sender: TObject);
    procedure FetchCallStackBtnClick(Sender: TObject);
    procedure FileCloseBtnClick(Sender: TObject);
    procedure FilesFilterClearBtnClick(Sender: TObject);
    procedure FilesFilterEditChange(Sender: TObject);
    procedure FilesFilterEditKeyPress(Sender: TObject; var Key: char);
    procedure FilesSearchTimerTimer(Sender: TObject);
    procedure FileTabsTabSelected(Sender: TObject; OldTab, NewTab: TntvTabItem);
    procedure FindPreviousActExecute(Sender: TObject);
    procedure FoldersActExecute(Sender: TObject);

    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShow(Sender: TObject);
    procedure GotoFileFolderActExecute(Sender: TObject);
    procedure HelpKeywordActExecute(Sender: TObject);
    procedure IPCServerMessage(Sender: TObject);
    procedure IPCServerMessageQueued(Sender: TObject);
    procedure LinesModeBtn1Click(Sender: TObject);
    procedure MainFileActExecute(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem42Click(Sender: TObject);
    procedure MessagesGridDblClick(Sender: TObject);

    procedure MessagesGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure NewActExecute(Sender: TObject);
    procedure OpenActExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileTabsClick(Sender: TObject);
    procedure NextActExecute(Sender: TObject);
    procedure OutputEditChangeUpdating(ASender: TObject; AnUpdating: Boolean);
    procedure OutputEditDblClick(Sender: TObject);

    procedure OutputEditSpecialLineMarkup(Sender: TObject; Line: integer; var Special: boolean; Markup: TSynSelectedColor);
    procedure DatabasePnlClick(Sender: TObject);
    procedure PriorActExecute(Sender: TObject);
    procedure CloseActExecute(Sender: TObject);
    procedure FileListDblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure RedoActExecute(Sender: TObject);
    procedure RedoActUpdate(Sender: TObject);
    procedure ResetMainFileActExecute(Sender: TObject);
    procedure SCMAddFileActExecute(Sender: TObject);
    procedure SearchGridDblClick(Sender: TObject);
    procedure SearchGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure SearchGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SelectProjectTendencyActExecute(Sender: TObject);
    procedure RefreshFilesActExecute(Sender: TObject);
    procedure RenameActExecute(Sender: TObject);
    procedure SaveActExecute(Sender: TObject);
    procedure SaveAllActExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FolderOpenAllActExecute(Sender: TObject);
    procedure FolderOpenActExecute(Sender: TObject);
    procedure FindActExecute(Sender: TObject);
    procedure FindNextActExecute(Sender: TObject);
    procedure FileListKeyPress(Sender: TObject; var Key: char);
    procedure EditorOptionsActExecute(Sender: TObject);
    procedure SelectSCMTypeActExecute(Sender: TObject);
    procedure GeneralOptionsActExecute(Sender: TObject);
    procedure DBGRunActExecute(Sender: TObject);
    procedure ProjectOptionsActExecute(Sender: TObject);
    procedure NewProjectActExecute(Sender: TObject);
    procedure OpenProjectActExecute(Sender: TObject);
    procedure SaveProjectActExecute(Sender: TObject);
    procedure SelectFileActExecute(Sender: TObject);
    procedure CloseProjectActExecute(Sender: TObject);
    procedure ExploreFolderActExecute(Sender: TObject);
    procedure MessagesActExecute(Sender: TObject);
    procedure SetAsMainFileAcExecute(Sender: TObject);
    procedure SetAsRootFolderActExecute(Sender: TObject);
    procedure ShowAllFilterActExecute(Sender: TObject);
    procedure ShowProjectFilterActExecute(Sender: TObject);
    procedure ShowTendencyFilterActExecute(Sender: TObject);
    procedure ShowKnownFilterActExecute(Sender: TObject);
    procedure ShowSpecialCharsActExecute(Sender: TObject);
    procedure SortByExtensionsActExecute(Sender: TObject);
    procedure SortByNamesActExecute(Sender: TObject);
    procedure StatePnlClick(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolsMnuClick(Sender: TObject);
    procedure TypeOptionsActExecute(Sender: TObject);
    procedure TypePnlClick(Sender: TObject);
    procedure TendencyPnlDblClick(Sender: TObject);
    procedure TendenciesOptionsActExecute(Sender: TObject);
    procedure UC16BEBOMMnuClick(Sender: TObject);
    procedure UC16LEBOMMnuClick(Sender: TObject);
    procedure UndoActExecute(Sender: TObject);
    procedure UndoActUpdate(Sender: TObject);
    procedure UTF8BOMMnuClick(Sender: TObject);
    procedure UTF8MnuClick(Sender: TObject);
    procedure UnixMnuClick(Sender: TObject);
    procedure ShowToolbarActExecute(Sender: TObject);
    procedure WatchesGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WindowsMnuClick(Sender: TObject);
    procedure MacMnuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DBGLintActExecute(Sender: TObject);
    procedure AboutActExecute(Sender: TObject);
    procedure SaveAsProjectActExecute(Sender: TObject);
    procedure ProjectExploreFolderActExecute(Sender: TObject);
    procedure ManageActExecute(Sender: TObject);
    procedure CloseAllFilesActExecute(Sender: TObject);
    procedure OpenIncludeActExecute(Sender: TObject);
    procedure CopyActUpdate(Sender: TObject);
    procedure PasteActUpdate(Sender: TObject);
    procedure CutActUpdate(Sender: TObject);
    procedure SelectAllActUpdate(Sender: TObject);
    procedure PasteActExecute(Sender: TObject);
    procedure CopyActExecute(Sender: TObject);
    procedure CutActExecute(Sender: TObject);
    procedure SelectAllActExecute(Sender: TObject);
    procedure OpenIncludeActUpdate(Sender: TObject);
    procedure GotoLineActUpdate(Sender: TObject);
    procedure GotoLineActExecute(Sender: TObject);
    procedure BrowseFolderMnuClick(Sender: TObject);
    procedure ReplaceActExecute(Sender: TObject);
    procedure RevertActExecute(Sender: TObject);
    procedure FileListKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ExploreFileFolderMnuClick(Sender: TObject);
    procedure OpenColorActUpdate(Sender: TObject);
    procedure OpenColorActExecute(Sender: TObject);
    procedure SCMCommitActExecute(Sender: TObject);
    procedure SCMDiffFileActExecute(Sender: TObject);
    procedure SCMCommitFileActExecute(Sender: TObject);
    procedure SCMUpdateFileActExecute(Sender: TObject);
    procedure SCMUpdateActExecute(Sender: TObject);
    procedure SCMRevertActExecute(Sender: TObject);
    procedure DBGDebugModeActUpdate(Sender: TObject);
    procedure DBGDebugModeActExecute(Sender: TObject);
    procedure DBGStepOverActExecute(Sender: TObject);
    procedure DBGStepIntoActExecute(Sender: TObject);
    procedure DBGResetActExecute(Sender: TObject);
    procedure DBGExecuteActExecute(Sender: TObject);
    procedure DBGStepOutActExecute(Sender: TObject);
    procedure SaveAsActExecute(Sender: TObject);
    procedure Add1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure DBGToggleBreakpointActExecute(Sender: TObject);
    procedure DBGBreakpointsActExecute(Sender: TObject);
    procedure CopyFileNameActExecute(Sender: TObject);
    procedure DBGAddWatchActExecute(Sender: TObject);
    procedure FolderHomeActExecute(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure FindInFilesActExecute(Sender: TObject);
    procedure NextMessageActExecute(Sender: TObject);
    procedure PriorMessageActExecute(Sender: TObject);
    procedure SCMCompareToActExecute(Sender: TObject);
    procedure SwitchFocusActExecute(Sender: TObject);
    procedure WorkspaceMnuClick(Sender: TObject);
  private
    FMessages: TEditorMessages;
    FShowFolderFiles: TShowFolderFiles;
    FSortFolderFiles: TSortFolderFiles;
    procedure LogGotoLine;
    procedure LogGotoLineFirstError;
    procedure SwitchFocus(Backword: Boolean = False);
    procedure TestDebug;
    function CanOpenInclude: boolean;
    procedure CatchErr(Sender: TObject; e: exception);
    procedure ForceForegroundWindow;

    procedure SearchFoundEvent(Index: Integer; FileName: string; const Line: string; LineNo, Column, FoundLength: Integer);
    procedure SetShowFolderFiles(AValue: TShowFolderFiles);
    procedure SetSortFolderFiles(AValue: TSortFolderFiles);
    procedure UpdateFileHeaderPanel;
    procedure UpdateCallStack;
    procedure OptionsChanged;
    function ChooseSCM(var vSCM: TEditorSCM): Boolean;

    procedure SetMainTitle;
    procedure EngineChanged;
    procedure UpdateWatches;
    procedure EngineDebug;
    procedure EngineRefresh;
    procedure EngineEdited;
    procedure TransactionChanged;
    procedure EngineState;
    procedure ProjectLoaded;
    procedure UpdateFolder;
    procedure ProjectChanged;
    procedure AddMenuItem(AName, ACaption, AParentMenu: string; AOnClickEvent: TNotifyEvent; AShortCut: TShortCut = 0);
    procedure UpdateMenuItems;
    procedure UpdatePanel;
    procedure SetFolder(const Value: string);
    procedure ReopenFileClick(Sender: TObject);
    procedure ReopenDatabaseClick(Sender: TObject);
    procedure ReopenFolderClick(Sender: TObject);
    procedure ReopenProjectClick(Sender: TObject);
    procedure AddWatch(s: string);
    procedure DeleteWatch(s: string);
    procedure EnumRecents;
    procedure EnumRecentFiles;
    procedure EnumRecentDatbases;
    procedure EnumRecentFolders;
    procedure EnumRecentProjects;
    function GetCurrentColorText: string;
    function GetFolder: string;
    procedure DeleteCurrentWatch;
    procedure MoveListIndex(vDirection: Integer);
  protected
    FProjectFrame: TFrame;
    FDatabaseFrame: TDBManagerForm;
    FOutputs: TOutputs;
    FMenuItemsList: TObjectList;
    procedure ExploreFolder(AFolder: string);
    procedure ExploreFile(AFileName: string);
    procedure RunFile;
    procedure CompileFile;
    //
    procedure ChangeState(var State: TEditorChangeStates);
    procedure EngineReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
    procedure EngineMessage(S: string; vMessageInfo: TMessageInfo);
    procedure AddError(vMessageInfo: TMessageInfo); overload;
    procedure EngineAction(EngineAction: TEditorAction);

    procedure FollowFolder(vFolder: string; FocusIt: Boolean);
    procedure ShowMessagesList;
    procedure ShowWatchesList;
    procedure ShowSearchGrid;
    procedure LoadAddons;
    procedure ShowFileAtLine(vFileName: string; vLine: Integer; vColumn: Integer = 0);
    property ShowFolderFiles: TShowFolderFiles read FShowFolderFiles write SetShowFolderFiles;
    property SortFolderFiles: TSortFolderFiles read FSortFolderFiles write SetSortFolderFiles;

    procedure CheckChangedEvent(Data: PtrInt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Folder: string read GetFolder write SetFolder;
    procedure UpdateBrowsePnl;
    procedure UpdateMessagesPnl;
    procedure UpdateToolbars;
  end;

var
  MainForm: TMainForm;

implementation

uses
  mnXMLUtils, StrUtils, SearchForms, mneProjectOptions, EditorOptions,
  EditorProfiles, mneSetups, Clipbrd, ColorUtils,
  SelectFiles, mneSettings, mneConsts,
  SynEditTypes, AboutForms, mneRecentsForms,
  mneBreakpoints, SynMacroRecorder,
  SearchInFilesForms, SelectList;

function SortByExt(List: TStringList; Index1, Index: Integer): Integer;
begin
  Result := CompareText(ExtractFileExt(List[Index1]), ExtractFileExt(List[Index]));
  if Result = 0 then
    Result := CompareText(List[Index1], List[Index]);
end;

{$R *.lfm}

{ TMainNotifyEngine }

procedure TMainForm.UpdateBrowsePnl;
begin
  BrowserPnl.Visible := FoldersAct.Checked;
  if FoldersAct.Checked then
    Engine.Update([ecsFolder]);
end;

procedure TMainForm.ApplicationPropertiesShowHint(var HintStr: string; var CanShow: boolean; var HintInfo: THintInfo);
begin
  if (HintInfo.HintControl.Action is TAction) then
    with (HintInfo.HintControl.Action as TAction) do
    begin
      HintInfo.HintStr := HintInfo.HintControl.Caption + ' (' + ShortCutToText(ShortCut) + ')';
    end;
end;

procedure TMainForm.CallStackGridDblClick(Sender: TObject);
var
  s: string;
  aLine: integer;
begin
  if CallStackGrid.Row > 0 then
  begin
    Engine.Files.OpenFile(CallStackGrid.Cells[1, CallStackGrid.Row], '');
    s := CallStackGrid.Cells[2, CallStackGrid.Row];
    if s <> '' then
    begin
      aLine := StrToIntDef(s, 0);
      if aLine > 0 then
      begin
        with Engine.Files do
        if Current.SynEdit <> nil then
        begin
          Current.SynEdit.CaretY := aLine;
          Current.SynEdit.CaretX := 0;
          Current.SynEdit.SelectLine;
          Current.SynEdit.SetFocus;
        end
      end;
    end;
  end;
end;

procedure TMainForm.TestDebug;
begin
  with Engine.Tendencies.Find('Pascal') do
  begin
    Prepare;
    Debugger.Start;
  end;
end;

procedure TMainForm.ChangeExtActExecute(Sender: TObject);
var
  s: string;
begin
  if Engine.Files.Current <> nil then
  begin
    s := Engine.Files.Current.Extension;
    if LeftStr(s, 1) = '.' then
      s := MidStr(s, 2, MaxInt);
    if MsgBox.Input(s, 'Please enter new file tendency for ' + s) then
    begin
      Engine.Files.Current.Extension := s;
    end;
  end;
end;

procedure TMainForm.CloseAllActExecute(Sender: TObject);
begin
  Engine.Session.Close;
  Engine.Files.CloseAll;
  ResetMainFileAct.Execute;
end;

procedure TMainForm.CloseOthersActExecute(Sender: TObject);
begin
  Engine.Files.CloseOthers;
end;

procedure TMainForm.DBBrowseActExecute(Sender: TObject);
begin
  DBEngine.OpenServer;
end;

procedure TMainForm.DBConnectActExecute(Sender: TObject);
begin
  DBEngine.OpenDatabase;
end;

procedure TMainForm.CreateFolderActExecute(Sender: TObject);
var
  s: string;
begin
  s := '';
  if MsgBox.Input(s, 'Enter name for new folder') and (s <> '') then
  begin
    CreateDir(Folder + s);
    Engine.Update([ecsFolder]);
  end;
end;

procedure TMainForm.DatabaseActExecute(Sender: TObject);
begin
  UpdateBrowsePnl;
end;

procedure TMainForm.DBConnectMnuClick(Sender: TObject);
begin
end;

procedure TMainForm.DBCreateDatabaseActExecute(Sender: TObject);
begin

end;

procedure TMainForm.DBDisconnectMnuClick(Sender: TObject);
begin

end;

procedure TMainForm.DBGCompileActExecute(Sender: TObject);
begin
  CompileFile;
end;

procedure TMainForm.DeleteActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
  begin
    if not MsgBox.No('Are you sure want delete ' + Engine.Files.Current.BaseName) then
      Engine.Files.Current.Delete;
  end;
end;

procedure TMainForm.EditorColorsActExecute(Sender: TObject);
begin
  Engine.Options.ColorsShow;
end;

procedure TMainForm.EditorPopupMenuPopup(Sender: TObject);
begin
  if (Engine.Files.Current <> nil) and (Engine.Files.Current is TSourceEditorFile) then
    ShowSpecialCharsAct.Checked := eoShowSpecialChars in (Engine.Files.Current as TSourceEditorFile).SynEdit.Options
  else
    ShowSpecialCharsAct.Checked := False;
end;

procedure TMainForm.EditorsPnlClick(Sender: TObject);
begin

end;

procedure TMainForm.FetchCallStackBtnClick(Sender: TObject);
begin
  if (Engine.Tendency.Debugger <> nil) then
  begin
  end;
end;

procedure TMainForm.FileCloseBtnClick(Sender: TObject);
begin
  CloseAct.Execute;
end;

procedure TMainForm.FilesFilterClearBtnClick(Sender: TObject);
begin
  FilesFilterEdit.Clear;
end;

procedure TMainForm.FilesFilterEditChange(Sender: TObject);
begin
  FilesSearchTimer.Enabled := False;
  FilesSearchTimer.Enabled := True;
end;

procedure TMainForm.FilesFilterEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then
  begin
    FileList.SetFocus;
    FileListKeyPress(Sender, Key);
    Key := #0;
  end;
end;

procedure TMainForm.FilesSearchTimerTimer(Sender: TObject);
begin
  FilesSearchTimer.Enabled := False;
  Engine.Update([ecsFolder]);
end;

procedure TMainForm.ApplicationPropertiesActivate(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    Application.QueueAsyncCall(@CheckChangedEvent, 0);
end;

procedure TMainForm.ApplicationPropertiesHint(Sender: TObject);
begin
  //if  is TMenuItem then
  begin
    //MessageLabel.Caption := Application.Hint; //TODO
  end;
end;

procedure TMainForm.AnsiMnuClick(Sender: TObject);
begin
  Engine.Files.Current.FileEncoding := EncodingAnsi;
end;

procedure TMainForm.NewSQLActExecute(Sender: TObject);
begin
  with Engine.Files.New('sql') do
  begin
    IsTemporary := True;
    SynEdit.Lines.Text := ''; //baaah
  end;
end;

procedure TMainForm.BackwordSwitchFocusActExecute(Sender: TObject);
begin
  SwitchFocus(True);
end;

procedure TMainForm.ClearUndoActExecute(Sender: TObject);
begin
  Engine.Files.Current.ClearUndo;
end;

procedure TMainForm.ClearUndoActUpdate(Sender: TObject);
begin
  ClearUndoAct.Enabled := (Engine.Files.Current <> nil) and Engine.Files.Current.CanUndo;
end;

procedure TMainForm.CompareLocalActExecute(Sender: TObject);
var
  aFileName: string;
  Values: TStringList;
begin
  if Engine.Files.Current <> nil then
  begin
    Values := TStringList.Create;
    try
      if Engine.Files.Current.Tendency.EnableMacros then
      begin
        Engine.Files.Current.ScanValues(Values);
        aFileName := DequoteStr(Values.Values['@localfile']);
        aFileName := ExpandToPath(aFileName , Engine.Session.Project.DefaultPath);
        if (aFileName <> '') and FileExists(aFileName) and not SameFileName(aFileName, Engine.Files.Current.FileName) then
        begin
          Engine.SCM.DiffToFile(aFileName, Engine.Files.Current.FileName);
          Engine.Files.CheckChanged;
        end;
      end;
    finally
      Values.Free;
    end;
  end;
end;

procedure TMainForm.DBDisconnectActExecute(Sender: TObject);
begin
  DBEngine.CloseDatabase;
end;

procedure TMainForm.FileTendencyActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    ShowTendencyForm(Engine.Files.Current.Tendency);
end;

procedure TMainForm.FileTabsTabSelected(Sender: TObject; OldTab, NewTab: TntvTabItem);
begin
  Engine.Files.SetCurrentIndex(FileTabs.ItemIndex, True);
end;

procedure TMainForm.FindPreviousActExecute(Sender: TObject);
begin
  Engine.Files.FindPrevious;
end;

procedure TMainForm.FoldersActExecute(Sender: TObject);
begin
  UpdateBrowsePnl;
end;

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  i: integer;
  aFolder: string;
  A: string;
begin
  Engine.BeginUpdate;
  try
    aFolder := '';
    for i := 0 to Length(FileNames) - 1 do
    begin
      A := FileNames[i];
      if DirectoryExists(A) and (i = 0) then
        aFolder := A
      else if FileExists(A) then
        Engine.Files.OpenFile(A, '', [ofoActivate]);
    end;
    if aFolder <> '' then
      Folder := aFolder;
  finally
    Engine.EndUpdate;
  end;
end;

procedure TMainForm.ForceForegroundWindow;
{$ifdef windows}
var
  aForeThread, aAppThread: DWORD;
  aProcessID: DWORD;
  {$endif}
begin
  {$ifdef windows}
  aProcessID := 0;
  aForeThread := GetWindowThreadProcessId(GetForegroundWindow(), aProcessID);
  aAppThread := GetCurrentThreadId();

  if (aForeThread <> aAppThread) then
  begin
    AttachThreadInput(aForeThread, aAppThread, True);
    BringWindowToTop(Handle);
    AttachThreadInput(aForeThread, aAppThread, False);
  end
  else
    BringWindowToTop(Handle);
  {$endif}
  BringToFront;
end;

procedure TMainForm.IPCServerMessage(Sender: TObject);
var
  c: integer;
begin
  c := IPCServer.MsgType;
  case c of
    0: ForceForegroundWindow;
    1:
    begin
      if Engine.Files.OpenFile(IPCServer.StringMessage, '') <> nil then
      begin
        ForceForegroundWindow;
      end;
    end;
  end;
end;

procedure TMainForm.IPCServerMessageQueued(Sender: TObject);
begin
  IPCServer.PeekMessage(IPCServer.ThreadTimeOut, True); //not sure if it a bug in FPC
end;

procedure TMainForm.LinesModeBtn1Click(Sender: TObject);
begin
end;

procedure TMainForm.MainFileActExecute(Sender: TObject);
begin
  if Engine.Session.Project.RunOptions.MainFile <> '' then
    Engine.Files.OpenFile(Engine.Session.MainFile, '');
end;

procedure TMainForm.MenuItem22Click(Sender: TObject);
begin
  if Engine.Session.Active then
  begin
    Folder := ExtractFilePath(Engine.Session.Project.FileName);
    Engine.ProcessRecentFolder(Folder);
  end;
end;

procedure TMainForm.MenuItem24Click(Sender: TObject);
begin
  if FileList.Selected <> nil then
    Clipbrd.Clipboard.AsText := Folder + FileList.Selected.Caption;
end;

procedure TMainForm.MenuItem42Click(Sender: TObject);
begin
  ExploreFolder(Engine.Session.Project.RunOptions.MainFile);
end;

procedure TMainForm.MessagesGridDblClick(Sender: TObject);
var
  aFile: string;
  aLine: integer;
begin
  if MessagesGrid.Row > 0 then
  begin
    aFile := MessagesGrid.Cells[3, MessagesGrid.Row];
    aLine := StrToIntDef(MessagesGrid.Cells[4, MessagesGrid.Row], 0);
    ShowFileAtLine(aFile, aLine);
  end;
end;

procedure TMainForm.MessagesGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_RETURN: MessagesGridDblClick(Sender);
    end;
  end;
end;

procedure TMainForm.NewActExecute(Sender: TObject);
var
  G: TFileGroups;
  E: Integer;
  i: Integer;
  aGroups: TFileGroups;
begin
  try
{    if Engine.Session.Active then
      aGroups := Engine.Session.Project.Tendency.Groups
    else}
      aGroups := Engine.Groups;
    G := TFileGroups.Create(False);
    try
      for i := 0 to aGroups.Count - 1  do
      begin
        if not (fgkUneditable in aGroups[i].Kind) then
          G.Add(aGroups[i]);
      end;
      //E := G.IndexOfName()
      //from old Engine.Files.New(Engine.Tendency.GetDefaultGroup);
      if ShowSelectList('Select file tendency', G, [slfSearch, slfUseNameTitle], E) then
        Engine.Files.New(G[E]);
    finally
      G.Free;
    end;
  finally
  end;
end;

procedure TMainForm.OpenActExecute(Sender: TObject);
begin
  Engine.Files.Open;
end;

procedure TMainForm.EngineChanged;
var
  i: integer;
begin
  FileTabs.Items.BeginUpdate;
  try
    FileTabs.Items.Clear;
    for i := 0 to Engine.Files.Count - 1 do
      FileTabs.Items.AddItem(ExtractFileName(Engine.Files[i].FileName), Engine.Files[i].GetCaption);
  finally
    FileTabs.Items.EndUpdate;
  end;
  FileTabs.Visible := FileTabs.Items.Count > 0;
  SetMainTitle;
  EnumRecents;
end;

procedure TMainForm.EngineRefresh;
begin
  SetMainTitle;

  if Engine.Files.Current <> nil then
  begin
    if (Engine.Files.Current.SynEdit <> nil) and (Engine.Files.Current.SynEdit.PopupMenu = nil) then
      Engine.Files.Current.SynEdit.PopupMenu := EditorPopupMenu;
    FileNameLbl.Caption := Engine.Files.Current.FileName;
    LinesModeBtn.Caption := Engine.Files.Current.LinesModeAsText;
    LinesModeBtn.Enabled := Engine.Files.Current.IsText;
    FileEncodeBtn.Caption := UpperCase(Engine.Files.Current.FileEncoding);
    FileEncodeBtn.Enabled := Engine.Files.Current.IsText;
    FileTabs.ItemIndex := Engine.Files.Current.Index;
    if Engine.Files.Current.FileName <> '' then
      FileTabs.Items[FileTabs.ItemIndex].Caption := Engine.Files.Current.GetCaption;
    if Folder = '' then
      if Engine.Files.Current.CanAddRecentFiles then
        Folder := ExtractFilePath(Engine.Files.Current.FileName);
    SaveAct.Enabled := Engine.Files.Current.IsChanged;
    SaveAllAct.Enabled := Engine.GetIsChanged;
  end
  else
  begin
    FileNameLbl.Caption := '';
    LinesModeBtn.Caption := '';
    LinesModeBtn.Enabled := False;
    FileEncodeBtn.Caption := '';
    FileEncodeBtn.Enabled := False;
    SaveAct.Enabled := False;
    SaveAllAct.Enabled := Engine.GetIsChanged;
  end;
  if Engine.Session.Active and (Engine.Session.Project.Tendency <> nil) then
    TendencyPnl.Caption := Engine.Session.Project.Tendency.Name
  else if (Engine.Files.Current <> nil) and (Engine.Files.Current.Tendency <> nil) then
    TendencyPnl.Caption := Engine.Files.Current.Tendency.Name
  else
    TendencyPnl.Caption := 'Undefined';
  //  DebugPnl.Visible := DebugPnl.Caption <> '';

  DBCommitAct.Enabled := DBEngine.HaveTransactions;
  DBRollbackAct.Enabled := DBEngine.HaveTransactions;

  Engine.Update([ecsMenu]);

  UpdateFileHeaderPanel;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  IPCServer.StopServer;
end;

procedure TMainForm.FileTabsClick(Sender: TObject);
begin
  Engine.Files.SetCurrentIndex(FileTabs.ItemIndex, True);
end;

procedure TMainForm.NextActExecute(Sender: TObject);
begin
  Engine.Files.Next;
end;

procedure TMainForm.OutputEditChangeUpdating(ASender: TObject; AnUpdating: Boolean);
begin
  (ASender as TSynEdit).InvalidateLine((ASender as TSynEdit).CaretY)
end;

procedure TMainForm.OutputEditDblClick(Sender: TObject);
begin
  LogGotoLine;
end;

procedure TMainForm.LogGotoLine;
var
  aLine: TOutputLine;
begin
  if OutputEdit.CaretY <= OutputEdit.Lines.Count then //y based on 1
  begin
    aLine := FOutputs[OutputEdit.CaretY - 1];
    if aLine.Info.FileName <> '' then
      ShowFileAtLine(aLine.Info.FileName, aLine.Info.Line, aLine.Info.Column);
  end;
end;

procedure TMainForm.LogGotoLineFirstError;
var
  aLine: TOutputLine;
  i: Integer;
begin
  if OutputEdit.CaretY <= OutputEdit.Lines.Count then //y based on 1
  begin
    for i := 0 to FOutputs.Count-1 do
    begin
      aLine := FOutputs[i];
      if aLine.Info.Kind = mskError then
      begin
        if aLine.Info.FileName <> '' then
        begin
          ShowFileAtLine(aLine.Info.FileName, aLine.Info.Line, aLine.Info.Column);
          OutputEdit.CaretY := aLine.LogLine;
          break;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.OutputEditSpecialLineMarkup(Sender: TObject; Line: integer; var Special: boolean; Markup: TSynSelectedColor);
begin
  if not IsShutdown then
  begin
    Special := (Sender as TSynEdit).CaretY = Line;
    if Special then
    begin
      Markup.Foreground := Engine.Options.Profile.Attributes.Active.Foreground;
      Markup.Background := Engine.Options.Profile.Attributes.Active.Background;
    end;
  end;
end;

procedure TMainForm.DatabasePnlClick(Sender: TObject);
begin

end;

procedure TMainForm.PriorActExecute(Sender: TObject);
begin
  Engine.Files.Prior;
end;

procedure TMainForm.CloseActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Engine.Files.Current.Close;
end;

procedure TMainForm.SetFolder(const Value: string);
begin
  if Engine.BrowseFolder <> Value then
  begin
    if Value = '..' then
      Engine.BrowseFolder := ExtractFilePath(IncludeTrailingPathDelimiter(ExpandFileName(Value)))
    else
      Engine.BrowseFolder := IncludeTrailingPathDelimiter(ExpandFileName(Value));
    if FoldersAct.Checked then
      Engine.Update([ecsFolder]);
  end;
end;

procedure TMainForm.FileListDblClick(Sender: TObject);
begin
  FolderOpenAct.Execute;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  answer: Integer;
  mr: TmsgChoice;
begin
  if Engine.Files.GetEditedCount > 0 then
  begin
    answer := MsgBox.Ask('There a files changed but not saved'#13, [Choice('Save &All', msgcYes), Choice('Ig&nore All', msgcNo), Choice('A&sk'), Choice('&Cancel', msgcCancel)], 0, 3, msgkConfirmation);
    //mr := MsgBox.YesNoCancel();
    if answer = 0 then
      Engine.Files.SaveAll(True) //force save all
    else if answer = 1 then
      Engine.Files.CancelAll
    else if answer = 3 then
      CanClose := False;
  end;

  if CanClose and (Engine.Session.Active) then
  begin
    if (Engine.Session.Project.FileName = '') then
    begin
      mr := MsgBox.YesNoCancel('Save project ' + Engine.Session.Project.Name + ' before close?');
      if mr = msgcCancel then
        CanClose := False
      else if mr = msgcYes then
        Engine.Session.Save;
    end
    else
      Engine.Session.Save;
  end;
end;

procedure TMainForm.RedoActExecute(Sender: TObject);
begin
  Engine.Files.Current.Redo;
end;

procedure TMainForm.RedoActUpdate(Sender: TObject);
begin
  RedoAct.Enabled := (Engine.Files.Current <> nil) and Engine.Files.Current.CanUndo;
end;

procedure TMainForm.ResetMainFileActExecute(Sender: TObject);
begin
  if Engine.Session.Project.RunOptions.MainFile <> '' then
  begin
    Engine.Session.Project.RunOptions.MainFile := '';
    Engine.Update([ecsRefresh]);
  end;
end;

procedure TMainForm.SCMAddFileActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Engine.SCM.AddFile(Engine.Files.Current.FileName);
end;

procedure TMainForm.SearchGridDblClick(Sender: TObject);
var
  s: string;
  y, x, l: integer;
begin
  if SearchGrid.Row > 0 then
  begin
    Engine.Files.OpenFile(SearchGrid.Cells[1, SearchGrid.Row], '');
    s := SearchGrid.Cells[2, SearchGrid.Row];
    if s <> '' then
    begin
      y := ptrint(SearchGrid.Rows[SearchGrid.Row].Objects[1]);
      x := ptrint(SearchGrid.Rows[SearchGrid.Row].Objects[2]);
      l := ptrint(SearchGrid.Rows[SearchGrid.Row].Objects[0]);
      if y > 0 then
      begin
        with Engine.Files do
        if Current.SynEdit <> nil then
        begin
          if l > 0 then
          begin
            with Current.SynEdit do
            begin
              CaretXY := LogicalToPhysicalPos(Point(x, y));
              BlockBegin := Point(LogicalCaretXY.x, LogicalCaretXY.y);
              BlockEnd := Point(LogicalCaretXY.x + l, LogicalCaretXY.y);
            end;
          end
          else
            Current.SynEdit.LogicalCaretXY := Point(1, y);
          Current.SynEdit.SetFocus;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.SearchGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  c, l: integer;
  h, w: integer;
  s, bf, md, af: string;
  aCanvas: TCanvas;
begin
  if (aRow > 0) then
  begin
    if (aCol > 2) then
    begin
      aCanvas := SearchGrid.Canvas;
      s := StringReplace(SearchGrid.Cells[aCol, aRow], #9, '    ', [rfReplaceAll]); //4 spaces
      //s := SearchGrid.Cells[aCol, aRow];

      w := aRect.Left + 2;
      h := aCanvas.TextHeight(s);

      l := ptrint(SearchGrid.Rows[aRow].Objects[0]);
      c := ptrint(SearchGrid.Rows[aRow].Objects[1]);
      bf := Copy(s, 1, c - 1);
      md := Copy(s, c, l);
      af := Copy(s, c + l, MaxInt);
      aRect.Top := aRect.Top + ((aRect.Bottom - aRect.Top - h) div 2);
      //aCanvas.FillRect(aRect);
      aCanvas.Font.Style := [];
      aCanvas.TextOut(w, aRect.Top, bf);
      w := w + aCanvas.TextWidth(bf);
      aCanvas.Font.Style := [fsBold];
      aCanvas.TextOut(w, aRect.Top, md);
      w := w + aCanvas.TextWidth(md);
      aCanvas.Font.Style := [];
      aCanvas.TextOut(w, aRect.Top, af);
      //aCanvas.Refresh;
    end;
  end;
end;

procedure TMainForm.SearchGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    SearchGridDblClick(Sender);
end;

procedure TMainForm.SelectProjectTendencyActExecute(Sender: TObject);
var
  lTendency: TEditorTendency;
begin
  if Engine.Session.Active then
  begin
    if not MsgBox.No('You cannot change the tendency without losing project setting, are you sure?') then
    begin
      lTendency := Engine.Session.Project.Tendency;
      if Engine.ChooseTendency(lTendency) then
      begin
        Engine.Session.Project.TendencyName := lTendency.Name;
        ProjectOptionsAct.Execute;
      end;
    end;
  end;
end;

procedure TMainForm.RefreshFilesActExecute(Sender: TObject);
begin
  Engine.Update([ecsFolder]);
end;

procedure TMainForm.RenameActExecute(Sender: TObject);
var
  s: string;
begin
  if Engine.Files.Current <> nil then
  begin
    s := Engine.Files.Current.NickName;
    if MsgBox.Input(s, 'Please enter new name for ' + s) then
      Engine.Files.Current.NickName := s;
  end;
end;

procedure TMainForm.SaveActExecute(Sender: TObject);
begin
  Engine.Files.Save(True);
end;

procedure TMainForm.EngineEdited;
begin
  if Engine.Files.Current <> nil then
  begin
    SaveAct.Enabled := Engine.Files.Current.IsChanged;
    if Engine.Files.Current.IsChanged then
      SaveAllAct.Enabled := True;
  end
  else
  begin
    SaveAct.Enabled := False;
    StatePnl.Caption := '';
    StatePnl.ParentColor := True;
  end;
end;

procedure TMainForm.TransactionChanged;
begin
  DBCommitAct.Enabled := DBEngine.HaveTransactions;
  DBRollbackAct.Enabled := DBEngine.HaveTransactions;
end;

procedure TMainForm.SaveAllActExecute(Sender: TObject);
begin
  Engine.SaveAll(False);
end;

procedure TMainForm.FolderOpenAllActExecute(Sender: TObject);
var
  i: integer;
begin
  Engine.BeginUpdate;
  try
    for i := 0 to FileList.Items.Count - 1 do
    begin
      if FileList.Items[i].Data <> nil then
        Engine.Files.OpenFile(Folder + FileList.Items[i].Caption);
    end;
  finally
    Engine.EndUpdate;
  end;
end;

procedure TMainForm.FolderOpenActExecute(Sender: TObject);
begin
  if FileList.Selected <> nil then
  begin
    if FileList.Selected.Data = nil then
      FollowFolder(FileList.Selected.Caption, FileList.Focused)
    else
      Engine.Files.OpenFile(Folder + FileList.Selected.Caption, '', [ofoActivate]);
  end;
end;

procedure TMainForm.FindActExecute(Sender: TObject);
begin
  Engine.Files.Find;
end;

procedure TMainForm.FindNextActExecute(Sender: TObject);
begin
  Engine.Files.FindNext;
end;

procedure TMainForm.FileListKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    FolderOpenAct.Execute;
end;

procedure TMainForm.EditorOptionsActExecute(Sender: TObject);
begin
  Engine.Options.OptionsShow;
end;

procedure TMainForm.SelectSCMTypeActExecute(Sender: TObject);
var
  lSCM: TEditorSCM;
begin
  lSCM := Engine.Session.Project.SCM;
  if ChooseSCM(lSCM) then
    Engine.Session.Project.SetSCMClass(lSCM);
end;

procedure TMainForm.GeneralOptionsActExecute(Sender: TObject);
begin
  ShowSettingForm(Engine);
end;

procedure TMainForm.DBGRunActExecute(Sender: TObject);
begin
  RunFile;
end;

procedure TMainForm.ProjectOptionsActExecute(Sender: TObject);
begin
  if Engine.Session.Active then
  begin
    if ShowProjectForm(Engine.Session.Project) then
      Engine.Session.Changed;
  end
  else
    if ShowProjectForm(Engine.DefaultProject) then
      Engine.Session.Changed;
end;

procedure TMainForm.NewProjectActExecute(Sender: TObject);
begin
  Engine.NewProject;
end;

procedure TMainForm.OpenProjectActExecute(Sender: TObject);
begin
  Engine.Session.Open;
end;

procedure TMainForm.SaveProjectActExecute(Sender: TObject);
begin
  Engine.Session.Save;
end;

procedure TMainForm.SelectFileActExecute(Sender: TObject);
var
  aFileName: string;
begin
  if ShowSelectFile(Engine.Session.GetRoot, aFileName) then
    Engine.Files.OpenFile(aFileName);
end;

procedure TMainForm.EnumRecentFiles;
var
  i, c: integer;
  aMenuItem: TRecentMenuItem;
begin
  ReopenFilesMnu.SubMenuImages := EditorResource.FileImages;
  ReopenFilesMnu.Clear;
  c := Engine.Options.RecentFiles.Count;
  if c > 10 then
    c := 10;
  for i := 0 to c - 1 do
  begin
    aMenuItem := TRecentMenuItem.Create(Self);
    aMenuItem.Caption := Engine.Options.RecentFiles[i].Name;
    aMenuItem.Params := Engine.Options.RecentFiles[i].Params;
    if aMenuItem.Params <> '' then
      aMenuItem.Hint := aMenuItem.Params
    else
      aMenuItem.Hint := aMenuItem.Caption;
    aMenuItem.OnClick := @ReopenFileClick;
    aMenuItem.ImageIndex :=  EditorResource.GetFileImageIndex(aMenuItem.Caption, 1);
    ReopenFilesMnu.Add(aMenuItem);
  end;
end;

procedure TMainForm.EnumRecentDatbases;
var
  i, c: integer;
  aMenuItem: TRecentMenuItem;
  EngineName, Resource, Host, Port, User, Password, Role: string;
begin
  ReconnectDatabasesMnu.SubMenuImages := EditorResource.FileImages;
  ReconnectDatabasesMnu.Clear;
  c := Engine.Options.RecentDatabases.Count;
  if c > 10 then
    c := 10;
  for i := 0 to c - 1 do
  begin
    mncDB.Engines.DecomposeConnectionString(Engine.Options.RecentDatabases[i].Params, EngineName, Resource, Host, Port, User, Password, Role);

    aMenuItem := TRecentMenuItem.Create(Self);
    aMenuItem.Caption := Engine.Options.RecentDatabases[i].Name;
    aMenuItem.Params := Engine.Options.RecentDatabases[i].Params;
    aMenuItem.Hint := aMenuItem.Params;
    aMenuItem.OnClick := @ReopenDatabaseClick;
    aMenuItem.ImageIndex :=  EditorResource.GetImageIndex(EngineName, cDatabaseImage);
    ReconnectDatabasesMnu.Add(aMenuItem);
  end;
end;

procedure TMainForm.EnumRecentFolders;
var
  i, c: integer;
  aMenuItem: TMenuItem;
begin
  RecentFoldersMnu.Clear;
  c := Engine.Options.RecentFolders.Count;
  if c > 10 then
    c := 10;
  for i := 0 to c - 1 do
  begin
    aMenuItem := TMenuItem.Create(Self);
    aMenuItem.Caption := Engine.Options.RecentFolders[i].Name;
    aMenuItem.Hint := aMenuItem.Caption;
    aMenuItem.OnClick := @ReopenFolderClick;
    RecentFoldersMnu.Add(aMenuItem);
  end;
end;

procedure TMainForm.ReopenFileClick(Sender: TObject);
var
  aFile: string;
begin
  if Sender is TMenuItem then
  begin
    aFile := (Sender as TMenuItem).Caption;
    if Engine.Session.Active then
      aFile := ExpandToPath(aFile, Engine.Session.Project.RunOptions.MainPath);
    Engine.Files.OpenFile(aFile);
  end;
end;

procedure TMainForm.ReopenDatabaseClick(Sender: TObject);
begin
  if Sender is TRecentMenuItem then
    DBEngine.OpenDatabase((Sender as TRecentMenuItem).Caption, (Sender as TRecentMenuItem).Params);
end;

procedure TMainForm.ReopenFolderClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    Folder := (Sender as TMenuItem).Caption;
end;

procedure TMainForm.ReopenProjectClick(Sender: TObject);
var
  aFile: string;
begin
  if Sender is TMenuItem then
  begin
    aFile := (Sender as TMenuItem).Caption;
    Engine.Session.Load(aFile);
  end;
end;

procedure TMainForm.EnumRecentProjects;
var
  i, c: integer;
  aMenuItem: TMenuItem;
begin
  ReopenProjectMnu.SubMenuImages := EditorResource.FileImages;
  ReopenProjectMnu.Clear;
  c := Engine.Options.RecentProjects.Count;
  if c > 10 then
    c := 10;
  for i := 0 to c - 1 do
  begin
    aMenuItem := TMenuItem.Create(Self);
    aMenuItem.Caption := Engine.Options.RecentProjects[i].Name;
    aMenuItem.Hint := aMenuItem.Caption;
    aMenuItem.OnClick := @ReopenProjectClick;
    aMenuItem.ImageIndex :=  EditorResource.GetFileImageIndex(aMenuItem.Caption, 1);
    ReopenProjectMnu.Add(aMenuItem);
  end;
end;

procedure TMainForm.CloseProjectActExecute(Sender: TObject);
begin
  Engine.Session.Close;
end;

procedure TMainForm.ExploreFolderActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
  begin
    ExploreFolder(Engine.Files.Current.FileName);
  end;
end;

procedure TMainForm.UpdateMessagesPnl;
begin
  MessagesPnl.Visible := MessagesAct.Checked;
end;

procedure TMainForm.UpdateToolbars;
begin
  TopToolbar.Visible := ShowToolbarAct.Checked;
end;

procedure TMainForm.MessagesActExecute(Sender: TObject);
begin
  UpdateMessagesPnl;
end;

procedure TMainForm.SetAsMainFileAcExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
  begin
    Engine.Session.Project.RunOptions.MainFile := Engine.Files.Current.FileName;
    Engine.Update([ecsRefresh]);
  end;
end;

procedure TMainForm.SetAsRootFolderActExecute(Sender: TObject);
begin
  Engine.Session.Project.RunOptions.MainPath := Folder;
end;

procedure TMainForm.ShowAllFilterActExecute(Sender: TObject);
begin
  ShowFolderFiles := sffAll;
end;

procedure TMainForm.ShowProjectFilterActExecute(Sender: TObject);
begin
  ShowFolderFiles := sffProject;
end;

procedure TMainForm.ShowTendencyFilterActExecute(Sender: TObject);
begin
  ShowFolderFiles := sffRelated;
end;

procedure TMainForm.ShowKnownFilterActExecute(Sender: TObject);
begin
  ShowFolderFiles := sffKnown;
end;

procedure TMainForm.ShowSpecialCharsActExecute(Sender: TObject);
begin
   if (Engine.Files.Current <> nil) and (Engine.Files.Current is TTextEditorFile) then
   begin
     ShowSpecialCharsAct.Checked := not ShowSpecialCharsAct.Checked;
     if ShowSpecialCharsAct.Checked then
       (Engine.Files.Current as TSourceEditorFile).SynEdit.Options := (Engine.Files.Current as TSourceEditorFile).SynEdit.Options + [eoShowSpecialChars]
     else
       (Engine.Files.Current as TSourceEditorFile).SynEdit.Options := (Engine.Files.Current as TSourceEditorFile).SynEdit.Options - [eoShowSpecialChars];
   end;
end;

procedure TMainForm.SortByExtensionsActExecute(Sender: TObject);
begin
  SortFolderFiles := srtfByExt;
end;

procedure TMainForm.SortByNamesActExecute(Sender: TObject);
begin
  SortFolderFiles := srtfByNames;
end;

procedure TMainForm.StatePnlClick(Sender: TObject);
begin

end;

procedure TMainForm.ToolButton4Click(Sender: TObject);
begin
end;

procedure TMainForm.ToolsMnuClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.TypeOptionsActExecute(Sender: TObject);
begin
  ShowTendencyForm(Engine.Tendency);
end;

procedure TMainForm.TypePnlClick(Sender: TObject);
begin
end;

procedure TMainForm.TendencyPnlDblClick(Sender: TObject);
begin
  SelectProjectTendencyAct.Execute;
end;

procedure TMainForm.TendenciesOptionsActExecute(Sender: TObject);
var
  lTendency: TEditorTendency;
begin
  if Engine.Session.Active then
    lTendency := Engine.Session.Project.Tendency
  else if Engine.Files.Current <> nil then
    lTendency := Engine.Files.Current.Tendency
  else
    lTendency := nil;
  if Engine.ChooseTendency(lTendency) then
    if (lTendency <> nil) then
      ShowTendencyForm(lTendency);
end;

procedure TMainForm.UC16BEBOMMnuClick(Sender: TObject);
begin
  Engine.Files.Current.FileEncoding := EncodingUCS2BE;
end;

procedure TMainForm.UC16LEBOMMnuClick(Sender: TObject);
begin
  Engine.Files.Current.FileEncoding := EncodingUCS2LE;
end;

procedure TMainForm.UndoActExecute(Sender: TObject);
begin
  Engine.Files.Current.Undo;
end;

procedure TMainForm.UndoActUpdate(Sender: TObject);
begin
  UndoAct.Enabled := (Engine.Files.Current <> nil) and Engine.Files.Current.CanUndo;
end;

procedure TMainForm.UTF8BOMMnuClick(Sender: TObject);
begin
  Engine.Files.Current.FileEncoding := EncodingUTF8BOM;
end;

procedure TMainForm.UTF8MnuClick(Sender: TObject);
begin
  Engine.Files.Current.FileEncoding := EncodingUTF8;
end;

procedure TMainForm.UnixMnuClick(Sender: TObject);
begin
  Engine.Files.Current.LinesMode := efmUnix;
end;

procedure TMainForm.ShowToolbarActExecute(Sender: TObject);
begin
  UpdateToolbars;
end;

procedure TMainForm.WatchesGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_DELETE: DeleteCurrentWatch;
    end;
  end;
end;

procedure TMainForm.WindowsMnuClick(Sender: TObject);
begin
  Engine.Files.Current.LinesMode := efmWindows;
end;

procedure TMainForm.MacMnuClick(Sender: TObject);
begin
  Engine.Files.Current.LinesMode := efmMac;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;

  FOutputs := TOutputs.Create;
  FMenuItemsList := TObjectList.Create(True);

  Engine.FilePanel := EditorsPnl;
  Engine.ProjectPanel := ProjectPnl;

  Engine.RegisterNotify(Self);

  FDatabaseFrame := TDBManagerForm.Create(Self); //before Engine.Prepare, to register notify
  with FDatabaseFrame do
  begin
    Parent := DatabasePnl;
    Align := alClient;
    Visible := True;
  end;

  Engine.Prepare(GetKeyShiftState = [ssShift]);

  Engine.BeginUpdate;
  try
    ShowFolderFiles := Engine.Options.ShowFolderFiles;
    SortFolderFiles := Engine.Options.SortFolderFiles;
    FoldersAct.Checked := Engine.Options.ShowFolder;
    MessagesAct.Checked := Engine.Options.ShowMessages;
    BrowserPnl.Width := Engine.Options.FoldersPanelWidth;
    ShowToolbarAct.Checked := Engine.Options.ShowToolbar;

    with MessagesPnl, BoundsRect do
      BoundsRect := Rect(Left, Bottom - Engine.Options.MessagesHeight, Right, Bottom);
    MessagesPnl.Visible := False;
    UpdateBrowsePnl;
    UpdateMessagesPnl;
    UpdateToolbars;
  finally
    Engine.EndUpdate;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnException := @CatchErr;

  FMessages := Engine.MessagesList.GetMessages('Messages');
  EngineChanged;
  EngineRefresh;
  EngineDebug;
  FileTabs.ShowBorder := False;
  IPCServer.ServerID := sApplicationID;
  IPCServer.StartServer;
  LoadAddons;
  //Engine.SendLog('MiniEdit started');
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if Engine.Options.WindowMaxmized then //in Show instead of Create cause for take the current monitor
    WindowState := wsMaximized
  else if Engine.Options.BoundRect.Right - Engine.Options.BoundRect.Left > 10 then   //Safe load width
    BoundsRect := Engine.Options.BoundRect;

  //Engine.Start;
end;

procedure TMainForm.GotoFileFolderActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
  begin
    if Engine.Files.Current.CanAddRecentFiles then
    begin
      Folder := ExtractFilePath(Engine.Files.Current.FileName);
      Engine.ProcessRecentFolder(Folder);
    end;
  end;
end;

procedure TMainForm.HelpKeywordActExecute(Sender: TObject);
var
  aWordBreaker: TSynWordBreaker;
  aLine, aWord: string;
  XY: TPoint;
  StartX, EndX: Integer;
begin
  if (Engine.Files.Current <> nil) and (Engine.Files.Current.SynEdit <> nil) then
  with Engine.Files do
  begin
    XY := Current.SynEdit.LogicalCaretXY;
    aWordBreaker := TSynWordBreaker.Create;
    try
      aWordBreaker.Reset;
      aWordBreaker.WordBreakChars := aWordBreaker.WordBreakChars - ['.']; //<---- depend on tendency, TODO
      aLine := Current.SynEdit.LineText;
      if aWordBreaker.IsInWord(aLine, XY.X) then
      begin
        StartX := aWordBreaker.PrevWordStart(aLine, XY.X, True);
        EndX := aWordBreaker.NextWordEnd(aLine, XY.X, True);
      end;
      aWord := Copy(aLine, StartX, EndX - StartX);
      //aWord := Trim(Current.SynEdit.GetWordAtRowCol(Current.SynEdit.LogicalCaretXY));
      if aWord <> '' then
      begin
        Current.Tendency.HelpKeyWord(aWord);
      end;
    finally
      aWordBreaker.Free;
    end;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Engine.Options.WindowMaxmized := WindowState = wsMaximized;

  if WindowState <> wsMaximized then
    Engine.Options.BoundRect := BoundsRect;

  Engine.Options.ShowFolder := FoldersAct.Checked;
  Engine.Options.ShowFolderFiles := ShowFolderFiles;
  Engine.Options.SortFolderFiles := SortFolderFiles;
  Engine.Options.ShowMessages := MessagesAct.Checked;
  Engine.Options.MessagesHeight := MessagesPnl.Height;
  Engine.Options.ShowToolbar := ShowToolbarAct.Checked;
  Engine.Options.FoldersPanelWidth := BrowserPnl.Width;
  if Engine.Session.Active then
    Engine.Options.LastProject := Engine.Session.Project.FileName
  else
      Engine.Options.LastProject := '';
  Engine.Session.Close;
  Engine.UnregisterNotify(Self);

  Engine.Shutdown;
end;

procedure TMainForm.DBGLintActExecute(Sender: TObject);
begin
  SaveAllAct.Execute;
  Engine.CurrentTendency.Run([rnaLint]);
end;

procedure TMainForm.AddError(vMessageInfo: TMessageInfo);
begin
  MessagesGrid.RowCount := MessagesGrid.RowCount + 1;
  //MessagesGrid.Cells[1, MessagesGrid.RowCount - 1] := IntToStr(vError.ID);
  MessagesGrid.Cells[1, MessagesGrid.RowCount - 1] := vMessageInfo.Name;
  MessagesGrid.Cells[2, MessagesGrid.RowCount - 1] := vMessageInfo.Message;
  MessagesGrid.Cells[3, MessagesGrid.RowCount - 1] := vMessageInfo.FileName;
  MessagesGrid.Cells[4, MessagesGrid.RowCount - 1] := IntToStr(vMessageInfo.Line);
end;

procedure TMainForm.AboutActExecute(Sender: TObject);
begin
  with TAboutForm.Create(Application) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.ProjectChanged;
var
  b: boolean;
begin
  b := Engine.Session.Active;
  //ProjectOptionsAct.Enabled := b;
  SaveProjectAct.Enabled := b;
  SaveAsProjectAct.Enabled := b;
  AddFileToProjectAct.Enabled := b;
  CloseProjectAct.Enabled := b;
  ProjectExploreFolderAct.Enabled := b;
  SCMMnu.Visible := Engine.SCM <> nil;
  if Engine.SCM <> nil then
    SCMMnu.Caption := Engine.SCM.Name
  else
    SCMMnu.Caption := '';

  UpdatePanel;
  Engine.Update([ecsMenu]);
  Engine.Update([ecsFolder]);
end;

procedure TMainForm.AddMenuItem(AName, ACaption, AParentMenu: string; AOnClickEvent: TNotifyEvent; AShortCut: TShortCut);
var
  AParentMnu: TMenuItem;
  MenuItem: TMenuItem;
  n: string;
begin
  if AParentMenu = '' then
    AParentMenu := 'Edit';

  AParentMnu := FindComponent(AParentMenu + 'Mnu') as TMenuItem;
  if AParentMnu = nil then
  begin
    Engine.SendLog(AParentMenu + ' menu item not found');
    exit;
  end;

  n := AParentMenu + '_MenuItem_sep';
  MenuItem := FindComponent(n) as TMenuItem;
  if MenuItem = nil then
  begin
    MenuItem := TMenuItem.Create(Self);
    AParentMnu.Add(MenuItem);
    MenuItem.Name := n;
    MenuItem.Caption := '-';
    FMenuItemsList.Add(MenuItem);

    {MenuItem := TMenuItem.Create(EditorPopupMenu);
    EditorPopupMenu.Items.Add(MenuItem);
    MenuItem.Name := AParentMnu + '_PopupMenuItem_sep';
    MenuItem.Caption := '-';
    FMenuItemsList.Add(MenuItem);}
  end;

  MenuItem := TMenuItem.Create(Self);
  AParentMnu.Add(MenuItem);
  MenuItem.Name := AParentMenu + '_MenuItem__' + AName;
  MenuItem.Caption := ACaption;
  MenuItem.OnClick := AOnClickEvent;
  MenuItem.ShortCut := AShortCut;
  FMenuItemsList.Add(MenuItem);

  {MenuItem := TMenuItem.Create(EditorPopupMenu);
  EditorPopupMenu.Items.Add(MenuItem);
  MenuItem.Name := AParentMnu + '_PopupMenuItem__' + AName;
  MenuItem.Caption := ACaption;
  MenuItem.OnClick := AOnClickEvent;
  //MenuItem.ShortCut := AShortCut; //No ShortCut for popup, no dublicate
  FMenuItemsList.Add(MenuItem);}
end;

procedure TMainForm.UpdateMenuItems;
begin
  with Engine.CurrentTendency do
  begin
    DBGRunAct.Enabled := CanAny([capExecute, capCompile, capLint]);
    DBGExecuteAct.Enabled := Can(capExecute);
    DBGCompileAct.Visible := Can(capCompile);
    DBGResetAct.Enabled := Can(capStop);
    DBGLintAct.Enabled := Can(capLint);
    DBCommitAct.Enabled := Can(capTransaction);
    DBRollbackAct.Enabled := Can(capTransaction);

    DBGDebugModeAct.Enabled := (capDebug in Capabilities);
    DBGDebugModeAct.Checked :=  (Engine.CurrentTendency.Debugger <> nil) and Engine.CurrentTendency.Debugger.Active;

    DBGAddWatchAct.Enabled := capTrace in Capabilities;
    DBGBreakpointsAct.Enabled := capTrace in Capabilities;
    DBGToggleBreakpointAct.Enabled := capTrace in Capabilities;

    DBGStepOverAct.Enabled := capTrace in Capabilities;
    DBGStepIntoAct.Enabled := capTrace in Capabilities;
    DBGStepOutAct.Enabled := capTrace in Capabilities;
    DBGRunToCursorAct.Enabled := capTrace in Capabilities;

    if Engine.Files.Current <> nil then
    begin
      FMenuItemsList.Clear;
      if Engine.Files.Current.Group <> nil then
        Engine.Files.Current.Group.Category.EnumMenuItems(@AddMenuItem);
    end;

    with Engine.CurrentTendency do
    begin
      MessagesTabs.Page[WatchesGrid].Visible := capDebug in Capabilities;
      MessagesTabs.Page[CallStackGrid].Visible := capTrace in Capabilities;
      //MessagesTabs.PageItem[MessagesGrid].Visible := capErrors in Capabilities;
    end;
  end;
end;

procedure TMainForm.UpdatePanel;
  procedure FreeFrame;
  begin
{    if (FProjectFrame <> nil) and Supports(FProjectFrame, IEditorOptions) then
      (FProjectFrame as IEditorOptions).Apply;} //Project is already freed we cant apply
    FreeAndNil(FProjectFrame);
  end;
begin
  if Engine.Session.Active then
  begin
    if (FProjectFrame = nil) or ((FProjectFrame as IEditorProjectFrame).Project <> Engine.Session.Project) then
    begin
      if (FProjectFrame <> nil) then
        FreeFrame;

      Engine.Session.Project.Options.CreateProjectPanel(Self, Engine.Session.Project, FProjectFrame);
      if FProjectFrame <> nil then
      begin
        FProjectFrame.Parent := ProjectPnl;
        FProjectFrame.Align := alClient;
        FProjectFrame.Visible := False;
        //FProjectFrame.Color := .Color;
        if Supports(FProjectFrame, IEditorOptions) then
          (FProjectFrame as IEditorOptions).Retrieve;
      end;
    end;
  end
  else
    FreeFrame;
  BrowserTabs.Page[ProjectPnl].Visible := FProjectFrame <> nil;
  //BrowserTabs.Page[DatabasePnl].Visible := DBEngine.DB.IsActive;
end;

procedure TMainForm.SaveAsProjectActExecute(Sender: TObject);
begin
  if Engine.Session.Active then
    Engine.Session.SaveAs;
end;

procedure TMainForm.ProjectExploreFolderActExecute(Sender: TObject);
begin
  if Engine.Session.Active then
    ExploreFolder(Engine.Session.Project.Path);
end;

procedure TMainForm.ManageActExecute(Sender: TObject);
begin
  with TManageRecentsForm.Create(Application) do
  begin
    Engine.BeginUpdate;
    try
      ShowModal;
    finally
      Engine.EndUpdate;
    end;
  end;
end;

procedure TMainForm.CloseAllFilesActExecute(Sender: TObject);
begin
  Engine.Files.CloseAll;
end;

procedure TMainForm.OpenIncludeActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Engine.Files.Current.OpenInclude;
end;

procedure TMainForm.CopyActUpdate(Sender: TObject);
begin
  CopyAct.Enabled := (Engine.Files.Current <> nil) and Engine.Files.Current.CanCopy;
end;

procedure TMainForm.PasteActUpdate(Sender: TObject);
begin
  PasteAct.Enabled := (Engine.Files.Current <> nil) and Engine.Files.Current.CanPaste;
end;

procedure TMainForm.CutActUpdate(Sender: TObject);
begin
  CutAct.Enabled := (Engine.Files.Current <> nil) and Engine.Files.Current.CanCopy;
end;

procedure TMainForm.SelectAllActUpdate(Sender: TObject);
begin
  SelectAllAct.Enabled := (Engine.Files.Current <> nil);
end;

procedure TMainForm.PasteActExecute(Sender: TObject);
begin
  Engine.Files.Current.Paste;
end;

procedure TMainForm.CopyActExecute(Sender: TObject);
begin
  Engine.Files.Current.Copy;
end;

procedure TMainForm.CutActExecute(Sender: TObject);
begin
  Engine.Files.Current.Cut;
end;

procedure TMainForm.SelectAllActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Engine.Files.Current.SelectAll;
end;

function TMainForm.CanOpenInclude: boolean;
begin
  Result := (Engine.Files.Current <> nil) and Engine.Files.Current.CanOpenInclude;
end;

procedure TMainForm.SetShowFolderFiles(AValue: TShowFolderFiles);
begin
  if FShowFolderFiles =AValue then exit;
  FShowFolderFiles :=AValue;
  if FoldersAct.Checked then
    Engine.Update([ecsFolder]);
  case FShowFolderFiles of
    sffRelated: ShowTendencyFilterAct.Checked := True;
    sffProject: ShowProjectFilterAct.Checked := True;
    sffKnown: ShowKnownFilterAct.Checked := True;
    sffAll: ShowAllFilterAct.Checked := True;
  end;
end;

procedure TMainForm.SetSortFolderFiles(AValue: TSortFolderFiles);
begin
  if FSortFolderFiles =AValue then
    Exit;
  FSortFolderFiles :=AValue;
  if FoldersAct.Checked then
    Engine.Update([ecsFolder]);
  case FSortFolderFiles of
    srtfByNames: SortByNamesAct.Checked := True;
    srtfByExt: SortByExtensionsAct.Checked := True;
  end;
end;

procedure TMainForm.OpenIncludeActUpdate(Sender: TObject);
begin
  OpenIncludeAct.Enabled := CanOpenInclude;
end;

procedure TMainForm.GotoLineActUpdate(Sender: TObject);
begin
  GotoLineAct.Enabled := (Engine.Files.Current <> nil) and (Engine.Files.Current is ITextEditor);
end;

procedure TMainForm.GotoLineActExecute(Sender: TObject);
begin
  if (Engine.Files.Current <> nil) and (Engine.Files.Current is ITextEditor) then
    Engine.Files.Current.GotoLine;
end;

procedure TMainForm.UpdateFolder;
var
  r: integer;
  All: Boolean;
  SearchRec: TSearchRec;
  aItem: TListItem;
  AExtensions: TStringList;

  function MatchExtension(vExtension: string): boolean;
  begin
    if not All then
    begin
      Result := AExtensions.IndexOf(vExtension) >= 0;
    end
    else
      Result := True;
  end;

var
  aFiles: TStringList;
  SaveSelected: Integer;
  Filter: string;
begin
  FolderPathLbl.Caption := Folder;
  FolderPathLbl.Hint := Folder;
  FileList.Items.BeginUpdate;
  SaveSelected := FileList.ItemIndex;
  try
    FileList.Clear;

    All := False;
    AExtensions := TStringList.Create;
    AExtensions.Delimiter := ';';
    try
      case ShowFolderFiles of
        sffRelated:
          if Engine.Session.Active then
            Engine.Session.Project.Tendency.Groups.EnumExtensions(AExtensions)
          else
            Engine.Groups.EnumExtensions(AExtensions);
        sffProject:
          if Engine.Session.Active then
          begin
            if Engine.Session.Project.FileFilter = '' then
              Engine.Session.Project.Tendency.Groups.EnumExtensions(AExtensions)
            else
             AExtensions.DelimitedText := Engine.Session.Project.FileFilter
          end
          else
            Engine.Groups.EnumExtensions(AExtensions);
        sffKnown: Engine.Groups.EnumExtensions(AExtensions);
        sffAll: All := True;
      end;
      AExtensions.Add('mne-project');

      aFiles := THashedStringList.Create;
      try
        //Folders
        if (Folder <> '') and DirectoryExists(Folder) then
        begin
          aFiles.Clear;
          if FilesFilterEdit.Text <> '' then
          begin
            Filter := FilesFilterEdit.Text;
            if Pos('*', Filter) <= 0 then
              Filter := '*' + Filter + '*'
          end
          else
            Filter := '*';
          r := FindFirst(Folder + Filter, faAnyFile or faDirectory, SearchRec);
          while r = 0 do
          begin
            if (SearchRec.Name <> '.') then
            begin
              if (SearchRec.Attr and faDirectory) <> 0 then
                aFiles.Add(SearchRec.Name);
            end;
            r := FindNext(SearchRec);
          end;
          SysUtils.FindClose(SearchRec);

          aFiles.Sort;

          for r := 0 to aFiles.Count -1 do
          begin
            aItem := FileList.Items.Add;
            aItem.Caption := aFiles[r];
            aItem.Data := nil;
            aItem.ImageIndex := 0;
          end;

          //Files
          aFiles.Clear;
          r := FindFirst(Folder + Filter, faAnyFile, SearchRec);
          while r = 0 do
          begin
            //if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
            if (SearchRec.Attr and faDirectory) = 0 then
            begin
              if MatchExtension(ExtractFileExt(SearchRec.Name)) then
                aFiles.Add(SearchRec.Name);
            end;
            r := FindNext(SearchRec);
          end;
          SysUtils.FindClose(SearchRec);

          if SortFolderFiles  = srtfByNames then
            aFiles.Sort
          else
            aFiles.CustomSort(@SortByExt);

          for r := 0 to aFiles.Count -1 do
          begin
            aItem := FileList.Items.Add;
            aItem.Caption := aFiles[r];
            aItem.Data := Pointer(1);
            aItem.ImageIndex := EditorResource.GetFileImageIndex(aFiles[r], -1);
          end;
        end;
      finally
        aFiles.Free;
      end;
    finally
      AExtensions.Free;
    end;
  finally
    if FileList.Items.Count > 0 then
    begin
      if (SaveSelected >= 0) and (SaveSelected < FileList.Items.Count) then
        FileList.ItemIndex := SaveSelected
      else
        FileList.Items[0].Selected := True;
    end;
    FileList.Items.EndUpdate;
  end;
end;

procedure TMainForm.BrowseFolderMnuClick(Sender: TObject);
var
  aFolder: string;
begin
  aFolder := Folder;
  if SelectFolder('Select root directory for you Engine', '', aFolder) then
  begin
    Folder := aFolder;
    Engine.ProcessRecentFolder(Folder);
  end;
end;

procedure TMainForm.ChangeState(var State: TEditorChangeStates);
begin
  if ResetUpdate(ecsChanged, State) then
    EngineChanged;
  if ecsRefresh in State then
    EngineRefresh;
  if ecsDebug in State then
    EngineDebug;
  if ecsShow in State then
    ForceForegroundWindow;
  if ecsEdit in State then
    EngineEdited;
  if ecsProject in State then
    ProjectChanged;
  if ecsProjectLoaded in State then
    ProjectLoaded;
  if ecsRecents in State then
    EnumRecents;
  if ecsState in State then
    EngineState;
  if ecsOptions in State then
    OptionsChanged;
  if ecsFolder in State then
    UpdateFolder;
  if ecsTransaction in State then
    TransactionChanged;
end;

procedure AddOutput(Sender: Pointer; Index, CharIndex, NextIndex: Integer; S: string; var Resume: Boolean);
var
  aLine: TOutputLine;
begin
  with TObject(Sender) as TMainForm do
  begin
    if (Index = 0) and (FOutputs.Count > 0) then
    begin
      aLine := FOutputs.Last;
      aLine.Info.Message := aLine.Info.Message + S;
    end
    else
    begin
      aLine := TOutputLine.Create;
      aLine.Info.Message := S;
      FOutputs.Add(aLine);
    end;

    if (Index = 0) and (OutputEdit.Lines.Count > 0) then
      OutputEdit.Lines[OutputEdit.Lines.Count -1] := OutputEdit.Lines[OutputEdit.Lines.Count -1] + S
    else
      OutputEdit.Lines.Add(S);
  end;
end;

procedure TMainForm.EngineMessage(S: string; vMessageInfo: TMessageInfo);
var
  aLine: TOutputLine;
begin
  case vMessageInfo.MessageType of
    msgtStatus:
      MessageLabel.Caption := Trim(S);
    msgtEndStatus:
    begin
      MessageLabel.Caption := Trim(S);
      StatusTimer.Enabled := True;
    end;
    msgtOutput:
    begin
      StrToStringsExCallback(S, 1, self, @AddOutput, [#13#10, #13, #10], [], [], []);
      OutputEdit.CaretY := OutputEdit.Lines.Count;
    end;
    msgtInteractive:
    begin
      aLine := TOutputLine.Create;
      aLine.Info := vMessageInfo;
      FOutputs.Add(aLine);
      OutputEdit.Lines.Add(S);
      OutputEdit.CaretY := OutputEdit.Lines.Count;
      aLine.LogLine := OutputEdit.CaretY;
      if (vMessageInfo.Processed) then
        AddError(vMessageInfo);
    end;
    msgtLog:
    begin
      LogEdit.Lines.Add(S);
      LogEdit.CaretY := LogEdit.Lines.Count;
    end;
  end;
end;

function TMainForm.ChooseSCM(var vSCM: TEditorSCM): Boolean;
var
  aName: string;
begin
  if (vSCM <> nil) then
    aName := vSCM.Name
  else
    aName := '';
  Result := ShowSelectList('Select SCM type', Engine.SourceManagements, [slfIncludeNone], aName);
  vSCM := Engine.SourceManagements.Find(aName);
end;

procedure TMainForm.SetMainTitle;
begin
  if (Engine.Session.Active) then
  begin
    if (Engine.Session.Project.Title <> '') then
      Caption := Engine.Session.Project.Title + ' - ' + sApplicationTitle
    else if (Engine.Session.Project.Name <> '') then
      Caption := Engine.Session.Project.Name + ' - ' + sApplicationTitle
    else if Engine.Files.Current <> nil then
      Caption := Engine.Files.Current.BaseName + ' - ' + sApplicationTitle
    else
      Caption := sApplicationTitle;
  end
  else
    Caption := sApplicationTitle;
  Application.Title := Caption;
end;

procedure TMainForm.ProjectLoaded;
begin
//  Folder := Engine.GetRoot; //do not load root to folder, for lsl temp files not to save
end;

procedure TMainForm.ReplaceActExecute(Sender: TObject);
begin
  Engine.Files.Replace;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FMenuItemsList);
  FreeAndNil(FOutputs);
  Application.OnException := nil;
  inherited;
end;

procedure TMainForm.RevertActExecute(Sender: TObject);
begin
  Engine.Files.Revert;
end;

procedure TMainForm.FileListKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_F5:
      begin
        Engine.Update([ecsFolder]);
      end;
      VK_BACK:
      begin
        FollowFolder('..', FileList.Focused);
      end;
    end;
  end;
end;

procedure TMainForm.FollowFolder(vFolder: string; FocusIt: Boolean);
var
  OldFolder: string;
  i, n: integer;
begin
  if vFolder = '..' then
    OldFolder := ExtractFileName(ExcludeTrailingPathDelimiter(Folder))
  else
    OldFolder := '';
  Folder := Folder + vFolder;
  n := 0; // if old folder not found or go inside it
  if (OldFolder <> '') then
  begin
    for i := 0 to FileList.Items.Count - 1 do
    begin
      if FileList.Items[i].Caption = OldFolder then
      begin
        n := i;
        break;
      end;
    end;
  end;
  if (FileList.Items.Count > 0) and (n >= 0) then
  begin
    FileList.ItemIndex := n;
    FileList.Items[FileList.ItemIndex].Focused := True;
    if FocusIt then
      FileList.SetFocus;
  end;
end;

procedure TMainForm.ExploreFileFolderMnuClick(Sender: TObject);
begin
  if FileList.Selected <> nil then
  begin
    ExploreFile(Folder + FileList.Selected.Caption);
  end;
end;

procedure TMainForm.OpenColorActUpdate(Sender: TObject);
begin
  OpenColorAct.Enabled := GetCurrentColorText <> '';
end;

function TMainForm.GetCurrentColorText: string;
begin
  Result := '';
  if (Engine.Files.Current <> nil) and (Engine.Files.Current.SynEdit <> nil) then
  with Engine.Files do
  begin
    Result := Trim(Current.SynEdit.GetWordAtRowCol(Current.SynEdit.LogicalCaretXY));
    if Result <> '' then
    begin
      if Result[1] <> Current.Group.Category.GetColorPrefix then
        Result := '';
    end;
  end;
end;

procedure TMainForm.OpenColorActExecute(Sender: TObject);
var
  aWord: string;
  aColor: TColor;
  aDialog: TColorDialog;
  aIsUpper: boolean;

  procedure CheckIsUpper;
  var
    i: integer;
  begin
    aIsUpper := False;
    for i := 1 to Length(aWord) do
    begin
      if aWord[i] in ['A'..'Z', 'a'..'z'] then
      begin
        aIsUpper := UpperCase(aWord[i]) = aWord[i];//todo IsUpper
        break;
      end;
    end;
  end;
begin
  if (Engine.Files.Current <> nil) and (Engine.Files.Current.SynEdit <> nil) then
  with Engine.Files do
  begin
    Current.SynEdit.SelectWord;
    aWord := Current.SynEdit.SelText;
    if (aWord <> '') and (Length(aWord) > 1) then
    begin
      CheckIsUpper;
      aColor := Current.Group.Category.DeformatColor(aWord);
      aDialog := TColorDialog.Create(Self);
      try
        aDialog.Color := aColor;
        if aDialog.Execute then
        begin
          aWord := Current.Group.Category.FormatColor(aDialog.Color);
          //aWord := ColorToRGBHex(aDialog.Color, Engine.Files.Current.Group.Category.GetColorPrefix);
          if aIsUpper then
            aWord := UpperCase(aWord)
          else
            aWord := LowerCase(aWord);
          Current.SynEdit.SelText := aWord;
        end;
      finally
        aDialog.Free;
      end;
    end;
  end;
end;

procedure TMainForm.EngineAction(EngineAction: TEditorAction);
begin
  case EngineAction of
    eaShowFolders:
    begin
      BrowserTabs.ActiveControl := FolderPnl;
    end;
    eaShowDatabases:
    begin
      BrowserTabs.ActiveControl := DatabasePnl;
    end;
    eaShowProject:
    begin
      if ProjectPnl.IsVisible then
        BrowserTabs.ActiveControl := ProjectPnl;
    end;
    eaClearOutput :
    begin
      FOutputs.Clear;
      OutputEdit.Lines.Clear;
      MessagesGrid.RowCount := 1;
    end;
    eaClearLog :
    begin
      LogEdit.Lines.Clear;
    end;
    eaEnd:
    begin
      {if MessagesGrid.RowCount > 1 then
      begin
        MessagesGrid.Row := 1;
        MessagesGrid.OnDblClick(MessagesGrid);
      end;}
      if FOutputs.Count > 0 then
      begin
        OutputEdit.CaretY := FOutputs[0].LogLine;
        LogGotoLineFirstError;
        OutputEdit.Invalidate;//there is bug refereshing marked line
      end;
    end;
  end;
end;

procedure TMainForm.SCMCommitActExecute(Sender: TObject);
begin
  Engine.SCM.CommitDirectory(Folder);
end;

procedure TMainForm.SCMDiffFileActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Engine.SCM.DiffFile(Engine.Files.Current.FileName);
end;

procedure TMainForm.SCMCommitFileActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Engine.SCM.CommitFile(Engine.Files.Current.FileName);
end;

procedure TMainForm.SCMUpdateFileActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Engine.SCM.UpdateFile(Engine.Files.Current.FileName);
end;

procedure TMainForm.SCMUpdateActExecute(Sender: TObject);
begin
  Engine.SCM.UpdateDirectory(Folder);
end;

procedure TMainForm.SCMRevertActExecute(Sender: TObject);
begin
  Engine.SCM.RevertDirectory(Folder);
end;

procedure TMainForm.DBGDebugModeActUpdate(Sender: TObject);
begin
  with Engine.CurrentTendency do
    DBGDebugModeAct.Enabled := (capDebug in Capabilities);
end;

procedure TMainForm.DBGDebugModeActExecute(Sender: TObject);
begin
  if Engine.CurrentTendency.Debugger <> nil then
  begin
    Engine.CurrentTendency.Debugger.Active := not Engine.CurrentTendency.Debugger.Active;
//    DBGDebugModeAct.Checked := not DBGDebugModeAct.Checked;
//    Engine.CurrentTendency.Debugger.Active := DBGDebugModeAct.Checked;
  end
  else
    DBGDebugModeAct.Checked := False;
end;

procedure TMainForm.DBGStepOverActExecute(Sender: TObject);
begin
  if Engine.CurrentTendency.Debugger <> nil then
    if Engine.CurrentTendency.Debugger.Running then
      Engine.CurrentTendency.Debugger.Action(dbaStepOver);
end;

procedure TMainForm.DBGStepIntoActExecute(Sender: TObject);
begin
  if Engine.CurrentTendency.Debugger <> nil then
    if Engine.CurrentTendency.Debugger.Running then
      Engine.CurrentTendency.Debugger.Action(dbaStepInto);
end;

procedure TMainForm.DBGResetActExecute(Sender: TObject);
begin
  Engine.CurrentTendency.Run([rnaKill]);
  {if Engine.CurrentTendency.Debugger <> nil then
    Engine.CurrentTendency.Debugger.Action(dbaReset)
  else
    Engine.Session.Run.Stop;}
end;

procedure TMainForm.DBGExecuteActExecute(Sender: TObject);
begin
  Engine.CurrentTendency.Run([rnaExecute])
end;

procedure TMainForm.DBGStepOutActExecute(Sender: TObject);
begin
  if Engine.CurrentTendency.Debugger <> nil then
    Engine.CurrentTendency.Debugger.Action(dbaStepOut);
end;

function TMainForm.GetFolder: string;
begin
  Result := Engine.BrowseFolder;
end;

procedure TMainForm.SaveAsActExecute(Sender: TObject);
begin
  Engine.Files.SaveAs;
end;

procedure TMainForm.EngineDebug;
begin
  //DBGRunAct.Enabled := not Engine.Session.Run.Active;
  UpdateMenuItems;
  if (Engine.CurrentTendency.Debugger <> nil) then
  begin
    //DebugPnl.Caption := Engine.CurrentTendency.Debugger.GetKey;
    UpdateFileHeaderPanel;
    UpdateCallStack;
    UpdateWatches;
    Engine.Files.Refresh; // not safe thread
  end;
end;

procedure TMainForm.UpdateFileHeaderPanel;
begin
  if (Engine.Files.Current <> nil) and (Engine.Files.Current.SynEdit = Engine.DebugLink.ExecutedControl) then
    BugSignBtn.Visible := True
  else
    BugSignBtn.Visible := False;
  FileHeaderPanel.Visible := Engine.Files.Count > 0;
  if FileHeaderPanel.Visible then
    FileHeaderPanel.Refresh;
  if Engine.Session.Project <> nil then
  begin
    if Engine.Session.Project.IsActive then
      MainFileAct.Caption := Engine.Session.Project.RunOptions.MainFile
    else
      MainFileAct.Caption := ExtractFileName(Engine.Session.Project.RunOptions.MainFile)
  end
  else
    MainFileAct.Caption := '';
  MainFileAct.Visible := MainFileAct.Caption <> '';
end;

procedure TMainForm.UpdateCallStack;
var
  i: integer;
  aIndex: integer;
begin
  if Engine.CurrentTendency.Debugger <> nil then
  begin
    aIndex := CallStackGrid.Row;
    CallStackGrid.BeginUpdate;
    try
      CallStackGrid.RowCount := Engine.DebugLink.CallStack.Count + 1;
      with Engine.DebugLink do
      try
        for i := 0 to CallStack.Count - 1 do
        begin
          CallStackGrid.Cells[1, i + 1] := Engine.DebugLink.CallStack[i].FileName;
          CallStackGrid.Cells[2, i + 1] := IntToStr(Engine.DebugLink.CallStack[i].Line);
        end;
      finally
      end;
    finally
      if (aIndex > 0) and (aIndex <= CallStackGrid.RowCount) then
        CallStackGrid.Row := aIndex;
      CallStackGrid.EndUpdate;
    end;
  end;
end;

procedure TMainForm.OptionsChanged;

  procedure CorrectGridColors(AGrid: TStringGrid);
  begin
    AGrid.FixedColor := Engine.Options.Profile.Attributes.Gutter.BackColor; //ntvTheme.Painter.ActiveColor;
    AGrid.TitleFont.Color := Engine.Options.Profile.Attributes.Gutter.Foreground;
    AGrid.FixedGridLineColor := Engine.Options.Profile.Attributes.Gutter.ForeColor;
    AGrid.Font.Color := Engine.Options.Profile.Attributes.Default.Foreground;
    AGrid.SelectedColor := Engine.Options.Profile.Attributes.Selected.Background;
    AGrid.FocusColor := Engine.Options.Profile.Attributes.Active.ForeColor;
  end;

var
  i: Integer;
begin
  Font.Name := Engine.Options.Profile.Attributes.FontName;
  Font.Size := Engine.Options.Profile.Attributes.FontSize;

  Color := Engine.Options.Profile.Attributes.Panel.Background;
  Font.Color := Engine.Options.Profile.Attributes.Panel.Foreground;

  if IsDarkColor(Engine.Options.Profile.Attributes.Panel.Background) then
    EditorResource.Switch(thsDark)
  else
    EditorResource.Switch(thsLight);

  //Theme.Active.Background := MixColors(Engine.Options.Profile.Attributes.Panel.Foreground, Engine.Options.Profile.Attributes.Panel.Background, 50);
  //Theme.Separator.Foreground := Lighten(Theme.Active.Background, 10);
  //Theme.Separator.Background := Darken(Theme.Active.Background, 10);

  Theme.Odd.Background := Engine.Options.Profile.Attributes.Default.Background;
  Theme.Panel.Foreground := Engine.Options.Profile.Attributes.Panel.Foreground;
  Theme.Panel.Background := Engine.Options.Profile.Attributes.Panel.Background;
  Theme.Default.Foreground := Engine.Options.Profile.Attributes.Default.Foreground;
  Theme.Default.Background := Engine.Options.Profile.Attributes.Default.Background;
  Theme.Button.Foreground := Engine.Options.Profile.Attributes.Gutter.Foreground;
  Theme.Button.Background := Engine.Options.Profile.Attributes.Gutter.Background;
  Theme.Active.Foreground := Engine.Options.Profile.Attributes.Active.Foreground;
  Theme.Active.Background := Engine.Options.Profile.Attributes.Active.Background;
  Theme.Header.Foreground := Engine.Options.Profile.Attributes.Gutter.Foreground;
  Theme.Header.Background := Engine.Options.Profile.Attributes.Gutter.Background;
  Theme.Separator.Background := Engine.Options.Profile.Attributes.Separator.Background;
  Theme.Separator.Foreground := Engine.Options.Profile.Attributes.Separator.Foreground;
  ThemeEngine.Correct;
  ThemeEngine.Notify;

//  FileList.Font.Name := Engine.Options.Profile.Attributes.FontName;
//  FileList.Font.Color := Engine.Options.Profile.Attributes.Default.Foreground;
//  FileList.Color := Engine.Options.Profile.Attributes.Default.Background;

  CorrectGridColors(MessagesGrid);
  CorrectGridColors(CallStackGrid);
  CorrectGridColors(WatchesGrid);
  CorrectGridColors(SearchGrid);

  {if FDatabaseFrame <> nil then
    CorrectGridColors(FDatabaseFrame.MembersGrid);}

  OutputEdit.Font.Color := Engine.Options.Profile.Attributes.Default.Foreground;
  OutputEdit.Color := Engine.Options.Profile.Attributes.Default.Background;
  for i := 0 to LogEdit.Gutter.Parts.Count - 1 do
  begin
    OutputEdit.Gutter.Parts.Part[i].MarkupInfo.Foreground := Engine.Options.Profile.Attributes.Gutter.Foreground;
    OutputEdit.Gutter.Parts.Part[i].MarkupInfo.Background := Engine.Options.Profile.Attributes.Gutter.Background;
  end;

  LogEdit.Font.Color := Engine.Options.Profile.Attributes.Default.Foreground;
  LogEdit.Color := Engine.Options.Profile.Attributes.Default.Background;
  for i := 0 to LogEdit.Gutter.Parts.Count - 1 do
  begin
    LogEdit.Gutter.Parts.Part[i].MarkupInfo.Foreground := Engine.Options.Profile.Attributes.Gutter.Foreground;
    LogEdit.Gutter.Parts.Part[i].MarkupInfo.Background := Engine.Options.Profile.Attributes.Gutter.Background;
  end;

  FilesFilterEdit.Color := Engine.Options.Profile.Attributes.Default.Background;
  FilesFilterEdit.Font.Color := Engine.Options.Profile.Attributes.Default.Foreground;
end;

procedure TMainForm.UpdateWatches;
var
  i: integer;
  aIndex: integer;
  aTendency: TEditorTendency;
begin
  //todo not good idea, we should refresh without clear the grid
  aTendency := Engine.Tendency;
  if aTendency.Debugger <> nil then
  begin
    aIndex := WatchesGrid.Row;
    WatchesGrid.BeginUpdate;
    try
      WatchesGrid.RowCount := aTendency.Debugger.Watches.Count + 1;
      DebugManager.Enter;
      try
        for i := 0 to aTendency.Debugger.Watches.Count - 1 do
        begin
          WatchesGrid.Cells[1, i + 1] := aTendency.Debugger.Watches[i].Name;
          WatchesGrid.Cells[2, i + 1] := aTendency.Debugger.Watches[i].VarType;
          WatchesGrid.Cells[3, i + 1] := aTendency.Debugger.Watches[i].Value;
        end;
      finally
        DebugManager.Leave;
      end;
    finally
      if (aIndex > 0) and (aIndex <= WatchesGrid.RowCount) then
        WatchesGrid.Row := aIndex;
      WatchesGrid.EndUpdate;
    end;
  end;
end;

procedure TMainForm.ShowMessagesList;
begin
  MessagesTabs.ActiveControl := MessagesGrid;
end;

procedure TMainForm.ShowWatchesList;
begin
  MessagesTabs.ActiveControl := WatchesGrid;
end;

procedure TMainForm.ShowSearchGrid;
begin
  MessagesTabs.ActiveControl := SearchGrid;
end;

procedure TMainForm.LoadAddons;
var
  i: integer;

  procedure AddMenu(vParentMenu, vName, vCaption: string; vOnClick: TNotifyEvent);
  var
    c: TComponent;

    function CreateMenuItem: TMenuItem;
    begin
      Result := TMenuItem.Create(Self);
      Result.Name := vName;
      Result.Caption := vCaption;
      Result.OnClick := vOnClick;
    end;

    function CreateToolButton: TToolButton;
    begin
      Result := TToolButton.Create(Self);
      Result.Name := vName;
      Result.Caption := vCaption;
      Result.OnClick := vOnClick;
    end;

  begin
    c := FindComponent(vParentMenu);
    if c <> nil then
    begin
      if c is TMenu then
        (c as TMenu).Items.Add(CreateMenuItem)
      else if c is TMenuItem then
        (c as TMenuItem).Add(CreateMenuItem);
      {else if c is TToolBar then
         CreateToolButton.Parent := c as TToolBar
      else if c is TToolButton then
        CreateToolButton.Parent := c as TbittToolBar}
    end
    else
      ToolsMnu.Add(CreateMenuItem);
    //m.Parent := ToolsMnu;
  end;

begin
  //MainMenu.Items.BeginUpdate;
  try
    for i := 0 to Addons.Count - 1 do
    begin
      if Supports(Addons[i].Addon, IMenuAddon) and (Supports(Addons[i].Addon, IClickAddon)) then
        AddMenu('', Addons[i].Name, (Addons[i].Addon as IMenuAddon).GetCaption, @((Addons[i].Addon as IClickAddon).Click));
    end;
  finally
    //MainMenu.Items.EndUpdate;
  end;
end;

procedure TMainForm.ShowFileAtLine(vFileName: string; vLine: Integer; vColumn: Integer);
begin
  Engine.Files.OpenFile(vFileName);
  with Engine.Files do
  begin
    if Current is TTextEditorFile then
      if vLine > 0 then
        if (Current <> nil) and (Current.SynEdit <> nil) then
        begin
          Current.SynEdit.LogicalCaretXY := Point(vColumn, vLine);
//          Current.SynEdit.SelectLine;
          Current.SynEdit.EnsureCursorPosVisible;
          Current.SetHighlightLine(vLine); //after changing
          Current.SynEdit.SetFocus;
        end;
  end;
end;

procedure TMainForm.CheckChangedEvent(Data: PtrInt);
begin
  Engine.Files.CheckChanged;
end;

procedure TMainForm.Add1Click(Sender: TObject);
var
  S: string;
begin
  S := '';
  if MsgBox.Input(S, 'Add Watch, Enter variable name') then
    if S <> '' then
      AddWatch(S);
end;

procedure TMainForm.AddWatch(s: string);
begin
  if Engine.CurrentTendency.Debugger <> nil then
  begin
    s := Trim(s);
    if s <> '' then
    begin
      Engine.CurrentTendency.Debugger.Watches.Add(s);
      UpdateWatches;
    end;
  end;
end;

procedure TMainForm.Delete1Click(Sender: TObject);
begin
  DeleteCurrentWatch;
end;

procedure TMainForm.DeleteWatch(s: string);
begin
  if Engine.CurrentTendency.Debugger <> nil then
  begin
    Engine.CurrentTendency.Debugger.Watches.Remove(s);
    //UpdateWatches;
  end;
end;

procedure TMainForm.EnumRecents;
begin
  EnumRecentFiles;
  EnumRecentDatbases;
  EnumRecentFolders;
  EnumRecentProjects;
end;

procedure TMainForm.DBGToggleBreakpointActExecute(Sender: TObject);
var
  aLine: integer;
begin
  if (Engine.Files.Current <> nil) and (Engine.Files.Current.Tendency.Debugger <> nil) then
  begin
    if (Engine.Files.Current.SynEdit <> nil) and (ActiveControl = Engine.Files.Current.SynEdit) and (capDebug in Engine.Files.Current.Group.Capapility) then
      with Engine.Files do
      begin
        aLine := Current.SynEdit.CaretY;
        DebugManager.Enter;
        try
          Current.Tendency.Debugger.Breakpoints.Toggle(Current.FileName, aLine);
        finally
          DebugManager.Leave;
        end;
        Current.SynEdit.InvalidateLine(aLine);
      end;
  end;
end;

procedure TMainForm.RunFile;
begin
  Engine.Run;
end;

procedure TMainForm.CompileFile;
begin
  if Engine.Files.Current <> nil then
  begin
    SaveAllAct.Execute;
    Engine.CurrentTendency.Run([rnaCompile]);
  end;
end;

procedure TMainForm.DBGBreakpointsActExecute(Sender: TObject);
begin
  ShowBreakpointsForm;
end;

procedure TMainForm.CopyFileNameActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Clipboard.AsText := Engine.Files.Current.FileName;
end;

procedure TMainForm.DBGAddWatchActExecute(Sender: TObject);
var
  s: string;
begin
  with Engine.Files do
    if (Current <> nil) and (Current.SynEdit <> nil) then
    begin
      if not Current.SynEdit.SelAvail then
        s := Trim(Current.SynEdit.GetWordAtRowCol(Current.SynEdit.LogicalCaretXY))
      else
        s := Current.SynEdit.SelText;
      AddWatch(s);
      MessagesTabs.ActiveControl := WatchesGrid;
    end;
end;

procedure TMainForm.DeleteCurrentWatch;
begin
  if WatchesGrid.Row > 0 then
  begin
    DeleteWatch(WatchesGrid.Cells[1, WatchesGrid.Row]);
  end;
end;

procedure TMainForm.FolderHomeActExecute(Sender: TObject);
begin
  Folder := Engine.GetRoot;
end;

procedure TMainForm.StatusTimerTimer(Sender: TObject);
begin
  StatusTimer.Enabled := False;
  MessageLabel.Caption := '';
end;

procedure TMainForm.Clear1Click(Sender: TObject);
begin
  if MessagesPopup.PopupComponent is TSynEdit then
  begin
    (MessagesPopup.PopupComponent as TSynEdit).Lines.Clear;
    if (MessagesPopup.PopupComponent as TSynEdit) = OutputEdit then
      FOutputs.Clear;
    MessagesGrid.RowCount := 1;
  end
  else if MessagesPopup.PopupComponent is TListView then
    (MessagesPopup.PopupComponent as TListView).Clear
  else if MessagesPopup.PopupComponent is TStringGrid then
    (MessagesPopup.PopupComponent as TStringGrid).RowCount := 1;
end;

{type
  TSearchListItem = class(TListItem)
  private
    FColumn: Integer;
    FLength: Integer;
  public
    property Column: Integer read FColumn write FColumn;
    property Length: Integer read FLength write FLength;
  end; } //TODO delete it

procedure TMainForm.SearchFoundEvent(Index: Integer; FileName: string; const Line: string; LineNo, Column, FoundLength: Integer);
begin
  if Index = 0 then
  begin
    MessagesAct.Checked := True;
    MessagesTabs.ActiveControl := SearchGrid;
    UpdateMessagesPnl;
    SearchGrid.RowCount := 2;
  end
  else
    SearchGrid.RowCount := SearchGrid.RowCount + 1;
  SearchGrid.Cells[1, SearchGrid.RowCount - 1] := FileName;
  SearchGrid.Cells[2, SearchGrid.RowCount - 1] := IntToStr(LineNo) + ', ' + IntToStr(Column);

  SearchGrid.Cells[3, SearchGrid.RowCount - 1] := Line;

  SearchGrid.Rows[SearchGrid.RowCount - 1].Objects[0] := TObject(PtrInt(FoundLength));
  SearchGrid.Rows[SearchGrid.RowCount - 1].Objects[1] := TObject(PtrInt(LineNo));
  SearchGrid.Rows[SearchGrid.RowCount - 1].Objects[2] := TObject(PtrInt(Column));
end;

procedure TMainForm.FindInFilesActExecute(Sender: TObject);
var
  aText, aFolder: string;
begin
  with Engine.Files do
    if (Current <> nil) and (Current.SynEdit <> nil) then
    begin
      if Current.SynEdit.SelAvail and (Current.SynEdit.BlockBegin.y = Current.SynEdit.BlockEnd.y) then
        aText := Current.SynEdit.SelText
      else
        aText := Current.SynEdit.GetWordAtRowCol(Current.SynEdit.LogicalCaretXY);
    end
    else
      aText := '';

  aFolder := Engine.GetRoot;

  if aFolder = '' then
    aFolder := Folder;
  if ShowSearchInFilesForm(@SearchFoundEvent, aText, ExpandFileName(aFolder), Engine.Options.SearchFolderHistory, Engine.Options.SearchHistory, Engine.Options.ReplaceHistory) then
    if SearchGrid.CanFocus then
      SearchGrid.SetFocus;
end;

procedure TMainForm.NextMessageActExecute(Sender: TObject);
begin
  MoveListIndex(+1);
end;

procedure TMainForm.MoveListIndex(vDirection: Integer);

  procedure DblClick(Grid: TStringGrid);
  begin
    if Assigned(Grid.OnDblClick) then
    begin
      if vDirection > 0 then
      begin
        if Grid.Row < Grid.RowCount - 1 then
          Grid.Row := Grid.Row + 1;
      end
      else if vDirection < 0 then
      begin
        if Grid.Row > 0 then
          Grid.Row := Grid.Row - 1;
      end;
      Grid.OnDblClick(Grid);
    end
  end;

begin
  if not (MessagesTabs.ActiveControl is TStringGrid) then
  begin
    MessagesTabs.ActiveControl := MessagesGrid;
    vDirection := 0;
  end;
  DblClick(MessagesTabs.ActiveControl as TStringGrid);
end;

procedure TMainForm.ExploreFolder(AFolder: string);
begin
  if Engine.Files.Current <> nil then
  begin
    ExploreFile(Engine.Files.Current.FileName);
  end;
end;

procedure TMainForm.ExploreFile(AFileName: string);
var
  s: string;
begin
  s := '';
{$ifdef WINDOWS}
  //ShellExecute(0, 'open', 'explorer.exe', PChar('/select,"' + Engine.Files.Current.Name + '"'), nil, SW_SHOW);
  RunCommand('Explorer', ['/select,"' + AFileName + '"'], s);
{$else}
  RunCommand('xdg-open', [ExtractFilePath(AFileName)], s);
{$endif}
end;

procedure TMainForm.PriorMessageActExecute(Sender: TObject);
begin
  MoveListIndex(-1);
end;

procedure TMainForm.EngineState;
begin
  if Engine.MacroRecorder.State = msRecording then
  begin
    StatePnl.Caption := 'C';
    StatePnl.Color := Engine.Options.Profile.Attributes.Active.BackColor;
  end
  else if Engine.Files.Current <> nil then
  begin
    CursorPnl.Caption := Engine.Files.Current.GetGlance;
    if Engine.Files.Current.IsNew then
      StatePnl.Caption := 'N'
    else if Engine.Files.Current.IsReadOnly then
      StatePnl.Caption := 'R'
    else
      StatePnl.Caption := 'W';
    StatePnl.ParentColor := True;
  end
  else
    StatePnl.Caption := '-';
end;

procedure TMainForm.SCMCompareToActExecute(Sender: TObject);
var
  aDialog: TOpenDialog;
begin
  if Engine.Files.Current <> nil then
  begin
    aDialog := TOpenDialog.Create(nil);
    try
      aDialog.Title := 'Open file';
      aDialog.Options := aDialog.Options - [ofAllowMultiSelect];
      aDialog.Filter := Engine.Groups.CreateFilter(True, '', Engine.Files.Current.Group);
      //      aDialog.InitialDir := Engine.BrowseFolder;
      if aDialog.Execute then
      begin
        Engine.SCM.DiffToFile(aDialog.FileName, Engine.Files.Current.FileName);
      end;
      Engine.Files.CheckChanged;
    finally
      aDialog.Free;
    end;
  end;
end;

procedure TMainForm.SwitchFocusActExecute(Sender: TObject);
begin
  SwitchFocus;
end;

procedure TMainForm.SwitchFocus(Backword: Boolean);
var
  aList: specialize TFPGList<TWinControl>;
  i, cur: Integer;
begin
  aList := specialize TFPGList<TWinControl>.Create;
  try
    if (Engine.Files.Current <> nil) then
      Engine.Files.Current.EnumSwitchControls(aList);
    if BrowserPnl.Visible then
      aList.Add(BrowserTabs.ActivableControl);
    if MessagesPnl.Visible then
      aList.Add(MessagesTabs.ActivableControl);

    if aList.Count = 0 then
      exit;

    cur := 0;
    for i := 0 to aList.Count - 1 do
    begin
      if aList[i].Focused or (aList[i] = ActiveControl) then
      begin
        cur := i;
        break;
      end;
    end;

    if Backword then
    begin
      dec(cur);
      if cur < 0 then
        cur := aList.Count -1;
    end
    else
    begin
      inc(cur);
      if cur > aList.Count -1 then
        cur := 0;
    end;
    aList[cur].SetFocus;
  finally
    aList.Free;
  end;
end;

procedure TMainForm.WorkspaceMnuClick(Sender: TObject);
begin
  with TEditorSetupForm.Create(Application) do
  begin
    NeedToRestartLbl.Visible := True;
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.EngineReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
begin
  case MsgBox.Ask(Format('Replace this ocurrence of "%s" with "%s"?', [ASearch, AReplace]), [msgcYes, msgcNo, msgcAll, msgcCancel], msgcYes, msgcCancel, msgkConfirmation) of
    msgcYes: ReplaceAction := raReplace;
    msgcNo: ReplaceAction := raSkip;
    msgcAll: ReplaceAction := raReplaceAll;
    else //msgcCancel
      ReplaceAction := raCancel;
  end;
end;

procedure TMainForm.CatchErr(Sender: TObject; e: exception);
begin
  MsgBox.Error(e.Message);
end;

end.
