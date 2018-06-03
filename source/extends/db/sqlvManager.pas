unit sqlvManager;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

{todo: Save/Load sql scripts: DONE}
{todo: Auto complete: DONE}
{todo: search for members: DONE}
{todo: More short cuts: DONE}
{todo: Export/Import As CSV: DONE}
{todo: Ask for param when have params in normal execute sql script: DONE}

{todo: Assoiate with *.sqlite}
{todo: Extract the Meta of whale database}
{todo: Find and Replace}
{todo: Blob access as PNG or JPG}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  dateutils, LCLType, LCLIntf, Types, mncConnections, LCLProc,
  contnrs, ExtCtrls, StdCtrls, SynEdit, FileUtil, Buttons, Menus,
  sqlvSessions, mncCSV, mncSQL,
  SynCompletion, SynEditAutoComplete, SynHighlighterHashEntries,
  mnUtils, mncMetas, mncCSVExchanges,
  {$ifdef FIREBIRD}
  SynHighlighterFirebird,
  {$else SQLITE}
  mncSQLite,
  mnSynHighlighterStdSQL,
  {$endif}
  sqlvConsts, sqlvClasses, sqlvStdClasses, LMessages,
  EditorEngine;

type
  TsqlState = (sqlsRoot, sqlsSQL, sqlsResults, sqlsInfo, sqlsMeta);

  TControlObject = class(TObject)
  public
    Control: TControl;
    UseActive: Boolean;
    Reverse: Boolean;
  end;

  { TControlObjects }

  TControlObjects = class(TObjectList)
  private
    function GetItem(Index: Integer): TControlObject;
  public
    function Add(ControlObject: TControlObject): Integer;
    property Items[Index: Integer]: TControlObject read GetItem; default;
  end;

  TPanelObject = class(TObject)
  private
    FList: TControlObjects;
  public
    Control: TControl;
    constructor Create;
    destructor Destroy; override;
    property List: TControlObjects read FList;
    procedure Show(Active: Boolean);
    procedure Hide(Active: Boolean);
  end;

  TPanelsList = class(TObjectList)
  private
    function GetItem(Index: Integer): TPanelObject;
  public
    constructor Create;
    function Find(AControl: TControl): TPanelObject;
    procedure Add(AControl: TControl; ALinkControl: TControl = nil; UseActive: Boolean = False; Reverse: Boolean = False);
    procedure Show(AControl: TControl; Active: Boolean);
    procedure HideAll;
    property Items[Index: Integer]: TPanelObject read GetItem; default;
  end;

  { TsqlvManagerForm }

  TsqlvManagerForm = class(TFrame, IEditorControl)
    ActionsPanel: TPanel;
    CacheMetaChk1: TCheckBox;
    ExclusiveChk: TCheckBox;
    FileMnu: TMenuItem;
    ExitMnu: TMenuItem;
    BackBtn: TSpeedButton;
    GroupsList: TComboBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    HelpMnu: TMenuItem;
    ActionsPopupMenu: TPopupMenu;
    SaveMnu: TMenuItem;
    SaveAsMnu: TMenuItem;
    OpenMnu: TMenuItem;
    SQLSavepointBtn: TSpeedButton;
    ExecuteBtn: TSpeedButton;
    StopBtn: TButton;
    VacuumChk: TCheckBox;
    FirstBtn: TSpeedButton;
    ActionsList: TComboBox;
    InfoBtn: TButton;
    Label4: TLabel;
    FileNameLbl: TLabel;
    Label5: TLabel;
    OpenBtn: TButton;
    GroupsPanel: TPanel;
    RefreshBtn: TButton;
    CacheMetaChk: TCheckBox;
    RecentsCbo: TComboBox;
    Label3: TLabel;
    OpenDialog: TOpenDialog;
    ResultsBtn: TButton;
    SaveDialog: TSaveDialog;
    MetaBtn: TButton;
    SQLSaveAsBtn: TButton;
    SQLNewBtn: TButton;
    StartBtn: TButton;
    SQLBtn: TButton;
    SQLLoadBtn: TButton;
    InfoPanel: TPanel;
    InfoLbl: TLabel;
    FetchCountLbl: TLabel;
    FetchedLbl: TLabel;
    ResultEdit: TMemo;
    SQLBackwardBtn: TSpeedButton;
    ConnectBtn: TButton;
    DatabasesCbo: TComboBox;
    DisconnectBtn: TButton;
    AutoCreateChk: TCheckBox;
    SQLForwardBtn: TSpeedButton;
    MainMenu: TMainMenu;
    AboutMnu: TMenuItem;
    MembersGrid: TStringGrid;
    SQLSaveBtn: TButton;
    BrowseBtn: TButton;
    RemoveBtn: TButton;
    MetaLbl: TLabel;
    ToolsMnu: TMenuItem;
    DataPathCbo: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    GroupPanel: TPanel;
    ClientPanel: TPanel;
    TopPanel: TPanel;
    ResultPanel: TPanel;
    RootPanel: TPanel;
    SQLEdit: TSynEdit;
    SQLPanel: TPanel;
    DataGrid: TStringGrid;
    AnsiCodePageChk: TCheckBox;
    procedure ActionsListSelect(Sender: TObject);
    procedure BackBtnClick(Sender: TObject);
    procedure BrowseBtnClick(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure DatabasesCboDropDown(Sender: TObject);
    procedure DataPathCboDropDown(Sender: TObject);
    procedure DataPathCboExit(Sender: TObject);
    procedure DataPathCboKeyPress(Sender: TObject; var Key: char);
    procedure DisconnectBtnClick(Sender: TObject);
    procedure ExecuteBtnClick(Sender: TObject);
    procedure FirstBtnClick(Sender: TObject);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure GroupsListKeyPress(Sender: TObject; var Key: char);
    procedure GroupsListSelect(Sender: TObject);
    procedure HelpMnuClick(Sender: TObject);
    procedure InfoBtnClick(Sender: TObject);
    procedure MembersGridClick(Sender: TObject);
    procedure MembersGridDblClick(Sender: TObject);
    procedure MembersGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MembersGridKeyPress(Sender: TObject; var Key: char);
    procedure MembersGridUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure MenuItem3Click(Sender: TObject);
    procedure OpenMnuClick(Sender: TObject);
    procedure SaveMnuClick(Sender: TObject);
    procedure SaveAsMnuClick(Sender: TObject);
    procedure RecentsCboSelect(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure MetaBtnClick(Sender: TObject);
    procedure MembersListDblClick(Sender: TObject);
    procedure MembersListKeyPress(Sender: TObject; var Key: char);
    procedure ResultsBtnClick(Sender: TObject);
    procedure SQLBackwardBtnClick(Sender: TObject);
    procedure SQLBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure SQLNewBtnClick(Sender: TObject);
    procedure SQLPanelClick(Sender: TObject);
    procedure SQLSaveAsBtnClick(Sender: TObject);
    procedure SQLSavepointBtnClick(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure SQLForwardBtnClick(Sender: TObject);
    procedure SQLLoadBtnClick(Sender: TObject);
    procedure SQLSaveBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    FSearching: Boolean;
    FFirstSearch: Boolean;
    FSearch: UTF8String;
    FSearchTime: TDateTime;
    FLastSQLFile: string;
    FLockEnum: Boolean;
    {$ifdef FIREBIRD}
    FSQLSyn: TSynFirebirdSyn;
    {$else SQLITE}
    FSQLSyn: TmnSynStdSQLSyn;
    {$endif}
    Completion: TSynCompletion;
    FDataPath: string;
    procedure LoadSQLFile;
    procedure SaveAsSQLFile;
    procedure SaveLastSQLFile;
    procedure CheckSearch;
    procedure SearchFor(S: string);
    procedure Connect;
    procedure Disconnect;
    procedure FileFoundEvent(FileIterator: TFileIterator);
    procedure DirectoryFoundEvent(FileIterator: TFileIterator);
    procedure EnumDatabases;
    procedure SetDataPath(const AValue: string);
    procedure Connected;
    procedure Disconnected;
    procedure SessionStarted;
    procedure SessionStoped;
    procedure CollectAttributes(vAttributes: TsqlvAttributes);
  private
    FState: TsqlState;
    PanelsList: TPanelsList;
    FCancel: Boolean;
    procedure ClearGrid;
    procedure DoAddKeyword(AKeyword: string; AKind: integer);
    procedure Execute(ExecuteType: TsqlvExecuteType; SQLCMD: TmncSQLCommand; SQL:TStringList; ShowGrid:Boolean);
    procedure OnExecuteCompletion(Sender: TObject);
    procedure FillGrid(SQLCMD: TmncSQLCommand);
    procedure UpdateCompletion;
    function LogTime(Start: TDateTime): string;
    procedure RefreshSQLHistory(Sender: TObject);
    procedure SetLastSQLFile(const AValue: string);
    procedure SetState(const AValue: TsqlState);
    procedure StateChanged;
    function GetDatabaseName: string;
    function GetRealDataPath: string;
    procedure SetRealDataPath(FileName: string);
    property DataPath: string read FDataPath write SetDataPath;
  public
    Actions: TsqlvAddons;
    GroupsNames: TsqlvAddons;//Fields, Indexes
    CurrentGroup: TsqlvAddon;
    function GetMainControl: TWinControl;
    procedure ActionsMenuSelect(Sender: TObject);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddRecentSQL(Silent: Boolean = False);
    procedure ExecuteScript(ExecuteType: TsqlvExecuteType);
    procedure OpenAction(vAction: TsqlvAddon; aValue: string);
    procedure OpenMember(AValue: string);
    procedure OpenGroup(AValue: string);
    procedure LoadActions(vGroup: string; Append: Boolean = False);
    property State: TsqlState read FState write SetState;
    property LastSQLFile: string read FLastSQLFile write SetLastSQLFile;
  end;

  { TsqlvMainGui }

  TsqlvMainGui = class(TsqlvGui)
  protected
    MainForm: TsqlvManagerForm;
    procedure LoadMembers(vGroup: TsqlvAddon; vAttributes: TsqlvAttributes);
  public
    constructor Create(AMainForm: TsqlvManagerForm);
    procedure ExecuteScript(ExecuteType: TsqlvExecuteType); override;
    procedure LoadEditor(vAddon: TsqlvAddon; S: string); override;
    procedure ShowMeta(vAddon: TsqlvAddon; vSelectDefault: Boolean); override;
  end;

implementation

{$R *.lfm}

uses
  CSVOptionsForms, ParamsForms, SynEditMiscProcs;

{ TsqlvMainGui }

procedure TsqlvMainGui.ExecuteScript(ExecuteType: TsqlvExecuteType);
begin
  with MainForm do
    ExecuteScript(ExecuteType);
end;

procedure TsqlvMainGui.LoadEditor(vAddon: TsqlvAddon; S: string);
begin
  with MainForm do
  begin
    AddRecentSQL;
    SQLEdit.Lines.BeginUpdate;
    SQLEdit.Lines.Text := S;
    SQLEdit.Lines.EndUpdate;
    State := sqlsSQL;
    SQLEdit.SetFocus;
  end;
end;

procedure TsqlvMainGui.ShowMeta(vAddon: TsqlvAddon; vSelectDefault: Boolean);
var
  i, c: Integer;
  d: Integer;
  g: string;
  b: Boolean;
  aGroups: TsqlvAddons;
  aGroup: TsqlvAddon;
begin
  with MainForm do
  begin
    aGroups := TsqlvAddons.Create;
    try
      sqlvEngine.Enum(vAddon.Name, aGroups, sqlvEngine.DB.IsActive);

      g := sqlvEngine.Stack.Current.Select;
      aGroup := nil;
      d := -1;
      c := 0;
      b := false;
      GroupsList.Items.BeginUpdate;
      try
        GroupsList.Clear;
        GroupsNames.Clear;
        for i := 0 to aGroups.Count -1 do
        begin
          if not (nsCommand in aGroups[i].Style) then //Group in style
          begin
            GroupsList.Items.Add(aGroups[i].Title);
            GroupsNames.Add(aGroups[i]);
            //if (d < 0) then
            begin
              if (d < 0) then //select first one
              begin
                d := c;
                aGroup := aGroups[i];
              end;
              //b mean already selected and override privouse assigb
              if not b and (((g = '') and (nsDefault in aGroups[i].Style))) or (((g <>'') and SameText(g, aGroups[i].Name))) then
              begin
                d := c;
                aGroup := aGroups[i];
                b := true;
              end;
            end;
            c := c + 1;
          end;
        end;
      finally
        GroupsList.Items.EndUpdate;
      end;
      if d < 0 then
        d := 0;
      if aGroups.Count > 0 then
        GroupsList.ItemIndex := d;
    finally
      aGroups.Free;
    end;
    MetaLbl.Caption := vAddon.Title + ': ' + sqlvEngine.Stack.Current.Attributes[sqlvEngine.Stack.Current.Addon.Name];

    if aGroup <> nil then
      sqlvEngine.Stack.Current.Select := aGroup.Name;

    LoadMembers(aGroup, sqlvEngine.Stack.Current.Attributes); //if group is nil it must clear the member grid
    State := sqlsMeta;
  end;
end;

procedure TsqlvMainGui.LoadMembers(vGroup: TsqlvAddon; vAttributes: TsqlvAttributes);
var
  i, j, c: Integer;
  aHeader: TStringList;
  aCols: Integer;
  aItems: TmncMetaItems;
begin
  if vGroup = nil then
    MainForm.MembersGrid.Clear
  else
  begin
    aItems := TmncMetaItems.Create;
    try
      with MainForm do
      begin
        aHeader := TStringList.Create;
        try
          vGroup.EnumHeader(aHeader);
          MembersGrid.ColCount := aHeader.Count;
          for i := 0 to aHeader.Count -1 do
          begin
            MembersGrid.Cells[i, 0] := aHeader[i];
      //      if i = 0 then
      //        MembersGrid.Columns[i].SizePriority := 50;
          end;

          aCols := aHeader.Count;
        finally
          aHeader.Free;
        end;

        vGroup.EnumMeta(aItems, vAttributes);
        c := 0;
        MembersGrid.BeginUpdate;
        try
          MembersGrid.RowCount := 1;//fixed only
          for i := 0 to aItems.Count -1 do
          begin
            MembersGrid.RowCount := c + 2;
            for j := 0 to aCols - 1 do
            begin
              if j = 0 then
                MembersGrid.Cells[j, c + 1] := aItems[i].Name
              else if (j - 1) < aItems[i].Attributes.Count then //maybe Attributes not have all data
                MembersGrid.Cells[j, c + 1] := aItems[i].Attributes.Items[j - 1].Value; //TODO must be assigned my name not by index
            end;
            c := c + 1;
          end;
        finally
          MembersGrid.EndUpdate;
        end;
        MembersGrid.Row := 0;
        MembersGrid.AutoSizeColumns;
        CurrentGroup := nil; //reduce flicker when fill Actions
        State := sqlsMeta;
        CurrentGroup := vGroup;
        LoadActions(vGroup.ItemName);
      end;
    finally
      aItems.Free;
    end;
  end;
end;

constructor TsqlvMainGui.Create(AMainForm: TsqlvManagerForm);
begin
  inherited Create;
  MainForm := AMainForm;
end;

{ TsqlvManagerForm }

procedure TsqlvManagerForm.ConnectBtnClick(Sender: TObject);
begin
  Connect;
end;

procedure TsqlvManagerForm.DatabasesCboDropDown(Sender: TObject);
begin
  DataPath := DataPathCbo.Text;
end;

procedure TsqlvManagerForm.BrowseBtnClick(Sender: TObject);
begin
  OpenDialog.FileName := sFileNameFilter;
  OpenDialog.DefaultExt := sFileExtFilter;
  OpenDialog.Filter := sFileNameFilter;
  OpenDialog.InitialDir := DataPathCbo.Text;
  if OpenDialog.Execute then
  begin
    SetRealDataPath(OpenDialog.FileName);
  end;
end;

procedure TsqlvManagerForm.ActionsListSelect(Sender: TObject);
var
  aValue: string;
  aAddon: TsqlvAddon;
begin
  if ActionsList.ItemIndex >= 0 then
  begin
    try
      aAddon := Actions[ActionsList.ItemIndex];
      if State = sqlsMeta then
        aValue := MembersGrid.Cells[0, MembersGrid.Row]
      else
        aValue := '';
      OpenAction(aAddon, aValue);
    finally
      ActionsList.ItemIndex := -1;
    end;
  end;
end;

procedure TsqlvManagerForm.BackBtnClick(Sender: TObject);
begin
  if sqlvEngine.Stack.Count > 1 then
  begin
    sqlvEngine.Stack.Pop;
    if (sqlvEngine.Stack.Current <> nil) then
      sqlvEngine.Run(sqlvEngine.Stack);
  end;
end;

procedure TsqlvManagerForm.DataPathCboDropDown(Sender: TObject);
begin
  DataPath := DataPathCbo.Text;
end;

procedure TsqlvManagerForm.DataPathCboExit(Sender: TObject);
begin
  DataPath := DataPathCbo.Text;
end;

procedure TsqlvManagerForm.DataPathCboKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    if not sqlvEngine.DB.IsActive then
      Connect;
end;

procedure TsqlvManagerForm.DisconnectBtnClick(Sender: TObject);
begin
  Disconnect;
end;

procedure TsqlvManagerForm.ExecuteBtnClick(Sender: TObject);
begin
  ExecuteScript(execNormal);
end;

procedure TsqlvManagerForm.FirstBtnClick(Sender: TObject);
begin
  sqlvEngine.Stack.Clear;
  sqlvEngine.Stack.Push(TsqlvProcess.Create('Databases', 'Database', 'Tables', DatabasesCbo.Text));
  sqlvEngine.Run;
end;

procedure TsqlvManagerForm.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
var
  ShiftState: TShiftState;
begin
  ShiftState := KeyDataToShiftState(Msg.KeyData);
  case Msg.CharCode of
    VK_F7:
    begin
      State := sqlsInfo;
      Handled := True;
    end;
    VK_F8:
    begin
      if State = sqlsSQL then
      begin
        State := sqlsMeta;
        MembersGrid.SetFocus;
      end
      else
      begin
        State := sqlsSQL;
        SQLEdit.SetFocus;
      end;
      Handled := True;
    end;
    else
    begin
      case State of
        sqlsRoot:
          case Msg.CharCode of
            VK_F9:
            begin
              if ssShift in ShiftState then
                Disconnect
              else
                Connect;
              Handled := True;
            end;
          end;
        sqlsMeta:
          case Msg.CharCode of
            VK_F6:
            begin
              if MembersGrid.Focused then
                GroupsList.SetFocus
              else
                MembersGrid.SetFocus;
              Handled := True;
            end;
          end;
        sqlsResults:
          case Msg.CharCode of
            VK_F6:
            begin
              State := sqlsSQL;
              SQLEdit.SetFocus;
              Handled := True;
            end;
            VK_F9:
            begin
              if sqlvEngine.DB.IsActive then
              begin
                ExecuteScript(execNormal);
                Handled := True;
              end;
            end;
          end;
        sqlsInfo:
          case Msg.CharCode of
            VK_F6:
            begin
              State := sqlsSQL;
              SQLEdit.SetFocus;
              Handled := True;
            end;
            VK_F9:
            begin
              if sqlvEngine.DB.IsActive then
              begin
                ExecuteScript(execNormal);
                Handled := True;
              end;
            end;
          end;
        sqlsSQL:
          case Msg.CharCode of
            Ord('S'):
            begin
              if ssCtrl in ShiftState then
                SaveLastSQLFile;
            end;
            VK_F6:
            begin
              State := sqlsResults;
              DataGrid.SetFocus;
              Handled := True;
            end;
            VK_F9:
            begin
              if sqlvEngine.DB.IsActive then
              begin
                ExecuteScript(execNormal);
                Handled := True;
              end;
            end;
          end;
        end;
      end;
  end;
end;

procedure TsqlvManagerForm.GroupsListKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    MembersGrid.SetFocus;
end;

procedure TsqlvManagerForm.GroupsListSelect(Sender: TObject);
begin
  OpenGroup(GroupsNames[GroupsList.ItemIndex].Name);
end;

procedure TsqlvManagerForm.HelpMnuClick(Sender: TObject);
begin
  ResultEdit.Clear;
  ResultEdit.Lines.Add('===Help===');
  ResultEdit.Lines.Add('[Keys]');
  ResultEdit.Lines.Add('F5 Refresh Meta');
  ResultEdit.Lines.Add('F6 Switch between Result and SQL editor');
  ResultEdit.Lines.Add('F7 Switch to Info');
  ResultEdit.Lines.Add('F8 Switch to SQL editor');
  ResultEdit.Lines.Add('F9 to excecute sql sctipt');
  State := sqlsInfo;
end;

procedure TsqlvManagerForm.InfoBtnClick(Sender: TObject);
begin
  State := sqlsInfo;
end;

procedure TsqlvManagerForm.MembersGridClick(Sender: TObject);
begin
  if not FSearching then
    FSearch := '';
end;

procedure TsqlvManagerForm.MembersGridDblClick(Sender: TObject);
begin
  if (MembersGrid.RowCount > 1) and (MembersGrid.Row >= 1) then
    OpenMember(MembersGrid.Cells[0, MembersGrid.Row]);
end;

procedure TsqlvManagerForm.MembersGridKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TsqlvManagerForm.MembersGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F3:
    begin
      if FSearch <> '' then
        SearchFor(FSearch);
    end;
    VK_RETURN:
    begin
      OpenMember(MembersGrid.Cells[0, MembersGrid.Row]);
    end;
  end;
end;

procedure TsqlvManagerForm.MembersGridUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  CheckSearch;
  FSearch := FSearch + UTF8Key;
  SearchFor(FSearch);
end;

procedure TsqlvManagerForm.MenuItem3Click(Sender: TObject);
begin
  State := sqlsRoot;
end;

procedure TsqlvManagerForm.OpenMnuClick(Sender: TObject);
begin
  LoadSQLFile;
end;

procedure TsqlvManagerForm.SaveMnuClick(Sender: TObject);
begin
  SaveLastSQLFile;
end;

procedure TsqlvManagerForm.SaveAsMnuClick(Sender: TObject);
begin
  SaveAsSQLFile;
end;

procedure TsqlvManagerForm.RecentsCboSelect(Sender: TObject);
begin
  if RecentsCbo.ItemIndex >= 0 then
    SetRealDataPath(RecentsCbo.Text);
end;

procedure TsqlvManagerForm.RefreshBtnClick(Sender: TObject);
begin
  EnumDatabases;
end;

procedure TsqlvManagerForm.RemoveBtnClick(Sender: TObject);
var
  i: Integer;
begin
  i := sqlvEngine.Recents.IndexOf(RecentsCbo.Text);
  if i >= 0 then
  begin
    sqlvEngine.Recents.Delete(i);
    sqlvEngine.SaveRecents;
    RecentsCbo.Items.Assign(sqlvEngine.Recents);
  end;
end;

procedure TsqlvManagerForm.MetaBtnClick(Sender: TObject);
begin
  State := sqlsMeta;
end;

procedure TsqlvManagerForm.MembersListDblClick(Sender: TObject);
begin
  OpenMember(MembersGrid.Cells[0, MembersGrid.Row]);
end;

procedure TsqlvManagerForm.MembersListKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
  begin
    OpenMember(MembersGrid.Cells[0, MembersGrid.Row]);
    Key := #0;
  end;
end;

procedure TsqlvManagerForm.ResultsBtnClick(Sender: TObject);
begin
  State := sqlsResults;
end;

procedure TsqlvManagerForm.SQLForwardBtnClick(Sender: TObject);
begin
  sqlvEngine.SQLHistory.Forward;
  if sqlvEngine.SQLHistory.Current <> nil then
    SQLEdit.Lines.Text := sqlvEngine.SQLHistory.Current.Text;
end;

procedure TsqlvManagerForm.SQLBackwardBtnClick(Sender: TObject);
begin
  sqlvEngine.SQLHistory.Backward;
  if (sqlvEngine.SQLHistory.Current <> nil) then
    SQLEdit.Lines.Text := sqlvEngine.SQLHistory.Current.Text;
end;

procedure TsqlvManagerForm.OpenBtnClick(Sender: TObject);
begin
  OpenMember(MembersGrid.Cells[0, MembersGrid.Row]);
end;

procedure TsqlvManagerForm.SQLNewBtnClick(Sender: TObject);
begin
  AddRecentSQL;
  LastSQLFile := '';
  SQLEdit.Lines.Clear;
  SQLEdit.Modified := False;
  SQLEdit.SetFocus;
end;

procedure TsqlvManagerForm.SQLPanelClick(Sender: TObject);
begin

end;

procedure TsqlvManagerForm.SQLSaveAsBtnClick(Sender: TObject);
begin
  SaveAsSQLFile;
end;

procedure TsqlvManagerForm.SQLSavepointBtnClick(Sender: TObject);
begin
  AddRecentSQL;
end;

procedure TsqlvManagerForm.SQLBtnClick(Sender: TObject);
begin
  State := sqlsSQL;
end;

procedure TsqlvManagerForm.StartBtnClick(Sender: TObject);
begin
  State := sqlsRoot;
end;

procedure TsqlvManagerForm.SQLLoadBtnClick(Sender: TObject);
begin
  LoadSQLFile;
end;

procedure TsqlvManagerForm.SQLSaveBtnClick(Sender: TObject);
begin
  SaveLastSQLFile;
end;

procedure TsqlvManagerForm.StopBtnClick(Sender: TObject);
begin
  FCancel := True;
end;

procedure TsqlvManagerForm.LoadSQLFile;
begin
  State := sqlsSQL;
  OpenDialog.FileName := '*.sql';
  OpenDialog.DefaultExt := 'sql';
  OpenDialog.Filter := '*.sql';
  if OpenDialog.FileName = '' then
    OpenDialog.InitialDir := Application.Location;
  if OpenDialog.Execute then
  begin
    AddRecentSQL;
    LastSQLFile := OpenDialog.FileName;
    SQLEdit.Lines.LoadFromFile(LastSQLFile);
    SQLEdit.Modified := False;
  end;
end;

procedure TsqlvManagerForm.SaveAsSQLFile;
begin
  State := sqlsSQL;
  SaveDialog.FileName := LastSQLFile;
  SaveDialog.DefaultExt := 'sql';
  SAveDialog.Filter := '*.sql';
  SaveDialog.InitialDir := Application.Location;
  if SaveDialog.Execute then
  begin
    LastSQLFile := SaveDialog.FileName;
    SaveLastSQLFile;
  end;
end;

procedure TsqlvManagerForm.SaveLastSQLFile;
begin
  State := sqlsSQL;
  if LastSQLFile = '' then
  begin
    SaveAsSQLFile;
  end
  else if LastSQLFile <> '' then
  begin
    SQLEdit.Lines.SaveToFile(LastSQLFile);
    SQLEdit.Modified := False;
  end;
end;

procedure TsqlvManagerForm.CheckSearch;
begin
  if (FSearch = '') or (SecondsBetween(Now, FSearchTime) > 3) then
  begin
    FFirstSearch := True;
    FSearch := '';
  end;
end;

procedure TsqlvManagerForm.SearchFor(S: string);
var
  i: Integer;
  t: string;
  f: Integer;
begin
  f := ord(not FFirstSearch);
  for i := MembersGrid.Row + f to MembersGrid.RowCount -1 do
  begin
    t := LeftStr(MembersGrid.Cells[0, i], Length(S));
    if SameText(S, t) then
    //t := MembersGrid.Cells[0, i];
    //if Pos(s, t) > 0 then
    begin
      FSearching := True;
      try
        MembersGrid.Row := i;
      finally
        FSearching := False;
      end;
      break;
    end;
  end;
  FSearchTime := Now;
end;

procedure TsqlvManagerForm.Connect;
begin
  if not sqlvEngine.DB.IsActive then
  begin
    sqlvEngine.Setting.CacheMetas := CacheMetaChk.Checked;
    sqlvEngine.DB.Open(GetDatabaseName, AutoCreateChk.Checked, ExclusiveChk.Checked, VacuumChk.Checked);
    sqlvEngine.Stack.Clear;
    sqlvEngine.Stack.Push(TsqlvProcess.Create('Databases', 'Database', 'Tables', DatabasesCbo.Text));
    sqlvEngine.Run(sqlvEngine.Stack);
  end;
end;

procedure TsqlvManagerForm.Disconnect;
begin
  if sqlvEngine.DB.IsActive then
  begin
    sqlvEngine.DB.Close;
    StateChanged;
  end;
end;

procedure TsqlvManagerForm.FileFoundEvent(FileIterator: TFileIterator);
begin
  DatabasesCbo.Items.Add(FileIterator.FileInfo.Name);
end;

procedure TsqlvManagerForm.DirectoryFoundEvent(FileIterator: TFileIterator);
begin
  DataPathCbo.Items.Add(FileIterator.FileName);
end;

procedure TsqlvManagerForm.EnumDatabases;
var
  aFiles: TStringList;
  aFileSearcher: TFileSearcher;
  OldFile: string;
  aUpPath: string;
begin
  if not FLockEnum then
  begin
    FLockEnum := True;
    try
      aFileSearcher := TFileSearcher.Create;
      aFiles := TStringList.Create;
      OldFile := DatabasesCbo.Text;
      DataPathCbo.Items.BeginUpdate;
      DatabasesCbo.Items.BeginUpdate;
      try
        DatabasesCbo.Items.Clear;
        DataPathCbo.Items.Clear;
        aUpPath := ExpandFileName(IncludeTrailingPathDelimiter(FDataPath) + '..\');
        if aUpPath <> FDataPath then
          DataPathCbo.Items.Add(aUpPath);
        DataPathCbo.Items.Add(FDataPath);
        aFileSearcher.OnDirectoryFound := @DirectoryFoundEvent;
        aFileSearcher.OnFileFound := @FileFoundEvent;
        aFileSearcher.Search(GetRealDataPath, sFileNameFilter, False);
        DataPathCbo.Text := FDataPath;
        if DatabasesCbo.Items.IndexOf(OldFile) >=0 then
          DatabasesCbo.Text := OldFile;
      finally
        aFileSearcher.Free;
        aFiles.Free;
        DataPathCbo.Items.EndUpdate;
        DatabasesCbo.Items.EndUpdate;
        DataPathCbo.Refresh;
        DatabasesCbo.Refresh;
      end;
    finally
      FLockEnum := False;
    end;
  end;
end;

procedure TsqlvManagerForm.SetDataPath(const AValue: string);
begin
  if not FLockEnum then
    if FDataPath <> AValue then
    begin
      FDataPath := AValue;
      EnumDatabases;
    end;
end;

procedure TsqlvManagerForm.Connected;
begin
  UpdateCompletion;
  RecentsCbo.Items.Assign(sqlvEngine.Recents);
end;

procedure TsqlvManagerForm.Disconnected;
begin
  UpdateCompletion;
end;

procedure TsqlvManagerForm.SessionStarted;
begin

end;

procedure TsqlvManagerForm.SessionStoped;
begin

end;

procedure TsqlvManagerForm.CollectAttributes(vAttributes: TsqlvAttributes);
begin
  vAttributes.Clone(sqlvEngine.Stack.Current.Attributes);
  //vAttributes.Values[sqlvEngine.Stack.Current.Addon.Name] := sqlvEngine.Stack.Current.Value;
  vAttributes.Values[CurrentGroup.ItemName] := MembersGrid.Cells[0, MembersGrid.Row];
  DumpAttributes(vAttributes);
end;

procedure TsqlvManagerForm.SetState(const AValue: TsqlState);
begin
  if FState <> AValue then
  begin
    FState :=AValue;
    StateChanged;
  end;
end;

procedure TsqlvManagerForm.StateChanged;
begin
  case FState of
    sqlsRoot:
    begin
      PanelsList.Show(RootPanel, sqlvEngine.DB.IsActive);
      LoadActions('GUI.Root');
    end;
    sqlsSQL:
    begin
      PanelsList.Show(SQLPanel, sqlvEngine.DB.IsActive);
      LoadActions('GUI.SQL');
    end;
    sqlsResults:
    begin
      PanelsList.Show(ResultPanel, sqlvEngine.DB.IsActive);
      if Visible then
        DataGrid.SetFocus;
      LoadActions('GUI.Results');
    end;
    sqlsInfo:
    begin
      PanelsList.Show(InfoPanel, sqlvEngine.DB.IsActive);
      LoadActions('GUI.Info');
    end;
    sqlsMeta:
    begin
      PanelsList.Show(GroupPanel, sqlvEngine.DB.IsActive);
      if CurrentGroup <> nil then
        LoadActions(CurrentGroup.ItemName);
      if Visible then //prevent from setfocus non visible control
        MembersGrid.SetFocus;
    end;
  end;
end;

procedure TsqlvManagerForm.LoadActions(vGroup: string; Append: Boolean);
var
  i, c: Integer;
  aList: TsqlvAddons;
  aMenuItem: TMenuItem;
begin
  if (vGroup = '') or not Append then
  begin
    ActionsList.Items.Clear;
    ActionsPopupMenu.Items.Clear;
    FreeAndNil(Actions);
  end;
  if vGroup <> '' then
  begin
    Actions := TsqlvAddons.Create;
    aList := TsqlvAddons.Create;
    try
      sqlvEngine.Enum(vGroup, aList, sqlvEngine.DB.IsActive);
      c := 0;
      for i := 0 to aList.Count - 1 do
        if nsCommand in aList[i].Style then
        begin
          ActionsList.Items.Add(aList[i].Title);
          aMenuItem := TMenuItem.Create(ActionsPopupMenu);
          aMenuItem.Caption := aList[i].Title;
          aMenuItem.OnClick := @ActionsMenuSelect;
          aMenuItem.Tag := c;
          ActionsPopupMenu.Items.Add(aMenuItem);
          Actions.Add(aList[i]);
          Inc(c);
        end;
    finally
      aList.Free;
    end;
  end;
  ActionsPanel.Visible := ActionsList.Items.Count > 0;
  if ActionsPanel.Visible then
    MembersGrid.PopupMenu := ActionsPopupMenu
  else
    MembersGrid.PopupMenu := nil;
end;

function TsqlvManagerForm.GetDatabaseName: string;
begin
  Result := GetRealDataPath + DatabasesCbo.Text;
end;

function TsqlvManagerForm.GetRealDataPath: string;
begin
  Result := FDataPath;
  //Result := ExpandToPath('', FDataPath, Application.Location);
end;

procedure TsqlvManagerForm.SetRealDataPath(FileName: string);
var
  aPath: string;
  i: Integer;
begin
  aPath := ExtractFilePath(FileName);
  DataPathCbo.Text := aPath;
  DataPath := aPath;
  FileName := ExtractFileName(FileName);
  i:= DatabasesCbo.Items.IndexOf(FileName);
  if i >= 0 then
    DatabasesCbo.ItemIndex := i
  else
    DatabasesCbo.Text := FileName;
end;

function TsqlvManagerForm.GetMainControl: TWinControl;
begin
  Result := RootPanel;
end;

procedure TsqlvManagerForm.ActionsMenuSelect(Sender: TObject);
var
  aValue: string;
  aAddon: TsqlvAddon;
begin
  try
    aAddon := Actions[(Sender as TMenuItem).Tag];
    if State = sqlsMeta then
      aValue := MembersGrid.Cells[0, MembersGrid.Row]
    else
      aValue := '';
    OpenAction(aAddon, aValue);
  finally
    ActionsList.ItemIndex := -1;
  end;
end;

constructor TsqlvManagerForm.Create(TheOwner: TComponent);
var
  aFile: string;
begin
  inherited Create(TheOwner);
  GroupsNames := TsqlvAddons.Create;
  sqlvEngine.WorkPath := Application.Location;
  sqlvEngine.DB.OnConnected := @Connected;
  sqlvEngine.DB.OnDisconnected := @Disconnected;
  sqlvEngine.DB.OnSessionStarted := @SessionStarted;
  sqlvEngine.DB.OnSessionStoped := @SessionStoped;
  {$ifdef FIREBIRD}
  FSQLSyn := TSynFirebirdSyn.Create(Self);
  {$else}
  FSQLSyn := TmnSynStdSQLSyn.Create(Self);
  {$endif}
  SQLEdit.Highlighter := FSQLSyn;
  PanelsList := TPanelsList.Create;
  PanelsList.Add(RootPanel);
  PanelsList.Add(RootPanel, DisconnectBtn, True);
  PanelsList.Add(RootPanel, ConnectBtn, True, True);
  PanelsList.Add(RootPanel, SQLBtn, False);
  PanelsList.Add(RootPanel, MetaBtn, True);
  PanelsList.Add(SQLPanel);
  PanelsList.Add(SQLPanel, ResultsBtn, True);
  PanelsList.Add(SQLPanel, MetaBtn, True);
  PanelsList.Add(SQLPanel, InfoBtn, True);
  PanelsList.Add(ResultPanel);
  PanelsList.Add(ResultPanel, SQLBtn, False);
  PanelsList.Add(ResultPanel, InfoBtn, True);
  PanelsList.Add(ResultPanel, MetaBtn, True);
  PanelsList.Add(GroupPanel);
  PanelsList.Add(GroupPanel, OpenBtn, True);
  PanelsList.Add(GroupPanel, SQLBtn, False);
  PanelsList.Add(InfoPanel);
  PanelsList.Add(InfoPanel, ResultsBtn, True);
  PanelsList.Add(InfoPanel, SQLBtn, False);
  PanelsList.Add(InfoPanel, MetaBtn, True);

  sqlvGui := TsqlvMainGui.Create(Self);

  sqlvEngine.SQLHistory.OnChanged := @RefreshSQLHistory;
  sqlvEngine.LoadFile('recent.sql', SQLEdit.Lines);
  sqlvEngine.History.Changed;
  sqlvEngine.SQLHistory.Changed;
  if ParamCount > 0 then
  begin
    aFile := ParamStr(1);
    if FileExists(aFile) then
    begin
      if SameText(ExtractFileExt(aFile), '.sql') then
      begin
        LastSQLFile := aFile;
        SQLEdit.Lines.LoadFromFile(aFile);
        State := sqlsSQL;
      end
      else if SameText(ExtractFileExt(aFile), sFileNameFilter) then
      begin
        SetRealDataPath(aFile);
        Connect;
      end;
    end;
  end
  else
  begin
    if sqlvEngine.Recents.Count > 0 then
      SetRealDataPath(sqlvEngine.Recents[0]);
  end;
  RecentsCbo.Items.Assign(sqlvEngine.Recents);
  StateChanged;
  UpdateCompletion;
end;

destructor TsqlvManagerForm.Destroy;
begin
  sqlvEngine.DB.Close;
  FreeAndNil(GroupsNames);
  FreeAndNil(PanelsList);
  FreeAndNil(sqlvGui);
  inherited Destroy;
end;

{ TPanelsList }

constructor TPanelsList.Create;
begin
  inherited Create(True);
end;

function TPanelsList.Find(AControl: TControl): TPanelObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
  begin
    if (Items[i].Control = AControl) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TPanelsList.GetItem(Index: Integer): TPanelObject;
begin
  Result := inherited items[Index] as TPanelObject;
end;

procedure TPanelsList.HideAll;
begin
  Show(nil, False);
end;

procedure TPanelsList.Show(AControl: TControl; Active: Boolean);
var
  i: Integer;
  aPanel : TPanelObject;
begin
  aPanel := nil;
  for i := 0 to Count -1 do
  begin
    if (Items[i].Control = AControl) then
      aPanel := Items[i]
    else
    begin
      Items[i].Hide(Active);
    end;
  end;
  if aPanel <> nil then
  begin
    aPanel.Show(Active);
  end;
end;

procedure TPanelsList.Add(AControl: TControl; ALinkControl: TControl; UseActive: Boolean; Reverse: Boolean = False);
var
  aPanel: TPanelObject;
  aLink: TControlObject;
begin
  if ALinkControl <> nil then
  begin
    aPanel := Find(AControl);
    if aPanel = nil then
      raise Exception.Create('Control not found in the list');
    aLink := TControlObject.Create;
    aLink.UseActive := UseActive;
    aLink.Reverse := Reverse;
    aLink.Control := ALinkControl;
    aPanel.List.Add(aLink);
  end
  else
  begin
    aPanel := TPanelObject.Create;
    aPanel.Control := AControl;
    inherited Add(aPanel);
  end;
end;

{ TPanelObject }

constructor TPanelObject.Create;
begin
  FList := TControlObjects.Create(True);
end;

destructor TPanelObject.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TPanelObject.Show(Active: Boolean);
var
  i: Integer;
begin
  Control.Visible := True;
  Control.Align := alClient;
  for i := 0 to List.Count -1 do
    List[i].Control.Visible := not List[i].UseActive or ((Active and not List[i].Reverse) or (not Active and List[i].Reverse));
end;

procedure TPanelObject.Hide(Active: Boolean);
var
  i: Integer;
begin
  Control.Visible := False;
  for i := 0 to List.Count -1 do
    List[i].Control.Visible := False;
end;

{ TControlObjects }

function TControlObjects.GetItem(Index: Integer): TControlObject;
begin
  Result := inherited items[Index] as TControlObject;
end;

function TControlObjects.Add(ControlObject: TControlObject): Integer;
begin
  Result := inherited Add(ControlObject);
end;

procedure TsqlvManagerForm.OpenMember(AValue: string);
var
  a: TsqlvAttributes;
begin
  a := TsqlvAttributes.Create;
  try
    {$ifdef DEBUG}
    DebugLn('CurrentGroup.Name='+CurrentGroup.Name);
    DebugLn('CurrentGroup.ItemName='+CurrentGroup.ItemName);
    DebugLn('AValue='+AValue);
    {$endif}
    CollectAttributes(a);
    with sqlvEngine.Stack do
      if Current.Addon <> nil then
      begin
        TsqlvProcess.Create(CurrentGroup.Name, CurrentGroup.ItemName, AValue, a);
        sqlvEngine.Run;
      end;
        //what if Addon <> nil or what if Current.Addon.Item = ''
  finally
    a.Free;
  end;
end;

procedure TsqlvManagerForm.OpenGroup(AValue: string);
var
  a: TsqlvAttributes;
begin
  a := TsqlvAttributes.Create;
  try
    {$ifdef DEBUG}
    DebugLn('CurrentGroup.Name='+CurrentGroup.Name);
    DebugLn('CurrentGroup.ItemName='+CurrentGroup.ItemName);
    DebugLn('OpenGroup.AValue='+AValue);
    {$endif}
    //CollectAttributes(a);
    with sqlvEngine.Stack do
      if (Current <> nil) and (GroupsList.Items.Count > 0) and (GroupsList.ItemIndex >=0) then
      begin
        sqlvEngine.Stack.Push(TsqlvProcess.Create(Current.Addon, AValue, Current.Attributes));
        sqlvEngine.Run;
      end;
  finally
    a.Free;
  end;
end;

procedure TsqlvManagerForm.RefreshSQLHistory(Sender: TObject);
begin
  SQLForwardBtn.Enabled := sqlvEngine.SQLHistory.HaveForward;
  SQLBackwardBtn.Enabled := sqlvEngine.SQLHistory.HaveBackward;
end;

procedure TsqlvManagerForm.SetLastSQLFile(const AValue: string);
begin
  FLastSQLFile := AValue;
  FileNameLbl.Caption := AValue;
end;

procedure TsqlvManagerForm.AddRecentSQL(Silent: Boolean);
begin
  sqlvEngine.SQLHistory.Add(SQLEdit.Text, Silent);
end;

procedure TsqlvManagerForm.Execute(ExecuteType: TsqlvExecuteType; SQLCMD: TmncSQLCommand; SQL:TStringList; ShowGrid:Boolean);
var
  t: TDateTime;
  aExport: TmncCSVExport;
  aImport: TmncCSVImport;
  aStream : TFileStream;
begin
  try
    ResultEdit.Lines.Add('========= Execute ==========');
    SQLCMD.SQL.Assign(SQL);
    try
      case ExecuteType of
        execNormal:
        begin
          SQLCMD.Parse;
          { FIREBIRD }
          t := NOW;
          SQLCMD.Prepare;
          ResultEdit.Lines.Add('Prepare time: ' + LogTime(t));
          if (SQLCMD.Params.Count > 0) then
            if not ShowSQLParams(SQLCMD) then
            begin
              SQLCMD.Active := False;
//              SQLCMD.Close;//parse not open the command
              ResultEdit.Lines.Add('Canceled by user');
              exit;
            end;
          { SQLITE
          t := NOW;
          SQLCMD.Prepare;
          ResultEdit.Lines.Add('Prepare time: ' + LogTime(t));}
          t := NOW;
          Screen.Cursor := crHourGlass;
          SQLCMD.Execute;
          Screen.Cursor := crDefault;
          ResultEdit.Lines.Add('Execute time: ' + LogTime(t));
          if ShowGrid then
          begin
            if not SQLCMD.Done then
            begin
              State := sqlsResults;
              Application.ProcessMessages;
              DataGrid.SetFocus;
              t := NOW;
              if SQLCMD.Done then
                ClearGrid
              else
                FillGrid(SQLCMD);
              ResultEdit.Lines.Add('Fetch time: ' + LogTime(t));
            end
            else
            begin
              ClearGrid;
              State := sqlsInfo;
            end;
          end;
        end;
        execExport:
        begin
          aExport := TmncCSVExport.Create;
          try
            SaveDialog.FileName := '*.csv';
            SaveDialog.DefaultExt := 'csv';
            SaveDialog.Filter := '*.csv';
            if SaveDialog.Execute and ShowCSVOptions('Export CSV', aExport.CSVOptions) then
            begin
              aStream := TFileStream.Create(SaveDialog.FileName, fmCreate);
              try
                aExport.Command := SQLCMD;
                aExport.Stream := aStream;
                Screen.Cursor := crHourGlass;
                t := NOW;
                aExport.Execute;
                ResultEdit.Lines.Add('Export time: ' + LogTime(t));
                ResultEdit.Lines.Add('Export count: ' + IntToStr(aExport.Count));
                Screen.Cursor := crDefault;
                State := sqlsInfo;
                ShowMessage('Export count: '+IntToStr(aExport.Count));
              finally
                aStream.Free;
              end;
            end;
          finally
            aExport.Free;
          end;
        end;
        execImport:
        begin
          OpenDialog.FileName := '*.csv';
          OpenDialog.DefaultExt := 'csv';
          OpenDialog.Filter := '*.csv';
          if OpenDialog.Execute then
          begin
            SQLCMD.Prepare;
            if (SQLCMD.Params.Count = 0) then
            begin
              ShowMessage('SQL statment must have params for import');
              exit;
            end;
            aImport := TmncCSVImport.Create;
            try
              if ShowCSVOptions('Import CSV', aImport.CSVOptions) then
              begin
                aStream := TFileStream.Create(OpenDialog.FileName, fmOpenRead or fmShareDenyWrite);
                try
                  aImport.Command := SQLCMD;
                  aImport.Stream := aStream;
                  Screen.Cursor := crHourGlass;
                  t := NOW;
                  aImport.Execute;
                  ResultEdit.Lines.Add('Import time: ' + LogTime(t));
                  ResultEdit.Lines.Add('Import count: ' + IntToStr(aImport.Count));
                  Screen.Cursor := crDefault;
                  State := sqlsInfo;
                  ShowMessage('Import count: '+ IntToStr(aImport.Count));
                finally
                  aStream.Free;
                end;
              end;
            finally
              aImport.Free;
            end;
          end;
        end;
      end;
      //TODO enable it later
      {if SQLCMD is TmncSQLiteCommand then
      begin
        ResultEdit.Lines.Add('Last Row ID: ' + IntToStr(SQLCMD.GetLastInsertID));
        ResultEdit.Lines.Add('Rows affected: ' + IntToStr(SQLCMD.GetRowsChanged));
      end;}
    except
      if ShowGrid then
        ClearGrid;
      raise;
    end;
  finally
  end;
end;

procedure TsqlvManagerForm.OnExecuteCompletion(Sender: TObject);
begin
end;

procedure TsqlvManagerForm.ExecuteScript(ExecuteType: TsqlvExecuteType);
var
  aStrings: TStringList;
  i: Integer;
  SQLCMD: TmncSQLCommand;
  aStart: Integer;
begin
  sqlvEngine.SaveFile('recent.sql', SQLEdit.Lines);
  SQLCMD := sqlvEngine.DB.Session.CreateCommand;
  aStrings := TStringList.Create;
  try
    SqlBtn.Enabled := False;
    StopBtn.Visible := True;
    try
      ResultEdit.Clear;
      AddRecentSQL;
      aStart := 0;
      for i := 0 to SQLEdit.Lines.Count - 1 do
      begin
        if Trim(SQLEdit.Lines[i]) = '^' then
        begin
          Execute(ExecuteType, SQLCMD, aStrings, False);
          aStrings.Clear;
          aStart := i + 1;//+1 to skip terminator
        end
        else
          aStrings.Add(SQLEdit.Lines[i]);
      end;
      if aStrings.Count > 0 then
        Execute(ExecuteType, SQLCMD, aStrings, True);
      SQLCMD.Session.Commit(True);
    except
      on E: Exception do
      begin
        SQLCMD.Session.Rollback(False);
        ResultEdit.Lines.Add(E.Message);
        SQLEdit.CaretX := 0;
        SQLEdit.CaretY := aStart + 1;//CaretY start from 1
        raise;
      end
    else
      raise;
    end;
  finally
    ResultEdit.Lines.Add('');
    Screen.Cursor := crDefault;
    SqlBtn.Enabled := True;
    StopBtn.Visible := False;
    aStrings.Free;
    SQLCMD.Free;
    sqlvEngine.DB.Session.Active := True;
  end;
end;

procedure TsqlvManagerForm.OpenAction(vAction: TsqlvAddon; aValue: string);
var
  a: TsqlvAttributes;
begin
  a := TsqlvAttributes.Create;
  try
    {$ifdef DEBUG}
    DebugLn('CurrentGroup.Name='+CurrentGroup.Name);
    DebugLn('CurrentGroup.ItemName='+CurrentGroup.ItemName);
    DebugLn('AValue=' + AValue);
    {$endif}
    CollectAttributes(a);
    if vAction <> nil then
      vAction.Execute(a);
  finally
    a.Free;
  end;
end;

procedure TsqlvManagerForm.ClearGrid;
begin
  DataGrid.FixedCols := 1;
  DataGrid.FixedRows := 1;
  DataGrid.ColCount := 2;
  DataGrid.RowCount := 2;
  DataGrid.ColWidths[0] := 20;
  DataGrid.Cells[0, 1] := '';
  DataGrid.Cells[1, 0] := '';
  DataGrid.Cells[1, 1] := '';
end;

procedure TsqlvManagerForm.FillGrid(SQLCMD: TmncSQLCommand);

  function GetTextWidth(Text: string): Integer;
  begin
    DataGrid.Canvas.Font := DataGrid.Font;
    Result := DataGrid.Canvas.TextWidth(Text);
  end;

  function GetCharWidth: Integer;
  begin
    Result := (GetTextWidth('Wi') div 2);
  end;
var
  i, z, c, cw, tw, w: Integer;
  s: string;
  str: utf8string;
begin
  FCancel := False;
  //DataGrid.BeginUpdate;
  try
    DataGrid.ColCount := SQLCMD.Columns.Count + 1;
    DataGrid.FixedCols := 1;
    DataGrid.FixedRows := 1;
    DataGrid.RowCount := 1;
    DataGrid.ColWidths[0] := 24;
    DataGrid.Row := 1;
    DataGrid.Col := 1;
    DataGrid.Cells[0, 0] := '';
    cw := GetCharWidth; //must calc from canvas
    for i := 1 to DataGrid.ColCount - 1 do
    begin
      s := SQLCMD.Columns[i - 1].Name;
      z := 10;//SQLCMD.Fields[i - 1].Size;
      if z < 4 then
        z := 4
      else if z > 20 then
        z := 20;
      w := z * cw;
      tw := GetTextWidth(s) + 12;
      if tw > w then
        w := tw;
      tw := GetTextWidth(SQLCMD.Fields.Items[i - 1].AsString) + 12;
      if tw > w then
        w := tw;
      DataGrid.ColWidths[i] := w;
      DataGrid.Cells[i, 0] := s;
    end;
    Application.ProcessMessages;

    c := 1;
    while not SQLCMD.Done do
    begin
      DataGrid.RowCount := c + 1;
      DataGrid.Cells[0, c] := IntToStr(c);
      for i := 1 to DataGrid.ColCount - 1 do
      begin
        str := SQLCMD.Fields.Items[i - 1].AsString;
        DataGrid.Cells[i, c] := str;
      end;
      Inc(c);
      //before 100 rows will see the grid row by row filled, cheeting the eyes of user
      if (c < 100) or (Frac(c / 100) = 0) then
      begin
        FetchCountLbl.Caption := IntToStr(c - 1);
        Application.ProcessMessages;
      end;
      if FCancel then
      begin
        SQLCMD.Close;
        break;
      end;
      SQLCMD.Next;
    end;
    w := GetTextWidth(IntToStr(c)) + 12;
    if w < 24 then
      w := 24;
    DataGrid.ColWidths[0] := w;
    FetchCountLbl.Caption := IntToStr(c - 1);
  finally
    Application.ProcessMessages;
    //DataGrid.EndUpdate;
  end;
end;

function TsqlvManagerForm.LogTime(Start: TDateTime): string;
var
  ms, m, s: Cardinal;
begin
  ms := MilliSecondsBetween(Now, Start);
  s := (ms div 1000);
  ms := (ms mod 1000);
  m := (s div 60);
  s := (s mod 60);
  Result := Format('%d:%d:%d', [m, s, ms]);
end;

procedure TsqlvManagerForm.DoAddKeyword(AKeyword: string; AKind: integer);
begin
  AKeyword := LowerCase(AKeyword);
  Completion.ItemList.Add(AKeyword);
end;

procedure TsqlvManagerForm.UpdateCompletion;
  procedure FillNow(Name: string; MetaItems: TmncMetaItems);
  var
    i: Integer;
  begin
    for i := 0 to MetaItems.Count - 1 do
      Completion.ItemList.Add(MetaItems[i].Name);
  end;
begin
  if Completion = nil then
  begin
    Completion := TSynCompletion.Create(nil);
    Completion.Width := 340;
    Completion.EndOfTokenChr := '{}()[].<>/\:!$&*+-=%;';
    Completion.ShortCut := scCtrl + VK_SPACE;
    Completion.CaseSensitive := False;

    Completion.OnExecute := @OnExecuteCompletion;
    Completion.AddEditor(SQLEdit);
    //Completion.TheForm.Font.Assign(SQLEdit.Font);
  end
  else
    Completion.ItemList.Clear;
  {$ifdef FIREBIRD}
  EnumerateKeywords(Ord(tkDatatype), FirebirdTypes, SQLEdit.IdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), FirebirdFunctions, SQLEdit.IdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), FirebirdKeywords, SQLEdit.IdentChars, @DoAddKeyword);
  {$else}
  EnumerateKeywords(Ord(tkDatatype), StdSQLTypes, SQLEdit.IdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), StdSQLFunctions, SQLEdit.IdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), StdSQLKeywords, SQLEdit.IdentChars, @DoAddKeyword);
  {$endif}
  if sqlvEngine.DB.IsActive then
  begin
    FillNow('Table', sqlvEngine.DB.Tables);
    FillNow('Fields', sqlvEngine.DB.Fields);
    FillNow('View', sqlvEngine.DB.Views);
  end;
  //FillNow('Procedure', (Owner as TfbvSession).Proceduers);
  //FillNow('Triggers', (Owner as TfbvSession).Triggers);
  //FillNow('Functions', (Owner as TfbvSession).Functions);
  //FillNow('Exceptions', (Owner as TfbvSession).Exceptions);
end;

end.
