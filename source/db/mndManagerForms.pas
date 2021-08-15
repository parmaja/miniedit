unit mndManagerForms;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

{todo: Ask for param when have params in normal execute sql script}
{todo: Extract the Meta of whale database}
{todo: Blob access as PNG or JPG}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  dateutils, LCLType, LCLIntf, Types, mncConnections, LCLProc, contnrs,
  ExtCtrls, StdCtrls, SynEdit, FileUtil, Buttons, Menus, mncCSV,
  mncSQL, SynCompletion, SynEditAutoComplete, SynHighlighterHashEntries,
  mnUtils, mncMeta, mncCSVExchanges, mnSynHighlighterStdSQL, mncMySQL,
  mncPostgre, mncSQLite, mncSQLiteMeta, mncPGMeta, mncFBMeta, ntvGrids,
  ntvPanels, ntvImgBtns, mndEngines, mndStdAddons, LMessages, ComCtrls,
  EditorClasses, EditorEngine, mneResources;

type

  { TDBManagerForm }

  TDBManagerForm = class(TFrame, INotifyEngine, INotifyEngineSetting, ImndNotify)
    DatabaseImage: TntvImgBtn;
    DatabaseImage1: TntvImgBtn;
    DatabaseLbl1: TLabel;
    Panel3: TPanel;
    ServerImage: TntvImgBtn;
    DatabaseLbl: TLabel;
    ServerLbl: TLabel;
    DatabaseMenu: TPopupMenu;
    BackBtn: TButton;
    CacheMetaChk1: TCheckBox;
    Edit1: TEdit;
    DatabasesList: TListView;
    MembersGrid: TntvGrid;
    OpenBtn: TButton;
    FileMnu: TMenuItem;
    ExitMnu: TMenuItem;
    FirstBtn: TButton;
    GroupsList: TComboBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    HelpMnu: TMenuItem;
    ActionsPopupMenu: TPopupMenu;
    DatabasesPnl: TntvPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    SaveMnu: TMenuItem;
    SaveAsMnu: TMenuItem;
    OpenMnu: TMenuItem;
    GroupsPanel: TPanel;
    AboutMnu: TMenuItem;
    Timer1: TTimer;
    ToolsMnu: TMenuItem;
    GroupPanel: TPanel;
    procedure BackBtnClick(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure DatabasesListDblClick(Sender: TObject);
    procedure DisconnectBtnClick(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
    procedure FirstBtnClick(Sender: TObject);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure GroupsListKeyPress(Sender: TObject; var Key: char);
    procedure GroupsListSelect(Sender: TObject);
    procedure MembersGridDblClick(Sender: TObject);
    procedure MembersGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MembersGridUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure MenuItem1Click(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure MembersListDblClick(Sender: TObject);
    procedure MembersListKeyPress(Sender: TObject; var Key: char);
    procedure OpenBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FSearching: Boolean;
    FFirstSearch: Boolean;
    FSearch: UTF8String;
    FSearchTime: TDateTime;
    procedure CheckSearch;
    procedure SearchFor(S: string);
    procedure ApplyFilter;
    procedure CollectMetaItems(vMetaItems: TmncMetaItems);
  private
    function LogTime(Start: TDateTime): string;
    procedure StateChanged;
    procedure LoadMembers(vGroup: TmndAddon; vMetaItems: TmncMetaItems);
  public
    Actions: TmndAddons;
    GroupsNames: TmndAddons;//Fields, Indexes
    CurrentGroup: TmndAddon;
    procedure ActionsMenuSelect(Sender: TObject);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenAction(vAction: TmndAddon; aValue: string);
    procedure OpenMember(AValue: string);
    procedure OpenGroup(AValue: string);
    procedure LoadActions(vGroup: string; Append: Boolean = False);

    procedure SaveOptions;
    procedure LoadOptions;

    procedure ServerChanged;
    procedure DatabaseChanged;
    procedure ShowMeta(vAddon: TmndAddon; vSelectDefault: Boolean);
    procedure ShowEditor(vAddon: TmndAddon; S: string);
  end;

implementation

{$R *.lfm}

uses
  CSVOptionsForms, ParamsForms, SynEditMiscProcs;

{ TDBManagerForm }

procedure TDBManagerForm.ShowMeta(vAddon: TmndAddon; vSelectDefault: Boolean);
var
  i, c: Integer;
  d: Integer;
  g: string;
  b: Boolean;
  aGroups: TmndAddons;
  aGroup: TmndAddon;
  MetaName: string;
begin
  DatabaseLbl.Caption := DBEngine.DB.Connection.Resource;
  MetaName := vAddon.Title + ': ' + DBEngine.Stack.Current.MetaItems.Values[DBEngine.Stack.Current.DisplayName];
  aGroups := TmndAddons.Create;
  try
    DBEngine.Enum(vAddon.Name, aGroups, DBEngine.DB.IsActive);

    g := DBEngine.Stack.Current.Select;
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

  if aGroup <> nil then
    DBEngine.Stack.Current.Select := aGroup.Name;

  LoadMembers(aGroup, DBEngine.Stack.Current.MetaItems); //if group is nil it must clear the member grid
end;

procedure TDBManagerForm.ShowEditor(vAddon: TmndAddon; S: string);
begin
  Engine.BeginUpdate;
  try
    with Engine.Files.New('sql') do
    begin
      IsTemporary := True;
      SynEdit.Lines.Text := S;
    end;
  finally
    Engine.EndUpdate;
  end;
end;

procedure TDBManagerForm.LoadMembers(vGroup: TmndAddon; vMetaItems: TmncMetaItems);
var
  i, j: Integer;
  aItems: TmncMetaItems;
begin
  MembersGrid.Clear;
  if vGroup = nil then
  begin
    MembersGrid.ColumnsCount := 1;
    MembersGrid.Columns[0].Title := '';
    MembersGrid.Capacity := 1;
    MembersGrid.Count := 1;
  end
  else
  begin
    aItems := TmncMetaItems.Create;
    try
      MembersGrid.BeginUpdate;
      MembersGrid.Reset;
      try
        vGroup.EnumMeta(aItems, vMetaItems);

        MembersGrid.ColumnsCount := aItems.Header.Count + 1;
        MembersGrid.Columns[0].Title := 'Name';
        for i := 1 to MembersGrid.ColumnsCount -1 do
        begin
          MembersGrid.Columns[i].Title := aItems.Header.Items[i - 1].Value;
        end;
        if MembersGrid.ColumnsCount > 0 then
          MembersGrid.Columns[0].AutoFit := True;

        MembersGrid.Capacity := aItems.Count;
        MembersGrid.Count := aItems.Count;
        for i := 0 to aItems.Count -1 do
        begin
          for j := 0 to MembersGrid.ColumnsCount - 1 do
          begin
            if j = 0 then
              MembersGrid.Values[j, i] := aItems[i].Name
            else if (j - 1) < aItems[i].Attributes.Count then //maybe Attributes not have all data
              MembersGrid.Values[j, i] := aItems[i].Attributes.Items[j - 1].Value; //TODO must be assigned my name not by index
          end;
        end;
        MembersGrid.Current.Row := 0;
        CurrentGroup := nil; //reduce flicker when fill Actions
        CurrentGroup := vGroup;
        LoadActions(vGroup.ItemName);
      finally
        MembersGrid.EndUpdate;
      end;
    finally
      aItems.Free;
    end;
  end;
end;

procedure TDBManagerForm.BackBtnClick(Sender: TObject);
begin
  if DBEngine.Stack.Count > 1 then
  begin
    DBEngine.Stack.Pop;
    if (DBEngine.Stack.Current <> nil) then
      DBEngine.Run(DBEngine.Stack);
  end;
end;

procedure TDBManagerForm.ConnectBtnClick(Sender: TObject);
begin
end;

procedure TDBManagerForm.DatabasesListDblClick(Sender: TObject);
var
  aDatabase: string;
begin
  if (DatabasesList.Selected <> nil) then
  begin
    aDatabase := DatabasesList.Selected.Caption;
    DBEngine.OpenDatabase(aDatabase, DBEngine.Server.Engine.Name, DBEngine.Server.Info.Host, DBEngine.Server.Info.Port, DBEngine.Server.Info.UserName, DBEngine.Server.Info.Password, DBEngine.Server.Info.Role);
  end;
end;

procedure TDBManagerForm.DisconnectBtnClick(Sender: TObject);
begin
end;

procedure TDBManagerForm.Edit1KeyPress(Sender: TObject; var Key: char);
begin
  Timer1.Enabled := True;
end;

procedure TDBManagerForm.FirstBtnClick(Sender: TObject);
begin
  DBEngine.Stack.Top;
  //DBEngine.Stack.Clear;
  //DBEngine.Stack.Push(TmndProcess.Create('Databases', 'Database', 'Tables', DatabasesCbo.Text));
  DBEngine.Run;
end;

procedure TDBManagerForm.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
begin
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
end;

procedure TDBManagerForm.GroupsListKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    MembersGrid.SetFocus;
end;

procedure TDBManagerForm.GroupsListSelect(Sender: TObject);
begin
  OpenGroup(GroupsNames[GroupsList.ItemIndex].Name);
end;

procedure TDBManagerForm.MembersGridDblClick(Sender: TObject);
begin
  if (MembersGrid.Count > 0) and (MembersGrid.Current.Row >= 0) then
    OpenMember(MembersGrid.Values[0, MembersGrid.Current.Row]);
end;

procedure TDBManagerForm.MembersGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F3:
    begin
      if FSearch <> '' then
        SearchFor(FSearch);
    end;
    VK_RETURN:
    begin
      OpenMember(MembersGrid.Values[0, MembersGrid.Current.Row]);
    end;
  end;
end;

procedure TDBManagerForm.MembersGridUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  CheckSearch;
  FSearch := FSearch + UTF8Key;
  SearchFor(FSearch);
end;

procedure TDBManagerForm.MenuItem1Click(Sender: TObject);
begin
  DatabasesListDblClick(Sender);
end;

procedure TDBManagerForm.RefreshBtnClick(Sender: TObject);
begin

end;

procedure TDBManagerForm.MembersListDblClick(Sender: TObject);
begin
  OpenMember(MembersGrid.Values[0, MembersGrid.Current.Row]);
end;

procedure TDBManagerForm.MembersListKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
  begin
    OpenMember(MembersGrid.Values[0, MembersGrid.Current.Row]);
    Key := #0;
  end;
end;

procedure TDBManagerForm.OpenBtnClick(Sender: TObject);
begin
  if MembersGrid.Current.Row < MembersGrid.Count then
    OpenMember(MembersGrid.Values[0, MembersGrid.Current.Row]);
end;

procedure TDBManagerForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  ApplyFilter;
end;

procedure TDBManagerForm.CheckSearch;
begin
  if (FSearch = '') or (SecondsBetween(Now, FSearchTime) > 3) then
  begin
    FFirstSearch := True;
    FSearch := '';
  end;
end;

procedure TDBManagerForm.SearchFor(S: string);
var
  i: Integer;
  t: string;
  f: Integer;
begin
  f := ord(not FFirstSearch);
  for i := MembersGrid.Current.Row + f to MembersGrid.Count -1 do
  begin
    t := LeftStr(MembersGrid.Values[0, i], Length(S));
    if SameText(S, t) then
    //t := MembersGrid.Cells[0, i];
    //if Pos(s, t) > 0 then
    begin
      FSearching := True;
      try
        MembersGrid.Current.Row := i;
      finally
        FSearching := False;
      end;
      break;
    end;
  end;
  FSearchTime := Now;
end;

procedure TDBManagerForm.ApplyFilter;
begin

end;

procedure TDBManagerForm.CollectMetaItems(vMetaItems: TmncMetaItems);
var
  aItemName: string;
begin
  vMetaItems.Clone(DBEngine.Stack.Current.MetaItems);
  //vMetaItems.Values[DBEngine.Stack.Current.Addon.Name] := DBEngine.Stack.Current.Value;
  aItemName := MembersGrid.Values[0, MembersGrid.Current.Row];
  vMetaItems.Values[CurrentGroup.ItemName] := aItemName;
  DumpMetaItems(vMetaItems);
end;

procedure TDBManagerForm.StateChanged;
begin
  if CurrentGroup <> nil then
    LoadActions(CurrentGroup.ItemName);
  if Visible then //prevent from setfocus non visible control
    MembersGrid.SetFocus;
end;

procedure TDBManagerForm.LoadActions(vGroup: string; Append: Boolean);
var
  i, c: Integer;
  aList: TmndAddons;
  aMenuItem: TMenuItem;
begin
  if (vGroup = '') or not Append then
  begin
    ActionsPopupMenu.Items.Clear;
    FreeAndNil(Actions);
  end;
  if vGroup <> '' then
  begin
    Actions := TmndAddons.Create;
    aList := TmndAddons.Create;
    try
      DBEngine.Enum(vGroup, aList, DBEngine.DB.IsActive);
      c := 0;
      for i := 0 to aList.Count - 1 do
        if nsCommand in aList[i].Style then
        begin
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
  MembersGrid.PopupMenu := ActionsPopupMenu;
end;

procedure TDBManagerForm.SaveOptions;
begin
  Engine.Options.Custom.Values['Databases.Height'] := IntToStr(DatabasesPnl.Height);
end;

procedure TDBManagerForm.LoadOptions;
begin
  DatabasesPnl.Height := StrToIntDef(Engine.Options.Custom.Values['Databases.Height'], DatabasesPnl.Height);
end;

procedure TDBManagerForm.ServerChanged;
var
  Strings: TStringList;
  Meta: TmncMeta;
  Items: TmncMetaItems;
  Item: TmncMetaItem;
  aListItem: TListItem;
  EngineName: string;
begin
  DatabasesList.Items.BeginUpdate;
  Strings := TStringList.Create;
  try
    DatabasesList.Clear;
    if (DBEngine.Server.Engine <> nil) and (DBEngine.Server.Engine.MetaClass <> nil) then
    begin
      EngineName := DBEngine.Server.Engine.Name;

      ServerLbl.Caption := EngineName;
      ServerImage.ImageIndex := EditorResource.GetImageIndex(EngineName, cDatabaseImage);

      Meta := DBEngine.Server.Engine.MetaClass.Create;
      Meta.ServerInfo := DBEngine.Server.Info;
      Items := TmncMetaItems.Create;
      try
        Meta.EnumDatabases(Items, [ekSort]);
        for Item in Items do
        begin
          aListItem := DatabasesList.Items.Add;
          aListItem.Caption := Item.Name;
          aListItem.ImageIndex := EditorResource.GetImageIndex(EngineName, cDatabaseImage);
        end;
      finally
        Items.Free;
      end;
    end
    else
    begin
      ServerLbl.Caption := 'No server connected';
      DatabaseImage.ImageIndex := cDatabaseImage;
    end;
  finally
    DatabasesList.EndUpdate;
    Strings.Free;
  end;
end;

procedure TDBManagerForm.DatabaseChanged;
begin
  if DBEngine.DB.IsActive then
  begin
    DatabaseLbl.Caption := DBEngine.DB.Connection.Resource;
    DatabaseImage.ImageIndex := EditorResource.GetImageIndex(DBEngine.DB.Connection.EngineName, cDatabaseImage);
  end
  else
  begin
    DatabaseLbl.Caption := '';
    DatabaseImage.ImageIndex := cDatabaseImage;
  end;
end;

procedure TDBManagerForm.ActionsMenuSelect(Sender: TObject);
var
  aValue: string;
  aAddon: TmndAddon;
begin
  aAddon := Actions[(Sender as TMenuItem).Tag];
  aValue := MembersGrid.Values[0, MembersGrid.Current.Row];
  OpenAction(aAddon, aValue);
end;

constructor TDBManagerForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  GroupsNames := TmndAddons.Create;
  DBEngine.NotifyObject := Self;
  Engine.RegisterNotify(Self);
  StateChanged;
end;

destructor TDBManagerForm.Destroy;
begin
  DBEngine.NotifyObject := nil;
  DBEngine.DB.Close;
  FreeAndNil(GroupsNames);
  FreeAndNil(Actions);
  Engine.UnregisterNotify(Self);
  inherited Destroy;
end;

procedure TDBManagerForm.OpenMember(AValue: string);
var
  a: TmncMetaItems;
begin
  a := TmncMetaItems.Create;
  try
    {$ifdef DEBUG}
    DebugLn('CurrentGroup.Name='+CurrentGroup.Name);
    DebugLn('CurrentGroup.ItemName='+CurrentGroup.ItemName);
    DebugLn('AValue='+AValue);
    {$endif}
    CollectMetaItems(a);
    with DBEngine.Stack do
      //if Current.Addon <> nil then
      //begin
        DBEngine.Stack.Push(TmndProcess.Create(CurrentGroup.Name, CurrentGroup.ItemName, AValue, a));
        DBEngine.Run;
      //end;
        //what if Addon <> nil or what if Current.Addon.Item = ''
  finally
    a.Free;
  end;
end;

procedure TDBManagerForm.OpenGroup(AValue: string);
var
  a: TmncMetaItems;
begin
  a := TmncMetaItems.Create;
  try
    {$ifdef DEBUG}
    DebugLn('CurrentGroup.Name='+CurrentGroup.Name);
    DebugLn('CurrentGroup.ItemName='+CurrentGroup.ItemName);
    DebugLn('OpenGroup.AValue='+AValue);
    {$endif}
    //CollectMetaItems(a);
    with DBEngine.Stack do
      if (Current <> nil) and (GroupsList.Items.Count > 0) and (GroupsList.ItemIndex >=0) then
      begin
        DBEngine.Stack.Push(TmndProcess.Create(Current.CurrentAddon, AValue, Current.MetaItems));
        DBEngine.Run;
      end;
  finally
    a.Free;
  end;
end;

procedure TDBManagerForm.OpenAction(vAction: TmndAddon; aValue: string);
var
  a: TmncMetaItems;
begin
  a := TmncMetaItems.Create;
  try
    {$ifdef DEBUG}
    DebugLn('CurrentGroup.Name='+CurrentGroup.Name);
    DebugLn('CurrentGroup.ItemName='+CurrentGroup.ItemName);
    DebugLn('AValue=' + AValue);
    {$endif}
    CollectMetaItems(a);
    if vAction <> nil then
      vAction.Execute(a);
  finally
    a.Free;
  end;
end;

function TDBManagerForm.LogTime(Start: TDateTime): string;
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

end.
