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
{


    Root : Addon (label)  Database
      Masters : Addons(combobox) [Tables, Indexes, ...]
        Master : Addons(itemindex of combobox) [Tables]
          Members Master Items : Memoer(grid) [Table1, Table2]
             Actions : Addons PopupMenu (Select, Drop, ...)
}

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

  { TActionMenuItem }

  TActionMenuItem = class(TMenuItem)
  protected
    procedure Click; override;
  public
    Addon: TmndAddon;
    MetaItem: TmncMetaItem;
  end;

  { TDBManagerForm }

  TDBManagerForm = class(TFrame, INotifyEngine, INotifyEngineSetting, ImndNotify)
    DatabaseImage: TntvImgBtn;
    DatabaseImage1: TntvImgBtn;
    MasterLbl: TLabel;
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
    RefreshBtn: TButton;
    FileMnu: TMenuItem;
    ExitMnu: TMenuItem;
    FirstBtn: TButton;
    AddonsList: TComboBox;
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
    procedure ActionsPopupMenuPopup(Sender: TObject);
    procedure BackBtnClick(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure DatabasesListDblClick(Sender: TObject);
    procedure DisconnectBtnClick(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
    procedure FirstBtnClick(Sender: TObject);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure AddonsListKeyPress(Sender: TObject; var Key: char);
    procedure AddonsListSelect(Sender: TObject);
    procedure MembersGridDblClick(Sender: TObject);
    procedure MembersGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MembersGridUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure MenuItem1Click(Sender: TObject);
    procedure MembersListDblClick(Sender: TObject);
    procedure MembersListKeyPress(Sender: TObject; var Key: char);
    procedure RefreshBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FSearching: Boolean;
    FFirstSearch: Boolean;
    FSearch: UTF8String;
    FSearchTime: TDateTime;
    procedure CheckSearch;
    procedure SearchFor(S: string);
    procedure ApplyFilter;
  private
    function LogTime(Start: TDateTime): string;
    procedure StateChanged;
  public
    Masters: TmndAddons;//Fields, Indexes
    Members: TmncMetaItems; //Grid
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    //procedure OpenRoot(AValue: string);

    procedure OpenMaster(AValue: string);

    procedure LoadMembers(vAddon: TmndAddon; vMetaItem: TmncMetaItem);
    procedure OpenMember(AMember: TmncMetaItem);

    procedure LoadActions(vMaster: string; vMetaItem: TmncMetaItem; Append: Boolean = False);

    procedure SaveOptions;
    procedure LoadOptions;

    procedure ServerChanged;
    procedure DatabaseChanged;

    procedure ShowMeta(vAddon: TmndAddon; vMetaItem: TmncMetaItem; vSelectDefault: Boolean);

    procedure ShowEditor(vAddon: TmndAddon; S: string);
  end;

implementation

{$R *.lfm}

uses
  CSVOptionsForms, ParamsForms, SynEditMiscProcs;

{ TActionMenuItem }

procedure TActionMenuItem.Click;
begin
  if Addon <> nil then
    Addon.Execute(MetaItem);
end;

{ TDBManagerForm }

procedure TDBManagerForm.ShowMeta(vAddon: TmndAddon; vMetaItem: TmncMetaItem; vSelectDefault: Boolean);
var
  i, c: Integer;
  d: Integer;
  Select: string;
  b: Boolean;
  aAddons: TmndAddons;
  aSelectedAddon: TmndAddon;
begin
  ServerLbl.Caption := DBEngine.DB.Connection.Host;
  DatabaseLbl.Caption := DBEngine.DB.Connection.Resource;
  MasterLbl.Caption := vAddon.Master;
  aAddons := TmndAddons.Create(False);
  try
    DBEngine.EnumAddons(vAddon.Master, aAddons, DBEngine.DB.IsActive);
    //DBEngine.EnumAddons(AddonName, aAddons, DBEngine.DB.IsActive);

    //select := DBEngine.Stack.Current.Select;
    Select := vAddon.Name;
    aSelectedAddon := nil;
    d := -1;
    c := 0;
    b := false;
    AddonsList.Items.BeginUpdate;
    try
      AddonsList.Clear;
      Masters.Clear;
      for i := 0 to aAddons.Count -1 do
      begin
        if not (nsCommand in aAddons[i].Style) then //Group in style
        begin
          AddonsList.Items.Add(aAddons[i].Title);
          Masters.Add(aAddons[i]);
          //if (d < 0) then
          begin
            if (d < 0) then //Select first one
            begin
              d := c;
              aSelectedAddon := aAddons[i];
            end;
            //b mean already selected and override privouse assigb
            if not b and (((Select = '') and (nsDefault in aAddons[i].Style))) or (((Select <>'') and SameText(Select, aAddons[i].Name))) then
            begin
              d := c;
              aSelectedAddon := aAddons[i];
              b := true;
            end;
          end;
          c := c + 1;
        end;
      end;
    finally
      AddonsList.Items.EndUpdate;
    end;
    if d < 0 then
      d := 0;
    if aAddons.Count > 0 then
      AddonsList.ItemIndex := d;
  finally
    aAddons.Free;
  end;

  {if aSelectedAddon <> nil then
    DBEngine.Stack.Current.Select := aSelectedAddon.Name;}

  LoadMembers(vAddon, vMetaItem); //if Master is nil it must clear the member grid
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

procedure TDBManagerForm.LoadMembers(vAddon: TmndAddon; vMetaItem: TmncMetaItem);
var
  i, j: Integer;
  aMetaItems: TmncMetaItems;
begin
  MembersGrid.Clear;
  if vAddon = nil then
  begin
    MembersGrid.ColumnsCount := 1;
    MembersGrid.Columns[0].Title := '';
    MembersGrid.Capacity := 1;
    MembersGrid.Count := 1;
  end
  else
  begin
    FreeAndNil(Members);
    Members := TmncMetaItems.Create;

    MembersGrid.BeginUpdate;
    MembersGrid.Reset;
    try
      vAddon.EnumMeta(Members, vMetaItem);

      if Members.Count > 0 then
      begin
        MembersGrid.ColumnsCount := Members[0].Attributes.Count + 1;
        MembersGrid.Columns[0].Title := 'Name';
        for i := 1 to MembersGrid.ColumnsCount -1 do
        begin
          MembersGrid.Columns[i].Title := Members[0].Attributes.Items[i - 1].Name;
        end;
        if MembersGrid.ColumnsCount > 0 then
          MembersGrid.Columns[0].AutoFit := True;

        MembersGrid.Capacity := Members.Count;
        MembersGrid.Count := Members.Count;
        for i := 0 to Members.Count -1 do
        begin
          for j := 0 to MembersGrid.ColumnsCount - 1 do
          begin
            if j = 0 then
              MembersGrid.Values[j, i] := Members[i].Name
            else if (j - 1) < Members[i].Attributes.Count then //maybe Attributes not have all data
              MembersGrid.Values[j, i] := Members[i].Attributes.Items[j - 1].Value; //TODO must be assigned my name not by index
          end;
        end;
        MembersGrid.Current.Row := 0;
      end;
    finally
      MembersGrid.EndUpdate;
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

procedure TDBManagerForm.ActionsPopupMenuPopup(Sender: TObject);
begin
  with ActionsPopupMenu do
  begin
    if PopupComponent = DatabaseLbl then
    else if PopupComponent = MasterLbl then
      LoadActions(DBEngine.Stack.Current.Addon.Master, DBEngine.Stack.Current.MetaItem, False)
    else if PopupComponent = AddonsList then
      LoadActions(DBEngine.Stack.Current.Addon.Name, DBEngine.Stack.Current.MetaItem, False)
    else
    begin
      LoadActions(DBEngine.Stack.Current.Addon.Name, Members.Items[MembersGrid.Current.Row], False);
    end
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
        AddonsList.SetFocus
      else
        MembersGrid.SetFocus;
      Handled := True;
    end;
  end;
end;

procedure TDBManagerForm.AddonsListKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    MembersGrid.SetFocus;
end;

procedure TDBManagerForm.AddonsListSelect(Sender: TObject);
begin
  OpenMaster(Masters[AddonsList.ItemIndex].Name);
end;

procedure TDBManagerForm.MembersGridDblClick(Sender: TObject);
begin
  if (MembersGrid.Count > 0) and (MembersGrid.Current.Row >= 0) then
    OpenMember(Members[MembersGrid.Current.Row]);
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
      OpenMember(Members[MembersGrid.Current.Row]);
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
  ShowMeta(DBEngine.Stack.Current.Addon, DBEngine.Stack.Current.MetaItem, False);
end;

procedure TDBManagerForm.MembersListDblClick(Sender: TObject);
begin
  OpenMember(Members[MembersGrid.Current.Row]);
end;

procedure TDBManagerForm.MembersListKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
  begin
    OpenMember(Members[MembersGrid.Current.Row]);
    Key := #0;
  end;
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

procedure TDBManagerForm.StateChanged;
begin
  if Visible then //prevent from setfocus non visible control
    MembersGrid.SetFocus;
end;

procedure TDBManagerForm.LoadActions(vMaster: string; vMetaItem: TmncMetaItem; Append: Boolean);
var
  i, c: Integer;
  aList: TmndAddons;
  aMenuItem: TActionMenuItem;
begin
  if (vMaster = '') or not Append then
    ActionsPopupMenu.Items.Clear;
  if vMaster <> '' then
  begin
    aList := TmndAddons.Create;
    try
      DBEngine.EnumAddons(vMaster, aList, DBEngine.DB.IsActive);
      c := 0;
      for i := 0 to aList.Count - 1 do
        if nsCommand in aList[i].Style then
        begin
          aMenuItem := TActionMenuItem.Create(ActionsPopupMenu);
          aMenuItem.Caption := aList[i].Title;
          aMenuItem.Addon := aList[i];
          aMenuItem.MetaItem := vMetaItem;
          ActionsPopupMenu.Items.Add(aMenuItem);
          Inc(c);
        end;
    finally
      aList.Free;
    end;
  end;
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

      ServerLbl.Caption := EngineName + ': ' + DBEngine.Server.Info.Host;
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
        Meta.Free;
      end;
      DatabasesList.Visible := True;
    end
    else
    begin
      if DBEngine.DB.IsActive then
        ServerLbl.Caption := DBEngine.DB.Connection.Host
      else
        ServerLbl.Caption := 'No server connected';
      DatabaseImage.ImageIndex := cDatabaseImage;
      DatabasesList.Visible := False;
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

constructor TDBManagerForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Masters := TmndAddons.Create;
  DBEngine.NotifyObject := Self;
  Engine.RegisterNotify(Self);
  StateChanged;
end;

destructor TDBManagerForm.Destroy;
begin
  DBEngine.NotifyObject := nil;
  DBEngine.DB.Close;
  FreeAndNil(Masters);
  FreeAndNil(Members);
  Engine.UnregisterNotify(Self);
  inherited Destroy;
end;

procedure TDBManagerForm.OpenMember(AMember: TmncMetaItem);
var
  a: TmncMetaItems;
  aDefault: string;
begin
  a := TmncMetaItems.Create;
  try
    with DBEngine.Stack do
      if Current.Addon <> nil then
      begin
        aDefault := Current.Addon.DefaultAddon;
        {if (aDefault = '')  then
          aDefault := DBEngine.FindDefault(Current.Addon.Name);}
        if aDefault <> '' then
        begin
          DBEngine.Stack.Push(TmndProcess.Create(aDefault, AMember));
          DBEngine.Run;
        end;
      end;
        //what if Addon <> nil or what if Current.Addon.Item = ''
  finally
    a.Free;
  end;
end;

procedure TDBManagerForm.OpenMaster(AValue: string);
var
  a: TmncMetaItems;
begin
  a := TmncMetaItems.Create;
  try
    with DBEngine.Stack do
      if (Current <> nil) and (AddonsList.Items.Count > 0) and (AddonsList.ItemIndex >=0) then
      begin
        DBEngine.Stack.Push(TmndProcess.Create(AValue, Current.MetaItem));
        DBEngine.Run;
      end;
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
