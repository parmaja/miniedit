unit sqlvManagerForms;
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
  ExtCtrls, StdCtrls, SynEdit, FileUtil, Buttons, Menus, sqlvSessions, mncCSV,
  mncSQL, SynCompletion, SynEditAutoComplete, SynHighlighterHashEntries,
  mnUtils, mncMeta, mncCSVExchanges, mnSynHighlighterStdSQL, mncMySQL,
  mncPostgre, mncSQLite, mncSQLiteMeta, mncPGMeta, mncFBMeta, ntvGrids,
  ntvPanels, sqlvConsts, sqlvClasses, sqlvStdClasses, LMessages, ComCtrls,
  EditorEngine;

type

  { TsqlvManagerForm }

  TsqlvManagerForm = class(TFrame)
    BackBtn: TButton;
    CacheMetaChk1: TCheckBox;
    Edit1: TEdit;
    MembersGrid: TntvGrid;
    MembersGrid1: TntvGrid;
    MetaLbl: TLabel;
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
    SaveMnu: TMenuItem;
    SaveAsMnu: TMenuItem;
    OpenMnu: TMenuItem;
    GroupsPanel: TPanel;
    AboutMnu: TMenuItem;
    ToolsMnu: TMenuItem;
    GroupPanel: TPanel;
    procedure BackBtnClick(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure DisconnectBtnClick(Sender: TObject);
    procedure FirstBtnClick(Sender: TObject);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure GroupsListKeyPress(Sender: TObject; var Key: char);
    procedure GroupsListSelect(Sender: TObject);
    procedure MembersGridClick(Sender: TObject);
    procedure MembersGridDblClick(Sender: TObject);
    procedure MembersGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MembersGridUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure RefreshBtnClick(Sender: TObject);
    procedure MembersListDblClick(Sender: TObject);
    procedure MembersListKeyPress(Sender: TObject; var Key: char);
    procedure OpenBtnClick(Sender: TObject);
  private
    FSearching: Boolean;
    FFirstSearch: Boolean;
    FSearch: UTF8String;
    FSearchTime: TDateTime;
    procedure CheckSearch;
    procedure SearchFor(S: string);
    procedure CollectAttributes(vAttributes: TsqlvAttributes);
  private
    function LogTime(Start: TDateTime): string;
    procedure StateChanged;
  public
    Actions: TsqlvAddons;
    GroupsNames: TsqlvAddons;//Fields, Indexes
    CurrentGroup: TsqlvAddon;
    procedure ActionsMenuSelect(Sender: TObject);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenAction(vAction: TsqlvAddon; aValue: string);
    procedure OpenMember(AValue: string);
    procedure OpenGroup(AValue: string);
    procedure LoadActions(vGroup: string; Append: Boolean = False);
  end;

  { TsqlvMainGui }

  TsqlvMainGui = class(TsqlvGui)
  protected
    ManagerForm: TsqlvManagerForm;
    procedure LoadMembers(vGroup: TsqlvAddon; vAttributes: TsqlvAttributes);
  public
    constructor Create(AManagerForm: TsqlvManagerForm);
    procedure LoadEditor(vAddon: TsqlvAddon; S: string); override;
    procedure ShowMeta(vAddon: TsqlvAddon; vSelectDefault: Boolean); override;
  end;

implementation

{$R *.lfm}

uses
  CSVOptionsForms, ParamsForms, SynEditMiscProcs;

{ TsqlvMainGui }

procedure TsqlvMainGui.ShowMeta(vAddon: TsqlvAddon; vSelectDefault: Boolean);
var
  i, c: Integer;
  d: Integer;
  g: string;
  b: Boolean;
  aGroups: TsqlvAddons;
  aGroup: TsqlvAddon;
begin
  with ManagerForm do
  begin
    aGroups := TsqlvAddons.Create;
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
    MetaLbl.Caption := vAddon.Title + ': ' + DBEngine.Stack.Current.Attributes[DBEngine.Stack.Current.Addon.Name];

    if aGroup <> nil then
      DBEngine.Stack.Current.Select := aGroup.Name;

    LoadMembers(aGroup, DBEngine.Stack.Current.Attributes); //if group is nil it must clear the member grid
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
    ManagerForm.MembersGrid.Clear
  else
  begin
    aItems := TmncMetaItems.Create;
    try
      with ManagerForm do
      begin
        aHeader := TStringList.Create;
        try
          vGroup.EnumHeader(aHeader);
          MembersGrid.ColumnsCount := aHeader.Count;
          for i := 0 to aHeader.Count -1 do
          begin
            MembersGrid.Columns[i].Title := aHeader[i];
            if i = 0 then
              MembersGrid.Columns[i].AutoSize := True;
          end;

          aCols := aHeader.Count;
        finally
          aHeader.Free;
        end;

        vGroup.EnumMeta(aItems, vAttributes);
        c := 0;
        MembersGrid.BeginUpdate;
        try
          for i := 0 to aItems.Count -1 do
          begin
            MembersGrid.RowsCount := c + 1;
            for j := 0 to aCols - 1 do
            begin
              if j = 0 then
                MembersGrid.Values[j, c + 1] := aItems[i].Name
              else if (j - 1) < aItems[i].Attributes.Count then //maybe Attributes not have all data
                MembersGrid.Values[j, c + 1] := aItems[i].Attributes.Items[j - 1].Value; //TODO must be assigned my name not by index
            end;
            c := c + 1;
          end;
        finally
          MembersGrid.EndUpdate;
        end;
        MembersGrid.Current.Row := 0;
        //MembersGrid.AutoSizeColumns;
        CurrentGroup := nil; //reduce flicker when fill Actions
        CurrentGroup := vGroup;
        LoadActions(vGroup.ItemName);
      end;
    finally
      aItems.Free;
    end;
  end;
end;

constructor TsqlvMainGui.Create(AManagerForm: TsqlvManagerForm);
begin
  inherited Create;
  ManagerForm := AManagerForm;
end;

procedure TsqlvMainGui.LoadEditor(vAddon: TsqlvAddon; S: string);
begin

end;

{ TsqlvManagerForm }

procedure TsqlvManagerForm.BackBtnClick(Sender: TObject);
begin
  if DBEngine.Stack.Count > 1 then
  begin
    DBEngine.Stack.Pop;
    if (DBEngine.Stack.Current <> nil) then
      DBEngine.Run(DBEngine.Stack);
  end;
end;

procedure TsqlvManagerForm.ConnectBtnClick(Sender: TObject);
begin

end;

procedure TsqlvManagerForm.DisconnectBtnClick(Sender: TObject);
begin

end;

procedure TsqlvManagerForm.FirstBtnClick(Sender: TObject);
begin
  DBEngine.Stack.Clear;
//  DBEngine.Stack.Push(TsqlvProcess.Create('Databases', 'Database', 'Tables', DatabasesCbo.Text));
  DBEngine.Run;
end;

procedure TsqlvManagerForm.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
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

procedure TsqlvManagerForm.GroupsListKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    MembersGrid.SetFocus;
end;

procedure TsqlvManagerForm.GroupsListSelect(Sender: TObject);
begin
  OpenGroup(GroupsNames[GroupsList.ItemIndex].Name);
end;

procedure TsqlvManagerForm.MembersGridClick(Sender: TObject);
begin

end;

procedure TsqlvManagerForm.MembersGridDblClick(Sender: TObject);
begin
  if (MembersGrid.RowsCount > 1) and (MembersGrid.Current.Row >= 1) then
    OpenMember(MembersGrid.Values[0, MembersGrid.Current.Row]);
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
      OpenMember(MembersGrid.Values[0, MembersGrid.Current.Row]);
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

procedure TsqlvManagerForm.RefreshBtnClick(Sender: TObject);
begin

end;

procedure TsqlvManagerForm.MembersListDblClick(Sender: TObject);
begin
  OpenMember(MembersGrid.Values[0, MembersGrid.Current.Row]);
end;

procedure TsqlvManagerForm.MembersListKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
  begin
    OpenMember(MembersGrid.Values[0, MembersGrid.Current.Row]);
    Key := #0;
  end;
end;

procedure TsqlvManagerForm.OpenBtnClick(Sender: TObject);
begin
  OpenMember(MembersGrid.Values[0, MembersGrid.Current.Row]);
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
  for i := MembersGrid.Current.Row + f to MembersGrid.RowsCount -1 do
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

procedure TsqlvManagerForm.CollectAttributes(vAttributes: TsqlvAttributes);
begin
  vAttributes.Clone(DBEngine.Stack.Current.Attributes);
  //vAttributes.Values[DBEngine.Stack.Current.Addon.Name] := DBEngine.Stack.Current.Value;
  vAttributes.Values[CurrentGroup.ItemName] := MembersGrid.Values[0, MembersGrid.Current.Row];
  DumpAttributes(vAttributes);
end;

procedure TsqlvManagerForm.StateChanged;
begin
  if CurrentGroup <> nil then
    LoadActions(CurrentGroup.ItemName);
  if Visible then //prevent from setfocus non visible control
    MembersGrid.SetFocus;
end;

procedure TsqlvManagerForm.LoadActions(vGroup: string; Append: Boolean);
var
  i, c: Integer;
  aList: TsqlvAddons;
  aMenuItem: TMenuItem;
begin
  if (vGroup = '') or not Append then
  begin
    ActionsPopupMenu.Items.Clear;
    FreeAndNil(Actions);
  end;
  if vGroup <> '' then
  begin
    Actions := TsqlvAddons.Create;
    aList := TsqlvAddons.Create;
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

procedure TsqlvManagerForm.ActionsMenuSelect(Sender: TObject);
var
  aValue: string;
  aAddon: TsqlvAddon;
begin
  aAddon := Actions[(Sender as TMenuItem).Tag];
  aValue := MembersGrid.Values[0, MembersGrid.Current.Row];
  OpenAction(aAddon, aValue);
end;

constructor TsqlvManagerForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  GroupsNames := TsqlvAddons.Create;
  sqlvGui := TsqlvMainGui.Create(Self);
  StateChanged;
end;

destructor TsqlvManagerForm.Destroy;
begin
  DBEngine.DB.Close;
  FreeAndNil(GroupsNames);
  FreeAndNil(sqlvGui);
  inherited Destroy;
end;

{ TPanelsList }

{ TControlObjects }

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
    with DBEngine.Stack do
      if Current.Addon <> nil then
      begin
        TsqlvProcess.Create(CurrentGroup.Name, CurrentGroup.ItemName, AValue, a);
        DBEngine.Run;
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
    with DBEngine.Stack do
      if (Current <> nil) and (GroupsList.Items.Count > 0) and (GroupsList.ItemIndex >=0) then
      begin
        DBEngine.Stack.Push(TsqlvProcess.Create(Current.Addon, AValue, Current.Attributes));
        DBEngine.Run;
      end;
  finally
    a.Free;
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

end.
