unit mneRecentsForms;

{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Menus, Dialogs, ComCtrls, StdCtrls, mneClasses, sqlvClasses, Types;

type

  { TManageRecentsForm }

  TManageRecentsForm = class(TForm)
    PageControl: TPageControl;
    RDatabasesList: TListBox;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ProjectsList: TListBox;
    OpenBtn: TButton;
    CloseBtn: TButton;
    RProjectsList: TListBox;
    RFilesList: TListBox;
    PopupMnu: TPopupMenu;
    AddtoProjects1: TMenuItem;
    Button1: TButton;
    Button3: TButton;
    MoveDownBtn: TButton;
    MoveUpBtn: TButton;
    Button2: TButton;
    TabSheet4: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure ProjectsListDblClick(Sender: TObject);
    procedure AddtoProjects1Click(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure MoveDownBtnClick(Sender: TObject);

    procedure ProjectsListMeasureItem(Control: TWinControl; Index: Integer; var AHeight: Integer);
  private
    procedure EnumRecentDatabases;
    procedure EnumRecentFile;
    procedure EnumRecentProjects;
    procedure EnumProjects;
    procedure NewProject;
    procedure RemoveNow;
    procedure OpenNow;
    procedure ChangeToIndex(ListBox: TlistBox; Index: integer);
  public
    { Public declarations }
  end;

implementation

uses EditorEngine, mneProjectOptions;

{$R *.lfm}

{ TForm1 }

procedure TManageRecentsForm.EnumRecentFile;
var
  i: integer;
begin
  RFilesList.Clear;
  for i := 0 to Engine.Options.RecentFiles.Count - 1 do
  begin
    RFilesList.Items.Add(Engine.Options.RecentFiles[i].Name);
  end;
end;

procedure TManageRecentsForm.EnumRecentProjects;
var
  i: integer;
begin
  RProjectsList.Clear;
  for i := 0 to Engine.Options.RecentProjects.Count - 1 do
  begin
    RProjectsList.Items.Add(Engine.Options.RecentProjects[i].Name);
  end;
end;

procedure TManageRecentsForm.FormCreate(Sender: TObject);
begin
  PageControl.TabIndex := 0;
  EnumRecentFile;
  EnumRecentProjects;
  EnumProjects;
  EnumRecentDatabases;
  if ProjectsList.Items.Count > 0 then
    ProjectsList.ItemIndex := 0;
  if RProjectsList.Items.Count > 0 then
    RProjectsList.ItemIndex := 0;
  if RFilesList.Items.Count > 0 then
    RFilesList.ItemIndex := 0;
  ActiveControl := ProjectsList;
end;

procedure TManageRecentsForm.NewProject;
begin
  if Engine.NewProject then
  begin
    EnumProjects;
    EnumRecentProjects;
  end;
end;

procedure TManageRecentsForm.Button3Click(Sender: TObject);
begin
  NewProject;
end;

procedure TManageRecentsForm.EnumProjects;
var
  i: integer;
begin
  ProjectsList.Clear;
  for i := 0 to Engine.Options.Projects.Count - 1 do
  begin
    ProjectsList.Items.Add(Engine.Options.Projects[i].Name);
  end;
end;

procedure TManageRecentsForm.Button1Click(Sender: TObject);
var
  aOpenDialog: TOpenDialog;
begin
  aOpenDialog := TOpenDialog.Create(nil);
  try
    aOpenDialog.Title := 'Open project';
    aOpenDialog.DefaultExt := Engine.Extenstion;
    aOpenDialog.Filter := 'Project files|*.' + Engine.Extenstion + '|All files|*.*';
    aOpenDialog.FileName := '*.' + aOpenDialog.DefaultExt;
    if aOpenDialog.Execute then
    begin
      Engine.ProcessProject(aOpenDialog.FileName);
      Engine.ProcessRecentProject(aOpenDialog.FileName);
      EnumProjects;
      EnumRecentProjects;
    end;
  finally
    aOpenDialog.Free;
  end;
end;

procedure TManageRecentsForm.Button2Click(Sender: TObject);
begin
  RemoveNow;
end;

procedure TManageRecentsForm.FormShow(Sender: TObject);
begin

end;

procedure TManageRecentsForm.RemoveNow;
var
  Old: integer;
begin
  case PageControl.TabIndex of
    0:
      if ProjectsList.ItemIndex >= 0 then
      begin
        Old := ProjectsList.ItemIndex;
        Engine.RemoveProject(ProjectsList.Items[Old]);
        EnumProjects;
        ChangeToIndex(ProjectsList, Old);
      end;
    1:
      if RProjectsList.ItemIndex >= 0 then
      begin
        Old := RProjectsList.ItemIndex;
        Engine.RemoveRecentProject(RProjectsList.Items[RProjectsList.ItemIndex]);
        EnumRecentProjects;
        ChangeToIndex(RProjectsList, Old);
      end;
    2:
      if RFilesList.ItemIndex >= 0 then
      begin
        Old := RFilesList.ItemIndex;
        Engine.RemoveRecentFile(RFilesList.Items[RFilesList.ItemIndex]);
        EnumRecentFile;
        ChangeToIndex(RFilesList, Old);
      end;
    3:
      if RDatabasesList.ItemIndex >= 0 then
      begin
        Old := RDatabasesList.ItemIndex;
        DBEngine.Recents.Delete(RDatabasesList.ItemIndex);
        EnumRecentDatabases;
        ChangeToIndex(RDatabasesList, Old);
      end;
  end;
end;

procedure TManageRecentsForm.OpenNow;
begin
  case PageControl.TabIndex of
    0:
      if ProjectsList.ItemIndex >= 0 then
      begin
        Engine.Session.Load(ProjectsList.Items[ProjectsList.ItemIndex]);
        Close;
      end;
    1:
      if RProjectsList.ItemIndex >= 0 then
      begin
        Engine.Session.Load(RProjectsList.Items[RProjectsList.ItemIndex]);
        Close;
      end;
    2:
      if RFilesList.ItemIndex >= 0 then
      begin
        Engine.Files.OpenFile(RFilesList.Items[RFilesList.ItemIndex]);
        Close;
      end;
  end;
end;

procedure TManageRecentsForm.OpenBtnClick(Sender: TObject);
begin
  OpenNow;
end;

procedure TManageRecentsForm.ProjectsListDblClick(Sender: TObject);
begin
  OpenNow;
end;

procedure TManageRecentsForm.AddtoProjects1Click(Sender: TObject);
var
  aProject: string;
  POld, ROld: integer;
begin
  if RProjectsList.ItemIndex >= 0 then
  begin
    ROld := RProjectsList.ItemIndex;
    POld := ProjectsList.ItemIndex;
    aProject := RProjectsList.Items[RProjectsList.ItemIndex];
    Engine.ProcessProject(aProject);
    Engine.ProcessRecentProject(aProject);
    EnumProjects;
    EnumRecentProjects;
    ChangeToIndex(RProjectsList, ROld);
    ChangeToIndex(ProjectsList, POld);
  end;
end;

procedure TManageRecentsForm.MoveUpBtnClick(Sender: TObject);
var
  i: integer;
begin
  if ProjectsList.ItemIndex > 0 then
  begin
    i := ProjectsList.ItemIndex;
    Engine.Options.Projects.Exchange(i, i - 1);
    EnumProjects;
    ProjectsList.ItemIndex := i - 1;
  end;
end;

procedure TManageRecentsForm.MoveDownBtnClick(Sender: TObject);
var
  i: integer;
begin
  if (ProjectsList.ItemIndex >= 0) and (ProjectsList.ItemIndex < (ProjectsList.Count - 1)) then
  begin
    i := ProjectsList.ItemIndex;
    Engine.Options.Projects.Exchange(i, i + 1);
    EnumProjects;
    ProjectsList.ItemIndex := i + 1;
  end;
end;

procedure TManageRecentsForm.ProjectsListMeasureItem(Control: TWinControl; Index: Integer; var AHeight: Integer);
begin
  AHeight := AHeight + 10;
end;

procedure TManageRecentsForm.EnumRecentDatabases;
var
  i: integer;
begin
  RDatabasesList.Clear;
  for i := 0 to DBEngine.Recents.Count - 1 do
  begin
    RDatabasesList.Items.Add(DBEngine.Recents[i]);
  end;
end;

procedure TManageRecentsForm.ChangeToIndex(ListBox: TlistBox; Index: integer);
begin
  if ListBox.Items.Count = 0 then
    Index := -1
  else if Index >= ListBox.Items.Count then
    Index := ListBox.Items.Count - 1;
  ListBox.ItemIndex := Index;
end;

end.

