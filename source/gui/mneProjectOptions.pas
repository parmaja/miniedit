unit mneProjectOptions;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, SynEdit,
  EditorEngine, EditorDebugger, mneClasses, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus;

type

  { TProjectForm }

  TProjectForm = class(TForm)
    Button3: TButton;
    Button4: TButton;
    DescriptionEdit: TEdit;
    Label2: TLabel;
    Label9: TLabel;
    MainEdit: TEdit;
    OverrideOptionsSheet: TTabSheet;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    NameEdit: TEdit;
    SaveDesktopChk: TCheckBox;
    TitleEdit: TEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    PageControl: TPageControl;
    PathPopupMenu: TPopupMenu;
    RootDirEdit: TEdit;
    SCMCbo: TComboBox;
    GeneralSheet: TTabSheet;
    procedure Button4Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
  private
    FProject: TEditorProject;
    FFrames: array of TFrame;
  protected
    procedure AddFrame(AFrame: TFrame);
  public
    procedure SelectPathFolder;
    procedure ApplyFrames;
    procedure Apply;
    procedure RetrieveFrames;
    procedure Retrieve;
  end;

function ShowProjectForm(vProject: TEditorProject): Boolean;

implementation

uses
  mneResources, SelectFiles;

{$R *.lfm}

function ShowProjectForm(vProject: TEditorProject): Boolean;
begin
  with TProjectForm.Create(Application) do
  begin
    Engine.BeginUpdate;
    try
      FProject := vProject;
      Retrieve;
      RetrieveFrames;
      PageControl.ActivePage := GeneralSheet;
      ActiveControl := NameEdit;
      Result := ShowModal = mrOk;
      if Result then
      begin
        Apply;
        ApplyFrames;
        Engine.Options.Apply;
        Engine.UpdateState([ecsOptions]);
      end;
    finally
      Engine.EndUpdate;
    end;
    Free;
  end;
end;

procedure TProjectForm.MenuItem1Click(Sender: TObject);
begin
  RootDirEdit.Text := Engine.BrowseFolder;
end;

procedure TProjectForm.Button4Click(Sender: TObject);
var
  s: string;
begin
  ShowSelectFile(FProject.RootDir, s);
  MainEdit.Text := s;
end;

procedure TProjectForm.MenuItem3Click(Sender: TObject);
begin
  SelectPathFolder;
end;

procedure TProjectForm.Apply;
begin
  FProject.Title := TitleEdit.Text;
  FProject.Name := NameEdit.Text;
  FProject.Description := DescriptionEdit.Text;
  FProject.RootDir := RootDirEdit.Text;
  FProject.SaveDesktop := SaveDesktopChk.Checked;
  FProject.SetSCMClass(TEditorSCM(SCMCbo.Items.Objects[SCMCbo.ItemIndex]));
  FProject.Options.MainFile := MainEdit.Text;
end;

procedure TProjectForm.RetrieveFrames;
var
  TabSheet: TTabSheet;
  i: Integer;
begin
  FProject.Options.CreateOptionsFrame(Self, FProject, @AddFrame);
  for i := 0 to Length(FFrames) - 1 do
  begin
    TabSheet := PageControl.AddTabSheet;
    TabSheet.Caption := FFrames[i].Caption;

    FFrames[i].Parent := TabSheet;
    FFrames[i].Align := alClient;
    FFrames[i].Visible := True;
    if Supports(FFrames[i], IEditorOptions) then
      (FFrames[i] as IEditorOptions).Retrieve;
  end;
end;

procedure TProjectForm.Retrieve;
begin
  if FProject.Tendency <> nil then
    Caption := Caption + ' [' + FProject.Tendency.Name + ']';
  TitleEdit.Text := FProject.Title;
  NameEdit.Text := FProject.Name;
  DescriptionEdit.Text := FProject.Description;
  RootDirEdit.Text := FProject.RootDir;
  SaveDesktopChk.Checked := FProject.SaveDesktop;
  if FProject.SCM <> nil then
    SCMCbo.ItemIndex := Engine.SourceManagements.IndexOf(FProject.SCM.Name) + 1
  else
    SCMCbo.ItemIndex := 0;
  MainEdit.Text := FProject.Options.MainFile;
end;

procedure TProjectForm.Button3Click(Sender: TObject);
begin
  SelectPathFolder;
end;

procedure TProjectForm.SelectPathFolder;
var
  aFolder: string;
begin
  aFolder := RootDirEdit.Text;
  if (aFolder = '') and (Engine.Files.Current <> nil) then
    aFolder := ExtractFilePath(Engine.Files.Current.Name);
  if SelectFolder('Select root directory for your project', '', aFolder) then
  begin
    RootDirEdit.Text := aFolder;
  end;
end;

procedure TProjectForm.ApplyFrames;
var
  i: Integer;
begin
  for i :=0 to Length(FFrames) - 1 do
    if Supports(FFrames[i], IEditorOptions) then
      (FFrames[i] as IEditorOptions).Apply;
end;

procedure TProjectForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  SCMCbo.Items.BeginUpdate;
  try
    SCMCbo.Items.Add('None');
    for i := 0 to Engine.SourceManagements.Count -1 do
    begin
      SCMCbo.Items.AddObject(Engine.SourceManagements[i].Title, Engine.SourceManagements[i]);
    end;
    SCMCbo.ItemIndex := 0;
  finally
    SCMCbo.Items.EndUpdate;
  end;
end;

procedure TProjectForm.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if not (csLoading in ComponentState) then //When createing the form when do not need to trigger
    if (PageControl.ActivePage = GeneralSheet) or (PageControl.ActivePage = OverrideOptionsSheet) then
      Apply;
end;

procedure TProjectForm.AddFrame(AFrame: TFrame);
begin
  SetLength(FFrames, Length(FFrames) + 1);
  FFrames[Length(FFrames)- 1] := AFrame;
end;

end.
