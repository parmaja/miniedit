unit mneProjectOptions;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, SynEdit,
  EditorEngine, EditorRun, mneClasses, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus;

type

  { TProjectForm }

  TProjectForm = class(TForm)
    Button3: TButton;
    Button4: TButton;
    DescriptionEdit: TMemo;
    IgnoreNamesEdit: TEdit;
    GroupBox1: TGroupBox;
    IndentModeCbo: TComboBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MainEdit: TEdit;
    GeneralOptionsSheet: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MainPathMnu: TMenuItem;
    NameEdit: TEdit;
    FileFilterEdit: TEdit;
    OverrideOptionsChk: TCheckBox;
    MainPathEdit: TEdit;
    SaveDesktopChk: TCheckBox;
    SCMCbo: TComboBox;
    TabWidthEdit: TEdit;
    TitleEdit: TEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    PageControl: TPageControl;
    PathPopupMenu: TPopupMenu;
    GeneralSheet: TTabSheet;
    procedure Button4Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MainPathMnuClick(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
  private
    FProject: TEditorProject;
    FFrames: array of TFrame;
  protected
    procedure AddFrame(AFrame: TFrame);
  public
    procedure SelectPathFolder;
    procedure ApplyFrames;
    procedure RetrieveFrames;
    procedure Apply;
    procedure Retrieve;
  end;

function ShowProjectForm(vProject: TEditorProject): Boolean;

implementation

uses
  EditorClasses, EditorProfiles, mneResources, SelectFiles;

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
      if FProject is TDefaultProject then
      begin
        GeneralSheet.Visible := False;
        PageControl.Pages[0].TabVisible := False;
        PageControl.ActivePage := GeneralOptionsSheet;
      end
      else
      begin
        PageControl.ActivePage := GeneralSheet;
        ActiveControl := NameEdit;
      end;
      Result := ShowModal = mrOk;
      if Result then
      begin
        Apply;
        ApplyFrames;
        Engine.Options.Apply;
        Engine.Update([ecsOptions]);
      end;
    finally
      Engine.EndUpdate;
    end;
    Free;
  end;
end;

procedure TProjectForm.MenuItem1Click(Sender: TObject);
begin
  MainPathEdit.Text := Engine.BrowseFolder;
end;

procedure TProjectForm.MenuItem2Click(Sender: TObject);
begin
  (PathPopupMenu.PopupComponent as TEdit).SelText := '?ProjectPath';
end;

procedure TProjectForm.Button4Click(Sender: TObject);
var
  s: string;
begin
  ShowSelectFile(FProject.RunOptions.MainPath, s);
  MainEdit.Text := s;
end;

procedure TProjectForm.MenuItem3Click(Sender: TObject);
begin
  SelectPathFolder;
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
  MainPathEdit.Text := FProject.RunOptions.MainPath;
  SaveDesktopChk.Checked := FProject.SaveDesktop;
  if FProject.SCM <> nil then
    SCMCbo.ItemIndex := Engine.SourceManagements.IndexOfName(FProject.SCM.Name) + 1
  else
    SCMCbo.ItemIndex := 0;
  MainEdit.Text := FProject.RunOptions.MainFile;

  //Add any new overrided options to cSynOverridedOptions in EditorProfiles unit
  EnumIndentMode(IndentModeCbo.Items);
  OverrideOptionsChk.Checked := FProject.Options.OverrideEditorOptions;
  TabWidthEdit.Text := IntToStr(FProject.Options.TabWidth);
  IndentModeCbo.ItemIndex := Ord(FProject.Options.IndentMode);
  FileFilterEdit.Text := FProject.FileFilter;
  IgnoreNamesEdit.Text := FProject.IgnoreNames;
end;

procedure TProjectForm.Apply;
begin
  FProject.Title := TitleEdit.Text;
  FProject.Name := NameEdit.Text;
  FProject.Description := DescriptionEdit.Text;
  FProject.RunOptions.MainPath := MainPathEdit.Text;
  FProject.SaveDesktop := SaveDesktopChk.Checked;
  FProject.SetSCMClass(TEditorSCM(SCMCbo.Items.Objects[SCMCbo.ItemIndex]));
  FProject.RunOptions.MainFile := MainEdit.Text;

  //FProject.Options.EditorOptions := [];
  FProject.Options.OverrideEditorOptions := OverrideOptionsChk.Checked;
  FProject.Options.TabWidth := StrToIntDef(TabWidthEdit.Text, 4);
  FProject.Options.IndentMode := TIndentMode(IndentModeCbo.ItemIndex);
  FProject.FileFilter := FileFilterEdit.Text;
  FProject.IgnoreNames := IgnoreNamesEdit.Text;
  FProject.UpdatePath;
end;

procedure TProjectForm.Button3Click(Sender: TObject);
begin
  SelectPathFolder;
end;

procedure TProjectForm.SelectPathFolder;
var
  aFolder: string;
begin
  aFolder := MainPathEdit.Text;
  if (aFolder = '') and (Engine.Files.Current <> nil) then
    aFolder := ExtractFilePath(Engine.Files.Current.FileName);
  if SelectFolder('Select root directory for your project', '', aFolder) then
  begin
    MainPathEdit.Text := aFolder;
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

procedure TProjectForm.MainPathMnuClick(Sender: TObject);
begin
  (PathPopupMenu.PopupComponent as TEdit).SelText := '?MainPath';
end;

procedure TProjectForm.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if not (csLoading in ComponentState) then //When createing the form when do not need to trigger
    if (PageControl.ActivePage = GeneralSheet) or (PageControl.ActivePage = GeneralOptionsSheet) then
      Apply;
end;

procedure TProjectForm.AddFrame(AFrame: TFrame);
begin
  SetLength(FFrames, Length(FFrames) + 1);
  FFrames[Length(FFrames)- 1] := AFrame;
end;

end.
