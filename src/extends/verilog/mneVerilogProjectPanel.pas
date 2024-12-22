unit mneVerilogProjectPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, EditorEngine, SelectFiles, mneVerilogClasses;

type

  { TVerilogProjectPanel }

  TVerilogProjectPanel = class(TFrame, IEditorOptions, IEditorProjectFrame)
    AddButton: TBitBtn;
    OpenDialog1: TOpenDialog;
    RemoveButton: TBitBtn;
    GroupBox1: TGroupBox;
    ProjectFileList: TListBox;
    Panel1: TPanel;
    procedure AddButtonClick(Sender: TObject);
    procedure ProjectFileListSelectionChange(Sender: TObject; User: boolean);
    procedure RemoveButtonClick(Sender: TObject);
  private
  protected
    function GetProject: TEditorProject;
  public
    Project: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TVerilogProjectPanel }

procedure TVerilogProjectPanel.ProjectFileListSelectionChange(Sender: TObject; User: boolean);
begin
  RemoveButton.Enabled:=(ProjectFileList.ItemIndex>=0);
end;

procedure TVerilogProjectPanel.AddButtonClick(Sender: TObject);
var
  path: string;
  i: SizeInt;
begin
  OpenDialog1.InitialDir:=Engine.GetRoot;

  if OpenDialog1.Execute then
  begin
    for i:=0 to OpenDialog1.Files.Count-1 do
    begin
      path:=ExtractRelativepath(IncludeTrailingBackslash(Engine.GetRoot),OpenDialog1.Files[i]);
      ProjectFileList.Items.Add(path);
    end;

    Apply();
  end;
end;

procedure TVerilogProjectPanel.RemoveButtonClick(Sender: TObject);
begin
  if ProjectFileList.SelCount>0 then
  begin
    ProjectFileList.DeleteSelected;

    Apply();
  end;
end;

function TVerilogProjectPanel.GetProject: TEditorProject;
begin
  Result := Project;
end;

procedure TVerilogProjectPanel.Apply;
begin
  with (Project.Options as TVerilogProjectOptions) do
  begin                      
    Files.Assign(ProjectFileList.Items);
  end;
end;

procedure TVerilogProjectPanel.Retrieve;
begin
  with (Project.Options as TVerilogProjectOptions) do
  begin
    ProjectFileList.Items.Assign(Files);
  end;
end;

end.

