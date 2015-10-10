unit mneDProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditorEngine, SelectFiles, mneDClasses;

type

  { TDProjectFrame }

  TDProjectFrame = class(TFrame, IEditorFrame)
    Button4: TButton;
    CancelBtn: TButton;
    PathsLbl: TLabel;
    MainEdit: TEdit;
    Label2: TLabel;
    OkBtn: TButton;
    PathsEdit: TSynEdit;
    procedure Button4Click(Sender: TObject);
  private
  protected
  public
    //Options: TDProjectOptions;
    Project: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TDProjectFrame }

procedure TDProjectFrame.Button4Click(Sender: TObject);
var
  s: string;
begin
  ShowSelectFile(Project.Path, s);
  MainEdit.Text := s;
end;

procedure TDProjectFrame.Apply;
begin
  (Project.Options as TDProjectOptions).MainFile := MainEdit.Text;
  (Project.Options as TDProjectOptions).Paths.Assign(PathsEdit.Lines);
end;

procedure TDProjectFrame.Retrieve;
begin
  MainEdit.Text := (Project.Options as TDProjectOptions).MainFile;
  PathsEdit.Lines.Assign((Project.Options as TDProjectOptions).Paths);
end;

end.
