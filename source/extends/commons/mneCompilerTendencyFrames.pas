unit mneCompilerTendencyFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditorEngine, mnePasClasses;

type

  { TCompilerTendencyFrame }

  TCompilerTendencyFrame = class(TFrame, IEditorOptions)
    Button3: TButton;
    CompilerLabel: TLabel;
    CompilerEdit: TEdit;
    OpenDialog: TOpenDialog;
    procedure Button3Click(Sender: TObject);
  private
  protected
  public
    FTendency: TEditorTendency;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TCompilerTendencyFrame }

procedure TCompilerTendencyFrame.Button3Click(Sender: TObject);
begin
  OpenDialog.Filter := 'EXE files|*.exe|All files|*.*';
  OpenDialog.FileName := CompilerEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
  if OpenDialog.Execute then
  begin
    CompilerEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TCompilerTendencyFrame.Apply;
begin
  FTendency.Command := CompilerEdit.Text;
end;

procedure TCompilerTendencyFrame.Retrieve;
begin
  CompilerEdit.Text := FTendency.Command;
end;

end.
