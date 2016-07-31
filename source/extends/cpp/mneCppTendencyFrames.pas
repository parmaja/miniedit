unit mneCppTendencyFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditorEngine, mneDClasses;

type

  { TDTendencyFrame }

  TCppTendencyFrame = class(TFrame, IEditorOptions)
    Button3: TButton;
    Label1: TLabel;
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

{ TCppTendencyFrame }

procedure TCppTendencyFrame.Button3Click(Sender: TObject);
begin
  OpenDialog.Filter := 'EXE files|*.exe|All files|*.*';
  OpenDialog.FileName := CompilerEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
  if OpenDialog.Execute then
  begin
    CompilerEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TCppTendencyFrame.Apply;
begin
  (FTendency as TDTendency).Command := CompilerEdit.Text;
end;

procedure TCppTendencyFrame.Retrieve;
begin
  CompilerEdit.Text := (FTendency as TDTendency).Command;
end;

end.

