unit mneLuaTendencyFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditorEngine;

type

  { TLuaTendencyFrame }

  TLuaTendencyFrame = class(TFrame, IEditorOptions)
    Button3: TButton;
    Label1: TLabel;
    CompilerEdit: TEdit;
    Label6: TLabel;
    OpenDialog: TOpenDialog;
    RequireEdit: TEdit;
    procedure Button3Click(Sender: TObject);
  private
  protected
  public
    FTendency: TEditorTendency;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

uses
  mneLuaClasses;

{$R *.lfm}

{ TLuaTendencyFrame }

procedure TLuaTendencyFrame.Button3Click(Sender: TObject);
begin
  OpenDialog.Filter := 'EXE files|*.exe|All files|*.*';
  OpenDialog.FileName := CompilerEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
  if OpenDialog.Execute then
  begin
    CompilerEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TLuaTendencyFrame.Apply;
begin
  (FTendency as TLuaTendency).Command := CompilerEdit.Text;
  (FTendency as TLuaTendency).Require := RequireEdit.Text;
end;

procedure TLuaTendencyFrame.Retrieve;
begin
  CompilerEdit.Text := (FTendency as TLuaTendency).Command;
  RequireEdit.Text := (FTendency as TLuaTendency).Require;
end;

end.

