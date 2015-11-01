unit mnePasProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditorEngine, SelectFiles, DebugClasses, mnePasClasses;

type

  { TPasProjectFrame }

  TPasProjectFrame = class(TFrame, IEditorOptions)
    UseCFGFileChk: TCheckBox;
    procedure Button4Click(Sender: TObject);
  private
  protected
  public
    Project: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TPasProjectFrame }

procedure TPasProjectFrame.Button4Click(Sender: TObject);
begin
end;

procedure TPasProjectFrame.Apply;
begin
  with (Project.Options as TPasProjectOptions) do
  begin
    UseCFG := UseCFGFileChk.Checked;
  end;
end;

procedure TPasProjectFrame.Retrieve;
begin
  with (Project.Options as TPasProjectOptions) do
  begin
    UseCFGFileChk.Checked := UseCFG;
  end;
end;

end.

