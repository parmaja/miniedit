unit mneVerilogProjectPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditorEngine, SelectFiles, DebugClasses, mneVerilogClasses;

type

  { TVerilogProjectPanel }

  TVerilogProjectPanel = class(TFrame, IEditorOptions, IEditorProjectFrame)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button4Click(Sender: TObject);
  private
  protected
    function GetProject: TEditorProject;
  public
    FProject: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TVerilogProjectPanel }

procedure TVerilogProjectPanel.Button4Click(Sender: TObject);
begin
end;

function TVerilogProjectPanel.GetProject: TEditorProject;
begin
  Result := FProject;
end;

procedure TVerilogProjectPanel.Apply;
begin
  with (FProject.Options as TVerilogProjectOptions) do
  begin
    //UseCFG := UseCFGFileChk.Checked;
  end;
end;

procedure TVerilogProjectPanel.Retrieve;
begin
  with (FProject.Options as TVerilogProjectOptions) do
  begin
    //UseCFGFileChk.Checked := UseCFG;
  end;
end;

end.

