unit mnePasProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditorEngine, SelectFiles, mnePasClasses;

type

  { TPasProjectFrame }

  TPasProjectFrame = class(TFrame, IEditorOptions, IEditorProjectFrame)
    procedure Button4Click(Sender: TObject);
  private
  protected
  public
    Project: TEditorProject;
    function GetProject: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TPasProjectFrame }

procedure TPasProjectFrame.Button4Click(Sender: TObject);
begin
end;

function TPasProjectFrame.GetProject: TEditorProject;
begin
  Result := Project;
end;

procedure TPasProjectFrame.Apply;
begin
  with (Project.Options as TPasProjectOptions) do
  begin
  end;
end;

procedure TPasProjectFrame.Retrieve;
begin
  with (Project.Options as TPasProjectOptions) do
  begin
  end;
end;

end.

