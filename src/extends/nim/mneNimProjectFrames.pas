unit mneNimProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, mneNimClasses;

type

  { TNimProjectFrame }

  TNimProjectFrame = class(TFrame, IEditorOptions, IEditorProjectFrame)
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

{ TNimProjectFrame }

function TNimProjectFrame.GetProject: TEditorProject;
begin
  Result := Project;
end;

procedure TNimProjectFrame.Apply;
begin
end;

procedure TNimProjectFrame.Retrieve;
begin
end;

end.

