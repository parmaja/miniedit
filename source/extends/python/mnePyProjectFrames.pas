unit mnePyProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, DebugClasses, mnePyClasses;

type

  { TPyProjectFrame }

  TPyProjectFrame = class(TFrame, IEditorOptions, IEditorProjectFrame)
    ExpandPathsChk1: TCheckBox;
    ExpandPathsChk2: TCheckBox;
  private
    Options: TPyProjectOptions;
  protected
    function GetProject: TEditorProject;
  public
    FProject: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TPyProjectFrame }

function TPyProjectFrame.GetProject: TEditorProject;
begin
  Result := FProject;
end;

procedure TPyProjectFrame.Apply;
begin
end;

procedure TPyProjectFrame.Retrieve;
begin
  Options := (FProject.Options as TPyProjectOptions);
end;

end.

