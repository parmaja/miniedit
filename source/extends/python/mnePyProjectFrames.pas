unit mnePyProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, mnePyClasses;

type

  { TPyProjectFrame }

  TPyProjectFrame = class(TFrame, IEditorOptions, IEditorProjectFrame)
    ExpandPathsChk1: TCheckBox;
    ExpandPathsChk2: TCheckBox;
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

{ TPyProjectFrame }

function TPyProjectFrame.GetProject: TEditorProject;
begin
  Result := Project;
end;

procedure TPyProjectFrame.Apply;
begin
end;

procedure TPyProjectFrame.Retrieve;
begin
end;

end.

