unit mnePyProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, DebugClasses, mnePyClasses;

type

  { TPyProjectFrame }

  TPyProjectFrame = class(TFrame, IEditorOptions)
    ExpandPathsChk1: TCheckBox;
    ExpandPathsChk2: TCheckBox;
  private
    Options: TPyProjectOptions;
  protected
  public
    Project: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TPyProjectFrame }

procedure TPyProjectFrame.Apply;
begin
end;

procedure TPyProjectFrame.Retrieve;
begin
  Options := (Project.Options as TPyProjectOptions);
end;

end.

