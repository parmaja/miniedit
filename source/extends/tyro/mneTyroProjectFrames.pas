unit mneTyroProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, mneTyroClasses;

type

  { TTyroProjectFrame }

  TTyroProjectFrame = class(TFrame, IEditorOptions, IEditorProjectFrame)
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

{ TTyroProjectFrame }

function TTyroProjectFrame.GetProject: TEditorProject;
begin
  Result := Project;
end;

procedure TTyroProjectFrame.Apply;
begin
end;

procedure TTyroProjectFrame.Retrieve;
begin
end;

end.
