unit mneCustomProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, DebugClasses, mneCustomClasses;

type

  { TCustomProjectFrame }

  TCustomProjectFrame = class(TFrame, IEditorOptions)
    ExpandPathsChk1: TCheckBox;
    ExpandPathsChk2: TCheckBox;
  private
    Options: TCustomProjectOptions;
  protected
  public
    Project: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TCustomProjectFrame }

procedure TCustomProjectFrame.Apply;
begin
end;

procedure TCustomProjectFrame.Retrieve;
begin
  Options := (Project.Options as TCustomProjectOptions);
end;

end.

