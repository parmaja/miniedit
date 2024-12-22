unit mneGccProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, mneGccClasses;

type

  { TGccProjectFrame }

  TGccProjectFrame = class(TFrame, IEditorOptions)
  private
    Options: TGccProjectOptions;
  protected
  public
    Project: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TGccProjectFrame }

procedure TGccProjectFrame.Apply;
begin
end;

procedure TGccProjectFrame.Retrieve;
begin
  Options := (Project.Options as TGccProjectOptions);
end;

end.

