unit mneConsoleForms;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{*TODO
  * No header grid
  * Sort column
  * Options on Tendency
  * View as text
  * Find and Replace
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Grids, ExtCtrls, StdCtrls,
  LCLType, Graphics, Menus, uCMDBox, EditorEngine, IniFiles,
  MsgBox, mnStreams;

type

  { TConsoleForm }

  TConsoleForm = class(TFrame)
    CMDBox: TCmdBox;
    ConsoleFooterPnl: TPanel;
    ContentPanel: TPanel;
  private
    FOnChanged: TNotifyEvent;
  protected
    FLoading: Boolean;
    procedure Changed;
  public
    constructor Create(TheOwner: TComponent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

{$R *.lfm}

procedure TConsoleForm.Changed;
begin
  if not FLoading and Assigned(FOnChanged) then
    FOnChanged(Self);
end;

constructor TConsoleForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  //CMDBox.Write('Ready!');
end;

end.

