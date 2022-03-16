unit mneBoardForms;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey 
 *}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Grids, ExtCtrls, StdCtrls, FileUtil,
  LCLType, Graphics, Menus, Buttons, ComCtrls, ValEdit, EditorEngine,
  SelectList, mneSelectComponents, mneBoardComponents, IniFiles, mnMsgBox,
  ntvBoard;

type

  { TBoardForm }

  TBoardForm = class(TFrame, IEditorControl)
    DesignImages: TImageList;
    ComponentsImages: TImageList;
    DesignToolBar: TToolBar;
    SelectBtn: TToolButton;
    RectBtn: TToolButton;
    CircleBtn: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    procedure ToolButton2Click(Sender: TObject);
  private
    FOnChanged: TNotifyEvent;
  protected
    FBoard: TntvBoard;
    NewComponentIndex: Integer;
    procedure Changed;
    procedure GetCreateElement(Sender: TObject; X, Y: Integer; var vElement: TElement);
  public
    constructor Create(TheOwner: TComponent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Board: TntvBoard read FBoard;
  end;

implementation

{$R *.lfm}

{ TBoardForm }

procedure TBoardForm.ToolButton2Click(Sender: TObject);
begin
  if not ShowSelectComponent('Components', BoardComponents, NewComponentIndex) then
    NewComponentIndex := -1;
end;

procedure TBoardForm.Changed;
begin
end;

procedure TBoardForm.GetCreateElement(Sender: TObject; X, Y: Integer; var vElement: TElement);
begin
  if NewComponentIndex >=0 then
  begin
    vElement := TComponentElement.Create(FBoard.CurrentLayout, X, Y);
    (vElement as TComponentElement).Component := BoardComponents[NewComponentIndex];
    (vElement as TComponentElement).CorrectSize;
    NewComponentIndex := -1;
  end
  else if RectBtn.Down then
    vElement := TRectangleElement.Create(FBoard.CurrentLayout, X, Y)
  else if CircleBtn.Down then
    vElement := TCircleElement.Create(FBoard.CurrentLayout, X, Y);
  SelectBtn.Down := True;
end;

constructor TBoardForm.Create(TheOwner: TComponent);
begin
  inherited;
  NewComponentIndex := -1;
  FBoard := TntvBoard.Create(Self);
  FBoard.Parent := Self;
  FBoard.Align := alClient;
  FBoard.OnGetCreateElement := @GetCreateElement;
end;

end.
