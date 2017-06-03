unit mneBoardForms;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Grids, ExtCtrls, StdCtrls, FileUtil,
  LCLType, Graphics, Menus, Buttons, ComCtrls, ValEdit, EditorEngine,
  SelectList, mneSelectComponents, IniFiles, MsgBox, ntvBoard;

type

  { TBoardForm }

  TBoardForm = class(TFrame, IEditorControl)
    DesignImages: TImageList;
    ComponentsImages: TImageList;
    DesignToolBar: TToolBar;
    ToolButton1: TToolButton;
    RectBtn: TToolButton;
    PolygnBtn: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    procedure CoolBar1Change(Sender: TObject);
    procedure DesignToolBarClick(Sender: TObject);
    procedure PolygnBtnClick(Sender: TObject);
    procedure RectBtnClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ValueListEditor1Click(Sender: TObject);
  private
    FOnChanged: TNotifyEvent;
  protected
    FBoard: TntvBoard;
    procedure Changed;
  public
    constructor Create(TheOwner: TComponent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    function GetMainControl: TWinControl;
  end;

implementation

{$R *.lfm}

{ TBoardForm }

procedure TBoardForm.CoolBar1Change(Sender: TObject);
begin

end;

procedure TBoardForm.DesignToolBarClick(Sender: TObject);
begin

end;

procedure TBoardForm.PolygnBtnClick(Sender: TObject);
begin
  FBoard.NextElement := TCircleElement;
end;

procedure TBoardForm.RectBtnClick(Sender: TObject);
begin
  FBoard.NextElement := TRectangleElement;
end;

procedure TBoardForm.ToolButton1Click(Sender: TObject);
begin
  FBoard.NextElement := nil;
end;

procedure EnumFilesCallback(AObject: TObject; const FileName: string; Count, Level:Integer; IsDirectory: Boolean; var Resume: Boolean);
var
  Elements: TEditorElements;
  Item: TEditorElement;
begin
  Elements := (AObject as TEditorElements);
  Item := TEditorElement.Create;
  Item.Name := FileName;
  Item.Title := ExtractFileName(FileName);
  Elements.Add(Item);
end;

procedure TBoardForm.ToolButton2Click(Sender: TObject);
var
  Elements: TEditorElements;
  i: Integer;
begin
  Elements := TEditorElements.Create;
  try
    EnumFileList(Application.Location + 'components\', '*.*', '', @EnumFilesCallback, Elements, 0, 1, true, [fftDir]);
    ShowSelectComponent('Components', Elements , [slfUseNameTitle], i);
  finally
    Elements.Free;
  end;
end;

procedure TBoardForm.ValueListEditor1Click(Sender: TObject);
begin

end;

procedure TBoardForm.Changed;
begin

end;

constructor TBoardForm.Create(TheOwner: TComponent);
begin
  inherited;
  FBoard := TntvBoard.Create(Self);
  FBoard.Parent := Self;
  FBoard.Align := alClient;
end;

function TBoardForm.GetMainControl: TWinControl;
begin
  Result := FBoard;
end;

end.

