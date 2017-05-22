{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
unit DesignerBoard;

interface

uses
  Messages, SysUtils, Classes, Controls, LCLIntf,
  Forms, Graphics, Contnrs, ImgList,
  StdCtrls, ExtCtrls;

const
  OblongCursors: array[0..7] of TCursor = (crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE);
  cHaftSize = 4;

type
  //THafts = ()

  TPointArray = array of TPoint;

  TElementStyle = set of (trtSnap, trtMove, trtSize);

  TLayout = class;
  TLayouts = class;
  TElement = class;
  TElementList = class;
  TContainer = class;

  { TElement }

  TElement = class(TComponent)
  private
    FCaptured: Boolean;
    FDesignX: Integer;
    FDesignY: Integer;
    FColor: TColor;
    FStyle: TElementStyle;
    FHaftList: TPointArray;
    FHaftIndex: Integer;
    FContainer: TContainer;
    FModified: Integer;
    function GetSelected: Boolean;
    procedure SetSelected(const Value: Boolean);
    procedure SetModified(const Value: Boolean);
    procedure SetContainer(const Value: TContainer);
    function GetModified: Boolean;
  protected

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure BeginModify; virtual;
    procedure Modifing; virtual;
    procedure EndModify; virtual;
    procedure Change; virtual;
    function GetClientRect: TRect; virtual;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor CreateBy(AOwner: TComponent; X: Integer = 0; Y: Integer = 0); overload; virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); virtual;
    function PtInHaft(X, Y: Integer; out vHaftIndex: Integer): Boolean; overload; virtual;
    function PtInHaft(X, Y: Integer): Boolean; overload;
    procedure CatchMouse(X, Y: Integer); virtual;
    property Captured: Boolean read FCaptured write FCaptured;
    property Modified: Boolean read GetModified write SetModified;
    procedure Refresh; virtual;
    procedure Invalidate; virtual;

    procedure SetCursor(Shift: TShiftState; X, Y: Integer); virtual;
    procedure Modify(Shift: TShiftState; X, Y: Integer); virtual;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); virtual;
    procedure PaintHaftList(vCanvas: TCanvas); virtual;
    procedure PaintHaft(vCanvas: TCanvas; P: TPoint);
    function PtToHaftRect(P: TPoint): TRect;
    procedure CreateHaftList; virtual;
    procedure Move(DX, DY: Integer); virtual;
    function HitTest(X, Y: Integer): Boolean; virtual;
    property Style: TElementStyle read FStyle write FStyle;
    property Color: TColor read FColor write FColor;
    property Selected: Boolean read GetSelected write SetSelected;
    property DesignX: Integer read FDesignX;
    property DesignY: Integer read FDesignY;
    property Container: TContainer read FContainer write SetContainer;
  end;

  TElementClass = class of TElement;

  TElementList = class(TObjectList)
  private
    function GetItem(Index: Integer): TElement;
    procedure SetItem(Index: Integer; const Value: TElement);
  public
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    property Items[Index: Integer]: TElement read GetItem write SetItem; default;
  end;

  TCustomBoard = class;

  TContainer = class(TComponent)
  private
    FElementList: TElementList;
    FBoundRect: TRect;
    function GetCursor: TCursor;
    procedure SetCursor(const Value: TCursor);
    procedure ReadElement(Reader: TReader);
    procedure WriteElement(Writer: TWriter);
    function GetHeight: Integer;
    function GetWidth: Integer;
  protected
    procedure SetBoundRect(const Value: TRect); virtual;
  public
    Board: TCustomBoard;
    Index: Integer;
    procedure Clear; virtual;
    procedure Refresh; virtual;
    procedure Change; virtual;
    function GetLayoutByPoint(X, Y: Integer): TLayout; virtual;
    function GetLayoutByIndex(vIndex: Integer): TLayout; virtual;
    procedure ExcludeClipRect(vCanvas: TCanvas); virtual;
    procedure InvalidateRect(const vRect: TRect); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init; virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(vFileName: String); virtual;
    procedure SaveToFile(vFileName: String); virtual;
    function HitTest(X, Y: Integer; out vElement: TElement): Boolean; virtual;
    procedure Paint(vCanvas: TCanvas); virtual;
    procedure PaintBackground(vCanvas: TCanvas); virtual;
    property ElementList: TElementList read FElementList;
    property BoundRect: TRect read FBoundRect write SetBoundRect;
    property Cursor: TCursor read GetCursor write SetCursor;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  { TLayout }

  TLayout = class(TContainer)
  private
    FLayouts: TLayouts;
    FEnabled: Boolean;
    FName: string;
  protected
    procedure SetBoundRect(const Value: TRect); override;
  public
    procedure ExcludeClipRect(vCanvas: TCanvas); override;
    function GetLayoutByIndex(vIndex: Integer): TLayout; override;
    function GetLayoutByPoint(X, Y: Integer): TLayout; override;
    constructor Create(AOwner: TComponent); override;
    procedure PaintBackground(vCanvas: TCanvas); override;
    property Name: string read FName write FName;
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
  end;

  TLayoutList = class(TObjectList)
  private
    function GetItem(Index: Integer): TLayout;
    procedure SetItem(Index: Integer; AObject: TLayout);
  public
    property Items[Index: Integer]: TLayout read GetItem write SetItem; default;
  end;

  { TLayouts }

  TLayouts = class(TContainer)
  private
    FLayoutList: TLayoutList;
    FCaption: String;
    FBackground: TColor;
    procedure ReadBoards(Reader: TReader);
    procedure WriteBoards(Writer: TWriter);
    function GetItem(vIndex: Integer): TLayout;
    procedure SetItem(vIndex: Integer; AValue: TLayout);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromFile(vFileName: String); override;
    procedure SaveToFile(vFileName: String); override;
    function HitTest(X, Y: Integer; out vElement: TElement): Boolean; override;
    procedure Paint(vCanvas: TCanvas); override;
    procedure PaintBackground(vCanvas: TCanvas); override;
    procedure ExcludeClipRect(vCanvas: TCanvas); override;
    property LayoutList: TLayoutList read FLayoutList;
    property Background: TColor read FBackground write FBackground;
    property Items[vIndex: Integer]: TLayout read GetItem write SetItem; default;
  published
    property Caption: String read FCaption write FCaption;
  end;

  { TCustomBoard }

  TCustomBoard = class(TCustomControl)
  private
    FCurrentLayout: TContainer;
    FLayouts: TLayouts;
    FDesignElement: TElement;
    procedure SetDesignElement(const Value: TElement);
    procedure SetCurrentLayout(const Value: TContainer);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;

    procedure Resize; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Change; virtual;
    procedure Reset; virtual;
  public
    NextElement: TElementClass;
    constructor Create(ABoard: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property DesignElement: TElement read FDesignElement write SetDesignElement;
    property CurrentLayout: TContainer read FCurrentLayout write SetCurrentLayout;
  published
    property Color default clWindow;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
    property Caption;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Visible;
  end;

  TDesignerBoard = class(TCustomBoard)
  end;

//-----------------------------------

  { TPolygonElement }

  TPolygonElement = class(TElement)
  private
    Polygon: TPointArray;
  public
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    procedure Move(DX, DY: Integer); override;
    function HitTest(X, Y: Integer): Boolean; override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
    procedure CreateHaftList; override;
    procedure PaintHaftList(vCanvas: TCanvas); override;
  end;

  TCariesElement = class(TPolygonElement)
  public
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
  end;

  { TOblongElement }

  TOblongElement = class(TElement)
  private
    FBoundRect: TRect;
    function GetWidth: Integer;
    function GetHeight: Integer;
  protected
    procedure BeginModify; override;
    procedure EndModify; override;
  public
    procedure Loaded; override;
    procedure CreateHaftList; override;
    function PtInHaft(X, Y: Integer; out vHaftIndex: Integer): Boolean; override;
    procedure SetCursor(Shift: TShiftState; X, Y: Integer); override;
    function HitTest(X, Y: Integer): Boolean; override;

    procedure Move(DX, DY: Integer); override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  published
    property Top: Integer read FBoundRect.Top write FBoundRect.Top;
    property Left: Integer read FBoundRect.Left write FBoundRect.Left;
    property Right: Integer read FBoundRect.Right write FBoundRect.Right;
    property Bottom: Integer read FBoundRect.Bottom write FBoundRect.Bottom;
  end;

  { TEllipseElement }

  TEllipseElement = class(TOblongElement)
  public
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
  end;

  { TRectangleElement }

  TRectangleElement = class(TOblongElement)
  public
    constructor Create(AOwner: TComponent); overload; override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
    property Color: TColor read FColor write FColor default clBlue;
  end;

  { TDebateElement }

  TDebateElement = class(TPolygonElement)
  public
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
  end;

  { THeavyElement }

  THeavyElement = class(TElement)
  private
    function GetBounds: TRect;
  protected
    DesignRect: TRect;
  public
    procedure Move(DX, DY: Integer); override;
    procedure CreateHaftList; override;
    function PtInHaft(X, Y: Integer; out vHaftIndex: Integer): Boolean; override;
    procedure SetCursor(Shift: TShiftState; X, Y: Integer); override;
    function HitTest(X, Y: Integer): Boolean; override;
    procedure EndModify; override;
    procedure DoPaint(vCanvas: TCanvas; vRect: TRect); virtual;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
    constructor Create(AOwner: TComponent); override;
    constructor CreateBy(AOwner: TComponent; X: Integer = 0; Y: Integer = 0); override;
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
  end;

procedure Register;
procedure RegisterElements(const Layout: String; TElements: array of TElementClass);

var
  ElementClasses: TList;

implementation

uses
  Types;

procedure Register;
begin
  RegisterComponents('Native', [TDesignerBoard]);
end;

procedure CorrectRect(var vRect: TRect);
var
  i: Integer;
begin
  if vRect.Left > vRect.Right then
  begin
    i := vRect.Left;
    vRect.Left := vRect.Right;
    vRect.Right := i;
  end;
  if vRect.Top > vRect.Bottom then
  begin
    i := vRect.Top;
    vRect.Top := vRect.Bottom;
    vRect.Bottom := i;
  end;
end;

procedure RegisterElements(const Layout: String; TElements: array of TElementClass);
var
  i: Integer;
begin
  for i := 0 to Length(TElements) - 1 do
  begin
    ElementClasses.Add(TElements[i]);
    RegisterClass(TElements[i]);
  end;
end;

{ TLayouts }

procedure TLayouts.Assign(Source: TPersistent);
begin
  inherited;
end;

procedure TLayouts.Clear;
var
  i: Integer;
begin
  for i := 0 to FLayoutList.Count - 1 do
  begin
    FLayoutList[i].Clear;
  end;
  inherited;
end;

constructor TLayouts.Create(AOwner: TComponent);
begin
  inherited;
  Background := $00DEE9FA;
  FLayoutList := TLayoutList.Create;
  with TLayout.Create(Self) do
  begin
    FName := 'Top';
  end;
  {with TLayout.Create(Self) do
  begin
    FName := 'Bottom';
  end;}
end;

destructor TLayouts.Destroy;
begin
  FreeAndNil(FLayoutList);
  inherited;
end;

procedure TLayouts.ExcludeClipRect(vCanvas: TCanvas);
var
  i: Integer;
begin
  for i := 0 to FLayoutList.Count - 1 do
  begin
    FLayoutList[i].ExcludeClipRect(vCanvas);
  end;
end;

function TLayouts.HitTest(X, Y: Integer; out vElement: TElement): Boolean;
var
  i: Integer;
begin
  Result := inherited HitTest(X, Y, vElement);
  if not Result then
    for i := 0 to FLayoutList.Count - 1 do
    begin
      Result := FLayoutList[i].HitTest(X, Y, vElement);
      if Result then
        break;
    end;
end;

procedure TLayouts.LoadFromFile(vFileName: String);
var
  aFile: TFileStream;
begin
  aFile := TFileStream.Create(vFileName, fmOpenRead);
  try
    LoadFromStream(aFile);
  finally
    aFile.Free;
  end;
end;

procedure TLayouts.Paint(vCanvas: TCanvas);
var
  i: Integer;
begin
  inherited;
  for i := 0 to FLayoutList.Count - 1 do
  begin
    FLayoutList[i].Paint(vCanvas);
  end;
end;

procedure TLayouts.PaintBackground(vCanvas: TCanvas);
var
  i: Integer;
begin
  inherited;
  for i := 0 to FLayoutList.Count - 1 do
  begin
    FLayoutList[i].PaintBackground(vCanvas);
  end;
end;

procedure TLayouts.ReadBoards(Reader: TReader);
var
  aLayout: TLayout;
  i: Integer;
begin
  Reader.ReadListBegin;
  i := 0;
  while not Reader.EndOfList do
  begin
    aLayout := LayoutList[i];
    Reader.ReadComponent(aLayout);
    Inc(i);
  end;
  Reader.ReadListEnd;
end;

function TLayouts.GetItem(vIndex: Integer): TLayout;
begin
  Result := FLayoutList[vIndex];
end;

procedure TLayouts.SetItem(vIndex: Integer; AValue: TLayout);
begin
  FLayoutList[vIndex] := AValue;
end;

procedure TLayouts.SaveToFile(vFileName: String);
var
  aFile: TFileStream;
begin
  aFile := TFileStream.Create(vFileName, fmCreate);
  try
    SaveToStream(aFile);
  finally
    aFile.Free;
  end;
end;

procedure TLayouts.WriteBoards(Writer: TWriter);
var
  i: Integer;
begin
  Writer.WriteListBegin;
  for i := 0 to FLayoutList.Count - 1 do
  begin
    Writer.WriteComponent(FLayoutList[i]);
  end;
  Writer.WriteListEnd;
end;

{ TLayoutList }

function TLayoutList.GetItem(Index: Integer): TLayout;
begin
  Result := TLayout(inherited Items[Index]);
end;

procedure TLayoutList.SetItem(Index: Integer; AObject: TLayout);
begin
  inherited Items[Index] := AObject;
end;

{ TCustomBoard }

procedure TCustomBoard.Change;
begin
end;

constructor TCustomBoard.Create(ABoard: TComponent);
begin
  inherited;
  //ControlStyle := ControlStyle + [csOpaque];
  DoubleBuffered := False;
  FLayouts := TLayouts.Create(Self);
  FLayouts.Init;
  FCurrentLayout := FLayouts[0];
  Width := 100;
  Height := 100;
  Color := clWindow;
  NextElement := TRectangleElement;
end;

destructor TCustomBoard.Destroy;
begin
  inherited;
end;

procedure TCustomBoard.DoEnter;
begin
  inherited;
  //PaintHaftList;
end;

procedure TCustomBoard.DoExit;
begin
  inherited;
  //  PaintHaftList;
end;

procedure TCustomBoard.Loaded;
begin
  inherited;
  CurrentLayout.BoundRect := ClientRect;
end;

procedure TCustomBoard.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aElement: TElement;
begin
  inherited;
  if NextElement <> nil then
  begin
    DesignElement := NextElement.CreateBy(CurrentLayout, X, Y);
    //NextElement := nil;
  end
  else if (DesignElement = nil) or not ((DesignElement.Captured) or (DesignElement.PtInHaft(X, Y))) then
  begin
    CurrentLayout.HitTest(X, Y, aElement);
    DesignElement := aElement;
  end;
  if DesignElement <> nil then
    DesignElement.MouseDown(Button, Shift, X, Y);
end;

//BaseMouseMove

procedure TCustomBoard.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  aElement: TElement;
begin
  inherited;
  aElement := DesignElement;
  if (aElement = nil) or not ((aElement.Captured) or (aElement.PtInHaft(X, Y))) then
    CurrentLayout.HitTest(X, Y, aElement);
  if aElement <> nil then
    aElement.MouseMove(Shift, X, Y)
  else
    Cursor := crDefault;
end;

procedure TCustomBoard.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if DesignElement <> nil then
    DesignElement.MouseUp(Button, Shift, X, Y);
end;

//BasePaint

procedure TCustomBoard.Paint;
begin
  inherited;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  //aRect := Canvas.ClipRect;
  Canvas.Brush.Color := clWindow;
  //Canvas.FillRect(ClientRect);
  FLayouts.PaintBackground(Canvas);
  FLayouts.Paint(Canvas);
end;

procedure TCustomBoard.Reset;
begin
end;

procedure TCustomBoard.Resize;
begin
  inherited;
  FLayouts.BoundRect := ClientRect;
  if not (csLoading in ComponentState) then
  begin
    FLayouts.BoundRect := ClientRect;
  end;
end;

procedure TCustomBoard.SetCurrentLayout(const Value: TContainer);
begin
  FCurrentLayout := Value;
  Refresh;
end;

{ TElementList }

function TElementList.GetItem(Index: Integer): TElement;
begin
  Result := TElement(inherited Items[Index]);
end;

procedure TElementList.MouseMove(Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TElementList.SetItem(Index: Integer; const Value: TElement);
begin
  inherited Items[Index] := Value;
end;

{ TElement }

procedure TElement.CatchMouse(X, Y: Integer);
begin
  FDesignX := X;
  FDesignY := Y;
end;

procedure TElement.CreateHaftList;
begin
end;

function TElement.HitTest(X, Y: Integer): Boolean;
begin
  Result := False;
end;

procedure TElement.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CatchMouse(X, Y);
  PtInHaft(X, Y, FHaftIndex);
  Captured := True;
end;

procedure TElement.Modify(Shift: TShiftState; X, Y: Integer);
begin
  Move(X - FDesignX, Y - FDesignY);
  Modifing;
end;

procedure TElement.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Captured then
  begin
    if not Modified then
      Modified := True;
    Modify(Shift, X, Y);
  end
  else
  begin
    PtInHaft(X, Y, FHaftIndex);
    SetCursor(Shift, X, Y);
  end;
  CatchMouse(X, Y);
end;

procedure TElement.SetCursor(Shift: TShiftState; X, Y: Integer);
begin
  Container.Cursor := crDefault;
end;

procedure TElement.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Captured := False;
  FHaftIndex := -1;
  CatchMouse(X, Y);
  if Modified then
    Modified := False
  else
    Invalidate;
end;

procedure TElement.Move(DX, DY: Integer);
begin

end;

procedure TElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin

end;

procedure TElement.PaintHaft(vCanvas: TCanvas; P: TPoint);
begin
  vCanvas.Rectangle(PtToHaftRect(P));
end;

procedure TElement.PaintHaftList(vCanvas: TCanvas);
var
  i: Integer;
begin
  CreateHaftList;
  inherited;
  vCanvas.Brush.Color := clBlack;
  for i := 0 to Length(FHaftList) - 1 do
  begin
    PaintHaft(vCanvas, FHaftList[i]);
  end;
end;

procedure TElement.Refresh;
begin
  Container.Refresh;
end;

{ TCariesElement }

procedure TCariesElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
  inherited;
  SetLength(Polygon, 3);
  Polygon[0].X := 0;
  Polygon[0].Y := 0;
  Polygon[1].X := 30;
  Polygon[1].Y := 30;
  Polygon[2].X := -30;
  Polygon[2].Y := 30;
end;

procedure TCariesElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;
  vCanvas.Brush.Color := clGreen;
  vCanvas.Polygon(Polygon);
end;

{ TOblongElement }

procedure TOblongElement.CreateHaftList;
begin
  inherited;
  Setlength(FHaftList, 8);
  FHaftList[0].X := Left;
  FHaftList[0].Y := Top;
  FHaftList[1].X := Left + Width div 2;
  FHaftList[1].Y := Top;
  FHaftList[2].X := Right;
  FHaftList[2].Y := Top;
  FHaftList[3].X := Right;
  FHaftList[3].Y := Top + Height div 2;
  FHaftList[4].X := Right;
  FHaftList[4].Y := Bottom;
  FHaftList[5].X := Left + Width div 2;
  FHaftList[5].Y := Bottom;
  FHaftList[6].X := Left;
  FHaftList[6].Y := Bottom;
  FHaftList[7].X := Left;
  FHaftList[7].Y := Top + Height div 2;
end;

function TOblongElement.HitTest(X, Y: Integer): Boolean;
begin
  if PtInRect(FBoundRect, Point(X, Y)) then
    Result := True
  else
    Result := False;
end;

function TOblongElement.GetHeight: Integer;
begin
  Result := Bottom - Top;
end;

function TOblongElement.GetWidth: Integer;
begin
  Result := Right - Left;
end;

procedure TOblongElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;
end;

procedure TOblongElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
  inherited;
  Color := clRed;
  if Dummy then
  begin
    FHaftIndex := 4;
    FBoundRect := Rect(X, Y, X, Y);
  end
  else
    FBoundRect := Rect(X, Y, X + 20, Y + 20);
end;

procedure TOblongElement.Move(DX, DY: Integer);
begin
  inherited;
  Invalidate;
  case FHaftIndex of
    -1:
    begin
      OffsetRect(FBoundRect, DX, DY);
    end;
    0:
    begin
      Inc(FBoundRect.Left, DX);
      Inc(FBoundRect.Top, DY);
    end;
    1:
    begin
      Inc(FBoundRect.Top, DY);
    end;
    2:
    begin
      Inc(FBoundRect.Top, DY);
      Inc(FBoundRect.Right, DX);
    end;
    3:
    begin
      Inc(FBoundRect.Right, DX);
    end;
    4:
    begin
      Inc(FBoundRect.Bottom, DY);
      Inc(FBoundRect.Right, DX);
    end;
    5:
    begin
      Inc(FBoundRect.Bottom, DY);
    end;
    6:
    begin
      Inc(FBoundRect.Bottom, DY);
      Inc(FBoundRect.Left, DX);
    end;
    7:
    begin
      Inc(FBoundRect.Left, DX);
    end;
  end;
  Invalidate;
end;

function TOblongElement.PtInHaft(X, Y: Integer; out vHaftIndex: Integer): Boolean;
var
  i: Integer;
  aRect: TRect;
begin
  vHaftIndex := -1;
  Result := False;
  CreateHaftList;
  for i := 0 to Length(FHaftList) - 1 do
  begin
    aRect := PtToHaftRect(FHaftList[i]);
    if PtInRect(aRect, Point(X, Y)) then
    begin
      vHaftIndex := i;
      Result := True;
      break;
    end;
  end;
end;

procedure TOblongElement.SetCursor(Shift: TShiftState; X, Y: Integer);
begin
  if FHaftIndex >= 0 then
  begin
    Container.Cursor := OblongCursors[FHaftIndex];
  end
  else
    Container.Cursor := crDefault;
end;

procedure TOblongElement.BeginModify;
begin
  inherited;
end;

procedure TOblongElement.EndModify;
begin
  CorrectRect(FBoundRect);
  inherited;
end;

procedure TOblongElement.Loaded;
begin
  inherited;
  CorrectRect(FBoundRect);
end;

{ TContainer }


destructor TContainer.Destroy;
begin
  if FElementList.Count > 0 then
    FreeAndNil(FElementList);
  inherited;
end;

procedure TContainer.Paint(vCanvas: TCanvas);
var
  i: Integer;
begin
  for i := 0 to ElementList.Count - 1 do
  begin
    ElementList[i].Paint(vCanvas, BoundRect);
    ElementList[i].PaintHaftList(vCanvas)
  end;
end;

function TContainer.HitTest(X, Y: Integer; out vElement: TElement): Boolean;
var
  i: Integer;
begin
  Result := False;
  vElement := nil;
  for i := 0 to ElementList.Count - 1 do
  begin
    if ElementList[i].HitTest(X, Y) then
    begin
      vElement := ElementList[i];
      Result := True;
      break;
    end;
  end;
end;

procedure TContainer.SetBoundRect(const Value: TRect);
begin
  FBoundRect := Value;
end;

function TContainer.GetCursor: TCursor;
begin
  if Board <> nil then
    Result := Board.Cursor
  else
    Result := crDefault;
end;

procedure TContainer.SetCursor(const Value: TCursor);
begin
  if Board <> nil then
    Board.Cursor := Value;
end;

procedure TContainer.Refresh;
begin
  if Board <> nil then
    Board.Refresh;
end;

procedure TContainer.InvalidateRect(const vRect: TRect);
begin
  if Board <> nil then
  begin
    InvalidateFrame(Board.Handle, @vRect, False, 0);
  end;
end;

procedure TCustomBoard.SetDesignElement(const Value: TElement);
begin
  if FDesignElement <> Value then
  begin
    FDesignElement := Value;
  end;
end;

procedure TContainer.PaintBackground(vCanvas: TCanvas);
begin
end;

procedure TContainer.ReadElement(Reader: TReader);
var
  aElement: TElement;
begin
  ElementList.Clear;
  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    Reader.Owner := Self;
    Reader.Root := Self;
    aElement := Reader.ReadComponent(nil) as TElement;
    if aElement <> nil then
      aElement.Loaded;
  end;
  Reader.ReadListEnd;
end;

procedure TContainer.WriteElement(Writer: TWriter);
var
  i: Integer;
begin
  Writer.WriteListBegin;
  for i := 0 to ElementList.Count - 1 do
  begin
    Writer.WriteComponent(ElementList[i]);
  end;
  Writer.WriteListEnd;
end;

constructor TContainer.Create(AOwner: TComponent);
begin
  inherited Create(nil);
  FElementList := TElementList.Create;
  if (AOwner <> nil) then
  begin
    if (AOwner is TCustomBoard) then
      Board := AOwner as TCustomBoard
    else if (AOwner is TLayouts) then
      Board := (AOwner as TLayouts).Board;
  end;
end;

procedure TContainer.Change;
begin
  if Board <> nil then
    Board.Change;
end;

procedure TContainer.Clear;
begin
  ElementList.Clear;
end;

function TContainer.GetHeight: Integer;
begin
  Result := BoundRect.Bottom - BoundRect.Top;
end;

function TContainer.GetWidth: Integer;
begin
  Result := BoundRect.Right - BoundRect.Left;
end;

function TContainer.GetLayoutByPoint(X, Y: Integer): TLayout;
begin
  Result := nil;
end;

procedure TContainer.ExcludeClipRect(vCanvas: TCanvas);
begin

end;

procedure TContainer.LoadFromStream(Stream: TStream);
begin

end;

procedure TContainer.SaveToStream(Stream: TStream);
begin

end;

function TContainer.GetLayoutByIndex(vIndex: Integer): TLayout;
begin
  Result := nil;
end;

procedure TContainer.LoadFromFile(vFileName: String);
begin
end;

procedure TContainer.SaveToFile(vFileName: String);
begin
end;

procedure TContainer.Init;
begin
end;

{ TElement }

constructor TElement.CreateBy(AOwner: TComponent; X: Integer; Y: Integer);
begin
  Create(AOwner);
  if Container.Board.NextElement <> nil then
  begin
    AfterCreate(X, Y, True);
    Invalidate;
  end;
end;

procedure TElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
end;

procedure TElement.Invalidate;
begin
  Container.Refresh;
end;

function TElement.PtInHaft(X, Y: Integer; out vHaftIndex: Integer): Boolean;
begin
  Result := False;
  vHaftIndex := -1;
end;

function TElement.PtToHaftRect(P: TPoint): TRect;
begin
  Result := Rect(P.X - cHaftSize, P.Y - cHaftSize, P.X + cHaftSize, P.Y + cHaftSize);
end;

function TElement.GetSelected: Boolean;
begin
  if Container.Board <> nil then
    Result := Container.Board.DesignElement = Self
  else
    Result := False;
end;

procedure TElement.SetSelected(const Value: Boolean);
begin
  if Value and (Container.Board <> nil) then
    Container.Board.DesignElement := Self
  else
    Container.Board.DesignElement := nil;
end;

function TElement.PtInHaft(X, Y: Integer): Boolean;
var
  i: Integer;
begin
  Result := PtInHaft(X, Y, i);
end;

procedure TElement.SetModified(const Value: Boolean);
begin
  if Value then
  begin
    Inc(FModified);
    if FModified = 1 then
      BeginModify;
  end
  else
  begin
    if FModified <= 0 then
      raise Exception.Create('Modified Counter Unexcepted')
    else
    begin
      Dec(FModified);
      if FModified = 0 then
        EndModify;
    end;
  end;
end;

procedure TElement.BeginModify;
begin
  Invalidate;
end;

procedure TElement.Modifing;
begin
  Invalidate;
end;

procedure TElement.EndModify;
begin
  Change;
  Invalidate;
end;

constructor TElement.Create(AOwner: TComponent);
begin
  inherited Create(nil);
  FStyle := [trtSnap, trtMove, trtSize];
  Container := AOwner as TContainer; //do not use FContainer
  if Container.Board <> nil then
    if Container.Board.NextElement = nil then
    begin
      AfterCreate(0, 0, False);
      Invalidate;
    end;
end;

procedure TElement.Loaded;
begin
  inherited;
  Invalidate;
end;

procedure TElement.SetContainer(const Value: TContainer);
begin
  if Value <> FContainer then
  begin
    if FContainer <> nil then
    begin
      FContainer.ElementList.Extract(Self);
    end;
    FContainer := Value;
    FContainer.ElementList.Add(Self);
  end;
end;

destructor TElement.Destroy;
begin
  inherited;
end;

procedure TElement.Change;
begin
  Container.Change;
end;

function TElement.GetClientRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function TElement.GetModified: Boolean;
begin
  Result := FModified > 0;
end;

procedure TElement.AfterConstruction;
begin
  inherited;
  Change;
end;

{ TEllipseElement }

procedure TEllipseElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
  inherited;
  if Dummy then
  else
    FBoundRect := Rect(X, Y, X + 10, Y + 10);
end;

procedure TEllipseElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;
  vCanvas.Brush.Color := clRed;
  vCanvas.Ellipse(FBoundRect);
end;

{ TRectangleElement }

constructor TRectangleElement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clBlue;
end;

procedure TRectangleElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;
  vCanvas.Brush.Color := Color;
  vCanvas.Brush.Style := bsSolid;
  vCanvas.Rectangle(FBoundRect);
end;

{ TPolygonElement }

procedure TPolygonElement.CreateHaftList;
begin
  inherited;
  FHaftList := Copy(Polygon, 0, Length(Polygon));
end;

function TPolygonElement.HitTest(X, Y: Integer): Boolean;
begin
end;

procedure TPolygonElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
  inherited;
  SetLength(Polygon, 1);
  Color := clRed;
end;

procedure TPolygonElement.Move(DX, DY: Integer);
var
  i: Integer;
begin
  inherited;
  Invalidate;
  for i := 0 to Length(Polygon) - 1 do
  begin
    Inc(Polygon[i].X, DX);
    Inc(Polygon[i].Y, DY);
  end;
  Invalidate;
end;

procedure TPolygonElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;

end;

procedure TPolygonElement.PaintHaftList(vCanvas: TCanvas);
begin
  inherited;
end;

{ TDebateElement }

procedure TDebateElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
  inherited;
  SetLength(Polygon, 6);
  Polygon[0].X := 0;
  Polygon[0].Y := 0;
  Polygon[1].X := 30;
  Polygon[1].Y := 30;
  Polygon[2].X := 100;
  Polygon[2].Y := 30;
  Polygon[3].X := 100;
  Polygon[3].Y := 60;
  Polygon[4].X := 30;
  Polygon[4].Y := 60;
  Polygon[5].X := 30;
  Polygon[5].Y := 35;
end;


procedure TDebateElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;
  vCanvas.Brush.Color := clInfoBk;
  vCanvas.Polygon(Polygon);
end;

{ THeavyElement }

procedure THeavyElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
  inherited;
  if Dummy then
    OffsetRect(DesignRect, X - (DesignRect.Right - DesignRect.Left) div 2, Y - (DesignRect.Bottom - DesignRect.Top) div 2);
end;

constructor THeavyElement.Create(AOwner: TComponent);
begin
  if not (AOwner is TLayout) then
    raise Exception.Create('Must Create On Layout')
  else
  begin
    inherited;
    DesignRect := GetBounds;
  end;
end;

constructor THeavyElement.CreateBy(AOwner: TComponent; X: Integer; Y: Integer);
var
  aLayout: TLayout;
begin
  if AOwner is TLayout then
    aLayout := AOwner as TLayout
  else
    aLayout := (AOwner as TContainer).Board.FLayouts[0];//TODO use Current layout
  inherited CreateBy(aLayout, X, Y);
end;

procedure THeavyElement.CreateHaftList;
begin
  inherited;
  Setlength(FHaftList, 8);
  with DesignRect do
  begin
    FHaftList[0].X := Left;
    FHaftList[0].Y := Top;
    FHaftList[1].X := Left + (Right - Left) div 2;
    FHaftList[1].Y := Top;
    FHaftList[2].X := Right;
    FHaftList[2].Y := Top;
    FHaftList[3].X := Right;
    FHaftList[3].Y := Top + (Bottom - Top) div 2;
    FHaftList[4].X := Right;
    FHaftList[4].Y := Bottom;
    FHaftList[5].X := Left + (Right - Left) div 2;
    FHaftList[5].Y := Bottom;
    FHaftList[6].X := Left;
    FHaftList[6].Y := Bottom;
    FHaftList[7].X := Left;
    FHaftList[7].Y := Top + (Bottom - Top) div 2;
  end;
end;

procedure THeavyElement.DoPaint(vCanvas: TCanvas; vRect: TRect);
begin

end;

procedure THeavyElement.EndModify;
var
  aLayout: TLayout;
begin
  Invalidate;
{  aLayout := GetLayoutByPoint(DesignX, DesignY);
  if aLayout <> nil then
  begin
    Container := aLayout;
  end;}
  DesignRect := GetBounds;
  inherited;
end;

function THeavyElement.GetBounds: TRect;
begin
  Result := Container.BoundRect;
end;

function THeavyElement.HitTest(X, Y: Integer): Boolean;
begin
  Result := PtInRect(GetBounds, Point(X, Y));
end;

procedure THeavyElement.Move(DX, DY: Integer);
begin
  inherited;
  Invalidate;
  OffsetRect(DesignRect, DX, DY);
  Invalidate;
end;

procedure THeavyElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;
  if Captured then
  begin
    vCanvas.Brush.Style := bsClear;
    vCanvas.Rectangle(DesignRect);
  end
  else
    DoPaint(vCanvas, vRect);
end;

function THeavyElement.PtInHaft(X, Y: Integer; out vHaftIndex: Integer): Boolean;
var
  i: Integer;
  aRect: TRect;
begin
  vHaftIndex := -1;
  Result := False;
  CreateHaftList;
  for i := 0 to Length(FHaftList) - 1 do
  begin
    aRect := PtToHaftRect(FHaftList[i]);
    if PtInRect(aRect, Point(X, Y)) then
    begin
      vHaftIndex := i;
      Result := True;
      break;
    end;
  end;
end;

procedure THeavyElement.SetCursor(Shift: TShiftState; X, Y: Integer);
begin
  Container.Cursor := crHandPoint;
end;

{ TLayout }

constructor TLayout.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := True;
  if AOwner is TLayouts then
  begin
    FLayouts := (AOwner as TLayouts);
    Index := FLayouts.FLayoutList.Add(Self);
  end;
end;

procedure TLayout.ExcludeClipRect(vCanvas: TCanvas);
begin
  with BoundRect do
    LCLIntf.ExcludeClipRect(vCanvas.Handle, Left, Top, Right, Bottom);
  inherited;
end;

function TLayout.GetLayoutByIndex(vIndex: Integer): TLayout;
begin
  Result := Self;
end;

function TLayout.GetLayoutByPoint(X, Y: Integer): TLayout;
begin
  Result := Self;
end;

procedure TLayout.PaintBackground(vCanvas: TCanvas);
begin
  inherited;
end;

{ TLayout }

procedure TLayout.SetBoundRect(const Value: TRect);
begin
  inherited;
  FBoundRect := Value;
end;

{ TSingleBoard }

initialization
  ElementClasses := TList.Create;
  RegisterElements('', [TEllipseElement]);
  RegisterElements('', [TRectangleElement]);
finalization
  FreeAndNil(ElementClasses);
end.


