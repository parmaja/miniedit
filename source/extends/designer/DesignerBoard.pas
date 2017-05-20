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
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics, Contnrs, ImgList, StdCtrls, ExtCtrls;

const
  OblongCursors: array[0..7] of TCursor = (crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE);
  cHaftSize = 4;

type
  TPointArray = array of TPoint;

  TElementStyle = set of (trtSnap, trtMove, trtSize);

  TLayout = class;
  TLayouts = class;
  TElement = class;
  TElementList = class;
  TContainer = class;

  TBoardWriter = class(TWriter)
  public
  end;

  TBoardReader = class(TReader)
  protected
    function Error(const Message: String): Boolean; override;
  public
  end;

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
    function GetLayoutByPoint(vContainer: TContainer; X, Y: Integer): TLayout; overload;
    function GetLayoutByPoint(X, Y: Integer): TLayout; overload;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure BeginModify; virtual;
    procedure EndModify; virtual;
    procedure Change; virtual;
  public
    procedure CombineRegion(var Rgn: HRGN); virtual;
    function CreateRegion: HRGN; virtual;
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
    function PtToHaftRect(P: TPoint): TRect;
    procedure CreateHaftList; virtual;
    procedure PaintHaft(vCanvas: TCanvas; P: TPoint);
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
    FClientRect: TRect;
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
    procedure CombineRegion(var Rgn: HRGN); virtual;
    property ElementList: TElementList read FElementList;
    property ClientRect: TRect read FClientRect;
    property BoundRect: TRect read FBoundRect write SetBoundRect;
    property Cursor: TCursor read GetCursor write SetCursor;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  TLayout = class(TContainer)
  private
    FLayouts: TLayouts;
    FEnabled: Boolean;
  protected
    procedure SetBoundRect(const Value: TRect); override;
  public
    procedure ExcludeClipRect(vCanvas: TCanvas); override;
    function GetLayoutByIndex(vIndex: Integer): TLayout; override;
    function GetLayoutByPoint(X, Y: Integer): TLayout; override;
    constructor Create(AOwner: TComponent); override;
    procedure PaintBackground(vCanvas: TCanvas); override;
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

  TLayouts = class(TContainer)
  private
    FLayoutList: TLayoutList;
    FLayoutHeight: Integer;
    FLayoutWidth: Integer;
    FCaption: String;
    FBkColor: TColor;
    procedure ReadBoards(Reader: TReader);
    procedure WriteBoards(Writer: TWriter);
  protected
    procedure SetBoundRect(const Value: TRect); override;
    procedure Allotment;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init; override;
    function GetLayoutByPoint(X, Y: Integer): TLayout; override;
    function GetLayoutByIndex(vIndex: Integer): TLayout; override;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromFile(vFileName: String); override;
    procedure SaveToFile(vFileName: String); override;
    function HitTest(X, Y: Integer; out vElement: TElement): Boolean; override;
    procedure Paint(vCanvas: TCanvas); override;
    procedure PaintBackground(vCanvas: TCanvas); override;
    procedure ExcludeClipRect(vCanvas: TCanvas); override;
    procedure CombineRegion(var Rgn: HRGN); override;
    property LayoutList: TLayoutList read FLayoutList;
    property LayoutWidth: Integer read FLayoutWidth write FLayoutWidth;
    property LayoutHeight: Integer read FLayoutHeight write FLayoutHeight;
    property BkColor: TColor read FBkColor write FBkColor stored False;
  published
    property Caption: String read FCaption write FCaption;
  end;

  TCustomBoard = class(TCustomControl)
  private
    FContainer: TContainer;
    FLayouts: TLayouts;
    HasHaft: Boolean;
    FCacheMode: Boolean;
    FDesignElement: TElement;
    FBorderStyle: TBorderStyle;
    procedure SetDesignElement(const Value: TElement);
    procedure SetContainer(const Value: TContainer);
  protected
    function GetLayoutByPoint(X, Y: Integer): TLayout;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure RemoveHaftList(vCanvas: TCanvas);
    procedure AddHaftList(vCanvas: TCanvas);
    procedure Change; virtual;
    procedure Reset; virtual;
  public
    NextElement: TElementClass;
    constructor Create(ABoard: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure SwitchToSingle(vIndex: Integer);
    procedure SwitchToNormal;
    property DesignElement: TElement read FDesignElement write SetDesignElement;
    property Container: TContainer read FContainer write SetContainer;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Caption;
    property Color;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Visible;
    property CacheMode: Boolean read FCacheMode write FCacheMode default False;
  end;

  TDesignerBoard = class(TCustomBoard)
  end;

  TPolygonElement = class(TElement)
  private
    Polygon: TPointArray;
  public
    function CreateRegion: HRGN; override;
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

  TWedgeElement = class(TElement)
  public
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
  end;

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
    function CreateRegion: HRGN; override;
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

  TBridgeElement = class(TOblongElement)
  private
  protected
    procedure EndModify; override;
  public
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
  end;

  TDebateElement = class(TPolygonElement)
  public
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    procedure Paint(vCanvas: TCanvas; vRect: TRect); override;
  end;

  THeavyElement = class(TElement)
  private
    function GetBounds: TRect;
  protected
    DesignRect: TRect;
  public
    procedure Move(DX, DY: Integer); override;
    function CreateRegion: HRGN; override;
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

  TCrownElement = class(THeavyElement)
  public
    procedure DoPaint(vCanvas: TCanvas; vRect: TRect); override;
  end;

  TPyorrheaElement = class(THeavyElement)
  public
    procedure DoPaint(vCanvas: TCanvas; vRect: TRect); override;
  end;

procedure OutLineBitmap(Canvas: TCanvas; Bitmap: TBitmap; const vMasks: array of TColor; const vColor: TColor; vLeft, vTop: Integer);
procedure DrawMaskBitmap(Canvas: TCanvas; Bitmap: TBitmap; const vMasks: array of TColor; const vColor: TColor; vLeft, vTop: Integer);

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

procedure DrawMaskBitmap(Canvas: TCanvas; Bitmap: TBitmap; const vMasks: array of TColor; const vColor: TColor; vLeft, vTop: Integer);
var
  BmpMask: TBitmap;
  i: Integer;
begin
  if Length(vMasks) = 0 then
    raise Exception.Create('Why u call this function , For noting ???!!!');

  BmpMask := TBitmap.Create;
  BmpMask.Handle := CreateBitmap(Bitmap.Width, Bitmap.Height, 1, 1, nil);

  SetBkColor(Bitmap.Canvas.Handle, ColorToRgb(vMasks[0]));
  BitBlt(BmpMask.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);

  for i := 1 to Length(vMasks) - 1 do
  begin
    SetBkColor(Bitmap.Canvas.Handle, ColorToRgb(vMasks[i]));
    BitBlt(BmpMask.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCPAINT);
  end;

  SetTextColor(Canvas.Handle, ColorToRgb(clWhite));
  SetBkColor(Canvas.Handle, ColorToRgb(vColor));
  BitBlt(Canvas.Handle, vLeft, vTop, Bitmap.Width, Bitmap.Height, BmpMask.Canvas.Handle, 0, 0, SRCAND);
  BmpMask.Free;
end;

procedure OutLineBitmap(Canvas: TCanvas; Bitmap: TBitmap; const vMasks: array of TColor; const vColor: TColor; vLeft, vTop: Integer);
var
  BmpMask: TBitmap;
  BmpOutLine: TBitmap;
  i: Integer;
begin
  if Length(vMasks) = 0 then
    raise Exception.Create('Why u call this function , For noting ???!!!');

  BmpMask := TBitmap.Create;
  BmpMask.Handle := CreateBitmap(Bitmap.Width, Bitmap.Height, 1, 1, nil);

  SetBkColor(Bitmap.Canvas.Handle, ColorToRgb(vMasks[0]));
  BitBlt(BmpMask.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);

  for i := 1 to Length(vMasks) - 1 do
  begin
    SetBkColor(Bitmap.Canvas.Handle, ColorToRgb(vMasks[i]));
    BitBlt(BmpMask.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCPAINT);
  end;

  BmpOutLine := TBitmap.Create;
  BmpOutLine.Handle := CreateBitmap(Bitmap.Width, Bitmap.Height, 1, 1, nil);

  BitBlt(BmpOutLine.Canvas.Handle, -2, -2, Bitmap.Width, Bitmap.Height, BmpMask.Canvas.Handle, 0, 0, SRCPAINT);
  BitBlt(BmpOutLine.Canvas.Handle, 0, -2, Bitmap.Width, Bitmap.Height, BmpMask.Canvas.Handle, 0, 0, SRCPAINT);
  BitBlt(BmpOutLine.Canvas.Handle, +2, -2, Bitmap.Width, Bitmap.Height, BmpMask.Canvas.Handle, 0, 0, SRCPAINT);
  BitBlt(BmpOutLine.Canvas.Handle, +2, 0, Bitmap.Width, Bitmap.Height, BmpMask.Canvas.Handle, 0, 0, SRCPAINT);
  BitBlt(BmpOutLine.Canvas.Handle, +2, +2, Bitmap.Width, Bitmap.Height, BmpMask.Canvas.Handle, 0, 0, SRCPAINT);
  BitBlt(BmpOutLine.Canvas.Handle, 0, +2, Bitmap.Width, Bitmap.Height, BmpMask.Canvas.Handle, 0, 0, SRCPAINT);
  BitBlt(BmpOutLine.Canvas.Handle, -2, +2, Bitmap.Width, Bitmap.Height, BmpMask.Canvas.Handle, 0, 0, SRCPAINT);
  BitBlt(BmpOutLine.Canvas.Handle, -2, 0, Bitmap.Width, Bitmap.Height, BmpMask.Canvas.Handle, 0, 0, SRCPAINT);
  BitBlt(BmpOutLine.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, BmpMask.Canvas.Handle, 0, 0, SRCINVERT);

  SetTextColor(Canvas.Handle, ColorToRgb(clWhite));
  SetBkColor(Canvas.Handle, ColorToRgb(vColor));
  BitBlt(Canvas.Handle, vLeft, vTop, Bitmap.Width, Bitmap.Height, BmpOutLine.Canvas.Handle, 0, 0, SRCAND);
  BmpOutLine.Free;
  BmpMask.Free;
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

procedure TLayouts.Allotment;
var
  i: Integer;
  w, h: Integer;
  aRect: TRect;
begin
  w := LayoutWidth;
  h := LayoutHeight;
  aRect := ClientRect;
  aRect.Right := aRect.Left + w;
  aRect.Bottom := aRect.Top + h;
  for i := 0 to 15 do
  begin
    FLayoutList[i].BoundRect := aRect;
    OffsetRect(aRect, w, 0);
  end;

  aRect := ClientRect;
  aRect.Right := aRect.Left + w;
  aRect.Top := aRect.Top + h;
  aRect.Bottom := aRect.Top + h;
  for i := 16 to 31 do
  begin
    FLayoutList[i].BoundRect := aRect;
    OffsetRect(aRect, w, 0);
  end;
end;

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

procedure TLayouts.CombineRegion(var Rgn: HRGN);
var
  i: Integer;
begin
  inherited;
  for i := 0 to FLayoutList.Count - 1 do
  begin
    FLayoutList[i].CombineRegion(Rgn);
  end;
end;

constructor TLayouts.Create(AOwner: TComponent);
begin
  inherited;
  BkColor := $00DEE9FA;
  FLayoutList := TLayoutList.Create;
  TLayout.Create(Self);
  TLayout.Create(Self);
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

function TLayouts.GetLayoutByIndex(vIndex: Integer): TLayout;
begin
  Result := LayoutList[vIndex];
end;

function TLayouts.GetLayoutByPoint(X, Y: Integer): TLayout;
var
  a, b: Integer;
begin
  a := (X - ClientRect.Left) div FLayoutWidth;
  if a < 16 then
  begin
    b := (Y - ClientRect.Top) div LayoutHeight;
    if b < 2 then
      Result := LayoutList[b * 16 + a]
    else
      Result := nil;
  end
  else
    Result := nil;
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

procedure TLayouts.Init;
begin
  inherited;
  Allotment;
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

procedure TLayouts.LoadFromStream(Stream: TStream);
var
  aReader: TReader;
begin
  if Board <> nil then
  begin
    Board.NextElement := nil;
    Board.DesignElement := nil;
  end;
  Clear;
  if Stream <> nil then
  begin
    aReader := TBoardReader.Create(Stream, 4096);
    aReader.BeginReferences;
    aReader.ReadComponent(self);
    aReader.EndReferences;
    aReader.Free;
  end;
  Allotment;
  Refresh;
end;

procedure TLayouts.Paint(vCanvas: TCanvas);
var
  i: Integer;
begin
  for i := 0 to FLayoutList.Count - 1 do
  begin
    FLayoutList[i].Paint(vCanvas);
  end;
  inherited;
end;

procedure TLayouts.PaintBackground(vCanvas: TCanvas);
var
  i: Integer;
begin
  for i := 0 to FLayoutList.Count - 1 do
  begin
    FLayoutList[i].PaintBackground(vCanvas);
  end;
  inherited;
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

procedure TLayouts.SaveToStream(Stream: TStream);
var
  aWriter: TWriter;
begin
  aWriter := TBoardWriter.Create(Stream, 4096);
  aWriter.WriteComponent(self);
  aWriter.Free;
end;

procedure TLayouts.SetBoundRect(const Value: TRect);
var
  DX, DY: Integer;
begin
  inherited;
  FClientRect := Rect(0, 0, LayoutWidth * 16, LayoutHeight * 2);
  DX := Abs(FClientRect.Right - Value.Right);
  if DX <> 0 then
    DX := DX div 2;
  DY := Abs(FClientRect.Bottom - Value.Bottom);
  if DY <> 0 then
    DY := DY div 2;
  OffsetRect(FClientRect, DX, DY);
  Allotment;
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

procedure TCustomBoard.AddHaftList(vCanvas: TCanvas);
begin
  if not HasHaft then
  begin
    if DesignElement <> nil then
      DesignElement.PaintHaftList(vCanvas);
    HasHaft := True;
  end;
end;

procedure TCustomBoard.Change;
begin

end;

constructor TCustomBoard.Create(ABoard: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  FLayouts := TLayouts.Create(Self);
  FContainer := FLayouts;
  FContainer.Init;
  Width := 100;
  Height := 100;
  FCacheMode := False;
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

function TCustomBoard.GetLayoutByPoint(X, Y: Integer): TLayout;
begin
  Result := Container.GetLayoutByPoint(X, Y);
end;

procedure TCustomBoard.Loaded;
begin
  inherited;
  Container.BoundRect := ClientRect;
end;

procedure TCustomBoard.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aElement: TElement;
begin
  inherited;
  if NextElement <> nil then
  begin
    DesignElement := NextElement.CreateBy(Container, X, Y);
    //NextElement := nil;
  end
  else if (DesignElement = nil) or not ((DesignElement.Captured) or (DesignElement.PtInHaft(X, Y))) then
  begin
    Container.HitTest(X, Y, aElement);
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
    Container.HitTest(X, Y, aElement);
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
var
  MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  aBufferd: Boolean;
  w, h: Integer;
  aCanvas: TCanvas;
  aRect: TRect;
begin
  aRect := Canvas.ClipRect;
  w := aRect.Right - aRect.Left;
  h := aRect.Bottom - aRect.Top;
  if FCacheMode and not EqualRect(ClientRect, aRect) then
  begin
    aCanvas := TCanvas.Create;
    MemBitmap := CreateCompatibleBitmap(Canvas.Handle, w, h);
    MemDC := CreateCompatibleDC(Canvas.Handle);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    SetWindowOrgEx(MemDc, aRect.Left, aRect.Top, nil);
    aBufferd := True;
    aCanvas.Handle := MemDC;
  end
  else
  begin
    MemBitmap := 0;
    MemDC := 0;
    OldBitmap := 0;
    aBufferd := False;
    aCanvas := Canvas;
  end;
  RemoveHaftList(aCanvas);
  if Container <> nil then
  begin
    Container.PaintBackground(aCanvas);
    Container.Paint(aCanvas);
  end;
  Container.ExcludeClipRect(aCanvas);
  aCanvas.Brush.Color := clWindow;
  aCanvas.FillRect(aCanvas.ClipRect);
  if aBufferd then
  begin
    BitBlt(Canvas.Handle, aRect.Left, aRect.Top, w, h, MemDC, aRect.Left, aRect.Top, SRCCOPY);
    aCanvas.Handle := 0;
    SelectObject(MemDC, OldBitmap);
    DeleteDC(MemDC);
    DeleteObject(MemBitmap);
    aCanvas.Free;
  end;
  AddHaftList(Canvas);
end;

procedure TCustomBoard.RemoveHaftList(vCanvas: TCanvas);
begin
  if HasHaft then
  begin
    if DesignElement <> nil then
      DesignElement.PaintHaftList(vCanvas);
    HasHaft := False;
  end;
end;

procedure TCustomBoard.Reset;
begin

end;

procedure TCustomBoard.Resize;
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    if Container <> nil then
      Container.BoundRect := ClientRect;
  end;
end;

procedure TCustomBoard.SetContainer(const Value: TContainer);
begin
  FContainer := Value;
  Refresh;
end;

procedure TCustomBoard.SwitchToNormal;
begin
  FContainer := FLayouts;
  FContainer.BoundRect := ClientRect;
  Refresh;
end;

procedure TCustomBoard.SwitchToSingle(vIndex: Integer);
begin
  FContainer := Container.GetLayoutByIndex(vIndex);
  FContainer.BoundRect := ClientRect;
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
  inherited;
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
  NewPen, OldPen: HPEN;
  OldRop2: Integer;
begin
  OldRop2 := SetROP2(vCanvas.Handle, R2_NOT);
  NewPen := CreatePen(BS_SOLID, 1, ColorToRgb(clBtnShadow));
  OldPen := SelectObject(vCanvas.Handle, NewPen);
  CreateHaftList;
  inherited;
  vCanvas.Brush.Color := clBlack;
  for i := 0 to Length(FHaftList) - 1 do
  begin
    PaintHaft(vCanvas, FHaftList[i]);
  end;
  SelectObject(vCanvas.Handle, OldPen);
  DeleteObject(NewPen);
  SetROP2(vCanvas.Handle, OldRop2);
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

{ TWedgeElement }

procedure TWedgeElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;

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

function TOblongElement.CreateRegion: HRGN;
begin
  Result := CreateRectRgn(Left, Top, Right, Bottom);
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
    ElementList[i].Paint(vCanvas, ClientRect);
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
  FClientRect := FBoundRect;
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
    Windows.InvalidateRect(Board.Handle, @vRect, False);
  end;
end;

procedure TContainer.CombineRegion(var Rgn: HRGN);
var
  i: Integer;
begin
  for i := 0 to ElementList.Count - 1 do
  begin
    ElementList[i].CombineRegion(Rgn);
  end;
end;

procedure TCustomBoard.SetDesignElement(const Value: TElement);
begin
  if FDesignElement <> Value then
  begin
    if FDesignElement <> nil then
      RemoveHaftList(Canvas);
    FDesignElement := Value;
    if FDesignElement <> nil then
      AddHaftList(Canvas);
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
  Result := ClientRect.Bottom - ClientRect.Top;
end;

function TContainer.GetWidth: Integer;
begin
  Result := ClientRect.Right - ClientRect.Left;
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
var
  aRgn: HRGN;
  aRect: TRect;
begin
  aRgn := CreateRegion;
  GetRgnBox(aRgn, aRect);
  DeleteObject(aRgn);
  InflateRect(aRect, 3, 3);
  Container.InvalidateRect(aRect);
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

procedure TElement.CombineRegion(var Rgn: HRGN);
var
  aRgn: HRGN;
begin
  aRgn := CreateRegion;
  CombineRgn(Rgn, Rgn, aRgn, RGN_DIFF);
  DeleteObject(aRgn);
end;

function TElement.CreateRegion: HRGN;
begin
  Result := NULLREGION;
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

procedure TElement.EndModify;
begin
  Change;
  Invalidate;
end;

constructor TElement.Create(AOwner: TComponent);
begin
  inherited Create(nil);
  FStyle := [trtSnap, trtMove, trtSize];
  Container := AOwner as TContainer;
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

function TElement.GetLayoutByPoint(X, Y: Integer): TLayout;
begin
  Result := GetLayoutByPoint(Container, X, Y);
end;

function TElement.GetLayoutByPoint(vContainer: TContainer; X, Y: Integer): TLayout;
begin
  if vContainer.Board <> nil then
    Result := vContainer.Board.GetLayoutByPoint(X, Y)
  else
    Result := nil;
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
  vCanvas.Rectangle(vRect);
end;

{ TPolygonElement }

procedure TPolygonElement.CreateHaftList;
begin
  inherited;
  FHaftList := Copy(Polygon, 0, Length(Polygon));
end;

function TPolygonElement.CreateRegion: HRGN;
begin
  Result := CreatePolygonRgn((@Polygon[0])^, Length(Polygon), ALTERNATE);
end;

function TPolygonElement.HitTest(X, Y: Integer): Boolean;
var
  rgn: HRGN;
begin
  rgn := CreateRegion;
  if PtInRegion(rgn, X, Y) then
    Result := True
  else
    Result := False;
  DeleteObject(rgn);
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
    aLayout := GetLayoutByPoint(AOwner as TContainer, DesignX, DesignY);
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

function THeavyElement.CreateRegion: HRGN;
begin
  with DesignRect do
    Result := CreateRectRgn(Left, Top, Right, Bottom);
end;

procedure THeavyElement.DoPaint(vCanvas: TCanvas; vRect: TRect);
begin

end;

procedure THeavyElement.EndModify;
var
  aLayout: TLayout;
begin
  Invalidate;
  aLayout := GetLayoutByPoint(DesignX, DesignY);
  if aLayout <> nil then
  begin
    Container := aLayout;
  end;
  DesignRect := GetBounds;
  inherited;
end;

function THeavyElement.GetBounds: TRect;
begin
  Result := Container.ClientRect;
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

{ TBoardReader }

function TBoardReader.Error(const Message: String): Boolean;
begin
  Result := True;
end;

{ TCrown2Element }

procedure TPyorrheaElement.DoPaint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;
  with DesignRect do
  begin
    //    OutLineBitmap(vCanvas, aBitmap, [clRed], clRed, Left, Top);
  end;
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
    Windows.ExcludeClipRect(vCanvas.Handle, Left, Top, Right, Bottom);
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
  vCanvas.Brush.Color := FLayouts.BkColor;
  vCanvas.FillRect(BoundRect);
end;

{ TLayout }

procedure TLayout.SetBoundRect(const Value: TRect);
var
  DX, DY: Integer;
begin
  inherited;
  FClientRect := Rect(0, 0, FLayouts.LayoutWidth, FLayouts.LayoutHeight);
  DX := Abs((FClientRect.Right - FClientRect.Left) - (Value.Right - Value.Left));
  if DX <> 0 then
    DX := DX div 2;
  DY := Abs((FClientRect.Bottom - FClientRect.Top) - (Value.Bottom - Value.Top));
  if DY <> 0 then
    DY := DY div 2;
  OffsetRect(FClientRect, Value.Left + DX, Value.Top + DY);
end;

{ TCrownElement }

procedure TCrownElement.DoPaint(vCanvas: TCanvas; vRect: TRect);
var
  aBitmap: TBitmap;
begin
  inherited;
  with DesignRect do
  begin
    aBitmap := TBitmap.Create;
    OutLineBitmap(vCanvas, aBitmap, [clLime, clBlue, clFuchsia], clBlue, Left, Top);
    aBitmap.Free;
  end;
end;

{ TBridgeElement }

procedure TBridgeElement.EndModify;
var
  aLayout: TLayout;
  TW, W, X, Y: Integer;
begin
  Invalidate;
  W := FBoundRect.Right - FBoundRect.Left;
  X := FBoundRect.Left + (W) div 2;
  Y := FBoundRect.Top + (FBoundRect.Bottom - FBoundRect.Top) div 2;
  aLayout := GetLayoutByPoint(X, Y);
  TW := aLayout.BoundRect.Right - aLayout.BoundRect.Left;
  w := w div TW * TW;
  //  FBoundRect.Left:=
  FBoundRect.Right := FBoundRect.Left + w;
  if aLayout.Index < 16 then
  begin
    FBoundRect.Bottom := aLayout.BoundRect.Bottom - 65;
    FBoundRect.Top := FBoundRect.Bottom - 10;
  end
  else
  begin
    FBoundRect.Top := aLayout.BoundRect.Top - 65;
    FBoundRect.Bottom := FBoundRect.Top - 10;
  end;
  inherited;
end;

procedure TBridgeElement.Paint(vCanvas: TCanvas; vRect: TRect);
begin
  inherited;
  vCanvas.Brush.Color := clGray;
  vCanvas.Rectangle(FBoundRect);
end;

{ TSingleBoard }

initialization
  ElementClasses := TList.Create;
  RegisterElements('', [TEllipseElement]);
  RegisterElements('', [TBridgeElement]);
  RegisterElements('', [TRectangleElement]);
  RegisterElements('', [TCrownElement]);
  RegisterElements('', [TPyorrheaElement]);

finalization
  FreeAndNil(ElementClasses);
end.


