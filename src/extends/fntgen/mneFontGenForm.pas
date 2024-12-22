unit mneFontGenForm;
{$mode objfpc}
{$H+}
{**
 * Mini ExampleEdit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}
interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SynEdit,
  mnXMLRttiProfile, mnFonts,
  IAddons;

type

  { TFontGenProfile }

  TFontGenProfile = class(TmnXMLProfile)
  private
    FFontName: string;
    FFontSize: Integer;
    FAntiAliasing: Boolean;
  public
    constructor Create;
  published
    property FontName: string read FFontName write FFontName;
    property FontSize: Integer read FFontSize write FFontSize;
    property AntiAliasing: Boolean read FAntiAliasing write FAntiAliasing;
  end;

  { TFontGenForm }

  TFontGenForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Button1: TButton;
    CancelBtn1: TButton;
    ExampleEdit: TEdit;
    FontBtn: TButton;
    FontDialog: TFontDialog;
    FontLbl: TLabel;
    Label1: TLabel;
    AntialiasingChk: TCheckBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    OpenDialog: TOpenDialog;
    PaintBox1: TPaintBox;
    SaveDialog: TSaveDialog;
    SelectEdit: TSynEdit;
    procedure Button1Click(Sender: TObject);
    procedure CancelBtn1Click(Sender: TObject);
    procedure FontBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AntialiasingChkChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    InChanging: Boolean;
  protected
    GenProfile: TFontGenProfile;
    RasterFont: TmnfRasterFont;
  public
    procedure Apply;
    procedure Retrieve;
    procedure ChangeEdit;
  end;

implementation

{$R *.lfm}

uses
  EditorEngine, FPCanvas, FPImage, FPImgCanv, FPWritePNG, IntfGraphics;

procedure PaintAliased(Canvas: TCanvas; x,y: integer; const TheText: string);
var
  w,h: integer;
  IntfImg: TLazIntfImage;
  Img: TPortableNetworkGraphic;
  dy: Integer;
  dx: Integer;
  col: TFPColor;
  FontColor: TColor;
  c: TColor;
begin
  w:=0;
  h:=0;
  Canvas.GetTextSize(TheText,w,h);
  if (w<=0) or (h<=0) then exit;
  Img:=TPortableNetworkGraphic.Create;
  IntfImg:=nil;
  try
    // paint text to a bitmap
    Img.Masked:=true;
    Img.SetSize(w,h);
    Img.Canvas.Brush.Style:=bsSolid;
    Img.Canvas.Brush.Color:=clWhite;
    Img.Canvas.FillRect(0,0,w,h);
    Img.Canvas.Font:=Canvas.Font;
    Img.Canvas.TextOut(0,0,TheText);
    // get memory image
    IntfImg:=Img.CreateIntfImage;
    // replace gray pixels
    FontColor:=ColorToRGB(Canvas.Font.Color);
    for dy:=0 to h-1 do begin
      for dx:=0 to w-1 do begin
        col:=IntfImg.Colors[dx,dy];
        c:=FPColorToTColor(col);
        if c<>FontColor then
          IntfImg.Colors[dx,dy]:=colTransparent;
      end;
    end;
    // create bitmap
    Img.LoadFromIntfImage(IntfImg);
    Img.Transparent := True;
    Img.TransparentColor := clBlack;
    Img.SaveToFile('d:\temp\myfont4.png');
    // paint
    Canvas.Draw(x,y,Img);
  finally
    IntfImg.Free;
    Img.Free;
  end;
end;

{ TFontGenProfile }

constructor TFontGenProfile.Create;
begin
  FontName := 'Courier New';
  FontSize := 10;
end;

procedure TFontGenForm.Apply;
begin
  GenProfile.SaveToFile(Engine.WorkSpace  + 'mne-font-gen.xml');
end;

procedure TFontGenForm.Retrieve;
begin
  InChanging := True;
  try
    if GenProfile = nil then
      GenProfile := TFontGenProfile.Create;
    if RasterFont = nil then
      RasterFont := TmnfRasterFont.Create;
    GenProfile.SafeLoadFromFile(Engine.WorkSpace  + 'mne-font-gen.xml');
  finally
    InChanging := False;
  end;
  ChangeEdit;
end;

procedure TFontGenForm.ChangeEdit;
begin
  FontLbl.Caption := GenProfile.FontName + ' ' + IntToStr(GenProfile.FontSize) + ' pt';
  RasterFont.Generate(GenProfile.FontName, GenProfile.FontSize, GenProfile.AntiAliasing);
  //ExampleImage.Picture.Assign(RasterFont.Image)
end;

procedure TFontGenForm.FormCreate(Sender: TObject);
begin
  Retrieve;
  SelectEdit.Font.Name := Engine.Options.Profile.Attributes.FontName;
  SelectEdit.Font.Size := Engine.Options.Profile.Attributes.FontSize;
end;

procedure TFontGenForm.AntialiasingChkChange(Sender: TObject);
begin
  if not InChanging then
  begin
    GenProfile.Antialiasing := AntialiasingChk.Checked;
    ChangeEdit;
  end;
end;

procedure TFontGenForm.FontBtnClick(Sender: TObject);
begin
  FontDialog.Font.Name := GenProfile.FontName;
  FontDialog.Font.Size := GenProfile.FontSize;
  FontDialog.Options := FontDialog.Options - [fdNoStyleSel];
  if FontDialog.Execute then
  begin
    GenProfile.FontName := FontDialog.Font.Name;
    GenProfile.FontSize := FontDialog.Font.Size;
    ChangeEdit;
  end;
end;

procedure TFontGenForm.CancelBtn1Click(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    Apply;
    RasterFont.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TFontGenForm.Button1Click(Sender: TObject);
begin
  PaintBox1.Canvas.Font.Color := clRed;
  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintAliased(PaintBox1.Canvas, 0, 0, 'TEST TEST');
end;

procedure TFontGenForm.OkBtnClick(Sender: TObject);
begin
  Apply;
end;

procedure TFontGenForm.PaintBox1Paint(Sender: TObject);
begin
  //PaintBox1.Canvas.Draw(0, 0, RasterFont.Image);
end;

type
  TFontGenAddon = class(TAddon, IClickAddon, IMenuAddon)
  public
    procedure Click(Sender: TObject);
    function GetCaption: string;
  end;

  { TFontGenAddon }

  procedure TFontGenAddon.Click(Sender: TObject);
  begin
    with TFontGenForm.Create(Application) do
    begin
      if ShowModal= mrOK then
      begin
        Apply;
      end
    end;
  end;

  function TFontGenAddon.GetCaption: string;
  begin
    Result := 'Bitmap Font Generator';
  end;

initialization
  Addons.Add('File', 'FontGen', TFontGenAddon);
end.

//' abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.,!?-+/():;%&`''*#=[]"'
