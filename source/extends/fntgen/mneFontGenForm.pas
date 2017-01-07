unit mneFontGenForm;
{$mode objfpc}
{$H+}
{**
 * Mini ExampleEdit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
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
    FNoAntiAliasing: Boolean;
  public
    constructor Create;
  published
    property FontName: string read FFontName write FFontName;
    property FontSize: Integer read FFontSize write FFontSize;
    property NoAntiAliasing: Boolean read FNoAntiAliasing write FNoAntiAliasing;
  end;

  { TFontGenForm }

  TFontGenForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    CancelBtn1: TButton;
    ExampleEdit: TEdit;
    FontBtn: TButton;
    FontDialog: TFontDialog;
    FontLbl: TLabel;
    ExampleImage: TImage;
    Label1: TLabel;
    NoAntialiasingChk: TCheckBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SelectEdit: TSynEdit;
    procedure CancelBtn1Click(Sender: TObject);
    procedure FontBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NoAntialiasingChkChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
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
  EditorEngine;

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
  RasterFont.Generate(GenProfile.FontName, GenProfile.FontSize);
  ExampleImage.Picture.Assign(RasterFont.FontBitmap)
end;

procedure TFontGenForm.FormCreate(Sender: TObject);
begin
  Retrieve;
  SelectEdit.Font.Name := Engine.Options.Profile.Attributes.FontName;
  SelectEdit.Font.Size := Engine.Options.Profile.Attributes.FontSize;
end;

procedure TFontGenForm.NoAntialiasingChkChange(Sender: TObject);
begin
  if not InChanging then
  begin
    GenProfile.NoAntialiasing := NoAntialiasingChk.Checked;
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

procedure TFontGenForm.OkBtnClick(Sender: TObject);
begin
  Apply;
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
