unit AboutForms;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}
interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  lclproc, lclintf, lclversion, fileinfo,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Button1: TButton;
    Image2: TImage;
    NameLbl: TLabel;
    Image1: TImage;
    SiteLbl: TLabel;
    EnvVersionLbl: TLabel;
    Label8: TLabel;
    PortionsLbl: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    VersionLbl: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SiteLblClick(Sender: TObject);
  private
  public
  end;

implementation

{$R *.lfm}

procedure TAboutForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
var
  s: string;
  fv: TFileVersionInfo;
begin
  s := ' Lazarus: '  + IntToStr(lcl_fullversion) + ' FPC: ' + IntToStr(FPC_FULLVERSION);
  //FPC_FULLVERSION //need C Style macro to enable it

  fv:=TFileVersionInfo.Create(nil);
  try
    fv.ReadFileInfo;
    VersionLbl.Caption := fv.VersionStrings.Values['FileVersion'];
    //writeln('Product version: ',fv.VersionStrings.Values['ProductVersion']);
  finally
    fv.Free;
  end;

  EnvVersionLbl.Caption := s;
  Namelbl.Font.Size := 12;
  VersionLbl.Font.Size := 12;
  PortionsLbl.Font.Style := [fsBold];
end;

procedure TAboutForm.SiteLblClick(Sender: TObject);
begin
  openurl((Sender as TLabel).Caption);
end;

end.

