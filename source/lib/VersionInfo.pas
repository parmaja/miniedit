unit VersionInfo;

{$mode objfpc}

interface

(*
  http://forum.lazarus.freepascal.org/index.php/topic,13957.msg73542.html#msg73542

  Building on the excellent vinfo.pas supplied by Paul Ishenin and available elsewhere on these Lazarus
  Forums
    - I hid the TVersionInfo class from the end user to simplify their (mine) number of required Uses...
    - Added defensive code to TVersionInfo if no build info is compiled into the exe
    - Deduced GetResourceStrings - works under Linux 64/GTK2 with Lazarus 0.9.30, but fails under
      Win XP 32bit/Lazarus 0.9.29 - suspecting my install as the lazresexplorer example also fails
      for me under Lazarus 0.9.29, but works with Lazarus 0.9.30

  Trawled through IDE source code, FPC source code and Lazarus supplied example program lasresexplorer
  to find the other defines and lookups...

  end user only needs to use VersionSupport - no other units necessary for their project.

  Jedi CodeFormatter seems to fail on the {$I %VARIABLE%} references, so sticking them all in here
  means end user code can be neatly formatted using Jedi CodeFormatter

  Other interesting includes I picked up in my travels are...
  //  {$I %HOME%} = User Home Directory
  //  {$I %FILE%} = Current pas file
  //  {$I %LINE%} = current line number

  Mike Thompson - mike.cornflake@gmail.com
  July 24 2011
*)

uses
  Classes, SysUtils;

function GetFileVersion: String;
function GetProductVersion: String;
function GetCompiledDate: String;
function GetCompilerInfo: String;
function GetTargetInfo: String;
function GetOS: String;
function GetResourceStrings(oStringList : TStringList) : Boolean;
function GetLCLVersion: String;
function GetWidgetSet: string;

Const
  WIDGETSET_GTK        = 'GTK widget set';
  WIDGETSET_GTK2       = 'GTK 2 widget set';
  WIDGETSET_WIN        = 'Win32/Win64 widget set';
  WIDGETSET_WINCE      = 'WinCE widget set';
  WIDGETSET_CARBON     = 'Carbon widget set';
  WIDGETSET_QT         = 'QT widget set';
  WIDGETSET_fpGUI      = 'fpGUI widget set';
  WIDGETSET_OTHER      = 'Other gui';

implementation

uses
  Resource, VersionTypes, VersionResource, LCLVersion, InterfaceBase;

type
  TVersionInfo = class(TObject)
  private
    FBuildInfoAvailable: Boolean;
    FVersResource: TVersionResource;
    function GetFixedInfo: TVersionFixedInfo;
    function GetStringFileInfo: TVersionStringFileInfo;
    function GetVarFileInfo: TVersionVarFileInfo;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(Instance: THandle);

    property BuildInfoAvailable: Boolean read FBuildInfoAvailable;

    property FixedInfo: TVersionFixedInfo read GetFixedInfo;
    property StringFileInfo: TVersionStringFileInfo read GetStringFileInfo;
    property VarFileInfo: TVersionVarFileInfo read GetVarFileInfo;
  end;

function GetWidgetSet: string;
begin
  case WidgetSet.LCLPlatform of
    lpGtk:   Result := WIDGETSET_GTK;
    lpGtk2:  Result := WIDGETSET_GTK2;
    lpWin32: Result := WIDGETSET_WIN;
    lpWinCE: Result := WIDGETSET_WINCE;
    lpCarbon:Result := WIDGETSET_CARBON;
    lpQT:    Result := WIDGETSET_QT;
    lpfpGUI: Result := WIDGETSET_fpGUI;
  else
    Result:=WIDGETSET_OTHER;
  end;
end;

function GetCompilerInfo: String;
begin
  Result := 'FPC '+{$I %FPCVERSION%};
end;

function GetTargetInfo: String;
begin
  Result := {$I %FPCTARGETCPU%}+' - '+{$I %FPCTARGETOS%};
end;

function GetOS: String;
begin
  Result := {$I %FPCTARGETOS%};
end;

function GetLCLVersion: String;
begin
  Result := 'LCL '+lcl_version;
end;

function GetCompiledDate: String;
var
  sDate, sTime: String;
begin
  sDate := {$I %DATE%};
  sTime := {$I %TIME%};

  Result := sDate + ' at ' + sTime;
end;

{ Routines to expose TVersionInfo data }

var
  FInfo: TVersionInfo;

Procedure CreateInfo;
begin
  If Not Assigned(FInfo) then
  begin
    FInfo := TVersionInfo.Create;
    FInfo.Load(HINSTANCE);
  end;
end;

function GetResourceStrings(oStringList: TStringList): Boolean;
var
  i, j : Integer;
  oTable : TVersionStringTable;
begin
  CreateInfo;

  oStringList.Clear;
  Result := False;

  If FInfo.BuildInfoAvailable then
  begin
    Result := True;
    For i := 0 To FInfo.StringFileInfo.Count-1 Do
    begin
      oTable := FInfo.StringFileInfo.Items[i];

      For j := 0 To oTable.Count-1 Do
        If Trim(oTable.ValuesByIndex[j])<>'' then
          oStringList.Values[oTable.Keys[j]] := oTable.ValuesByIndex[j];
    end;
  end;
end;

function ProductVersionToString(PV: TFileProductVersion): String;
begin
  Result := Format('%d.%d.%d.%d', [PV[0], PV[1], PV[2], PV[3]]);
end;

function GetProductVersion: String;
begin
  CreateInfo;

  If FInfo.BuildInfoAvailable then
    Result := ProductVersionToString(FInfo.FixedInfo.ProductVersion)
  Else
    Result := 'No build information available';
end;

function GetFileVersion: String;
begin
  CreateInfo;

  If FInfo.BuildInfoAvailable then
    Result := ProductVersionToString(FInfo.FixedInfo.FileVersion)
  Else
    Result := 'No build information available';
end;

{ TVersionInfo }

function TVersionInfo.GetFixedInfo: TVersionFixedInfo;
begin
  Result := FVersResource.FixedInfo;
end;

function TVersionInfo.GetStringFileInfo: TVersionStringFileInfo;
begin
  Result := FVersResource.StringFileInfo;
end;

function TVersionInfo.GetVarFileInfo: TVersionVarFileInfo;
begin
  Result := FVersResource.VarFileInfo;
end;

Constructor TVersionInfo.Create;
begin
  Inherited Create;

  FVersResource := TVersionResource.Create;
  FBuildInfoAvailable := False;
end;

Destructor TVersionInfo.Destroy;
begin
  FVersResource.Free;

  Inherited Destroy;
end;

Procedure TVersionInfo.Load(Instance: THandle);
var
  Stream: TResourceStream;
  ResID: Integer;
  Res: TFPResourceHandle;
begin
  FBuildInfoAvailable := False;
  ResID := 1;

  // Defensive code to prevent failure if no resource available...
  Res := FindResource(Instance, PAnsiChar(PtrInt(ResID)), PChar(RT_VERSION));
  If Res = 0 then
    Exit;

  Stream := TResourceStream.CreateFromID(Instance, ResID, PChar(RT_VERSION));
  Try
    FVersResource.SetCustomRawDataStream(Stream);

    // access some property to load from the stream
    FVersResource.FixedInfo;

    // clear the stream
    FVersResource.SetCustomRawDataStream(nil);

    FBuildInfoAvailable := True;
  Finally
    Stream.Free;
  end;
end;

initialization
  FInfo := nil;

finalization
  If Assigned(FInfo) then
    FInfo.Free;
end.
