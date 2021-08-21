unit mndEngines;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

{$mode objfpc}{$H+}

uses
  SysUtils, Variants, Classes, Controls, Dialogs, Forms, Contnrs, ImgList,
  mnClasses, mnUtils, mnXMLRttiProfile,
  mncConnections, mncCSVExchanges, mncSQL, mncCSV, mncDB, mncMeta, mnParams, mnFields,
  EditorEngine, EditorClasses, fgl,
  mndOpenDatabases, mndConnectServers;

const
  IMG_UNKOWN = 0;
  IMG_DATABASE = 1;
  IMG_DOMAIN = 2;
  IMG_GENERATOR = 3;
  IMG_EXCEPTION = 4;
  IMG_TABLE = 5;
  IMG_VIEW = 6;
  IMG_PROCEDURE = 7;
  IMG_FUNCTION = 8;
  IMG_TRIGGER = 9;
  IMG_INDEX = 10;
  IMG_FIELD = 11;
  IMG_DATA = 12;
  IMG_COMMAND = 13;
  IMG_INTERACTIVE = 14;
  IMG_DATABASES = 15;

type
  EmndException = class(Exception);

  TmndExecuteType = (execNormal, execExport, execImport);

  TMetaInfo = record
    Name: string;
    Value: string;
  end;

  { TmndSetting }

  TmndSetting = class(TmnXMLProfile)
  private
    FCSVANSIContents: Boolean;
    FCSVDelimiterChar: Char;
    FCSVHeader: TmncCSVHeader;
    FCSVQuoteChar: Char;
    FOpenSaveDialogFilters: string;
    FCacheMetas: Boolean;
    FLogoutSQL: string;
    FLoginSQL: string;
    FInternalLogoutSQL: string;
    FInternalLoginSQL: string;
  public
    constructor Create;
    property InternalLoginSQL:string read FInternalLoginSQL write FInternalLoginSQL;
    property InternalLogoutSQL:string read FInternalLogoutSQL write FInternalLogoutSQL;
  published
    property CacheMetas: Boolean read FCacheMetas write FCacheMetas default True;
    property OpenSaveDialogFilters:string read FOpenSaveDialogFilters write FOpenSaveDialogFilters;
    property LoginSQL: string read FLoginSQL write FLoginSQL;
    property LogoutSQL: string read FLogoutSQL write FLogoutSQL;
    property CSVQuoteChar: Char read FCSVQuoteChar write FCSVQuoteChar default '"';
    property CSVDelimiterChar: Char read FCSVDelimiterChar write FCSVDelimiterChar default ';';
    property CSVHeader: TmncCSVHeader read FCSVHeader write FCSVHeader default hdrNone;
    property CSVANSIContents: Boolean read FCSVANSIContents write FCSVANSIContents default False;
  end;

  TmndShow = (shwnElement, shwnBrowse, shwnFile);

  TmndAddon = class;

  { TmndProcess }

  TmndProcess = class(TObject)
  private
    FAddon: TmndAddon;
    FMetaItem: TmncMetaItem;
    //FMetaItems: TmncMetaItems;
    FShowIn: TmndShow;
  strict protected
    function GetDisplayName: string; virtual;
  public
    Value: string;
    constructor Create;
    constructor Create(vAddon: TmndAddon; vMetaItem: TmncMetaItem = nil; AShowIn: TmndShow = shwnElement);
    constructor Create(AddonName: string; vMetaItem: TmncMetaItem = nil; AShowIn: TmndShow = shwnElement);
    constructor Create(AddonName: string; vValue: string; AShowIn: TmndShow = shwnElement);

    destructor Destroy; override;

    procedure Execute(vMetaItem: TmncMetaItem; FallDefault: Boolean = False);
    property DisplayName: string read GetDisplayName;

    //property MetaItems: TmncMetaItems read FMetaItems write FMetaItems;
    property MetaItem: TmncMetaItem read FMetaItem write FMetaItem;
    property Addon: TmndAddon read FAddon;// write FAddon;
    property ShowIn: TmndShow read FShowIn write FShowIn;
  end;

  { TmndStack }

  TmndStack = class(specialize TmnObjectList<TmndProcess>)
  private
    function GetCurrent: TmndProcess;
  public
    constructor Create;
    function Add(AItem: TmndProcess): Integer; overload;
    function Push(AItem: TmndProcess): Integer;
    procedure Pop;
    procedure Top; //pop all to top
    procedure Trim(ToCount: Integer); //Similar to SetCount
    property Current: TmndProcess read GetCurrent;
  end;

  TmndAddons = class;

{
  nsDefault: Default Addon for execute parent when parent can not parent
  nsCommand: It is command not SQL member
  nsEditor: Show a script in SQL editor like triggers or stored prpocedures
  nsButton: Make it visible as button or menu in gui form
  nsNeedSession: Enum only when session is active
}

  TmndAddonStyle = set of (nsDefault, nsMember, nsCommand, nsEditor, nsButton, nsNeedSession);

  ImndAddon = interface(IInterface)
    function GetAddon: TmndAddon;
  end;

  { TmndAddon }

  TmndAddon = class(TmnNamedObject)
  private
    FDefaultAddon: string;
    FItemName: string;
    FMaster: string;
    FStyle: TmndAddonStyle;
    FTitle: string;
    FKind: TmetaKind;
    FImageIndex: TImageIndex;
  protected
    function GetCanExecute: Boolean; virtual;
    procedure DoExecute(vMetaItem: TmncMetaItem); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ShowProperty; virtual;
    procedure Execute(vMetaItem: TmncMetaItem = nil; FallDefault: Boolean = False);
    procedure EnumAddons(Addons: TmndAddons);
    procedure EnumDefaults(Addons: TmndAddons);
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem = nil); virtual;
    property CanExecute: Boolean read GetCanExecute;
    //property Name: string read FName write FName; //Name = 'Tables' it is already exists in TmnNamedObject
    property Master: string read FMaster write FMaster; //Master is parent Addon like Tables.Master = 'Database'
    property ItemName: string read FItemName write FItemName; //Item name eg  Tables.Item = 'Table'
    property DefaultAddon: string read FDefaultAddon write FDefaultAddon; deprecated; //Default Addon from Childs addons
    property Kind: TmetaKind read FKind write FKind default sokNone;
    property Style: TmndAddonStyle read FStyle write FStyle;
    property Title: string read FTitle write FTitle;
    property ImageIndex: TImageIndex read FImageIndex write FImageIndex default -1;
  end;

  TmndAddonClass = class of TmndAddon;

  { TmndCustomAddons }

  TmndCustomAddons = class(specialize TmnNamedObjectList<TmndAddon>)
  private
  public
    procedure EnumAddons(MasterName: string; Addons: TmndAddons; SessionActive: Boolean; OnlyDefaults: Boolean = False); overload;
    function IsExists(vAddon: TmndAddon): Boolean;
  end;

  { TmndAddons }

  TmndAddons = class(TmndCustomAddons)
  public
    constructor Create(FreeObjects : Boolean = False); virtual;
    function Add(vAddon: TmndAddon): Integer;
  end;

  { TmndCustomHistoryItem }

  TmndCustomHistoryItem = class(TObject)
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TmndHistory }

  TmndCustomHistory = class(specialize TmnObjectList<TmndCustomHistoryItem>)
  private
    FIndex: Integer;
    FMaxCount: Integer;
    FOnChanged: TNotifyEvent;
    function GetCurrent: TmndCustomHistoryItem;
  protected
  public
    constructor Create;
    function Add(History: TmndCustomHistoryItem): Integer;
    function HaveBackward: Boolean;
    function Backward: Boolean;
    function HaveForward: Boolean;
    function Forward: Boolean;
    procedure Changed; virtual;
    property Current: TmndCustomHistoryItem read GetCurrent;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property Index: Integer read FIndex write FIndex;
  end;

  TmndSQLHistoryItem = class(TmndCustomHistoryItem)
  private
    FStrings: TStringList;
    function GetText: string;
    procedure SetText(AValue: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    property Text: string read GetText write SetText;
    property Strings: TStringList read FStrings;
  end;

  { TmndSQLHistory }

  TmndSQLHistory = class(TmndCustomHistory)
  private
    function GetCurrent: TmndSQLHistoryItem;
  protected
    function RequireItem: TmndCustomHistoryItem; override;
  public
    procedure Add(const Text: string; Silent: Boolean);
    property Current: TmndSQLHistoryItem read GetCurrent;
  end;

  { TmndAddonHistoryItem }

  TmndAddonHistoryItem = class(TmndCustomHistoryItem)
  private
    FAddon: TmndAddon;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Addon: TmndAddon read FAddon write FAddon;
  end;

  TmndAddonHistory = class(TmndCustomHistory)
  private
    function GetCurrent: TmndAddonHistoryItem;
  protected
    function RequireItem: TmndCustomHistoryItem; override;
  public
    procedure Add(const Addon: TmndAddon; Silent: Boolean);
    property Current: TmndAddonHistoryItem read GetCurrent;
  end;

  { TmndNotify }

  ImndNotify = Interface(INotifyEngine)
    ['{E6F8D9BD-F716-4758-8B08-DDDBD3FA1732}']
    procedure ServerChanged; virtual; abstract;
    procedure DatabaseChanged; virtual; abstract;
    procedure ShowMeta(vAddon: TmndAddon; vMetaItem: TmncMetaItem; vSelectDefault: Boolean); virtual; abstract;
    procedure ShowEditor(vAddon: TmndAddon; S: string); virtual; abstract;
  end;

  {TmndNotifyObjects = class(specialize TmnObjectList<TmndNotify>)
  end;}

  TmndServerInfo = record
    Engine: TmncEngine;
    Info: TmncServerInfo;
  end;

  //TmndOnNotifySession = procedure of object;

  { TmndDB }

  TmndDB = class(TObject)
  private
    FConnection: TmncSQLConnection;
    FSession: TmncSQLSession;
    FTables: TmncMetaItems;
    FSequences: TmncMetaItems;
    FProceduers: TmncMetaItems;
    FViews: TmncMetaItems;
    FFunctions: TmncMetaItems;
    FExceptions: TmncMetaItems;
    FDomains: TmncMetaItems;
    FFields: TmncMetaItems;
    FExclusive: Boolean;
    FVacuum: Boolean;
    procedure RunLoginSQL;
    procedure RunLogoutSQL;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateMeta: TmncMeta;
    procedure LoadMeta;
    procedure Open(vCreate: Boolean; DatabaseEngine, Host, Port, DatabaseName, UserName, Password, Role: string; vExclusive: Boolean = False; vVacuum: Boolean = False);
    procedure Close;
    function IsActive: Boolean;
    procedure Connected;
    procedure Disconnected;

    property Connection: TmncSQLConnection read FConnection;
    property Session: TmncSQLSession read FSession;

    property Tables: TmncMetaItems read FTables;
    property Proceduers: TmncMetaItems read FProceduers;
    property Views: TmncMetaItems read FViews;
    property Sequences: TmncMetaItems read FSequences;
    property Functions: TmncMetaItems read FFunctions;
    property Exceptions: TmncMetaItems read FExceptions;
    property Domains: TmncMetaItems read FDomains;
    property Fields: TmncMetaItems read FFields;
  end;

  { TDBEngine }

  TDBEngine = class(TmndCustomAddons, ImndNotify, INotifyEngine, INotifyEngineSetting)
  private
    FDB: TmndDB;
    FEngines: TStringLIst;
    FSetting: TmndSetting;
    FStack: TmndStack;
    FHistory: TmndAddonHistory;
    FSQLHistory: TmndSQLHistory;
    FNotifyObject: ImndNotify;
    procedure SetNotifyObject(AValue: ImndNotify);
  public
    Server: TmndServerInfo;
    constructor Create;
    destructor Destroy; override;

    procedure OpenServer(FileParams: string);
    procedure OpenServer;
    procedure OpenDatabase(AliasName: string; FileParams: string);
    procedure OpenDatabase(Resource: string; EngineName, Host, Port, User, Password, Role: string);
    procedure OpenDatabase;
    procedure CloseDatabase;
    procedure CreateDatabase;

    procedure LoadOptions;
    procedure SaveOptions;

    procedure ChangeState(State: TEditorChangeStates);

    procedure Run(vStack: TmndStack);
    procedure Run(vAddon: TmndAddon; vMetaItems: TmncMetaItems);
    procedure Run;
    //procedure Run(vMaster, vName, vValue: string; vSelect: string = '');
    procedure RegisterFilter(Filter: string);
    procedure RegisterAddon(Classes: array of TmndAddonClass);
    procedure LoadFile(FileName:string; Strings: TStrings);
    procedure SaveFile(FileName:string; Strings: TStrings);
    function GetAllSupportedFiles: string;

    procedure ServerChanged;
    procedure DatabaseChanged;
    procedure ShowMeta(vAddon: TmndAddon; vMetaItem: TmncMetaItem; vSelectDefault: Boolean);
    procedure ShowEditor(vAddon: TmndAddon; S: string);
    procedure ShowEditor(vAddon: TmndAddon; S: TStringList);

    property Setting: TmndSetting read FSetting;
    property Engines: TStringLIst read FEngines;
    property DB: TmndDB read FDB;
    property History: TmndAddonHistory read FHistory;
    property Stack: TmndStack read FStack;
    property SQLHistory: TmndSQLHistory read FSQLHistory;
    property NotifyObject: ImndNotify read FNotifyObject write SetNotifyObject {implements ImndNotify};
  end;

function DBEngine: TDBEngine;

procedure DumpMetaItems(a: TmncMetaItems);
procedure DumpMetaItem(a: TmncMetaItem);

const
  sSqliteFilter = 'Sqlite (*.sqlite)|*.sqlite|FirebirdSQL (*.fdb)|*.fdb';
  sAllFilesFilter = 'All files (*.*)|*.*';
  sFileNameFilter = '*.sqlite; *.fdb';
  sFileExtFilter = 'sqlite';
  mndConfig = 'mne.mndiewer.config';

implementation

uses
  LCLproc;

procedure DumpMetaItems(a: TmncMetaItems);
var
  i: Integer;
  t: TmncMetaItem;
begin
  for i := 0 to a.Count -1 do
  begin
    t := a.Items[i];
    DumpMetaItem(t);
  end;
  DebugLn('-----------------------------------------------------');
end;

procedure DumpMetaItem(a: TmncMetaItem);
begin
  DebugLn(AlignStr(a.Name, 20, [alsCut, alsLeft]));
end;

var
  FDBEngine: TDBEngine = nil;

function DBEngine: TDBEngine;
begin
  if FDBEngine = nil then
    FDBEngine := TDBEngine.Create;
  Result := FDBEngine;
end;

{ TmndProcess }

function TmndProcess.GetDisplayName: string;
begin
  if Addon <> nil then
    Result := Addon.Name
  else
    Result := '';
end;

constructor TmndProcess.Create;
begin
  inherited Create;
//  FMetaItems := TmncMetaItems.Create;
  FMetaItem := TmncMetaItem.Create;
end;

constructor TmndProcess.Create(vAddon: TmndAddon; vMetaItem: TmncMetaItem; AShowIn: TmndShow);
begin
  Create;
  FAddon := vAddon;
  //MetaItems.Clone(vMetaItems);
  MetaItem.Clone(vMetaItem);
  FShowIn := AShowIn;
end;

constructor TmndProcess.Create(AddonName: string; vMetaItem: TmncMetaItem; AShowIn: TmndShow);
var
  aAddon: TmndAddon;
begin
  aAddon := DBEngine.Find(AddonName);
  if aAddon = nil then
    raise Exception.Create('Addon not found: ' + AddonName);
  Create(aAddon, vMetaItem, AShowIn);
end;

constructor TmndProcess.Create(AddonName: string; vValue: string; AShowIn: TmndShow);
begin
  Create(AddonName, nil, AShowIn);
  Value := vValue;
end;

destructor TmndProcess.Destroy;
begin
  FreeAndNil(FMetaItem);
  //FreeAndNil(FMetaItems);
  inherited;
end;

procedure TmndProcess.Execute(vMetaItem: TmncMetaItem; FallDefault: Boolean);
begin
  if Addon <> nil then
  begin
    {$ifdef DEBUG}
    DebugLn('>>>Run Addon:' + Addon.Name);
    {$endif}
    Addon.Execute(vMetaItem, FallDefault);
  end;
end;

{ TmndStack }

function TmndStack.GetCurrent: TmndProcess;
begin
  Result := Last as TmndProcess;
end;

constructor TmndStack.Create;
begin
  inherited Create(True);
end;

function TmndStack.Add(AItem: TmndProcess): Integer;
begin
  Result := inherited Add(AItem);
end;

function TmndStack.Push(AItem: TmndProcess): Integer;
begin
  Result := Add(AItem);
end;

procedure TmndStack.Trim(ToCount: Integer);
begin
  SetCount(ToCount);
end;

procedure TmndStack.Pop;
begin
  if Count > 0 then
    Delete(Count - 1);
end;

procedure TmndStack.Top;
begin
  if Count > 0  then
    SetCount(1);
end;

{ TmndAddonHistory }

function TmndAddonHistory.GetCurrent: TmndAddonHistoryItem;
begin
  REsult := inherited GetCurrent as TmndAddonHistoryItem;
end;

function TmndAddonHistory.RequireItem: TmndCustomHistoryItem;
begin
  Result := TmndAddonHistoryItem.Create;
end;

procedure TmndAddonHistory.Add(const Addon: TmndAddon; Silent: Boolean);
var
  i: Integer;
  aHistory: TmndAddonHistoryItem;
begin
  aHistory := TmndAddonHistoryItem.Create;
  aHistory.Addon := Addon;
  i := inherited Add(aHistory);
  if not Silent then
    Index := i;
  Changed;
end;

{ TmndAddonHistoryItem }

constructor TmndAddonHistoryItem.Create;
begin
  inherited Create;
end;

destructor TmndAddonHistoryItem.Destroy;
begin
  inherited;
end;

{ TmndCustomHistoryItem }

constructor TmndCustomHistoryItem.Create;
begin
  inherited Create;
end;

destructor TmndCustomHistoryItem.Destroy;
begin
  inherited;
end;

{ TmndSQLHistory }

function TmndSQLHistory.RequireItem: TmndCustomHistoryItem;
begin
  Result := TmndSQLHistoryItem.Create;
end;

{ TmndCustomHistoryItem }

function TmndSQLHistoryItem.GetText: string;
begin
  Result := FStrings.Text;
end;

procedure TmndSQLHistoryItem.SetText(AValue: string);
begin
  FStrings.Text := AValue;
end;

constructor TmndSQLHistoryItem.Create;
begin
  inherited;
  FStrings := TStringList.Create;
end;

destructor TmndSQLHistoryItem.Destroy;
begin
  FreeAndNil(FStrings);
  inherited;
end;

procedure TDBEngine.RegisterAddon(Classes: array of TmndAddonClass);
var
  i: Integer;
begin
  for i := 0 to Length(Classes) - 1 do
    Add(Classes[i].Create);
end;

procedure TDBEngine.LoadFile(FileName: string; Strings: TStrings);
begin
  if FileExists(Engine.WorkSpace + FileName) then
    Strings.LoadFromFile(Engine.WorkSpace  + FileName);
end;

procedure TDBEngine.SaveFile(FileName: string; Strings: TStrings);
begin
  Strings.SaveToFile(Engine.WorkSpace + FileName);
end;

procedure TDBEngine.SaveOptions;
begin
  FSetting.SaveToFile(Engine.WorkSpace + mndConfig);
end;

procedure TDBEngine.ChangeState(State: TEditorChangeStates);
begin
end;

procedure TDBEngine.Run(vStack: TmndStack);
begin
  if (vStack = nil) and (vStack.Count = 0) then
    raise Exception.Create('Stack is empty');
  vStack.Current.Execute(vStack.Current.MetaItem, True);
end;

procedure TDBEngine.Run(vAddon: TmndAddon; vMetaItems:TmncMetaItems);
begin
  if vAddon = nil then
    raise Exception.Create('Addon not found');
  //vAddon.Execute(Value, vStack, True);
end;

procedure TDBEngine.Run;
begin
  Run(Stack);
end;

{ TmndAddons }

procedure TmndCustomAddons.EnumAddons(MasterName: string; Addons: TmndAddons; SessionActive: Boolean; OnlyDefaults:Boolean = False);
var
  i: Integer;
  aDefault: Integer;
  c: Integer;
begin
  inherited;
  aDefault := -1;
  c := 0;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Master, MasterName) and (not OnlyDefaults or (nsDefault in Items[i].Style)) and (SessionActive or not (nsNeedSession in Items[i].Style)) then
    begin
      if (aDefault < 0) and (nsDefault in Items[i].Style) then
        aDefault := c;
      if not Addons.IsExists(Items[i]) then
        Addons.Add(Items[i]);
      Inc(c);
    end;
  end;
  if aDefault >= 0 then
    Addons.Move(aDefault, 0);
end;

function TmndCustomAddons.IsExists(vAddon: TmndAddon): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if vAddon = Items[i] then
    begin
      Result := True;
      break;
    end;
  end;
end;

{ TmndAddons }

constructor TmndAddons.Create(FreeObjects: boolean);
begin
  inherited Create(FreeObjects);
end;

function TmndAddons.Add(vAddon: TmndAddon): Integer;
begin
  Result := inherited Add(vAddon);
end;

{ TmndAddon }

constructor TmndAddon.Create;
begin
  inherited;
  FImageIndex := -1;
end;

destructor TmndAddon.Destroy;
begin
  inherited Destroy;
end;

{procedure TmndAddon.Enum(Session: TmndDB; Strings: TStrings);
var
  i: Integer;
  aDefault: Integer;
  c:Integer;
begin
  inherited;
  aDefault := -1;
  c := 0;
  for i := 0 to mndClasses.Count - 1 do
  begin
    if SameText(mndClasses[i].Master, Name) then
    begin
      if (aDefault < 0) and mndClasses[i].IsDefault then
        aDefault := c;
      Strings.AddObject(mndClasses[i].Title, mndClasses[i]);
      Inc(c);
    end;
  end;
  if aDefault >= 0 then
    Strings.Move(aDefault, 0);
end;}

procedure TmndAddon.EnumAddons(Addons: TmndAddons);
begin
  DBEngine.EnumAddons(Name, Addons, DBEngine.DB.IsActive);
end;

procedure TmndAddon.EnumDefaults(Addons: TmndAddons);
begin
  DBEngine.EnumAddons(Name, Addons, DBEngine.DB.IsActive, True);
end;

procedure TmndAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
begin
end;

procedure TmndAddon.Execute(vMetaItem: TmncMetaItem; FallDefault: Boolean = False);
var
  aAddons: TmndAddons;
begin
  if CanExecute then
    DoExecute(vMetaItem)
  else if FallDefault then
  begin
    //if Addon.CanRunDefault then //TODO
    aAddons := TmndAddons.Create; //It only contain live Addons, not freed when free this list
    try
      EnumDefaults(aAddons);
      if aAddons.Count > 0 then
        aAddons[0].Execute(vMetaItem);
    finally
      aAddons.Free;
    end;
  end;
end;

function TmndAddon.GetCanExecute: Boolean;
begin
  Result := True;
end;

procedure TmndAddon.DoExecute(vMetaItem: TmncMetaItem);
begin
end;

procedure TmndAddon.ShowProperty;
begin
end;

{ TmndClass }

procedure TDBEngine.SetNotifyObject(AValue: ImndNotify);
begin
  if FNotifyObject =AValue then Exit;
  FNotifyObject :=AValue;
end;

constructor TDBEngine.Create;
begin
  inherited Create(True);
  FHistory := TmndAddonHistory.Create;
  FStack := TmndStack.Create;
  FSQLHistory := TmndSQLHistory.Create;
  FSetting := TmndSetting.Create;
  FDB := TmndDB.Create;
  Engine.RegisterNotify(Self);
  FEngines := TStringList.Create;
  mncDB.Engines.EnumConnections(FEngines);
end;

destructor TDBEngine.Destroy;
begin
  Engine.UnregisterNotify(Self);
  FreeAndNil(FDB);
  FreeAndNil(FSetting);
  FreeAndNil(FHistory);
  FreeAndNil(FSQLHistory);
  FreeAndNil(FEngines);
  FreeAndNil(FStack);
  inherited;
end;

procedure TDBEngine.OpenServer(FileParams: string);
var
  EngineName, Resource, Host, Port, User, Password, Role: string;
  AliasName: string;
begin
  mncDB.Engines.DecomposeConnectionString(FileParams, EngineName, Resource, Host, Port, User, Password, Role);
  Server.Engine := mncDB.Engines.Find(EngineName);

  Server.Info.Host := Host;
  Server.Info.Port := Port;
  Server.Info.UserName := User;
  Server.Info.Password := Password;
  Server.Info.Role := Role;

  if Server.Info.Host = '' then
    Server.Info.Host := 'localhost';

  if (ccPath in Server.Engine.ConnectionClass.Capabilities) and not (ccNetwork in Server.Engine.ConnectionClass.Capabilities) then
  begin
    AliasName := ExtractFilePath(Resource);
  end
  else
  begin
    AliasName := '@' + EngineName + ':' + Server.Info.Host;
    if Server.Info.Port <> '' then
      AliasName := AliasName + ':' + Server.Info.Port;
  end;
  Engine.ProcessRecentDatabase(AliasName, FileParams);

  ServerChanged;
  Engine.SendAction(eaShowDatabases);
end;

procedure TDBEngine.OpenServer;
var
  FileParams: string;
  EngineName, Host, Port, User, Password, Role: string;
begin
  with TConnectDBServerForm.Create(Application) do
  begin
    if ShowModal = mrOK then
    begin
      EngineName := (DatabaseEngineCbo.Items.Objects[DatabaseEngineCbo.ItemIndex] as TmncEngine).Name;

      Host := HostEdit.Text;
      Port := PortEdit.Text;
      User := UserEdit.Text;
      Password := PasswordEdit.Text;
      Role := RoleEdit.Text;

      FileParams := mncDB.Engines.ComposeConnectionString(EngineName, '', Host, Port, User, Password, Role);
      OpenServer(FileParams);
    end;
  end;
end;

procedure TDBEngine.OpenDatabase(AliasName: string; FileParams: string);
var
  EngineName, Resource, Host, Port, User, Password, Role: string;
  aName: string;
  AEngine: TmncEngine;
begin
  mncDB.Engines.DecomposeConnectionString(FileParams, EngineName, Resource, Host, Port, User, Password, Role);
  if Resource = '' then
  begin
    OpenServer(FileParams);
    Exit;
  end;

  AEngine := mncDB.Engines.Find(EngineName);

  if (ccPath in AEngine.ConnectionClass.Capabilities) and
     (not (ccNetwork in AEngine.ConnectionClass.Capabilities) or (Host = '')) then
  begin
    Engine.ProcessRecentDatabase(Resource, FileParams); //sqlite, or firebird as file
  end
  else
  begin
    aName := Resource + '@' + EngineName + ':';
    if Host = '' then
      aName := aName + 'localhost'
    else if Port <> '' then
      aName := aName + Host + ':' + Port
    else
      aName := aName + Host;
    Engine.ProcessRecentDatabase(aName, FileParams);//sqlite, or firebird as file
  end;

  if DB.IsActive then
    DB.Close;

  //Setting.CacheMetas := CacheMetaChk.Checked;
  DB.Open(False, EngineName, Host, Port, Resource, User, Password, Role, False, False);
  DatabaseChanged;
  Stack.Clear;
  Stack.Push(TmndProcess.Create('Table', ''));
  Run;
  Engine.SendAction(eaShowDatabases);
end;

procedure TDBEngine.OpenDatabase(Resource: string; EngineName, Host, Port, User, Password, Role: string);
var
  FileParams: string;
begin
  FileParams := mncDB.Engines.ComposeConnectionString(EngineName, Resource, Host, Port, User, Password, Role);
  OpenDatabase(Resource, FileParams);
end;

procedure TDBEngine.OpenDatabase;
var
  EngineName, Resource, Host, Port, User, Password, Role: string;
begin
  with TOpenDatabaseForm.Create(Application) do
  begin
    if ShowModal = mrOK then
    begin
      EngineName := (DatabaseEngineCbo.Items.Objects[DatabaseEngineCbo.ItemIndex] as TmncEngine).Name;

      Host := HostEdit.Text;
      Port := PortEdit.Text;
      Resource := DatabaseEdit.Text;
      User := UserEdit.Text;
      Password := PasswordEdit.Text;
      Role := RoleEdit.Text;
      //ExclusiveChk.Checked,
      OpenDatabase(Resource, EngineName, Host, Port, User, Password, Role);
    end;
  end;
end;

procedure TDBEngine.CloseDatabase;
begin
  if DB.IsActive then
    DB.Close;
  DatabaseChanged;
end;

procedure TDBEngine.CreateDatabase;
begin
  with TOpenDatabaseForm.Create(Application) do
  begin
    if ShowModal = mrOK then
    begin
      if DBEngine.DB.IsActive then
        DBEngine.DB.Close;

      DBEngine.DB.Open(True, (DatabaseEngineCbo.Items.Objects[DatabaseEngineCbo.ItemIndex] as TmncEngine).Name, HostEdit.Text, PortEdit.Text, DatabaseEdit.Text, UserEdit.Text, PasswordEdit.Text, RoleEdit.Text, ExclusiveChk.Checked);
      DBEngine.Stack.Clear;
      DBEngine.Stack.Push(TmndProcess.Create('Table', DatabaseEdit.Text));
      DBEngine.Run(DBEngine.Stack);
    end;
  end;
end;

procedure TDBEngine.RegisterFilter(Filter: string);
begin
  if AnsiPos('|', Filter) = 0 then
    raise EmndException.Create('Invalid mndiewer filter');
  if AnsiPos(Filter, Setting.OpenSaveDialogFilters) = 0 then
  begin
    if Setting.OpenSaveDialogFilters <> '' then
      Setting.OpenSaveDialogFilters := Setting.OpenSaveDialogFilters + ';';
    Setting.OpenSaveDialogFilters := Setting.OpenSaveDialogFilters + Filter;
  end;
end;

function TDBEngine.GetAllSupportedFiles: string;
var
  aStrings: TStringList;
  s: string;
  i: Integer;
begin
  s := '.sqlite|.fdb';
  if s <> '' then
    s := s + '|';
  s := s + Setting.OpenSaveDialogFilters;
  if s <> '' then
  begin
    Result := 'All Supported|*.sqlite';
    aStrings := TStringList.Create;
    StrToStrings(s, aStrings, ['|'], [' ', #13, #10]);
    for i := 0 to aStrings.Count - 1 do
    begin
      if Odd(i) then
        Result := Result + ';' + aStrings[i];
    end;
    aStrings.Free;
    Result := Result + '|' + sSqliteFilter + '|' + s + '|' + sAllFilesFilter;
  end
  else
    Result := sSqliteFilter + '|' + sAllFilesFilter;
end;

procedure TDBEngine.ServerChanged;
begin
  FNotifyObject.ServerChanged;
end;

procedure TDBEngine.DatabaseChanged;
begin
  FNotifyObject.DatabaseChanged;
end;

procedure TDBEngine.ShowMeta(vAddon: TmndAddon; vMetaItem: TmncMetaItem; vSelectDefault: Boolean);
begin
  FNotifyObject.ShowMeta(vAddon, vMetaItem, vSelectDefault);
end;

procedure TDBEngine.ShowEditor(vAddon: TmndAddon; S: string);
begin
  FNotifyObject.ShowEditor(vAddon, S);
end;

procedure TDBEngine.ShowEditor(vAddon: TmndAddon; S: TStringList);
begin
  FNotifyObject.ShowEditor(vAddon, S.Text);
end;

procedure TDBEngine.LoadOptions;
begin
  FSetting.SafeLoadFromFile(Engine.WorkSpace + mndConfig);
end;

{ TmndCustomHistory }

procedure TmndCustomHistory.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

function TmndCustomHistory.GetCurrent: TmndCustomHistoryItem;
begin
  if (Index < Count) and (FIndex >=0) then
    Result := Items[Index]
  else
    Result := nil;
end;

constructor TmndCustomHistory.Create;
begin
  inherited Create(True);
  Index := 0;
  MaxCount := 50;
end;

function TmndCustomHistory.Add(History: TmndCustomHistoryItem): Integer;
begin
  if (Count > MaxCount) and (Count > 0) then
  begin
    Delete(0);
    FIndex := FIndex - 1
  end;
  Result := inherited Add(History);
end;

function TmndSQLHistory.GetCurrent: TmndSQLHistoryItem;
begin
  Result := inherited GetCurrent as TmndSQLHistoryItem;
end;

procedure TmndSQLHistory.Add(const Text: string; Silent: Boolean);
var
  i: Integer;
  aHistory: TmndSQLHistoryItem;
begin
  if (Count > 0) then
  begin
    aHistory := Items[Count - 1] as TmndSQLHistoryItem;
    if (aHistory.Text = Text) then
      exit;//do not duplicate the last one
    if (Index < Count) and (Index >= 0) then
    begin
      aHistory := Items[Index] as TmndSQLHistoryItem;
      if (aHistory.Text = Text) then
        exit;//do not duplicate the current one
    end;
  end;
  aHistory := TmndSQLHistoryItem.Create;
  aHistory.Text := Text;
  //aHistory.Addon := AAddon;
  i := inherited Add(aHistory);
  if not Silent then
    Index := i;
  Changed;
end;

function TmndCustomHistory.HaveForward: Boolean;
begin
  Result := (Count > 0) and (Index < Count - 1);
end;

function TmndCustomHistory.HaveBackward: Boolean;
begin
  Result := (Count> 0) and (FIndex > 0);
end;

function TmndCustomHistory.Forward: Boolean;
begin
  Result := HaveForward;
  if Result then
  begin
    Index := FIndex + 1;
    Changed;
  end;
end;

function TmndCustomHistory.Backward: Boolean;
begin
  Result := HaveBackward;
  if Result then
  begin
    Index := FIndex - 1;
    Changed;
  end;
end;

{ TmndSetting }

constructor TmndSetting.Create;
begin
  inherited Create;
  FCSVQuoteChar := '"';
  FCSVDelimiterChar := ';';
  FCacheMetas := True;
end;

{ TmndDB }

procedure TmndDB.Connected;
begin
  if FVacuum then
    Connection.Vacuum;
  Session.Start;
  LoadMeta;
  RunLoginSQL;
end;

constructor TmndDB.Create;
begin
  inherited;
  FTables := TmncMetaItems.Create;
  FProceduers := TmncMetaItems.Create;
  FViews := TmncMetaItems.Create;
  FSequences := TmncMetaItems.Create;
  FExceptions := TmncMetaItems.Create;
  FFunctions := TmncMetaItems.Create;
  FDomains := TmncMetaItems.Create;
  FFields := TmncMetaItems.Create;
end;

destructor TmndDB.Destroy;
begin
  FreeAndNil(FTables);
  FreeAndNil(FProceduers);
  FreeAndNil(FViews);
  FreeAndNil(FSequences);
  FreeAndNil(FExceptions);
  FreeAndNil(FFunctions);
  FreeAndNil(FDomains);
  FreeAndNil(FFields);
  FreeAndNil(FSession);
  FreeAndNil(FConnection);
  inherited;
end;

function TmndDB.CreateMeta: TmncMeta;
begin
  Result := Engines.CreateMeta(Connection);
  Result.Link := Session;
end;

procedure TmndDB.Disconnected;
begin
  RunLogoutSQL;
end;

procedure TmndDB.LoadMeta;
var
  AMeta: TmncMeta;
begin
  if DBEngine.Setting.CacheMetas then
  begin
    AMeta := Engines.CreateMeta(FConnection);
    try
      AMeta.Link := Session;
      AMeta.EnumObjects(Tables, sokTable, DBEngine.DB.Connection.Resource, [ekSystem, ekSort]);
{      AMeta.EnumObjects(Views, sokView, '', [ekSort]);
      AMeta.EnumObjects(Proceduers, sokProcedure, '', [ekSort]);
      AMeta.EnumObjects(Sequences, sokSequence, '', [ekSort]);
      AMeta.EnumObjects(Functions, sokFunction, '', [ekSort]);
      AMeta.EnumObjects(Exceptions, sokException, '', [ekSort]);
      AMeta.EnumObjects(Domains, sokDomain, '', [ekSort]);
      AMeta.EnumObjects(Fields, sokField);}
    finally
      AMeta.Free;
    end;
  end;
end;

procedure TmndDB.Open(vCreate: Boolean; DatabaseEngine, Host, Port, DatabaseName, UserName, Password, Role: string; vExclusive: Boolean; vVacuum: Boolean);
begin
  FConnection := Engines.CreateConnection(DatabaseEngine) as TmncSQLConnection;

  FVacuum := vVacuum;
  FExclusive := vExclusive;

  Connection.Resource := DatabaseName;
  Connection.UserName := UserName;
  Connection.Password := Password;
  Connection.Role := Role;
  //Connection.AutoCreate := vAutoCreate;
  //DBConnection.Exclusive := FExclusive;//TODO

  FSession := FConnection.CreateSession;

  Connection.Connect;
  Connected;
  //Engine.SendLog()
  Engine.Update([ecsChanged, ecsState, ecsRefresh, ecsRecents, ecsProject, ecsProjectLoaded]);
end;

procedure TmndDB.Close;
begin
  if IsActive then
    Disconnected;
  if (Session <> nil) and Session.Active then
    Session.Stop;
  if (Connection <> nil) and Connection.Connected then
    Connection.Disconnect;
  FreeAndNil(FSession);
  FreeAndNil(FConnection);
end;

function TmndDB.IsActive: Boolean;
begin
  Result := (Connection <> nil) and Connection.Active;
end;

procedure TmndDB.RunLoginSQL;
var
  CMD: TmncSQLCommand;
begin
  CMD := Session.CreateCommand;
  try
    if DBEngine.Setting.InternalLoginSQL <> '' then
    begin
      CMD.SQL.Text := DBEngine.Setting.InternalLoginSQL;
      CMD.Execute;
      CMD.Session.Commit(True);
    end;
    if DBEngine.Setting.LoginSQL <> '' then
    begin
      CMD.Close;
      CMD.SQL.Text := DBEngine.Setting.LoginSQL;
      CMD.Execute;
      CMD.Session.Commit(True);
    end;
  finally
    CMD.Free;
  end;
end;

procedure TmndDB.RunLogoutSQL;
var
  CMD: TmncSQLCommand;
begin
  CMD := Session.CreateCommand;
  try
    if DBEngine.Setting.InternalLogoutSQL <> '' then
    begin
      CMD.SQL.Text := DBEngine.Setting.InternalLogoutSQL;
      CMD.Execute;
      CMD.Session.Commit(True);
    end;
    if DBEngine.Setting.LogoutSQL <> '' then
    begin
      CMD.Close;
      CMD.SQL.Text := DBEngine.Setting.LogoutSQL;
      CMD.Execute;
      CMD.Session.Commit(True);
    end;
  finally
    CMD.Free;
  end;
end;

initialization
finalization
  FreeAndNil(FDBEngine);
end.
