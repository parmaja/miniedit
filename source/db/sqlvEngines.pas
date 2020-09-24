unit sqlvEngines;
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
  sqlvOpenDatabases, sqlvConnectServers;

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
  EsqlvException = class(Exception);

  TsqlvExecuteType = (execNormal, execExport, execImport);

  TMetaInfo = record
    Name: string;
    Value: string;
  end;

  { TsqlvSetting }

  TsqlvSetting = class(TmnXMLProfile)
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
    property CacheMetas: Boolean read FCacheMetas write FCacheMetas default False;
    property OpenSaveDialogFilters:string read FOpenSaveDialogFilters write FOpenSaveDialogFilters;
    property LoginSQL: string read FLoginSQL write FLoginSQL;
    property LogoutSQL: string read FLogoutSQL write FLogoutSQL;
    property CSVQuoteChar: Char read FCSVQuoteChar write FCSVQuoteChar default '"';
    property CSVDelimiterChar: Char read FCSVDelimiterChar write FCSVDelimiterChar default ';';
    property CSVHeader: TmncCSVHeader read FCSVHeader write FCSVHeader default hdrNone;
    property CSVANSIContents: Boolean read FCSVANSIContents write FCSVANSIContents default False;
  end;

  { TsqlvAttribute }

  TsqlvAttribute = class(TmnField)
  private
  protected
  public
    procedure Clone(Attribute: TsqlvAttribute); virtual;
  end;

  { TsqlvAttributes }

  TsqlvAttributes = class(TmnFields)
  private
    function GetItem(Index: Integer): TsqlvAttribute;
  protected
    function CreateField: TmnField; override;
  public
    procedure Clone(vAttributes: TsqlvAttributes);//Assign procedure used in parent class :(
    property Items[Index: Integer]: TsqlvAttribute read GetItem;
  end;

  TsqlvShow = (shwnElement, shwnBrowse, shwnFile);

  TsqlvAddon = class;

  { TsqlvProcess }

  TsqlvProcess = class(TObject)
  private
    FAddon: TsqlvAddon;
    FAttributes: TsqlvAttributes;
    FSelect: string;
    FShowIn: TsqlvShow;
  public
    constructor Create;
    constructor Create(vAddon: TsqlvAddon; vSelect: string; vAttributes: TsqlvAttributes = nil; AShowIn: TsqlvShow = shwnElement);
    constructor Create(Group, Name: string; vSelect: string; vAttributes: TsqlvAttributes = nil; AShowIn: TsqlvShow = shwnElement);
    constructor Create(Group, Name: string; vSelect: string; vValue: string; AShowIn: TsqlvShow = shwnElement);

    destructor Destroy; override;
    property Addon: TsqlvAddon read FAddon write FAddon;
    property Attributes: TsqlvAttributes read FAttributes write FAttributes;
    property Select: string read FSelect write FSelect;
    property ShowIn: TsqlvShow read FShowIn write FShowIn;
  end;

  { TsqlvStack }

  TsqlvStack = class(specialize TmnObjectList<TsqlvProcess>)
  private
    function GetCurrent: TsqlvProcess;
  public
    constructor Create;
    function Add(AItem: TsqlvProcess): Integer; overload;
    function Push(AItem: TsqlvProcess): Integer;
    procedure Pop;
    procedure Top; //pop all to top
    procedure Trim(ToCount: Integer); //Similar to SetCount
    property Current: TsqlvProcess read GetCurrent;
  end;

  TsqlvAddons = class;

{
  nsDefault: Default Addon for execute parent when parent can not parent
  nsCommand: It is command not SQL member
  nsEditor: Show a script in SQL editor like triggers or stored prpocedures
  nsButton: Make it visible as button or menu in gui form
  nsNeedSession: Enum only when session is active
}

  TsqlvAddonStyle = set of (nsDefault, nsGroup, nsCommand, nsEditor, nsButton, nsNeedSession);

  IsqlvAddon = interface(IInterface)
    function GetAddon: TsqlvAddon;
  end;

  { TsqlvAddon }

  TsqlvAddon = class(TmnNamedObject)
  private
    FItemName: string;
    FGroup: string;
    FStyle: TsqlvAddonStyle;
    FTitle: string;
    FKind: TmetaKind;
    FImageIndex: TImageIndex;
  protected
    function GetCanExecute: Boolean; virtual;
    procedure DoExecute(vAttributes: TsqlvAttributes); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ShowProperty; virtual;
    procedure Execute(vAttributes: TsqlvAttributes; FallDefault: Boolean = False);
    procedure Execute(const Value: string);
    procedure Enum(Addons: TsqlvAddons);
    procedure EnumDefaults(Addons: TsqlvAddons);
    procedure EnumHeader(Header: TStringList); virtual;
    procedure EnumMeta(vItems: TmncMetaItems; vAttributes: TsqlvAttributes = nil); virtual; abstract;
    property CanExecute: Boolean read GetCanExecute;
    property Group: string read FGroup write FGroup; //Group is parent Addon like Tabkes.Group = 'Database'
    //property Name: string read FName write FName; //Name = 'Tables'
    property ItemName: string read FItemName write FItemName; //Item name eg  Tables.Item = 'Table'
    property Kind: TmetaKind read FKind write FKind default sokNone;
    property Style: TsqlvAddonStyle read FStyle write FStyle;
    property Title: string read FTitle write FTitle;
    property ImageIndex: TImageIndex read FImageIndex write FImageIndex default -1;
  end;

  TsqlvAddonClass = class of TsqlvAddon;

  { TsqlvCustomAddons }

  TsqlvCustomAddons = class(specialize TmnNamedObjectList<TsqlvAddon>)
  private
  public
    procedure Enum(GroupName: string; Addons: TsqlvAddons; SessionActive: Boolean; OnlyDefaults: Boolean = False); overload;
    function Find(const Group, Name: string): TsqlvAddon;
    function Find(const Group, Name: string; Deep: Boolean): TsqlvAddon;
    function IsExists(vAddon: TsqlvAddon): Boolean;
  end;

  { TsqlvAddons }

  TsqlvAddons = class(TsqlvCustomAddons)
  public
    constructor Create; virtual;
    function Add(vAddon: TsqlvAddon): Integer;
  end;

  { TsqlvCustomHistoryItem }

  TsqlvCustomHistoryItem = class(TObject)
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TsqlvHistory }

  TsqlvCustomHistory = class(specialize TmnObjectList<TsqlvCustomHistoryItem>)
  private
    FIndex: Integer;
    FMaxCount: Integer;
    FOnChanged: TNotifyEvent;
    function GetCurrent: TsqlvCustomHistoryItem;
  protected
  public
    constructor Create;
    function Add(History: TsqlvCustomHistoryItem): Integer;
    function HaveBackward: Boolean;
    function Backward: Boolean;
    function HaveForward: Boolean;
    function Forward: Boolean;
    procedure Changed; virtual;
    property Current: TsqlvCustomHistoryItem read GetCurrent;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property Index: Integer read FIndex write FIndex;
  end;

  TsqlvSQLHistoryItem = class(TsqlvCustomHistoryItem)
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

  { TsqlvSQLHistory }

  TsqlvSQLHistory = class(TsqlvCustomHistory)
  private
    function GetCurrent: TsqlvSQLHistoryItem;
  protected
    function RequireItem: TsqlvCustomHistoryItem; override;
  public
    procedure Add(const Text: string; Silent: Boolean);
    property Current: TsqlvSQLHistoryItem read GetCurrent;
  end;

  { TsqlvAddonHistoryItem }

  TsqlvAddonHistoryItem = class(TsqlvCustomHistoryItem)
  private
    FAddon: TsqlvAddon;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Addon: TsqlvAddon read FAddon write FAddon;
  end;

  TsqlvAddonHistory = class(TsqlvCustomHistory)
  private
    function GetCurrent: TsqlvAddonHistoryItem;
  protected
    function RequireItem: TsqlvCustomHistoryItem; override;
  public
    procedure Add(const Addon: TsqlvAddon; Silent: Boolean);
    property Current: TsqlvAddonHistoryItem read GetCurrent;
  end;


  { TsqlvNotify }

  IsqlvNotify = Interface(IInterface)
    ['{E6F8D9BD-F716-4758-8B08-DDDBD3FA1732}']
    procedure ServerChanged; virtual; abstract;
    procedure ShowMeta(vAddon: TsqlvAddon; vSelectDefault: Boolean); virtual; abstract;
    procedure LoadEditor(vAddon: TsqlvAddon; S: string); virtual; abstract;
  end;

  {TsqlvNotifyObjects = class(specialize TmnObjectList<TsqlvNotify>)
  end;}

  TsqlvServerInfo = record
    Engine: TmncEngine;
    Info: TmncServerInfo;
  end;

  //TsqlvOnNotifySession = procedure of object;

  { TsqlvDB }

  TsqlvDB = class(TObject)
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
    procedure Open(vCreate:Boolean; DatabaseEngine, DatabaseName, UserName, Password, Role: string; vExclusive: Boolean = False; vVacuum: Boolean = False);
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

  TDBEngine = class(TsqlvCustomAddons, IsqlvNotify, INotifyEngine, ISettingNotifyEngine)
  private
    FDB: TsqlvDB;
    FEngines: TStringLIst;
    FSetting: TsqlvSetting;
    FRecents: TStringList;
    FStack: TsqlvStack;
    FHistory: TsqlvAddonHistory;
    FSQLHistory: TsqlvSQLHistory;
    FNotifyObject: IsqlvNotify;
    procedure SetNotifyObject(AValue: IsqlvNotify);
  public
    Server: TsqlvServerInfo;
    constructor Create;
    destructor Destroy; override;

    procedure ConnectServer;
    procedure OpenDatabase(FileName: string; FileParam: string);
    procedure OpenDatabase;
    procedure CreateDatabase;

    procedure LoadOptions;
    procedure SaveOptions;

    procedure ChangeState(State: TEditorChangeStates);

    procedure Run(vStack: TsqlvStack);
    procedure Run(vAddon: TsqlvAddon; vAttributes: TsqlvAttributes);
    procedure Run;
    //procedure Run(vGroup, vName, vValue: string; vSelect: string = '');
    procedure RegisterFilter(Filter: string);
    procedure RegisterViewer(Classes: array of TsqlvAddonClass);
    procedure AddRecent(Name:string);
    procedure LoadFile(FileName:string; Strings: TStrings);
    procedure SaveFile(FileName:string; Strings: TStrings);
    function GetAllSupportedFiles: string;

    procedure ServerChanged;
    procedure ShowMeta(vAddon: TsqlvAddon; vSelectDefault: Boolean);
    procedure LoadEditor(vAddon: TsqlvAddon; S: string);
    procedure LoadEditor(vAddon: TsqlvAddon; S: TStringList);

    property Setting: TsqlvSetting read FSetting;
    property Recents: TStringList read FRecents;
    property Engines: TStringLIst read FEngines;
    property DB: TsqlvDB read FDB;
    property History: TsqlvAddonHistory read FHistory;
    property Stack: TsqlvStack read FStack;
    property SQLHistory: TsqlvSQLHistory read FSQLHistory;
    property NotifyObject: IsqlvNotify read FNotifyObject write SetNotifyObject {implements IsqlvNotify};
  end;

function DBEngine: TDBEngine;

procedure DumpAttributes(a: TsqlvAttributes);

const
  sSqliteFilter = 'Sqlite (*.sqlite)|*.sqlite|FirebirdSQL (*.fdb)|*.fdb';
  sAllFilesFilter = 'All files (*.*)|*.*';
  sFileNameFilter = '*.sqlite; *.fdb';
  sFileExtFilter = 'sqlite';
  sqlvConfig = 'mne.sqlviewer.config';
  sqlvRecents = 'mne.sqlviewer.recents';

implementation

uses
  LCLproc;

procedure DumpAttributes(a: TsqlvAttributes);
var
  i: Integer;
  t: TsqlvAttribute;
begin
  for i := 0 to a.Count -1 do
  begin
    t := a.Items[i];
    DebugLn(AlignStr(t.Name, 20, [alsCut, alsLeft]) + ': ' + t.Value);
  end;
  DebugLn('-----------------------------------------------------');
end;

var
  FDBEngine: TDBEngine = nil;

function DBEngine: TDBEngine;
begin
  if FDBEngine = nil then
    FDBEngine := TDBEngine.Create;
  Result := FDBEngine;
end;

{ TsqlvAttribute }

procedure TsqlvAttribute.Clone(Attribute: TsqlvAttribute);
begin
  Name := Attribute.Name;
  Value := Attribute.Value;
end;

{ TsqlvProcess }

constructor TsqlvProcess.Create;
begin
  inherited Create;
  FAttributes := TsqlvAttributes.Create;
end;

constructor TsqlvProcess.Create(vAddon: TsqlvAddon; vSelect: string; vAttributes: TsqlvAttributes; AShowIn: TsqlvShow);
begin
  Create;
  Addon := vAddon;
  Select := vSelect;
  Attributes.Clone(vAttributes);
  FShowIn := AShowIn;
end;

constructor TsqlvProcess.Create(Group, Name: string; vSelect: string; vAttributes: TsqlvAttributes; AShowIn: TsqlvShow);
var
  aAddon: TsqlvAddon;
begin
  aAddon := DBEngine.Find(Group, Name, True);
  if aAddon = nil then
    raise Exception.Create('Addon not found' + Group + '\' + Name);
  Create(aAddon, Select, vAttributes, AShowIn);
end;

constructor TsqlvProcess.Create(Group, Name: string; vSelect: string; vValue: string; AShowIn: TsqlvShow);
var
  a: TsqlvAttributes;
begin
  a := TsqlvAttributes.Create;
  try
    a.Add(Name, vValue);
    Create(Group, Name, Select, a, AShowIn);
  finally
    a.Free;
  end;
end;

destructor TsqlvProcess.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

{ TsqlvAttributes }

function TsqlvAttributes.GetItem(Index: Integer): TsqlvAttribute;
begin
  Result := (inherited Items[Index]) as TsqlvAttribute;
end;

function TsqlvAttributes.CreateField: TmnField;
begin
  Result := TsqlvAttribute.Create;
end;

procedure TsqlvAttributes.Clone(vAttributes: TsqlvAttributes);
var
  i: Integer;
  a: TsqlvAttribute;
begin
  Clear;
  if vAttributes <> nil then
  begin
    for i := 0 to vAttributes.Count -1 do
    begin
      a := TsqlvAttribute.Create;
      a.Clone(vAttributes.Items[i]);
      Add(a);
    end;
  end;
end;

{ TsqlvStack }

function TsqlvStack.GetCurrent: TsqlvProcess;
begin
  Result := Last as TsqlvProcess;
end;

constructor TsqlvStack.Create;
begin
  inherited Create(True);
end;

function TsqlvStack.Add(AItem: TsqlvProcess): Integer;
begin
  Result := inherited Add(AItem);
end;

function TsqlvStack.Push(AItem: TsqlvProcess): Integer;
begin
  Result := Add(AItem);
end;

procedure TsqlvStack.Trim(ToCount: Integer);
begin
  SetCount(ToCount);
end;

procedure TsqlvStack.Pop;
begin
  if Count > 0 then
    Delete(Count - 1);
end;

procedure TsqlvStack.Top;
begin
  if Count > 0  then
    SetCount(1);
end;

{ TsqlvAddonHistory }

function TsqlvAddonHistory.GetCurrent: TsqlvAddonHistoryItem;
begin
  REsult := inherited GetCurrent as TsqlvAddonHistoryItem;
end;

function TsqlvAddonHistory.RequireItem: TsqlvCustomHistoryItem;
begin
  Result := TsqlvAddonHistoryItem.Create;
end;

procedure TsqlvAddonHistory.Add(const Addon: TsqlvAddon; Silent: Boolean);
var
  i: Integer;
  aHistory: TsqlvAddonHistoryItem;
begin
  aHistory := TsqlvAddonHistoryItem.Create;
  aHistory.Addon := Addon;
  i := inherited Add(aHistory);
  if not Silent then
    Index := i;
  Changed;
end;

{ TsqlvAddonHistoryItem }

constructor TsqlvAddonHistoryItem.Create;
begin
  inherited Create;
end;

destructor TsqlvAddonHistoryItem.Destroy;
begin
  inherited;
end;

{ TsqlvCustomHistoryItem }

constructor TsqlvCustomHistoryItem.Create;
begin
  inherited Create;
end;

destructor TsqlvCustomHistoryItem.Destroy;
begin
  inherited;
end;

{ TsqlvSQLHistory }

function TsqlvSQLHistory.RequireItem: TsqlvCustomHistoryItem;
begin
  Result := TsqlvSQLHistoryItem.Create;
end;

{ TsqlvCustomHistoryItem }

function TsqlvSQLHistoryItem.GetText: string;
begin
  Result := FStrings.Text;
end;

procedure TsqlvSQLHistoryItem.SetText(AValue: string);
begin
  FStrings.Text := AValue;
end;

constructor TsqlvSQLHistoryItem.Create;
begin
  inherited;
  FStrings := TStringList.Create;
end;

destructor TsqlvSQLHistoryItem.Destroy;
begin
  FreeAndNil(FStrings);
  inherited;
end;

procedure TDBEngine.RegisterViewer(Classes: array of TsqlvAddonClass);
var
  i: Integer;
begin
  for i := 0 to Length(Classes) - 1 do
    Add(Classes[i].Create);
end;

procedure TDBEngine.AddRecent(Name: string);
var
  i: Integer;
begin
  i := Recents.IndexOf(Name);
  if i > 0 then
    Recents.Move(i, 0)
  else if i < 0 then
    Recents.Insert(0, Name);
  if Recents.Count > 10 then
    Recents.Capacity := 10;
  FRecents.SaveToFile(Engine.WorkSpace + sqlvRecents);
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
  FSetting.SaveToFile(Engine.WorkSpace + sqlvConfig);
  FRecents.SaveToFile(Engine.WorkSpace + sqlvRecents);
end;

procedure TDBEngine.ChangeState(State: TEditorChangeStates);
begin
end;

procedure TDBEngine.Run(vStack: TsqlvStack);
begin
  if (vStack = nil) and (vStack.Count = 0) then
    raise Exception.Create('Stack is empty');
  with vStack.Current do
  begin
    if Addon <> nil then
    begin
      {$ifdef DEBUG}
      DebugLn('>>>Run Addon:' + Addon.Name);
      {$endif}
      Addon.Execute(vStack.Current.Attributes, True);
    end;
  end;
end;

procedure TDBEngine.Run(vAddon: TsqlvAddon; vAttributes:TsqlvAttributes);
begin
  if vAddon = nil then
    raise Exception.Create('Addon not found');
  //vAddon.Execute(Value, vStack, True);
end;

procedure TDBEngine.Run;
begin
  Run(Stack);
end;

{ TsqlvAddons }

procedure TsqlvCustomAddons.Enum(GroupName: string; Addons: TsqlvAddons; SessionActive: Boolean; OnlyDefaults:Boolean = False);
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
    if SameText(Items[i].Group, GroupName) and (not OnlyDefaults or (nsDefault in Items[i].Style)) and (SessionActive or not (nsNeedSession in Items[i].Style)) then
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

function TsqlvCustomAddons.Find(const Group, Name: string): TsqlvAddon;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Group, Items[i].Group) and SameText(Name, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TsqlvCustomAddons.Find(const Group, Name: string; Deep: Boolean): TsqlvAddon;
var
  aAddons: TsqlvAddons;
begin
  Result := Find(Group, Name);
  if Deep then
    if Result = nil then
    begin
      Result := Find(Group, Name);
      if (Result = nil) then
      begin
        aAddons := TsqlvAddons.Create;
        try
          Enum(Name, aAddons, True);
          if aAddons.Count > 0 then
            Result := aAddons[0];
        finally
          aAddons.Free;
        end;
      end;
    end;
end;

function TsqlvCustomAddons.IsExists(vAddon: TsqlvAddon): Boolean;
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

{ TsqlvAddons }

constructor TsqlvAddons.Create;
begin
  inherited Create(False);
end;

function TsqlvAddons.Add(vAddon: TsqlvAddon): Integer;
begin
  Result := inherited Add(vAddon);
end;

{ TsqlvAddon }

constructor TsqlvAddon.Create;
begin
  inherited;
  FImageIndex := -1;
end;

destructor TsqlvAddon.Destroy;
begin
  inherited Destroy;
end;

{procedure TsqlvAddon.Enum(Session: TsqlvDB; Strings: TStrings);
var
  i: Integer;
  aDefault: Integer;
  c:Integer;
begin
  inherited;
  aDefault := -1;
  c := 0;
  for i := 0 to sqlvClasses.Count - 1 do
  begin
    if SameText(sqlvClasses[i].Group, Name) then
    begin
      if (aDefault < 0) and sqlvClasses[i].IsDefault then
        aDefault := c;
      Strings.AddObject(sqlvClasses[i].Title, sqlvClasses[i]);
      Inc(c);
    end;
  end;
  if aDefault >= 0 then
    Strings.Move(aDefault, 0);
end;}

procedure TsqlvAddon.Enum(Addons: TsqlvAddons);
begin
  DBEngine.Enum(Name, Addons, DBEngine.DB.IsActive);
end;

procedure TsqlvAddon.EnumDefaults(Addons: TsqlvAddons);
begin
  DBEngine.Enum(Name, Addons, DBEngine.DB.IsActive, True);
end;

procedure TsqlvAddon.EnumHeader(Header: TStringList);
begin
  Header.Clear;
  if ItemName = '' then
    Header.Add(Title)
  else
    Header.Add(ItemName);
end;

procedure TsqlvAddon.Execute(vAttributes: TsqlvAttributes; FallDefault: Boolean = False);
var
  aAddons: TsqlvAddons;
begin
  if CanExecute then
    DoExecute(vAttributes)
  else if FallDefault then
  begin
    //if Addon.CanRunDefault then //TODO
    aAddons := TsqlvAddons.Create; //It only contain live Addons, not freed when free this list
    try
      EnumDefaults(aAddons);
{      if vValue = '' then
        vValue := Addon.Name;}//TODO
      if aAddons.Count > 0 then
        aAddons[0].Execute(vAttributes);
    finally
      aAddons.Free;
    end;
  end;
end;

procedure TsqlvAddon.Execute(const Value: string);
begin
  DoExecute(nil);
end;

function TsqlvAddon.GetCanExecute: Boolean;
begin
  Result := True;
end;

procedure TsqlvAddon.DoExecute(vAttributes: TsqlvAttributes);
begin
end;

procedure TsqlvAddon.ShowProperty;
begin
end;

{ TsqlvClass }

procedure TDBEngine.SetNotifyObject(AValue: IsqlvNotify);
begin
  if FNotifyObject =AValue then Exit;
  FNotifyObject :=AValue;
end;

constructor TDBEngine.Create;
begin
  inherited Create(True);
  FHistory := TsqlvAddonHistory.Create;
  FStack := TsqlvStack.Create;
  FSQLHistory := TsqlvSQLHistory.Create;
  FSetting := TsqlvSetting.Create;
  FRecents := TStringList.Create;
  FDB := TsqlvDB.Create;
  Engine.RegisterNotify(Self);
  FEngines:=TStringList.Create;
  mncDB.Engines.EnumConnections(FEngines);
end;

destructor TDBEngine.Destroy;
begin
  Engine.UnregisterNotify(Self);
  FreeAndNil(FDB);
  FreeAndNil(FSetting);
  FreeAndNil(FRecents);
  FreeAndNil(FHistory);
  FreeAndNil(FSQLHistory);
  FreeAndNil(FEngines);
  inherited;
end;

procedure TDBEngine.ConnectServer;
begin
  with TConnectDBServerForm.Create(Application) do
  begin
    if ShowModal = mrOK then
    begin
      if DB.IsActive then
        DB.Close;

      Server.Engine := (DatabaseEngineCbo.Items.Objects[DatabaseEngineCbo.ItemIndex] as TmncEngine);
      Server.Info.Host := HostEdit.Text;
      if Server.Info.Host = '' then
      begin
        if (ccPath in Server.Engine.ConnectionClass.Capabilities) then
          Server.Info.Host := Engine.BrowseFolder
        else
          Server.Info.Host := '127.0.0.1';
      end;
      Server.Info.Port := PortEdit.Text;
      Server.Info.UserName := UserEdit.Text;
      Server.Info.Role := RoleEdit.Text;
      Server.Info.Password := PasswordEdit.Text;
      Stack.Clear;
      ServerChanged;
    end;
  end;
end;

procedure TDBEngine.OpenDatabase(FileName: string; FileParam: string);
begin
  if DB.IsActive then
    DB.Close;

  //DBEngine.Setting.CacheMetas := CacheMetaChk.Checked;
  //DBEngine.DB.Open(False, (DatabaseEngineCbo.Items.Objects[DatabaseEngineCbo.ItemIndex] as TmncEngine).Name, DatabaseCbo.Text, UserEdit.Text, PasswordEdit.Text, RoleEdit.Text, ExclusiveChk.Checked, VacuumChk.Checked);
  Stack.Clear;
  //Stack.Push(TsqlvProcess.Create('Databases', 'Database', 'Tables', DatabaseCbo.Text));
  Run;
end;

procedure TDBEngine.OpenDatabase;
begin
  with TOpenDatabaseForm.Create(Application) do
  begin
    if ShowModal = mrOK then
    begin
      if DBEngine.DB.IsActive then
        DBEngine.DB.Close;

      DBEngine.Setting.CacheMetas := CacheMetaChk.Checked;
      DBEngine.DB.Open(False, (DatabaseEngineCbo.Items.Objects[DatabaseEngineCbo.ItemIndex] as TmncEngine).Name, DatabaseCbo.Text, UserEdit.Text, PasswordEdit.Text, RoleEdit.Text, ExclusiveChk.Checked, VacuumChk.Checked);
      DBEngine.Stack.Clear;
      DBEngine.Stack.Push(TsqlvProcess.Create('Databases', 'Database', 'Tables', DatabaseCbo.Text));
      DBEngine.Run;
    end;
  end;
end;

procedure TDBEngine.CreateDatabase;
begin
  with TOpenDatabaseForm.Create(Application) do
  begin
    if ShowModal = mrOK then
    begin
      if DBEngine.DB.IsActive then
        DBEngine.DB.Close;

      DBEngine.Setting.CacheMetas := CacheMetaChk.Checked;
      DBEngine.DB.Open(True, (DatabaseEngineCbo.Items.Objects[DatabaseEngineCbo.ItemIndex] as TmncEngine).Name, DatabaseCbo.Text, UserEdit.Text, PasswordEdit.Text, RoleEdit.Text, ExclusiveChk.Checked, VacuumChk.Checked);
      DBEngine.Stack.Clear;
      DBEngine.Stack.Push(TsqlvProcess.Create('Databases', 'Database', 'Tables', DatabaseCbo.Text));
      DBEngine.Run(DBEngine.Stack);
    end;
  end;
end;

procedure TDBEngine.RegisterFilter(Filter: string);
begin
  if AnsiPos('|', Filter) = 0 then
    raise EsqlvException.Create('Invalid sqlviewer filter');
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

procedure TDBEngine.ShowMeta(vAddon: TsqlvAddon; vSelectDefault: Boolean);
begin
  FNotifyObject.ShowMeta(vAddon, vSelectDefault);
end;

procedure TDBEngine.LoadEditor(vAddon: TsqlvAddon; S: string);
begin
  FNotifyObject.LoadEditor(vAddon, S);
end;

procedure TDBEngine.LoadEditor(vAddon: TsqlvAddon; S: TStringList);
begin
  FNotifyObject.LoadEditor(vAddon, S.Text);
end;

procedure TDBEngine.LoadOptions;
begin
  FSetting.SafeLoadFromFile(Engine.WorkSpace + sqlvConfig);
  if FileExists(Engine.WorkSpace + sqlvRecents) then
    FRecents.LoadFromFile(Engine.WorkSpace + sqlvRecents);
end;

{ TsqlvCustomHistory }

procedure TsqlvCustomHistory.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

function TsqlvCustomHistory.GetCurrent: TsqlvCustomHistoryItem;
begin
  if (Index < Count) and (FIndex >=0) then
    Result := Items[Index]
  else
    Result := nil;
end;

constructor TsqlvCustomHistory.Create;
begin
  inherited Create(True);
  Index := 0;
  MaxCount := 50;
end;

function TsqlvCustomHistory.Add(History: TsqlvCustomHistoryItem): Integer;
begin
  if (Count > MaxCount) and (Count > 0) then
  begin
    Delete(0);
    FIndex := FIndex - 1
  end;
  Result := inherited Add(History);
end;

function TsqlvSQLHistory.GetCurrent: TsqlvSQLHistoryItem;
begin
  Result := inherited GetCurrent as TsqlvSQLHistoryItem;
end;

procedure TsqlvSQLHistory.Add(const Text: string; Silent: Boolean);
var
  i: Integer;
  aHistory: TsqlvSQLHistoryItem;
begin
  if (Count > 0) then
  begin
    aHistory := Items[Count - 1] as TsqlvSQLHistoryItem;
    if (aHistory.Text = Text) then
      exit;//do not duplicate the last one
    if (Index < Count) and (Index >= 0) then
    begin
      aHistory := Items[Index] as TsqlvSQLHistoryItem;
      if (aHistory.Text = Text) then
        exit;//do not duplicate the current one
    end;
  end;
  aHistory := TsqlvSQLHistoryItem.Create;
  aHistory.Text := Text;
  //aHistory.Addon := AAddon;
  i := inherited Add(aHistory);
  if not Silent then
    Index := i;
  Changed;
end;

function TsqlvCustomHistory.HaveForward: Boolean;
begin
  Result := (Count > 0) and (Index < Count - 1);
end;

function TsqlvCustomHistory.HaveBackward: Boolean;
begin
  Result := (Count> 0) and (FIndex > 0);
end;

function TsqlvCustomHistory.Forward: Boolean;
begin
  Result := HaveForward;
  if Result then
  begin
    Index := FIndex + 1;
    Changed;
  end;
end;

function TsqlvCustomHistory.Backward: Boolean;
begin
  Result := HaveBackward;
  if Result then
  begin
    Index := FIndex - 1;
    Changed;
  end;
end;

{ TsqlvSetting }

constructor TsqlvSetting.Create;
begin
  inherited Create;
  FCSVQuoteChar := '"';
  FCSVDelimiterChar := ';';
end;

{ TsqlvDB }

procedure TsqlvDB.Connected;
begin
  if FVacuum then
    Connection.Vacuum;
  Session.Start;
  LoadMeta;
  RunLoginSQL;
end;

constructor TsqlvDB.Create;
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

destructor TsqlvDB.Destroy;
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

function TsqlvDB.CreateMeta: TmncMeta;
begin
  Result := Engines.CreateMeta(Connection);
  Result.Link := Session;
end;

procedure TsqlvDB.Disconnected;
begin
  RunLogoutSQL;
end;

procedure TsqlvDB.LoadMeta;
var
  AMeta: TmncMeta;
begin
  if DBEngine.Setting.CacheMetas then
  begin
    AMeta := Engines.CreateMeta(FConnection);
    try
      AMeta.Link := Session;
      AMeta.EnumObjects(Tables, sokTable, '', [ekSystem, ekSort]);
      AMeta.EnumObjects(Views, sokView, '', [ekSort]);
      AMeta.EnumObjects(Proceduers, sokProcedure, '', [ekSort]);
      AMeta.EnumObjects(Sequences, sokSequence, '', [ekSort]);
      AMeta.EnumObjects(Functions, sokFunction, '', [ekSort]);
      AMeta.EnumObjects(Exceptions, sokException, '', [ekSort]);
      AMeta.EnumObjects(Domains, sokDomain, '', [ekSort]);
      AMeta.EnumObjects(Fields, sokField);
    finally
      AMeta.Free;
    end;
  end;
end;

procedure TsqlvDB.Open(vCreate: Boolean; DatabaseEngine, DatabaseName, UserName, Password, Role: string; vExclusive: Boolean; vVacuum: Boolean);
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

  DBEngine.AddRecent(Engines.ComposeConnectionString(Connection));

  FSession := FConnection.CreateSession;

  Connection.Connect;
  Connected;
  //Engine.SendLog()
  Engine.UpdateState([ecsChanged, ecsState, ecsRefresh, ecsRecents, ecsProject, ecsProjectLoaded]);
end;

procedure TsqlvDB.Close;
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

function TsqlvDB.IsActive: Boolean;
begin
  Result := (Connection <> nil) and Connection.Active;
end;

procedure TsqlvDB.RunLoginSQL;
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

procedure TsqlvDB.RunLogoutSQL;
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
