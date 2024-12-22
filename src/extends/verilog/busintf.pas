unit busintf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, verilogparser;

type
  TBusSide = (bsBidir, bsMaster, bsSlave);

  TGenericOption = (goRemovePortsIf0);
  TGenericOptions = set of TGenericOption;

  TPortOption = (poOptional);
  TPortOptions = set of TPortOption;

  TGeneric = record
    Name,Value: string;
    Options: TGenericOptions;
  end;

  TPort = record
    Name, Width: string;
    Dir: TPortDirection;
    Options: TPortOptions;
  end;

  TBusInterface = class
  protected
    BusSide: TBusSide;
    procedure AddGeneric(const AName, AValue: string; AOption: TGenericOptions = []);
    procedure AddSub(const AName, AValue: string);
    procedure AddPort(const AName, AWidth: string; ADir: TPortDirection; AOptions: TPortOptions = []);

    function Clone: TBusInterface;
  public
    Generics: array of TGeneric;
    Substitutions: array of TVerilogGeneric;
    Ports: array of TPort;

    function GetInstantiation(ASide: TBusSide; ARegistered: boolean): string;
  end;

function GetIntf(const AIntf: string): TBusInterface;

implementation                                

var
  BusInterfaces: TStringList;

procedure AddAXI;

  procedure AddAXI4Full;
  var
    bi: TBusInterface;
  begin
    bi:=TBusInterface.Create;

    bi.AddGeneric('ID_WIDTH','4', [goRemovePortsIf0]);
    bi.AddGeneric('ADDR_WIDTH','32');
    bi.AddGeneric('DATA_WIDTH','32');
    bi.AddGeneric('USER_WIDTH','0', [goRemovePortsIf0]);

    bi.AddSub('Prefix', 'AXI_');

    bi.AddPort('$Prefix$AWID',    '[(%ID_WIDTH%)-1:0]',    pdOut);
    bi.AddPort('$Prefix$AWADDR',  '[(%ADDR_WIDTH%)-1:0]',  pdOut);
    bi.AddPort('$Prefix$AWLEN',   '[7:0]',                 pdOut);
    bi.AddPort('$Prefix$AWSIZE',  '[2:0]',                 pdOut);
    bi.AddPort('$Prefix$AWBURST', '[1:0]',                 pdOut);
    bi.AddPort('$Prefix$AWLOCK',  '[1:0]',                 pdOut);
    bi.AddPort('$Prefix$AWCACHE', '[3:0]',                 pdOut);
    bi.AddPort('$Prefix$AWPROT',  '[2:0]',                 pdOut);
    bi.AddPort('$Prefix$AWQOS',   '[3:0]',                 pdOut);
    bi.AddPort('$Prefix$AWREGION','[3:0]',                 pdOut);
    bi.AddPort('$Prefix$AWUSER',  '[(%USER_WIDTH%)-1:0]',  pdOut, [poOptional]);
    bi.AddPort('$Prefix$AWVALID', '',                      pdOut);
    bi.AddPort('$Prefix$AWREADY', '',                      pdIn);

    bi.AddPort('$Prefix$WID',    '[(%ID_WIDTH%)-1:0]',     pdOut);
    bi.AddPort('$Prefix$WDATA',  '[(%DATA_WIDTH%)-1:0]',   pdOut);
    bi.AddPort('$Prefix$WSTRB',  '[(%DATA_WIDTH%)/8-1:0]', pdOut);
    bi.AddPort('$Prefix$WLAST',  '',                       pdOut);
    bi.AddPort('$Prefix$WUSER',  '[(%USER_WIDTH%)-1:0]',   pdOut, [poOptional]);
    bi.AddPort('$Prefix$WVALID', '',                       pdOut);
    bi.AddPort('$Prefix$WREADY', '',                       pdIn);

    bi.AddPort('$Prefix$BID',    '[(%ID_WIDTH%)-1:0]',     pdIn);
    bi.AddPort('$Prefix$BRESP',  '[1:0]',                  pdIn);
    bi.AddPort('$Prefix$BUSER',  '[(%USER_WIDTH%)-1:0]',   pdIn, [poOptional]);
    bi.AddPort('$Prefix$BVALID', '',                       pdIn);
    bi.AddPort('$Prefix$BREADY', '',                       pdOut);

    bi.AddPort('$Prefix$ARID',    '[(%ID_WIDTH%)-1:0]',    pdOut);
    bi.AddPort('$Prefix$ARADDR',  '[(%ADDR_WIDTH%)-1:0]',  pdOut);
    bi.AddPort('$Prefix$ARLEN',   '[7:0]',                 pdOut);
    bi.AddPort('$Prefix$ARSIZE',  '[2:0]',                 pdOut);
    bi.AddPort('$Prefix$ARBURST', '[1:0]',                 pdOut);
    bi.AddPort('$Prefix$ARLOCK',  '[1:0]',                 pdOut);
    bi.AddPort('$Prefix$ARCACHE', '[3:0]',                 pdOut);
    bi.AddPort('$Prefix$ARPROT',  '[2:0]',                 pdOut);
    bi.AddPort('$Prefix$ARQOS',   '[3:0]',                 pdOut);
    bi.AddPort('$Prefix$ARREGION','[3:0]',                 pdOut);
    bi.AddPort('$Prefix$ARUSER',  '[(%USER_WIDTH%)-1:0]',  pdOut, [poOptional]);
    bi.AddPort('$Prefix$ARVALID', '',                      pdOut);
    bi.AddPort('$Prefix$ARREADY', '',                      pdIn);

    bi.AddPort('$Prefix$RID',    '[(%ID_WIDTH%)-1:0]',     pdIn, [poOptional]);
    bi.AddPort('$Prefix$RDATA',  '[(%DATA_WIDTH%)-1:0]',   pdIn);
    bi.AddPort('$Prefix$RRESP',  '[1:0]',                  pdIn);
    bi.AddPort('$Prefix$RLAST',  '',                       pdIn);
    bi.AddPort('$Prefix$RUSER',  '[(%USER_WIDTH%)-1:0]',   pdIn, [poOptional]);
    bi.AddPort('$Prefix$RVALID', '',                       pdIn);
    bi.AddPort('$Prefix$RREADY', '',                       pdOut);

    bi.BusSide:=bsMaster;

    BusInterfaces.AddObject('AXI4Full', bi);
  end;

  procedure AddAXI4Lite;
  var
    bi: TBusInterface;
  begin
    bi:=TBusInterface.Create;

    bi.AddGeneric('ADDR_WIDTH','32');
    bi.AddGeneric('DATA_WIDTH','32');

    bi.AddSub('Prefix', 'AXI_');

    bi.AddPort('$Prefix$AWADDR',  '[(%ADDR_WIDTH%)-1:0]',  pdOut);
    bi.AddPort('$Prefix$AWPROT',  '[2:0]',                 pdOut, [poOptional]);
    bi.AddPort('$Prefix$AWVALID', '',                      pdOut);
    bi.AddPort('$Prefix$AWREADY', '',                      pdIn);

    bi.AddPort('$Prefix$WDATA',  '[(%DATA_WIDTH%)-1:0]',   pdOut);
    bi.AddPort('$Prefix$WSTRB',  '[(%DATA_WIDTH%)/8-1:0]', pdOut);
    bi.AddPort('$Prefix$WVALID', '',                       pdOut);
    bi.AddPort('$Prefix$WREADY', '',                       pdIn);

    bi.AddPort('$Prefix$BRESP',  '[1:0]',                  pdIn);
    bi.AddPort('$Prefix$BVALID', '',                       pdIn);
    bi.AddPort('$Prefix$BREADY', '',                       pdOut);

    bi.AddPort('$Prefix$ARADDR',  '[(%ADDR_WIDTH%)-1:0]',  pdOut);
    bi.AddPort('$Prefix$ARPROT',  '[2:0]',                 pdOut, [poOptional]);
    bi.AddPort('$Prefix$ARVALID', '',                      pdOut);
    bi.AddPort('$Prefix$ARREADY', '',                      pdIn);

    bi.AddPort('$Prefix$RDATA',  '[(%DATA_WIDTH%)-1:0]',   pdIn);
    bi.AddPort('$Prefix$RRESP',  '[1:0]',                  pdIn);
    bi.AddPort('$Prefix$RVALID', '',                       pdIn);
    bi.AddPort('$Prefix$RREADY', '',                       pdOut);

    bi.BusSide:=bsMaster;

    BusInterfaces.AddObject('AXI4Lite', bi);
  end;

  procedure AddAXI4Stream;
  var
    bi: TBusInterface;
  begin
    bi:=TBusInterface.Create;

    bi.AddGeneric('ID_WIDTH','4', [goRemovePortsIf0]);
    bi.AddGeneric('DEST_WIDTH','32');
    bi.AddGeneric('DATA_WIDTH','32'); 
    bi.AddGeneric('USER_WIDTH','0', [goRemovePortsIf0]);

    bi.AddSub('Prefix', 'AXI_');
                                                                      
    bi.AddPort('$Prefix$TVALID', '',                       pdOut);
    bi.AddPort('$Prefix$TREADY', '',                       pdIn);
    bi.AddPort('$Prefix$TDATA',  '[(%DATA_WIDTH%)-1:0]',   pdOut);
    bi.AddPort('$Prefix$TSTRB',  '[(%DATA_WIDTH%)/8-1:0]', pdOut, [poOptional]);
    bi.AddPort('$Prefix$TKEEP',  '[(%DATA_WIDTH%)/8-1:0]', pdOut, [poOptional]);
    bi.AddPort('$Prefix$TLAST',  '',                       pdOut, [poOptional]);
    bi.AddPort('$Prefix$TID',    '[(%ID_WIDTH%)-1:0]',     pdOut, [poOptional]);
    bi.AddPort('$Prefix$TDEST',  '[(%DEST_WIDTH%)-1:0]',   pdOut, [poOptional]); 
    bi.AddPort('$Prefix$TUSER',  '[(%USER_WIDTH%)-1:0]',   pdOut, [poOptional]);

    bi.BusSide:=bsMaster;

    BusInterfaces.AddObject('AXI4Stream', bi);
  end;

begin
  AddAXI4Full;
  AddAXI4Lite;
  AddAXI4Stream;
end;

procedure AddInbuilt;
begin
  AddAXI;
end;

function GetIntf(const AIntf: string): TBusInterface;
var
  i: Integer;
begin
  i:=BusInterfaces.IndexOf(AIntf);
  if i<0 then
    result:=nil
  else
    result:=TBusInterface(BusInterfaces.Objects[i]).Clone;
end;

procedure TBusInterface.AddGeneric(const AName, AValue: string; AOption: TGenericOptions);
begin
  setlength(Generics, high(Generics)+2);
  Generics[high(Generics)].Name:=AName;
  Generics[high(Generics)].Value:=AValue;
  Generics[high(Generics)].Options:=AOption;
end;

procedure TBusInterface.AddSub(const AName, AValue: string);
begin
  setlength(Substitutions, high(Substitutions)+2);
  Substitutions[high(Substitutions)].Name:=AName;
  Substitutions[high(Substitutions)].Value:=AValue;
end;

procedure TBusInterface.AddPort(const AName, AWidth: string; ADir: TPortDirection; AOptions: TPortOptions);
begin
  setlength(Ports, high(Ports)+2);
  Ports[high(Ports)].Name:=AName;
  Ports[high(Ports)].Width:=AWidth;
  Ports[high(Ports)].Dir:=ADir;
  Ports[high(ports)].Options:=AOptions;
end;

function TBusInterface.Clone: TBusInterface;
begin
  result:=TBusInterface.Create;
  result.Generics:=Copy(Generics);
  Result.Substitutions:=Copy(Substitutions);
  result.Ports:=Copy(ports);
  result.BusSide:=BusSide;
end;

const
  ioStr: array[boolean, boolean, TPortDirection] of string = (
    (('inout', 'input', 'output', 'inout'),
     ('inout', 'output', 'input', 'inout')),
    (('inout', 'input', 'output reg', 'inout'),
     ('inout', 'output reg', 'input', 'inout')));

function TBusInterface.GetInstantiation(ASide: TBusSide; ARegistered: boolean): string;
var
  port: TPort;
  st, st2: String;
  sub: TVerilogGeneric;
  s2: TGeneric;
begin
  result:='';

  for port in Ports do
  begin
    st:=port.name;

    for sub in Substitutions do st:=StringReplace(st, '$'+sub.Name+'$', sub.Value, [rfReplaceAll]);

    if port.Width<>'' then
    begin
      st2:=port.Width;

      for s2 in Generics do st2:=StringReplace(st2, '%'+s2.Name+'%', s2.Value, [rfReplaceAll]);

      result:=result + format('%s %s %s,'+LineEnding, [ioStr[ASide=BusSide, ARegistered, port.Dir], st2, st]);
    end
    else
      result:=result + format('%s %s,'+LineEnding, [ioStr[ASide=BusSide, ARegistered,port.Dir], st]);
  end;
end;

initialization
  BusInterfaces:=TStringList.Create;
  BusInterfaces.OwnsObjects:=true;
  BusInterfaces.CaseSensitive:=true;
  BusInterfaces.Duplicates:=dupError;
  BusInterfaces.Sorted:=true;

  AddInbuilt;
finalization
  BusInterfaces.Free;

end.

