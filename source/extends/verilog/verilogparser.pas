unit verilogparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils;

type
  TPortDirection = (pdUnknown, pdIn, pdOut, pdInOut);

  TVerilogPort = record
    Name, Width: string;
    Direction: TPortDirection;
  end;

  TVerilogGeneric = record
    Name, Value: string;
  end;

  TVerilogModule = record
    Name: string;
    Generics: array of TVerilogGeneric;
    Ports: array of TVerilogPort;
  end;

  TVerilogParser = class
  private
    fModules: array of TVerilogModule;
    procedure DoTrim(const AStr: string; var AOffset: sizeint);
    function GetModule(AIndex: longint): TVerilogModule;
    function GetModuleCount: longint;

    function GetIdentifier(const AStr: string; var AOffset: sizeint; out AIdent: string): boolean;
  public
    procedure Parse(const ASource: string);

    property Module[AIndex: longint]: TVerilogModule read GetModule; default;
    property ModuleCount: longint read GetModuleCount;
  end;
                                       
function GenerateWires(const AMod: TVerilogModule; AIndent: SizeInt): string;
function GenerateInstance(const AMod: TVerilogModule; AIndent: SizeInt): string;

implementation               

function GenerateWires(const AMod: TVerilogModule; AIndent: SizeInt): string;
var
  i: sizeint;
begin
  result:='';

  if high(AMod.Ports)>=0 then
  begin
    for i:=0 to high(AMod.Ports) do
      if AMod.Ports[i].Width<>'' then
        result:=result+Format('%swire %s %s;'+LineEnding, [StringOfChar(' ',AIndent), AMod.Ports[i].Width, AMod.Ports[i].Name])
      else
        result:=result+Format('%swire %s;'+LineEnding, [StringOfChar(' ',AIndent), AMod.Ports[i].Name]);
  end;
end;

function GenerateInstance(const AMod: TVerilogModule; AIndent: SizeInt): string;
var
  i: sizeint;
begin
  result:=StringOfChar(' ',AIndent)+AMod.Name+' ';
  if high(AMod.Generics)>=0 then
  begin
    result:=result+Format('#(.%s(%s)', [AMod.Generics[0].Name, AMod.Generics[0].Value]);
    for i:=1 to high(AMod.Generics) do
      result:=result+Format(',.%s(%s)', [AMod.Generics[i].Name, AMod.Generics[i].Value]);
    result:=result+') ';
  end;

  result:=result+AMod.Name+'('+LineEnding;

  if high(AMod.Ports)>=0 then
  begin
    result:=result+StringOfChar(' ',AIndent)+Format('.%s(%s)', [AMod.Ports[0].Name, AMod.Ports[0].Name]);
    for i:=1 to high(AMod.Ports) do
      result:=result+Format(','+lineending+StringOfChar(' ',AIndent+2)+'.%s(%s)', [AMod.Ports[i].Name, AMod.Ports[i].Name]);
  end;

  result:=result+lineending+StringOfChar(' ',AIndent)+');';
end;

function TVerilogParser.GetModule(AIndex: longint): TVerilogModule;
begin
  result:=fModules[AIndex];
end;

function TVerilogParser.GetModuleCount: longint;
begin
  result:=Length(fModules);
end;

procedure TVerilogParser.DoTrim(const AStr: string; var AOffset: sizeint);
var
  t: SizeInt;
begin
  repeat
    t:=AOffset;

    while AStr[AOffset] in [#0..#32] do inc(AOffset);
    if PosEx('//',AStr,AOffset)=AOffset then AOffset:=PosSetEx(#13#10,AStr,AOffset);
  until t=AOffset;
end;

function TVerilogParser.GetIdentifier(const AStr: string; var AOffset: sizeint; out AIdent: string): boolean;
var
  t: SizeInt;
begin
  DoTrim(astr,AOffset);

  if AStr[AOffset] in ['a'..'z','A'..'Z','_','\'] then
  begin
    if AStr[AOffset]='\' then
      t:=PosSetEx([#0..#32],AStr,AOffset)
    else
      t:=PosSetEx([#0..#255]-['a'..'z','A'..'Z','_','\','0'..'9','$'],AStr,AOffset);

    AIdent:=AStr[AOffset..t-1];
    result:=true;
  end
  else
  begin
    AIdent:='';
    result:=false;
  end;
end;

procedure TVerilogParser.Parse(const ASource: string);
var
  i, t, t2, t3, t4: SizeInt;
  name, id, value, width: string;
  dir: TPortDirection;

  procedure ScanParameters(a,b: sizeint);
  var
    tmp: SizeInt;
  begin
    repeat
      tmp:=PosEx('parameter',ASource,a);
      if tmp<1 then break;

      dec(tmp);
      if (tmp<b) and
         GetIdentifier(ASource,tmp,id) and
         (id='parameter') then
      begin
        inc(tmp,length(id));

        dotrim(ASource,tmp);

        if ASource[tmp]='[' then
        begin
          tmp:=PosEx(']',ASource,tmp);
          if tmp<1 then break;
          inc(tmp);
        end;

        repeat
          if GetIdentifier(ASource,tmp,id) then
          begin
            name:=id;
            inc(tmp,length(id));

            if GetIdentifier(ASource, tmp, id) or
               (not (ASource[tmp] in ['='])) then
              break;

            inc(tmp);
            DoTrim(ASource, tmp);

            t4:=tmp;
            if ASource[tmp]='"' then
            begin
              tmp:=PosEx('"', ASource, tmp+1);
              value:=ASource[t4..tmp];

              inc(tmp);
              DoTrim(ASource, tmp);
            end
            else
            begin
              tmp:=PosSetEx([';',','], ASource, tmp)-1;
              value:=trim(ASource[t4..tmp]);

              inc(tmp);
              DoTrim(ASource, tmp);
            end;

            setlength(fModules[high(fModules)].Generics, high(fModules[high(fModules)].Generics)+2);
            fModules[high(fModules)].Generics[high(fModules[high(fModules)].Generics)].Name:=name;
            fModules[high(fModules)].Generics[high(fModules[high(fModules)].Generics)].Value:=value;
          end
          else
            break;

          if ASource[tmp]=',' then
            inc(tmp)
          else
            break;
        until false;

        a:=tmp+1;
      end
      else
        a:=tmp+1;
    until a>b;
  end;

  procedure ScanIO(const typ: string; dir: TPortDirection; a,b: sizeint);
  var
    i,tmp: SizeInt;
  begin
    repeat
      tmp:=PosEx(typ,ASource,a);
      if tmp<1 then break;

      dec(tmp);
      if (tmp<b) and
         GetIdentifier(ASource,tmp,id) and
         (id=typ) then
      begin
        inc(tmp,length(id));

        dotrim(ASource,tmp);    
        width:='';

        repeat
          GetIdentifier(ASource, tmp, id);

          if (ASource[tmp]<>'[') and (id='reg') then
          begin
            inc(tmp,length(id));
            GetIdentifier(ASource, tmp, id);
          end;

          if (ASource[tmp]<>'[') and (id='reg') and
             ((id='supply0') or (id='supply1') or
              (id='tri') or (id='triand') or (id='trior') or
              (id='tri0') or (id='tri1') or
              (id='uwire') or (id='wire') or
              (id='wand') or (id='word')) then
          begin
            inc(tmp,length(id));
            GetIdentifier(ASource, tmp, id);
          end;

          if (ASource[tmp]<>'[') and (id='signed') then
          begin
            inc(tmp,length(id));
            GetIdentifier(ASource, tmp, id);
          end;

          if ASource[tmp]='[' then
          begin
            t4:=PosEx(']',ASource,tmp+1);
            width:=ASource[tmp..t4];

            tmp:=t4+1;
            GetIdentifier(ASource, tmp, id);
          end;

          for i:=0 to high(fModules[high(fModules)].Ports) do
          begin
            if fModules[high(fModules)].Ports[i].name=id then
            begin              
              fModules[high(fModules)].Ports[i].Width:=width;
              fModules[high(fModules)].Ports[i].Direction:=dir;
              break;
            end;
          end;

          inc(tmp,length(id));
          dotrim(ASource,tmp);

          if ASource[tmp]=',' then
            inc(tmp)
          else
            break;
        until false;

        a:=tmp+1;
      end
      else
        a:=tmp+2;
    until a>b;
  end;

begin
  i:=1;
  repeat
    t:=PosEx('module',ASource,i);

    if t>0 then
    begin         
      t2:=PosEx('endmodule',ASource,t);

      inc(t,6);

      t:=PosSetEx(['a'..'z','A'..'Z','_','\'],ASource,t);
      if (t>t2) or (t<0) then begin i:=t2+6; continue; end;

      if ASource[t]='\' then
        t3:=PosSetEx([#0..#32],ASource,t)
      else
        t3:=PosSetEx([#0..#32,'(','#'],ASource,t);
      if (t3>t2) or (t3<0) then begin i:=t2+6; continue; end;

      name:=ASource[t..t3-1];

      setlength(fModules, high(fModules)+2);
      fModules[high(fModules)].Name:=name;

      // Scan parameters
      t3:=PosSetEx(['(','#',';'],ASource,t);
      if asource[t3]='#' then
      begin
        t3:=PosSetEx(['('],ASource,t3)+1;

        repeat
          if not GetIdentifier(ASource, t3, id) then break;

          if id='parameter' then
          begin
            inc(t3,length(id));
            if not GetIdentifier(ASource, t3, id) then break;
          end;
          if id='signed' then
          begin
            inc(t3,length(id));
            if not GetIdentifier(ASource, t3, id) then break;
          end;

          name:=id;
          inc(t3,length(id));

          if GetIdentifier(ASource, t3, id) or
             (not (ASource[t3] in ['='])) then
          begin i:=t2+6; break; end;

          inc(t3);
          DoTrim(ASource, t3);

          t4:=t3;
          if ASource[t3]='"' then
          begin
            t3:=PosEx('"', ASource, t3+1);
            value:=ASource[t4..t3];

            inc(t3);
            DoTrim(ASource, t3);
          end
          else
          begin
            t3:=PosSetEx([',',')'], ASource, t3)-1;
            value:=trim(ASource[t4..t3]);

            inc(t3);
            DoTrim(ASource, t3);
          end;

          setlength(fModules[high(fModules)].Generics, high(fModules[high(fModules)].Generics)+2);
          fModules[high(fModules)].Generics[high(fModules[high(fModules)].Generics)].Name:=name;
          fModules[high(fModules)].Generics[high(fModules[high(fModules)].Generics)].Value:=value;

          if (ASource[t3]<>',') then break;
          inc(t3);
        until false;

        if ASource[t3]<>')' then begin i:=t2+6; continue; end else inc(t3);

        // Align to port definitions '('
        if GetIdentifier(ASource, t3, id) or
           (not (ASource[t3] in ['(',';'])) then
        begin i:=t2+6; continue; end;
      end;

      // Parse ports
      if ASource[t3]='(' then
      begin
        inc(t3);     

        dir:=pdUnknown;    
        width:='';
        repeat
          if not GetIdentifier(ASource, t3, id) then break;

          if id='input' then begin dir:=pdIn; width:=''; end
          else if id='output' then begin dir:=pdOut; width:=''; end
          else if id='inout' then begin dir:=pdInOut; width:=''; end;

          if dir<>pdUnknown then
          begin
            inc(t3,length(id));
            GetIdentifier(ASource, t3, id);
          end;

          if (ASource[t3]<>'[') and (id='reg') then
          begin
            inc(t3,length(id));
            GetIdentifier(ASource, t3, id);
          end;
                            
          if (ASource[t3]<>'[') and (id='reg') and
             ((id='supply0') or (id='supply1') or
              (id='tri') or (id='triand') or (id='trior') or
              (id='tri0') or (id='tri1') or
              (id='uwire') or (id='wire') or
              (id='wand') or (id='word')) then
          begin
            inc(t3,length(id));
            GetIdentifier(ASource, t3, id);
          end;
                                          
          if (ASource[t3]<>'[') and (id='signed') then
          begin
            inc(t3,length(id));
            GetIdentifier(ASource, t3, id);
          end;

          if ASource[t3]='[' then
          begin
            t4:=PosEx(']',ASource,t3+1);
            width:=ASource[t3..t4];

            t3:=t4+1;           
            GetIdentifier(ASource, t3, id);
          end;

          setlength(fModules[high(fModules)].Ports, high(fModules[high(fModules)].Ports)+2);
          fModules[high(fModules)].Ports[high(fModules[high(fModules)].Ports)].Name:=id;
          fModules[high(fModules)].Ports[high(fModules[high(fModules)].Ports)].Width:=width;
          fModules[high(fModules)].Ports[high(fModules[high(fModules)].Ports)].Direction:=dir;

          inc(t3,length(id));
          dotrim(ASource,t3);

          if ASource[t3]=',' then
            inc(t3)
          else
            break;
        until false;

        if ASource[t3]<>')' then begin i:=t2+6; continue; end else inc(t3);
      end
      else
      begin i:=t2+6; continue; end;

      ScanParameters(t3, t2);

      ScanIO('input',pdIn,t3,t2);
      ScanIO('output',pdOut,t3,t2);
      ScanIO('inout',pdInOut,t3,t2);

      i:=t2+6;
    end
    else
      i:=-1;
  until i<0;
end;

end.

