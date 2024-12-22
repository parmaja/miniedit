{
  Lua4Lazarus

    TLuaObject

    License: New BSD
      Copyright(c)2010- Malcome@Japan All rights reserved.

    Version History:
      1.53.150205 by Malcome Japan.
        - Test Lazarus 1.4RC1 (FPC 2.6.4), Lazarus 1.5 (FPC 3.0.1)
        - for Lua 5.3

      1.52.0 by Malcome Japan.
        - Test Lazarus 1.3 (FPC 2.6.2)
        - for Lua 5.2

      1.0.0 by Malcome@Japan.
        - Test Lazarus 0.9.29 (FPC 2.4.1)
        - for Lua 5.1

    License for Lua 5.0 and later versions:
      Copyright(c)1994-2015 Lua.org, PUC-Rio.

}
unit l4l_object;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

type

  { TLuaObject }

  TLuaObject = class(TPersistent)
  private
    FLS: Plua_State;
  protected
    property LS: Plua_State read FLS;
    function Iterator({%H-}index: integer): integer; virtual;
  public
    constructor Create(L : Plua_State); virtual;
  published
  end;

procedure l4l_SetLuaObject(obj: TLuaObject);
procedure l4l_PushLuaObject(obj: TLuaObject);
function l4l_isobject(L : Plua_State;  n: Integer): boolean;
function l4l_isobject(L : Plua_State;  n: Integer; c: TClass): boolean;
function l4l_toobject(L : Plua_State;  n: Integer): TLuaObject;

implementation
uses
  typinfo;

const
  FIELD_OBJ = '___l4lObject___';
  FIELD_FN = '___l4lFuncName___';
  FIELD_IC = '___l4lIteCount___';

  PROP_HEAD = 'l4l_';

function gc(L : Plua_State) : Integer; cdecl;
var
  p: PPointer;
begin
  p:= lua_touserdata(L, 1);
  try TLuaObject(p^).Free; except end;
  Result:= 0;
end;

function call(L : Plua_State) : Integer; cdecl;
var
  p: PPointer;
  obj: TLuaObject;
  method: function:integer of object;
begin
  lua_getfield(L, 1, FIELD_OBJ);
  p:= lua_touserdata(L, -1);
  lua_remove(L, -1);
  obj:= TLuaObject(p^);
  lua_getfield(L, 1, FIELD_FN);
  lua_remove(L, 1);
  p:= lua_touserdata(L, -1);
  lua_remove(L, -1);
  TMethod(method).Data := obj;
  TMethod(method).Code := p^;
  try
    Result := method();
  except
    on E: Exception do luaL_error(L, PChar(E.Message));
  end;
end;

function Index(L : Plua_State) : Integer; cdecl;
var
  p: PPointer;
  key: string;
  obj: TLuaObject;
  pi: PPropInfo;
  o: TObject;
begin
  Result := 0;
  lua_getfield(L, 1, FIELD_OBJ);
  p:= lua_touserdata(L, -1);
  obj:= TLuaObject(p^);
  key := lua_tostring(L, 2);
  if Assigned(obj.MethodAddress(PROP_HEAD + key)) then begin
    lua_getfield(L, 1, PChar(LowerCase(key)));
  end else begin
    try
      pi := FindPropInfo(obj, PROP_HEAD + LowerCase(key));
    except
      luaL_error(L, 'Unknown property: "%s".', PChar(key));
    end;
    try
      case pi^.PropType^.Kind of
        tkInteger, tkQWord:
          lua_pushinteger(L, GetOrdProp(obj, pi));
        tkInt64: lua_pushinteger(L, GetInt64Prop(obj, pi));
        tkFloat: lua_pushnumber(L, GetFloatProp(obj, pi));
        tkBool: begin
          if GetOrdProp(obj, pi) = 0 then
            lua_pushboolean(L, False)
          else
            lua_pushboolean(L, True);
        end;
        tkClass: begin
          o:= GetObjectProp(obj, pi);
          if o is TLuaObject then begin
            l4l_PushLuaObject(o as TLuaObject);
          end else begin
            lua_pushnil(L);
          end;
        end;
        else begin
          lua_pushstring(L, PChar(GetStrProp(obj, pi)));
        end;
      end;
    except
      on E: Exception do luaL_error(L, PChar(E.Message));
    end;
  end;
  Result := 1;
end;

function NewIndex(L : Plua_State) : Integer; cdecl;
var
  p: PPointer;
  key: string;
  obj: TLuaObject;
  pi: PPropInfo;
begin
  Result:=0;
  lua_getfield(L, 1, FIELD_OBJ);
  p:= lua_touserdata(L, -1);
  obj:= TLuaObject(p^);
  key := lua_tostring(L, 2);
  try
    pi := FindPropInfo(obj, PROP_HEAD + LowerCase(key));
  except
    luaL_error(L, 'Unknown property: "%s".', PChar(key));
  end;
  try
    case pi^.PropType^.Kind of
      tkInteger, tkInt64, tkQWord:
        SetOrdProp(obj, pi, lua_tointeger(L, 3));
      tkFloat: SetFloatProp(obj, pi, lua_tonumber(L, 3));
      tkBool: SetOrdProp(obj, pi, Ord(lua_toboolean(L, 3)));
      tkClass: begin
        lua_getfield(L, 3, FIELD_OBJ);
        p:= lua_touserdata(L, -1);
        if Assigned(p) and Assigned(p^) then begin
          SetObjectProp(obj, pi, TObject(p^));
        end else
          SetObjectProp(obj, pi, TObject(nil));
      end;
      else begin
        SetStrProp(obj, pi, lua_tostring(L, 3));
      end;
    end;
  except
    on E: Exception do luaL_error(L, PChar(E.Message));
  end;
end;

function iterator(L : Plua_State) : Integer; cdecl;
var
  i: integer;
  p: PPointer;
  obj: TLuaObject;
begin
  Result:= 0;
  lua_getfield(L, 1, FIELD_OBJ);
  p:= lua_touserdata(L, -1);
  obj:= TLuaObject(p^);
  if lua_isnil(L, 3) then begin
    i := 0;
  end else begin
    lua_getfield(L, 1, FIELD_IC);
    i:= lua_tointeger(L, -1) + 1;
  end;
  lua_pushstring(L, FIELD_IC);
  lua_pushinteger(L, i);
  lua_rawset(L, 1);
  try
    Result := obj.Iterator(i);
  except
    on E: Exception do luaL_error(L, PChar(E.Message));
  end;
end;

procedure l4l_SetLuaObject(obj: TLuaObject);
type
  TMethodRec = packed record
    name : pshortstring;
    addr : pointer;
  end;
  TMethodTable = packed record
   count : dword;
   entries : packed array[0..0] of TMethodRec;
  end;
  PMethodTable =  ^TMethodTable;
var
  p: PPointer;
  i, t: integer;
  mt: PMethodTable;
  s:string;
  cl: TClass;
begin
  t:= lua_gettop(obj.LS);
  lua_pushstring(obj.LS, FIELD_OBJ);
  p:= lua_newuserdata(obj.LS, SizeOf(Pointer));
  p^:=obj;
  if lua_getmetatable(obj.LS, -1) = 0 then lua_newtable(obj.LS);
  lua_pushstring(obj.LS, '__gc');
  lua_pushcfunction(obj.LS, @gc);
  lua_settable(obj.LS, -3);
  lua_setmetatable(obj.LS, -2);
  lua_settable(obj.LS, -3);

  cl:= obj.ClassType;
  while Assigned(cl) do begin
    p := Pointer(PAnsiChar(cl) + vmtMethodtable);
    mt := p^;
    if Assigned(mt) then begin
      for i:=0 to mt^.count-1 do begin
        s:= LowerCase(mt^.entries[i].name^);
        if Copy(s, 1, 4) <> PROP_HEAD then continue;
        Delete(s, 1, 4);
        lua_pushstring(obj.LS, PChar(s));
        lua_newtable(obj.LS);
        lua_pushstring(obj.LS, FIELD_FN);
        p:= lua_newuserdata(obj.LS, SizeOf(Pointer));
        p^:= mt^.entries[i].addr;
        lua_settable(obj.LS, -3);
        lua_newtable(obj.LS);
        lua_pushstring(obj.LS, '__call');
        lua_pushcfunction(obj.LS, @call);
        lua_settable(obj.LS, -3);
        lua_pushstring(obj.LS, '__index');
        lua_pushvalue(obj.LS, t); // SuperClass
        lua_settable(obj.LS, -3);
        lua_setmetatable(obj.LS, -2);
        lua_settable(obj.LS, -3);
      end;
    end;
    cl := cl.ClassParent;
  end;

  lua_newtable(obj.LS);
  lua_pushstring(obj.LS, '__newindex');
  lua_pushcfunction(obj.LS, @NewIndex);
  lua_settable(obj.LS, -3);
  lua_pushstring(obj.LS, '__index');
  lua_pushcfunction(obj.LS, @Index);
  lua_settable(obj.LS, -3);
  lua_pushstring(obj.LS, '__call');
  lua_pushcfunction(obj.LS, @iterator);
  lua_settable(obj.LS, -3);
  lua_setmetatable(obj.LS, -2);
end;

procedure l4l_PushLuaObject(obj: TLuaObject);
begin
  lua_newtable(obj.LS);
  l4l_SetLuaObject(obj);
end;

function l4l_isobject(L : Plua_State;  n: Integer): boolean;
begin
  Result:= l4l_isobject(L, n, TLuaObject);
end;

function l4l_isobject(L : Plua_State;  n: Integer; c: TClass): boolean;
var
  p: PPointer;
begin
  Result:= False;
  if lua_istable(L, n) then begin
    lua_getfield(L, n, FIELD_OBJ);
    if not lua_isnil(L, -1) then begin
      p:= lua_touserdata(L, -1);
      Result := TObject(p^) is c;
    end;
    lua_remove(L, -1);
  end;
end;

function l4l_toobject(L: Plua_State; n: Integer): TLuaObject;
var
  p: PPointer;
begin
  Result:= nil;
  if lua_istable(L, n) then begin
    lua_getfield(L, n, FIELD_OBJ);
    if not lua_isnil(L, -1) then begin
      p:= lua_touserdata(L, -1);
      Result := TLuaObject(p^);
    end;
    lua_remove(L, -1);
  end;
end;

{ TLuaObject }

function TLuaObject.Iterator(index: integer): integer;
begin
  Result := 0;
end;

constructor TLuaObject.Create(L: Plua_State);
begin
  FLS := L;
end;

end.

