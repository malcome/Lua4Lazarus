{
  Lua4Lazarus

    ActiveXObject

    License: New BSD
      Copyright(c)2010- Malcome@Japan All rights reserved.

    Note:

}
unit l4l_activex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua53;

function CreateActiveXObject(L : Plua_State) : Integer; cdecl;
function CreateRefType(L : Plua_State) : Integer; cdecl;

implementation
uses
  Windows, ComObj, ActiveX, variants, varutils, l4l_object;

const
  FIELD_ID = '___IDispatch___';
  FIELD_FN = '___FuncName___';
  FIELD_ID_PARENT = '___IDispatchParent___';
  FIELD_ID_CLIENT = '___IDispatchClient___';

type

  { TInterfacedBase }

  TInterfacedBase = class(TObject, IUnknown)
    function QueryInterface(constref iid : tguid; out obj) : longint; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  { TEventSink }

  TEventSink = class(TInterfacedBase, IDispatch)
  private
    L: PLua_State;
    FCookie: DWORD;
    FCPoint: IConnectionPoint;
    FInfo: ITypeInfo;
    EventList: TStringList;
  public
    constructor Create(aL: PLua_State);
    destructor Destroy; override;
    procedure AttachEvent(server: IDispatch);
    procedure DetachEvent;
    function IsEventSupport: boolean;
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
    function GetTypeInfo({%H-}Index, {%H-}LocaleID: Integer; out {%H-}TypeInfo): HRESULT; stdcall;
    function GetIDsOfNames(const {%H-}IID: TGUID; {%H-}Names: Pointer; {%H-}NameCount, {%H-}LocaleID: Integer; {%H-}DispIDs: Pointer): HRESULT; stdcall;
    function Invoke(DispID: Integer; const {%H-}IID: TGUID; {%H-}LocaleID: Integer;
                    {%H-}Flags: Word; var {%H-}Params; {%H-}VarResult, {%H-}ExcepInfo,
                    {%H-}ArgErr: Pointer): HRESULT; stdcall;
  end;

function call(L : Plua_State) : Integer; cdecl; forward;
function gc_ID(L : Plua_State) : Integer; cdecl; forward;
procedure DoCreateActiveXObject(L : Plua_State; id: IDispatch); forward;

procedure ChkErr(L : Plua_State; Val: HResult; const prop: string='');
var
  s: string;
begin
  if not(Succeeded(Val)) then begin
    case val of
      DISP_E_MEMBERNOTFOUND: s:= 'Member Not Found';
      DISP_E_TYPEMISMATCH: s:= 'Type Mismatch';
      DISP_E_UNKNOWNNAME: s:= 'Unknown Name';
      DISP_E_BADPARAMCOUNT: s:= 'Bad Param Count';
      E_INVALIDARG: s:= 'Invaid Arg'
      else s:= 'ActiveX Error';
    end;
    s:= s + Format('(%x)', [Val]);
    if prop <> '' then s := s + ' in "' + prop + '".';
    luaL_error(L, PChar(s));
  end;
end;

procedure Lua2Var(L : Plua_State; va: POleVariant; index: integer; toWideStr: boolean = True);
var
  i, j: integer;
  p: POleVariant;
  obj: TLuaObject;
  hp: TVarArrayBoundArray;
begin
  VariantInit(TVarData(va^));
  case lua_type(L, index) of
    LUA_TNIL: va^:= VarAsType(va^, varNull);
    LUA_TBOOLEAN: va^:= lua_toboolean(L, index);
    LUA_TNUMBER: begin
      if lua_isinteger(L, index) then
        va^:= lua_tointeger(L, index)
      else
        va^:= lua_tonumber(L, index);
    end;
    LUA_TSTRING: begin
      if toWideStr then
        va^:= UTF8Decode(lua_tostring(L, index))
      else
        va^:= lua_tostring(L, index);
    end;
    LUA_TTABLE: begin
      lua_getmetatable(L, index);
      lua_getfield(L, -1, FIELD_ID);
      if lua_isnil(L, -1) then begin
        obj:= l4l_toobject(L, index);
        if not Assigned(obj) then begin
          j:= lua_rawlen(L, index);
          if j > 0 then begin
            //va^:= VarArrayCreate([0, j-1], varVariant);
            hp[0].LowBound:= 0;
            hp[0].ElementCount:= j;
            TVarData(va^).vType:= varVariant or varArray;
            TVarData(va^).vArray:= SafeArrayCreate(varVariant, 1, hp);
            SafeArrayAccessData(TVarData(va^).vArray, p{%H-});
            for i:= 1 to j do begin
              lua_rawgeti(L, index, i);
              Lua2Var(L, p, lua_gettop(L));
              lua_pop(L, 1);
              Inc(p);
            end;
            SafeArrayUnaccessData(TVarData(va^).vArray);
          end else begin
            TVarData(va^).vType:= varVariant or varArray;
            TVarData(va^).vArray:= nil;
          end;
        end else begin
          // TLuaObject
          //if obj is TLuaInteger then begin
          //  va^:= TLuaInteger(obj).Value;
          //end else begin
            va^:= VarAsType(va^, varNull);
          //end;
        end;
      end else begin
        p:= lua_touserdata(L, -1);
        TVarData(va^).vtype:= varVariant or varByRef;
        TVarData(va^).vpointer:= p;
      end;
      lua_pop(L, 2);
    end;
  end{case};
end;

function Index(L : Plua_State) : Integer; cdecl;
var
  p: POleVariant;
  key: string;
  s: string;
  ws: WideString;
  id: IDispatch;
  di: TDispID;
  param: TDispParams;
  ret: OleVariant;
  hr: HResult;
begin
  Result:= 0;

  key := LowerCase(lua_tostring(L, 2));
  lua_pushstring(L, key);
  lua_rawget(L, 1);
  if not lua_isnil(L, -1) then begin
    // for method name is case insesitive
    Result:= 1;
    Exit;
  end;
  lua_pop(L, 1);

  lua_getmetatable(L, 1);
  lua_getfield(L, -1, FIELD_ID);
  p:= lua_touserdata(L, -1);
  lua_pop(L, 2);

  if TVarData(p^).vtype <> varDispatch then begin
    //key:= LowerCase(key);
    if key = 'value' then begin
      case VarType(p^) of
        varNull: lua_pushnil(L);
        varSmallint,varInteger,varshortint,varByte,
        varword,varlongword,varint64,varqword: lua_pushinteger(L, p^);
        varSingle,varDouble,vardecimal: lua_pushnumber(L, p^);
        varBoolean: lua_pushboolean(L, p^);
        // TODO: varArray
        else begin
          ws := p^;
          s := UTF8Encode(ws);
          lua_pushstring(L, s);
        end;
      end;
    end else
      Exit;
  end else begin
    id:= IDispatch(TVarData(p^).vDispatch);
    ws:= UTF8Decode(key);
    ChkErr(L, id.GetIDsOfNames(GUID_NULL, @ws, 1, GetUserDefaultLCID, @di), key);
    param.rgvarg := nil;
    param.rgdispidNamedArgs := nil;
    param.cArgs := 0;
    param.cNamedArgs := 0;
    VariantInit(TVarData({%H-}ret));

    hr:= id.Invoke(di, GUID_NULL, GetUserDefaultLCID,
                   DISPATCH_PROPERTYGET, param, @ret, nil, nil);
    if hr = 0 then begin
      // Return property value
      case VarType(ret) of
        varNull: lua_pushnil(L);
        varSmallint,varInteger,varshortint,varByte,
        varword,varlongword,varint64,varqword: lua_pushinteger(L, ret);
        varSingle,varDouble,vardecimal: lua_pushnumber(L, ret);
        varBoolean: lua_pushboolean(L, ret);
        varDispatch: DoCreateActiveXObject(L, ret);
        else begin
          ws := ret;
          s := UTF8Encode(ws);
          lua_pushstring(L, s);
        end;
      end;
    end else begin
      // Return Table for function(method) call
      lua_newtable(L);
    end;

    // Change Metatable for function call
    if lua_getmetatable(L, -1) = 0 then lua_newtable(L);
    lua_pushstring(L, FIELD_FN);
    lua_pushstring(L, key);
    lua_rawset(L, -3);

    lua_pushstring(L, FIELD_ID_PARENT);
    p:= lua_newuserdata(L, SizeOf(OleVariant));
    VariantInit(TVarData(p^));
    p^:= id;
    if lua_getmetatable(L, -1) = 0 then lua_newtable(L);
    lua_pushstring(L, '__gc');
    lua_pushcfunction(L, @gc_ID);
    lua_settable(L, -3);
    lua_setmetatable(L, -2);

    lua_rawset(L, -3);
    lua_pushstring(L, '__call');
    lua_pushcfunction(L, @call);
    lua_rawset(L, -3);
    lua_setmetatable(L, -2);

    if hr = DISP_E_MEMBERNOTFOUND then begin
      // Key is method, not property
      lua_pushstring(L, key); // key is Lower Case
      lua_pushvalue(L, -2);
      lua_rawset(L, 1);
    end;
  end;
  Result := 1;
end;

function NewIndex(L : Plua_State) : Integer; cdecl;
var
  p: POleVariant;
  key: string;
  ws: WideString;
  id: IDispatch;
  di, diput: TDispID;
  param: TDispParams;
  v: OleVariant;
  i: integer;
  sink: ^TEventSink;
begin
  Result:=0;
  lua_getmetatable(L, 1);
  lua_getfield(L, -1, FIELD_ID);
  p:= lua_touserdata(L, -1);
  lua_pop(L, 2);
  key := lua_tostring(L, 2);

  if TVarData(p^).vtype <> varDispatch then begin
    key:= LowerCase(key);
    if key = 'value' then begin
      Lua2Var(L, p, 3, False);
    end else
      Exit;
  end else begin
    id:= IDispatch(TVarData(p^).vDispatch);
    ws:= UTF8Decode(key);
    if lua_isfunction(L, 3) or lua_isnil(L, 3) then begin
      // Set event
      lua_getmetatable(L, 1);
      lua_getfield(L, -1, FIELD_ID_CLIENT);
      sink:= lua_touserdata(L, -1);
      lua_pop(L, 2);
      key:= LowerCase(key);
      if lua_isfunction(L, 3) then begin
        i:= sink^.EventList.IndexOf(key);
        if i >= 0 then begin
          // Release old function from REGISTRY
          luaL_unref(L, LUA_REGISTRYINDEX, Integer(sink^.EventList.Objects[i]));
          // Save new function to REGISTRY
          lua_pushvalue(L, 3);
          sink^.EventList.Objects[i]:= TObject(luaL_Ref(L, LUA_REGISTRYINDEX));
        end else begin
          if sink^.EventList.Count = 0 then
            sink^.AttachEvent(id); // Start event receive
          // Save new function to REGISTRY
          lua_pushvalue(L, 3);
          sink^.EventList.AddObject(key, TObject(luaL_Ref(L, LUA_REGISTRYINDEX)));
        end;
      end else begin
        // delete event
        i:= sink^.EventList.IndexOf(key);
        if i >= 0 then begin
          sink^.EventList.Delete(i);
          if sink^.EventList.Count = 0 then sink^.DetachEvent;
        end;
      end;
    end else begin
      // Set value
      ChkErr(L, id.GetIDsOfNames(GUID_NULL, @ws, 1, GetUserDefaultLCID, @di), key);
      Lua2Var(L, @v, 3);
      diput := DISPID_PROPERTYPUT;
      param.rgvarg := @v;
      param.cArgs := 1;
      param.rgdispidNamedArgs := @diput;
      param.cNamedArgs := 1;
      ChkErr(L, id.Invoke(di, GUID_NULL, GetUserDefaultLCID,
       DISPATCH_PROPERTYPUT, param, nil, nil, nil), key);
    end;
  end;
end;

function call(L : Plua_State) : Integer; cdecl;
var
  i, c, t: integer;
  p: POleVariant;
  id: IDispatch;
  di: TDispID;
  s, func: string;
  ws: WideString;
  arglist, pp: {$IFDEF VER2_4}lPVariantArg{$ELSE}PVariantArg{$ENDIF};
  param: TDispParams;
  ret: OleVariant;
begin
  Result:= 0;
  c:= lua_gettop(L);
  t:= 1; // ToDo: const?
  lua_getmetatable(L, 1);
  lua_getfield(L, -1, FIELD_ID_PARENT);
  p:= lua_touserdata(L, -1);
  lua_pop(L, 2);
  if TVarData(p^).vtype <> varDispatch then Exit;
  id:= IDispatch(TVarData(p^).vDispatch);

  lua_getmetatable(L, 1);
  lua_getfield(L, -1, FIELD_FN);
  func:= lua_tostring(L, -1);
  lua_pop(L, 2);
  ws:= UTF8Decode(func);
  ChkErr(L, id.GetIDsOfNames(GUID_NULL, @ws, 1, GetUserDefaultLCID, @di), func);
  GetMem(arglist, SizeOf({$IFDEF VER2_4}VariantArg{$ELSE}TVariantArg{$ENDIF}) * (c-t));
  try
    // 逆順で
    pp := arglist;
    for i := c downto t+1 do begin
      Lua2Var(L, POleVariant(pp), i);
      Inc(pp);
    end;
    param.cArgs := c - t;
{$IFDEF VER2_4}
    param.rgvarg := arglist;
{$ELSE}
    param.rgvarg := PVariantArgList(arglist);
{$ENDIF}
    param.rgdispidNamedArgs := nil;
    param.cNamedArgs := 0;
    VariantInit(TVarData({%H-}ret));

    ChkErr(L, id.Invoke(
     di,
     GUID_NULL,
     GetUserDefaultLCID,
     DISPATCH_PROPERTYGET or DISPATCH_METHOD,
     param, @ret, nil, nil), func);
  finally
    FreeMem(arglist);
  end;

  case VarType(ret) of
    varNull: lua_pushnil(L);
    varSmallint,varInteger,varshortint,varByte,
    varword,varlongword,varint64,varqword: lua_pushinteger(L, ret);
    varSingle,varDouble,vardecimal: lua_pushnumber(L, ret);
    varBoolean: lua_pushboolean(L, ret);
    varDispatch: DoCreateActiveXObject(L, ret);
    else{case} begin
      ws := ret;
      s := UTF8Encode(ws);
      lua_pushstring(L, s);
    end;
  end;

  Result := 1;
end;

function equal(L : Plua_State) : Integer; cdecl;
var
  p1, p2: POleVariant;
  ti1, ti2: ITypeInfo;
  ta1, ta2: lPTypeAttr;
begin
  Result:= 1;
  lua_pushboolean(L, False);

  if lua_type(L, 2) <> LUA_TTABLE then Exit;
  lua_getmetatable(L, 2);
  lua_getfield(L, -1, FIELD_ID);
  if lua_isnil(L, -1) then begin
    lua_pop(L, 2);
    Exit;
  end;
  p1:= lua_touserdata(L, -1);
  lua_pop(L, 2);
  if TVarData(p1^).vtype <> varDispatch then Exit;
  if IDispatch(TVarData(p1^).vdispatch).GetTypeInfo(0, 0, ti1) <> S_OK then Exit;
  if ti1.GetTypeAttr(ta1) <> S_OK then Exit;
  try
    lua_getmetatable(L, 1);
    lua_getfield(L, -1, FIELD_ID);
    if lua_isnil(L, -1) then begin
      lua_pop(L, 2);
      Exit;
    end;
    p2:= lua_touserdata(L, -1);
    lua_pop(L, 2);
    if TVarData(p2^).vtype <> varDispatch then Exit;
    if IDispatch(TVarData(p2^).vdispatch).GetTypeInfo(0, 0, ti2) <> S_OK then Exit;
    if ti2.GetTypeAttr(ta2) <> S_OK then Exit;
    try
      if IsEqualIID(ta1^.GUID, ta2^.GUID) then begin
        lua_pop(L, 1);
        lua_pushboolean(L, True);
      end;
    finally
      ti2.ReleaseTypeAttr(ta2);
    end;
  finally
    ti1.ReleaseTypeAttr(ta1);
  end;
end;

function iterator(L : Plua_State) : Integer; cdecl;
var
  i: integer;
  p: POleVariant;
  id: IDispatch;
  s: string;
  ws: WideString;
  param: TDispParams;
  v, ret: OleVariant;
begin
  Result:= 0;
  lua_getmetatable(L, 1);
  lua_getfield(L, -1, FIELD_ID);
  p:= lua_touserdata(L, -1);
  lua_pop(L, 2);

  if TVarData(p^).vtype <> varDispatch then Exit;

  id:= IDispatch(TVarData(p^).vDispatch);
  i:= lua_tointeger(L, lua_upvalueindex(1));
  {%H-}lua_pushinteger(L, i+1);
  lua_replace(L, lua_upvalueindex(1));

  VariantInit(TVarData({%H-}v));
  v := i;
  param.cArgs := 1;
  param.rgvarg := @v;
  param.rgdispidNamedArgs := nil;
  param.cNamedArgs := 0;
  VariantInit(TVarData({%H-}ret));

  if id.Invoke(
   DISPID_VALUE,
   GUID_NULL,
   GetUserDefaultLCID,
   DISPATCH_PROPERTYGET or DISPATCH_METHOD,
   param, @ret, nil, nil) = 0 then begin
    case VarType(ret) of
      varNull: lua_pushnil(L);
      varSmallint,varInteger,varshortint,varByte,
      varword,varlongword,varint64,varqword: lua_pushinteger(L, ret);
      varSingle,varDouble,vardecimal: lua_pushnumber(L, ret);
      varBoolean: lua_pushboolean(L, ret);
      varDispatch: begin
        if TVarData(ret).vdispatch <> nil then begin
          DoCreateActiveXObject(L, ret);
        end else begin
          lua_pushnil(L);
        end;
      end;
      else begin
        ws := ret;
        s := UTF8Encode(ws);
        lua_pushstring(L, s);
      end;
    end;
  end else begin
    lua_pushnil(L);
  end;
  Result := 1;
end;

function pairs(L : Plua_State) : Integer; cdecl;
begin
  lua_pushinteger(L, 0); // upvalue for loop counter
  lua_pushcclosure(L, @iterator, 1); // f
  lua_pushvalue(L, 1); // t
  lua_pushnil(L); // s
  Result:= 3;
end;

function gc({%H-}L : Plua_State) : Integer; cdecl;
begin
  Result:= 0;
end;

function gc_ID(L : Plua_State) : Integer; cdecl;
var
  p: POleVariant;
begin
  Result:= 0;
  p:= lua_touserdata(L, 1);
  if TVarData(p^).vtype <> varDispatch then Exit;
  IDispatch(TVarData(p^).vDispatch)._Release;
  //p^:= Unassigned;
end;

function gc_ID_client(L : Plua_State) : Integer; cdecl;
var
  sink: ^TEventSink;
begin
  Result:= 0;
  sink:= lua_touserdata(L, 1);
  sink^.Free;
end;

procedure DoCreateActiveXObject(L : Plua_State; id: IDispatch);
var
  p: POleVariant;
  sink: ^TEventSink;
begin
  //id._AddRef;
  lua_newtable(L);

  lua_newtable(L); // new metatable

  lua_pushstring(L, '__gc');
  lua_pushcfunction(L, @gc);
  lua_settable(L, -3);

  lua_pushstring(L, FIELD_ID);
  p:= lua_newuserdata(L, SizeOf(OleVariant));
  VariantInit(TVarData(p^));
  p^:= id;
  if lua_getmetatable(L, -1) = 0 then lua_newtable(L);
  lua_pushstring(L, '__gc');
  lua_pushcfunction(L, @gc_ID);
  lua_settable(L, -3);
  lua_setmetatable(L, -2);
  lua_settable(L, -3);

  lua_pushstring(L, '__newindex');
  lua_pushcfunction(L, @NewIndex);
  lua_settable(L, -3);

  lua_pushstring(L, '__index');
  lua_pushcfunction(L, @Index);
  lua_settable(L, -3);

  lua_pushstring(L, '__eq');
  lua_pushcfunction(L, @equal);
  lua_settable(L, -3);

  lua_pushstring(L, '__pairs');
  lua_pushcfunction(L, @pairs);
  lua_settable(L, -3);

  lua_pushstring(L, '__ipairs');
  lua_pushcfunction(L, @pairs);
  lua_settable(L, -3);

  lua_pushstring(L, FIELD_ID_CLIENT);
  sink:= lua_newuserdata(L, SizeOf(TEventSink));
  if lua_getmetatable(L, -1) = 0 then lua_newtable(L);
  lua_pushstring(L, '__gc');
  lua_pushcfunction(L, @gc_ID_client);
  lua_settable(L, -3);
  lua_setmetatable(L, -2);
  lua_settable(L, -3);
  sink^:= TEventSink.Create(L);

  lua_setmetatable(L, -2);
end;

function CreateActiveXObject(L : Plua_State) : Integer; cdecl;
begin
  DoCreateActiveXObject(L, CreateOleObject(lua_tostring(L, 1)));
  Result := 1;
end;

// Make ref(ByRef) type
function CreateRefType(L : Plua_State) : Integer; cdecl;
var
  p: POleVariant;
  ParamCount: integer;
begin
  ParamCount:= lua_gettop(L);
  lua_newtable(L);

  lua_newtable(L);
  lua_pushstring(L, FIELD_ID);
  p:= lua_newuserdata(L, SizeOf(OleVariant));
  VariantInit(TVarData(p^));
  if ParamCount > 0 then begin
    Lua2Var(L, p, 1, False);
  end;
  lua_settable(L, -3);

  lua_pushstring(L, '__newindex');
  lua_pushcfunction(L, @NewIndex);
  lua_settable(L, -3);
  lua_pushstring(L, '__index');
  lua_pushcfunction(L, @Index);
  lua_settable(L, -3);
  lua_pushstring(L, '__pairs');
  lua_pushcfunction(L, @pairs);
  lua_settable(L, -3);
  lua_pushstring(L, '__ipairs');
  lua_pushcfunction(L, @pairs);
  lua_settable(L, -3);
  lua_setmetatable(L, -2);

  Result := 1;
end;

{ TInterfacedBase }

function TInterfacedBase.QueryInterface(constref iid : tguid; out obj) : longint; stdcall;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TInterfacedBase._AddRef: Integer; stdcall;
begin
  Result := -1;
end;

function TInterfacedBase._Release: Integer; stdcall;
begin
  Result := -1;
end;

{ TEventSink }

function TEventSink.GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
begin
  Count:= 0;
  Result:= S_OK;
end;

function TEventSink.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HRESULT; stdcall;
begin
  Result:= E_NOTIMPL;
end;

function TEventSink.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount,
  LocaleID: Integer; DispIDs: Pointer): HRESULT; stdcall;
begin
  Result:= E_NOTIMPL;
end;

function TEventSink.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HRESULT; stdcall;
var
  dispparams: TDispParams;
  name: WideString;
  name2: string;
  i: Integer;
  v: OleVariant;
begin
  Result := S_FALSE;
  if FInfo.GetDocumentation(DispId, @name, nil, nil, nil) <> S_OK then Exit;
  if name = '' then Exit;
  name2:= UTF8Encode(name);
  name:= '';
  i:= EventList.IndexOf(LowerCase(name2));
  if i >= 0 then begin
    lua_rawgeti(L, LUA_REGISTRYINDEX, Integer(EventList.Objects[i]));

    dispparams := TDispParams(Params);
    // upsidedown!
    for i := dispparams.cArgs - 1 downto 0 do begin
      try
        v := OleVariant(dispparams.rgvarg^[i]);
      except
        VariantInit(TVarData(v));
      end;
      case VarType(v) of
        varNull: lua_pushnil(L);
        varSmallint,varInteger,varshortint,varByte,
        varword,varlongword,varint64,varqword: lua_pushinteger(L, v);
        varSingle,varDouble,vardecimal: lua_pushnumber(L, v);
        varBoolean: lua_pushboolean(L, v);
        varDispatch: DoCreateActiveXObject(L, v);
        else begin
          lua_pushstring(L, UTF8Encode(v));
        end;
      end; {case}
    end; {for}

    lua_pcall(L, dispparams.cArgs, 0{result}, 0);
    // TODO: result
  end;
  Result := S_OK;
end;

constructor TEventSink.Create(aL: PLua_State);
begin
  inherited Create;
  L:= aL;
  EventList:= TStringList.Create;
  EventList.Sorted:= True;
  FInfo:= nil;
  FCPoint:= nil;
end;

destructor TEventSink.Destroy;
var
  i: Integer;
begin
  for i:= 0 to EventList.Count-1 do
    luaL_unref(L, LUA_REGISTRYINDEX, Integer(EventList.Objects[i]));
  EventList.Free;
  if IsEventSupport then
    FCPoint.UnAdvise(FCookie);
  inherited Destroy;
end;

procedure TEventSink.AttachEvent(server: IDispatch);
var
  cp_c: IConnectionPointContainer;
  cp: IConnectionPoint;
  enum: IEnumConnectionPoints;
  fetched: LongInt;
  num: LongWord;
  iid: TIID;
  ti: ITypeInfo;
  tl: ITypeLib;
  i: integer;
begin
  if server.QueryInterface(IID_IConnectionPointContainer, cp_c) <> S_OK then
    Exit;
  ChkErr(L, cp_c.EnumConnectionPoints(enum));
  if Assigned(enum) then begin
    enum.Reset;
    i:= 0;
    while enum.Next(1, cp, @fetched) = S_OK do begin
      if fetched = 1 then begin
        if cp.GetConnectionInterface(iid) <> S_OK then continue;
        if server.GetTypeInfo(0, 0, ti) <> S_OK then continue;
        if ti.GetContainingTypeLib(tl, num) <> S_OK then continue;
        if tl.GetTypeInfoOfGuid(iid, FInfo) <> S_OK then continue;
        if cp.Advise(Self, FCookie) <> S_OK then continue;
        FCPoint:= cp;
      end;
      Inc(i);
      Break; // TODO
    end;
  end;
end;

procedure TEventSink.DetachEvent;
begin
  if Assigned(FCPoint) then begin
    FCPoint.UnAdvise(FCookie);
  end;
end;

function TEventSink.IsEventSupport: boolean;
begin
  Result:= Assigned(FCPoint);
end;

end.

