unit RegOpts;


interface


uses
  Windows, Registry, SysUtils, Classes;


(*****************************************************************************)
(*  TRegOpt
(*****************************************************************************)

type
  TClassRegOpt = class of TRegOpt;

  TRegOpt = class
  private
    FFlagLazy: Boolean;
    FTextNameProgram: string;
    FTextNameSetting: string;

    procedure UpdateItems(FlagSaveDirty: Boolean);

  protected
    FlagDirty: Boolean;
    FlagDirtyList: Boolean;
    ListRegOpt: TList;

    procedure LoadValue; virtual; abstract;
    procedure SaveValue; virtual; abstract;

  public
    constructor Create     (TextNameProgram: string; TextNameSetting: string; FlagLazy: Boolean = True);
    constructor CreateEmpty(TextNameProgram: string; TextNameSetting: string; FlagLazy: Boolean = True);
    constructor CreateList (TextNameProgram: string; TextNameSetting: string; FlagLazy: Boolean = True);
    destructor Destroy; override;

    procedure ItemAdd(CountItems: Integer = 1);
    procedure ItemInsert(IndexItem: Integer; CountItems: Integer = 1);
    procedure ItemDelete(IndexItem: Integer; CountItems: Integer = 1);
    function ItemCount: Integer;

    property Lazy: Boolean       read FFlagLazy;
    property NameProgram: string read FTextNameProgram;
    property NameSetting: string read FTextNameSetting;
  end;


(*****************************************************************************)
(*  TRegOptInteger
(*****************************************************************************)

type
  TRegOptInteger = class(TRegOpt)
  private
    FValue: Integer;

    procedure SetValue(NewValue: Integer);
    function GetItem(IndexItem: Integer): TRegOptInteger;

  protected
    procedure LoadValue; override;
    procedure SaveValue; override;

  public
    constructor Create(TextNameProgram: string; TextNameSetting: string; ValueDefault: Integer; FlagLazy: Boolean = True);
    property Value: Integer read FValue write SetValue;
    property Items[IndexItem: Integer]: TRegOptInteger read GetItem; default;
  end;


(*****************************************************************************)
(*  TRegOptBoolean
(*****************************************************************************)

type
  TRegOptBoolean = class(TRegOpt)
  private
    FValue: Boolean;

    procedure SetValue(NewValue: Boolean);
    function GetItem(IndexItem: Integer): TRegOptBoolean;

  protected
    procedure LoadValue; override;
    procedure SaveValue; override;

  public
    constructor Create(TextNameProgram: string; TextNameSetting: string; ValueDefault: Boolean; FlagLazy: Boolean = True);
    property Value: Boolean read FValue write SetValue;
    property Items[IndexItem: Integer]: TRegOptBoolean read GetItem; default;
  end;


(*****************************************************************************)
(*  TRegOptString
(*****************************************************************************)

type
  TRegOptString = class(TRegOpt)
  private
    FValue: string;

    procedure SetValue(NewValue: string);
    function GetItem(IndexItem: Integer): TRegOptString;

  protected
    procedure LoadValue; override;
    procedure SaveValue; override;

  public
    constructor Create(TextNameProgram: string; TextNameSetting: string; ValueDefault: string; FlagLazy: Boolean = True);
    property Value: string read FValue write SetValue;
    property Items[IndexItem: Integer]: TRegOptString read GetItem; default;
  end;


implementation


(*****************************************************************************)
(*  TRegOpt
(*****************************************************************************)

var
  Reg: TRegistry;
  ListObjects: TList;


constructor TRegOpt.Create(TextNameProgram: string; TextNameSetting: string; FlagLazy: Boolean = True);
begin
  ListObjects.Add(Self);

  FFlagLazy := FlagLazy;
  FTextNameProgram := TextNameProgram;
  FTextNameSetting := TextNameSetting;

  LoadValue;
end;


constructor TRegOpt.CreateEmpty(TextNameProgram: string; TextNameSetting: string; FlagLazy: Boolean = True);
begin
  ListObjects.Add(Self);

  FFlagLazy := FlagLazy;
  FTextNameProgram := TextNameProgram;
  FTextNameSetting := TextNameSetting;

  LoadValue;
end;


constructor TRegOpt.CreateList(TextNameProgram: string; TextNameSetting: string; FlagLazy: Boolean = True);
var
  RegLoad: TRegistry;
  TextNameSettingItem: string;
begin
  ListObjects.Add(Self);

  FFlagLazy := FlagLazy;
  FTextNameProgram := TextNameProgram;
  FTextNameSetting := TextNameSetting;

  ListRegOpt := TList.Create;
  RegLoad := TRegistry.Create;

  if RegLoad.OpenKey('\Software\' + NameProgram, False) then
  begin
    while True do
    begin
      TextNameSettingItem := Format('%s[%d]', [NameSetting, ListRegOpt.Count + 1]);
      if not RegLoad.ValueExists(TextNameSettingItem) then Break;
      ListRegOpt.Add(TClassRegOpt(ClassType).Create(NameProgram, TextNameSettingItem, Lazy));
    end;

    RegLoad.CloseKey;
    RegLoad.Free;
  end;

  LoadValue;
end;


destructor TRegOpt.Destroy;
begin
  ListObjects.Remove(Self);

  if Lazy then
  begin
    if Assigned(ListRegOpt) then
    begin
      if FlagDirtyList then UpdateItems(False);
      ListRegOpt.Free;
    end;

    if FlagDirty then SaveValue;
  end;

  inherited;
end;


procedure TRegOpt.ItemAdd(CountItems: Integer);
begin
  if Assigned(ListRegOpt) then
    ItemInsert(ListRegOpt.Count, CountItems);
end;


procedure TRegOpt.ItemInsert(IndexItem: Integer; CountItems: Integer);
var
  IndexItemInserted: Integer;
  IndexItemMoved: Integer;
  RegOptInserted: TRegOpt;
  TextNameSettingItem: string;
begin
  if Assigned(ListRegOpt) then
  begin
    if IndexItem > ListRegOpt.Count then
      IndexItem := ListRegOpt.Count;

    for IndexItemInserted := 1 to CountItems do
    begin
      TextNameSettingItem := Format('%s[%d]', [NameSetting, IndexItem + IndexItemInserted]);
      RegOptInserted := TClassRegOpt(ClassType).CreateEmpty(NameProgram, TextNameSettingItem, Lazy);
      RegOptInserted.FlagDirty := True;
      ListRegOpt.Insert(IndexItem + IndexItemInserted - 1, RegOptInserted)
    end;

    for IndexItemMoved := IndexItem + CountItems to ListRegOpt.Count - 1 do
    begin
      TextNameSettingItem := Format('%s[%d]', [NameSetting, IndexItemMoved + 1]);
      TRegOpt(ListRegOpt[IndexItemMoved]).FTextNameSetting := TextNameSettingItem;
      TRegOpt(ListRegOpt[IndexItemMoved]).FlagDirty := True;
    end;

    if not Lazy then UpdateItems(True) else FlagDirtyList := True;
  end;
end;


procedure TRegOpt.ItemDelete(IndexItem: Integer; CountItems: Integer);
var
  IndexItemDeleted: Integer;
  IndexItemMoved: Integer;
  TextNameSettingItem: string;
begin
  if Assigned(ListRegOpt) and (IndexItem < ListRegOpt.Count) then
  begin
    for IndexItemDeleted := 1 to CountItems do
    begin
      TRegOpt(ListRegOpt[IndexItem]).FFlagLazy := False;
      TRegOpt(ListRegOpt[IndexItem]).Free;
      ListRegOpt.Delete(IndexItem);
    end;

    for IndexItemMoved := IndexItem to ListRegOpt.Count - 1 do
    begin
      TextNameSettingItem := Format('%s[%d]', [NameSetting, IndexItemMoved + 1]);
      TRegOpt(ListRegOpt[IndexItemMoved]).FTextNameSetting := TextNameSettingItem;
      TRegOpt(ListRegOpt[IndexItemMoved]).FlagDirty := True;
    end;

    if not Lazy then UpdateItems(True) else FlagDirtyList := True;
  end;
end;


function TRegOpt.ItemCount: Integer;
begin
  Result := -1;
  if Assigned(ListRegOpt) then
    Result := ListRegOpt.Count;
end;


procedure TRegOpt.UpdateItems(FlagSaveDirty: Boolean);
var
  IndexItem: Integer;
  TextNameSettingItem: string;
begin
  if Assigned(ListRegOpt) then
  begin
    if FlagSaveDirty then
    begin
      for IndexItem := ListRegOpt.Count - 1 downto 0 do
      begin
        if TRegOpt(ListRegOpt[IndexItem]).FlagDirty then
        begin
          TRegOpt(ListRegOpt[IndexItem]).SaveValue;
          TRegOpt(ListRegOpt[IndexItem]).FlagDirty := False;
        end;
      end;
    end;

    if Reg.OpenKey('\Software\' + NameProgram, False) then
    begin
      TextNameSettingItem := Format('%s[%d]', [NameSetting, ListRegOpt.Count + 1]);
      if Reg.ValueExists(TextNameSettingItem) then
        Reg.DeleteValue(TextNameSettingItem);
      Reg.CloseKey;
    end;
  end;
end;


(*****************************************************************************)
(*  TRegOptInteger
(*****************************************************************************)

constructor TRegOptInteger.Create(TextNameProgram: string; TextNameSetting: string; ValueDefault: Integer; FlagLazy: Boolean = True);
begin
  FValue := ValueDefault;
  inherited Create(TextNameProgram, TextNameSetting, FlagLazy);
end;


procedure TRegOptInteger.SetValue(NewValue: Integer);
begin
  FValue := NewValue;
  if not Lazy then SaveValue else FlagDirty := True;
end;


function TRegOptInteger.GetItem(IndexItem: Integer): TRegOptInteger;
begin
  Result := nil;
  if Assigned(ListRegOpt) and (IndexItem >= 0) and (IndexItem < ListRegOpt.Count) then
    Result := TRegOptInteger(ListRegOpt[IndexItem]);
end;


procedure TRegOptInteger.LoadValue;
begin
  if Reg.OpenKey('\Software\' + NameProgram, False) then
  begin
    if Reg.ValueExists(NameSetting) then
      FValue := Reg.ReadInteger(NameSetting);
    Reg.CloseKey;
  end;
end;


procedure TRegOptInteger.SaveValue;
begin
  if Reg.OpenKey('\Software\' + NameProgram, True) then
  begin
    Reg.WriteInteger(NameSetting, FValue);
    Reg.CloseKey;
  end;
end;


(*****************************************************************************)
(*  TRegOptBoolean
(*****************************************************************************)

constructor TRegOptBoolean.Create(TextNameProgram: string; TextNameSetting: string; ValueDefault: Boolean; FlagLazy: Boolean = True);
begin
  FValue := ValueDefault;
  inherited Create(TextNameProgram, TextNameSetting, FlagLazy);
end;


procedure TRegOptBoolean.SetValue(NewValue: Boolean);
begin
  FValue := NewValue;
  if not Lazy then SaveValue else FlagDirty := True;
end;


function TRegOptBoolean.GetItem(IndexItem: Integer): TRegOptBoolean;
begin
  Result := nil;
  if Assigned(ListRegOpt) and (IndexItem >= 0) and (IndexItem < ListRegOpt.Count) then
    Result := TRegOptBoolean(ListRegOpt[IndexItem]);
end;


procedure TRegOptBoolean.LoadValue;
begin
  if Reg.OpenKey('\Software\' + NameProgram, False) and Reg.ValueExists(NameSetting) then
  begin
    if Reg.ValueExists(NameSetting) then
      FValue := Reg.ReadBool(NameSetting);
    Reg.CloseKey;
  end;
end;


procedure TRegOptBoolean.SaveValue;
begin
  if Reg.OpenKey('\Software\' + NameProgram, True) then
  begin
    Reg.WriteBool(NameSetting, FValue);
    Reg.CloseKey;
  end;
end;


(*****************************************************************************)
(*  TRegOptString
(*****************************************************************************)

constructor TRegOptString.Create(TextNameProgram: string; TextNameSetting: string; ValueDefault: string; FlagLazy: Boolean = True);
begin
  FValue := ValueDefault;
  inherited Create(TextNameProgram, TextNameSetting, FlagLazy);
end;


procedure TRegOptString.SetValue(NewValue: string);
begin
  FValue := NewValue;
  if not Lazy then SaveValue else FlagDirty := True;
end;


function TRegOptString.GetItem(IndexItem: Integer): TRegOptString;
begin
  Result := nil;
  if Assigned(ListRegOpt) and (IndexItem >= 0) and (IndexItem < ListRegOpt.Count) then
    Result := TRegOptString(ListRegOpt[IndexItem]);
end;


procedure TRegOptString.LoadValue;
begin
  if Reg.OpenKey('\Software\' + NameProgram, False) and Reg.ValueExists(NameSetting) then
  begin
    if Reg.ValueExists(NameSetting) then
      FValue := Reg.ReadString(NameSetting);
    Reg.CloseKey;
  end;
end;


procedure TRegOptString.SaveValue;
begin
  if Reg.OpenKey('\Software\' + NameProgram, True) then
  begin
    Reg.WriteString(NameSetting, FValue);
    Reg.CloseKey;
  end;
end;


(*****************************************************************************)
(*  Initialization and Finalization
(*****************************************************************************)


procedure Initialize;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CURRENT_USER;

  ListObjects := TList.Create;
end;


procedure Finalize;
var
  IndexObject: Integer;
begin
  for IndexObject := ListObjects.Count - 1 downto 0 do
    TRegOpt(ListObjects[IndexObject]).Free;

  Reg.Free;
  ListObjects.Free;
end;


initialization Initialize;
finalization Finalize;


end.
