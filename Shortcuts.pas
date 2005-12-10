unit Shortcuts;

interface

uses Windows, Ole2, ShlObj;

const
  CLSID_InternetShortcut: TGUID = (
    D1:$FBF23B40; D2:$E3F0; D3:$101B; D4:($84,$88,$00,$AA,$00,$3E,$56,$F8));
  IID_IUniformResourceLocator: TGUID = (
    D1:$FBF23B80; D2:$E3F0; D3:$101B; D4:($84,$88,$00,$AA,$00,$3E,$56,$F8));

  IURL_SETURL_FL_GUESS_PROTOCOL = $0001;
  IURL_SETURL_FL_USE_DEFAULT_PROTOCOL = $0002;
  IURL_INVOKECOMMAND_FL_ALLOW_UI = $0001;
  IURL_INVOKECOMMAND_FL_USE_DEFAULT_VERB = $0002;

type
  PUrlInvokeCommandInfo = ^TUrlInvokeCommandInfo;
  TUrlInvokeCommandInfo = record
    dwcbSize: DWord;
    dwFlags: DWord;
    hwndParent: HWnd;
    pcszVerb: LPCSTR;
  end;

  IUniformResourceLocator = class(IUnknown)
    function SetURL(pcszURL: LPCSTR; dwInFlags: DWord): HResult; virtual; stdcall; abstract;
    function GetURL(var ppszURL: LPSTR): HResult; virtual; stdcall; abstract;
    function InvokeCommand(var purlici: TUrlInvokeCommandInfo): HResult; virtual; stdcall; abstract;
  end;

type
  PInternetShortcut = ^TInternetShortcut;
  TInternetShortcut = class
  private
    UniformResourceLocator: IUniformResourceLocator;
    procedure SetURL(URL: string);
    function GetURL: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(ShortcutFile: string);
    procedure Save(ShortcutFile: string);
    property URL: string read GetURL write SetURL;
  end;

  PFileShortcut = ^TFileShortcut;
  TFileShortcut = class
  private
    ShellLink: IShellLink;
    procedure SetPath(Path: string);
    function GetPath: string;
    procedure SetArguments(Arguments: string);
    function GetArguments: string;
    procedure SetWorkingDirectory(WorkingDirectory: string);
    function GetWorkingDirectory: string;
    procedure SetDescription(Description: string);
    function GetDescription: string;
    procedure SetHotkey(Hotkey: Word);
    function GetHotkey: Word;
    procedure SetIconPath(IconPath: string);
    function GetIconPath: string;
    procedure SetIconIndex(IconIndex: Integer);
    function GetIconIndex: Integer;
    procedure SetShowCmd(ShowCmd: Integer);
    function GetShowCmd: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(ShortcutFile: string);
    procedure Save(ShortcutFile: string);
    procedure Resolve(WindowHandle: HWnd; Flags: DWord);
    property Path: string read GetPath write SetPath;
    property Arguments: string read GetArguments write SetArguments;
    property Description: string read GetDescription write SetDescription;
    property WorkingDirectory: string read GetWorkingDirectory write SetWorkingDirectory;
    property Hotkey: Word read GetHotkey write SetHotkey;
    property IconPath: string read GetIconPath write SetIconPath;
    property IconIndex: Integer read GetIconIndex write SetIconIndex;
    property ShowCmd: Integer read GetShowCmd write SetShowCmd;
  end;

implementation

constructor TInternetShortcut.Create;
begin
  CoInitialize(nil);
  CoCreateInstance(CLSID_InternetShortcut, nil, CLSCTX_INPROC_SERVER, IID_IUniformResourceLocator, UniformResourceLocator);
end;

destructor TInternetShortcut.Destroy;
begin
  CoUninitialize;
  inherited;
end;

procedure TInternetShortcut.SetURL(URL: string);
begin
  if Assigned(UniformResourceLocator) then
  begin
    UniformResourceLocator.SetURL(PChar(URL), IURL_SETURL_FL_GUESS_PROTOCOL);
  end;
end;

function TInternetShortcut.GetURL: string;
var
  URL: PChar;
  Malloc: IMalloc;
begin
  if Assigned(UniformResourceLocator) then
  begin
    UniformResourceLocator.GetURL(URL);
    GetURL := URL;
    //if SHGetMalloc(Malloc) = NOERROR then
    if CoGetMalloc(MEMCTX_TASK, Malloc) = NOERROR then
    begin
      Malloc.Free(URL);
      Malloc.Release;
    end;
  end;
end;

procedure TInternetShortcut.Load(ShortcutFile: string);
var
  Buffer: PChar;
  FilePart: PChar;
  PersistFile: IPersistFile;
begin
  if Assigned(UniformResourceLocator) then
  begin
    if UniformResourceLocator.QueryInterface(IID_IPersistFile, PersistFile) = S_OK then
    begin
      GetMem(Buffer, MAX_PATH + 1);
      GetFullPathName(PChar(ShortcutFile), MAX_PATH + 1, Buffer, FilePart);
      PersistFile.Load(StringToOleStr(Buffer), STGM_READ);
    end;
  end;
end;

procedure TInternetShortcut.Save(ShortcutFile: string);
var
  Buffer: PChar;
  FilePart: PChar;
  PersistFile: IPersistFile;
begin
  if Assigned(UniformResourceLocator) then
  begin
    if UniformResourceLocator.QueryInterface(IID_IPersistFile, PersistFile) = S_OK then
    begin
      GetMem(Buffer, MAX_PATH + 1);
      GetFullPathName(PChar(ShortcutFile), MAX_PATH + 1, Buffer, FilePart);
      PersistFile.Save(StringToOleStr(Buffer), True);
    end;
  end;
end;



constructor TFileShortcut.Create;
begin
  CoInitialize(nil);
  CoCreateInstance(Ole2.TGUID(CLSID_ShellLink), nil, CLSCTX_INPROC_SERVER, Ole2.TGUID(IID_IShellLinkA), ShellLink);
end;

destructor TFileShortcut.Destroy;
begin
  CoUninitialize;
  inherited;
end;

procedure TFileShortcut.SetPath(Path: string);
begin
  if Assigned(ShellLink) then
  begin
    ShellLink.SetPath(PChar(Path));
  end;
end;

function TFileShortcut.GetPath: string;
var
  FindData: TWin32FindData;
  Buffer: PChar;
begin
  if Assigned(ShellLink) then
  begin
    GetMem(Buffer, MAX_PATH + 1);
    ShellLink.GetPath(Buffer, MAX_PATH + 1, FindData, SLGP_UNCPRIORITY);
    GetPath := Buffer;
    FreeMem(Buffer);
  end;
end;

procedure TFileShortcut.SetArguments(Arguments: string);
begin
  if Assigned(ShellLink) then
  begin
    ShellLink.SetArguments(PChar(Arguments));
  end;
end;

function TFileShortcut.GetArguments: string;
var
  Buffer: PChar;
begin
  if Assigned(ShellLink) then
  begin
    GetMem(Buffer, 256);
    ShellLink.GetArguments(Buffer, 256);
    GetArguments := Buffer;
    FreeMem(Buffer);
  end;
end;

procedure TFileShortcut.SetDescription(Description: string);
begin
  if Assigned(ShellLink) then
  begin
    ShellLink.SetDescription(PChar(Description));
  end;
end;

function TFileShortcut.GetDescription: string;
var
  Buffer: PChar;
begin
  if Assigned(ShellLink) then
  begin
    GetMem(Buffer, 256);
    ShellLink.GetDescription(Buffer, 256);
    GetDescription := Buffer;
    FreeMem(Buffer);
  end;
end;

procedure TFileShortcut.SetWorkingDirectory(WorkingDirectory: string);
begin
  if Assigned(ShellLink) then
  begin
    ShellLink.SetWorkingDirectory(PChar(WorkingDirectory));
  end;
end;

function TFileShortcut.GetWorkingDirectory: string;
var
  Buffer: PChar;
begin
  if Assigned(ShellLink) then
  begin
    GetMem(Buffer, 256);
    ShellLink.GetWorkingDirectory(Buffer, 256);
    GetWorkingDirectory := Buffer;
    FreeMem(Buffer);
  end;
end;

procedure TFileShortcut.SetHotkey(Hotkey: Word);
begin
  if Assigned(ShellLink) then
  begin
    ShellLink.SetHotkey(Hotkey);
  end;
end;

function TFileShortcut.GetHotkey: Word;
var
  Hotkey: Word;
begin
  if Assigned(ShellLink) then
  begin
    ShellLink.GetHotkey(Hotkey);
    GetHotkey := Hotkey;
  end
  else begin
    GetHotkey := 0;
  end;
end;

procedure TFileShortcut.SetIconPath(IconPath: string);
begin
  if Assigned(ShellLink) then
  begin
    ShellLink.SetIconLocation(PChar(IconPath), 0);
  end;
end;

function TFileShortcut.GetIconPath: string;
var
  IconIndex: Integer;
  Buffer: PChar;
begin
  if Assigned(ShellLink) then
  begin
    GetMem(Buffer, MAX_PATH + 1);
    ShellLink.GetIconLocation(Buffer, MAX_PATH + 1, IconIndex);
    GetIconPath := Buffer;
    FreeMem(Buffer);
  end;
end;

procedure TFileShortcut.SetIconIndex(IconIndex: Integer);
begin
  if Assigned(ShellLink) then
  begin
    ShellLink.SetIconLocation(PChar(GetIconPath), IconIndex);
  end;
end;

function TFileShortcut.GetIconIndex: Integer;
var
  IconIndex: Integer;
begin
  if Assigned(ShellLink) then
  begin
    ShellLink.GetIconLocation(nil, 0, IconIndex);
    GetIconIndex := IconIndex;
  end
  else begin
    GetIconIndex := -1;
  end;
end;

procedure TFileShortcut.SetShowCmd(ShowCmd: Integer);
begin
  if Assigned(ShellLink) then
  begin
    ShellLink.SetShowCmd(ShowCmd);
  end;
end;

function TFileShortcut.GetShowCmd: Integer;
var
  ShowCmd: Integer;
begin
  if Assigned(ShellLink) then
  begin
    ShellLink.GetShowCmd(ShowCmd);
    GetShowCmd := ShowCmd;
  end
  else begin
    GetShowCmd := 0;
  end;
end;

procedure TFileShortcut.Load(ShortcutFile: string);
var
  Buffer: PChar;
  FilePart: PChar;
  PersistFile: IPersistFile;
begin
  if Assigned(ShellLink) then
  begin
    if ShellLink.QueryInterface(System.TGUID(IID_IPersistFile), PersistFile) = S_OK then
    begin
      GetMem(Buffer, MAX_PATH + 1);
      GetFullPathName(PChar(ShortcutFile), MAX_PATH + 1, Buffer, FilePart);
      PersistFile.Load(StringToOleStr(Buffer), STGM_READ);
    end;
  end;
end;

procedure TFileShortcut.Save(ShortcutFile: string);
var
  Buffer: PChar;
  FilePart: PChar;
  PersistFile: IPersistFile;
begin
  if Assigned(ShellLink) then
  begin
    if ShellLink.QueryInterface(System.TGUID(IID_IPersistFile), PersistFile) = S_OK then
    begin
      GetMem(Buffer, MAX_PATH + 1);
      GetFullPathName(PChar(ShortcutFile), MAX_PATH + 1, Buffer, FilePart);
      PersistFile.Save(StringToOleStr(Buffer), True);
    end;
  end;
end;

procedure TFileShortcut.Resolve(WindowHandle: HWnd; Flags: DWord);
begin
  ShellLink.Resolve(WindowHandle, Flags);
end;

end.
