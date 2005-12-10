unit SysTools;


interface


uses
  Windows, Shellapi, ShlObj, Registry, Messages, SysUtils, Classes;


(*****************************************************************************)
(*  Tools
(*****************************************************************************)

type
  TFileNames = array of TFileName;


procedure Log(TextLog: string);

procedure ReadFiles(TextDirectory: string; TextFileMask: string; var TextFiles: TFileNames; var CountFiles: Integer);
function BrowseFolder(HandleWindowParent: HWnd; TextTitle: string): string;

function GetFileCount(TextFileMask: string): Integer;
function GetRelativePath(TextFile: string; TextFileReference: string): string;
function GetAbsolutePath(TextFile: string; TextFileReference: string): string;
function GetDesktopPath: string;
function GetTempPath: string;
function GetTempFile(TextPrefix: string): string;
function GetLongFile(TextPath: string; TextFileShort: string): string;
function GetLongPath(TextPathShort: string): string;
function GetFirstParam(TextLine: string): string;
function GetQuotedParam(TextParam: string): string;

function LaunchProgram(TextCommand: string): Boolean;


(*****************************************************************************)
(*  TThreadPipeRead
(*****************************************************************************)

type
  TPipedProcess = class;
  TPipedOutput = (poStandard, poError);

  TThreadPipeRead = class(TThread)
  private
    BufferPipe:     string;
    BufferPipeAccu: string;
    HandlePipe:     Integer;
    PipedOutput:    TPipedOutput;
    PipedProcess:   TPipedProcess;

    procedure DoOutput;

  protected
    procedure Execute; override;

  public
    constructor Create(APipedProcess: TPipedProcess; AHandlePipe: Cardinal; APipedOutput: TPipedOutput);
  end;


(*****************************************************************************)
(*  TThreadDebug
(*****************************************************************************)

  TThreadDebug = class(TThread)
  private
    PipedProcess: TPipedProcess;

  protected
    procedure Execute; override;

  public
    constructor Create(APipedProcess: TPipedProcess);
  end;


(*****************************************************************************)
(*  TPipedProcess
(*****************************************************************************)

  EPipedProcess       = class(Exception);
  EPipedProcessPipe   = class(EPipedProcess);
  EPipedProcessCreate = class(EPipedProcess);

  TEventOutput = procedure (Sender: TObject; const Data: string; Pipe: TPipedOutput) of object;
  TEventDebug  = procedure (Sender: TObject; const DebugEvent: TDebugEvent; var ContinueStatus: DWord) of object;

  PPipedProcess = ^TPipedProcess;

  TPipedProcess = class
  private
    FOnDebug:     TEventDebug;
    FOnOutput:    TEventOutput;
    FOnTerminate: TNotifyEvent;

    FCommand:   string;
    FDirectory: string;
    FExitCode:  Cardinal;
    FHandle:    Cardinal;
    FId:        Cardinal;

    CountThreadsPipe: Integer;
    ThreadDebug: TThreadDebug;
    ThreadPipeReadStdout: TThreadPipeRead;
    ThreadPipeReadStderr: TThreadPipeRead;

    procedure Launch(FlagsProcess: Cardinal);
    procedure ThreadPipeReadTerminate(Sender: TObject);

  public
    procedure Abort;
    procedure Debug;
    procedure Execute;
    function  Executing: Boolean;

    property OnDebug:     TEventDebug  read FOnDebug     write FOnDebug;
    property OnOutput:    TEventOutput read FOnOutput    write FOnOutput;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;

    property Command:   string   read FCommand   write FCommand;
    property Directory: string   read FDirectory write FDirectory;
    property ExitCode:  Cardinal read FExitCode;
    property Handle:    Cardinal read FHandle;
    property Id:        Cardinal read FId;
  end;


implementation


(*****************************************************************************)
(*  Imports
(*****************************************************************************)

function ReadFile(hFile: THandle; pBuffer: Pointer; nNumberOfBytesToRead: DWord; var lpNumberOfBytesRead: DWord; lpOverlapped: POverlapped): Boolean; stdcall; external 'kernel32.dll';


(*****************************************************************************)
(*  Tools
(*****************************************************************************)

var
  FlagLogCleared: Boolean = False;


procedure Log(TextLog: string);
var
  FileLog: TextFile;
  TextFileLog: string;
begin
  TextFileLog := ChangeFileExt(ParamStr(0), '.log');

  if not FlagLogCleared then
  begin
    DeleteFile(TextFileLog);
    FlagLogCleared := True;
  end;

  try
    AssignFile(FileLog, TextFileLog);
    if FileExists(TextFileLog) then Append(FileLog) else Rewrite(FileLog);
    WriteLn(FileLog, Format('[%s] %s', [DateTimeToStr(Now), TextLog]));
    CloseFile(FileLog);

  except
    on Exception do;
  end;
end;


procedure ReadFiles(TextDirectory: string; TextFileMask: string; var TextFiles: TFileNames; var CountFiles: Integer);
var
  ResultFind: Integer;
  SearchRec: TSearchRec;
begin
  try
    TextDirectory := IncludeTrailingBackslash(ExpandFileName(TextDirectory));
    ResultFind := FindFirst(TextDirectory + TextFileMask, faReadOnly + faHidden + faArchive, SearchRec);

    while ResultFind = 0 do
    begin
      Inc(CountFiles);

           if Length(TextFiles) = 0          then SetLength(TextFiles, 1)
      else if Length(TextFiles) < CountFiles then SetLength(TextFiles, Length(TextFiles) * 2);

      TextFiles[CountFiles - 1] := TextDirectory + SearchRec.Name;

      ResultFind := FindNext(SearchRec);
    end;

    FindClose(SearchRec);

  except
    on Exception do;
  end;
end;


function BrowseFolder(HandleWindowParent: HWnd; TextTitle: string): string;
var
  BrowseInfo: TBrowseInfo;
  PointerIdListPath: Pointer;
begin
  SetLength(Result, MAX_PATH);

  BrowseInfo.hwndOwner      := HandleWindowParent;
  BrowseInfo.pidlRoot       := nil;
  BrowseInfo.pszDisplayName := nil;
  BrowseInfo.lpszTitle      := PChar(TextTitle);
  BrowseInfo.ulFlags        := BIF_RETURNONLYFSDIRS;
  BrowseInfo.lpfn           := nil;

  PointerIdListPath := SHBrowseForFolder(BrowseInfo);

  if Assigned(PointerIdListPath) and SHGetPathFromIDList(PointerIdListPath, PChar(Result))
    then SetLength(Result, Pos(#0, Result) - 1)
    else Result := EmptyStr;
end;


function GetFileCount(TextFileMask: string): Integer;
var
  ResultFind: Integer;
  SearchRec: TSearchRec;
begin
  Result := 0;

  try
    ResultFind := FindFirst(TextFileMask, faReadOnly + faHidden + faArchive, SearchRec);

    while ResultFind = 0 do
    begin
      Inc(Result);
      ResultFind := FindNext(SearchRec);
    end;

  except
    on Exception do;
  end;
end;


function GetRelativePath(TextFile: string; TextFileReference: string): string;
var
  CountParts: Integer;
  IndexCharSeparator: Integer;
  IndexCharSeparatorReference: Integer;
begin
  TextFile          := ExpandFileName(TextFile);
  TextFileReference := ExpandFileName(TextFileReference);

  CountParts := 0;

  while True do
  begin
    IndexCharSeparator          := Pos('\', TextFile);
    IndexCharSeparatorReference := Pos('\', TextFileReference);

    if    (IndexCharSeparator          = 0)
       or (IndexCharSeparatorReference = 0)
       or not AnsiSameText(Copy(TextFile,          1, IndexCharSeparator),
                           Copy(TextFileReference, 1, IndexCharSeparatorReference)) then Break;

    Inc(CountParts);
    Delete(TextFile,          1, IndexCharSeparator);
    Delete(TextFileReference, 1, IndexCharSeparatorReference);
  end;

  if CountParts = 0 then
  begin
    Result := TextFile;
  end

  else if CountParts = 1 then
  begin
    Result := '\' + TextFile;
  end

  else begin
    while True do
    begin
      IndexCharSeparatorReference := Pos('\', TextFileReference);
      if IndexCharSeparatorReference = 0 then Break;

      TextFile := '..\' + TextFile;
      Delete(TextFileReference, 1, IndexCharSeparatorReference);
    end;

    Result := TextFile;
  end;
end;


function GetAbsolutePath(TextFile: string; TextFileReference: string): string;
var
  IndexCharSeparator: Integer;
  TextFilePart: string;
begin
  Result := ExtractFilePath(TextFileReference);

  if (Length(TextFile) >= 2) and (TextFile[2] = ':') then
  begin
    Result := Copy(TextFile, 1, 2) + '\';
    Delete(TextFile, 1, 2);
  end;

  if (Length(TextFile) >= 1) and (TextFile[1] = '\') then
  begin
    Result := Copy(Result, 1, 3);
    Delete(TextFile, 1, 1);
  end;

  while (Length(TextFile) >= 1) and (TextFile[1] = '\') do
    Delete(TextFile, 1, 1);

  while Length(TextFile) > 0 do
  begin
    IndexCharSeparator := Pos('\', TextFile) - 1;
    if IndexCharSeparator < 0 then
      IndexCharSeparator := Length(TextFile);

    TextFilePart := Copy(TextFile, 1, IndexCharSeparator);

         if TextFilePart = '..' then Result := ExtractFilePath(ExcludeTrailingBackslash(Result))
    else if TextFilePart <> '.' then Result := IncludeTrailingBackslash(Result) + TextFilePart;

    while (IndexCharSeparator < Length(TextFile)) and (TextFile[IndexCharSeparator + 1] = '\') do
      Inc(IndexCharSeparator);
    Delete(TextFile, 1, IndexCharSeparator);
  end;
end;


function GetDesktopPath: string;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  if Registry.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders')
    then Result := Registry.ReadString('Desktop')
    else Result := EmptyStr;
  Registry.Free;
end;


function GetTempPath: string;
var
  BufferPath: array [0..MAX_PATH] of Char;
begin
  if Windows.GetTempPath(SizeOf(BufferPath), BufferPath) <> 0
    then Result := BufferPath
    else Result := '';
end;


function GetTempFile(TextPrefix: string): string;
var
  BufferFile: array [0..MAX_PATH] of Char;
begin
  if Windows.GetTempFileName(PChar(GetTempPath), PChar(TextPrefix), 0, BufferFile) <> 0
    then Result := BufferFile
    else Result := '';
end;


function GetLongFile(TextPath: string; TextFileShort: string): string;
var
  SearchRec: TSearchRec;
begin
  Result := TextFileShort;

  try
    if FindFirst(IncludeTrailingBackslash(TextPath) + TextFileShort, faAnyFile, SearchRec) = 0 then
      Result := SearchRec.Name;
  except
    on EInOutError do;
  end;
end;


function GetLongPath(TextPathShort: string): string;
var
  IndexCharSeparator: Integer;
  TextFileLong: string;
begin
  Result := '';
  TextPathShort := ExpandFileName(TextPathShort);

  while Length(TextPathShort) > 0 do
  begin
    IndexCharSeparator := Pos('\', TextPathShort) - 1;
    if IndexCharSeparator < 0 then
      IndexCharSeparator := Length(TextPathShort);

    TextFileLong := Copy(TextPathShort, 1, IndexCharSeparator);
    if Length(Result) = 0
      then TextFileLong := UpperCase(TextFileLong)
      else TextFileLong := '\' + GetLongFile(Result, TextFileLong);

    AppendStr(Result, TextFileLong);
    Delete(TextPathShort, 1, IndexCharSeparator + 1);
  end;
end;


function GetFirstParam(TextLine: string): string;
var
  IndexCharSeparator: Integer;
begin
  TextLine := TrimLeft(TextLine);

  if Length(TextLine) = 0 then
  begin
    Result := '';
  end

  else if TextLine[1] = '"' then
  begin
    Result := TextLine;
    Delete(Result, 1, 1);

    IndexCharSeparator := Pos('"', Result) - 1;
    if IndexCharSeparator < 0 then
      IndexCharSeparator := Length(Result);

    Result := Trim(Copy(Result, 1, IndexCharSeparator));
  end

  else begin
    IndexCharSeparator := Pos(' ', TextLine) - 1;
    if IndexCharSeparator < 0 then
      IndexCharSeparator := Length(TextLine);

    Result := Copy(TextLine, 1, IndexCharSeparator);
  end;
end;


function GetQuotedParam(TextParam: string): string;
begin
  if Pos(' ', TextParam) > 0
    then Result := '"' + TextParam + '"'
    else Result := TextParam;
end;


function LaunchProgram(TextCommand: string): Boolean;
var
  IndexCharSeparator: Integer;
  TextParameters: string;
begin
  TextCommand := TrimLeft(TextCommand);

  if (Length(TextCommand) > 0) and (TextCommand[1] = '"') then
  begin
    Delete(TextCommand, 1, 1);
    IndexCharSeparator := Pos('"', TextCommand);
  end
  else begin
    IndexCharSeparator := Pos(' ', TextCommand);
  end;

  if IndexCharSeparator = 0 then
    IndexCharSeparator := Length(TextCommand) + 1;

  TextParameters := TrimLeft(Copy(TextCommand, IndexCharSeparator + 1, Length(TextCommand)));
  Delete(TextCommand, IndexCharSeparator, Length(TextCommand));

  Result := ShellExecute(0, nil, PChar(TextCommand), PChar(TextParameters), PChar(ExtractFilePath(TextCommand)), SW_SHOWDEFAULT) > 32;
end;


(*****************************************************************************)
(*  TThreadPipeRead
(*****************************************************************************)

constructor TThreadPipeRead.Create(APipedProcess: TPipedProcess; AHandlePipe: Cardinal; APipedOutput: TPipedOutput);
begin
  HandlePipe   := AHandlePipe;
  PipedProcess := APipedProcess;

  FreeOnTerminate := True;
  inherited Create(True);
end;


procedure TThreadPipeRead.Execute;
var
  CountBytes: Cardinal;
begin
  SetLength(BufferPipe, $1000);

  while ReadFile(HandlePipe, PChar(BufferPipe), $1000, CountBytes, nil) and (CountBytes > 0) do
  begin
    BufferPipeAccu := BufferPipeAccu + Copy(BufferPipe, 1, CountBytes);
    Synchronize(DoOutput);
  end;
end;


procedure TThreadPipeRead.DoOutput;
var
  BufferPipeCopy: string;
begin
  if Assigned(PipedProcess.FOnOutput) then
  begin
    BufferPipeCopy := BufferPipeAccu;
    BufferPipeAccu := '';
    PipedProcess.FOnOutput(Self, BufferPipeCopy, PipedOutput);
  end;
end;


(*****************************************************************************)
(*  TPipedProcess
(*****************************************************************************)

function EnumWindowsProc(HandleWindow: HWnd; LParam: LParam): Boolean; stdcall;
var
  HandleWindowForeground: HWnd;
  IdProcess: Cardinal;
begin
  GetWindowThreadProcessId(HandleWindow, Addr(IdProcess));

  if IdProcess = PPipedProcess(LParam).FId then
  begin
    case Win32Platform of
      VER_PLATFORM_WIN32_WINDOWS:
      begin
        HandleWindowForeground := GetForegroundWindow;

        if IsWindow(HandleWindowForeground) and SetForegroundWindow(HandleWindow) then
        begin
          keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), 0,               0);
          keybd_event(VK_CANCEL,  MapVirtualKey(VK_CANCEL,  0), 0,               0);
          keybd_event(VK_CANCEL,  MapVirtualKey(VK_CANCEL,  0), KEYEVENTF_KEYUP, 0);
          keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), KEYEVENTF_KEYUP, 0);
        end;

        SetForegroundWindow(HandleWindowForeground);
      end;

      VER_PLATFORM_WIN32_NT:
      begin
        PostMessage(HandleWindow, WM_CLOSE, 0, 0);
      end;
    end;
    
    Result := False;
  end
  else begin
    Result := True;
  end;
end;


procedure TPipedProcess.Abort;
begin
  if Executing then
  begin
    EnumWindows(Addr(EnumWindowsProc), LParam(Addr(Self)));

    if Assigned(ThreadDebug) then
      ThreadDebug.Terminate;
  end;
end;


procedure TPipedProcess.Execute;
begin
  if Executing then
    raise EPipedProcess.Create('Cannot execute the same process again before it is terminated');

  ThreadDebug := nil;
  Launch(CREATE_NEW_PROCESS_GROUP);
end;


procedure TPipedProcess.Debug;
begin
  if Executing then
    raise EPipedProcess.Create('Cannot debug the same process again before it is terminated');

  ThreadDebug := TThreadDebug.Create(Self);
end;


procedure TPipedProcess.Launch(FlagsProcess: Cardinal);
var
  HandlePipeStdoutOld: Cardinal;
  HandlePipeStdoutRead: Cardinal;
  HandlePipeStdoutReadDuplicate: Cardinal;
  HandlePipeStdoutWrite: Cardinal;
  HandlePipeStderrOld: Cardinal;
  HandlePipeStderrRead: Cardinal;
  HandlePipeStderrReadDuplicate: Cardinal;
  HandlePipeStderrWrite: Cardinal;
  PointerDirectory: PChar;
  StructProcessInformation: TProcessInformation;
  StructSecurityAttributes: TSecurityAttributes;
  StructStartupInfo: TStartupInfo;
begin
  StructSecurityAttributes.nLength := SizeOf(StructSecurityAttributes);
  StructSecurityAttributes.bInheritHandle := True;
  StructSecurityAttributes.lpSecurityDescriptor := nil;

  HandlePipeStdoutOld := GetStdHandle(STD_OUTPUT_HANDLE);
  if not CreatePipe(HandlePipeStdoutRead, HandlePipeStdoutWrite, Addr(StructSecurityAttributes), 0)                                                        then raise EPipedProcessPipe.Create('Unable to create pipe for standard output');

  try
    if not SetStdHandle(STD_OUTPUT_HANDLE, HandlePipeStdoutWrite)                                                                                            then raise EPipedProcessPipe.Create('Unable to set pipe to standard output');
    if not DuplicateHandle(GetCurrentProcess, HandlePipeStdoutRead, GetCurrentProcess, Addr(HandlePipeStdoutReadDuplicate), 0, False, DUPLICATE_SAME_ACCESS) then raise EPipedProcessPipe.Create('Unable to duplicate pipe for standard output');

  finally
    CloseHandle(HandlePipeStdoutRead);
  end;

  HandlePipeStderrOld := GetStdHandle(STD_ERROR_HANDLE);
  if not CreatePipe(HandlePipeStderrRead, HandlePipeStderrWrite, Addr(StructSecurityAttributes), 0)                                                        then raise EPipedProcessPipe.Create('Unable to create pipe for error output');

  try
    if not SetStdHandle(STD_ERROR_HANDLE, HandlePipeStderrWrite)                                                                                             then raise EPipedProcessPipe.Create('Unable to set pipe to error output');
    if not DuplicateHandle(GetCurrentProcess, HandlePipeStderrRead, GetCurrentProcess, Addr(HandlePipeStderrReadDuplicate), 0, False, DUPLICATE_SAME_ACCESS) then raise EPipedProcessPipe.Create('Unable to duplicate pipe for standard output');

  finally
    CloseHandle(HandlePipeStderrRead);
  end;

  ZeroMemory(Addr(StructProcessInformation), SizeOf(StructProcessInformation));
  ZeroMemory(Addr(StructStartupInfo),        SizeOf(StructStartupInfo));

  StructStartupInfo.cb          := SizeOf(StructStartupInfo);
  StructStartupInfo.wShowWindow := SW_HIDE;
  StructStartupInfo.dwFlags     := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
  StructStartupInfo.hStdOutput  := HandlePipeStdoutWrite;
  StructStartupInfo.hStdError   := HandlePipeStderrWrite;

  try
    PointerDirectory := nil;
    if Length(FDirectory) > 0 then PointerDirectory := PChar(FDirectory);

    if not CreateProcess(nil, PChar(FCommand), nil, nil, True, FlagsProcess, nil, PointerDirectory, StructStartupInfo, StructProcessInformation) then
      raise EPipedProcessCreate.Create('Unable to create process');

    FHandle := StructProcessInformation.hProcess;
    FId     := StructProcessInformation.dwProcessId;

  finally
    SetStdHandle(STD_OUTPUT_HANDLE, HandlePipeStdoutOld);
    SetStdHandle(STD_ERROR_HANDLE,  HandlePipeStderrOld);
    CloseHandle(HandlePipeStdoutWrite);
    CloseHandle(HandlePipeStderrWrite);
  end;

  CountThreadsPipe := 2;
  ThreadPipeReadStdout := TThreadPipeRead.Create(Self, HandlePipeStdoutReadDuplicate, poStandard);
  ThreadPipeReadStderr := TThreadPipeRead.Create(Self, HandlePipeStderrReadDuplicate, poError);
  ThreadPipeReadStdout.OnTerminate := ThreadPipeReadTerminate;
  ThreadPipeReadStderr.OnTerminate := ThreadPipeReadTerminate;
  ThreadPipeReadStdout.Resume;
  ThreadPipeReadStderr.Resume;
end;


function TPipedProcess.Executing: Boolean;
begin
  try
    Result := FHandle <> 0;

  except
    on Exception do Result := False;
  end;
end;


procedure TPipedProcess.ThreadPipeReadTerminate(Sender: TObject);
begin
  Dec(CountThreadsPipe);

  if CountThreadsPipe = 0 then
  begin
    if Assigned(ThreadDebug) then
      ThreadDebug.Terminate;

    GetExitCodeProcess(FHandle, FExitCode);

    FHandle := 0;
    FId     := 0;

    if Assigned(FOnTerminate) then
      FOnTerminate(Self);
  end;
end;


(*****************************************************************************)
(*  TThreadDebug
(*****************************************************************************)

constructor TThreadDebug.Create(APipedProcess: TPipedProcess);
begin
  PipedProcess := APipedProcess;

  FreeOnTerminate := True;
  inherited Create(False);
end;


procedure TThreadDebug.Execute;
var
  ContinueStatus: DWord;
  DebugEvent: TDebugEvent;
begin
  PipedProcess.Launch(CREATE_NEW_PROCESS_GROUP + DEBUG_PROCESS);

  while not Terminated do
  begin
    if not WaitForDebugEvent(DebugEvent, INFINITE) then
      Break;

    ContinueStatus := 0;
    if Assigned(PipedProcess.FOnDebug) then
      PipedProcess.FOnDebug(Self, DebugEvent, ContinueStatus);

    if ContinueStatus = 0 then
    begin
      ContinueStatus := DBG_CONTINUE;

      case DebugEvent.dwDebugEventCode of
        EXCEPTION_DEBUG_EVENT:
        begin
          if DebugEvent.Exception.ExceptionRecord.ExceptionCode <> EXCEPTION_BREAKPOINT then
            ContinueStatus := DBG_EXCEPTION_NOT_HANDLED;
        end;

        CREATE_PROCESS_DEBUG_EVENT:
        begin
          CloseHandle(DebugEvent.CreateProcessInfo.hFile);
        end;

        LOAD_DLL_DEBUG_EVENT:
        begin
          CloseHandle(DebugEvent.LoadDll.hFile);
        end;
      end;
    end;

    ContinueDebugEvent(DebugEvent.dwProcessId, DebugEvent.dwThreadId, ContinueStatus)
  end;
end;


end.
