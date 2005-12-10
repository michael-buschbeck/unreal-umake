unit UMake_Configuration;


interface


uses
  Classes, SysUtils, SysTools, FileCtrl, Hashes;


(*****************************************************************************)
(*  TConfiguration
(*****************************************************************************)

type
  EConfiguration = class(Exception);

  EConfigurationGameDirInvalid     = class(EConfiguration);
  EConfigurationGameDirNotFound    = class(EConfiguration);
  EConfigurationGameIniNotFound    = class(EConfiguration);
  EConfigurationPackageDirInvalid  = class(EConfiguration);
  EConfigurationPackageDirNotFound = class(EConfiguration);

  TConfiguration = class
  private
    HashFilePackage: TStringHash;
    TextDirCacheRecord: string;
    TextDirGame: string;
    TextDirPackage: string;
    TextFileIniGame: string;
    TextFileIniPackage: string;
    TextNamePackage: string;

    function FindIniGame: Boolean;
    function FindIniPackage: Boolean;

    procedure StringListPathsChange(Sender: TObject);

  public
    StringListPaths: TStringList;
    StringListPackages: TStringList;

    constructor Create(ATextNamePackage: string; ATextDirGame: string);
    destructor Destroy; override;

    procedure Read;
    procedure Write;

    function FindFilePackage(TextPackage: string): string;

    property Package:    string read TextNamePackage;
    property DirGame:    string read TextDirGame;
    property DirPackage: string read TextDirPackage;
  end;


implementation


(*****************************************************************************)
(*  TConfiguration
(*****************************************************************************)

constructor TConfiguration.Create(ATextNamePackage: string; ATextDirGame: string);
begin
  TextNamePackage := ATextNamePackage;
  TextDirGame     := ATextDirGame;

  if not DirectoryExists(TextDirGame) then
    raise EConfigurationGameDirNotFound.Create('Game directory not found');
  if not FileExists(IncludeTrailingBackslash(TextDirGame) + 'System\ucc.exe') then
    raise EConfigurationGameDirInvalid.Create('Game directory does not contain valid "System" subdirectory');

  TextDirPackage := IncludeTrailingBackslash(TextDirGame) + TextNamePackage;

  if not DirectoryExists(TextDirPackage) then
    raise EConfigurationPackageDirNotFound.Create('Package directory not found');
  if not DirectoryExists(IncludeTrailingBackslash(TextDirPackage) + 'Classes') then
    raise EConfigurationPackageDirInvalid.Create('Package directory does not contain "Classes" subdirectory');

  HashFilePackage    := TStringHash.Create;
  StringListPaths    := TStringList.Create;
  StringListPackages := TStringList.Create;

  StringListPaths.OnChange := StringListPathsChange;
end;


destructor TConfiguration.Destroy;
begin
  HashFilePackage.Free;
  StringListPaths.Free;
  StringListPackages.Free;

  inherited;
end;


function TConfiguration.FindIniGame: Boolean;
var
  FileIni: TextFile;
  TextFileIni: string;
  TextLineIni: string;
  ResultFind: Integer;
  SearchRecIni: TSearchRec;
begin
  Result := False;
  ResultFind := FindFirst(IncludeTrailingBackslash(TextDirGame) + 'System\*.ini', faAnyFile, SearchRecIni);

  while ResultFind = 0 do
  begin
    if not AnsiSameText(SearchRecIni.Name, 'Default.ini') then
    begin
      TextFileIni := IncludeTrailingBackslash(TextDirGame) + 'System\' + SearchRecIni.Name;

      try
        AssignFile(FileIni, TextFileIni);
        Reset(FileIni);
        Readln(FileIni, TextLineIni);
        CloseFile(FileIni);

        if AnsiSameText(TextLineIni, '[url]') then
        begin
          Result := True;
          TextFileIniGame := TextFileIni;
          Break;
        end;

      except
        on EInOutError do;
      end;
    end;

    ResultFind := FindNext(SearchRecIni);
  end;

  FindClose(SearchRecIni);
end;


function TConfiguration.FindIniPackage: Boolean;
begin
  TextFileIniPackage := IncludeTrailingBackslash(TextDirPackage) + 'make.ini';
  Result := FileExists(TextFileIniPackage);
end;


procedure TConfiguration.Read;
var
  FileIni: TextFile;
  IndexCharSeparator: Integer;
  TextLine: string;
  TextLineName: string;
  TextLineValue: string;
  TextSection: string;


  procedure ReadIni(TextFileIni: string);
  begin
    StringListPaths.Clear;
    StringListPackages.Clear;

    AssignFile(FileIni, TextFileIni);
    Reset(FileIni);

    while not Eof(FileIni) do
    begin
      Readln(FileIni, TextLine);
      TextLine := Trim(TextLine);

      if (Length(TextLine) = 0) or (TextLine[1] = ';') then
        Continue;

      if TextLine[1] = '[' then
      begin
        TextSection := Copy(TextLine, 2, Length(TextLine) - 2);
      end
      else begin
        IndexCharSeparator := Pos('=', TextLine);
        if IndexCharSeparator = 0 then
          Continue;

        TextLineName := Copy(TextLine, 1, IndexCharSeparator - 1);
        TextLineValue := Copy(TextLine, IndexCharSeparator + 1, Length(TextLine));

        if AnsiSameText(TextSection, 'Core.System') then
        begin
          if AnsiSameText(TextLineName, 'Paths') then
            StringListPaths.Add(TextLineValue);
          if AnsiSameText(TextLineName, 'CacheRecordPath') then
            TextDirCacheRecord := TextLineValue;
        end

        else if AnsiSameText(TextSection, 'Editor.EditorEngine') then
        begin
          if AnsiSameText(TextLineName, 'EditPackages') then
            StringListPackages.Add(TextLineValue);
        end;
      end;
    end;

    CloseFile(FileIni);
  end;


var
  FlagFound: Boolean;
  IndexPackage: Integer;
begin
  if not FindIniGame then
    raise EConfigurationGameIniNotFound.Create('Game configuration file not found');
  ReadIni(TextFileIniGame);

  if FindIniPackage then
    ReadIni(TextFileIniPackage);

  FlagFound := False;
  for IndexPackage := 0 to StringListPackages.Count - 1 do
  begin
    if AnsiSameText(StringListPackages[IndexPackage], TextNamePackage) then
    begin
      FlagFound := True;
      Break;
    end;
  end;

  if not FlagFound then
    StringListPackages.Add(TextNamePackage);
end;


procedure TConfiguration.Write;
var
  StringListIni: TStringList;


  procedure InsertSettings(TextSection: string; TextName: string; StringListSettings: TStringList);
  var
    IndexLine: Integer;
    IndexLineSection: Integer;
    IndexLineSetting: Integer;
    IndexSetting: Integer;
    TextLine: string;
  begin
    TextSection := '[' + TextSection + ']';
    TextName := TextName + '=';

    IndexLineSection := 0;
    while (IndexLineSection < StringListIni.Count) and not AnsiSameText(Trim(StringListIni[IndexLineSection]), TextSection) do
      Inc(IndexLineSection);

    if IndexLineSection < StringListIni.Count then
    begin
      IndexLineSetting := -1;
      IndexLine := IndexLineSection + 1;

      while IndexLine < StringListIni.Count do
      begin
        TextLine := Trim(StringListIni[IndexLine]);

        if (Length(TextLine) > 0) and (TextLine[1] = '[') then
        begin
          Break;
        end

        else if AnsiSameText(Copy(TextLine, 1, Length(TextName)), TextName) then
        begin
          if IndexLineSetting < 0 then
            IndexLineSetting := IndexLine;
          StringListIni.Delete(IndexLine);
        end

        else begin
          Inc(IndexLine);
        end;
      end;

      if IndexLineSetting < 0 then
      begin
        IndexLineSetting := IndexLine;
        while Length(Trim(StringListIni[IndexLineSetting - 1])) = 0 do
          Dec(IndexLineSetting);
      end;

      for IndexSetting := StringListSettings.Count - 1 downto 0 do
        StringListIni.Insert(IndexLineSetting, TextName + StringListSettings[IndexSetting]);
    end
    else begin
      StringListIni.Add('');
      StringListIni.Add(TextSection);

      for IndexSetting := 0 to StringListSettings.Count - 1 do
        StringListIni.Add(TextName + StringListSettings[IndexSetting]);
    end;
  end;


  procedure InsertSetting(TextSection: string; TextName: string; TextSetting: string);
  var
    StringListSetting: TStringList;
  begin
    StringListSetting := TStringList.Create;
    StringListSetting.Add(TextSetting);
    InsertSettings(TextSection, TextName, StringListSetting);
    StringListSetting.Free;
  end;


var
  FlagChanged: Boolean;
  IndexLine: Integer;
  StringListIniOriginal: TStringList;
begin
  StringListIni         := TStringList.Create;
  StringListIniOriginal := TStringList.Create;

  FindIniPackage;
  if FileExists(TextFileIniPackage) then
  begin
    StringListIni.LoadFromFile(TextFileIniPackage);
    StringListIniOriginal.Assign(StringListIni);
  end
  else begin
    StringListIni.Add('; Generated by UMake');
    StringListIni.Add('');
    StringListIni.Add('[Engine.Engine]');
    StringListIni.Add('EditorEngine=Editor.EditorEngine');
    StringListIni.Add('');
    StringListIni.Add('[Editor.EditorEngine]');
    StringListIni.Add('CacheSizeMegs=32');
  end;

  InsertSettings('Core.System',         'Paths',           StringListPaths);
  InsertSettings('Editor.EditorEngine', 'EditPackages',    StringListPackages);

  if Length(TextDirCacheRecord) > 0 then
    InsertSetting ('Core.System', 'CacheRecordPath', TextDirCacheRecord);

  if StringListIni.Count <> StringListIniOriginal.Count then
  begin
    FlagChanged := True;
  end
  else begin
    FlagChanged := False;
    for IndexLine := 0 to StringListIni.Count - 1 do
    begin
      if not AnsiSameText(StringListIni[IndexLine], StringListIniOriginal[IndexLine]) then
      begin
        FlagChanged := True;
        Break;
      end;
    end;
  end;

  if FlagChanged then
    StringListIni.SaveToFile(TextFileIniPackage);

  StringListIni.Free;
end;


procedure TConfiguration.StringListPathsChange(Sender: TObject);
begin
  HashFilePackage.Clear;
end;


function TConfiguration.FindFilePackage(TextPackage: string): string;
var
  IndexPath: Integer;
  TextFilePackage: string;
begin
  if not HashFilePackage.Exists(LowerCase(TextPackage)) then
  begin
    for IndexPath := 0 to StringListPaths.Count - 1 do
    begin
      TextFilePackage := StringReplace(StringListPaths[IndexPath], '/', '\', [rfReplaceAll]);
      TextFilePackage := GetAbsolutePath(TextFilePackage, IncludeTrailingBackslash(TextDirGame) + 'System\');
      TextFilePackage := StringReplace(TextFilePackage, '*', TextPackage, [rfReplaceAll]);

      if FileExists(TextFilePackage) then
      begin
        HashFilePackage[LowerCase(TextPackage)] := TextFilePackage;
        Break;
      end;
    end;

    if not HashFilePackage.Exists(LowerCase(TextPackage)) then
      HashFilePackage[LowerCase(TextPackage)] := '';
  end;

  Result := HashFilePackage[LowerCase(TextPackage)];
end;


end.
